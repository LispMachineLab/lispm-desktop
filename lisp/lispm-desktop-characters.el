;;; lispm-desktop-characters.el --- Characters: an app to make them, and theirs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; LispM's characters (lispm-characters.el of LispM Agent) are someone,
;; not a session: a name, a soul, a picture, a memory and a past of their
;; own, a conversation in each place they are.  The desktop gives them
;; applications:
;;
;;   Characters   lists them, and makes, changes and deletes them, in
;;                LispM's form of a character (`lispm-character-form'),
;;                shown in a window of its own; its keys are n, RET, e
;;                and d, as its buttons are New, Talk, Change and Delete.
;;   one each     every character the user made is an application of its
;;                own, its picture its icon, in the Applications folder:
;;                opened, it shows the character's own conversation.
;;
;; The applications come and go with the characters, as LispM tells of
;; them (`lispm-character-change-functions').  A picture is no SVG, which
;; the Finder and the desktop draw large icons in, so a character's large
;; icon is an SVG holding its picture, made once.
;;
;; An application installed from the App Store with a skill is a
;; character too, of a kind (`lispm-define-app-character'): its skill is
;; its soul and its icon its picture; agents reach it by name, as @Rotom.
;; It needs no application of its own: it is one.
;;
;; LispM Agent without characters, as one older than them, leaves all
;; this out.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'image)
(require 'lispm-desktop-core)
(require 'lispm-desktop-i18n)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-app-window)
(require 'lispm-desktop-platinum)

(declare-function lispm-characters "lispm-core" ())
(declare-function lispm-get-character "lispm-core" (character &optional noerror))
(declare-function lispm-character-name "lispm-core" (character))
(declare-function lispm-character-title* "lispm-core" (character))
(declare-function lispm-character-description "lispm-core" (character))
(declare-function lispm-character-avatar "lispm-core" (character))
(declare-function lispm-character-skill "lispm-core" (character))
(declare-function lispm-character-agent "lispm-core" (character &optional root))
(declare-function lispm-character-form "lispm-characters" (&optional character))
(declare-function lispm-delete-character "lispm-characters" (character))
(declare-function lispm-define-app-character "lispm-characters" (name &rest props))
(declare-function lispm-remove-app-character "lispm-characters" (name))
(declare-function lispm-chat "lispm-chat" (agent))
(declare-function lispm-desktop-app-ui-define "lispm-desktop-app-ui" (key &rest properties))
(declare-function lispm-desktop-app-ui-menus "lispm-desktop-app-ui" (app))
(declare-function lispm-desktop-finder-folder-changed "lispm-desktop-finder" (key))
(declare-function lispm-desktop-remove-alias "lispm-desktop-surface" (app))
(declare-function lispm-desktop-store--installed-skills "lispm-desktop-store" (id))
(defvar lispm-character-change-functions)
(defvar lispm-desktop-store-installed)

(defvar lispm-desktop-characters-icon-directory
  (locate-user-emacs-file "lispm-desktop/character-icons/")
  "Directory of the large icons made of the characters' pictures.")

(defconst lispm-desktop-characters-buffer-name "*Characters*"
  "Name of the buffer listing the characters.")

(defun lispm-desktop-characters-available-p ()
  "Return non-nil when LispM has characters."
  (and (require 'lispm-characters nil t) (fboundp 'lispm-character-form)))

(defun lispm-desktop-characters--people ()
  "The characters the user made: those that are no application."
  (seq-remove #'lispm-character-skill (lispm-characters)))

;;;; Icons

(defun lispm-desktop-characters--large-icon (picture)
  "Return a 32-pixel SVG icon holding PICTURE, a file, framed in black.
It is written once, in `lispm-desktop-characters-icon-directory', named
after the picture, whose name changes with what it shows."
  (let* ((directory (file-name-as-directory lispm-desktop-characters-icon-directory))
         (file (expand-file-name (format "%s-%s-32.svg"
                                         (file-name-base picture)
                                         (substring (secure-hash 'md5 (expand-file-name picture)) 0 8))
                                 directory)))
    (unless (file-exists-p file)
      (make-directory directory t)
      (let* ((type (or (image-supported-file-p picture) 'png))
             (data (with-temp-buffer
                     (set-buffer-multibyte nil)
                     (insert-file-contents-literally picture)
                     (base64-encode-string (buffer-string) t)))
             (coding-system-for-write 'utf-8))
        (with-temp-file file
          (insert (format "<svg xmlns=\"http://www.w3.org/2000/svg\" \
xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"32\" height=\"32\" viewBox=\"0 0 32 32\">\
<image x=\"1\" y=\"1\" width=\"30\" height=\"30\" preserveAspectRatio=\"xMidYMid slice\" \
xlink:href=\"data:image/%s;base64,%s\"/>\
<rect x=\"0.5\" y=\"0.5\" width=\"31\" height=\"31\" fill=\"none\" stroke=\"#000000\"/></svg>\n"
                          (if (eq type 'jpeg) "jpeg" type) data)))))
    file))

(defun lispm-desktop-characters--icons (character)
  "Return CHARACTER's icons, (SMALL . LARGE): its picture, else a person's."
  (if-let* ((picture (lispm-character-avatar character))
            ((file-readable-p picture)))
      (cons picture (lispm-desktop-characters--large-icon picture))
    (cons 'character 'character-32)))

;;;; Each character's application

(defun lispm-desktop-characters-app (name)
  "Return the key of the application of the character NAME, a symbol."
  (intern (format "character-%s" name)))

(defun lispm-desktop-characters-talk (character)
  "Open CHARACTER's own conversation, in its application's window."
  (interactive (list (lispm-desktop-characters--read (lispm-desktop-tr "Talk to: "))))
  (let* ((character (lispm-get-character character))
         (name (lispm-character-name character)))
    (lispm-desktop-app-window-show (lispm-desktop-characters-app name)
                                   :name (lispm-character-title* character)
                                   :buffer (lispm-chat (lispm-character-agent character)))))

(defun lispm-desktop-characters--define-app (character)
  "Make CHARACTER, one the user made, an application of its own."
  (let* ((name (lispm-character-name character))
         (key (lispm-desktop-characters-app name))
         (icons (lispm-desktop-characters--icons character)))
    (lispm-desktop-define-app key
                              :name (lispm-character-title* character)
                              :icon (car icons)
                              :folder (cdr icons)
                              :description (lispm-character-description character)
                              :character name
                              :standalone t
                              :menus (lambda () (lispm-desktop-app-ui-menus key))
                              :activate (lambda () (lispm-desktop-characters-talk name))
                              :visible (lambda () (lispm-desktop-app-window-open-p key))
                              :listed-while-open t)))

(defun lispm-desktop-characters--remove-app (name)
  "Take the application of the character NAME away, its window and its alias."
  (let ((key (lispm-desktop-characters-app name)))
    (when (assq key lispm-desktop-apps)
      (lispm-desktop-app-window-close key)
      (lispm-desktop-remove-app key)
      (when (fboundp 'lispm-desktop-remove-alias)
        (lispm-desktop-remove-alias key)))))

(defun lispm-desktop-characters--changed (name change)
  "Follow the character NAME through CHANGE, `defined' or `removed'.
This is a function of `lispm-character-change-functions'."
  (let ((character (and (eq change 'defined) (lispm-get-character name t))))
    (if (and character (not (lispm-character-skill character)))
        (lispm-desktop-characters--define-app character)
      (lispm-desktop-characters--remove-app name))
    (when (fboundp 'lispm-desktop-finder-folder-changed)
      (lispm-desktop-finder-folder-changed 'applications))
    (lispm-desktop-characters--refresh)))

;;;; The App Store's applications

(defvar lispm-desktop-characters--apps nil
  "The applications that are characters as the desktop made them, by key.")

(defun lispm-desktop-characters--store-skill (key)
  "The name of the skill the application KEY, installed from the App Store,
came with; nil if none.  Its first, if it came with several."
  (when-let* (((fboundp 'lispm-desktop-store--installed-skills))
              (skill (car (lispm-desktop-store--installed-skills key))))
    (symbol-name (car skill))))

(defun lispm-desktop-characters--sync-apps ()
  "Make each application installed from the App Store with a skill a
character of its kind, and those uninstalled characters no more.
Run from `lispm-desktop-apps-changed-hook'."
  (when (fboundp 'lispm-define-app-character)
    (let (now)
      (pcase-dolist (`(,key . ,props) lispm-desktop-apps)
        (when-let* (((plist-get props :store))
                    (skill (lispm-desktop-characters--store-skill key)))
          (push key now)
          (unless (memq key lispm-desktop-characters--apps)
            (lispm-define-app-character
             key :title (lispm-desktop-tr (plist-get props :name)) :skill skill
             :description (plist-get props :description)
             :avatar (ignore-errors (lispm-desktop-platinum-icon-file
                                     (or (plist-get props :large-icon) (plist-get props :icon))))))))
      (dolist (key lispm-desktop-characters--apps)
        (unless (memq key now)
          (lispm-remove-app-character key)))
      (setq lispm-desktop-characters--apps now))))

;;;; The Characters application

(defvar-keymap lispm-desktop-characters-mode-map
  :doc "Keymap of `lispm-desktop-characters-mode'."
  "RET" #'lispm-desktop-characters-talk-here
  "n" #'lispm-desktop-characters-new
  "e" #'lispm-desktop-characters-change
  "d" #'lispm-desktop-characters-delete
  "g" #'lispm-desktop-characters-refresh)

(define-derived-mode lispm-desktop-characters-mode special-mode "Characters"
  "The characters LispM has, one a line.
\\<lispm-desktop-characters-mode-map>\\[lispm-desktop-characters-talk-here] talks to \
the one point is on, \\[lispm-desktop-characters-new] makes one,
\\[lispm-desktop-characters-change] changes it, \\[lispm-desktop-characters-delete] \
deletes it.

\\{lispm-desktop-characters-mode-map}"
  (setq-local revert-buffer-function (lambda (&rest _) (lispm-desktop-characters--draw))
              truncate-lines t))

(defun lispm-desktop-characters--buffer ()
  "Return the buffer listing the characters, drawn."
  (with-current-buffer (get-buffer-create lispm-desktop-characters-buffer-name)
    (unless (derived-mode-p 'lispm-desktop-characters-mode)
      (lispm-desktop-characters-mode))
    (lispm-desktop-characters--draw)
    (current-buffer)))

(defun lispm-desktop-characters--draw ()
  "Draw the list of characters in this buffer, point on the one it was on."
  (let ((name (get-text-property (point) 'lispm-desktop-characters-name))
        (inhibit-read-only t))
    (erase-buffer)
    (dolist (character (lispm-desktop-characters--people))
      (let ((start (point))
            (icon (cdr (lispm-desktop-characters--icons character))))
        ;; Its large icon, as large as a line: its picture, or a person's.
        (insert (if (or (lispm-desktop-tty-p) (not (display-images-p)))
                    (lispm-desktop-platinum-icon 'character)
                  (propertize " " 'display (create-image (lispm-desktop-platinum-icon-file icon) nil nil
                                                         :height 24 :ascent 'center)))
                "  " (propertize (lispm-character-title* character) 'face 'bold))
        (when-let* ((description (lispm-character-description character)))
          (insert "   " (propertize description 'face 'shadow)))
        (insert "\n")
        (put-text-property start (point) 'lispm-desktop-characters-name
                           (lispm-character-name character))))
    (goto-char (point-min))
    (when-let* ((at (and name (text-property-any (point-min) (point-max)
                                                 'lispm-desktop-characters-name name))))
      (goto-char at))))

(defun lispm-desktop-characters--refresh ()
  "Draw the list of characters again, if it is there."
  (when-let* ((buffer (get-buffer lispm-desktop-characters-buffer-name)))
    (with-current-buffer buffer
      (lispm-desktop-characters--draw))))

(defun lispm-desktop-characters-refresh ()
  "Draw the list of characters again."
  (interactive)
  (lispm-desktop-characters--refresh))

(defun lispm-desktop-characters--at-point ()
  "The character of the line point is on, or an error."
  (or (when-let* ((name (get-text-property (point) 'lispm-desktop-characters-name)))
        (lispm-get-character name t))
      (user-error "%s" (lispm-desktop-tr "No character here"))))

(defun lispm-desktop-characters--read (prompt)
  "Read the name of a character the user made, with PROMPT; return it."
  (let ((titles (mapcar #'lispm-character-title* (lispm-desktop-characters--people))))
    (unless titles
      (user-error "%s" (lispm-desktop-tr "There is no character yet")))
    (lispm-get-character (completing-read prompt titles nil t))))

(defun lispm-desktop-characters-open ()
  "Open the Characters application: the list of the characters."
  (interactive)
  (unless (lispm-desktop-characters-available-p)
    (user-error "%s" (lispm-desktop-tr "This LispM has no characters yet: update LispM Agent")))
  (lispm-desktop-app-window-show 'characters
                                 :name (lispm-desktop-tr "Characters")
                                 :buffer (lispm-desktop-characters--buffer)))

(defvar-local lispm-desktop-characters--form nil
  "Non-nil in the buffer of a form of a character the Characters app shows.")

(defun lispm-desktop-characters--form-closed ()
  "Close the window of the form of a character, its form gone."
  (unless (seq-some (lambda (buffer)
                      (and (eq (buffer-local-value 'major-mode buffer) 'lispm-form-mode)
                           (buffer-local-value 'lispm-desktop-characters--form buffer)))
                    (buffer-list))
    (lispm-desktop-app-window-close 'character-form)))

(defun lispm-desktop-characters--show-form (form)
  "Show FORM, the buffer of a form of a character, in its window: one at a time."
  (when (lispm-desktop-app-window-open-p 'character-form)
    (lispm-desktop-app-window-close 'character-form))
  (with-current-buffer form
    (setq lispm-desktop-characters--form t)
    (add-hook 'kill-buffer-hook
              (lambda () (run-at-time 0 nil #'lispm-desktop-characters--form-closed))
              nil t))
  (lispm-desktop-app-window-show 'character-form
                                 :name (buffer-local-value 'lispm-form--title form)
                                 :buffer form))

(defun lispm-desktop-characters-new ()
  "Make a character, in its form."
  (interactive)
  (lispm-desktop-characters--show-form (lispm-character-form)))

(defun lispm-desktop-characters-change (&optional character)
  "Change CHARACTER, by default the one point is on, in its form."
  (interactive)
  (lispm-desktop-characters--show-form
   (lispm-character-form (or character (lispm-desktop-characters--at-point)))))

(defun lispm-desktop-characters-talk-here ()
  "Talk to the character point is on."
  (interactive)
  (lispm-desktop-characters-talk (lispm-desktop-characters--at-point)))

(defun lispm-desktop-characters-delete (&optional character)
  "Delete CHARACTER, by default the one point is on, once you agree."
  (interactive)
  (let ((character (or character (lispm-desktop-characters--at-point))))
    (when (yes-or-no-p (lispm-desktop-tr "Delete %s, with its memory and its past?"
                                         (lispm-character-title* character)))
      (lispm-delete-character character))))

;;;; Setting up

(defun lispm-desktop-characters--define-skins ()
  "Say what the Characters window and each character's window are like."
  (lispm-desktop-app-ui-define
   'lispm-desktop-characters-mode
   :title (lambda () (lispm-desktop-tr "Characters"))
   :list t
   :buttons `((,(lispm-desktop-tr-noop "New…") lispm-desktop-characters-new)
              (,(lispm-desktop-tr-noop "Talk") lispm-desktop-characters-talk-here)
              (,(lispm-desktop-tr-noop "Change…") lispm-desktop-characters-change)
              (,(lispm-desktop-tr-noop "Delete…") lispm-desktop-characters-delete))
   :keys `(("n" . ,(lispm-desktop-tr-noop "Make a character"))
           ("Return" . ,(lispm-desktop-tr-noop "Talk to the character"))
           ("e" . ,(lispm-desktop-tr-noop "Change the character"))
           ("d" . ,(lispm-desktop-tr-noop "Delete the character"))
           ("g" . ,(lispm-desktop-tr-noop "Draw the list again")))
   :empty `(,(lambda () (null (lispm-desktop-characters--people)))
            ,(lispm-desktop-tr-noop "No characters yet.  A character is someone: a name, \
a soul, a picture, a memory and a past of its own, whom you and every agent reach by name, as @Ada.")
            ((,(lispm-desktop-tr-noop "New Character…") lispm-desktop-characters-new))))
  ;; A character's window shows its conversation, wrapped at words as
  ;; LispM's chat wraps it, not cut off as an application's lines are.
  (lispm-desktop-app-ui-define 'lispm-chat-mode :setup #'lispm-desktop-characters--wrap))

(defun lispm-desktop-characters--wrap ()
  "Wrap the lines of the conversation in this buffer at words, as LispM's
chat does, where a character's window shows it."
  (setq-local truncate-lines nil)
  (visual-line-mode 1))

(defvar lispm-form--title)

(defun lispm-desktop-setup-characters ()
  "Give LispM's characters their applications, if LispM has characters."
  (when (lispm-desktop-characters-available-p)
    (lispm-desktop-app-window-define 'characters
                                     :name (lispm-desktop-tr-noop "Characters")
                                     :size '(560 . 420)
                                     :documents 'keep
                                     :close #'lispm-desktop-app-window-close-box)
    (lispm-desktop-app-window-define 'character-form
                                     :app 'characters
                                     :name (lispm-desktop-tr-noop "Character")
                                     :size '(560 . 720)
                                     :documents 'keep
                                     :close #'lispm-desktop-app-window-close-box)
    (lispm-desktop-define-app 'characters
                              :name (lispm-desktop-tr-noop "Characters")
                              :icon 'characters
                              :folder 'characters-32
                              :description (lispm-desktop-tr-noop "Make characters: someone, with a \
soul, a picture, a memory and a past of their own, whom you and every agent reach by name.")
                              :standalone t
                              :menus (lambda () (lispm-desktop-app-ui-menus 'characters))
                              :activate #'lispm-desktop-characters-open
                              :visible (lambda () (lispm-desktop-app-window-open-p 'characters))
                              :listed-while-open t)
    (with-eval-after-load 'lispm-desktop-app-ui
      (lispm-desktop-characters--define-skins))
    (add-hook 'lispm-character-change-functions #'lispm-desktop-characters--changed)
    (add-hook 'lispm-desktop-apps-changed-hook #'lispm-desktop-characters--sync-apps)
    (dolist (character (lispm-desktop-characters--people))
      (lispm-desktop-characters--define-app character))
    (lispm-desktop-characters--sync-apps)))

(provide 'lispm-desktop-characters)
;;; lispm-desktop-characters.el ends here
