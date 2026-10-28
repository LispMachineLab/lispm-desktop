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
(require 'lispm-desktop-form)
(require 'svg)

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
(declare-function lispm-show "lispm" (agent))
(declare-function lispm-character-model "lispm-core" (character))
(declare-function lispm-character-backend "lispm-core" (character))
(declare-function lispm-character-endpoint "lispm-core" (character))
(declare-function lispm-character-conversations "lispm-characters" (character))
(declare-function lispm-character-days "lispm-characters" (character &optional count))
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
  "The characters LispM has, one a row, and who the one point is on is.
\\<lispm-desktop-characters-mode-map>\\[lispm-desktop-characters-talk-here] talks to \
it, or opens the conversation point is on, \\[lispm-desktop-characters-new] makes one,
\\[lispm-desktop-characters-change] changes it, \\[lispm-desktop-characters-delete] \
deletes it.

\\{lispm-desktop-characters-mode-map}"
  (setq-local revert-buffer-function (lambda (&rest _) (lispm-desktop-characters--draw))
              truncate-lines t)
  (add-hook 'post-command-hook #'lispm-desktop-characters--follow nil t)
  (add-hook 'window-size-change-functions #'lispm-desktop-characters--resized nil t))

(defvar-local lispm-desktop-characters--selected nil
  "The name of the character whose information the list shows.")

(defun lispm-desktop-characters--buffer ()
  "Return the buffer listing the characters, drawn."
  (with-current-buffer (get-buffer-create lispm-desktop-characters-buffer-name)
    (unless (derived-mode-p 'lispm-desktop-characters-mode)
      (lispm-desktop-characters-mode))
    (lispm-desktop-characters--draw)
    (current-buffer)))

(defun lispm-desktop-characters--icon (character height)
  "CHARACTER's picture, or a person's, HEIGHT pixels tall, as a string."
  (if (or (lispm-desktop-tty-p) (not (display-images-p)))
      (lispm-desktop-platinum-icon 'character)
    (propertize " " 'display (create-image (lispm-desktop-platinum-icon-file
                                            (cdr (lispm-desktop-characters--icons character)))
                                           nil nil :height height :ascent 'center))))

(defun lispm-desktop-characters--width ()
  "Pixels the list has for its rows: its window's, or what a window of the
Characters has, before one shows it."
  (if-let* ((window (get-buffer-window (current-buffer) t)))
      (window-body-width window t)
    (- (or (car (plist-get (alist-get 'characters lispm-desktop-app-window-kinds) :size)) 600) 24)))

(defun lispm-desktop-characters--draw ()
  "Draw the list of characters in this buffer, point on the one it was on,
and below it, who the one point is on is."
  (let* ((name (or (get-text-property (point) 'lispm-desktop-characters-name)
                   lispm-desktop-characters--selected))
         (people (lispm-desktop-characters--people))
         (selected (or (seq-find (lambda (character) (eq (lispm-character-name character) name)) people)
                       (car people)))
         (on-row (get-text-property (point) 'lispm-desktop-characters-name))
         (inhibit-read-only t))
    (erase-buffer)
    (setq lispm-desktop-characters--selected (and selected (lispm-character-name selected)))
    (dolist (character people)
      (let ((start (point))
            (indent (lispm-desktop-px 12)))
        (insert (propertize " " 'display `(space :width (,indent)))
                (lispm-desktop-characters--icon character 32)
                (propertize " " 'display `(space :width (,indent)))
                (propertize (lispm-character-title* character) 'face 'lispm-ui-title))
        (when-let* ((description (lispm-character-description character)))
          (insert (propertize " " 'display `(space :align-to (,(lispm-desktop-px 160))))
                  (propertize description 'face 'shadow)))
        (insert "\n")
        (put-text-property start (point) 'lispm-desktop-characters-name
                           (lispm-character-name character))))
    (when selected
      (insert "\n")
      (lispm-desktop-characters--card selected))
    (goto-char (point-min))
    (when-let* ((at (and (or on-row (not (get-text-property (point) 'lispm-desktop-characters-name)))
                         name
                         (text-property-any (point-min) (point-max)
                                            'lispm-desktop-characters-name name))))
      (goto-char at))))

(defun lispm-desktop-characters--card-row (insert &optional thin)
  "Insert a row of the card, what INSERT, a function, inserts, between its
group box's sides; THIN makes it a gap of a few pixels."
  (let* ((side (lispm-ui-form-group 'side))
         (margin (lispm-desktop-px 12))
         (face (and thin '(:height 40))))
    (insert (propertize " " 'display `(space :align-to (,margin)) 'face face) side
            (propertize " " 'display `(space :width (,(lispm-desktop-px 10))) 'face face))
    (funcall insert)
    (insert (propertize " " 'display `(space :align-to (- right (,(+ margin 2)))) 'face face) side
            (if thin (propertize "\n" 'face face) "\n"))))

(defun lispm-desktop-characters--thinks (character)
  "What CHARACTER thinks with, in words."
  (concat (cond ((lispm-character-endpoint character)
                 (lispm-desktop-tr "Its own endpoint, %s" (plist-get (lispm-character-endpoint character) :url)))
                ((lispm-character-backend character)
                 (lispm-desktop-tr "The backend %s" (lispm-character-backend character)))
                (t (lispm-desktop-tr "LispM's model")))
          (if-let* ((model (lispm-character-model character))) (concat " · " model) "")))

(defun lispm-desktop-characters--conversation-line (conversation)
  "What the list says of CONVERSATION, as `lispm-character-conversations' gives it."
  (let ((agent (plist-get conversation :live)))
    (concat (if (plist-get conversation :home)
                (lispm-desktop-tr "At home")
              (lispm-desktop-tr "In %s" (abbreviate-file-name (plist-get conversation :root))))
            (propertize " · " 'face 'shadow)
            (propertize (cond ((and agent (not (eq (plist-get conversation :state) 'idle)))
                               (lispm-desktop-tr "working now"))
                              (agent (lispm-desktop-tr "open"))
                              ((plist-get conversation :time)
                               (lispm-desktop-format-time "%a %e %b %H:%M" (plist-get conversation :time)))
                              (t ""))
                        'face 'shadow))))

(defun lispm-desktop-characters--card (character)
  "Insert who CHARACTER is, in a group box titled with its name: its picture,
what it is told to be, what it thinks with, its conversations, its days."
  (let* ((margin (lispm-desktop-px 12))
         (width (- (lispm-desktop-characters--width) (* 2 margin)))
         (heading (lambda (text)
                    (lispm-desktop-characters--card-row
                     (lambda () (insert (propertize text 'face 'lispm-ui-title))))))
         (line (lambda (text &optional object)
                 (lispm-desktop-characters--card-row
                  (lambda ()
                    (insert (propertize " " 'display `(space :width (,(lispm-desktop-px 12))))
                            (if object
                                (propertize text 'lispm-desktop-characters-conversation object
                                            'mouse-face 'highlight
                                            'help-echo (lispm-desktop-tr "Return opens the conversation"))
                              text)))))))
    (insert (propertize " " 'display `(space :align-to (,margin)))
            (lispm-desktop-platinum-group-edge width (lispm-character-title* character))
            "\n")
    (lispm-desktop-characters--card-row #'ignore t)
    ;; Its picture in a well, beside who it is; on a text terminal, who it
    ;; is alone, a line each.
    (if (lispm-desktop-tty-p)
        (dolist (text (delq nil (list (propertize (lispm-character-title* character)
                                                  'face 'lispm-ui-title)
                                      (lispm-character-description character)
                                      (propertize (lispm-desktop-characters--thinks character)
                                                  'face 'shadow))))
          (lispm-desktop-characters--card-row (lambda () (insert text))))
    (lispm-desktop-characters--card-row
     (lambda ()
       (insert (if (lispm-desktop-tty-p)
                   ""
                 (propertize " " 'display (lispm-ui-form--well (lispm-character-avatar character))))
               (propertize " " 'display `(space :width (,(lispm-desktop-px 12)))))
       (let ((lines (delq nil (list (propertize (lispm-character-title* character)
                                                'face '(lispm-ui-title (:height 1.3)))
                                    (lispm-character-description character)
                                    (propertize (lispm-desktop-characters--thinks character)
                                                'face 'shadow)))))
         ;; Beside the picture, one line under another, as tall as it.
         (insert (propertize " " 'display
                             `(image :type svg :ascent center
                                     :data ,(lispm-desktop-characters--lines-svg lines width))))))))
    (when (fboundp 'lispm-character-conversations)
      (when-let* ((conversations (lispm-character-conversations character)))
        (lispm-desktop-characters--card-row #'ignore t)
        (funcall heading (lispm-desktop-tr "Conversations"))
        (dolist (conversation (seq-take conversations 6))
          (funcall line (lispm-desktop-characters--conversation-line conversation) conversation))))
    (when (fboundp 'lispm-character-days)
      (when-let* ((days (lispm-character-days character 4)))
        (lispm-desktop-characters--card-row #'ignore t)
        (funcall heading (lispm-desktop-tr "Lately"))
        (dolist (day days)
          (funcall line (lispm-desktop-characters--day-line day (- width (lispm-desktop-px 48)))))))
    (lispm-desktop-characters--card-row #'ignore t)
    (insert (propertize " " 'display `(space :align-to (,margin)))
            (lispm-desktop-platinum-group-edge width nil t)
            "\n")))

(defun lispm-desktop-characters--day-line (day width)
  "What the list says of DAY, a line of a character's diary, in WIDTH pixels:
when, and who asked what."
  (let* ((text (car (split-string (plist-get day :text) "\n")))
         (what (if (string-match "\\`In .*?, \\(.*?\\) asked: \\(.*\\)" text)
                   (let ((who (match-string 1 text))
                         (asked (match-string 2 text)))
                     (concat (if (equal who "the user") (lispm-desktop-tr "You") who) ": " asked))
                 text))
         (when (concat (unless (equal (plist-get day :day) (format-time-string "%Y-%m-%d"))
                         (concat (plist-get day :day) " "))
                       (car (split-string (plist-get day :title)))
                       "  ")))
    (concat (propertize when 'face 'shadow)
            (lispm-desktop-characters--fit what (- width (string-pixel-width when))))))

(defun lispm-desktop-characters--fit (text width)
  "TEXT, cut short with an ellipsis to fit in WIDTH pixels, as the list draws it."
  (if (<= (string-pixel-width text) width)
      text
    (let ((end (length text)))
      (while (and (> end 0) (> (string-pixel-width (concat (substring text 0 end) "…")) width))
        (setq end (1- end)))
      (concat (substring text 0 end) "…"))))

(defun lispm-desktop-characters--lines-svg (lines width)
  "SVG text of LINES, strings, one under another, in their faces' fonts."
  (let* ((palette lispm-ui-current-palette)
         (height (* (length lines) 20))
         (svg (svg-create (max 1 (- width 120)) height)))
    (cl-loop for text in lines
             for y from 15 by 20
             do (let* ((face (get-text-property 0 'face text))
                       (font (lispm-ui-svg-font
                              (if (eq face 'shadow) 'lispm-ui-list 'lispm-ui-title))))
                  (lispm-ui-svg-label svg (substring-no-properties text) 0 y
                                      (if (eq face 'shadow)
                                          (lispm-ui-grey palette #x66)
                                        (plist-get palette :text))
                                      (if (consp face)
                                          (plist-put (copy-sequence font) :size
                                                     (* 1.3 (plist-get font :size)))
                                        font)
                                      "start")))
    (with-temp-buffer (svg-print svg) (buffer-string))))

(defun lispm-desktop-characters--follow ()
  "Show who the character of the row point is on is, once point moved to
another's.  Run from the list's `post-command-hook'."
  (when-let* ((name (get-text-property (point) 'lispm-desktop-characters-name))
              ((not (eq name lispm-desktop-characters--selected))))
    (lispm-desktop-characters--draw)))

(defun lispm-desktop-characters--resized (window)
  "Draw the list in WINDOW again for its width, once its width changed."
  (with-current-buffer (window-buffer window)
    (lispm-desktop-characters--draw)))

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
  "The character of the row point is on, or whose information it is in.
An error if there is none."
  (or (when-let* ((name (or (get-text-property (point) 'lispm-desktop-characters-name)
                            lispm-desktop-characters--selected)))
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
                      (and (eq (buffer-local-value 'major-mode buffer) 'lispm-ui-form-mode)
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
  (pcase-let* ((`(,width . ,height) (lispm-desktop-characters--form-size form))
               (frame (lispm-desktop-app-window-show
                       'character-form
                       :name (lispm-ui-form-title form)
                       :buffer form
                       :size (cons width height))))
    ;; The window opens where the last form's was, but each form is a size
    ;; of its own: an application's form is shorter than a character's.
    (unless (and (= width (frame-text-width frame)) (= height (frame-text-height frame)))
      (set-frame-size frame width height t)
      (lispm-desktop-platinum-frame-update frame)
      (lispm-desktop-apps-keep-on-desktop frame))
    frame))

(defun lispm-desktop-characters--form-size (form)
  "The size of the window of FORM, a form's buffer: as wide as its group
boxes, as tall as all of it, as far as the screen allows."
  (let* ((outer (or (plist-get (lispm-ui-form-geometry form) :outer) 480))
         (height (cdr (buffer-text-pixel-size form)))
         (screen (lispm-desktop-apps-screen)))
    (cons (+ outer (lispm-desktop-px 40))
          (min (+ height (lispm-desktop-px 24))
               (- (frame-pixel-height screen) (lispm-desktop-px 120))))))

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
  "Talk to the character point is on, or open the conversation point is on."
  (interactive)
  (if-let* ((conversation (get-text-property (point) 'lispm-desktop-characters-conversation)))
      (if (plist-get conversation :home)
          (lispm-desktop-characters-talk lispm-desktop-characters--selected)
        (lispm-show (lispm-character-agent lispm-desktop-characters--selected
                                           (plist-get conversation :root))))
    (lispm-desktop-characters-talk (lispm-desktop-characters--at-point))))

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

(declare-function lispm-ui-form-title "lispm-ui-form" (&optional buffer))
(declare-function lispm-ui-form-geometry "lispm-ui-form" (&optional buffer))

(defun lispm-desktop-setup-characters ()
  "Give LispM's characters their applications, if LispM has characters."
  (when (lispm-desktop-characters-available-p)
    (lispm-desktop-app-window-define 'characters
                                     :name (lispm-desktop-tr-noop "Characters")
                                     :size '(600 . 620)
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
