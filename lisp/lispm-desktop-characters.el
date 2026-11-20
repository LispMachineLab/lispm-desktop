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
(declare-function lispm-character-soul "lispm-core" (character))
(declare-function lispm-character-appearance "lispm-core" (character))
(declare-function lispm-character-workflow "lispm-core" (character))
(declare-function lispm-character-embedding-model "lispm-core" (character))
(declare-function lispm-character-role "lispm-core" (character))
(declare-function lispm-character-effort "lispm-core" (character))
(declare-function lispm-character-file "lispm-core" (character))
(declare-function lispm-character-directory "lispm-core" (character))
(declare-function lispm-character-self "lispm-characters" (character))
(declare-function lispm-character-knowledge-files "lispm-characters" (character))
(declare-function lispm-character-rag-chunks "lispm-characters" (character))
(declare-function lispm-character-rag-add "lispm-characters" (character title content &optional keywords))
(declare-function lispm-character-generate-avatar "lispm-characters" (character &optional prompt callback))
(declare-function lispm-character-conversations "lispm-characters" (character))
(declare-function lispm-character-days "lispm-characters" (character &optional count))
(declare-function lispm-skills--find "lispm-skills" (name &optional agent))
(declare-function lispm-skills--index "lispm-skills" (skill))
(declare-function lispm-skills--text "lispm-skills" (file))
(declare-function lispm-memory--entries "lispm-memory" (buffer))
(declare-function lispm-memory--visit "lispm-memory" (file header &optional to-change))
(declare-function lispm-desktop-app-ui-define "lispm-desktop-app-ui" (key &rest properties))
(declare-function lispm-desktop-app-ui-menus "lispm-desktop-app-ui" (app))
(declare-function lispm-desktop-finder-folder-changed "lispm-desktop-finder" (key))
(declare-function lispm-desktop-remove-alias "lispm-desktop-surface" (app))
(declare-function lispm-desktop-store--installed-skills "lispm-desktop-store" (id))
(declare-function lispm-character-rag-enabled-p "lispm-characters" (character-or-agent))
(declare-function lispm-character-comfyui-enabled-p "lispm-characters" (character-or-agent))
(declare-function lispm-visit-character-file "lispm-characters" (character))
(declare-function lispm-agent-character "lispm-core" (agent))
(declare-function lispm-ui-menu "lispm-ui-menu" (items &optional x y align))
(declare-function lispm-desktop-popup-frame-xy "lispm-desktop-popup" (position))
(declare-function lispm-desktop-lispm-chat-buttons "lispm-desktop-lispm" ())
(declare-function lispm-desktop-lispm-chat-status "lispm-desktop-lispm" ())
(defvar lispm-character-change-functions)
(defvar lispm-desktop-store-installed)
(defvar lispm-chat-agent)
(defvar lispm-desktop-context-menu-function)

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

(defun lispm-desktop-characters--listed ()
  "The characters the Characters application lists: those the user made,
then the applications that are characters."
  (append (lispm-desktop-characters--people)
          (seq-filter #'lispm-character-skill (lispm-characters))))

;;;; Icons

(defun lispm-desktop-characters--svg-file-p (file)
  "Return non-nil when FILE is an SVG image file."
  (or (eq (image-supported-file-p file) 'svg)
      (string-suffix-p ".svg" (downcase file))))

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
      (let* ((type (or (and (lispm-desktop-characters--svg-file-p picture) 'svg)
                       (image-supported-file-p picture)
                       'png))
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
                          (pcase type ('jpeg "jpeg") ('svg "svg+xml") (_ type)) data)))))
    file))

(defun lispm-desktop-characters--icons (character)
  "Return CHARACTER's icons, (SMALL . LARGE): its picture, else a person's."
  (if-let* ((picture (lispm-character-avatar character))
            ((file-readable-p picture)))
      (cons picture (if (lispm-desktop-characters--svg-file-p picture)
                        picture
                      (lispm-desktop-characters--large-icon picture)))
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
      (unless character
        (lispm-desktop-app-window-close (lispm-desktop-characters-app name)))
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

(defun lispm-desktop-characters--app-skill (key props)
  "The name of the skill of the application KEY, of PROPS; nil if none."
  (or (plist-get props :skill)
      (and (plist-get props :store)
           (lispm-desktop-characters--store-skill key))))

(defun lispm-desktop-characters--sync-apps ()
  "Make each application with a skill a character of its kind, and those
uninstalled characters no more.
Run from `lispm-desktop-apps-changed-hook'."
  (when (fboundp 'lispm-define-app-character)
    (let (now)
      (pcase-dolist (`(,key . ,props) lispm-desktop-apps)
        (when-let* ((skill (lispm-desktop-characters--app-skill key props)))
          (push key now)
          (unless (memq key lispm-desktop-characters--apps)
            (lispm-define-app-character
             key :title (lispm-desktop-tr (plist-get props :name)) :skill skill
             :description (plist-get props :description)
             :avatar (ignore-errors (lispm-ui-icon-file
                                     (or (plist-get props :large-icon)
                                         (plist-get props :folder)
                                         (plist-get props :icon))))))))
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
  "c" #'lispm-desktop-characters-change
  "d" #'lispm-desktop-characters-delete
  "a" #'lispm-desktop-characters-add-knowledge
  "i" #'lispm-desktop-characters-generate-avatar
  "o" #'lispm-desktop-characters-open-file
  "v" #'lispm-desktop-characters-toggle-view
  "]" #'lispm-desktop-characters-next
  "[" #'lispm-desktop-characters-previous
  "M-n" #'lispm-desktop-characters-next
  "M-p" #'lispm-desktop-characters-previous
  "<right>" #'lispm-desktop-characters-right
  "<left>" #'lispm-desktop-characters-left
  "C-f" #'lispm-desktop-characters-right
  "C-b" #'lispm-desktop-characters-left
  "TAB" #'lispm-desktop-characters-next-tab
  "<backtab>" #'lispm-desktop-characters-previous-tab
  "1" #'lispm-desktop-characters-tab-all
  "2" #'lispm-desktop-characters-tab-soul
  "3" #'lispm-desktop-characters-tab-memory
  "4" #'lispm-desktop-characters-tab-knowledge
  "5" #'lispm-desktop-characters-tab-conversations
  "<down-mouse-1>" #'lispm-desktop-characters-mouse-select
  "<mouse-1>" #'lispm-desktop-characters-mouse-click
  "<double-down-mouse-1>" #'ignore
  "<double-mouse-1>" #'lispm-desktop-characters-mouse-open
  "<down-mouse-3>" #'lispm-desktop-characters-context-menu
  "<mouse-3>" #'ignore
  "g" #'lispm-desktop-characters-refresh
  "q" #'lispm-desktop-characters-close
  "M-w" #'lispm-desktop-characters-close)

(define-derived-mode lispm-desktop-characters-mode special-mode "Characters"
  "The characters LispM has, one a row, and who the one point is on is.
\\<lispm-desktop-characters-mode-map>\\[lispm-desktop-characters-talk-here] talks to \
it, or opens the conversation point is on, \\[lispm-desktop-characters-new] makes one,
\\[lispm-desktop-characters-change] changes it, \\[lispm-desktop-characters-delete] \
deletes it.

\\{lispm-desktop-characters-mode-map}"
  (setq-local revert-buffer-function (lambda (&rest _) (lispm-desktop-characters--draw))
              lispm-desktop-context-menu-function #'lispm-desktop-characters--keyboard-menu
              truncate-lines t)
  (add-hook 'post-command-hook #'lispm-desktop-characters--follow nil t)
  (add-hook 'window-size-change-functions #'lispm-desktop-characters--resized nil t))

(defvar-local lispm-desktop-characters--selected nil
  "The name of the character whose information the list shows.")

(defvar-local lispm-desktop-characters--view 'list
  "How the characters are laid out above the card: `list' or `grid'.")

(defvar-local lispm-desktop-characters--tab 'all
  "Which section of the card is shown.
One of `all', `soul', `memory', `knowledge' or `conversations'.")

(defconst lispm-desktop-characters--tabs '(all soul memory knowledge conversations)
  "The tabs of a character card, in order.")

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
      (lispm-ui-icon 'character)
    (propertize " " 'display (create-image (lispm-ui-icon-file
                                            (cdr (lispm-desktop-characters--icons character)))
                                           nil nil :height height :ascent 'center))))

(defun lispm-desktop-characters--width ()
  "Pixels the list has for its rows: its window's, or what a window of the
Characters has, before one shows it."
  (if-let* ((window (get-buffer-window (current-buffer) t)))
      (window-body-width window t)
    (- (or (car (plist-get (alist-get 'characters lispm-desktop-app-window-kinds) :size)) 600) 24)))

(defun lispm-desktop-characters--badge (character)
  "Short identity badge for CHARACTER: @name and its role or app skill."
  (concat "@" (symbol-name (lispm-character-name character))
          (cond ((lispm-character-skill character)
                 (concat " · " (lispm-character-skill character)))
                ((lispm-character-role character)
                 (concat " · " (symbol-name (lispm-character-role character))))
                (t ""))))

(defun lispm-desktop-characters--grid-tile (character selected-p)
  "Insert a portrait tile for CHARACTER in the SillyTavern-style avatar grid."
  (let* ((name (lispm-character-name character))
         (title (lispm-character-title* character))
         (tag (if (lispm-character-skill character)
                  (concat "@" (symbol-name name) " · app")
                (concat "@" (symbol-name name))))
         (start (point)))
    (if (or (lispm-desktop-tty-p) (not (display-images-p)))
        (let ((cell (format (if selected-p "[*%s %s]" "[ %s %s]")
                            (lispm-ui-icon 'character) title)))
          (insert (propertize cell 'face (if selected-p 'highlight 'lispm-ui-title))))
      (let* ((palette (or lispm-ui-current-palette
                          '(:chrome "#DDDDDD" :content "#FFFFFF" :outline "#000000"
                                    :text "#000000" :highlight "#000080" :highlight-text "#FFFFFF")))
             (w 128) (h 78)
             (svg (svg-create w h))
             (bg (if selected-p
                     (or (plist-get palette :content) "#FFFFFF")
                   (or (plist-get palette :chrome) "#DDDDDD")))
             (border (if selected-p
                         (or (plist-get palette :highlight) "#000080")
                       (or (plist-get palette :outline) "#000000")))
             (icon-file (ignore-errors
                          (lispm-ui-icon-file (cdr (lispm-desktop-characters--icons character)))))
             (font (lispm-ui-svg-font 'lispm-ui-title))
             (subfont (lispm-ui-svg-font 'lispm-ui-list)))
        (lispm-ui-svg-fill svg 0 0 w h bg)
        (svg-rectangle svg 0.5 0.5 (- w 1) (- h 1)
                       :fill "none" :stroke border :stroke-width (if selected-p 2 1))
        (when (and icon-file (file-readable-p icon-file))
          (lispm-ui-svg-emacs-image svg (create-image icon-file) (/ (- w 32) 2) 6 32 32))
        (lispm-ui-svg-label svg (lispm-desktop-characters--fit title (- w 12))
                            (/ w 2) 54 (or (plist-get palette :text) "#000000") font "middle")
        (lispm-ui-svg-label svg (lispm-desktop-characters--fit tag (- w 12))
                            (/ w 2) 70 (lispm-ui-grey palette #x66) subfont "middle")
        (insert (propertize title 'display `(image :type svg :ascent center
                                                   :data ,(with-temp-buffer
                                                            (svg-print svg)
                                                            (buffer-string)))))))
    (put-text-property start (point) 'lispm-desktop-characters-name name)
    (put-text-property start (point) 'mouse-face 'highlight)))

(defun lispm-desktop-characters--draw-grid (characters selected)
  "Draw CHARACTERS as a multi-column portrait grid, SELECTED highlighted."
  (let* ((width (lispm-desktop-characters--width))
         (cols (max 1 (/ (max 128 (- width 24)) 136)))
         (indent (lispm-desktop-px 12))
         (gap (lispm-desktop-px 8))
         (rows (seq-partition characters cols)))
    (dolist (row rows)
      (insert (propertize " " 'display `(space :width (,indent))))
      (cl-loop for character in row
               for rest on row
               do (lispm-desktop-characters--grid-tile character (eq character selected))
               when (cdr rest)
               do (insert (propertize " " 'display `(space :width (,gap)))))
      (insert "\n"))))

(defun lispm-desktop-characters--draw ()
  "Draw the list or grid of characters in this buffer, point on the one it
was on, and below it, who the one point is on is."
  (let* ((name (or (get-text-property (point) 'lispm-desktop-characters-name)
                   lispm-desktop-characters--selected))
         (characters (lispm-desktop-characters--listed))
         (selected (or (seq-find (lambda (character) (eq (lispm-character-name character) name)) characters)
                       (car characters)))
         (on-row (get-text-property (point) 'lispm-desktop-characters-name))
         (inhibit-read-only t))
    (erase-buffer)
    (setq lispm-desktop-characters--selected (and selected (lispm-character-name selected)))
    (if (eq lispm-desktop-characters--view 'grid)
        (lispm-desktop-characters--draw-grid characters selected)
      (dolist (character characters)
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
                             (lispm-character-name character)))))
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
  (let* ((side (or (lispm-ui-form-group 'side)
                   ;; No palette yet, as on a terminal without a theme's
                   ;; colours: the group box's side as text.
                   (lispm-ui-group-side 1)))
         (margin (lispm-desktop-px 12))
         (face (and thin '(:height 40))))
    (insert (propertize " " 'display `(space :align-to (,margin)) 'face face) side
            (propertize " " 'display `(space :width (,(lispm-desktop-px 10))) 'face face))
    (funcall insert)
    (insert (propertize " " 'display `(space :align-to (- right (,(+ margin 2)))) 'face face) side
            (if thin (propertize "\n" 'face face) "\n"))))

(defun lispm-desktop-characters--thinks (character)
  "What CHARACTER thinks with, and its role or skill, in words."
  (concat (cond ((lispm-character-skill character)
                 (concat (lispm-desktop-tr "Skill") ": " (lispm-character-skill character) " · "))
                ((lispm-character-role character)
                 (concat (lispm-desktop-tr "Role") ": "
                         (symbol-name (lispm-character-role character)) " · "))
                (t ""))
          (cond ((lispm-character-endpoint character)
                 (lispm-desktop-tr "Its own endpoint, %s" (plist-get (lispm-character-endpoint character) :url)))
                ((lispm-character-backend character)
                 (lispm-desktop-tr "The backend %s" (lispm-character-backend character)))
                (t (lispm-desktop-tr "LispM's model")))
          (if-let* ((model (lispm-character-model character))) (concat " · " model) "")
          (if-let* ((effort (lispm-character-effort character))) (concat " · " effort) "")
          (if (and (fboundp 'lispm-character-rag-enabled-p)
                   (lispm-character-rag-enabled-p character))
              (concat " · " (lispm-desktop-tr "Knowledge"))
            "")
          (if (and (fboundp 'lispm-character-comfyui-enabled-p)
                   (lispm-character-comfyui-enabled-p character))
              " · ComfyUI"
            "")))

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

(defun lispm-desktop-characters--wrap-lines (text width &optional limit)
  "Split and wrap TEXT into lines fitting WIDTH pixels, at most LIMIT lines."
  (when (and text (not (string-blank-p text)))
    (let ((lines (mapcan (lambda (paragraph)
                           (let ((trimmed (string-trim paragraph)))
                             (if (string-empty-p trimmed)
                                 nil
                               (or (ignore-errors
                                     (lispm-ui-text-wrap trimmed (lambda (s) (<= (string-pixel-width s) width))))
                                   (list (lispm-desktop-characters--fit trimmed width))))))
                         (split-string (string-trim text) "\n+"))))
      (if limit (seq-take lines limit) lines))))

(defun lispm-desktop-characters--skill-info (character)
  "Return (:file FILE :summary TEXT :tools LIST) for CHARACTER's skill, or nil."
  (when-let* ((name (lispm-character-skill character))
              (skill (and (fboundp 'lispm-skills--find) (ignore-errors (lispm-skills--find name))))
              (file (plist-get skill :file)))
    (let* ((raw (and (fboundp 'lispm-skills--text) (ignore-errors (lispm-skills--text file))))
           (body (and raw (string-trim (replace-regexp-in-string
                                        "\\`\\(?:#\\+.*\n\\)+" ""
                                        (car (split-string raw "^\\* "))))))
           (tools (and (fboundp 'lispm-skills--index)
                       (ignore-errors (mapcar #'car (plist-get (lispm-skills--index skill) :tools))))))
      (list :file file
            :summary (or (and body (not (string-empty-p body)) body)
                         (plist-get skill :description))
            :tools tools))))

(defun lispm-desktop-characters--memories (character)
  "Personal memory entries of CHARACTER from its memory.org, as plists."
  (when (and (fboundp 'lispm-character-directory) (fboundp 'lispm-memory--entries))
    (let ((file (expand-file-name "memory.org"
                                  (lispm-character-directory (lispm-character-name character)))))
      (when (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (delay-mode-hooks (org-mode))
          (mapcar (lambda (entry) (plist-put entry :file file))
                  (ignore-errors (lispm-memory--entries (current-buffer)))))))))

(defun lispm-desktop-characters--tab-keymap (tab)
  "Keymap selecting TAB on click or Return."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] (lambda () (interactive) (lispm-desktop-characters-select-tab tab)))
    (define-key map (kbd "RET") (lambda () (interactive) (lispm-desktop-characters-select-tab tab)))
    map))

(defun lispm-desktop-characters--card-tabs (character width)
  "Insert the tab strip of CHARACTER's card across WIDTH pixels."
  (let* ((current (or lispm-desktop-characters--tab 'all))
         (tabs `((,(lispm-desktop-tr "All") ,(eq current 'all)
                  keymap ,(lispm-desktop-characters--tab-keymap 'all)
                  mouse-face highlight
                  help-echo ,(lispm-desktop-tr "Next card tab"))
                 (,(if (lispm-character-skill character)
                       (lispm-desktop-tr "Skill")
                     (lispm-desktop-tr "Soul"))
                  ,(eq current 'soul)
                  keymap ,(lispm-desktop-characters--tab-keymap 'soul)
                  mouse-face highlight
                  help-echo ,(lispm-desktop-tr "Next card tab"))
                 (,(lispm-desktop-tr-in "agents' memory" "Memory") ,(eq current 'memory)
                  keymap ,(lispm-desktop-characters--tab-keymap 'memory)
                  mouse-face highlight
                  help-echo ,(lispm-desktop-tr "Next card tab"))
                 (,(lispm-desktop-tr "Knowledge") ,(eq current 'knowledge)
                  keymap ,(lispm-desktop-characters--tab-keymap 'knowledge)
                  mouse-face highlight
                  help-echo ,(lispm-desktop-tr "Next card tab"))
                 (,(lispm-desktop-tr "Conversations") ,(eq current 'conversations)
                  keymap ,(lispm-desktop-characters--tab-keymap 'conversations)
                  mouse-face highlight
                  help-echo ,(lispm-desktop-tr "Next card tab"))))
         (tab-width (if (lispm-desktop-tty-p)
                        (max 24 (/ (max 240 (- width 24)) (frame-char-width)))
                      (max 120 (- width 24)))))
    (dolist (row (split-string (lispm-ui-tabs tabs tab-width) "\n"))
      (lispm-desktop-characters--card-row (lambda () (insert row))))))

(defun lispm-desktop-characters--card-soul-section (character tab text-width heading line)
  "Render CHARACTER's Soul or Skill section for TAB using HEADING and LINE."
  (when (memq tab '(all soul))
    (let ((limit (if (eq tab 'all) 4 12)))
      (if (lispm-character-skill character)
          (let* ((info (lispm-desktop-characters--skill-info character))
                 (file (plist-get info :file))
                 (summary (or (plist-get info :summary) (lispm-character-description character)))
                 (tools (plist-get info :tools)))
            (when (or summary tools (eq tab 'soul))
              (lispm-desktop-characters--card-row #'ignore t)
              (funcall heading (lispm-desktop-tr "Skill"))
              (dolist (row (lispm-desktop-characters--wrap-lines summary text-width limit))
                (funcall line row nil file))
              (when tools
                (funcall line (propertize (lispm-desktop-tr "Tools: %s" (string-join tools ", "))
                                          'face 'shadow)
                         nil file))))
        (when-let* ((soul (lispm-character-soul character))
                    (rows (lispm-desktop-characters--wrap-lines soul text-width limit)))
          (lispm-desktop-characters--card-row #'ignore t)
          (funcall heading (lispm-desktop-tr "Soul"))
          (dolist (row rows)
            (funcall line row nil (lispm-character-file character))))
        (when-let* (((fboundp 'lispm-character-appearance))
                    (appearance (lispm-character-appearance character)))
          (funcall line
                   (propertize (lispm-desktop-tr "Appearance: %s"
                                                 (concat appearance
                                                         (if-let* ((wf (and (fboundp 'lispm-character-workflow)
                                                                            (lispm-character-workflow character))))
                                                             (concat " (" wf ")")
                                                           "")))
                               'face 'shadow)
                   nil (lispm-character-file character)))))))

(defun lispm-desktop-characters--card-memory-section (character tab text-width heading line)
  "Render CHARACTER's Self and Memory sections for TAB using HEADING and LINE."
  (when (memq tab '(all soul memory))
    (when-let* (((fboundp 'lispm-character-self))
                (self (lispm-character-self character))
                (self-file (and (fboundp 'lispm-character-directory)
                                (expand-file-name "self.org"
                                                  (lispm-character-directory
                                                   (lispm-character-name character)))))
                (rows (lispm-desktop-characters--wrap-lines
                       self text-width (if (eq tab 'all) 3 10))))
      (lispm-desktop-characters--card-row #'ignore t)
      (funcall heading (lispm-desktop-tr "In its own words"))
      (dolist (row rows)
        (funcall line row nil self-file))))
  (when (memq tab '(all memory))
    (let ((memories (lispm-desktop-characters--memories character)))
      (when (or memories (eq tab 'memory))
        (lispm-desktop-characters--card-row #'ignore t)
        (funcall heading (lispm-desktop-tr-in "agents' memory" "Memory"))
        (if (null memories)
            (funcall line (propertize (lispm-desktop-tr "No memories yet.") 'face 'shadow))
          (dolist (entry (seq-take memories (if (eq tab 'all) 4 10)))
            (let* ((tags (plist-get entry :tags))
                   (tag-str (if tags (concat "  :" (string-join tags ":") ":") ""))
                   (target (cons (plist-get entry :file) (plist-get entry :beg))))
              (funcall line
                       (concat (lispm-desktop-characters--fit
                                (concat "• " (plist-get entry :title))
                                (- text-width (string-pixel-width tag-str)))
                               (propertize tag-str 'face 'shadow))
                       nil target)
              (when (eq tab 'memory)
                (dolist (row (lispm-desktop-characters--wrap-lines
                              (plist-get entry :body) (- text-width 16) 2))
                  (funcall line (propertize (concat "  " row) 'face 'shadow) nil target))))))))))

(defun lispm-desktop-characters--card-knowledge-section (character tab text-width heading line)
  "Render CHARACTER's RAG Knowledge section for TAB using HEADING and LINE."
  (when (memq tab '(all knowledge))
    (let ((files (and (fboundp 'lispm-character-knowledge-files)
                      (ignore-errors (lispm-character-knowledge-files character))))
          (chunks (and (fboundp 'lispm-character-rag-chunks)
                       (ignore-errors (lispm-character-rag-chunks character)))))
      (when (or files (eq tab 'knowledge))
        (lispm-desktop-characters--card-row #'ignore t)
        (funcall heading (lispm-desktop-tr "Knowledge"))
        (if (null files)
            (funcall line (propertize (lispm-desktop-tr "No knowledge files yet.") 'face 'shadow))
          (dolist (chunk (seq-take chunks (if (eq tab 'all) 4 10)))
            (let* ((kws (plist-get chunk :keywords))
                   (kw-str (if kws (concat "  :" (string-join kws ":") ":") ""))
                   (file (plist-get chunk :file)))
              (funcall line
                       (concat (lispm-desktop-characters--fit
                                (concat "• " (file-name-nondirectory file) " · " (plist-get chunk :title))
                                (- text-width (string-pixel-width kw-str)))
                               (propertize kw-str 'face 'shadow))
                       nil file)
              (when (eq tab 'knowledge)
                (dolist (row (lispm-desktop-characters--wrap-lines
                              (plist-get chunk :text) (- text-width 16) 2))
                  (funcall line (propertize (concat "  " row) 'face 'shadow) nil file))))))))))

(defun lispm-desktop-characters--card-conversations-section (character tab text-width heading line)
  "Render CHARACTER's Conversations and Lately sections for TAB."
  (when (memq tab '(all conversations))
    (when (fboundp 'lispm-character-conversations)
      (let ((conversations (lispm-character-conversations character)))
        (when (or conversations (eq tab 'conversations))
          (lispm-desktop-characters--card-row #'ignore t)
          (funcall heading (lispm-desktop-tr "Conversations"))
          (if (null conversations)
              (funcall line (propertize (lispm-desktop-tr "No conversations yet.") 'face 'shadow))
            (dolist (conversation (seq-take conversations (if (eq tab 'all) 6 12)))
              (funcall line (lispm-desktop-characters--conversation-line conversation) conversation))))))
    (when (fboundp 'lispm-character-days)
      (when-let* ((days (lispm-character-days character (if (eq tab 'all) 4 12))))
        (lispm-desktop-characters--card-row #'ignore t)
        (funcall heading (lispm-desktop-tr "Lately"))
        (dolist (day days)
          (let ((episode-file (and (fboundp 'lispm-character-directory)
                                   (plist-get day :day)
                                   (expand-file-name
                                    (format "episodes/%s.org" (plist-get day :day))
                                    (lispm-character-directory (lispm-character-name character))))))
            (funcall line
                     (lispm-desktop-characters--day-line day text-width)
                     nil (and episode-file (file-readable-p episode-file) episode-file))))))))

(defun lispm-desktop-characters--card (character)
  "Insert who CHARACTER is, in a group box titled with its name: its picture,
what it is told to be, what it thinks with, its soul or skill, its memory,
its conversations and its days."
  (let* ((margin (lispm-desktop-px 12))
         (width (- (lispm-desktop-characters--width) (* 2 margin)))
         (text-width (max 80 (- width (lispm-desktop-px 48))))
         (tab (or lispm-desktop-characters--tab 'all))
         (title-line (concat (lispm-character-title* character)
                             "  " (lispm-desktop-characters--badge character)))
         (heading (lambda (text)
                    (lispm-desktop-characters--card-row
                     (lambda () (insert (propertize text 'face 'lispm-ui-title))))))
         (line (lambda (text &optional object file)
                 (lispm-desktop-characters--card-row
                  (lambda ()
                    (insert (propertize " " 'display `(space :width (,(lispm-desktop-px 12))))
                            (cond (object
                                   (propertize text 'lispm-desktop-characters-conversation object
                                               'mouse-face 'highlight
                                               'help-echo (lispm-desktop-tr "Return opens the conversation")))
                                  (file
                                   (propertize text 'lispm-desktop-characters-file file
                                               'mouse-face 'highlight
                                               'help-echo (lispm-desktop-tr "Return opens the file")))
                                  (t text))))))))
    (insert (propertize " " 'display `(space :align-to (,margin)))
            (lispm-ui-group-edge width (lispm-character-title* character))
            "\n")
    (lispm-desktop-characters--card-row #'ignore t)
    ;; Its picture in a well, beside who it is; on a text terminal, who it
    ;; is alone, a line each.
    (if (lispm-desktop-tty-p)
        (dolist (text (delq nil (list (propertize title-line 'face 'lispm-ui-title)
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
         (let ((lines (delq nil (list (propertize title-line 'face '(lispm-ui-title (:height 1.3)))
                                      (lispm-character-description character)
                                      (propertize (lispm-desktop-characters--thinks character)
                                                  'face 'shadow)))))
           ;; Beside the picture, one line under another, as tall as it.
           (insert (propertize " " 'display
                               `(image :type svg :ascent center
                                       :data ,(lispm-desktop-characters--lines-svg lines width))))))))
    (lispm-desktop-characters--card-row #'ignore t)
    (lispm-desktop-characters--card-tabs character width)
    ;; 1. Soul (for person characters) or Skill (for app characters).
    (lispm-desktop-characters--card-soul-section character tab text-width heading line)
    ;; 2. Self (in its own words) and Memory (Lorebook).
    (lispm-desktop-characters--card-memory-section character tab text-width heading line)
    ;; 3. Knowledge base (RAG lorebook and attached documents).
    (lispm-desktop-characters--card-knowledge-section character tab text-width heading line)
    ;; 4. Conversations & Lately (diary).
    (lispm-desktop-characters--card-conversations-section character tab text-width heading line)
    (lispm-desktop-characters--card-row #'ignore t)
    (insert (propertize " " 'display `(space :align-to (,margin)))
            (lispm-ui-group-edge width nil t)
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
  "Read the name of a character, with PROMPT; return it."
  (let ((titles (mapcar #'lispm-character-title* (lispm-desktop-characters--listed))))
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
  (lispm-desktop-form-window-closed 'character-form 'lispm-desktop-characters--form))

(defun lispm-desktop-characters--show-form (form)
  "Show FORM, the buffer of a form of a character, in its window: one at a time."
  (lispm-desktop-form-show 'character-form form 'lispm-desktop-characters--form))

(defun lispm-desktop-characters-new ()
  "Make a character, in its form."
  (interactive)
  (lispm-desktop-characters--show-form (lispm-character-form)))

(defun lispm-desktop-characters-change (&optional character)
  "Change CHARACTER, by default the one point is on, in its form."
  (interactive)
  (lispm-desktop-characters--show-form
   (lispm-character-form (or character (lispm-desktop-characters--at-point)))))

(defun lispm-desktop-characters-toggle-view ()
  "Switch the characters layout between list and avatar grid."
  (interactive)
  (setq lispm-desktop-characters--view
        (if (eq lispm-desktop-characters--view 'grid) 'list 'grid))
  (lispm-desktop-characters--draw))

(defun lispm-desktop-characters-next (&optional step)
  "Select the next character in the list or grid (or offset by STEP)."
  (interactive)
  (when-let* ((characters (lispm-desktop-characters--listed))
              (names (mapcar #'lispm-character-name characters))
              (count (length names))
              ((> count 0)))
    (let* ((current (or (get-text-property (point) 'lispm-desktop-characters-name)
                        lispm-desktop-characters--selected))
           (idx (or (seq-position names current) 0))
           (target (nth (mod (+ idx (or step 1)) count) names)))
      (setq lispm-desktop-characters--selected target)
      (goto-char (point-min))
      (lispm-desktop-characters--draw)
      (when-let* ((pos (text-property-any (point-min) (point-max)
                                          'lispm-desktop-characters-name target)))
        (goto-char pos)))))

(defun lispm-desktop-characters-previous ()
  "Select the previous character in the list or grid."
  (interactive)
  (lispm-desktop-characters-next -1))

(defun lispm-desktop-characters-right ()
  "Move to the next character tile in grid view, or forward one character."
  (interactive)
  (if (and (eq lispm-desktop-characters--view 'grid)
           (or (get-text-property (point) 'lispm-desktop-characters-name)
               (text-property-not-all (line-beginning-position) (line-end-position)
                                      'lispm-desktop-characters-name nil)))
      (lispm-desktop-characters-next 1)
    (forward-char 1)))

(defun lispm-desktop-characters-left ()
  "Move to the previous character tile in grid view, or backward one character."
  (interactive)
  (if (and (eq lispm-desktop-characters--view 'grid)
           (or (get-text-property (point) 'lispm-desktop-characters-name)
               (text-property-not-all (line-beginning-position) (line-end-position)
                                      'lispm-desktop-characters-name nil)))
      (lispm-desktop-characters-next -1)
    (backward-char 1)))

(defun lispm-desktop-characters-close ()
  "Close the Characters floating window, or quit the current window."
  (interactive)
  (if (lispm-desktop-app-window-id (selected-frame))
      (lispm-desktop-app-window-close 'characters)
    (quit-window)))

(defun lispm-desktop-characters-select-tab (tab)
  "Show TAB of the character card: `all', `soul', `memory' or `conversations'."
  (interactive)
  (when (memq tab lispm-desktop-characters--tabs)
    (setq lispm-desktop-characters--tab tab)
    (lispm-desktop-characters--draw)))

(defun lispm-desktop-characters-next-tab ()
  "Switch to the next tab of the character card."
  (interactive)
  (let* ((tabs lispm-desktop-characters--tabs)
         (pos (or (seq-position tabs lispm-desktop-characters--tab) 0)))
    (lispm-desktop-characters-select-tab (nth (mod (1+ pos) (length tabs)) tabs))))

(defun lispm-desktop-characters-previous-tab ()
  "Switch to the previous tab of the character card."
  (interactive)
  (let* ((tabs lispm-desktop-characters--tabs)
         (pos (or (seq-position tabs lispm-desktop-characters--tab) 0)))
    (lispm-desktop-characters-select-tab (nth (mod (1- pos) (length tabs)) tabs))))

(defun lispm-desktop-characters-tab-all ()
  "Show all sections of the character card."
  (interactive)
  (lispm-desktop-characters-select-tab 'all))

(defun lispm-desktop-characters-tab-soul ()
  "Show the Soul or Skill section of the character card."
  (interactive)
  (lispm-desktop-characters-select-tab 'soul))

(defun lispm-desktop-characters-tab-memory ()
  "Show the Memory section of the character card."
  (interactive)
  (lispm-desktop-characters-select-tab 'memory))

(defun lispm-desktop-characters-tab-knowledge ()
  "Show the Knowledge (RAG) section of the character card."
  (interactive)
  (lispm-desktop-characters-select-tab 'knowledge))

(defun lispm-desktop-characters-tab-conversations ()
  "Show the Conversations and Lately section of the character card."
  (interactive)
  (lispm-desktop-characters-select-tab 'conversations))

(defun lispm-desktop-characters-add-knowledge (title content &optional keywords)
  "Add a knowledge entry TITLE with CONTENT and KEYWORDS to the character at point."
  (interactive
   (list (read-string (lispm-desktop-tr "Entry title: "))
         (read-string (lispm-desktop-tr "Content: "))
         (read-string (lispm-desktop-tr "Keywords (comma-separated, optional): "))))
  (unless (fboundp 'lispm-character-rag-add)
    (user-error "%s" (lispm-desktop-tr "This LispM has no characters yet: update LispM Agent")))
  (let ((character (lispm-desktop-characters--at-point)))
    (lispm-character-rag-add character title content
                             (and keywords (not (string-blank-p keywords)) keywords))
    (lispm-desktop-characters--draw)))

(defun lispm-desktop-characters-generate-avatar ()
  "Generate an avatar picture for the character at point with ComfyUI."
  (interactive)
  (unless (fboundp 'lispm-character-generate-avatar)
    (user-error "%s" (lispm-desktop-tr "This LispM has no characters yet: update LispM Agent")))
  (lispm-character-generate-avatar (lispm-desktop-characters--at-point)))

(defun lispm-desktop-characters-open-file (&optional character)
  "Open the character file or skill manual of CHARACTER.
Defaults to the character at point."
  (interactive)
  (let* ((character (or character (lispm-desktop-characters--at-point)))
         (file (or (lispm-character-file character)
                   (and (lispm-character-skill character)
                        (plist-get (lispm-desktop-characters--skill-info character) :file)))))
    (cond
     ((and file (file-readable-p file))
      (find-file file))
     ((fboundp 'lispm-visit-character-file)
      (lispm-visit-character-file character))
     (t
      (lispm-desktop-characters-change character)))))

(defun lispm-desktop-characters-talk-here ()
  "Talk to the character point is on, or open the conversation or file point is on."
  (interactive)
  (cond
   ((get-text-property (point) 'lispm-desktop-characters-conversation)
    (let ((conversation (get-text-property (point) 'lispm-desktop-characters-conversation)))
      (if (plist-get conversation :home)
          (lispm-desktop-characters-talk lispm-desktop-characters--selected)
        (lispm-show (lispm-character-agent lispm-desktop-characters--selected
                                           (plist-get conversation :root))))))
   ((get-text-property (point) 'lispm-desktop-characters-file)
    (let* ((target (get-text-property (point) 'lispm-desktop-characters-file))
           (file (if (consp target) (car target) target))
           (pos (and (consp target) (cdr target))))
      (when (and file (file-readable-p file))
        (find-file file)
        (when (integerp pos)
          (goto-char (min pos (point-max)))))))
   (t
    (lispm-desktop-characters-talk (lispm-desktop-characters--at-point)))))

(defun lispm-desktop-characters-delete (&optional character)
  "Delete CHARACTER, by default the one point is on, once you agree."
  (interactive)
  (let ((character (or character (lispm-desktop-characters--at-point))))
    (when (yes-or-no-p (lispm-desktop-tr "Delete %s, with its memory and its past?"
                                         (lispm-character-title* character)))
      (lispm-delete-character character))))

(defun lispm-desktop-characters--press-select (event)
  "Move point to where mouse EVENT pressed and select any character there."
  (when-let* ((posn (event-start event))
              (window (posn-window posn))
              ((window-live-p window)))
    (select-window window)
    (when-let* ((pt (posn-point posn)))
      (goto-char pt)
      (when-let* ((name (get-text-property pt 'lispm-desktop-characters-name))
                  ((not (eq name lispm-desktop-characters--selected))))
        (lispm-desktop-characters--draw)))
    (and lispm-desktop-characters--selected
         (lispm-get-character lispm-desktop-characters--selected t))))

(defun lispm-desktop-characters-mouse-select (event)
  "Select the character clicked by mouse EVENT."
  (interactive "e")
  (lispm-desktop-characters--press-select event))

(defun lispm-desktop-characters-mouse-click (event)
  "Act on a single click EVENT: open a card conversation or file link, or select."
  (interactive "e")
  (lispm-desktop-characters--press-select event)
  (when (or (get-text-property (point) 'lispm-desktop-characters-conversation)
            (get-text-property (point) 'lispm-desktop-characters-file))
    (lispm-desktop-characters-talk-here)))

(defun lispm-desktop-characters-mouse-open (event)
  "Open the character or card item double-clicked by EVENT."
  (interactive "e")
  (when (lispm-desktop-characters--press-select event)
    (lispm-desktop-characters-talk-here)))

(defun lispm-desktop-characters--menu-items (&optional character)
  "Return the right-click context menu items for CHARACTER."
  (let ((character (or character
                       (and lispm-desktop-characters--selected
                            (lispm-get-character lispm-desktop-characters--selected t)))))
    `((:label ,(lispm-desktop-tr "Talk") :action lispm-desktop-characters-talk-here :key "RET"
              :enabled ,(and character t))
      (:label ,(lispm-desktop-tr "Change…") :action lispm-desktop-characters-change :key "e"
              :enabled ,(and character t))
      separator
      (:label ,(lispm-desktop-tr "Add to knowledge base") :action lispm-desktop-characters-add-knowledge :key "a"
              :enabled ,(and character t))
      (:label ,(lispm-desktop-tr "Generate avatar with ComfyUI") :action lispm-desktop-characters-generate-avatar :key "i"
              :enabled ,(and character (not (lispm-character-skill character)) t))
      (:label ,(lispm-desktop-tr "Open character file") :action lispm-desktop-characters-open-file :key "o"
              :enabled ,(and character t))
      separator
      (:label ,(lispm-desktop-tr "Switch between list and grid") :action lispm-desktop-characters-toggle-view :key "v")
      (:label ,(lispm-desktop-tr "New Character…") :action lispm-desktop-characters-new :key "n")
      (:label ,(lispm-desktop-tr "Delete…") :action lispm-desktop-characters-delete :key "d"
              :enabled ,(and character
                             (or (not (lispm-character-skill character))
                                 (and (lispm-character-file character) t)))))))

(defun lispm-desktop-characters-context-menu (event)
  "Pop up the right-click menu for the character under mouse EVENT."
  (interactive "e")
  (let ((character (lispm-desktop-characters--press-select event))
        (xy (lispm-desktop-popup-frame-xy event)))
    (lispm-ui-menu (lispm-desktop-characters--menu-items character) (car xy) (cdr xy))))

(defun lispm-desktop-characters--keyboard-menu ()
  "Pop up the right-click menu of the selected character from the keyboard."
  (let ((xy (lispm-desktop-popup-frame-xy (point))))
    (lispm-ui-menu (lispm-desktop-characters--menu-items) (car xy) (cdr xy))))

(defun lispm-desktop-characters-app-character (app)
  "Return the character associated with desktop application APP, or nil."
  (when (and app (lispm-desktop-characters-available-p))
    (or (when-let* ((name (plist-get (cdr (assq app lispm-desktop-apps)) :character)))
          (lispm-get-character name t))
        (lispm-get-character app t))))

(defun lispm-desktop-characters-app-menu-items (app)
  "Return right-click menu items for APP when it is backed by a character."
  (when-let* ((character (lispm-desktop-characters-app-character app)))
    `(,@(when (lispm-character-skill character)
          `((:label ,(lispm-desktop-tr "Talk")
                    :action ,(lambda () (interactive) (lispm-desktop-characters-talk character)))))
      (:label ,(lispm-desktop-tr "Change…")
              :action ,(lambda () (interactive) (lispm-desktop-characters-change character))))))

(defun lispm-desktop-characters-change-current ()
  "Open the configuration form for the character of the current chat buffer."
  (interactive)
  (if-let* ((agent (bound-and-true-p lispm-chat-agent))
            (name (and (fboundp 'lispm-agent-character) (lispm-agent-character agent)))
            (character (lispm-get-character name t)))
      (lispm-desktop-characters-change character)
    (user-error "%s" (lispm-desktop-tr "No character here"))))

(defun lispm-desktop-characters--chat-buttons ()
  "Toolbar buttons for `lispm-chat-mode', adding `Change…' in a character's chat."
  (let ((base (when (fboundp 'lispm-desktop-lispm-chat-buttons)
                (lispm-desktop-lispm-chat-buttons))))
    (if (and (bound-and-true-p lispm-chat-agent)
             (fboundp 'lispm-agent-character)
             (lispm-agent-character lispm-chat-agent))
        (append base `((,(lispm-desktop-tr-noop "Change…") lispm-desktop-characters-change-current)))
      base)))

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
           ("a" . ,(lispm-desktop-tr-noop "Add to knowledge base"))
           ("i" . ,(lispm-desktop-tr-noop "Generate avatar with ComfyUI"))
           ("o" . ,(lispm-desktop-tr-noop "Open character file"))
           ("v" . ,(lispm-desktop-tr-noop "Switch between list and grid"))
           ("Tab" . ,(lispm-desktop-tr-noop "Next card tab"))
           ("g" . ,(lispm-desktop-tr-noop "Draw the list again")))
   :empty `(,(lambda () (null (lispm-desktop-characters--listed)))
            ,(lispm-desktop-tr-noop "No characters yet.  A character is someone: a name, \
a soul, a picture, a memory and a past of its own, whom you and every agent reach by name, as @Ada.")
            ((,(lispm-desktop-tr-noop "New Character…") lispm-desktop-characters-new))))
  ;; A character's window shows its conversation, wrapped at words as
  ;; LispM's chat wraps it, not cut off as an application's lines are.
  (lispm-desktop-app-ui-define
   'lispm-chat-mode
   :setup #'lispm-desktop-characters--wrap
   :buttons #'lispm-desktop-characters--chat-buttons
   :status #'lispm-desktop-lispm-chat-status))

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
