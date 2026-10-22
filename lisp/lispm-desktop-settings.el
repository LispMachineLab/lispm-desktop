;;; lispm-desktop-settings.el --- Control Panels, as Mac OS 9's  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-desktop
;; SPDX-License-Identifier: AGPL-3.0-or-later

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

;; Control Panels is an application of its own, as Mac OS 9's control
;; panels were.  Its folder, opened from the desktop's Control Panels icon,
;; is a Finder window holding a panel each; the λ menu's Control Panels
;; lists them too.  A panel opens in a window floating over the desktop,
;; titled with its name, its tabs choosing among the panels as the
;; Appearance control panel's did:
;;
;;   Appearance  the colour theme
;;   Desktop     the desktop's colour, its picture and how it covers it,
;;               and Arrange Icons
;;   Keyboard    Vim or Emacs keys
;;   Language    the language LispM speaks, see lispm-desktop-i18n.el
;;   Internet    the browser's home page and search address, its history
;;               and its bookmarks
;;   Network     the proxy every connection LispM makes goes through, see
;;               lispm-desktop-proxy.el
;;   LispM       LispM Agent's own settings: the model its agents use, what
;;               they may do unasked, how their conversations are summed
;;               up and remembered, their roles and skills, see LispM
;;               Agent's lispm-settings.el; shown once it is loaded
;;   Sandbox     how strictly agents' shell commands and MCP servers are
;;               confined, the rules of the Custom level, and the domains
;;               they may reach, see LispM Agent's lispm-sandbox.el; shown
;;               once it is loaded
;;   Startup     whether the computer opens LispM at login, and the
;;               startup items LispM opens once started, see
;;               lispm-desktop-startup.el
;;   Kernel      whether LispM Kernel, lispm-kernel, runs LispM's background
;;               work, and how it stands, see lispm-desktop-kernel.el; and
;;               the Emacs LispM.app runs on, see lispm-desktop-emacs.el
;;   Software Update  Update Now, and how LispM checks for updates and
;;               installs them by itself, see lispm-desktop-update.el;
;;               whether the App Store's applications update by
;;               themselves, and which can be
;;
;; Each choice takes effect at once where it can, and is saved for the next
;; start with Customize, as the menus' choices are; personal.el can still
;; set any of them.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-app-window)
(require 'lispm-desktop-popup)
(require 'lispm-desktop-update)
(require 'lispm-desktop-emacs)
(require 'lispm-desktop-kernel)
(require 'lispm-desktop-store)
(require 'lispm-desktop-proxy)
(require 'lispm-desktop-startup)
(require 'lispm-desktop-loading)
(require 'lispm-desktop-i18n)
(require 'lispm-desktop-kernel)

(defvar lispm-desktop-color)
(defvar lispm-desktop-picture)
(defvar lispm-desktop-picture-position)
(defvar lispm-desktop-icon-places)
(defvar lispm-browser-home-url)
(defvar lispm-browser-search-url)
(defvar lispm-browser-history-size)
(declare-function lispm-desktop-choose-picture "lispm-desktop-surface" (file))
(declare-function lispm-desktop-remove-picture "lispm-desktop-surface" ())
(declare-function lispm-desktop-arrange-icons "lispm-desktop-surface" ())
(declare-function lispm-desktop--color "lispm-desktop-surface" ())
(declare-function lispm-desktop--hex-color "lispm-desktop-surface" (color))
(declare-function lispm-browser-history "lispm-browser-places" ())
(declare-function lispm-browser-clear-history "lispm-browser-places" ())
(declare-function lispm-browser-bookmarks "lispm-browser-places" ())
(declare-function lispm-desktop-browser-edit-bookmarks "lispm-desktop-browser-places" ())
(declare-function lispm-browser-available-p "lispm-browser" ())
(declare-function lispm-browser-proxy-followed-p "lispm-browser-proxy" ())
(defvar lispm-sandbox-level)
(defvar lispm-sandbox-levels)
(defvar lispm-sandbox-allowed-domains)
(defvar lispm-sandbox-srt-program)
(defvar lispm-sandbox-srt-version)
(defvar lispm-sandbox-custom)
(declare-function lispm-sandbox-set-level "lispm-sandbox" (level &optional save))
(declare-function lispm-sandbox-level-name "lispm-sandbox" (level))
(declare-function lispm-sandbox-level-description "lispm-sandbox" (&optional level))
(declare-function lispm-sandbox-rules "lispm-sandbox" (&optional level))
(declare-function lispm-sandbox-settings "lispm-sandbox" (&optional level))
(declare-function lispm-sandbox-customize "lispm-sandbox" (&optional from))
(declare-function lispm-sandbox-srt-status "lispm-sandbox" ())
(declare-function lispm-sandbox-install "lispm-sandbox" ())
(defvar lispm-efforts)
(defvar lispm-permission-classes)
(defvar lispm-permission-actions)
(defvar lispm-compact-sizes)
(defvar lispm-compact-threshold)
(defvar lispm-compact-save-memory)
(defvar lispm-git)
(defvar lispm-default-role)
(defvar lispm-skill-directories)
(declare-function lispm-model-status "lispm-settings" ())
(declare-function lispm-model-choices "lispm-settings" ())
(declare-function lispm-set-model "lispm-settings" (backend model &optional save))
(declare-function lispm-set-effort "lispm-settings" (effort &optional save))
(declare-function lispm-effort-name "lispm-settings" (effort))
(declare-function lispm-backend-actions "lispm-settings" ())
(declare-function lispm-settings-set "lispm-settings" (variable value &optional save))
(declare-function lispm-permission-class-description "lispm-settings" (class))
(declare-function lispm-permission-action-name "lispm-settings" (action))
(declare-function lispm-default-permission "lispm-settings" (class))
(declare-function lispm-set-default-permission "lispm-settings" (class action &optional save))
(declare-function lispm-trusted-skill-files "lispm-settings" ())
(declare-function lispm-untrust-skill "lispm-settings" (file))
(declare-function lispm-compact-size "lispm-settings" ())
(declare-function lispm-compact-size-name "lispm-settings" (size))
(declare-function lispm-compact-size-note "lispm-settings" (size))
(declare-function lispm-set-compact-size "lispm-settings" (size &optional save))
(declare-function lispm-open-memory "lispm-settings" (file))
(declare-function lispm-set-default-role "lispm-settings" (role &optional save))
(declare-function lispm-skill-directory-builtin-p "lispm-settings" (directory))
(declare-function lispm-skill-directory-count "lispm-settings" (directory))
(declare-function lispm-add-skill-directory "lispm-settings" (directory &optional save))
(declare-function lispm-remove-skill-directory "lispm-settings" (directory &optional save))
(declare-function lispm-roles "lispm-core" ())
(declare-function lispm-role-name "lispm-core" (role))
(declare-function lispm-role-description "lispm-core" (role))

(defconst lispm-desktop-settings-buffer-name " *Control Panels*"
  "Name of the control panel's window's buffer.")

(defconst lispm-desktop-settings-panels
  `((themes ,(lispm-desktop-tr-noop "Appearance") lispm-desktop-settings--themes "appearance-32")
    (desktop ,(lispm-desktop-tr-noop "Desktop") lispm-desktop-settings--desktop "desktop-picture-32")
    (keyboard ,(lispm-desktop-tr-noop "Keyboard") lispm-desktop-settings--keyboard "keyboard-32")
    (language ,(lispm-desktop-tr-noop "Language") lispm-desktop-settings--language "language-32")
    (browser ,(lispm-desktop-tr-noop "Internet") lispm-desktop-settings--browser "globe-32"
             lispm-desktop-settings--browser-p)
    (network ,(lispm-desktop-tr-noop "Network") lispm-desktop-settings--network "network-32")
    (lispm ,(lispm-desktop-tr-noop "LispM") lispm-desktop-settings--lispm "lispm-32"
           lispm-desktop-settings--lispm-p)
    (sandbox ,(lispm-desktop-tr-noop "Sandbox") lispm-desktop-settings--sandbox "sandbox-32"
             lispm-desktop-settings--sandbox-p)
    (startup ,(lispm-desktop-tr-noop "Startup") lispm-desktop-settings--startup "startup-32")
    (kernel ,(lispm-desktop-tr-noop "Kernel") lispm-desktop-settings--kernel "kernel-32")
    (update ,(lispm-desktop-tr-noop "Software Update") lispm-desktop-settings--update "software-update-32"))
  "The panels, in their tabs' order: (KEY LABEL INSERT ICON [VISIBLE]).
LABEL is in English, translated where shown, see
`lispm-desktop-settings-panel-label'.  INSERT fills the window with the
panel, ICON is its large icon in the Control Panels folder; VISIBLE, when
given, is a function deciding whether the panel shows.")

(defvar-local lispm-desktop-settings--panel 'themes
  "The panel the control panel's window shows.")

(defun lispm-desktop-settings--browser-p ()
  "Return non-nil when the browser can run, and so has settings."
  (and (fboundp 'lispm-browser-available-p) (lispm-browser-available-p)))

(defun lispm-desktop-settings--sandbox-p ()
  "Return non-nil when LispM's agents can run in a sandbox, and so it has
settings: LispM Agent 0.3.0 or later is loaded."
  (and (featurep 'lispm-sandbox) (fboundp 'lispm-sandbox-set-level)))

(defun lispm-desktop-settings--lispm-p ()
  "Return non-nil when LispM Agent says what its settings are, for the
LispM panel: LispM Agent 0.6.0 or later is loaded."
  (and (featurep 'lispm-settings) (fboundp 'lispm-model-status)))

(defun lispm-desktop-settings--panels ()
  "Return the panels that show here."
  (seq-filter (lambda (panel)
                (let ((visible (nth 4 panel)))
                  (or (not visible) (funcall visible))))
              lispm-desktop-settings-panels))

(defun lispm-desktop-settings-panel-label (panel)
  "Return the name of PANEL, an entry of `lispm-desktop-settings-panels',
in the language LispM speaks."
  (lispm-desktop-tr (nth 1 panel)))

(defun lispm-desktop-settings-open-p ()
  "Return non-nil while the control panel's window is open."
  (lispm-desktop-app-window-open-p 'settings))

;;;; Controls
;;
;; Radio buttons and push buttons are drawn as Mac OS 9's, in the theme's
;; colours.  Both are text buttons: Tab moves between them, ringing the one
;; reached in the focus colour, and Return or Space presses it.

(defun lispm-desktop-settings--heading-indent ()
  "Return the blank before a heading."
  (propertize " " 'display `(space :width (,(lispm-desktop-px 12)))))

(defun lispm-desktop-settings--indent ()
  "Return the blank before a row of settings."
  (propertize " " 'display `(space :width (,(lispm-desktop-px 24)))))

(defun lispm-desktop-settings--under-label (control)
  "Return the blank before a line under the label of a CONTROL, `radio' or
`check-box', so that the line starts where the label does: after the
control, 16 or 18 pixels wide with the room for its ring."
  (concat (lispm-desktop-settings--indent)
          (propertize " " 'display `(space :width (,(if (lispm-desktop-tty-p) 3
                                                      (if (eq control 'radio) 16 18)))))
          " "))

(defun lispm-desktop-settings--radio-button (label on action help)
  "Insert a radio button named LABEL, ON or off, calling ACTION when pressed.
HELP describes it."
  (let ((radio (lambda (focused)
                 (lispm-desktop-platinum-image-of (lispm-desktop-platinum-radio on nil focused)))))
    (insert-text-button
     (concat (propertize " " 'display (funcall radio nil)
                         'lispm-desktop-platinum-plain (funcall radio nil)
                         'lispm-desktop-platinum-focused (funcall radio t))
             " " label)
     'action (lambda (_) (funcall action) (lispm-desktop-settings--render))
     'face 'default 'follow-link t 'help-echo help)))

(defun lispm-desktop-settings--radio (label on action &optional note)
  "Insert a radio button named LABEL, ON or off, running ACTION when pressed,
NOTE after it in the dimmed text."
  (insert (lispm-desktop-settings--indent))
  (lispm-desktop-settings--radio-button label on action (lispm-desktop-tr "Choose %s" label))
  (when note
    (insert "  " (propertize note 'face 'shadow)))
  (insert "\n"))

(defun lispm-desktop-settings--check-box (label on action &optional note)
  "Insert a check box named LABEL, checked when ON, running ACTION when
pressed, NOTE after it in the dimmed text."
  (let ((box (lambda (focused)
               (lispm-desktop-platinum-image-of (lispm-desktop-platinum-check-box on nil focused 2)))))
    (insert (lispm-desktop-settings--indent))
    (insert-text-button
     (concat (propertize " " 'display (funcall box nil)
                         'lispm-desktop-platinum-plain (funcall box nil)
                         'lispm-desktop-platinum-focused (funcall box t))
             " " label)
     'action (lambda (_) (funcall action) (lispm-desktop-settings--render))
     'face 'default 'follow-link t
     'help-echo (if on (lispm-desktop-tr "Turn off: %s" label) (lispm-desktop-tr "Turn on: %s" label)))
    (when note
      (insert "  " (propertize note 'face 'shadow)))
    (insert "\n")))

(defconst lispm-desktop-settings--icon-button-size '(52 . 50)
  "Width and height in pixels of a bevel button holding a large icon, as
Software Update's Update Now button.")

(defun lispm-desktop-settings--icon-button-image (icon state focused)
  "Return the image of a bevel button holding ICON, a 32-pixel icon's name,
in STATE, nil or `pressed', ringed when FOCUSED.  The ring's room is
always left, so the button keeps its place.  On a text terminal the
button is text, the icon a character in it."
  (if (lispm-desktop-tty-p)
      (lispm-desktop-platinum-text-control
       (lispm-desktop-platinum-text-button (concat " " (substring-no-properties
                                                         (lispm-desktop-platinum-icon icon))
                                                   " ")
                                           state)
       focused)
   (lispm-desktop-platinum-image
   (list 'settings-icon-button icon state focused)
   (lambda ()
     (pcase-let* ((`(,w . ,h) lispm-desktop-settings--icon-button-size)
                  (palette lispm-desktop-platinum-current-palette)
                  (pad 3)
                  (svg (svg-create (+ w (* 2 pad)) (+ h (* 2 pad)))))
       (when focused
         (lispm-desktop-platinum-svg-focus-ring svg pad pad w h 1 palette))
       (lispm-desktop-platinum-svg-embed
        svg (lispm-desktop-platinum-draw-bevel-button palette w h :state state) pad pad w h)
       (lispm-desktop-platinum-svg-emacs-image
        svg (create-image (lispm-desktop-platinum-icon-file icon))
        (+ pad (/ (- w 32) 2)) (+ pad (/ (- h 32) 2)) 32 32)
       svg)))))

(defun lispm-desktop-settings--icon-button (icon label action help)
  "Insert, centred, a bevel button holding ICON, a 32-pixel icon's name,
LABEL under it, running ACTION when pressed; HELP describes it.
Return, Space and the mouse press it as a push button."
  (let ((half (if (lispm-desktop-tty-p)
                  (/ (string-width (lispm-desktop-settings--icon-button-image icon nil nil)) 2)
                (/ (+ (car lispm-desktop-settings--icon-button-size) 6) 2))))
    (insert (propertize " " 'display `(space :align-to (- center (,half)))))
    (insert-text-button " "
                        'display (lispm-desktop-settings--icon-button-image icon nil nil)
                        'lispm-desktop-platinum-plain (lispm-desktop-settings--icon-button-image icon nil nil)
                        'lispm-desktop-platinum-focused (lispm-desktop-settings--icon-button-image icon nil t)
                        'lispm-desktop-platinum-pressed (lispm-desktop-settings--icon-button-image icon 'pressed nil)
                        'action (lambda (_) (funcall action) (lispm-desktop-settings--render))
                        'lispm-desktop-platinum-label label
                        'face nil 'keymap lispm-desktop-platinum-push-button-map
                        'help-echo help)
    (insert "\n")
    (let ((label (propertize label 'face 'bold)))
      (insert (propertize " " 'display `(space :align-to (- center (,(/ (string-pixel-width label) 2)))))
              label "\n"))))

(defun lispm-desktop-settings--button (label action &optional help)
  "Insert a push button named LABEL running ACTION when pressed; HELP
describes it."
  (lispm-desktop-platinum-insert-push-button
   label (lambda () (funcall action) (lispm-desktop-settings--render))
   :help (or help label)))

(defun lispm-desktop-settings--heading (title)
  "Insert the heading TITLE of a group of settings.
The group runs to the next heading; once the panel is drawn, it is framed
in a group box titled TITLE, see `lispm-desktop-settings--frame-groups'."
  (insert (propertize (concat (lispm-desktop-settings--heading-indent) title)
                      'face 'bold 'lispm-desktop-settings-group title)
          "\n"))

(defconst lispm-desktop-settings--group-margin 12
  "Pixels between a group box and the pane's sides.")

(defun lispm-desktop-settings--frame-groups ()
  "Frame each group of settings in the current buffer in a group box, as
Mac OS 9's control panels framed theirs: its heading becomes the box's
titled top edge, each of its lines gets the box's sides, as tall as the
line, and its bottom edge closes it, a gap after it."
  (let* ((window (get-buffer-window (current-buffer) t))
         (width (if window (window-body-width window t) 500))
         (tty (lispm-desktop-tty-p))
         ;; A text terminal keeps the window's last column for its sign of
         ;; a long line.
         (width (- width (if tty 2 0)))
         (margin (lispm-desktop-px lispm-desktop-settings--group-margin))
         (outside (propertize " " 'display `(space :width (,margin))))
         (box (- width (* 2 margin)))
         (headings nil))
    (goto-char (point-min))
    (while (not (eobp))
      (when-let* ((title (get-text-property (point) 'lispm-desktop-settings-group)))
        (push (cons (point) title) headings))
      (forward-line 1))
    ;; From the last group up, so earlier places hold.
    (let ((end (point-max)))
      (pcase-dolist (`(,start . ,title) headings)
        (save-excursion
          (goto-char end)
          (insert outside (lispm-desktop-platinum-group-edge box nil t) "\n"
                  (if tty "" (concat (propertize " " 'display '(space :height (8))) "\n")))
          (goto-char start)
          (forward-line 1)
          (while (< (point) end)
            (let* ((height (if window
                               ;; The newline counted in, so a blank line
                               ;; measures as tall as it shows.
                               (max 1 (cdr (window-text-pixel-size window (line-beginning-position)
                                                                   (min (1+ (line-end-position))
                                                                        (point-max)))))
                             16))
                   (left (concat outside (lispm-desktop-platinum-group-side height)))
                   (right (concat (propertize " " 'display
                                              (if tty
                                                  `(space :align-to ,(+ margin box -1))
                                                `(space :align-to (- right (,(+ margin 2))))))
                                  (lispm-desktop-platinum-group-side height))))
              (put-text-property (line-beginning-position) (min (1+ (line-end-position)) (point-max))
                                 'line-prefix left)
              (end-of-line)
              ;; On a blank line the side is the line's first text, so it
              ;; carries the line's prefix too.
              (insert (propertize right 'line-prefix left))
              (setq end (+ end (length right)))
              (forward-line 1)))
          (goto-char start)
          (delete-region start (line-end-position))
          (insert outside (propertize (lispm-desktop-platinum-group-edge box title)
                                      'lispm-desktop-platinum-label title)))
        (setq end start)))))

(defun lispm-desktop-settings--note (text)
  "Insert TEXT, a remark about the settings above, dimmed."
  (insert (lispm-desktop-settings--indent) (propertize text 'face 'shadow) "\n"))

(defun lispm-desktop-settings--text-width (indent)
  "Return the pixels a line of the pane has for text after INDENT pixels,
inside its group box."
  (let ((window (get-buffer-window (current-buffer) t)))
    (- (if window (window-body-width window t) 500)
       indent (* 4 lispm-desktop-settings--group-margin))))

(defun lispm-desktop-settings--wrap (text width)
  "Return TEXT broken between words, or the characters of Chinese and
Japanese, into lines at most WIDTH pixels wide when dimmed; a word wider
than that stands alone, cut short."
  (mapcar (lambda (line)
            (let ((wide (lispm-desktop-platinum-content-width line 'shadow)))
              (if (<= wide width)
                  line
                (lispm-desktop-settings--truncate
                 line (max 2 (1- (/ (* (length line) width) wide)))))))
          (lispm-desktop-i18n-wrap
           text (lambda (line) (<= (lispm-desktop-platinum-content-width line 'shadow) width)))))

(defun lispm-desktop-settings--paragraph (text &optional indent)
  "Insert TEXT, a remark about the settings above, dimmed and wrapped to
the pane.  INDENT, a string, comes before each line instead of the
indent of a row of settings."
  (let ((indent (or indent (lispm-desktop-settings--indent))))
    (dolist (line (lispm-desktop-settings--wrap
                   text (lispm-desktop-settings--text-width (string-pixel-width indent))))
      (insert indent (propertize line 'face 'shadow) "\n"))))

(defun lispm-desktop-settings--save (variable value)
  "Set VARIABLE to VALUE now and for every later start."
  (customize-save-variable variable value))

;;;; Panels

(defun lispm-desktop-settings--other-theme ()
  "Ask for any theme installed, and switch to it now and for later starts."
  (let ((theme (completing-read (lispm-desktop-tr "Theme: ") (mapcar #'symbol-name (custom-available-themes))
                                nil t)))
    (unless (string-empty-p theme)
      (lispm-desktop-set-theme (intern theme)))))

(defun lispm-desktop-settings--themes ()
  "Insert the Themes panel: the look, and the colour theme."
  (lispm-desktop-settings--heading (lispm-desktop-tr "Look"))
  (pcase-dolist (`(,look . ,properties) lispm-desktop-looks)
    (lispm-desktop-settings--radio (lispm-desktop-look-title look) (eq lispm-desktop-look look)
                                   (lambda () (lispm-desktop-set-look look))
                                   (when-let* ((description (plist-get properties :description)))
                                     (lispm-desktop-tr description))))
  (insert "\n")
  (lispm-desktop-settings--paragraph
   (lispm-desktop-tr "Choosing a look takes its own colours with it, unless you chose a theme of your own."))
  (insert "\n")
  (lispm-desktop-settings--heading (lispm-desktop-tr "Theme"))
  (lispm-desktop-settings--radio "Dracula" (eq lispm-desktop-theme 'dracula)
                                 (lambda () (lispm-desktop-set-theme 'dracula))
                                 (lispm-desktop-tr "dark, Dracula's colours"))
  (lispm-desktop-settings--radio "Platinum" (eq lispm-desktop-theme 'platinum)
                                 (lambda () (lispm-desktop-set-theme 'platinum))
                                 (lispm-desktop-tr "Mac OS 9's greys"))
  (lispm-desktop-settings--radio "Windows 3.2" (eq lispm-desktop-theme 'win3)
                                 (lambda () (lispm-desktop-set-theme 'win3))
                                 (lispm-desktop-tr "navy and VGA grey"))
  (lispm-desktop-settings--radio "OS/2 2.0" (eq lispm-desktop-theme 'os2)
                                 (lambda () (lispm-desktop-set-theme 'os2))
                                 (lispm-desktop-tr "teal, pale yellow, pale grey"))
  (lispm-desktop-settings--radio "Phosphor" (eq lispm-desktop-theme 'phosphor)
                                 (lambda () (lispm-desktop-set-theme 'phosphor))
                                 (lispm-desktop-tr "green phosphor on black glass"))
  (lispm-desktop-settings--radio "Neon" (eq lispm-desktop-theme 'neon)
                                 (lambda () (lispm-desktop-set-theme 'neon))
                                 (lispm-desktop-tr "cyan, magenta and violet on night purple"))
  (when (and lispm-desktop-theme (not (memq lispm-desktop-theme '(dracula platinum win3 os2 phosphor neon))))
    (lispm-desktop-settings--radio (capitalize (symbol-name lispm-desktop-theme)) t #'ignore))
  (insert (lispm-desktop-settings--indent))
  (lispm-desktop-settings--button (lispm-desktop-tr "Other Theme…") #'lispm-desktop-settings--other-theme
                                  (lispm-desktop-tr "Choose any other theme installed"))
  (insert "\n\n")
  (lispm-desktop-settings--paragraph
   (lispm-desktop-tr "The menu bar, the title bars and the lists take their colours from the theme.")))

(defun lispm-desktop-settings--swatch (color)
  "Return a swatch of COLOR, one icon wide, or two cells of it on a text
terminal."
  (if (lispm-desktop-tty-p)
      (propertize "██" 'face `(:foreground ,color))
   (propertize " " 'display (lispm-desktop-platinum-image
                            (list 'swatch color)
                            (lambda ()
                              (let ((svg (svg-create 28 14)))
                                (svg-rectangle svg 0.5 0.5 27 13 :fill color
                                               :stroke (lispm-desktop-platinum-color :text))
                                svg))))))

(defun lispm-desktop-settings--choose-color ()
  "Ask for the desktop's colour and keep it, written #RRGGBB.
`read-color' returns a colour typed by name as #RRRRGGGGBBBB, which the
desktop's SVG cannot read."
  (let ((color (lispm-desktop--hex-color (read-color (lispm-desktop-tr "Desktop colour: ") t))))
    (when color
      (lispm-desktop-settings--save 'lispm-desktop-color color))))

(defun lispm-desktop-settings--picture-preview ()
  "Insert a small view of the desktop picture."
  (when-let* ((file (and lispm-desktop-picture (expand-file-name lispm-desktop-picture)))
              ((file-readable-p file))
              ((display-images-p)))
    (insert (lispm-desktop-settings--indent))
    (insert-image (create-image file nil nil :max-width 160 :max-height 100 :ascent 'center))
    (insert "\n")))

(defun lispm-desktop-settings--desktop ()
  "Insert the Desktop panel: its colour, its picture, its icons."
  (lispm-desktop-settings--heading (lispm-desktop-tr "Colour"))
  (lispm-desktop-settings--radio (lispm-desktop-tr "From the theme") (null lispm-desktop-color)
                                 (lambda () (lispm-desktop-settings--save 'lispm-desktop-color nil)))
  (insert (lispm-desktop-settings--indent))
  (lispm-desktop-settings--radio-button (lispm-desktop-tr "Your own") (and lispm-desktop-color t)
                                        #'lispm-desktop-settings--choose-color
                                        (lispm-desktop-tr "Choose a colour"))
  (insert " " (lispm-desktop-settings--swatch (lispm-desktop--color)) " ")
  (lispm-desktop-settings--button (lispm-desktop-tr "Choose…") #'lispm-desktop-settings--choose-color
                                  (lispm-desktop-tr "Choose the desktop's colour"))
  (insert "\n")
  (lispm-desktop-settings--heading (lispm-desktop-tr "Picture"))
  (lispm-desktop-settings--picture-preview)
  (lispm-desktop-settings--note (if lispm-desktop-picture
                                    (abbreviate-file-name lispm-desktop-picture)
                                  (lispm-desktop-tr "None: the desktop shows its colour.")))
  (insert (lispm-desktop-settings--indent))
  (lispm-desktop-settings--button (lispm-desktop-tr "Choose…")
                                  (lambda () (call-interactively #'lispm-desktop-choose-picture))
                                  (lispm-desktop-tr "Choose a picture for the desktop"))
  (insert " ")
  (when lispm-desktop-picture
    (lispm-desktop-settings--button (lispm-desktop-tr "None") #'lispm-desktop-remove-picture
                                    (lispm-desktop-tr "Show the desktop's colour alone")))
  (insert "\n\n")
  (pcase-dolist (`(,position ,label ,note)
                 `((fill ,(lispm-desktop-tr "Fill") ,(lispm-desktop-tr "cropped to the desktop"))
                   (fit ,(lispm-desktop-tr "Fit") ,(lispm-desktop-tr "whole, the colour around"))
                   (center ,(lispm-desktop-tr "Center") ,(lispm-desktop-tr "at its own size"))
                   (tile ,(lispm-desktop-tr "Tile") ,(lispm-desktop-tr "repeated"))))
    (lispm-desktop-settings--radio label (eq lispm-desktop-picture-position position)
                                   (lambda () (lispm-desktop-settings--save 'lispm-desktop-picture-position position))
                                   note))
  (lispm-desktop-settings--heading (lispm-desktop-tr "Icons"))
  (insert (lispm-desktop-settings--indent))
  (lispm-desktop-settings--button (lispm-desktop-tr "Arrange Icons") #'lispm-desktop-arrange-icons
                                  (lispm-desktop-tr "Put the desktop's icons back where they stood at first"))
  (insert "\n")
  (lispm-desktop-settings--paragraph
   (lispm-desktop-tr "Dragged icons land on the grid; this puts every one back.")))

(defun lispm-desktop-settings--keyboard ()
  "Insert the Keyboard panel: Vim or Emacs keys."
  (lispm-desktop-settings--heading (lispm-desktop-tr "Editing"))
  (lispm-desktop-settings--radio (lispm-desktop-tr "Vim keys") (eq lispm-desktop-editing-style 'vim)
                                 (lambda () (lispm-desktop-settings--save 'lispm-desktop-editing-style 'vim))
                                 (lispm-desktop-tr "Evil in files; Emacs keys elsewhere"))
  (lispm-desktop-settings--radio (lispm-desktop-tr "Emacs keys") (eq lispm-desktop-editing-style 'emacs)
                                 (lambda () (lispm-desktop-settings--save 'lispm-desktop-editing-style 'emacs)))
  (insert "\n")
  (lispm-desktop-settings--note (lispm-desktop-tr "Takes effect at the next start."))
  (when (eq system-type 'darwin)
    (lispm-desktop-settings--paragraph
     (lispm-desktop-tr "Command is Meta: Command-C and Command-V copy and paste."))))

(defun lispm-desktop-settings--language ()
  "Insert the Language panel: the language LispM speaks, the computer's
or one of its own, each named in itself, and in the language spoken."
  (lispm-desktop-settings--heading (lispm-desktop-tr "Language"))
  (lispm-desktop-settings--radio (lispm-desktop-tr "The computer's language") (null lispm-desktop-language)
                                 (lambda () (lispm-desktop-set-language nil))
                                 (lispm-desktop-language-name (lispm-desktop-system-language)))
  (pcase-dolist (`(,language ,english) lispm-desktop-languages)
    (let ((name (lispm-desktop-language-name language))
          (translated (lispm-desktop-tr english)))
      (lispm-desktop-settings--radio name (eq lispm-desktop-language language)
                                     (lambda () (lispm-desktop-set-language language))
                                     (unless (equal translated name) translated))))
  (insert "\n")
  (lispm-desktop-settings--paragraph
   (lispm-desktop-tr "Menus, windows and messages change at once. Documents, web pages, and what programs and agents say keep their own language.")))

(defun lispm-desktop-settings--change (variable prompt)
  "Ask with PROMPT for a new value of the string VARIABLE, and keep it."
  (let ((value (string-trim (read-string prompt (symbol-value variable)))))
    (unless (string-empty-p value)
      (lispm-desktop-settings--save variable value))))

(defun lispm-desktop-settings--browser ()
  "Insert the Browser panel: home page, search, history and bookmarks."
  (lispm-desktop-settings--heading (lispm-desktop-tr "Home Page"))
  (lispm-desktop-settings--note lispm-browser-home-url)
  (insert (lispm-desktop-settings--indent))
  (lispm-desktop-settings--button (lispm-desktop-tr "Change…")
                                  (lambda () (lispm-desktop-settings--change 'lispm-browser-home-url
                                                                             (lispm-desktop-tr "Home page: ")))
                                  (lispm-desktop-tr "Change the page Home goes to"))
  (insert "\n")
  (lispm-desktop-settings--heading (lispm-desktop-tr "Search"))
  (lispm-desktop-settings--note lispm-browser-search-url)
  (insert (lispm-desktop-settings--indent))
  (lispm-desktop-settings--button (lispm-desktop-tr "Change…")
                                  (lambda () (lispm-desktop-settings--change
                                              'lispm-browser-search-url
                                              (lispm-desktop-tr "Search address (%s for the words): ")))
                                  (lispm-desktop-tr "Change where what is typed in the location field is searched"))
  (insert "\n")
  (lispm-desktop-settings--heading (lispm-desktop-tr "History"))
  (let ((visited (length (lispm-browser-history))))
    (lispm-desktop-settings--paragraph
     (lispm-desktop-tr-n "%d page visited is kept, of at most %d." "%d pages visited are kept, of at most %d."
                         visited visited lispm-browser-history-size)))
  (insert (lispm-desktop-settings--indent))
  (lispm-desktop-settings--button (lispm-desktop-tr "Clear History…") #'lispm-browser-clear-history
                                  (lispm-desktop-tr "Forget every page visited"))
  (insert "\n")
  (lispm-desktop-settings--heading (lispm-desktop-tr "Bookmarks"))
  (lispm-desktop-settings--note (lispm-desktop-tr-n "%d bookmark." "%d bookmarks." (length (lispm-browser-bookmarks))))
  (insert (lispm-desktop-settings--indent))
  (lispm-desktop-settings--button (lispm-desktop-tr "Edit Bookmarks…") #'lispm-desktop-browser-edit-bookmarks
                                  (lispm-desktop-tr "List the bookmarks in the browser"))
  (insert "\n"))

(defun lispm-desktop-settings--proxy-server ()
  "Ask for the proxy's address, HOST:PORT, and keep it; return non-nil
when one is kept."
  (let ((server (string-trim (read-string (lispm-desktop-tr "Proxy server (host:port): ") lispm-desktop-proxy-server))))
    (cond ((string-empty-p server) nil)
          ((lispm-desktop-proxy-parse-server server)
           (lispm-desktop-settings--save 'lispm-desktop-proxy-server server)
           t)
          (t (user-error (lispm-desktop-tr "Write the proxy server as host:port, as 127.0.0.1:7890"))))))

(defun lispm-desktop-settings--proxy-type (type)
  "Send LispM's connections through a proxy of TYPE, `http' or `socks5', or
through none when TYPE is nil.  The proxy's address is asked for when
none is known yet."
  (when (or (null type)
            (lispm-desktop-proxy-parse-server lispm-desktop-proxy-server)
            (lispm-desktop-settings--proxy-server))
    (lispm-desktop-settings--save 'lispm-desktop-proxy-type type)))

(defun lispm-desktop-settings--proxy-exceptions ()
  "Ask for the hosts reached directly, not through the proxy, and keep them."
  (lispm-desktop-settings--save
   'lispm-desktop-proxy-exceptions
   (split-string (read-string (lispm-desktop-tr "Hosts reached directly (separated by commas): ")
                              (string-join lispm-desktop-proxy-exceptions ", "))
                 "[, ]+" t)))

(defun lispm-desktop-settings--network ()
  "Insert the Network panel: the proxy, none, HTTP or SOCKS5, its address,
and the hosts reached directly."
  (lispm-desktop-settings--heading (lispm-desktop-tr "Proxy"))
  (pcase-dolist (`(,type ,label ,note) `((nil ,(lispm-desktop-tr "None") ,(lispm-desktop-tr "the system's own, if any"))
                                         (http "HTTP" ,(lispm-desktop-tr "HTTPS goes through it too"))
                                         (socks5 "SOCKS5" ,(lispm-desktop-tr "it looks the names up too"))))
    (lispm-desktop-settings--radio label (eq lispm-desktop-proxy-type type)
                                   (lambda () (lispm-desktop-settings--proxy-type type))
                                   note))
  (insert "\n")
  (lispm-desktop-settings--note (if lispm-desktop-proxy-server
                                    (lispm-desktop-tr "Server: %s" lispm-desktop-proxy-server)
                                  (lispm-desktop-tr "No server yet.")))
  (insert (lispm-desktop-settings--indent))
  (lispm-desktop-settings--button (lispm-desktop-tr "Change…") #'lispm-desktop-settings--proxy-server
                                  (lispm-desktop-tr "Change the proxy's address, host:port"))
  (insert "\n\n")
  (lispm-desktop-settings--paragraph
   (lispm-desktop-tr "Emacs, the browser's pages and the programs Emacs starts all connect through it."))
  (when (and lispm-desktop-proxy-type (lispm-desktop-settings--browser-p)
             (fboundp 'lispm-browser-proxy-followed-p)
             (not (lispm-browser-proxy-followed-p)))
    (lispm-desktop-settings--paragraph (lispm-desktop-tr "The browser's pages follow once its module is built.")))
  (lispm-desktop-settings--heading (lispm-desktop-tr "Direct Connections"))
  (lispm-desktop-settings--note
   (if lispm-desktop-proxy-exceptions
       (lispm-desktop-settings--truncate (string-join lispm-desktop-proxy-exceptions ", ") 58)
     (lispm-desktop-tr "None but this computer's own addresses.")))
  (insert (lispm-desktop-settings--indent))
  (lispm-desktop-settings--button (lispm-desktop-tr "Change…") #'lispm-desktop-settings--proxy-exceptions
                                  (lispm-desktop-tr "Change the hosts reached directly, not through the proxy"))
  (insert "\n\n")
  (lispm-desktop-settings--paragraph
   (lispm-desktop-tr "Hosts reached directly, not through the proxy, and their subdomains; this computer's own addresses always are.")))

(defun lispm-desktop-settings--fit (text width)
  "Return TEXT as it fits in WIDTH pixels when dimmed: whole, or its first
words and an ellipsis."
  (if (<= (lispm-desktop-platinum-content-width text 'shadow) width)
      text
    (concat (car (lispm-desktop-settings--wrap text (max 20 (- width (lispm-desktop-px 12))))) "…")))

(defun lispm-desktop-settings--note-room (label)
  "Return the pixels left for a note after a radio button or check box
named LABEL in a row of settings."
  (- (lispm-desktop-settings--text-width (lispm-desktop-px 48))
     (lispm-desktop-platinum-content-width label 'default)))

(defun lispm-desktop-settings--model-items ()
  "Return the items of the menu choosing the model LispM's agents use: the
models of each backend, a separator between backends, then Other Model…."
  (let* ((status (lispm-model-status))
         (backend (plist-get status :backend))
         (model (plist-get status :model))
         (choices (lispm-model-choices))
         (backends (delete-dups (mapcar (lambda (choice) (plist-get choice :backend)) choices)))
         (items nil))
    (dolist (name backends)
      (when items
        (push 'separator items))
      (dolist (choice choices)
        (when (eq (plist-get choice :backend) name)
          (let ((label (or (plist-get choice :label) (plist-get choice :model))))
            (push (list :label (if (cdr backends) (format "%s (%s)" label name) label)
                        :checked (and (eq name backend) (equal (plist-get choice :model) model))
                        :action (lambda () (lispm-set-model name (plist-get choice :model) t)))
                  items)))))
    (when items
      (push 'separator items))
    (push (list :label (lispm-desktop-tr "Other Model…") :enabled (and backend t)
                :action #'lispm-desktop-settings--other-model)
          items)
    (nreverse items)))

(defun lispm-desktop-settings--other-model ()
  "Ask for a model of the backend LispM's agents use, by its name, and have
them use it, now and at later starts."
  (let* ((status (lispm-model-status))
         (backend (plist-get status :backend))
         (model (string-trim (read-string (lispm-desktop-tr "Model of %s: " backend) (plist-get status :model)))))
    (unless (string-empty-p model)
      (lispm-set-model backend model t))))

(defun lispm-desktop-settings--choose-model ()
  "Pop up the models to choose from under the button pressed, and have
LispM's agents use the one chosen, now and at later starts."
  (let ((position (if (mouse-event-p last-input-event)
                      (posn-point (event-start last-input-event))
                    (point))))
    (pcase-let ((`(,x . ,y) (lispm-desktop-popup-frame-xy position)))
      (lispm-desktop-popup-menu (lispm-desktop-settings--model-items)
                                x (+ y (if (lispm-desktop-tty-p) 1 (+ lispm-desktop-platinum-button-height 2)))))))

(defun lispm-desktop-settings--lispm-model ()
  "Insert the LispM panel's Model group: the model LispM's agents use, its
effort, how its backend stands, and what can be done about it."
  (let* ((status (lispm-model-status))
         (backend (plist-get status :backend))
         (model (plist-get status :model)))
    (lispm-desktop-settings--heading (lispm-desktop-tr "Model"))
    (insert (lispm-desktop-settings--indent))
    (if model
        (insert (propertize model 'face 'bold) "  "
                (propertize (lispm-desktop-tr "via %s" backend) 'face 'shadow) "  ")
      (insert (propertize (lispm-desktop-tr "None") 'face 'bold) "  "))
    (lispm-desktop-settings--button (lispm-desktop-tr "Choose…") #'lispm-desktop-settings--choose-model
                                    (lispm-desktop-tr "Choose the model agents use"))
    (insert "\n")
    (lispm-desktop-settings--paragraph (plist-get status :message))
    (insert "\n" (lispm-desktop-settings--indent) (propertize (lispm-desktop-tr "Effort: ") 'face 'shadow))
    (pcase-dolist (`(,effort . ,_) lispm-efforts)
      (let ((name (lispm-effort-name effort)))
        (lispm-desktop-settings--radio-button name (equal effort (plist-get status :effort))
                                              (lambda () (lispm-set-effort effort t))
                                              (lispm-desktop-tr "Choose %s" name))
        (insert "  ")))
    (insert "\n\n" (lispm-desktop-settings--indent))
    ;; What programs providing models offer: setting one up, a key.
    (pcase-dolist (`(,label ,function ,help) (lispm-backend-actions))
      (lispm-desktop-settings--button label function help)
      (insert " "))
    (lispm-desktop-settings--button (lispm-desktop-tr "Backends…") (lambda () (customize-variable 'lispm-backends))
                                    (lispm-desktop-tr "Edit the servers of models LispM knows in Customize"))
    (insert "\n\n")
    (lispm-desktop-settings--paragraph
     (lispm-desktop-tr "Agents whose role names no model take a change from their next request on; more effort thinks longer, and costs more."))))

(defun lispm-desktop-settings--permission-columns ()
  "Return where the Permissions table's columns start: an alist from each
action of `lispm-permission-actions' to its distance from the pane's
right edge, as `lispm-desktop-align-right' takes it, each column as wide
as its name."
  (let ((right (lispm-desktop-px (+ (* 2 lispm-desktop-settings--group-margin) 6)))
        (columns nil))
    (pcase-dolist (`(,action . ,_) (reverse lispm-permission-actions))
      (let ((name (lispm-permission-action-name action)))
        (setq right (+ right (lispm-desktop-px 14)
                       (max (lispm-desktop-platinum-content-width name 'shadow) (lispm-desktop-px 18))))
        (push (cons action right) columns)))
    columns))

(defun lispm-desktop-settings--untrust-skill ()
  "Ask for a project's skill whose code runs unasked, and have agents ask
before running it again."
  (let ((files (mapcar #'abbreviate-file-name (lispm-trusted-skill-files))))
    (lispm-untrust-skill (completing-read (lispm-desktop-tr "Trust no longer the skill: ") files nil t))))

(defun lispm-desktop-settings--lispm-permissions ()
  "Insert the LispM panel's Permissions group: what the default policy
lets each class of tools do, as a table of radio buttons, the projects'
skills whose code is trusted, and the way to the Sandbox panel."
  (let* ((columns (lispm-desktop-settings--permission-columns))
         (window (get-buffer-window (current-buffer) t))
         (room (- (if window (window-body-width window t) 500)
                  (lispm-desktop-px 24) (cdar columns) (lispm-desktop-px 8))))
    (lispm-desktop-settings--heading (lispm-desktop-tr "Permissions"))
    (lispm-desktop-settings--paragraph (lispm-desktop-tr "What agents may do without asking you:"))
    (insert (lispm-desktop-settings--indent))
    (pcase-dolist (`(,action . ,right) columns)
      (insert (propertize " " 'display `(space :align-to ,(lispm-desktop-align-right right)))
              (propertize (lispm-permission-action-name action) 'face 'shadow)))
    (insert "\n")
    (pcase-dolist (`(,class . ,_) lispm-permission-classes)
      (let ((chosen (lispm-default-permission class))
            (description (lispm-permission-class-description class)))
        (insert (lispm-desktop-settings--indent) (lispm-desktop-settings--fit description room))
        (pcase-dolist (`(,action . ,right) columns)
          (insert (propertize " " 'display `(space :align-to ,(lispm-desktop-align-right right))))
          (lispm-desktop-settings--radio-button
           "" (eq chosen action) (lambda () (lispm-set-default-permission class action t))
           (lispm-desktop-tr "%s: %s" (lispm-permission-action-name action) description)))
        (insert "\n")))
    (insert "\n")
    (lispm-desktop-settings--paragraph (lispm-desktop-tr "A role's own policy comes first for its agents."))
    (insert "\n")
    (if-let* ((files (lispm-trusted-skill-files)))
        (progn
          (lispm-desktop-settings--paragraph
           (lispm-desktop-tr "Projects' skills whose code runs unasked: %s."
                             (lispm-desktop-settings--sandbox-entries
                              (mapcar (lambda (file) (file-name-base (directory-file-name file))) files) 40)))
          (insert (lispm-desktop-settings--indent))
          (lispm-desktop-settings--button (lispm-desktop-tr "Remove…") #'lispm-desktop-settings--untrust-skill
                                          (lispm-desktop-tr "Have agents ask again before running a skill's code"))
          (insert "\n"))
      (lispm-desktop-settings--paragraph
       (lispm-desktop-tr "A project's skill runs its code once you trust it; you trust none yet.")))
    (when (lispm-desktop-settings--sandbox-p)
      (insert "\n" (lispm-desktop-settings--indent))
      (lispm-desktop-settings--button (lispm-desktop-tr "Sandbox…") (lambda () (lispm-desktop-settings-select 'sandbox))
                                      (lispm-desktop-tr "Show the Sandbox panel"))
      (insert "  " (propertize (lispm-desktop-tr "what commands may do once they run") 'face 'shadow) "\n"))))

(defun lispm-desktop-settings--lispm-memory ()
  "Insert the LispM panel's Memory group: how large conversations grow
before they are summed up, whether memory is saved first, git's history,
and Show Memory…."
  (let ((size (lispm-compact-size)))
    (lispm-desktop-settings--heading (lispm-desktop-tr-in "agents' memory" "Memory"))
    (lispm-desktop-settings--paragraph
     (lispm-desktop-tr "A conversation grown past its size is summed up, and goes on from the summary:"))
    (pcase-dolist (`(,choice . ,_) lispm-compact-sizes)
      (let ((name (lispm-compact-size-name choice)))
        (lispm-desktop-settings--radio name (eq size choice) (lambda () (lispm-set-compact-size choice t))
                                       (lispm-desktop-settings--fit (lispm-compact-size-note choice)
                                                                    (lispm-desktop-settings--note-room name)))))
    (unless size
      (lispm-desktop-settings--radio (lispm-desktop-tr "Your own size") t #'ignore
                                     (lispm-desktop-tr "summed up past %d tokens" lispm-compact-threshold)))
    (insert "\n")
    (lispm-desktop-settings--check-box
     (lispm-desktop-tr "Save memory before summing up") lispm-compact-save-memory
     (lambda () (lispm-settings-set 'lispm-compact-save-memory (not lispm-compact-save-memory) t))
     (lispm-desktop-tr "what should outlive the conversation"))
    (lispm-desktop-settings--check-box
     (lispm-desktop-tr "Keep a history in git") lispm-git
     (lambda () (lispm-settings-set 'lispm-git (not lispm-git) t))
     (lispm-desktop-tr "each change a commit"))
    (insert "\n" (lispm-desktop-settings--indent))
    (lispm-desktop-settings--button (lispm-desktop-tr "Show Memory…") (lambda () (call-interactively #'lispm-open-memory))
                                    (lispm-desktop-tr "Open the memory of a project or a role, or a task list"))
    (insert "\n")))

(defun lispm-desktop-settings--add-skill-directory ()
  "Ask for a directory of skills, and add it to those agents read, last."
  (lispm-add-skill-directory (read-directory-name (lispm-desktop-tr "Directory of skills: ") "~/" nil t) t))

(defun lispm-desktop-settings--remove-skill-directory ()
  "Ask for a directory of skills, not LispM's own, and take it out of those
agents read; its skills stay where they are."
  (let ((directories (mapcar #'abbreviate-file-name
                             (seq-remove #'lispm-skill-directory-builtin-p lispm-skill-directories))))
    (lispm-remove-skill-directory
     (completing-read (lispm-desktop-tr "Take out the directory of skills: ") directories nil t nil nil
                      (car (last directories)))
     t)))

(defun lispm-desktop-settings--lispm-roles ()
  "Insert the LispM panel's Roles & Skills group: the role agents start
with, and the directories of skills, with Add… and Remove…."
  (lispm-desktop-settings--heading (lispm-desktop-tr "Roles & Skills"))
  (lispm-desktop-settings--paragraph (lispm-desktop-tr "Agents started without choosing a role are:"))
  (dolist (role (lispm-roles))
    (let ((name (symbol-name (lispm-role-name role)))
          (description (lispm-role-description role)))
      (lispm-desktop-settings--radio name (eq (lispm-role-name role) lispm-default-role)
                                     (lambda () (lispm-set-default-role (lispm-role-name role) t))
                                     (and description
                                          (lispm-desktop-settings--fit description
                                                                       (lispm-desktop-settings--note-room name))))))
  (insert "\n")
  (lispm-desktop-settings--paragraph
   (lispm-desktop-tr "Skills come from these directories, a later one's replacing an earlier one's of the same name:"))
  (dolist (directory lispm-skill-directories)
    (let ((count (lispm-skill-directory-count directory))
          (name (if (lispm-skill-directory-builtin-p directory)
                    (lispm-desktop-tr "LispM's own skills")
                  (abbreviate-file-name directory))))
      (insert (lispm-desktop-settings--indent)
              (lispm-desktop-settings--truncate name 48) "  "
              (propertize (if (file-directory-p directory)
                              (lispm-desktop-tr-n "%d skill" "%d skills" count count)
                            (lispm-desktop-tr "not there yet"))
                          'face 'shadow)
              "\n")))
  (insert "\n" (lispm-desktop-settings--indent))
  (lispm-desktop-settings--button (lispm-desktop-tr "Add…") #'lispm-desktop-settings--add-skill-directory
                                  (lispm-desktop-tr "Add a directory of skills"))
  (when (seq-remove #'lispm-skill-directory-builtin-p lispm-skill-directories)
    (insert " ")
    (lispm-desktop-settings--button (lispm-desktop-tr "Remove…") #'lispm-desktop-settings--remove-skill-directory
                                    (lispm-desktop-tr "Take a directory out of those skills come from")))
  (insert "\n"))

(defun lispm-desktop-settings--lispm ()
  "Insert the LispM panel: the model LispM's agents use, what they may do
without asking, how their conversations are summed up and remembered,
and their roles and skills.  LispM Agent's lispm-settings.el does the
rest; programs providing models add to the Model group through it."
  (lispm-desktop-settings--lispm-model)
  (lispm-desktop-settings--lispm-permissions)
  (lispm-desktop-settings--lispm-memory)
  (lispm-desktop-settings--lispm-roles))

(defun lispm-desktop-settings--sandbox-level (level)
  "Run agents' shell commands and MCP servers at sandbox LEVEL, now and at
every later start."
  (lispm-sandbox-set-level level t))

(defun lispm-desktop-settings--sandbox-domain-p (domain)
  "Return non-nil when srt can let commands reach DOMAIN: a name such as
example.com, *.example.com for its subdomains, or example.com:443."
  (or (equal domain "localhost")
      (string-match-p "\\`\\(?:\\*\\.\\)?[[:alnum:]-]+\\(?:\\.[[:alnum:]-]+\\)+\\(?::[0-9]+\\)?\\'"
                      domain)))

(defun lispm-desktop-settings--sandbox-read-domains (prompt domains &optional deny)
  "Ask with PROMPT for domains, separated by commas, DOMAINS first; return
them.  Those srt cannot allow are refused, or deny when DENY: it may
deny every domain, *."
  (let* ((domains (split-string (read-string prompt (string-join domains ", ")) "[, ]+" t))
         (wrong (seq-remove (lambda (domain)
                              (or (lispm-desktop-settings--sandbox-domain-p domain)
                                  (and deny (equal domain "*"))))
                            domains)))
    (when wrong
      (user-error (if deny
                      (lispm-desktop-tr "srt cannot deny %s: write a domain as example.com, *.example.com or example.com:443")
                    (lispm-desktop-tr "srt cannot allow %s: write a domain as example.com, *.example.com or example.com:443"))
                  (string-join wrong ", ")))
    domains))

(defun lispm-desktop-settings--sandbox-domains ()
  "Ask for the domains sandboxed commands may reach at Low and Middle, and
keep them."
  (lispm-desktop-settings--save
   'lispm-sandbox-allowed-domains
   (lispm-desktop-settings--sandbox-read-domains
    (lispm-desktop-tr "Domains sandboxed commands may reach (separated by commas): ")
    lispm-sandbox-allowed-domains)))

(defun lispm-desktop-settings--sandbox-custom-p ()
  "Return non-nil when LispM Agent has the sandbox level Custom, of rules
of the user's own: LispM Agent 0.4.0 or later."
  (and (assq 'custom lispm-sandbox-levels) (fboundp 'lispm-sandbox-rules)))

(defun lispm-desktop-settings--sandbox-description (level)
  "Return what LEVEL allows, as a sentence; Custom's is said from its rules."
  (let ((description (if (fboundp 'lispm-sandbox-level-description)
                         (lispm-sandbox-level-description level)
                       (alist-get level lispm-sandbox-levels))))
    (lispm-desktop-tr "%s." (concat (upcase (substring description 0 1)) (substring description 1)))))

(defconst lispm-desktop-settings--sandbox-lists
  `((:deny-read ,(lispm-desktop-tr-noop "Not readable") file
                ,(lispm-desktop-tr-noop "Change the files and directories commands may not read"))
    (:allow-read ,(lispm-desktop-tr-noop "Readable after all") file
                 ,(lispm-desktop-tr-noop "Change the places inside those that commands may read after all"))
    (:allow-write ,(lispm-desktop-tr-noop "Writable") file
                  ,(lispm-desktop-tr-noop "Change where commands may write, and nowhere else"))
    (:deny-write ,(lispm-desktop-tr-noop "Not writable") file
                 ,(lispm-desktop-tr-noop "Change the places inside those that commands may not write"))
    (:allowed-domains ,(lispm-desktop-tr-noop "Reachable domains") allow
                      ,(lispm-desktop-tr-noop "Change the domains commands may reach, and no others"))
    (:denied-domains ,(lispm-desktop-tr-noop "Refused domains") deny
                     ,(lispm-desktop-tr-noop "Change the domains refused even when allowed"))
    (:unix-sockets ,(lispm-desktop-tr-noop "Unix sockets") file
                   ,(lispm-desktop-tr-noop "Change the Unix sockets commands may connect to")))
  "The Custom level's rules that are lists, as the Sandbox panel shows them:
\(KEY LABEL KIND HELP), LABEL and HELP in English, translated where shown.
KIND is `file' for file names, `allow' or `deny' for domains to allow or
deny.  See `lispm-sandbox-custom'.")

(defconst lispm-desktop-settings--sandbox-switches
  `((:local-binding ,(lispm-desktop-tr-noop "Serve on local ports, and reach this computer's") nil)
    (:pty ,(lispm-desktop-tr-noop "Use pseudo-terminals") darwin)
    (:apple-events ,(lispm-desktop-tr-noop "Send Apple Events and open applications") darwin))
  "The Custom level's rules that are on or off, as the Sandbox panel shows
them: (KEY LABEL SYSTEM), LABEL in English, translated where shown.
SYSTEM, when given, is the only `system-type' the rule means something
on.  See `lispm-sandbox-custom'.")

(defun lispm-desktop-settings--sandbox-rule (key value)
  "Set the Custom level's rule KEY to VALUE, now and for later starts.
The other rules are kept: Middle's while there were none of the user's."
  (let ((rules (copy-tree (lispm-sandbox-rules 'custom))))
    (lispm-desktop-settings--save 'lispm-sandbox-custom (plist-put rules key value))))

(defun lispm-desktop-settings--sandbox-change-list (key)
  "Ask for the entries of the Custom level's rule KEY, a list, and keep
them.  File names are separated by commas, domains by commas or spaces."
  (pcase-let* ((`(,label ,kind . ,_) (alist-get key lispm-desktop-settings--sandbox-lists))
               (entries (plist-get (lispm-sandbox-rules 'custom) key))
               (prompt (lispm-desktop-tr "%s (separated by commas): " (lispm-desktop-tr label))))
    (lispm-desktop-settings--sandbox-rule
     key (if (eq kind 'file)
             (split-string (read-string prompt (string-join entries ", ")) "," t "[ \t\n]+")
           (lispm-desktop-settings--sandbox-read-domains prompt entries (eq kind 'deny))))))

(defun lispm-desktop-settings--sandbox-start-from (level)
  "Make the Custom level's rules those of LEVEL, now and for later starts.
Rules of the user's own are replaced only once the user says so."
  (when (or (null lispm-sandbox-custom)
            (lispm-desktop-popup-alert
             'caution (lispm-desktop-tr "Replace your Custom rules with %s's?" (lispm-sandbox-level-name level))
             `((,(lispm-desktop-tr "Cancel") nil cancel) (,(lispm-desktop-tr "Replace") t default))
             (lispm-desktop-tr "The rules you set are lost.")))
    (lispm-desktop-settings--save 'lispm-sandbox-custom (copy-tree (lispm-sandbox-rules level)))))

(defun lispm-desktop-settings--sandbox-entries (entries width)
  "Return ENTRIES, strings, as a list in WIDTH characters at most: the first
few, and how many more there are."
  (if (null entries)
      (lispm-desktop-tr "none")
    (let ((more (lambda (shown)
                  (if (< shown (length entries))
                      (lispm-desktop-tr "%s and %d more" (string-join (seq-take entries shown) ", ")
                                        (- (length entries) shown))
                    (string-join (seq-take entries shown) ", ")))))
      (or (seq-some (lambda (shown)
                      (let ((text (funcall more shown)))
                        (and (<= (length text) width) text)))
                    (number-sequence (length entries) 1 -1))
          (lispm-desktop-settings--truncate (funcall more 1) width)))))

(defun lispm-desktop-settings--sandbox-custom ()
  "Insert the Custom level's rules, each with what changes it: a Change…
button for each list, a check box for each switch."
  (let ((rules (lispm-sandbox-rules 'custom)))
    (lispm-desktop-settings--heading (lispm-desktop-tr "Custom Rules"))
    (insert (lispm-desktop-settings--indent) (propertize (lispm-desktop-tr "Start from: ") 'face 'shadow))
    (dolist (level '(low middle high))
      (let ((name (lispm-sandbox-level-name level)))
        (lispm-desktop-settings--button name (lambda () (lispm-desktop-settings--sandbox-start-from level))
                                        (lispm-desktop-tr "Make the Custom rules %s's" name))
        (insert " ")))
    (insert "\n\n")
    (pcase-dolist (`(,key ,label ,_ ,help) lispm-desktop-settings--sandbox-lists)
      (let ((label (lispm-desktop-tr "%s: " (lispm-desktop-tr label))))
        (insert (lispm-desktop-settings--indent))
        (lispm-desktop-settings--button (lispm-desktop-tr "Change…")
                                        (lambda () (lispm-desktop-settings--sandbox-change-list key))
                                        (lispm-desktop-tr help))
        (insert "  " label
                (propertize (lispm-desktop-settings--sandbox-entries
                             ;; Relative file names are the project's.
                             (mapcar (lambda (entry) (if (equal entry ".") (lispm-desktop-tr "the project") entry))
                                     (plist-get rules key))
                             (max 12 (- 52 (length label))))
                            'face 'shadow)
                "\n")))
    (insert "\n")
    (pcase-dolist (`(,key ,label ,system) lispm-desktop-settings--sandbox-switches)
      (when (or (null system) (eq system system-type))
        (lispm-desktop-settings--check-box
         (lispm-desktop-tr label) (plist-get rules key)
         (lambda () (lispm-desktop-settings--sandbox-rule key (not (plist-get (lispm-sandbox-rules 'custom) key)))))
        (when (eq key :apple-events)
          (lispm-desktop-settings--paragraph
           (lispm-desktop-tr "Caution: the applications they open run outside the sandbox.")
           (lispm-desktop-settings--under-label 'check-box)))))
    (insert "\n")
    (when-let* ((wrong (condition-case error
                           (progn (lispm-sandbox-settings 'custom) nil)
                         (error (error-message-string error)))))
      (lispm-desktop-settings--paragraph (concat wrong "."))
      (insert "\n"))
    (lispm-desktop-settings--paragraph
     (lispm-desktop-tr "Whatever the rules, commands write their temporary directory, and never where Emacs and LispM keep their files."))
    (when (fboundp 'lispm-sandbox-customize)
      (insert (lispm-desktop-settings--indent))
      (lispm-desktop-settings--button (lispm-desktop-tr "Customize…") #'lispm-sandbox-customize
                                      (lispm-desktop-tr "Edit the Custom rules in Customize"))
      (insert "\n"))))

(defun lispm-desktop-settings--sandbox-srt (status)
  "Return what the Sandbox panel says of srt, the program confining
commands, given its STATUS (see `lispm-sandbox-srt-status'): sentences."
  (cons (pcase status
          ('ready (if lispm-sandbox-srt-program
                      (lispm-desktop-tr "srt, Anthropic's sandbox runtime, runs from %s."
                                        (abbreviate-file-name lispm-sandbox-srt-program))
                    (lispm-desktop-tr "srt %s, Anthropic's sandbox runtime, is installed." lispm-sandbox-srt-version)))
          ('installing (lispm-desktop-tr "Installing srt %s, Anthropic's sandbox runtime, with npm…"
                                         lispm-sandbox-srt-version))
          (`(failed . ,why) (lispm-desktop-tr "Sandbox unavailable: %s."
                                              (lispm-desktop-settings--truncate
                                               (string-trim why "[ \t\n]+" "[ \t\n.]+") 240)))
          (_ (lispm-desktop-tr "Any level but None installs srt, Anthropic's sandbox runtime, with npm first.")))
        (unless (or (eq status 'ready) (eq lispm-sandbox-level 'none))
          (list (lispm-desktop-tr "Until it runs, agents' commands fail rather than run unconfined.")))))

(defun lispm-desktop-settings--sandbox ()
  "Insert the Sandbox panel: how strictly agents' shell commands and MCP
servers are confined, whether srt is there to confine them, the rules
of the level Custom while it is chosen, and the domains they may reach.
LispM Agent's lispm-sandbox.el does the rest."
  (lispm-desktop-settings--heading (lispm-desktop-tr "Sandbox Level"))
  (lispm-desktop-settings--paragraph (lispm-desktop-tr "How far agents' shell commands and MCP servers are confined:"))
  (pcase-dolist (`(,level . ,_) lispm-sandbox-levels)
    (lispm-desktop-settings--radio (lispm-sandbox-level-name level) (eq lispm-sandbox-level level)
                                   (lambda () (lispm-desktop-settings--sandbox-level level)))
    (lispm-desktop-settings--paragraph (lispm-desktop-settings--sandbox-description level)
                                       (lispm-desktop-settings--under-label 'radio)))
  (insert "\n")
  (when (fboundp 'lispm-sandbox-srt-status)
    (let ((status (lispm-sandbox-srt-status)))
      (mapc #'lispm-desktop-settings--paragraph (lispm-desktop-settings--sandbox-srt status))
      (when (and (eq (car-safe status) 'failed) (not lispm-sandbox-srt-program))
        (insert (lispm-desktop-settings--indent))
        (lispm-desktop-settings--button (lispm-desktop-tr "Try Again") #'lispm-sandbox-install
                                        (lispm-desktop-tr "Install srt again"))
        (insert "\n"))
      (insert "\n")))
  (lispm-desktop-settings--paragraph
   (lispm-desktop-tr "Running commands take a new level's network rules at once, its file rules when they start again."))
  (when (and (eq lispm-sandbox-level 'custom) (lispm-desktop-settings--sandbox-custom-p))
    (lispm-desktop-settings--sandbox-custom))
  (lispm-desktop-settings--heading (lispm-desktop-tr "Allowed Domains"))
  (lispm-desktop-settings--note
   (if lispm-sandbox-allowed-domains
       (lispm-desktop-settings--truncate (string-join lispm-sandbox-allowed-domains ", ") 58)
     (lispm-desktop-tr "None.")))
  (insert (lispm-desktop-settings--indent))
  (lispm-desktop-settings--button (lispm-desktop-tr "Change…") #'lispm-desktop-settings--sandbox-domains
                                  (lispm-desktop-tr "Change the domains sandboxed commands may reach"))
  (insert "\n\n")
  (lispm-desktop-settings--paragraph
   (lispm-desktop-tr "Low reaches them besides package registries and code hosts, Middle them alone; High reaches no network.")))

(defvar lispm-desktop-settings--kernel-timer nil
  "Timer drawing the Kernel panel again while it tells how far lispm-kernel is.")

(defun lispm-desktop-settings--kernel-later ()
  "Draw the Kernel panel again in a moment, while it is shown."
  (unless (timerp lispm-desktop-settings--kernel-timer)
    (setq lispm-desktop-settings--kernel-timer
          (run-at-time 0.5 nil
                       (lambda ()
                         (setq lispm-desktop-settings--kernel-timer nil)
                         (when-let* (((lispm-desktop-settings-open-p))
                                     (buffer (get-buffer lispm-desktop-settings-buffer-name))
                                     ((eq (buffer-local-value 'lispm-desktop-settings--panel buffer) 'kernel)))
                           (lispm-desktop-settings--render)))))))

(defun lispm-desktop-settings--kernel-status ()
  "Insert how lispm-kernel stands, and the button doing what can be done
about it; keep drawing it while it changes by itself."
  (pcase (lispm-desktop-kernel-status)
    ('(off)
     (lispm-desktop-settings--paragraph (lispm-desktop-tr "Off: Emacs reads the sessions itself.")))
    (`(installing ,fraction)
     (lispm-desktop-settings--paragraph
      (if fraction
          (lispm-desktop-tr "Getting what it needs… %d%%" (floor (* 100 fraction)))
        (lispm-desktop-tr "Getting what it needs…")))
     (insert "\n" (lispm-desktop-settings--indent))
     (lispm-desktop-settings--button (lispm-desktop-tr "Show Installer") #'lispm-desktop-needs-show
                                     (lispm-desktop-tr "Show how far the Installer is"))
     (insert "\n")
     (lispm-desktop-settings--kernel-later))
    (`(running ,pid ,since)
     (lispm-desktop-settings--paragraph
      (let ((version (lispm-desktop-kernel-version)))
        (cond ((and pid since version)
               (lispm-desktop-tr "Version %s, running as process %d since %s." version pid
                                 (lispm-desktop-settings--when since)))
              ((and pid since)
               (lispm-desktop-tr "Running as process %d since %s." pid (lispm-desktop-settings--when since)))
              (version (lispm-desktop-tr "Version %s, running." version))
              (t (lispm-desktop-tr "Running.")))))
     (insert "\n" (lispm-desktop-settings--indent))
     (lispm-desktop-settings--button (lispm-desktop-tr "Restart") #'lispm-desktop-kernel-restart
                                     (lispm-desktop-tr "Stop lispm-kernel and start it again"))
     (insert "\n"))
    (`(exited ,why ,at)
     (lispm-desktop-settings--paragraph
      (concat (lispm-desktop-tr "It stopped: %s." why) "  "
              (if (> at (float-time))
                  (lispm-desktop-tr "It starts again by itself %s." (lispm-desktop-settings--when at))
                (lispm-desktop-tr "It starts again by itself once the sidebar reads sessions."))))
     (insert "\n" (lispm-desktop-settings--indent))
     (lispm-desktop-settings--button (lispm-desktop-tr "Restart Now") #'lispm-desktop-kernel-restart
                                     (lispm-desktop-tr "Start lispm-kernel again now"))
     (insert "\n")
     (lispm-desktop-settings--kernel-later))
    (`(stopped ,why)
     (lispm-desktop-settings--paragraph
      (if why
          (lispm-desktop-tr "It does not run: %s." (string-remove-suffix "." why))
        (lispm-desktop-tr "It does not run.")))
     (insert "\n" (lispm-desktop-settings--indent))
     (lispm-desktop-settings--button (lispm-desktop-tr "Try Again") #'lispm-desktop-kernel-restart
                                     (lispm-desktop-tr "Try again to turn lispm-kernel on"))
     (insert "\n"))))

(defun lispm-desktop-settings--kernel ()
  "Insert the Kernel panel: whether LispM Kernel runs LispM's background
work, and how it stands, a change taking effect at once; then the Emacs
LispM runs on, which LispM.app switches."
  (lispm-desktop-settings--heading (lispm-desktop-tr "LispM Kernel"))
  (lispm-desktop-settings--paragraph
   (lispm-desktop-tr "LispM's own runtime, a Common Lisp program beside Emacs: it reads the agents' sessions in the background, so that Emacs never waits on them."))
  (lispm-desktop-settings--check-box
   (lispm-desktop-tr "Use LispM Kernel") lispm-desktop-kernel
   (lambda () (lispm-desktop-settings--save 'lispm-desktop-kernel (not lispm-desktop-kernel))))
  (insert "\n")
  (lispm-desktop-settings--kernel-status)
  (insert "\n")
  (lispm-desktop-settings--paragraph
   (lispm-desktop-tr "A change takes effect at once, and nothing needs restarting.  What it lacks, the Installer gets."))
  (lispm-desktop-settings--emacs-kernel))

(defun lispm-desktop-settings--startup-read-item ()
  "Ask what LispM is to open at startup; return it as a startup item, see
`lispm-desktop-startup-items'."
  (let* ((kinds `((,(lispm-desktop-tr "Application") . app) (,(lispm-desktop-tr "Document or folder") . file)
                  (,(lispm-desktop-tr "Program, run in the background") . program)
                  (,(lispm-desktop-tr "Emacs command") . command)))
         (kind (or (cdr (assoc (completing-read (lispm-desktop-tr "Open at startup: ") kinds nil t) kinds))
                   (user-error (lispm-desktop-tr "Choose what to open")))))
    (pcase kind
      ('app
       (let ((apps (mapcar (pcase-lambda (`(,key . ,properties))
                             (cons (lispm-desktop-tr (plist-get properties :name)) key))
                           lispm-desktop-apps)))
         (list :app (or (cdr (assoc (completing-read (lispm-desktop-tr "Application: ") apps nil t) apps))
                        (user-error (lispm-desktop-tr "Choose an application"))))))
      ('file
       (list :file (abbreviate-file-name (read-file-name (lispm-desktop-tr "Document or folder: ") nil nil t))))
      ('program
       (let ((command (string-trim (read-string (lispm-desktop-tr "Program, as a shell command: ")))))
         (when (string-empty-p command)
           (user-error (lispm-desktop-tr "Type the command running the program")))
         (let* ((directory (abbreviate-file-name
                            (file-name-as-directory
                             (read-directory-name (lispm-desktop-tr "Run it in: ") "~/" nil t))))
                (default (lispm-desktop-startup-program-name command))
                (name (string-trim (read-string (lispm-desktop-tr "Name: ") default))))
           `(:program ,command
             ,@(unless (equal directory "~/") (list :directory directory))
             ,@(unless (or (string-empty-p name) (equal name default)) (list :name name))))))
      ('command
       (list :command (intern (completing-read (lispm-desktop-tr "Emacs command: ") obarray #'commandp t)))))))

(defun lispm-desktop-settings--startup-add ()
  "Ask what LispM is to open at startup, and add it to the startup items,
after the others."
  (let* ((item (lispm-desktop-settings--startup-read-item))
         (name (lispm-desktop-startup-item-name item)))
    (when (member name (mapcar #'lispm-desktop-startup-item-name lispm-desktop-startup-items))
      (user-error (lispm-desktop-tr "There is already a startup item called “%s”") name))
    (lispm-desktop-settings--save 'lispm-desktop-startup-items
                                  (append lispm-desktop-startup-items (list item)))))

(defun lispm-desktop-settings--startup-choose (prompt)
  "Ask with PROMPT for one of the startup items, the first by default;
return its index."
  (let ((names (or (mapcar #'lispm-desktop-startup-item-name lispm-desktop-startup-items)
                   (user-error (lispm-desktop-tr "There are no startup items")))))
    (seq-position names (completing-read prompt names nil t nil nil (car names)))))

(defun lispm-desktop-settings--startup-remove ()
  "Ask for a startup item and take it out of the startup items.  What it
opened stays open; a program it runs runs on."
  (let ((index (lispm-desktop-settings--startup-choose (lispm-desktop-tr "Remove the startup item: "))))
    (lispm-desktop-settings--save 'lispm-desktop-startup-items
                                  (append (seq-take lispm-desktop-startup-items index)
                                          (seq-drop lispm-desktop-startup-items (1+ index))))))

(defun lispm-desktop-settings--startup-open ()
  "Ask for a startup item and open it now; a program's output shows."
  (lispm-desktop-startup-open-item
   (nth (lispm-desktop-settings--startup-choose (lispm-desktop-tr "Open the startup item: ")) lispm-desktop-startup-items)
   t))

(defun lispm-desktop-settings--startup-toggle (index)
  "Turn the startup item at INDEX off when it is on, on when it is off."
  (lispm-desktop-settings--save
   'lispm-desktop-startup-items
   (seq-map-indexed (lambda (item i)
                      (if (= i index)
                          (lispm-desktop-startup-item-with item :disabled (not (plist-get item :disabled)))
                        item))
                    lispm-desktop-startup-items)))

(defun lispm-desktop-settings--startup-item-note (item)
  "Return what the Startup panel says of ITEM after its name."
  (pcase (lispm-desktop-startup-item-kind item)
    ('app (lispm-desktop-tr "application"))
    ('file (let* ((file (expand-file-name (plist-get item :file)))
                  (in (abbreviate-file-name (file-name-directory (directory-file-name file)))))
             (if (file-directory-p file)
                 (lispm-desktop-tr "folder in %s" in)
               (lispm-desktop-tr "document in %s" in))))
    ;; How it runs first, as the command may be cut short.
    ('program (lispm-desktop-tr "%s: %s"
                                (pcase (lispm-desktop-startup-program-status item)
                                  ('running (lispm-desktop-tr "running"))
                                  (`(exited . ,(and code (pred integerp))) (lispm-desktop-tr "stopped, exit %d" code))
                                  (`(exited . ,_) (lispm-desktop-tr "stopped"))
                                  (_ (lispm-desktop-tr "program")))
                                (plist-get item :program)))
    ('command (let ((command (symbol-name (plist-get item :command))))
                (if (equal command (lispm-desktop-startup-item-name item))
                    (lispm-desktop-tr "Emacs command")
                  (lispm-desktop-tr "Emacs command %s" command))))
    (_ (lispm-desktop-tr "names nothing to open"))))

(defun lispm-desktop-settings--startup ()
  "Insert the Startup panel: whether the computer opens LispM at login,
and the startup items LispM opens once started."
  (lispm-desktop-settings--heading (lispm-desktop-tr "Login"))
  (if (not (lispm-desktop-startup-login-file))
      (lispm-desktop-settings--paragraph (lispm-desktop-tr "This system gives LispM no way to open at login."))
    (lispm-desktop-settings--check-box
     (lispm-desktop-tr "Open LispM when you log in") (lispm-desktop-startup-login-p)
     (lambda () (lispm-desktop-open-at-login (not (lispm-desktop-startup-login-p)))))
    (lispm-desktop-settings--note
     (lispm-desktop-settings--truncate (lispm-desktop-tr "Opens %s." (lispm-desktop-startup-login-description)) 62))
    (when (eq system-type 'darwin)
      (lispm-desktop-settings--paragraph (lispm-desktop-tr "System Settings lists it in its Login Items."))))
  (lispm-desktop-settings--heading (lispm-desktop-tr "Startup Items"))
  (lispm-desktop-settings--paragraph (lispm-desktop-tr "Opened once LispM has started, in this order:"))
  (if (null lispm-desktop-startup-items)
      (lispm-desktop-settings--note (lispm-desktop-tr "None."))
    (seq-do-indexed
     (lambda (item index)
       (let ((name (lispm-desktop-startup-item-name item)))
         (lispm-desktop-settings--check-box
          name (not (plist-get item :disabled))
          (lambda () (lispm-desktop-settings--startup-toggle index))
          (lispm-desktop-settings--truncate (lispm-desktop-settings--startup-item-note item)
                                            (max 12 (- 54 (length name)))))))
     lispm-desktop-startup-items))
  (insert "\n" (lispm-desktop-settings--indent))
  (lispm-desktop-settings--button (lispm-desktop-tr "Add…") #'lispm-desktop-settings--startup-add
                                  (lispm-desktop-tr "Add an application, a document, a program or a command"))
  (when lispm-desktop-startup-items
    (insert " ")
    (lispm-desktop-settings--button (lispm-desktop-tr "Remove…") #'lispm-desktop-settings--startup-remove
                                    (lispm-desktop-tr "Take an item out of the startup items"))
    (insert " ")
    (lispm-desktop-settings--button (lispm-desktop-tr "Open…") #'lispm-desktop-settings--startup-open
                                    (lispm-desktop-tr "Open an item now, or show a program's output")))
  (insert "\n\n")
  (lispm-desktop-settings--paragraph
   (lispm-desktop-tr "A program runs in the background while LispM runs, and stops as it quits; its output is in its own buffer.")))

(defun lispm-desktop-settings--when (time)
  "Return TIME as the Software Update panel tells it: today at 23:50, or
on a day before, 18 October 2026 at 09:12."
  (if (equal (format-time-string "%F" time) (format-time-string "%F"))
      (lispm-desktop-format-time "today at %H:%M" time)
    (lispm-desktop-format-time "%-d %B %Y at %H:%M" time)))

(defun lispm-desktop-settings--truncate (text width)
  "Return TEXT cut to WIDTH characters, an ellipsis standing for the rest."
  (truncate-string-to-width text width nil nil "…"))

(defvar lispm-desktop-settings--update-loading nil
  "The loading Software Update's check is, while it runs.")

(defun lispm-desktop-settings--update-loading ()
  "Show the watch on the pointer over Control Panels while Software Update
checks."
  (cond ((and (lispm-desktop-update-checking-p) (not lispm-desktop-settings--update-loading))
         (setq lispm-desktop-settings--update-loading
               (lispm-desktop-loading-start lispm-desktop-settings-buffer-name)))
        ((and (not (lispm-desktop-update-checking-p)) lispm-desktop-settings--update-loading)
         (lispm-desktop-loading-stop lispm-desktop-settings--update-loading)
         (setq lispm-desktop-settings--update-loading nil))))

(defun lispm-desktop-settings--update-status ()
  "Insert what Software Update is doing, or found last: a line telling it,
or a line an update, Restart to Update after them."
  (let ((updates (lispm-desktop-update-updates))
        (failed (seq-filter (lambda (result) (eq (plist-get result :state) 'failed))
                            (lispm-desktop-update-results)))
        (checked (lispm-desktop-update-last-check)))
    (cond
     ((lispm-desktop-update-checking-p)
      (lispm-desktop-settings--note (lispm-desktop-tr "Checking for updates…")))
     (updates
      (dolist (update updates)
        (let* ((changes (plist-get update :changes))
               (head (lispm-desktop-tr-n "%s, %d change" "%s, %d changes" (length changes)
                                         (lispm-desktop-update-title update) (length changes))))
          (insert (lispm-desktop-settings--indent) (propertize head 'face 'bold))
          (when changes
            (insert (propertize (concat ": " (lispm-desktop-settings--truncate
                                              (car changes) (max 10 (- 58 (length head)))))
                                'face 'shadow)))
          (insert "\n")))
      (insert (lispm-desktop-settings--indent))
      (lispm-desktop-settings--button (lispm-desktop-tr "Restart to Update") #'lispm-desktop-update-restart
                                      (lispm-desktop-tr "Restart LispM, installing the updates as it quits"))
      (insert "  " (propertize (cond ((lispm-desktop-update-needs-installer-p)
                                      (lispm-desktop-tr "install.sh remakes LispM.app"))
                                     ((lispm-desktop-update-installs-on-quit-p)
                                      (lispm-desktop-tr "or as LispM quits"))
                                     (t (lispm-desktop-tr "or later, from here")))
                               'face 'shadow)
              "\n"))
     (failed
      (dolist (result failed)
        (lispm-desktop-settings--note
         (lispm-desktop-settings--truncate (lispm-desktop-tr "%s could not be checked: %s"
                                                             (plist-get result :name)
                                                             (lispm-desktop-tr (plist-get result :reason)))
                                           64))))
     (checked
      (lispm-desktop-settings--paragraph (lispm-desktop-tr "LispM is up to date; checked %s."
                                                           (lispm-desktop-settings--when checked))))
     (t (lispm-desktop-settings--note (lispm-desktop-tr "Not checked yet."))))))

(defun lispm-desktop-settings--show-store-updates ()
  "Show the App Store's Updates."
  (lispm-desktop-store-show)
  (lispm-desktop-store-show-category 'updates))

(defun lispm-desktop-settings--applications ()
  "Insert the part of the Software Update panel about the App Store's
applications and extensions: whether their updates install by
themselves, and which wait."
  (lispm-desktop-settings--heading (lispm-desktop-tr "Applications"))
  (lispm-desktop-settings--check-box
   (lispm-desktop-tr "Update applications automatically") lispm-desktop-store-update-automatically
   (lambda () (lispm-desktop-settings--save 'lispm-desktop-store-update-automatically
                                            (not lispm-desktop-store-update-automatically)))
   (if lispm-desktop-store-update-automatically
       (lispm-desktop-tr "when LispM is idle, an application once closed")
     (lispm-desktop-tr "they wait in the App Store")))
  (if-let* ((updates (lispm-desktop-store-updates)))
      (progn
        (insert (lispm-desktop-settings--indent)
                (lispm-desktop-tr "%s can be updated" (lispm-desktop-store--update-names updates)) "  ")
        (lispm-desktop-settings--button (lispm-desktop-tr "Show Updates") #'lispm-desktop-settings--show-store-updates
                                        (lispm-desktop-tr "Show them in the App Store's Updates"))
        (insert "\n"))
    (lispm-desktop-settings--paragraph (lispm-desktop-tr "The applications from the App Store are up to date."))))

(defun lispm-desktop-settings--update ()
  "Insert the Software Update panel: Update Now, what it found, how it
checks and installs by itself, and what is installed."
  (lispm-desktop-settings--heading (lispm-desktop-tr "Software Update"))
  (lispm-desktop-settings--paragraph
   (lispm-desktop-tr "Checks for new versions of LispM and of the App Store's applications."))
  (lispm-desktop-settings--icon-button "software-update-32" (lispm-desktop-tr "Update Now") #'lispm-desktop-update-now
                                       (lispm-desktop-tr "Check for updates now"))
  (lispm-desktop-settings--update-status)
  (lispm-desktop-settings--heading (lispm-desktop-tr "Automatic Updates"))
  (lispm-desktop-settings--check-box
   (lispm-desktop-tr "Check for updates automatically") lispm-desktop-update-check-automatically
   (lambda () (lispm-desktop-settings--save 'lispm-desktop-update-check-automatically
                                            (not lispm-desktop-update-check-automatically))))
  (insert (lispm-desktop-settings--indent) (lispm-desktop-settings--indent))
  (lispm-desktop-settings--radio-button (lispm-desktop-tr "Every day") (eq lispm-desktop-update-schedule 'daily)
                                        (lambda () (lispm-desktop-settings--save 'lispm-desktop-update-schedule 'daily))
                                        (lispm-desktop-tr "Check once a day"))
  (insert "   ")
  (lispm-desktop-settings--radio-button (lispm-desktop-tr "Every week") (eq lispm-desktop-update-schedule 'weekly)
                                        (lambda () (lispm-desktop-settings--save 'lispm-desktop-update-schedule 'weekly))
                                        (lispm-desktop-tr "Check once a week"))
  (insert "\n")
  (lispm-desktop-settings--check-box
   (lispm-desktop-tr "Ask me before installing updates") (not lispm-desktop-update-install-automatically)
   (lambda () (lispm-desktop-settings--save 'lispm-desktop-update-install-automatically
                                            (not lispm-desktop-update-install-automatically)))
   (if lispm-desktop-update-install-automatically
       (lispm-desktop-tr "else, as LispM quits")
     (lispm-desktop-tr "they wait for Restart")))
  (lispm-desktop-settings--applications)
  (lispm-desktop-settings--heading (lispm-desktop-tr "Installed"))
  (let ((kept (seq-filter (lambda (result) (eq (plist-get result :state) 'kept))
                          (lispm-desktop-update-results))))
    (pcase-dolist (`(,name ,directory ,version) (lispm-desktop-update-installed))
      (insert (lispm-desktop-settings--indent) name
              (propertize " " 'display `(space :align-to (,(lispm-desktop-px 150))))
              (or version "?"))
      (when-let* ((result (seq-find (lambda (result) (equal (plist-get result :directory) directory)) kept)))
        (insert "  " (propertize (lispm-desktop-tr "left as it is: %s" (lispm-desktop-tr (plist-get result :reason))) 'face 'shadow)))
      (insert "\n")))
  (insert (lispm-desktop-settings--indent))
  (lispm-desktop-settings--button (lispm-desktop-tr "Show Log") #'lispm-desktop-update-show-log
                                  (lispm-desktop-tr "List the updates ready, and what Software Update installed"))
  (insert "\n"))

(defun lispm-desktop-settings--emacs-note (build)
  "Return the note after the radio button of BUILD, a build of Emacs
install.sh offers."
  (let ((major (plist-get build :major)))
    (cond ((eql major emacs-major-version) (lispm-desktop-tr "running now"))
          ((plist-get build :development)
           (if-let* ((date (lispm-desktop-emacs-build-date build)))
               (lispm-desktop-tr "experimental, built %s" date)
             (lispm-desktop-tr "experimental")))
          (t (lispm-desktop-tr "the default")))))

(defun lispm-desktop-settings--emacs-status ()
  "Insert where switching Emacs stands: a download's progress and Cancel,
Restart to Switch once ready, or why it could not be."
  (let ((target (lispm-desktop-emacs-target)))
    (cond
     ((lispm-desktop-emacs-staging-p)
      (let ((name (lispm-desktop-emacs-name target))
            (progress (lispm-desktop-emacs-progress)))
        (insert (lispm-desktop-settings--indent)
                (propertize (if (and progress (< progress 100))
                                (lispm-desktop-tr "Downloading %s… %d%%" name (floor progress))
                              (lispm-desktop-tr "Making LispM.app of %s…" name))
                            'face 'shadow)
                "  ")
        (lispm-desktop-settings--button (lispm-desktop-tr "Cancel") #'lispm-desktop-emacs-cancel
                                        (lispm-desktop-tr "Stop, and stay on this Emacs"))
        (insert "\n")))
     ((lispm-desktop-emacs-staged)
      (insert (lispm-desktop-settings--indent))
      (lispm-desktop-settings--button (lispm-desktop-tr "Restart to Switch") #'lispm-desktop-emacs-restart
                                      (lispm-desktop-tr "Quit LispM, switch LispM.app to it, and open it again"))
      (insert "  " (propertize (lispm-desktop-tr "%s is ready" (lispm-desktop-emacs-name target)) 'face 'shadow) "\n"))
     (lispm-desktop-emacs-problem
      (lispm-desktop-settings--paragraph
       (lispm-desktop-tr "It could not be made ready: %s" lispm-desktop-emacs-problem))))))

(defun lispm-desktop-settings--emacs-kernel ()
  "Insert the Kernel panel's Emacs Kernel group: the Emacs LispM runs on,
which LispM.app switches."
  (lispm-desktop-settings--heading (lispm-desktop-tr "Emacs Kernel"))
  (lispm-desktop-settings--paragraph
   (lispm-desktop-tr "LispM runs on %s." (lispm-desktop-emacs-name emacs-major-version emacs-version)))
  (if (not (lispm-desktop-emacs-app))
      (lispm-desktop-settings--paragraph
       (lispm-desktop-tr "Only LispM.app switches its Emacs; in an Emacs of your own, install another yourself."))
    (let ((chosen (or (lispm-desktop-emacs-target) emacs-major-version)))
      (dolist (build (lispm-desktop-emacs-builds))
        (let ((major (plist-get build :major)))
          (lispm-desktop-settings--radio
           (lispm-desktop-emacs-name major (plist-get build :version)) (eql major chosen)
           (lambda () (lispm-desktop-emacs-stage major))
           (lispm-desktop-settings--emacs-note build)))))
    (lispm-desktop-settings--emacs-status)
    (lispm-desktop-settings--paragraph
     (lispm-desktop-tr "Switching downloads the other Emacs, keeps LispM.app as it is in ~/.lispm/backup, puts the new one in its place, and restarts LispM."))
    (when-let* ((backup (lispm-desktop-emacs-backup)))
      (lispm-desktop-settings--paragraph
       (lispm-desktop-tr "Kept: LispM.app on %s, to switch back to at once."
                         (lispm-desktop-emacs-name (plist-get backup :major)))))))

;;;; The window

(defvar-keymap lispm-desktop-settings-tab-map
  :doc "Keymap of a panel's tab."
  "<header-line> <down-mouse-1>" #'ignore
  "<header-line> <mouse-1>" #'lispm-desktop-settings-tab-click)

(defun lispm-desktop-settings-tab-click (event)
  "Show the panel whose tab EVENT clicked."
  (interactive "e")
  (when-let* ((string (posn-string (event-start event)))
              (panel (get-text-property (cdr string) 'lispm-desktop-settings-panel (car string))))
    (lispm-desktop-settings-select panel)))

(defun lispm-desktop-settings--tabs ()
  "Return the window's header line: a tab per panel, the shown one in front."
  (lispm-desktop-platinum-tabs
   (mapcar (lambda (panel)
             (let ((label (lispm-desktop-settings-panel-label panel)))
               (list label (eq (car panel) lispm-desktop-settings--panel)
                     'lispm-desktop-settings-panel (car panel) 'keymap lispm-desktop-settings-tab-map
                     'pointer 'arrow 'help-echo (lispm-desktop-tr "Show %s" label))))
           (lispm-desktop-settings--panels))
   (- (window-pixel-width) (window-right-divider-width))))

(defun lispm-desktop-settings--control-index ()
  "Return how many buttons come before point's, when point is on one."
  (when (button-at (point))
    (let ((count 0) (position (point-min)))
      (while (and (setq position (next-button position)) (< (button-start position) (point)))
        (setq count (1+ count)))
      count)))

(defun lispm-desktop-settings--render ()
  "Fill the control panel's window with the panel it shows, point kept."
  (when-let* ((buffer (get-buffer lispm-desktop-settings-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (line (line-number-at-pos))
            (panel (or (assq lispm-desktop-settings--panel (lispm-desktop-settings--panels))
                       (car (lispm-desktop-settings--panels)))))
        (setq lispm-desktop-settings--panel (car panel))
        (let ((control (lispm-desktop-settings--control-index)))
          (erase-buffer)
          (insert (propertize " " 'display '(space :height (10))) "\n")
          (funcall (nth 2 panel))
          (lispm-desktop-settings--frame-groups)
          (goto-char (point-min))
          (if control
              ;; The keyboard stays on the control it was on.
              (dotimes (_ (1+ control)) (forward-button 1 nil nil t))
            (forward-line (1- line))))
        (setq lispm-desktop-platinum-focus nil
              lispm-desktop-platinum-title (lispm-desktop-settings-panel-label panel))
        (lispm-desktop-platinum-show-focus)
        (force-mode-line-update)))))

(defun lispm-desktop-settings-select (panel)
  "Show PANEL, a key of the panels, in the control panel's window.
The panels are `lispm-desktop-settings-panels'."
  (with-current-buffer (lispm-desktop-settings--buffer)
    (setq lispm-desktop-settings--panel panel)
    (goto-char (point-min))
    (lispm-desktop-settings--render)))

(defun lispm-desktop-settings-next-panel (&optional back)
  "Show the next panel, or the previous one when BACK."
  (interactive)
  (let* ((keys (mapcar #'car (lispm-desktop-settings--panels)))
         (index (or (seq-position keys lispm-desktop-settings--panel) 0)))
    (lispm-desktop-settings-select (nth (mod (+ index (if back -1 1)) (length keys)) keys))))

(defun lispm-desktop-settings-previous-panel ()
  "Show the previous panel."
  (interactive)
  (lispm-desktop-settings-next-panel t))

(defvar-local lispm-desktop-settings--width nil
  "The width in pixels the panel's group boxes were drawn for.")

(defun lispm-desktop-settings--refit (window)
  "Draw the panel again once WINDOW's width changed, its group boxes as wide
as the pane.  This is a buffer-local `window-size-change-functions' function."
  (let ((width (window-body-width window t)))
    (unless (eql width lispm-desktop-settings--width)
      (setq lispm-desktop-settings--width width)
      (lispm-desktop-settings--render))))

(defun lispm-desktop-settings--widen ()
  "Widen the control panel's window, while it is open, until its tabs show
whole, as far as its screen allows; a row of tabs still too wide cuts its
widest names short.  A language with longer names than the one the
window opened in needs it.  This is one of
`lispm-desktop-language-changed-hook'."
  (when-let* ((frame (lispm-desktop-app-window-frame 'settings))
              ((frame-live-p frame))
              ((not (lispm-desktop-app-window-rolled-p frame)))
              (window (get-buffer-window lispm-desktop-settings-buffer-name frame)))
    (let ((short (- (+ (lispm-desktop-platinum-tabs-width
                        (mapcar #'lispm-desktop-settings-panel-label (lispm-desktop-settings--panels)))
                       12)
                    (window-pixel-width window)))
          (most (- (if-let* ((screen (lispm-desktop-apps-window-screen frame)))
                       (frame-inner-width screen)
                     (display-pixel-width))
                   60)))
      (when (and (> short 0) (< (frame-text-width frame) most))
        (let ((frame-resize-pixelwise t))
          (set-frame-size frame (min most (+ (frame-text-width frame) short)) (frame-text-height frame) t))
        (lispm-desktop-platinum-frame-update frame)
        (lispm-desktop-apps-keep-on-desktop frame)))))

(defvar-keymap lispm-desktop-settings-mode-map
  :doc "Keymap of the control panel's window."
  :parent button-buffer-map
  "<right>" #'lispm-desktop-settings-next-panel
  "<left>" #'lispm-desktop-settings-previous-panel
  "C-f" #'lispm-desktop-settings-next-panel
  "C-b" #'lispm-desktop-settings-previous-panel
  "SPC" #'lispm-desktop-platinum-push
  "q" #'lispm-desktop-settings-close
  "M-w" #'lispm-desktop-settings-close)

(define-derived-mode lispm-desktop-settings-mode lispm-desktop-app-window-mode "Control Panels"
  "Major mode of the control panel's window: a panel of settings, a tab each.
\\{lispm-desktop-settings-mode-map}"
  (setq-local lispm-desktop-platinum-info (lambda () (lispm-desktop-tr "Choices are kept for the next start."))
              header-line-format '(:eval (lispm-desktop-settings--tabs))
              lispm-desktop-platinum-title (lambda () (lispm-desktop-tr "Control Panels"))
              lispm-desktop-platinum-icon (lispm-desktop-platinum-icon "control-panels")
              lispm-desktop-platinum-box-functions (list :close #'lispm-desktop-app-window-close-box
                                                         :collapse #'lispm-desktop-app-window-collapse-box))
  ;; The pane the tabs choose among, lighter than the window's platinum.
  (face-remap-add-relative 'default 'lispm-desktop-platinum-list-body)
  (add-hook 'window-size-change-functions #'lispm-desktop-settings--refit nil t)
  ;; The tabs fill the header line, their pane's edge under them.
  (face-remap-add-relative 'header-line '(:box nil :underline nil :overline nil))
  (add-hook 'post-command-hook #'lispm-desktop-platinum-show-focus nil t)
  (hl-line-mode -1))

(defun lispm-desktop-settings--buffer ()
  "Return the control panel's window's buffer, made when missing."
  (or (get-buffer lispm-desktop-settings-buffer-name)
      (with-current-buffer (get-buffer-create lispm-desktop-settings-buffer-name)
        (lispm-desktop-settings-mode)
        (current-buffer))))

(defun lispm-desktop-settings--default-geometry (screen)
  "Return where the control panel's window opens first: the middle of
SCREEN, wide enough for every panel's tab."
  (let ((width (max 520 (+ (lispm-desktop-platinum-tabs-width
                            (mapcar #'lispm-desktop-settings-panel-label (lispm-desktop-settings--panels)))
                           ;; The window's sides, and a little of the
                           ;; pane's edge after the last tab.
                           24)))
        (height 500))
    (list (max 20 (/ (- (frame-inner-width screen) width) 2))
          (+ (lispm-desktop-app-window-menu-bar-height screen) 50)
          width height)))

(lispm-desktop-app-window-define 'settings
  :name (lispm-desktop-tr-noop "Control Panels") :geometry #'lispm-desktop-settings--default-geometry
  :buffer #'lispm-desktop-settings--buffer :roll 'chrome)

(defun lispm-desktop-settings-show (&optional panel)
  "Open the control panel's window over the desktop, showing PANEL, a key of
`lispm-desktop-settings-panels', or the panel it showed last."
  (interactive)
  (lispm-desktop-app-window-ensure 'settings)
  (if panel
      (lispm-desktop-settings-select panel)
    (lispm-desktop-settings--render))
  (lispm-desktop-app-window-present 'settings))

(defun lispm-desktop-settings-close ()
  "Close the control panel's window; the desktop comes in front."
  (interactive)
  (lispm-desktop-app-window-close 'settings))

;;;; Menus

(defun lispm-desktop-settings-panel-items ()
  "Return a menu item opening each panel, as the λ menu's Control Panels
lists them, as Mac OS 9's Apple menu listed its control panels."
  (mapcar (lambda (panel)
            (list :label (lispm-desktop-settings-panel-label panel)
                  :action (lambda () (lispm-desktop-settings-show (car panel)))))
          (lispm-desktop-settings--panels)))

(defun lispm-desktop-settings--folder-items ()
  "Return the Control Panels folder's items, see `lispm-desktop-finder-folders'."
  (mapcar (lambda (panel)
            (list (lispm-desktop-settings-panel-label panel) (nth 3 panel)
                  (lambda () (lispm-desktop-settings-show (car panel)))))
          (lispm-desktop-settings--panels)))

(declare-function lispm-desktop-finder-open-directory "lispm-desktop-finder" (directory))
(defvar lispm-desktop-finder-folders)

(defun lispm-desktop-settings-open-folder ()
  "Open the Control Panels folder in the Finder window: a panel each, as
the Control Panels folder of Mac OS 9's System Folder."
  (interactive)
  (lispm-desktop-finder-open-directory 'control-panels))

(defun lispm-desktop-settings--file-items ()
  "Return the items of the Control Panels File menu."
  `((:label ,(lispm-desktop-tr "Close Window") :action lispm-desktop-settings-close)))

(defun lispm-desktop-settings--view-items ()
  "Return the items of the Control Panels View menu: a panel each, the shown one
checked."
  (let ((shown (buffer-local-value 'lispm-desktop-settings--panel (lispm-desktop-settings--buffer))))
    (mapcar (lambda (panel)
              (list :label (lispm-desktop-settings-panel-label panel)
                    :action (lambda () (lispm-desktop-settings-show (car panel)))
                    :checked (eq (car panel) shown)))
            (lispm-desktop-settings--panels))))

(defun lispm-desktop-settings-menus ()
  "Return the Control Panels menus, for the menu bar while they are in front."
  `((lispm-desktop-settings-file ,(lispm-desktop-tr "File") lispm-desktop-settings--file-items)
    (lispm-desktop-settings-view ,(lispm-desktop-tr "View") lispm-desktop-settings--view-items)))

(defun lispm-desktop-setup-settings ()
  "Make Control Panels an application: its folder holds a panel each, and
its window follows the application in front and shows the theme's colours
when they change, and what is chosen elsewhere: the sandbox level chosen
with /sandbox in a chat, for one."
  (setf (alist-get 'control-panels lispm-desktop-finder-folders)
        `(:title ,(lispm-desktop-tr-noop "Control Panels") :icon "control-panels"
                 :items lispm-desktop-settings--folder-items))
  (add-hook 'enable-theme-functions (lambda (&rest _) (lispm-desktop-settings--render)) 95)
  (add-hook 'lispm-desktop-language-changed-hook #'lispm-desktop-settings--widen)
  (add-hook 'lispm-desktop-update-changed-hook #'lispm-desktop-settings--render)
  (add-hook 'lispm-desktop-update-changed-hook #'lispm-desktop-settings--update-loading)
  (add-hook 'lispm-desktop-startup-changed-hook #'lispm-desktop-settings--render)
  (add-hook 'lispm-desktop-emacs-changed-hook #'lispm-desktop-settings--render)
  ;; lispm-kernel's, once it is turned on.
  (add-hook 'lispm-kernel-program-obtained-functions
            (lambda (&rest _) (lispm-desktop-settings--render)))
  ;; LispM Agent's, which may be loaded later, or never.
  (add-hook 'lispm-sandbox-change-hook #'lispm-desktop-settings--render)
  (add-hook 'lispm-settings-change-hook #'lispm-desktop-settings--render))

(provide 'lispm-desktop-settings)
;;; lispm-desktop-settings.el ends here
