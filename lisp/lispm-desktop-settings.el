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
;;   Internet    the browser's home page and search address, its history
;;               and its bookmarks
;;   Network     the proxy every connection LispM makes goes through, see
;;               lispm-desktop-proxy.el
;;   Sandbox     how strictly agents' shell commands and MCP servers are
;;               confined, the rules of the Custom level, and the domains
;;               they may reach, see LispM Agent's lispm-sandbox.el; shown
;;               once it is loaded
;;   Startup     whether the computer opens LispM at login, and the
;;               startup items LispM opens once started, see
;;               lispm-desktop-startup.el
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
(require 'lispm-desktop-store)
(require 'lispm-desktop-proxy)
(require 'lispm-desktop-startup)

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

(defconst lispm-desktop-settings-buffer-name " *Control Panels*"
  "Name of the control panel's window's buffer.")

(defconst lispm-desktop-settings-panels
  '((themes "Appearance" lispm-desktop-settings--themes "appearance-32")
    (desktop "Desktop" lispm-desktop-settings--desktop "desktop-picture-32")
    (keyboard "Keyboard" lispm-desktop-settings--keyboard "keyboard-32")
    (browser "Internet" lispm-desktop-settings--browser "globe-32" lispm-desktop-settings--browser-p)
    (network "Network" lispm-desktop-settings--network "network-32")
    (sandbox "Sandbox" lispm-desktop-settings--sandbox "sandbox-32" lispm-desktop-settings--sandbox-p)
    (startup "Startup" lispm-desktop-settings--startup "startup-32")
    (update "Software Update" lispm-desktop-settings--update "software-update-32"))
  "The panels, in their tabs' order: (KEY LABEL INSERT ICON [VISIBLE]).
INSERT fills the window with the panel, ICON is its large icon in the
Control Panels folder; VISIBLE, when given, is a function deciding whether
the panel shows.")

(defvar-local lispm-desktop-settings--panel 'themes
  "The panel the control panel's window shows.")

(defun lispm-desktop-settings--browser-p ()
  "Return non-nil when the browser can run, and so has settings."
  (and (fboundp 'lispm-browser-available-p) (lispm-browser-available-p)))

(defun lispm-desktop-settings--sandbox-p ()
  "Return non-nil when LispM's agents can run in a sandbox, and so it has
settings: LispM Agent 0.3.0 or later is loaded."
  (and (featurep 'lispm-sandbox) (fboundp 'lispm-sandbox-set-level)))

(defun lispm-desktop-settings--panels ()
  "Return the panels that show here."
  (seq-filter (lambda (panel)
                (let ((visible (nth 4 panel)))
                  (or (not visible) (funcall visible))))
              lispm-desktop-settings-panels))

(defun lispm-desktop-settings-open-p ()
  "Return non-nil while the control panel's window is open."
  (lispm-desktop-app-window-open-p 'settings))

;;;; Controls
;;
;; Radio buttons and push buttons are drawn as Mac OS 9's, in the theme's
;; colours.  Both are text buttons: Tab moves between them, ringing the one
;; reached in the focus colour, and Return or Space presses it.

(defconst lispm-desktop-settings--heading-indent (propertize " " 'display '(space :width (12)))
  "The blank before a heading.")

(defconst lispm-desktop-settings--indent (propertize " " 'display '(space :width (24)))
  "The blank before a row of settings.")

(defun lispm-desktop-settings--under-label (control)
  "Return the blank before a line under the label of a CONTROL, `radio' or
`check-box', so that the line starts where the label does: after the
control, 16 or 18 pixels wide with the room for its ring."
  (concat lispm-desktop-settings--indent
          (propertize " " 'display `(space :width (,(if (eq control 'radio) 16 18))))
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
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--radio-button label on action (format "Choose %s" label))
  (when note
    (insert "  " (propertize note 'face 'shadow)))
  (insert "\n"))

(defun lispm-desktop-settings--check-box (label on action &optional note)
  "Insert a check box named LABEL, checked when ON, running ACTION when
pressed, NOTE after it in the dimmed text."
  (let ((box (lambda (focused)
               (lispm-desktop-platinum-image-of (lispm-desktop-platinum-check-box on nil focused 2)))))
    (insert lispm-desktop-settings--indent)
    (insert-text-button
     (concat (propertize " " 'display (funcall box nil)
                         'lispm-desktop-platinum-plain (funcall box nil)
                         'lispm-desktop-platinum-focused (funcall box t))
             " " label)
     'action (lambda (_) (funcall action) (lispm-desktop-settings--render))
     'face 'default 'follow-link t 'help-echo (format "%s %s" (if on "Turn off:" "Turn on:") label))
    (when note
      (insert "  " (propertize note 'face 'shadow)))
    (insert "\n")))

(defconst lispm-desktop-settings--icon-button-size '(52 . 50)
  "Width and height in pixels of a bevel button holding a large icon, as
Software Update's Update Now button.")

(defun lispm-desktop-settings--icon-button-image (icon state focused)
  "Return the image of a bevel button holding ICON, a 32-pixel icon's name,
in STATE, nil or `pressed', ringed when FOCUSED.  The ring's room is
always left, so the button keeps its place."
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
       svg))))

(defun lispm-desktop-settings--icon-button (icon label action help)
  "Insert, centred, a bevel button holding ICON, a 32-pixel icon's name,
LABEL under it, running ACTION when pressed; HELP describes it.
Return, Space and the mouse press it as a push button."
  (let ((half (/ (+ (car lispm-desktop-settings--icon-button-size) 6) 2)))
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
  (insert (propertize (concat lispm-desktop-settings--heading-indent title)
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
         (margin lispm-desktop-settings--group-margin)
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
                  (propertize " " 'display '(space :height (8))) "\n")
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
                   (right (concat (propertize " " 'display `(space :align-to (- right (,(+ margin 2)))))
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
  (insert lispm-desktop-settings--indent (propertize text 'face 'shadow) "\n"))

(defun lispm-desktop-settings--text-width (indent)
  "Return the pixels a line of the pane has for text after INDENT pixels,
inside its group box."
  (let ((window (get-buffer-window (current-buffer) t)))
    (- (if window (window-body-width window t) 500)
       indent (* 4 lispm-desktop-settings--group-margin))))

(defun lispm-desktop-settings--wrap (text width)
  "Return TEXT broken between words into lines at most WIDTH pixels wide
when dimmed; a word wider than that stands alone, cut short."
  (let ((lines nil) (line nil)
        (fit (lambda (word)
               (let ((wide (lispm-desktop-platinum-content-width word 'shadow)))
                 (if (<= wide width)
                     word
                   (lispm-desktop-settings--truncate
                    word (max 2 (1- (/ (* (length word) width) wide)))))))))
    (dolist (word (split-string text "[ \t\n]+" t))
      (let ((longer (if line (concat line " " word) word)))
        (if (and line (<= (lispm-desktop-platinum-content-width longer 'shadow) width))
            (setq line longer)
          (when line (push line lines))
          (setq line (funcall fit word)))))
    (nreverse (if line (cons line lines) lines))))

(defun lispm-desktop-settings--paragraph (text &optional indent)
  "Insert TEXT, a remark about the settings above, dimmed and wrapped to
the pane.  INDENT, a string, comes before each line instead of the
indent of a row of settings."
  (let ((indent (or indent lispm-desktop-settings--indent)))
    (dolist (line (lispm-desktop-settings--wrap
                   text (lispm-desktop-settings--text-width (string-pixel-width indent))))
      (insert indent (propertize line 'face 'shadow) "\n"))))

(defun lispm-desktop-settings--save (variable value)
  "Set VARIABLE to VALUE now and for every later start."
  (customize-save-variable variable value))

;;;; Panels

(defun lispm-desktop-settings--other-theme ()
  "Ask for any theme installed, and switch to it now and for later starts."
  (let ((theme (completing-read "Theme: " (mapcar #'symbol-name (custom-available-themes))
                                nil t)))
    (unless (string-empty-p theme)
      (lispm-desktop-set-theme (intern theme)))))

(defun lispm-desktop-settings--themes ()
  "Insert the Themes panel: the colour theme."
  (lispm-desktop-settings--heading "Theme")
  (lispm-desktop-settings--radio "Dracula" (eq lispm-desktop-theme 'dracula)
                                 (lambda () (lispm-desktop-set-theme 'dracula)) "dark, Dracula's colours")
  (lispm-desktop-settings--radio "Platinum" (eq lispm-desktop-theme 'platinum)
                                 (lambda () (lispm-desktop-set-theme 'platinum)) "Mac OS 9's greys")
  (when (and lispm-desktop-theme (not (memq lispm-desktop-theme '(dracula platinum))))
    (lispm-desktop-settings--radio (capitalize (symbol-name lispm-desktop-theme)) t #'ignore))
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--button "Other Theme…" #'lispm-desktop-settings--other-theme
                                  "Choose any other theme installed")
  (insert "\n\n")
  (lispm-desktop-settings--note "The menu bar, the title bars and the lists take their colours")
  (lispm-desktop-settings--note "from the theme."))

(defun lispm-desktop-settings--swatch (color)
  "Return a swatch of COLOR, one icon wide."
  (propertize " " 'display (lispm-desktop-platinum-image
                            (list 'swatch color)
                            (lambda ()
                              (let ((svg (svg-create 28 14)))
                                (svg-rectangle svg 0.5 0.5 27 13 :fill color
                                               :stroke (lispm-desktop-platinum-color :text))
                                svg)))))

(defun lispm-desktop-settings--choose-color ()
  "Ask for the desktop's colour and keep it, written #RRGGBB.
`read-color' returns a colour typed by name as #RRRRGGGGBBBB, which the
desktop's SVG cannot read."
  (let ((color (lispm-desktop--hex-color (read-color "Desktop colour: " t))))
    (when color
      (lispm-desktop-settings--save 'lispm-desktop-color color))))

(defun lispm-desktop-settings--picture-preview ()
  "Insert a small view of the desktop picture."
  (when-let* ((file (and lispm-desktop-picture (expand-file-name lispm-desktop-picture)))
              ((file-readable-p file))
              ((display-images-p)))
    (insert lispm-desktop-settings--indent)
    (insert-image (create-image file nil nil :max-width 160 :max-height 100 :ascent 'center))
    (insert "\n")))

(defun lispm-desktop-settings--desktop ()
  "Insert the Desktop panel: its colour, its picture, its icons."
  (lispm-desktop-settings--heading "Colour")
  (lispm-desktop-settings--radio "From the theme" (null lispm-desktop-color)
                                 (lambda () (lispm-desktop-settings--save 'lispm-desktop-color nil)))
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--radio-button "Your own" (and lispm-desktop-color t)
                                        #'lispm-desktop-settings--choose-color "Choose a colour")
  (insert " " (lispm-desktop-settings--swatch (lispm-desktop--color)) " ")
  (lispm-desktop-settings--button "Choose…" #'lispm-desktop-settings--choose-color "Choose the desktop's colour")
  (insert "\n")
  (lispm-desktop-settings--heading "Picture")
  (lispm-desktop-settings--picture-preview)
  (lispm-desktop-settings--note (if lispm-desktop-picture
                                    (abbreviate-file-name lispm-desktop-picture)
                                  "None: the desktop shows its colour."))
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--button "Choose…" (lambda () (call-interactively #'lispm-desktop-choose-picture))
                                  "Choose a picture for the desktop")
  (insert " ")
  (when lispm-desktop-picture
    (lispm-desktop-settings--button "None" #'lispm-desktop-remove-picture "Show the desktop's colour alone"))
  (insert "\n\n")
  (pcase-dolist (`(,position ,label ,note) '((fill "Fill" "cropped to the desktop")
                                             (fit "Fit" "whole, the colour around")
                                             (center "Center" "at its own size")
                                             (tile "Tile" "repeated")))
    (lispm-desktop-settings--radio label (eq lispm-desktop-picture-position position)
                                   (lambda () (lispm-desktop-settings--save 'lispm-desktop-picture-position position))
                                   note))
  (lispm-desktop-settings--heading "Icons")
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--button "Arrange Icons" #'lispm-desktop-arrange-icons
                                  "Put the desktop's icons back where they stood at first")
  (insert "\n")
  (lispm-desktop-settings--note "Dragged icons land on the grid; this puts every one back."))

(defun lispm-desktop-settings--keyboard ()
  "Insert the Keyboard panel: Vim or Emacs keys."
  (lispm-desktop-settings--heading "Editing")
  (lispm-desktop-settings--radio "Vim keys" (eq lispm-desktop-editing-style 'vim)
                                 (lambda () (lispm-desktop-settings--save 'lispm-desktop-editing-style 'vim))
                                 "Evil in files; Emacs keys elsewhere")
  (lispm-desktop-settings--radio "Emacs keys" (eq lispm-desktop-editing-style 'emacs)
                                 (lambda () (lispm-desktop-settings--save 'lispm-desktop-editing-style 'emacs)))
  (insert "\n")
  (lispm-desktop-settings--note "Takes effect at the next start.")
  (when (eq system-type 'darwin)
    (lispm-desktop-settings--note "Command is Meta: Command-C and Command-V copy and paste.")))

(defun lispm-desktop-settings--change (variable prompt)
  "Ask with PROMPT for a new value of the string VARIABLE, and keep it."
  (let ((value (string-trim (read-string prompt (symbol-value variable)))))
    (unless (string-empty-p value)
      (lispm-desktop-settings--save variable value))))

(defun lispm-desktop-settings--browser ()
  "Insert the Browser panel: home page, search, history and bookmarks."
  (lispm-desktop-settings--heading "Home Page")
  (lispm-desktop-settings--note lispm-browser-home-url)
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--button "Change…"
                                  (lambda () (lispm-desktop-settings--change 'lispm-browser-home-url "Home page: "))
                                  "Change the page Home goes to")
  (insert "\n")
  (lispm-desktop-settings--heading "Search")
  (lispm-desktop-settings--note lispm-browser-search-url)
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--button "Change…"
                                  (lambda () (lispm-desktop-settings--change 'lispm-browser-search-url
                                                                             "Search address (%s for the words): "))
                                  "Change where what is typed in the location field is searched")
  (insert "\n")
  (lispm-desktop-settings--heading "History")
  (lispm-desktop-settings--note (format "%d pages visited are kept, of at most %d."
                                        (length (lispm-browser-history)) lispm-browser-history-size))
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--button "Clear History…" #'lispm-browser-clear-history
                                  "Forget every page visited")
  (insert "\n")
  (lispm-desktop-settings--heading "Bookmarks")
  (lispm-desktop-settings--note (format "%d bookmarks." (length (lispm-browser-bookmarks))))
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--button "Edit Bookmarks…" #'lispm-desktop-browser-edit-bookmarks
                                  "List the bookmarks in the browser")
  (insert "\n"))

(defun lispm-desktop-settings--proxy-server ()
  "Ask for the proxy's address, HOST:PORT, and keep it; return non-nil
when one is kept."
  (let ((server (string-trim (read-string "Proxy server (host:port): " lispm-desktop-proxy-server))))
    (cond ((string-empty-p server) nil)
          ((lispm-desktop-proxy-parse-server server)
           (lispm-desktop-settings--save 'lispm-desktop-proxy-server server)
           t)
          (t (user-error "Write the proxy server as host:port, as 127.0.0.1:7890")))))

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
   (split-string (read-string "Hosts reached directly (separated by commas): "
                              (string-join lispm-desktop-proxy-exceptions ", "))
                 "[, ]+" t)))

(defun lispm-desktop-settings--network ()
  "Insert the Network panel: the proxy, none, HTTP or SOCKS5, its address,
and the hosts reached directly."
  (lispm-desktop-settings--heading "Proxy")
  (pcase-dolist (`(,type ,label ,note) '((nil "None" "the system's own, if any")
                                         (http "HTTP" "HTTPS goes through it too")
                                         (socks5 "SOCKS5" "it looks the names up too")))
    (lispm-desktop-settings--radio label (eq lispm-desktop-proxy-type type)
                                   (lambda () (lispm-desktop-settings--proxy-type type))
                                   note))
  (insert "\n")
  (lispm-desktop-settings--note (if lispm-desktop-proxy-server
                                    (format "Server: %s" lispm-desktop-proxy-server)
                                  "No server yet."))
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--button "Change…" #'lispm-desktop-settings--proxy-server
                                  "Change the proxy's address, host:port")
  (insert "\n\n")
  (lispm-desktop-settings--note "Emacs, the browser's pages and the programs Emacs starts")
  (lispm-desktop-settings--note "all connect through it.")
  (when (and lispm-desktop-proxy-type (lispm-desktop-settings--browser-p)
             (fboundp 'lispm-browser-proxy-followed-p)
             (not (lispm-browser-proxy-followed-p)))
    (lispm-desktop-settings--note "The browser's pages follow once its module is built."))
  (lispm-desktop-settings--heading "Direct Connections")
  (lispm-desktop-settings--note
   (if lispm-desktop-proxy-exceptions
       (lispm-desktop-settings--truncate (string-join lispm-desktop-proxy-exceptions ", ") 58)
     "None but this computer's own addresses."))
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--button "Change…" #'lispm-desktop-settings--proxy-exceptions
                                  "Change the hosts reached directly, not through the proxy")
  (insert "\n\n")
  (lispm-desktop-settings--note "Hosts reached directly, not through the proxy, and their")
  (lispm-desktop-settings--note "subdomains; this computer's own addresses always are."))

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
      (user-error "srt cannot %s %s: write a domain as example.com, *.example.com or example.com:443"
                  (if deny "deny" "allow") (string-join wrong ", ")))
    domains))

(defun lispm-desktop-settings--sandbox-domains ()
  "Ask for the domains sandboxed commands may reach at Low and Middle, and
keep them."
  (lispm-desktop-settings--save
   'lispm-sandbox-allowed-domains
   (lispm-desktop-settings--sandbox-read-domains
    "Domains sandboxed commands may reach (separated by commas): " lispm-sandbox-allowed-domains)))

(defun lispm-desktop-settings--sandbox-custom-p ()
  "Return non-nil when LispM Agent has the sandbox level Custom, of rules
of the user's own: LispM Agent 0.4.0 or later."
  (and (assq 'custom lispm-sandbox-levels) (fboundp 'lispm-sandbox-rules)))

(defun lispm-desktop-settings--sandbox-description (level)
  "Return what LEVEL allows, as a sentence; Custom's is said from its rules."
  (let ((description (if (fboundp 'lispm-sandbox-level-description)
                         (lispm-sandbox-level-description level)
                       (alist-get level lispm-sandbox-levels))))
    (concat (upcase (substring description 0 1)) (substring description 1) ".")))

(defconst lispm-desktop-settings--sandbox-lists
  '((:deny-read "Not readable" file "the files and directories commands may not read")
    (:allow-read "Readable after all" file "the places inside those that commands may read after all")
    (:allow-write "Writable" file "where commands may write, and nowhere else")
    (:deny-write "Not writable" file "the places inside those that commands may not write")
    (:allowed-domains "Reachable domains" allow "the domains commands may reach, and no others")
    (:denied-domains "Refused domains" deny "the domains refused even when allowed")
    (:unix-sockets "Unix sockets" file "the Unix sockets commands may connect to"))
  "The Custom level's rules that are lists, as the Sandbox panel shows them:
\(KEY LABEL KIND HELP).  KIND is `file' for file names, `allow' or `deny'
for domains to allow or deny.  See `lispm-sandbox-custom'.")

(defconst lispm-desktop-settings--sandbox-switches
  '((:local-binding "Serve on local ports, and reach this computer's" nil)
    (:pty "Use pseudo-terminals" darwin)
    (:apple-events "Send Apple Events and open applications" darwin))
  "The Custom level's rules that are on or off, as the Sandbox panel shows
them: (KEY LABEL SYSTEM).  SYSTEM, when given, is the only `system-type'
the rule means something on.  See `lispm-sandbox-custom'.")

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
               (prompt (format "%s (separated by commas): " label)))
    (lispm-desktop-settings--sandbox-rule
     key (if (eq kind 'file)
             (split-string (read-string prompt (string-join entries ", ")) "," t "[ \t\n]+")
           (lispm-desktop-settings--sandbox-read-domains prompt entries (eq kind 'deny))))))

(defun lispm-desktop-settings--sandbox-start-from (level)
  "Make the Custom level's rules those of LEVEL, now and for later starts.
Rules of the user's own are replaced only once the user says so."
  (when (or (null lispm-sandbox-custom)
            (lispm-desktop-popup-alert
             'caution (format "Replace your Custom rules with %s's?" (lispm-sandbox-level-name level))
             '(("Cancel" nil cancel) ("Replace" t default))
             "The rules you set are lost."))
    (lispm-desktop-settings--save 'lispm-sandbox-custom (copy-tree (lispm-sandbox-rules level)))))

(defun lispm-desktop-settings--sandbox-entries (entries width)
  "Return ENTRIES, strings, as a list in WIDTH characters at most: the first
few, and how many more there are."
  (if (null entries)
      "none"
    (let ((more (lambda (shown)
                  (concat (string-join (seq-take entries shown) ", ")
                          (if (< shown (length entries))
                              (format " and %d more" (- (length entries) shown))
                            "")))))
      (or (seq-some (lambda (shown)
                      (let ((text (funcall more shown)))
                        (and (<= (length text) width) text)))
                    (number-sequence (length entries) 1 -1))
          (lispm-desktop-settings--truncate (funcall more 1) width)))))

(defun lispm-desktop-settings--sandbox-custom ()
  "Insert the Custom level's rules, each with what changes it: a Change…
button for each list, a check box for each switch."
  (let ((rules (lispm-sandbox-rules 'custom)))
    (lispm-desktop-settings--heading "Custom Rules")
    (insert lispm-desktop-settings--indent (propertize "Start from: " 'face 'shadow))
    (dolist (level '(low middle high))
      (let ((name (lispm-sandbox-level-name level)))
        (lispm-desktop-settings--button name (lambda () (lispm-desktop-settings--sandbox-start-from level))
                                        (format "Make the Custom rules %s's" name))
        (insert " ")))
    (insert "\n\n")
    (pcase-dolist (`(,key ,label ,_ ,help) lispm-desktop-settings--sandbox-lists)
      (insert lispm-desktop-settings--indent)
      (lispm-desktop-settings--button "Change…" (lambda () (lispm-desktop-settings--sandbox-change-list key))
                                      (concat "Change " help))
      (insert "  " label ": "
              (propertize (lispm-desktop-settings--sandbox-entries
                           ;; Relative file names are the project's.
                           (mapcar (lambda (entry) (if (equal entry ".") "the project" entry))
                                   (plist-get rules key))
                           (max 12 (- 50 (length label))))
                          'face 'shadow)
              "\n"))
    (insert "\n")
    (pcase-dolist (`(,key ,label ,system) lispm-desktop-settings--sandbox-switches)
      (when (or (null system) (eq system system-type))
        (lispm-desktop-settings--check-box
         label (plist-get rules key)
         (lambda () (lispm-desktop-settings--sandbox-rule key (not (plist-get (lispm-sandbox-rules 'custom) key)))))
        (when (eq key :apple-events)
          (lispm-desktop-settings--paragraph
           "Caution: the applications they open run outside the sandbox."
           (lispm-desktop-settings--under-label 'check-box)))))
    (insert "\n")
    (when-let* ((wrong (condition-case error
                           (progn (lispm-sandbox-settings 'custom) nil)
                         (error (error-message-string error)))))
      (lispm-desktop-settings--paragraph (concat wrong "."))
      (insert "\n"))
    (lispm-desktop-settings--paragraph
     "Whatever the rules, commands write their temporary directory, and never where Emacs and LispM keep their files.")
    (when (fboundp 'lispm-sandbox-customize)
      (insert lispm-desktop-settings--indent)
      (lispm-desktop-settings--button "Customize…" #'lispm-sandbox-customize
                                      "Edit the Custom rules in Customize")
      (insert "\n"))))

(defun lispm-desktop-settings--sandbox-srt (status)
  "Return what the Sandbox panel says of srt, the program confining
commands, given its STATUS (see `lispm-sandbox-srt-status'): sentences."
  (cons (pcase status
          ('ready (if lispm-sandbox-srt-program
                      (format "srt, Anthropic's sandbox runtime, runs from %s."
                              (abbreviate-file-name lispm-sandbox-srt-program))
                    (format "srt %s, Anthropic's sandbox runtime, is installed." lispm-sandbox-srt-version)))
          ('installing (format "Installing srt %s, Anthropic's sandbox runtime, with npm…"
                               lispm-sandbox-srt-version))
          (`(failed . ,why) (format "Sandbox unavailable: %s."
                                    (lispm-desktop-settings--truncate (string-trim why "[ \t\n]+" "[ \t\n.]+")
                                                                      240)))
          (_ "Any level but None installs srt, Anthropic's sandbox runtime, with npm first."))
        (unless (or (eq status 'ready) (eq lispm-sandbox-level 'none))
          (list "Until it runs, agents' commands fail rather than run unconfined."))))

(defun lispm-desktop-settings--sandbox ()
  "Insert the Sandbox panel: how strictly agents' shell commands and MCP
servers are confined, whether srt is there to confine them, the rules
of the level Custom while it is chosen, and the domains they may reach.
LispM Agent's lispm-sandbox.el does the rest."
  (lispm-desktop-settings--heading "Sandbox Level")
  (lispm-desktop-settings--note "How far agents' shell commands and MCP servers are confined:")
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
        (insert lispm-desktop-settings--indent)
        (lispm-desktop-settings--button "Try Again" #'lispm-sandbox-install "Install srt again")
        (insert "\n"))
      (insert "\n")))
  (lispm-desktop-settings--paragraph
   "Running commands take a new level's network rules at once, its file rules when they start again.")
  (when (and (eq lispm-sandbox-level 'custom) (lispm-desktop-settings--sandbox-custom-p))
    (lispm-desktop-settings--sandbox-custom))
  (lispm-desktop-settings--heading "Allowed Domains")
  (lispm-desktop-settings--note
   (if lispm-sandbox-allowed-domains
       (lispm-desktop-settings--truncate (string-join lispm-sandbox-allowed-domains ", ") 58)
     "None."))
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--button "Change…" #'lispm-desktop-settings--sandbox-domains
                                  "Change the domains sandboxed commands may reach")
  (insert "\n\n")
  (lispm-desktop-settings--paragraph
   "Low reaches them besides package registries and code hosts, Middle them alone; High reaches no network."))

(defun lispm-desktop-settings--startup-read-item ()
  "Ask what LispM is to open at startup; return it as a startup item, see
`lispm-desktop-startup-items'."
  (let* ((kinds '(("Application" . app) ("Document or folder" . file)
                  ("Program, run in the background" . program) ("Emacs command" . command)))
         (kind (or (cdr (assoc (completing-read "Open at startup: " kinds nil t) kinds))
                   (user-error "Choose what to open"))))
    (pcase kind
      ('app
       (let ((apps (mapcar (pcase-lambda (`(,key . ,properties)) (cons (plist-get properties :name) key))
                           lispm-desktop-apps)))
         (list :app (or (cdr (assoc (completing-read "Application: " apps nil t) apps))
                        (user-error "Choose an application")))))
      ('file
       (list :file (abbreviate-file-name (read-file-name "Document or folder: " nil nil t))))
      ('program
       (let ((command (string-trim (read-string "Program, as a shell command: "))))
         (when (string-empty-p command)
           (user-error "Type the command running the program"))
         (let* ((directory (abbreviate-file-name
                            (file-name-as-directory (read-directory-name "Run it in: " "~/" nil t))))
                (default (lispm-desktop-startup-program-name command))
                (name (string-trim (read-string "Name: " default))))
           `(:program ,command
             ,@(unless (equal directory "~/") (list :directory directory))
             ,@(unless (or (string-empty-p name) (equal name default)) (list :name name))))))
      ('command
       (list :command (intern (completing-read "Emacs command: " obarray #'commandp t)))))))

(defun lispm-desktop-settings--startup-add ()
  "Ask what LispM is to open at startup, and add it to the startup items,
after the others."
  (let* ((item (lispm-desktop-settings--startup-read-item))
         (name (lispm-desktop-startup-item-name item)))
    (when (member name (mapcar #'lispm-desktop-startup-item-name lispm-desktop-startup-items))
      (user-error "There is already a startup item called “%s”" name))
    (lispm-desktop-settings--save 'lispm-desktop-startup-items
                                  (append lispm-desktop-startup-items (list item)))))

(defun lispm-desktop-settings--startup-choose (prompt)
  "Ask with PROMPT for one of the startup items, the first by default;
return its index."
  (let ((names (or (mapcar #'lispm-desktop-startup-item-name lispm-desktop-startup-items)
                   (user-error "There are no startup items"))))
    (seq-position names (completing-read prompt names nil t nil nil (car names)))))

(defun lispm-desktop-settings--startup-remove ()
  "Ask for a startup item and take it out of the startup items.  What it
opened stays open; a program it runs runs on."
  (let ((index (lispm-desktop-settings--startup-choose "Remove the startup item: ")))
    (lispm-desktop-settings--save 'lispm-desktop-startup-items
                                  (append (seq-take lispm-desktop-startup-items index)
                                          (seq-drop lispm-desktop-startup-items (1+ index))))))

(defun lispm-desktop-settings--startup-open ()
  "Ask for a startup item and open it now; a program's output shows."
  (lispm-desktop-startup-open-item
   (nth (lispm-desktop-settings--startup-choose "Open the startup item: ") lispm-desktop-startup-items)
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
    ('app "application")
    ('file (let ((file (expand-file-name (plist-get item :file))))
             (format "%s in %s" (if (file-directory-p file) "folder" "document")
                     (abbreviate-file-name (file-name-directory (directory-file-name file))))))
    ;; How it runs first, as the command may be cut short.
    ('program (format "%s: %s"
                      (pcase (lispm-desktop-startup-program-status item)
                        ('running "running")
                        (`(exited . ,(and code (pred integerp))) (format "stopped, exit %d" code))
                        (`(exited . ,_) "stopped")
                        (_ "program"))
                      (plist-get item :program)))
    ('command (let ((command (symbol-name (plist-get item :command))))
                (if (equal command (lispm-desktop-startup-item-name item))
                    "Emacs command"
                  (format "Emacs command %s" command))))
    (_ "names nothing to open")))

(defun lispm-desktop-settings--startup ()
  "Insert the Startup panel: whether the computer opens LispM at login,
and the startup items LispM opens once started."
  (lispm-desktop-settings--heading "Login")
  (if (not (lispm-desktop-startup-login-file))
      (lispm-desktop-settings--note "This system gives LispM no way to open at login.")
    (lispm-desktop-settings--check-box
     "Open LispM when you log in" (lispm-desktop-startup-login-p)
     (lambda () (lispm-desktop-open-at-login (not (lispm-desktop-startup-login-p)))))
    (lispm-desktop-settings--note
     (lispm-desktop-settings--truncate (format "Opens %s." (lispm-desktop-startup-login-description)) 62))
    (when (eq system-type 'darwin)
      (lispm-desktop-settings--note "System Settings lists it in its Login Items.")))
  (lispm-desktop-settings--heading "Startup Items")
  (lispm-desktop-settings--note "Opened once LispM has started, in this order:")
  (if (null lispm-desktop-startup-items)
      (lispm-desktop-settings--note "None.")
    (seq-do-indexed
     (lambda (item index)
       (let ((name (lispm-desktop-startup-item-name item)))
         (lispm-desktop-settings--check-box
          name (not (plist-get item :disabled))
          (lambda () (lispm-desktop-settings--startup-toggle index))
          (lispm-desktop-settings--truncate (lispm-desktop-settings--startup-item-note item)
                                            (max 12 (- 54 (length name)))))))
     lispm-desktop-startup-items))
  (insert "\n" lispm-desktop-settings--indent)
  (lispm-desktop-settings--button "Add…" #'lispm-desktop-settings--startup-add
                                  "Add an application, a document, a program or a command")
  (when lispm-desktop-startup-items
    (insert " ")
    (lispm-desktop-settings--button "Remove…" #'lispm-desktop-settings--startup-remove
                                    "Take an item out of the startup items")
    (insert " ")
    (lispm-desktop-settings--button "Open…" #'lispm-desktop-settings--startup-open
                                    "Open an item now, or show a program's output"))
  (insert "\n\n")
  (lispm-desktop-settings--note "A program runs in the background while LispM runs, and")
  (lispm-desktop-settings--note "stops as it quits; its output is in its own buffer."))

(defun lispm-desktop-settings--when (time)
  "Return TIME as the Software Update panel tells it: today at 23:50, or
on a day before, 18 October 2026 at 09:12."
  (if (equal (format-time-string "%F" time) (format-time-string "%F"))
      (format-time-string "today at %H:%M" time)
    (format-time-string "%-d %B %Y at %H:%M" time)))

(defun lispm-desktop-settings--plural (count word)
  "Return COUNT and WORD, made plural when COUNT is not one."
  (format "%d %s%s" count word (if (= count 1) "" "s")))

(defun lispm-desktop-settings--truncate (text width)
  "Return TEXT cut to WIDTH characters, an ellipsis standing for the rest."
  (truncate-string-to-width text width nil nil "…"))

(defun lispm-desktop-settings--update-status ()
  "Insert what Software Update is doing, or found last: a line telling it,
or a line an update, Restart to Update after them."
  (let ((updates (lispm-desktop-update-updates))
        (failed (seq-filter (lambda (result) (eq (plist-get result :state) 'failed))
                            (lispm-desktop-update-results)))
        (checked (lispm-desktop-update-last-check)))
    (cond
     ((lispm-desktop-update-checking-p)
      (lispm-desktop-settings--note "Checking for updates…"))
     (updates
      (dolist (update updates)
        (let* ((changes (plist-get update :changes))
               (head (format "%s, %s" (lispm-desktop-update-title update)
                             (lispm-desktop-settings--plural (length changes) "change"))))
          (insert lispm-desktop-settings--indent (propertize head 'face 'bold))
          (when changes
            (insert (propertize (concat ": " (lispm-desktop-settings--truncate
                                              (car changes) (max 10 (- 58 (length head)))))
                                'face 'shadow)))
          (insert "\n")))
      (insert lispm-desktop-settings--indent)
      (lispm-desktop-settings--button "Restart to Update" #'lispm-desktop-update-restart
                                      "Restart LispM, installing the updates as it quits")
      (insert "  " (propertize (cond ((lispm-desktop-update-needs-installer-p) "install.sh remakes LispM.app")
                                     ((lispm-desktop-update-installs-on-quit-p) "or as LispM quits")
                                     (t "or later, from here"))
                               'face 'shadow)
              "\n"))
     (failed
      (dolist (result failed)
        (lispm-desktop-settings--note
         (lispm-desktop-settings--truncate (format "%s could not be checked: %s" (plist-get result :name)
                                                   (plist-get result :reason))
                                           64))))
     (checked
      (lispm-desktop-settings--note (format "LispM is up to date; checked %s."
                                            (lispm-desktop-settings--when checked))))
     (t (lispm-desktop-settings--note "Not checked yet.")))))

(defun lispm-desktop-settings--show-store-updates ()
  "Show the App Store's Updates."
  (lispm-desktop-store-show)
  (lispm-desktop-store-show-category 'updates))

(defun lispm-desktop-settings--applications ()
  "Insert the part of the Software Update panel about the App Store's
applications and extensions: whether their updates install by
themselves, and which wait."
  (lispm-desktop-settings--heading "Applications")
  (lispm-desktop-settings--check-box
   "Update applications automatically" lispm-desktop-store-update-automatically
   (lambda () (lispm-desktop-settings--save 'lispm-desktop-store-update-automatically
                                            (not lispm-desktop-store-update-automatically)))
   (if lispm-desktop-store-update-automatically
       "when LispM is idle, an application once closed"
     "they wait in the App Store"))
  (if-let* ((updates (lispm-desktop-store-updates)))
      (progn
        (insert lispm-desktop-settings--indent
                (format "%s can be updated  " (lispm-desktop-store--update-names updates)))
        (lispm-desktop-settings--button "Show Updates" #'lispm-desktop-settings--show-store-updates
                                        "Show them in the App Store's Updates")
        (insert "\n"))
    (lispm-desktop-settings--note "The applications from the App Store are up to date.")))

(defun lispm-desktop-settings--update ()
  "Insert the Software Update panel: Update Now, what it found, how it
checks and installs by itself, and what is installed."
  (lispm-desktop-settings--heading "Software Update")
  (lispm-desktop-settings--note "Checks for new versions of LispM and of the App Store's applications.")
  (lispm-desktop-settings--icon-button "software-update-32" "Update Now" #'lispm-desktop-update-now
                                       "Check for updates now")
  (lispm-desktop-settings--update-status)
  (lispm-desktop-settings--heading "Automatic Updates")
  (lispm-desktop-settings--check-box
   "Check for updates automatically" lispm-desktop-update-check-automatically
   (lambda () (lispm-desktop-settings--save 'lispm-desktop-update-check-automatically
                                            (not lispm-desktop-update-check-automatically))))
  (insert lispm-desktop-settings--indent lispm-desktop-settings--indent)
  (lispm-desktop-settings--radio-button "Every day" (eq lispm-desktop-update-schedule 'daily)
                                        (lambda () (lispm-desktop-settings--save 'lispm-desktop-update-schedule 'daily))
                                        "Check once a day")
  (insert "   ")
  (lispm-desktop-settings--radio-button "Every week" (eq lispm-desktop-update-schedule 'weekly)
                                        (lambda () (lispm-desktop-settings--save 'lispm-desktop-update-schedule 'weekly))
                                        "Check once a week")
  (insert "\n")
  (lispm-desktop-settings--check-box
   "Ask me before installing updates" (not lispm-desktop-update-install-automatically)
   (lambda () (lispm-desktop-settings--save 'lispm-desktop-update-install-automatically
                                            (not lispm-desktop-update-install-automatically)))
   (if lispm-desktop-update-install-automatically "else, as LispM quits" "they wait for Restart"))
  (lispm-desktop-settings--applications)
  (lispm-desktop-settings--heading "Installed")
  (let ((kept (seq-filter (lambda (result) (eq (plist-get result :state) 'kept))
                          (lispm-desktop-update-results))))
    (pcase-dolist (`(,name ,directory ,version) (lispm-desktop-update-installed))
      (insert lispm-desktop-settings--indent name
              (propertize " " 'display '(space :align-to (150)))
              (or version "?"))
      (when-let* ((result (seq-find (lambda (result) (equal (plist-get result :directory) directory)) kept)))
        (insert "  " (propertize (format "left as it is: %s" (plist-get result :reason)) 'face 'shadow)))
      (insert "\n")))
  (insert lispm-desktop-settings--indent)
  (lispm-desktop-settings--button "Show Log" #'lispm-desktop-update-show-log
                                  "List the updates ready, and what Software Update installed")
  (insert "\n"))

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
   (mapcar (pcase-lambda (`(,key ,label . ,_))
             (list label (eq key lispm-desktop-settings--panel)
                   'lispm-desktop-settings-panel key 'keymap lispm-desktop-settings-tab-map
                   'pointer 'arrow 'help-echo (format "Show %s" label)))
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
              lispm-desktop-platinum-title (nth 1 panel))
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
  (setq-local lispm-desktop-platinum-info "Choices are kept for the next start."
              header-line-format '(:eval (lispm-desktop-settings--tabs))
              lispm-desktop-platinum-title "Control Panels"
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
                            (mapcar #'cadr (lispm-desktop-settings--panels)))
                           ;; The window's sides, and a little of the
                           ;; pane's edge after the last tab.
                           24)))
        (height 500))
    (list (max 20 (/ (- (frame-inner-width screen) width) 2))
          (+ (lispm-desktop-app-window-menu-bar-height screen) 50)
          width height)))

(lispm-desktop-app-window-define 'settings
  :name "Control Panels" :geometry #'lispm-desktop-settings--default-geometry
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
  (mapcar (pcase-lambda (`(,key ,label . ,_))
            (list :label label :action (lambda () (lispm-desktop-settings-show key))))
          (lispm-desktop-settings--panels)))

(defun lispm-desktop-settings--folder-items ()
  "Return the Control Panels folder's items, see `lispm-desktop-finder-folders'."
  (mapcar (pcase-lambda (`(,key ,label ,_ ,icon . ,_))
            (list label icon (lambda () (lispm-desktop-settings-show key))))
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
  '((:label "Close Window" :action lispm-desktop-settings-close)))

(defun lispm-desktop-settings--view-items ()
  "Return the items of the Control Panels View menu: a panel each, the shown one
checked."
  (let ((shown (buffer-local-value 'lispm-desktop-settings--panel (lispm-desktop-settings--buffer))))
    (mapcar (pcase-lambda (`(,key ,label . ,_))
              (list :label label :action (lambda () (lispm-desktop-settings-show key))
                    :checked (eq key shown)))
            (lispm-desktop-settings--panels))))

(defun lispm-desktop-settings-menus ()
  "Return the Control Panels menus, for the menu bar while they are in front."
  '((lispm-desktop-settings-file "File" lispm-desktop-settings--file-items)
    (lispm-desktop-settings-view "View" lispm-desktop-settings--view-items)))

(defun lispm-desktop-setup-settings ()
  "Make Control Panels an application: its folder holds a panel each, and
its window follows the application in front and shows the theme's colours
when they change, and what is chosen elsewhere: the sandbox level chosen
with /sandbox in a chat, for one."
  (setf (alist-get 'control-panels lispm-desktop-finder-folders)
        '(:title "Control Panels" :icon "control-panels" :items lispm-desktop-settings--folder-items))
  (add-hook 'enable-theme-functions (lambda (&rest _) (lispm-desktop-settings--render)) 95)
  (add-hook 'lispm-desktop-update-changed-hook #'lispm-desktop-settings--render)
  (add-hook 'lispm-desktop-startup-changed-hook #'lispm-desktop-settings--render)
  ;; LispM Agent's, which may be loaded later, or never.
  (add-hook 'lispm-sandbox-change-hook #'lispm-desktop-settings--render))

(provide 'lispm-desktop-settings)
;;; lispm-desktop-settings.el ends here
