;;; lispm-desktop-bar.el --- The LispM bar, dropped from the menu bar  -*- lexical-binding: t; -*-

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

;; LispM Agent's bar (lispm-bar.el), the way into the whole system, gets
;; a title of its own at the menu bar's right end, beside the clock,
;; where Mac OS X put Spotlight's.  Clicked, or with C-c l or M-x ai, it
;; drops its field under the title: a child frame of its own minibuffer,
;; so that history and the minibuffer's keys work there as anywhere,
;; and under it a panel holding what the field finds and LispM's reply,
;; coloured as a menu.  A click elsewhere closes it, as Escape does.
;;
;; The desktop gives the bar what it finds of the desktop's own: the
;; applications, those built in and those the App Store installed, and
;; the Control Panels, each opened by Return; and it says what LispM
;; does when it opens one.  On a text terminal the bar keeps LispM
;; Agent's own field, the minibuffer, and the window over it.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-popup)
(require 'lispm-desktop-i18n)

(declare-function lispm-desktop-lispm-terminal "lispm-desktop-lispm" (&optional agent))

(defvar lispm-bar-source-functions)
(defvar lispm-bar-read-function)
(defvar lispm-bar-show-function)
(defvar lispm-bar-hide-function)
(defvar lispm-bar-triangle-function)
(defvar lispm-bar-ask-icon)
(defvar lispm-bar-activity-functions)
(defvar lispm-desktop-store-installed)
(declare-function lispm-ai "lispm-bar" ())
(declare-function lispm-bar-open-p "lispm-bar" ())
(declare-function lispm-bar-expanded-p "lispm-bar" ())
(declare-function lispm-bar-show-in-window "lispm-bar" (buffer))
(declare-function lispm-bar-hide-window "lispm-bar" (buffer))
(declare-function lispm-desktop-store-show "lispm-desktop-store" ())
(declare-function lispm-desktop-settings-show "lispm-desktop-settings" (&optional panel))
(declare-function lispm-desktop-settings--panels "lispm-desktop-settings" (&optional category))
(declare-function lispm-desktop-settings-panel-label "lispm-desktop-settings" (panel))
(declare-function lispm-desktop-settings--categories "lispm-desktop-settings" ())
(declare-function lispm-desktop-settings-category-label "lispm-desktop-settings" (category))
(declare-function lispm-desktop-settings-show-category "lispm-desktop-settings" (category))
(defvar lispm-desktop-settings-categories)

(defgroup lispm-desktop-bar nil
  "The LispM bar, dropped from the menu bar."
  :group 'lispm-desktop
  :prefix "lispm-desktop-bar-")

(defcustom lispm-desktop-bar-width 480
  "Width in pixels of the LispM bar's field and of the panel under it."
  :type 'natnum
  :group 'lispm-desktop-bar)

;;;; What the bar finds of the desktop's

(defun lispm-desktop-bar--application (name icon open &rest fields)
  "Return the application NAME, its ICON's name, for the bar to find.
OPEN opens it; FIELDS are more texts it is found by, as a subtitle."
  (list :kind 'application :name name
        :icon (lispm-desktop-platinum-icon icon)
        :fields (cons (cons 3 name) (delq nil (mapcar (lambda (text) (and text (cons 1.5 text))) fields)))
        :open open))

(defun lispm-desktop-bar--focus-mode ()
  "Return Focus Mode, LispM's terminal entered at once, for the bar to
find, by its name in English and in the language spoken, and by
\"terminal\" and \"console\"."
  (let ((name (lispm-desktop-tr "Focus Mode")))
    (list :kind 'mode :label (lispm-desktop-tr "Mode") :name name
          :icon (lispm-desktop-platinum-icon "terminal")
          :detail (lispm-desktop-tr "LispM's terminal: the sessions and a listener, by keyboard")
          :fields `((3 . ,name) (3 . "Focus Mode") (1.5 . "terminal console")
                    (1.5 . ,(lispm-desktop-tr "LispM's terminal: the sessions and a listener, by keyboard")))
          :open #'lispm-desktop-lispm-terminal)))

(defun lispm-desktop-bar-applications ()
  "Return the desktop's applications, and the Control Panels, for the
LispM bar to find, as `lispm-bar-source-functions' gives them: those of
the Application menu that can run here, but the desktop and the control
panels, those the App Store installed as its catalogs describe them, the
App Store; Focus Mode, where LispM Agent has it; then a panel each of the
control panels, and those of them not named as one of their panels is."
  (let ((things nil))
    (pcase-dolist (`(,key . ,props) lispm-desktop-apps)
      (let ((catalog (alist-get key lispm-desktop-store-installed)))
        ;; Those closed too, as what the App Store installed.
        (when (and (not (eq key 'desktop))
                   (not (assq key (bound-and-true-p lispm-desktop-settings-categories)))
                   (lispm-desktop-apps-openable-p key))
          (push (lispm-desktop-bar--application
                 (lispm-desktop-apps-name key) (plist-get props :icon)
                 (lambda () (lispm-desktop-apps-activate key))
                 (plist-get catalog :subtitle) (plist-get catalog :name))
                things))))
    (when (and (fboundp 'lispm-desktop-store-show)
               (not (seq-find (lambda (thing) (equal (plist-get thing :name) "App Store")) things)))
      (push (lispm-desktop-bar--application "App Store" "app-store" #'lispm-desktop-store-show) things))
    (when (lispm-desktop-lispm-terminal-available-p)
      (push (lispm-desktop-bar--focus-mode) things))
    (when (fboundp 'lispm-desktop-settings--panels)
      (let ((panels (lispm-desktop-settings--panels)))
        (dolist (panel panels)
          (push (list :kind 'control-panel :label (lispm-desktop-tr "Control Panel")
                      :name (lispm-desktop-settings-panel-label panel)
                      :icon (lispm-desktop-platinum-icon (or (nth 3 panel) "control-panels"))
                      :open (let ((key (car panel))) (lambda () (lispm-desktop-settings-show key))))
                things))
        (dolist (category (lispm-desktop-settings--categories))
          (let ((name (lispm-desktop-settings-category-label category)))
            (unless (member name (mapcar #'lispm-desktop-settings-panel-label panels))
              (push (list :kind 'control-panel :label (lispm-desktop-tr "Control Panel")
                          :name name :icon (lispm-desktop-platinum-icon (nth 3 category))
                          :open (let ((key (car category)))
                                  (lambda () (lispm-desktop-settings-show-category key))))
                    things))))))
    (nreverse things)))

(defun lispm-desktop-bar-activity (tool input)
  "Return what LispM does with a call of TOOL on INPUT, of the desktop's
tools, for the bar's activity line, as \"Opening Stickies…\"; or nil."
  (let ((name (lambda (key)
                (or (and (stringp key) (lispm-desktop-apps-name (intern-soft key))) key))))
    (pcase tool
      ("desktop_open" (lispm-desktop-tr "Opening %s…" (funcall name (alist-get 'app input))))
      ("store_install" (lispm-desktop-tr "Installing %s…" (alist-get 'id input)))
      ("store_show" (lispm-desktop-tr "Opening %s…" "App Store"))
      ("settings_show" (lispm-desktop-tr "Opening %s…" (lispm-desktop-tr "Control Panels"))))))

;;;; The field and the panel

(defvar lispm-desktop-bar--field nil
  "The LispM bar's field: a child frame of the screen, of its own minibuffer only.")

(defun lispm-desktop-bar-field-p (frame)
  "Return non-nil when FRAME is the LispM bar's field."
  (and frame (eq frame lispm-desktop-bar--field)))

(defvar lispm-desktop-bar--panel nil
  "The LispM bar's panel under its field: a child frame showing the bar's lines.")

(defvar lispm-desktop-bar--room nil
  "How tall the panel may grow while the bar is open, in pixels:
\(COMPACT . OPENED-OUT), measured once as it opens.  Measuring the
screen lays out the menu bar, the tab bar: done on every redraw of the
spinner, it once crashed Emacs 31.1 in the bidi iterator.")

(defvar lispm-desktop-bar--focused nil
  "The frames whose keys go to the bar's field while it is open.")

(defvar lispm-desktop-bar--return-to nil
  "The frame selected once the bar closes: the one it was opened from, or
the last one LispM selected while it was open, as an application opened.")

(defun lispm-desktop-bar--graphic-p ()
  "Return non-nil when the bar drops from the menu bar: on a graphical
display, the desktop drawn.  On a text terminal it keeps the minibuffer."
  (and (display-graphic-p) (lispm-desktop-popup-graphic-p)))

(defun lispm-desktop-bar--make-frame (screen minibuffer)
  "Return a hidden child frame of SCREEN for the bar, its MINIBUFFER as
`make-frame' takes it: `only' for the field."
  (let ((after-make-frame-functions nil)
        (before-make-frame-hook nil))
    (make-frame
     `((parent-frame . ,screen) (minibuffer . ,minibuffer) (name . "LispM")
       (undecorated . t) (visibility . nil) (unsplittable . t)
       ;; The keys typed come to the field, from whichever frame they are
       ;; typed in, see `lispm-desktop-bar-read'.
       (no-accept-focus . t) (no-focus-on-map . t)
       (no-other-frame . t) (skip-taskbar . t) (desktop-dont-save . t)
       (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
       (child-frame-border-width . 1) (internal-border-width . 6)
       (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
       (left-fringe . 0) (right-fringe . 0) (line-spacing . 2)
       (no-special-glyphs . t) (cursor-type . bar)
       (width . 60) (height . 1)))))

(defun lispm-desktop-bar--paint (frame background)
  "Give FRAME, the bar's field or panel, BACKGROUND and the palette's text
and outline, and the menus' font."
  (modify-frame-parameters frame `((background-color . ,background)
                                   (foreground-color . ,(lispm-ui-color :text))))
  (set-face-attribute 'child-frame-border frame :background (lispm-ui-color :outline))
  (set-face-attribute 'internal-border frame :background background)
  (set-face-attribute 'fringe frame :background background)
  (let ((family (face-attribute 'lispm-desktop-popup-item :family nil 'default)))
    (when (stringp family)
      (set-face-attribute 'default frame :family family))))

(defun lispm-desktop-bar--frames (screen)
  "Return the bar's field and panel over SCREEN, (FIELD . PANEL), made when
missing, painted in the palette now in use."
  ;; Killing the bar's buffer deletes the panel, its window dedicated.
  (unless (and (frame-live-p lispm-desktop-bar--field)
               (frame-live-p lispm-desktop-bar--panel)
               (eq (frame-parent lispm-desktop-bar--field) screen))
    (when (frame-live-p lispm-desktop-bar--panel)
      (delete-frame lispm-desktop-bar--panel t))
    (when (frame-live-p lispm-desktop-bar--field)
      (delete-frame lispm-desktop-bar--field t))
    (setq lispm-desktop-bar--field (lispm-desktop-bar--make-frame screen 'only))
    (setq lispm-desktop-bar--panel (lispm-desktop-bar--make-frame
                                    screen (minibuffer-window lispm-desktop-bar--field)))
    (let ((window (frame-root-window lispm-desktop-bar--panel)))
      (set-window-dedicated-p window nil)
      (set-window-fringes window 0 0)))
  (let ((field lispm-desktop-bar--field)
        (panel lispm-desktop-bar--panel))
    ;; A text field is white, as Mac OS 9's; the panel is a menu.
    (lispm-desktop-bar--paint field (lispm-ui-grey lispm-ui-current-palette #xFF))
    (lispm-desktop-bar--paint panel (lispm-ui-color :chrome))
    (set-face-attribute 'lispm-bar-selected panel :inherit nil :extend t
                        :background (lispm-ui-color :highlight)
                        :foreground (lispm-ui-color :highlight-text))
    (set-face-attribute 'lispm-bar-dim panel :inherit nil :foreground (lispm-ui-color :dim))
    (cons field panel)))

(defun lispm-desktop-bar--x (screen)
  "Return where the bar's left edge goes on SCREEN: its right edge under the
menu bar's right end, where the bar's title is."
  (max 0 (- (frame-inner-width screen) lispm-desktop-bar-width 16)))

(defun lispm-desktop-bar--place-field (field panel)
  "Put FIELD, the bar's, under the menu bar's right end, a line tall, and
PANEL, empty, under it; note how tall the panel may grow, see
`lispm-desktop-bar--room'."
  (let ((screen (frame-parent field))
        (top (lispm-desktop-platinum--menu-y)))
    (lispm-desktop-popup-set-size field lispm-desktop-bar-width (frame-char-height field))
    (set-frame-position field (lispm-desktop-bar--x screen) top)
    (lispm-desktop-popup-set-size panel lispm-desktop-bar-width (frame-char-height panel))
    ;; The two share a line, as one panel.
    (pcase-let ((`(,_ ,_ ,_ ,bottom) (frame-edges field 'outer-edges)))
      (set-frame-position panel (lispm-desktop-bar--x screen) (1- bottom)))
    (let* ((screen-height (frame-native-height screen))
           (below (- screen-height top (frame-pixel-height field) 16)))
      (setq lispm-desktop-bar--room (cons (min below (floor (* screen-height 0.5)))
                                          (min below (floor (* screen-height 0.66))))))))

(defun lispm-desktop-bar--keep-in-front ()
  "Keep the bar over the windows opened while it is open, as when LispM
opens an application, and send it the keys typed in them.  A frame
selected meanwhile would take the minibuffer the field reads from: the
field is selected again, and that frame once the bar closes."
  (let ((field lispm-desktop-bar--field)
        (panel lispm-desktop-bar--panel))
    (when (and (lispm-bar-open-p) (frame-live-p field) (frame-visible-p field)
               (active-minibuffer-window))
      (unless (memq (selected-frame) (list field panel))
        (setq lispm-desktop-bar--return-to (selected-frame))
        (select-frame field 'norecord))
      (let ((new nil))
        (dolist (frame (frame-list))
          (unless (or (memq frame (list field panel)) (memq frame lispm-desktop-bar--focused)
                      (not (frame-visible-p frame))
                      ;; Menus and the like never have the keyboard.
                      (frame-parameter frame 'no-accept-focus))
            (redirect-frame-focus frame field)
            (push frame lispm-desktop-bar--focused)
            (setq new t)))
        (let ((top (seq-find #'frame-visible-p (ignore-errors (frame-list-z-order (frame-parent field))))))
          (when (or new (and top (not (memq top (list field panel)))))
            (when (frame-visible-p panel)
              (raise-frame panel))
            (raise-frame field)))))))

(defun lispm-desktop-bar--selection-changed (_frame)
  "Keep the bar in front once another window is selected, as LispM's
applications are when they open.  This is a `window-selection-change-functions'
function while the bar is open."
  (run-at-time 0.05 nil #'lispm-desktop-bar--keep-in-front))

(defun lispm-desktop-bar-show (buffer)
  "Show BUFFER, the bar's lines, in the panel under its field, as tall as
they are, up to half the screen, or two thirds of it opened out.  This is
`lispm-bar-show-function'; on a text terminal the bar shows in a window."
  (let ((panel lispm-desktop-bar--panel)
        (field lispm-desktop-bar--field))
    (if (not (and (frame-live-p panel) (frame-visible-p field)))
        (lispm-bar-show-in-window buffer)
      (let ((window (frame-root-window panel)))
        (unless (eq (window-buffer window) buffer)
          (set-window-buffer window buffer)
          (set-window-dedicated-p window t)
          (with-current-buffer buffer
            (lispm-desktop-popup-hold-size)))
        ;; The size only: the spinner redraws four times a second.
        (let* ((most (if (lispm-bar-expanded-p) (cdr lispm-desktop-bar--room) (car lispm-desktop-bar--room)))
               (size (cons lispm-desktop-bar-width
                           (max (frame-char-height panel)
                                (cdr (window-text-pixel-size window nil nil nil (max 1 (or most 400))))))))
          (unless (equal size (frame-parameter panel 'lispm-desktop-popup-size))
            (lispm-desktop-popup-set-size panel (car size) (cdr size))))
        (with-selected-window window
          (when (>= (point) (point-max))
            (recenter -1)))
        (unless (frame-visible-p panel)
          (make-frame-visible panel))
        (lispm-desktop-bar--keep-in-front)))))

(defun lispm-desktop-bar-hide (buffer)
  "Hide the bar's panel and field once it closes; BUFFER is its lines'.
This is `lispm-bar-hide-function'."
  (lispm-bar-hide-window buffer)
  (dolist (frame (list lispm-desktop-bar--panel lispm-desktop-bar--field))
    (when (and (frame-live-p frame) (frame-visible-p frame))
      (make-frame-invisible frame t))))

(defun lispm-desktop-bar--close-elsewhere ()
  "Close the bar once a window of another frame is selected, as a click
elsewhere does.  This is a `post-command-hook' function while it reads."
  (when (and (lispm-bar-open-p)
             (active-minibuffer-window)
             (not (memq (selected-frame) (list lispm-desktop-bar--field lispm-desktop-bar--panel))))
    (run-at-time 0 nil (lambda ()
                         (when-let* ((field (active-minibuffer-window)))
                           (with-selected-window field
                             (abort-recursive-edit)))))))

(defun lispm-desktop-bar-read (read)
  "Call READ, which reads the bar's field, in the field dropped from the
menu bar, with the bar's title highlighted meanwhile.  The frames keys
are typed in send them to the field, which takes no focus from them.
This is `lispm-bar-read-function'; on a text terminal READ reads from the
minibuffer as ever."
  (if (not (lispm-desktop-bar--graphic-p))
      (funcall read)
    (pcase-let* ((screen (lispm-desktop-apps-screen))
                 (`(,field . ,panel) (lispm-desktop-bar--frames screen))
                 (from (selected-frame))
                 (minibuffer-follows-selected-frame nil)
                 (resize-mini-frames nil))
      (setq lispm-desktop-bar--focused (delete-dups (list from screen panel))
            lispm-desktop-bar--return-to from)
      (lispm-desktop-platinum-with-open-menu 'lispm-desktop-bar
        (lispm-desktop-bar--place-field field panel)
        (make-frame-visible field)
        (unwind-protect
            (progn
              (dolist (frame lispm-desktop-bar--focused)
                (redirect-frame-focus frame field))
              (add-hook 'post-command-hook #'lispm-desktop-bar--close-elsewhere)
              (add-hook 'window-selection-change-functions #'lispm-desktop-bar--selection-changed)
              (select-frame field 'norecord)
              (funcall read))
          (remove-hook 'post-command-hook #'lispm-desktop-bar--close-elsewhere)
          (remove-hook 'window-selection-change-functions #'lispm-desktop-bar--selection-changed)
          (dolist (frame lispm-desktop-bar--focused)
            (when (and (frame-live-p frame) (eq (frame-focus frame) field))
              (redirect-frame-focus frame nil)))
          (setq lispm-desktop-bar--focused nil)
          (let ((to (if (frame-live-p lispm-desktop-bar--return-to) lispm-desktop-bar--return-to from)))
            (setq lispm-desktop-bar--return-to nil)
            (when (frame-live-p to)
              (select-frame to 'norecord)))
          (dolist (frame (list panel field))
            (when (frame-live-p frame)
              (make-frame-invisible frame t))))))))

;;;; The menu bar's title

(defun lispm-desktop-bar-open ()
  "Open the LispM bar under its title, or close it if it is open."
  (interactive)
  (lispm-ai))

(defun lispm-desktop-bar-status ()
  "Return the bar's title for the menu bar's right end: a magnifying glass,
as Spotlight's.  This is a `lispm-desktop-platinum-status-functions'
function."
  (list (lispm-desktop-platinum-status-title 'lispm-desktop-bar "find" nil #'lispm-desktop-bar-open
                                             (lispm-desktop-tr "Find, or ask LispM"))))

(defun lispm-desktop-setup-bar ()
  "Give the LispM bar its title in the menu bar, its field and panel under
it, what it finds of the desktop's and its key, C-c l.  Nothing is done
without LispM Agent's bar, lispm-bar.el."
  (when (require 'lispm-bar nil t)
    (add-hook 'lispm-bar-source-functions #'lispm-desktop-bar-applications)
    (add-hook 'lispm-bar-activity-functions #'lispm-desktop-bar-activity)
    (setq lispm-bar-read-function #'lispm-desktop-bar-read
          lispm-bar-show-function #'lispm-desktop-bar-show
          lispm-bar-hide-function #'lispm-desktop-bar-hide
          lispm-bar-triangle-function #'lispm-desktop-platinum-triangle
          lispm-bar-ask-icon (lambda () (lispm-desktop-platinum-icon 'lambda)))
    (add-hook 'lispm-desktop-platinum-status-functions #'lispm-desktop-bar-status)
    (keymap-global-set "C-c l" #'lispm-ai)
    (force-mode-line-update t)))

(provide 'lispm-desktop-bar)
;;; lispm-desktop-bar.el ends here
