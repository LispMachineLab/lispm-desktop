;;; lispm-desktop-app-window.el --- The windows of installed applications  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-desktop
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; An application installed from the App Store, an Emacs package such as
;; Elfeed or Emacs's own Calculator, runs in a window of its own floating
;; over the desktop, as Control Panels does; so does the App Store.
;;
;; The window is a child frame of the screen carrying the application as
;; its frame parameter `lispm-desktop-app': selecting it brings the
;; application in front, its menus in the menu bar.  It hides from the
;; full-screen editor and browser and shows again with the desktop, opens
;; where it was last, cascading from the others the first time, and its
;; close box closes it.  What the application showed in it stays: opened
;; again, the window shows it again, as a Mac OS 9 application kept its
;; documents while its windows were closed.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-apps)

(declare-function lispm-desktop-activate "lispm-desktop-surface" ())

(defvar lispm-desktop-app-window--frames nil
  "The applications' open windows: (APP . FRAME).")

(defvar lispm-desktop-app-window--geometry nil
  "Where each application's window was last: (APP LEFT TOP WIDTH HEIGHT).")

(defconst lispm-desktop-app-window-size '(640 . 440)
  "Size in pixels of an application's window the first time it opens.")

(defconst lispm-desktop-app-window-cascade 22
  "Pixels each window opening for the first time is put right of and below
the one opened before, as Mac OS 9 cascaded new windows.")

(defun lispm-desktop-app-window-frame (app)
  "Return the window of APP, a key of `lispm-desktop-apps', while it is open."
  (let ((frame (alist-get app lispm-desktop-app-window--frames)))
    (and (frame-live-p frame) frame)))

(defun lispm-desktop-app-window-open-p (app)
  "Return non-nil while APP's window is open."
  (and (lispm-desktop-app-window-frame app) t))

(defun lispm-desktop-app-window--default-geometry (screen width height)
  "Return where a window WIDTH by HEIGHT pixels opens first over SCREEN.
It cascades from the windows open, the first under the menu bar's left."
  (let ((step (* lispm-desktop-app-window-cascade
                 (length (seq-filter (lambda (entry) (frame-live-p (cdr entry)))
                                     lispm-desktop-app-window--frames)))))
    (list (min (+ 60 step) (max 20 (- (frame-inner-width screen) width 20)))
          (+ (if (fboundp 'tab-bar-height) (tab-bar-height screen t) 0) 40 step)
          width height)))

(defun lispm-desktop-app-window--make-frame (app screen name width height)
  "Return a window of APP over SCREEN's desktop, named NAME, not yet shown.
WIDTH and HEIGHT size it the first time."
  (pcase-let* ((`(,left ,top ,width ,height)
                (or (alist-get app lispm-desktop-app-window--geometry)
                    (lispm-desktop-app-window--default-geometry screen width height)))
               (frame (let ((after-make-frame-functions nil)
                            (before-make-frame-hook nil))
                        (make-frame
                         `((parent-frame . ,screen)
                           (minibuffer . ,(minibuffer-window screen))
                           (name . ,name) (lispm-desktop-app . ,app)
                           (lispm-desktop-app-window . ,app)
                           (lispm-desktop-close-function . lispm-desktop-app-window--close-box)
                           (lispm-desktop-window . t) (top-visible . 30)
                           (undecorated . t) (visibility . nil)
                           (tab-bar-lines . 0) (tab-bar-lines-keep-state . t)
                           (menu-bar-lines . 0) (tool-bar-lines . 0)
                           (internal-border-width . 0)
                           (child-frame-border-width . ,(lispm-desktop-platinum-window-border))
                           (no-other-frame . t) (skip-taskbar . t) (desktop-dont-save . t)
                           (user-position . t)
                           (left . ,left) (top . ,top)
                           (width . (text-pixels . ,width))
                           (height . (text-pixels . ,height)))))))
    (set-face-attribute 'internal-border frame :background (lispm-desktop-platinum--color :chrome))
    frame))

(defun lispm-desktop-app-window--blank ()
  "Return the blank buffer a new application's window opens on."
  (or (get-buffer " *lispm-desktop-app-window*")
      (with-current-buffer (get-buffer-create " *lispm-desktop-app-window*")
        (setq buffer-read-only t)
        (current-buffer))))

(defun lispm-desktop-app-window--run-setup (window setup)
  "Run SETUP with WINDOW selected, a new application's window holding only
a blank buffer at first; then close what of the window still shows the
blank, as where an application split its window, Calculator its trail."
  (let ((blank (lispm-desktop-app-window--blank))
        (frame (window-frame window)))
    (set-window-buffer window blank)
    (with-selected-window window
      (funcall setup))
    (dolist (other (window-list frame 'never))
      (when (and (eq (window-buffer other) blank) (cdr (window-list frame 'never)))
        (delete-window other)))))

(cl-defun lispm-desktop-app-window-show (app &key name buffer setup size)
  "Open APP's window over the desktop, or bring it in front; return it.
NAME titles the window the window system knows.  The first time it
opens, the window shows BUFFER, dedicated to it, or SETUP, a function,
runs with the window selected to fill it, as an application's command
does; SIZE, (WIDTH . HEIGHT) in pixels, sizes it, by default
`lispm-desktop-app-window-size'."
  ;; With the desktop hidden, the window opens out on the computer's screen.
  (unless (or (lispm-desktop-apps-hidden-p) (not (fboundp 'lispm-desktop-activate)))
    (lispm-desktop-activate))
  (let ((frame (lispm-desktop-app-window-frame app)))
    (unless frame
      (let ((size (or size lispm-desktop-app-window-size)))
        (setq frame (lispm-desktop-app-window--make-frame app (lispm-desktop-apps-screen)
                                                          (or name (symbol-name app))
                                                          (car size) (cdr size))))
      (setf (alist-get app lispm-desktop-app-window--frames) frame)
      (let ((window (frame-root-window frame)))
        (cond (buffer
               (set-window-buffer window buffer)
               (set-window-dedicated-p window t))
              (setup
               (lispm-desktop-app-window--run-setup window setup)))))
    (lispm-desktop-platinum-show-window frame)
    (lispm-desktop-apps-keep-on-desktop frame)
    (lispm-desktop-platinum-raise-window frame)
    (select-frame-set-input-focus frame)
    (lispm-desktop-apps-set-frontmost app)
    frame))

(defun lispm-desktop-app-window-close (app)
  "Close APP's window, remembering where it was; the desktop comes in front."
  (when-let* ((frame (lispm-desktop-app-window-frame app)))
    (when (lispm-desktop-platinum-rolled-up-height frame)
      (lispm-desktop-platinum-roll-down frame (lispm-desktop-platinum-rolled-up-height frame)))
    (setf (alist-get app lispm-desktop-app-window--geometry) (lispm-desktop-apps--geometry frame))
    (lispm-desktop-apps-focus-after frame)
    (delete-frame frame))
  (setq lispm-desktop-app-window--frames (assq-delete-all app lispm-desktop-app-window--frames))
  (when (and (eq (lispm-desktop-apps-frontmost) app) (not (lispm-desktop-apps-hidden-p)))
    (lispm-desktop-apps-set-frontmost 'desktop)))

(defun lispm-desktop-app-window--close-box (window)
  "Close WINDOW from its close box: the window alone, while others share its
application's window, else the application's window.
This is the frame parameter `lispm-desktop-close-function' of the windows."
  (let ((frame (window-frame window)))
    (if (cdr (window-list frame 'never))
        (delete-window window)
      (lispm-desktop-app-window-close (frame-parameter frame 'lispm-desktop-app-window)))))

(defun lispm-desktop-app-window--forget (frame)
  "Forget FRAME once deleted, by its close box or otherwise.
This is one of `delete-frame-functions'."
  (when-let* ((app (frame-parameter frame 'lispm-desktop-app-window)))
    (when (eq (alist-get app lispm-desktop-app-window--frames) frame)
      (setq lispm-desktop-app-window--frames (assq-delete-all app lispm-desktop-app-window--frames)))))

(defun lispm-desktop-app-window--follow-frontmost (app)
  "Show the applications' windows unless the full-screen editor or browser
is in front, which they would cover; the application in front stays over
them.  This is one of `lispm-desktop-apps-frontmost-functions'."
  (let ((shown (not (lispm-desktop-apps-full-screen-p app))))
    (pcase-dolist (`(,_ . ,frame) lispm-desktop-app-window--frames)
      ;; Out on the computer's screen, a window stays where it is.
      (when (and (frame-live-p frame) (not (lispm-desktop-apps-out-p frame)))
        (cond ((and shown (not (frame-visible-p frame)))
               (lispm-desktop-platinum-show-window frame))
              ((and (not shown) (frame-visible-p frame))
               (lispm-desktop-platinum-hide-window frame)))))
    (when-let* ((front (and shown (lispm-desktop-app-window-frame app))))
      (lispm-desktop-platinum-raise-window front))))

(defun lispm-desktop-app-window--refocus ()
  "Give the window of the application in front the keyboard back after a
menu bar menu closes."
  (when-let* ((frame (lispm-desktop-app-window-frame (lispm-desktop-apps-frontmost))))
    (when (and (frame-visible-p frame) (not (eq (window-frame (selected-window)) frame)))
      (select-frame-set-input-focus frame))))

(defun lispm-desktop-app-window-file-items (app)
  "Return the items of APP's File menu: Close Window.
The keyboard closes it as any frame, with \\`C-x 5 0'."
  `((:label "Close Window" :action ,(lambda () (lispm-desktop-app-window-close app)) :key "C-x 5 0")))

(defun lispm-desktop-setup-app-windows ()
  "Let the applications' windows follow the application in front."
  (add-hook 'lispm-desktop-apps-frontmost-functions #'lispm-desktop-app-window--follow-frontmost)
  (add-hook 'lispm-desktop-platinum-menu-closed-hook #'lispm-desktop-app-window--refocus)
  (add-hook 'delete-frame-functions #'lispm-desktop-app-window--forget))

(provide 'lispm-desktop-app-window)
;;; lispm-desktop-app-window.el ends here
