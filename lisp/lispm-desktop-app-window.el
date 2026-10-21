;;; lispm-desktop-app-window.el --- The windows floating over the desktop  -*- lexical-binding: t; -*-

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

;; The windows floating over the desktop, child frames of the screen: the
;; Finder window, Control Panels, the App Store, an installed
;; application's window, the floating editor and the floating browser.
;; Each is of a kind, defined with `lispm-desktop-app-window-define'; a
;; kind not defined is an installed application's, an Emacs package such
;; as Elfeed or Emacs's own Calculator, as the App Store opens one.
;;
;; A window carries its application as its frame parameter
;; `lispm-desktop-app': selecting it brings the application in front, its
;; menus in the menu bar.  It hides from the full-screen editor and browser
;; and shows again with the desktop, opens where it was last, cascading
;; from the others the first time unless its kind puts it elsewhere, rolls
;; up into its title bar, and its close box closes it.  What an
;; application showed in its window stays: opened again, the window shows
;; it again, as a Mac OS 9 application kept its documents while its
;; windows were closed.
;;
;; The editor and the browser float only some of the time, and their own
;; code opens, hides and closes their windows; they take the frame, the
;; rolling up and the keyboard after a menu from here.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-apps)

(declare-function lispm-desktop-activate "lispm-desktop-surface" ())

(defvar lispm-desktop-app-window-kinds nil
  "The kinds of floating windows: (ID . PROPERTIES).
See `lispm-desktop-app-window-define'.")

(defvar lispm-desktop-app-window--frames nil
  "The floating windows open: (ID . FRAME).")

(defvar lispm-desktop-app-window--geometry nil
  "Where each kind's window was last: (ID LEFT TOP WIDTH HEIGHT).")

(defconst lispm-desktop-app-window-size '(640 . 440)
  "Size in pixels of an application's window the first time it opens.")

(defconst lispm-desktop-app-window-cascade 22
  "Pixels each window opening for the first time is put right of and below
the one opened before, as Mac OS 9 cascaded new windows.")

(defun lispm-desktop-app-window-define (id &rest properties)
  "Make ID a kind of floating window with PROPERTIES; return ID.
A kind not defined is an installed application's window.  PROPERTIES:

:app        the application the window puts in front, a key of
            `lispm-desktop-apps'; ID by default.
:name       the window's name, which the window system knows.
:geometry   a function of the screen returning where the window opens
            the first time, (LEFT TOP WIDTH HEIGHT) in pixels; without
            it, the window cascades from the others.
:size       (WIDTH . HEIGHT), the size of a window that cascades, by
            default `lispm-desktop-app-window-size'.
:buffer     a function returning the buffer the window shows, dedicated
            to it.
:params     frame parameters of the window's own.
:after-make a function called with the window's frame once it is made.
:documents  `keep' when documents opened from the window stay in it, as an
            application's do; see lispm-desktop-editor.el.
:close      a function closing a window of its frame from the window's
            close box when the window's buffer names none, as an
            application's does with `lispm-desktop-app-window-close-box';
            see `lispm-desktop-platinum-close-window'.
:roll       how the window rolls up into its title bar: `chrome' puts
            its header and bottom line away; a function, called with the
            frame selected, leaves one window in it and returns the height
            in pixels to roll up to, or nil not to; nil rolls nothing.
:on-close   a function called once the window was closed.
:hidden-by  a function of the application in front, non-nil when the
            window hides from it; `lispm-desktop-apps-full-screen-p' by
            default.
:follow     nil when the window's own code shows and hides it as the
            application in front changes.
:raise      nil when the window does not come over the others while its
            application comes in front.
:refocus-p  a function deciding whether the window takes the keyboard
            back after a menu bar menu closes, while its application is
            in front."
  (setf (alist-get id lispm-desktop-app-window-kinds) properties)
  id)

(defconst lispm-desktop-app-window-application
  '(:documents keep :close lispm-desktop-app-window-close-box)
  "The properties of an installed application's window, of a kind not
defined; see `lispm-desktop-app-window-define'.")

(defun lispm-desktop-app-window--get (id property &optional default)
  "Return PROPERTY of the kind ID, or DEFAULT when it gives none."
  (let ((properties (if-let* ((kind (assq id lispm-desktop-app-window-kinds)))
                        (cdr kind)
                      lispm-desktop-app-window-application)))
    (if (plist-member properties property) (plist-get properties property) default)))

(defun lispm-desktop-app-window--app (id)
  "Return the application the window of kind ID puts in front."
  (lispm-desktop-app-window--get id :app id))

(defun lispm-desktop-app-window-frame (id)
  "Return the window of kind ID while it is open."
  (let ((frame (alist-get id lispm-desktop-app-window--frames)))
    (and (frame-live-p frame) frame)))

(defun lispm-desktop-app-window-open-p (id)
  "Return non-nil while the window of kind ID is open."
  (and (lispm-desktop-app-window-frame id) t))

(defun lispm-desktop-app-window-id (frame)
  "Return the kind of FRAME, a floating window, or nil."
  (frame-parameter frame 'lispm-desktop-window-kind))

(defun lispm-desktop-app-window-of-app (app)
  "Return the open window of the application APP, or nil."
  (seq-some (pcase-lambda (`(,id . ,frame))
              (and (frame-live-p frame) (eq (lispm-desktop-app-window--app id) app) frame))
            lispm-desktop-app-window--frames))

(defun lispm-desktop-app-window-menu-bar-height (screen)
  "Return the height in pixels of SCREEN's menu bar, its tab bar; 0 where
Emacs has no window system, and so no tab bar."
  (if (fboundp 'tab-bar-height) (tab-bar-height screen t) 0))

(defun lispm-desktop-app-window--cascade (screen width height)
  "Return where a window WIDTH by HEIGHT pixels opens first over SCREEN.
It cascades from the windows open that cascade, the first under the menu
bar's left."
  (let ((step (* lispm-desktop-app-window-cascade
                 (seq-count (pcase-lambda (`(,id . ,frame))
                              (and (frame-live-p frame)
                                   (not (lispm-desktop-app-window--get id :geometry))))
                            lispm-desktop-app-window--frames))))
    (list (min (+ 60 step) (max 20 (- (frame-inner-width screen) width 20)))
          (+ (lispm-desktop-app-window-menu-bar-height screen) 40 step)
          width height)))

(defun lispm-desktop-app-window-make-frame (id screen &optional name size)
  "Return a new window of kind ID over SCREEN's desktop, not yet shown.
It opens where the window of its kind was last, else where its kind puts
it first.  NAME and SIZE stand in for the kind's :name and :size."
  (pcase-let* ((size (or size (lispm-desktop-app-window--get id :size) lispm-desktop-app-window-size))
               (`(,left ,top ,width ,height)
                (or (alist-get id lispm-desktop-app-window--geometry)
                    (if-let* ((geometry (lispm-desktop-app-window--get id :geometry)))
                        (funcall geometry screen)
                      (lispm-desktop-app-window--cascade screen (car size) (cdr size)))))
               (frame (let ((after-make-frame-functions nil)
                            (before-make-frame-hook nil))
                        (make-frame
                         `((parent-frame . ,screen)
                           (minibuffer . ,(minibuffer-window screen))
                           (name . ,(or name (lispm-desktop-app-window--get id :name) (symbol-name id)))
                           (lispm-desktop-app . ,(lispm-desktop-app-window--app id))
                           (lispm-desktop-window-kind . ,id)
                           ,@(when (eq (lispm-desktop-app-window--get id :documents) 'keep)
                               `((lispm-desktop-app-window . ,id)))
                           ,@(when-let* ((close (lispm-desktop-app-window--get id :close)))
                               `((lispm-desktop-close-function . ,close)))
                           ,@(lispm-desktop-app-window--get id :params)
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
    (set-face-attribute 'internal-border frame :background (lispm-desktop-platinum-color :chrome))
    (setf (alist-get id lispm-desktop-app-window--frames) frame)
    (let ((window (frame-root-window frame)))
      (cond ((lispm-desktop-app-window--get id :buffer)
             (set-window-buffer window (funcall (lispm-desktop-app-window--get id :buffer)))
             (set-window-dedicated-p window t))
            ;; A new frame shows some buffer of before, of another
            ;; application perhaps: an application's shows the blank one
            ;; until it is filled.
            ((eq (lispm-desktop-app-window--get id :documents) 'keep)
             (set-window-buffer window (lispm-desktop-app-window-blank)))))
    (when-let* ((after-make (lispm-desktop-app-window--get id :after-make)))
      (funcall after-make frame))
    frame))

(defun lispm-desktop-app-window-blank ()
  "Return the blank buffer a new window opens on, before it is filled."
  (or (get-buffer " *lispm-desktop-app-window*")
      (with-current-buffer (get-buffer-create " *lispm-desktop-app-window*")
        (setq buffer-read-only t)
        (current-buffer))))

(defun lispm-desktop-app-window--run-setup (window setup)
  "Run SETUP with WINDOW selected, a new application's window holding only
a blank buffer at first; then close what of the window still shows the
blank, as where an application split its window, Calculator its trail."
  (let ((blank (lispm-desktop-app-window-blank))
        (frame (window-frame window)))
    (set-window-buffer window blank)
    (with-selected-window window
      (funcall setup))
    (dolist (other (window-list frame 'never))
      (when (and (eq (window-buffer other) blank) (cdr (window-list frame 'never)))
        (delete-window other)))))

(defun lispm-desktop-app-window-ensure (id &optional name size)
  "Return the window of kind ID, made when missing, not yet shown.
The desktop comes in front first, unless it is hidden in the Dock: then
the window opens out on the computer's screen.  NAME and SIZE stand in
for the kind's :name and :size."
  (unless (or (lispm-desktop-apps-hidden-p) (not (fboundp 'lispm-desktop-activate)))
    (lispm-desktop-activate))
  (or (lispm-desktop-app-window-frame id)
      (lispm-desktop-app-window-make-frame id (lispm-desktop-apps-screen) name size)))

(defun lispm-desktop-app-window-present (id)
  "Show the window of kind ID, kept on the desktop, over the others and
with the keyboard; put its application in front.  Return the window."
  (when-let* ((frame (lispm-desktop-app-window-frame id)))
    (lispm-desktop-platinum-show-window frame)
    (lispm-desktop-apps-keep-on-desktop frame)
    (lispm-desktop-platinum-raise-window frame)
    (select-frame-set-input-focus frame)
    (lispm-desktop-apps-set-frontmost (lispm-desktop-app-window--app id))
    frame))

(cl-defun lispm-desktop-app-window-show (id &key name buffer setup size)
  "Open the window of kind ID over the desktop, or bring it in front; return it.
NAME titles the window the window system knows.  The first time it
opens, the window shows BUFFER, dedicated to it, or SETUP, a function,
runs with the window selected to fill it, as an application's command
does; SIZE, (WIDTH . HEIGHT) in pixels, sizes it, by default the kind's
:size or `lispm-desktop-app-window-size'."
  (let* ((open (lispm-desktop-app-window-frame id))
         (frame (lispm-desktop-app-window-ensure id name size)))
    (unless open
      (let ((window (frame-root-window frame)))
        (cond (buffer
               (set-window-buffer window buffer)
               (set-window-dedicated-p window t))
              (setup
               (lispm-desktop-app-window--run-setup window setup)))))
    (lispm-desktop-app-window-present id)))

;;;; Rolling up

(defun lispm-desktop-app-window-rolled-p (frame)
  "Return non-nil while FRAME, a floating window, is rolled up."
  (and (frame-live-p frame) (lispm-desktop-platinum-rolled-up-height frame) t))

(defun lispm-desktop-app-window-roll-up (frame)
  "Roll FRAME, a floating window, up into its title bar, as its kind's
:roll does it; return non-nil once it is rolled up."
  (let ((roll (lispm-desktop-app-window--get (lispm-desktop-app-window-id frame) :roll)))
    (unless (or (not roll) (lispm-desktop-app-window-rolled-p frame))
      (if (eq roll 'chrome)
          (let ((window (frame-root-window frame)))
            (set-window-parameter window 'header-line-format 'none)
            (set-window-parameter window 'mode-line-format 'none)
            (lispm-desktop-platinum-roll-up frame (window-tab-line-height window))
            t)
        (with-selected-frame frame
          (let ((config (current-window-configuration)))
            (when-let* ((height (funcall roll frame)))
              (set-frame-parameter frame 'lispm-desktop-app-window-unrolled config)
              (lispm-desktop-platinum-roll-up frame height)
              t)))))))

(defun lispm-desktop-app-window-roll-down (frame)
  "Roll FRAME, a floating window rolled up, down again as it was."
  (when (lispm-desktop-app-window-rolled-p frame)
    (let ((config (frame-parameter frame 'lispm-desktop-app-window-unrolled))
          (window (frame-root-window frame)))
      (lispm-desktop-platinum-roll-down frame (lispm-desktop-platinum-rolled-up-height frame))
      (set-window-parameter window 'header-line-format nil)
      (set-window-parameter window 'mode-line-format nil)
      (when config
        (set-frame-parameter frame 'lispm-desktop-app-window-unrolled nil)
        (with-selected-frame frame
          (set-window-configuration config))))))

(defun lispm-desktop-app-window-toggle-roll (frame)
  "Roll FRAME, a floating window, up into its title bar, or down again."
  (when (frame-live-p frame)
    (if (lispm-desktop-app-window-rolled-p frame)
        (lispm-desktop-app-window-roll-down frame)
      (lispm-desktop-app-window-roll-up frame))))

(defun lispm-desktop-app-window-collapse-box (window)
  "Roll WINDOW's floating window up into its title bar, or down again, from
its collapse box, as Mac OS 9's WindowShade did."
  (lispm-desktop-app-window-toggle-roll (window-frame window)))

;;;; Closing

(defun lispm-desktop-app-window-keyboard-p (frame)
  "Return non-nil when FRAME, a floating window, has the keyboard: it is
selected, or a menu bar menu took the keyboard from it, the window of the
application in front, to give it back as it closes, see
`lispm-desktop-app-window--refocus'."
  (or (eq (selected-frame) frame)
      (and (lispm-desktop-apps-screen-p (selected-frame))
           (frame-visible-p frame)
           (eq (frame-parameter frame 'lispm-desktop-app) (lispm-desktop-apps-frontmost))
           (funcall (lispm-desktop-app-window--get (lispm-desktop-app-window-id frame)
                                                   :refocus-p #'always)))))

(defun lispm-desktop-app-window--retire (frame)
  "Forget FRAME, a window of a kind going away, as its close box closes it
or \\[delete-frame] deletes it: rolled down first, its place remembered,
the keyboard passed on when it had it, to the window behind and its
application, see `lispm-desktop-apps-focus-after'; with no window behind,
the desktop put in front when FRAME's application was.  A window no
longer open is left alone."
  (when-let* ((id (lispm-desktop-app-window-id frame))
              ((eq (alist-get id lispm-desktop-app-window--frames) frame)))
    (lispm-desktop-app-window-roll-down frame)
    (setf (alist-get id lispm-desktop-app-window--geometry) (lispm-desktop-apps--geometry frame))
    (setq lispm-desktop-app-window--frames (assq-delete-all id lispm-desktop-app-window--frames))
    (let ((next (and (lispm-desktop-app-window-keyboard-p frame)
                     (lispm-desktop-apps-focus-after frame))))
      (when-let* ((on-close (lispm-desktop-app-window--get id :on-close)))
        (funcall on-close))
      (let ((app (lispm-desktop-app-window--app id)))
        (when (and (not next) (eq (lispm-desktop-apps-frontmost) app) (not (eq app 'desktop))
                   (not (lispm-desktop-apps-hidden-p)))
          (lispm-desktop-apps-set-frontmost 'desktop))))))

(defun lispm-desktop-app-window-release (frame)
  "Forget FRAME as its kind's window, its place remembered, for its own code
to delete it: the floating editor or browser going full screen or away."
  (when-let* ((id (lispm-desktop-app-window-id frame))
              ((eq (alist-get id lispm-desktop-app-window--frames) frame)))
    (setf (alist-get id lispm-desktop-app-window--geometry) (lispm-desktop-apps--geometry frame))
    (setq lispm-desktop-app-window--frames (assq-delete-all id lispm-desktop-app-window--frames))))

(defun lispm-desktop-app-window-close (id)
  "Close the window of kind ID, remembering where it was.  The keyboard
passes on only when the window had it, and the desktop comes in front
only when ID's application was."
  (when-let* ((frame (lispm-desktop-app-window-frame id)))
    (lispm-desktop-app-window--retire frame)
    (delete-frame frame)))

(defun lispm-desktop-app-window-close-box (window)
  "Close WINDOW from its close box: the window alone, while others share its
frame, else the floating window.
This is the frame parameter `lispm-desktop-close-function' of the windows."
  (let ((frame (window-frame window)))
    (if (cdr (window-list frame 'never))
        (delete-window window)
      (lispm-desktop-app-window-close (lispm-desktop-app-window-id frame)))))

(defun lispm-desktop-app-window--forget (frame)
  "Forget FRAME once deleted otherwise than by its close box, as with
\\[delete-frame], as its close box would.
This is one of `delete-frame-functions'."
  (lispm-desktop-app-window--retire frame))

;;;; With the application in front

(defun lispm-desktop-app-window--follow-frontmost (app)
  "Show the floating windows unless the application APP in front hides them,
as the full-screen editor and browser do, which they would cover; hide
them from it.  The window of the application in front stays over the
others.  A window out on the computer's screen stays where it is.
This is one of `lispm-desktop-apps-frontmost-functions'."
  (pcase-dolist (`(,id . ,frame) (reverse lispm-desktop-app-window--frames))
    (when (and (frame-live-p frame)
               (lispm-desktop-app-window--get id :follow t)
               (not (lispm-desktop-apps-out-p frame)))
      (let ((shown (not (funcall (lispm-desktop-app-window--get id :hidden-by
                                                                #'lispm-desktop-apps-full-screen-p)
                                 app))))
        (cond ((and shown (not (frame-visible-p frame)))
               (lispm-desktop-platinum-show-window frame))
              ((and (not shown) (frame-visible-p frame))
               (lispm-desktop-platinum-hide-window frame))))))
  (when-let* ((front (lispm-desktop-app-window-of-app app))
              ((lispm-desktop-app-window--get (lispm-desktop-app-window-id front) :raise t))
              ((frame-visible-p front)))
    (lispm-desktop-platinum-raise-window front)))

(defun lispm-desktop-app-window--refocus ()
  "Give the window of the application in front the keyboard back after a
menu bar menu closes: a click on the menu bar focuses the frame under it,
and the application stays in front."
  (when-let* ((frame (lispm-desktop-app-window-of-app (lispm-desktop-apps-frontmost)))
              ((frame-visible-p frame))
              ((not (eq (selected-frame) frame)))
              (refocus (lispm-desktop-app-window--get (lispm-desktop-app-window-id frame)
                                                      :refocus-p #'always))
              ((funcall refocus)))
    (select-frame-set-input-focus frame)))

;;;; The windows' buffers

(defvar evil-emacs-state-cursor)
(defvar evil-normal-state-cursor)

(define-derived-mode lispm-desktop-app-window-mode special-mode "Window"
  "Major mode a floating window's own buffer derives from, as the Finder
window's and Control Panels': no cursor, no fringes, no wrapped or
numbered lines."
  (visual-line-mode -1)
  (setq-local cursor-type nil
              evil-emacs-state-cursor '(nil)
              evil-normal-state-cursor '(nil)
              truncate-lines t
              left-fringe-width 0
              right-fringe-width 0
              show-trailing-whitespace nil)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1)))

;;;; Menus

(defun lispm-desktop-app-window-file-items (id)
  "Return the items of the File menu of the window of kind ID: Close Window.
The keyboard closes it as any frame, with \\`C-x 5 0'."
  `((:label "Close Window" :action ,(lambda () (lispm-desktop-app-window-close id)) :key "C-x 5 0")))

(defun lispm-desktop-setup-app-windows ()
  "Let the floating windows follow the application in front."
  (add-hook 'lispm-desktop-apps-frontmost-functions #'lispm-desktop-app-window--follow-frontmost)
  (add-hook 'lispm-desktop-platinum-menu-closed-hook #'lispm-desktop-app-window--refocus)
  (add-hook 'delete-frame-functions #'lispm-desktop-app-window--forget))

(provide 'lispm-desktop-app-window)
;;; lispm-desktop-app-window.el ends here
