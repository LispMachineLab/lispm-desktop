;;; lispm-desktop-platinum-scroll.el --- Scroll bars  -*- lexical-binding: t; -*-

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

;; Mac OS 9's scroll bars, drawn in each window's right fringe: arrows, a
;; track and a thumb sized to the part of the buffer shown.  LispM UI
;; draws them; the desktop places their frames and handles scrolling.

;;; Code:

(require 'lispm-ui-scroll)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-platinum-frames)

;;;; Scroll bars
;;
;; Mac OS 9's scroll bar runs down a window's right edge: a recessed
;; track, a thumb in the variation colour with grip ridges, and the two
;; arrow boxes together at its bottom, as Mac OS 9 placed them with its
;; proportional thumbs.  Each window that scrolls gets one, drawn as an
;; SVG in a child frame laid over the window's right fringe, which is
;; widened to make room:
;;
;;   window --geometry--> child frame position and height
;;   window --start, end, size--> thumb --SVG--> child frame's one line
;;
;; Bars follow the windows on every command and scroll; an inactive
;; window's bar, and one with nothing to scroll, lies flat, as Mac OS 9's
;; did.  Sessions (terminals and chats) scroll their own way and get none.

(defcustom lispm-desktop-platinum-scroll-bars t
  "Non-nil gives windows Mac OS 9's scroll bars.
See `lispm-desktop-platinum-scroll-sync'."
  :type 'boolean
  :group 'lispm-desktop-platinum)

(defvar lispm-desktop-platinum--scroll-bars nil
  "Alist of windows to the child frames holding their scroll bars.")

(defvar lispm-desktop-platinum--scroll-timer nil
  "Pending timer bringing the scroll bars up to date, if any.")

(defvar lispm-desktop-platinum-scroll-bar-functions nil
  "Functions deciding whether a buffer's window gets a scroll bar, tried in
turn: each is called with the buffer and returns `yes', `no', or nil to
leave it to the next.  Without an answer, the main area's windows get
one and side windows do not.  Sessions, the browser's page and the
desktop say no; the sidebar's lists say yes.")

(defun lispm-desktop-platinum--scroll-window-p (window)
  "Return non-nil when WINDOW gets a scroll bar, see
`lispm-desktop-platinum-scroll-bar-functions'."
  (and (not (window-minibuffer-p window))
       (not (window-parameter window 'lispm-desktop-chrome))
       (pcase (run-hook-with-args-until-success 'lispm-desktop-platinum-scroll-bar-functions
                                                (window-buffer window))
         ('yes t)
         ('no nil)
         (_ (not (window-parameter window 'window-side))))))

(defvar lispm-desktop-platinum--scroll-map
  (let ((map (make-sparse-keymap)))
    (dolist (press '("<down-mouse-1>" "<double-down-mouse-1>" "<triple-down-mouse-1>"))
      (keymap-set map press #'lispm-desktop-platinum-scroll-press))
    (dolist (release '("<mouse-1>" "<double-mouse-1>" "<triple-mouse-1>" "<drag-mouse-1>"))
      (keymap-set map release #'ignore))
    (keymap-set map "<wheel-up>" #'lispm-desktop-platinum-scroll-wheel)
    (keymap-set map "<wheel-down>" #'lispm-desktop-platinum-scroll-wheel)
    map)
  "Keymap of a scroll bar's buffer.")

(defun lispm-desktop-platinum--scroll-frame (window)
  "Return the child frame holding WINDOW's scroll bar, made when missing.
It takes no focus and hooks run for new frames do not see it."
  (let ((frame (window-parameter window 'lispm-desktop-platinum-scroll-frame)))
    (if (frame-live-p frame)
        frame
      (let* ((buffer (generate-new-buffer " *lispm-desktop-scroll-bar*"))
             (frame (let ((after-make-frame-functions nil)
                          (before-make-frame-hook nil))
                      (make-frame
                       `((parent-frame . ,(window-frame window))
                         (minibuffer . nil) (no-accept-focus . t) (no-focus-on-map . t)
                         (undecorated . t) (unsplittable . t) (no-other-frame . t)
                         (internal-border-width . 0) (child-frame-border-width . 0)
                         (left-fringe . 0) (right-fringe . 0)
                         (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
                         (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
                         (line-spacing . 0) (cursor-type . nil) (no-special-glyphs . t)
                         (desktop-dont-save . t) (visibility . nil)
                         (background-color . ,(lispm-ui-color :chrome))
                         (width . 2) (height . 1))))))
        (with-current-buffer buffer
          (setq-local mode-line-format nil
                      header-line-format nil
                      tab-line-format nil
                      cursor-type nil
                      cursor-in-non-selected-windows nil
                      display-line-numbers nil
                      truncate-lines t
                      line-spacing 0
                      ;; The bar's one line is as wide as the frame; scrolled
                      ;; sideways to show point after it, it would lose a column.
                      auto-hscroll-mode nil
                      lispm-desktop-platinum--scroll-target window)
          (use-local-map lispm-desktop-platinum--scroll-map))
        (let ((root (frame-root-window frame)))
          (set-window-buffer root buffer)
          (set-window-dedicated-p root t)
          (set-window-fringes root 0 0)
          (set-window-margins root 0 0))
        (set-frame-parameter frame 'lispm-desktop-platinum-scroll-target window)
        (set-window-parameter window 'lispm-desktop-platinum-scroll-frame frame)
        (push (cons window frame) lispm-desktop-platinum--scroll-bars)
        frame))))

(defvar-local lispm-desktop-platinum--scroll-target nil
  "The window whose scroll bar this buffer draws.")

(defun lispm-desktop-platinum--scroll-update (window)
  "Bring WINDOW's scroll bar up to date: place it, size it, draw it."
  (let ((width lispm-ui-scroll-width))
    (unless (= (or (cadr (window-fringes window)) 0) width)
      (set-window-fringes window nil width nil t))
    (let* ((frame (lispm-desktop-platinum--scroll-frame window))
           (inside (window-inside-pixel-edges window))
           (x (- (nth 2 (window-pixel-edges window))
                 (window-right-divider-width window)
                 (window-scroll-bar-width window)
                 width))
           (y (nth 1 inside))
           (height (max 1 (- (nth 3 inside) y)))
           (buffer (window-buffer window))
           (thumb (with-current-buffer buffer
                    (lispm-ui-scroll-thumb height (window-start window)
                                           (window-end window t)
                                           (point-min) (point-max))))
           (active (eq window (frame-selected-window (window-frame window))))
           (place (list x y height))
           (key (list lispm-ui-palette-generation height thumb active)))
      (unless (equal place (frame-parameter frame 'lispm-desktop-platinum-scroll-place))
        (set-frame-parameter frame 'lispm-desktop-platinum-scroll-place place)
        ;; Sized in pixels, below the width of the ten columns a window
        ;; may otherwise not shrink under.
        (let ((window-min-width 1)
              (window-safe-min-width 1))
          (set-frame-size frame width height t))
        (set-frame-position frame x y))
      (unless (equal key (frame-parameter frame 'lispm-desktop-platinum-scroll-key))
        (set-frame-parameter frame 'lispm-desktop-platinum-scroll-key key)
        (set-frame-parameter frame 'lispm-ui-scroll-thumb thumb)
        (lispm-desktop-platinum-set-background frame (lispm-ui-color :chrome))
        (let ((root (frame-root-window frame)))
          (with-current-buffer (window-buffer root)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (propertize " " 'display
                                  (svg-image (lispm-ui-draw-scroll-bar
                                              lispm-ui-current-palette height thumb active)
                                             :ascent 100 :scale 1)))))
          (set-window-point root 1)
          (set-window-hscroll root 0)))
      (unless (frame-visible-p frame)
        (make-frame-visible frame)))))

(defun lispm-desktop-platinum--scroll-drop (window frame)
  "Take WINDOW's scroll bar, held in child FRAME, away."
  (when (frame-live-p frame)
    (let ((buffer (window-buffer (frame-root-window frame))))
      (delete-frame frame t)
      (kill-buffer buffer)))
  (when (window-live-p window)
    (set-window-parameter window 'lispm-desktop-platinum-scroll-frame nil)
    (set-window-fringes window nil nil nil t)))

(defun lispm-desktop-platinum-scroll-sync ()
  "Give every window that wants a scroll bar its own, up to date, and
take the bars of windows gone or no longer wanting one away."
  (setq lispm-desktop-platinum--scroll-timer nil)
  (let ((wanted nil))
    (when (and lispm-desktop-platinum-scroll-bars lispm-ui-current-palette)
      (dolist (frame (frame-list))
        ;; Floating windows out on the computer's screen go without, as
        ;; they do over the desktop, and so do their frames and outlines.
        (when (and (frame-visible-p frame) (not (frame-parent frame))
                   (not (frame-parameter frame 'lispm-desktop-window))
                   (not (frame-parameter frame 'lispm-desktop-out))
                   (not (lispm-desktop-platinum-chrome-frame-p frame))
                   (display-graphic-p frame))
          (dolist (window (window-list frame 'no-mini))
            (when (lispm-desktop-platinum--scroll-window-p window)
              (push window wanted)
              (lispm-desktop-platinum--scroll-update window))))))
    (setq lispm-desktop-platinum--scroll-bars
          (seq-filter (pcase-lambda (`(,window . ,frame))
                        (or (and (memq window wanted) (frame-live-p frame))
                            (progn (lispm-desktop-platinum--scroll-drop window frame) nil)))
                      lispm-desktop-platinum--scroll-bars))))

(defun lispm-desktop-platinum--scroll-schedule (&rest _)
  "Bring the scroll bars up to date once Emacs has redisplayed."
  (unless (timerp lispm-desktop-platinum--scroll-timer)
    (setq lispm-desktop-platinum--scroll-timer
          (run-with-idle-timer 0 nil #'lispm-desktop-platinum-scroll-sync))))

(defun lispm-desktop-platinum--scroll-in (window step)
  "Call STEP, a function of no arguments, with WINDOW selected; redraw its bar."
  (with-selected-window window
    (ignore-errors (funcall step)))
  (lispm-desktop-platinum--scroll-update window)
  (redisplay t))

(defun lispm-desktop-platinum--scroll-repeat (window step)
  "Call STEP in WINDOW, and again and again while the mouse button is held.
As Mac OS 9's arrows and track did, holding repeats after a pause."
  (lispm-desktop-platinum--scroll-in window step)
  (when (sit-for 0.35)
    (while (sit-for 0.05)
      (lispm-desktop-platinum--scroll-in window step))))

(defun lispm-desktop-platinum--scroll-mouse-y (frame)
  "Return the mouse's height in pixels from the top of scroll bar FRAME."
  (pcase-let ((`(,over ,_ . ,y) (mouse-pixel-position)))
    (cond ((null y) 0)
          ((eq over frame) y)
          (t (- y (cdr (frame-position frame)))))))

(defun lispm-desktop-platinum--scroll-drag (window frame grab thumb)
  "Drag WINDOW's thumb, held GRAB pixels from its top, until the button rises.
THUMB is its (TOP . LENGTH) when the drag began; FRAME holds the bar."
  (let* ((layout (lispm-ui-scroll-layout (frame-pixel-height frame)))
         (from (plist-get layout :track))
         (track (plist-get layout :track-length))
         (length (cdr thumb)))
    (track-mouse
      (while (mouse-movement-p (let ((echo-keystrokes 0)) (lispm-ui-modal-read-event)))
        (let* ((top (max from (min (+ from (- track length))
                                   (- (lispm-desktop-platinum--scroll-mouse-y frame) grab))))
               (fraction (/ (float (- top from)) (max 1 (- track length)))))
          (lispm-desktop-platinum--scroll-in
           window
           (lambda ()
             (let* ((shown (- (window-end nil t) (window-start)))
                    (span (max 0 (- (point-max) (point-min) shown)))
                    (position (+ (point-min) (round (* fraction span)))))
               (set-window-start nil (save-excursion (goto-char position)
                                                     (line-beginning-position)))))))))))

(defun lispm-desktop-platinum-scroll-press (event)
  "Act on a press in a scroll bar, as Mac OS 9's scroll bars did.
The arrows, where `lispm-ui-scroll-layout' puts them,
together at the bar's bottom in Mac OS 9's, scroll a line and the track
above or below the thumb a page, over and over while held; the thumb
drags the window's text along."
  (interactive "e")
  (let* ((position (event-start event))
         (frame (window-frame (posn-window position)))
         (window (frame-parameter frame 'lispm-desktop-platinum-scroll-target))
         (thumb (frame-parameter frame 'lispm-ui-scroll-thumb))
         (y (cdr (posn-x-y position)))
         (layout (lispm-ui-scroll-layout (frame-pixel-height frame)))
         (arrow (plist-get layout :arrow)))
    (when (window-live-p window)
      (cond ((<= (plist-get layout :down) y (+ (plist-get layout :down) arrow -1))
             (lispm-desktop-platinum--scroll-repeat window (lambda () (scroll-up 1))))
            ((<= (plist-get layout :up) y (+ (plist-get layout :up) arrow -1))
             (lispm-desktop-platinum--scroll-repeat window (lambda () (scroll-down 1))))
            ((null thumb))
            ((< y (car thumb)) (lispm-desktop-platinum--scroll-repeat window #'scroll-down))
            ((>= y (+ (car thumb) (cdr thumb))) (lispm-desktop-platinum--scroll-repeat window #'scroll-up))
            (t (lispm-desktop-platinum--scroll-drag window frame (- y (car thumb)) thumb))))))

(defun lispm-desktop-platinum-scroll-wheel (event)
  "Scroll the window of the scroll bar the mouse wheel turned over, by EVENT."
  (interactive "e")
  (let ((window (frame-parameter (window-frame (posn-window (event-start event)))
                                 'lispm-desktop-platinum-scroll-target)))
    (when (window-live-p window)
      (lispm-desktop-platinum--scroll-in window (if (eq (event-basic-type event) 'wheel-up)
                                                    (lambda () (scroll-down 3))
                                                  (lambda () (scroll-up 3)))))))

(defun lispm-desktop-platinum-scroll-enable ()
  "Keep the scroll bars in step with windows, scrolling and selection."
  (add-hook 'post-command-hook #'lispm-desktop-platinum--scroll-schedule)
  (add-hook 'window-scroll-functions #'lispm-desktop-platinum--scroll-schedule)
  (add-hook 'window-size-change-functions #'lispm-desktop-platinum--scroll-schedule)
  (add-hook 'window-selection-change-functions #'lispm-desktop-platinum--scroll-schedule)
  (add-hook 'window-buffer-change-functions #'lispm-desktop-platinum--scroll-schedule)
  (lispm-desktop-platinum--scroll-schedule))

(provide 'lispm-desktop-platinum-scroll)
;;; lispm-desktop-platinum-scroll.el ends here
