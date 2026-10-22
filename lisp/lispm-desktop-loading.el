;;; lispm-desktop-loading.el --- The watch on the pointer while LispM works  -*- lexical-binding: t; -*-

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

;; Mac OS 9 said "wait" with its pointer: a wristwatch, the minute hand
;; going round.  LispM shows it over its windows too:
;;
;;   busy      while Emacs itself has been at work longer than
;;             `hourglass-delay', as a long command keeps it; Emacs's own
;;             `display-hourglass' turns that off
;;   loading   while something loads for what is in front, as the App
;;             Store fetching its catalogs or installing an application
;;
;; Code declares a loading with `lispm-desktop-with-loading' around work it
;; does there and then, `lispm-desktop-loading-process' for a process it
;; waits for, or `lispm-desktop-loading-start' and `lispm-desktop-loading-stop'
;; around anything else.  A loading is for a frame, a window or a buffer,
;; and the watch shows while that is in front, as Mac OS 9's watch was the
;; application in front's; or for nothing in particular, and then shows
;; whatever is in front.
;;
;; In the Mac build lispm-desktop-term's module draws the watch, see
;; native/lispm-desktop-term/src/cursor.rs: Emacs has neither pointers of
;; images nor a busy pointer there.  The frames are Platinum's, see
;; lispm-desktop-platinum-pointer.  Until the module is built, the pointer
;; stays as it is.  Under X, Emacs shows busy itself, with X's watch, and
;; loadings change nothing.

;;; Code:

(require 'seq)
(require 'lispm-desktop-platinum-pointer)

(declare-function lispm-desktop-term-load "lispm-desktop-term" ())
(declare-function lispm-desktop-term-module-cursor-define "lispm-desktop-term-module"
                  (frames hot-x hot-y interval))
(declare-function lispm-desktop-term-module-cursor-loading "lispm-desktop-term-module" (flag))
(declare-function lispm-desktop-term-module-cursor-busy-delay "lispm-desktop-term-module" (seconds))

(defvar x-hourglass-pointer-shape)
(defvar x-pointer-watch)

(defcustom lispm-desktop-loading-interval 0.15
  "Seconds the watch's minute hand stays at each of its eight places."
  :type 'number
  :group 'lispm-desktop)

(defvar lispm-desktop-loading--loadings nil
  "The loadings under way, each a list (WHERE).")

(defvar lispm-desktop-loading--shown nil
  "Non-nil while the watch shows for a loading.")

(defun lispm-desktop-loading--module-p ()
  "Return non-nil when lispm-desktop-term's module draws the pointer: it is
loaded, built since it learnt to, and the pointer is AppKit's, Emacs
being on the Mac's display.  On a text terminal, where the module runs
the terminals all the same, there is no pointer to draw."
  (and (featurep 'lispm-desktop-term-module)
       (fboundp 'lispm-desktop-term-module-cursor-define)
       (eq window-system 'ns)))

(defun lispm-desktop-loading-in-front-p (where)
  "Return non-nil when WHERE is in front.
A frame is when it is the selected frame or holds it, a window when its
frame is, a buffer or a buffer's name when the selected frame shows the
buffer; nil always is."
  (pcase where
    ('nil t)
    ((pred stringp) (when-let* ((buffer (get-buffer where)))
                      (lispm-desktop-loading-in-front-p buffer)))
    ((pred bufferp) (and (buffer-live-p where) (get-buffer-window where (selected-frame)) t))
    ((pred windowp) (and (window-live-p where) (lispm-desktop-loading-in-front-p (window-frame where))))
    ((pred framep) (and (frame-live-p where)
                        (or (eq where (selected-frame)) (frame-ancestor-p where (selected-frame)))))))

(defun lispm-desktop-loading-p ()
  "Return non-nil while a loading is under way for what is in front."
  (seq-some (lambda (loading) (lispm-desktop-loading-in-front-p (car loading)))
            lispm-desktop-loading--loadings))

(defun lispm-desktop-loading--update (&rest _)
  "Show the watch while a loading is for what is in front, else not."
  (let ((shown (and (lispm-desktop-loading-p) t)))
    (unless (eq shown lispm-desktop-loading--shown)
      (setq lispm-desktop-loading--shown shown)
      (when (lispm-desktop-loading--module-p)
        (lispm-desktop-term-module-cursor-loading shown)))))

(defun lispm-desktop-loading-start (&optional where)
  "Show the watch on the pointer while WHERE is in front, until
`lispm-desktop-loading-stop' is given what this returns.
WHERE is a frame, a window, a buffer or a buffer's name, as
`lispm-desktop-loading-in-front-p' takes it; nil, whatever is in front."
  (let ((loading (list where)))
    (push loading lispm-desktop-loading--loadings)
    (lispm-desktop-loading--update)
    loading))

(defun lispm-desktop-loading-stop (loading)
  "Stop LOADING, which `lispm-desktop-loading-start' returned.
Stopping it again does nothing."
  (setq lispm-desktop-loading--loadings (delq loading lispm-desktop-loading--loadings))
  (lispm-desktop-loading--update))

(defmacro lispm-desktop-with-loading (where &rest body)
  "Run BODY with the watch on the pointer while WHERE is in front, and
return what BODY returns.  WHERE is as `lispm-desktop-loading-start' takes it."
  (declare (indent 1) (debug t))
  (let ((loading (make-symbol "loading")))
    `(let ((,loading (lispm-desktop-loading-start ,where)))
       (unwind-protect
           (progn ,@body)
         (lispm-desktop-loading-stop ,loading)))))

(defun lispm-desktop-loading-process (process &optional where)
  "Show the watch on the pointer while PROCESS runs and WHERE is in front.
WHERE is as `lispm-desktop-loading-start' takes it.  Return PROCESS."
  (let ((loading (lispm-desktop-loading-start where)))
    (add-function :after (process-sentinel process)
                  (lambda (process _event)
                    (unless (process-live-p process)
                      (lispm-desktop-loading-stop loading))))
    (unless (process-live-p process)
      (lispm-desktop-loading-stop loading))
    process))

;;;; Busy

(defun lispm-desktop-loading--busy-delay ()
  "Return the seconds Emacs may be busy before the watch shows:
`hourglass-delay', or nil for never, as when `display-hourglass' is nil.
An Emacs without a window system has neither."
  (let ((delay (bound-and-true-p hourglass-delay)))
    (and (bound-and-true-p display-hourglass) (numberp delay) (float delay))))

(defun lispm-desktop-loading--tell-busy ()
  "Tell the module how long Emacs may be busy before the watch shows."
  (when (lispm-desktop-loading--module-p)
    (lispm-desktop-term-module-cursor-busy-delay (lispm-desktop-loading--busy-delay))))

(defun lispm-desktop-loading--busy-changed (_symbol _value operation _where)
  "Tell the module anew once `display-hourglass' or `hourglass-delay' is
set by OPERATION, which a variable watcher sees before it is."
  (when (eq operation 'set)
    (run-at-time 0 nil #'lispm-desktop-loading--tell-busy)))

;;;; Setup

(defun lispm-desktop-loading--start-module ()
  "Give lispm-desktop-term's module the watch, and say when to show it."
  (when (and (lispm-desktop-loading--module-p) (not noninteractive))
    (lispm-desktop-term-module-cursor-define
     (vconcat (lispm-desktop-platinum-watch-frames))
     (car lispm-desktop-platinum-watch-hot-spot) (cdr lispm-desktop-platinum-watch-hot-spot)
     (round (* 1000 lispm-desktop-loading-interval)))
    (lispm-desktop-loading--tell-busy)
    (setq lispm-desktop-loading--shown nil)
    (lispm-desktop-loading--update)))

(defun lispm-desktop-setup-loading ()
  "Show Mac OS 9's watch on the pointer while LispM is busy, and while it
loads something for what is in front.  In the Mac build, the watch is
drawn by lispm-desktop-term's module once it is loaded, which this loads
when it is built; under X, Emacs shows its own busy pointer as X's watch."
  (add-hook 'window-selection-change-functions #'lispm-desktop-loading--update)
  (add-hook 'window-buffer-change-functions #'lispm-desktop-loading--update)
  (add-variable-watcher 'display-hourglass #'lispm-desktop-loading--busy-changed)
  (add-variable-watcher 'hourglass-delay #'lispm-desktop-loading--busy-changed)
  (when (and (eq window-system 'x) (boundp 'x-pointer-watch))
    (setq x-hourglass-pointer-shape x-pointer-watch))
  (with-eval-after-load 'lispm-desktop-term-module
    (lispm-desktop-loading--start-module))
  (when (eq window-system 'ns)
    (lispm-desktop-term-load)))

(provide 'lispm-desktop-loading)
;;; lispm-desktop-loading.el ends here
