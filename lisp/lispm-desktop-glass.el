;;; lispm-desktop-glass.el --- See-through windows for looks of glass  -*- lexical-binding: t; -*-

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

;; A look may be made of glass: its floating windows, the frames drawn
;; round them, its menus and alerts let the desktop show through where
;; they are dark.  Emacs paints every pixel of a frame opaque, so the
;; window server is asked to work each pixel's opacity out from its colour
;; as it composites the window:
;;
;;   alpha = GAIN * (red + green + blue)
;;
;; Pure black is clear, the darkest colours are see-through and the rest
;; is solid, text on dark glass too.  A look names its GAIN with :glass,
;; see `lispm-desktop-look-define'; its theme then paints what should be
;; see-through in colours dark enough, and cuts corners out in black.
;;
;; The desktop itself is never glass: it lies under everything, and a
;; filter over the whole screen would run at every composite of it.
;;
;; Only macOS draws glass, through lispm-desktop-term's module (glass.rs);
;; elsewhere, or without the module, windows stay opaque.  Windows are
;; matched to frames by the frame's own record, whose address Emacs
;; prints with the frame.  Emacs makes a frame a new window as it moves
;; in or out of another frame, so the frames are looked over again a
;; moment after each change and whenever Emacs is idle a while.

;;; Code:

(require 'lispm-desktop-look)

(declare-function lispm-desktop-term-load "lispm-desktop-term" ())
(declare-function lispm-desktop-term-module-glass-apply "lispm-desktop-term-module" (address gain))
(declare-function lispm-desktop-term-module-glass-clear "lispm-desktop-term-module" (address))

(defcustom lispm-desktop-glass t
  "Non-nil lets a look of glass make its windows see-through where they
are dark, see lispm-desktop-glass.el.  Glass needs macOS and
lispm-desktop-term's module; nil keeps every window opaque."
  :type 'boolean
  :group 'lispm-desktop)

(defconst lispm-desktop-glass-idle 2
  "Seconds Emacs stays idle before the frames are looked over again, while
a look of glass is in use.")

(defvar lispm-desktop-glass--timer nil
  "The idle timer looking the frames over, while a look of glass is in use.")

(defun lispm-desktop-glass-gain ()
  "Return the gain of the look's glass, or nil when windows stay opaque."
  (and lispm-desktop-glass (lispm-desktop-look-get :glass)))

(defun lispm-desktop-glass--module-p ()
  "Return non-nil when lispm-desktop-term's module can draw glass."
  (and (eq (window-system) 'ns)
       (fboundp 'lispm-desktop-term-load)
       (lispm-desktop-term-load)
       (fboundp 'lispm-desktop-term-module-glass-apply)))

(defun lispm-desktop-glass-address (frame)
  "Return the address of FRAME's record, as Emacs prints it, or nil."
  (let ((printed (format "%S" frame)))
    (when (string-match " 0x\\([[:xdigit:]]+\\)>\\'" printed)
      (string-to-number (match-string 1 printed) 16))))

(defun lispm-desktop-glass-frame-p (frame)
  "Return non-nil when a look of glass makes FRAME see-through: a frame
floating over the desktop, as a window, the frame drawn round one, a
menu or an alert, or a window out on the computer's screen.  The screen,
which draws the desktop, and the outlines a window moves by stay opaque."
  (and (frame-live-p frame)
       (not (frame-parameter frame 'lispm-desktop-platinum-outline))
       (or (frame-parent frame)
           (frame-parameter frame 'lispm-desktop-window))
       t))

(defun lispm-desktop-glass-update ()
  "Make the frames glass that the look's glass covers, and the others opaque."
  (when (lispm-desktop-glass--module-p)
    (let ((gain (lispm-desktop-glass-gain)))
      (dolist (frame (frame-list))
        (when-let* ((address (lispm-desktop-glass-address frame)))
          (let ((wanted (and gain (lispm-desktop-glass-frame-p frame) (float gain))))
            (cond
             (wanted (lispm-desktop-term-module-glass-apply address wanted))
             ((frame-parameter frame 'lispm-desktop-glass)
              (lispm-desktop-term-module-glass-clear address)
              ;; Emacs gives the window its background again.
              (set-frame-parameter frame 'background-color (frame-parameter frame 'background-color))))
            (unless (eql wanted (frame-parameter frame 'lispm-desktop-glass))
              (set-frame-parameter frame 'lispm-desktop-glass wanted))))))))

(defun lispm-desktop-glass--soon (&rest _)
  "Look the frames over a moment from now, once Emacs made their windows."
  (run-at-time 0.1 nil #'lispm-desktop-glass-update))

(defun lispm-desktop-glass--follow ()
  "Look the frames over now, and while Emacs is idle as long as a look of
glass is in use."
  (when (timerp lispm-desktop-glass--timer)
    (cancel-timer lispm-desktop-glass--timer)
    (setq lispm-desktop-glass--timer nil))
  (when (lispm-desktop-glass-gain)
    (setq lispm-desktop-glass--timer
          (run-with-idle-timer lispm-desktop-glass-idle t #'lispm-desktop-glass-update)))
  (lispm-desktop-glass-update))

(defun lispm-desktop-glass--setting-changed (_symbol _value operation _where)
  "Follow `lispm-desktop-glass' as it is set.  This is a variable watcher."
  (when (eq operation 'set)
    (run-at-time 0 nil #'lispm-desktop-glass--follow)))

(defun lispm-desktop-setup-glass ()
  "Make the windows of a look of glass see-through, as looks and themes
change and frames are made."
  (add-hook 'lispm-desktop-look-changed-hook #'lispm-desktop-glass--follow)
  (add-hook 'enable-theme-functions #'lispm-desktop-glass--soon 95)
  (add-hook 'after-make-frame-functions #'lispm-desktop-glass--soon)
  (add-variable-watcher 'lispm-desktop-glass #'lispm-desktop-glass--setting-changed)
  (lispm-desktop-glass--follow))

(provide 'lispm-desktop-glass)
;;; lispm-desktop-glass.el ends here
