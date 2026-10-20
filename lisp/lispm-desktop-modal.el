;;; lispm-desktop-modal.el --- What the desktop's modal panels share  -*- lexical-binding: t; -*-

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

;; Menus, alerts, the workspaces' panel, the Control Strip's keyboard walk
;; and the first start's pages each read events in a loop of their own
;; until something is chosen.  What they share is here: reading an event
;; without quitting, what each key does, stepping a focus ring, blinking a
;; control as it is pressed, finding what a click is on, and telling a
;; drag from a click.
;;
;; Every key works from the HHKB, which has no arrow keys of its own: C-f,
;; C-b, C-n and C-p go where the arrows do, Tab and Shift-Tab ring the next
;; and previous control, Return chooses, Space presses the control ringed
;; and Escape or C-g gives up.  A loop takes from here only the roles it
;; has use for; a menu's Right, say, opens a submenu.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defun lispm-desktop-modal-read (&optional timeout)
  "Read the next event, a quit included, without quitting.
With TIMEOUT, return nil after that many seconds without one.  The click
that opened a panel is not echoed as an unfinished key."
  (let* ((inhibit-quit t)
         (event (let ((echo-keystrokes 0)) (read-event nil nil timeout))))
    (when (eq event ?\C-g)
      (setq quit-flag nil))
    event))

(defconst lispm-desktop-modal-keys
  '((next tab ?\t)
    (previous backtab S-tab S-iso-lefttab)
    (right ?\C-f right)
    (left ?\C-b left)
    (down ?\C-n down)
    (up ?\C-p up)
    (confirm return ?\r ?\n kp-enter)
    (press ?\s)
    (cancel escape ?\e ?\C-g ?\M-.)
    (back ?\d backspace))
  "What keys do in the desktop's modal panels: (ROLE EVENT...).
`next' and `previous' ring the next or previous control, `right',
`left', `down' and `up' go that way, `confirm' chooses the default,
`press' presses the control ringed, `cancel' gives up and `back' goes
back.  Escape is ESC, C-g and Command-period, as on Mac OS 9.")

(defun lispm-desktop-modal-role (event)
  "Return the role of EVENT in a modal panel, see `lispm-desktop-modal-keys'."
  (car (seq-find (lambda (entry) (memql event (cdr entry))) lispm-desktop-modal-keys)))

(defun lispm-desktop-modal-step (event)
  "Return 1 when EVENT moves on in a focus ring, -1 when it moves back, else nil:
Tab, C-f and C-n move on, Shift-Tab, C-b and C-p back."
  (pcase (lispm-desktop-modal-role event)
    ((or 'next 'right 'down) 1)
    ((or 'previous 'left 'up) -1)))

(defun lispm-desktop-modal-cancel-p (event)
  "Return non-nil when EVENT gives up: Escape, C-g or Command-period."
  (eq (lispm-desktop-modal-role event) 'cancel))

(defun lispm-desktop-modal-confirm-p (event)
  "Return non-nil when EVENT chooses: Return or Enter."
  (eq (lispm-desktop-modal-role event) 'confirm))

(defun lispm-desktop-modal-mouse-down-p (event)
  "Return non-nil when EVENT presses the first mouse button down."
  (and (consp event) (eq (event-basic-type event) 'mouse-1)
       (memq 'down (event-modifiers event))))

(defun lispm-desktop-modal-mouse-up-p (event)
  "Return non-nil when EVENT lets the first mouse button go."
  (and (consp event) (eq (event-basic-type event) 'mouse-1)
       (not (memq 'down (event-modifiers event)))))

(defun lispm-desktop-modal-over (event frame)
  "Return EVENT's position when it is over FRAME, else nil."
  (let ((position (event-end event)))
    (when (and (windowp (posn-window position))
               (eq (window-frame (posn-window position)) frame))
      position)))

(defun lispm-desktop-modal-cycle (index step count &optional ok-p)
  "Return the index STEP places from INDEX among COUNT, wrapping around.
From no INDEX, a step on goes to the first and a step back to the last.
With OK-P, indexes it refuses are passed over; nil when it takes none."
  (when (> count 0)
    (let ((at (or index (if (> step 0) -1 count))))
      (if (not ok-p)
          (mod (+ at step) count)
        (cl-loop repeat count
                 do (setq at (mod (+ at (cl-signum step)) count))
                 when (funcall ok-p at) return at)))))

(defconst lispm-desktop-modal-flashes
  '((button . 0.133) (menu-item . 0.05))
  "How long a control chosen blinks, in seconds: a button pressed from the
keyboard stays pushed in for eight ticks, as Mac OS 9's did; a menu item
chosen goes off and on for three ticks each.")

(defun lispm-desktop-modal-flash (kind draw &optional undraw)
  "Blink a control of KIND, a key of `lispm-desktop-modal-flashes', as it is
chosen: call DRAW to draw it pressed, show it for the time KIND blinks,
then call UNDRAW, when given, and show that."
  (funcall draw)
  (redisplay t)
  (sit-for (alist-get kind lispm-desktop-modal-flashes))
  (when undraw
    (funcall undraw)
    (redisplay t)))

(defun lispm-desktop-modal-inside-p (rect x y)
  "Return non-nil when X, Y lies in RECT, (LEFT TOP WIDTH HEIGHT) in pixels.
The rectangle holds its left and top edges, not its right and bottom."
  (pcase-let ((`(,left ,top ,width ,height) rect))
    (and rect (<= left x) (< x (+ left width)) (<= top y) (< y (+ top height)))))

(defun lispm-desktop-modal-hit (layout x y)
  "Return the value of what LAYOUT has at X, Y, or nil.
LAYOUT is a list of (VALUE LEFT TOP WIDTH HEIGHT); the first holding X, Y
wins."
  (car (seq-find (lambda (entry) (lispm-desktop-modal-inside-p (cdr entry) x y)) layout)))

(defun lispm-desktop-modal-moved-p (delta)
  "Return non-nil when DELTA, (DX . DY) in pixels, is a move rather than a
hand's jitter in a click: more than `double-click-fuzz' either way."
  (and delta
       (> (max (abs (car delta)) (abs (cdr delta)))
          (if (numberp double-click-fuzz) double-click-fuzz 3))))

(provide 'lispm-desktop-modal)
;;; lispm-desktop-modal.el ends here
