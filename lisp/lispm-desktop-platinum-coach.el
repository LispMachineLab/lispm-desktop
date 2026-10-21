;;; lispm-desktop-platinum-coach.el --- Coach marks, Apple Guide's red circles  -*- lexical-binding: t; -*-

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

;; Apple Guide, Mac OS's help a step at a time, drew a coach mark around
;; what a step spoke of: a red loop, as if drawn by hand with a marker, its
;; ends overlapping, round a menu, an icon or a control.  One thing is
;; circled at a time, `lispm-desktop-platinum-coach-mark'; whatever draws
;; that thing draws the mark round it, so it lies under the windows over
;; the thing as the thing does:
;;
;;   (menu . KEY)   a menu bar title, drawn here
;;   anything else  what the desktop agrees on with whoever sets the mark,
;;                  as the desktop's icons and places on the screen
;;
;; Setting the mark redraws the menu bar and calls
;; `lispm-desktop-platinum-coach-mark-functions', so the rest is drawn
;; again too.

;;; Code:

(require 'svg)

(defconst lispm-desktop-platinum-coach-mark-color "#DD0000"
  "Colour of a coach mark, Apple Guide's red.")

(defvar lispm-desktop-platinum-coach-mark nil
  "What a coach mark circles now, or nil for nothing.
See the commentary of lispm-desktop-platinum-coach.el for the values.")

(defvar lispm-desktop-platinum-coach-mark-functions nil
  "Functions called with the coach mark whenever it is set, changed or
not, to draw again what they draw: what it circles may have moved.")

(defun lispm-desktop-platinum-coach-marked-p (target)
  "Return non-nil when TARGET is what the coach mark circles now."
  (and target (equal target lispm-desktop-platinum-coach-mark)))

(defun lispm-desktop-platinum-set-coach-mark (mark)
  "Circle MARK, or nothing when MARK is nil, and draw it where it goes."
  (setq lispm-desktop-platinum-coach-mark mark)
  (force-mode-line-update t)
  (run-hook-with-args 'lispm-desktop-platinum-coach-mark-functions mark))

;;;; Drawing (pure)

(defconst lispm-desktop-platinum--coach-mark-roundness 3.0
  "Exponent of the superellipse a coach mark follows: 2 would be an
ellipse; more is squarer, closer to the corners of what it circles.")

(defun lispm-desktop-platinum--oval-point (cx cy a b angle)
  "Return the point at ANGLE of the oval of half-axes A and B round CX, CY:
\(X Y), on the superellipse `lispm-desktop-platinum--coach-mark-roundness'."
  (let ((power (/ 2.0 lispm-desktop-platinum--coach-mark-roundness))
        (cos (cos angle))
        (sin (sin angle)))
    (list (+ cx (* a (if (< cos 0) -1 1) (expt (abs cos) power)))
          (+ cy (* b (if (< sin 0) -1 1) (expt (abs sin) power))))))

(defun lispm-desktop-platinum-coach-mark-box (x y width height &optional margin)
  "Return the box of a coach mark round the rectangle WIDTH by HEIGHT at
X, Y, MARGIN pixels clear of it, by default none: (X Y WIDTH HEIGHT),
the box of the oval through the corners of the rectangle so grown."
  (let* ((margin (or margin 0))
         ;; Each half-axis of the oval through the corners is the half
         ;; side times 2^(1/n).
         (grow (expt 2.0 (/ 1.0 lispm-desktop-platinum--coach-mark-roundness)))
         (w (* grow (+ width (* 2 margin))))
         (h (* grow (+ height (* 2 margin)))))
    (list (- (+ x (/ width 2.0)) (/ w 2)) (- (+ y (/ height 2.0)) (/ h 2)) w h)))

(defun lispm-desktop-platinum-coach-mark-path (x y width height &optional spread)
  "Return the path data of a coach mark filling the box WIDTH by HEIGHT
at X, Y: an oval touching the box's sides, drawn as a hand goes round
with a marker, from the top's left once round and a little further.  It
starts SPREAD pixels, by default three, inside the oval and ends as far
outside, half of it either side, wavering a little on the way, so that
its two ends lie side by side."
  (let* ((spread (or spread 3.0))
         (a (/ width 2.0))
         (b (/ height 2.0))
         (cx (+ x a))
         (cy (+ y b))
         (start (* -0.68 float-pi))
         (sweep (* 2.16 float-pi))
         (steps (max 32 (ceiling (* 1.1 float-pi (+ a b)) 3)))
         (points nil))
    (dotimes (i (1+ steps))
      (let* ((along (/ (float i) steps))
             (out (* spread (+ (- along 0.5) (* 0.15 (sin (* 2 float-pi 3 along))))))
             (point (lispm-desktop-platinum--oval-point cx cy (+ a out) (+ b out)
                                                        (+ start (* along sweep)))))
        (push (format "%.1f %.1f" (car point) (cadr point)) points)))
    (concat "M" (mapconcat #'identity (nreverse points) " L"))))

(defun lispm-desktop-platinum-coach-mark-element (x y width height &optional pen spread)
  "Return a coach mark filling the box WIDTH by HEIGHT at X, Y as the text
of an SVG path element: drawn with a pen PEN pixels wide, by default
three, the loop SPREAD as `lispm-desktop-platinum-coach-mark-path' takes it."
  (format "<path d=\"%s\" fill=\"none\" stroke=\"%s\" stroke-width=\"%s\" stroke-linecap=\"round\" stroke-linejoin=\"round\"/>"
          (lispm-desktop-platinum-coach-mark-path x y width height spread)
          lispm-desktop-platinum-coach-mark-color (or pen 3)))

(defun lispm-desktop-platinum-svg-coach-mark (svg x y width height &optional pen spread)
  "Draw on SVG a coach mark filling the box WIDTH by HEIGHT at X, Y.
PEN and SPREAD are as `lispm-desktop-platinum-coach-mark-element' takes them."
  (svg-node svg 'path :d (lispm-desktop-platinum-coach-mark-path x y width height spread)
            :fill "none" :stroke lispm-desktop-platinum-coach-mark-color
            :stroke-width (or pen 3) :stroke-linecap "round" :stroke-linejoin "round"))

(provide 'lispm-desktop-platinum-coach)
;;; lispm-desktop-platinum-coach.el ends here
