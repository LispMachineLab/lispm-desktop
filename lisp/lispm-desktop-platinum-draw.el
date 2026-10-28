;;; lispm-desktop-platinum-draw.el --- Drawing Mac OS 9's forms as SVG  -*- lexical-binding: t; -*-

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

;; The parts of title bars the chrome is drawn of, all pure: palette and
;; geometry in, an SVG out.  The grounds and ridges of title bars, their
;; boxes, triangles.  They are drawn with LispM UI's primitives and kept
;; in its cache of drawn images (lispm-ui-draw.el).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-platinum-palette)
(require 'lispm-ui-draw)
(require 'lispm-ui-text)


(defconst lispm-desktop-platinum-title-height 22
  "Height in pixels of a title bar, and of a window's bottom edge.")

(defconst lispm-desktop-platinum-menu-bar-height 20
  "Height in pixels of the menu bar, Mac OS 9's.")

(defconst lispm-desktop-platinum-box-size 13
  "Side in pixels of a title bar box.")

(defconst lispm-desktop-platinum-box-top 3
  "Pixels from a title bar's top to its boxes'.")

;;;; Drawing (pure)

(defconst lispm-desktop-platinum-title-bar-height 21
  "Height in pixels of a title bar: its lit top line to the content's
black line, both included.  The outline above it is drawn by what is
above: the menu bar's black line, a window divider, a window's frame.")

(defun lispm-desktop-platinum--draw-title-ground (svg x width palette active)
  "Draw a title bar's ground on SVG from X, WIDTH pixels wide.
An ACTIVE bar is platinum, lit along its top, shaded along its bottom
over the content's black line; an inactive one lies flat, over a grey
line."
  (let ((grey (lambda (level) (lispm-ui-grey palette level)))
        (h lispm-desktop-platinum-title-bar-height))
    (lispm-ui-svg-fill svg x 0 width h (funcall grey (if active #xCC #xDD)))
    (if (not active)
        (lispm-ui-svg-fill svg x (1- h) width 1 (funcall grey #x55))
      (lispm-ui-svg-fill svg x 0 width 1 (funcall grey #xFF))
      (lispm-ui-svg-fill svg x (- h 2) width 1 (funcall grey #x99))
      (lispm-ui-svg-fill svg x (1- h) width 1 (plist-get palette :outline)))))

(defun lispm-desktop-platinum--draw-ridges (svg from to palette)
  "Draw a title bar's six ridges on SVG from column FROM to TO, both in.
Each ridge is a lit line over a shaded one, the lit line starting a
pixel further left and the shaded one ending a pixel further right, so
their ends look engraved."
  (when (< from to)
    (dotimes (i 6)
      (let ((y (+ 3 (* 2 i))))
        (lispm-ui-svg-row svg from (1- to) y (lispm-ui-grey palette #xFF))
        (lispm-ui-svg-row svg (1+ from) to (1+ y) (lispm-ui-grey palette #x77))))))

(defun lispm-desktop-platinum-draw-box (palette kind &optional modified pressed)
  "Return an SVG of a title bar box of KIND, `close', `zoom' or `collapse'.
The box is `lispm-desktop-platinum-box-size' square on an active title
bar's ground, as Mac OS 9 drew it: a recessed well round a dark frame,
inside it a bevel round a square shaded diagonally from grey to white.
The zoom box's glyph is a square sharing the frame's top left corner,
the collapse box's two lines across it.  MODIFIED puts a dot in a close
box; PRESSED draws the box held down, its square darkened and its bevel
turned over."
  (let* ((s lispm-desktop-platinum-box-size)
         (svg (svg-create s lispm-desktop-platinum-title-bar-height))
         (grey (lambda (level) (lispm-ui-grey palette level)))
         (y lispm-desktop-platinum-box-top)
         (ink (funcall grey #x22))
         (ramp (if pressed '(#x55 #x66 #x77 #x88 #x99 #xAA #xBB)
                 '(#x99 #xAA #xBB #xCC #xDD #xEE #xFF))))
    (cl-flet ((dot (x dy color) (lispm-ui-svg-dot svg x (+ y dy) color))
              (row (from to dy color) (lispm-ui-svg-row svg from to (+ y dy) color))
              (column (x from to color) (lispm-ui-svg-column svg x (+ y from) (+ y to) color)))
      (lispm-desktop-platinum--draw-title-ground svg 0 s palette t)
      ;; The well.
      (row 0 11 0 (funcall grey #x88))
      (column 0 0 11 (funcall grey #x88))
      (row 1 12 12 (funcall grey #xFF))
      (column 12 1 12 (funcall grey #xFF))
      ;; The frame.
      (row 1 11 1 ink)
      (row 1 11 11 ink)
      (column 1 1 11 ink)
      (column 11 1 11 ink)
      ;; The bevel, lit from the top left, or from below when pressed.
      (row 2 10 2 (funcall grey (if pressed #x88 #xCC)))
      (column 2 3 10 (funcall grey (if pressed #x88 #xCC)))
      (dot 2 2 (funcall grey (if pressed #x77 #xFF)))
      (column 10 3 10 (funcall grey (if pressed #xCC #x88)))
      (row 3 10 10 (funcall grey (if pressed #xCC #x88)))
      ;; The square, shaded along its diagonal.
      (dotimes (j 7)
        (dotimes (i 7)
          (dot (+ 3 i) (+ 3 j) (funcall grey (nth (/ (+ i j) 2) ramp)))))
      (pcase kind
        ('zoom
         (column 7 1 7 ink)
         (row 1 7 7 ink))
        ('collapse
         (row 1 11 5 ink)
         (row 1 11 7 ink)))
      (when (and modified (eq kind 'close))
        (svg-circle svg (/ s 2.0) (+ y (/ s 2.0)) 2 :fill (plist-get palette :outline))))
    svg))

(cl-defun lispm-desktop-platinum-draw-title-part (palette width &key active ridges label label-x
                                                          lead trail)
  "Return an SVG of a stretch of title bar WIDTH pixels wide, in PALETTE.
It shows the ground of an ACTIVE or inactive bar, the RIDGES, a list of
\(FROM . TO) columns, and LABEL, a string as Emacs would show it in the
titles' face, its icon included, from column LABEL-X.  LEAD draws the
lit line down the bar's left end, TRAIL the shaded one down its right."
  (let ((svg (svg-create width lispm-desktop-platinum-title-bar-height))
        (h lispm-desktop-platinum-title-bar-height))
    (lispm-desktop-platinum--draw-title-ground svg 0 width palette active)
    (when active
      (when lead
        (lispm-ui-svg-column svg 0 0 (- h 3) (lispm-ui-grey palette #xFF)))
      (when trail
        (lispm-ui-svg-column svg (1- width) 1 (- h 3) (lispm-ui-grey palette #x99)))
      (pcase-dolist (`(,from . ,to) ridges)
        (lispm-desktop-platinum--draw-ridges svg from to palette)))
    (when label
      (lispm-ui-svg-content
       svg label label-x 0 (- h 2)
       (if active (plist-get palette :text) (lispm-ui-grey palette #x77))
       (if active 'lispm-ui-title 'lispm-desktop-platinum-title-inactive)))
    svg))

(defconst lispm-desktop-platinum--triangle
  '("O" "OO" "OLO" "OLFO" "OLFFO" "OLFFDO" "OLFDOSs" "OLDOSs" "OFOSs" "OOSs" "OSs" " s")
  "Mac OS 9's disclosure triangle pointing right, a row of pixels a string.
O is the outline, L, F and D the light, face and dark of the variation
colour, S and s the shadow falling to the lower right.")

(cl-defgeneric lispm-desktop-platinum-draw-triangle (palette open)
  "Return an SVG of a list's disclosure triangle in PALETTE.
OPEN points it down, at a disclosed group; otherwise it points right.
It is drawn as Mac OS 9 drew it: outlined, in the variation colour lit
from the left, casting a shadow; pointing down, it is the same triangle
turned over its diagonal."
  (let ((svg (svg-create 12 12))
        (colors `((?O . ,(plist-get palette :outline)) (?L . ,(plist-get palette :thumb-light))
                  (?F . ,(plist-get palette :thumb)) (?D . ,(plist-get palette :thumb-dark))
                  (?S . ,(lispm-ui-grey palette #x88))
                  (?s . ,(lispm-ui-grey palette #xBB)))))
    (seq-do-indexed
     (lambda (row y)
       (seq-do-indexed
        (lambda (pixel x)
          (when-let* ((color (alist-get pixel colors)))
            (if open
                (lispm-ui-svg-dot svg y (+ x 3) color)
              (lispm-ui-svg-dot svg (+ x 3) y color))))
        row))
     lispm-desktop-platinum--triangle)
    svg))

(defun lispm-desktop-platinum--strut (width)
  "Return a blank image WIDTH pixels wide, as tall as a title bar.
Every title bar opens with one, so active and inactive bars are equally
tall and the content below never shifts when the focus moves."
  (lispm-ui-image (list 'strut width)
                                 (lambda ()
                                   (let ((svg (svg-create width lispm-desktop-platinum-title-height)))
                                     (lispm-ui-svg-fill svg 0 0 width lispm-desktop-platinum-title-height
                                                                   (plist-get lispm-ui-current-palette :chrome))
                                     svg))))

(defun lispm-desktop-platinum--box (kind &optional modified pressed)
  "Return the image of a title bar box of KIND.
See `lispm-desktop-platinum-draw-box'."
  (lispm-ui-image (list 'box kind modified pressed)
                                 (lambda () (lispm-desktop-platinum-draw-box lispm-ui-current-palette kind
                                                                             modified pressed))))

(defun lispm-desktop-platinum--title-part (width &rest options)
  "Return a stretch of title bar WIDTH pixels wide as a string displaying it.
OPTIONS are those of `lispm-desktop-platinum-draw-title-part'."
  (propertize " " 'display
              (lispm-ui-image
               (list 'title-part width
                     (plist-get options :active) (plist-get options :ridges)
                     (and (plist-get options :label)
                          (lispm-ui-content-key (plist-get options :label)))
                     (plist-get options :label-x) (plist-get options :lead) (plist-get options :trail))
               (lambda () (apply #'lispm-desktop-platinum-draw-title-part lispm-ui-current-palette
                                 width options)))))

(defun lispm-desktop-platinum-triangle (open)
  "Return a disclosure triangle as a string, pointing down when OPEN.
On a text terminal it is a character, a cell wide."
  (if (lispm-desktop-tty-p)
      (if open "▾" "▸")
    (propertize " " 'display (lispm-ui-image
                              (list 'triangle open)
                              (lambda () (lispm-desktop-platinum-draw-triangle lispm-ui-current-palette open))))))

(defconst lispm-desktop-platinum-bevel-padding 8
  "Pixels between a bevel button's sides and its label.")

(provide 'lispm-desktop-platinum-draw)
;;; lispm-desktop-platinum-draw.el ends here
