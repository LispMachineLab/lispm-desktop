;;; lispm-desktop-platinum-controls.el --- Mac OS 9's controls  -*- lexical-binding: t; -*-

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

;; Mac OS 9's controls, drawn in the theme's colours: push, bevel and
;; header buttons, tabs, placards, radio buttons and check boxes, group
;; boxes, window frames and the Control Strip's segments; and the same
;; controls as text, and as buttons in buffers.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-platinum-draw)
(require 'lispm-desktop-modal)


;;;; Header strips, header buttons and group boxes


(defconst lispm-desktop-platinum-header-strip-height 22
  "Height in pixels of a window's header strip, as the Finder window's.")

(defun lispm-desktop-platinum--draw-header-ground (svg width palette)
  "Draw on SVG the ground of a header strip WIDTH pixels wide, in PALETTE:
lit along its top, platinum, shaded along its bottom over a black line."
  (let ((h lispm-desktop-platinum-header-strip-height)
        (grey (lambda (level) (lispm-desktop-platinum-grey palette level))))
    (lispm-desktop-platinum-svg-fill svg 0 0 width h (funcall grey #xDD))
    (lispm-desktop-platinum-svg-row svg 0 (1- width) 0 (funcall grey #xFF))
    (lispm-desktop-platinum-svg-row svg 0 (1- width) (- h 2) (funcall grey #xAA))
    (lispm-desktop-platinum-svg-row svg 0 (1- width) (1- h) (plist-get palette :outline))))

(defun lispm-desktop-platinum-draw-header-strip (palette width &optional label left)
  "Return an SVG of a window's header strip WIDTH pixels wide, in PALETTE.
It runs under the title bar as the Finder window's did, lit along its
top, platinum, shaded along its bottom over a black line, LABEL centred
in the small system font, as \"9 items, 705 MB available\".  LEFT is the
width of the strip's opening drawn before it, as by
`lispm-desktop-platinum-draw-header-button': LABEL is centred on the
whole strip, kept clear of the opening."
  (let* ((h lispm-desktop-platinum-header-strip-height)
         (left (or left 0))
         (svg (svg-create width h)))
    (lispm-desktop-platinum--draw-header-ground svg width palette)
    (when label
      (let ((x (- (/ (- (+ left width) (lispm-desktop-platinum-content-width
                                         label 'lispm-desktop-platinum-small))
                     2)
                  left)))
        (lispm-desktop-platinum-svg-content
         svg label (if (> left 0) (max 8 x) x)
         1 (- h 3) (plist-get palette :text) 'lispm-desktop-platinum-small)))
    svg))

(defun lispm-desktop-platinum-header-strip (label &optional left)
  "Return the header strip of the window being drawn, showing LABEL.
It is a string to put in the window's header line; see
`lispm-desktop-platinum-draw-header-strip'.  LEFT is the width of the
strip's opening put before it, as `lispm-desktop-platinum-header-button'
returns one."
  (let* ((left (or left 0))
         (width (- (window-pixel-width) (window-right-divider-width) (window-scroll-bar-width) left)))
    (propertize " " 'display
                (lispm-desktop-platinum-image
                 (list 'header-strip label width left)
                 (lambda () (lispm-desktop-platinum-draw-header-strip lispm-desktop-platinum-current-palette
                                                                      width label left))))))

(defconst lispm-desktop-platinum-header-button-height 17
  "Height in pixels of a bevel button opening a header strip.")

(defun lispm-desktop-platinum-header-button-width (label)
  "Return the width of a header strip's opening holding a button reading LABEL."
  (+ 6 (lispm-desktop-platinum-content-width label 'lispm-desktop-platinum-small)
     (* 2 lispm-desktop-platinum-bevel-padding)))

(defun lispm-desktop-platinum-draw-header-button (palette label &optional state)
  "Return an SVG of a header strip's opening, a bevel button reading LABEL.
Mac OS 9's Finder windows held no button there; LispM's opens the strip
with one, standing on the strip's ground six pixels in from its left,
LABEL in the small system font, in PALETTE.  STATE is as
`lispm-desktop-platinum-draw-bevel-button' takes it.  The rest of the
strip follows, drawn by `lispm-desktop-platinum-draw-header-strip'."
  (let* ((width (lispm-desktop-platinum-header-button-width label))
         (svg (svg-create width lispm-desktop-platinum-header-strip-height)))
    (lispm-desktop-platinum--draw-header-ground svg width palette)
    (lispm-desktop-platinum--draw-bevel-button svg 6 2 palette (- width 6)
                                               lispm-desktop-platinum-header-button-height
                                               :state state :label label
                                               :face 'lispm-desktop-platinum-small)
    svg))

(defun lispm-desktop-platinum-header-button (label &optional state)
  "Return a header strip's opening, a bevel button reading LABEL in STATE,
as a string displaying it; see `lispm-desktop-platinum-draw-header-button'."
  (propertize " " 'display
              (lispm-desktop-platinum-image
               (list 'header-button (lispm-desktop-platinum--content-key label) state)
               (lambda () (lispm-desktop-platinum-draw-header-button lispm-desktop-platinum-current-palette
                                                                     label state)))))

(defconst lispm-desktop-platinum-group-title-height 18
  "Height in pixels of a group box's top edge, which carries its title.")

(defun lispm-desktop-platinum-draw-group-edge (palette width &optional title bottom)
  "Return an SVG of a group box's top edge WIDTH pixels wide, in PALETTE.
Mac OS 9 framed a group of controls in an engraved line, a grey line
with a lit one along it, TITLE breaking the top line near its left end
in the system font.  BOTTOM draws the bottom edge instead, closing the
frame's sides at the top of a line as tall as a line of text."
  (let* ((h (if bottom (default-line-height) lispm-desktop-platinum-group-title-height))
         (y (if bottom 1 (/ h 2)))
         (svg (svg-create width h))
         (dark (lispm-desktop-platinum-grey palette #x88))
         (lit (lispm-desktop-platinum-grey palette #xFF)))
    (if bottom
        (progn
          (lispm-desktop-platinum-svg-column svg 0 0 y dark)
          (lispm-desktop-platinum-svg-column svg 1 0 (1- y) lit)
          (lispm-desktop-platinum-svg-column svg (- width 2) 0 y dark)
          (lispm-desktop-platinum-svg-column svg (1- width) 0 (1+ y) lit))
      (lispm-desktop-platinum-svg-column svg 0 y (1- h) dark)
      (lispm-desktop-platinum-svg-column svg 1 (1+ y) (1- h) lit)
      (lispm-desktop-platinum-svg-column svg (- width 2) y (1- h) dark)
      (lispm-desktop-platinum-svg-column svg (1- width) y (1- h) lit))
    (lispm-desktop-platinum-svg-row svg 0 (- width 2) y dark)
    (lispm-desktop-platinum-svg-row svg 1 (1- width) (1+ y) lit)
    (when title
      (let ((title-width (lispm-desktop-platinum-content-width title 'lispm-desktop-platinum-title)))
        (lispm-desktop-platinum-svg-fill svg 8 0 (+ title-width 8) h (lispm-desktop-platinum-grey palette #xEE))
        (lispm-desktop-platinum-svg-content svg title 12 0 h (plist-get palette :text)
                                              'lispm-desktop-platinum-title)))
    svg))

(defun lispm-desktop-platinum-group-edge (width &optional title bottom)
  "Return a group box's edge WIDTH pixels wide as a string displaying it.
See `lispm-desktop-platinum-draw-group-edge'."
  (propertize " " 'display
              (lispm-desktop-platinum-image
               (list 'group-edge width title bottom)
               (lambda () (lispm-desktop-platinum-draw-group-edge lispm-desktop-platinum-current-palette
                                                                  width title bottom)))))

(defun lispm-desktop-platinum-group-side (height)
  "Return a group box's side for a line HEIGHT pixels tall, as a string
displaying it: the grey line and the lit one beside it."
  (propertize " " 'display
              (lispm-desktop-platinum-image
               (list 'group-side height)
               (lambda ()
                 (let ((svg (svg-create 2 height)))
                   (lispm-desktop-platinum-svg-fill svg 0 0 1 height
                                                 (lispm-desktop-platinum-grey lispm-desktop-platinum-current-palette #x88))
                   (lispm-desktop-platinum-svg-fill svg 1 0 1 height
                                                 (lispm-desktop-platinum-grey lispm-desktop-platinum-current-palette #xFF))
                   svg)))))

;;;; Controls (pure)
;;
;; Mac OS 9's controls as its Appearance Manager drew them in Platinum,
;; measured pixel by pixel on Mac OS 9.0.4: push buttons and the ring of
;; the default one, bevel buttons, tabs, placards, radio buttons, check
;; boxes, focus rings, window frames, scroll bars and the Control Strip's
;; segments.  Each draws with `lispm-desktop-platinum-grey', so Platinum's
;; greys come out as themselves and another theme gets their
;; counterparts.  Labels are SVG text in the system font, as heavy as the
;; titles and menus Emacs draws (`lispm-desktop-platinum-svg-font').

(defconst lispm-desktop-platinum-button-height 20
  "Height in pixels of a push button, its outline included.")

(defconst lispm-desktop-platinum-tab-height 21
  "Height in pixels of a tab, its outline included.
The edge of the pane the tabs stand on takes three rows more.")

(defconst lispm-desktop-platinum-strip-height 24
  "Height in pixels of the Control Strip, its outline included.")

(defun lispm-desktop-platinum-svg-pixel-map (svg x y rows colors)
  "Draw ROWS, strings of pixels, on SVG from X, Y.
COLORS maps each character to its colour; a character it leaves out is
left clear."
  (seq-do-indexed
   (lambda (row dy)
     (seq-do-indexed
      (lambda (pixel dx)
        (when-let* ((color (alist-get pixel colors)))
          (lispm-desktop-platinum--dot svg (+ x dx) (+ y dy) color)))
      row))
   rows))

;;;;; Push buttons

(defun lispm-desktop-platinum--push-button-colors (palette state)
  "Return the colours of a push button's parts in STATE, as a plist.
STATE is nil, `pressed' or `disabled'.  The parts are its :outline, the
:edge pixels rounding its corners, the outer and inner lines lit along
its top and left (:lit, :lit-inner), its :face, the inner and outer
lines shaded along its bottom and right (:shade-inner, :shade), the
pixels where lit and shaded meet (:corner, :corner-low, :corner-inner)
and its :label."
  (cl-flet ((g (level) (lispm-desktop-platinum-grey palette level)))
    (pcase state
      ('pressed
       (list :outline (plist-get palette :outline) :edge (g #x22)
             :lit (g #x44) :lit-inner (g #x55) :face (g #x66)
             :shade-inner (g #x77) :shade (g #x88)
             :corner (g #x44) :corner-low (g #x77) :corner-inner (g #x44)
             :label (lispm-desktop-platinum-legible (g #x66) (plist-get palette :text) (g #xFF))))
      ('disabled
       (let ((face (g #xDD)))
         (list :outline (g #x88) :edge (g #x88)
               :lit face :lit-inner face :face face :shade-inner face :shade face
               :corner face :corner-low face :corner-inner face
               :label (g #x77))))
      (_
       (list :outline (plist-get palette :outline) :edge (g #x22)
             :lit (g #xDD) :lit-inner (g #xFF) :face (g #xDD)
             :shade-inner (g #xAA) :shade (g #x77)
             :corner (g #xBB) :corner-low (g #xBB) :corner-inner (g #xFF)
             :label (plist-get palette :text))))))

(defun lispm-desktop-platinum--draw-push-frame (svg x y width colors)
  "Draw a push button WIDTH pixels wide from X, Y of SVG, without its label.
COLORS is a value of `lispm-desktop-platinum--push-button-colors'.  The
outline is rounded by stepping in a pixel on each of its first two rows;
inside it, two lines are lit along the top and left and two shaded along
the bottom and right.  The pixels its corners cut off are left clear."
  (let ((w width)
        (h lispm-desktop-platinum-button-height))
    (cl-flet ((dot (dx dy part)
                (lispm-desktop-platinum--dot svg (+ x dx) (+ y dy) (plist-get colors part)))
              (row (from to dy part)
                (lispm-desktop-platinum-svg-row svg (+ x from) (+ x to) (+ y dy) (plist-get colors part)))
              (column (dx from to part)
                (lispm-desktop-platinum-svg-column svg (+ x dx) (+ y from) (+ y to) (plist-get colors part))))
      (lispm-desktop-platinum-svg-fill svg (+ x 1) (+ y 1) (- w 2) (- h 2) (plist-get colors :face))
      ;; The outline.
      (row 3 (- w 4) 0 :outline)
      (row 3 (- w 4) (- h 1) :outline)
      (column 0 3 (- h 4) :outline)
      (column (- w 1) 3 (- h 4) :outline)
      (pcase-dolist (`(,dx . ,dy) `((1 . 1) (,(- w 2) . 1) (1 . ,(- h 2)) (,(- w 2) . ,(- h 2))))
        (dot dx dy :outline))
      (pcase-dolist (`(,dx . ,dy) `((2 . 0) (,(- w 3) . 0) (0 . 2) (,(- w 1) . 2)
                                    (0 . ,(- h 3)) (,(- w 1) . ,(- h 3))
                                    (2 . ,(- h 1)) (,(- w 3) . ,(- h 1))))
        (dot dx dy :edge))
      ;; Lit along the top and left.
      (row 3 (- w 4) 1 :lit)
      (row 3 (- w 4) 2 :lit-inner)
      (column 1 3 (- h 4) :lit)
      (column 2 3 (- h 4) :lit-inner)
      (dot 3 3 :lit-inner)
      ;; Shaded along the bottom and right.
      (column (- w 3) 3 (- h 4) :shade-inner)
      (column (- w 2) 3 (- h 4) :shade)
      (dot (- w 4) (- h 4) :shade-inner)
      (row 3 (- w 4) (- h 3) :shade-inner)
      (row 3 (- w 3) (- h 2) :shade)
      (dot (- w 3) (- h 3) :shade)
      (dot (- w 2) (- h 3) :shade)
      ;; Where lit meets shaded.
      (dot 2 1 :corner)
      (dot (- w 3) 1 :corner)
      (dot 1 2 :corner)
      (dot 1 (- h 3) :corner)
      (dot 2 2 :corner-inner)
      (dot (- w 2) 2 :corner-low)
      (dot 2 (- h 2) :corner-low)
      (dot (- w 3) 2 :face)
      (dot 2 (- h 3) :face))))

(defun lispm-desktop-platinum--draw-default-ring (svg x y width palette &optional disabled)
  "Draw the ring of a default push button WIDTH pixels wide, from X, Y of SVG.
The ring stands three pixels about the button: an outline rounded at its
corners, then a band lit along the top and left and shaded along the
bottom and right, which shows too where the button's corners are cut.
A DISABLED button's ring is a flat grey outline."
  (let* ((w (+ width 6))
         (h (+ lispm-desktop-platinum-button-height 6))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
         (outline (if disabled (funcall grey #x88) (plist-get palette :outline)))
         (edge (if disabled outline (funcall grey #x22)))
         (lit (funcall grey #xDD))
         (lit-inner (if disabled lit (funcall grey #xAA)))
         (shade-inner (if disabled lit (funcall grey #xAA)))
         (shade (if disabled lit (funcall grey #x77)))
         (meet (if disabled lit (funcall grey #x88))))
    (cl-flet ((dot (dx dy color) (lispm-desktop-platinum--dot svg (+ x dx) (+ y dy) color))
              (row (from to dy color)
                (lispm-desktop-platinum-svg-row svg (+ x from) (+ x to) (+ y dy) color))
              (column (dx from to color)
                (lispm-desktop-platinum-svg-column svg (+ x dx) (+ y from) (+ y to) color)))
      ;; The band.
      (row 3 (- w 4) 1 lit)
      (row 2 3 2 lit)
      (row 4 (- w 5) 2 lit-inner)
      (column 1 3 (- h 4) lit)
      (column 2 3 3 lit)
      (column 2 4 (- h 5) lit-inner)
      (column (- w 3) 4 (- h 5) shade-inner)
      (column (- w 2) 3 (- h 4) shade)
      (row 4 (- w 5) (- h 3) shade-inner)
      (row 3 (- w 4) (- h 2) shade)
      (dot (- w 3) (- h 4) meet)
      (dot (- w 4) (- h 3) meet)
      (dot (- w 3) (- h 3) shade)
      ;; The corners the button's outline cuts off.
      (pcase-dolist (`(,cx ,cy ,sx ,sy) `((3 3 1 1) (,(- w 4) 3 -1 1)
                                          (3 ,(- h 4) 1 -1) (,(- w 4) ,(- h 4) -1 -1)))
        (dot cx cy shade-inner)
        (dot (+ cx sx) cy shade)
        (dot cx (+ cy sy) shade))
      ;; The outline.
      (row 4 (- w 5) 0 outline)
      (row 4 (- w 5) (- h 1) outline)
      (column 0 4 (- h 5) outline)
      (column (- w 1) 4 (- h 5) outline)
      (pcase-dolist (`(,dx . ,dy) `((2 . 1) (1 . 2) (,(- w 3) . 1) (,(- w 2) . 2)
                                    (1 . ,(- h 3)) (2 . ,(- h 2))
                                    (,(- w 2) . ,(- h 3)) (,(- w 3) . ,(- h 2))))
        (dot dx dy outline))
      (pcase-dolist (`(,dx . ,dy) `((3 . 0) (,(- w 4) . 0) (0 . 3) (,(- w 1) . 3)
                                    (0 . ,(- h 4)) (,(- w 1) . ,(- h 4))
                                    (3 . ,(- h 1)) (,(- w 4) . ,(- h 1))))
        (dot dx dy edge)))))

(defun lispm-desktop-platinum-svg-focus-ring (svg x y width height radius palette)
  "Draw on SVG the keyboard focus ring about the WIDTH by HEIGHT box at X, Y.
The ring is two pixels in PALETTE's focus colour just outside the box,
its corners rounded by RADIUS, as Mac OS 9 ringed the control the
keyboard was on."
  (svg-rectangle svg (- x 1) (- y 1) (+ width 2) (+ height 2)
                 :rx (+ radius 1) :fill "none"
                 :stroke (plist-get palette :focus) :stroke-width 2))

(defun lispm-desktop-platinum-push-button-width (label)
  "Return the width of a push button reading LABEL: room for it, at least 60.
LABEL is measured in the titles' face, the system font."
  (max 60 (+ 24 (string-pixel-width (propertize label 'face 'lispm-desktop-platinum-title)))))

(cl-defun lispm-desktop-platinum-draw-push-button (palette label width
                                                          &key state default focused (pad 0) font)
  "Return an SVG of a push button WIDTH pixels wide reading LABEL, in PALETTE.
The button is `lispm-desktop-platinum-button-height' tall, drawn as Mac
OS 9 drew it: rounded, outlined, lit along the top and left, its label
centred in the system font.  STATE is nil; `pressed', held down, dark
and lit from below, its label light; or `disabled', outlined in grey,
flat, its label dimmed.

DEFAULT rings the button, as the one Return presses; the ring takes
three pixels about it.  FOCUSED rings it, or its default ring, in the
focus colour, two pixels more, as the button Space presses: LispM's own
addition, for the keyboard.  PAD is the room left clear about the
button, raised to what its rings need, so buttons drawn with the same
PAD keep their places as the focus moves.  FONT is a value of
`lispm-desktop-platinum-svg-font', by default the system font's."
  (let* ((ring (if default 3 0))
         (pad (max pad (+ ring (if focused 2 0))))
         (h lispm-desktop-platinum-button-height)
         (svg (svg-create (+ width (* 2 pad)) (+ h (* 2 pad)))))
    (lispm-desktop-platinum-svg-push-button svg pad pad palette label width
                                              :state state :default default :focused focused :font font)
    svg))

(cl-defun lispm-desktop-platinum-svg-push-button (svg x y palette label width
                                                        &key state default focused font)
  "Draw on SVG at X, Y a push button WIDTH pixels wide reading LABEL.
Its rings, when DEFAULT or FOCUSED, are drawn about it; see
`lispm-desktop-platinum-draw-push-button' for the rest."
  (let ((ring (if default 3 0))
        (h lispm-desktop-platinum-button-height)
        (colors (lispm-desktop-platinum--push-button-colors palette state)))
    (when focused
      (lispm-desktop-platinum-svg-focus-ring svg (- x ring) (- y ring)
                                               (+ width (* 2 ring)) (+ h (* 2 ring))
                                               (+ 3 ring) palette))
    (when default
      (lispm-desktop-platinum--draw-default-ring svg (- x 3) (- y 3) width palette
                                                 (eq state 'disabled)))
    (lispm-desktop-platinum--draw-push-frame svg x y width colors)
    (lispm-desktop-platinum--label svg label (+ x (/ width 2.0)) (+ y 14)
                                   (plist-get colors :label)
                                   (or font (lispm-desktop-platinum-svg-font)))))

;;;;; Bevel buttons

(defun lispm-desktop-platinum-bevel-button-colors (palette state)
  "Return the colours of a bevel button's parts in STATE, as a plist.
STATE is nil, `pressed' or `disabled'.  The parts are its outline lit
along the top and left (:outline-lit) and shaded along the bottom and
right (:outline-shade), its two lit lines (:lit, :lit-inner), its :face,
its two shaded lines (:shade-inner, :shade) and its :label."
  (cl-flet ((g (level) (lispm-desktop-platinum-grey palette level)))
    (pcase state
      ('pressed
       (list :outline-lit (g #x11) :outline-shade (g #x44)
             :lit (g #x44) :lit-inner (g #x66) :face (g #x88)
             :shade-inner (g #x99) :shade (g #xBB)
             :label (lispm-desktop-platinum-legible (g #x88) (plist-get palette :text) (g #xFF))))
      ('disabled
       (let ((face (g #xCC)))
         (list :outline-lit (g #x99) :outline-shade (g #x99)
               :lit face :lit-inner face :face face :shade-inner face :shade face
               :label (g #x77))))
      (_
       (list :outline-lit (g #x66) :outline-shade (g #x33)
             :lit (g #xCC) :lit-inner (g #xFF) :face (g #xCC)
             :shade-inner (g #x99) :shade (g #x77)
             :label (plist-get palette :text))))))

(cl-defun lispm-desktop-platinum-draw-bevel-button (palette width height &key state label
                                                            (face 'lispm-desktop-platinum-list))
  "Return an SVG of a bevel button WIDTH by HEIGHT pixels, in PALETTE.
Bevel buttons are square, as Mac OS 9's toolbars held them: an outline
darker along the bottom and right, two lines lit along the top and left
and two shaded along the bottom and right.  STATE is nil, `pressed'
\(darkened, its bevel turned over) or `disabled' (flat, its outline and
label greyed).  LABEL, when given, is centred: a string as Emacs would
show it in FACE, its icons and images included, see
`lispm-desktop-platinum-svg-content'."
  (let ((svg (svg-create width height)))
    (lispm-desktop-platinum--draw-bevel-button svg 0 0 palette width height
                                               :state state :label label :face face)
    svg))

(cl-defun lispm-desktop-platinum--draw-bevel-button (svg x y palette width height
                                                         &key state label
                                                         (face 'lispm-desktop-platinum-list))
  "Draw on SVG at X, Y a bevel button WIDTH by HEIGHT pixels, in PALETTE.
See `lispm-desktop-platinum-draw-bevel-button'."
  (let ((colors (lispm-desktop-platinum-bevel-button-colors palette state))
        (w width)
        (h height))
    (cl-flet ((row (from to line part)
                (lispm-desktop-platinum-svg-row svg (+ x from) (+ x to) (+ y line) (plist-get colors part)))
              (column (col from to part)
                (lispm-desktop-platinum-svg-column svg (+ x col) (+ y from) (+ y to) (plist-get colors part))))
      (lispm-desktop-platinum-svg-fill svg x y w h (plist-get colors :face))
      (row 1 (- w 2) 1 :lit)
      (column 1 1 (- h 2) :lit)
      (row 2 (- w 3) 2 :lit-inner)
      (column 2 2 (- h 3) :lit-inner)
      (column (- w 3) 2 (- h 3) :shade-inner)
      (row 2 (- w 3) (- h 3) :shade-inner)
      (column (- w 2) 1 (- h 2) :shade)
      (row 1 (- w 2) (- h 2) :shade)
      (row 0 (- w 1) 0 :outline-lit)
      (column 0 0 (- h 1) :outline-lit)
      (column (- w 1) 1 (- h 1) :outline-shade)
      (row 1 (- w 1) (- h 1) :outline-shade))
    (when label
      (lispm-desktop-platinum-svg-content
       svg label (+ x (/ (- w (lispm-desktop-platinum-content-width label face)) 2)) y h
       (plist-get colors :label) face))))

;;;;; Tabs

(defconst lispm-desktop-platinum--tab-slant
  '((10 . t) (8 . t) (7 . t) (6 . t) (6) (5 . t) (5) (5) (4 . t) (4) (4)
    (3 . t) (3) (3) (2 . t) (2) (2) (1 . t) (1) (1) (0 . t))
  "Where a tab's left side runs, row by row from its top, as (COLUMN . STEP).
COLUMN holds the outline; STEP is non-nil on a row where the outline
steps out, which is softened there.  The right side is the mirror.")

(cl-defun lispm-desktop-platinum-draw-tab (palette label width &key front
                                                   (face 'lispm-desktop-platinum-title))
  "Return an SVG of a tab WIDTH pixels wide reading LABEL, in PALETTE.
Tabs are trapezoids, their sides slanting out a pixel every three rows
from top corners rounded off, and stand on the edge of the pane they
choose among: the SVG is `lispm-desktop-platinum-tab-height' tall, and
three rows more for that edge.  The tab in FRONT is the pane's colour
and opens into it; a tab behind is darker, lit along its top and left,
and the pane's outline runs under it.  LABEL is a string as Emacs would
show it in FACE, by default the system font's, its icons included; every
label is black, none heavier than the others."
  (let* ((h lispm-desktop-platinum-tab-height)
         (w width)
         (svg (svg-create w (+ h 3)))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
         (outline (plist-get palette :outline))
         (edge (funcall grey #x22))
         (body (funcall grey (if front #xEE #xCC)))
         (between (funcall grey #xCC))
         (lit (funcall grey (if front #xFF #xDD)))
         (soft (funcall grey (if front #xBB #xAA)))
         (shade (funcall grey #x88))
         (shade-soft (funcall grey #x44))
         (shade-lit (funcall grey (if front #xDD #xCC))))
    (cl-flet ((dot (x y color) (lispm-desktop-platinum--dot svg x y color))
              (row (from to y color) (lispm-desktop-platinum-svg-row svg from to y color)))
      (lispm-desktop-platinum-svg-fill svg 0 0 w h (plist-get palette :chrome))
      (seq-do-indexed
       (pcase-lambda (`(,left . ,step) y)
         (let ((right (- w 1 left)))
           (pcase y
             (0 (row (1+ left) (1- right) 0 outline)
                (dot left 0 edge) (dot right 0 edge))
             (1 (row (+ left 2) (- right 2) 1 between)
                (dot left 1 edge) (dot (1+ left) 1 outline) (dot (+ left 2) 1 soft)
                (dot right 1 edge) (dot (1- right) 1 outline))
             (2 (row (+ left 3) (- right 3) 2 lit)
                (dot left 2 edge) (dot (1+ left) 2 soft) (dot (+ left 2) 2 between)
                (dot right 2 edge) (dot (1- right) 2 shade) (dot (- right 2) 2 shade-lit))
             (_ (row (1+ left) (1- right) y body)
                (dot left y (if step edge outline))
                (dot right y (if step edge outline))
                (if (= y 3)
                    (progn (dot (1+ left) y soft) (dot (+ left 2) y between) (dot (+ left 3) y lit))
                  (dot (1+ left) y (if step soft between))
                  (dot (+ left 2) y lit))
                (dot (1- right) y (if step shade-soft shade))
                (when front (dot (- right 2) y shade-lit))))))
       lispm-desktop-platinum--tab-slant)
      ;; The pane's edge: its outline, then a lit band, under the tabs
      ;; behind; the tab in front opens into the pane.
      (if front
          (progn
            (row 0 (1- w) h body)
            (row 0 (1- w) (+ h 1) body)
            (row 0 (1- w) (+ h 2) body)
            (dot 0 h outline) (dot 1 h between) (dot 2 h lit)
            (dot (- w 1) h outline) (dot (- w 2) h soft) (dot (- w 3) h shade-lit)
            (row 0 1 (+ h 1) between) (row (- w 2) (- w 1) (+ h 1) between)
            (dot 0 (+ h 2) (funcall grey #xFF)) (dot (- w 1) (+ h 2) (funcall grey #xFF)))
        (row 0 (1- w) h outline)
        (row 0 (1- w) (+ h 1) between)
        (row 0 (1- w) (+ h 2) (funcall grey #xFF))))
    (lispm-desktop-platinum-svg-content
     svg label (/ (- w (lispm-desktop-platinum-content-width label face)) 2) 0 h
     (plist-get palette :text) face)
    svg))

(defun lispm-desktop-platinum-draw-tab-edge (palette width)
  "Return an SVG of the pane's edge WIDTH pixels wide, beside or between tabs.
It is as tall as a tab and the pane's edge under it, see
`lispm-desktop-platinum-draw-tab': the window's body above, the pane's
outline and lit band below, in PALETTE."
  (let* ((h lispm-desktop-platinum-tab-height)
         (svg (svg-create width (+ h 3))))
    (lispm-desktop-platinum-svg-fill svg 0 0 width h (plist-get palette :chrome))
    (lispm-desktop-platinum-svg-row svg 0 (1- width) h (plist-get palette :outline))
    (lispm-desktop-platinum-svg-row svg 0 (1- width) (+ h 1) (lispm-desktop-platinum-grey palette #xCC))
    (lispm-desktop-platinum-svg-row svg 0 (1- width) (+ h 2) (lispm-desktop-platinum-grey palette #xFF))
    svg))

;;;;; Placards, radio buttons, check boxes

(cl-defun lispm-desktop-platinum-draw-placard (palette width height &key label font)
  "Return an SVG of a placard WIDTH by HEIGHT pixels, in PALETTE.
A placard is the plate Mac OS 9 showed status on, as the Finder window
counted its items on one: outlined, lit along the top and left, shaded
along the bottom and right.  LABEL, when given, is centred in FONT, a
value of `lispm-desktop-platinum-svg-font', by default the list's."
  (let ((svg (svg-create width height))
        (w width)
        (h height))
    (lispm-desktop-platinum-svg-fill svg 0 0 w h (plist-get palette :outline))
    (lispm-desktop-platinum-svg-fill svg 1 1 (- w 2) (- h 2) (lispm-desktop-platinum-grey palette #xDD))
    (lispm-desktop-platinum-svg-row svg 1 (- w 2) 1 (lispm-desktop-platinum-grey palette #xFF))
    (lispm-desktop-platinum-svg-column svg 1 1 (- h 2) (lispm-desktop-platinum-grey palette #xFF))
    (lispm-desktop-platinum-svg-row svg 2 (- w 2) (- h 2) (lispm-desktop-platinum-grey palette #xAA))
    (lispm-desktop-platinum-svg-column svg (- w 2) 2 (- h 2) (lispm-desktop-platinum-grey palette #xAA))
    (when label
      (let ((font (or font (lispm-desktop-platinum-svg-font 'lispm-desktop-platinum-list))))
        (lispm-desktop-platinum--label svg label (/ w 2.0)
                                       (round (+ (/ h 2.0) (* 0.35 (plist-get font :size))))
                                       (plist-get palette :text) font)))
    svg))

(defun lispm-desktop-platinum--gradient (svg id from to)
  "Define on SVG the gradient ID, from colour FROM at the top left to TO at
the bottom right."
  (let ((defs (or (car (dom-by-tag svg 'defs))
                  (let ((defs (dom-node 'defs)))
                    (dom-add-child-before svg defs)
                    defs))))
    (dom-append-child defs (dom-node 'linearGradient
                                     `((id . ,id) (x1 . 0) (y1 . 0) (x2 . 1) (y2 . 1))
                                     (dom-node 'stop `((offset . "0%") (stop-color . ,from)))
                                     (dom-node 'stop `((offset . "100%") (stop-color . ,to)))))))

(cl-defun lispm-desktop-platinum-draw-radio (palette on &key state focused (pad 0))
  "Return an SVG of a radio button in PALETTE, ON or off.
It is a twelve-pixel sphere, outlined, shaded from white at its top left
to grey at its bottom right, with a dot when ON.  STATE is nil,
`pressed' (darkened) or `disabled' (flat and grey).  FOCUSED rings it in
the focus colour, two pixels out; PAD is the room left about it, raised
to two pixels for the ring."
  (let* ((pad (max pad (if focused 2 0)))
         (size (+ 12 (* 2 pad)))
         (middle (/ size 2.0))
         (svg (svg-create size size))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
         (disabled (eq state 'disabled))
         (from (funcall grey (if (eq state 'pressed) #x88 #xFF)))
         (to (funcall grey (if (eq state 'pressed) #x55 #x88))))
    (lispm-desktop-platinum--gradient svg "sphere" from to)
    (when focused
      (svg-circle svg middle middle 7 :fill "none" :stroke (plist-get palette :focus) :stroke-width 2))
    (svg-circle svg middle middle 5.5 :fill (if disabled (funcall grey #xDD) "url(#sphere)")
                :stroke (if disabled (funcall grey #x88) (plist-get palette :outline))
                :stroke-width 1)
    (when on
      (svg-circle svg middle middle 2
                  :fill (if disabled
                            (funcall grey #x88)
                          (lispm-desktop-platinum-legible
                           (lispm-desktop-platinum-blend from to 0.5)
                           (plist-get palette :outline) (plist-get palette :text)))))
    svg))

(cl-defun lispm-desktop-platinum-draw-check-box (palette on &key state focused (pad 0))
  "Return an SVG of a check box in PALETTE, checked when ON.
The box is twelve pixels square, outlined, lit along its top and left
and shaded along its bottom and right; the check mark breaks out past
its top right corner, as Mac OS 9's did, so the SVG is fourteen pixels
square, the box at its bottom left.  STATE is nil, `pressed' (darkened)
or `disabled' (flat and grey).  FOCUSED rings the box in the focus
colour, two pixels out; PAD is the room left about it, raised to two
pixels for the ring."
  (let* ((pad (max pad (if focused 2 0)))
         (svg (svg-create (+ 14 (* 2 pad)) (+ 14 (* 2 pad))))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
         (disabled (eq state 'disabled))
         (pressed (eq state 'pressed))
         (ink (if disabled (funcall grey #x88) (plist-get palette :outline))))
    (when focused
      (lispm-desktop-platinum-svg-focus-ring svg pad (+ pad 2) 12 12 0 palette))
    (lispm-desktop-platinum-svg-fill svg pad (+ pad 2) 12 12 ink)
    (lispm-desktop-platinum-svg-fill svg (+ pad 1) (+ pad 3) 10 10 (funcall grey (if pressed #xAA #xDD)))
    (unless disabled
      (lispm-desktop-platinum-svg-row svg (+ pad 1) (+ pad 9) (+ pad 3) (funcall grey (if pressed #x88 #xFF)))
      (lispm-desktop-platinum-svg-column svg (+ pad 1) (+ pad 3) (+ pad 11) (funcall grey (if pressed #x88 #xFF)))
      (lispm-desktop-platinum-svg-row svg (+ pad 2) (+ pad 10) (+ pad 12) (funcall grey (if pressed #xDD #x88)))
      (lispm-desktop-platinum-svg-column svg (+ pad 10) (+ pad 4) (+ pad 12) (funcall grey (if pressed #xDD #x88))))
    (when on
      (svg-polyline svg (mapcar (pcase-lambda (`(,x . ,y)) (cons (+ x pad) (+ y pad)))
                                '((2.5 . 7.5) (5.5 . 10.5) (12.5 . 1.5)))
                    :fill "none" :stroke ink :stroke-width 2 :stroke-linecap "square"))
    svg))

;;;;; Window frames

(defconst lispm-desktop-platinum-frame-inset '(6 . 1)
  "Where a floating window stands inside its frame, as (LEFT . TOP) pixels:
past the outline and the four-pixel band and the content's line on the
left, right and bottom, and right under the outline at the top, where
its title bar runs.")

(cl-defun lispm-desktop-platinum-draw-window-frame (palette width height &key active)
  "Return an SVG of a window's frame WIDTH by HEIGHT pixels, in PALETTE.
The SVG takes in the outline and the drop shadow, a pixel along the
right and bottom; the window stands inside it at
`lispm-desktop-platinum-frame-inset', its title bar under the outline.
An ACTIVE window is outlined in black and framed in a band lit from the
top left (white, two of platinum, a shade), then the content's own
black line; beside the title bar the band takes the bar's colours, so
the bar reads as running from edge to edge.  An inactive window is
outlined in a dark grey, its frame flat.  The middle is left clear."
  (let* ((w width)
         (h height)
         (svg (svg-create w h))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
         (outline (if active (plist-get palette :outline) (funcall grey #x55)))
         (title lispm-desktop-platinum-title-bar-height)
         (right (- w 2))
         (bottom (- h 2)))
    (cl-flet ((row (from to y color) (lispm-desktop-platinum-svg-row svg from to y color))
              (column (x from to color) (lispm-desktop-platinum-svg-column svg x from to color)))
      ;; The shadow.
      (column (1- w) 2 (1- h) (plist-get palette :outline))
      (row 2 (1- w) (1- h) (plist-get palette :outline))
      ;; The band, flat to begin with.
      (lispm-desktop-platinum-svg-fill svg 1 1 5 (- h 3) (funcall grey (if active #xCC #xDD)))
      (lispm-desktop-platinum-svg-fill svg (- w 7) 1 5 (- h 3) (funcall grey (if active #xCC #xDD)))
      (lispm-desktop-platinum-svg-fill svg 1 (- h 7) (- w 3) 5 (funcall grey (if active #xCC #xDD)))
      (if (not active)
          (progn
            (column 5 (1+ title) (- h 7) outline)
            (column (- w 7) (1+ title) (- h 7) outline)
            (row 5 (- w 7) (- h 7) outline)
            (row 1 5 title outline)
            (row (- w 7) (- w 3) title outline))
        ;; Lit from the top left: white on the outer line there, on the
        ;; inner one along the bottom and right; shaded likewise.
        (column 1 1 (- h 4) (funcall grey #xFF))
        (row 1 (- w 4) (- h 3) (funcall grey #x99))
        (column (- w 3) 2 (- h 3) (funcall grey #x99))
        (column 4 (1+ title) (- h 7) (funcall grey #x99))
        (column (- w 6) (1+ title) (- h 6) (funcall grey #xFF))
        (row 4 (- w 6) (- h 6) (funcall grey #xFF))
        ;; The content's black line.
        (column 5 title (- h 7) outline)
        (column (- w 7) title (- h 7) outline)
        (row 5 (- w 7) (- h 7) outline)
        ;; Beside the title bar, the bar's own lines.
        (row 1 5 1 (funcall grey #xFF))
        (row (- w 7) (- w 4) 1 (funcall grey #xFF))
        (row 2 5 (1- title) (funcall grey #x99))
        (row (- w 7) (- w 4) (1- title) (funcall grey #x99))
        (row 1 5 title outline)
        (row (- w 7) (- w 3) title outline))
      ;; The outline.
      (row 0 right 0 outline)
      (row 0 right bottom outline)
      (column 0 0 bottom outline)
      (column right 0 bottom outline))
    svg))

;;;;; The Control Strip

(defconst lispm-desktop-platinum--strip-arrow
  '("    l   " "   ll   " "  ljllll" " ljjjjjl" "ljjjjjjl" "ljjjjjjl" " ljjjjjl" "  ljllll"
    "   ll   " "    l   ")
  "The Control Strip's left arrow, a row of pixels a string: l its outline,
j its fill.  The right arrow is its mirror.")

(defconst lispm-desktop-platinum--strip-collapse
  '("llllllll" "lccccccc" "lcrrrrqb" "lcrrrrqb" "lcrrrrqb" "lcrrrrqb" "lcqqqqqb" "lcbbbbbb")
  "The glyph of the Control Strip's collapse box, a row of pixels a string.")

(defconst lispm-desktop-platinum--strip-tab
  '("#########........" "#fffffff##......." "#fwwwwwww##......" "#fwfffffff##....."
    "#fwwfffffff##...." "#fwfgfffffff##..." "#fwfffwffffff##.." "#fwffffgff##ff##."
    "#fwwffffff#w#ff##" "#fwfgfffff#ww#f8#" "#fwfffwfff#ww#f8#" "#fwffffgff#ww#f8#"
    "#fwwffffff#ww#f8#" "#fwfgfffff#ww#f8#" "#fwfffwfff#ww#f8#" "#fwffffgff#w#ff##"
    "#fwwffffff##ff##." "#fwfgffffffff##.." "#fwfffffffff##..." "#fwffffffff##...."
    "#fwfffffff##....." "#f8888888##......" "#8444444##......." "#########........")
  "The Control Strip's tab, a row of pixels a string: its outline (#), a
platinum face (f) lit along its top and left (w), dotted with a grip
(w over g), shaded along its bottom (8, 4), and a pointed end holding a
handle.")

(defconst lispm-desktop-platinum--strip-tab-right
  '("........#########" ".......##fffffff#" "......##fwwwwww8#" ".....##fwffffff8#"
    "....##fwfffwfff8#" "...##fwfffffgff8#" "..##fwffwffffff8#" ".##fw##ffgfffff8#"
    "##fw#w#ffffwfff8#" "#fw#ww#fffffgff8#" "#fw#ww#fwffffff8#" "#fw#ww#ffgfffff8#"
    "#fw#ww#ffffwfff8#" "#fw#ww#fffffgff8#" "#ff#ww#fwffffff8#" "##ff#w#ffgfffff8#"
    ".##ff##ffffwfff8#" "..##ffffffffgff8#" "...##ffffffffff8#" "....##fffffffff8#"
    ".....##ffffffff8#" "......##88888884#" ".......##4444444#" "........#########")
  "The Control Strip's tab on the screen's right edge, where it is the
strip's left end, read off Mac OS 9.0.4 as `lispm-desktop-platinum--strip-tab'
was.  Its outline is that tab's mirror, its light is not: still lit along
the top and left (w) and shaded along the right and bottom (8, 4), and the
grip's dots fall where Mac OS 9 put them.")

(defun lispm-desktop-platinum-strip-segment-width (kind)
  "Return the width in pixels of a Control Strip segment of KIND, see
`lispm-desktop-platinum-draw-strip-segment'."
  (pcase kind
    ('collapse 15)
    ((or 'left 'right) 14)
    ('module 28)
    ('tab 17)))

(cl-defun lispm-desktop-platinum-draw-strip-segment (palette kind &key state icon menu focused behind side)
  "Return an SVG of a segment of the Control Strip of KIND, in PALETTE.
KIND is `collapse', the box at the strip's left end, which also draws the
strip's left outline; `left' or `right', the arrows scrolling the
modules; `module', a module showing ICON, a sixteen-pixel SVG file or
SVG drawing, and a small triangle when it has a MENU; or `tab', the
strip's right end.  Every segment but the tab ends in the black line
parting it from the next.  STATE is nil, `pressed' (darkened, lit from
below) or `disabled' (sunk, its arrow greyed); FOCUSED rings a module
in the focus colour, as the keyboard reaches it.  BEHIND is the colour
round the tab's pointed end, what the strip lies on.  SIDE `right' draws
the segment of a strip on the screen's right edge, where Mac OS 9 put the
tab at its left end and the collapse box at its right end, that box then
drawing the strip's right outline.  The segment is
`lispm-desktop-platinum-strip-height' tall, its outline included, and
drawn in the strip's own greys, :strip and its light and shades."
  (let* ((h lispm-desktop-platinum-strip-height)
         (w (lispm-desktop-platinum-strip-segment-width kind))
         (svg (svg-create w h))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
         (outline (plist-get palette :outline))
         (face (plist-get palette :strip))
         (light (plist-get palette :strip-light))
         (shade (plist-get palette :strip-shade))
         (dark (plist-get palette :strip-dark))
         ;; Arrows and triangles black on Platinum's greys; on a dark
         ;; theme's, its text colour.
         (ink (lispm-desktop-platinum-legible face outline (plist-get palette :text))))
    (if (eq kind 'tab)
        (progn
          (when behind
            (lispm-desktop-platinum-svg-fill svg 0 0 w h behind))
          (lispm-desktop-platinum-svg-pixel-map
           svg 0 0 (if (eq side 'right) lispm-desktop-platinum--strip-tab-right
                     lispm-desktop-platinum--strip-tab)
         `((?# . ,outline) (?f . ,(funcall grey #xCC)) (?w . ,light)
           (?g . ,(funcall grey #x55)) (?8 . ,(funcall grey #x88)) (?4 . ,(funcall grey #x44)))))
      (let* ((outer (and (eq kind 'collapse) (if (eq side 'right) 'right 'left)))
             (left (if (eq outer 'left) 1 0))
             (right (- w (if (eq outer 'right) 3 2)))
             (pressed (eq state 'pressed))
             (sunk (or pressed (eq state 'disabled))))
        (lispm-desktop-platinum-svg-fill svg 0 0 w h outline)
        (lispm-desktop-platinum-svg-fill svg left 1 (- right left -1) (- h 2) (if pressed dark face))
        (cl-flet ((band (top-left bottom-right width)
                    (dotimes (i width)
                      (lispm-desktop-platinum-svg-row svg (+ left i) (- right i) (+ 1 i) (nth i top-left))
                      (lispm-desktop-platinum-svg-column svg (+ left i) (+ 1 i) (- h 2 i) (nth i top-left))
                      (lispm-desktop-platinum-svg-row svg (+ left i 1) (- right i) (- h 2 i)
                                                   (nth i bottom-right))
                      (lispm-desktop-platinum-svg-column svg (- right i) (+ 1 i) (- h 2 i)
                                                      (nth i bottom-right)))))
          (cond (pressed
                 (let ((deep (lispm-desktop-platinum-blend dark outline 0.5)))
                   (band (list deep deep) (list face face) 2)))
                (sunk (band (list dark) (list light) 1))
                (t (band (list face light) (list dark shade) 2))))
        (pcase kind
          ('collapse
           (lispm-desktop-platinum-svg-pixel-map
            svg (+ left 2) 8 lispm-desktop-platinum--strip-collapse
            `((?l . ,(funcall grey #x55)) (?c . ,(plist-get palette :thumb-light))
              (?r . ,(funcall grey #x88)) (?q . ,(funcall grey #x44)) (?b . ,light))))
          ((or 'left 'right)
           (let ((colors (if (eq state 'disabled)
                             `((?l . ,(funcall grey #x55)) (?j . ,(funcall grey #xDD)))
                           `((?l . ,ink) (?j . ,ink)))))
             (lispm-desktop-platinum-svg-pixel-map
              svg 2 7 (if (eq kind 'left) lispm-desktop-platinum--strip-arrow
                        (mapcar #'reverse lispm-desktop-platinum--strip-arrow))
              colors)))
          ('module
           (when focused
             (dotimes (i 2)
               (lispm-desktop-platinum-svg-row svg (+ left i) (- right i) (+ 1 i) (plist-get palette :focus))
               (lispm-desktop-platinum-svg-row svg (+ left i) (- right i) (- h 2 i) (plist-get palette :focus))
               (lispm-desktop-platinum-svg-column svg (+ left i) (+ 1 i) (- h 2 i) (plist-get palette :focus))
               (lispm-desktop-platinum-svg-column svg (- right i) (+ 1 i) (- h 2 i) (plist-get palette :focus))))
           (cond ((stringp icon) (lispm-desktop-platinum-svg-icon svg icon 2 3 16))
                 (icon (lispm-desktop-platinum-svg-embed svg icon 2 3 16 16)))
           (when menu
             (lispm-desktop-platinum-svg-pixel-map
              svg 21 8 '("#" "##" "###" "####" "####" "###" "##" "#")
              `((?# . ,ink))))))))
    svg))

(defconst lispm-desktop-platinum-tab-padding 13
  "Pixels between a tab's sides, where they meet the pane, and its label.")

(defun lispm-desktop-platinum-bevel-button (label &optional state height face)
  "Return a bevel button reading LABEL, as a string displaying it.
LABEL is shown as Emacs would show it in FACE, by default the list's,
its icons and images included; STATE is nil, `pressed' or `disabled';
HEIGHT defaults to 20 pixels.  See `lispm-desktop-platinum-draw-bevel-button'."
  (let* ((face (or face 'lispm-desktop-platinum-list))
         (height (or height 20))
         (width (+ (lispm-desktop-platinum-content-width label face)
                   (* 2 lispm-desktop-platinum-bevel-padding))))
    (propertize " " 'display
                (lispm-desktop-platinum-image
                 (list 'bevel (lispm-desktop-platinum--content-key label) state height face width)
                 (lambda ()
                   (lispm-desktop-platinum-draw-bevel-button lispm-desktop-platinum-current-palette width height
                                                             :state state :label label :face face))))))

(cl-defun lispm-desktop-platinum-push-button (label &key state default focused pad)
  "Return a push button reading LABEL, as a string displaying it.
STATE, DEFAULT, FOCUSED and PAD are as `lispm-desktop-platinum-draw-push-button'
takes them; PAD defaults to two pixels, room for the focus ring, so a
button keeps its place as the keyboard comes and goes."
  (let ((width (lispm-desktop-platinum-push-button-width label))
        (pad (or pad (if default 5 2))))
    (propertize " " 'display
                (lispm-desktop-platinum-image
                 (list 'push label width state default focused pad)
                 (lambda ()
                   (lispm-desktop-platinum-draw-push-button lispm-desktop-platinum-current-palette label width
                                                            :state state :default default
                                                            :focused focused :pad pad))))))

(defun lispm-desktop-platinum--tab-edge (width)
  "Return the pane's edge WIDTH pixels wide, as a string displaying it."
  (propertize " " 'display
              (lispm-desktop-platinum-image
               (list 'tab-edge width)
               (lambda () (lispm-desktop-platinum-draw-tab-edge lispm-desktop-platinum-current-palette width)))))

(defun lispm-desktop-platinum-tabs (tabs width &optional face)
  "Return a row of TABS standing on their pane's edge, WIDTH pixels wide.
Each tab is (LABEL FRONT . PROPERTIES): LABEL shown as Emacs would show
it in FACE, by default the system font's; FRONT non-nil for the tab in
front; PROPERTIES, text properties the tab's string carries, such as its
keymap and help.  The row opens with a short run of the pane's edge and
runs it on to WIDTH, as Mac OS 9 drew a tab control along its pane."
  (let* ((face (or face 'lispm-desktop-platinum-title))
         (lead 8)
         (used lead)
         (parts
          (mapcar (pcase-lambda (`(,label ,front . ,properties))
                    (let ((tab-width (+ (lispm-desktop-platinum-content-width label face)
                                        (* 2 lispm-desktop-platinum-tab-padding))))
                      (setq used (+ used tab-width 2))
                      (concat (apply #'propertize " " 'display
                                     (lispm-desktop-platinum-image
                                      (list 'tab (lispm-desktop-platinum--content-key label) front face tab-width)
                                      (lambda ()
                                        (lispm-desktop-platinum-draw-tab lispm-desktop-platinum-current-palette
                                                                         label tab-width :front front
                                                                         :face face)))
                                     properties)
                              (lispm-desktop-platinum--tab-edge 2))))
                  tabs)))
    (concat (lispm-desktop-platinum--tab-edge lead)
            (apply #'concat parts)
            (when (> width used)
              (lispm-desktop-platinum--tab-edge (- width used))))))

(defun lispm-desktop-platinum-radio (on &optional state focused)
  "Return a radio button, ON or off, in STATE, as a string displaying it.
FOCUSED rings it; either way it leaves room for the ring, so it keeps its
place as the keyboard comes and goes.  See `lispm-desktop-platinum-draw-radio'."
  (propertize " " 'display
              (lispm-desktop-platinum-image
               (list 'radio on state focused)
               (lambda () (lispm-desktop-platinum-draw-radio lispm-desktop-platinum-current-palette on
                                                             :state state :focused focused :pad 2)))))

(defconst lispm-desktop-platinum--list-check-boxes
  '((on "############" "#wwwwwwwwwd#" "#wd#dddd#dg#" "#wd##dd##sg#" "#wdd####sag#"
        "#wddd##sadg#" "#wdd####ddg#" "#wd##sa##dg#" "#wd#sadd#sg#" "#wddaddddag#"
        "#dggggggggg#" "############")
    (off "############" "#wwwwwwwwwd#" "#wddddddddg#" "#wddddddddg#" "#wddddddddg#"
         "#wddddddddg#" "#wddddddddg#" "#wddddddddg#" "#wddddddddg#" "#wddddddddg#"
         "#dggggggggg#" "############")
    (mixed "############" "#wwwwwwwwwd#" "#wddddddddg#" "#wddddddddg#" "#wddddddddg#"
           "#wd######ag#" "#wd######ag#" "#wddaaaaaag#" "#wddddddddg#" "#wddddddddg#"
           "#dggggggggg#" "############"))
  "The check boxes of Extensions Manager's list, as Mac OS 9 drew them, a row
of pixels a string: checked with a cross, empty, or crossed by a bar for
a group only some of whose items are on.  # is the outline, w, d, a, g
and s the greys #FF, #DD, #AA, #88 and #77.")

(defun lispm-desktop-platinum-draw-list-check-box (palette state)
  "Return an SVG of a list's check box in PALETTE, twelve pixels square.
STATE is `on', `off' or `mixed', see `lispm-desktop-platinum--list-check-boxes'."
  (let ((svg (svg-create 12 12)))
    (lispm-desktop-platinum-svg-pixel-map
     svg 0 0 (alist-get state lispm-desktop-platinum--list-check-boxes)
     `((?# . ,(plist-get palette :outline))
       ,@(mapcar (pcase-lambda (`(,char . ,level)) (cons char (lispm-desktop-platinum-grey palette level)))
                 '((?w . #xFF) (?d . #xDD) (?a . #xAA) (?g . #x88) (?s . #x77)))))
    svg))

(defun lispm-desktop-platinum-list-check-box (state)
  "Return a list's check box in STATE as a string displaying it.
See `lispm-desktop-platinum-draw-list-check-box'."
  (propertize " " 'display
              (lispm-desktop-platinum-image
               (list 'list-check-box state)
               (lambda () (lispm-desktop-platinum-draw-list-check-box lispm-desktop-platinum-current-palette state)))))

(defun lispm-desktop-platinum-check-box (on &optional state focused pad)
  "Return a check box, checked when ON, in STATE, as a string displaying it.
FOCUSED rings it, and PAD leaves room about it; see
`lispm-desktop-platinum-draw-check-box'."
  (propertize " " 'display
              (lispm-desktop-platinum-image
               (list 'check-box on state focused pad)
               (lambda () (lispm-desktop-platinum-draw-check-box lispm-desktop-platinum-current-palette on
                                                                 :state state :focused focused
                                                                 :pad (or pad 0))))))

;;;; Controls in buffers
;;
;; Push buttons and radio buttons inserted in a buffer, as Control Panels
;; holds them, are buttons of button.el: Tab and Shift-Tab go from one to
;; the next, Return presses the one point is on.  Point itself is not
;; shown in such a buffer, so the control it is on is drawn ringed in the
;; focus colour, as Mac OS 9 ringed the control the keyboard was on; a
;; push button pressed, from the keyboard or with the mouse, is drawn
;; pushed in until it is let go.

(defvar-local lispm-desktop-platinum-focus nil
  "Position of the control drawn focused in this buffer, or nil.")

(defun lispm-desktop-platinum--swap-image (position image)
  "Show IMAGE for the control at POSITION of the current buffer."
  (with-silent-modifications
    (let ((inhibit-read-only t))
      (put-text-property position (1+ position) 'display image))))

(defun lispm-desktop-platinum-show-focus ()
  "Draw the control at point focused, and the one focused before plain.
Controls carry their images as the text properties
`lispm-desktop-platinum-plain' and `lispm-desktop-platinum-focused'.
Buffers holding controls run this from `post-command-hook', and after
drawing their controls again."
  (let ((here (and (get-text-property (point) 'lispm-desktop-platinum-focused) (point)))
        (before lispm-desktop-platinum-focus))
    (unless (eql here before)
      (when (and before (< before (point-max))
                 (get-text-property before 'lispm-desktop-platinum-plain))
        (lispm-desktop-platinum--swap-image before (get-text-property before 'lispm-desktop-platinum-plain)))
      (when here
        (lispm-desktop-platinum--swap-image here (get-text-property here 'lispm-desktop-platinum-focused)))
      (setq lispm-desktop-platinum-focus here))))

(defun lispm-desktop-platinum--press-at (position)
  "Draw the push button at POSITION pushed in, until it is let go.
From the mouse, it is let go when the button rises, and pressed only if
it rises over the push button, as on Mac OS 9; from the keyboard, it
blinks.  Return non-nil when the button was pressed."
  (let ((pressed (get-text-property position 'lispm-desktop-platinum-pressed))
        (shown (get-text-property position 'display)))
    (lispm-desktop-platinum--swap-image position pressed)
    (redisplay t)
    (prog1 (if (not (mouse-event-p last-input-event))
               (progn (sit-for (alist-get 'button lispm-desktop-modal-flashes)) t)
             (let ((release (let ((echo-keystrokes 0)) (read-event))))
               (and (mouse-event-p release)
                    (eq (posn-window (event-end release)) (posn-window (event-start last-input-event)))
                    (eql (posn-point (event-end release)) position))))
      (lispm-desktop-platinum--swap-image position shown))))

(defun lispm-desktop-platinum-push (&optional position)
  "Press the push button at POSITION, by default point's, as Return and
Space do, or as a press of the mouse does; other buttons are pushed as
`push-button' pushes them."
  (interactive)
  (let ((position (or position (point))))
    (if (and (get-text-property position 'lispm-desktop-platinum-pressed)
             (not (lispm-desktop-platinum--press-at position)))
        nil
      (push-button position))))

(defun lispm-desktop-platinum--mouse-push (event)
  "Press the push button EVENT went down on, see `lispm-desktop-platinum-push'."
  (interactive "e")
  (let ((position (event-start event)))
    (with-selected-window (posn-window position)
      (lispm-desktop-platinum-push (posn-point position)))))

(defvar-keymap lispm-desktop-platinum-push-button-map
  :doc "Keymap of a push button in a buffer."
  :parent button-map
  "RET" #'lispm-desktop-platinum-push
  "SPC" #'lispm-desktop-platinum-push
  "<down-mouse-1>" #'lispm-desktop-platinum--mouse-push
  "<mouse-1>" #'ignore)

(defun lispm-desktop-platinum-image-of (control)
  "Return the image CONTROL, a string displaying one, shows."
  (get-text-property 0 'display control))

(cl-defun lispm-desktop-platinum-insert-push-button (label action &key help default disabled)
  "Insert a push button reading LABEL, which calls ACTION with no arguments.
HELP describes it; DEFAULT rings it, as the button Return presses.  The
button is drawn focused while point is on it, see
`lispm-desktop-platinum-show-focus'; its text, drawn in its image, is
kept in its `lispm-desktop-platinum-label' property.  A DISABLED button
is drawn dimmed and does nothing; Tab passes it by."
  (if disabled
      (insert (propertize " " 'display (lispm-desktop-platinum-image-of
                                        (lispm-desktop-platinum-push-button label :state 'disabled))
                          'lispm-desktop-platinum-label label 'face nil 'help-echo (or help label)))
    (lispm-desktop-platinum--insert-push-button label action help default)))

(defun lispm-desktop-platinum--insert-push-button (label action help default)
  "Insert an enabled push button, see `lispm-desktop-platinum-insert-push-button'."
  (insert-text-button " "
                      'display (lispm-desktop-platinum-image-of
                                (lispm-desktop-platinum-push-button label :default default))
                      'lispm-desktop-platinum-plain (lispm-desktop-platinum-image-of
                                                     (lispm-desktop-platinum-push-button label :default default))
                      'lispm-desktop-platinum-focused (lispm-desktop-platinum-image-of
                                                       (lispm-desktop-platinum-push-button
                                                        label :default default :focused t))
                      'lispm-desktop-platinum-pressed (lispm-desktop-platinum-image-of
                                                       (lispm-desktop-platinum-push-button
                                                        label :default default :state 'pressed))
                      'action (lambda (_) (funcall action))
                      'lispm-desktop-platinum-label label
                      'face nil 'keymap lispm-desktop-platinum-push-button-map
                      'help-echo (or help label)))

(provide 'lispm-desktop-platinum-controls)
;;; lispm-desktop-platinum-controls.el ends here
