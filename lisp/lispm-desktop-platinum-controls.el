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
;; header buttons, pop-up menus, tabs, placards, radio buttons and check
;; boxes, group boxes, window frames and the Control Strip's segments;
;; and the same controls as text, and as buttons in buffers, which any
;; window of the desktop's may hold: push buttons, radio buttons, check
;; boxes, bevel buttons holding an icon and pop-up menus (see "Controls in
;; buffers" below).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-platinum-draw)
(require 'lispm-desktop-platinum-icons)
(require 'lispm-ui-modal)
(require 'lispm-desktop-i18n)

(declare-function lispm-desktop-platinum-fit "lispm-desktop-platinum-titlebar" (string width))
(declare-function lispm-desktop-popup-available-p "lispm-desktop-popup" ())
(declare-function lispm-desktop-popup-choose "lispm-desktop-popup" (items &optional x y align))


;;;; Header strips, header buttons and group boxes


(defconst lispm-desktop-platinum-header-strip-height 22
  "Height in pixels of a window's header strip, as the Finder window's.")

(cl-defgeneric lispm-desktop-platinum--draw-header-ground (svg width palette)
  "Draw on SVG the ground of a header strip WIDTH pixels wide, in PALETTE:
lit along its top, platinum, shaded along its bottom over a black line.
The strip's opening, `lispm-desktop-platinum-draw-header-button', stands
on it too."
  (let ((h lispm-desktop-platinum-header-strip-height)
        (grey (lambda (level) (lispm-ui-grey palette level))))
    (lispm-ui-svg-fill svg 0 0 width h (funcall grey #xDD))
    (lispm-ui-svg-row svg 0 (1- width) 0 (funcall grey #xFF))
    (lispm-ui-svg-row svg 0 (1- width) (- h 2) (funcall grey #xAA))
    (lispm-ui-svg-row svg 0 (1- width) (1- h) (plist-get palette :outline))))

(cl-defgeneric lispm-desktop-platinum-draw-header-strip (palette width &optional label left)
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
      (let ((x (- (/ (- (+ left width) (lispm-ui-text-width
                                         label 'lispm-ui-small))
                     2)
                  left)))
        (lispm-ui-svg-content
         svg label (if (> left 0) (max 8 x) x)
         1 (- h 3) (plist-get palette :text) 'lispm-ui-small)))
    svg))

(defun lispm-desktop-platinum-header-strip (label &optional left)
  "Return the header strip of the window being drawn, showing LABEL.
It is a string to put in the window's header line; see
`lispm-desktop-platinum-draw-header-strip'.  LEFT is the width of the
strip's opening put before it, as `lispm-desktop-platinum-header-button'
returns one."
  (let* ((left (or left 0))
         (width (- (window-pixel-width) (window-right-divider-width) (window-scroll-bar-width) left)))
    (if (lispm-desktop-tty-p)
        ;; A window with another on its right has the border between them.
        (let* ((width (- width (if (window-at-side-p nil 'right) 0 1)))
               (label (truncate-string-to-width (or label "") (max 0 (- width 2)) nil nil "…"))
               (before (max 1 (- (/ (- (+ left width) (string-width label)) 2) left))))
          (propertize (truncate-string-to-width
                       (concat (make-string before ?\s) label (make-string width ?\s)) width)
                      'face 'lispm-desktop-platinum-toolbar))
      (propertize " " 'display
                  (lispm-ui-image
                   (list 'header-strip label width left)
                   (lambda () (lispm-desktop-platinum-draw-header-strip lispm-ui-current-palette
                                                                        width label left)))))))

(defconst lispm-desktop-platinum-header-button-height 17
  "Height in pixels of a bevel button opening a header strip.")

(defun lispm-desktop-platinum-header-button-width (label)
  "Return the width of a header strip's opening holding a button reading LABEL."
  (if (lispm-desktop-tty-p)
      (string-width (lispm-desktop-platinum-header-button label))
    (+ 6 (lispm-ui-text-width label 'lispm-ui-small)
       (* 2 lispm-desktop-platinum-bevel-padding))))

(cl-defgeneric lispm-desktop-platinum-draw-header-button (palette label &optional state)
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
                                               :face 'lispm-ui-small)
    svg))

(defun lispm-desktop-platinum-header-button (label &optional state)
  "Return a header strip's opening, a bevel button reading LABEL in STATE,
as a string displaying it; see `lispm-desktop-platinum-draw-header-button'.
On a text terminal it is a button of text a cell in."
  (if (lispm-desktop-tty-p)
      (concat (propertize " " 'face 'lispm-desktop-platinum-toolbar)
              (lispm-desktop-platinum-text-button label state))
    (propertize " " 'display
                (lispm-ui-image
                 (list 'header-button (lispm-ui-content-key label) state)
                 (lambda () (lispm-desktop-platinum-draw-header-button lispm-ui-current-palette
                                                                       label state))))))

(defconst lispm-desktop-platinum-group-title-height 18
  "Height in pixels of a group box's top edge, which carries its title.")

(cl-defgeneric lispm-desktop-platinum-draw-group-edge (palette width &optional title bottom)
  "Return an SVG of a group box's top edge WIDTH pixels wide, in PALETTE.
Mac OS 9 framed a group of controls in an engraved line, a grey line
with a lit one along it, TITLE breaking the top line near its left end
in the system font.  BOTTOM draws the bottom edge instead, closing the
frame's sides at the top of a line as tall as a line of text."
  (let* ((h (if bottom (default-line-height) lispm-desktop-platinum-group-title-height))
         (y (if bottom 1 (/ h 2)))
         (svg (svg-create width h))
         (dark (lispm-ui-grey palette #x88))
         (lit (lispm-ui-grey palette #xFF)))
    (if bottom
        (progn
          (lispm-ui-svg-column svg 0 0 y dark)
          (lispm-ui-svg-column svg 1 0 (1- y) lit)
          (lispm-ui-svg-column svg (- width 2) 0 y dark)
          (lispm-ui-svg-column svg (1- width) 0 (1+ y) lit))
      (lispm-ui-svg-column svg 0 y (1- h) dark)
      (lispm-ui-svg-column svg 1 (1+ y) (1- h) lit)
      (lispm-ui-svg-column svg (- width 2) y (1- h) dark)
      (lispm-ui-svg-column svg (1- width) y (1- h) lit))
    (lispm-ui-svg-row svg 0 (- width 2) y dark)
    (lispm-ui-svg-row svg 1 (1- width) (1+ y) lit)
    (when title
      (let ((title-width (lispm-ui-text-width title 'lispm-ui-title)))
        (lispm-ui-svg-fill svg 8 0 (+ title-width 8) h (lispm-ui-grey palette #xEE))
        (lispm-ui-svg-content svg title 12 0 h (plist-get palette :text)
                                              'lispm-ui-title)))
    svg))

(defun lispm-desktop-platinum-group-edge (width &optional title bottom)
  "Return a group box's edge WIDTH pixels wide as a string displaying it.
See `lispm-desktop-platinum-draw-group-edge'.  On a text terminal it is a
line drawn in text, WIDTH cells long, TITLE breaking it near its left end."
  (if (lispm-desktop-tty-p)
      (let ((rule `(:foreground ,(lispm-ui-color :muted))))
        (if bottom
            (propertize (concat "└" (make-string (max 0 (- width 2)) ?─) "┘") 'face rule)
          (let ((title (if title (concat " " title " ") "")))
            (concat (propertize "┌─" 'face rule)
                    (propertize title 'face 'lispm-ui-title)
                    (propertize (concat (make-string (max 0 (- width 3 (string-width title))) ?─) "┐")
                                'face rule)))))
   (propertize " " 'display
              (lispm-ui-image
               (list 'group-edge width title bottom)
               (lambda () (lispm-desktop-platinum-draw-group-edge lispm-ui-current-palette
                                                                  width title bottom))))))

(cl-defgeneric lispm-desktop-platinum-draw-group-side (palette height)
  "Return an SVG of a group box's side for a line HEIGHT pixels tall, in
PALETTE: the grey line and the lit one beside it, two pixels wide."
  (let ((svg (svg-create 2 height)))
    (lispm-ui-svg-fill svg 0 0 1 height (lispm-ui-grey palette #x88))
    (lispm-ui-svg-fill svg 1 0 1 height (lispm-ui-grey palette #xFF))
    svg))

(defun lispm-desktop-platinum-group-side (height)
  "Return a group box's side for a line HEIGHT pixels tall, as a string
displaying it; see `lispm-desktop-platinum-draw-group-side'.  On a text
terminal it is a line drawn in text."
  (if (lispm-desktop-tty-p)
      (propertize "│" 'face `(:foreground ,(lispm-ui-color :muted)))
    (propertize " " 'display
                (lispm-ui-image
                 (list 'group-side height)
                 (lambda () (lispm-desktop-platinum-draw-group-side lispm-ui-current-palette
                                                                    height))))))

;;;; Controls (pure)
;;
;; Mac OS 9's controls as its Appearance Manager drew them in Platinum,
;; measured pixel by pixel on Mac OS 9.0.4: push buttons and the ring of
;; the default one, bevel buttons, tabs, placards, radio buttons, check
;; boxes, focus rings, window frames, scroll bars and the Control Strip's
;; segments.  Each draws with `lispm-ui-grey', so Platinum's
;; greys come out as themselves and another theme gets their
;; counterparts.  Labels are SVG text in the system font, as heavy as the
;; titles and menus Emacs draws (`lispm-ui-svg-font').

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
          (lispm-ui-svg-dot svg (+ x dx) (+ y dy) color)))
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
  (cl-flet ((g (level) (lispm-ui-grey palette level)))
    (pcase state
      ('pressed
       (list :outline (plist-get palette :outline) :edge (g #x22)
             :lit (g #x44) :lit-inner (g #x55) :face (g #x66)
             :shade-inner (g #x77) :shade (g #x88)
             :corner (g #x44) :corner-low (g #x77) :corner-inner (g #x44)
             :label (lispm-ui-legible (g #x66) (plist-get palette :text) (g #xFF))))
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
                (lispm-ui-svg-dot svg (+ x dx) (+ y dy) (plist-get colors part)))
              (row (from to dy part)
                (lispm-ui-svg-row svg (+ x from) (+ x to) (+ y dy) (plist-get colors part)))
              (column (dx from to part)
                (lispm-ui-svg-column svg (+ x dx) (+ y from) (+ y to) (plist-get colors part))))
      (lispm-ui-svg-fill svg (+ x 1) (+ y 1) (- w 2) (- h 2) (plist-get colors :face))
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
         (grey (lambda (level) (lispm-ui-grey palette level)))
         (outline (if disabled (funcall grey #x88) (plist-get palette :outline)))
         (edge (if disabled outline (funcall grey #x22)))
         (lit (funcall grey #xDD))
         (lit-inner (if disabled lit (funcall grey #xAA)))
         (shade-inner (if disabled lit (funcall grey #xAA)))
         (shade (if disabled lit (funcall grey #x77)))
         (meet (if disabled lit (funcall grey #x88))))
    (cl-flet ((dot (dx dy color) (lispm-ui-svg-dot svg (+ x dx) (+ y dy) color))
              (row (from to dy color)
                (lispm-ui-svg-row svg (+ x from) (+ x to) (+ y dy) color))
              (column (dx from to color)
                (lispm-ui-svg-column svg (+ x dx) (+ y from) (+ y to) color)))
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

(cl-defgeneric lispm-desktop-platinum-svg-focus-ring (svg x y width height radius palette)
  "Draw on SVG the keyboard focus ring about the WIDTH by HEIGHT box at X, Y.
The ring is two pixels in PALETTE's focus colour just outside the box,
its corners rounded by RADIUS, as Mac OS 9 ringed the control the
keyboard was on."
  (svg-rectangle svg (- x 1) (- y 1) (+ width 2) (+ height 2)
                 :rx (+ radius 1) :fill "none"
                 :stroke (plist-get palette :focus) :stroke-width 2))

(cl-defgeneric lispm-desktop-platinum-push-button-width (label)
  "Return the width of a push button reading LABEL: room for it, at least 60.
LABEL is measured in the titles' face, the system font."
  (max 60 (+ 24 (string-pixel-width (propertize label 'face 'lispm-ui-title)))))

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
`lispm-ui-svg-font', by default the system font's."
  (let* ((ring (if default 3 0))
         (pad (max pad (+ ring (if focused 2 0))))
         (h lispm-desktop-platinum-button-height)
         (svg (svg-create (+ width (* 2 pad)) (+ h (* 2 pad)))))
    (lispm-desktop-platinum-svg-push-button svg pad pad palette label width
                                              :state state :default default :focused focused :font font)
    svg))

(cl-defgeneric lispm-desktop-platinum-svg-push-button (svg x y palette label width
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
    (lispm-ui-svg-label svg label (+ x (/ width 2.0)) (+ y 14)
                        (plist-get colors :label)
                        (or font (lispm-ui-svg-font)))))

;;;;; Pop-up menus

(defun lispm-desktop-platinum-popup-button-width (label)
  "Return the width of a pop-up menu showing LABEL: room for it and its
arrows, at least 80.  LABEL is measured in the titles' face."
  (max 80 (+ 40 (string-pixel-width (propertize label 'face 'lispm-ui-title)))))

(cl-defgeneric lispm-desktop-platinum-svg-popup-arrows (svg x y palette color)
  "Draw on SVG the arrows of a pop-up menu in the 12 by 20 pixel box at X, Y,
in COLOR, PALETTE's: a triangle up over a triangle down, as Mac OS 9 drew
them, for a menu that opens about its current item."
  (ignore palette)
  (svg-polygon svg (list (cons (+ x 2) (+ y 8)) (cons (+ x 10) (+ y 8)) (cons (+ x 6) (+ y 4)))
               :fill color)
  (svg-polygon svg (list (cons (+ x 2) (+ y 11)) (cons (+ x 10) (+ y 11)) (cons (+ x 6) (+ y 15)))
               :fill color))

(cl-defun lispm-desktop-platinum-draw-popup-button (palette label width &key state focused (pad 0))
  "Return an SVG of a pop-up menu WIDTH pixels wide showing LABEL, in PALETTE.
It is a push button, as the look draws one, with LABEL on its left and
the menu's arrows on its right, past a line.  STATE is nil, `pressed' or
`disabled'; FOCUSED rings it in the focus colour; PAD is the room left
about it, raised to what the ring needs."
  (let* ((pad (max pad (if focused 2 0)))
         (h lispm-desktop-platinum-button-height)
         (svg (svg-create (+ width (* 2 pad)) (+ h (* 2 pad))))
         (colors (lispm-desktop-platinum--push-button-colors palette state)))
    (lispm-desktop-platinum-svg-push-button svg pad pad palette "" width :state state :focused focused)
    (lispm-ui-svg-label svg label (+ pad 10) (+ pad 14) (plist-get colors :label)
                        (lispm-ui-svg-font) "start")
    (svg-line svg (+ pad width -21.5) (+ pad 4) (+ pad width -21.5) (+ pad h -4)
              :stroke (plist-get colors :shade))
    (lispm-desktop-platinum-svg-popup-arrows svg (+ pad width -17) pad palette (plist-get colors :label))
    svg))

(cl-defun lispm-desktop-platinum-popup-button (label &key state focused pad)
  "Return a pop-up menu showing LABEL, as a string displaying it.
STATE, FOCUSED and PAD are as `lispm-desktop-platinum-draw-popup-button'
takes them; PAD defaults to two pixels, room for the focus ring.  On a
text terminal it is LABEL and an arrow, between brackets."
  (if (lispm-desktop-tty-p)
      (propertize " " 'display (lispm-desktop-platinum-text-control
                                (lispm-desktop-platinum-text-button (concat " " label " \u25be ") state)
                                focused state))
    (let ((width (lispm-desktop-platinum-popup-button-width label))
          (pad (or pad 2)))
      (propertize " " 'display
                  (lispm-ui-image
                   (list 'popup label width state focused pad)
                   (lambda ()
                     (lispm-desktop-platinum-draw-popup-button lispm-ui-current-palette
                                                               label width :state state
                                                               :focused focused :pad pad)))))))

;;;;; Bevel buttons

(defun lispm-desktop-platinum-bevel-button-colors (palette state)
  "Return the colours of a bevel button's parts in STATE, as a plist.
STATE is nil, `pressed' or `disabled'.  The parts are its outline lit
along the top and left (:outline-lit) and shaded along the bottom and
right (:outline-shade), its two lit lines (:lit, :lit-inner), its :face,
its two shaded lines (:shade-inner, :shade) and its :label."
  (cl-flet ((g (level) (lispm-ui-grey palette level)))
    (pcase state
      ('pressed
       (list :outline-lit (g #x11) :outline-shade (g #x44)
             :lit (g #x44) :lit-inner (g #x66) :face (g #x88)
             :shade-inner (g #x99) :shade (g #xBB)
             :label (lispm-ui-legible (g #x88) (plist-get palette :text) (g #xFF))))
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
                                                            (face 'lispm-ui-list))
  "Return an SVG of a bevel button WIDTH by HEIGHT pixels, in PALETTE.
Bevel buttons are square, as Mac OS 9's toolbars held them: an outline
darker along the bottom and right, two lines lit along the top and left
and two shaded along the bottom and right.  STATE is nil, `pressed'
\(darkened, its bevel turned over) or `disabled' (flat, its outline and
label greyed).  LABEL, when given, is centred: a string as Emacs would
show it in FACE, its icons and images included, see
`lispm-ui-svg-content'."
  (let ((svg (svg-create width height)))
    (lispm-desktop-platinum--draw-bevel-button svg 0 0 palette width height
                                               :state state :label label :face face)
    svg))

(cl-defgeneric lispm-desktop-platinum--draw-bevel-button (svg x y palette width height
                                                              &key state label face)
  "Draw on SVG at X, Y a bevel button WIDTH by HEIGHT pixels, in PALETTE.
See `lispm-desktop-platinum-draw-bevel-button'; FACE is by default the list's."
  (let ((face (or face 'lispm-ui-list))
        (colors (lispm-desktop-platinum-bevel-button-colors palette state))
        (w width)
        (h height))
    (cl-flet ((row (from to line part)
                (lispm-ui-svg-row svg (+ x from) (+ x to) (+ y line) (plist-get colors part)))
              (column (col from to part)
                (lispm-ui-svg-column svg (+ x col) (+ y from) (+ y to) (plist-get colors part))))
      (lispm-ui-svg-fill svg x y w h (plist-get colors :face))
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
      (lispm-ui-svg-content
       svg label (+ x (/ (- w (lispm-ui-text-width label face)) 2)) y h
       (plist-get colors :label) face))))

;;;;; Tabs

(defconst lispm-desktop-platinum--tab-slant
  '((10 . t) (8 . t) (7 . t) (6 . t) (6) (5 . t) (5) (5) (4 . t) (4) (4)
    (3 . t) (3) (3) (2 . t) (2) (2) (1 . t) (1) (1) (0 . t))
  "Where a tab's left side runs, row by row from its top, as (COLUMN . STEP).
COLUMN holds the outline; STEP is non-nil on a row where the outline
steps out, which is softened there.  The right side is the mirror.")

(cl-defgeneric lispm-desktop-platinum-draw-tab (palette label width &key front face)
  "Return an SVG of a tab WIDTH pixels wide reading LABEL, in PALETTE.
Tabs are trapezoids, their sides slanting out a pixel every three rows
from top corners rounded off, and stand on the edge of the pane they
choose among: the SVG is `lispm-desktop-platinum-tab-height' tall, and
three rows more for that edge.  The tab in FRONT is the pane's colour
and opens into it; a tab behind is darker, lit along its top and left,
and the pane's outline runs under it.  LABEL is a string as Emacs would
show it in FACE, by default the system font's, its icons included; every
label is black, none heavier than the others."
  (let* ((face (or face 'lispm-ui-title))
         (h lispm-desktop-platinum-tab-height)
         (w width)
         (svg (svg-create w (+ h 3)))
         (grey (lambda (level) (lispm-ui-grey palette level)))
         (outline (plist-get palette :outline))
         (edge (funcall grey #x22))
         (body (funcall grey (if front #xEE #xCC)))
         (between (funcall grey #xCC))
         (lit (funcall grey (if front #xFF #xDD)))
         (soft (funcall grey (if front #xBB #xAA)))
         (shade (funcall grey #x88))
         (shade-soft (funcall grey #x44))
         (shade-lit (funcall grey (if front #xDD #xCC))))
    (cl-flet ((dot (x y color) (lispm-ui-svg-dot svg x y color))
              (row (from to y color) (lispm-ui-svg-row svg from to y color)))
      (lispm-ui-svg-fill svg 0 0 w h (plist-get palette :chrome))
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
    (lispm-ui-svg-content
     svg label (/ (- w (lispm-ui-text-width label face)) 2) 0 h
     (plist-get palette :text) face)
    svg))

(cl-defgeneric lispm-desktop-platinum-draw-tab-edge (palette width)
  "Return an SVG of the pane's edge WIDTH pixels wide, beside or between tabs.
It is as tall as a tab and the pane's edge under it, see
`lispm-desktop-platinum-draw-tab': the window's body above, the pane's
outline and lit band below, in PALETTE."
  (let* ((h lispm-desktop-platinum-tab-height)
         (svg (svg-create width (+ h 3))))
    (lispm-ui-svg-fill svg 0 0 width h (plist-get palette :chrome))
    (lispm-ui-svg-row svg 0 (1- width) h (plist-get palette :outline))
    (lispm-ui-svg-row svg 0 (1- width) (+ h 1) (lispm-ui-grey palette #xCC))
    (lispm-ui-svg-row svg 0 (1- width) (+ h 2) (lispm-ui-grey palette #xFF))
    svg))

(defconst lispm-desktop-platinum-tab-close-size 11
  "Side in pixels of the cross closing a tab.")

(cl-defgeneric lispm-desktop-platinum-draw-tab-close (palette &key pressed)
  "Return an SVG of the cross closing a tab, in PALETTE.
Mac OS 9's tabs had none, so it is a plain cross in a dark grey,
`lispm-desktop-platinum-tab-close-size' square, on no ground of its own:
the tab's colour shows about it.  PRESSED, it is lit on a grey disc."
  (let* ((s lispm-desktop-platinum-tab-close-size)
         (svg (svg-create s s))
         (from 2)
         (to (- s 2)))
    (when pressed
      (svg-circle svg (/ s 2.0) (/ s 2.0) (/ s 2.0) :fill (lispm-ui-grey palette #x77)))
    (dolist (line `((,from ,from ,to ,to) (,from ,to ,to ,from)))
      (apply #'svg-line svg (append line
                                    (list :stroke (lispm-ui-grey palette (if pressed #xFF #x44))
                                          :stroke-width 1.5 :stroke-linecap "round"))))
    svg))

;;;;; Placards, radio buttons, check boxes

(cl-defgeneric lispm-desktop-platinum-draw-placard (palette width height &key label font state)
  "Return an SVG of a placard WIDTH by HEIGHT pixels, in PALETTE.
A placard is the plate Mac OS 9 showed status on, as the Finder window
counted its items on one: outlined, lit along the top and left, shaded
along the bottom and right.  STATE `inactive' lays it flat in a grey
outline, as on an inactive window.  LABEL, when given, is centred in
FONT, a value of `lispm-ui-svg-font', by default the list's."
  (let ((svg (svg-create width height))
        (inactive (eq state 'inactive))
        (w width)
        (h height))
    (lispm-ui-svg-fill svg 0 0 w h (if inactive
                                       (lispm-ui-grey palette #x99)
                                     (plist-get palette :outline)))
    (lispm-ui-svg-fill svg 1 1 (- w 2) (- h 2) (lispm-ui-grey palette #xDD))
    (unless inactive
      (lispm-ui-svg-row svg 1 (- w 2) 1 (lispm-ui-grey palette #xFF))
      (lispm-ui-svg-column svg 1 1 (- h 2) (lispm-ui-grey palette #xFF))
      (lispm-ui-svg-row svg 2 (- w 2) (- h 2) (lispm-ui-grey palette #xAA))
      (lispm-ui-svg-column svg (- w 2) 2 (- h 2) (lispm-ui-grey palette #xAA)))
    (when label
      (let ((font (or font (lispm-ui-svg-font 'lispm-ui-list))))
        (lispm-ui-svg-label svg label (/ w 2.0)
                            (round (+ (/ h 2.0) (* 0.35 (plist-get font :size))))
                            (plist-get palette (if inactive :muted :text)) font)))
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

(cl-defgeneric lispm-desktop-platinum-draw-radio (palette on &key state focused pad)
  "Return an SVG of a radio button in PALETTE, ON or off.
It is a twelve-pixel sphere, outlined, shaded from white at its top left
to grey at its bottom right, with a dot when ON.  STATE is nil,
`pressed' (darkened) or `disabled' (flat and grey).  FOCUSED rings it in
the focus colour, two pixels out; PAD is the room left about it, raised
to two pixels for the ring."
  (let* ((pad (max (or pad 0) (if focused 2 0)))
         (size (+ 12 (* 2 pad)))
         (middle (/ size 2.0))
         (svg (svg-create size size))
         (grey (lambda (level) (lispm-ui-grey palette level)))
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
                          (lispm-ui-legible
                           (lispm-ui-blend from to 0.5)
                           (plist-get palette :outline) (plist-get palette :text)))))
    svg))

(cl-defgeneric lispm-desktop-platinum-draw-check-box (palette on &key state focused pad)
  "Return an SVG of a check box in PALETTE, checked when ON.
The box is twelve pixels square, outlined, lit along its top and left
and shaded along its bottom and right; the check mark breaks out past
its top right corner, as Mac OS 9's did, so the SVG is fourteen pixels
square, the box at its bottom left.  STATE is nil, `pressed' (darkened)
or `disabled' (flat and grey).  FOCUSED rings the box in the focus
colour, two pixels out; PAD is the room left about it, raised to two
pixels for the ring."
  (let* ((pad (max (or pad 0) (if focused 2 0)))
         (svg (svg-create (+ 14 (* 2 pad)) (+ 14 (* 2 pad))))
         (grey (lambda (level) (lispm-ui-grey palette level)))
         (disabled (eq state 'disabled))
         (pressed (eq state 'pressed))
         (ink (if disabled (funcall grey #x88) (plist-get palette :outline))))
    (when focused
      (lispm-desktop-platinum-svg-focus-ring svg pad (+ pad 2) 12 12 0 palette))
    (lispm-ui-svg-fill svg pad (+ pad 2) 12 12 ink)
    (lispm-ui-svg-fill svg (+ pad 1) (+ pad 3) 10 10 (funcall grey (if pressed #xAA #xDD)))
    (unless disabled
      (lispm-ui-svg-row svg (+ pad 1) (+ pad 9) (+ pad 3) (funcall grey (if pressed #x88 #xFF)))
      (lispm-ui-svg-column svg (+ pad 1) (+ pad 3) (+ pad 11) (funcall grey (if pressed #x88 #xFF)))
      (lispm-ui-svg-row svg (+ pad 2) (+ pad 10) (+ pad 12) (funcall grey (if pressed #xDD #x88)))
      (lispm-ui-svg-column svg (+ pad 10) (+ pad 4) (+ pad 12) (funcall grey (if pressed #xDD #x88))))
    (when on
      (svg-polyline svg (mapcar (pcase-lambda (`(,x . ,y)) (cons (+ x pad) (+ y pad)))
                                '((2.5 . 7.5) (5.5 . 10.5) (12.5 . 1.5)))
                    :fill "none" :stroke ink :stroke-width 2 :stroke-linecap "square"))
    svg))

;;;;; Progress bars

(defconst lispm-desktop-platinum-progress-height 14
  "Height in pixels of a progress bar, its etched edge included.")

(defun lispm-desktop-platinum--progress-ramp (palette)
  "Return the shades a progress bar is filled with, darkest first, as a
vector: the variation colour's ramp, in Lavender #000055, #333399,
#6666CC, #9999FF and #CCCCFF, then Platinum's #EEEEEE."
  (vector (lispm-ui-blend (plist-get palette :highlight-dark) "#000000" 0.375)
          (plist-get palette :highlight)
          (plist-get palette :thumb-dark)
          (plist-get palette :thumb)
          (plist-get palette :thumb-light)
          (lispm-ui-grey palette #xEE)))

(cl-defgeneric lispm-desktop-platinum-svg-progress-bar (svg x y width fraction palette)
  "Draw on SVG at X, Y a progress bar WIDTH pixels wide, FRACTION filled.
It is Mac OS 9's, as its startup screen drew it: etched into the
platinum and outlined; the part filled is a tube of the variation
colour lit along its middle and its left, shaded toward its right end,
which a black line closes; the rest is a groove shaded from its top
left.  The bar is `lispm-desktop-platinum-progress-height' pixels high."
  (let* ((grey (lambda (level) (lispm-ui-grey palette level)))
         (ramp (lispm-desktop-platinum--progress-ramp palette))
         (inner (- width 4))
         (filled (round (* inner (min 1.0 (max 0.0 fraction)))))
         (left (+ x 2))
         (top (+ y 2))
         (bottom (+ top 9)))
    (cl-flet ((dot (column row shade)
                (lispm-ui-svg-fill svg (+ left column) (+ top row) 1 1 (aref ramp shade))))
      (lispm-ui-svg-row svg x (+ x width -2) y (funcall grey #xAA))
      (lispm-ui-svg-column svg x (1+ y) (+ y 12) (funcall grey #xAA))
      (lispm-ui-svg-row svg (1+ x) (+ x width -1) (+ y 13) (funcall grey #xFF))
      (lispm-ui-svg-column svg (+ x width -1) (1+ y) (+ y 12) (funcall grey #xFF))
      (lispm-ui-svg-fill svg (1+ x) (1+ y) (- width 2) 12 (plist-get palette :outline))
      (if (< filled 6)
          ;; Too short a tube to be lit: its darker shade alone.
          (when (> filled 0)
            (lispm-ui-svg-fill svg left top filled 10 (aref ramp 1)))
        (seq-do-indexed
         (lambda (shade row)
           (lispm-ui-svg-row svg left (+ left filled -1) (+ top row) (aref ramp shade))
           (dot 0 row 2)
           (dot 1 row (min 5 (1+ shade)))
           (dot (- filled 3) row (if (= row 4) 4 shade))
           (dot (- filled 2) row (pcase row (1 2) (9 0) (_ 1)))
           (dot (- filled 1) row (if (= row 0) 1 0)))
         [1 2 3 4 5 4 3 2 1 0]))
      (when (< filled inner)
        (let ((from (+ left filled (if (> filled 0) 1 0)))
              (to (+ left inner -1)))
          (when (> filled 0)
            (lispm-ui-svg-column svg (+ left filled) top bottom (plist-get palette :outline)))
          (when (<= from to)
            (lispm-ui-svg-fill svg from top (1+ (- to from)) 10 (funcall grey #xBB))
            (lispm-ui-svg-row svg (1+ from) (1- to) top (funcall grey #x88))
            (lispm-ui-svg-column svg to (1+ top) bottom (funcall grey #xDD))
            (lispm-ui-svg-row svg (+ from 2) to bottom (funcall grey #xDD))
            (lispm-ui-svg-column svg (1+ from) top bottom (funcall grey #x88))
            (lispm-ui-svg-column svg from top bottom (funcall grey #x55))))))))

(defun lispm-desktop-platinum-draw-progress-bar (palette width fraction)
  "Return an SVG of a progress bar WIDTH pixels wide, FRACTION filled, in
PALETTE; see `lispm-desktop-platinum-svg-progress-bar'."
  (let ((svg (svg-create width lispm-desktop-platinum-progress-height)))
    (lispm-desktop-platinum-svg-progress-bar svg 0 0 width fraction palette)
    svg))

;;;;; Window frames

(defconst lispm-desktop-platinum-frame-inset '(6 . 1)
  "Where a floating window stands inside its frame, as (LEFT . TOP) pixels:
past the outline and the four-pixel band and the content's line on the
left, right and bottom, and right under the outline at the top, where
its title bar runs.")

(cl-defgeneric lispm-desktop-platinum-draw-window-frame (palette width height &key active)
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
         (grey (lambda (level) (lispm-ui-grey palette level)))
         (outline (if active (plist-get palette :outline) (funcall grey #x55)))
         (title lispm-desktop-platinum-title-bar-height)
         (right (- w 2))
         (bottom (- h 2)))
    (cl-flet ((row (from to y color) (lispm-ui-svg-row svg from to y color))
              (column (x from to color) (lispm-ui-svg-column svg x from to color)))
      ;; The shadow.
      (column (1- w) 2 (1- h) (plist-get palette :outline))
      (row 2 (1- w) (1- h) (plist-get palette :outline))
      ;; The band, flat to begin with.
      (lispm-ui-svg-fill svg 1 1 5 (- h 3) (funcall grey (if active #xCC #xDD)))
      (lispm-ui-svg-fill svg (- w 7) 1 5 (- h 3) (funcall grey (if active #xCC #xDD)))
      (lispm-ui-svg-fill svg 1 (- h 7) (- w 3) 5 (funcall grey (if active #xCC #xDD)))
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

(cl-defgeneric lispm-desktop-platinum-draw-strip-segment (palette kind &key state icon menu focused behind side)
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
         (grey (lambda (level) (lispm-ui-grey palette level)))
         (outline (plist-get palette :outline))
         (face (plist-get palette :strip))
         (light (plist-get palette :strip-light))
         (shade (plist-get palette :strip-shade))
         (dark (plist-get palette :strip-dark))
         ;; Arrows and triangles black on Platinum's greys; on a dark
         ;; theme's, its text colour.
         (ink (lispm-ui-legible face outline (plist-get palette :text))))
    (if (eq kind 'tab)
        (progn
          (when behind
            (lispm-ui-svg-fill svg 0 0 w h behind))
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
        (lispm-ui-svg-fill svg 0 0 w h outline)
        (lispm-ui-svg-fill svg left 1 (- right left -1) (- h 2) (if pressed dark face))
        (cl-flet ((band (top-left bottom-right width)
                    (dotimes (i width)
                      (lispm-ui-svg-row svg (+ left i) (- right i) (+ 1 i) (nth i top-left))
                      (lispm-ui-svg-column svg (+ left i) (+ 1 i) (- h 2 i) (nth i top-left))
                      (lispm-ui-svg-row svg (+ left i 1) (- right i) (- h 2 i)
                                                   (nth i bottom-right))
                      (lispm-ui-svg-column svg (- right i) (+ 1 i) (- h 2 i)
                                                      (nth i bottom-right)))))
          (cond (pressed
                 (let ((deep (lispm-ui-blend dark outline 0.5)))
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
               (lispm-ui-svg-row svg (+ left i) (- right i) (+ 1 i) (plist-get palette :focus))
               (lispm-ui-svg-row svg (+ left i) (- right i) (- h 2 i) (plist-get palette :focus))
               (lispm-ui-svg-column svg (+ left i) (+ 1 i) (- h 2 i) (plist-get palette :focus))
               (lispm-ui-svg-column svg (- right i) (+ 1 i) (- h 2 i) (plist-get palette :focus))))
           (cond ((stringp icon) (lispm-ui-svg-icon svg icon 2 3 16))
                 (icon (lispm-ui-svg-embed svg icon 2 3 16 16)))
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
  (let* ((face (or face 'lispm-ui-list))
         (height (or height 20))
         (width (+ (lispm-ui-text-width label face)
                   (* 2 lispm-desktop-platinum-bevel-padding))))
    (if (lispm-desktop-tty-p)
        (lispm-desktop-platinum-text-button label state)
      (propertize " " 'display
                  (lispm-ui-image
                   (list 'bevel (lispm-ui-content-key label) state height face width)
                   (lambda ()
                     (lispm-desktop-platinum-draw-bevel-button lispm-ui-current-palette width height
                                                               :state state :label label :face face)))))))

(defun lispm-desktop-platinum-text-button (label &optional state default)
  "Return a button reading LABEL on a text terminal, as text: LABEL between
brackets.  STATE is nil, `pressed' or `disabled'; DEFAULT draws the
default button, which Return presses, in the highlight colour, as the
ring about it marks it on Mac OS 9."
  (let* ((palette lispm-ui-current-palette)
         (ink (cond ((eq state 'disabled) (plist-get palette :muted))
                    ((or default (eq state 'pressed)) (plist-get palette :highlight-text))
                    (t (plist-get palette :text))))
         (ground (cond ((or default (eq state 'pressed)) (plist-get palette :highlight))
                       (t (plist-get palette :chrome))))
         (button (concat "[" label "]")))
    (add-face-text-property 0 (length button) `(:foreground ,ink :background ,ground) t button)
    button))

(cl-defun lispm-desktop-platinum-push-button (label &key state default focused pad)
  "Return a push button reading LABEL, as a string displaying it.
STATE, DEFAULT, FOCUSED and PAD are as `lispm-desktop-platinum-draw-push-button'
takes them; PAD defaults to two pixels, room for the focus ring, so a
button keeps its place as the keyboard comes and goes."
  (let ((width (lispm-desktop-platinum-push-button-width label))
        (pad (or pad (if default 5 2))))
    (if (lispm-desktop-tty-p)
        (propertize " " 'display (lispm-desktop-platinum-text-control
                                  (lispm-desktop-platinum-text-button (concat " " label " ") state default)
                                  focused))
      (propertize " " 'display
                  (lispm-ui-image
                   (list 'push label width state default focused pad)
                   (lambda ()
                     (lispm-desktop-platinum-draw-push-button lispm-ui-current-palette label width
                                                              :state state :default default
                                                              :focused focused :pad pad)))))))

(defun lispm-desktop-platinum--tab-edge (width)
  "Return the pane's edge WIDTH pixels wide, as a string displaying it."
  (propertize " " 'display
              (lispm-ui-image
               (list 'tab-edge width)
               (lambda () (lispm-desktop-platinum-draw-tab-edge lispm-ui-current-palette width)))))

(defconst lispm-desktop-platinum--tabs-lead 8
  "Pixels of the pane's edge before a row of tabs.")

(defun lispm-desktop-platinum--tab-width (label face)
  "Return the width in pixels of a tab reading LABEL in FACE."
  (+ (lispm-ui-text-width label face)
     (* 2 lispm-desktop-platinum-tab-padding)))

(defun lispm-desktop-platinum-tabs-width (labels &optional face)
  "Return the width in pixels a row of tabs reading LABELS takes, as
`lispm-desktop-platinum-tabs' draws it in FACE, the pane's edge after
them left out: the narrowest the row fits in."
  (let ((face (or face 'lispm-ui-title)))
    (apply #'+ lispm-desktop-platinum--tabs-lead
           (mapcar (lambda (label) (+ (lispm-desktop-platinum--tab-width label face) 2)) labels))))

(defun lispm-desktop-platinum--fit-tabs (labels width face)
  "Return LABELS shortened so that a row of tabs reading them in FACE fits
WIDTH pixels: the widest cut, an ellipsis ending them, no narrower than
the widest the row allows, the others whole.  As they are where the row
fits, or where no tab could be narrower than 48 pixels."
  (let* ((padding (* 2 lispm-desktop-platinum-tab-padding))
         (widths (mapcar (lambda (label) (lispm-desktop-platinum--tab-width label face)) labels))
         (room (- width lispm-desktop-platinum--tabs-lead (* 2 (length labels))))
         (cap (apply #'max 0 widths)))
    (while (and (> (apply #'+ (mapcar (lambda (w) (min w cap)) widths)) room)
                (> cap 48))
      (setq cap (1- cap)))
    (if (= cap (apply #'max 0 widths))
        labels
      (seq-mapn (lambda (label w)
                  (if (<= w cap)
                      label
                    (lispm-desktop-platinum-fit (propertize label 'face face) (- cap padding))))
                labels widths))))

(defconst lispm-desktop-platinum--tab-close-gap 4
  "Pixels between a tab's cross and its label.")

(defconst lispm-desktop-platinum--tab-close-reach 3
  "Pixels round a tab's cross a press on it still lands in.")

(defun lispm-desktop-platinum--tab-close (pressed)
  "Return the cross closing a tab, PRESSED or not, then the gap after it,
as a label's start, see `lispm-desktop-platinum-draw-tab-close'."
  (concat (propertize " " 'display
                      (lispm-ui-image
                       (list 'tab-close pressed)
                       (lambda () (lispm-desktop-platinum-draw-tab-close
                                   lispm-ui-current-palette :pressed pressed))))
          (propertize " " 'display `(space :width (,lispm-desktop-platinum--tab-close-gap)))))

(declare-function lispm-desktop-platinum-box-button "lispm-desktop-platinum-titlebar"
                  (string kind command help))
(declare-function lispm-desktop-platinum-pressed-p "lispm-desktop-platinum-titlebar" (target))

(defun lispm-desktop-platinum--tab-with-close (image width properties close target)
  "Return a tab drawn as IMAGE, WIDTH pixels wide, its cross pressable.
The tab is shown in three slices of IMAGE: the cross, with the pixels
round it of `lispm-desktop-platinum--tab-close-reach', between the rest
of the tab before and after it.  The rest carries PROPERTIES; the cross
runs CLOSE once pressed and let go on it, as a title bar's box does, see
`lispm-desktop-platinum-box-button', and is held down as TARGET."
  (let* ((height (+ lispm-desktop-platinum-tab-height 3))
         ;; Labels are centred in the tab, the cross opening the label.
         (from (- lispm-desktop-platinum-tab-padding lispm-desktop-platinum--tab-close-reach))
         (to (+ lispm-desktop-platinum-tab-padding lispm-desktop-platinum-tab-close-size
                lispm-desktop-platinum--tab-close-reach)))
    (cl-flet ((slice (x w) `((slice ,x 0 ,w ,height) ,image)))
      (concat (apply #'propertize " " 'display (slice 0 from) properties)
              (lispm-desktop-platinum-box-button
               (propertize " " 'display (slice from (- to from)) 'pointer 'arrow)
               target (lambda (_window) (funcall close)) (lispm-desktop-tr "Close this tab"))
              (apply #'propertize " " 'display (slice to (- width to)) properties)))))

(defun lispm-desktop-platinum--plist-without (plist key)
  "Return PLIST without KEY and its value."
  (let (kept)
    (while plist
      (unless (eq (car plist) key)
        (setq kept (cons (cadr plist) (cons (car plist) kept))))
      (setq plist (cddr plist)))
    (nreverse kept)))

(defun lispm-desktop-platinum-tabs (tabs width &optional face)
  "Return a row of TABS standing on their pane's edge, WIDTH pixels wide.
Each tab is (LABEL FRONT . PROPERTIES): LABEL shown as Emacs would show
it in FACE, by default the system font's; FRONT non-nil for the tab in
front; PROPERTIES, text properties the tab's string carries, such as its
keymap and help.  A `:close' among PROPERTIES is no text property but a
function closing the tab: the tab then shows a cross before its label,
which calls it, see `lispm-desktop-platinum-draw-tab-close'.
The row opens with a short run of the pane's edge and runs it on to
WIDTH, as Mac OS 9 drew a tab control along its pane.  A row too wide
for WIDTH, as in a longer language, cuts its widest labels short, see
`lispm-desktop-platinum--fit-tabs'.  On a text terminal the row is text,
WIDTH columns wide, see `lispm-desktop-platinum--text-tabs'."
  (if (lispm-desktop-tty-p)
      (lispm-desktop-platinum--text-tabs tabs width)
    (lispm-desktop-platinum--image-tabs tabs width face)))

(defun lispm-desktop-platinum--text-tabs (tabs width)
  "Return a row of TABS WIDTH columns wide on a text terminal, as text.
TABS are as `lispm-desktop-platinum-tabs' takes them.  Each label stands
in a cell of its own either side, the tab in front highlighted, between
runs of a rule that is the pane's edge; a tab with a `:close' function
shows a cross before its label, which calls it.  Labels too long for
WIDTH are cut, the longest first."
  (let* ((rule (lambda (n) (propertize (make-string (max 0 n) ?─) 'face 'lispm-desktop-platinum-toolbar-label)))
         (crosses (seq-map-indexed
                   (pcase-lambda (`(,_ ,_ . ,properties) index)
                     (when-let* ((close (plist-get properties :close)))
                       (lispm-desktop-platinum-box-button
                        (propertize "×" 'pointer 'arrow) (list 'tab-close index)
                        (lambda (_window) (funcall close)) (lispm-desktop-tr "Close this tab"))))
                   tabs))
         (labels (mapcar #'car tabs))
         ;; A rule before each tab and one after the last, a cell either
         ;; side of each label, two more for a cross.
         (frame (+ 1 (* 3 (length tabs)) (* 2 (length (delq nil (copy-sequence crosses))))))
         (cap (apply #'max 0 (mapcar #'string-width labels))))
    (while (and (> cap 3)
                (> (+ frame (apply #'+ (mapcar (lambda (label) (min cap (string-width label))) labels)))
                   width))
      (setq cap (1- cap)))
    (let ((row (concat
                (funcall rule 1)
                (apply #'concat
                       (seq-mapn
                        (pcase-lambda (`(,label ,front . ,properties) cross)
                          (let ((tab (concat " " (when cross (concat cross " "))
                                             (truncate-string-to-width label cap nil nil "…") " ")))
                            (add-face-text-property 0 (length tab)
                                                    (if front 'lispm-desktop-platinum-menu-title-active
                                                      'lispm-desktop-platinum-toolbar)
                                                    t tab)
                            (concat (apply #'propertize tab
                                           (lispm-desktop-platinum--plist-without properties :close))
                                    (funcall rule 1))))
                        tabs crosses)))))
      (concat row (funcall rule (- width (string-width row)))))))

(defun lispm-desktop-platinum--image-tabs (tabs width face)
  "Return a row of TABS WIDTH pixels wide in FACE, drawn as images.
See `lispm-desktop-platinum-tabs'."
  (let* ((face (or face 'lispm-ui-title))
         (lead lispm-desktop-platinum--tabs-lead)
         (used lead)
         ;; The labels as shown, a cross before each that closes its
         ;; tab, cut short where the row would not fit.
         (labels (lispm-desktop-platinum--fit-tabs
                  (seq-map-indexed (pcase-lambda (`(,label ,_ . ,properties) index)
                                     (if (plist-get properties :close)
                                         (concat (lispm-desktop-platinum--tab-close
                                                  (lispm-desktop-platinum-pressed-p (list 'tab-close index)))
                                                 label)
                                       label))
                                   tabs)
                  width face))
         (parts
          (seq-map-indexed
           (pcase-lambda (`(,_ ,front . ,properties) index)
             (let* ((close (plist-get properties :close))
                    (properties (lispm-desktop-platinum--plist-without properties :close))
                    (target (list 'tab-close index))
                    (label (nth index labels))
                    (tab-width (lispm-desktop-platinum--tab-width label face))
                    (image (lispm-ui-image
                            (list 'tab (lispm-ui-content-key label) front face tab-width)
                            (lambda ()
                              (lispm-desktop-platinum-draw-tab lispm-ui-current-palette
                                                               label tab-width :front front
                                                               :face face)))))
               (setq used (+ used tab-width 2))
               (concat (if close
                           (lispm-desktop-platinum--tab-with-close image tab-width properties
                                                                   close target)
                         (apply #'propertize " " 'display image properties))
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
              (if (lispm-desktop-tty-p)
                  (lispm-desktop-platinum-text-control (if on "(•)" "( )") focused state)
               (lispm-ui-image
               (list 'radio on state focused)
               (lambda () (lispm-desktop-platinum-draw-radio lispm-ui-current-palette on
                                                             :state state :focused focused :pad 2))))))

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

(cl-defgeneric lispm-desktop-platinum-draw-list-check-box (palette state)
  "Return an SVG of a list's check box in PALETTE, twelve pixels square.
STATE is `on', `off' or `mixed', see `lispm-desktop-platinum--list-check-boxes'."
  (let ((svg (svg-create 12 12)))
    (lispm-desktop-platinum-svg-pixel-map
     svg 0 0 (alist-get state lispm-desktop-platinum--list-check-boxes)
     `((?# . ,(plist-get palette :outline))
       ,@(mapcar (pcase-lambda (`(,char . ,level)) (cons char (lispm-ui-grey palette level)))
                 '((?w . #xFF) (?d . #xDD) (?a . #xAA) (?g . #x88) (?s . #x77)))))
    svg))

(defun lispm-desktop-platinum-list-check-box (state)
  "Return a list's check box in STATE as a string displaying it.
See `lispm-desktop-platinum-draw-list-check-box'."
  (propertize " " 'display
              (if (lispm-desktop-tty-p)
                  (lispm-desktop-platinum-text-control (pcase state ('on "[✓]") ('mixed "[-]") (_ "[ ]")))
                (lispm-ui-image
                 (list 'list-check-box state)
                 (lambda () (lispm-desktop-platinum-draw-list-check-box lispm-ui-current-palette
                                                                        state))))))

(defun lispm-desktop-platinum-check-box (on &optional state focused pad)
  "Return a check box, checked when ON, in STATE, as a string displaying it.
FOCUSED rings it, and PAD leaves room about it; see
`lispm-desktop-platinum-draw-check-box'."
  (propertize " " 'display
              (if (lispm-desktop-tty-p)
                  (lispm-desktop-platinum-text-control (if on "[✓]" "[ ]") focused state)
                (lispm-ui-image
                 (list 'check-box on state focused pad)
                 (lambda () (lispm-desktop-platinum-draw-check-box lispm-ui-current-palette on
                                                                   :state state :focused focused
                                                                   :pad (or pad 0)))))))

(defun lispm-desktop-platinum-text-control (text &optional focused state)
  "Return TEXT, a control drawn as text on a text terminal, ringed in the
focus colour when FOCUSED, dimmed when STATE is `disabled'.  Controls
inserted in a buffer show it as their `display', as they show their
images elsewhere, see `lispm-desktop-platinum-show-focus'."
  (let ((text (copy-sequence text)))
    (cond (focused
           (add-face-text-property 0 (length text)
                                   `(:foreground ,(lispm-ui-color :highlight-text)
                                     :background ,(lispm-ui-color :focus))
                                   nil text))
          ((eq state 'disabled)
           (add-face-text-property 0 (length text) `(:foreground ,(lispm-ui-color :muted))
                                   nil text)))
    text))

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
               (progn (sit-for (alist-get 'button lispm-ui-modal-flashes)) t)
             (let ((release (let ((echo-keystrokes 0)) (lispm-ui-modal-read-event))))
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

(defun lispm-desktop-platinum--insert-control (images label action help &optional keymap)
  "Insert a control showing IMAGES, as a button calling ACTION with no arguments.
IMAGES is a function of FOCUSED and STATE returning the control's image,
or its text on a text terminal.  LABEL follows it, if non-nil; HELP
describes it.  KEYMAP, by default button.el's, is the control's."
  (insert-text-button
   (concat (propertize " " 'display (funcall images nil nil)
                       'lispm-desktop-platinum-plain (funcall images nil nil)
                       'lispm-desktop-platinum-focused (funcall images t nil)
                       'lispm-desktop-platinum-pressed (funcall images nil 'pressed))
           (if label (concat " " label) ""))
   'action (lambda (_) (funcall action))
   'face 'default 'follow-link t 'help-echo help
   'keymap (or keymap button-map)))

(cl-defun lispm-desktop-platinum-insert-radio-button (label on action &key help)
  "Insert a radio button named LABEL, ON or off, which calls ACTION with no
arguments when pressed; HELP describes it.  Drawn focused while point is
on it, see `lispm-desktop-platinum-show-focus'."
  (lispm-desktop-platinum--insert-control
   (lambda (focused state)
     (lispm-desktop-platinum-image-of (lispm-desktop-platinum-radio on state focused)))
   label action (or help label)))

(cl-defun lispm-desktop-platinum-insert-check-box (label on action &key help)
  "Insert a check box named LABEL, checked when ON, which calls ACTION with no
arguments when pressed; HELP describes it.  Drawn focused while point is
on it, see `lispm-desktop-platinum-show-focus'."
  (lispm-desktop-platinum--insert-control
   (lambda (focused state)
     (lispm-desktop-platinum-image-of (lispm-desktop-platinum-check-box on state focused 2)))
   label action (or help label)))

(cl-defun lispm-desktop-platinum-insert-popup-button (label choices action &key help current)
  "Insert a pop-up menu showing LABEL, offering CHOICES, a list of (VALUE .
TEXT); pressed, its menu opens at it, CURRENT's item checked, and the
VALUE of the item chosen is given to ACTION.  HELP describes it.  Return,
Space and the mouse open it, as they press a push button."
  (lispm-desktop-platinum--insert-control
   (lambda (focused state) (lispm-desktop-platinum-image-of
                            (lispm-desktop-platinum-popup-button label :state state :focused focused)))
   nil
   (lambda ()
     (when-let* ((choice (lispm-desktop-platinum-choose choices current nil (or help label))))
       (funcall action (car choice))))
   (or help label)
   lispm-desktop-platinum-push-button-map))

(defun lispm-desktop-platinum-choose (choices &optional current position label)
  "Open a menu of CHOICES, a list of (VALUE . TEXT), at POSITION, by default
point, CURRENT's item checked; return (VALUE) of the item chosen, or nil.
Where no menu can be drawn, the choices are read in the minibuffer, after
LABEL."
  (if (not (lispm-desktop-popup-available-p))
      (let ((text (completing-read (format-prompt (or label "") (cdr (assoc current choices)))
                                   (mapcar #'cdr choices) nil t nil nil
                                   (cdr (assoc current choices)))))
        (when-let* ((choice (rassoc text choices)))
          (list (car choice))))
    (let* ((position (or position (point)))
           (xy (and (pos-visible-in-window-p position nil t)
                    (posn-x-y (posn-at-point position))))
           (item (lispm-desktop-popup-choose
                  (mapcar (pcase-lambda (`(,value . ,text))
                            (list :label text :value value :checked (equal value current)))
                          choices)
                  (and xy (+ (car xy) (car (window-inside-pixel-edges))))
                  (and xy (+ (cdr xy) (cadr (window-inside-pixel-edges))
                             lispm-desktop-platinum-button-height)))))
      (and item (list (plist-get item :value))))))

(cl-defun lispm-desktop-platinum-insert-icon-button (icon label action &key help)
  "Insert a bevel button holding ICON, a 32-pixel icon's name, which calls
ACTION with no arguments when pressed; HELP describes it.  LABEL is its
name, for those who read it, as the icon is all it shows.  Return,
Space and the mouse press it as a push button."
  (insert-text-button " "
                      'display (lispm-desktop-platinum-icon-button icon nil nil)
                      'lispm-desktop-platinum-plain (lispm-desktop-platinum-icon-button icon nil nil)
                      'lispm-desktop-platinum-focused (lispm-desktop-platinum-icon-button icon nil t)
                      'lispm-desktop-platinum-pressed (lispm-desktop-platinum-icon-button icon 'pressed nil)
                      'action (lambda (_) (funcall action))
                      'lispm-desktop-platinum-label label
                      'face nil 'keymap lispm-desktop-platinum-push-button-map
                      'help-echo (or help label)))

(defconst lispm-desktop-platinum-icon-button-size '(52 . 50)
  "Width and height in pixels of a bevel button holding a large icon, as
Software Update's Update Now button.")

(defun lispm-desktop-platinum-icon-button (icon state focused)
  "Return the image of a bevel button holding ICON, a 32-pixel icon's name,
in STATE, nil or `pressed', ringed when FOCUSED.  The ring's room is
always left, so the button keeps its place.  On a text terminal the
button is text, the icon a character in it."
  (if (lispm-desktop-tty-p)
      (lispm-desktop-platinum-text-control
       (lispm-desktop-platinum-text-button (concat " " (substring-no-properties
                                                         (lispm-desktop-platinum-icon icon))
                                                   " ")
                                           state)
       focused)
    (lispm-ui-image
     (list 'icon-button icon state focused)
     (lambda ()
       (pcase-let* ((`(,w . ,h) lispm-desktop-platinum-icon-button-size)
                    (palette lispm-ui-current-palette)
                    (pad 3)
                    (svg (svg-create (+ w (* 2 pad)) (+ h (* 2 pad)))))
         (when focused
           (lispm-desktop-platinum-svg-focus-ring svg pad pad w h 1 palette))
         (lispm-ui-svg-embed
          svg (lispm-desktop-platinum-draw-bevel-button palette w h :state state) pad pad w h)
         (lispm-ui-svg-emacs-image
          svg (create-image (lispm-desktop-platinum-icon-file icon))
          (+ pad (/ (- w 32) 2)) (+ pad (/ (- h 32) 2)) 32 32)
         svg)))))

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
