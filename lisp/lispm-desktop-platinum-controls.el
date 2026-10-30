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

;; The chrome's own controls, drawn in the theme's colours: header strips
;; and header buttons, tabs, placards, progress bars, window frames and
;; the Control Strip's segments, and bevel buttons holding an icon, which
;; any window of the desktop's may hold.  The toolkit's controls, push,
;; bevel and text buttons, pop-up menus, radio buttons, check boxes and
;; group boxes, as images, as text and as buttons in buffers, are LispM
;; UI's (lispm-ui-controls.el); the desktop opens their menus.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-platinum-draw)
(require 'lispm-desktop-platinum-icons)
(require 'lispm-ui-modal)
(require 'lispm-ui-controls)
(require 'lispm-desktop-i18n)

(declare-function lispm-desktop-platinum-fit "lispm-desktop-platinum-titlebar" (string width))
(declare-function lispm-ui-popup-available-p "lispm-ui-popup" ())
(declare-function lispm-ui-menu-choose "lispm-ui-menu" (items &optional x y align))

;;;; Header strips and header buttons


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
       (* 2 lispm-ui-bevel-padding))))

(cl-defgeneric lispm-desktop-platinum-draw-header-button (palette label &optional state)
  "Return an SVG of a header strip's opening, a bevel button reading LABEL.
Mac OS 9's Finder windows held no button there; LispM's opens the strip
with one, standing on the strip's ground six pixels in from its left,
LABEL in the small system font, in PALETTE.  STATE is as
`lispm-ui-draw-bevel-button' takes it.  The rest of the
strip follows, drawn by `lispm-desktop-platinum-draw-header-strip'."
  (let* ((width (lispm-desktop-platinum-header-button-width label))
         (svg (svg-create width lispm-desktop-platinum-header-strip-height)))
    (lispm-desktop-platinum--draw-header-ground svg width palette)
    (lispm-ui-svg-bevel-button svg 6 2 palette (- width 6)
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
              (lispm-ui-text-button label state))
    (propertize " " 'display
                (lispm-ui-image
                 (list 'header-button (lispm-ui-content-key label) state)
                 (lambda () (lispm-desktop-platinum-draw-header-button lispm-ui-current-palette
                                                                       label state))))))

;;;; Controls (pure)
;;
;; Mac OS 9's controls as its Appearance Manager drew them in Platinum,
;; measured pixel by pixel on Mac OS 9.0.4: tabs, placards, progress
;; bars, window frames and the Control Strip's segments.  Each draws with `lispm-ui-grey', so Platinum's
;; greys come out as themselves and another theme gets their
;; counterparts.  Labels are SVG text in the system font, as heavy as the
;; titles and menus Emacs draws (`lispm-ui-svg-font').

(defconst lispm-desktop-platinum-tab-height 21
  "Height in pixels of a tab, its outline included.
The edge of the pane the tabs stand on takes three rows more.")

(defconst lispm-desktop-platinum-strip-height 24
  "Height in pixels of the Control Strip, its outline included.")

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

;;;;; Placards

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
         (title lispm-ui-title-bar-height)
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
          (lispm-ui-svg-pixel-map
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
           (lispm-ui-svg-pixel-map
            svg (+ left 2) 8 lispm-desktop-platinum--strip-collapse
            `((?l . ,(funcall grey #x55)) (?c . ,(plist-get palette :thumb-light))
              (?r . ,(funcall grey #x88)) (?q . ,(funcall grey #x44)) (?b . ,light))))
          ((or 'left 'right)
           (let ((colors (if (eq state 'disabled)
                             `((?l . ,(funcall grey #x55)) (?j . ,(funcall grey #xDD)))
                           `((?l . ,ink) (?j . ,ink)))))
             (lispm-ui-svg-pixel-map
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
             (lispm-ui-svg-pixel-map
              svg 21 8 '("#" "##" "###" "####" "####" "###" "##" "#")
              `((?# . ,ink))))))))
    svg))

(defconst lispm-desktop-platinum-tab-padding 13
  "Pixels between a tab's sides, where they meet the pane, and its label.")

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

;;;; Controls in buffers
;;
;; Controls inserted in a buffer, as Control Panels holds them, are LispM
;; UI's (`lispm-ui-insert-push-button' and the others): buttons of
;; button.el, the one point is on drawn ringed in the focus colour.  The
;; desktop's own are the bevel buttons holding an icon.

(cl-defun lispm-desktop-platinum-insert-icon-button (icon label action &key help)
  "Insert a bevel button holding ICON, a 32-pixel icon's name, which calls
ACTION with no arguments when pressed; HELP describes it.  LABEL is its
name, for those who read it, as the icon is all it shows.  Return,
Space and the mouse press it as a push button."
  (insert-text-button " "
                      'display (lispm-desktop-platinum-icon-button icon nil nil)
                      'lispm-ui-plain (lispm-desktop-platinum-icon-button icon nil nil)
                      'lispm-ui-focused (lispm-desktop-platinum-icon-button icon nil t)
                      'lispm-ui-pressed (lispm-desktop-platinum-icon-button icon 'pressed nil)
                      'action (lambda (_) (funcall action))
                      'lispm-ui-label label
                      'face nil 'keymap lispm-ui-push-button-map
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
      (lispm-ui-text-control
       (lispm-ui-text-button (concat " " (substring-no-properties
                                          (lispm-ui-icon icon))
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
           (lispm-ui-svg-focus-ring svg pad pad w h 1 palette))
         (lispm-ui-svg-embed
          svg (lispm-ui-draw-bevel-button palette w h :state state) pad pad w h)
         (lispm-ui-svg-emacs-image
          svg (create-image (lispm-ui-icon-file icon))
          (+ pad (/ (- w 32) 2)) (+ pad (/ (- h 32) 2)) 32 32)
         svg)))))

(provide 'lispm-desktop-platinum-controls)
;;; lispm-desktop-platinum-controls.el ends here
