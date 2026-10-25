;;; lispm-desktop-os2.el --- OS/2 2.0's Workplace Shell look for the chrome  -*- lexical-binding: t; -*-

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

;; The look `os2': the chrome drawn as OS/2 2.0's Workplace Shell drew
;; its windows, read off screenshots of OS/2 2.0 at 640 by 480:
;;
;;   title bars  the active one teal, its title white and bold, an inactive
;;               one pale grey, its title grey; grey lines along their top
;;               and bottom
;;   buttons     the system menu at the left, the window's own small icon
;;               on a raised square; minimize and maximize at the right,
;;               raised squares holding a small window and a large one
;;   borders     pale yellow round the active window, pale grey round the
;;               others, lit outside and grooved where the window starts
;;   menus       pale grey, raised, the item under the mouse in the
;;               highlight colour; a submenu's arrow on a small button
;;   controls    raised buttons outlined in black; scroll bars with arrow
;;               buttons at either end over a stippled track
;;   desktop     icons' titles straight on the desktop, a selected icon
;;               set on grey
;;
;; Every colour comes from the palette; the os2 theme supplies OS/2 2.0's
;; own scheme.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-look)
(require 'lispm-desktop-pc)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-popup)
(require 'lispm-desktop-surface)
(require 'lispm-desktop-i18n)

(defconst lispm-desktop-os2-button-width 18
  "Width in pixels of a title bar's buttons, the system menu's included.")

(lispm-desktop-look-define
 'os2
 :title "OS/2 2.0"
 :description (lispm-desktop-tr-noop "OS/2 2.0's Workplace Shell: teal title bars, pale yellow borders")
 :theme 'os2
 :icons '("os2")
 :system-families '("Helvetica" "Helv" "Arial")
 :list-families '("Helvetica" "Helv" "Arial")
 :family 'pc
 ;; See lispm-desktop-pc.el.
 :metrics `(:box-width ,lispm-desktop-os2-button-width :system-icon t
            :bevels (:raised ((:light) (:shadow)) :pushed ((:shadow) (:light))
                     :button ((:light :light) (:shadow :shadow)) :button-pushed ((:shadow :shadow) (:light))
                     :tab ((:light) (:shadow)) :dialog ((:light) (:shadow)) :menu ((:light) (:shadow)))
            :button-focus 4
            :alert-gap 8 :alert-bottom 12
            :dialog-ground :border-inactive :dialog-inset 0 :dialog-body :chrome
            :strip-lines (:shadow :light) :strip-ink :text :tree-box :text :group-side (:shadow :light)
            :radio-sunk t :radio-dot 2 :child-frame-border :shadow :dividers (:border-inactive :shadow :light)
            :menu-inset 2 :menu-separator (3 :shadow :light) :submenu-button t)
 :box-help (list (lispm-desktop-tr-noop "System menu: restore, minimize, maximize, close")
                 (lispm-desktop-tr-noop "Minimize: roll up to the title bar")
                 (lispm-desktop-tr-noop "Maximize, or restore" "OS/2 2.0"))
 :system-menu (list (lispm-desktop-tr-noop "Restore" "OS/2 2.0") (lispm-desktop-tr-noop "Minimize" "OS/2 2.0")
                    (lispm-desktop-tr-noop "Maximize" "OS/2 2.0") (lispm-desktop-tr-noop "Close" "OS/2 2.0")))

;;;; Colours

(defun lispm-desktop-os2--stipple (svg palette)
  "Define on SVG the stipple of a scroll bar's track, every other pixel
grey and white, and return its fill."
  (let ((defs (or (car (dom-by-tag svg 'defs))
                  (let ((defs (dom-node 'defs)))
                    (dom-add-child-before svg defs)
                    defs))))
    (dom-append-child defs (dom-node 'pattern '((id . "stipple") (width . 2) (height . 2)
                                                (patternUnits . "userSpaceOnUse"))
                                     (dom-node 'rect `((width . 2) (height . 2)
                                                       (fill . ,(plist-get palette :light))))
                                     (dom-node 'rect `((width . 1) (height . 1)
                                                       (fill . ,(lispm-desktop-pc-shadow palette))))
                                     (dom-node 'rect `((x . 1) (y . 1) (width . 1) (height . 1)
                                                       (fill . ,(lispm-desktop-pc-shadow palette))))))
    "url(#stipple)"))

;;;; Title bars

(cl-defmethod lispm-desktop-pc-draw-caption (palette width &context (lispm-desktop-look (eql os2))
                                                     &optional active label label-x)
  "Return an SVG of a stretch of title bar WIDTH pixels wide, in PALETTE:
the bar's colour, ACTIVE or not, between grey lines along its top and
bottom.  LABEL, as Emacs would show it in the titles' face, is drawn
from column LABEL-X in the bar's text colour."
  (pcase-let* ((h lispm-desktop-platinum-title-bar-height)
               (svg (svg-create width h))
               (`(,ground . ,ink) (lispm-desktop-pc-caption-colors palette active))
               (shadow (lispm-desktop-pc-shadow palette)))
    (lispm-desktop-look-svg-fill svg 0 0 width h ground)
    (lispm-desktop-look-svg-fill svg 0 0 width 1 shadow)
    (lispm-desktop-look-svg-fill svg 0 (1- h) width 1 shadow)
    (when label
      (let ((label (copy-sequence label)))
        (add-face-text-property 0 (length label) `(:foreground ,ink) nil label)
        (lispm-desktop-platinum-svg-content
         svg label label-x 1 (- h 2) ink
         (if active 'lispm-desktop-platinum-title 'lispm-desktop-platinum-title-inactive))))
    svg))

(cl-defmethod lispm-desktop-pc-draw-system-box (palette &context (lispm-desktop-look (eql os2))
                                                        &optional pressed icon)
  "Return an SVG of the system menu button, the window's ICON on a raised
square, between the title bar's grey lines.  ICON is a string as Emacs
would show it, its image included, or nil; PRESSED pushes it in."
  (let* ((w lispm-desktop-os2-button-width)
         (h lispm-desktop-platinum-title-bar-height)
         (svg (svg-create w h))
         (shadow (lispm-desktop-pc-shadow palette)))
    (lispm-desktop-look-svg-fill svg 0 0 w h shadow)
    (lispm-desktop-pc-svg-raise svg 0 1 w (- h 2) palette (and pressed :pushed))
    (if icon
        (lispm-desktop-platinum-svg-content svg icon 1 2 (- h 4) (plist-get palette :text)
                                            'lispm-desktop-platinum-list)
      (lispm-desktop-look-svg-fill svg 4 9 10 2 (plist-get palette :text)))
    svg))

(cl-defmethod lispm-desktop-pc-draw-caption-button (palette kind &context (lispm-desktop-look (eql os2))
                                                            &optional pressed)
  "Return an SVG of a title bar button of KIND, `minimize' or `maximize',
raised between the title bar's grey lines: minimize holds a small window
marked at its corners, maximize a large one outlined.  PRESSED pushes it
in."
  (let* ((w lispm-desktop-os2-button-width)
         (h lispm-desktop-platinum-title-bar-height)
         (svg (svg-create w h))
         (ink (lispm-desktop-platinum-legible (plist-get palette :chrome)
                                              (plist-get palette :outline) (plist-get palette :text)))
         (shift (if pressed 1 0)))
    (lispm-desktop-look-svg-fill svg 0 0 w h (lispm-desktop-pc-shadow palette))
    (lispm-desktop-pc-svg-raise svg 0 1 w (- h 2) palette (and pressed :pushed))
    (if (eq kind 'minimize)
        (lispm-desktop-look-svg-pixels svg (+ 6 shift) (+ 7 shift)
                                       '("##..##" "#....#" "......" "......" "#....#" "##..##")
                                       `((?# . ,ink)))
      (lispm-desktop-look-svg-outline svg (+ 5 shift) (+ 6 shift) 8 8 ink))
    svg))

;;;; Borders

(cl-defmethod lispm-desktop-platinum-draw-window-frame (palette width height
                                                                &context (lispm-desktop-look (eql os2))
                                                                &key active)
  "Return an OS/2 2.0 sizing border WIDTH by HEIGHT pixels, in PALETTE:
lit along its outer top and left, shaded and outlined along its outer
bottom and right, the border's colour between, pale yellow round the
ACTIVE window, and a groove where the window starts."
  (let* ((w width)
         (h height)
         (svg (svg-create w h))
         (light (plist-get palette :light))
         (shadow (lispm-desktop-pc-shadow palette)))
    (lispm-desktop-look-svg-fill svg 0 0 w h (plist-get palette (if active :border :border-inactive)))
    (lispm-desktop-look-svg-edges svg 0 0 w h (list light) (list (plist-get palette :outline) shadow))
    ;; The groove round the window: shaded where it is lit outside.
    (lispm-desktop-look-svg-fill svg 5 1 1 (- h 7) shadow)
    (lispm-desktop-look-svg-fill svg (- w 7) 1 1 (- h 7) light)
    (lispm-desktop-look-svg-fill svg 5 (- h 7) (- w 11) 1 light)
    svg))

(cl-defmethod lispm-desktop-platinum-draw-grow-box (palette height
                                                            &context (lispm-desktop-look (eql os2)))
  "Return the corner a window resizes from, sixteen pixels square, raised
as OS/2's buttons were, a small window in a large one marking it."
  (let* ((s 16)
         (svg (svg-create s height))
         (y (/ (- height s) 2))
         (ink (plist-get palette :text)))
    (lispm-desktop-look-svg-fill svg 0 0 s height (plist-get palette :chrome))
    (lispm-desktop-pc-svg-raise svg 0 y s s palette)
    (lispm-desktop-look-svg-outline svg 4 (+ y 4) 9 9 ink)
    (lispm-desktop-look-svg-outline svg 4 (+ y 8) 5 5 ink)
    svg))

;;;; The menu bar and menus

(cl-defmethod lispm-desktop-platinum-draw-menu-title (palette width
                                                              &context (lispm-desktop-look (eql os2))
                                                              &key label highlighted corner marked)
  "Return a stretch of an OS/2 2.0 menu bar WIDTH pixels wide, in PALETTE:
pale grey over a grey line, LABEL centred, HIGHLIGHTED in the highlight
colour.  CORNER and MARKED are Platinum's, not drawn."
  (ignore corner marked)
  (let* ((h lispm-desktop-platinum-menu-bar-height)
         (svg (svg-create width h)))
    (lispm-desktop-look-svg-fill svg 0 0 width (1- h) (plist-get palette :menu))
    (lispm-desktop-look-svg-fill svg 0 (1- h) width 1 (lispm-desktop-pc-shadow palette))
    (when highlighted
      (lispm-desktop-look-svg-fill svg 0 1 width (- h 3) (plist-get palette :highlight)))
    (when label
      (let ((ink (plist-get palette (if highlighted :highlight-text :menu-text)))
            (label (copy-sequence label)))
        (add-face-text-property 0 (length label) `(:foreground ,ink) nil label)
        (lispm-desktop-platinum-svg-content
         svg label
         (/ (- width (lispm-desktop-platinum-content-width label 'lispm-desktop-platinum-menu-title)) 2)
         0 (1- h) ink 'lispm-desktop-platinum-menu-title)))
    svg))

;;;; Message boxes

(cl-defmethod lispm-desktop-pc-svg-alert-icon (svg icon x y &context (lispm-desktop-look (eql os2)))
  "Draw OS/2 2.0's message box ICON on SVG, its top left at X, Y:
`note' a white i on a blue disc, `caution' an exclamation mark on a
yellow triangle, `stop' a white cross on a red disc."
  (pcase icon
    ('note
     (svg-circle svg (+ x 16) (+ y 16) 14.5 :fill "#0000FF" :stroke "#000080" :stroke-width 1)
     (svg-text svg "i" :x (+ x 16) :y (+ y 24) :text-anchor "middle" :fill "#FFFFFF"
               :font-family "Times New Roman, Times, serif" :font-size 22 :font-weight "bold"))
    ('stop
     (svg-circle svg (+ x 16) (+ y 16) 14.5 :fill "#FF0000" :stroke "#800000" :stroke-width 1)
     (svg-line svg (+ x 10) (+ y 10) (+ x 22) (+ y 22) :stroke "#FFFFFF" :stroke-width 4)
     (svg-line svg (+ x 22) (+ y 10) (+ x 10) (+ y 22) :stroke "#FFFFFF" :stroke-width 4))
    (_
     (svg-polygon svg (list (cons (+ x 16) (+ y 1.5)) (cons (+ x 31) (+ y 29.5)) (cons (+ x 1) (+ y 29.5)))
                  :fill "#FFFF00" :stroke "#000000" :stroke-width 1)
     (svg-text svg "!" :x (+ x 16) :y (+ y 26) :text-anchor "middle" :fill "#000000"
               :font-family "Helvetica, Arial, sans-serif" :font-size 18 :font-weight "bold"))))

;;;; Controls

(cl-defmethod lispm-desktop-platinum-draw-group-edge (palette width
                                                              &context (lispm-desktop-look (eql os2))
                                                              &optional title bottom)
  "Return an edge of OS/2 2.0's group box WIDTH pixels wide, grooved: a
grey line beside a lit one, TITLE breaking the top near its left end;
BOTTOM draws the bottom edge instead."
  (let* ((h (if bottom (default-line-height) lispm-desktop-platinum-group-title-height))
         (y (if bottom 1 (/ h 2)))
         (svg (svg-create width h))
         (shadow (lispm-desktop-pc-shadow palette))
         (light (plist-get palette :light)))
    (if bottom
        (progn (lispm-desktop-look-svg-fill svg 0 0 1 (1+ y) shadow)
               (lispm-desktop-look-svg-fill svg 1 0 1 y light)
               (lispm-desktop-look-svg-fill svg (- width 2) 0 1 (1+ y) shadow)
               (lispm-desktop-look-svg-fill svg (1- width) 0 1 (+ 2 y) light))
      (lispm-desktop-look-svg-fill svg 0 y 1 (- h y) shadow)
      (lispm-desktop-look-svg-fill svg 1 (1+ y) 1 (- h y 1) light)
      (lispm-desktop-look-svg-fill svg (- width 2) y 1 (- h y) shadow)
      (lispm-desktop-look-svg-fill svg (1- width) y 1 (- h y) light))
    (lispm-desktop-look-svg-fill svg 0 y (1- width) 1 shadow)
    (lispm-desktop-look-svg-fill svg 1 (1+ y) (1- width) 1 light)
    (when title
      (let ((title-width (lispm-desktop-platinum-content-width title 'lispm-desktop-platinum-title)))
        (lispm-desktop-look-svg-fill svg 6 0 (+ title-width 8) h (plist-get palette :content))
        (lispm-desktop-platinum-svg-content svg title 10 0 h (plist-get palette :text)
                                            'lispm-desktop-platinum-title)))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-tab (palette label width
                                                       &context (lispm-desktop-look (eql os2))
                                                       &key front face)
  "Return a notebook's tab WIDTH pixels wide reading LABEL, as OS/2 2.0's
settings notebooks drew theirs: raised, its top corners bevelled; the
tab in FRONT stands taller and opens into the page, one behind stands on
the page's lit edge."
  (let* ((face (or face 'lispm-desktop-platinum-title))
         (h lispm-desktop-platinum-tab-height)
         (w width)
         (svg (svg-create w (+ h 3)))
         (page (plist-get palette :content))
         (light (plist-get palette :light))
         (shadow (lispm-desktop-pc-shadow palette))
         (top (if front 0 2))
         (bottom (if front (+ h 2) (1- h))))
    (lispm-desktop-look-svg-fill svg 0 0 w (+ h 3) (plist-get palette :chrome))
    (unless front
      (lispm-desktop-look-svg-fill svg 0 h w 1 light)
      (lispm-desktop-look-svg-fill svg 0 (1+ h) w 2 page))
    (lispm-desktop-look-svg-fill svg 1 (1+ top) (- w 2) (- bottom top) (if front page (plist-get palette :chrome)))
    (lispm-desktop-look-svg-fill svg 3 top (- w 6) 1 light)
    (lispm-desktop-look-svg-fill svg 2 (1+ top) 1 1 light)
    (lispm-desktop-look-svg-fill svg 1 (+ top 2) 1 (- bottom top 1) light)
    (lispm-desktop-look-svg-fill svg (- w 3) (1+ top) 1 1 shadow)
    (lispm-desktop-look-svg-fill svg (- w 2) (+ top 2) 1 (- bottom top 1) shadow)
    (lispm-desktop-platinum-svg-content
     svg label (/ (- w (lispm-desktop-platinum-content-width label face)) 2) top (- h top)
     (plist-get palette :text) face)
    svg))

(cl-defmethod lispm-desktop-platinum-draw-check-box (palette on
                                                             &context (lispm-desktop-look (eql os2))
                                                             &key state focused pad)
  "Return OS/2 2.0's check box: a white square sunk into the grey, ticked
when ON; grey when STATE is `disabled', its inside grey when `pressed'.
FOCUSED dots a ring round it.  It stands where Platinum's does, at the
bottom left of fourteen pixels."
  (let* ((pad (max (or pad 0) (if focused 2 0)))
         (svg (svg-create (+ 14 (* 2 pad)) (+ 14 (* 2 pad))))
         (x pad)
         (y (+ pad 1))
         (ink (if (eq state 'disabled) (lispm-desktop-pc-shadow palette) (plist-get palette :outline))))
    (when focused
      (lispm-desktop-look-svg-dotted svg (- x 2) (- y 2) 17 17 (plist-get palette :text)))
    (lispm-desktop-look-svg-fill svg x y 13 13
                                 (if (eq state 'pressed) (plist-get palette :chrome) (plist-get palette :content)))
    (lispm-desktop-look-svg-edges svg x y 13 13 (list (lispm-desktop-pc-shadow palette) ink)
                                  (list (plist-get palette :light)))
    (when on
      (svg-polyline svg (list (cons (+ x 3) (+ y 6.5)) (cons (+ x 5.5) (+ y 9.5)) (cons (+ x 10) (+ y 3)))
                    :fill "none" :stroke ink :stroke-width 2))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-list-check-box (palette state
                                                                  &context (lispm-desktop-look (eql os2)))
  "Return a list's twelve-pixel check box in OS/2 2.0's form: ticked when
STATE is `on', a bar across it when `mixed'."
  (let ((svg (svg-create 12 12))
        (ink (plist-get palette :outline)))
    (lispm-desktop-look-svg-fill svg 0 0 12 12 (plist-get palette :content))
    (lispm-desktop-look-svg-edges svg 0 0 12 12 (list (lispm-desktop-pc-shadow palette) ink)
                                  (list (plist-get palette :light)))
    (pcase state
      ('on (svg-polyline svg '((2.5 . 6) (4.5 . 8.5) (9 . 3)) :fill "none" :stroke ink :stroke-width 2))
      ('mixed (lispm-desktop-look-svg-fill svg 3 5 6 2 ink)))
    svg))

(cl-defmethod lispm-desktop-platinum-svg-progress-bar (svg x y width fraction palette
                                                           &context (lispm-desktop-look (eql os2)))
  "Draw on SVG at X, Y a progress bar WIDTH pixels wide, sunk into the
grey, FRACTION of it filled in the title bar's colour."
  (let* ((h lispm-desktop-platinum-progress-height)
         (inner (- width 2))
         (filled (round (* inner (min 1.0 (max 0.0 fraction))))))
    (lispm-desktop-look-svg-fill svg x y width h (plist-get palette :content))
    (lispm-desktop-look-svg-edges svg x y width h (list (lispm-desktop-pc-shadow palette))
                                  (list (plist-get palette :light)))
    (lispm-desktop-look-svg-fill svg (1+ x) (1+ y) filled (- h 2) (plist-get palette :title))))

(cl-defmethod lispm-desktop-platinum-draw-list-header (palette columns
                                                               &context (lispm-desktop-look (eql os2))
                                                               &optional label-x)
  "Return a details view's column titles in PALETTE, as OS/2 2.0 drew them:
each centred over a grey line, on the view's ground.  COLUMNS lists
\(LABEL WIDTH SORTED [X]); the sorted column's title is bold.  LABEL-X is
not used: titles are centred."
  (ignore label-x)
  (let* ((h lispm-desktop-platinum-list-header-height)
         (width (+ 16 (apply #'+ (mapcar #'cadr columns))))
         (svg (svg-create width h))
         (x 0))
    (lispm-desktop-look-svg-fill svg 0 0 width h (plist-get palette :content))
    (pcase-dolist (`(,label ,w ,sorted) columns)
      (lispm-desktop-look-svg-fill svg (+ x 4) (- h 3) (- w 8) 1 (lispm-desktop-pc-shadow palette))
      (when label
        (let ((label (if sorted (propertize label 'face 'bold) label)))
          (lispm-desktop-platinum-svg-content
           svg label (+ x (/ (- w (lispm-desktop-platinum-content-width label 'lispm-desktop-platinum-list)) 2))
           0 (- h 3) (plist-get palette :text) 'lispm-desktop-platinum-list)))
      (lispm-desktop-look-svg-fill svg (+ x w -1) 2 1 (- h 4) (lispm-desktop-pc-shadow palette))
      (setq x (+ x w)))
    svg))

;;;; Scroll bars

(cl-defmethod lispm-desktop-platinum-scroll-thumb (height start end min max
                                                          &context (lispm-desktop-look (eql os2)))
  "Return the thumb of an OS/2 2.0 scroll bar HEIGHT pixels tall, as long
as the part of a buffer from MIN to MAX the window shows, START to END;
nil when everything shows, or the track is too short."
  (let* ((layout (lispm-desktop-platinum-scroll-layout height))
         (track (plist-get layout :track-length))
         (size (- max min))
         (shown (- end start)))
    (when (and (> size 0) (> track 24) (or (> start min) (< end max)))
      (let* ((length (min track (max 12 (round (* track (/ (float shown) size))))))
             (fraction (/ (float (- start min)) (max 1 (- size shown)))))
        (cons (+ (plist-get layout :track) (round (* (- track length) (min 1.0 fraction))))
              length)))))

(cl-defmethod lispm-desktop-platinum-draw-scroll-bar (palette height thumb active
                                                              &context (lispm-desktop-look (eql os2)))
  "Return OS/2 2.0's scroll bar HEIGHT pixels tall, in PALETTE: raised arrow
buttons at both ends, a stippled track between, THUMB raised on it.
Nothing to scroll, THUMB nil, greys the arrows; an inactive bar, ACTIVE
nil, shows no thumb."
  (let* ((w lispm-desktop-platinum-scroll-width)
         (svg (svg-create w height))
         (layout (lispm-desktop-platinum-scroll-layout height))
         (b lispm-desktop-pc-scroll-button)
         (live (and active thumb))
         (shadow (lispm-desktop-pc-shadow palette)))
    (lispm-desktop-look-svg-fill svg 0 0 w height (lispm-desktop-os2--stipple svg palette))
    (dolist (button (list (cons (plist-get layout :up) 'up) (cons (plist-get layout :down) 'down)))
      (lispm-desktop-pc-svg-raise svg 0 (car button) w b palette)
      (lispm-desktop-look-svg-arrow svg 4 (+ (car button) 6) 4 (cdr button)
                                    (if live (plist-get palette :text) shadow)))
    (when live
      (lispm-desktop-pc-svg-raise svg 0 (car thumb) w (cdr thumb) palette))
    svg))

;;;; The desktop

(cl-defmethod lispm-desktop--draw-icon (spec x y selected label-width family size ascent
                                             &context (lispm-desktop-look (eql os2)))
  "Return SVG of the icon SPEC with its title under it, as the Workplace
Shell drew icons on the desktop: the title straight on the desktop, and,
when SELECTED, icon and title set on the highlight colour.  See
`lispm-desktop--draw-icon'."
  (let* ((palette lispm-desktop-platinum-current-palette)
         (desktop (lispm-desktop--color))
         (ink (if selected
                  (plist-get palette :highlight-text)
                (lispm-desktop-platinum-legible desktop (plist-get palette :text) (plist-get palette :content))))
         (icon-left (- (+ x (/ lispm-desktop-column 2)) (/ lispm-desktop-icon-size 2)))
         (highlight (and selected (plist-get palette :highlight))))
    (concat
     (when selected
       (format "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"%s\"/>"
               (- icon-left 2) (- y 2) (+ lispm-desktop-icon-size 4) (+ lispm-desktop-icon-size 4) highlight))
     (lispm-desktop--nest (lispm-desktop--item-icon spec) nil icon-left y)
     (lispm-desktop--icon-label spec x y label-width family size ascent highlight ink))))

(provide 'lispm-desktop-os2)
;;; lispm-desktop-os2.el ends here
