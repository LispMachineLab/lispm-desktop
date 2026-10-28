;;; lispm-desktop-win3.el --- Windows 3.2's look for the chrome  -*- lexical-binding: t; -*-

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

;; The look `win3': the chrome drawn as Windows 3.2 drew its windows,
;; the Simplified Chinese release of Windows 3.1, which looked as 3.1 did.
;; Measured on Windows 3.1 running in an emulator:
;;
;;   captions    eighteen pixels of the active title colour, the title
;;               centred in the bold system font, a black line under them
;;   boxes       the control-menu box at the left, a bar in a grey square,
;;               which drops the window's menu; the minimize and maximize
;;               buttons at the right, grey, lit along the top and left and
;;               shaded two pixels deep along the bottom and right, holding
;;               black arrowheads; black lines part them from the caption
;;   borders     a sizing border four pixels deep, outlined in black,
;;               notched where the caption ends
;;   menus       white, outlined in black, the item under the mouse in the
;;               highlight colour, light on dark
;;   controls    push buttons outlined in black with their corners cut, a
;;               default button's outline two pixels thick, the keyboard's
;;               control marked by a dotted rectangle; check boxes white
;;               squares crossed when checked; scroll bars of square thumbs
;;               between arrow buttons at either end
;;
;; Every colour comes from the palette; the win3 theme supplies Windows
;; 3.2's own scheme.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-look)
(require 'lispm-desktop-pc)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-popup)
(require 'lispm-desktop-surface)
(require 'lispm-desktop-i18n)

(defconst lispm-desktop-win3-button-width 18
  "Width in pixels of a caption's buttons and of its control-menu box.")

(lispm-ui-look-define
 'win3
 :title "Windows 3.2"
 :description (lispm-desktop-tr-noop "Windows 3.2's look: navy captions, the control-menu box, sizing borders")
 :theme 'win3
 :icons '("win3")
 :system-families '("Microsoft Sans Serif" "MS Sans Serif" "Arial" "Helvetica")
 :list-families '("Microsoft Sans Serif" "MS Sans Serif" "Arial" "Helvetica")
 :family 'pc
 ;; See lispm-desktop-pc.el.  The boxes have a black line each.
 :metrics `(:box-width ,(1+ lispm-desktop-win3-button-width)
            :bevels (:raised ((:outline :light) (:outline :shadow)) :pushed ((:outline :shadow) (:outline))
                     :button ((:light) (:shadow :shadow)) :button-pushed ((:shadow) ())
                     :tab ((:outline) (:outline)) :dialog ((:outline) (:outline)))
            :button-cut t :button-focus 3
            :alert-gap 10 :alert-bottom 14
            :dialog-ground :border :dialog-inset 3 :dialog-body :content
            :strip-lines (:outline) :strip-ink :outline :tree-box :shadow :group-side (:text :content)
            :radio-dot 2.5 :child-frame-border :outline :dividers (:border :outline :outline)
            :menu-inset 0 :menu-separator (1 :menu-text))
 :box-help (list (lispm-desktop-tr-noop "Control menu: restore, minimize, maximize, close")
                 (lispm-desktop-tr-noop "Minimize: roll up to the caption")
                 (lispm-desktop-tr-noop "Maximize, or restore" "Windows 3.2"))
 :system-menu (list (lispm-desktop-tr-noop "Restore" "Windows 3.2") (lispm-desktop-tr-noop "Minimize" "Windows 3.2")
                    (lispm-desktop-tr-noop "Maximize" "Windows 3.2") (lispm-desktop-tr-noop "Close" "Windows 3.2")))

;;;; Captions

(cl-defmethod lispm-desktop-pc-draw-caption (palette width &context (lispm-desktop-look (eql win3))
                                                     &optional active label label-x)
  "Return an SVG of a stretch of caption WIDTH pixels wide, in PALETTE.
It is the title bar's height: the caption's colour, ACTIVE or not, over
the black line under it.  LABEL, as Emacs would show it in the titles'
face, is drawn from column LABEL-X in the caption's text colour."
  (pcase-let* ((h lispm-desktop-platinum-title-bar-height)
               (svg (svg-create width h))
               (`(,ground . ,ink) (lispm-desktop-pc-caption-colors palette active)))
    (lispm-ui-svg-paint svg 0 0 width (1- h) ground)
    (lispm-ui-svg-paint svg 0 (1- h) width 1 (plist-get palette :outline))
    (when label
      (let ((label (copy-sequence label)))
        (add-face-text-property 0 (length label) `(:foreground ,ink) nil label)
        (lispm-ui-svg-content
         svg label label-x 0 (1- h) ink
         (if active 'lispm-ui-title 'lispm-desktop-platinum-title-inactive))))
    svg))

(cl-defmethod lispm-desktop-pc-draw-system-box (palette &context (lispm-desktop-look (eql win3))
                                                        &optional pressed _icon)
  "Return an SVG of the control-menu box, a black line closing its right.
It is a grey square holding a white bar outlined in black, which casts a
shadow down and right; PRESSED inverts the square, as the window's menu
drops from it."
  (let* ((w lispm-desktop-win3-button-width)
         (h lispm-desktop-platinum-title-bar-height)
         (svg (svg-create (1+ w) h))
         (y (/ (- h 4) 2))
         (outline (plist-get palette :outline))
         (shadow (lispm-desktop-pc-shadow palette)))
    (lispm-ui-svg-paint svg 0 0 w (1- h) (if pressed shadow (plist-get palette :chrome)))
    (lispm-ui-svg-paint svg w 0 1 h outline)
    (lispm-ui-svg-paint svg 0 (1- h) w 1 outline)
    (lispm-ui-svg-paint svg 3 (+ y 3) 13 1 (if pressed (plist-get palette :chrome) shadow))
    (lispm-ui-svg-paint svg 15 (1+ y) 1 3 (if pressed (plist-get palette :chrome) shadow))
    (lispm-ui-svg-paint svg 2 y 13 3 outline)
    (lispm-ui-svg-paint svg 3 (1+ y) 11 1 (plist-get palette :light))
    svg))

(cl-defmethod lispm-desktop-pc-draw-caption-button (palette kind &context (lispm-desktop-look (eql win3))
                                                            &optional pressed)
  "Return an SVG of a caption button of KIND, `minimize' or `maximize'.
A black line opens its left; the button is grey, lit along its top and
left and shaded two pixels deep along its bottom and right, holding a
black arrowhead, pointing down to minimize and up to maximize.  PRESSED
shades its top and left instead and moves the arrowhead a pixel down and
right."
  (let* ((w lispm-desktop-win3-button-width)
         (h lispm-desktop-platinum-title-bar-height)
         (svg (svg-create (1+ w) h))
         (outline (plist-get palette :outline))
         (shift (if pressed 1 0)))
    (lispm-ui-svg-paint svg 0 0 1 h outline)
    (lispm-ui-svg-paint svg 1 0 w (1- h) (plist-get palette :chrome))
    (lispm-ui-svg-paint svg 1 (1- h) w 1 outline)
    (lispm-desktop-pc-svg-bevel svg 1 0 w (1- h) palette (if pressed :button-pushed :button))
    (lispm-ui-svg-arrow svg (+ 6 shift) (+ 7 shift) 4 (if (eq kind 'minimize) 'down 'up)
                        (lispm-ui-legible (plist-get palette :chrome)
                                          outline (plist-get palette :text)))
    svg))

;;;; Sizing borders and the size box

(cl-defmethod lispm-desktop-platinum-draw-window-frame (palette width height
                                                                &context (lispm-desktop-look (eql win3))
                                                                &key active)
  "Return a Windows 3.2 sizing border WIDTH by HEIGHT pixels, in PALETTE.
A black outline, the border's colour, and the black line round the window
inside it; notches cross the border where the caption ends and as far
from the corners along the bottom, as Windows marked where a drag
resizes from a corner.  The window stands at
`lispm-desktop-platinum-frame-inset'; the caption runs under the outline."
  (let* ((w width)
         (h height)
         (svg (svg-create w h))
         (outline (plist-get palette :outline))
         (band (plist-get palette (if active :border :border-inactive)))
         (title lispm-desktop-platinum-title-bar-height)
         (corner (+ title 1)))
    (lispm-ui-svg-paint svg 0 0 w h band)
    (lispm-ui-svg-outline svg 0 0 w h outline)
    ;; The black line round the window, beside the caption and under it.
    (lispm-ui-svg-paint svg 5 1 1 (- h 7) outline)
    (lispm-ui-svg-paint svg (- w 7) 1 1 (- h 7) outline)
    (lispm-ui-svg-paint svg 5 (- h 7) (- w 11) 1 outline)
    ;; The corners' notches.
    (dolist (y (list corner (- h corner)))
      (lispm-ui-svg-paint svg 1 y 4 1 outline)
      (lispm-ui-svg-paint svg (- w 6) y 5 1 outline))
    (dolist (x (list corner (- w corner)))
      (lispm-ui-svg-paint svg x (- h 6) 1 5 outline))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-grow-box (palette height
                                                            &context (lispm-desktop-look (eql win3)))
  "Return Windows 3.2's size box, sixteen pixels square, in PALETTE: the
grey square where a window's scroll bars meet, raised as a button, with
a small window outlined in it."
  (let* ((s 16)
         (svg (svg-create s height))
         (y (/ (- height s) 2)))
    (lispm-ui-svg-paint svg 0 0 s height (plist-get palette :chrome))
    (lispm-desktop-pc-svg-bevel svg 0 y s s palette :raised)
    (lispm-ui-svg-outline svg 5 (+ y 5) 7 7 (plist-get palette :outline))
    (lispm-ui-svg-paint svg 5 (+ y 6) 7 1 (plist-get palette :outline))
    svg))

;;;; The menu bar and menus

(cl-defmethod lispm-desktop-platinum-draw-menu-title (palette width
                                                              &context (lispm-desktop-look (eql win3))
                                                              &key label highlighted corner marked)
  "Return a stretch of Windows 3.2's menu bar WIDTH pixels wide, in PALETTE.
The bar is the menu colour over a black line; LABEL is centred, and
HIGHLIGHTED inverts its stretch into the highlight colour, as an open
menu's title.  Screens had square corners, so CORNER is not drawn, nor
MARKED's coach mark."
  (ignore corner marked)
  (let* ((h lispm-desktop-platinum-menu-bar-height)
         (svg (svg-create width h)))
    (lispm-ui-svg-paint svg 0 0 width (1- h)
                        (plist-get palette (if highlighted :highlight :menu)))
    (lispm-ui-svg-paint svg 0 (1- h) width 1 (plist-get palette :outline))
    (when label
      (let ((ink (plist-get palette (if highlighted :highlight-text :menu-text))))
        (lispm-ui-svg-content
         svg (propertize label 'face `(:foreground ,ink))
         (/ (- width (lispm-ui-text-width label 'lispm-desktop-platinum-menu-title)) 2)
         0 (1- h) ink 'lispm-desktop-platinum-menu-title)))
    svg))

(cl-defmethod lispm-desktop-pc-menu-label (label ink &context (lispm-desktop-look (eql win3)))
  "Return a menu item's LABEL in INK alone, as Windows 3.2's menus showed
their items in the system font."
  (propertize label 'face `(:foreground ,ink)))

;;;; Message boxes

(cl-defmethod lispm-desktop-pc-svg-alert-icon (svg icon x y &context (lispm-desktop-look (eql win3)))
  "Draw Windows 3.2's message box ICON on SVG, its top left at X, Y.
`stop' is the red stop sign; `note' and `caution' are a white balloon
holding a blue i or a black exclamation mark."
  (pcase icon
    ('stop
     (svg-polygon svg (mapcar (pcase-lambda (`(,dx . ,dy)) (cons (+ x dx) (+ y dy)))
                              '((9.5 . 0.5) (22.5 . 0.5) (31.5 . 9.5) (31.5 . 22.5)
                                (22.5 . 31.5) (9.5 . 31.5) (0.5 . 22.5) (0.5 . 9.5)))
                  :fill "#FF0000" :stroke "#000000" :stroke-width 1)
     (svg-polygon svg (mapcar (pcase-lambda (`(,dx . ,dy)) (cons (+ x dx) (+ y dy)))
                              '((10.5 . 2.5) (21.5 . 2.5) (29.5 . 10.5) (29.5 . 21.5)
                                (21.5 . 29.5) (10.5 . 29.5) (2.5 . 21.5) (2.5 . 10.5)))
                  :fill "none" :stroke "#FFFFFF" :stroke-width 1)
     (svg-text svg "STOP" :x (+ x 16) :y (+ y 20) :text-anchor "middle" :fill "#FFFFFF"
               :font-family "Arial, Helvetica, sans-serif" :font-size 9 :font-weight "bold"))
    (_
     (svg-path svg `((moveto ((,(+ x 4.5) . ,(+ y 2.5))))
                     (lineto ((,(+ x 27.5) . ,(+ y 2.5)) (,(+ x 30.5) . ,(+ y 5.5))
                              (,(+ x 30.5) . ,(+ y 20.5)) (,(+ x 27.5) . ,(+ y 23.5))
                              (,(+ x 14.5) . ,(+ y 23.5)) (,(+ x 6.5) . ,(+ y 30.5))
                              (,(+ x 8.5) . ,(+ y 23.5)) (,(+ x 4.5) . ,(+ y 23.5))
                              (,(+ x 1.5) . ,(+ y 20.5)) (,(+ x 1.5) . ,(+ y 5.5))))
                     (closepath))
               :fill "#FFFFFF" :stroke "#000000" :stroke-width 1)
     (svg-text svg (if (eq icon 'note) "i" "!") :x (+ x 16) :y (+ y 20) :text-anchor "middle"
               :fill (if (eq icon 'note) "#0000FF" "#000000")
               :font-family "Times New Roman, Times, serif" :font-size 20 :font-weight "bold"))))

;;;; Controls

(cl-defmethod lispm-ui-draw-group-edge (palette width
                                                &context (lispm-desktop-look (eql win3))
                                                &optional title bottom)
  "Return an edge of Windows 3.2's group box WIDTH pixels wide: a black
line, TITLE breaking the top one near its left end; BOTTOM draws the
bottom edge instead."
  (let* ((h (if bottom (default-line-height) lispm-ui-group-title-height))
         (y (if bottom 1 (/ h 2)))
         (svg (svg-create width h))
         (ink (plist-get palette :text)))
    (if bottom
        (progn (lispm-ui-svg-paint svg 0 0 1 (1+ y) ink)
               (lispm-ui-svg-paint svg (1- width) 0 1 (1+ y) ink))
      (lispm-ui-svg-paint svg 0 y 1 (- h y) ink)
      (lispm-ui-svg-paint svg (1- width) y 1 (- h y) ink))
    (lispm-ui-svg-paint svg 0 y width 1 ink)
    (when title
      (let ((title-width (lispm-ui-text-width title 'lispm-ui-title)))
        (lispm-ui-svg-paint svg 6 0 (+ title-width 8) h (plist-get palette :content))
        (lispm-ui-svg-content svg title 10 0 h ink 'lispm-ui-title)))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-tab (palette label width
                                                       &context (lispm-desktop-look (eql win3))
                                                       &key front face)
  "Return a tab WIDTH pixels wide reading LABEL, as tabbed dialogs of the
Windows 3.x days drew them: square, lit along the top and left, black
down the right over a shaded line, the top corners cut.  The tab in
FRONT stands two pixels taller and opens into the pane; one behind
stands on the pane's lit edge."
  (let* ((face (or face 'lispm-ui-title))
         (h lispm-desktop-platinum-tab-height)
         (w width)
         (svg (svg-create w (+ h 3)))
         (pane (plist-get palette :content))
         (light (plist-get palette :light))
         (shadow (lispm-desktop-pc-shadow palette))
         (outline (plist-get palette :outline))
         (top (if front 0 2))
         (bottom (if front (+ h 2) (1- h))))
    (lispm-ui-svg-paint svg 0 0 w (+ h 3) (plist-get palette :chrome))
    (unless front
      (lispm-ui-svg-paint svg 0 h w 1 light)
      (lispm-ui-svg-paint svg 0 (1+ h) w 2 pane))
    (lispm-ui-svg-paint svg 1 (1+ top) (- w 2) (- bottom top) pane)
    (lispm-ui-svg-paint svg 2 top (- w 4) 1 light)
    (lispm-ui-svg-paint svg 1 (1+ top) 1 (- bottom top) light)
    (lispm-ui-svg-paint svg (- w 2) (1+ top) 1 (- bottom top) outline)
    (lispm-ui-svg-paint svg (- w 3) (1+ top) 1 (- bottom top) shadow)
    (when front
      (lispm-ui-svg-paint svg 0 h 1 1 light)
      (lispm-ui-svg-paint svg (1- w) h 1 1 light))
    (lispm-ui-svg-content
     svg label (/ (- w (lispm-ui-text-width label face)) 2) top (- h top)
     (plist-get palette :text) face)
    svg))

(cl-defmethod lispm-ui-draw-check-box (palette on
                                               &context (lispm-desktop-look (eql win3))
                                               &key state focused pad)
  "Return Windows 3.2's check box: a white square outlined in black, crossed
when ON; grey when STATE is `disabled', its inside grey when `pressed'.
FOCUSED dots a ring round it.  It stands where Platinum's does, at the
bottom left of fourteen pixels."
  (let* ((pad (max (or pad 0) (if focused 2 0)))
         (svg (svg-create (+ 14 (* 2 pad)) (+ 14 (* 2 pad))))
         (x pad)
         (y (+ pad 1))
         (ink (if (eq state 'disabled) (lispm-desktop-pc-shadow palette) (plist-get palette :outline))))
    (when focused
      (lispm-ui-svg-dotted svg (- x 2) (- y 2) 17 17 (plist-get palette :text)))
    (lispm-ui-svg-paint svg x y 13 13 ink)
    (lispm-ui-svg-paint svg (1+ x) (1+ y) 11 11
                        (if (eq state 'pressed) (plist-get palette :chrome) (plist-get palette :content)))
    (when on
      (svg-line svg (+ x 1.5) (+ y 1.5) (+ x 11.5) (+ y 11.5) :stroke ink :stroke-width 1)
      (svg-line svg (+ x 11.5) (+ y 1.5) (+ x 1.5) (+ y 11.5) :stroke ink :stroke-width 1))
    svg))

(cl-defmethod lispm-ui-draw-list-check-box (palette state
                                                    &context (lispm-desktop-look (eql win3)))
  "Return a list's twelve-pixel check box in Windows 3.2's form, crossed
when STATE is `on', a bar across it when `mixed'."
  (let ((svg (svg-create 12 12))
        (ink (plist-get palette :outline)))
    (lispm-ui-svg-paint svg 0 0 12 12 ink)
    (lispm-ui-svg-paint svg 1 1 10 10 (plist-get palette :content))
    (pcase state
      ('on (svg-line svg 1.5 1.5 10.5 10.5 :stroke ink :stroke-width 1)
           (svg-line svg 10.5 1.5 1.5 10.5 :stroke ink :stroke-width 1))
      ('mixed (lispm-ui-svg-paint svg 3 5 6 2 ink)))
    svg))

(cl-defmethod lispm-desktop-platinum-svg-progress-bar (svg x y width fraction palette
                                                           &context (lispm-desktop-look (eql win3)))
  "Draw on SVG at X, Y Windows 3.2 Setup's gauge WIDTH pixels wide: a black
frame round white, FRACTION of it filled in the highlight colour."
  (let* ((h lispm-desktop-platinum-progress-height)
         (inner (- width 2))
         (filled (round (* inner (min 1.0 (max 0.0 fraction))))))
    (lispm-ui-svg-paint svg x y width h (plist-get palette :outline))
    (lispm-ui-svg-paint svg (1+ x) (1+ y) inner (- h 2) (plist-get palette :content))
    (lispm-ui-svg-paint svg (1+ x) (1+ y) filled (- h 2) (plist-get palette :highlight))))

(cl-defmethod lispm-desktop-platinum-draw-list-header (palette columns
                                                               &context (lispm-desktop-look (eql win3))
                                                               &optional label-x)
  "Return a list's column headings as buttons in a row, in PALETTE.
COLUMNS lists (LABEL WIDTH SORTED [X]); the sorted column's heading is
drawn pushed in.  LABEL-X places labels, by default eight pixels in."
  (let* ((h lispm-desktop-platinum-list-header-height)
         (width (+ 16 (apply #'+ (mapcar #'cadr columns))))
         (svg (svg-create width h))
         (x 0))
    (pcase-dolist (`(,label ,w ,sorted ,x-in) (append columns '((nil 16 nil))))
      (lispm-ui-svg-bevel-button svg x 0 palette w h :state (and sorted 'pressed))
      (when label
        (lispm-ui-svg-content svg label (+ x (or x-in label-x 8) (if sorted 1 0)) 0 h
                              (plist-get palette :text) 'lispm-ui-list))
      (setq x (+ x w)))
    (lispm-ui-svg-arrow svg (- width 12) 8 4 'down (plist-get palette :text))
    svg))

;;;; Scroll bars

(cl-defmethod lispm-desktop-platinum-scroll-thumb (height start end min max
                                                          &context (lispm-desktop-look (eql win3)))
  "Return the thumb of a Windows 3.2 scroll bar HEIGHT pixels tall: a
square as wide as the bar, standing as far along the track as START is
along a buffer from MIN to MAX, END showing last; nil when everything
shows, or the track is too short."
  (let* ((layout (lispm-desktop-platinum-scroll-layout height))
         (track (plist-get layout :track-length))
         (size (- max min))
         (shown (- end start))
         (length lispm-desktop-platinum-scroll-width))
    (when (and (> size 0) (> track (* 2 length)) (or (> start min) (< end max)))
      (let ((fraction (/ (float (- start min)) (max 1 (- size shown)))))
        (cons (+ (plist-get layout :track) (round (* (- track length) (min 1.0 fraction))))
              length)))))

(defun lispm-desktop-win3--svg-scroll-button (svg y palette direction enabled)
  "Draw on SVG from row Y a scroll bar's arrow button pointing DIRECTION,
in PALETTE: outlined in black, raised, its arrowhead black when ENABLED
and grey when not."
  (lispm-desktop-pc-svg-raise svg 0 y lispm-desktop-platinum-scroll-width lispm-desktop-pc-scroll-button palette)
  (lispm-ui-svg-arrow svg 4 (+ y 6) 4 direction
                      (if enabled (plist-get palette :outline) (lispm-desktop-pc-shadow palette))))

(cl-defmethod lispm-desktop-platinum-draw-scroll-bar (palette height thumb active
                                                              &context (lispm-desktop-look (eql win3)))
  "Return Windows 3.2's scroll bar HEIGHT pixels tall, in PALETTE: arrow
buttons at both ends, the grey track between, outlined in black, and
THUMB a raised square on it.  Nothing to scroll, THUMB nil, greys the
arrows; an inactive bar, ACTIVE nil, shows no thumb."
  (let* ((w lispm-desktop-platinum-scroll-width)
         (svg (svg-create w height))
         (layout (lispm-desktop-platinum-scroll-layout height))
         (live (and active thumb)))
    (lispm-ui-svg-paint svg 0 0 w height (plist-get palette :chrome))
    (lispm-ui-svg-outline svg 0 0 w height (plist-get palette :outline))
    (lispm-desktop-win3--svg-scroll-button svg (plist-get layout :up) palette 'up live)
    (lispm-desktop-win3--svg-scroll-button svg (plist-get layout :down) palette 'down live)
    (when live
      (lispm-desktop-pc-svg-bevel svg 0 (car thumb) w (cdr thumb) palette :raised))
    svg))

;;;; The desktop

(cl-defmethod lispm-desktop--draw-icon (spec x y selected label-width family size ascent
                                             &context (lispm-desktop-look (eql win3)))
  "Return SVG of the icon SPEC with its title under it, as Windows 3.2
drew icons on the desktop: the title on the desktop's colour, or, when
SELECTED, in the caption's colours.  See `lispm-desktop--draw-icon'."
  (let* ((palette lispm-ui-current-palette)
         (ground (if selected (plist-get palette :title) (lispm-desktop--color)))
         (ink (if selected
                  (plist-get palette :title-text)
                (lispm-ui-legible ground (plist-get palette :text) (plist-get palette :content)))))
    (concat
     (lispm-desktop--nest (lispm-desktop--item-icon spec) nil
                          (- (+ x (/ lispm-desktop-column 2)) (/ lispm-desktop-icon-size 2)) y)
     (lispm-desktop--icon-label spec x y label-width family size ascent ground ink))))

;;;; Faces

(cl-defmethod lispm-desktop-platinum-apply-look-faces (palette &context (lispm-desktop-look (eql win3)))
  "Set the faces the PC looks set, see lispm-desktop-pc.el, in PALETTE, and
raise buttons as Windows 3.2 did, lit along their top and left."
  (cl-call-next-method)
  (set-face-attribute 'lispm-ui-button nil
                      :background (plist-get palette :chrome) :foreground (plist-get palette :text)
                      :box `(:line-width (1 . 1) :color ,(plist-get palette :light) :style released-button)))

(provide 'lispm-desktop-win3)
;;; lispm-desktop-win3.el ends here
