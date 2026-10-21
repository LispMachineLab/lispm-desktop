;;; lispm-desktop-glow.el --- Glowing looks for the chrome: phosphor and neon  -*- lexical-binding: t; -*-

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

;; Two looks of no real system, drawn alike: lines of light on dark glass.
;;
;;   phosphor   a green phosphor screen: scanned title bars with ridges
;;              either side of the title, Lisp falling down the desktop
;;   neon       neon over a city at night: title bars from magenta to
;;              violet, the title at their left, cyan lines, magenta
;;              accents, a signal split into cyan and magenta ghosts
;;
;; Both keep Mac OS 9's layout, as every look does.  Windows, menus and
;; buttons have their top left and bottom right corners cut; lines glow;
;; a window's two square corners carry bracket marks.  Both are made of
;; glass (lispm-desktop-glass.el): their themes paint windows in colours
;; dark enough to see through, and the corners cut away in black, which
;; glass makes clear.  Each brings a desktop picture of its own, under
;; pictures/, which a picture of the user's own replaces.
;;
;; Colours come from the palette.  Besides Platinum's, the themes name
;; the parts these looks add, each falling back on a colour every palette
;; has, so any theme draws them:
;;
;;   :glow       lines and their glow             (:accent)
;;   :hot        bracket marks, accents           (:highlight)
;;   :title-end  where an active title bar's gradient ends, and the bar
;;               of light down a menu's left      (:title)
;;   :bar :bar-text :bar-line  the menu bar, its titles and the line
;;               under it                         (:desktop :menu-text :glow)
;;   :track      a scroll bar's track             (:chrome)
;;   :pressed :pressed-text  a button held down   (:highlight-dark ...)
;;   :danger     the stop alert's sign            (:hot)
;;
;; A look glows by its :glowing property, which the drawing methods here
;; are specialized on.  The looks differ by properties of theirs, see
;; `lispm-desktop-glow-style':
;; :title-style `scan' or `gradient', :glitch for the split signal, and
;; :solid-thumb for scroll thumbs filled rather than outlined.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-look)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-popup)
(require 'lispm-desktop-surface)
(require 'lispm-desktop-i18n)

(defconst lispm-desktop-glow-families
  '("JetBrains Mono" "SF Mono" "Roboto Mono" "Menlo" "Monaco" "DejaVu Sans Mono")
  "Font families of the glowing looks' titles, menus and lists, the first
installed one winning.")

(lispm-desktop-look-define
 'phosphor
 :title "Phosphor"
 :description (lispm-desktop-tr-noop "Green phosphor on black glass, Lisp falling down the desktop")
 :theme 'phosphor
 :system-families lispm-desktop-glow-families
 :list-families lispm-desktop-glow-families
 :glowing t
 :glass 4.0
 :picture "pictures/phosphor.jpg"
 :title-style 'scan)

(lispm-desktop-look-define
 'neon
 :title "Neon"
 :description (lispm-desktop-tr-noop "Cyan, magenta and violet neon over a city at night")
 :theme 'neon
 :system-families lispm-desktop-glow-families
 :list-families lispm-desktop-glow-families
 :glowing t
 :glass 1.9
 :picture "pictures/neon.jpg"
 :title-style 'gradient
 :glitch t
 :solid-thumb t)

(defconst lispm-desktop-glow-cut 8
  "Pixels cut off a window's top left and bottom right corners.")

(defconst lispm-desktop-glow-box-width 20
  "Width in pixels of a title bar's boxes, the gaps about them included.")

(defconst lispm-desktop-glow-barcode '(2 1 1 3 1 2 1 1 2 3 1 1 2 1 3 1 1 2)
  "Widths of the bars of the data strip on a neon title bar.")

;;;; Colours and parts

(defun lispm-desktop-glow-p ()
  "Return non-nil while the look in use glows, as phosphor and neon do:
the drawing methods of this file are specialized on it."
  (and (lispm-desktop-look-get :glowing) t))

(defun lispm-desktop-glow-style (property)
  "Return PROPERTY of the look in use, see `lispm-desktop-look-define'."
  (lispm-desktop-look-get property))

(defun lispm-desktop-glow-color (palette key)
  "Return the colour KEY of PALETTE, falling back as the Commentary says."
  (or (plist-get palette key)
      (pcase key
        (:glow (plist-get palette :accent))
        (:hot (plist-get palette :highlight))
        (:title-end (plist-get palette :title))
        (:bar (plist-get palette :desktop))
        (:bar-text (plist-get palette :menu-text))
        (:bar-line (lispm-desktop-glow-color palette :glow))
        (:track (plist-get palette :chrome))
        (:pressed (plist-get palette :highlight-dark))
        (:pressed-text (plist-get palette :highlight-text))
        (:danger (lispm-desktop-glow-color palette :hot)))
      (plist-get palette :text)))

(defun lispm-desktop-glow--on (color)
  "Return black or white, whichever stands out on COLOR."
  (lispm-desktop-platinum-legible color "#000000" "#FFFFFF"))

(defun lispm-desktop-glow--image (key draw)
  "Return the image of KEY drawn by DRAW, kept apart for the look in use."
  (lispm-desktop-platinum-image (list 'glow lispm-desktop-look key) draw))

(defun lispm-desktop-glow--node (parent tag &rest attributes)
  "Append a TAG element of ATTRIBUTES, a plist, to PARENT and return it."
  (let ((node (dom-node tag (cl-loop for (key value) on attributes by #'cddr
                                     collect (cons (intern (substring (symbol-name key) 1)) value)))))
    (dom-append-child parent node)
    node))

(defun lispm-desktop-glow--defs (svg)
  "Give SVG the glow filter `glow', which its glowing parts refer to."
  (let* ((defs (lispm-desktop-glow--node svg 'defs))
         (filter (lispm-desktop-glow--node defs 'filter :id "glow" :x "-50%" :y "-50%"
                                           :width "200%" :height "200%"))
         (merge (progn (lispm-desktop-glow--node filter 'feGaussianBlur :stdDeviation "1.2" :result "b")
                       (lispm-desktop-glow--node filter 'feMerge))))
    (lispm-desktop-glow--node merge 'feMergeNode :in "b")
    (lispm-desktop-glow--node merge 'feMergeNode :in "SourceGraphic")
    svg))

(defun lispm-desktop-glow--create (width height)
  "Return an SVG WIDTH by HEIGHT pixels with the glow filter in it."
  (lispm-desktop-glow--defs (svg-create width height)))

(defun lispm-desktop-glow--glowing (svg)
  "Return a group of SVG whose parts glow."
  (lispm-desktop-glow--node svg 'g :filter "url(#glow)"))

(defun lispm-desktop-glow-chamfer (x y width height cut &optional corners)
  "Return the path of the WIDTH by HEIGHT box at X, Y, its CORNERS cut by
CUT pixels: a list of `tl', `tr', `br' and `bl'."
  (let ((tl (memq 'tl corners)) (tr (memq 'tr corners))
        (br (memq 'br corners)) (bl (memq 'bl corners))
        (r (+ x width)) (b (+ y height)))
    (concat (format "M%s,%s " (if tl (+ x cut) x) y)
            (format "L%s,%s " (if tr (- r cut) r) y)
            (if tr (format "L%s,%s " r (+ y cut)) "")
            (format "L%s,%s " r (if br (- b cut) b))
            (if br (format "L%s,%s " (- r cut) b) "")
            (format "L%s,%s " (if bl (+ x cut) x) b)
            (if bl (format "L%s,%s " x (- b cut)) "")
            (format "L%s,%s Z" x (if tl (+ y cut) y)))))

(defun lispm-desktop-glow--path (svg d &rest attributes)
  "Draw the path D on SVG with ATTRIBUTES, a plist of SVG's own."
  (apply #'lispm-desktop-glow--node svg 'path :d d attributes))

(defun lispm-desktop-glow--line (svg x1 y1 x2 y2 color &optional width dash)
  "Draw a line on SVG from X1, Y1 to X2, Y2 in COLOR, WIDTH pixels, DASH."
  (apply #'lispm-desktop-glow--node svg 'line :x1 x1 :y1 y1 :x2 x2 :y2 y2
         :stroke color :stroke-width (or width 1)
         (and dash (list :stroke-dasharray dash))))

(defun lispm-desktop-glow--clear-corner (svg x y cut corner)
  "Paint CORNER of SVG, `tl' at X, Y or `br' ending there, black for CUT
pixels: glass makes it clear."
  (lispm-desktop-glow--path
   svg (pcase corner
         ('tl (format "M%s,%s L%s,%s L%s,%s Z" x y (+ x cut) y x (+ y cut)))
         ('br (format "M%s,%s L%s,%s L%s,%s Z" x y (- x cut) y x (- y cut))))
   :fill "#000000"))

;;;; Title bars

(defun lispm-desktop-glow--caption-ground (svg palette x width total active)
  "Paint SVG's title bar ground WIDTH pixels wide from X, a stretch of a
bar TOTAL pixels wide starting at column X of it, ACTIVE or not.
Phosphor scans an active bar with lines; neon runs it from :title to
:title-end along its whole width."
  (let ((h lispm-desktop-platinum-title-bar-height))
    (if (not active)
        (lispm-desktop-look-svg-fill svg 0 0 width h (plist-get palette :title-inactive))
      (pcase (lispm-desktop-glow-style :title-style)
        ('gradient
         (let* ((defs (lispm-desktop-glow--node svg 'defs))
                (gradient (lispm-desktop-glow--node defs 'linearGradient :id "title"
                                                    :gradientUnits "userSpaceOnUse"
                                                    :x1 (- x) :y1 0 :x2 (- total x) :y2 0)))
           (lispm-desktop-glow--node gradient 'stop :offset 0 :stop-color (plist-get palette :title))
           (lispm-desktop-glow--node gradient 'stop :offset 1
                                     :stop-color (lispm-desktop-glow-color palette :title-end))
           (lispm-desktop-glow--node svg 'rect :x 0 :y 0 :width width :height h :fill "url(#title)")))
        (_
         (lispm-desktop-look-svg-fill svg 0 0 width h (plist-get palette :title))
         (cl-loop for y from 1 below (- h 2) by 3
                  do (lispm-desktop-glow--node svg 'rect :x 0 :y y :width width :height 1
                                               :fill (lispm-desktop-glow-color palette :glow)
                                               :fill-opacity 0.18)))))
    ;; The line under the bar.
    (lispm-desktop-look-svg-fill svg 0 (- h (if active 2 1)) width (if active 2 1)
                                 (if active
                                     (lispm-desktop-glow-color palette :glow)
                                   (plist-get palette :border-inactive)))
    ;; The top left corner, cut.
    (when (= x 0)
      (lispm-desktop-glow--clear-corner svg 0 0 lispm-desktop-glow-cut 'tl))))

(defun lispm-desktop-glow-draw-caption (palette x width total active &optional label label-x title-width end)
  "Return an SVG of a title bar's stretch WIDTH pixels wide, in PALETTE.
It starts at column X of a bar TOTAL pixels wide; ACTIVE or not.  LABEL,
as Emacs would show it in the titles' face, is drawn from the bar's
column LABEL-X, TITLE-WIDTH pixels wide; an active phosphor bar draws
ridges either side of it, an active neon bar the data strip before END,
where the bar's boxes begin."
  (let* ((h lispm-desktop-platinum-title-bar-height)
         (svg (lispm-desktop-glow--create width h))
         (ink (plist-get palette (if active :title-text :title-inactive-text))))
    (lispm-desktop-glow--caption-ground svg palette x width total active)
    (when active
      (pcase (lispm-desktop-glow-style :title-style)
        ('gradient
         (when end
           (let ((bx (- end x 58)))
             (dolist (bw lispm-desktop-glow-barcode)
               (when (and (>= bx 0) (< (+ bx bw) width) (< bx (- end x 8)))
                 (lispm-desktop-look-svg-fill svg bx 6 bw 9 ink))
               (setq bx (+ bx bw 1))))))
        (_
         (when (and label label-x title-width)
           (let ((left (- label-x x 10))
                 (right (+ (- label-x x) title-width 10)))
             (dolist (y '(7.5 12.5))
               (when (> left 4)
                 (lispm-desktop-glow--line svg 4 y left y ink 1))
               (when (< right (- width 4))
                 (lispm-desktop-glow--line svg right y (- width 4) y ink 1))))))))
    (when label
      (let ((label (copy-sequence label)))
        (add-face-text-property 0 (length label) `(:foreground ,ink) nil label)
        (lispm-desktop-platinum-svg-content
         (if active (lispm-desktop-glow--glowing svg) svg)
         label (- label-x x) 0 (1- h) ink
         (if active 'lispm-desktop-platinum-title 'lispm-desktop-platinum-title-inactive))))
    svg))

(defun lispm-desktop-glow-draw-box (palette kind x total active &optional pressed)
  "Return an SVG of a title bar's box of KIND, `close', `collapse' or
`zoom', standing at column X of a bar TOTAL pixels wide: a square on the
bar's ground, filled when PRESSED.  Collapse holds a bar, zoom a square."
  (let* ((w lispm-desktop-glow-box-width)
         (h lispm-desktop-platinum-title-bar-height)
         (svg (lispm-desktop-glow--create w h))
         (ink (plist-get palette (if active :title-text :title-inactive-text)))
         (accent (lispm-desktop-glow-color palette :glow))
         (bx 4.5) (by (+ (/ (- h 2 11) 2) 0.5)))
    (lispm-desktop-glow--caption-ground svg palette x w total active)
    (lispm-desktop-glow--node svg 'rect :x bx :y by :width 11 :height 11
                              :fill (if pressed ink "none") :stroke ink :stroke-width 1.5)
    (unless pressed
      (pcase kind
        ('close (when active
                  (lispm-desktop-look-svg-fill svg (+ bx 3) (+ by 3) 5 5 accent)))
        ('collapse (lispm-desktop-glow--line svg (+ bx 2.5) (+ by 5.5) (+ bx 8.5) (+ by 5.5) ink 1.5))
        ('zoom (lispm-desktop-look-svg-fill svg (+ bx 3) (+ by 3) 5 5 ink))))
    svg))

(cl-defmethod lispm-desktop-platinum-title-layout (width title-width
                                                         &context ((lispm-desktop-glow-p) (eql t))
                                                         &optional controls margins)
  "Lay a glowing title bar WIDTH pixels wide out for a title TITLE-WIDTH wide.
The close box opens it, zoom and collapse close it; phosphor centres the
title between them, neon puts it after the close box."
  (pcase-let* ((`(,close ,zoom ,collapse) (or controls '(on on on)))
               (b lispm-desktop-glow-box-width)
               (`(,lead . ,trail) (or margins '(0 . 0)))
               (lead (lispm-desktop-look-box-margin lead))
               (trail (lispm-desktop-look-box-margin trail))
               (start (+ lead (if close b (+ lispm-desktop-glow-cut 4))))
               (end (- width trail (if zoom b 0) (if collapse b 0)))
               (room (- end start 8 (if (eq (lispm-desktop-glow-style :title-style) 'gradient) 64 0)))
               (title (max 0 (min title-width room)))
               (label-x (if (eq (lispm-desktop-glow-style :title-style) 'gradient)
                            (+ start 6)
                          (+ start (max 4 (/ (- end start title) 2))))))
    (list :start start :end end :title title :label-x label-x :margins (cons lead trail)
          :ridges nil)))

(cl-defmethod lispm-desktop-platinum-draw-title-bar (width active _icon title modified controls
                                                           &context ((lispm-desktop-glow-p) (eql t))
                                                           &optional pressed status _edges margins)
  "Return a glowing title bar WIDTH pixels wide reading TITLE.
The close box closes the window, collapse rolls it up and zoom zooms it.
MODIFIED marks the title with a star; STATUS follows it."
  (pcase-let* ((label (concat title (when modified " *") (when status (concat "  " status))))
               (face (if active 'lispm-desktop-platinum-title 'lispm-desktop-platinum-title-inactive))
               (`(,close ,zoom ,collapse) controls)
               (layout (lispm-desktop-platinum-title-layout
                        width (lispm-desktop-platinum-content-width label face) controls margins))
               (`(,lead . ,trail) (plist-get layout :margins))
               (start (plist-get layout :start))
               (end (plist-get layout :end))
               (label-x (plist-get layout :label-x))
               (label (let ((label (copy-sequence label)))
                        (add-face-text-property 0 (length label) face t label)
                        (lispm-desktop-platinum-fit label (plist-get layout :title))))
               (title-width (lispm-desktop-platinum-content-width label face))
               (title-help (lispm-desktop-platinum-title-help))
               (palette lispm-desktop-platinum-current-palette)
               (b lispm-desktop-glow-box-width))
    (cl-flet ((caption (from to &optional text)
                (propertize " " 'display
                            (lispm-desktop-glow--image
                             (list 'caption from (- to from) width active
                                   (and text (lispm-desktop-platinum--content-key text))
                                   label-x title-width end)
                             (lambda ()
                               (lispm-desktop-glow-draw-caption palette from (- to from) width active
                                                                text label-x title-width end)))
                            'keymap lispm-desktop-platinum-title-map
                            'help-echo title-help))
              (button (kind x command help)
                (lispm-desktop-platinum-box-button
                 (propertize " " 'display
                             (lispm-desktop-glow--image
                              (list 'box kind x width active (eq pressed kind))
                              (lambda ()
                                (lispm-desktop-glow-draw-box palette kind x width active
                                                             (eq pressed kind)))))
                 kind command help)))
      (concat
       (if close
           (concat (when (> lead 0) (caption 0 lead))
                   (button 'close lead #'lispm-desktop-platinum-close-window
                           (lispm-desktop-tr "Close this window")))
         (caption 0 start))
       (caption start end label)
       (when zoom
         (button 'zoom end #'lispm-desktop-platinum-zoom-window
                 (lispm-desktop-tr "Zoom this window")))
       (when collapse
         (button 'collapse (+ end (if zoom b 0)) #'lispm-desktop-platinum-collapse-window
                 (lispm-desktop-tr "Collapse this window to its title bar")))
       (when (> trail 0) (caption (- width trail) width))))))

;;;; Window frames and the grow box

(cl-defmethod lispm-desktop-platinum-draw-window-frame (palette width height
                                                                &context ((lispm-desktop-glow-p) (eql t))
                                                                &key active)
  "Return a glowing window frame WIDTH by HEIGHT pixels, in PALETTE.
Black, which glass makes clear, but for a line round the window, its top
left and bottom right corners cut and glowing while ACTIVE, and bracket
marks at its two square corners.  The window stands at
`lispm-desktop-platinum-frame-inset'; its title bar and grow box cut
their corners in black, so the line shows through them."
  (let* ((w width) (h height)
         (svg (lispm-desktop-glow--create w h))
         (c lispm-desktop-glow-cut)
         (line (plist-get palette (if active :border :border-inactive)))
         (hot (lispm-desktop-glow-color palette :hot)))
    (lispm-desktop-look-svg-fill svg 0 0 w h "#000000")
    (lispm-desktop-glow--path (if active (lispm-desktop-glow--glowing svg) svg)
                              (lispm-desktop-glow-chamfer 5.5 0.5 (- w 12) (- h 7) c '(tl br))
                              :fill "none" :stroke line :stroke-width 1)
    (when active
      (lispm-desktop-glow--path svg (format "M%s,0.75 H%s V%s" (- w 22) (- w 2.75) 16)
                                :fill "none" :stroke hot :stroke-width 1.5)
      (lispm-desktop-glow--path svg (format "M2.75,%s V%s H%s" (- h 22) (- h 2.75) 18)
                                :fill "none" :stroke hot :stroke-width 1.5))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-grow-box (palette height
                                                            &context ((lispm-desktop-glow-p) (eql t)))
  "Return the glowing looks' grow box, sixteen pixels wide, in PALETTE: the
window's bottom right corner cut away in black, three hatched lines
along the cut."
  (let* ((s 16)
         (svg (lispm-desktop-glow--create s height))
         (glow (lispm-desktop-glow-color palette :glow)))
    (lispm-desktop-look-svg-fill svg 0 0 s height (lispm-desktop-glow-color palette :track))
    (lispm-desktop-glow--clear-corner svg s height lispm-desktop-glow-cut 'br)
    (dolist (d '(10 13 16))
      (lispm-desktop-glow--line svg (- s d -1) (- height 1.5) (- s 1.5) (- height d -1) glow 1))
    svg))

;;;; The menu bar and menus

(cl-defmethod lispm-desktop-platinum-draw-menu-title (palette width
                                                              &context ((lispm-desktop-glow-p) (eql t))
                                                              &key label highlighted corner marked)
  "Return a stretch of the glowing menu bar WIDTH pixels wide, in PALETTE:
dark, a glowing line under it, LABEL centred and glowing; HIGHLIGHTED
fills its stretch, as an open menu's title.  CORNER and MARKED are not
drawn."
  (ignore corner marked)
  (let* ((h lispm-desktop-platinum-menu-bar-height)
         (svg (lispm-desktop-glow--create width h)))
    (lispm-desktop-look-svg-fill svg 0 0 width h
                                 (if highlighted (plist-get palette :highlight)
                                   (lispm-desktop-glow-color palette :bar)))
    (lispm-desktop-look-svg-fill (lispm-desktop-glow--glowing svg) 0 (- h 2) width 2
                                 (lispm-desktop-glow-color palette :bar-line))
    (when label
      (let ((ink (if highlighted (plist-get palette :highlight-text)
                   (lispm-desktop-glow-color palette :bar-text))))
        (lispm-desktop-platinum-svg-content
         (if highlighted svg (lispm-desktop-glow--glowing svg))
         (propertize label 'face `(:foreground ,ink))
         (/ (- width (lispm-desktop-platinum-content-width label 'lispm-desktop-platinum-menu-title)) 2)
         0 (- h 2) ink 'lispm-desktop-platinum-menu-title)))
    svg))

(cl-defmethod lispm-desktop-popup-draw-menu (palette items width highlight
                                                     &context ((lispm-desktop-glow-p) (eql t)))
  "Return a glowing menu of ITEMS WIDTH pixels wide, in PALETTE.
On the menu colour, a bar of light down its left; the item under the
mouse, HIGHLIGHT, filled, its corners cut; key equivalents flush right
in the glow colour, arrows on those holding submenus, separators dashed,
disabled items dim.  The menu's frame draws the line round it."
  (pcase-let* ((`(,tops . ,height) (lispm-desktop-popup--rows items))
               (svg (lispm-desktop-glow--create width height))
               (icons (lispm-desktop-popup--icon-room items))
               (glow (lispm-desktop-glow-color palette :glow)))
    (lispm-desktop-look-svg-fill svg 0 0 width height (plist-get palette :menu))
    (lispm-desktop-look-svg-fill svg 0 0 3 height (lispm-desktop-glow-color palette :title-end))
    (seq-do-indexed
     (lambda (item index)
       (let ((top (nth index tops)))
         (if (lispm-desktop-popup--separator-p item)
             (lispm-desktop-glow--line svg 10 (+ top (/ lispm-desktop-popup-separator-height 2) 0.5)
                                       (- width 10) (+ top (/ lispm-desktop-popup-separator-height 2) 0.5)
                                       (plist-get palette :border-inactive) 1 "2,3")
           (let* ((h (lispm-desktop-popup--item-height))
                  (lit (eql index highlight))
                  (enabled (lispm-desktop-popup--enabled-p item))
                  (ink (cond (lit (plist-get palette :highlight-text))
                             (enabled (plist-get palette :menu-text))
                             (t (plist-get palette :border-inactive))))
                  (face (lispm-desktop-popup--face (or lit enabled)))
                  (key (and (not (plist-get item :submenu)) (lispm-desktop-popup--key item)))
                  (label (propertize (plist-get item :label) 'face `(:foreground ,ink))))
             (when lit
               (lispm-desktop-glow--path svg (lispm-desktop-glow-chamfer 5 top (- width 10) h 4 '(tl br))
                                         :fill (plist-get palette :highlight)))
             (when (plist-get item :checked)
               (lispm-desktop-platinum-svg-content svg (propertize "✓" 'face `(:foreground ,ink))
                                                   (if lispm-desktop-popup-small 5 7) top h ink face))
             (when-let* ((icon (plist-get item :icon))
                         (size (min lispm-desktop-popup-icon-size h)))
               (lispm-desktop-platinum-svg-emacs-image svg icon (lispm-desktop-popup--label-x)
                                                       (+ top (/ (- h size) 2)) size size))
             (lispm-desktop-platinum-svg-content svg label (+ (lispm-desktop-popup--label-x) icons)
                                                 top h ink face)
             (cond ((plist-get item :submenu)
                    (lispm-desktop-look-svg-arrow svg (- width 12) (+ top (/ (- h 7) 2)) 4 'right ink))
                   (key
                    (let ((key-ink (cond (lit ink) (enabled glow) (t ink))))
                      (lispm-desktop-platinum-svg-content
                       svg (propertize key 'face `(:foreground ,key-ink))
                       (- width (lispm-desktop-popup--key-margin) (lispm-desktop-popup--label-width key))
                       top h key-ink face))))))))
     items)
    svg))

;;;; Alerts

(defun lispm-desktop-glow--svg-alert-icon (svg icon x y palette)
  "Draw the alert ICON on SVG, its top left at X, Y, thirty-two pixels:
`caution' a filled triangle holding an exclamation mark, `note' a box
with its corners cut holding an i, `stop' a filled octagon holding a
cross."
  (let ((glow (lispm-desktop-glow-color palette :glow))
        (hot (lispm-desktop-glow-color palette :hot))
        (group (lispm-desktop-glow--glowing svg))
        (font "sans-serif"))
    (pcase icon
      ('stop
       (lispm-desktop-glow--path group (lispm-desktop-glow-chamfer (+ x 1) (+ y 1) 30 30 9 '(tl tr br bl))
                                 :fill (lispm-desktop-glow-color palette :danger))
       (svg-text svg "×" :x (+ x 16) :y (+ y 23) :text-anchor "middle" :fill "#FFFFFF"
                 :font-family font :font-size 22 :font-weight "bold"))
      ('note
       (lispm-desktop-glow--path group (lispm-desktop-glow-chamfer (+ x 2) (+ y 2) 28 28 7 '(tl tr br bl))
                                 :fill "none" :stroke glow :stroke-width 2)
       (svg-text svg "i" :x (+ x 16) :y (+ y 22) :text-anchor "middle" :fill glow
                 :font-family font :font-size 17 :font-weight "bold"))
      (_
       (lispm-desktop-glow--path group (format "M%s,%s L%s,%s L%s,%s Z" (+ x 16) (+ y 1) (+ x 31) (+ y 29)
                                               (+ x 1) (+ y 29))
                                 :fill hot)
       (svg-text svg "!" :x (+ x 16) :y (+ y 26) :text-anchor "middle"
                 :fill (lispm-desktop-glow--on hot)
                 :font-family font :font-size 18 :font-weight "bold")))))

(cl-defmethod lispm-desktop-popup-draw-alert (palette icon message detail buttons pressed focused
                                                      &context ((lispm-desktop-glow-p) (eql t)))
  "Return a glowing alert of ICON, MESSAGE, DETAIL and BUTTONS, in PALETTE.
Laid out as Mac OS 9's, see `lispm-desktop-popup--alert-layout': a dark
body inside a glowing line, a striped band along its top, the icon, the
message and the buttons; neon splits the line into cyan and magenta
ghosts.  PRESSED and FOCUSED are values of buttons, held down or
holding the keyboard."
  (let* ((layout (lispm-desktop-popup--alert-layout message detail buttons))
         (w (or (plist-get layout :width) lispm-desktop-popup-alert-width))
         (h (plist-get layout :height))
         (svg (lispm-desktop-glow--create w h))
         (c 10)
         (hot (lispm-desktop-glow-color palette :hot))
         (glow (lispm-desktop-glow-color palette :glow))
         (y 15))
    (lispm-desktop-look-svg-fill svg 0 0 w h "#000000")
    (lispm-desktop-glow--path svg (lispm-desktop-glow-chamfer 0 0 w h c '(tl br)) :fill (plist-get palette :menu))
    (when (lispm-desktop-glow-style :glitch)
      (lispm-desktop-glow--path svg (lispm-desktop-glow-chamfer 2.5 1.5 (- w 5) (- h 4) c '(tl br))
                                :fill "none" :stroke glow :stroke-opacity 0.6)
      (lispm-desktop-glow--path svg (lispm-desktop-glow-chamfer 1.5 2.5 (- w 5) (- h 4) c '(tl br))
                                :fill "none" :stroke (lispm-desktop-glow-color palette :title-end)
                                :stroke-opacity 0.6))
    (lispm-desktop-glow--path (lispm-desktop-glow--glowing svg)
                              (lispm-desktop-glow-chamfer 0.75 0.75 (- w 1.5) (- h 1.5) c '(tl br))
                              :fill "none" :stroke hot :stroke-width 1.5)
    ;; The striped band.
    (let ((stripes (lispm-desktop-glow--node svg 'g :fill hot :fill-opacity 0.7)))
      (cl-loop for x from (+ c 4) below (- w 8) by 8
               do (lispm-desktop-glow--path stripes (format "M%s,4 h4 l-3,5 h-4 Z" (+ x 3)))))
    (lispm-desktop-glow--svg-alert-icon svg icon 25 y palette)
    (dolist (line (plist-get layout :message))
      (lispm-desktop-platinum-svg-content svg line lispm-desktop-popup-alert-text-x y 16
                                          (plist-get palette :text) 'lispm-desktop-platinum-title)
      (setq y (+ y 16)))
    (when (plist-get layout :detail)
      (setq y (+ y 6))
      (dolist (line (plist-get layout :detail))
        (lispm-desktop-platinum-svg-content svg line lispm-desktop-popup-alert-text-x y 16
                                            (plist-get palette :muted) 'lispm-desktop-platinum-list)
        (setq y (+ y 16))))
    (pcase-dolist (`(,label ,value ,role ,x ,top ,width) (plist-get layout :buttons))
      (lispm-desktop-platinum-svg-push-button svg x top palette label width
                                              :state (and (equal pressed value) 'pressed)
                                              :default (eq role 'default)
                                              :focused (and focused (equal focused value))))
    svg))

;;;; Controls

(cl-defmethod lispm-desktop-platinum-svg-focus-ring (svg x y width height _radius palette
                                                         &context ((lispm-desktop-glow-p) (eql t)))
  "Mark on SVG the control in the WIDTH by HEIGHT box at X, Y as having the
keyboard: a glowing line about it, its corners cut."
  (lispm-desktop-glow--path svg (lispm-desktop-glow-chamfer (- x 2.5) (- y 2.5) (+ width 5) (+ height 5) 6 '(tl br))
                            :fill "none" :stroke (plist-get palette :focus) :stroke-width 1.5))

(cl-defmethod lispm-desktop-platinum-svg-push-button (svg x y palette label width
                                                          &context ((lispm-desktop-glow-p) (eql t))
                                                          &key state default focused font)
  "Draw on SVG at X, Y a glowing push button WIDTH pixels wide.
Its top left and bottom right corners cut; outlined in the glow colour,
the label in it, in FONT, the system font's by default.  DEFAULT fills
it with the glow colour, ringed in phosphor and casting a hot shadow in
neon; STATE `pressed' fills it with :pressed and moves it a pixel down
and right, `disabled' dashes its outline.  FOCUSED rings it."
  (let* ((h lispm-desktop-platinum-button-height)
         (glow (lispm-desktop-glow-color palette :glow))
         (pressed (eq state 'pressed))
         (disabled (eq state 'disabled))
         (shift (if pressed 1 0))
         (box (lambda (dx) (lispm-desktop-glow-chamfer (+ x dx 0.5) (+ y dx 0.5) (1- width) (1- h) 5 '(tl br))))
         (ink (cond (pressed (lispm-desktop-glow-color palette :pressed-text))
                    (default (lispm-desktop-glow--on glow))
                    (disabled (plist-get palette :border-inactive))
                    (t glow))))
    (cond
     (pressed
      (lispm-desktop-glow--path svg (funcall box shift) :fill (lispm-desktop-glow-color palette :pressed)))
     (default
      (if (lispm-desktop-glow-style :glitch)
          (lispm-desktop-glow--path svg (funcall box 3) :fill (lispm-desktop-glow-color palette :hot))
        (lispm-desktop-glow--path svg (lispm-desktop-glow-chamfer (- x 2.5) (- y 2.5) (+ width 5) (+ h 5) 7 '(tl br))
                                  :fill "none" :stroke glow :stroke-opacity 0.7))
      (lispm-desktop-glow--path (lispm-desktop-glow--glowing svg) (funcall box 0) :fill glow))
     (disabled
      (lispm-desktop-glow--path svg (funcall box 0) :fill (plist-get palette :menu)
                                :stroke (plist-get palette :border-inactive) :stroke-dasharray "3,2"))
     (t
      (lispm-desktop-glow--path svg (funcall box 0) :fill (plist-get palette :menu))
      (lispm-desktop-glow--path (lispm-desktop-glow--glowing svg) (funcall box 0)
                                :fill "none" :stroke glow :stroke-width 1.2)))
    (lispm-desktop-platinum--label svg label (+ x (/ width 2.0) shift) (+ y 14 shift) ink
                                   (or font (lispm-desktop-platinum-svg-font)))
    (when focused
      (lispm-desktop-platinum-svg-focus-ring svg x y width h 0 palette))))

(cl-defmethod lispm-desktop-platinum--draw-bevel-button (svg x y palette width height
                                                             &context ((lispm-desktop-glow-p) (eql t))
                                                             &key state label face)
  "Draw on SVG at X, Y a glowing toolbar button WIDTH by HEIGHT pixels: dark,
outlined, its corners cut; filled with the glow colour when STATE is
`pressed', its label dim when `disabled'.  LABEL is shown as Emacs would
show it in FACE."
  (let* ((face (or face 'lispm-desktop-platinum-list))
         (pressed (eq state 'pressed))
         (glow (lispm-desktop-glow-color palette :glow))
         (ink (cond (pressed (lispm-desktop-glow--on glow))
                    ((eq state 'disabled) (plist-get palette :border-inactive))
                    (t (plist-get palette :text)))))
    (lispm-desktop-glow--path svg (lispm-desktop-glow-chamfer (+ x 0.5) (+ y 0.5) (1- width) (1- height) 3 '(tl br))
                              :fill (if pressed glow (plist-get palette :header))
                              :stroke (if pressed glow (plist-get palette :border-inactive)))
    (when label
      (lispm-desktop-platinum-svg-content
       svg (propertize label 'face `(:foreground ,ink))
       (+ x (/ (- width (lispm-desktop-platinum-content-width label face)) 2)) y height ink face))))

(cl-defmethod lispm-desktop-platinum-draw-header-strip (palette width
                                                                &context ((lispm-desktop-glow-p) (eql t))
                                                                &optional label left)
  "Return the strip under a glowing title bar, WIDTH pixels wide: the
header colour over a dim line, LABEL centred in the small font, clear of
LEFT pixels of opening before it."
  (let* ((h lispm-desktop-platinum-header-strip-height)
         (left (or left 0))
         (svg (svg-create width h)))
    (lispm-desktop-look-svg-fill svg 0 0 width h (plist-get palette :header))
    (lispm-desktop-look-svg-fill svg 0 (1- h) width 1 (plist-get palette :border-inactive))
    (when label
      (let ((x (- (/ (- (+ left width) (lispm-desktop-platinum-content-width label 'lispm-desktop-platinum-small))
                     2)
                  left)))
        (lispm-desktop-platinum-svg-content svg label (if (> left 0) (max 8 x) x) 0 (1- h)
                                            (plist-get palette :muted) 'lispm-desktop-platinum-small)))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-header-button (palette label
                                                                 &context ((lispm-desktop-glow-p) (eql t))
                                                                 &optional state)
  "Return the opening of the strip under a glowing title bar: a toolbar
button reading LABEL in STATE."
  (let* ((width (lispm-desktop-platinum-header-button-width label))
         (h lispm-desktop-platinum-header-strip-height)
         (svg (svg-create width h)))
    (lispm-desktop-look-svg-fill svg 0 0 width h (plist-get palette :header))
    (lispm-desktop-look-svg-fill svg 0 (1- h) width 1 (plist-get palette :border-inactive))
    (lispm-desktop-platinum--draw-bevel-button svg 6 2 palette (- width 6)
                                               lispm-desktop-platinum-header-button-height
                                               :state state :label label
                                               :face 'lispm-desktop-platinum-small)
    svg))

(cl-defmethod lispm-desktop-platinum-draw-group-edge (palette width
                                                              &context ((lispm-desktop-glow-p) (eql t))
                                                              &optional title bottom)
  "Return an edge of a glowing group box WIDTH pixels wide: a dim line, TITLE
breaking the top one near its left end in the glow colour; BOTTOM draws
the bottom edge instead."
  (let* ((h (if bottom (default-line-height) lispm-desktop-platinum-group-title-height))
         (y (if bottom 1 (/ h 2)))
         (svg (svg-create width h))
         (line (plist-get palette :border-inactive)))
    (if bottom
        (progn (lispm-desktop-look-svg-fill svg 0 0 1 (1+ y) line)
               (lispm-desktop-look-svg-fill svg (1- width) 0 1 (1+ y) line))
      (lispm-desktop-look-svg-fill svg 0 y 1 (- h y) line)
      (lispm-desktop-look-svg-fill svg (1- width) y 1 (- h y) line))
    (lispm-desktop-look-svg-fill svg 0 y width 1 line)
    (when title
      (let ((title-width (lispm-desktop-platinum-content-width title 'lispm-desktop-platinum-title)))
        (lispm-desktop-look-svg-fill svg 6 0 (+ title-width 8) h (plist-get palette :chrome))
        (lispm-desktop-platinum-svg-content svg title 10 0 h (lispm-desktop-glow-color palette :glow)
                                            'lispm-desktop-platinum-title)))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-group-side (palette height
                                                              &context ((lispm-desktop-glow-p) (eql t)))
  "Return a side of a glowing group box: a dim line, then the pane."
  (let ((svg (svg-create 2 height)))
    (lispm-desktop-look-svg-fill svg 0 0 1 height (plist-get palette :border-inactive))
    (lispm-desktop-look-svg-fill svg 1 0 1 height (plist-get palette :chrome))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-tab (palette label width
                                                       &context ((lispm-desktop-glow-p) (eql t))
                                                       &key front face)
  "Return a glowing tab WIDTH pixels wide reading LABEL: its top left corner
cut, outlined in the glow colour and opening into the pane when it is in
FRONT, dim and standing on the pane's line behind."
  (let* ((face (or face 'lispm-desktop-platinum-title))
         (h lispm-desktop-platinum-tab-height)
         (w width)
         (svg (lispm-desktop-glow--create w (+ h 3)))
         (glow (lispm-desktop-glow-color palette :glow))
         (line (plist-get palette :border-inactive))
         (top (if front 0 2)))
    (lispm-desktop-look-svg-fill svg 0 0 w (+ h 3) (plist-get palette :chrome))
    (unless front
      (lispm-desktop-look-svg-fill svg 0 h w 1 glow))
    (lispm-desktop-glow--path svg (format "M0.5,%s V%s L%s,%s H%s V%s" (+ h (if front 3 0)) (+ top 6)
                                          6.5 (+ top 0.5) (- w 0.5) (+ h (if front 3 0)))
                              :fill (if front (plist-get palette :chrome) (plist-get palette :header))
                              :stroke (if front glow line))
    (lispm-desktop-platinum-svg-content
     (if front (lispm-desktop-glow--glowing svg) svg)
     label (/ (- w (lispm-desktop-platinum-content-width label face)) 2) top (- h top)
     (if front glow (plist-get palette :muted)) face)
    svg))

(cl-defmethod lispm-desktop-platinum-draw-tab-edge (palette width
                                                            &context ((lispm-desktop-glow-p) (eql t)))
  "Return the pane's edge WIDTH pixels wide beside glowing tabs: the
window's body above, the pane's glowing line, the pane below."
  (let* ((h lispm-desktop-platinum-tab-height)
         (svg (svg-create width (+ h 3))))
    (lispm-desktop-look-svg-fill svg 0 0 width (+ h 3) (plist-get palette :chrome))
    (lispm-desktop-look-svg-fill svg 0 h width 1 (lispm-desktop-glow-color palette :glow))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-tab-close (palette
                                                     &context ((lispm-desktop-glow-p) (eql t))
                                                     &key pressed)
  "Return the cross closing a tab: dim, in the glow colour while PRESSED."
  (let* ((s lispm-desktop-platinum-tab-close-size)
         (svg (svg-create s s))
         (ink (if pressed (lispm-desktop-glow-color palette :glow) (plist-get palette :muted))))
    (lispm-desktop-glow--line svg 3 3 (- s 3) (- s 3) ink 1.5)
    (lispm-desktop-glow--line svg (- s 3) 3 3 (- s 3) ink 1.5)
    svg))

(cl-defmethod lispm-desktop-platinum-draw-placard (palette width height
                                                           &context ((lispm-desktop-glow-p) (eql t))
                                                           &key label font)
  "Return a glowing placard WIDTH by HEIGHT pixels: the header colour,
outlined dimly, its corners cut, LABEL centred."
  (let ((svg (svg-create width height)))
    (lispm-desktop-glow--path svg (lispm-desktop-glow-chamfer 0.5 0.5 (1- width) (1- height) 3 '(tl br))
                              :fill (plist-get palette :header) :stroke (plist-get palette :border-inactive))
    (when label
      (let ((font (or font (lispm-desktop-platinum-svg-font 'lispm-desktop-platinum-list))))
        (lispm-desktop-platinum--label svg label (/ width 2.0)
                                       (round (+ (/ height 2.0) (* 0.35 (plist-get font :size))))
                                       (plist-get palette :muted) font)))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-radio (palette on
                                                         &context ((lispm-desktop-glow-p) (eql t))
                                                         &key state focused pad)
  "Return a glowing radio button: a ring in the glow colour, a dot in it
when ON; dim when STATE is `disabled', filled when `pressed'.  FOCUSED
rings it again."
  (let* ((pad (max (or pad 0) (if focused 2 0)))
         (size (+ 12 (* 2 pad)))
         (middle (/ size 2.0))
         (svg (lispm-desktop-glow--create size size))
         (ink (if (eq state 'disabled) (plist-get palette :border-inactive)
                (lispm-desktop-glow-color palette :glow))))
    (when focused
      (svg-circle svg middle middle 7.5 :fill "none" :stroke (plist-get palette :focus) :stroke-width 1))
    (svg-circle svg middle middle 5.25 :stroke ink :stroke-width 1.5
                :fill (if (eq state 'pressed) (lispm-desktop-glow-color palette :pressed) (plist-get palette :menu)))
    (when on
      (svg-circle (lispm-desktop-glow--glowing svg) middle middle 2.5 :fill ink))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-check-box (palette on
                                                             &context ((lispm-desktop-glow-p) (eql t))
                                                             &key state focused pad)
  "Return a glowing check box: a box in the glow colour, its corners cut,
checked when ON; dim when STATE is `disabled', filled when `pressed'.
FOCUSED rings it.  It stands where Platinum's does."
  (let* ((pad (max (or pad 0) (if focused 2 0)))
         (svg (lispm-desktop-glow--create (+ 14 (* 2 pad)) (+ 14 (* 2 pad))))
         (x pad)
         (y (+ pad 1))
         (ink (if (eq state 'disabled) (plist-get palette :border-inactive)
                (lispm-desktop-glow-color palette :glow))))
    (when focused
      (lispm-desktop-glow--path svg (lispm-desktop-glow-chamfer (- x 1.5) (- y 1.5) 15 15 4 '(tl br))
                                :fill "none" :stroke (plist-get palette :focus)))
    (lispm-desktop-glow--path svg (lispm-desktop-glow-chamfer (+ x 0.75) (+ y 0.75) 11.5 11.5 3 '(tl br))
                              :fill (if (eq state 'pressed) (lispm-desktop-glow-color palette :pressed)
                                      (plist-get palette :menu))
                              :stroke ink :stroke-width 1.5)
    (when on
      (lispm-desktop-glow--path (lispm-desktop-glow--glowing svg)
                                (format "M%s,%s L%s,%s L%s,%s" (+ x 3) (+ y 6.5) (+ x 5.5) (+ y 9) (+ x 10) (+ y 3.5))
                                :fill "none" :stroke ink :stroke-width 2))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-list-check-box (palette state
                                                                  &context ((lispm-desktop-glow-p) (eql t)))
  "Return a list's twelve-pixel glowing check box, checked when STATE is
`on', a bar across it when `mixed'."
  (let ((svg (svg-create 12 12))
        (ink (lispm-desktop-glow-color palette :glow)))
    (lispm-desktop-glow--path svg (lispm-desktop-glow-chamfer 0.5 0.5 11 11 3 '(tl br))
                              :fill (plist-get palette :menu) :stroke ink)
    (pcase state
      ('on (lispm-desktop-glow--path svg "M2.5,6 L5,8.5 L9.5,3" :fill "none" :stroke ink :stroke-width 1.5))
      ('mixed (lispm-desktop-look-svg-fill svg 3 5 6 2 ink)))
    svg))

(cl-defmethod lispm-desktop-platinum-svg-progress-bar (svg x y width fraction palette
                                                           &context ((lispm-desktop-glow-p) (eql t)))
  "Draw on SVG at X, Y a glowing progress bar WIDTH pixels wide: a dark
track outlined dimly, FRACTION of it a glowing bar."
  (let* ((h lispm-desktop-platinum-progress-height)
         (filled (round (* (- width 4) (min 1.0 (max 0.0 fraction))))))
    (lispm-desktop-glow--path svg (lispm-desktop-glow-chamfer (+ x 0.5) (+ y 0.5) (1- width) (1- h) 3 '(tl br))
                              :fill (lispm-desktop-glow-color palette :track)
                              :stroke (plist-get palette :border-inactive))
    (when (> filled 0)
      (lispm-desktop-look-svg-fill svg (+ x 2) (+ y 3) filled (- h 6) (lispm-desktop-glow-color palette :glow)))))

(cl-defmethod lispm-desktop-platinum-draw-triangle (palette open
                                                            &context ((lispm-desktop-glow-p) (eql t)))
  "Return a list's disclosure mark, twelve pixels square: a chevron in the
glow colour, pointing down when OPEN, right when not."
  (let ((svg (svg-create 12 12)))
    (lispm-desktop-glow--path svg (if open "M3,4.5 L6,7.5 L9,4.5" "M4.5,3 L7.5,6 L4.5,9")
                              :fill "none" :stroke (lispm-desktop-glow-color palette :glow) :stroke-width 1.5)
    svg))

(cl-defmethod lispm-desktop-platinum-draw-list-header (palette columns
                                                               &context ((lispm-desktop-glow-p) (eql t))
                                                               &optional label-x)
  "Return a list's column headings in a glowing row: dim labels between dim
lines, the sorted column's label in the glow colour and underlined.
COLUMNS lists (LABEL WIDTH SORTED [X]); LABEL-X places labels, by
default eight pixels in."
  (let* ((h lispm-desktop-platinum-list-header-height)
         (width (+ 16 (apply #'+ (mapcar #'cadr columns))))
         (svg (svg-create width h))
         (glow (lispm-desktop-glow-color palette :glow))
         (x 0))
    (lispm-desktop-look-svg-fill svg 0 0 width h (plist-get palette :header))
    (lispm-desktop-look-svg-fill svg 0 (1- h) width 1 (plist-get palette :border-inactive))
    (pcase-dolist (`(,label ,w ,sorted ,x-in) columns)
      (when (> x 0)
        (lispm-desktop-look-svg-fill svg x 4 1 (- h 8) (plist-get palette :border-inactive)))
      (when sorted
        (lispm-desktop-look-svg-fill svg x (- h 2) w 2 glow))
      (when label
        (lispm-desktop-platinum-svg-content svg label (+ x (or x-in label-x 8)) 0 h
                                            (if sorted glow (plist-get palette :muted))
                                            'lispm-desktop-platinum-list))
      (setq x (+ x w)))
    (lispm-desktop-look-svg-arrow svg (- width 12) 8 4 'down glow)
    svg))

;;;; Scroll bars

(defconst lispm-desktop-glow--scroll-arrow 16
  "Height in pixels of a glowing scroll bar's arrow box.")

(cl-defmethod lispm-desktop-platinum-scroll-layout (height &context ((lispm-desktop-glow-p) (eql t)))
  "Lay a glowing scroll bar HEIGHT pixels tall out: an arrow at either end,
the track between."
  (let ((a lispm-desktop-glow--scroll-arrow))
    (list :track a :track-length (max 0 (- height a a)) :up 0 :down (- height a) :arrow a)))

(cl-defmethod lispm-desktop-platinum-scroll-thumb (height start end min max
                                                          &context ((lispm-desktop-glow-p) (eql t)))
  "Return the thumb of a glowing scroll bar HEIGHT pixels tall, as long as
the part of the buffer from MIN to MAX shown, START to END, is of it,
twenty pixels at least; nil when everything shows, or the track is too
short."
  (let* ((layout (lispm-desktop-platinum-scroll-layout height))
         (track (plist-get layout :track-length))
         (size (- max min))
         (shown (- end start)))
    (when (and (> size 0) (> track 40) (or (> start min) (< end max)))
      (let* ((length (max 20 (min track (round (* track (/ (float shown) size))))))
             (fraction (/ (float (- start min)) (max 1 (- size shown)))))
        (cons (+ (plist-get layout :track) (round (* (- track length) (min 1.0 fraction))))
              length)))))

(defun lispm-desktop-glow--svg-arrow-box (svg y palette direction live)
  "Draw on SVG from row Y a scroll bar's arrow box pointing DIRECTION: a
square outlined in the glow colour holding a chevron, dim when not LIVE."
  (let ((ink (if live (lispm-desktop-glow-color palette :glow) (plist-get palette :border-inactive))))
    (lispm-desktop-glow--node svg 'rect :x 0.5 :y (+ y 0.5) :width 15 :height 15
                              :fill (lispm-desktop-glow-color palette :track) :stroke ink)
    (lispm-desktop-glow--path svg (if (eq direction 'up)
                                      (format "M4,%s L8,%s L12,%s" (+ y 10) (+ y 6) (+ y 10))
                                    (format "M4,%s L8,%s L12,%s" (+ y 6) (+ y 10) (+ y 6)))
                              :fill "none" :stroke ink :stroke-width 1.6)))

(cl-defmethod lispm-desktop-platinum-draw-scroll-bar (palette height thumb active
                                                              &context ((lispm-desktop-glow-p) (eql t)))
  "Return a glowing scroll bar HEIGHT pixels tall, in PALETTE: arrow boxes at
both ends, the dark track between marked every eight pixels, THUMB on it,
its corners cut and three grips across it; neon fills it.  Nothing to
scroll, THUMB nil, dims the arrows; an inactive bar, ACTIVE nil, shows no
thumb."
  (let* ((w lispm-desktop-platinum-scroll-width)
         (svg (lispm-desktop-glow--create w height))
         (layout (lispm-desktop-platinum-scroll-layout height))
         (live (and active thumb))
         (glow (lispm-desktop-glow-color palette :glow))
         (line (plist-get palette :border-inactive)))
    (lispm-desktop-look-svg-fill svg 0 0 w height (lispm-desktop-glow-color palette :track))
    (lispm-desktop-look-svg-fill svg 0 0 1 height line)
    (cl-loop for ty from (+ (plist-get layout :track) 4) below (plist-get layout :down) by 8
             do (lispm-desktop-look-svg-fill svg 2 ty 3 1 line))
    (lispm-desktop-glow--svg-arrow-box svg (plist-get layout :up) palette 'up live)
    (lispm-desktop-glow--svg-arrow-box svg (plist-get layout :down) palette 'down live)
    (when live
      (let* ((top (car thumb)) (length (cdr thumb))
             (solid (lispm-desktop-glow-style :solid-thumb))
             (grip (if solid (lispm-desktop-glow--on glow) glow)))
        (lispm-desktop-glow--path (lispm-desktop-glow--glowing svg)
                                  (lispm-desktop-glow-chamfer 2.5 (+ top 1.5) 11 (- length 3) 3 '(tl br))
                                  :fill glow :fill-opacity (if solid 1 0.22) :stroke glow)
        (dolist (d '(-3 0 3))
          (lispm-desktop-look-svg-fill svg 5 (+ top (/ length 2) d) 6 1 grip))))
    svg))

;;;; The Control Strip

(cl-defmethod lispm-desktop-platinum-draw-strip-segment (palette kind
                                                                 &context ((lispm-desktop-glow-p) (eql t))
                                                                 &key state icon menu focused behind side)
  "Return a segment of the glowing Control Strip: dark boxes outlined in the
glow colour holding their icons, the tab at its end a hot shape with
grips.  KIND, STATE, ICON, MENU, FOCUSED, BEHIND and SIDE are as
Platinum's take them."
  (let* ((h lispm-desktop-platinum-strip-height)
         (w (lispm-desktop-platinum-strip-segment-width kind))
         (svg (lispm-desktop-glow--create w h))
         (glow (lispm-desktop-glow-color palette :glow))
         (ink (if (eq state 'disabled) (plist-get palette :border-inactive) glow)))
    (if (eq kind 'tab)
        (let ((x (if (eq side 'right) 4 0)))
          (lispm-desktop-look-svg-fill svg 0 0 w h (or behind (plist-get palette :desktop)))
          (lispm-desktop-glow--path (lispm-desktop-glow--glowing svg)
                                    (if (eq side 'right)
                                        (format "M%s,0 h9 v%s h-9 l-4,-6 v%s Z" (+ x 4) h (- 12 h))
                                      (format "M%s,0 h9 l4,6 v%s l-4,6 h-9 Z" x (- h 12)))
                                    :fill (lispm-desktop-glow-color palette :hot))
          (dotimes (i 4)
            (lispm-desktop-look-svg-fill svg (+ x 4) (+ 6 (* 3 i)) 5 1
                                         (lispm-desktop-glow--on (lispm-desktop-glow-color palette :hot)))))
      (lispm-desktop-look-svg-fill svg 0 0 w h (if (eq state 'pressed) (lispm-desktop-glow-color palette :pressed)
                                                 (lispm-desktop-glow-color palette :track)))
      (lispm-desktop-glow--node svg 'rect :x 0.5 :y 0.5 :width (1- w) :height (1- h) :fill "none"
                                :stroke (plist-get palette :border-inactive))
      (pcase kind
        ('collapse (lispm-desktop-look-svg-fill svg 4 11 7 2 ink))
        ((or 'left 'right) (lispm-desktop-look-svg-arrow svg 5 8 4 kind ink))
        ('module
         (when focused
           (lispm-desktop-glow--node svg 'rect :x 1.5 :y 1.5 :width (- w 3) :height (- h 3)
                                     :fill "none" :stroke (plist-get palette :focus)))
         (cond ((stringp icon) (lispm-desktop-platinum-svg-icon svg icon 3 4 16))
               (icon (lispm-desktop-platinum-svg-embed svg icon 3 4 16 16)))
         (when menu
           (lispm-desktop-look-svg-arrow svg 20 10 3 'down ink)))))
    svg))

;;;; The desktop

(defun lispm-desktop-glow--icon-filter (palette)
  "Return the SVG filter drawing an icon as the glowing looks do: its shape
in the glow colour, as bright as it was light, glowing."
  (pcase-let ((`(,r ,g ,b) (or (lispm-desktop-platinum-rgb (lispm-desktop-glow-color palette :glow))
                               '(1.0 1.0 1.0))))
    (format (concat "<defs><filter id=\"lispm-glow-icon\" x=\"-30%%\" y=\"-30%%\" width=\"160%%\" height=\"160%%\">"
                    "<feColorMatrix type=\"matrix\" values=\""
                    "%.3f %.3f %.3f 0 0 %.3f %.3f %.3f 0 0 %.3f %.3f %.3f 0 0 0 0 0 1 0\" result=\"tint\"/>"
                    "<feGaussianBlur in=\"tint\" stdDeviation=\"1.5\" result=\"blur\"/>"
                    "<feMerge><feMergeNode in=\"blur\"/><feMergeNode in=\"tint\"/></feMerge>"
                    "</filter></defs>")
            (* 0.4 r) (* 0.8 r) (* 0.3 r) (* 0.4 g) (* 0.8 g) (* 0.3 g) (* 0.4 b) (* 0.8 b) (* 0.3 b))))

(cl-defmethod lispm-desktop--draw-icon (spec x y selected label-width family size ascent
                                             &context ((lispm-desktop-glow-p) (eql t)))
  "Return SVG of the icon SPEC with its name under it, as the glowing looks
draw icons on the desktop: the icon in the glow colour on a dark plate,
its name on black, or in the highlight when SELECTED.  See
`lispm-desktop--draw-icon'."
  (let* ((palette lispm-desktop-platinum-current-palette)
         (s lispm-desktop-icon-size)
         (center (+ x (/ lispm-desktop-column 2)))
         (label-center (lispm-desktop--label-center spec x))
         (label-left (- label-center (/ label-width 2)))
         (label-top (+ y s 4)))
    (concat
     (lispm-desktop-glow--icon-filter palette)
     (format "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"#000000\" fill-opacity=\"0.6\"/>"
             (- center (/ s 2) 4) (- y 2) (+ s 8) (+ s 4))
     "<g filter=\"url(#lispm-glow-icon)\">"
     (lispm-desktop--nest (lispm-desktop--item-icon spec) nil (- center (/ s 2)) y)
     "</g>"
     (format "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"%s\"/>"
             label-left label-top label-width (+ size 4)
             (if selected (plist-get palette :highlight) "#000000"))
     (format "<text x=\"%d\" y=\"%d\" font-family=\"%s\" font-size=\"%d\" text-anchor=\"middle\" fill=\"%s\">%s</text>"
             label-center (+ label-top 2 ascent) (lispm-desktop--escape family) size
             (if selected (plist-get palette :highlight-text) (lispm-desktop-glow-color palette :glow))
             (lispm-desktop--escape (plist-get spec :label))))))

;;;; Faces

(cl-defmethod lispm-desktop-platinum-apply-look-faces (palette &context ((lispm-desktop-glow-p) (eql t)))
  "Set the faces the glowing looks draw otherwise than Platinum, in PALETTE:
lists on the window's own glass, rows without lit lines, placards and
buttons outlined dimly, menus and tooltips framed in the glow colour."
  (let ((line (plist-get palette :border-inactive))
        (glow (lispm-desktop-glow-color palette :glow)))
    (set-face-attribute 'lispm-desktop-platinum-list-body nil :background (plist-get palette :chrome))
    (set-face-attribute 'lispm-desktop-platinum-sorted nil :background (plist-get palette :header))
    (set-face-attribute 'lispm-desktop-platinum-row nil :overline 'unspecified)
    (dolist (face '(lispm-desktop-platinum-placard lispm-desktop-platinum-placard-inactive))
      (set-face-attribute face nil :box `(:line-width (1 . 1) :color ,line) :background (plist-get palette :header)))
    (set-face-attribute 'lispm-desktop-platinum-header nil
                        :background (plist-get palette :header) :foreground (plist-get palette :text)
                        :box `(:line-width (1 . 4) :color ,(plist-get palette :header)))
    (set-face-attribute 'lispm-desktop-platinum-button nil
                        :background (plist-get palette :header) :foreground glow
                        :box `(:line-width (1 . 1) :color ,glow))
    (set-face-attribute 'tooltip nil :background (plist-get palette :menu) :foreground (plist-get palette :menu-text))
    (set-face-attribute 'child-frame-border nil :background glow)
    (set-face-attribute 'window-divider nil :foreground line)
    (set-face-attribute 'window-divider-first-pixel nil :foreground line)
    (set-face-attribute 'window-divider-last-pixel nil :foreground line)))

(provide 'lispm-desktop-glow)
;;; lispm-desktop-glow.el ends here
