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
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-popup)
(require 'lispm-desktop-surface)
(require 'lispm-desktop-i18n)

(lispm-desktop-look-define
 'win3
 :title "Windows 3.2"
 :description (lispm-desktop-tr-noop "Windows 3.2's look: navy captions, the control-menu box, sizing borders")
 :theme 'win3
 :icons '("win3")
 :system-families '("Microsoft Sans Serif" "MS Sans Serif" "Arial" "Helvetica")
 :list-families '("Microsoft Sans Serif" "MS Sans Serif" "Arial" "Helvetica"))

(defconst lispm-desktop-win3-button-width 18
  "Width in pixels of a caption's buttons and of its control-menu box.")

;;;; Colours

(defun lispm-desktop-win3--shadow (palette)
  "Return the button shadow of PALETTE: Windows' dark grey."
  (or (plist-get palette :shade) (plist-get palette :outline)))

(defun lispm-desktop-win3--pane (palette)
  "Return the ground dialogs and lists stand on: the window colour."
  (plist-get palette :content))

(defun lispm-desktop-win3--image (key draw)
  "Return the image of KEY, a drawing of this look, see
`lispm-desktop-platinum-image'."
  (lispm-desktop-platinum-image (cons 'win3 key) draw))

;;;; Captions

(defun lispm-desktop-win3--caption-colors (palette active)
  "Return the ground and the text of a caption in PALETTE, ACTIVE or not."
  (if active
      (cons (plist-get palette :title) (plist-get palette :title-text))
    (cons (plist-get palette :title-inactive) (plist-get palette :title-inactive-text))))

(defun lispm-desktop-win3-draw-caption (palette width &optional active label label-x)
  "Return an SVG of a stretch of caption WIDTH pixels wide, in PALETTE.
It is the title bar's height: the caption's colour, ACTIVE or not, over
the black line under it.  LABEL, as Emacs would show it in the titles'
face, is drawn from column LABEL-X in the caption's text colour."
  (pcase-let* ((h lispm-desktop-platinum-title-bar-height)
               (svg (svg-create width h))
               (`(,ground . ,ink) (lispm-desktop-win3--caption-colors palette active)))
    (lispm-desktop-look-svg-fill svg 0 0 width (1- h) ground)
    (lispm-desktop-look-svg-fill svg 0 (1- h) width 1 (plist-get palette :outline))
    (when label
      (let ((label (copy-sequence label)))
        (add-face-text-property 0 (length label) `(:foreground ,ink) nil label)
        (lispm-desktop-platinum-svg-content
         svg label label-x 0 (1- h) ink
         (if active 'lispm-desktop-platinum-title 'lispm-desktop-platinum-title-inactive))))
    svg))

(defun lispm-desktop-win3-draw-control-box (palette &optional pressed)
  "Return an SVG of the control-menu box, a black line closing its right.
It is a grey square holding a white bar outlined in black, which casts a
shadow down and right; PRESSED inverts the square, as the window's menu
drops from it."
  (let* ((w lispm-desktop-win3-button-width)
         (h lispm-desktop-platinum-title-bar-height)
         (svg (svg-create (1+ w) h))
         (y (/ (- h 4) 2))
         (outline (plist-get palette :outline))
         (shadow (lispm-desktop-win3--shadow palette)))
    (lispm-desktop-look-svg-fill svg 0 0 w (1- h) (if pressed shadow (plist-get palette :chrome)))
    (lispm-desktop-look-svg-fill svg w 0 1 h outline)
    (lispm-desktop-look-svg-fill svg 0 (1- h) w 1 outline)
    (lispm-desktop-look-svg-fill svg 3 (+ y 3) 13 1 (if pressed (plist-get palette :chrome) shadow))
    (lispm-desktop-look-svg-fill svg 15 (1+ y) 1 3 (if pressed (plist-get palette :chrome) shadow))
    (lispm-desktop-look-svg-fill svg 2 y 13 3 outline)
    (lispm-desktop-look-svg-fill svg 3 (1+ y) 11 1 (plist-get palette :light))
    svg))

(defun lispm-desktop-win3-draw-caption-button (palette kind &optional pressed)
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
         (shadow (lispm-desktop-win3--shadow palette))
         (shift (if pressed 1 0)))
    (lispm-desktop-look-svg-fill svg 0 0 1 h outline)
    (lispm-desktop-look-svg-fill svg 1 0 w (1- h) (plist-get palette :chrome))
    (lispm-desktop-look-svg-fill svg 1 (1- h) w 1 outline)
    (if pressed
        (lispm-desktop-look-svg-edges svg 1 0 w (1- h) (list shadow) nil)
      (lispm-desktop-look-svg-edges svg 1 0 w (1- h) (list (plist-get palette :light)) (list shadow shadow)))
    (lispm-desktop-look-svg-arrow svg (+ 6 shift) (+ 7 shift) 4 (if (eq kind 'minimize) 'down 'up)
                                  (lispm-desktop-platinum-legible (plist-get palette :chrome)
                                                                  outline (plist-get palette :text)))
    svg))

(cl-defmethod lispm-desktop-platinum-title-layout (width title-width
                                                         &context (lispm-desktop-look (eql win3))
                                                         &optional controls margins)
  "Lay a Windows 3.2 caption WIDTH pixels wide out for a title TITLE-WIDTH wide.
The control-menu box opens it, minimize and maximize close it, each with
its black line; the title is centred between them, narrowed to fit."
  (pcase-let* ((`(,close ,zoom ,collapse) (or controls '(on on on)))
               (b (1+ lispm-desktop-win3-button-width))
               (`(,lead . ,trail) (or margins '(0 . 0)))
               (lead (lispm-desktop-look-box-margin lead))
               (trail (lispm-desktop-look-box-margin trail))
               (start (+ lead (if close b 0)))
               (end (- width trail (if zoom b 0) (if collapse b 0)))
               (title (max 0 (min title-width (- end start 8))))
               (label-x (+ start (max 4 (/ (- end start title) 2)))))
    (list :start start :end end :title title :label-x label-x :margins (cons lead trail)
          :ridges nil)))

(defun lispm-desktop-win3-control-menu (window)
  "Drop WINDOW's control menu from its control-menu box, as Windows did:
Restore, Minimize, Maximize and Close, those WINDOW cannot do disabled."
  (pcase-let* ((`(,_ ,zoom ,collapse) (lispm-desktop-platinum-window-controls window))
               (`(,left ,top ,_ ,_) (window-pixel-edges window))
               (margins (lispm-desktop-platinum-window-box-margins window)))
    (lispm-desktop-popup-menu
     (list (list :label (lispm-desktop-tr-in "Windows 3.2" "Restore")
                 :enabled (and zoom (frame-parameter nil 'lispm-desktop-platinum-unzoom) t)
                 :action (lambda () (lispm-desktop-platinum-zoom-window window)))
           (list :label (lispm-desktop-tr-in "Windows 3.2" "Minimize") :enabled (and collapse t)
                 :action (lambda () (lispm-desktop-platinum-collapse-window window)))
           (list :label (lispm-desktop-tr-in "Windows 3.2" "Maximize") :enabled (and zoom t)
                 :action (lambda () (lispm-desktop-platinum-zoom-window window)))
           'separator
           (list :label (lispm-desktop-tr-in "Windows 3.2" "Close")
                 :action (lambda () (lispm-desktop-platinum-close-window window))))
     (+ left (lispm-desktop-look-box-margin (car margins)))
     (+ top (window-tab-line-height window)))))

(cl-defmethod lispm-desktop-platinum-draw-title-bar (width active _icon title modified controls
                                                           &context (lispm-desktop-look (eql win3))
                                                           &optional pressed status _edges margins)
  "Return a Windows 3.2 caption WIDTH pixels wide reading TITLE.
The control-menu box drops the window's menu, minimize collapses it and
maximize zooms it; inactive windows keep their buttons, as Windows'
did.  MODIFIED marks the title with a star; STATUS follows it."
  (pcase-let* ((label (concat title (when modified " *") (when status (concat "  " status))))
               (face (if active 'lispm-desktop-platinum-title 'lispm-desktop-platinum-title-inactive))
               (`(,close ,zoom ,collapse) controls)
               (layout (lispm-desktop-platinum-title-layout
                        width (lispm-desktop-platinum-content-width label face) controls margins))
               (`(,lead . ,trail) (plist-get layout :margins))
               (start (plist-get layout :start))
               (end (plist-get layout :end))
               (label (let ((label (copy-sequence label)))
                        (add-face-text-property 0 (length label) face t label)
                        (lispm-desktop-platinum-fit label (plist-get layout :title))))
               (title-help (lispm-desktop-platinum-title-help))
               (palette lispm-desktop-platinum-current-palette))
    (cl-flet ((caption (from to &optional text)
                (propertize " " 'display
                            (lispm-desktop-win3--image
                             (list 'caption (- to from) active
                                   (and text (lispm-desktop-platinum--content-key text))
                                   (- (plist-get layout :label-x) from))
                             (lambda ()
                               (lispm-desktop-win3-draw-caption palette (- to from) active text
                                                                (- (plist-get layout :label-x) from))))
                            'keymap lispm-desktop-platinum-title-map
                            'help-echo title-help))
              (button (kind box command help)
                (lispm-desktop-platinum-box-button
                 (propertize " " 'display
                             (lispm-desktop-win3--image
                              (list 'button kind (eq pressed box))
                              (lambda ()
                                (if (eq kind 'control)
                                    (lispm-desktop-win3-draw-control-box palette (eq pressed box))
                                  (lispm-desktop-win3-draw-caption-button palette kind
                                                                          (eq pressed box))))))
                 box command help)))
      (concat
       (when (> lead 0) (caption 0 lead))
       (when close
         (button 'control 'close #'lispm-desktop-win3-control-menu
                 (lispm-desktop-tr "Control menu: restore, minimize, maximize, close")))
       (caption start end label)
       (when collapse
         (button 'minimize 'collapse #'lispm-desktop-platinum-collapse-window
                 (lispm-desktop-tr "Minimize: roll up to the caption")))
       (when zoom
         (button 'maximize 'zoom #'lispm-desktop-platinum-zoom-window
                 (lispm-desktop-tr-in "Windows 3.2" "Maximize, or restore")))
       (when (> trail 0) (caption 0 trail))))))

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
    (lispm-desktop-look-svg-fill svg 0 0 w h band)
    (lispm-desktop-look-svg-outline svg 0 0 w h outline)
    ;; The black line round the window, beside the caption and under it.
    (lispm-desktop-look-svg-fill svg 5 1 1 (- h 7) outline)
    (lispm-desktop-look-svg-fill svg (- w 7) 1 1 (- h 7) outline)
    (lispm-desktop-look-svg-fill svg 5 (- h 7) (- w 11) 1 outline)
    ;; The corners' notches.
    (dolist (y (list corner (- h corner)))
      (lispm-desktop-look-svg-fill svg 1 y 4 1 outline)
      (lispm-desktop-look-svg-fill svg (- w 6) y 5 1 outline))
    (dolist (x (list corner (- w corner)))
      (lispm-desktop-look-svg-fill svg x (- h 6) 1 5 outline))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-grow-box (palette height
                                                            &context (lispm-desktop-look (eql win3)))
  "Return Windows 3.2's size box, sixteen pixels square, in PALETTE: the
grey square where a window's scroll bars meet, raised as a button, with
a small window outlined in it."
  (let* ((s 16)
         (svg (svg-create s height))
         (y (/ (- height s) 2))
         (shadow (lispm-desktop-win3--shadow palette)))
    (lispm-desktop-look-svg-fill svg 0 0 s height (plist-get palette :chrome))
    (lispm-desktop-look-svg-outline svg 0 y s s (plist-get palette :outline))
    (lispm-desktop-look-svg-edges svg 1 (1+ y) (- s 2) (- s 2) (list (plist-get palette :light)) (list shadow))
    (lispm-desktop-look-svg-outline svg 5 (+ y 5) 7 7 (plist-get palette :outline))
    (lispm-desktop-look-svg-fill svg 5 (+ y 6) 7 1 (plist-get palette :outline))
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
    (lispm-desktop-look-svg-fill svg 0 0 width (1- h)
                                 (plist-get palette (if highlighted :highlight :menu)))
    (lispm-desktop-look-svg-fill svg 0 (1- h) width 1 (plist-get palette :outline))
    (when label
      (let ((ink (plist-get palette (if highlighted :highlight-text :menu-text))))
        (lispm-desktop-platinum-svg-content
         svg (propertize label 'face `(:foreground ,ink))
         (/ (- width (lispm-desktop-platinum-content-width label 'lispm-desktop-platinum-menu-title)) 2)
         0 (1- h) ink 'lispm-desktop-platinum-menu-title)))
    svg))

(cl-defmethod lispm-desktop-popup-draw-menu (palette items width highlight
                                                     &context (lispm-desktop-look (eql win3)))
  "Return Windows 3.2's menu of ITEMS WIDTH pixels wide, in PALETTE.
White inside the black outline the menu's frame draws, the item under
the mouse, HIGHLIGHT, in the highlight colour across the whole width,
check marks before checked items, key equivalents flush right, arrows
on those holding submenus, separators a black line between margins,
disabled items grey."
  (pcase-let* ((`(,tops . ,height) (lispm-desktop-popup--rows items))
               (svg (svg-create width height))
               (icons (lispm-desktop-popup--icon-room items)))
    (lispm-desktop-look-svg-fill svg 0 0 width height (plist-get palette :menu))
    (seq-do-indexed
     (lambda (item index)
       (let ((top (nth index tops)))
         (if (lispm-desktop-popup--separator-p item)
             (lispm-desktop-look-svg-fill svg 1 (+ top (/ lispm-desktop-popup-separator-height 2))
                                          (- width 2) 1 (plist-get palette :menu-text))
           (let* ((h (lispm-desktop-popup--item-height))
                  (lit (eql index highlight))
                  (enabled (lispm-desktop-popup--enabled-p item))
                  (ink (cond (lit (plist-get palette :highlight-text))
                             (enabled (plist-get palette :menu-text))
                             (t (lispm-desktop-win3--shadow palette))))
                  (face (lispm-desktop-popup--face (or lit enabled)))
                  (key (and (not (plist-get item :submenu)) (lispm-desktop-popup--key item)))
                  (label (propertize (plist-get item :label) 'face `(:foreground ,ink))))
             (when lit
               (lispm-desktop-look-svg-fill svg 0 top width h (plist-get palette :highlight)))
             (when (plist-get item :checked)
               (lispm-desktop-platinum-svg-content svg (propertize "✓" 'face `(:foreground ,ink))
                                                   (if lispm-desktop-popup-small 3 5) top h ink face))
             (when-let* ((icon (plist-get item :icon))
                         (size (min lispm-desktop-popup-icon-size h)))
               (lispm-desktop-platinum-svg-emacs-image svg icon (lispm-desktop-popup--label-x)
                                                       (+ top (/ (- h size) 2)) size size))
             (lispm-desktop-platinum-svg-content svg label (+ (lispm-desktop-popup--label-x) icons)
                                                 top h ink face)
             (cond ((plist-get item :submenu)
                    (lispm-desktop-look-svg-arrow svg (- width 10) (+ top (/ (- h 7) 2)) 4 'right ink))
                   (key
                    (lispm-desktop-platinum-svg-content
                     svg (propertize key 'face `(:foreground ,ink))
                     (- width (lispm-desktop-popup--key-margin) (lispm-desktop-popup--label-width key))
                     top h ink face)))))))
     items)
    svg))

;;;; Message boxes

(defconst lispm-desktop-win3--alert-border 4
  "Depth in pixels of a message box's border, inside its outline.")

(cl-defmethod lispm-desktop-popup--alert-layout (message detail buttons
                                                         &context (lispm-desktop-look (eql win3)))
  "Lay a Windows 3.2 message box of MESSAGE, DETAIL and BUTTONS out.
Under the caption, the icon at the top left and the message beside it;
the buttons in a row centred along the bottom, the default first."
  (let* ((w lispm-desktop-look-alert-width)
         (border lispm-desktop-win3--alert-border)
         (caption lispm-desktop-platinum-title-bar-height)
         (text-width (- w lispm-desktop-look-alert-text-x 20))
         (lines (lispm-desktop-popup--wrap message text-width 'lispm-desktop-platinum-title))
         (details (and detail (lispm-desktop-popup--wrap detail text-width 'lispm-desktop-platinum-list)))
         (body (+ border caption 16))
         (text-bottom (+ body (* 16 (length lines)) (if details (+ 6 (* 16 (length details))) 0)))
         (top (+ (max text-bottom (+ body 32)) 16))
         (ordered (lispm-desktop-look-button-order buttons))
         (widths (mapcar (lambda (button) (lispm-desktop-look-push-button-width (car button))) ordered))
         (total (+ (apply #'+ widths) (* 10 (max 0 (1- (length ordered))))))
         (x (/ (- w total) 2))
         (placed nil))
    (cl-mapc (lambda (button width)
               (pcase-let ((`(,label ,value ,role) button))
                 (push (list label value role x top width) placed)
                 (setq x (+ x width 10))))
             ordered widths)
    (list :width w
          :height (+ top lispm-desktop-platinum-button-height 14 border)
          :message lines :detail details
          :buttons (seq-sort-by (lambda (button) (seq-position buttons (nth 1 button)
                                                               (lambda (b value) (equal (nth 1 b) value))))
                                #'< placed))))

(defun lispm-desktop-win3-svg-alert-icon (svg icon x y)
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

(cl-defmethod lispm-desktop-popup-draw-alert (palette icon message detail buttons pressed focused
                                                      &context (lispm-desktop-look (eql win3)))
  "Return Windows 3.2's message box of ICON, MESSAGE, DETAIL and BUTTONS.
A dialog border, the caption with its control-menu box, a white body
holding the icon, the message and the buttons, see
`lispm-desktop-popup--alert-layout'.  PRESSED and FOCUSED are values of
buttons, held down or holding the keyboard."
  (let* ((layout (lispm-desktop-popup--alert-layout message detail buttons))
         (w (plist-get layout :width))
         (h (plist-get layout :height))
         (b lispm-desktop-win3--alert-border)
         (caption lispm-desktop-platinum-title-bar-height)
         (svg (svg-create w h))
         (y (+ b caption 16)))
    (lispm-desktop-look-svg-fill svg 0 0 w h (plist-get palette :border))
    (lispm-desktop-look-svg-outline svg (1- b) (1- b) (- w (* 2 (1- b))) (- h (* 2 (1- b)))
                                    (plist-get palette :outline))
    (lispm-desktop-look-svg-fill svg b (+ b caption) (- w (* 2 b)) (- h b b caption)
                                 (lispm-desktop-win3--pane palette))
    (dom-append-child svg (let ((part (lispm-desktop-win3-draw-control-box palette)))
                            (dom-set-attribute part 'x b) (dom-set-attribute part 'y b) part))
    (let ((title (lispm-desktop-win3-draw-caption
                  palette (- w b b lispm-desktop-win3-button-width 1) t lispm-desktop-look-alert-title
                  (/ (- (- w b b lispm-desktop-win3-button-width 1)
                        (lispm-desktop-platinum-content-width lispm-desktop-look-alert-title
                                                              'lispm-desktop-platinum-title))
                     2))))
      (dom-set-attribute title 'x (+ b lispm-desktop-win3-button-width 1))
      (dom-set-attribute title 'y b)
      (dom-append-child svg title))
    (lispm-desktop-win3-svg-alert-icon svg icon (+ b 16) y)
    (dolist (line (plist-get layout :message))
      (lispm-desktop-platinum-svg-content svg line lispm-desktop-look-alert-text-x y 16
                                          (plist-get palette :text) 'lispm-desktop-platinum-title)
      (setq y (+ y 16)))
    (when (plist-get layout :detail)
      (setq y (+ y 6))
      (dolist (line (plist-get layout :detail))
        (lispm-desktop-platinum-svg-content svg line lispm-desktop-look-alert-text-x y 16
                                            (plist-get palette :text) 'lispm-desktop-platinum-list)
        (setq y (+ y 16))))
    (pcase-dolist (`(,label ,value ,role ,x ,top ,width) (plist-get layout :buttons))
      (lispm-desktop-platinum-svg-push-button svg x top palette label width
                                              :state (and (equal pressed value) 'pressed)
                                              :default (eq role 'default)
                                              :focused (and focused (equal focused value))))
    svg))

;;;; Controls

(cl-defmethod lispm-desktop-platinum-svg-focus-ring (svg x y width height _radius palette
                                                         &context (lispm-desktop-look (eql win3)))
  "Mark on SVG the control in the WIDTH by HEIGHT box at X, Y as Windows
marked the one the keyboard was on: a dotted rectangle just inside it."
  (lispm-desktop-look-svg-dotted svg (+ x 3) (+ y 3) (- width 6) (- height 6)
                                 (plist-get palette :text)))

(cl-defmethod lispm-desktop-platinum-svg-push-button (svg x y palette label width
                                                          &context (lispm-desktop-look (eql win3))
                                                          &key state default focused font)
  "Draw on SVG at X, Y a Windows 3.2 push button WIDTH pixels wide.
Outlined in black, its corners cut; lit a pixel along its top and left
and shaded two along its bottom and right; the label centred in FONT,
the system font's by default.  DEFAULT doubles its outline; FOCUSED
dots a rectangle round the label; STATE `pressed' shades its top and
left and moves the label a pixel down and right, `disabled' greys the
label."
  (let* ((h lispm-desktop-platinum-button-height)
         (outline (plist-get palette :outline))
         (shadow (lispm-desktop-win3--shadow palette))
         (face (plist-get palette :chrome))
         (pressed (eq state 'pressed))
         (in (if default 2 1))
         (shift (if pressed 1 0)))
    (lispm-desktop-look-svg-fill svg (1+ x) y (- width 2) h outline)
    (lispm-desktop-look-svg-fill svg x (1+ y) width (- h 2) outline)
    (when default
      (lispm-desktop-look-svg-outline svg (1+ x) (1+ y) (- width 2) (- h 2) outline))
    (lispm-desktop-look-svg-fill svg (+ x in) (+ y in) (- width in in) (- h in in) face)
    (if pressed
        (lispm-desktop-look-svg-edges svg (+ x in) (+ y in) (- width in in) (- h in in) (list shadow) nil)
      (lispm-desktop-look-svg-edges svg (+ x in) (+ y in) (- width in in) (- h in in)
                                    (list (plist-get palette :light)) (list shadow shadow)))
    (let ((font (or font (lispm-desktop-platinum-svg-font))))
      (lispm-desktop-platinum--label svg label (+ x (/ width 2.0) shift) (+ y 14 shift)
                                     (if (eq state 'disabled) shadow (plist-get palette :text))
                                     font)
      (when focused
        (let ((text (min (- width 8) (+ 6 (string-pixel-width
                                           (propertize label 'face 'lispm-desktop-platinum-title))))))
          (lispm-desktop-look-svg-dotted svg (+ x (/ (- width text) 2) shift) (+ y 3 shift)
                                         text (- h 6) (plist-get palette :text)))))))

(cl-defmethod lispm-desktop-platinum--draw-bevel-button (svg x y palette width height
                                                             &context (lispm-desktop-look (eql win3))
                                                             &key state label face)
  "Draw on SVG at X, Y a Windows 3.2 toolbar button WIDTH by HEIGHT pixels:
outlined in black, lit along its top and left, shaded along its bottom
and right; pushed in when STATE is `pressed', its label greyed when
`disabled'.  LABEL is shown as Emacs would show it in FACE."
  (let* ((face (or face 'lispm-desktop-platinum-list))
         (shadow (lispm-desktop-win3--shadow palette))
         (pressed (eq state 'pressed))
         (shift (if pressed 1 0)))
    (lispm-desktop-look-svg-fill svg x y width height (plist-get palette :chrome))
    (lispm-desktop-look-svg-outline svg x y width height (plist-get palette :outline))
    (if pressed
        (lispm-desktop-look-svg-edges svg (1+ x) (1+ y) (- width 2) (- height 2) (list shadow) nil)
      (lispm-desktop-look-svg-edges svg (1+ x) (1+ y) (- width 2) (- height 2)
                                    (list (plist-get palette :light)) (list shadow)))
    (when label
      (let ((ink (if (eq state 'disabled) shadow (plist-get palette :text))))
        (lispm-desktop-platinum-svg-content
         svg (if (eq state 'disabled) (propertize label 'face `(:foreground ,ink)) label)
         (+ x shift (/ (- width (lispm-desktop-platinum-content-width label face)) 2)) (+ y shift) height
         ink face)))))

(cl-defmethod lispm-desktop-platinum-draw-header-strip (palette width
                                                                &context (lispm-desktop-look (eql win3))
                                                                &optional label left)
  "Return Windows 3.2's strip under a caption WIDTH pixels wide: grey over
a black line, LABEL centred in the small font, clear of LEFT pixels of
opening before it."
  (let* ((h lispm-desktop-platinum-header-strip-height)
         (left (or left 0))
         (svg (svg-create width h)))
    (lispm-desktop-look-svg-fill svg 0 0 width h (plist-get palette :chrome))
    (lispm-desktop-look-svg-fill svg 0 (1- h) width 1 (plist-get palette :outline))
    (when label
      (let ((x (- (/ (- (+ left width) (lispm-desktop-platinum-content-width label 'lispm-desktop-platinum-small))
                     2)
                  left)))
        (lispm-desktop-platinum-svg-content svg label (if (> left 0) (max 8 x) x) 0 (1- h)
                                            (plist-get palette :text) 'lispm-desktop-platinum-small)))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-header-button (palette label
                                                                 &context (lispm-desktop-look (eql win3))
                                                                 &optional state)
  "Return the opening of Windows 3.2's strip under a caption: a toolbar
button reading LABEL in STATE, on the strip's grey."
  (let* ((width (lispm-desktop-platinum-header-button-width label))
         (h lispm-desktop-platinum-header-strip-height)
         (svg (svg-create width h)))
    (lispm-desktop-look-svg-fill svg 0 0 width h (plist-get palette :chrome))
    (lispm-desktop-look-svg-fill svg 0 (1- h) width 1 (plist-get palette :outline))
    (lispm-desktop-platinum--draw-bevel-button svg 6 2 palette (- width 6)
                                               lispm-desktop-platinum-header-button-height
                                               :state state :label label
                                               :face 'lispm-desktop-platinum-small)
    svg))

(cl-defmethod lispm-desktop-platinum-draw-group-edge (palette width
                                                              &context (lispm-desktop-look (eql win3))
                                                              &optional title bottom)
  "Return an edge of Windows 3.2's group box WIDTH pixels wide: a black
line, TITLE breaking the top one near its left end; BOTTOM draws the
bottom edge instead."
  (let* ((h (if bottom (default-line-height) lispm-desktop-platinum-group-title-height))
         (y (if bottom 1 (/ h 2)))
         (svg (svg-create width h))
         (ink (plist-get palette :text)))
    (if bottom
        (progn (lispm-desktop-look-svg-fill svg 0 0 1 (1+ y) ink)
               (lispm-desktop-look-svg-fill svg (1- width) 0 1 (1+ y) ink))
      (lispm-desktop-look-svg-fill svg 0 y 1 (- h y) ink)
      (lispm-desktop-look-svg-fill svg (1- width) y 1 (- h y) ink))
    (lispm-desktop-look-svg-fill svg 0 y width 1 ink)
    (when title
      (let ((title-width (lispm-desktop-platinum-content-width title 'lispm-desktop-platinum-title)))
        (lispm-desktop-look-svg-fill svg 6 0 (+ title-width 8) h (lispm-desktop-win3--pane palette))
        (lispm-desktop-platinum-svg-content svg title 10 0 h ink 'lispm-desktop-platinum-title)))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-group-side (palette height
                                                              &context (lispm-desktop-look (eql win3)))
  "Return a side of Windows 3.2's group box: a black line, then the pane."
  (let ((svg (svg-create 2 height)))
    (lispm-desktop-look-svg-fill svg 0 0 1 height (plist-get palette :text))
    (lispm-desktop-look-svg-fill svg 1 0 1 height (lispm-desktop-win3--pane palette))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-tab (palette label width
                                                       &context (lispm-desktop-look (eql win3))
                                                       &key front face)
  "Return a tab WIDTH pixels wide reading LABEL, as tabbed dialogs of the
Windows 3.x days drew them: square, lit along the top and left, black
down the right over a shaded line, the top corners cut.  The tab in
FRONT stands two pixels taller and opens into the pane; one behind
stands on the pane's lit edge."
  (let* ((face (or face 'lispm-desktop-platinum-title))
         (h lispm-desktop-platinum-tab-height)
         (w width)
         (svg (svg-create w (+ h 3)))
         (pane (lispm-desktop-win3--pane palette))
         (light (plist-get palette :light))
         (shadow (lispm-desktop-win3--shadow palette))
         (outline (plist-get palette :outline))
         (top (if front 0 2))
         (bottom (if front (+ h 2) (1- h))))
    (lispm-desktop-look-svg-fill svg 0 0 w (+ h 3) (plist-get palette :chrome))
    (unless front
      (lispm-desktop-look-svg-fill svg 0 h w 1 light)
      (lispm-desktop-look-svg-fill svg 0 (1+ h) w 2 pane))
    (lispm-desktop-look-svg-fill svg 1 (1+ top) (- w 2) (- bottom top) pane)
    (lispm-desktop-look-svg-fill svg 2 top (- w 4) 1 light)
    (lispm-desktop-look-svg-fill svg 1 (1+ top) 1 (- bottom top) light)
    (lispm-desktop-look-svg-fill svg (- w 2) (1+ top) 1 (- bottom top) outline)
    (lispm-desktop-look-svg-fill svg (- w 3) (1+ top) 1 (- bottom top) shadow)
    (when front
      (lispm-desktop-look-svg-fill svg 0 h 1 1 light)
      (lispm-desktop-look-svg-fill svg (1- w) h 1 1 light))
    (lispm-desktop-platinum-svg-content
     svg label (/ (- w (lispm-desktop-platinum-content-width label face)) 2) top (- h top)
     (plist-get palette :text) face)
    svg))

(cl-defmethod lispm-desktop-platinum-draw-tab-edge (palette width
                                                            &context (lispm-desktop-look (eql win3)))
  "Return the pane's edge WIDTH pixels wide beside tabs: grey above, the
pane's lit line and the pane below."
  (let* ((h lispm-desktop-platinum-tab-height)
         (svg (svg-create width (+ h 3))))
    (lispm-desktop-look-svg-fill svg 0 0 width h (plist-get palette :chrome))
    (lispm-desktop-look-svg-fill svg 0 h width 1 (plist-get palette :light))
    (lispm-desktop-look-svg-fill svg 0 (1+ h) width 2 (lispm-desktop-win3--pane palette))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-placard (palette width height
                                                           &context (lispm-desktop-look (eql win3))
                                                           &key label font)
  "Return a status panel WIDTH by HEIGHT pixels, sunk into the grey:
shaded along its top and left, lit along its bottom and right."
  (let ((svg (svg-create width height)))
    (lispm-desktop-look-svg-fill svg 0 0 width height (plist-get palette :chrome))
    (lispm-desktop-look-svg-edges svg 0 0 width height
                                  (list (lispm-desktop-win3--shadow palette)) (list (plist-get palette :light)))
    (when label
      (let ((font (or font (lispm-desktop-platinum-svg-font 'lispm-desktop-platinum-list))))
        (lispm-desktop-platinum--label svg label (/ width 2.0)
                                       (round (+ (/ height 2.0) (* 0.35 (plist-get font :size))))
                                       (plist-get palette :text) font)))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-radio (palette on
                                                         &context (lispm-desktop-look (eql win3))
                                                         &key state focused pad)
  "Return Windows 3.2's option button: a white circle outlined in black, a
black dot when ON; grey when STATE is `disabled', the circle's inside
grey when `pressed'.  FOCUSED dots a ring round it."
  (let* ((pad (max (or pad 0) (if focused 2 0)))
         (size (+ 12 (* 2 pad)))
         (middle (/ size 2.0))
         (svg (svg-create size size))
         (ink (if (eq state 'disabled) (lispm-desktop-win3--shadow palette) (plist-get palette :outline))))
    (when focused
      (svg-circle svg middle middle 7 :fill "none" :stroke (plist-get palette :text)
                  :stroke-width 1 :stroke-dasharray "1,1"))
    (svg-circle svg middle middle 5.5 :stroke ink :stroke-width 1
                :fill (if (eq state 'pressed) (plist-get palette :chrome) (plist-get palette :content)))
    (when on
      (svg-circle svg middle middle 2.5 :fill ink))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-check-box (palette on
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
         (ink (if (eq state 'disabled) (lispm-desktop-win3--shadow palette) (plist-get palette :outline))))
    (when focused
      (lispm-desktop-look-svg-dotted svg (- x 2) (- y 2) 17 17 (plist-get palette :text)))
    (lispm-desktop-look-svg-fill svg x y 13 13 ink)
    (lispm-desktop-look-svg-fill svg (1+ x) (1+ y) 11 11
                                 (if (eq state 'pressed) (plist-get palette :chrome) (plist-get palette :content)))
    (when on
      (svg-line svg (+ x 1.5) (+ y 1.5) (+ x 11.5) (+ y 11.5) :stroke ink :stroke-width 1)
      (svg-line svg (+ x 11.5) (+ y 1.5) (+ x 1.5) (+ y 11.5) :stroke ink :stroke-width 1))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-list-check-box (palette state
                                                                  &context (lispm-desktop-look (eql win3)))
  "Return a list's twelve-pixel check box in Windows 3.2's form, crossed
when STATE is `on', a bar across it when `mixed'."
  (let ((svg (svg-create 12 12))
        (ink (plist-get palette :outline)))
    (lispm-desktop-look-svg-fill svg 0 0 12 12 ink)
    (lispm-desktop-look-svg-fill svg 1 1 10 10 (plist-get palette :content))
    (pcase state
      ('on (svg-line svg 1.5 1.5 10.5 10.5 :stroke ink :stroke-width 1)
           (svg-line svg 10.5 1.5 1.5 10.5 :stroke ink :stroke-width 1))
      ('mixed (lispm-desktop-look-svg-fill svg 3 5 6 2 ink)))
    svg))

(cl-defmethod lispm-desktop-platinum-svg-progress-bar (svg x y width fraction palette
                                                           &context (lispm-desktop-look (eql win3)))
  "Draw on SVG at X, Y Windows 3.2 Setup's gauge WIDTH pixels wide: a black
frame round white, FRACTION of it filled in the highlight colour."
  (let* ((h lispm-desktop-platinum-progress-height)
         (inner (- width 2))
         (filled (round (* inner (min 1.0 (max 0.0 fraction))))))
    (lispm-desktop-look-svg-fill svg x y width h (plist-get palette :outline))
    (lispm-desktop-look-svg-fill svg (1+ x) (1+ y) inner (- h 2) (plist-get palette :content))
    (lispm-desktop-look-svg-fill svg (1+ x) (1+ y) filled (- h 2) (plist-get palette :highlight))))

(cl-defmethod lispm-desktop-platinum-draw-triangle (palette open
                                                            &context (lispm-desktop-look (eql win3)))
  "Return a tree's disclosure box in PALETTE, twelve pixels square: a box
outlined in grey holding a minus when OPEN, a plus when closed."
  (let ((svg (svg-create 12 12))
        (ink (plist-get palette :text)))
    (lispm-desktop-look-svg-fill svg 1 1 9 9 (plist-get palette :content))
    (lispm-desktop-look-svg-outline svg 1 1 9 9 (lispm-desktop-win3--shadow palette))
    (lispm-desktop-look-svg-fill svg 3 5 5 1 ink)
    (unless open
      (lispm-desktop-look-svg-fill svg 5 3 1 5 ink))
    svg))

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
      (lispm-desktop-platinum--draw-bevel-button svg x 0 palette w h :state (and sorted 'pressed))
      (when label
        (lispm-desktop-platinum-svg-content svg label (+ x (or x-in label-x 8) (if sorted 1 0)) 0 h
                                            (plist-get palette :text) 'lispm-desktop-platinum-list))
      (setq x (+ x w)))
    (lispm-desktop-look-svg-arrow svg (- width 12) 8 4 'down (plist-get palette :text))
    svg))

;;;; Scroll bars

(defconst lispm-desktop-win3--scroll-button 16
  "Height in pixels of a scroll bar's arrow button, its outline included.")

(cl-defmethod lispm-desktop-platinum-scroll-layout (height &context (lispm-desktop-look (eql win3)))
  "Lay a Windows 3.2 scroll bar HEIGHT pixels tall out: an arrow button at
either end, the track between."
  (let ((b lispm-desktop-win3--scroll-button))
    (list :track b :track-length (max 0 (- height b b)) :up 0 :down (- height b) :arrow b)))

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
  (let ((b lispm-desktop-win3--scroll-button)
        (w lispm-desktop-platinum-scroll-width)
        (shadow (lispm-desktop-win3--shadow palette)))
    (lispm-desktop-look-svg-fill svg 0 y w b (plist-get palette :chrome))
    (lispm-desktop-look-svg-outline svg 0 y w b (plist-get palette :outline))
    (lispm-desktop-look-svg-edges svg 1 (1+ y) (- w 2) (- b 2) (list (plist-get palette :light)) (list shadow))
    (lispm-desktop-look-svg-arrow svg 4 (+ y 6) 4 direction
                                  (if enabled (plist-get palette :outline) shadow))))

(cl-defmethod lispm-desktop-platinum-draw-scroll-bar (palette height thumb active
                                                              &context (lispm-desktop-look (eql win3)))
  "Return Windows 3.2's scroll bar HEIGHT pixels tall, in PALETTE: arrow
buttons at both ends, the grey track between, outlined in black, and
THUMB a raised square on it.  Nothing to scroll, THUMB nil, greys the
arrows; an inactive bar, ACTIVE nil, shows no thumb."
  (let* ((w lispm-desktop-platinum-scroll-width)
         (svg (svg-create w height))
         (layout (lispm-desktop-platinum-scroll-layout height))
         (live (and active thumb))
         (shadow (lispm-desktop-win3--shadow palette)))
    (lispm-desktop-look-svg-fill svg 0 0 w height (plist-get palette :chrome))
    (lispm-desktop-look-svg-outline svg 0 0 w height (plist-get palette :outline))
    (lispm-desktop-win3--svg-scroll-button svg (plist-get layout :up) palette 'up live)
    (lispm-desktop-win3--svg-scroll-button svg (plist-get layout :down) palette 'down live)
    (when live
      (let ((top (car thumb)) (length (cdr thumb)))
        (lispm-desktop-look-svg-outline svg 0 top w length (plist-get palette :outline))
        (lispm-desktop-look-svg-edges svg 1 (1+ top) (- w 2) (- length 2)
                                      (list (plist-get palette :light)) (list shadow))))
    svg))

;;;; The Control Strip

(cl-defmethod lispm-desktop-platinum-draw-strip-segment (palette kind
                                                                 &context (lispm-desktop-look (eql win3))
                                                                 &key state icon menu focused behind side)
  "Return a segment of the Control Strip, as a Windows 3.2 toolbar drew
its buttons: KIND, STATE, ICON, MENU, FOCUSED, BEHIND and SIDE are as
Platinum's take them."
  (let* ((h lispm-desktop-platinum-strip-height)
         (w (lispm-desktop-platinum-strip-segment-width kind))
         (svg (svg-create w h))
         (ink (plist-get palette :outline))
         (shadow (lispm-desktop-win3--shadow palette)))
    (if (eq kind 'tab)
        (progn
          (lispm-desktop-look-svg-fill svg 0 0 w h (or behind (plist-get palette :desktop)))
          (lispm-desktop-look-svg-fill svg (if (eq side 'right) 4 0) 0 13 h (plist-get palette :chrome))
          (lispm-desktop-look-svg-outline svg (if (eq side 'right) 4 0) 0 13 h ink)
          (dotimes (i 4)
            (lispm-desktop-look-svg-fill svg (if (eq side 'right) 8 4) (+ 6 (* 3 i)) 5 1 shadow)))
      (lispm-desktop-platinum--draw-bevel-button svg 0 0 palette w h
                                                 :state (and (memq state '(pressed disabled)) state))
      (pcase kind
        ('collapse (lispm-desktop-look-svg-fill svg 4 11 7 2 ink))
        ((or 'left 'right)
         (lispm-desktop-look-svg-arrow svg 5 8 4 kind (if (eq state 'disabled) shadow ink)))
        ('module
         (when focused
           (lispm-desktop-look-svg-dotted svg 2 2 (- w 4) (- h 4) (plist-get palette :text)))
         (cond ((stringp icon) (lispm-desktop-platinum-svg-icon svg icon 3 4 16))
               (icon (lispm-desktop-platinum-svg-embed svg icon 3 4 16 16)))
         (when menu
           (lispm-desktop-look-svg-arrow svg 20 10 3 'down ink)))))
    svg))

;;;; The desktop

(cl-defmethod lispm-desktop--draw-icon (spec x y selected label-width family size ascent
                                             &context (lispm-desktop-look (eql win3)))
  "Return SVG of the icon SPEC with its title under it, as Windows 3.2
drew icons on the desktop: the title on the desktop's colour, or, when
SELECTED, in the caption's colours.  See `lispm-desktop--draw-icon'."
  (let* ((palette lispm-desktop-platinum-current-palette)
         (ground (if selected (plist-get palette :title) (lispm-desktop--color)))
         (ink (if selected
                  (plist-get palette :title-text)
                (lispm-desktop-platinum-legible ground (plist-get palette :text) (plist-get palette :content))))
         (center (+ x (/ lispm-desktop-column 2)))
         (label-center (lispm-desktop--label-center spec x))
         (label-left (- label-center (/ label-width 2)))
         (label-top (+ y lispm-desktop-icon-size 4)))
    (concat
     (lispm-desktop--nest (lispm-desktop--item-icon spec) nil (- center (/ lispm-desktop-icon-size 2)) y)
     (format "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"%s\"/>"
             label-left label-top label-width (+ size 4) ground)
     (format "<text x=\"%d\" y=\"%d\" font-family=\"%s\" font-size=\"%d\" text-anchor=\"middle\" fill=\"%s\">%s</text>"
             label-center (+ label-top 2 ascent) (lispm-desktop--escape family) size ink
             (lispm-desktop--escape (plist-get spec :label))))))

;;;; Faces

(cl-defmethod lispm-desktop-platinum-apply-look-faces (palette &context (lispm-desktop-look (eql win3)))
  "Set the faces Windows 3.2 draws otherwise than Platinum, in PALETTE:
lists and dialogs on the window colour, rows without Platinum's lit
lines, status panels sunk into the grey, fields framed in black."
  (let ((pane (lispm-desktop-win3--pane palette))
        (shadow (lispm-desktop-win3--shadow palette))
        (light (plist-get palette :light)))
    (set-face-attribute 'lispm-desktop-platinum-list-body nil :background pane)
    (set-face-attribute 'lispm-desktop-platinum-sorted nil :background pane)
    (set-face-attribute 'lispm-desktop-platinum-row nil :overline 'unspecified)
    (set-face-attribute 'lispm-desktop-platinum-placard nil
                        :box `(:line-width (1 . 1) :color ,shadow :style pressed-button)
                        :background (plist-get palette :chrome))
    (set-face-attribute 'lispm-desktop-platinum-placard-inactive nil
                        :box `(:line-width (1 . 1) :color ,shadow :style pressed-button)
                        :background (plist-get palette :chrome))
    (set-face-attribute 'lispm-desktop-platinum-header nil
                        :background (plist-get palette :chrome) :foreground (plist-get palette :text)
                        :box `(:line-width (1 . 4) :color ,(plist-get palette :chrome)))
    (set-face-attribute 'lispm-desktop-platinum-button nil
                        :background (plist-get palette :chrome) :foreground (plist-get palette :text)
                        :box `(:line-width (1 . 1) :color ,light :style released-button))
    (set-face-attribute 'tooltip nil :background (plist-get palette :menu)
                        :foreground (plist-get palette :menu-text))
    (set-face-attribute 'child-frame-border nil :background (plist-get palette :outline))
    ;; Tiled windows meet across a sizing border: black, grey, black.
    (set-face-attribute 'window-divider nil :foreground (plist-get palette :border))
    (set-face-attribute 'window-divider-first-pixel nil :foreground (plist-get palette :outline))
    (set-face-attribute 'window-divider-last-pixel nil :foreground (plist-get palette :outline))))

(provide 'lispm-desktop-win3)
;;; lispm-desktop-win3.el ends here
