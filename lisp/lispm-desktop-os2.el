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
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-popup)
(require 'lispm-desktop-surface)
(require 'lispm-desktop-i18n)

(lispm-desktop-look-define
 'os2
 :title "OS/2 2.0"
 :description (lispm-desktop-tr-noop "OS/2 2.0's Workplace Shell: teal title bars, pale yellow borders")
 :theme 'os2
 :icons '("os2")
 :system-families '("Helvetica" "Helv" "Arial")
 :list-families '("Helvetica" "Helv" "Arial"))

(defconst lispm-desktop-os2-button-width 18
  "Width in pixels of a title bar's buttons, the system menu's included.")

;;;; Colours

(defun lispm-desktop-os2--shadow (palette)
  "Return the shadow of PALETTE's raised parts: OS/2's dark grey."
  (or (plist-get palette :shade) (plist-get palette :outline)))

(defun lispm-desktop-os2--image (key draw)
  "Return the image of KEY, a drawing of this look, see
`lispm-desktop-platinum-image'."
  (lispm-desktop-platinum-image (cons 'os2 key) draw))

(defun lispm-desktop-os2--raise (svg x y width height palette &optional pressed)
  "Draw on SVG the WIDTH by HEIGHT rectangle at X, Y raised in PALETTE:
lit along its top and left, shaded along its bottom and right; PRESSED
turns its edges over."
  (let ((light (plist-get palette :light))
        (shadow (lispm-desktop-os2--shadow palette)))
    (lispm-desktop-look-svg-fill svg x y width height (plist-get palette :chrome))
    (if pressed
        (lispm-desktop-look-svg-edges svg x y width height (list shadow) (list light))
      (lispm-desktop-look-svg-edges svg x y width height (list light) (list shadow)))))

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
                                                       (fill . ,(lispm-desktop-os2--shadow palette))))
                                     (dom-node 'rect `((x . 1) (y . 1) (width . 1) (height . 1)
                                                       (fill . ,(lispm-desktop-os2--shadow palette))))))
    "url(#stipple)"))

;;;; Title bars

(defun lispm-desktop-os2--caption-colors (palette active)
  "Return the ground and the text of a title bar in PALETTE, ACTIVE or not."
  (if active
      (cons (plist-get palette :title) (plist-get palette :title-text))
    (cons (plist-get palette :title-inactive) (plist-get palette :title-inactive-text))))

(defun lispm-desktop-os2-draw-caption (palette width &optional active label label-x)
  "Return an SVG of a stretch of title bar WIDTH pixels wide, in PALETTE:
the bar's colour, ACTIVE or not, between grey lines along its top and
bottom.  LABEL, as Emacs would show it in the titles' face, is drawn
from column LABEL-X in the bar's text colour."
  (pcase-let* ((h lispm-desktop-platinum-title-bar-height)
               (svg (svg-create width h))
               (`(,ground . ,ink) (lispm-desktop-os2--caption-colors palette active))
               (shadow (lispm-desktop-os2--shadow palette)))
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

(defun lispm-desktop-os2-draw-system-button (palette icon &optional pressed)
  "Return an SVG of the system menu button, the window's ICON on a raised
square, between the title bar's grey lines.  ICON is a string as Emacs
would show it, its image included, or nil; PRESSED pushes it in."
  (let* ((w lispm-desktop-os2-button-width)
         (h lispm-desktop-platinum-title-bar-height)
         (svg (svg-create w h))
         (shadow (lispm-desktop-os2--shadow palette)))
    (lispm-desktop-look-svg-fill svg 0 0 w h shadow)
    (lispm-desktop-os2--raise svg 0 1 w (- h 2) palette pressed)
    (if icon
        (lispm-desktop-platinum-svg-content svg icon 1 2 (- h 4) (plist-get palette :text)
                                            'lispm-desktop-platinum-list)
      (lispm-desktop-look-svg-fill svg 4 9 10 2 (plist-get palette :text)))
    svg))

(defun lispm-desktop-os2-draw-caption-button (palette kind &optional pressed)
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
    (lispm-desktop-look-svg-fill svg 0 0 w h (lispm-desktop-os2--shadow palette))
    (lispm-desktop-os2--raise svg 0 1 w (- h 2) palette pressed)
    (if (eq kind 'minimize)
        (lispm-desktop-look-svg-pixels svg (+ 6 shift) (+ 7 shift)
                                       '("##..##" "#....#" "......" "......" "#....#" "##..##")
                                       `((?# . ,ink)))
      (lispm-desktop-look-svg-outline svg (+ 5 shift) (+ 6 shift) 8 8 ink))
    svg))

(cl-defmethod lispm-desktop-platinum-title-layout (width title-width
                                                         &context (lispm-desktop-look (eql os2))
                                                         &optional controls margins)
  "Lay an OS/2 2.0 title bar WIDTH pixels wide out for a title TITLE-WIDTH
wide: the system menu button opens it, minimize and maximize close it,
and the title is centred between them, narrowed to fit."
  (pcase-let* ((`(,close ,zoom ,collapse) (or controls '(on on on)))
               (b lispm-desktop-os2-button-width)
               (`(,lead . ,trail) (or margins '(0 . 0)))
               (lead (lispm-desktop-look-box-margin lead))
               (trail (lispm-desktop-look-box-margin trail))
               (start (+ lead (if close b 0)))
               (end (- width trail (if zoom b 0) (if collapse b 0)))
               (title (max 0 (min title-width (- end start 8))))
               (label-x (+ start (max 4 (/ (- end start title) 2)))))
    (list :start start :end end :title title :label-x label-x :margins (cons lead trail)
          :ridges nil)))

(defun lispm-desktop-os2-system-menu (window)
  "Drop WINDOW's system menu from its system menu button, as OS/2 did:
Restore, Minimize, Maximize and Close, those WINDOW cannot do disabled."
  (pcase-let* ((`(,_ ,zoom ,collapse) (lispm-desktop-platinum-window-controls window))
               (`(,left ,top ,_ ,_) (window-pixel-edges window))
               (margins (lispm-desktop-platinum-window-box-margins window)))
    (lispm-desktop-popup-menu
     (list (list :label (lispm-desktop-tr-in "OS/2 2.0" "Restore")
                 :enabled (and zoom (frame-parameter nil 'lispm-desktop-platinum-unzoom) t)
                 :action (lambda () (lispm-desktop-platinum-zoom-window window)))
           (list :label (lispm-desktop-tr-in "OS/2 2.0" "Minimize") :enabled (and collapse t)
                 :action (lambda () (lispm-desktop-platinum-collapse-window window)))
           (list :label (lispm-desktop-tr-in "OS/2 2.0" "Maximize") :enabled (and zoom t)
                 :action (lambda () (lispm-desktop-platinum-zoom-window window)))
           'separator
           (list :label (lispm-desktop-tr-in "OS/2 2.0" "Close")
                 :action (lambda () (lispm-desktop-platinum-close-window window))))
     (+ left (lispm-desktop-look-box-margin (car margins)))
     (+ top (window-tab-line-height window)))))

(cl-defmethod lispm-desktop-platinum-draw-title-bar (width active icon title modified controls
                                                           &context (lispm-desktop-look (eql os2))
                                                           &optional pressed status _edges margins)
  "Return an OS/2 2.0 title bar WIDTH pixels wide reading TITLE.
The system menu button shows ICON and drops the window's menu, minimize
collapses it and maximize zooms it; inactive windows keep their buttons,
as OS/2's did.  MODIFIED marks the title with a star; STATUS follows it."
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
                            (lispm-desktop-os2--image
                             (list 'caption (- to from) active
                                   (and text (lispm-desktop-platinum--content-key text))
                                   (- (plist-get layout :label-x) from))
                             (lambda ()
                               (lispm-desktop-os2-draw-caption palette (- to from) active text
                                                               (- (plist-get layout :label-x) from))))
                            'keymap lispm-desktop-platinum-title-map
                            'help-echo title-help))
              (button (kind box command help)
                (lispm-desktop-platinum-box-button
                 (propertize " " 'display
                             (lispm-desktop-os2--image
                              (list 'button kind (eq pressed box)
                                    (and (eq kind 'system) icon (lispm-desktop-platinum--content-key icon)))
                              (lambda ()
                                (if (eq kind 'system)
                                    (lispm-desktop-os2-draw-system-button palette icon (eq pressed box))
                                  (lispm-desktop-os2-draw-caption-button palette kind (eq pressed box))))))
                 box command help)))
      (concat
       (when (> lead 0) (caption 0 lead))
       (when close
         (button 'system 'close #'lispm-desktop-os2-system-menu
                 (lispm-desktop-tr "System menu: restore, minimize, maximize, close")))
       (caption start end label)
       (when collapse
         (button 'minimize 'collapse #'lispm-desktop-platinum-collapse-window
                 (lispm-desktop-tr "Minimize: roll up to the title bar")))
       (when zoom
         (button 'maximize 'zoom #'lispm-desktop-platinum-zoom-window
                 (lispm-desktop-tr-in "OS/2 2.0" "Maximize, or restore")))
       (when (> trail 0) (caption 0 trail))))))

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
         (shadow (lispm-desktop-os2--shadow palette)))
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
    (lispm-desktop-os2--raise svg 0 y s s palette)
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
    (lispm-desktop-look-svg-fill svg 0 (1- h) width 1 (lispm-desktop-os2--shadow palette))
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

(cl-defmethod lispm-desktop-popup-draw-menu (palette items width highlight
                                                     &context (lispm-desktop-look (eql os2)))
  "Return an OS/2 2.0 menu of ITEMS WIDTH pixels wide, in PALETTE: pale
grey, raised inside the frame's outline, HIGHLIGHT's item in the
highlight colour, separators grooved, a submenu's arrow on a small
raised button, disabled items grey."
  (pcase-let* ((`(,tops . ,height) (lispm-desktop-popup--rows items))
               (svg (svg-create width height))
               (icons (lispm-desktop-popup--icon-room items))
               (shadow (lispm-desktop-os2--shadow palette)))
    (lispm-desktop-look-svg-fill svg 0 0 width height (plist-get palette :menu))
    (lispm-desktop-look-svg-edges svg 0 0 width height (list (plist-get palette :light)) (list shadow))
    (seq-do-indexed
     (lambda (item index)
       (let ((top (nth index tops)))
         (if (lispm-desktop-popup--separator-p item)
             (let ((y (+ top (/ lispm-desktop-popup-separator-height 2) -1)))
               (lispm-desktop-look-svg-fill svg 3 y (- width 6) 1 shadow)
               (lispm-desktop-look-svg-fill svg 3 (1+ y) (- width 6) 1 (plist-get palette :light)))
           (let* ((h (lispm-desktop-popup--item-height))
                  (lit (eql index highlight))
                  (enabled (lispm-desktop-popup--enabled-p item))
                  (ink (cond (lit (plist-get palette :highlight-text))
                             (enabled (plist-get palette :menu-text))
                             (t shadow)))
                  (face (lispm-desktop-popup--face (or lit enabled)))
                  (key (and (not (plist-get item :submenu)) (lispm-desktop-popup--key item)))
                  (label (copy-sequence (plist-get item :label))))
             (add-face-text-property 0 (length label) `(:foreground ,ink) nil label)
             (when lit
               (lispm-desktop-look-svg-fill svg 2 top (- width 4) h (plist-get palette :highlight)))
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
                    (let ((s (- h 4)))
                      (lispm-desktop-os2--raise svg (- width s 4) (+ top 2) s s palette)
                      (lispm-desktop-look-svg-arrow svg (- width s -1) (+ top 2 (/ (- s 7) 2)) 4 'right
                                                    (plist-get palette :text))))
                   (key
                    (lispm-desktop-platinum-svg-content
                     svg (propertize key 'face `(:foreground ,ink))
                     (- width (lispm-desktop-popup--key-margin) (lispm-desktop-popup--label-width key))
                     top h ink face)))))))
     items)
    svg))

;;;; Message boxes

(defconst lispm-desktop-os2--alert-border 4
  "Depth in pixels of a message box's border, inside its outline.")

(cl-defmethod lispm-desktop-popup--alert-layout (message detail buttons
                                                         &context (lispm-desktop-look (eql os2)))
  "Lay an OS/2 2.0 message box of MESSAGE, DETAIL and BUTTONS out: under
the title bar the icon at the top left and the message beside it, the
buttons in a row centred along the bottom, the default first."
  (let* ((w lispm-desktop-look-alert-width)
         (border lispm-desktop-os2--alert-border)
         (caption lispm-desktop-platinum-title-bar-height)
         (text-width (- w lispm-desktop-look-alert-text-x 20))
         (lines (lispm-desktop-popup--wrap message text-width 'lispm-desktop-platinum-title))
         (details (and detail (lispm-desktop-popup--wrap detail text-width 'lispm-desktop-platinum-list)))
         (body (+ border caption 16))
         (text-bottom (+ body (* 16 (length lines)) (if details (+ 6 (* 16 (length details))) 0)))
         (top (+ (max text-bottom (+ body 32)) 16))
         (ordered (lispm-desktop-look-button-order buttons))
         (widths (mapcar (lambda (button) (lispm-desktop-look-push-button-width (car button))) ordered))
         (total (+ (apply #'+ widths) (* 8 (max 0 (1- (length ordered))))))
         (x (/ (- w total) 2))
         (placed nil))
    (cl-mapc (lambda (button width)
               (pcase-let ((`(,label ,value ,role) button))
                 (push (list label value role x top width) placed)
                 (setq x (+ x width 8))))
             ordered widths)
    (list :width w
          :height (+ top lispm-desktop-platinum-button-height 12 border)
          :message lines :detail details
          :buttons (seq-sort-by (lambda (button) (seq-position buttons (nth 1 button)
                                                               (lambda (b value) (equal (nth 1 b) value))))
                                #'< placed))))

(defun lispm-desktop-os2-svg-alert-icon (svg icon x y)
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

(cl-defmethod lispm-desktop-popup-draw-alert (palette icon message detail buttons pressed focused
                                                      &context (lispm-desktop-look (eql os2)))
  "Return OS/2 2.0's message box of ICON, MESSAGE, DETAIL and BUTTONS: a
border, the title bar with its system menu, a pale grey body holding the
icon, the message and the buttons.  PRESSED and FOCUSED are values of
buttons, held down or holding the keyboard."
  (let* ((layout (lispm-desktop-popup--alert-layout message detail buttons))
         (w (plist-get layout :width))
         (h (plist-get layout :height))
         (b lispm-desktop-os2--alert-border)
         (caption lispm-desktop-platinum-title-bar-height)
         (svg (svg-create w h))
         (y (+ b caption 16))
         (title lispm-desktop-look-alert-title))
    (lispm-desktop-look-svg-fill svg 0 0 w h (plist-get palette :border-inactive))
    (lispm-desktop-look-svg-edges svg 0 0 w h (list (plist-get palette :light))
                                  (list (lispm-desktop-os2--shadow palette)))
    (lispm-desktop-look-svg-fill svg b (+ b caption) (- w b b) (- h b b caption) (plist-get palette :chrome))
    (dolist (part (list (cons b (lispm-desktop-os2-draw-system-button palette nil))
                        (cons (+ b lispm-desktop-os2-button-width)
                              (lispm-desktop-os2-draw-caption
                               palette (- w b b lispm-desktop-os2-button-width) t title
                               (/ (- (- w b b lispm-desktop-os2-button-width)
                                     (lispm-desktop-platinum-content-width title 'lispm-desktop-platinum-title))
                                  2)))))
      (dom-set-attribute (cdr part) 'x (car part))
      (dom-set-attribute (cdr part) 'y b)
      (dom-append-child svg (cdr part)))
    (lispm-desktop-os2-svg-alert-icon svg icon (+ b 16) y)
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
                                                         &context (lispm-desktop-look (eql os2)))
  "Mark on SVG the control in the WIDTH by HEIGHT box at X, Y as OS/2 marked
the one the keyboard was on: a dotted rectangle just inside it."
  (lispm-desktop-look-svg-dotted svg (+ x 3) (+ y 3) (- width 6) (- height 6)
                                 (plist-get palette :text)))

(cl-defmethod lispm-desktop-platinum-svg-push-button (svg x y palette label width
                                                          &context (lispm-desktop-look (eql os2))
                                                          &key state default focused font)
  "Draw on SVG at X, Y an OS/2 2.0 push button WIDTH pixels wide: outlined
in black, raised two pixels deep, the label centred in FONT, the system
font's by default.  DEFAULT doubles its outline; FOCUSED dots a
rectangle round the label; STATE `pressed' pushes it in, `disabled'
greys the label."
  (let* ((h lispm-desktop-platinum-button-height)
         (outline (plist-get palette :outline))
         (light (plist-get palette :light))
         (shadow (lispm-desktop-os2--shadow palette))
         (pressed (eq state 'pressed))
         (in (if default 2 1))
         (shift (if pressed 1 0)))
    (lispm-desktop-look-svg-fill svg x y width h outline)
    (lispm-desktop-look-svg-fill svg (+ x in) (+ y in) (- width in in) (- h in in) (plist-get palette :chrome))
    (if pressed
        (lispm-desktop-look-svg-edges svg (+ x in) (+ y in) (- width in in) (- h in in)
                                      (list shadow shadow) (list light))
      (lispm-desktop-look-svg-edges svg (+ x in) (+ y in) (- width in in) (- h in in)
                                    (list light light) (list shadow shadow)))
    (let ((font (or font (lispm-desktop-platinum-svg-font))))
      (lispm-desktop-platinum--label svg label (+ x (/ width 2.0) shift) (+ y 14 shift)
                                     (if (eq state 'disabled) shadow (plist-get palette :text))
                                     font)
      (when focused
        (let ((text (min (- width 8) (+ 6 (string-pixel-width
                                           (propertize label 'face 'lispm-desktop-platinum-title))))))
          (lispm-desktop-look-svg-dotted svg (+ x (/ (- width text) 2) shift) (+ y 4 shift)
                                         text (- h 8) (plist-get palette :text)))))))

(cl-defmethod lispm-desktop-platinum--draw-bevel-button (svg x y palette width height
                                                             &context (lispm-desktop-look (eql os2))
                                                             &key state label face)
  "Draw on SVG at X, Y an OS/2 2.0 raised button WIDTH by HEIGHT pixels,
pushed in when STATE is `pressed', its label greyed when `disabled'.
LABEL is shown as Emacs would show it in FACE."
  (let* ((face (or face 'lispm-desktop-platinum-list))
         (pressed (eq state 'pressed))
         (shift (if pressed 1 0)))
    (lispm-desktop-os2--raise svg x y width height palette pressed)
    (when label
      (let ((ink (if (eq state 'disabled) (lispm-desktop-os2--shadow palette) (plist-get palette :text))))
        (lispm-desktop-platinum-svg-content
         svg (if (eq state 'disabled) (propertize label 'face `(:foreground ,ink)) label)
         (+ x shift (/ (- width (lispm-desktop-platinum-content-width label face)) 2)) (+ y shift) height
         ink face)))))

(cl-defmethod lispm-desktop-platinum-draw-header-strip (palette width
                                                                &context (lispm-desktop-look (eql os2))
                                                                &optional label left)
  "Return OS/2 2.0's strip under a title bar WIDTH pixels wide: pale grey
over a groove, LABEL in the small font, clear of LEFT pixels of opening."
  (let* ((h lispm-desktop-platinum-header-strip-height)
         (left (or left 0))
         (svg (svg-create width h)))
    (lispm-desktop-look-svg-fill svg 0 0 width h (plist-get palette :chrome))
    (lispm-desktop-look-svg-fill svg 0 (- h 2) width 1 (lispm-desktop-os2--shadow palette))
    (lispm-desktop-look-svg-fill svg 0 (1- h) width 1 (plist-get palette :light))
    (when label
      (let ((x (- (/ (- (+ left width) (lispm-desktop-platinum-content-width label 'lispm-desktop-platinum-small))
                     2)
                  left)))
        (lispm-desktop-platinum-svg-content svg label (if (> left 0) (max 8 x) x) 0 (- h 2)
                                            (plist-get palette :text) 'lispm-desktop-platinum-small)))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-header-button (palette label
                                                                 &context (lispm-desktop-look (eql os2))
                                                                 &optional state)
  "Return the opening of OS/2 2.0's strip under a title bar: a raised button
reading LABEL in STATE on the strip's grey."
  (let* ((width (lispm-desktop-platinum-header-button-width label))
         (h lispm-desktop-platinum-header-strip-height)
         (svg (svg-create width h)))
    (lispm-desktop-look-svg-fill svg 0 0 width h (plist-get palette :chrome))
    (lispm-desktop-look-svg-fill svg 0 (- h 2) width 1 (lispm-desktop-os2--shadow palette))
    (lispm-desktop-look-svg-fill svg 0 (1- h) width 1 (plist-get palette :light))
    (lispm-desktop-platinum--draw-bevel-button svg 6 2 palette (- width 6)
                                               lispm-desktop-platinum-header-button-height
                                               :state state :label label
                                               :face 'lispm-desktop-platinum-small)
    svg))

(cl-defmethod lispm-desktop-platinum-draw-group-edge (palette width
                                                              &context (lispm-desktop-look (eql os2))
                                                              &optional title bottom)
  "Return an edge of OS/2 2.0's group box WIDTH pixels wide, grooved: a
grey line beside a lit one, TITLE breaking the top near its left end;
BOTTOM draws the bottom edge instead."
  (let* ((h (if bottom (default-line-height) lispm-desktop-platinum-group-title-height))
         (y (if bottom 1 (/ h 2)))
         (svg (svg-create width h))
         (shadow (lispm-desktop-os2--shadow palette))
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

(cl-defmethod lispm-desktop-platinum-draw-group-side (palette height
                                                              &context (lispm-desktop-look (eql os2)))
  "Return a side of OS/2 2.0's group box: a grey line beside a lit one."
  (let ((svg (svg-create 2 height)))
    (lispm-desktop-look-svg-fill svg 0 0 1 height (lispm-desktop-os2--shadow palette))
    (lispm-desktop-look-svg-fill svg 1 0 1 height (plist-get palette :light))
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
         (shadow (lispm-desktop-os2--shadow palette))
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

(cl-defmethod lispm-desktop-platinum-draw-tab-edge (palette width
                                                            &context (lispm-desktop-look (eql os2)))
  "Return the page's edge WIDTH pixels wide beside tabs: grey above, the
page's lit line and the page below."
  (let* ((h lispm-desktop-platinum-tab-height)
         (svg (svg-create width (+ h 3))))
    (lispm-desktop-look-svg-fill svg 0 0 width h (plist-get palette :chrome))
    (lispm-desktop-look-svg-fill svg 0 h width 1 (plist-get palette :light))
    (lispm-desktop-look-svg-fill svg 0 (1+ h) width 2 (plist-get palette :content))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-placard (palette width height
                                                           &context (lispm-desktop-look (eql os2))
                                                           &key label font)
  "Return a status field WIDTH by HEIGHT pixels, sunk into the grey."
  (let ((svg (svg-create width height)))
    (lispm-desktop-look-svg-fill svg 0 0 width height (plist-get palette :chrome))
    (lispm-desktop-look-svg-edges svg 0 0 width height
                                  (list (lispm-desktop-os2--shadow palette)) (list (plist-get palette :light)))
    (when label
      (let ((font (or font (lispm-desktop-platinum-svg-font 'lispm-desktop-platinum-list))))
        (lispm-desktop-platinum--label svg label (/ width 2.0)
                                       (round (+ (/ height 2.0) (* 0.35 (plist-get font :size))))
                                       (plist-get palette :text) font)))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-radio (palette on
                                                         &context (lispm-desktop-look (eql os2))
                                                         &key state focused pad)
  "Return OS/2 2.0's radio button: a white circle sunk into the grey, a
black dot when ON; grey when STATE is `disabled', its inside grey when
`pressed'.  FOCUSED dots a ring round it."
  (let* ((pad (max (or pad 0) (if focused 2 0)))
         (size (+ 12 (* 2 pad)))
         (middle (/ size 2.0))
         (svg (svg-create size size))
         (ink (if (eq state 'disabled) (lispm-desktop-os2--shadow palette) (plist-get palette :outline))))
    (when focused
      (svg-circle svg middle middle 7 :fill "none" :stroke (plist-get palette :text)
                  :stroke-width 1 :stroke-dasharray "1,1"))
    (svg-circle svg middle middle 5.5 :stroke (lispm-desktop-os2--shadow palette) :stroke-width 1
                :fill (if (eq state 'pressed) (plist-get palette :chrome) (plist-get palette :content)))
    (svg-circle svg middle middle 4.5 :stroke ink :stroke-width 1 :fill "none")
    (when on
      (svg-circle svg middle middle 2 :fill ink))
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
         (ink (if (eq state 'disabled) (lispm-desktop-os2--shadow palette) (plist-get palette :outline))))
    (when focused
      (lispm-desktop-look-svg-dotted svg (- x 2) (- y 2) 17 17 (plist-get palette :text)))
    (lispm-desktop-look-svg-fill svg x y 13 13
                                 (if (eq state 'pressed) (plist-get palette :chrome) (plist-get palette :content)))
    (lispm-desktop-look-svg-edges svg x y 13 13 (list (lispm-desktop-os2--shadow palette) ink)
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
    (lispm-desktop-look-svg-edges svg 0 0 12 12 (list (lispm-desktop-os2--shadow palette) ink)
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
    (lispm-desktop-look-svg-edges svg x y width h (list (lispm-desktop-os2--shadow palette))
                                  (list (plist-get palette :light)))
    (lispm-desktop-look-svg-fill svg (1+ x) (1+ y) filled (- h 2) (plist-get palette :title))))

(cl-defmethod lispm-desktop-platinum-draw-triangle (palette open
                                                            &context (lispm-desktop-look (eql os2)))
  "Return a tree view's disclosure box in PALETTE, twelve pixels square:
outlined in black, a minus when OPEN, a plus when closed."
  (let ((svg (svg-create 12 12))
        (ink (plist-get palette :text)))
    (lispm-desktop-look-svg-fill svg 1 1 9 9 (plist-get palette :content))
    (lispm-desktop-look-svg-outline svg 1 1 9 9 ink)
    (lispm-desktop-look-svg-fill svg 3 5 5 1 ink)
    (unless open
      (lispm-desktop-look-svg-fill svg 5 3 1 5 ink))
    svg))

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
      (lispm-desktop-look-svg-fill svg (+ x 4) (- h 3) (- w 8) 1 (lispm-desktop-os2--shadow palette))
      (when label
        (let ((label (if sorted (propertize label 'face 'bold) label)))
          (lispm-desktop-platinum-svg-content
           svg label (+ x (/ (- w (lispm-desktop-platinum-content-width label 'lispm-desktop-platinum-list)) 2))
           0 (- h 3) (plist-get palette :text) 'lispm-desktop-platinum-list)))
      (lispm-desktop-look-svg-fill svg (+ x w -1) 2 1 (- h 4) (lispm-desktop-os2--shadow palette))
      (setq x (+ x w)))
    svg))

;;;; Scroll bars

(defconst lispm-desktop-os2--scroll-button 16
  "Height in pixels of a scroll bar's arrow button.")

(cl-defmethod lispm-desktop-platinum-scroll-layout (height &context (lispm-desktop-look (eql os2)))
  "Lay an OS/2 2.0 scroll bar HEIGHT pixels tall out: an arrow button at
either end, the track between."
  (let ((b lispm-desktop-os2--scroll-button))
    (list :track b :track-length (max 0 (- height b b)) :up 0 :down (- height b) :arrow b)))

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
         (b lispm-desktop-os2--scroll-button)
         (live (and active thumb))
         (shadow (lispm-desktop-os2--shadow palette)))
    (lispm-desktop-look-svg-fill svg 0 0 w height (lispm-desktop-os2--stipple svg palette))
    (dolist (button (list (cons (plist-get layout :up) 'up) (cons (plist-get layout :down) 'down)))
      (lispm-desktop-os2--raise svg 0 (car button) w b palette)
      (lispm-desktop-look-svg-arrow svg 4 (+ (car button) 6) 4 (cdr button)
                                    (if live (plist-get palette :text) shadow)))
    (when live
      (lispm-desktop-os2--raise svg 0 (car thumb) w (cdr thumb) palette))
    svg))

;;;; The Control Strip

(cl-defmethod lispm-desktop-platinum-draw-strip-segment (palette kind
                                                                 &context (lispm-desktop-look (eql os2))
                                                                 &key state icon menu focused behind side)
  "Return a segment of the Control Strip as raised OS/2 2.0 buttons: KIND,
STATE, ICON, MENU, FOCUSED, BEHIND and SIDE are as Platinum's take them."
  (let* ((h lispm-desktop-platinum-strip-height)
         (w (lispm-desktop-platinum-strip-segment-width kind))
         (svg (svg-create w h))
         (ink (plist-get palette :text))
         (shadow (lispm-desktop-os2--shadow palette)))
    (if (eq kind 'tab)
        (progn
          (lispm-desktop-look-svg-fill svg 0 0 w h (or behind (plist-get palette :desktop)))
          (lispm-desktop-os2--raise svg (if (eq side 'right) 4 0) 0 13 h palette)
          (dotimes (i 4)
            (lispm-desktop-look-svg-fill svg (if (eq side 'right) 8 4) (+ 6 (* 3 i)) 5 1 shadow)))
      (lispm-desktop-os2--raise svg 0 0 w h palette (eq state 'pressed))
      (pcase kind
        ('collapse (lispm-desktop-look-svg-fill svg 4 11 7 2 ink))
        ((or 'left 'right)
         (lispm-desktop-look-svg-arrow svg 5 8 4 kind (if (eq state 'disabled) shadow ink)))
        ('module
         (when focused
           (lispm-desktop-look-svg-dotted svg 2 2 (- w 4) (- h 4) ink))
         (cond ((stringp icon) (lispm-desktop-platinum-svg-icon svg icon 3 4 16))
               (icon (lispm-desktop-platinum-svg-embed svg icon 3 4 16 16)))
         (when menu
           (lispm-desktop-look-svg-arrow svg 20 10 3 'down ink)))))
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
         (center (+ x (/ lispm-desktop-column 2)))
         (icon-left (- center (/ lispm-desktop-icon-size 2)))
         (label-center (lispm-desktop--label-center spec x))
         (label-left (- label-center (/ label-width 2)))
         (label-top (+ y lispm-desktop-icon-size 4)))
    (concat
     (when selected
       (format "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"%s\"/>"
               (- icon-left 2) (- y 2) (+ lispm-desktop-icon-size 4) (+ lispm-desktop-icon-size 4)
               (plist-get palette :highlight)))
     (lispm-desktop--nest (lispm-desktop--item-icon spec) nil icon-left y)
     (when selected
       (format "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"%s\"/>"
               label-left label-top label-width (+ size 4) (plist-get palette :highlight)))
     (format "<text x=\"%d\" y=\"%d\" font-family=\"%s\" font-size=\"%d\" text-anchor=\"middle\" fill=\"%s\">%s</text>"
             label-center (+ label-top 2 ascent) (lispm-desktop--escape family) size ink
             (lispm-desktop--escape (plist-get spec :label))))))

;;;; Faces

(cl-defmethod lispm-desktop-platinum-apply-look-faces (palette &context (lispm-desktop-look (eql os2)))
  "Set the faces OS/2 2.0 draws otherwise than Platinum, in PALETTE: views
on the window colour, rows without Platinum's lit lines, status fields
sunk into the grey."
  (let ((pane (plist-get palette :content))
        (shadow (lispm-desktop-os2--shadow palette)))
    (set-face-attribute 'lispm-desktop-platinum-list-body nil :background pane)
    (set-face-attribute 'lispm-desktop-platinum-sorted nil :background pane)
    (set-face-attribute 'lispm-desktop-platinum-row nil :overline 'unspecified)
    (dolist (face '(lispm-desktop-platinum-placard lispm-desktop-platinum-placard-inactive))
      (set-face-attribute face nil :background (plist-get palette :chrome)
                          :box `(:line-width (1 . 1) :color ,shadow :style pressed-button)))
    (set-face-attribute 'lispm-desktop-platinum-header nil
                        :background (plist-get palette :chrome) :foreground (plist-get palette :text)
                        :box `(:line-width (1 . 4) :color ,(plist-get palette :chrome)))
    (set-face-attribute 'tooltip nil :background (plist-get palette :menu)
                        :foreground (plist-get palette :menu-text))
    (set-face-attribute 'child-frame-border nil :background shadow)
    ;; Tiled windows meet across a border: lit, pale grey, shaded.
    (set-face-attribute 'window-divider nil :foreground (plist-get palette :border-inactive))
    (set-face-attribute 'window-divider-first-pixel nil :foreground shadow)
    (set-face-attribute 'window-divider-last-pixel nil :foreground (plist-get palette :light))))

(provide 'lispm-desktop-os2)
;;; lispm-desktop-os2.el ends here
