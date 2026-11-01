;;; lispm-desktop-pc.el --- The PC looks' family: Windows 3.2 and OS/2 2.0  -*- lexical-binding: t; -*-

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

;; The family `pc': the looks of PC systems drawn alike, Windows 3.2's
;; (lispm-desktop-win3.el) and OS/2 2.0's (lispm-desktop-os2.el).  Their
;; windows open with a system menu box and close with minimize and
;; maximize; their message boxes centre a row of buttons under a caption;
;; their controls are grey rectangles, lit along the top and left and
;; shaded along the bottom and right.  This file draws what they draw
;; alike, by each look's metrics:
;;
;;   :box-width     a title bar box's width, a line parting it included
;;   :system-icon   non-nil when the system menu box shows the window's icon
;;   :bevels        the bevels, a plist from a name to (TOP-LEFT
;;                  BOTTOM-RIGHT), the palette's keys of the lines along a
;;                  rectangle's top and left, outermost first, and along its
;;                  bottom and right, see `lispm-ui-svg-edges';
;;                  :shadow names `lispm-desktop-pc-shadow'.  A look names
;;                  :raised and :pushed, toolbar buttons up and down,
;;                  :button and :button-pushed, push buttons inside their
;;                  outline, :tab, the Control Strip's end, :dialog, a
;;                  message box's frame, and :menu, a menu's, if any
;;   :button-cut    non-nil when push buttons have their corners cut
;;   :button-focus  how far in a push button's focus is dotted
;;   :alert-gap :alert-bottom  pixels between a message box's buttons, and
;;                  under them
;;   :dialog-ground :dialog-inset :dialog-body  a message box's ground, how
;;                  far in its :dialog bevel stands, and its body's colour
;;   :strip-lines   the lines under a header strip, top first
;;   :strip-ink     the Control Strip's marks
;;   :tree-box      a disclosure box's outline
;;   :group-side    a group box's side: its line, then the pane
;;   :radio-sunk :radio-dot  whether an option button is sunk in a grey
;;                  ring, and its dot's radius
;;   :child-frame-border :dividers  the colours round child frames, and of
;;                  a divider, its first pixel and its last
;;   :menu-inset    how far in from a menu's sides its highlight stands
;;   :menu-separator  (INSET LINE...), a menu's separator, its lines from
;;                  INSET to INSET from the sides
;;   :submenu-button  non-nil when a submenu's arrow stands on a button
;;
;; colours named by the palette's keys.  A look names its words too: the
;; help of its title bar's boxes, :box-help, and the items of its system
;; menu, :system-menu, each marked with `lispm-desktop-tr-noop'.  What a
;; look draws its own way it draws in methods of its own: the title bar's
;; stretches and boxes, see `lispm-desktop-pc-draw-caption', a menu
;; item's label, the message box's icons, and the rest of its controls.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-look)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-popup)
(require 'lispm-desktop-i18n)

;;;; Colours and bevels

(defun lispm-desktop-pc-shadow (palette)
  "Return the shadow of PALETTE's raised parts: Windows' and OS/2's dark grey."
  (or (plist-get palette :shade) (plist-get palette :outline)))

(defun lispm-desktop-pc-color (palette key)
  "Return the colour KEY of PALETTE; :shadow is `lispm-desktop-pc-shadow'."
  (if (eq key :shadow) (lispm-desktop-pc-shadow palette) (plist-get palette key)))

(defun lispm-desktop-pc-colors (palette keys)
  "Return the colours of PALETTE that KEYS name, see `lispm-desktop-pc-color'."
  (mapcar (lambda (key) (lispm-desktop-pc-color palette key)) keys))

(defun lispm-desktop-pc-svg-bevel (svg x y width height palette bevel)
  "Frame the WIDTH by HEIGHT rectangle at X, Y of SVG in BEVEL, one of the
look's :bevels, in PALETTE; a bevel the look has none of frames nothing."
  (pcase-let ((`(,lit ,shaded) (plist-get (lispm-ui-look-metric :bevels) bevel)))
    (lispm-ui-svg-edges svg x y width height
                        (lispm-desktop-pc-colors palette lit) (lispm-desktop-pc-colors palette shaded))))

(defun lispm-desktop-pc-svg-raise (svg x y width height palette &optional bevel)
  "Draw on SVG the WIDTH by HEIGHT rectangle at X, Y in PALETTE's grey,
framed in BEVEL, by default :raised."
  (lispm-ui-svg-paint svg x y width height (plist-get palette :chrome))
  (lispm-desktop-pc-svg-bevel svg x y width height palette (or bevel :raised)))

;;;; Title bars

(cl-defgeneric lispm-desktop-pc-draw-caption (palette width &optional active label label-x)
  "Return an SVG of a stretch of title bar WIDTH pixels wide, in PALETTE,
the bar's height, ACTIVE or not.  LABEL, as Emacs would show it in the
titles' face, is drawn from column LABEL-X in the bar's text colour.")

(cl-defgeneric lispm-desktop-pc-draw-system-box (palette &optional pressed icon)
  "Return an SVG of the system menu box, the look's :box-width wide, in
PALETTE: PRESSED while its menu drops; ICON is the window's, a string as
Emacs would show it, when the look's :system-icon shows it.")

(cl-defgeneric lispm-desktop-pc-draw-caption-button (palette kind &optional pressed)
  "Return an SVG of a title bar button of KIND, `minimize' or `maximize',
the look's :box-width wide, in PALETTE, PRESSED or not.")

(defun lispm-desktop-pc-caption-colors (palette active)
  "Return the ground and the text of a title bar in PALETTE, ACTIVE or not."
  (if active
      (cons (plist-get palette :title) (plist-get palette :title-text))
    (cons (plist-get palette :title-inactive) (plist-get palette :title-inactive-text))))

(cl-defmethod lispm-ui-title-layout (width title-width
                                           &context (lispm-ui-look (lispm-ui-look-family pc))
                                           &optional controls margins)
  "Lay a PC title bar WIDTH pixels wide out for a title TITLE-WIDTH wide.
The system menu box opens it, minimize and maximize close it, each the
look's :box-width; the title is centred between them, narrowed to fit."
  (pcase-let* ((`(,close ,zoom ,collapse) (or controls '(on on on)))
               (b (lispm-ui-look-metric :box-width))
               (`(,lead . ,trail) (or margins '(0 . 0)))
               (lead (lispm-desktop-look-box-margin lead))
               (trail (lispm-desktop-look-box-margin trail))
               (start (+ lead (if close b 0)))
               (end (- width trail (if zoom b 0) (if collapse b 0)))
               (title (max 0 (min title-width (- end start 8))))
               (label-x (+ start (max 4 (/ (- end start title) 2)))))
    (list :start start :end end :title title :label-x label-x :margins (cons lead trail)
          :ridges nil)))

(defun lispm-desktop-pc-system-menu (window)
  "Drop WINDOW's system menu from its system menu box, as Windows and OS/2
did: Restore, Minimize, Maximize and Close, those WINDOW cannot do
disabled, in the look's own words, its :system-menu."
  (pcase-let* ((`(,_ ,zoom ,collapse) (lispm-desktop-platinum-window-controls window))
               (`(,left ,top ,_ ,_) (window-pixel-edges window))
               (margins (lispm-desktop-platinum-window-box-margins window))
               (`(,restore ,minimize ,maximize ,close)
                (mapcar #'lispm-desktop-tr (lispm-ui-look-get :system-menu))))
    (lispm-ui-menu
     (list (list :label restore
                 :enabled (and zoom (frame-parameter nil 'lispm-desktop-platinum-unzoom) t)
                 :action (lambda () (lispm-desktop-platinum-zoom-window window)))
           (list :label minimize :enabled (and collapse t)
                 :action (lambda () (lispm-desktop-platinum-collapse-window window)))
           (list :label maximize :enabled (and zoom t)
                 :action (lambda () (lispm-desktop-platinum-zoom-window window)))
           'separator
           (list :label close
                 :action (lambda () (lispm-desktop-platinum-close-window window))))
     (+ left (lispm-desktop-look-box-margin (car margins)))
     (+ top (window-tab-line-height window)))))

(cl-defmethod lispm-desktop-platinum-draw-title-bar (width active icon title modified controls
                                                           &context (lispm-desktop-look (lispm-ui-look-family pc))
                                                           &optional pressed status _edges margins)
  "Return a PC title bar WIDTH pixels wide reading TITLE.
The system menu box drops the window's menu, showing ICON where the look
does, minimize collapses it and maximize zooms it; inactive windows keep
their buttons, as Windows' and OS/2's did.  MODIFIED marks the title with
a star; STATUS follows it."
  (pcase-let* ((label (concat title (when modified " *") (when status (concat "  " status))))
               (face (if active 'lispm-ui-title 'lispm-ui-title-inactive))
               (`(,close ,zoom ,collapse) controls)
               (layout (lispm-ui-title-layout
                        width (lispm-ui-text-width label face) controls margins))
               (`(,lead . ,trail) (plist-get layout :margins))
               (start (plist-get layout :start))
               (end (plist-get layout :end))
               (label (let ((label (copy-sequence label)))
                        (add-face-text-property 0 (length label) face t label)
                        (lispm-desktop-platinum-fit label (plist-get layout :title))))
               (title-help (lispm-desktop-platinum-title-help))
               (palette lispm-ui-current-palette)
               (icon (and (lispm-ui-look-metric :system-icon) icon))
               (`(,system-help ,minimize-help ,maximize-help)
                (mapcar #'lispm-desktop-tr (lispm-ui-look-get :box-help))))
    (cl-flet ((caption (from to &optional text)
                (propertize " " 'display
                            (lispm-ui-look-image
                             (list 'caption (- to from) active
                                   (and text (lispm-ui-content-key text))
                                   (- (plist-get layout :label-x) from))
                             (lambda ()
                               (lispm-desktop-pc-draw-caption palette (- to from) active text
                                                              (- (plist-get layout :label-x) from))))
                            'keymap lispm-desktop-platinum-title-map
                            'help-echo title-help))
              (button (kind box command help)
                (lispm-desktop-platinum-box-button
                 (propertize " " 'display
                             (lispm-ui-look-image
                              (list 'button kind (eq pressed box)
                                    (and (eq kind 'system) icon (lispm-ui-content-key icon)))
                              (lambda ()
                                (if (eq kind 'system)
                                    (lispm-desktop-pc-draw-system-box palette (eq pressed box) icon)
                                  (lispm-desktop-pc-draw-caption-button palette kind (eq pressed box))))))
                 box command help)))
      (concat
       (when (> lead 0) (caption 0 lead))
       (when close
         (button 'system 'close #'lispm-desktop-pc-system-menu system-help))
       (caption start end label)
       (when collapse
         (button 'minimize 'collapse #'lispm-desktop-platinum-collapse-window minimize-help))
       (when zoom
         (button 'maximize 'zoom #'lispm-desktop-platinum-zoom-window maximize-help))
       (when (> trail 0) (caption 0 trail))))))

;;;; Menus

(cl-defgeneric lispm-desktop-pc-menu-label (label ink)
  "Return a menu item's LABEL in INK, its own faces kept under it."
  (let ((label (copy-sequence label)))
    (add-face-text-property 0 (length label) `(:foreground ,ink) nil label)
    label))

(cl-defmethod lispm-ui-draw-menu (palette items width highlight
                                          &context (lispm-ui-look (lispm-ui-look-family pc)))
  "Return a PC menu of ITEMS WIDTH pixels wide, in PALETTE.
The menu colour inside the outline the menu's frame draws, framed in the
look's :menu bevel; the item under the mouse, HIGHLIGHT, in the
highlight colour, its :menu-inset in from the sides; check marks before
checked items, key equivalents flush right, arrows on those holding
submenus, on a button where the look's :submenu-button says, separators
its :menu-separator, disabled items grey."
  (pcase-let* ((`(,tops . ,height) (lispm-ui-menu--rows items))
               (svg (svg-create width height))
               (icons (lispm-ui-menu--icon-room items))
               (shadow (lispm-desktop-pc-shadow palette))
               (inset (lispm-ui-look-metric :menu-inset))
               (`(,line-inset . ,lines) (lispm-ui-look-metric :menu-separator)))
    (lispm-ui-svg-paint svg 0 0 width height (plist-get palette :menu))
    (lispm-desktop-pc-svg-bevel svg 0 0 width height palette :menu)
    (seq-do-indexed
     (lambda (item index)
       (let ((top (nth index tops)))
         (if (lispm-ui-menu--separator-p item)
             (seq-do-indexed (lambda (color i)
                               (lispm-ui-svg-paint
                                svg line-inset (+ top (/ lispm-ui-menu-separator-height 2) (- 1 (length lines)) i)
                                (- width line-inset line-inset) 1 color))
                             (lispm-desktop-pc-colors palette lines))
           (let* ((h (lispm-ui-menu--item-height))
                  (lit (eql index highlight))
                  (enabled (lispm-ui-menu--enabled-p item))
                  (ink (cond (lit (plist-get palette :highlight-text))
                             (enabled (plist-get palette :menu-text))
                             (t shadow)))
                  (face (lispm-ui-menu--face (or lit enabled)))
                  (key (and (not (plist-get item :submenu)) (lispm-ui-menu--key item))))
             (when lit
               (lispm-ui-svg-paint svg inset top (- width inset inset) h (plist-get palette :highlight)))
             (when (plist-get item :checked)
               (lispm-ui-svg-content svg (propertize "✓" 'face `(:foreground ,ink))
                                     (if lispm-ui-menu-small 3 5) top h ink face))
             (when-let* ((icon (plist-get item :icon))
                         (size (min lispm-ui-menu-icon-size h)))
               (lispm-ui-svg-emacs-image svg icon (lispm-ui-menu--label-x)
                                         (+ top (/ (- h size) 2)) size size))
             (lispm-ui-svg-content svg (lispm-desktop-pc-menu-label (plist-get item :label) ink)
                                   (+ (lispm-ui-menu--label-x) icons) top h ink face)
             (cond ((and (plist-get item :submenu) (lispm-ui-look-metric :submenu-button))
                    (let ((s (- h 4)))
                      (lispm-desktop-pc-svg-raise svg (- width s 4) (+ top 2) s s palette)
                      (lispm-ui-svg-arrow svg (- width s -1) (+ top 2 (/ (- s 7) 2)) 4 'right
                                          (plist-get palette :text))))
                   ((plist-get item :submenu)
                    (lispm-ui-svg-arrow svg (- width 10) (+ top (/ (- h 7) 2)) 4 'right ink))
                   (key
                    (lispm-ui-svg-content
                     svg (propertize key 'face `(:foreground ,ink))
                     (- width (lispm-ui-menu--key-margin) (lispm-ui-menu--label-width key))
                     top h ink face)))))))
     items)
    svg))

;;;; Message boxes

(defconst lispm-desktop-pc-alert-border 4
  "Depth in pixels of a message box's border, inside its outline.")

(cl-defgeneric lispm-desktop-pc-svg-alert-icon (svg icon x y)
  "Draw the look's message box ICON on SVG, its top left at X, Y:
`stop', `note' or `caution', thirty-two pixels square.")

(cl-defmethod lispm-ui-alert-layout (message detail buttons
                                             &context (lispm-ui-look (lispm-ui-look-family pc)))
  "Lay a PC message box of MESSAGE, DETAIL and BUTTONS out.
Under the caption, the icon at the top left and the message beside it;
the buttons in a row centred along the bottom, the default first, the
look's :alert-gap apart and :alert-bottom over the border."
  (let* ((w lispm-desktop-look-alert-width)
         (border lispm-desktop-pc-alert-border)
         (gap (lispm-ui-look-metric :alert-gap))
         (caption lispm-ui-title-bar-height)
         (text-width (- w lispm-desktop-look-alert-text-x 20))
         (lines (lispm-ui-alert--wrap message text-width 'lispm-ui-title))
         (details (and detail (lispm-ui-alert--wrap detail text-width 'lispm-ui-list)))
         (body (+ border caption 16))
         (text-bottom (+ body (* 16 (length lines)) (if details (+ 6 (* 16 (length details))) 0)))
         (top (+ (max text-bottom (+ body 32)) 16))
         (ordered (lispm-desktop-look-button-order buttons))
         (widths (mapcar (lambda (button) (lispm-desktop-look-push-button-width (car button))) ordered))
         (total (+ (apply #'+ widths) (* gap (max 0 (1- (length ordered))))))
         (x (/ (- w total) 2))
         (placed nil))
    (cl-mapc (lambda (button width)
               (pcase-let ((`(,label ,value ,role) button))
                 (push (list label value role x top width) placed)
                 (setq x (+ x width gap))))
             ordered widths)
    (list :width w
          :height (+ top lispm-ui-button-height (lispm-ui-look-metric :alert-bottom) border)
          :message lines :detail details
          :buttons (seq-sort-by (lambda (button) (seq-position buttons (nth 1 button)
                                                               (lambda (b value) (equal (nth 1 b) value))))
                                #'< placed))))

(cl-defmethod lispm-ui-draw-alert (palette icon message detail buttons pressed focused
                                           &context (lispm-ui-look (lispm-ui-look-family pc)))
  "Return a PC message box of ICON, MESSAGE, DETAIL and BUTTONS.
Its :dialog-ground framed in its :dialog bevel, the caption with its
system menu box, a body of its :dialog-body holding the icon, the
message and the buttons, see `lispm-ui-alert-layout'.
PRESSED and FOCUSED are values of buttons, held down or holding the
keyboard."
  (let* ((layout (lispm-ui-alert-layout message detail buttons))
         (w (plist-get layout :width))
         (h (plist-get layout :height))
         (b lispm-desktop-pc-alert-border)
         (box (lispm-ui-look-metric :box-width))
         (inset (lispm-ui-look-metric :dialog-inset))
         (caption lispm-ui-title-bar-height)
         (svg (svg-create w h))
         (y (+ b caption 16))
         (title lispm-desktop-look-alert-title))
    (lispm-ui-svg-paint svg 0 0 w h (plist-get palette (lispm-ui-look-metric :dialog-ground)))
    (lispm-desktop-pc-svg-bevel svg inset inset (- w inset inset) (- h inset inset) palette :dialog)
    (lispm-ui-svg-paint svg b (+ b caption) (- w b b) (- h b b caption)
                        (lispm-desktop-pc-color palette (lispm-ui-look-metric :dialog-body)))
    (dolist (part (list (cons b (lispm-desktop-pc-draw-system-box palette))
                        (cons (+ b box)
                              (lispm-desktop-pc-draw-caption
                               palette (- w b b box) t title
                               (/ (- (- w b b box)
                                     (lispm-ui-text-width title 'lispm-ui-title))
                                  2)))))
      (dom-set-attribute (cdr part) 'x (car part))
      (dom-set-attribute (cdr part) 'y b)
      (dom-append-child svg (cdr part)))
    (lispm-desktop-pc-svg-alert-icon svg icon (+ b 16) y)
    (lispm-ui-svg-alert-contents svg layout lispm-desktop-look-alert-text-x y palette
                                 pressed focused)
    svg))

;;;; Controls

(cl-defmethod lispm-ui-svg-focus-ring (svg x y width height _radius palette
                                           &context (lispm-ui-look (lispm-ui-look-family pc)))
  "Mark on SVG the control in the WIDTH by HEIGHT box at X, Y as Windows
and OS/2 marked the one the keyboard was on: a dotted rectangle just
inside it."
  (lispm-ui-svg-dotted svg (+ x 3) (+ y 3) (- width 6) (- height 6)
                       (plist-get palette :text)))

(cl-defmethod lispm-ui-svg-push-button (svg x y palette label width
                                            &context (lispm-ui-look (lispm-ui-look-family pc))
                                            &key state default focused font)
  "Draw on SVG at X, Y a PC push button WIDTH pixels wide.
Outlined in black, its corners cut where the look's :button-cut says,
framed inside in its :button bevel; the label centred in FONT, the
system font's by default.  DEFAULT doubles its outline; FOCUSED dots a
rectangle round the label, :button-focus in; STATE `pressed' frames it
in :button-pushed and moves the label a pixel down and right,
`disabled' greys the label."
  (let* ((h lispm-ui-button-height)
         (outline (plist-get palette :outline))
         (shadow (lispm-desktop-pc-shadow palette))
         (pressed (eq state 'pressed))
         (in (if default 2 1))
         (shift (if pressed 1 0))
         (inset (lispm-ui-look-metric :button-focus)))
    (if (lispm-ui-look-metric :button-cut)
        (progn
          (lispm-ui-svg-paint svg (1+ x) y (- width 2) h outline)
          (lispm-ui-svg-paint svg x (1+ y) width (- h 2) outline)
          (when default
            (lispm-ui-svg-outline svg (1+ x) (1+ y) (- width 2) (- h 2) outline)))
      (lispm-ui-svg-paint svg x y width h outline))
    (lispm-desktop-pc-svg-raise svg (+ x in) (+ y in) (- width in in) (- h in in) palette
                                (if pressed :button-pushed :button))
    (let ((font (or font (lispm-ui-svg-font))))
      (lispm-ui-svg-label svg label (+ x (/ width 2.0) shift) (+ y 14 shift)
                          (if (eq state 'disabled) shadow (plist-get palette :text))
                          font)
      (when focused
        (let ((text (min (- width 8) (+ 6 (string-pixel-width
                                           (propertize label 'face 'lispm-ui-title))))))
          (lispm-ui-svg-dotted svg (+ x (/ (- width text) 2) shift) (+ y inset shift)
                               text (- h inset inset) (plist-get palette :text)))))))

(cl-defmethod lispm-ui-svg-bevel-button (svg x y palette width height
                                             &context (lispm-ui-look (lispm-ui-look-family pc))
                                             &key state label face)
  "Draw on SVG at X, Y a PC toolbar button WIDTH by HEIGHT pixels, :raised,
or :pushed when STATE is `pressed', its label greyed when `disabled'.
LABEL is shown as Emacs would show it in FACE."
  (let* ((face (or face 'lispm-ui-list))
         (pressed (eq state 'pressed))
         (shift (if pressed 1 0)))
    (lispm-desktop-pc-svg-raise svg x y width height palette (and pressed :pushed))
    (when label
      (let ((ink (if (eq state 'disabled) (lispm-desktop-pc-shadow palette) (plist-get palette :text))))
        (lispm-ui-svg-content
         svg (if (eq state 'disabled) (propertize label 'face `(:foreground ,ink)) label)
         (+ x shift (/ (- width (lispm-ui-text-width label face)) 2)) (+ y shift) height
         ink face)))))

(cl-defmethod lispm-desktop-platinum--draw-header-ground (svg width palette
                                                              &context (lispm-desktop-look (lispm-ui-look-family pc)))
  "Draw on SVG the ground of a PC header strip WIDTH pixels wide, in
PALETTE: grey over the look's :strip-lines."
  (let* ((h lispm-desktop-platinum-header-strip-height)
         (lines (lispm-desktop-pc-colors palette (lispm-ui-look-metric :strip-lines))))
    (lispm-ui-svg-paint svg 0 0 width h (plist-get palette :chrome))
    (seq-do-indexed (lambda (color i)
                      (lispm-ui-svg-paint svg 0 (+ (- h (length lines)) i) width 1 color))
                    lines)))

(cl-defmethod lispm-desktop-platinum-draw-header-strip (palette width
                                                                &context (lispm-desktop-look (lispm-ui-look-family pc))
                                                                &optional label left)
  "Return a PC look's strip under a title bar WIDTH pixels wide: its
ground, LABEL centred in the small font above its lines, clear of LEFT
pixels of opening before it."
  (let* ((h lispm-desktop-platinum-header-strip-height)
         (left (or left 0))
         (svg (svg-create width h)))
    (lispm-desktop-platinum--draw-header-ground svg width palette)
    (when label
      (let ((x (- (/ (- (+ left width) (lispm-ui-text-width label 'lispm-ui-small))
                     2)
                  left)))
        (lispm-ui-svg-content svg label (if (> left 0) (max 8 x) x) 0
                              (- h (length (lispm-ui-look-metric :strip-lines)))
                              (plist-get palette :text) 'lispm-ui-small)))
    svg))

(cl-defmethod lispm-ui-draw-group-side (palette height
                                                &context (lispm-ui-look (lispm-ui-look-family pc)))
  "Return a side of a PC group box: the lines of the look's :group-side."
  (pcase-let ((svg (svg-create 2 height))
              (`(,line ,pane) (lispm-desktop-pc-colors palette (lispm-ui-look-metric :group-side))))
    (lispm-ui-svg-paint svg 0 0 1 height line)
    (lispm-ui-svg-paint svg 1 0 1 height pane)
    svg))

(cl-defmethod lispm-desktop-platinum-draw-tab-edge (palette width
                                                            &context (lispm-desktop-look (lispm-ui-look-family pc)))
  "Return the pane's edge WIDTH pixels wide beside tabs: grey above, the
pane's lit line and the pane below."
  (let* ((h lispm-desktop-platinum-tab-height)
         (svg (svg-create width (+ h 3))))
    (lispm-ui-svg-paint svg 0 0 width h (plist-get palette :chrome))
    (lispm-ui-svg-paint svg 0 h width 1 (plist-get palette :light))
    (lispm-ui-svg-paint svg 0 (1+ h) width 2 (plist-get palette :content))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-placard (palette width height
                                                           &context (lispm-desktop-look (lispm-ui-look-family pc))
                                                           &key label font state)
  "Return a status panel WIDTH by HEIGHT pixels, sunk into the grey:
shaded along its top and left, lit along its bottom and right, on an
inactive window (STATE `inactive') as on the active one, as Windows'."
  (ignore state)
  (let ((svg (svg-create width height)))
    (lispm-ui-svg-paint svg 0 0 width height (plist-get palette :chrome))
    (lispm-ui-svg-edges svg 0 0 width height
                        (list (lispm-desktop-pc-shadow palette)) (list (plist-get palette :light)))
    (when label
      (let ((font (or font (lispm-ui-svg-font 'lispm-ui-list))))
        (lispm-ui-svg-label svg label (/ width 2.0)
                            (round (+ (/ height 2.0) (* 0.35 (plist-get font :size))))
                            (plist-get palette :text) font)))
    svg))

(cl-defmethod lispm-ui-draw-radio (palette on
                                           &context (lispm-ui-look (lispm-ui-look-family pc))
                                           &key state focused pad)
  "Return a PC option button: a white circle outlined in black, in a grey
ring where the look's :radio-sunk says, a black dot of :radio-dot when
ON; grey when STATE is `disabled', the circle's inside grey when
`pressed'.  FOCUSED dots a ring round it."
  (let* ((pad (max (or pad 0) (if focused 2 0)))
         (size (+ 12 (* 2 pad)))
         (middle (/ size 2.0))
         (svg (svg-create size size))
         (sunk (lispm-ui-look-metric :radio-sunk))
         (ink (if (eq state 'disabled) (lispm-desktop-pc-shadow palette) (plist-get palette :outline))))
    (when focused
      (svg-circle svg middle middle 7 :fill "none" :stroke (plist-get palette :text)
                  :stroke-width 1 :stroke-dasharray "1,1"))
    (svg-circle svg middle middle 5.5 :stroke (if sunk (lispm-desktop-pc-shadow palette) ink) :stroke-width 1
                :fill (if (eq state 'pressed) (plist-get palette :chrome) (plist-get palette :content)))
    (when sunk
      (svg-circle svg middle middle 4.5 :stroke ink :stroke-width 1 :fill "none"))
    (when on
      (svg-circle svg middle middle (lispm-ui-look-metric :radio-dot) :fill ink))
    svg))

(cl-defmethod lispm-ui-draw-triangle (palette open
                                              &context (lispm-ui-look (lispm-ui-look-family pc)))
  "Return a tree's disclosure box in PALETTE, twelve pixels square: a box
outlined in the look's :tree-box holding a minus when OPEN, a plus when
closed."
  (let ((svg (svg-create 12 12))
        (ink (plist-get palette :text)))
    (lispm-ui-svg-paint svg 1 1 9 9 (plist-get palette :content))
    (lispm-ui-svg-outline svg 1 1 9 9 (lispm-desktop-pc-color palette (lispm-ui-look-metric :tree-box)))
    (lispm-ui-svg-paint svg 3 5 5 1 ink)
    (unless open
      (lispm-ui-svg-paint svg 5 3 1 5 ink))
    svg))

;;;; Scroll bars

(defconst lispm-desktop-pc-scroll-button 16
  "Height in pixels of a scroll bar's arrow button, its outline included.")

(cl-defmethod lispm-ui-scroll-layout (height &context (lispm-ui-look (lispm-ui-look-family pc)))
  "Lay a PC scroll bar HEIGHT pixels tall out: an arrow button at either
end, the track between."
  (let ((b lispm-desktop-pc-scroll-button))
    (list :track b :track-length (max 0 (- height b b)) :up 0 :down (- height b) :arrow b)))

;;;; The Control Strip

(cl-defmethod lispm-desktop-platinum-draw-strip-segment (palette kind
                                                                 &context (lispm-desktop-look (lispm-ui-look-family pc))
                                                                 &key state icon menu focused behind side)
  "Return a segment of the Control Strip, as a PC toolbar drew its
buttons, its marks in the look's :strip-ink, the tab at its end grey in
the :tab bevel: KIND, STATE, ICON, MENU, FOCUSED, BEHIND and SIDE are as
Platinum's take them."
  (let* ((h lispm-desktop-platinum-strip-height)
         (w (lispm-desktop-platinum-strip-segment-width kind))
         (svg (svg-create w h))
         (ink (lispm-desktop-pc-color palette (lispm-ui-look-metric :strip-ink)))
         (shadow (lispm-desktop-pc-shadow palette)))
    (if (eq kind 'tab)
        (let ((x (if (eq side 'right) 4 0)))
          (lispm-ui-svg-paint svg 0 0 w h (or behind (plist-get palette :desktop)))
          (lispm-desktop-pc-svg-raise svg x 0 13 h palette :tab)
          (dotimes (i 4)
            (lispm-ui-svg-paint svg (+ x 4) (+ 6 (* 3 i)) 5 1 shadow)))
      (lispm-ui-svg-bevel-button svg 0 0 palette w h
                                 :state (and (memq state '(pressed disabled)) state))
      (pcase kind
        ('collapse (lispm-ui-svg-paint svg 4 11 7 2 ink))
        ((or 'left 'right)
         (lispm-ui-svg-arrow svg 5 8 4 kind (if (eq state 'disabled) shadow ink)))
        ('module
         (when focused
           (lispm-ui-svg-dotted svg 2 2 (- w 4) (- h 4) (plist-get palette :text)))
         (cond ((stringp icon) (lispm-ui-svg-icon svg icon 3 4 16))
               (icon (lispm-ui-svg-embed svg icon 3 4 16 16)))
         (when menu
           (lispm-ui-svg-arrow svg 20 10 3 'down ink)))))
    svg))

;;;; Faces

(cl-defmethod lispm-desktop-platinum-apply-look-faces (palette &context (lispm-desktop-look (lispm-ui-look-family pc)))
  "Set the faces the PC looks draw otherwise than Platinum, in PALETTE:
lists and dialogs on the window colour, rows without Platinum's lit
lines, status panels sunk into the grey, child frames and dividers in
the look's :child-frame-border and :dividers."
  (let ((pane (plist-get palette :content))
        (shadow (lispm-desktop-pc-shadow palette)))
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
    (set-face-attribute 'child-frame-border nil
                        :background (lispm-desktop-pc-color palette (lispm-ui-look-metric :child-frame-border)))
    ;; Tiled windows meet across a border, between its first and last pixels.
    (pcase-let ((`(,divider ,first ,last) (lispm-desktop-pc-colors palette (lispm-ui-look-metric :dividers))))
      (set-face-attribute 'window-divider nil :foreground divider)
      (set-face-attribute 'window-divider-first-pixel nil :foreground first)
      (set-face-attribute 'window-divider-last-pixel nil :foreground last))))

(provide 'lispm-desktop-pc)
;;; lispm-desktop-pc.el ends here
