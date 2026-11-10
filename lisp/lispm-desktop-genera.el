;;; lispm-desktop-genera.el --- Symbolics Genera's look and Dynamic Windows chrome  -*- lexical-binding: t; -*-

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

;; The standalone look `genera': the high-contrast 1-bit monochrome
;; Dynamic Windows interface inspired by Symbolics Open Genera 2.0,
;; adapted for LispM Desktop and completely independent of both Mac OS 9
;; (`platinum') and Windows 3.2 (`pc'):
;;
;;   screen      full-screen `Dynamic Lisp Listener 1' with no desktop
;;               icons, no top menu bar, and no bottom-left Control Strip
;;   headers     paper-white pane label bars with bold-italic black titles,
;;               a 1-bit Command Menu box at the left, and Bury / Expand
;;               boxes at the right
;;   frames      1-bit double-rule Dynamic Windows borders (3px outer black
;;               rule, 2px white gap, 1px inner black rule)
;;   scrollbars  1-bit vertical and horizontal tracks with empty square
;;               end-boxes (`□') and 45-degree diagonal-hatched (`////')
;;               elevator bars
;;   who-line    white-ground Who-Line and 2-line inverse-video black Mouse
;;               Documentation Line at the bottom of the screen
;;   menus       1-bit Command Menus on paper white, framed in black with a
;;               2px drop edge, items inverted in solid black under the mouse

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-look)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-popup)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-surface)
(require 'lispm-desktop-i18n)

(defconst lispm-desktop-genera-button-width 18
  "Width in pixels of a Genera pane label bar's 1-bit control boxes.")

(defconst lispm-desktop-genera-box-width (1+ lispm-desktop-genera-button-width)
  "Width in pixels of a Genera pane label bar box including its dividing rule.")

(defconst lispm-desktop-genera-scroll-button 16
  "Size in pixels of a Genera scroll bar's square end-box.")

(defconst lispm-desktop-genera-alert-border 6
  "Depth in pixels of a Genera notification dialog's double-rule border.")

(defconst lispm-desktop-genera-families
  '("JetBrains Mono" "SF Mono" "Menlo" "Monaco" "DejaVu Sans Mono" "Courier")
  "Font families of Genera's label bars, Command Menus and Who-Line.")

(defconst lispm-desktop-genera-who-line-format
  '((:eval (lispm-desktop-genera-who-line)))
  "LispM's Who-Line on the root screen in the `genera' look.")

(lispm-ui-look-define
 'genera
 :title "Symbolics Genera"
 :description (lispm-desktop-tr-noop "Symbolics Genera's monochrome Dynamic Windows: inverse-video labels, Who-Line and Command Menus")
 :theme 'genera
 :system-families lispm-desktop-genera-families
 :list-families lispm-desktop-genera-families
 :menu-bar nil
 :control-strip nil
 :icon-side nil
 :floating-apps t
 :title-double-click 'zoom
 :placards #'lispm-desktop-genera-placards
 :mode-line lispm-desktop-genera-who-line-format
 :background-menu #'lispm-desktop-genera-system-menu
 :background-double-click #'lispm-desktop-genera-mouse-system-menu
 :background-open #'lispm-desktop-genera-command)

(defun lispm-desktop-genera--look-p (&optional look)
  "Return `genera' when LOOK is `genera' or inherits from `genera'."
  (let ((name (or look lispm-desktop-look)))
    (and (or (eq name 'genera)
             (eq (lispm-ui-look-get :parent name) 'genera)
             (eq (lispm-ui-look-family name) 'genera))
         'genera)))

(cl-generic-define-generalizer lispm-desktop-genera--generalizer
  50 (lambda (look &rest _) `(lispm-desktop-genera--look-p ,look))
  (lambda (tag &rest _) (and (eq tag 'genera) '((lispm-desktop-genera-look genera)))))

(cl-defmethod cl-generic-generalizers ((_specializer (head lispm-desktop-genera-look)))
  "Support for (lispm-desktop-genera-look genera) specializers."
  (list lispm-desktop-genera--generalizer))

;;;; Paper-white bold-italic pane headers and Pane Command Menu

(defun lispm-desktop-genera-draw-caption (palette width &optional active label label-x)
  "Return an SVG of a Genera Dynamic Windows pane header WIDTH pixels wide.
Panes wear a crisp paper-white header with bold-italic black text when
ACTIVE (italic when inactive) and a 1px solid black rule below."
  (let* ((h lispm-ui-title-bar-height)
         (svg (svg-create width h))
         (outline (plist-get palette :outline))
         (ground (plist-get palette :content))
         (ink (plist-get palette (if active :outline :muted))))
    (lispm-ui-svg-paint svg 0 0 width (1- h) ground)
    (lispm-ui-svg-paint svg 0 (1- h) width 1 outline)
    (when label
      (let ((label (copy-sequence label)))
        (add-face-text-property 0 (length label)
                                `(:foreground ,ink :slant italic :weight ,(if active 'bold 'normal))
                                nil label)
        (lispm-ui-svg-content
         svg label (or label-x 6) 0 (1- h) ink
         (if active 'lispm-ui-title 'lispm-ui-title-inactive))))
    svg))

(defun lispm-desktop-genera-draw-box (palette kind &optional pressed)
  "Return an SVG of a Genera 1-bit pane header box of KIND (`menu',
`collapse', `zoom' or `restore'), inverted in solid black when PRESSED."
  (let* ((w lispm-desktop-genera-button-width)
         (h lispm-ui-title-bar-height)
         (svg (svg-create (1+ w) h))
         (outline (plist-get palette :outline))
         (bg (if pressed outline (plist-get palette :content)))
         (fg (if pressed (plist-get palette :content) outline)))
    (if (eq kind 'menu)
        (progn
          (lispm-ui-svg-paint svg 0 0 w (1- h) bg)
          (lispm-ui-svg-paint svg w 0 1 h outline)
          (lispm-ui-svg-paint svg 0 (1- h) w 1 outline)
          (dolist (dy '(-4 0 4))
            (lispm-ui-svg-paint svg 4 (+ (/ h 2) dy -1) (- w 8) 1 fg)))
      (lispm-ui-svg-paint svg 0 0 1 h outline)
      (lispm-ui-svg-paint svg 1 0 w (1- h) bg)
      (lispm-ui-svg-paint svg 1 (1- h) w 1 outline)
      (pcase kind
        ('collapse
         (lispm-ui-svg-paint svg 5 (- h 7) (- w 8) 2 fg))
        ('restore
         (lispm-ui-svg-outline svg 7 4 7 6 fg)
         (lispm-ui-svg-paint svg 4 7 7 6 bg)
         (lispm-ui-svg-outline svg 4 7 7 6 fg)
         (lispm-ui-svg-paint svg 4 7 7 2 fg))
        (_
         (lispm-ui-svg-outline svg 5 4 (- w 8) (- h 9) fg)
         (lispm-ui-svg-paint svg 5 4 (- w 8) 2 fg))))
    svg))

(cl-defmethod lispm-ui-title-layout (width title-width
                                           &context (lispm-ui-look (lispm-desktop-genera-look genera))
                                           &optional _controls margins)
  "Lay a Genera Dynamic Windows pane header WIDTH pixels wide out:
a pure paper-white label bar with bold-italic text and no control buttons."
  (pcase-let* ((`(,lead . ,trail) (or margins '(0 . 0)))
               (lead (lispm-desktop-look-box-margin lead))
               (trail (lispm-desktop-look-box-margin trail))
               (start lead)
               (end (- width trail))
               (title (max 0 (min title-width (- end start 12))))
               (label-x (+ start 6)))
    (list :start start :end end :title title :label-x label-x :margins (cons lead trail)
          :ridges nil)))

(defvar lispm-desktop-editor--frame)
(declare-function lispm-desktop-editor-close "lispm-desktop-editor" ())

(defun lispm-desktop-genera-bury-window (&optional window)
  "Bury WINDOW (or the selected window) in Open Genera style:
hide its floating pane behind `Dynamic Lisp Listener 1' without killing
its buffers, or bury the buffer of a split pane."
  (interactive)
  (let* ((window (if (window-live-p window) window (selected-window)))
         (frame (window-frame window)))
    (cond
     ((and (bound-and-true-p lispm-desktop-editor--frame)
           (eq frame lispm-desktop-editor--frame)
           (fboundp 'lispm-desktop-editor-close))
      (lispm-desktop-editor-close))
     ((and (frame-live-p frame)
           (frame-parameter frame 'lispm-desktop-window))
      (let ((next (lispm-desktop-apps-focus-after frame)))
        (lispm-desktop-platinum-hide-window frame)
        (unless (or next (lispm-desktop-apps-hidden-p))
          (when (fboundp 'lispm-desktop-activate)
            (lispm-desktop-activate)))))
     ((window-combined-p window)
      (with-selected-window window
        (bury-buffer))))))

(defun lispm-desktop-genera-kill-window (&optional window)
  "Kill WINDOW (or the selected window) in Open Genera style:
close its floating application or Finder pane, close the floating Emacs
pane when WINDOW is its strip, sidebar or last main pane, or close a
split pane."
  (interactive)
  (let* ((window (if (window-live-p window) window (selected-window)))
         (frame (window-frame window))
         (buffer (window-buffer window)))
    (cond
     ((equal (buffer-name buffer) lispm-desktop-buffer-name)
      (message "Dynamic Lisp Listener 1 is the root screen."))
     ((and (bound-and-true-p lispm-desktop-editor--frame)
           (eq frame lispm-desktop-editor--frame)
           (fboundp 'lispm-desktop-editor-close))
      (cond
       ((or (lispm-desktop-platinum--box-function window :close)
            (window-parameter window 'window-side))
        (lispm-desktop-editor-close))
       ((cdr (lispm-desktop-platinum-main-windows frame))
        (lispm-desktop-platinum-close-window window))
       (t
        (when (lispm-desktop-platinum-confirm-close buffer)
          (unless (equal (buffer-name buffer) "*GNU Emacs*")
            (let ((kill-buffer-query-functions nil))
              (kill-buffer buffer)))
          (lispm-desktop-editor-close)))))
     (t
      (lispm-desktop-platinum-close-window window)))))

(defun lispm-desktop-genera--frame-screen-xy (frame x y)
  "Return X, Y in FRAME as (SCREEN-X . SCREEN-Y) on its root screen."
  (if-let* ((_ (frame-live-p frame))
            (parent (lispm-desktop-platinum-window-parent frame)))
      (pcase-let ((`(,wx . ,wy) (lispm-desktop-platinum-window-position frame))
                  (border (or (frame-parameter frame 'child-frame-border-width) 0)))
        (cons (+ wx border x) (+ wy border y)))
    (cons x y)))

(defun lispm-desktop-genera--window-screen-xy (window x y)
  "Return X, Y in WINDOW's frame as (SCREEN-X . SCREEN-Y) on the root screen."
  (lispm-desktop-genera--frame-screen-xy
   (and (window-live-p window) (window-frame window)) x y))

(defun lispm-desktop-genera--desktop-only-label-p (label)
  "Return non-nil when LABEL names a desktop-icon command irrelevant to Genera."
  (and (stringp label)
       (member label
               (list "Make Alias on Desktop"
                     "Remove from Desktop"
                     (and (fboundp 'lispm-desktop-tr)
                          (lispm-desktop-tr "Make Alias on Desktop"))
                     (and (fboundp 'lispm-desktop-tr)
                          (lispm-desktop-tr "Remove from Desktop"))))))

(defun lispm-desktop-genera--active-items (items)
  "Return menu ITEMS filtered to actionable, enabled commands in Genera.
Drops disabled items (`:enabled nil'), desktop-icon-only items, and submenus
whose active items are empty."
  (let ((out nil))
    (dolist (item items)
      (cond
       ((eq item 'separator)
        (push 'separator out))
       ((consp item)
        (let ((enabled (if (plist-member item :enabled)
                           (plist-get item :enabled)
                         t))
              (label (plist-get item :label))
              (submenu (plist-get item :submenu))
              (action (plist-get item :action)))
          (when (and enabled
                     (not (lispm-desktop-genera--desktop-only-label-p label)))
            (cond
             (submenu
              (if (functionp submenu)
                  (let ((copy (copy-sequence item)))
                    (plist-put copy :submenu
                               (lambda ()
                                 (lispm-desktop-genera--active-items
                                  (funcall submenu))))
                    (push copy out))
                (let ((active-sub (lispm-desktop-genera--active-items submenu)))
                  (when active-sub
                    (let ((copy (copy-sequence item)))
                      (plist-put copy :submenu active-sub)
                      (push copy out))))))
             (action
              (push item out))))))))
    (lispm-desktop-genera--dedup-items (nreverse out))))

(defun lispm-desktop-genera-window-operation-items (&optional window)
  "Return Dynamic Windows operation items for WINDOW (or the selected window).
Derived from `lispm-desktop-platinum-window-controls' and floating frame state."
  (let* ((window (if (window-live-p window) window (selected-window)))
         (frame (window-frame window))
         (floating (and (frame-live-p frame)
                        (frame-parameter frame 'lispm-desktop-window))))
    (pcase-let ((`(,close ,zoom ,_collapse)
                 (lispm-desktop-platinum-window-controls window)))
      (lispm-desktop-genera--active-items
       (list (list :label "Bury"
                   :enabled (and (or floating (window-combined-p window)) t)
                   :action (lambda () (lispm-desktop-genera-bury-window window)))
             (list :label "Expand"
                   :enabled (and zoom t)
                   :action (lambda () (lispm-desktop-platinum-zoom-window window)))
             (list :label "Move"
                   :enabled (and floating t)
                   :action (lambda ()
                             (when (frame-live-p frame)
                               (lispm-desktop-platinum-drag-outline frame 'move))))
             (list :label "Reshape"
                   :enabled (and floating t)
                   :action (lambda ()
                             (when (frame-live-p frame)
                               (lispm-desktop-platinum-drag-outline frame 'grow))))
             'separator
             (list :label "Kill"
                   :enabled (and close t)
                   :action (lambda () (lispm-desktop-genera-kill-window window)))
             (list :label "Refresh"
                   :action #'redraw-display))))))

(defun lispm-desktop-genera-window-menu (window &optional x y)
  "Pop up Genera's Pane Command Menu for WINDOW at X, Y (or under its label)."
  (pcase-let* ((`(,left ,top ,_ ,_) (window-pixel-edges window))
               (margins (lispm-desktop-platinum-window-box-margins window))
               (`(,sx . ,sy)
                (if (and x y)
                    (lispm-desktop-genera--window-screen-xy window x y)
                  (lispm-desktop-genera--window-screen-xy
                   window
                   (+ left (lispm-desktop-look-box-margin (car margins)))
                   (+ top (window-tab-line-height window)))))
               (frame (window-frame window))
               (app-menus (lispm-desktop-genera-app-menu-items))
               (select-items (and (not noninteractive)
                                  (lispm-desktop-genera-select-items))))
    (let ((lispm-ui-menu-parent
           (or (lispm-desktop-platinum-window-parent frame) frame)))
      (lispm-ui-menu
       (lispm-desktop-genera--active-items
        (append
         (lispm-desktop-genera-window-operation-items window)
         (when app-menus (cons 'separator app-menus))
         (list 'separator
               (list :label "Select"
                     :submenu #'lispm-desktop-genera-select-items)
               (list :label "Create"
                     :submenu (lambda () (lispm-desktop-genera-create-items t))))
         (when select-items (cons 'separator select-items))))
       sx sy))))

(defun lispm-desktop-genera-mouse-window-menu (event)
  "Pop up Genera's Pane Command Menu where EVENT right-clicked a pane label."
  (interactive "e")
  (let* ((posn (event-start event))
         (window (or (and posn (windowp (posn-window posn)) (posn-window posn))
                     (selected-window)))
         (xy (lispm-desktop-popup-frame-xy event)))
    (when (windowp window)
      (select-window window))
    (lispm-desktop-genera-window-menu window (car xy) (cdr xy))))

(defvar lispm-desktop-genera-title-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lispm-desktop-platinum-title-map)
    (keymap-set map "<tab-line> <down-mouse-2>" #'lispm-desktop-genera-mouse-window-menu)
    (keymap-set map "<tab-line> <mouse-2>" #'ignore)
    (keymap-set map "<tab-line> <down-mouse-3>" #'lispm-desktop-genera-mouse-window-menu)
    (keymap-set map "<tab-line> <mouse-3>" #'ignore)
    (keymap-set map "<tab-line> C-<down-mouse-1>" #'lispm-desktop-genera-mouse-window-menu)
    (keymap-set map "<tab-line> C-<mouse-1>" #'ignore)
    map)
  "Keymap of a Genera Dynamic Windows pane label bar:
Mouse-L drags the pane (`Move'); Mouse-R (or Mouse-M / C-Mouse-L) opens
the Pane Command Menu (`Bury', `Kill', `Move', `Reshape', `Expand',
`Select', `Create').")

(cl-defmethod lispm-desktop-platinum-draw-title-bar (width active _icon title modified controls
                                                           &context (lispm-desktop-look (lispm-desktop-genera-look genera))
                                                           &optional _pressed status _edges margins)
  "Return a Genera Dynamic Windows pane header WIDTH pixels wide reading TITLE:
a pure paper-white label bar with bold-italic black text and a 1px bottom
rule, without PC or Mac button boxes."
  (pcase-let* ((label (concat title (when modified " *") (when status (concat "  " status))))
               (face (if active 'lispm-ui-title 'lispm-ui-title-inactive))
               (layout (lispm-ui-title-layout
                        width (lispm-ui-text-width label face) controls margins))
               (label-x (plist-get layout :label-x))
               (label (let ((label (copy-sequence label)))
                        (add-face-text-property 0 (length label) face t label)
                        (lispm-desktop-platinum-fit label (plist-get layout :title))))
               (palette lispm-ui-current-palette))
    (propertize " " 'display
                (lispm-ui-look-image
                 (list 'genera-caption width active
                       (and label (lispm-ui-content-key label))
                       label-x)
                 (lambda ()
                   (lispm-desktop-genera-draw-caption
                    palette width active label label-x)))
                'keymap lispm-desktop-genera-title-map
                'help-echo "Mouse-L: Drag to Move;  Mouse-R: Window Operations (Bury, Kill, Reshape, Expand, Select, Create)")))

;;;; 1-bit Dynamic Windows double-rule pane frames and grow box

(cl-defmethod lispm-desktop-platinum-draw-window-frame (palette width height
                                                                &context (lispm-desktop-look (lispm-desktop-genera-look genera))
                                                                &key active)
  "Return a Genera 1-bit double-rule window frame WIDTH by HEIGHT pixels:
a thick 3px outer black border (1px when inactive), a paper-white gap, and an
inner 1px black rule around the pane."
  (let* ((w width)
         (h height)
         (svg (svg-create w h))
         (outline (plist-get palette :outline))
         (band (plist-get palette :content)))
    (lispm-ui-svg-paint svg 0 0 w h band)
    (lispm-ui-svg-outline svg 0 0 w h outline)
    (when active
      (lispm-ui-svg-outline svg 1 1 (- w 2) (- h 2) outline)
      (lispm-ui-svg-outline svg 2 2 (- w 4) (- h 4) outline))
    (lispm-ui-svg-paint svg 5 1 1 (- h 7) outline)
    (lispm-ui-svg-paint svg (- w 7) 1 1 (- h 7) outline)
    (lispm-ui-svg-paint svg 5 (- h 7) (- w 11) 1 outline)
    svg))

(cl-defmethod lispm-desktop-platinum-draw-grow-box (palette height
                                                            &context (lispm-desktop-look (lispm-desktop-genera-look genera)))
  "Return a 1-bit Genera corner resize box, sixteen pixels square, in PALETTE."
  (let* ((s 16)
         (svg (svg-create s height))
         (y (/ (- height s) 2))
         (outline (plist-get palette :outline)))
    (lispm-ui-svg-paint svg 0 0 s height (plist-get palette :chrome))
    (lispm-ui-svg-paint svg 0 y s s (plist-get palette :content))
    (lispm-ui-svg-outline svg 0 y s s outline)
    (lispm-ui-svg-paint svg 4 (+ y 10) 7 2 outline)
    (lispm-ui-svg-paint svg 9 (+ y 5) 2 7 outline)
    svg))

;;;; 1-bit Command Menus and Notification Alerts

(cl-defmethod lispm-ui-draw-menu (palette items width highlight
                                          &context (lispm-ui-look (lispm-desktop-genera-look genera)))
  "Return a 1-bit Genera Command Menu of ITEMS WIDTH pixels wide, in PALETTE:
paper white framed with a black border and 2px bottom/right drop shadow,
the highlighted row inverted in solid black."
  (pcase-let* ((`(,tops . ,height) (lispm-ui-menu--rows items))
               (svg (svg-create width height))
               (icons (lispm-ui-menu--icon-room items))
               (outline (plist-get palette :outline))
               (ground (plist-get palette :content))
               (muted (plist-get palette :muted))
               (inset 2))
    (lispm-ui-svg-paint svg 0 0 width height ground)
    (lispm-ui-svg-outline svg 0 0 width height outline)
    (lispm-ui-svg-paint svg 1 (1- height) (1- width) 1 outline)
    (lispm-ui-svg-paint svg (1- width) 1 1 (1- height) outline)
    (seq-do-indexed
     (lambda (item index)
       (let ((top (nth index tops)))
         (if (lispm-ui-menu--separator-p item)
             (lispm-ui-svg-paint svg 2 (+ top (/ lispm-ui-menu-separator-height 2))
                                 (- width 4) 1 outline)
           (let* ((h (lispm-ui-menu--item-height))
                  (lit (eql index highlight))
                  (enabled (lispm-ui-menu--enabled-p item))
                  (ink (cond (lit ground)
                             (enabled outline)
                             (t muted)))
                  (face (lispm-ui-menu--face (or lit enabled)))
                  (key (and (not (plist-get item :submenu)) (lispm-ui-menu--key item)))
                  (label (propertize (plist-get item :label) 'face `(:foreground ,ink))))
             (when lit
               (lispm-ui-svg-paint svg inset top (- width inset inset) h outline))
             (when (plist-get item :checked)
               (lispm-ui-svg-content svg (propertize "✓" 'face `(:foreground ,ink))
                                     (if lispm-ui-menu-small 3 5) top h ink face))
             (when-let* ((icon (plist-get item :icon))
                         (size (min lispm-ui-menu-icon-size h)))
               (lispm-ui-svg-emacs-image svg icon (lispm-ui-menu--label-x)
                                         (+ top (/ (- h size) 2)) size size))
             (lispm-ui-svg-content svg label (+ (lispm-ui-menu--label-x) icons) top h ink face)
             (cond ((plist-get item :submenu)
                    (lispm-ui-svg-arrow svg (- width 10) (+ top (/ (- h 7) 2)) 4 'right ink))
                   (key
                    (lispm-ui-svg-content
                     svg (propertize key 'face `(:foreground ,ink))
                     (- width (lispm-ui-menu--key-margin) (lispm-ui-menu--label-width key))
                     top h ink face)))))))
     items)
    svg))

(defun lispm-desktop-genera--svg-alert-icon (svg icon x y)
  "Draw a 1-bit monochrome Genera notification glyph for ICON at X, Y on SVG."
  (let ((s 32))
    (lispm-ui-svg-paint svg x y s s "#FFFFFF")
    (lispm-ui-svg-outline svg x y s s "#000000")
    (lispm-ui-svg-outline svg (+ x 2) (+ y 2) (- s 4) (- s 4) "#000000")
    (svg-text svg (pcase icon ('stop "ABORT") ('caution "WARN") (_ "LISP"))
              :x (+ x 16) :y (+ y 19) :text-anchor "middle" :fill "#000000"
              :font-family "monospace" :font-size 8 :font-weight "bold")))

(cl-defmethod lispm-ui-alert-layout (message detail buttons
                                             &context (lispm-ui-look (lispm-desktop-genera-look genera)))
  "Lay a 1-bit Genera notification box of MESSAGE, DETAIL and BUTTONS out."
  (let* ((w lispm-desktop-look-alert-width)
         (border lispm-desktop-genera-alert-border)
         (gap 10)
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
          :height (+ top lispm-ui-button-height 14 border)
          :message lines :detail details
          :buttons (seq-sort-by (lambda (button) (seq-position buttons (nth 1 button)
                                                               (lambda (b value) (equal (nth 1 b) value))))
                                #'< placed))))

(cl-defmethod lispm-ui-draw-alert (palette icon message detail buttons pressed focused
                                           &context (lispm-ui-look (lispm-desktop-genera-look genera)))
  "Return a 1-bit Genera notification dialog of ICON, MESSAGE, DETAIL and
BUTTONS."
  (let* ((layout (lispm-ui-alert-layout message detail buttons))
         (w (plist-get layout :width))
         (h (plist-get layout :height))
         (b lispm-desktop-genera-alert-border)
         (caption lispm-ui-title-bar-height)
         (svg (svg-create w h))
         (outline (plist-get palette :outline))
         (ground (plist-get palette :content))
         (y (+ b caption 16))
         (title lispm-desktop-look-alert-title)
         (header (lispm-desktop-genera-draw-caption palette (- w b b) t title 8)))
    (lispm-ui-svg-paint svg 0 0 w h ground)
    (lispm-ui-svg-outline svg 0 0 w h outline)
    (lispm-ui-svg-outline svg 1 1 (- w 2) (- h 2) outline)
    (lispm-ui-svg-outline svg 4 4 (- w 8) (- h 8) outline)
    (dom-set-attribute header 'x b)
    (dom-set-attribute header 'y b)
    (dom-append-child svg header)
    (lispm-desktop-genera--svg-alert-icon svg icon (+ b 16) y)
    (lispm-ui-svg-alert-contents svg layout lispm-desktop-look-alert-text-x y palette
                                 pressed focused)
    svg))

;;;; 1-bit Controls, Placards, Tabs and Scroll Bars

(cl-defmethod lispm-ui-svg-focus-ring (svg x y width height _radius palette
                                           &context (lispm-ui-look (lispm-desktop-genera-look genera)))
  "Mark on SVG the control in the WIDTH by HEIGHT box at X, Y with a 1-bit
dotted ring."
  (lispm-ui-svg-dotted svg (+ x 2) (+ y 2) (- width 4) (- height 4)
                       (plist-get palette :outline)))

(cl-defmethod lispm-ui-svg-push-button (svg x y palette label width
                                            &context (lispm-ui-look (lispm-desktop-genera-look genera))
                                            &key state default focused font)
  "Draw on SVG at X, Y a 1-bit Genera command button WIDTH pixels wide:
paper white outlined in black with a 2px bottom/right drop shadow, or
solid black inverse video when STATE is `pressed'."
  (let* ((h lispm-ui-button-height)
         (outline (plist-get palette :outline))
         (pressed (eq state 'pressed))
         (disabled (eq state 'disabled))
         (bg (if pressed outline (plist-get palette :content)))
         (fg (cond (pressed (plist-get palette :content))
                   (disabled (plist-get palette :muted))
                   (t outline)))
         (font (or font (lispm-ui-svg-font))))
    (lispm-ui-svg-paint svg x y width h bg)
    (lispm-ui-svg-outline svg x y width h outline)
    (when (and default (not pressed))
      (lispm-ui-svg-outline svg (1+ x) (1+ y) (- width 2) (- h 2) outline))
    (unless pressed
      (lispm-ui-svg-paint svg (+ x 2) (- (+ y h) 2) (- width 2) 2 outline)
      (lispm-ui-svg-paint svg (- (+ x width) 2) (+ y 2) 2 (- h 2) outline))
    (when focused
      (lispm-ui-svg-dotted svg (+ x 3) (+ y 3) (- width 6) (- h 6) fg))
    (lispm-ui-svg-label svg label (+ x (/ width 2.0)) (+ y 14) fg font)))

(cl-defmethod lispm-ui-svg-bevel-button (svg x y palette width height
                                             &context (lispm-ui-look (lispm-desktop-genera-look genera))
                                             &key state label face)
  "Draw on SVG at X, Y a 1-bit Genera button WIDTH by HEIGHT pixels,
inverted in solid black when STATE is `pressed'."
  (let* ((face (or face 'lispm-ui-list))
         (outline (plist-get palette :outline))
         (pressed (eq state 'pressed))
         (disabled (eq state 'disabled))
         (bg (if pressed outline (plist-get palette :content)))
         (fg (cond (pressed (plist-get palette :content))
                   (disabled (plist-get palette :muted))
                   (t outline))))
    (lispm-ui-svg-paint svg x y width height bg)
    (lispm-ui-svg-outline svg x y width height outline)
    (unless pressed
      (lispm-ui-svg-paint svg (1+ x) (+ y height -1) (1- width) 1 outline)
      (lispm-ui-svg-paint svg (+ x width -1) (1+ y) 1 (1- height) outline))
    (when label
      (lispm-ui-svg-content
       svg (propertize label 'face `(:foreground ,fg))
       (+ x (/ (- width (lispm-ui-text-width label face)) 2)) y height
       fg face))))

(cl-defmethod lispm-desktop-platinum--draw-header-ground (svg width palette
                                                              &context (lispm-desktop-look (lispm-desktop-genera-look genera)))
  "Draw on SVG the ground of a Genera header strip WIDTH pixels wide:
paper white over a 1px black rule."
  (let ((h lispm-desktop-platinum-header-strip-height))
    (lispm-ui-svg-paint svg 0 0 width h (plist-get palette :content))
    (lispm-ui-svg-paint svg 0 (1- h) width 1 (plist-get palette :outline))))

(cl-defmethod lispm-desktop-platinum-draw-header-strip (palette width
                                                                &context (lispm-desktop-look (lispm-desktop-genera-look genera))
                                                                &optional label left)
  "Return a Genera strip under a pane header WIDTH pixels wide."
  (let* ((h lispm-desktop-platinum-header-strip-height)
         (left (or left 0))
         (svg (svg-create width h)))
    (lispm-desktop-platinum--draw-header-ground svg width palette)
    (when label
      (let ((x (- (/ (- (+ left width) (lispm-ui-text-width label 'lispm-ui-small))
                     2)
                  left)))
        (lispm-ui-svg-content svg label (if (> left 0) (max 8 x) x) 0 (1- h)
                              (plist-get palette :outline) 'lispm-ui-small)))
    svg))

(cl-defmethod lispm-ui-draw-group-side (palette height
                                                &context (lispm-ui-look (lispm-desktop-genera-look genera)))
  "Return a side of a 1-bit Genera group box: a 1px black line and pane."
  (let ((svg (svg-create 2 height)))
    (lispm-ui-svg-paint svg 0 0 1 height (plist-get palette :outline))
    (lispm-ui-svg-paint svg 1 0 1 height (plist-get palette :content))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-placard (palette width height
                                                           &context (lispm-desktop-look (lispm-desktop-genera-look genera))
                                                           &key label font state)
  "Return a Genera Who-Line plate WIDTH by HEIGHT pixels: a crisp 1px
black-outlined box on paper white with a 1px drop edge when active."
  (let ((svg (svg-create width height))
        (outline (plist-get palette :outline))
        (active (not (eq state 'inactive))))
    (lispm-ui-svg-paint svg 0 0 width height (plist-get palette (if active :content :chrome)))
    (lispm-ui-svg-outline svg 0 0 width height outline)
    (when active
      (lispm-ui-svg-paint svg 1 (1- height) (1- width) 1 outline)
      (lispm-ui-svg-paint svg (1- width) 1 1 (1- height) outline))
    (when label
      (let ((font (or font (lispm-ui-svg-font 'lispm-ui-list))))
        (lispm-ui-svg-label svg label (/ width 2.0)
                            (round (+ (/ height 2.0) (* 0.35 (plist-get font :size))))
                            outline font)))
    svg))

(cl-defmethod lispm-ui-draw-check-box (palette on
                                               &context (lispm-ui-look (lispm-desktop-genera-look genera))
                                               &key state focused pad)
  "Return a 1-bit Genera check box: a square outlined in black, filled with
an inverse-video black square when ON."
  (let* ((pad (max (or pad 0) (if focused 2 0)))
         (svg (svg-create (+ 14 (* 2 pad)) (+ 14 (* 2 pad))))
         (x pad)
         (y (+ pad 1))
         (ink (if (eq state 'disabled) (plist-get palette :muted) (plist-get palette :outline))))
    (when focused
      (lispm-ui-svg-dotted svg (- x 2) (- y 2) 17 17 ink))
    (lispm-ui-svg-paint svg x y 13 13 ink)
    (lispm-ui-svg-paint svg (1+ x) (1+ y) 11 11 (plist-get palette :content))
    (when on
      (lispm-ui-svg-paint svg (+ x 3) (+ y 3) 7 7 ink))
    svg))

(cl-defmethod lispm-ui-draw-radio (palette on
                                           &context (lispm-ui-look (lispm-desktop-genera-look genera))
                                           &key state focused pad)
  "Return a 1-bit Genera option circle: a white circle outlined in black,
holding a solid black dot when ON."
  (let* ((pad (max (or pad 0) (if focused 2 0)))
         (size (+ 12 (* 2 pad)))
         (middle (/ size 2.0))
         (svg (svg-create size size))
         (ink (if (eq state 'disabled) (plist-get palette :muted) (plist-get palette :outline))))
    (when focused
      (svg-circle svg middle middle 7 :fill "none" :stroke ink
                  :stroke-width 1 :stroke-dasharray "1,1"))
    (svg-circle svg middle middle 5.5 :stroke ink :stroke-width 1
                :fill (plist-get palette :content))
    (when on
      (svg-circle svg middle middle 3.0 :fill ink))
    svg))

(cl-defmethod lispm-ui-draw-triangle (palette open
                                              &context (lispm-ui-look (lispm-desktop-genera-look genera)))
  "Return a 1-bit Genera tree disclosure box, twelve pixels square."
  (let ((svg (svg-create 12 12))
        (ink (plist-get palette :outline)))
    (lispm-ui-svg-paint svg 1 1 9 9 (plist-get palette :content))
    (lispm-ui-svg-outline svg 1 1 9 9 ink)
    (lispm-ui-svg-paint svg 3 5 5 1 ink)
    (unless open
      (lispm-ui-svg-paint svg 5 3 1 5 ink))
    svg))

(cl-defmethod lispm-desktop-platinum-draw-tab (palette label width
                                                       &context (lispm-desktop-look (lispm-desktop-genera-look genera))
                                                       &key front face)
  "Return a 1-bit Genera tab WIDTH pixels wide reading LABEL: inverse-video
black when in FRONT, paper white outlined in black when behind."
  (let* ((face (or face 'lispm-ui-title))
         (h lispm-desktop-platinum-tab-height)
         (w width)
         (svg (svg-create w (+ h 3)))
         (outline (plist-get palette :outline))
         (bg (if front outline (plist-get palette :content)))
         (fg (if front (plist-get palette :content) outline)))
    (lispm-ui-svg-paint svg 0 0 w (+ h 3) (plist-get palette :chrome))
    (lispm-ui-svg-paint svg 0 (1+ h) w 2 (plist-get palette :content))
    (lispm-ui-svg-paint svg 0 h w 1 outline)
    (lispm-ui-svg-paint svg 1 1 (- w 2) h bg)
    (lispm-ui-svg-outline svg 1 1 (- w 2) (1+ h) outline)
    (lispm-ui-svg-content
     svg (propertize label 'face `(:foreground ,fg))
     (/ (- w (lispm-ui-text-width label face)) 2) 1 h fg face)
    svg))

(cl-defmethod lispm-desktop-platinum-draw-tab-edge (palette width
                                                            &context (lispm-desktop-look (lispm-desktop-genera-look genera)))
  "Return the pane's top edge WIDTH pixels wide beside Genera tabs."
  (let* ((h lispm-desktop-platinum-tab-height)
         (svg (svg-create width (+ h 3))))
    (lispm-ui-svg-paint svg 0 0 width h (plist-get palette :chrome))
    (lispm-ui-svg-paint svg 0 h width 1 (plist-get palette :outline))
    (lispm-ui-svg-paint svg 0 (1+ h) width 2 (plist-get palette :content))
    svg))

(defun lispm-desktop-genera--svg-hatch (svg x y width height color)
  "Draw Genera's 1-bit 45-degree hatch (`////') in X, Y, WIDTH, HEIGHT, COLOR."
  (let ((step 3)
        (limit (+ width height)))
    (lispm-ui-svg-outline svg x y width height color)
    (let ((d 2))
      (while (< d limit)
        (let ((x1 (+ x (max 1 (- d height -1))))
              (y1 (+ y (min (1- height) d)))
              (x2 (+ x (min (1- width) d)))
              (y2 (+ y (max 1 (- d width -1)))))
          (when (and (< x1 (+ x width)) (< y2 (+ y height)))
            (svg-line svg x1 y1 x2 y2 :stroke color :stroke-width 1)))
        (setq d (+ d step))))))

(cl-defmethod lispm-ui-scroll-layout (height &context (lispm-ui-look (lispm-desktop-genera-look genera)))
  "Lay a Genera 1-bit scroll bar HEIGHT pixels tall out: a square end-box at
either end and the track between."
  (let ((b lispm-desktop-genera-scroll-button))
    (list :track b :track-length (max 0 (- height b b)) :up 0 :down (- height b) :arrow b)))

(cl-defmethod lispm-ui-draw-scroll-bar (palette height thumb active
                                                &context (lispm-ui-look (lispm-desktop-genera-look genera)))
  "Return a Genera 1-bit scroll bar HEIGHT pixels tall: a paper-white
track outlined in black, 1-bit empty square end-boxes at top and bottom,
and a 45-degree diagonal-hatched (`////') elevator bar."
  (let* ((w lispm-ui-scroll-width)
         (svg (svg-create w height))
         (layout (lispm-ui-scroll-layout height))
         (outline (plist-get palette :outline))
         (live (and active thumb))
         (btn lispm-desktop-genera-scroll-button))
    (lispm-ui-svg-paint svg 0 0 w height (plist-get palette :content))
    (lispm-ui-svg-outline svg 0 0 w height outline)
    (dolist (y (list (plist-get layout :up) (plist-get layout :down)))
      (lispm-ui-svg-outline svg 0 y w btn outline)
      (lispm-ui-svg-outline svg 3 (+ y 3) (- w 6) (- btn 6)
                            (if live outline (plist-get palette :muted))))
    (when live
      (lispm-desktop-genera--svg-hatch svg 1 (car thumb) (- w 2) (cdr thumb) outline))
    svg))

(cl-defmethod lispm-desktop--draw-icon (spec x y selected label-width family size ascent
                                             &context (lispm-desktop-look (lispm-desktop-genera-look genera)))
  "Return SVG of the icon SPEC in Genera's 1-bit monochrome style: its
label inverted in solid black when SELECTED."
  (let* ((palette lispm-ui-current-palette)
         (ground (if selected (plist-get palette :outline) (lispm-desktop--color)))
         (ink (if selected (plist-get palette :content) (plist-get palette :outline))))
    (concat
     (lispm-desktop--nest (lispm-desktop--item-icon spec) nil
                          (- (+ x (/ lispm-desktop-column 2)) (/ lispm-desktop-icon-size 2)) y)
     (lispm-desktop--icon-label spec x y label-width family size ascent ground ink))))

;;;; Dynamic Lisp Listener 1 (LispM-adapted root screen)

(defun lispm-desktop-genera--svg-keycap (x y label &optional bg fg)
  "Return an SVG 1-bit hexagonal LispM keycap at X, Y with LABEL."
  (let ((w (+ 10 (* 5 (length label))))
        (bg (or bg (plist-get lispm-ui-current-palette :content) "#FFFFFF"))
        (fg (or fg (plist-get lispm-ui-current-palette :outline) "#000000")))
    (concat
     (format "<polygon points=\"%d,%d %d,%d %d,%d %d,%d %d,%d %d,%d\" fill=\"%s\" stroke=\"%s\" stroke-width=\"1\"/>"
             x (- y 4) (+ x 4) (- y 8) (- (+ x w) 4) (- y 8)
             (+ x w) (- y 4) (- (+ x w) 4) y (+ x 4) y bg fg)
     (format "<text x=\"%d\" y=\"%d\" text-anchor=\"middle\" font-family=\"Helvetica, Geneva, sans-serif\" font-size=\"7\" font-weight=\"bold\" fill=\"%s\">%s</text>"
             (+ x (/ w 2)) (- y 2) fg label))))

(defun lispm-desktop-genera--xml-escape (text)
  "Escape XML special characters in TEXT for inline SVG rendering."
  (let ((s (or text "")))
    (setq s (string-replace "&" "&amp;" s))
    (setq s (string-replace "<" "&lt;" s))
    (setq s (string-replace ">" "&gt;" s))
    (string-replace "\"" "&quot;" s)))

(cl-defmethod lispm-desktop--background (width height &context (lispm-desktop-look (lispm-desktop-genera-look genera))
                                               &optional picture)
  "Return SVG of LispM's `Dynamic Lisp Listener 1' screen WIDTH by HEIGHT:
a double-rule outer frame, 1-bit diagonal-hatched scroll bars, the
`Dynamic Lisp Listener 1' pane label, LispM's system herald and subsystem
table, Listener keyboard/mouse instructions, and the 2-line inverse-video
black Mouse Documentation Line."
  (if picture
      (cl-call-next-method)
    (let* ((bg (or (plist-get lispm-ui-current-palette :content) "#FFFFFF"))
           (fg (or (plist-get lispm-ui-current-palette :outline) "#000000"))
           (cx (/ width 2))
           (doc-h 36)
           (doc-y (max 0 (- height doc-h)))
           (pane-b (max 20 (- doc-y 4)))
           (bar-y (max 20 (- pane-b 18)))
           (label-y (max 20 (- pane-b 11)))
           (left-x (max 64 (/ width 14)))
           (mid-x (+ left-x (max 260 (/ (* width 38) 100))))
           (right-x (max (+ left-x 420) (+ mid-x 24)))
           (top-y (max 42 (/ pane-b 13)))
           (table-y (+ top-y 118))
           (rule-y (max (+ table-y 94) (/ (* pane-b 46) 100)))
           (notice-y (+ rule-y 142))
           (prompt-y (min (- bar-y 18) (+ notice-y 104)))
           (host (lispm-desktop-genera--xml-escape (or (system-name) "LISPM-HOST")))
           (root-dir (lispm-desktop-genera--xml-escape
                      (abbreviate-file-name (or lispm-desktop-directory default-directory)))))
      (concat
       "<defs><pattern id=\"genera-hatch\" width=\"4\" height=\"4\" patternUnits=\"userSpaceOnUse\">"
       (format "<rect width=\"4\" height=\"4\" fill=\"%s\"/>" bg)
       (format "<path d=\"M-1,1 l2,-2 M0,4 l4,-4 M3,5 l2,-2\" stroke=\"%s\" stroke-width=\"1\"/>" fg)
       "</pattern></defs>"
       (format "<rect width=\"%d\" height=\"%d\" fill=\"%s\"/>" width height bg)
       ;; Double-rule outer Dynamic Windows screen frame (3px black + 2px white + 1px black).
       (format "<rect x=\"1.5\" y=\"1.5\" width=\"%d\" height=\"%d\" fill=\"none\" stroke=\"%s\" stroke-width=\"3\"/>"
               (max 0 (- width 3)) (max 0 (- doc-y 3)) fg)
       (format "<rect x=\"5.5\" y=\"5.5\" width=\"%d\" height=\"%d\" fill=\"none\" stroke=\"%s\" stroke-width=\"1\"/>"
               (max 0 (- width 11)) (max 0 (- pane-b 6)) fg)
       ;; Left 1-bit vertical scroll bar with empty square end-boxes and 45-degree diagonal hatch.
       (when (> pane-b 80)
         (concat
          (format "<rect x=\"18.5\" y=\"20.5\" width=\"7\" height=\"%d\" fill=\"url(#genera-hatch)\" stroke=\"%s\" stroke-width=\"1\"/>"
                  (- pane-b 48) fg)
          (format "<rect x=\"18.5\" y=\"20.5\" width=\"7\" height=\"7\" fill=\"%s\" stroke=\"%s\" stroke-width=\"1\"/>"
                  bg fg)
          (format "<rect x=\"18.5\" y=\"%d.5\" width=\"7\" height=\"7\" fill=\"%s\" stroke=\"%s\" stroke-width=\"1\"/>"
                  (- pane-b 35) bg fg)))
       ;; Bottom italic "Dynamic Lisp Listener 1" label and horizontal 45-degree diagonal-hatched scroll bar beside it.
       (when (and (> width 300) (> pane-b 100))
         (concat
          (format "<text x=\"34\" y=\"%d\" font-family=\"Helvetica, Geneva, sans-serif\" font-size=\"12\" font-style=\"italic\" fill=\"%s\">Dynamic Lisp Listener 1</text>"
                  label-y fg)
          (when (> width 360)
            (concat
             (format "<rect x=\"188.5\" y=\"%d.5\" width=\"%d\" height=\"7\" fill=\"url(#genera-hatch)\" stroke=\"%s\" stroke-width=\"1\"/>"
                     bar-y (- width 214) fg)
             (format "<rect x=\"188.5\" y=\"%d.5\" width=\"7\" height=\"7\" fill=\"%s\" stroke=\"%s\" stroke-width=\"1\"/>"
                     bar-y bg fg)
             (format "<rect x=\"%d.5\" y=\"%d.5\" width=\"7\" height=\"7\" fill=\"%s\" stroke=\"%s\" stroke-width=\"1\"/>"
                     (- width 33) bar-y bg fg)))))
       ;; LispM Desktop herald and Dynamic Lisp Listener 1 content.
       (when (and (> width 480) (> height 260))
         (concat
          (format "<text x=\"%d\" y=\"%d\" text-anchor=\"middle\" font-family=\"Helvetica, Geneva, sans-serif\" font-size=\"22\" font-style=\"italic\" fill=\"%s\">LispM Desktop™</text>"
                  cx top-y fg)
          (format "<text x=\"%d\" y=\"%d\" text-anchor=\"middle\" font-family=\"Helvetica, Geneva, sans-serif\" font-size=\"12\" fill=\"%s\">This machine is <tspan font-weight=\"bold\" font-style=\"italic\">%s</tspan>, a LispM Workstation,</text>"
                  cx (+ top-y 30) fg host)
          (format "<text x=\"%d\" y=\"%d\" text-anchor=\"middle\" font-family=\"Helvetica, Geneva, sans-serif\" font-size=\"12\" fill=\"%s\">running on <tspan font-weight=\"bold\" font-style=\"italic\">GNU Emacs %s</tspan> (%s).</text>"
                  cx (+ top-y 44) fg (lispm-desktop-genera--xml-escape emacs-version)
                  (lispm-desktop-genera--xml-escape system-configuration))
          (format "<text x=\"%d\" y=\"%d\" text-anchor=\"middle\" font-family=\"Helvetica, Geneva, sans-serif\" font-size=\"12\" fill=\"%s\">LispM Desktop Environment</text>"
                  cx (+ top-y 66) fg)
          (format "<text x=\"%d\" y=\"%d\" text-anchor=\"middle\" font-family=\"Helvetica, Geneva, sans-serif\" font-size=\"12\" fill=\"%s\">Loaded from %s</text>"
                  cx (+ top-y 80) fg root-dir)
          (format "<text x=\"%d\" y=\"%d\" text-anchor=\"middle\" font-family=\"Helvetica, Geneva, sans-serif\" font-size=\"12\" fill=\"%s\">Multi-agent Lisp workstation with Dynamic Windows, Focus Mode, Editor and Control Panels.</text>"
                  cx (+ top-y 94) fg)
          ;; LispM subsystem components table (shown when height permits).
          (when (> pane-b 460)
            (concat
             (mapconcat
              (pcase-lambda (`(,dy ,sys ,ver ,note))
                (concat
                 (format "<text x=\"%d\" y=\"%d\" font-family=\"Menlo, Monaco, monospace\" font-size=\"9\" fill=\"%s\">%s</text>"
                         (+ left-x 60) (+ table-y dy) fg sys)
                 (format "<text x=\"%d\" y=\"%d\" font-family=\"Menlo, Monaco, monospace\" font-size=\"9\" fill=\"%s\">%s</text>"
                         (- mid-x 64) (+ table-y dy) fg ver)
                 (format "<text x=\"%d\" y=\"%d\" font-family=\"Menlo, Monaco, monospace\" font-size=\"9\" fill=\"%s\">%s</text>"
                         mid-x (+ table-y dy) fg note)))
              `((0  "LispM Desktop" "0.1"            "Dynamic Windows, Who-Line and 1-bit monochrome chrome")
                (13 "LispM Agent"   "0.1"            "Multi-agent sessions, characters, diaries and workspace tools")
                (26 "LispM UI"      "0.1"            "Command menus, dialogs, forms and file chooser")
                (39 "LispM Browser" "0.1"            "Integrated web browser and documentation viewer")
                (52 "LispM Auth"    "0.1"            "Session lock, encrypted credentials and sandboxed execution")
                (65 "GNU Emacs"     ,emacs-version   "Native Elisp runtime, Zmacs-style buffers and Lisp Listener")
                (78 "Sandboxed Runtime (srt) and MCP tool integration enabled" "" "LispM Kit i18n"))
              "")))
          ;; Horizontal rule and Dynamic Lisp Listener 1 instructions for LispM.
          (when (> pane-b 340)
            (concat
             (format "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"%s\" stroke-width=\"1\"/>"
                     left-x rule-y (+ left-x 450) rule-y fg)
             (format "<text x=\"%d\" y=\"%d\" font-family=\"Helvetica, Geneva, sans-serif\" font-size=\"11\" fill=\"%s\">You are typing to <tspan font-style=\"italic\">Dynamic Lisp Listener 1.</tspan></text>"
                     left-x (+ rule-y 15) fg)
             (format "<text x=\"%d\" y=\"%d\" font-family=\"Helvetica, Geneva, sans-serif\" font-size=\"11\" fill=\"%s\">Press Return to enter a command in the LispM Command Processor.</text>"
                     left-x (+ rule-y 29) fg)
             (format "<text x=\"%d\" y=\"%d\" font-family=\"Helvetica, Geneva, sans-serif\" font-size=\"11\" fill=\"%s\">Press Control-</text>"
                     left-x (+ rule-y 43) fg)
             (lispm-desktop-genera--svg-keycap (+ left-x 76) (+ rule-y 43) "HELP" bg fg)
             (format "<text x=\"%d\" y=\"%d\" font-family=\"Helvetica, Geneva, sans-serif\" font-size=\"11\" fill=\"%s\">(or C-h ?) for Emacs and input editing commands.</text>"
                     (+ left-x 112) (+ rule-y 43) fg)
             (format "<text x=\"%d\" y=\"%d\" font-family=\"Helvetica, Geneva, sans-serif\" font-size=\"11\" fill=\"%s\">Type &quot;Help Commands&quot; to display a list of LispM Command Processor commands.</text>"
                     left-x (+ rule-y 62) fg)
             (format "<text x=\"%d\" y=\"%d\" font-family=\"Helvetica, Geneva, sans-serif\" font-size=\"11\" fill=\"%s\">Press</text>"
                     left-x (+ rule-y 76) fg)
             (lispm-desktop-genera--svg-keycap (+ left-x 34) (+ rule-y 76) "SELECT" bg fg)
             (lispm-desktop-genera--svg-keycap (+ left-x 82) (+ rule-y 76) "HELP" bg fg)
             (format "<text x=\"%d\" y=\"%d\" font-family=\"Helvetica, Geneva, sans-serif\" font-size=\"11\" fill=\"%s\">(or C-c ?) for the LispM Desktop Guide and applications.</text>"
                     (+ left-x 118) (+ rule-y 76) fg)
             (format "<text x=\"%d\" y=\"%d\" font-family=\"Helvetica, Geneva, sans-serif\" font-size=\"11\" fill=\"%s\">Press</text>"
                     left-x (+ rule-y 90) fg)
             (lispm-desktop-genera--svg-keycap (+ left-x 34) (+ rule-y 90) "FUNCTION" bg fg)
             (lispm-desktop-genera--svg-keycap (+ left-x 92) (+ rule-y 90) "HELP" bg fg)
             (format "<text x=\"%d\" y=\"%d\" font-family=\"Helvetica, Geneva, sans-serif\" font-size=\"11\" fill=\"%s\">(or C-c T) to enter Focus Mode (full-screen Agent &amp; Lisp Listener).</text>"
                     (+ left-x 128) (+ rule-y 90) fg)
             (format "<text x=\"%d\" y=\"%d\" font-family=\"Helvetica, Geneva, sans-serif\" font-size=\"11\" fill=\"%s\">Right-click (or click [Select] on the Who-Line) to open the LispM System Menu.</text>"
                     left-x (+ rule-y 104) fg)
             (format "<text x=\"%d\" y=\"%d\" font-family=\"Helvetica, Geneva, sans-serif\" font-size=\"11\" fill=\"%s\">Press Symbol-</text>"
                     left-x (+ rule-y 118) fg)
             (lispm-desktop-genera--svg-keycap (+ left-x 74) (+ rule-y 118) "HELP" bg fg)
             (format "<text x=\"%d\" y=\"%d\" font-family=\"Helvetica, Geneva, sans-serif\" font-size=\"11\" fill=\"%s\">(or C-c a) to switch or create an AI Agent session.</text>"
                     (+ left-x 110) (+ rule-y 118) fg)
             (format "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"%s\" stroke-width=\"1\"/>"
                     left-x (+ rule-y 124) (+ left-x 450) (+ rule-y 124) fg)))
          ;; LispM configuration tip & Free Software notice block (shown when height permits).
          (when (> pane-b 530)
            (concat
             (format "<text x=\"%d\" y=\"%d\" font-family=\"Menlo, Monaco, monospace\" font-size=\"9.5\" font-weight=\"bold\" fill=\"%s\">Use Control Panels or Configure to customize your workstation:</text>"
                     (+ left-x 6) notice-y fg)
             (format "<text x=\"%d\" y=\"%d\" font-family=\"Menlo, Monaco, monospace\" font-size=\"9.5\" font-weight=\"bold\" fill=\"%s\">Appearance, Language, Security, AI Providers &amp; Sandboxed Tools.</text>"
                     (+ left-x 6) (+ notice-y 13) fg)
             (format "<text x=\"%d\" y=\"%d\" font-family=\"Menlo, Monaco, monospace\" font-size=\"9.5\" font-weight=\"bold\" fill=\"%s\">Right-click or double-click anywhere for the LispM System Menu,</text>"
                     (+ left-x 6) (+ notice-y 26) fg)
             (format "<text x=\"%d\" y=\"%d\" font-family=\"Menlo, Monaco, monospace\" font-size=\"9.5\" font-weight=\"bold\" fill=\"%s\">or press Return to evaluate a command or Lisp form.</text>"
                     (+ left-x 6) (+ notice-y 39) fg)
             (format "<text x=\"%d\" y=\"%d\" font-family=\"Menlo, Monaco, monospace\" font-size=\"10\" fill=\"%s\">Status:  All LispM Desktop subsystems are initialized and ready.</text>"
                     left-x (+ notice-y 56) fg)
             (when (> width (+ right-x 360))
               (concat
                (format "<text x=\"%d\" y=\"%d\" font-family=\"Menlo, Monaco, monospace\" font-size=\"8.5\" fill=\"%s\">LispM Desktop — Free Software under the GNU Affero General Public License</text>"
                        right-x notice-y fg)
                (format "<text x=\"%d\" y=\"%d\" font-family=\"Menlo, Monaco, monospace\" font-size=\"8.5\" fill=\"%s\">(AGPL-3.0-or-later). Built on GNU Emacs and the LispM Kit, LispM UI,</text>"
                        right-x (+ notice-y 11) fg)
                (format "<text x=\"%d\" y=\"%d\" font-family=\"Menlo, Monaco, monospace\" font-size=\"8.5\" fill=\"%s\">LispM Browser, LispM Auth and LispM Agent workstation stack. Use the</text>"
                        right-x (+ notice-y 22) fg)
                (format "<text x=\"%d\" y=\"%d\" font-family=\"Menlo, Monaco, monospace\" font-size=\"8.5\" fill=\"%s\"><tspan font-style=\"italic\">Show System</tspan> command to inspect loaded LispM packages and active agents.</text>"
                        right-x (+ notice-y 33) fg)))))
          ;; Ready. / Command: █
          (format "<text x=\"%d\" y=\"%d\" font-family=\"Menlo, Monaco, monospace\" font-size=\"10\" fill=\"%s\">Ready.</text>"
                  left-x (- prompt-y 15) fg)
          (format "<text x=\"%d\" y=\"%d\" font-family=\"Menlo, Monaco, monospace\" font-size=\"10\" fill=\"%s\">Command: █</text>"
                  left-x prompt-y fg)))
       ;; Bottom 2-line inverse-video black Mouse Documentation Line.
       (format "<rect x=\"0\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"%s\"/>"
               doc-y width doc-h fg)
       (format "<text x=\"10\" y=\"%d\" font-family=\"Menlo, Monaco, monospace\" font-size=\"11\" font-weight=\"bold\" fill=\"%s\">Mouse-L: Command Processor;  Mouse-R: LispM System Menu (Select / Create / Bury / Kill);  [Bury] / [Kill]: Close Pane.</text>"
               (+ doc-y 14) bg)
       (format "<text x=\"10\" y=\"%d\" font-family=\"Menlo, Monaco, monospace\" font-size=\"11\" font-weight=\"bold\" fill=\"%s\">Press Return to run a command, C-c T for Focus Mode, or C-c ? for the LispM Guide.</text>"
               (+ doc-y 30) bg)))))

(defun lispm-desktop-genera--dedup-items (items)
  "Return menu ITEMS with duplicate `:label' entries and extra separators removed."
  (let ((seen nil)
        (out nil))
    (dolist (item items)
      (cond ((eq item 'separator)
             (when (and out (not (eq (car out) 'separator)))
               (push 'separator out)))
            ((consp item)
             (let ((label (plist-get item :label)))
               (unless (and (stringp label) (member label seen))
                 (when (stringp label)
                   (push label seen))
                 (push item out))))))
    (when (eq (car out) 'separator)
      (pop out))
    (nreverse out)))

(defun lispm-desktop-genera-open-window-items ()
  "Return menu items for live floating windows (visible or buried) on screen."
  (let* ((screen (lispm-desktop-apps-screen))
         (frames (seq-uniq (append (lispm-desktop-apps-windows-in-order screen)
                                   (lispm-desktop-apps-windows screen)))))
    (seq-keep
     (lambda (frame)
       (when (frame-live-p frame)
         (list :label (or (frame-parameter frame 'name)
                          (when-let* ((app (frame-parameter frame 'lispm-desktop-app)))
                            (lispm-desktop-apps-name app))
                          "Window")
               :checked (and (frame-visible-p frame) (eq frame (selected-frame)))
               :action (lambda ()
                         (lispm-desktop-platinum-show-window frame)
                         (lispm-desktop-platinum-raise-window frame)
                         (select-frame-set-input-focus frame)
                         (when-let* ((app (frame-parameter frame 'lispm-desktop-app)))
                           (lispm-desktop-apps-set-frontmost app))))))
     frames)))

(defun lispm-desktop-genera-select-items ()
  "Return `Select' menu items derived from LispM's declarative UI registries:
open windows, running/available applications and modes
\(`lispm-desktop-apps-application-items'), and desktop entrypoints
\(`lispm-desktop-entry-items')."
  (let ((windows (lispm-desktop-genera-open-window-items))
        (apps (and (fboundp 'lispm-desktop-apps-application-items)
                   (lispm-desktop-apps-application-items)))
        (entries (and (fboundp 'lispm-desktop-entry-items)
                      (lispm-desktop-entry-items))))
    (lispm-desktop-genera--dedup-items
     (append windows
             (when (and windows (or apps entries)) '(separator))
             apps
             (when (and apps entries) '(separator))
             entries))))

(defun lispm-desktop-genera-create-window (action &optional sweep)
  "Create or open a window by running ACTION, placing it at SWEEP.
When SWEEP is t, let the user sweep or click its place on screen first
via `lispm-desktop-platinum-sweep-outline'; when SWEEP is a geometry list
\(LEFT TOP WIDTH HEIGHT), apply it directly."
  (let ((place (if (eq sweep t)
                   (lispm-desktop-platinum-sweep-outline (lispm-desktop-apps-screen))
                 sweep)))
    (unless (eq place 'cancel)
      (when (and (frame-live-p (selected-frame))
                 (frame-parameter (selected-frame) 'lispm-desktop-app-window))
        (let ((screen (lispm-desktop-apps-screen)))
          (when (frame-live-p screen)
            (select-frame screen 'norecord))))
      (if (commandp action)
          (call-interactively action)
        (when (functionp action)
          (funcall action)))
      (when (consp place)
        (let* ((screen (lispm-desktop-apps-screen))
               (frame (or (and (frame-live-p (selected-frame))
                               (frame-parameter (selected-frame) 'lispm-desktop-window)
                               (selected-frame))
                          (car (lispm-desktop-apps-windows-in-order screen)))))
          (when frame
            (lispm-desktop-apps-apply-window-geometry frame place)))))))

(defun lispm-desktop-genera--wrap-create-items (items &optional sweep)
  "Wrap menu ITEMS so every `:action' creates/places its window with SWEEP."
  (mapcar
   (lambda (item)
     (if (not (consp item))
         item
       (let ((copy (copy-sequence item))
             (action (plist-get item :action))
             (submenu (plist-get item :submenu)))
         (when action
           (plist-put copy :action
                      (lambda ()
                        (lispm-desktop-genera-create-window action sweep))))
         (when submenu
           (plist-put copy :submenu
                      (lambda ()
                        (lispm-desktop-genera--wrap-create-items
                         (if (functionp submenu) (funcall submenu) submenu)
                         sweep))))
         copy)))
   items))

(defun lispm-desktop-genera-create-items (&optional sweep)
  "Return `Create' menu items derived from LispM's declarative UI registries:
desktop entrypoints (`lispm-desktop-entry-items') and launcher items
\(`lispm-desktop-lambda-items'), wrapped with
`lispm-desktop-genera-create-window'."
  (let ((entries (and (fboundp 'lispm-desktop-entry-items)
                      (lispm-desktop-entry-items)))
        (lambda-items (and (fboundp 'lispm-desktop-lambda-items)
                           (lispm-desktop-lambda-items))))
    (lispm-desktop-genera--wrap-create-items
     (lispm-desktop-genera--dedup-items
      (append entries
              (when (and entries lambda-items) '(separator))
              lambda-items))
     sweep)))

(defun lispm-desktop-genera-app-menu-items ()
  "Return submenus for the frontmost application's declarative `:menus'."
  (when (fboundp 'lispm-desktop-apps-menus)
    (let ((app (and (fboundp 'lispm-desktop-apps-frontmost)
                    (lispm-desktop-apps-frontmost))))
      (lispm-desktop-genera--active-items
       (seq-keep
        (pcase-lambda (`(,key ,title ,items-fn))
          (unless (and (eq app 'desktop)
                       (memq key '(lispm-desktop-menu-view
                                   lispm-desktop-menu-special)))
            (list :label title :submenu items-fn)))
        (lispm-desktop-apps-menus))))))

(defun lispm-desktop-genera-system-items (&optional window)
  "Return LispM's Genera System Menu items for WINDOW, derived declaratively
from `Select', `Create', the frontmost application's menus, window operations,
Command Processor and system power commands."
  (let* ((window (if (window-live-p window) window (selected-window)))
         (frame (window-frame window))
         (floating (and (frame-live-p frame)
                        (frame-parameter frame 'lispm-desktop-window)))
         (closable (and (not (equal (buffer-name (window-buffer window))
                                    lispm-desktop-buffer-name))
                        (or floating (window-combined-p window))))
         (select-items (unless closable (lispm-desktop-genera-select-items)))
         (app-menus (lispm-desktop-genera-app-menu-items))
         (special (and (fboundp 'lispm-desktop--special-items)
                       (lispm-desktop--special-items))))
    (lispm-desktop-genera--active-items
     (append
      (list (list :label "Select"
                  :submenu #'lispm-desktop-genera-select-items)
            (list :label "Create"
                  :submenu (lambda () (lispm-desktop-genera-create-items t)))
            (list :label "Window Operations"
                  :submenu (lambda () (lispm-desktop-genera-window-operation-items window))))
      (when closable
        (list 'separator
              (list :label "Bury"
                    :action (lambda () (lispm-desktop-genera-bury-window window)))
              (list :label "Kill"
                    :action (lambda () (lispm-desktop-genera-kill-window window)))))
      (when app-menus
        (cons 'separator app-menus))
      (when (and select-items (not closable))
        (cons 'separator select-items))
      (list 'separator
            (list :label "Command Processor (Command:)…"
                  :action #'lispm-desktop-genera-command)
            (list :label "Refresh"
                  :action #'redraw-display))
      (when special
        (cons 'separator special))))))

(defun lispm-desktop-genera--flatten-menu-items (items &optional prefix)
  "Flatten menu ITEMS into ((COMMAND-NAME . ACTION) ...) with optional PREFIX."
  (let ((acc nil))
    (dolist (item items)
      (when (consp item)
        (let* ((label (plist-get item :label))
               (name (if (and prefix label) (concat prefix label) label))
               (action (plist-get item :action))
               (submenu (plist-get item :submenu)))
          (when (and (stringp name) action)
            (push (cons name action) acc))
          (when (and (stringp label) submenu)
            (let ((sub (ignore-errors
                         (if (functionp submenu) (funcall submenu) submenu))))
              (dolist (entry (lispm-desktop-genera--flatten-menu-items
                              sub (concat (or prefix "") label ": ")))
                (push entry acc)))))))
    (nreverse acc)))

(defun lispm-desktop-genera--command-entries ()
  "Return ((NAME . ACTION) ...) for `lispm-desktop-genera-command', derived
from the declarative `Select', `Create', application and special menus."
  (let ((select (lispm-desktop-genera--flatten-menu-items
                 (lispm-desktop-genera-select-items) "Select "))
        (direct (lispm-desktop-genera--flatten-menu-items
                 (lispm-desktop-genera-select-items)))
        (create (lispm-desktop-genera--flatten-menu-items
                 (lispm-desktop-genera-create-items nil) "Create "))
        (special (and (fboundp 'lispm-desktop--special-items)
                      (lispm-desktop-genera--flatten-menu-items
                       (lispm-desktop--special-items)))))
    (append
     (list (cons "Help Commands"
                 (lambda ()
                   (message "Commands: %s, or any Lisp form."
                            (string-join
                             (mapcar #'car (lispm-desktop-genera--flatten-menu-items
                                            (lispm-desktop-genera-select-items)
                                            "Select "))
                             ", "))))
           (cons "Show System"
                 (lambda ()
                   (message "LispM Desktop on %s — GNU Emacs %s (%s)."
                            (or (system-name) "LISPM-HOST")
                            emacs-version system-configuration)))
           (cons "Refresh Screen" #'redraw-display))
     select create special direct)))

(defun lispm-desktop-genera-command (input)
  "Execute a LispM Command Processor INPUT from `Dynamic Lisp Listener 1'."
  (interactive
   (let ((entries (lispm-desktop-genera--command-entries)))
     (list (completing-read "Command: " (mapcar #'car entries) nil nil))))
  (let* ((cmd (string-trim input))
         (entries (lispm-desktop-genera--command-entries))
         (match (seq-find (lambda (entry)
                            (string-equal-ignore-case cmd (car entry)))
                          entries)))
    (cond
     ((string-empty-p cmd)
      (funcall (cdr (assoc "Help Commands" entries))))
     (match
      (let ((action (cdr match)))
        (if (commandp action)
            (call-interactively action)
          (funcall action))))
     (t
      (condition-case err
          (message "=> %S" (eval (car (read-from-string cmd)) t))
        (error (message "Command Processor error: %s"
                        (error-message-string err))))))))

(defun lispm-desktop-genera-system-menu (&optional x y window)
  "Pop up LispM's Genera System Menu at X, Y on the root screen for WINDOW."
  (interactive)
  (let* ((window (if (window-live-p window) window (selected-window)))
         (frame (window-frame window))
         (lispm-ui-menu-parent
          (or (lispm-desktop-platinum-window-parent frame) frame)))
    (lispm-ui-menu (lispm-desktop-genera-system-items window) x y)))

(cl-defmethod lispm-desktop-popup-augment-items (items &context (lispm-desktop-look (lispm-desktop-genera-look genera)) &optional window)
  "Include LispM's Genera System Menu items in right-click menus of any pane."
  (let* ((window (if (window-live-p window) window (selected-window)))
         (active (lispm-desktop-genera--active-items items))
         (system (lispm-desktop-genera-system-items window)))
    (lispm-desktop-genera--active-items
     (if active
         (append active (cons 'separator system))
       system))))

(defun lispm-desktop-genera-mouse-system-menu (event)
  "Pop up LispM's System Menu where EVENT clicked."
  (interactive "e")
  (let* ((posn (event-start event))
         (window (or (and posn (windowp (posn-window posn)) (posn-window posn))
                     (selected-window)))
         (frame (window-frame window))
         (xy (lispm-desktop-popup-frame-xy event)))
    (pcase-let ((`(,sx . ,sy)
                 (lispm-desktop-genera--frame-screen-xy frame (car xy) (cdr xy))))
      (lispm-desktop-genera-system-menu sx sy window))))

(defun lispm-desktop-genera-mouse-bury-window (event)
  "Bury the window whose `[Bury]' placard EVENT clicked."
  (interactive "e")
  (let* ((posn (event-start event))
         (window (or (and posn (windowp (posn-window posn)) (posn-window posn))
                     (selected-window))))
    (lispm-desktop-genera-bury-window window)))

(defun lispm-desktop-genera-mouse-kill-window (event)
  "Kill the window whose `[Kill]' placard EVENT clicked."
  (interactive "e")
  (let* ((posn (event-start event))
         (window (or (and posn (windowp (posn-window posn)) (posn-window posn))
                     (selected-window))))
    (lispm-desktop-genera-kill-window window)))

(defvar-keymap lispm-desktop-genera-who-line-map
  :doc "Keymap of Genera's Who-Line and Select placard."
  "<mode-line> <down-mouse-1>" #'lispm-desktop-genera-mouse-system-menu
  "<mode-line> <mouse-1>" #'ignore
  "<mode-line> <down-mouse-3>" #'lispm-desktop-genera-mouse-system-menu
  "<mode-line> <mouse-3>" #'ignore)

(defvar-keymap lispm-desktop-genera-bury-placard-map
  :doc "Keymap of Genera's `[Bury]' placard."
  "<mode-line> <down-mouse-1>" #'ignore
  "<mode-line> <mouse-1>" #'lispm-desktop-genera-mouse-bury-window
  "<mode-line> <down-mouse-3>" #'lispm-desktop-genera-mouse-window-menu
  "<mode-line> <mouse-3>" #'ignore)

(defvar-keymap lispm-desktop-genera-kill-placard-map
  :doc "Keymap of Genera's `[Kill]' placard."
  "<mode-line> <down-mouse-1>" #'ignore
  "<mode-line> <mouse-1>" #'lispm-desktop-genera-mouse-kill-window
  "<mode-line> <down-mouse-3>" #'lispm-desktop-genera-mouse-window-menu
  "<mode-line> <mouse-3>" #'ignore)

(defun lispm-desktop-genera-placards (active)
  "Return Genera's `[Select]', `[Bury]' and `[Kill]' placards at the left
of a window's bottom edge."
  (list (lispm-desktop-platinum-placard
         "[Select]"
         :active active :menu t :keymap lispm-desktop-genera-who-line-map
         :help "Click for LispM System Menu (Select / Create / Window Operations)")
        (lispm-desktop-platinum-placard
         "[Bury]"
         :active active :keymap lispm-desktop-genera-bury-placard-map
         :help "Click to bury this pane behind Dynamic Lisp Listener 1")
        (lispm-desktop-platinum-placard
         "[Kill]"
         :active active :keymap lispm-desktop-genera-kill-placard-map
         :help "Click to close (Kill) this pane")))

(defface lispm-desktop-genera-who-line
  '((t :background "#FFFFFF" :foreground "#000000" :overline "#000000"))
  "Face of LispM's white-ground Who-Line at screen bottom in the `genera' look."
  :group 'lispm-desktop)

(defun lispm-desktop-genera-who-line ()
  "Return LispM's bottom Who-Line in the `genera' look."
  (let* ((user (or (and (fboundp 'user-login-name) (user-login-name)) "USER"))
         (host (or (and (fboundp 'system-name) (system-name)) "LISPM-HOST"))
         (left (format " [%s]          %s:        User Input              [Select]"
                       (format-time-string "%a %e %b %H:%M:%S") user))
         (right (format "%s is ready (LispM Desktop)  " host)))
    (propertize
     (concat left
             (propertize " " 'display
                         `(space :align-to (- right ,(1+ (length right)))))
             right
             (propertize " " 'display '(space :align-to right)))
     'face 'lispm-desktop-genera-who-line
     'keymap lispm-desktop-genera-who-line-map
     'help-echo "Click for LispM System Menu (Select Activity)")))

(defun lispm-desktop-genera-sync-who-line ()
  "Sync the root screen's `mode-line-format' with `lispm-desktop-mode-line-format'."
  (when-let* ((buffer (get-buffer lispm-desktop-buffer-name)))
    (with-current-buffer buffer
      (setq-local mode-line-format (lispm-desktop-mode-line-format))
      (force-mode-line-update t))))

(add-hook 'lispm-desktop-look-changed-hook #'lispm-desktop-genera-sync-who-line)
(add-hook 'lispm-desktop-mode-hook #'lispm-desktop-genera-sync-who-line)

(cl-defmethod lispm-desktop-platinum-apply-look-faces (palette &context (lispm-desktop-look (lispm-desktop-genera-look genera)))
  "Set the faces the `genera' look draws in PALETTE: 1px black-outlined
buttons, paper-white list bodies and placards, crisp 1px dividers, and
the white-ground Who-Line."
  (let ((bg (plist-get palette :content))
        (fg (plist-get palette :outline)))
    (set-face-attribute 'lispm-desktop-platinum-list-body nil :background bg)
    (set-face-attribute 'lispm-desktop-platinum-sorted nil :background bg)
    (set-face-attribute 'lispm-desktop-platinum-row nil :overline 'unspecified)
    (set-face-attribute 'lispm-ui-button nil
                        :background bg :foreground fg
                        :box `(:line-width (1 . 1) :color ,fg))
    (dolist (face '(lispm-desktop-platinum-placard lispm-desktop-platinum-placard-inactive))
      (set-face-attribute face nil
                          :background bg :foreground fg
                          :box `(:line-width (1 . 1) :color ,fg)))
    (set-face-attribute 'lispm-desktop-platinum-header nil
                        :background bg :foreground fg
                        :box `(:line-width (1 . 1) :color ,fg))
    (set-face-attribute 'tooltip nil :background bg :foreground fg)
    (set-face-attribute 'child-frame-border nil :background fg)
    (set-face-attribute 'window-divider nil :foreground fg)
    (set-face-attribute 'window-divider-first-pixel nil :foreground fg)
    (set-face-attribute 'window-divider-last-pixel nil :foreground fg)
    (set-face-attribute 'lispm-desktop-genera-who-line nil
                        :background bg :foreground fg
                        :overline fg
                        :family (face-attribute 'lispm-ui-list :family nil t)
                        :height 110 :weight 'normal))
  (lispm-desktop-genera-sync-who-line))

(provide 'lispm-desktop-genera)
;;; lispm-desktop-genera.el ends here
