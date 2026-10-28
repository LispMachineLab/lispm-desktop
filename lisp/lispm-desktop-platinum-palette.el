;;; lispm-desktop-platinum-palette.el --- The chrome's colours, from the theme  -*- lexical-binding: t; -*-

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

;; The colours every part of the chrome is drawn in: a palette derived from
;; the theme's faces, or named outright by it, and the faces of title bars,
;; menus, lists and placards.  Drawn images are memoized by the palette's
;; generation, so a theme enabled draws everything anew.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-look)



(defgroup lispm-desktop-platinum nil
  "Mac OS 9's layout and forms for lispm-desktop."
  :group 'lispm-desktop
  :prefix "lispm-desktop-platinum-")

(defcustom lispm-desktop-platinum-colors nil
  "Chrome colours a theme names outright, a plist.
Keys are those of `lispm-ui-derive-palette' (:chrome, :header,
:light, :shade, :frame, :muted, :accent, :outline, :highlight, :thumb,
:focus, :strip, :title, :border, :menu, ...); a colour left out is
derived from the theme's faces.  Themes set it, as the platinum theme
does; users rarely need to."
  :type '(plist :value-type color))

;;;; Palette

(defun lispm-ui-rgb (color)
  "Return COLOR as a list of red, green and blue between 0 and 1, or nil.
A colour written #RRGGBB is read as written, whatever the display can
show; any other name is resolved by the display."
  (if (and (stringp color)
           (string-match (concat "\\`#\\([[:xdigit:]]\\{2\\}\\)"
                                 "\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{2\\}\\)\\'")
                         color))
      (mapcar (lambda (group) (/ (string-to-number (match-string group color) 16) 255.0))
              '(1 2 3))
    (and color (color-name-to-rgb color))))

(defun lispm-ui-blend (from to ratio)
  "Return the colour RATIO of the way from colour FROM to colour TO.
Each component is rounded to the nearest of its 256 values, so a blend
falling on one of them comes back exactly.  Return nil when either
colour cannot be resolved, as on a text terminal."
  (when-let* ((a (lispm-ui-rgb from))
              (b (lispm-ui-rgb to)))
    (apply #'format "#%02x%02x%02x"
           (cl-mapcar (lambda (x y) (round (* 255 (+ x (* ratio (- y x)))))) a b))))

(defvar lispm-ui-current-palette nil
  "The chrome's colours, a plist derived from the theme.
See `lispm-ui-derive-palette'.")

(defvar lispm-ui-palette-generation 0
  "Count of palettes derived so far; memoized drawings carry it in their key.")

(defun lispm-ui-derive-palette (content text dim accent)
  "Return the chrome's palette for a theme drawing CONTENT, TEXT, DIM and ACCENT.
CONTENT and TEXT are the editor's background and foreground, DIM its
dimmed text, ACCENT its highlight colour.  The chrome is the content a
shade darker; a bevel's lit edge leans toward the text, its shaded edge
toward black, and frames are darker still; controls are outlined in
near black.  The desktop behind the windows is the content tinted with
the accent.

As Mac OS 9's variation colour was a ramp, so is the accent: menus are
highlighted in it darkened, scroll thumbs are it lightened, and the
keyboard focus is ringed in it.  The Control Strip has greys of its own,
a little darker than the chrome; alerts are framed in a light red.

Other looks than Platinum draw a few more parts: active captions
\(:title, :title-text) in the highlight, inactive ones (:title-inactive,
:title-inactive-text) in the chrome, sizing borders (:border,
:border-inactive) in the chrome, menus (:menu, :menu-text) on the
document's ground."
  (let* ((chrome (lispm-ui-blend content "#000000" 0.25))
         (light (lispm-ui-blend chrome text 0.3))
         (outline (lispm-ui-blend content "#000000" 0.85))
         (highlight (lispm-ui-blend accent "#000000" 0.4))
         (palette (list :content content :text text :dim dim :accent accent
                        :chrome chrome :light light :outline outline)))
    (append
     palette
     (list :desktop (lispm-ui-blend content accent 0.25)
           :header (lispm-ui-blend chrome content 0.5)
           :shade (lispm-ui-blend chrome "#000000" 0.6)
           :frame (lispm-ui-blend content "#000000" 0.65)
           :muted (lispm-ui-blend dim text 0.35)
           :highlight highlight
           :highlight-dark (lispm-ui-blend highlight "#000000" 0.45)
           :highlight-text (lispm-ui-legible highlight text content)
           :thumb (lispm-ui-blend accent "#FFFFFF" 0.35)
           :thumb-light (lispm-ui-blend accent "#FFFFFF" 0.65)
           :thumb-dark accent
           :focus accent
           :alert (lispm-ui-blend light "#FF0000" 0.4)
           :alert-dark (lispm-ui-blend light "#FF0000" 0.6)
           :strip (lispm-ui-grey palette #xC0)
           :strip-light (lispm-ui-grey palette #xFF)
           :strip-shade (lispm-ui-grey palette #xA0)
           :strip-dark (lispm-ui-grey palette #x80)
           ;; What other looks draw: captions in the highlight, an
           ;; inactive one in the chrome; sizing borders in the chrome;
           ;; menus on the document's own ground.
           :title highlight
           :title-text (lispm-ui-legible highlight text content)
           :title-inactive chrome
           :title-inactive-text (lispm-ui-blend dim text 0.35)
           :border chrome
           :border-inactive chrome
           :menu content
           :menu-text text))))

(defun lispm-ui-grey (palette level)
  "Return the colour of PALETTE standing where Platinum's grey LEVEL stands.
LEVEL is one of Mac OS 9's greys as a byte, #xDD its chrome, #xFF its
lit edges and #x00 its outlines: the greys between the chrome and white
are taken between PALETTE's :chrome and :light, those below the chrome
between its :chrome and :outline.  With Platinum's own palette every
level comes back as itself, so a control is drawn with the greys Mac OS 9
drew it with, and a theme of other colours gets their counterparts."
  (let ((chrome #xDD))
    (if (>= level chrome)
        (lispm-ui-blend (plist-get palette :chrome) (plist-get palette :light)
                        (/ (- level chrome) (float (- #xFF chrome))))
      (lispm-ui-blend (plist-get palette :chrome) (plist-get palette :outline)
                      (/ (- chrome level) (float chrome))))))

(defun lispm-ui-luminance (color)
  "Return the relative luminance of COLOR, between 0 and 1."
  (pcase-let ((`(,r ,g ,b) (or (lispm-ui-rgb color) '(0 0 0))))
    (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))

(defun lispm-ui-legible (background &rest colors)
  "Return whichever of COLORS stands out most from BACKGROUND."
  (let ((ground (lispm-ui-luminance background)))
    (car (seq-sort-by (lambda (color) (abs (- (lispm-ui-luminance color) ground)))
                      #'> colors))))

(defun lispm-ui-merge-palette (derived named)
  "Return the palette DERIVED with the colours NAMED put in their place."
  (let ((palette (copy-sequence derived)))
    (cl-loop for (key value) on named by #'cddr
             do (setq palette (plist-put palette key value)))
    palette))

(defun lispm-ui-color (name)
  "Return the palette colour NAME."
  (plist-get lispm-ui-current-palette name))

;;;; Faces


(defface lispm-ui-title '((t :weight bold))
  "Face of the active window's title."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-title-inactive '((t :weight bold :inherit shadow))
  "Face of an inactive window's title."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-menu-title '((t :weight bold))
  "Face of a menu bar title."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-menu-title-active '((t :weight bold :inverse-video t))
  "Face of a highlighted menu bar title, as of an open menu."
  :group 'lispm-desktop-platinum)

(defface lispm-ui-list nil
  "Face of list views: the sidebar's rows."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-header nil
  "Face of a list window's header, below its title bar."
  :group 'lispm-desktop-platinum)

(defface lispm-ui-small nil
  "Face of the small system font, as Mac OS 9's Geneva 9: window headers,
the Control Strip's menus."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-list-body nil
  "Face of a list view's body: lighter than the platinum, as Finder's."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-row nil
  "Face of a list's row: a lit line atop it parts it from the row above."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-sorted nil
  "Face of the column a list is sorted by: shaded down its length, as in
Finder's list view."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-placard '((t :box (:line-width (1 . 1))))
  "Face of a placard on the active window's bottom edge."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-placard-inactive '((t :inherit shadow))
  "Face of a placard on an inactive window's bottom edge."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-toolbar nil
  "Face of a toolbar: the browser's, around its address field."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-toolbar-label nil
  "Face of a label in a toolbar, such as the address field's."
  :group 'lispm-desktop-platinum)

(defface lispm-ui-field '((t :box (:line-width (1 . 1))))
  "Face of an edit text field: framed in black, in the list colour."
  :group 'lispm-desktop-platinum)

(defface lispm-ui-field-focused '((t :inherit lispm-ui-field))
  "Face of an edit text field with the keyboard: ringed in the focus colour."
  :group 'lispm-desktop-platinum)

(defface lispm-ui-button '((t :box (:line-width (1 . 1) :style released-button)))
  "Face of a bevel button."
  :group 'lispm-desktop-platinum)

(defface lispm-ui-button-pressed '((t :box (:line-width (1 . 1) :style pressed-button)))
  "Face of a bevel button pushed in: a toggle that is on."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-column '((t :box (:line-width (1 . 1) :style released-button)))
  "Face of a column heading."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-column-sorted '((t :box (:line-width (1 . 1) :style released-button)))
  "Face of the heading of the column a list is sorted by."
  :group 'lispm-desktop-platinum)

(provide 'lispm-desktop-platinum-palette)
;;; lispm-desktop-platinum-palette.el ends here
