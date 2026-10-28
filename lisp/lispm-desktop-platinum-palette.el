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

;; The colours every part of the chrome is drawn in are LispM UI's palette
;; (lispm-ui-palette.el), derived from the theme's faces, with the colours
;; the theme names outright in `lispm-desktop-platinum-colors'; drawn
;; images are memoized by the palette's generation, so a theme enabled
;; draws everything anew.  Here too are the faces of title bars, menus,
;; lists and placards, beside LispM UI's own.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-look)
(require 'lispm-ui-palette)

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

(defun lispm-desktop-platinum-named-colors ()
  "Return the colours the theme names outright, see
`lispm-desktop-platinum-colors': one of `lispm-ui-palette-functions'."
  lispm-desktop-platinum-colors)

(add-hook 'lispm-ui-palette-functions #'lispm-desktop-platinum-named-colors)

;;;; Faces

(defface lispm-desktop-platinum-title-inactive '((t :weight bold :inherit shadow))
  "Face of an inactive window's title."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-menu-title '((t :weight bold))
  "Face of a menu bar title."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-menu-title-active '((t :weight bold :inverse-video t))
  "Face of a highlighted menu bar title, as of an open menu."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-header nil
  "Face of a list window's header, below its title bar."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-list-body nil
  "Face of a list view's body: lighter than the platinum, as Finder's."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-row nil
  "Face of a list's row: a lit line atop it parts it from the row above."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-row-current '((t :inherit secondary-selection :extend t))
  "Face of a list's row naming what is being worked on elsewhere, as the
agent sidebar's row of the session at work: the selection's colour faded
into the list, as a selection in a list without the keyboard."
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

(defface lispm-desktop-platinum-column '((t :box (:line-width (1 . 1) :style released-button)))
  "Face of a column heading."
  :group 'lispm-desktop-platinum)

(defface lispm-desktop-platinum-column-sorted '((t :box (:line-width (1 . 1) :style released-button)))
  "Face of the heading of the column a list is sorted by."
  :group 'lispm-desktop-platinum)

(provide 'lispm-desktop-platinum-palette)
;;; lispm-desktop-platinum-palette.el ends here
