;;; lispm-desktop-ui-compat.el --- The desktop's former names of what LispM UI holds  -*- lexical-binding: t; -*-

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

;; What moved from the desktop to LispM UI is called by LispM UI's names
;; now, throughout the desktop (scripts/lispm-ui-renames.eld).  For what
;; was written against the desktop's names, a theme's, an application's
;; or a person's own personal.el, the names it was known by stay aliases
;; of LispM UI's: its variables, its faces, and the functions of the
;; looks it documented.  The renames leave this file as it is.
;;
;; Generic functions are never aliased: a method defined on an alias
;; would make a generic function of its own.  A look's methods are
;; defined on LispM UI's names.

;;; Code:

(require 'lispm-ui-palette)
(require 'lispm-ui-look)
(require 'lispm-ui-draw)
(require 'lispm-ui-form)

;;;; Variables

(defvaralias 'lispm-desktop-platinum-current-palette 'lispm-ui-current-palette)
(defvaralias 'lispm-desktop-platinum-generation 'lispm-ui-palette-generation)
(defvaralias 'lispm-desktop-looks 'lispm-ui-looks)
(defvaralias 'lispm-desktop-form-control-properties 'lispm-ui-form-control-properties)
;; The title a buffer's window shows, which had the name of the face of
;; titles.
(defvaralias 'lispm-desktop-platinum-title 'lispm-desktop-platinum-window-title)

;;;; Faces

(pcase-dolist (`(,old . ,new) '((lispm-desktop-platinum-title . lispm-ui-title)
                                (lispm-desktop-platinum-list . lispm-ui-list)
                                (lispm-desktop-platinum-small . lispm-ui-small)
                                (lispm-desktop-platinum-field . lispm-ui-field)
                                (lispm-desktop-platinum-field-focused . lispm-ui-field-focused)
                                (lispm-desktop-platinum-button . lispm-ui-button)
                                (lispm-desktop-platinum-button-pressed . lispm-ui-button-pressed)
                                (lispm-desktop-form-text-area . lispm-ui-form-text-area)))
  (put old 'face-alias new))

;;;; Looks

(defalias 'lispm-desktop-look-define #'lispm-ui-look-define)
(defalias 'lispm-desktop-look-get #'lispm-ui-look-get)
(defalias 'lispm-desktop-look-title #'lispm-ui-look-title)
(defalias 'lispm-desktop-look-family #'lispm-ui-look-family)
(defalias 'lispm-desktop-look-metric #'lispm-ui-look-metric)
(defalias 'lispm-desktop-look-families #'lispm-ui-look-families)
(defalias 'lispm-desktop-look-image #'lispm-ui-look-image)
(defalias 'lispm-desktop-look-svg-fill #'lispm-ui-svg-paint)
(defalias 'lispm-desktop-look-svg-edges #'lispm-ui-svg-edges)
(defalias 'lispm-desktop-look-svg-outline #'lispm-ui-svg-outline)
(defalias 'lispm-desktop-look-svg-arrow #'lispm-ui-svg-arrow)
(defalias 'lispm-desktop-look-svg-dotted #'lispm-ui-svg-dotted)
(defalias 'lispm-desktop-look-svg-pixels #'lispm-ui-svg-pixels)

(provide 'lispm-desktop-ui-compat)
;;; lispm-desktop-ui-compat.el ends here
