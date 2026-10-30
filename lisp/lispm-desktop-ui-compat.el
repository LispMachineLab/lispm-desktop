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
;; of LispM UI's: its variables, its faces, and the functions it
;; documented, of the looks and of controls in buffers.  The renames
;; leave this file as it is.
;;
;; Generic functions are never aliased: a method defined on an alias
;; would make a generic function of its own.  A look's methods are
;; defined on LispM UI's names.

;;; Code:

(require 'lispm-ui-palette)
(require 'lispm-ui-look)
(require 'lispm-ui-draw)
(require 'lispm-ui-controls)
(require 'lispm-ui-form)
(require 'lispm-ui-icons)
(require 'lispm-ui-menu)
(require 'lispm-ui-ask)
(require 'lispm-ui-alert)
(require 'lispm-ui-list)
(require 'lispm-ui-scroll)
(require 'lispm-ui-window)
(require 'lispm-ui-file-dialog)

(defvar savehist-file)

(defun lispm-desktop-ui-compat--restore-recent ()
  "Recover both Recent lists from the first savehist restoration.
An old file can assign both names, which are now aliases.  Read only
their quoted lists of file names; never evaluate the history file."
  (when (bound-and-true-p savehist-mode)
    (remove-hook 'savehist-mode-hook #'lispm-desktop-ui-compat--restore-recent)
    (let (saved)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents savehist-file)
            (let ((read-circle nil))
              (while t
                (pcase (read (current-buffer))
                  (`(setq ,name ',files)
                   (when (and (memq name '(lispm-desktop-choose-recent lispm-ui-choose-recent))
                              (proper-list-p files) (seq-every-p #'stringp files))
                     (setf (alist-get name saved) files)))))))
        (error nil))
      (when (assq 'lispm-desktop-choose-recent saved)
        ;; UI's choices come first regardless of the assignments' order.
        (setq lispm-ui-choose-recent
              (delete-dups (append (alist-get 'lispm-ui-choose-recent saved)
                                   lispm-ui-choose-recent
                                   (alist-get 'lispm-desktop-choose-recent saved) nil)))))))

;;;; Variables

(defvaralias 'lispm-desktop-platinum-current-palette 'lispm-ui-current-palette)
(defvaralias 'lispm-desktop-platinum-generation 'lispm-ui-palette-generation)
(defvaralias 'lispm-desktop-looks 'lispm-ui-looks)
(defvaralias 'lispm-desktop-form-control-properties 'lispm-ui-form-control-properties)
;; The toolkit's controls.
(defvaralias 'lispm-desktop-platinum-button-height 'lispm-ui-button-height)
(defvaralias 'lispm-desktop-platinum-bevel-padding 'lispm-ui-bevel-padding)
(defvaralias 'lispm-desktop-platinum-group-title-height 'lispm-ui-group-title-height)
(defvaralias 'lispm-desktop-platinum-push-button-map 'lispm-ui-push-button-map)
(defvaralias 'lispm-desktop-platinum-focus 'lispm-ui-focus)
;; The title a buffer's window shows, which had the name of the face of
;; titles.
(defvaralias 'lispm-desktop-platinum-title 'lispm-desktop-platinum-window-title)
;; The per-window icon is desktop state, separate from the UI icon function.
(defvaralias 'lispm-desktop-platinum-icon 'lispm-desktop-platinum-window-icon)

;; Icons, raster pictures, menus and alerts.
(defvaralias 'lispm-desktop-platinum--icons 'lispm-ui--icons)
(defvaralias 'lispm-desktop-platinum--icon-colors 'lispm-ui--icon-colors)
(defvaralias 'lispm-desktop-platinum--icon-pictures 'lispm-ui--icon-pictures)
(defvaralias 'lispm-desktop-platinum--rasters 'lispm-ui-raster--cache)
(defvaralias 'lispm-desktop-platinum-raster-samples 'lispm-ui-raster-samples)
(defvaralias 'lispm-desktop-platinum-text-cell 'lispm-ui-text-cell)
(defvaralias 'lispm-desktop-platinum-lisp-extensions 'lispm-ui-lisp-extensions)
(defvaralias 'lispm-desktop-platinum-code-extensions 'lispm-ui-code-extensions)
(defvaralias 'lispm-desktop-popup-small 'lispm-ui-menu-small)
(defvaralias 'lispm-desktop-popup-parent 'lispm-ui-menu-parent)
(defvaralias 'lispm-desktop-popup-menu-bar-step 'lispm-ui-menu-bar-step)
(defvaralias 'lispm-desktop-popup-item-height 'lispm-ui-menu-item-height)
(defvaralias 'lispm-desktop-popup-separator-height 'lispm-ui-menu-separator-height)
(defvaralias 'lispm-desktop-popup-label-x 'lispm-ui-menu-label-x)
(defvaralias 'lispm-desktop-popup-key-margin 'lispm-ui-menu-key-margin)
(defvaralias 'lispm-desktop-popup-icon-size 'lispm-ui-menu-icon-size)
(defvaralias 'lispm-desktop-popup-typed-keys 'lispm-ui-menu-typed-keys)
(defvaralias 'lispm-desktop-popup-alert-width 'lispm-ui-alert-width)
(defvaralias 'lispm-desktop-popup-alert-text-x 'lispm-ui-alert-text-x)
(defvaralias 'lispm-desktop-popup-button-gap 'lispm-ui-alert-button-gap)
(defvaralias 'lispm-desktop-popup-text-alert-width 'lispm-ui-text-alert-width)

;; Lists, scroll bars, title drawing and the file dialog.
(defvaralias 'lispm-desktop-platinum-list-header-height 'lispm-ui-list-header-height)
(defvaralias 'lispm-desktop-platinum-scroll-width 'lispm-ui-scroll-width)
(defvaralias 'lispm-desktop-platinum-scroll-arrow 'lispm-ui-scroll-arrow)
(defvaralias 'lispm-desktop-platinum-title-bar-height 'lispm-ui-title-bar-height)
(defvaralias 'lispm-desktop-platinum-box-size 'lispm-ui-title-box-size)
(defvaralias 'lispm-desktop-platinum-box-margin 'lispm-ui-title-box-margin)
(defvaralias 'lispm-desktop-choose-width 'lispm-ui-file-dialog-width)
(defvaralias 'lispm-desktop-choose-rows 'lispm-ui-file-dialog-rows)
(defvaralias 'lispm-desktop-choose-row-height 'lispm-ui-file-dialog-row-height)
(defvaralias 'lispm-desktop-choose-preview-limit 'lispm-ui-file-dialog-preview-limit)
;; savehist may restore the old name before the desktop loads.  Keep
;; current UI choices first, and migrate only before the names are aliases.
(unless (eq (indirect-variable 'lispm-desktop-choose-recent)
            (indirect-variable 'lispm-ui-choose-recent))
  (when (boundp 'lispm-desktop-choose-recent)
    (setq lispm-ui-choose-recent
          (delete-dups (append lispm-ui-choose-recent
                               (symbol-value 'lispm-desktop-choose-recent) nil)))
    (set 'lispm-desktop-choose-recent lispm-ui-choose-recent))
  ;; If savehist has yet to restore, recover dual-name files once it does.
  ;; A reload or a later mode toggle must not resurrect discarded entries.
  (unless (or (bound-and-true-p savehist-mode)
              (and (boundp 'savehist-loaded) (symbol-value 'savehist-loaded)))
    (add-hook 'savehist-mode-hook #'lispm-desktop-ui-compat--restore-recent)))
(defvaralias 'lispm-desktop-choose-recent 'lispm-ui-choose-recent)
(defvaralias 'lispm-desktop-choose-recent-count 'lispm-ui-choose-recent-count)

;;;; Faces

(pcase-dolist (`(,old . ,new) '((lispm-desktop-platinum-title . lispm-ui-title)
                                (lispm-desktop-platinum-title-inactive . lispm-ui-title-inactive)
                                (lispm-desktop-platinum-list . lispm-ui-list)
                                (lispm-desktop-platinum-small . lispm-ui-small)
                                (lispm-desktop-platinum-field . lispm-ui-field)
                                (lispm-desktop-platinum-field-focused . lispm-ui-field-focused)
                                (lispm-desktop-platinum-button . lispm-ui-button)
                                (lispm-desktop-platinum-button-pressed . lispm-ui-button-pressed)
                                (lispm-desktop-form-text-area . lispm-ui-form-text-area)
                                (lispm-desktop-popup-item . lispm-ui-menu-item)
                                (lispm-desktop-popup-item-highlight . lispm-ui-menu-item-highlight)
                                (lispm-desktop-popup-item-disabled . lispm-ui-menu-item-disabled)))
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

;;;; Controls in buffers

(defalias 'lispm-desktop-platinum-insert-push-button #'lispm-ui-insert-push-button)
(defalias 'lispm-desktop-platinum-insert-radio-button #'lispm-ui-insert-radio-button)
(defalias 'lispm-desktop-platinum-insert-check-box #'lispm-ui-insert-check-box)
(defalias 'lispm-desktop-platinum-insert-popup-button #'lispm-ui-insert-popup-button)

;;;; Questions and menus

(defalias 'lispm-desktop-popup-alert #'lispm-ui-alert)
(defalias 'lispm-desktop-popup-menu #'lispm-ui-menu)
;;;; File dialogs

(defalias 'lispm-desktop-choose-file #'lispm-ui-choose-file)

(provide 'lispm-desktop-ui-compat)
;;; lispm-desktop-ui-compat.el ends here
