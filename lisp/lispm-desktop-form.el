;;; lispm-desktop-form.el --- LispM's forms, in the desktop's look  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

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

;; LispM UI draws forms written as data with Emacs's widgets
;; (lispm-ui-form.el), laid out as a dialog, and dresses them in its look
;; once it has a palette, which the desktop derives from the theme
;; (lispm-desktop-platinum-palette.el): its controls, group boxes, text
;; fields and image wells, the control or field the keyboard is on ringed
;; in the focus colour, all drawn again when the look, the theme or the
;; language changes.  The desktop shows them in its windows:
;;
;;   - shown in an application's window, a form is titled with its own
;;     title, its keys listed in its Help menu, not above its fields, and
;;     its fields are as wide as the system face, narrower than Emacs's
;;     own, makes them;
;;   - a pop-up menu's choices open as the desktop's menu, from the mouse
;;     or the keyboard (`lispm-ui-form-choose-function');
;;   - a picture is chosen in the desktop's own dialog, the picture
;;     selected previewed (`lispm-ui-form-choose-file-function'), never in the
;;     system's panel, which an application shown on its own would hide.
;;
;; What LispM UI asks a person otherwise, an alert, a choice or a file
;; (lispm-ui-ask.el), it asks in the desktop's alerts, menus and Choose a
;; File dialog, and it speaks the desktop's language.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-popup)
(require 'lispm-desktop-choose)
(require 'lispm-desktop-i18n)
(require 'lispm-ui-form)
(require 'lispm-ui-ask)

(declare-function lispm-desktop-app-ui-define "lispm-desktop-app-ui" (key &rest properties))

;;;; Choosing

(defun lispm-desktop-form-choose (label choices value event)
  "Open the desktop's menu of CHOICES of a form's pop-up menu, VALUE checked.
LABEL, CHOICES, VALUE and EVENT are as `lispm-ui-form-choose-function'
gives them.  Where no menu can be drawn, the form's own way is taken."
  (if (lispm-desktop-popup-available-p)
      (lispm-desktop-platinum-choose choices value
                                     (or (and (mouse-event-p event) (posn-point (event-start event)))
                                         (point))
                                     label)
    (lispm-ui-form-choose label choices value event)))

(defun lispm-desktop-form-choose-file (type label file predicate)
  "Show the desktop's dialog choosing a file for a form's field of TYPE and
LABEL, FILE chosen now, among the files PREDICATE takes; return the file
chosen, or nil.  A picture's is titled so, and previews the picture
selected.  TYPE, LABEL, FILE and PREDICATE are as
`lispm-ui-form-choose-file-function' gives them."
  (ignore label)
  (lispm-desktop-choose-file :title (if (eq type 'picture)
                                        (lispm-desktop-tr "Choose a Picture")
                                      (lispm-desktop-tr "Choose a File"))
                             :file file :predicate predicate :preview (eq type 'picture)))

;;;; Showing

(defun lispm-desktop-form--layout ()
  "Lay the form in this buffer out for a desktop's window, before it is
drawn: the window shows its title and its keys, and the system face is
narrower than Emacs's own, so its fields widen."
  (setq-local lispm-ui-form-show-header nil
              lispm-ui-form-width 44))

(defun lispm-desktop-form--title ()
  "The title of the form in this buffer, for its window."
  (lispm-ui-form-title))

(defun lispm-desktop-form--wrap ()
  "Wrap the lines of the form in this buffer at words, as its help is read."
  (setq-local truncate-lines nil
              word-wrap t))

(defun lispm-desktop-form-setup ()
  "Show LispM UI's forms in the desktop's windows, choosing in its menus and
dialog, and have LispM UI ask what it asks in the desktop's alerts, menus
and dialog, in its language."
  (setq lispm-ui-form-choose-function #'lispm-desktop-form-choose
        lispm-ui-form-choose-file-function #'lispm-desktop-form-choose-file)
  (add-hook 'lispm-ui-form-mode-hook #'lispm-desktop-form--layout)
  (setq lispm-ui-alert-function #'lispm-desktop-popup-alert
        lispm-ui-choose-function #'lispm-desktop-platinum-choose
        lispm-ui-choose-file-function #'lispm-desktop-choose-file)
  ;; Before LispM's agent's, which follows the desktop's too.
  (add-hook 'lispm-ui-language-functions #'lispm-desktop-current-language -50)
  (add-hook 'lispm-desktop-language-changed-hook #'lispm-ui-language-changed)
  (with-eval-after-load 'lispm-desktop-app-ui
    (lispm-desktop-app-ui-define 'lispm-ui-form-mode
                                 :title #'lispm-desktop-form--title
                                 :buttons nil
                                 :bottom nil
                                 :setup #'lispm-desktop-form--wrap)))

(provide 'lispm-desktop-form)
;;; lispm-desktop-form.el ends here
