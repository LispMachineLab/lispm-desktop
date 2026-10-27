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

;; LispM Agent writes forms as data and draws them with Emacs's widgets
;; (lispm-form.el), knowing nothing of the desktop.  The desktop dresses
;; them in its look, as it draws its own windows:
;;
;;   - their push buttons, check boxes, radio buttons and pop-up menus
;;     are the desktop's controls (lispm-desktop-platinum-controls.el),
;;     as `lispm-form-glyph-function' asks for them;
;;   - they are drawn as Mac OS 9 drew its dialogs: in the system face, on
;;     the platinum ground, their text fields white and framed, a text
;;     area of several lines a white panel;
;;   - the control or the field the keyboard is on is ringed in the focus
;;     colour, as Mac OS 9 ringed it;
;;   - a pop-up menu's choices open as the desktop's menu, from the mouse
;;     or the keyboard (`lispm-form-choose-function').
;;
;; A form is drawn again when the look, the theme or the language
;; changes.  Shown in an application's window, it is titled with its own
;; title, with no strip of buttons: its own are at its foot.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'map)
(require 'lispm-desktop-core)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-popup)
(require 'lispm-desktop-i18n)

(defvar lispm-form-glyph-function)
(defvar lispm-form-choose-function)
(defvar lispm-form-drawn-hook)
(defvar lispm-form--title)
(defvar widget-field-list)
(declare-function lispm-form-redraw "lispm-form" ())
(declare-function lispm-form-choose "lispm-form" (label choices value event))
(declare-function widget-at "wid-edit" (&optional pos))
(declare-function widget-field-at "wid-edit" (pos))
(declare-function widget-get "wid-edit" (widget property))
(declare-function widget-value "wid-edit" (widget))
(declare-function lispm-desktop-app-ui-define "lispm-desktop-app-ui" (key &rest properties))

(defvar-local lispm-desktop-form--focus nil
  "What of the form in this buffer is drawn focused: (START END PLAIN) for a
control, the text from START to END showing it focused in place of its
image PLAIN; (field . OVERLAY) for a text field; or nil.")

(defvar-local lispm-desktop-form--remaps nil
  "The face remappings dressing the form in this buffer, to undo.")

(defface lispm-desktop-form-text-area '((t :extend t))
  "Face of a text area of several lines of a form: a white panel."
  :group 'lispm-desktop-platinum)

;;;; Controls

(defun lispm-desktop-form--image (kind state label &optional focused)
  "Return the image of a form's control of KIND in STATE reading LABEL, ringed
when FOCUSED; nil on a text terminal, where the form draws it as text."
  (unless (lispm-desktop-tty-p)
    (lispm-desktop-platinum-image-of
     (pcase kind
       ('check (lispm-desktop-platinum-check-box (eq state 'on) nil focused 2))
       ('radio (lispm-desktop-platinum-radio (eq state 'on) nil focused))
       ('button (lispm-desktop-platinum-push-button label :default (eq state 'default) :focused focused))
       ('popup (lispm-desktop-platinum-popup-button label :focused focused))))))

(defun lispm-desktop-form-glyph (kind state label)
  "Return the image of a form's control in the desktop's look, or nil.
KIND, STATE and LABEL are as `lispm-form-glyph-function' gives them.
Emacs's widgets centre a check box's or a radio button's image on its
line themselves, and an image saying so twice is no image."
  (when-let* ((image (lispm-desktop-form--image kind state label)))
    (if (memq kind '(check radio))
        (cons 'image (map-delete (copy-sequence (cdr image)) :ascent))
      image)))

(defun lispm-desktop-form-choose (label choices value event)
  "Open the desktop's menu of CHOICES of a form's pop-up menu, VALUE checked.
LABEL, CHOICES, VALUE and EVENT are as `lispm-form-choose-function'
gives them.  Where no menu can be drawn, the form's own way is taken."
  (if (lispm-desktop-popup-available-p)
      (lispm-desktop-platinum-choose choices value
                                     (or (and (mouse-event-p event) (posn-point (event-start event)))
                                         (point))
                                     label)
    (lispm-form-choose label choices value event)))

;;;; Focus

(defvar lispm-desktop-form-control-properties '(:lispm-form-control :lispm-mail-control)
  "The properties of a widget saying what control of a form it is, as
`lispm-form-glyph-function' gives them: LispM's forms', and those of
applications drawing forms of their own the same way, as LispM Mail's
settings.")

(defun lispm-desktop-form--control-at (position)
  "The control of the form at POSITION drawn as an image, as (START END
PLAIN FOCUSED): the text it is drawn over, and its images; or nil."
  (when-let* ((widget (widget-at position))
              (control (seq-some (lambda (property) (widget-get widget property))
                                 lispm-desktop-form-control-properties))
              (overlay (widget-get widget :button-overlay))
              ((overlayp overlay))
              (start (overlay-start overlay))
              ((consp (get-text-property start 'display))))
    (pcase-let ((`(,kind ,state ,label)
                 (if (consp control)
                     control
                   (list control (if (widget-value widget) 'on 'off) nil))))
      (when-let* ((plain (lispm-desktop-form--image kind state label))
                  (focused (lispm-desktop-form--image kind state label t)))
        (list start (overlay-end overlay) plain focused)))))

(defun lispm-desktop-form--show (start end image)
  "Show IMAGE over the text from START to END, quietly."
  (with-silent-modifications
    (let ((inhibit-read-only t))
      (put-text-property start end 'display image))))

(defun lispm-desktop-form--field-overlay (position)
  "The overlay of the form's text field at POSITION, or nil."
  (when-let* ((field (widget-field-at position)))
    (let ((overlay (widget-get field :field-overlay)))
      (and (overlayp overlay) overlay))))

(defun lispm-desktop-form--follow ()
  "Draw focused what of the form in this buffer point is on, and plain what
was before: a control ringed, a text field framed in the focus colour.
Run from the buffer's `post-command-hook', and once the form is drawn."
  (let* ((control (lispm-desktop-form--control-at (point)))
         (field (and (not control) (lispm-desktop-form--field-overlay (point))))
         (now (cond (control control)
                    ;; A text area has no frame to ring.
                    ((and field (not (overlay-get field 'lispm-desktop-form-area)))
                     (cons 'field field)))))
    (unless (equal now lispm-desktop-form--focus)
      (pcase lispm-desktop-form--focus
        (`(field . ,overlay)
         (when (overlay-buffer overlay)
           (overlay-put overlay 'face 'widget-field)))
        (`(,start ,end ,plain ,_)
         (when (<= end (point-max))
           (lispm-desktop-form--show start end plain))))
      (pcase now
        (`(field . ,overlay) (overlay-put overlay 'face 'lispm-desktop-platinum-field-focused))
        (`(,start ,end ,_ ,focused) (lispm-desktop-form--show start end focused)))
      (setq lispm-desktop-form--focus now))))

;;;; Dressing

(defun lispm-desktop-form-dress ()
  "Dress the form in this buffer, just drawn, in the look and theme in use:
in the system face on the platinum ground, its fields framed, its text
areas white panels, what point is on drawn focused.  Run from
`lispm-form-drawn-hook', and from an application's like it."
  (mapc #'face-remap-remove-relative lispm-desktop-form--remaps)
  (setq lispm-desktop-form--remaps
        (unless (lispm-desktop-tty-p)
          (list (face-remap-add-relative 'default 'lispm-desktop-platinum-list
                                         `(:background ,(lispm-desktop-platinum-color :chrome)))
                (face-remap-add-relative 'widget-field 'lispm-desktop-platinum-field)
                (face-remap-add-relative 'lispm-desktop-form-text-area
                                         `(:background ,(lispm-desktop-platinum-color :content))))))
  (add-hook 'post-command-hook #'lispm-desktop-form--follow nil t)
  (dolist (field widget-field-list)
    (when-let* (((eq (car field) 'text))
                (overlay (widget-get field :field-overlay))
                ((overlayp overlay)))
      (overlay-put overlay 'face 'lispm-desktop-form-text-area)
      (overlay-put overlay 'lispm-desktop-form-area t)
      ;; Its lines but the first start in it: they line up with that one.
      (overlay-put overlay 'line-prefix "  ")
      ;; Its last newline is in an overlay of its own.
      (when-let* ((end (widget-get field :field-end-overlay))
                  ((overlayp end)))
        (overlay-put end 'face 'lispm-desktop-form-text-area)
        (overlay-put end 'line-prefix "  "))))
  ;; What was focused was drawn over.
  (setq lispm-desktop-form--focus nil)
  (lispm-desktop-form--follow))

(defun lispm-desktop-form-redraw-all (&rest _)
  "Draw every form again, in the look, theme and language now in use."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'lispm-form-mode)
        (lispm-form-redraw)))))

(defun lispm-desktop-form--title ()
  "The title of the form in this buffer, for its window."
  lispm-form--title)

(defun lispm-desktop-form-setup ()
  "Dress LispM's forms in the desktop's look, if LispM has forms."
  (when (require 'lispm-form nil t)
    (setq lispm-form-glyph-function #'lispm-desktop-form-glyph
          lispm-form-choose-function #'lispm-desktop-form-choose)
    (add-hook 'lispm-form-drawn-hook #'lispm-desktop-form-dress)
    (add-hook 'enable-theme-functions #'lispm-desktop-form-redraw-all)
    (add-hook 'lispm-desktop-language-changed-hook #'lispm-desktop-form-redraw-all)
    (with-eval-after-load 'lispm-desktop-app-ui
      (lispm-desktop-app-ui-define 'lispm-form-mode
                                   :title #'lispm-desktop-form--title
                                   :buttons nil
                                   :bottom nil
                                   :setup #'lispm-desktop-form--wrap))))

(defun lispm-desktop-form--wrap ()
  "Wrap the lines of the form in this buffer at words, as its help is read."
  (setq-local truncate-lines nil
              word-wrap t))

(provide 'lispm-desktop-form)
;;; lispm-desktop-form.el ends here
