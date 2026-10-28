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
;; (lispm-ui-form.el), laid out as a dialog, knowing nothing of the
;; desktop, as LispM's agents and applications write them.  The desktop
;; dresses them in its look, as it draws its own windows:
;;
;;   - in the system face on the platinum ground, the faces remapped
;;     before the form is drawn, so that it is measured in them;
;;   - their push buttons, check boxes, radio buttons and pop-up menus
;;     are the desktop's controls (lispm-desktop-platinum-controls.el),
;;     and a picture sits in an image well, as `lispm-ui-form-glyph-function'
;;     asks for them;
;;   - their groups are framed in the desktop's group boxes
;;     (`lispm-ui-form-group-function'), their text fields white and framed,
;;     a text area a white panel framed in grey;
;;   - the control or the field the keyboard is on is ringed in the focus
;;     colour, as Mac OS 9 ringed it;
;;   - a pop-up menu's choices open as the desktop's menu, from the mouse
;;     or the keyboard (`lispm-ui-form-choose-function');
;;   - a picture is chosen in the desktop's own dialog, the picture
;;     selected previewed (`lispm-ui-form-choose-file-function'), never in the
;;     system's panel, which an application shown on its own would hide.
;;
;; A form is drawn again when the look, the theme or the language
;; changes.  Shown in an application's window, it is titled with its own
;; title, its keys listed in its Help menu, not above its fields.
;;
;; What LispM UI asks a person otherwise, an alert, a choice or a file
;; (lispm-ui-ask.el), it asks in the desktop's alerts, menus and Choose a
;; File dialog, and it speaks the desktop's language.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'map)
(require 'face-remap)
(require 'lispm-desktop-core)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-popup)
(require 'lispm-desktop-choose)
(require 'lispm-desktop-i18n)
(require 'lispm-ui-form)
(require 'lispm-ui-ask)

(declare-function lispm-desktop-app-ui-define "lispm-desktop-app-ui" (key &rest properties))

(defvar-local lispm-desktop-form--focus nil
  "What of the form in this buffer is drawn focused: (START END PLAIN) for a
control, the text from START to END showing it focused in place of its
image PLAIN; (field . OVERLAY) for a text field; or nil.")

(defvar-local lispm-desktop-form--remaps nil
  "The face remappings dressing the form in this buffer, to undo.")

(defface lispm-desktop-form-text-area '((t :extend t))
  "Face of a text area of several lines of a form not LispM's: a white panel."
  :group 'lispm-desktop-platinum)

(defconst lispm-desktop-form-well-size 64
  "Width and height in pixels of the picture in a form's image well.")

;;;; Controls

(defun lispm-desktop-form--image (kind state label &optional focused)
  "Return the image of a form's control of KIND in STATE reading LABEL, ringed
when FOCUSED; nil on a text terminal, where the form draws it as text."
  (unless (lispm-desktop-tty-p)
    (if (eq kind 'picture)
        (lispm-desktop-form--well label)
      (lispm-desktop-platinum-image-of
       (pcase kind
         ('check (lispm-desktop-platinum-check-box (eq state 'on) nil focused 2))
         ('radio (lispm-desktop-platinum-radio (eq state 'on) nil focused))
         ('button (lispm-desktop-platinum-push-button label :default (eq state 'default) :focused focused))
         ('popup (lispm-desktop-platinum-popup-button label :focused focused)))))))

(defun lispm-desktop-form--well (file)
  "Return an image well holding the picture in FILE, or empty for nil: the
picture framed in a well sunk into the window, as Mac OS 9 drew one."
  (lispm-desktop-platinum-image
   (list 'form-well file)
   (lambda ()
     (let* ((size lispm-desktop-form-well-size)
            (palette lispm-desktop-platinum-current-palette)
            (grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
            (svg (svg-create (+ size 4) (+ size 4))))
       ;; Shaded along its top and left, lit along its bottom and right.
       (lispm-desktop-platinum-svg-fill svg 0 0 (+ size 4) (+ size 4) (funcall grey #xFF))
       (lispm-desktop-platinum-svg-fill svg 0 0 (+ size 3) (+ size 3) (funcall grey #x88))
       (lispm-desktop-platinum-svg-fill svg 1 1 (+ size 2) (+ size 2) (plist-get palette :outline))
       (lispm-desktop-platinum-svg-fill svg 2 2 size size (plist-get palette :content))
       (when (and file (file-readable-p file))
         (lispm-desktop-platinum-svg-emacs-image svg (create-image file) 2 2 size size))
       svg))))

(defun lispm-desktop-form-group (part &rest args)
  "Return PART of a form's group box in the desktop's look, a string.
PART and ARGS are as `lispm-ui-form-group-function' gives them: the top
edge, titled, and the bottom one are the desktop's group box's; a side
is its grey line and its lit one, as tall as the line they stand on."
  (pcase part
    ('top (lispm-desktop-platinum-group-edge (car args) (cadr args)))
    ('bottom (lispm-desktop-platinum-group-edge (car args) nil t))
    ('side (if (lispm-desktop-tty-p)
               (lispm-desktop-platinum-group-side 1)
             (let ((palette lispm-desktop-platinum-current-palette))
               (concat (propertize " " 'display '(space :width (1))
                                   'face `(:background ,(lispm-desktop-platinum-grey palette #x88)))
                       (propertize " " 'display '(space :width (1))
                                   'face `(:background ,(lispm-desktop-platinum-grey palette #xFF)))))))))

(defun lispm-desktop-form-glyph (kind state label)
  "Return the image of a form's control in the desktop's look, or nil.
KIND, STATE and LABEL are as `lispm-ui-form-glyph-function' gives them.
Emacs's widgets centre a check box's or a radio button's image on its
line themselves, and an image saying so twice is no image."
  (when-let* ((image (lispm-desktop-form--image kind state label)))
    (if (memq kind '(check radio))
        (cons 'image (map-delete (copy-sequence (cdr image)) :ascent))
      image)))

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

;;;; Focus

(defvar lispm-desktop-form-control-properties '(:lispm-form-control :lispm-mail-control)
  "The properties of a widget saying what control of a form it is, as
`lispm-ui-form-glyph-function' gives them: LispM's forms', and those of
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
  "The overlay of the form's text field of one line at POSITION, or nil.
A text area has no frame to ring."
  (when-let* ((field (widget-field-at position))
              ((not (eq (widget-type field) 'text))))
    (let ((overlay (widget-get field :field-overlay)))
      (and (overlayp overlay) overlay))))

(defun lispm-desktop-form--follow ()
  "Draw focused what of the form in this buffer point is on, and plain what
was before: a control ringed, a text field framed in the focus colour.
Run from the buffer's `post-command-hook', and once the form is drawn."
  (let* ((control (lispm-desktop-form--control-at (point)))
         (field (and (not control) (lispm-desktop-form--field-overlay (point))))
         (now (cond (control control)
                    (field (cons 'field field)))))
    (unless (equal now lispm-desktop-form--focus)
      (pcase lispm-desktop-form--focus
        (`(field . ,overlay)
         (when (overlay-buffer overlay)
           (overlay-put overlay 'face 'widget-field)))
        (`(,start ,end ,plain ,_)
         (when (<= end (point-max))
           (lispm-desktop-form--show start end plain))))
      (pcase now
        (`(field . ,overlay)
         (overlay-put overlay 'face 'lispm-desktop-platinum-field-focused))
        (`(,start ,end ,_ ,focused) (lispm-desktop-form--show start end focused)))
      (setq lispm-desktop-form--focus now))))

;;;; Dressing

(defun lispm-desktop-form--faces ()
  "Remap the faces of the form in this buffer to the look and theme in use:
the system face on the platinum ground, the fields white and framed, a
text area white, framed in grey, the help small.  LispM's forms take them
before they are drawn, to be measured in them."
  (mapc #'face-remap-remove-relative lispm-desktop-form--remaps)
  (when (derived-mode-p 'lispm-ui-form-mode)
    ;; The window shows the title and the keys.
    (setq-local lispm-ui-form-show-header nil)
    ;; The system face is narrower than Emacs's own: the fields widen.
    (setq-local lispm-ui-form-width 44))
  (setq lispm-desktop-form--remaps
        (unless (lispm-desktop-tty-p)
          (let ((content (lispm-desktop-platinum-color :content)))
            (list (face-remap-add-relative 'default 'lispm-desktop-platinum-list
                                           `(:background ,(lispm-desktop-platinum-color :chrome)))
                  (face-remap-add-relative 'widget-field 'lispm-desktop-platinum-field)
                  (face-remap-add-relative 'lispm-ui-form-area `(:background ,content :foreground
                                                              ,(lispm-desktop-platinum-color :text)))
                  (face-remap-add-relative 'lispm-ui-form-area-edge
                                           `(:background ,(lispm-desktop-platinum-grey
                                                           lispm-desktop-platinum-current-palette #x88)))
                  (face-remap-add-relative 'lispm-desktop-form-text-area `(:background ,content))
                  (face-remap-add-relative 'lispm-ui-form-help 'lispm-desktop-platinum-small
                                           `(:foreground ,(lispm-desktop-platinum-color :muted)))
                  (face-remap-add-relative 'lispm-ui-form-title 'lispm-desktop-platinum-title))))))

(defun lispm-desktop-form--text-areas ()
  "Draw the text areas of the form in this buffer as white panels, their
lines after the first lined up with it: a form not LispM's, which draws
its own."
  (dolist (field widget-field-list)
    (when-let* (((eq (car field) 'text))
                (overlay (widget-get field :field-overlay))
                ((overlayp overlay)))
      (overlay-put overlay 'face 'lispm-desktop-form-text-area)
      (overlay-put overlay 'line-prefix "  ")
      ;; Its last newline is in an overlay of its own.
      (when-let* ((end (widget-get field :field-end-overlay))
                  ((overlayp end)))
        (overlay-put end 'face 'lispm-desktop-form-text-area)
        (overlay-put end 'line-prefix "  ")))))

(defun lispm-desktop-form-dress ()
  "Dress the form in this buffer, just drawn: what point is on drawn
focused, as the keyboard moves.  A form not LispM's, drawn with Emacs's
widgets as an application's settings are, is put in the look and theme
in use here too, its text areas white panels.  Run from
`lispm-ui-form-drawn-hook', and from an application's like it."
  (unless (derived-mode-p 'lispm-ui-form-mode)
    (lispm-desktop-form--faces)
    (lispm-desktop-form--text-areas))
  (add-hook 'post-command-hook #'lispm-desktop-form--follow nil t)
  ;; What was focused was drawn over.
  (setq lispm-desktop-form--focus nil)
  (lispm-desktop-form--follow))

(defun lispm-desktop-form-redraw-all (&rest _)
  "Draw every form again, in the look, theme and language now in use."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'lispm-ui-form-mode)
        (lispm-desktop-form--faces)
        (lispm-ui-form-redraw)))))

(defun lispm-desktop-form--title ()
  "The title of the form in this buffer, for its window."
  (lispm-ui-form-title))

(defun lispm-desktop-form-setup ()
  "Dress LispM UI's forms in the desktop's look, and have LispM UI ask what
it asks in the desktop's alerts, menus and dialog, in its language."
  (setq lispm-ui-form-glyph-function #'lispm-desktop-form-glyph
        lispm-ui-form-group-function #'lispm-desktop-form-group
        lispm-ui-form-choose-function #'lispm-desktop-form-choose
        lispm-ui-form-choose-file-function #'lispm-desktop-form-choose-file)
  (add-hook 'lispm-ui-form-mode-hook #'lispm-desktop-form--faces)
  (add-hook 'lispm-ui-form-drawn-hook #'lispm-desktop-form-dress)
  (add-hook 'enable-theme-functions #'lispm-desktop-form-redraw-all)
  (add-hook 'lispm-desktop-language-changed-hook #'lispm-desktop-form-redraw-all)
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

(defun lispm-desktop-form--wrap ()
  "Wrap the lines of the form in this buffer at words, as its help is read."
  (setq-local truncate-lines nil
              word-wrap t))

(provide 'lispm-desktop-form)
;;; lispm-desktop-form.el ends here
