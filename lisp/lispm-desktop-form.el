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
;;   - a picture is chosen in LispM UI's dialog, with its preview.
;;
;; What LispM UI asks a person otherwise, an alert, a choice or a file
;; (lispm-ui-ask.el), it draws itself, speaking the desktop's language.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-app-window)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-popup)
(require 'lispm-desktop-i18n)
(require 'lispm-ui-form)
(require 'lispm-ui-ask)

(declare-function lispm-desktop-app-ui-define "lispm-desktop-app-ui" (key &rest properties))

;;;; Choosing

(defun lispm-desktop-form-choose (label choices value event)
  "Open the desktop's menu of CHOICES of a form's pop-up menu, VALUE checked.
LABEL, CHOICES, VALUE and EVENT are as `lispm-ui-form-choose-function'
gives them.  Where no menu can be drawn, the form's own way is taken."
  (if (lispm-ui-popup-available-p)
      (lispm-ui-choose choices value
                       (or (and (mouse-event-p event) (posn-point (event-start event)))
                           (point))
                       label)
    (lispm-ui-form-choose label choices value event)))

;;;; Showing

(defvar-local lispm-desktop-form--prompt nil
  "Non-nil in the buffer of a modal prompt form shown in its desktop window.")

(defun lispm-desktop-form-window-size (form)
  "The size of the window of FORM, a form's buffer: as wide as its group
boxes, as tall as all of it, as far as the screen allows."
  (let* ((outer (or (plist-get (lispm-ui-form-geometry form) :outer) 480))
         (height (with-current-buffer form
                   (let ((truncate-lines t))
                     (cdr (buffer-text-pixel-size form (get-buffer-window form t))))))
         (min-h (if (buffer-local-value 'lispm-desktop-form--prompt form) 180 340))
         (screen (lispm-desktop-apps-screen))
         (max-height (if (and (frame-live-p screen) (lispm-desktop-apps-hidden-p screen))
                         (- (nth 3 (lispm-desktop-apps--workarea screen))
                            (lispm-desktop-px 120))
                       (- (frame-pixel-height screen) (lispm-desktop-px 120)))))
    (cons (max (lispm-desktop-px 500) (+ outer (lispm-desktop-px 40)))
          (min (max (lispm-desktop-px min-h) (+ height (lispm-desktop-px 36)))
               max-height))))

(defun lispm-desktop-form-window-closed (kind variable)
  "Close the window of kind KIND once no form it holds is left.
VARIABLE is non-nil in the buffer of each form KIND's window shows."
  (unless (seq-some (lambda (buffer)
                      (and (eq (buffer-local-value 'major-mode buffer) 'lispm-ui-form-mode)
                           (buffer-local-value variable buffer)))
                    (buffer-list))
    (lispm-desktop-app-window-close kind)))

(defun lispm-desktop-form-show (kind form variable)
  "Show FORM, a form's buffer, in the window of kind KIND: one at a time.
VARIABLE, made non-nil in FORM, marks it as a form KIND's window shows
until it dies, when the window closes."
  (dolist (buffer (buffer-list))
    (when (and (not (eq buffer form))
               (buffer-live-p buffer)
               (eq (buffer-local-value 'major-mode buffer) 'lispm-ui-form-mode)
               (buffer-local-value variable buffer))
      (with-current-buffer buffer
        (set (make-local-variable variable) nil))
      (kill-buffer buffer)))
  (when (lispm-desktop-app-window-open-p kind)
    (lispm-desktop-app-window-close kind))
  (with-current-buffer form
    (when-let* ((title (lispm-ui-form-title form))
                (base (format "*%s*" title)))
      (unless (equal (buffer-name form) base)
        (rename-buffer base t)))
    (set (make-local-variable variable) t)
    (add-hook 'kill-buffer-hook
              (lambda ()
                (set (make-local-variable variable) nil)
                (lispm-desktop-form-window-closed kind variable))
              nil t))
  (pcase-let* ((`(,width . ,height) (lispm-desktop-form-window-size form))
               (frame (lispm-desktop-app-window-show
                       kind
                       :name (lispm-ui-form-title form)
                       :buffer form
                       :size (cons width height))))
    ;; The window opens where the last form's was, but each form is a size
    ;; of its own: an application's form is shorter than a character's.
    (unless (and (= width (frame-text-width frame)) (= height (frame-text-height frame)))
      (set-frame-size frame width height t)
      (lispm-desktop-platinum-frame-update frame)
      (lispm-desktop-apps-keep-on-desktop frame))
    frame))

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

(defun lispm-desktop-form--fit ()
  "Size the floating window showing this form to the form's current size."
  (let ((buffer (current-buffer)))
    (dolist (window (get-buffer-window-list buffer 'never t))
      (let ((frame (window-frame window)))
        (when (and (frame-live-p frame)
                   (frame-parameter frame 'lispm-desktop-window)
                   (null (cdr (window-list frame 'never)))
                   (not (lispm-desktop-app-window-rolled-p frame)))
          (pcase-let ((`(,width . ,height) (lispm-desktop-form-window-size buffer)))
            (unless (and (= width (frame-text-width frame)) (= height (frame-text-height frame)))
              (set-frame-size frame width height t)
              (lispm-desktop-platinum-frame-update frame)
              (lispm-desktop-apps-keep-on-desktop frame))))))))

(defun lispm-desktop-form--modal-show (buffer)
  "Show modal prompt form BUFFER in a desktop window; return a cleanup function."
  (if (and (display-graphic-p) (not noninteractive))
      (let ((prev-frame (selected-frame))
            (prev-window (selected-window))
            (frame (lispm-desktop-form-show 'prompt buffer 'lispm-desktop-form--prompt)))
        (when (frame-live-p frame)
          (select-frame-set-input-focus frame)
          (select-window (frame-root-window frame)))
        (lambda ()
          (when (lispm-desktop-app-window-open-p 'prompt)
            (lispm-desktop-app-window-close 'prompt))
          (when (frame-live-p prev-frame)
            (select-frame-set-input-focus prev-frame))
          (when (window-live-p prev-window)
            (select-window prev-window))))
    (lispm-ui-form--modal-show-default buffer)))

(defun lispm-desktop-form-setup ()
  "Show LispM UI's forms in the desktop's windows and language."
  (lispm-desktop-app-window-define 'prompt
                                   :size '(520 . 220)
                                   :documents 'keep
                                   :close #'lispm-desktop-app-window-close-box)
  (setq lispm-ui-form-choose-function #'lispm-desktop-form-choose
        lispm-ui-form-modal-show-function #'lispm-desktop-form--modal-show)
  (lispm-ui-intercept-enable)
  (add-hook 'lispm-ui-form-mode-hook #'lispm-desktop-form--layout)
  (add-hook 'lispm-ui-form-drawn-hook #'lispm-desktop-form--fit)
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
