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
(require 'lispm-finder)

(declare-function lispm-desktop-app-ui-define "lispm-desktop-app-ui" (key &rest properties))

;;;; Choosing

(defun lispm-desktop-form-choose (label choices value event)
  "Open the desktop's menu of CHOICES of a form's pop-up menu, VALUE checked.
LABEL, CHOICES, VALUE and EVENT are as `lispm-ui-form-choose-function'
gives them.  Where no menu can be drawn, the form's own way is taken."
  (if (lispm-ui-popup-available-p)
      (let* ((frame (selected-frame))
             (screen (lispm-desktop-apps-screen))
             (on-screen (and (frame-live-p screen)
                             (not (eq frame screen))
                             (eq (lispm-desktop-platinum-window-parent frame) screen))))
        (if (not on-screen)
            (lispm-ui-choose choices value
                             (or (and (mouse-event-p event) (posn-point (event-start event)))
                                 (point))
                             label)
          (let* ((position (or (and (mouse-event-p event) (posn-point (event-start event)))
                               (point)))
                 (xy (and (pos-visible-in-window-p position nil t)
                          (posn-x-y (posn-at-point position))))
                 (win-pos (lispm-desktop-platinum-window-position frame))
                 (edges (window-inside-pixel-edges))
                 (lispm-ui-menu-parent screen)
                 (item (lispm-ui-menu-choose
                        (mapcar (pcase-lambda (`(,val . ,text))
                                  (list :label text :value val :checked (equal val value)))
                                choices)
                        (and xy (+ (car win-pos) (car edges) (car xy)))
                        (and xy (+ (cdr win-pos) (cadr edges) (cdr xy)
                                   lispm-ui-button-height)))))
            (and item (list (plist-get item :value))))))
    (lispm-ui-form-choose label choices value event)))

;;;; Showing

(defvar-local lispm-desktop-form--prompt nil
  "Non-nil in the buffer of a modal prompt form shown in its desktop window.")

(defun lispm-desktop-form-window-size (form)
  "The size of the window of FORM, a form's buffer: as wide as its group
boxes, as tall as all of it, as far as the screen allows."
  (let* ((geom (lispm-ui-form-geometry form))
         (prompt-p (buffer-local-value 'lispm-desktop-form--prompt form))
         (outer (or (plist-get geom :outer) 480))
         (width (if prompt-p
                    (+ (or (plist-get geom :right) 400)
                       (or (plist-get geom :start) (lispm-desktop-px 32)))
                  (max (lispm-desktop-px 500) (+ outer (lispm-desktop-px 40)))))
         (win (get-buffer-window form t))
         (height (with-current-buffer form
                   (let ((truncate-lines (if win truncate-lines t)))
                     (cdr (buffer-text-pixel-size form win)))))
         (min-h (if prompt-p 126 340))
         (pad-h (if prompt-p 42 32))
         (screen (lispm-desktop-apps-screen))
         (max-height (if (and (frame-live-p screen) (lispm-desktop-apps-hidden-p screen))
                         (- (nth 3 (lispm-desktop-apps--workarea screen))
                            (lispm-desktop-px 120))
                       (- (frame-pixel-height screen) (lispm-desktop-px 120)))))
    (cons width
          (min (max (lispm-desktop-px min-h) (+ height (lispm-desktop-px pad-h)))
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
              lispm-ui-form-width (if lispm-desktop-form--prompt 36 44)
              line-spacing 6
              display-line-numbers nil)
  (when (bound-and-true-p display-line-numbers-mode)
    (display-line-numbers-mode -1)))

(defun lispm-desktop-form--title ()
  "The title of the form in this buffer, for its window."
  (lispm-ui-form-title))

(defun lispm-desktop-form--wrap ()
  "Wrap the lines of the form in this buffer at words, as its help is read."
  (setq-local truncate-lines nil
              word-wrap t
              display-line-numbers nil
              lispm-desktop-platinum-window-icon
              (or lispm-desktop-platinum-window-icon 'none))
  (when (bound-and-true-p display-line-numbers-mode)
    (display-line-numbers-mode -1)))

(defun lispm-desktop-form--center-modal (frame &optional target)
  "Center modal prompt FRAME over TARGET, or over its screen."
  (when-let* (((frame-live-p frame))
              (screen (lispm-desktop-platinum-window-parent frame)))
    (let* ((target (or target (frame-parameter frame 'lispm-ui-modal-prompt) screen))
           (in-target (and (frame-live-p target)
                           (not (eq target screen))
                           (eq (lispm-desktop-platinum-window-parent target) screen)))
           (pos (if in-target (lispm-desktop-platinum-window-position target) '(0 . 0)))
           (tw (if in-target (frame-pixel-width target) (frame-inner-width screen)))
           (th (if in-target (frame-pixel-height target) (frame-inner-height screen)))
           (w (frame-text-width frame))
           (h (frame-text-height frame))
           (left (+ (car pos) (/ (- tw w) 2)))
           (top (+ (cdr pos) (/ (- th h) 2))))
      (lispm-desktop-apps--put frame (list left top w h))
      (lispm-desktop-apps-keep-on-desktop frame)
      (set-frame-parameter frame 'lispm-desktop-window-place
                           (lispm-desktop-apps--geometry frame)))))

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
              (if (frame-parameter frame 'lispm-ui-modal-prompt)
                  (lispm-desktop-form--center-modal frame)
                (lispm-desktop-apps-keep-on-desktop frame)))))))))

(defvar lispm-desktop-form--modal-stack nil
  "Stack of active modal prompt child frames on the desktop screen.")

(defun lispm-desktop-form--modal-show (buffer)
  "Show modal prompt form BUFFER centered over the current frame; return cleanup."
  (if (and (display-graphic-p) (not noninteractive))
      (let* ((prev-frame (selected-frame))
             (prev-window (selected-window))
             (anchor-frame (or (frame-parameter prev-frame 'lispm-ui-modal-prompt)
                               prev-frame))
             (screen (lispm-desktop-apps-screen))
             (title (or (lispm-ui-form-title buffer) "Prompt"))
             (z-order (+ 30 (* 5 (length lispm-desktop-form--modal-stack))))
             (frame nil)
             (cleanup
              (lambda ()
                (setq lispm-desktop-form--modal-stack
                      (delq frame lispm-desktop-form--modal-stack))
                (when (frame-live-p frame)
                  (let ((outer (lispm-desktop-platinum-window-outer frame)))
                    (delete-frame frame t)
                    (when (and (not (eq outer frame)) (frame-live-p outer))
                      (let ((buf (window-buffer (frame-root-window outer))))
                        (delete-frame outer t)
                        (when (buffer-live-p buf)
                          (kill-buffer buf))))))
                (when (frame-live-p prev-frame)
                  (select-frame-set-input-focus prev-frame))
                (when (window-live-p prev-window)
                  (select-window prev-window)))))
        (with-current-buffer buffer
          (let ((base (format "*%s*" title)))
            (unless (equal (buffer-name buffer) base)
              (rename-buffer base t)))
          (setq-local lispm-desktop-form--prompt t
                      lispm-ui-form-width 36
                      line-spacing 6
                      display-line-numbers nil
                      lispm-desktop-platinum-window-title #'lispm-desktop-form--title
                      lispm-desktop-platinum-window-icon 'none
                      lispm-desktop-platinum-box-functions
                      (list :close (lambda (win)
                                     (let ((buf (window-buffer win)))
                                       (when (buffer-live-p buf)
                                         (with-current-buffer buf
                                           (lispm-ui-form-cancel)))))))
          (when (bound-and-true-p display-line-numbers-mode)
            (display-line-numbers-mode -1))
          (lispm-ui-form-redraw))
        (let ((ok nil))
          (unwind-protect
              (pcase-let* ((`(,width . ,height) (lispm-desktop-form-window-size buffer))
                           (left (/ (- (frame-inner-width screen) width) 2))
                           (top (/ (- (frame-inner-height screen) height) 2)))
                (setq frame
                      (let ((after-make-frame-functions nil)
                            (before-make-frame-hook nil))
                        (make-frame
                         `((parent-frame . ,screen)
                           (minibuffer . ,(minibuffer-window screen))
                           (name . ,title)
                           (lispm-desktop-app . ,(lispm-desktop-apps-frontmost))
                           (lispm-desktop-window-kind . prompt)
                           (lispm-ui-modal-prompt . ,anchor-frame)
                           (lispm-desktop-close-function
                            . ,(lambda (win)
                                 (let ((buf (window-buffer win)))
                                   (when (buffer-live-p buf)
                                     (with-current-buffer buf
                                       (lispm-ui-form-cancel))))))
                           (lispm-desktop-window . t) (top-visible . ,z-order)
                           (undecorated . t) (visibility . nil)
                           (tab-bar-lines . 0) (tab-bar-lines-keep-state . t)
                           (menu-bar-lines . 0) (tool-bar-lines . 0)
                           (internal-border-width . 0)
                           (child-frame-border-width . ,(lispm-desktop-platinum-window-border))
                           (no-other-frame . t) (skip-taskbar . t) (desktop-dont-save . t)
                           (user-position . t)
                           (left . ,left) (top . ,top)
                           (width . (text-pixels . ,width))
                           (height . (text-pixels . ,height))))))
                (push frame lispm-desktop-form--modal-stack)
                (set-face-attribute 'internal-border frame :background (lispm-ui-color :chrome))
                (let ((win (frame-root-window frame)))
                  (set-window-buffer win buffer)
                  (set-window-dedicated-p win t)
                  (with-current-buffer buffer
                    (setq-local lispm-desktop-platinum-window-icon 'none
                                mode-line-format nil
                                header-line-format nil
                                display-line-numbers nil)
                    (when (bound-and-true-p display-line-numbers-mode)
                      (display-line-numbers-mode -1)))
                  (set-window-fringes win 0 0)
                  (set-window-parameter win 'mode-line-format 'none)
                  (set-window-parameter win 'header-line-format 'none))
                (pcase-let ((`(,w . ,h) (lispm-desktop-form-window-size buffer)))
                  (set-frame-size frame w h t))
                (lispm-desktop-apps-show-window frame)
                (lispm-desktop-form--center-modal frame anchor-frame)
                (lispm-desktop-platinum-raise-window frame)
                (select-frame-set-input-focus frame)
                (select-window (frame-root-window frame))
                (lispm-desktop-platinum-frame-update frame)
                (with-current-buffer buffer
                  (force-mode-line-update))
                (setq ok t)
                cleanup)
            (unless ok
              (funcall cleanup)))))
    (lispm-ui-form--modal-show-default buffer)))

(defun lispm-desktop-form--finder-modal-show (buffer title width height)
  "Show Finder chooser BUFFER in a floating Platinum modal window of WIDTHxHEIGHT."
  (if (and (display-graphic-p) (not noninteractive))
      (let* ((prev-frame (selected-frame))
             (prev-window (selected-window))
             (anchor-frame (or (frame-parameter prev-frame 'lispm-ui-modal-prompt)
                               prev-frame))
             (screen (lispm-desktop-apps-screen))
             (z-order (+ 30 (* 5 (length lispm-desktop-form--modal-stack))))
             (left (/ (- (frame-inner-width screen) width) 2))
             (top (/ (- (frame-inner-height screen) height) 2))
             (close-fn (lambda (win)
                         (let ((buf (window-buffer win)))
                           (when (buffer-live-p buf)
                             (with-current-buffer buf
                               (lispm-finder-choose-cancel))))))
             (frame nil)
             (cleanup
              (lambda ()
                (setq lispm-desktop-form--modal-stack
                      (delq frame lispm-desktop-form--modal-stack))
                (when (frame-live-p frame)
                  (let ((outer (lispm-desktop-platinum-window-outer frame)))
                    (delete-frame frame t)
                    (when (and (not (eq outer frame)) (frame-live-p outer))
                      (let ((buf (window-buffer (frame-root-window outer))))
                        (delete-frame outer t)
                        (when (buffer-live-p buf)
                          (kill-buffer buf))))))
                (when (frame-live-p prev-frame)
                  (select-frame-set-input-focus prev-frame))
                (when (window-live-p prev-window)
                  (select-window prev-window)))))
        (with-current-buffer buffer
          (setq-local display-line-numbers nil
                      lispm-desktop-platinum-window-title title
                      lispm-desktop-platinum-window-icon
                      (lispm-desktop-platinum-folder-icon lispm-finder--directory)
                      lispm-desktop-platinum-title-menu-function #'lispm-finder--path-items
                      lispm-desktop-context-menu-function #'lispm-finder--keyboard-menu
                      lispm-desktop-platinum-box-functions (list :close close-fn))
          (when (bound-and-true-p display-line-numbers-mode)
            (display-line-numbers-mode -1)))
        (let ((ok nil))
          (unwind-protect
              (progn
                (setq frame
                      (let ((after-make-frame-functions nil)
                            (before-make-frame-hook nil))
                        (make-frame
                         `((parent-frame . ,screen)
                           (minibuffer . ,(minibuffer-window screen))
                           (name . ,title)
                           (lispm-desktop-app . ,(lispm-desktop-apps-frontmost))
                           (lispm-desktop-window-kind . finder-choose)
                           (lispm-ui-modal-prompt . ,anchor-frame)
                           (lispm-desktop-close-function . ,close-fn)
                           (lispm-desktop-window . t) (top-visible . ,z-order)
                           (undecorated . t) (visibility . nil)
                           (tab-bar-lines . 0) (tab-bar-lines-keep-state . t)
                           (menu-bar-lines . 0) (tool-bar-lines . 0)
                           (internal-border-width . 0)
                           (child-frame-border-width . ,(lispm-desktop-platinum-window-border))
                           (no-other-frame . t) (skip-taskbar . t) (desktop-dont-save . t)
                           (user-position . t)
                           (left . ,left) (top . ,top)
                           (width . (text-pixels . ,width))
                           (height . (text-pixels . ,height))))))
                (push frame lispm-desktop-form--modal-stack)
                (set-face-attribute 'internal-border frame :background (lispm-ui-color :chrome))
                (let ((win (frame-root-window frame)))
                  (set-window-buffer win buffer)
                  (set-window-dedicated-p win t)
                  (set-window-fringes win 0 0)
                  (set-window-parameter win 'mode-line-format nil)
                  (set-window-parameter win 'header-line-format nil))
                (with-current-buffer buffer
                  (setq-local header-line-format '(:eval (lispm-finder--choose-header))
                              mode-line-format '(:eval (lispm-finder--choose-footer))))
                (set-frame-size frame width height t)
                (lispm-desktop-apps-show-window frame)
                (lispm-desktop-form--center-modal frame anchor-frame)
                (lispm-desktop-platinum-raise-window frame)
                (select-frame-set-input-focus frame)
                (select-window (frame-root-window frame))
                (lispm-desktop-platinum-frame-update frame)
                (with-current-buffer buffer
                  (force-mode-line-update))
                (setq ok t)
                cleanup)
            (unless ok
              (funcall cleanup)))))
    (lispm-finder--modal-show-default buffer title width height)))

(setq lispm-finder-modal-show-function #'lispm-desktop-form--finder-modal-show)

(defun lispm-desktop-form-setup ()
  "Show LispM UI's forms in the desktop's windows and language."
  (lispm-desktop-app-window-define 'prompt
                                   :size '(520 . 220)
                                   :documents 'keep
                                   :close #'lispm-desktop-app-window-close-box)
  (setq lispm-ui-form-choose-function #'lispm-desktop-form-choose
        lispm-ui-form-modal-show-function #'lispm-desktop-form--modal-show
        lispm-finder-modal-show-function #'lispm-desktop-form--finder-modal-show)
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
