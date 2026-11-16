;;; lispm-desktop-finder.el --- The file manager: Finder windows on the desktop  -*- lexical-binding: t; -*-

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

;; Bridges the shared `lispm-finder' component (`lispm-finder.el') into
;; `lispm-desktop': hosts Finder in a desktop child frame, connects
;; desktop Trash drag-and-drop, desktop aliases, Platinum window chrome,
;; and provides `lispm-desktop-finder-*' compatibility aliases.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-finder)
(require 'lispm-desktop-core)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-app-window)
(require 'lispm-desktop-surface)
(require 'lispm-desktop-tty-images)
(require 'lispm-desktop-i18n)

(declare-function lispm-desktop-editor-activate "lispm-desktop-editor" ())
(declare-function lispm-ui-menu "lispm-ui-menu" (items &optional x y align))
(declare-function lispm-desktop-popup-frame-xy "lispm-desktop-popup" (position))
(declare-function lispm-desktop-popup-augment-items "lispm-desktop-popup" (items &optional window))
(declare-function lispm-ui-alert "lispm-ui-ask" (icon message buttons &optional detail))

;;;; Variable & Face Aliases

(eval-and-compile
  (dolist (pair '((lispm-desktop-finder-buffer-name . lispm-finder-buffer-name)
                  (lispm-desktop-finder-cell-width . lispm-finder-cell-width)
                  (lispm-desktop-finder-icon-size . lispm-finder-icon-size)
                  (lispm-desktop-finder-text-cell-width . lispm-finder-text-cell-width)
                  (lispm-desktop-finder-text-icon-size . lispm-finder-text-icon-size)
                  (lispm-desktop-finder--had-focus . lispm-finder--had-focus)
                  (lispm-desktop-finder--directory . lispm-finder--directory)
                  (lispm-desktop-finder--back . lispm-finder--back)
                  (lispm-desktop-finder--forward . lispm-finder--forward)
                  (lispm-desktop-finder--items . lispm-finder--items)
                  (lispm-desktop-finder--selected . lispm-finder--selected)
                  (lispm-desktop-finder--selected-app . lispm-finder--selected-app)
                  (lispm-desktop-finder--columns . lispm-finder--columns)
                  (lispm-desktop-finder--typed . lispm-finder--typed)
                  (lispm-desktop-finder-folders . lispm-finder-folders)
                  (lispm-desktop-finder--icons . lispm-finder--icons)
                  (lispm-desktop-finder-item-map . lispm-finder-item-map)
                  (lispm-desktop-finder--up-target . lispm-finder--up-target)
                  (lispm-desktop-finder--up-button-map . lispm-finder--up-button-map)
                  (lispm-desktop-finder-mode-map . lispm-finder-mode-map)))
    (unless (local-variable-if-set-p (car pair))
      (defvaralias (car pair) (cdr pair)))))

(put 'lispm-desktop-finder-label 'face-alias 'lispm-finder-label)

;;;; Function Aliases & Wrappers

(defalias 'lispm-desktop-finder--folder #'lispm-finder--folder)
(defalias 'lispm-desktop-finder--folder-items #'lispm-finder--folder-items)
(defalias 'lispm-desktop-finder--read #'lispm-finder--read)
(defalias 'lispm-desktop-finder--icon-name #'lispm-finder--icon-name)
(defalias 'lispm-desktop-finder--file-icon-name #'lispm-finder--file-icon-name)
(defalias 'lispm-desktop-finder--read-again #'lispm-finder--read-again)
(defalias 'lispm-desktop-finder--icon #'lispm-finder--icon)
(defalias 'lispm-desktop-finder--up-label #'lispm-finder--up-label)
(defalias 'lispm-desktop-finder--up-button #'lispm-finder--up-button)
(defalias 'lispm-desktop-finder--info #'lispm-finder--info)
(defalias 'lispm-desktop-finder--title #'lispm-finder--title)
(defalias 'lispm-desktop-finder--cell-left #'lispm-finder--cell-left)
(defalias 'lispm-desktop-finder-real-p #'lispm-finder-real-p)
(defalias 'lispm-desktop-finder--parent #'lispm-finder--parent)
(defalias 'lispm-desktop-finder--go #'lispm-finder--go)
(defalias 'lispm-desktop-finder-back #'lispm-finder-back)
(defalias 'lispm-desktop-finder-forward #'lispm-finder-forward)
(defalias 'lispm-desktop-finder-up #'lispm-finder-up)
(defalias 'lispm-desktop-finder-refresh #'lispm-finder-refresh)
(defalias 'lispm-desktop-finder-folder-changed #'lispm-finder-folder-changed)
(defalias 'lispm-desktop-finder-open #'lispm-finder-open)
(defalias 'lispm-desktop-finder--real-only #'lispm-finder--real-only)
(defalias 'lispm-desktop-finder-new-folder #'lispm-finder-new-folder)
(defalias 'lispm-desktop-finder--trash-action #'lispm-finder--trash-action)
(defalias 'lispm-desktop-finder--app #'lispm-finder--app)
(defalias 'lispm-desktop-finder-alias-p #'lispm-finder-alias-p)
(defalias 'lispm-desktop-finder-trash-label #'lispm-finder-trash-label)
(defalias 'lispm-desktop-finder-trash-p #'lispm-finder-trash-p)
(defalias 'lispm-desktop-finder--throw-away #'lispm-finder--throw-away)
(defalias 'lispm-desktop-finder-move-to-trash #'lispm-finder-move-to-trash)
(defalias 'lispm-desktop-finder-rename #'lispm-finder-rename)
(defalias 'lispm-desktop-finder--select #'lispm-finder--select)
(defalias 'lispm-desktop-finder--step #'lispm-finder--step)
(defalias 'lispm-desktop-finder-select-right #'lispm-finder-select-right)
(defalias 'lispm-desktop-finder-select-left #'lispm-finder-select-left)
(defalias 'lispm-desktop-finder-select-down #'lispm-finder-select-down)
(defalias 'lispm-desktop-finder-select-up #'lispm-finder-select-up)
(defalias 'lispm-desktop-finder-type-select #'lispm-finder-type-select)
(defalias 'lispm-desktop-finder--item-at #'lispm-finder--item-at)
(defalias 'lispm-desktop-finder-mouse-select #'lispm-finder-mouse-select)
(defalias 'lispm-desktop-finder--drag #'lispm-finder--drag)
(defalias 'lispm-desktop-finder-mouse-open #'lispm-finder-mouse-open)
(defalias 'lispm-desktop-finder-deselect #'lispm-finder-deselect)
(defalias 'lispm-desktop-finder-context-menu #'lispm-finder-context-menu)
(defalias 'lispm-desktop-finder--item-position #'lispm-finder--item-position)
(defalias 'lispm-desktop-finder--keyboard-menu #'lispm-finder--keyboard-menu)
(defalias 'lispm-desktop-finder--menu #'lispm-finder--menu)

(defun lispm-desktop-finder--header ()
  "Return the Finder window's header strip."
  (lispm-finder--header))

(defun lispm-desktop-finder--press-up (event)
  "Hold the button EVENT pressed, going to the enclosing folder if let go on it."
  (interactive "e")
  (lispm-finder--press-up event))

(keymap-set lispm-finder--up-button-map "<header-line> <down-mouse-1>"
            #'lispm-desktop-finder--press-up)

(defun lispm-desktop-finder--path-items (window)
  "Return the menu of the shown folder's path."
  (lispm-finder--path-items window))

(defun lispm-desktop-finder--text-items (window props)
  "Insert the folder's items for WINDOW on a text terminal."
  (lispm-finder--text-items window props))

(defun lispm-desktop-finder--render (&rest args)
  "Draw the folder's items as rows of large icons over their names."
  (apply #'lispm-finder--render args))

(defun lispm-desktop-finder-make-alias ()
  "Make an alias of the application selected on the desktop."
  (interactive)
  (lispm-finder-make-alias))

(defun lispm-desktop-finder--edit (file)
  "Open FILE in the editor, brought in front."
  (lispm-desktop-editor-activate)
  (find-file file))

(defun lispm-desktop-finder--buffer ()
  "Return the Finder window's buffer, made when missing."
  (or (get-buffer lispm-finder-buffer-name)
      (with-current-buffer (get-buffer-create lispm-finder-buffer-name)
        (lispm-desktop-finder-mode)
        (current-buffer))))

(defmacro lispm-desktop-finder--in (&rest body)
  "Run BODY in the Finder window's buffer."
  (declare (indent 0))
  `(with-current-buffer (lispm-desktop-finder--buffer) ,@body))

;;;; Mode & Window Setup

(put 'lispm-finder-mode 'derived-mode-parent 'lispm-desktop-app-window-mode)

(keymap-set lispm-finder-mode-map "M-["
            (lispm-desktop-graphic-binding #'lispm-desktop-finder-back))
(keymap-set lispm-finder-mode-map "M-]"
            (lispm-desktop-graphic-binding #'lispm-desktop-finder-forward))
(keymap-set lispm-finder-mode-map "M-m" #'lispm-desktop-finder-make-alias)
(keymap-set lispm-finder-mode-map "M-w" #'lispm-desktop-finder-close)

(defun lispm-desktop-finder--configure-buffer ()
  "Configure a Finder buffer for the desktop's Platinum window chrome."
  (visual-line-mode -1)
  (setq-local auto-hscroll-mode nil)
  (unless lispm-finder--choosing
    (setq-local header-line-format '(:eval (lispm-desktop-finder--header))
                lispm-desktop-platinum-info ""
                mode-line-format lispm-desktop-platinum-mode-line-format
                lispm-desktop-platinum-title-menu-function #'lispm-desktop-finder--path-items
                lispm-desktop-context-menu-function #'lispm-desktop-finder--keyboard-menu
                lispm-desktop-platinum-box-functions
                (list :close #'lispm-desktop-app-window-close-box
                      :zoom #'lispm-desktop-finder--zoom-box
                      :collapse #'lispm-desktop-app-window-collapse-box))))

(define-derived-mode lispm-desktop-finder-mode lispm-finder-mode "Finder"
  "Major mode of the Finder window on the desktop.
\\{lispm-desktop-finder-mode-map}"
  (lispm-desktop-finder--configure-buffer)
  (run-hooks 'lispm-desktop-app-window-mode-hook)
  (add-hook 'lispm-desktop-tty-images-ready-hook #'lispm-desktop-finder--render))

(add-hook 'lispm-finder-mode-hook #'lispm-desktop-finder--configure-buffer)

(defun lispm-desktop-finder--on-rendered ()
  "Update Platinum window title and icon when Finder renders."
  (when (local-variable-if-set-p 'lispm-desktop-finder--directory)
    (dolist (pair '((lispm-desktop-finder--directory . lispm-finder--directory)
                    (lispm-desktop-finder--back . lispm-finder--back)
                    (lispm-desktop-finder--forward . lispm-finder--forward)
                    (lispm-desktop-finder--items . lispm-finder--items)
                    (lispm-desktop-finder--selected . lispm-finder--selected)
                    (lispm-desktop-finder--selected-app . lispm-finder--selected-app)
                    (lispm-desktop-finder--columns . lispm-finder--columns)
                    (lispm-desktop-finder--typed . lispm-finder--typed)))
      (set (car pair) (symbol-value (cdr pair)))))
  (setq lispm-desktop-platinum-window-title
        (if lispm-finder--choosing
            (or lispm-finder--choose-title (lispm-finder--title lispm-finder--directory))
          (lispm-desktop-finder--title lispm-finder--directory))
        lispm-desktop-platinum-window-icon
        (if-let* ((folder (lispm-desktop-finder--folder lispm-finder--directory)))
            (lispm-ui-icon (plist-get folder :icon))
          (lispm-desktop-platinum-folder-icon lispm-finder--directory))
        lispm-desktop-platinum-info ""))

;;;; The window

(defun lispm-desktop-finder--default-geometry (screen)
  "Return where the Finder window opens first: the left of SCREEN's desktop."
  (list 40 (+ (lispm-desktop-app-window-menu-bar-height screen) 30)
        (min 700 (round (* (frame-inner-width screen) 0.55)))
        (min 460 (round (* (frame-inner-height screen) 0.6)))))

(defun lispm-desktop-finder--closed ()
  "Forget the Finder window once it closed."
  (setq lispm-finder--had-focus nil)
  (when-let* ((buffer (get-buffer lispm-finder-buffer-name)))
    (kill-buffer buffer)))

(lispm-desktop-app-window-define 'finder
  :app 'desktop :name "Finder" :geometry #'lispm-desktop-finder--default-geometry
  :buffer #'lispm-desktop-finder--buffer :roll 'chrome :on-close #'lispm-desktop-finder--closed
  :raise nil :refocus-p (lambda () lispm-finder--had-focus))

(defun lispm-desktop-finder-frame ()
  "Return the Finder window while it is open."
  (lispm-desktop-app-window-frame 'finder))

(defun lispm-desktop-finder--show-window (_buffer)
  "Ensure and present the desktop's Finder window."
  (lispm-desktop-app-window-ensure 'finder)
  (lispm-desktop-app-window-present 'finder))

(defun lispm-desktop-finder-open-directory (directory)
  "Show DIRECTORY in the Finder window over the desktop, the desktop in front.
DIRECTORY is a directory's name, or a key of `lispm-desktop-finder-folders'."
  (interactive (list (read-directory-name (lispm-desktop-tr "Folder: ") nil default-directory 'lambda)))
  (lispm-desktop-app-window-ensure 'finder)
  (lispm-desktop-finder--go directory (lispm-desktop-finder--in (and lispm-finder--directory t)))
  (lispm-desktop-app-window-present 'finder)
  (setq lispm-finder--had-focus t)
  (lispm-desktop-finder--render))

(defun lispm-desktop-finder-home ()
  "Show the home folder in the Finder window, as the desktop's Home disk does."
  (interactive)
  (lispm-desktop-finder-open-directory "~"))

(defun lispm-desktop-finder-close ()
  "Close the Finder window."
  (interactive)
  (if (lispm-finder--in lispm-finder--choosing)
      (lispm-finder-choose-cancel)
    (lispm-desktop-app-window-close 'finder)))

(defun lispm-desktop-finder--zoom-box (_window)
  "Grow the Finder window over the desktop, or back, from its zoom box."
  (when-let* ((frame (lispm-desktop-finder-frame)))
    (let* ((screen (lispm-desktop-apps-window-screen frame))
           (zoomed (frame-parameter frame 'lispm-desktop-finder-unzoom)))
      (if zoomed
          (pcase-let ((`(,left ,top ,width ,height) zoomed))
            (set-frame-parameter frame 'lispm-desktop-finder-unzoom nil)
            (set-frame-parameter frame 'lispm-desktop-platinum-zoom nil)
            (lispm-desktop-platinum-move-window frame left top)
            (set-frame-size frame width height t))
        (pcase-let ((`(,left . ,top) (lispm-desktop-platinum-window-position frame)))
          (set-frame-parameter frame 'lispm-desktop-finder-unzoom
                               (list left top (frame-text-width frame) (frame-text-height frame)))
          (set-frame-parameter frame 'lispm-desktop-platinum-zoom t))
        (if (lispm-desktop-apps-out-p frame)
            (pcase-let ((`(,x ,y ,w ,h) (lispm-desktop-apps--workarea frame)))
              (lispm-desktop-platinum-move-window frame (+ x 12) (+ y 12))
              (set-frame-size frame (- w 40) (- h 70) t))
          (let ((top (+ (lispm-desktop-app-window-menu-bar-height screen) 12)))
            (lispm-desktop-platinum-move-window frame 12 top)
            (set-frame-size frame (- (frame-inner-width screen) 140)
                            (- (frame-inner-height screen) top 30) t)))))))

;;;; With the desktop

(defun lispm-desktop-finder--note-selected (&rest _)
  "Note that the Finder window has the keyboard once it is selected."
  (when-let* ((frame (lispm-desktop-finder-frame))
              ((eq (selected-frame) frame)))
    (setq lispm-finder--had-focus t)))

(defun lispm-desktop-finder-shown-p ()
  "Return non-nil while the Finder window is open."
  (lispm-desktop-app-window-open-p 'finder))

(defun lispm-desktop-finder--go-items ()
  "Return the items of the desktop's Go menu, moving the Finder window."
  (let ((open (lispm-desktop-finder-shown-p)))
    `((:label ,(lispm-desktop-tr "Back") :action lispm-desktop-finder-back
              :enabled ,(and open (lispm-desktop-finder--in lispm-finder--back) t))
      (:label ,(lispm-desktop-tr "Forward") :action lispm-desktop-finder-forward
              :enabled ,(and open (lispm-desktop-finder--in lispm-finder--forward) t))
      (:label ,(lispm-desktop-tr "Enclosing Folder") :action lispm-desktop-finder-up :enabled ,(and open t))
      separator
      (:label ,(lispm-desktop-tr "Home") :action lispm-desktop-finder-home))))

;; Retain catalog keys in `lispm-desktop' message catalogs for shared strings.
(defconst lispm-desktop-finder--i18n-catalog-keys
  (list (lispm-desktop-tr-noop "Go to the enclosing folder (Delete)")
        (lispm-desktop-tr-noop "Nothing can be made or renamed in “%s”")
        (lispm-desktop-tr-noop "Name of the new folder: ")
        (lispm-desktop-tr-noop "untitled folder")
        (lispm-desktop-tr-noop "There is already an item called “%s” here")
        (lispm-desktop-tr-noop "“%s” was moved to the Trash")
        (lispm-desktop-tr-noop "Select an item first")
        (lispm-desktop-tr-noop "“%s” cannot have an alias on the desktop")
        (lispm-desktop-tr-noop "Move to Trash")
        (lispm-desktop-tr-noop "“%s” could not be thrown away.")
        (lispm-desktop-tr-noop "OK")
        (lispm-desktop-tr-noop "“%s” cannot be thrown away")
        (lispm-desktop-tr-noop "Rename to: ")
        (lispm-desktop-tr-noop "Open")
        (lispm-desktop-tr-noop "New Folder…")
        (lispm-desktop-tr-noop "Rename…")
        (lispm-desktop-tr-noop "Make Alias on Desktop")
        (lispm-desktop-tr-noop "Copy Path"))
  "Catalog keys shared between `lispm-desktop' and `lispm-finder'.")

(defun lispm-desktop-finder--i18n-plural-keys (count available)
  "Retain plural catalog entries for `lispm-desktop' i18n checks."
  (list (lispm-desktop-tr-n "%d item, %s available" "%d items, %s available"
                            count count available)
        (lispm-desktop-tr-n "%d item" "%d items" count)))

;; Wire `lispm-finder' callbacks to `lispm-desktop'.
(setq lispm-finder-show-function #'lispm-desktop-finder--show-window
      lispm-finder-close-function #'lispm-desktop-finder-close
      lispm-finder-shown-function #'lispm-desktop-finder-shown-p
      lispm-finder-open-file-function #'lispm-desktop-finder--edit
      lispm-finder-drag-to-trash-function #'lispm-desktop-drag-to-trash
      lispm-finder-trash-changed-function #'lispm-desktop-check-trash
      lispm-finder-alias-possible-function #'lispm-desktop-alias-possible-p
      lispm-finder-make-alias-function #'lispm-desktop-make-alias
      lispm-finder-tty-p-function #'lispm-desktop-tty-p
      lispm-finder-tty-images-p-function #'lispm-desktop-tty-images-p
      lispm-finder-tty-images-show-function #'lispm-desktop-tty-images-show
      lispm-finder-text-items-function #'lispm-desktop-finder--text-items
      lispm-finder-render-function #'lispm-desktop-finder--render
      lispm-finder-press-function #'lispm-desktop-platinum-press
      lispm-finder-translate-function #'lispm-desktop-tr
      lispm-finder-frame-xy-function #'lispm-desktop-popup-frame-xy
      lispm-finder-menu-augment-function #'lispm-desktop-popup-augment-items
      lispm-finder-extra-item-properties '(lispm-desktop-finder-item)
      lispm-finder-extra-target-properties '(lispm-desktop-platinum-target))

(add-hook 'lispm-finder-rendered-hook #'lispm-desktop-finder--on-rendered)
(add-hook 'lispm-finder-language-functions #'lispm-desktop-current-language -50)

(defun lispm-desktop-setup-finder ()
  "Note when the desktop's Finder window has the keyboard, and draw it
again in another look, or another language."
  (add-hook 'window-selection-change-functions #'lispm-desktop-finder--note-selected)
  (add-hook 'lispm-desktop-look-changed-hook #'lispm-desktop-finder--render)
  (add-hook 'lispm-desktop-language-changed-hook #'lispm-desktop-finder--render)
  (add-hook 'lispm-desktop-language-changed-hook #'lispm-finder-language-changed))

(provide 'lispm-desktop-finder)
;;; lispm-desktop-finder.el ends here
