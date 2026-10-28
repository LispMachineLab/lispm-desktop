;;; lispm-desktop-browser-places.el --- The browser's History and Bookmarks pane  -*- lexical-binding: t; -*-

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

;; What the browser remembers, as Netscape Communicator remembered it: the
;; pages visited, its History, and the pages kept, its bookmarks.
;; lispm-browser-places.el keeps them; this file shows them.
;;
;; Both list in a pane at the browser window's left, a tab choosing one:
;; History by day, the most recent first, and bookmarks in the order they
;; were made.  A click opens a page in the shown tab, a middle click in a
;; new one; a right click offers the rest.  Bookmarks also fill the
;; Bookmarks menu, the location toolbar's Bookmarks button and the
;; personal toolbar.

;;; Code:

(require 'bookmark)
(require 'seq)
(require 'subr-x)
(require 'lispm-browser-places)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-browser)
(require 'lispm-desktop-i18n)

(declare-function lispm-desktop-popup-menu "lispm-desktop-popup" (items &optional x y align))
(declare-function lispm-desktop-sidebar-toolbar-button "lispm-desktop-sidebar" (label command help row &optional state))

;;;; The Bookmarks menu

(defun lispm-desktop-browser--bookmark-items ()
  "Return the items of the Bookmarks menu: Add Bookmark and Edit Bookmarks,
then a bookmark each, as Netscape's Bookmarks menu had them."
  (let ((bookmarks (lispm-browser-bookmarks)))
    `((:label ,(lispm-desktop-tr "Add Bookmark") :action lispm-browser-add-bookmark
              :enabled ,(and (lispm-browser-shown-buffer) t)
              :key ,(lispm-desktop-browser--key 'lispm-browser-add-bookmark))
      (:label ,(lispm-desktop-tr "Edit Bookmarks…") :action lispm-desktop-browser-edit-bookmarks
              :checked ,(eq (lispm-desktop-browser--places-shown) 'bookmarks)
              :key ,(lispm-desktop-browser--key 'lispm-desktop-browser-edit-bookmarks))
      separator
      ,@(if bookmarks
            (mapcar (pcase-lambda (`(,name . ,url))
                      (list :label (truncate-string-to-width name 48 nil nil "…")
                            :action (lambda () (lispm-browser-open-url url))))
                    bookmarks)
          `((:label ,(lispm-desktop-tr "No Bookmarks") :action ignore :enabled nil))))))

;;;; The personal toolbar

(defun lispm-desktop-browser--ribbon ()
  "Return the bookmark ribbon, one icon wide."
  (propertize " " 'display
              (lispm-desktop-browser--image
               '(ribbon)
               (lambda ()
                 (lispm-desktop-browser--svg
                  16 16
                  (format "<g transform='scale(0.66)'>%s</g>"
                          (format-spec (alist-get 'bookmarks lispm-desktop-browser--icons)
                                       (lispm-desktop-browser--ink t))))))))

(defun lispm-desktop-browser--personal-toolbar ()
  "Return the personal toolbar, the address window's mode line: a button
per bookmark, as Netscape's personal toolbar held them."
  (let ((bookmarks (lispm-browser-bookmarks)))
    (concat
     (propertize " " 'display '(space :width (8) :height (26) :ascent 70))
     (if bookmarks
         (mapconcat (pcase-lambda (`(,name . ,url))
                      (lispm-desktop-sidebar-toolbar-button
                       (concat " " (lispm-desktop-browser--ribbon) " "
                               (truncate-string-to-width name 22 nil nil "…") " ")
                       (lambda () (interactive) (lispm-browser-open-url url))
                       url 'mode-line 'lispm-ui-button))
                    bookmarks " ")
       (propertize (lispm-desktop-tr "Bookmarks you add show here.") 'face 'lispm-desktop-platinum-toolbar-label)))))

;;;; The pane

(defconst lispm-desktop-browser-places-buffer-name " *Browser Places*"
  "Name of the buffer of the browser's History and Bookmarks pane.")

(defconst lispm-desktop-browser-places-width 280
  "Width in pixels of the History and Bookmarks pane.")

(defvar-local lispm-desktop-browser--places-view 'bookmarks
  "What the pane lists: `bookmarks' or `history'.")

(defun lispm-desktop-browser--places-window ()
  "Return the pane's window in the browser, or nil."
  (get-buffer-window lispm-desktop-browser-places-buffer-name
                     (and (lispm-desktop-browser-floating-p) lispm-desktop-browser--frame)))

(defun lispm-desktop-browser--places-shown ()
  "Return what the pane lists while it is shown, `bookmarks' or `history'."
  (when-let* ((window (lispm-desktop-browser--places-window)))
    (buffer-local-value 'lispm-desktop-browser--places-view (window-buffer window))))

(defvar-keymap lispm-desktop-browser-places-tab-map
  :doc "Keymap of a tab of the pane."
  "<tab-line> <down-mouse-1>" #'ignore
  "<tab-line> <mouse-1>" #'lispm-desktop-browser-places-tab-click)

(defun lispm-desktop-browser-places-tab-click (event)
  "Show the list whose tab EVENT clicked."
  (interactive "e")
  (let ((position (event-start event)))
    (when-let* ((view (get-text-property (cdr (posn-string position)) 'lispm-desktop-browser-view
                                         (car (posn-string position)))))
      (with-current-buffer (window-buffer (posn-window position))
        (setq lispm-desktop-browser--places-view view)
        (lispm-desktop-browser--render-places)))))

(defun lispm-desktop-browser--places-tab (view label help)
  "Return the pane's tab for VIEW named LABEL, in front when shown, HELP
saying what it shows, as `lispm-desktop-platinum-tabs' takes it."
  (list label (eq view lispm-desktop-browser--places-view)
        'lispm-desktop-browser-view view 'keymap lispm-desktop-browser-places-tab-map
        'pointer 'arrow 'help-echo help))

(defun lispm-desktop-browser--places-tabs ()
  "Return the pane's tab line: a tab each for Bookmarks and History, and a
button closing the pane."
  (let ((close (lispm-desktop-sidebar-toolbar-button "✕" #'lispm-desktop-browser-hide-places
                                                      (lispm-desktop-tr "Close the pane") 'tab-line)))
    (concat (lispm-desktop-platinum-tabs
             (list (lispm-desktop-browser--places-tab 'bookmarks (lispm-desktop-tr "Bookmarks")
                                                      (lispm-desktop-tr "Show the Bookmarks"))
                   (lispm-desktop-browser--places-tab 'history (lispm-desktop-tr "History")
                                                      (lispm-desktop-tr "Show the History")))
             (- (window-pixel-width) (window-right-divider-width) (string-pixel-width close) 6))
            (propertize " " 'display '(space :width (3)))
            close)))

(defun lispm-desktop-browser--place-line (url text &rest properties)
  "Return a pane row for the page at URL showing TEXT, with PROPERTIES."
  (apply #'propertize (concat text "\n")
         'lispm-desktop-browser-url url 'mouse-face 'highlight 'help-echo url
         properties))

(defun lispm-desktop-browser--places-note (text)
  "Return TEXT, a remark in the pane where it lists nothing, dimmed and
broken between words, or the characters of Chinese and Japanese, into
indented lines as wide as the pane."
  (let ((width (- lispm-desktop-browser-places-width 24)))
    (propertize (concat "\n"
                        (mapconcat (lambda (line) (concat "  " line "\n"))
                                   (lispm-ui-text-wrap
                                    text (lambda (line)
                                           (<= (lispm-ui-text-width
                                                (concat "  " line) 'lispm-ui-list)
                                               width)))
                                   ""))
                'face 'shadow)))

(defun lispm-desktop-browser--render-places ()
  "List the bookmarks or the history in the pane's buffer."
  (when-let* ((buffer (get-buffer lispm-desktop-browser-places-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (line (line-number-at-pos)))
        (erase-buffer)
        (pcase lispm-desktop-browser--places-view
          ('bookmarks
           (let ((bookmarks (lispm-browser-bookmarks)))
             (dolist (bookmark bookmarks)
               (insert (lispm-desktop-browser--place-line (cdr bookmark)
                                                          (concat " " (lispm-desktop-browser--ribbon) " " (car bookmark))
                                                          'lispm-desktop-browser-bookmark (car bookmark))))
             (unless bookmarks
               (insert (lispm-desktop-browser--places-note
                        (lispm-desktop-tr "No bookmarks yet: %s keeps the page shown."
                                          (concat (lispm-desktop-tr "Bookmarks") " › "
                                                  (lispm-desktop-tr "Add Bookmark"))))))
             (setq lispm-desktop-platinum-info
                   (lispm-desktop-tr-n "%d bookmark" "%d bookmarks" (length bookmarks)))))
          (_
           (let ((history (lispm-browser-history))
                 (heading nil))
             (dolist (entry history)
               (pcase-let ((`(,url ,title ,time) entry))
                 (let ((day (lispm-browser-history-day time)))
                   (unless (equal day heading)
                     (setq heading day)
                     (insert (propertize (concat " " day "\n") 'face 'bold)))
                   (insert (lispm-desktop-browser--place-line
                            url (concat "   " (or title url)
                                        (propertize (concat "  " (lispm-desktop-format-time "%H:%M" time))
                                                    'face 'shadow)))))))
             (unless history
               (insert (lispm-desktop-browser--places-note (lispm-desktop-tr "No pages visited yet."))))
             (setq lispm-desktop-platinum-info
                   (lispm-desktop-tr-n "%d page" "%d pages" (length history))))))
        (goto-char (point-min))
        (forward-line (1- line))
        (force-mode-line-update)))))

(defun lispm-desktop-browser--places-changed ()
  "Redraw what lists the bookmarks or the history: the pane, the personal
toolbar."
  (lispm-desktop-browser--render-places)
  (when-let* ((buffer (get-buffer lispm-desktop-browser-address-buffer-name)))
    (with-current-buffer buffer
      (force-mode-line-update))))

(defun lispm-desktop-browser--place-at (&optional position)
  "Return the address of the pane row at POSITION, by default point."
  (get-text-property (or position (point)) 'lispm-desktop-browser-url))

(defun lispm-desktop-browser-places-open (&optional new-tab)
  "Open the page of the row at point, in a new tab when NEW-TAB."
  (interactive "P")
  (when-let* ((url (lispm-desktop-browser--place-at)))
    (lispm-browser-open-url url new-tab)))

(defun lispm-desktop-browser-places-open-new-tab ()
  "Open the page of the row at point in a new tab."
  (interactive)
  (lispm-desktop-browser-places-open t))

(defun lispm-desktop-browser-places-click (event)
  "Open the page of the row EVENT clicked; a middle click opens a new tab."
  (interactive "e")
  (let ((position (event-start event)))
    (when-let* ((point (posn-point position)))
      (with-selected-window (posn-window position)
        (goto-char point)
        (lispm-desktop-browser-places-open (eq (event-basic-type event) 'mouse-2))))))

(defun lispm-desktop-browser-places-delete ()
  "Delete the bookmark, or forget the page, of the row at point."
  (interactive)
  (when-let* ((url (lispm-desktop-browser--place-at)))
    (if-let* ((name (get-text-property (point) 'lispm-desktop-browser-bookmark)))
        (lispm-browser-delete-bookmark name)
      (lispm-browser-history-remove url))))

(defun lispm-desktop-browser-places-rename ()
  "Rename the bookmark of the row at point."
  (interactive)
  (if-let* ((name (get-text-property (point) 'lispm-desktop-browser-bookmark)))
      (lispm-browser-rename-bookmark name)
    (user-error (lispm-desktop-tr "Only a bookmark can be renamed"))))

(defun lispm-desktop-browser-places-context-menu (event)
  "Pop up what can be done with the row EVENT clicked."
  (interactive "e")
  (let ((position (event-start event)))
    (with-selected-window (posn-window position)
      (when (posn-point position)
        (goto-char (posn-point position)))
      (when-let* ((url (lispm-desktop-browser--place-at)))
        (let ((bookmark (get-text-property (point) 'lispm-desktop-browser-bookmark)))
          (lispm-desktop-popup-menu
           `((:label ,(lispm-desktop-tr "Open") :action lispm-desktop-browser-places-open)
             (:label ,(lispm-desktop-tr "Open in New Tab") :action lispm-desktop-browser-places-open-new-tab)
             separator
             (:label ,(lispm-desktop-tr "Copy Address")
                     :action ,(lambda () (kill-new url) (message (lispm-desktop-tr "Copied %s") url)))
             ,@(if bookmark
                   `((:label ,(lispm-desktop-tr "Rename…") :action lispm-desktop-browser-places-rename)
                     (:label ,(lispm-desktop-tr "Delete Bookmark") :action lispm-desktop-browser-places-delete))
                 `((:label ,(lispm-desktop-tr "Forget Page") :action lispm-desktop-browser-places-delete)
                   separator
                   (:label ,(lispm-desktop-tr "Clear History…") :action lispm-browser-clear-history))))))))))

(defun lispm-desktop-browser-places-switch ()
  "Show the other list, bookmarks or history."
  (interactive)
  (setq lispm-desktop-browser--places-view (if (eq lispm-desktop-browser--places-view 'bookmarks) 'history 'bookmarks))
  (lispm-desktop-browser--render-places))

(defvar-keymap lispm-desktop-browser-places-mode-map
  :doc "Keymap of the browser's History and Bookmarks pane."
  "RET" #'lispm-desktop-browser-places-open
  "M-RET" #'lispm-desktop-browser-places-open-new-tab
  "<mouse-1>" #'lispm-desktop-browser-places-click
  "<mouse-2>" #'lispm-desktop-browser-places-click
  "<mouse-3>" #'lispm-desktop-browser-places-context-menu
  "d" #'lispm-desktop-browser-places-delete
  "DEL" #'lispm-desktop-browser-places-delete
  "<delete>" #'lispm-desktop-browser-places-delete
  "r" #'lispm-desktop-browser-places-rename
  "g" #'lispm-desktop-browser--render-places
  "TAB" #'lispm-desktop-browser-places-switch
  "n" #'next-line
  "p" #'previous-line
  "q" #'lispm-desktop-browser-hide-places)

(define-derived-mode lispm-desktop-browser-places-mode special-mode "Places"
  "Major mode of the browser's History and Bookmarks pane.
\\{lispm-desktop-browser-places-mode-map}"
  (setq-local tab-line-format '(:eval (lispm-desktop-browser--places-tabs))
              ;; A window growing gives its width to the page, not the pane.
              window-size-fixed 'width
              header-line-format nil
              truncate-lines t
              cursor-type nil
              lispm-desktop-platinum-window-title (lambda () (lispm-desktop-tr "Bookmarks")))
  (visual-line-mode -1)
  (setq-local truncate-lines t)
  (face-remap-add-relative 'default 'lispm-ui-list)
  (hl-line-mode 1)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1)))

(defun lispm-desktop-browser--places-buffer ()
  "Return the pane's buffer, made when missing."
  (or (get-buffer lispm-desktop-browser-places-buffer-name)
      (with-current-buffer (get-buffer-create lispm-desktop-browser-places-buffer-name)
        (lispm-desktop-browser-places-mode)
        (current-buffer))))

(defun lispm-desktop-browser--show-places (view)
  "Show the pane listing VIEW, `bookmarks' or `history', at the browser
window's left, the browser shown first when it is not.  Asked for from
the keyboard, the pane takes the keyboard, so its keys work at once."
  (unless (lispm-desktop-browser--window)
    (lispm-desktop-browser-show))
  (with-current-buffer (lispm-desktop-browser--places-buffer)
    (setq lispm-desktop-browser--places-view view)
    (lispm-desktop-browser--render-places))
  (unless (lispm-desktop-browser--places-window)
    (when-let* ((page (lispm-desktop-browser--window)))
      (let ((window (split-window page (- lispm-desktop-browser-places-width) 'left t)))
        (set-window-buffer window (lispm-desktop-browser--places-buffer))
        (set-window-dedicated-p window t)
        (set-window-parameter window 'lispm-desktop-chrome t)
        (set-window-parameter window 'no-other-window t))))
  (when (and (lispm-desktop-typed-event-p last-input-event) (lispm-desktop-browser--places-window))
    (select-window (lispm-desktop-browser--places-window))))

(defun lispm-desktop-browser-hide-places ()
  "Close the History and Bookmarks pane; the page takes the keyboard back."
  (interactive)
  (when-let* ((window (lispm-desktop-browser--places-window)))
    (let ((selected (eq window (selected-window))))
      (delete-window window)
      (when (and selected (lispm-desktop-browser--window))
        (select-window (lispm-desktop-browser--window))))))

(defun lispm-desktop-browser-show-history ()
  "Show the pages visited in the pane, or close the pane when it shows them."
  (interactive)
  (if (eq (lispm-desktop-browser--places-shown) 'history)
      (lispm-desktop-browser-hide-places)
    (lispm-desktop-browser--show-places 'history)))

(defun lispm-desktop-browser-edit-bookmarks ()
  "Show the bookmarks in the pane, where they are opened, renamed and
deleted, or close the pane when it shows them."
  (interactive)
  (if (eq (lispm-desktop-browser--places-shown) 'bookmarks)
      (lispm-desktop-browser-hide-places)
    (lispm-desktop-browser--show-places 'bookmarks)))

(defun lispm-desktop-setup-browser-places ()
  "Redraw the pane and the personal toolbar as the history and the
bookmarks change, and as the language spoken does."
  (add-hook 'lispm-browser-places-changed-hook #'lispm-desktop-browser--places-changed)
  (add-hook 'lispm-desktop-language-changed-hook #'lispm-desktop-browser--places-changed))

(provide 'lispm-desktop-browser-places)
;;; lispm-desktop-browser-places.el ends here
