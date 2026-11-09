;;; lispm-desktop-app-manager.el --- Installed Apps control panel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-desktop
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Apps is a control panel of its own, not the App Store's catalog.  It
;; lists what is installed even when a catalog no longer lists it or its
;; library failed to load.  The Store still owns installation, update,
;; confirmation and removal; this window is another view of that state.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-store)
(require 'lispm-desktop-i18n)

(declare-function lispm-desktop-settings-show "lispm-desktop-settings" (&optional panel))
(declare-function lispm-desktop-settings--buffer-name "lispm-desktop-settings" (category))
(declare-function lispm-desktop-settings--fill "lispm-desktop-settings" (buffer))
(declare-function lispm-desktop-settings--heading "lispm-desktop-settings" (title))
(declare-function lispm-desktop-settings--paragraph "lispm-desktop-settings" (text))
(declare-function lispm-desktop-settings--note "lispm-desktop-settings" (text))
(declare-function lispm-desktop-settings--indent "lispm-desktop-settings" ())
(declare-function lispm-desktop-settings--check-box "lispm-desktop-settings" (label on action &optional note))
(declare-function lispm-desktop-settings--save "lispm-desktop-settings" (variable value))

(defvar lispm-desktop-app-manager-field-map)

(defvar-local lispm-desktop-app-manager--selected nil
  "ID of the selected installed item.")
(defvar-local lispm-desktop-app-manager--search ""
  "Words to match against installed items, without case.")
(defvar-local lispm-desktop-app-manager--filter 'all
  "Kind of installed item to show: all, app or extension.")
(defvar-local lispm-desktop-app-manager--page 0
  "Zero-based page of installed items shown.")
(defvar-local lispm-desktop-app-manager--field nil
  "The overlay drawing the search field, once the panel is drawn.")
(defvar-local lispm-desktop-app-manager--field-start nil
  "Marker where the search field's text starts.")
(defvar-local lispm-desktop-app-manager--field-end nil
  "Marker where the search field's text ends, moving as it is typed in.")
(defvar-local lispm-desktop-app-manager--field-focused nil
  "Non-nil while the search field has the keyboard.")
(defvar-local lispm-desktop-app-manager--field-point 0
  "How far into the search field point was, to put it back after a redraw.")
(defvar-local lispm-desktop-app-manager--search-timer nil
  "Timer showing what the search field holds matches once typing pauses.")
(defvar-local lispm-desktop-app-manager--set-up nil
  "Non-nil once the Apps buffer's search field hooks are in place.")
(defconst lispm-desktop-app-manager-search-delay 0.15
  "Seconds typing in the search field pauses before the list shows the
matches.")
(defconst lispm-desktop-app-manager--field-width 150
  "Pixels wide the Apps search field is.")
(defconst lispm-desktop-app-manager--page-room 13
  "Lines of the Apps window kept from the list for its other parts: the
heading, the search field and buttons above, the page number and the
selected item's details.")
(defconst lispm-desktop-app-manager--page-minimum 5
  "Least installed items the Apps window shows at once, however short.")
(defconst lispm-desktop-app-manager--page-maximum 24
  "Most installed items the Apps window shows at once, however tall.")

(defun lispm-desktop-app-manager--page-size ()
  "Return how many installed items the Apps window shows at once.
The window's height decides: the list takes its lines less those kept for
the rest of the panel, see `lispm-desktop-app-manager--page-room'."
  (let* ((window (get-buffer-window (current-buffer) t))
         (height (if window (window-body-height window) 24))
         (room lispm-desktop-app-manager--page-room))
    (max lispm-desktop-app-manager--page-minimum
         (min lispm-desktop-app-manager--page-maximum (- height room)))))

(defun lispm-desktop-app-manager-items ()
  "Return installed applications and extensions as (ID . PROPERTIES).
Include only items the App Store installed, even when a catalog no
longer lists them or their library failed to load."
  (mapcar (pcase-lambda (`(,id . ,props))
            (cons id (append (list :managed t) props)))
          (lispm-desktop-store-installed-items)))

(defun lispm-desktop-app-manager-update-items ()
  "Return installed applications and extensions with updates available
as (ID . PROPERTIES)."
  (let ((updates (lispm-desktop-store-updates)))
    (seq-filter (pcase-lambda (`(,id . ,_))
                  (or (memq id updates)
                      (plist-get (lispm-desktop-store-installed-info id) :update-available)))
                (lispm-desktop-app-manager-items))))

(defun lispm-desktop-app-manager--matches ()
  "Return installed items matching this buffer's search and filter."
  (let ((words (split-string (downcase lispm-desktop-app-manager--search) nil t)))
    (seq-filter
     (pcase-lambda (`(,id . ,props))
       (and (pcase lispm-desktop-app-manager--filter
              ('all t)
              (kind (eq kind (plist-get props :kind))))
            (let ((text (downcase (format "%s %s %s" id (plist-get props :name)
                                           (or (plist-get props :description) "")))))
              (seq-every-p (lambda (word) (string-search word text)) words))))
     (lispm-desktop-app-manager-items))))

(defun lispm-desktop-app-manager--field-bounds ()
  "Return where the search field's text is, (START . END), or nil."
  (when (and (markerp lispm-desktop-app-manager--field-start)
             (marker-position lispm-desktop-app-manager--field-start)
             (markerp lispm-desktop-app-manager--field-end)
             (marker-position lispm-desktop-app-manager--field-end))
    (cons lispm-desktop-app-manager--field-start
          lispm-desktop-app-manager--field-end)))

(defun lispm-desktop-app-manager--in-field-p (&optional position)
  "Return non-nil when POSITION, by default point, is in the search field."
  (when-let* ((bounds (lispm-desktop-app-manager--field-bounds)))
    (<= (car bounds) (or position (point)) (cdr bounds))))

(defun lispm-desktop-app-manager--field-focused-p ()
  "Return non-nil while the search field has the keyboard: point in it, in
the window selected."
  (and (eq (window-buffer) (current-buffer))
       (lispm-desktop-app-manager--in-field-p (window-point))))

(defun lispm-desktop-app-manager--insert-field ()
  "Insert the search field's text, the search, at point and mark its bounds.
The field is dressed once the panel is drawn, see
`lispm-desktop-app-manager--after-fill'."
  (let ((start (point)))
    (insert lispm-desktop-app-manager--search)
    ;; The end stays where the text ends while the panel is drawn around
    ;; it; once drawn, typing at its end moves it on.
    (setq lispm-desktop-app-manager--field-start (copy-marker start)
          lispm-desktop-app-manager--field-end (copy-marker (point)))))

(defun lispm-desktop-app-manager--protect (start end)
  "Make the panel read-only but for the search field from START to END, and
put the field's overlay over it.  Text typed in the field takes none of the
properties of the text around it."
  (let ((inhibit-read-only t))
    (add-text-properties (point-min) start '(read-only t))
    (add-text-properties end (point-max) '(read-only t))
    ;; Nothing goes before the panel's first line either.
    (put-text-property (point-min) (1+ (point-min)) 'front-sticky '(read-only))
    (when (> start (point-min))
      (put-text-property (1- start) start 'rear-nonsticky t))
    (unless (overlayp lispm-desktop-app-manager--field)
      (setq lispm-desktop-app-manager--field (make-overlay start end nil nil t)))
    (move-overlay lispm-desktop-app-manager--field start end)
    (overlay-put lispm-desktop-app-manager--field 'keymap
                 lispm-desktop-app-manager-field-map)
    (overlay-put lispm-desktop-app-manager--field 'pointer 'text)
    (overlay-put lispm-desktop-app-manager--field 'field
                 'lispm-desktop-app-manager--search)))

(defun lispm-desktop-app-manager--field-pad (text &optional face)
  "Return the blank after TEXT that keeps the search field one width.
The field's text and this blank together are as wide as
`lispm-desktop-app-manager--field-width', and a gap follows them.  FACE
draws the blank."
  (if (lispm-desktop-tty-p)
      (propertize (make-string (max 1 (- 24 (string-width text))) ?\s)
                  'face face)
    (propertize " " 'face face 'display
                `(space :width (,(max 12 (+ (lispm-desktop-px 8)
                                            (- (lispm-desktop-px lispm-desktop-app-manager--field-width)
                                               (string-pixel-width text)))))))))

(defun lispm-desktop-app-manager--dress-field ()
  "Draw the search field as Mac OS 9's edit text field: framed, ringed
while it has the keyboard, and the cursor in it then; empty, it reads
Search, dimmed."
  (when-let* ((field lispm-desktop-app-manager--field)
              ((overlay-buffer field)))
    (let* ((focused (lispm-desktop-app-manager--field-focused-p))
           (face (if focused 'lispm-ui-field-focused 'lispm-ui-field))
           (empty (= (overlay-start field) (overlay-end field)))
           (text (buffer-substring-no-properties (overlay-start field) (overlay-end field))))
      (setq lispm-desktop-app-manager--field-focused focused)
      (overlay-put field 'face face)
      (overlay-put field 'help-echo
                   (lispm-desktop-tr "Search the installed applications"))
      (overlay-put field 'before-string
                   (propertize " " 'face face 'display
                               `(space :width (,(lispm-desktop-px 4)))))
      (overlay-put field 'after-string
                   (concat (if empty
                               (propertize (lispm-desktop-tr "Search") 'cursor t
                                           'face `(:inherit ,face :foreground
                                                            ,(lispm-ui-grey
                                                              lispm-ui-current-palette #x88)))
                             (propertize " " 'face face 'cursor t))
                           (lispm-desktop-app-manager--field-pad text face)))
      (setq-local cursor-type (and focused '(bar . 1))
                  evil-emacs-state-cursor (list (and focused '(bar . 1)))
                  evil-normal-state-cursor (list (and focused '(bar . 1)))))))

(defun lispm-desktop-app-manager--follow-focus (&rest _)
  "Note whether the search field has the keyboard, drawing it so or not.
This is a buffer-local `post-command-hook' and
`window-selection-change-functions' function."
  (when-let* ((bounds (lispm-desktop-app-manager--field-bounds)))
    (let ((focused (lispm-desktop-app-manager--field-focused-p)))
      (when focused
        (setq lispm-desktop-app-manager--field-point
              (max 0 (min (- (window-point) (car bounds))
                          (- (cdr bounds) (car bounds))))))
      (unless (eq focused lispm-desktop-app-manager--field-focused)
        (setq lispm-desktop-app-manager--field-focused focused)
        (lispm-desktop-app-manager--dress-field)))))

(defun lispm-desktop-app-manager--after-fill ()
  "Dress the search field once the panel is drawn and framed, and put point
back in it while it had the keyboard.  This is one of
`lispm-desktop-settings-after-fill-functions'."
  (when (lispm-desktop-app-manager--field-bounds)
    (let* ((bounds (lispm-desktop-app-manager--field-bounds))
           (start (car bounds))
           (end (cdr bounds))
           (focused lispm-desktop-app-manager--field-focused)
           (goal (and focused
                      (max start
                           (min end (+ start lispm-desktop-app-manager--field-point))))))
      (lispm-desktop-app-manager--protect start end)
      ;; The panel is drawn around the field now, so from here on typing
      ;; at the field's end moves the end along with the text.
      (set-marker-insertion-type lispm-desktop-app-manager--field-end t)
      ;; Back in the field before it is drawn, so that its ring stays.
      (when goal
        (goto-char goal)
        (when-let* ((window (get-buffer-window (current-buffer) t)))
          (set-window-point window goal)))
      (lispm-desktop-app-manager--dress-field))))

(defun lispm-desktop-app-manager--composing-p ()
  "Return non-nil while an input method composes text in the current buffer,
the text it shows there not yet typed."
  (seq-some (lambda (symbol)
              (and (boundp symbol)
                   (let ((overlay (symbol-value symbol)))
                     (and (overlayp overlay) (eq (overlay-buffer overlay) (current-buffer))
                          (< (overlay-start overlay) (overlay-end overlay))))))
            '(ns-working-overlay quail-overlay)))

(defun lispm-desktop-app-manager--field-changed (start end _length)
  "Take the search field's text as the search once it changed; the list
shows what it matches as typing pauses.  This is a buffer-local
`after-change-functions' function."
  (when-let* ((bounds (lispm-desktop-app-manager--field-bounds))
              ((<= (car bounds) start end (cdr bounds))))
    (setq lispm-desktop-app-manager--search
          (replace-regexp-in-string
           "[\n\r]" " "
           (buffer-substring-no-properties (car bounds) (cdr bounds)))
          lispm-desktop-app-manager--selected nil
          lispm-desktop-app-manager--page 0)
    (lispm-desktop-app-manager--dress-field)
    (lispm-desktop-app-manager--search-soon)))

(defun lispm-desktop-app-manager--yank-text (text)
  "Return TEXT, about to be yanked here, as one line for the search field.
The field is a single panel row, so the line breaks and tabs of a paste
become blanks rather than drawing the panel apart."
  (replace-regexp-in-string "[\n\r\t]+" " " text))

(defun lispm-desktop-app-manager--search-soon ()
  "Show what the search field holds matches once typing pauses."
  (when (timerp lispm-desktop-app-manager--search-timer)
    (cancel-timer lispm-desktop-app-manager--search-timer))
  (let ((buffer (current-buffer)))
    (setq lispm-desktop-app-manager--search-timer
          (run-with-timer lispm-desktop-app-manager-search-delay nil
                          (lambda ()
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (if (lispm-desktop-app-manager--composing-p)
                                    (lispm-desktop-app-manager--search-soon)
                                  (setq lispm-desktop-app-manager--search-timer nil)
                                  (lispm-desktop-app-manager-refresh)))))))))

(defun lispm-desktop-app-manager--leave-field ()
  "Take the keyboard from the search field for the list."
  (when (lispm-desktop-app-manager--in-field-p)
    (goto-char (point-min))
    (deactivate-mark))
  (when-let* ((window (get-buffer-window (current-buffer) t))
              ((lispm-desktop-app-manager--in-field-p (window-point window))))
    (set-window-point window (point-min))))

(defun lispm-desktop-app-manager--replace-marked ()
  "Delete the text marked in the search field before a command that types
or pastes there, as `delete-selection-mode' does, from the buffer's
`pre-command-hook': the desktop leaves that mode off, and the field
behaves so all the same."
  (when (and (not delete-selection-mode) (use-region-p)
             (lispm-desktop-app-manager--in-field-p (region-beginning))
             (lispm-desktop-app-manager--in-field-p (region-end)))
    (let ((delete-selection-mode t))
      (delete-selection-pre-hook))))

(defun lispm-desktop-app-manager-search-done ()
  "Leave the search field for the list, which shows the matches at once."
  (interactive)
  (when (timerp lispm-desktop-app-manager--search-timer)
    (cancel-timer lispm-desktop-app-manager--search-timer)
    (setq lispm-desktop-app-manager--search-timer nil)
    (lispm-desktop-app-manager-refresh))
  (lispm-desktop-app-manager--leave-field)
  (lispm-desktop-app-manager--dress-field))

(defun lispm-desktop-app-manager-clear-search ()
  "Empty the search field, showing every installed item again."
  (interactive)
  (setq lispm-desktop-app-manager--search ""
        lispm-desktop-app-manager--selected nil
        lispm-desktop-app-manager--page 0)
  (lispm-desktop-app-manager-refresh))

(defun lispm-desktop-app-manager-search-cancel ()
  "Empty the search field, showing every installed item again; empty
already, leave it for the list."
  (interactive)
  (if (string-empty-p lispm-desktop-app-manager--search)
      (progn (lispm-desktop-app-manager--leave-field)
             (lispm-desktop-app-manager--dress-field))
    (lispm-desktop-app-manager-clear-search)))

(defun lispm-desktop-app-manager-field-forward ()
  "Move point a character on in the search field, never past its end."
  (interactive)
  (when (< (point) (cdr (lispm-desktop-app-manager--field-bounds)))
    (forward-char 1)))

(defun lispm-desktop-app-manager-field-backward ()
  "Move point a character back in the search field, never before its start."
  (interactive)
  (when (> (point) (car (lispm-desktop-app-manager--field-bounds)))
    (backward-char 1)))

(defvar lispm-desktop-app-manager-field-map
  (let ((map (define-keymap :full t
               "RET" #'lispm-desktop-app-manager-search-done
               "C-n" #'lispm-desktop-app-manager-search-done
               "<down>" #'lispm-desktop-app-manager-search-done
               "C-g" #'lispm-desktop-app-manager-search-cancel
               "DEL" #'delete-backward-char
               "C-f" #'lispm-desktop-app-manager-field-forward
               "C-b" #'lispm-desktop-app-manager-field-backward
               "<right>" #'lispm-desktop-app-manager-field-forward
               "<left>" #'lispm-desktop-app-manager-field-backward)))
    ;; What is typed goes in the field, the window's letter keys aside.
    (set-char-table-range (nth 1 map) '(32 . 126) #'self-insert-command)
    (lispm-desktop-bind-clipboard-keys map)
    map)
  "Keymap of the Apps search field: what is typed goes in it, and Emacs's
keys edit it, the window's own letter keys left aside.  Return, C-n and
Tab leave it for the list; C-g empties it, then leaves it.")

(defun lispm-desktop-app-manager-search-field ()
  "Put the keyboard in the Apps search field, its text marked."
  (interactive)
  (when-let* ((bounds (lispm-desktop-app-manager--field-bounds)))
    (goto-char (cdr bounds))
    (when-let* ((window (get-buffer-window (current-buffer) t)))
      (set-window-point window (point)))
    (when (< (car bounds) (cdr bounds))
      (push-mark (car bounds) t t))
    (lispm-desktop-app-manager--follow-focus)
    (lispm-desktop-app-manager--dress-field)))

(defun lispm-desktop-app-manager--set-up ()
  "Arrange the Apps buffer's search field, once."
  (unless lispm-desktop-app-manager--set-up
    (setq lispm-desktop-app-manager--set-up t)
    (add-hook 'after-change-functions #'lispm-desktop-app-manager--field-changed nil t)
    (add-hook 'yank-transform-functions #'lispm-desktop-app-manager--yank-text nil t)
    (add-hook 'pre-command-hook #'lispm-desktop-app-manager--replace-marked nil t)
    (add-hook 'post-command-hook #'lispm-desktop-app-manager--follow-focus nil t)
    (add-hook 'window-selection-change-functions
              #'lispm-desktop-app-manager--follow-focus nil t)
    (add-hook 'lispm-desktop-settings-after-fill-functions
              #'lispm-desktop-app-manager--after-fill nil t)
    (local-set-key (kbd "/") #'lispm-desktop-app-manager-search-field)
    (local-set-key (kbd "C-s") #'lispm-desktop-app-manager-search-field)))

(defun lispm-desktop-app-manager-refresh (&rest _)
  "Refresh the existing Apps buffer without opening a buffer or window."
  (when (fboundp 'lispm-desktop-settings--buffer-name)
    (when-let* ((buffer (get-buffer (lispm-desktop-settings--buffer-name 'settings-apps))))
      (lispm-desktop-settings--fill buffer))))

(defun lispm-desktop-app-manager-search (text)
  "Show installed items matching TEXT, the search field's text.
An empty TEXT clears the search.  For the keyboard, see
`lispm-desktop-app-manager-search-field'."
  (setq lispm-desktop-app-manager--search text
        lispm-desktop-app-manager--selected nil
        lispm-desktop-app-manager--page 0)
  (lispm-desktop-app-manager-refresh))

(defun lispm-desktop-app-manager--choose (id)
  "Select the installed item ID and refresh its details."
  (setq lispm-desktop-app-manager--selected id)
  (lispm-desktop-app-manager-refresh))

(defun lispm-desktop-app-manager--turn (step)
  "Move STEP pages through installed items."
  (setq lispm-desktop-app-manager--page (+ lispm-desktop-app-manager--page step)
        lispm-desktop-app-manager--selected nil)
  (lispm-desktop-app-manager-refresh))

(defun lispm-desktop-app-manager--clear-field ()
  "Clear the search field markers and overlay when the Updates tab is drawn."
  (setq lispm-desktop-app-manager--field-start nil
        lispm-desktop-app-manager--field-end nil
        lispm-desktop-app-manager--field-focused nil)
  (when (overlayp lispm-desktop-app-manager--field)
    (delete-overlay lispm-desktop-app-manager--field))
  (setq-local cursor-type nil
              evil-emacs-state-cursor '(nil)
              evil-normal-state-cursor '(nil)))

(defun lispm-desktop-app-manager-update-item (id)
  "Update the installed item ID."
  (let ((info (and (assq id (lispm-desktop-app-manager-items))
                   (lispm-desktop-store-installed-info id))))
    (unless (and info (not (plist-get info :busy))
                 (or (plist-get info :update-available)
                     (memq id (lispm-desktop-store-updates))))
      (user-error (lispm-desktop-tr "No update is available")))
    (lispm-desktop-store-update id)
    (lispm-desktop-app-manager-refresh)))

(defun lispm-desktop-app-manager-update-all ()
  "Update every installed item that has an update available."
  (interactive)
  (let ((ids (seq-filter (lambda (id)
                           (let ((info (lispm-desktop-store-installed-info id)))
                             (and info (not (plist-get info :busy)))))
                         (mapcar #'car (lispm-desktop-app-manager-update-items)))))
    (unless ids
      (user-error (lispm-desktop-tr "No update is available")))
    (dolist (id ids)
      (lispm-desktop-store-update id))
    (lispm-desktop-app-manager-refresh)))

(defun lispm-desktop-app-manager-action (action)
  "Perform ACTION, open, update or uninstall, on the selected item.
Recheck current state before acting.  Store actions always ask for their
normal confirmation; busy items cannot be updated or uninstalled."
  (let* ((id lispm-desktop-app-manager--selected)
         (props (cdr (assq id (lispm-desktop-app-manager-items))))
         (info (and (plist-get props :managed) (lispm-desktop-store-installed-info id))))
    (unless props (user-error (lispm-desktop-tr "Select an application first")))
    (pcase action
      ('open (unless (and (eq (plist-get props :kind) 'app)
                          (lispm-desktop-apps-openable-p id))
               (user-error (lispm-desktop-tr "This application cannot be opened")))
             (lispm-desktop-apps-activate id)
             (lispm-desktop-app-manager-refresh))
      ('update (lispm-desktop-app-manager-update-item id))
      ('uninstall (unless (and info (not (plist-get info :busy)))
                    (user-error (lispm-desktop-tr "This application cannot be uninstalled")))
                  (lispm-desktop-store-uninstall id)
                  (lispm-desktop-app-manager-refresh))
      (_ (error "Unknown Apps action: %s" action)))))

(defun lispm-desktop-app-manager--button (label action &optional disabled)
  "Insert a push button with LABEL and ACTION, optionally DISABLED."
  (lispm-ui-insert-push-button label action :disabled disabled))

(defun lispm-desktop-app-manager--details (id props)
  "Insert details and management actions for installed item ID of PROPS."
  (let* ((info (lispm-desktop-store-installed-info id))
         (busy (plist-get info :busy)))
    (lispm-desktop-settings--heading (plist-get props :name))
    (lispm-desktop-settings--note
     (lispm-desktop-tr "%s · Version: %s"
                       (if (eq (plist-get props :kind) 'extension)
                           (lispm-desktop-tr "Extension") (lispm-desktop-tr "Application"))
                       (plist-get info :version)))
    (when info
      (lispm-desktop-settings--paragraph (lispm-desktop-tr "Source: %s" (plist-get info :origin))))
    (when-let* ((description (plist-get props :description)))
      (lispm-desktop-settings--paragraph description))
    (when busy (lispm-desktop-settings--note (lispm-desktop-tr "Updating…")))
    (insert (lispm-desktop-settings--indent))
    (lispm-desktop-app-manager--button
     (lispm-desktop-tr "Open") (lambda () (lispm-desktop-app-manager-action 'open))
     (not (and (eq (plist-get props :kind) 'app) (lispm-desktop-apps-openable-p id))))
    (insert " ")
    (lispm-desktop-app-manager--button
     (lispm-desktop-tr "Uninstall") (lambda () (lispm-desktop-app-manager-action 'uninstall))
     (or (not info) busy))
    (insert "\n")))

(defun lispm-desktop-app-manager-insert ()
  "Insert the Apps manager's installed inventory and selected item details."
  (lispm-desktop-app-manager--set-up)
  (lispm-desktop-settings--heading (lispm-desktop-tr "Apps"))
  (insert (lispm-desktop-settings--indent))
  (lispm-desktop-app-manager--insert-field)
  (lispm-ui-insert-popup-button
   (pcase lispm-desktop-app-manager--filter
     ('app (lispm-desktop-tr "Applications")) ('extension (lispm-desktop-tr "Extensions"))
     (_ (lispm-desktop-tr "All")))
   (list (cons 'all (lispm-desktop-tr "All"))
         (cons 'app (lispm-desktop-tr "Applications"))
         (cons 'extension (lispm-desktop-tr "Extensions")))
   (lambda (filter)
     (setq lispm-desktop-app-manager--filter filter
           lispm-desktop-app-manager--selected nil lispm-desktop-app-manager--page 0)
     (lispm-desktop-app-manager-refresh))
   :current lispm-desktop-app-manager--filter)
  (insert " ")
  (lispm-desktop-app-manager--button (lispm-desktop-tr "Refresh") #'lispm-desktop-app-manager-refresh)
  (insert "\n")
  (let* ((items (lispm-desktop-app-manager--matches))
         (size (lispm-desktop-app-manager--page-size))
         (pages (max 1 (ceiling (length items) size))))
    (setq lispm-desktop-app-manager--page (max 0 (min (1- pages) lispm-desktop-app-manager--page)))
    (let* ((page (seq-subseq items (* size lispm-desktop-app-manager--page)
                            (min (length items) (* size (1+ lispm-desktop-app-manager--page)))))
           (selected (or (assq lispm-desktop-app-manager--selected page) (car page))))
      (setq lispm-desktop-app-manager--selected (car selected))
      (if (null items)
          (lispm-desktop-settings--note (lispm-desktop-tr "No matching applications"))
        (pcase-dolist (`(,id . ,props) page)
          (insert (lispm-desktop-settings--indent))
          (lispm-ui-insert-radio-button
           (plist-get props :name) (eq id lispm-desktop-app-manager--selected)
           (lambda () (lispm-desktop-app-manager--choose id)))
          (insert "\n")))
      (when (> pages 1)
        (insert (lispm-desktop-settings--indent))
        (lispm-desktop-app-manager--button
         (lispm-desktop-tr "Previous") (lambda () (lispm-desktop-app-manager--turn -1))
         (= lispm-desktop-app-manager--page 0))
        (insert (format " %d / %d " (1+ lispm-desktop-app-manager--page) pages))
        (lispm-desktop-app-manager--button
         (lispm-desktop-tr "Next") (lambda () (lispm-desktop-app-manager--turn 1))
         (= lispm-desktop-app-manager--page (1- pages)))
        (insert "\n"))
      (when selected (lispm-desktop-app-manager--details (car selected) (cdr selected))))))

(defun lispm-desktop-app-manager-insert-updates ()
  "Insert the Apps manager's Updates tab: automatic update setting and
installed items with updates available, with Update All and per-item Update
buttons."
  (lispm-desktop-app-manager--clear-field)
  (lispm-desktop-settings--heading (lispm-desktop-tr "Automatic Updates"))
  (lispm-desktop-settings--check-box
   (lispm-desktop-tr "Update applications automatically") lispm-desktop-store-update-automatically
   (lambda () (lispm-desktop-settings--save 'lispm-desktop-store-update-automatically
                                            (not lispm-desktop-store-update-automatically)))
   (if lispm-desktop-store-update-automatically
       (lispm-desktop-tr "when LispM is idle, an application once closed")
     (lispm-desktop-tr "they wait in the App Store")))
  (lispm-desktop-settings--heading (lispm-desktop-tr "Updates"))
  (let* ((items (lispm-desktop-app-manager-update-items))
         (ready (seq-some (pcase-lambda (`(,id . ,_))
                            (let ((info (lispm-desktop-store-installed-info id)))
                              (and info (not (plist-get info :busy)))))
                          items)))
    (insert (lispm-desktop-settings--indent))
    (lispm-desktop-app-manager--button
     (lispm-desktop-tr "Check for Updates") #'lispm-desktop-store-check-for-updates)
    (insert " ")
    (lispm-desktop-app-manager--button
     (lispm-desktop-tr "Update All") #'lispm-desktop-app-manager-update-all (not ready))
    (insert "\n")
    (if (null items)
        (lispm-desktop-settings--paragraph
         (lispm-desktop-tr "The applications from the App Store are up to date."))
      (pcase-dolist (`(,id . ,props) items)
        (let* ((info (lispm-desktop-store-installed-info id))
               (busy (plist-get info :busy)))
          (insert (lispm-desktop-settings--indent)
                  (propertize (plist-get props :name) 'face 'bold)
                  "  "
                  (propertize
                   (lispm-desktop-tr "%s · Version: %s"
                                     (if (eq (plist-get props :kind) 'extension)
                                         (lispm-desktop-tr "Extension")
                                       (lispm-desktop-tr "Application"))
                                     (plist-get info :version))
                   'face 'shadow))
          (when busy
            (insert "  " (propertize (lispm-desktop-tr "Updating…") 'face 'shadow)))
          (insert "  ")
          (lispm-desktop-app-manager--button
           (lispm-desktop-tr "Update")
           (lambda () (lispm-desktop-app-manager-update-item id))
           busy)
          (insert "\n"))))))

;;;###autoload
(defun lispm-desktop-app-manager ()
  "Open Apps, the independent installed-application control panel."
  (interactive)
  (require 'lispm-desktop-settings)
  (lispm-desktop-settings-show 'apps))

(add-hook 'lispm-desktop-store-changed-hook #'lispm-desktop-app-manager-refresh)
(add-hook 'lispm-desktop-apps-changed-hook #'lispm-desktop-app-manager-refresh)
(add-hook 'lispm-desktop-update-changed-hook #'lispm-desktop-app-manager-refresh)

(provide 'lispm-desktop-app-manager)
;;; lispm-desktop-app-manager.el ends here
