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

(defvar-local lispm-desktop-app-manager--selected nil
  "ID of the selected installed item.")
(defvar-local lispm-desktop-app-manager--search ""
  "Words to match against installed items, without case.")
(defvar-local lispm-desktop-app-manager--filter 'all
  "Kind of installed item to show: all, app, extension or builtin.")
(defvar-local lispm-desktop-app-manager--page 0
  "Zero-based page of installed items shown.")
(defconst lispm-desktop-app-manager--page-size 5
  "Number of installed items shown before the details.")

(defun lispm-desktop-app-manager-items ()
  "Return installed applications and extensions as (ID . PROPERTIES).
Include built-in applications and application registrations with large
icons, but not transient windows.  Store records win over registrations;
:managed distinguishes them from applications the Store cannot remove."
  (let ((installed (lispm-desktop-store-installed-items)))
    (append
     (mapcar (pcase-lambda (`(,id . ,props))
               (cons id (append (list :managed t) props))) installed)
     (mapcan (pcase-lambda (`(,id . ,props))
               (when (and (not (assq id installed))
                          (or (memq id '(desktop editor browser store))
                              (lispm-desktop-apps-large-icon id)))
                 (list (cons id (append (list :name (lispm-desktop-apps-name id)
                                              :kind 'app :managed nil)
                                        props)))))
             lispm-desktop-apps))))

(defun lispm-desktop-app-manager--matches ()
  "Return installed items matching this buffer's search and filter."
  (let ((words (split-string (downcase lispm-desktop-app-manager--search) nil t)))
    (seq-filter
     (pcase-lambda (`(,id . ,props))
       (and (pcase lispm-desktop-app-manager--filter
              ('all t)
              ('builtin (not (plist-get props :managed)))
              (kind (eq kind (plist-get props :kind))))
            (let ((text (downcase (format "%s %s %s" id (plist-get props :name)
                                           (or (plist-get props :description) "")))))
              (seq-every-p (lambda (word) (string-search word text)) words))))
     (lispm-desktop-app-manager-items))))

(defun lispm-desktop-app-manager-refresh (&rest _)
  "Refresh the existing Apps buffer without opening a buffer or window."
  (when (fboundp 'lispm-desktop-settings--buffer-name)
    (when-let* ((buffer (get-buffer (lispm-desktop-settings--buffer-name 'settings-apps))))
      (lispm-desktop-settings--fill buffer))))

(defun lispm-desktop-app-manager-search (text)
  "Show installed items matching TEXT.  An empty string clears the search."
  (interactive (list (read-string (lispm-desktop-tr "Search Apps: ")
                                   lispm-desktop-app-manager--search)))
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

(defun lispm-desktop-app-manager-action (action)
  "Perform ACTION, open, update or uninstall, on the selected item.
Recheck current state before acting.  Store actions always ask for their
normal confirmation; built-ins and busy items cannot be uninstalled."
  (let* ((id lispm-desktop-app-manager--selected)
         (props (cdr (assq id (lispm-desktop-app-manager-items))))
         (info (and (plist-get props :managed) (lispm-desktop-store-installed-info id))))
    (unless props (user-error (lispm-desktop-tr "Select an application first")))
    (pcase action
      ('open (unless (and (eq (plist-get props :kind) 'app)
                          (lispm-desktop-apps-openable-p id))
               (user-error (lispm-desktop-tr "This application cannot be opened")))
             (lispm-desktop-apps-activate id))
      ('update (unless (and info (not (plist-get info :busy))
                            (plist-get info :update-available))
                 (user-error (lispm-desktop-tr "No update is available")))
               (lispm-desktop-store-update id))
      ('uninstall (unless (and info (not (plist-get info :busy)))
                    (user-error (lispm-desktop-tr "This application cannot be uninstalled")))
                  (lispm-desktop-store-uninstall id))
      (_ (error "Unknown Apps action: %s" action)))
    (lispm-desktop-app-manager-refresh)))

(defun lispm-desktop-app-manager--button (label action &optional disabled)
  "Insert a push button with LABEL and ACTION, optionally DISABLED."
  (lispm-ui-insert-push-button label action :disabled disabled))

(defun lispm-desktop-app-manager--details (id props)
  "Insert details and management actions for installed item ID of PROPS."
  (let* ((managed (plist-get props :managed))
         (info (and managed (lispm-desktop-store-installed-info id)))
         (busy (plist-get info :busy)))
    (lispm-desktop-settings--heading (plist-get props :name))
    (lispm-desktop-settings--note
     (if managed
         (lispm-desktop-tr "%s · Version: %s"
                           (if (eq (plist-get props :kind) 'extension)
                               (lispm-desktop-tr "Extension") (lispm-desktop-tr "Application"))
                           (plist-get info :version))
       (lispm-desktop-tr "Built-in")))
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
     (lispm-desktop-tr "Update") (lambda () (lispm-desktop-app-manager-action 'update))
     (not (and info (not busy) (plist-get info :update-available))))
    (insert " ")
    (lispm-desktop-app-manager--button
     (lispm-desktop-tr "Uninstall") (lambda () (lispm-desktop-app-manager-action 'uninstall))
     (or (not managed) busy))
    (insert "\n")))

(defun lispm-desktop-app-manager-insert ()
  "Insert the Apps manager's installed inventory and selected item details."
  (lispm-desktop-settings--heading (lispm-desktop-tr "Apps"))
  (insert (lispm-desktop-settings--indent))
  (lispm-desktop-app-manager--button
   (lispm-desktop-tr "Search…") (lambda () (call-interactively #'lispm-desktop-app-manager-search)))
  (insert " ")
  (lispm-ui-insert-popup-button
   (pcase lispm-desktop-app-manager--filter
     ('app (lispm-desktop-tr "Applications")) ('extension (lispm-desktop-tr "Extensions"))
     ('builtin (lispm-desktop-tr "Built-in")) (_ (lispm-desktop-tr "All")))
   (list (cons 'all (lispm-desktop-tr "All"))
         (cons 'app (lispm-desktop-tr "Applications"))
         (cons 'extension (lispm-desktop-tr "Extensions"))
         (cons 'builtin (lispm-desktop-tr "Built-in")))
   (lambda (filter)
     (setq lispm-desktop-app-manager--filter filter
           lispm-desktop-app-manager--selected nil lispm-desktop-app-manager--page 0)
     (lispm-desktop-app-manager-refresh))
   :current lispm-desktop-app-manager--filter)
  (insert " ")
  (lispm-desktop-app-manager--button (lispm-desktop-tr "Refresh") #'lispm-desktop-app-manager-refresh)
  (insert "\n")
  (unless (string-empty-p lispm-desktop-app-manager--search)
    (lispm-desktop-settings--note lispm-desktop-app-manager--search)
    (insert (lispm-desktop-settings--indent))
    (lispm-desktop-app-manager--button (lispm-desktop-tr "Clear Search")
                                      (lambda () (lispm-desktop-app-manager-search "")))
    (insert "\n"))
  (let* ((items (lispm-desktop-app-manager--matches))
         (size lispm-desktop-app-manager--page-size)
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
