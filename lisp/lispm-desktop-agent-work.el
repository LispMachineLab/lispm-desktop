;;; lispm-desktop-agent-work.el --- A directory's live agents -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; The Agents sidebar's native folder view.  It lists activated conversations
;; in that exact directory, including idle agents.  Only cheap live metadata is
;; read; the view neither loads session transcripts nor starts agents.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'lispm-desktop-ai)
(require 'lispm-desktop-sidebar)
(require 'lispm-desktop-interface)
(require 'lispm-ui-controls)

(declare-function lispm-agents-roster "lispm-agents-roster" ())
(declare-function lispm-desktop-term-status-label "lispm-desktop-term" (record &optional brief))

(defvar lispm-desktop-agent-work--buffers nil "Open native directory work views.")
(defvar lispm-desktop-agent-work--timer nil "One pending event-driven redraw.")
(defvar-local lispm-desktop-agent-work--directory nil "Exact directory shown by this view.")
(defvar-local lispm-desktop-agent-work--dirty t "Whether this view needs a fresh snapshot.")

(defconst lispm-desktop-agent-work--hooks
  '(lispm-agents-changed-hook lispm-agents-sessions-changed-hook
    lispm-agents-roster-change-functions lispm-desktop-term-status-functions
    lispm-desktop-term-exit-functions)
  "Existing agent events that may change the live list.")

(defun lispm-desktop-agent-work--directory-key (directory)
  "Normalize DIRECTORY without grouping subdirectories or worktrees."
  (when (stringp directory) (directory-file-name (expand-file-name directory))))

(defun lispm-desktop-agent-work--snapshot ()
  "Return cheap live records, using the optional roster when available."
  (if (fboundp 'lispm-agents-roster)
      (seq-filter (lambda (record)
                    (and (plist-get record :alive) (buffer-live-p (plist-get record :buffer))))
                  (lispm-agents-roster))
    (let ((seen (make-hash-table :test #'eq)))
      (delq nil
            (mapcar (lambda (buffer)
                      (let ((identity (with-current-buffer buffer
                                        (or (bound-and-true-p lispm-chat-agent)
                                            (bound-and-true-p lispm-session-agent) buffer))))
                        (unless (gethash identity seen)
                          (puthash identity t seen)
                          (list :buffer buffer :alive t :name (lispm-agents-buffer-label buffer)
                                :kind (lispm-agents-of-buffer buffer)
                                :root (buffer-local-value 'default-directory buffer)
                                :status (lispm-agents-buffer-status buffer)))))
                    (lispm-agents-live-buffers))))))

(defun lispm-desktop-agent-work--open (buffer)
  "Display the already live agent BUFFER without resuming a saved session."
  (when (and (buffer-live-p buffer) (lispm-agents-alive-p buffer))
    (funcall lispm-agents-show-function buffer)))

(defun lispm-desktop-agent-work--draw (records)
  "Draw the current directory's live RECORDS without querying agents."
  (let* ((inhibit-read-only t)
         (position (point))
         (directory (lispm-desktop-agent-work--directory-key lispm-desktop-agent-work--directory))
         (visible (seq-filter
                   (lambda (record)
                     (equal directory (lispm-desktop-agent-work--directory-key (plist-get record :root))))
                   records))
         (owner (current-buffer)))
    (erase-buffer)
    (insert (propertize (format "%s · %d" (lispm-desktop-tr "Agents") (length visible)) 'face 'bold)
            "\n" (abbreviate-file-name lispm-desktop-agent-work--directory) "\n\n")
    (lispm-ui-insert-push-button
     (lispm-desktop-tr "Refresh")
     (lambda () (when (buffer-live-p owner)
                  (with-current-buffer owner (lispm-desktop-agent-work-refresh)))))
    (insert "\n\n")
    (if (null visible) (lispm-desktop-sidebar-insert-empty (lispm-desktop-tr "None"))
      (dolist (record visible)
        (let* ((buffer (plist-get record :buffer))
               (name (or (plist-get record :name) (buffer-name buffer)))
               (status (plist-get record :status)))
          (lispm-desktop-sidebar-insert-entry
           (lispm-agents-icon (plist-get record :kind)) name
           (if status (lispm-desktop-term-status-label status t) (lispm-desktop-tr "Unknown"))
           (apply-partially #'lispm-desktop-agent-work--open buffer)
           (lispm-desktop-tr "Open %s" name)
           (list 'lispm-desktop-agent-work-buffer buffer)))))
    (setq lispm-desktop-agent-work--dirty nil)
    (goto-char (min position (point-max)))))

(defun lispm-desktop-agent-work-refresh ()
  "Refresh this directory's live agents without reading stored sessions."
  (interactive)
  (lispm-desktop-agent-work--draw (lispm-desktop-agent-work--snapshot)))

(defun lispm-desktop-agent-work--refresh-visible (&rest _)
  "Refresh dirty visible views from one cheap snapshot."
  (let ((views (seq-filter
                (lambda (buffer)
                  (and (buffer-live-p buffer)
                       (buffer-local-value 'lispm-desktop-agent-work--dirty buffer)
                       (get-buffer-window buffer 'visible)))
                lispm-desktop-agent-work--buffers)))
    (when views
      (let ((records (lispm-desktop-agent-work--snapshot)))
        (dolist (buffer views)
          (with-current-buffer buffer (lispm-desktop-agent-work--draw records)))))))

(defun lispm-desktop-agent-work--changed (&rest _)
  "Mark open views dirty and schedule one redraw only while a view is visible."
  (dolist (buffer lispm-desktop-agent-work--buffers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (setq lispm-desktop-agent-work--dirty t))))
  (when (and (not lispm-desktop-agent-work--timer)
             (seq-some (lambda (buffer) (get-buffer-window buffer 'visible))
                       lispm-desktop-agent-work--buffers))
    (setq lispm-desktop-agent-work--timer
          (run-with-idle-timer
           0.1 nil (lambda ()
                     (setq lispm-desktop-agent-work--timer nil)
                     (lispm-desktop-agent-work--refresh-visible))))))

(defun lispm-desktop-agent-work--close ()
  "Release observers after the last native work view closes."
  (setq lispm-desktop-agent-work--buffers (delq (current-buffer) lispm-desktop-agent-work--buffers))
  (unless lispm-desktop-agent-work--buffers
    (when lispm-desktop-agent-work--timer (cancel-timer lispm-desktop-agent-work--timer))
    (setq lispm-desktop-agent-work--timer nil)
    (dolist (hook lispm-desktop-agent-work--hooks)
      (remove-hook hook #'lispm-desktop-agent-work--changed))
    (remove-hook 'window-state-change-functions #'lispm-desktop-agent-work--refresh-visible)))

(defvar-keymap lispm-desktop-agent-work-mode-map
  :parent special-mode-map
  "g" #'lispm-desktop-agent-work-refresh
  "TAB" #'forward-button
  "<backtab>" #'backward-button)

(define-derived-mode lispm-desktop-agent-work-mode special-mode "Agent Work"
  "List activated agents in one directory, without requiring an Office plugin."
  (unless lispm-ui-current-palette (lispm-ui-palette-follow-theme))
  (setq-local truncate-lines t lispm-desktop-conversation-buffer t)
  (add-hook 'kill-buffer-hook #'lispm-desktop-agent-work--close nil t)
  (add-hook 'change-major-mode-hook #'lispm-desktop-agent-work--close nil t))

(defun lispm-desktop-agent-work (directory)
  "Show all activated agents in exact DIRECTORY in the main session area."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (buffer (get-buffer-create (format "*Agents: %s*" (abbreviate-file-name directory)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lispm-desktop-agent-work-mode) (lispm-desktop-agent-work-mode))
      (setq default-directory directory lispm-desktop-agent-work--directory directory)
      (cl-pushnew buffer lispm-desktop-agent-work--buffers)
      (dolist (hook lispm-desktop-agent-work--hooks)
        (add-hook hook #'lispm-desktop-agent-work--changed))
      (add-hook 'window-state-change-functions #'lispm-desktop-agent-work--refresh-visible)
      (lispm-desktop-agent-work-refresh))
    (lispm-desktop-main-area-show-terminal buffer)))

(provide 'lispm-desktop-agent-work)
;;; lispm-desktop-agent-work.el ends here
