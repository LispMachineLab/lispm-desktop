;;; lispm-desktop-agent-list.el --- The agent session sidebar  -*- lexical-binding: t; -*-

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

;; The agents view of the left sidebar: the recorded and running sessions
;; of the agents (`lispm-agents-define'), grouped by project, with
;; per-session actions.  The agents and their sessions are
;; lispm-desktop-ai.el's; this file only lists them.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-ai)
(require 'lispm-desktop-sidebar)
(require 'lispm-desktop-popup)
(require 'lispm-desktop-i18n)

(declare-function lispm-desktop-term-status "lispm-desktop-term" (&optional buffer))
(declare-function lispm-desktop-term-status-label "lispm-desktop-term" (record &optional brief))

;;;; Agent session sidebar

(defconst lispm-desktop-agent-list-buffer-name "*Agents*"
  "Name of the agent sidebar buffer.")

(defconst lispm-desktop-agent-list-project-preview 5
  "Number of recorded sessions an unexpanded project group shows.")

(defconst lispm-desktop-agent-list-fold-age (* 7 86400)
  "Seconds of inactivity after which a project group starts folded.")

(defvar-local lispm-desktop-agent-list--folds nil
  "Alist of directory keys to an explicit folded state chosen by the user.
Directories absent from it use `lispm-desktop-agent-list--default-folded-p'.")

(defvar-local lispm-desktop-agent-list--expanded nil
  "Directory keys whose groups list every session instead of a preview.")

(defun lispm-desktop-agent-list--default-folded-p (group now)
  "Return non-nil when GROUP starts folded at time NOW.
Groups without running terminals and idle for
`lispm-desktop-agent-list-fold-age' fold."
  (and (null (plist-get group :running))
       (> (- now (plist-get group :time)) lispm-desktop-agent-list-fold-age)))

(defun lispm-desktop-agent-list--folded-p (group now)
  "Return non-nil when GROUP is folded at time NOW."
  (let ((choice (assoc (plist-get group :key) lispm-desktop-agent-list--folds)))
    (if choice (cdr choice) (lispm-desktop-agent-list--default-folded-p group now))))

(defun lispm-desktop-agent-list--toggle-fold (group)
  "Fold GROUP when it is unfolded and unfold it otherwise, then re-render."
  (setq lispm-desktop-agent-list--folds
        (cons (cons (plist-get group :key)
                    (not (lispm-desktop-agent-list--folded-p group (float-time))))
              (assoc-delete-all (plist-get group :key) lispm-desktop-agent-list--folds)))
  (lispm-desktop-agent-list--redraw))

(defun lispm-desktop-agent-list--toggle-expanded (directory)
  "List every session of DIRECTORY's group, or only a preview again."
  (setq lispm-desktop-agent-list--expanded
        (if (member directory lispm-desktop-agent-list--expanded)
            (delete directory lispm-desktop-agent-list--expanded)
          (cons directory lispm-desktop-agent-list--expanded)))
  (lispm-desktop-agent-list--redraw))

(defun lispm-desktop-agent-list--running-label (buffer)
  "Return the label of running agent terminal BUFFER in the sidebar: what
its program says it is doing, see `lispm-desktop-term-status', else
\"running\"."
  (if-let* ((record (and buffer (fboundp 'lispm-desktop-term-status)
                         (lispm-desktop-term-status buffer))))
      (lispm-desktop-term-status-label record t)
    (propertize (lispm-desktop-tr "running") 'face 'success)))

(defvar-keymap lispm-desktop-agent-list-entry-map
  :doc "Keymap on session entries: button keys plus the context menu."
  :parent button-map
  "<down-mouse-3>" #'ignore
  "<mouse-3>" #'lispm-desktop-agent-list-context-menu)

(defun lispm-desktop-agent-list--insert-project (group now running-ids)
  "Insert project GROUP, its running terminals and a preview of its sessions.
NOW dates the sessions; RUNNING-IDS are the ids of sessions live terminals
run.  The project is a row with a disclosure triangle, as a folder in a
Finder list; its sessions are rows one level in, newest first."
  (let* ((folded (lispm-desktop-agent-list--folded-p group now))
         (directory (plist-get group :dir))
         (chats (plist-get group :chats))
         (sessions (plist-get group :sessions)))
    (lispm-desktop-sidebar-insert-group
     (not folded)
     (if chats (lispm-desktop-tr "Chats") (file-name-nondirectory directory))
     (lispm-agents-age (plist-get group :time) now)
     (apply-partially #'lispm-desktop-agent-list--toggle-fold group)
     (cond ((and folded chats) (lispm-desktop-tr "Open project-less conversations"))
           (folded (lispm-desktop-tr "Open %s" directory))
           (chats (lispm-desktop-tr "Close project-less conversations"))
           (t (lispm-desktop-tr "Close %s" directory)))
     (unless chats directory))
    (unless folded
      ;; A terminal whose session is already listed shows as that session's
      ;; running row; only terminals without a recorded session list here.
      (dolist (buffer (seq-remove
                       (lambda (buffer)
                         (when-let* ((id (buffer-local-value 'lispm-agents-buffer-session-id buffer)))
                           (seq-some (lambda (session) (equal (plist-get session :id) id))
                                     sessions)))
                       (plist-get group :running)))
        (lispm-desktop-sidebar-insert-entry
         (lispm-agents-icon (lispm-agents-of-buffer buffer))
         (lispm-agents-buffer-label buffer)
         (lispm-desktop-agent-list--running-label buffer)
         (apply-partially #'lispm-agents-show buffer)
         (lispm-desktop-tr "Show this terminal")
         nil 1))
      (let* ((expanded (member (plist-get group :key) lispm-desktop-agent-list--expanded))
             (shown (if expanded sessions
                      (seq-take sessions lispm-desktop-agent-list-project-preview))))
        (dolist (session shown)
          (let* ((id (plist-get session :id))
                 (archived (lispm-agents-archived-p session))
                 (location (lispm-agents-location-label
                            (plist-get session :cwd)
                            (if chats 'chats (expand-file-name directory)))))
            (lispm-desktop-sidebar-insert-entry
             (lispm-agents-icon (plist-get session :agent))
             (if archived
                 (propertize (plist-get session :title) 'face '(:inherit (shadow lispm-desktop-platinum-list)))
               (plist-get session :title))
             (if (member id running-ids)
                 (lispm-desktop-agent-list--running-label (lispm-agents-session-buffer id))
               (lispm-agents-age (plist-get session :time) now))
             (apply-partially #'lispm-agents-resume-session session)
             (let ((agent (plist-get session :agent)))
               (cond ((and location archived)
                      (lispm-desktop-tr "Resume %s session %s, in %s, archived (right-click for more)"
                                        agent id location))
                     (location
                      (lispm-desktop-tr "Resume %s session %s, in %s (right-click for more)"
                                        agent id location))
                     (archived
                      (lispm-desktop-tr "Resume %s session %s, archived (right-click for more)" agent id))
                     (t (lispm-desktop-tr "Resume %s session %s (right-click for more)" agent id))))
             (list 'lispm-agents-session session 'keymap lispm-desktop-agent-list-entry-map)
             1)))
        (when (or expanded (> (length sessions) (length shown)))
          (lispm-desktop-sidebar-insert-entry
           ""
           (propertize (if expanded
                           (lispm-desktop-tr "Fewer")
                         (let ((more (- (length sessions) (length shown))))
                           (lispm-desktop-tr-n "%d more…" "%d more…" more)))
                       'face '(:inherit (shadow lispm-desktop-platinum-list)))
           nil
           (apply-partially #'lispm-desktop-agent-list--toggle-expanded (plist-get group :key))
           (lispm-desktop-tr "Toggle the full session list of this project")
           nil 1))))))

(defvar-local lispm-desktop-agent-list--show-archived nil
  "Non-nil when this sidebar lists archived sessions too.")

(defun lispm-desktop-agent-list-toggle-show-archived ()
  "Show archived sessions in the sidebar, or hide them again."
  (interactive)
  (with-current-buffer lispm-desktop-agent-list-buffer-name
    (setq lispm-desktop-agent-list--show-archived (not lispm-desktop-agent-list--show-archived)))
  (lispm-desktop-agent-list--redraw))

(defun lispm-desktop-agent-list--info (sessions running reading)
  "Return the count on the sidebar's bottom edge: SESSIONS listed, RUNNING
terminals, and, when READING, that sessions are still being read."
  (seq-reduce (lambda (shown detail) (lispm-desktop-tr "%s, %s" shown detail))
              (delq nil (list (when (> running 0)
                                (lispm-desktop-tr-n "%d running" "%d running" running))
                              (when reading (lispm-desktop-tr "still reading…"))))
              (lispm-desktop-tr-n "%d session" "%d sessions" sessions)))

(defun lispm-desktop-agent-list--render ()
  "Render the agent sidebar, grouped by working directory, into this buffer.
Archived sessions are left out unless `lispm-desktop-agent-list--show-archived'.
The sessions are those known now, see `lispm-agents-sessions-snapshot':
while a session reader has none yet, the window's bottom edge says the
sessions are still being read."
  (let* ((inhibit-read-only t)
         (now (float-time))
         (snapshot (lispm-agents-sessions-snapshot))
         (all (car snapshot))
         (reading (cdr snapshot))
         ;; Terminals claim their sessions first, so a session a new
         ;; terminal just claimed already counts as running below.
         (_ (lispm-agents-refresh-buffer-titles all))
         (running (lispm-agents-running-buffers))
         (running-ids (lispm-agents-running-ids running))
         (archived (seq-count #'lispm-agents-archived-p all))
         (sessions (if lispm-desktop-agent-list--show-archived all
                     (seq-remove #'lispm-agents-archived-p all))))
    (require 'nerd-icons)
    (erase-buffer)
    (lispm-desktop-sidebar-set-toolbar
     'agents #'lispm-desktop-agent-list-refresh
     ;; A new-session button per agent that can run here, those built
     ;; in first.
     (mapcar (pcase-lambda (`(,agent . ,props))
               (list (concat (lispm-agents-icon agent) "+") (plist-get props :command)
                     (lispm-desktop-tr "New %s session in the current project"
                                       (plist-get props :name))))
             (lispm-agents-available))
     ;; The archive toggle shows the archived count, pressed while archived
     ;; sessions are listed.
     (when (> archived 0)
       `((,(lispm-desktop-tr-n "%d Archived" "%d Archived" archived)
          lispm-desktop-agent-list-toggle-show-archived
          ,(if lispm-desktop-agent-list--show-archived
               (lispm-desktop-tr "Hide archived sessions")
             (lispm-desktop-tr "Show archived sessions"))
          ,lispm-desktop-agent-list--show-archived))))
    (setq lispm-desktop-platinum-info
          (lispm-desktop-agent-list--info (length sessions) (length running) reading))
    (lispm-desktop-sidebar-insert-columns (lispm-desktop-tr "Name") (lispm-desktop-tr "Modified"))
    (dolist (group (lispm-agents-group-by-directory sessions running))
      (lispm-desktop-agent-list--insert-project group now running-ids))))

(defun lispm-desktop-agent-list--redraw ()
  "Re-render the agent sidebar, keeping point on the same line, with the
sessions as they are known: session readers are not asked to read anew,
see `lispm-agents-reread-sessions-hook'."
  (lispm-desktop-sidebar-refresh-list 'agents))

(defun lispm-desktop-agent-list-refresh ()
  "Read the sessions anew and re-render the agent sidebar, keeping point
on the same line.  Session readers read anew first, see
`lispm-agents-reread-sessions-hook'."
  (interactive)
  (run-hooks 'lispm-agents-reread-sessions-hook)
  (lispm-desktop-agent-list--redraw))

(defvar lispm-desktop-agent-list--refresh-timer nil
  "The idle timer of a pending sidebar re-render, or nil.")

(defun lispm-desktop-agent-list--refresh-when-idle ()
  "Re-render the agent sidebar once Emacs is idle, unless that is pending.
Reading transcripts can take seconds the first time, so the re-render
never runs inside the code that asked for it.  Session readers are not
asked to read anew, see `lispm-desktop-agent-list--redraw'."
  (unless lispm-desktop-agent-list--refresh-timer
    (setq lispm-desktop-agent-list--refresh-timer
          (run-with-idle-timer
           0.2 nil (lambda ()
                     (setq lispm-desktop-agent-list--refresh-timer nil)
                     (lispm-desktop-agent-list--redraw))))))

(defun lispm-desktop-agent-list-refresh-on-exit (&optional terminal &rest _)
  "Refresh the agent sidebar once Emacs is idle after an agent exits.
TERMINAL is the exited terminal's buffer or process; other terminals
leave the sidebar alone."
  (let ((buffer (if (processp terminal) (process-buffer terminal) terminal)))
    (when (lispm-agents-buffer-p buffer)
      (lispm-desktop-agent-list--refresh-when-idle))))

(defvar-local lispm-desktop-agent-list--status-shown nil
  "The sidebar label of this agent terminal when its status last changed.")

(defun lispm-desktop-agent-list-refresh-on-status (buffer)
  "Refresh the agent sidebar once Emacs is idle when agent terminal BUFFER's
label there changed, see `lispm-desktop-agent-list--running-label'.
Progress alone changes no label, so it never re-renders the sidebar."
  (when (lispm-agents-buffer-p buffer)
    (let ((label (lispm-desktop-agent-list--running-label buffer)))
      (unless (equal label (buffer-local-value 'lispm-desktop-agent-list--status-shown buffer))
        (with-current-buffer buffer
          (setq lispm-desktop-agent-list--status-shown label))
        (lispm-desktop-agent-list--refresh-when-idle)))))

;;;; Acting on the session at point

(defun lispm-desktop-agent-list-context-menu (event)
  "Pop up the actions for the session clicked by mouse EVENT.
It is drawn as Mac OS 9's contextual menus were, see `lispm-desktop-popup-menu'."
  (interactive "e")
  (let* ((posn (event-start event))
         (session (with-current-buffer (window-buffer (posn-window posn))
                    (get-text-property (posn-point posn) 'lispm-agents-session))))
    (when session
      (pcase-let ((`(,x . ,y) (lispm-desktop-popup-frame-xy event)))
        (lispm-desktop-popup-menu
         (mapcar (lambda (entry)
                   (if (eq entry 'separator)
                       entry
                     (list :label (car entry)
                           :action (lambda () (lispm-agents-session-act (cdr entry) session)))))
                 `((,(lispm-desktop-tr "Resume") . resume)
                   (,(lispm-desktop-tr "Rename…") . rename)
                   (,(if (lispm-agents-archived-p session)
                         (lispm-desktop-tr "Unarchive")
                       (lispm-desktop-tr "Archive"))
                    . archive)
                   (,(lispm-desktop-tr "Delete…") . delete)
                   separator
                   (,(lispm-desktop-tr "Copy Session ID") . copy)
                   (,(lispm-desktop-tr "Open Transcript") . transcript)
                   (,(lispm-desktop-tr "Open Directory") . directory)))
         (1+ x) (1+ y))))))

(defun lispm-desktop-agent-list--session-at-point ()
  "Return the session of the sidebar entry at point."
  (or (get-text-property (point) 'lispm-agents-session)
      (user-error (lispm-desktop-tr "No session at point"))))

(defun lispm-desktop-agent-list-rename ()
  "Rename the session at point."
  (interactive)
  (lispm-agents-rename-session (lispm-desktop-agent-list--session-at-point)))

(defun lispm-desktop-agent-list-toggle-archive ()
  "Archive or unarchive the session at point."
  (interactive)
  (lispm-agents-toggle-archive-session (lispm-desktop-agent-list--session-at-point)))

(defun lispm-desktop-agent-list-delete ()
  "Move the session at point to the system trash."
  (interactive)
  (lispm-agents-delete-session (lispm-desktop-agent-list--session-at-point)))

(defun lispm-desktop-agent-list-copy-id ()
  "Copy the id of the session at point."
  (interactive)
  (lispm-agents-session-act 'copy (lispm-desktop-agent-list--session-at-point)))

(defvar-keymap lispm-desktop-agent-list-mode-map
  :doc "Keymap for the agent sidebar."
  :parent special-mode-map
  "g" #'lispm-desktop-agent-list-refresh
  "r" #'lispm-desktop-agent-list-rename
  "a" #'lispm-desktop-agent-list-toggle-archive
  "A" #'lispm-desktop-agent-list-toggle-show-archived
  "D" #'lispm-desktop-agent-list-delete
  "w" #'lispm-desktop-agent-list-copy-id
  "n" #'forward-button
  "p" #'backward-button
  "TAB" #'forward-button
  "<backtab>" #'backward-button)

(lispm-agents-add-key-map 'lispm-desktop-agent-list-mode-map)

(define-derived-mode lispm-desktop-agent-list-mode special-mode "Agents"
  "Sidebar listing the agents' running and recorded sessions.
Each agent adds the key starting it, see `lispm-agents-define'.
\\{lispm-desktop-agent-list-mode-map}"
  (lispm-desktop-sidebar-setup-buffer))

(lispm-desktop-sidebar-define-view
 'agents :title (lispm-desktop-tr-noop "Agents") :icon "robot" :command 'lispm-desktop-sidebar-agents
 :help (lispm-desktop-tr-noop "Agent sessions")
 :buffer lispm-desktop-agent-list-buffer-name
 :mode #'lispm-desktop-agent-list-mode
 :render #'lispm-desktop-agent-list--render)

(defun lispm-desktop-sidebar-agents ()
  "Show the agent sessions in the left sidebar, or hide them when shown."
  (interactive)
  (lispm-desktop-sidebar-select 'agents))

(keymap-set lispm-agents-map "a" #'lispm-desktop-sidebar-agents)

(defun lispm-desktop-agent-list--agents-changed ()
  "Show the agents anew once Emacs is idle, where the sidebar lists them,
shown or not.  This is on `lispm-agents-changed-hook' and
`lispm-agents-sessions-arrived-hook'."
  (when (get-buffer lispm-desktop-agent-list-buffer-name)
    (lispm-desktop-agent-list--refresh-when-idle)))

(defun lispm-desktop-setup-agent-list ()
  "Keep the agents sidebar current: as agents and sessions change, as
session readers get newer sessions, and as agent terminals exit or say
what they are doing."
  (add-hook 'lispm-agents-changed-hook #'lispm-desktop-agent-list--agents-changed)
  (add-hook 'lispm-agents-sessions-arrived-hook #'lispm-desktop-agent-list--agents-changed)
  (add-hook 'lispm-agents-sessions-changed-hook #'lispm-desktop-agent-list-refresh)
  (add-hook 'eat-exit-hook #'lispm-desktop-agent-list-refresh-on-exit)
  (add-hook 'lispm-desktop-term-exit-functions #'lispm-desktop-agent-list-refresh-on-exit)
  (add-hook 'lispm-desktop-term-status-functions #'lispm-desktop-agent-list-refresh-on-status))

(provide 'lispm-desktop-agent-list)
;;; lispm-desktop-agent-list.el ends here
