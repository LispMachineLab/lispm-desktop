;;; lispm-desktop-ai.el --- The editor's agents, on the desktop  -*- lexical-binding: t; -*-

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

;; The editor's agents are LispM Agent's (lispm-agents.el): Claude Code
;; and Codex, which come with it, LispM's own (lispm-desktop-lispm.el)
;; and those the App Store adds, as Hermes.  They know no interface but
;; Emacs's; this module draws them into the desktop's:
;;
;; - they run in the desktop's terminals (lispm-desktop-term.el), Eat's
;;   while its module is not built;
;; - a session shows in its project's workspace, alone in a new one;
;; - a file an agent opens, or a change it proposes, shows in the main
;;   area's file column;
;; - their windows wear the agent's mark and the session's title;
;; - deleting a session asks with an alert;
;; - C-c a holds their commands, with n for the next terminal waiting
;;   for you.
;;
;; The names these had in the desktop, before they moved to LispM Agent,
;; stay as obsolete aliases, for extensions written with them.

;;; Code:

(require 'cl-lib)
(require 'lispm-agents)
(require 'lispm-agents-mcp)
(require 'lispm-agents-ide)
(require 'lispm-agents-claude)
(require 'lispm-agents-codex)
(require 'lispm-desktop-core)
(require 'lispm-desktop-i18n)

(declare-function lispm-desktop-terminal-make "lispm-desktop-term" (name program &rest args))
(declare-function lispm-desktop-terminal-live-p "lispm-desktop-term" (buffer))
(declare-function lispm-desktop-terminal-paste "lispm-desktop-term" (buffer text))
(declare-function lispm-desktop-term-waiting-buffers "lispm-desktop-term" ())
(declare-function lispm-desktop-platinum-main-window "lispm-desktop-platinum-titlebar" ())
(declare-function lispm-desktop-sidebar-show-in-main "lispm-desktop-sidebar" (buffer))
(declare-function lispm-desktop-workspace-enter "lispm-desktop-interface" (name &optional directory))
(declare-function lispm-desktop-workspace-show-alone "lispm-desktop-interface" (buffer))
(declare-function lispm-desktop-popup-alert "lispm-desktop-popup" (icon message buttons &optional detail))
(defvar tabspaces-default-tab)
(defvar lispm-desktop-term-exit-functions)

(defconst lispm-desktop-agent-icon-directory
  (expand-file-name "icons" lispm-desktop-directory)
  "Directory of the desktop's image icons of agents.")

(defun lispm-desktop-agent--workspace-name (directory)
  "Return the workspace owning sessions run in DIRECTORY.
Sessions belong to their project root, so worktrees and subdirectories share
the repository's workspace; project-less chats share the default workspace."
  (let ((root (lispm-agents-project-root directory)))
    (if (eq root 'chats)
        (cons tabspaces-default-tab nil)
      (cons (file-name-nondirectory root) root))))

(defun lispm-desktop-agent--show (buffer)
  "Show agent BUFFER in its project's workspace and select it.
In a workspace just created for it, BUFFER is shown alone; otherwise it is
placed as `lispm-desktop-sidebar-show-in-main' places terminals.  This is
`lispm-agents-show-function' on the desktop."
  (pcase-let ((`(,name . ,root) (lispm-desktop-agent--workspace-name
                                 (buffer-local-value 'default-directory buffer))))
    (if (lispm-desktop-workspace-enter name root)
        (lispm-desktop-workspace-show-alone buffer)
      (lispm-desktop-sidebar-show-in-main buffer))))

(defun lispm-desktop-agent-next-waiting ()
  "Show the next terminal whose program waits for you.
Programs blocked on an answer come first, then those that failed, then
those that finished, as they report themselves, see
`lispm-desktop-term-waiting-buffers'; repeating the command goes on to
the next one."
  (interactive)
  (let* ((waiting (lispm-desktop-term-waiting-buffers))
         (next (or (cadr (memq (current-buffer) waiting)) (car waiting))))
    (unless next
      (user-error (lispm-desktop-tr "No terminal is waiting for you")))
    (if (lispm-agents-buffer-p next)
        (lispm-agents-show next)
      (lispm-desktop-sidebar-show-in-main next))))

(defun lispm-desktop-agent--confirm (question detail action)
  "Ask QUESTION, with DETAIL, in a stop alert offering ACTION or Cancel.
This is `lispm-agents-confirm-function' on the desktop."
  (eq 'go (lispm-desktop-popup-alert
           'stop question
           `((,(lispm-desktop-tr "Cancel") cancel cancel) (,action go default))
           detail)))

(defun lispm-desktop-agent--buffer-icon (buffer)
  "Return the mark of the agent whose session BUFFER holds, or nil.
This is one of `lispm-desktop-platinum-buffer-icon-functions'."
  (when (lispm-agents-buffer-p buffer)
    (lispm-agents-icon (lispm-agents-of-buffer buffer))))

(defvar lispm-agents-terminal-send-function)

;; On the desktop the agents run in its terminals and show in its windows.
(setq lispm-agents-terminal-make-function #'lispm-desktop-terminal-make
      lispm-agents-terminal-live-p-function #'lispm-desktop-terminal-live-p
      lispm-agents-terminal-paste-function #'lispm-desktop-terminal-paste
      lispm-agents-terminal-send-function #'lispm-desktop-terminal-send
      lispm-agents-show-function #'lispm-desktop-agent--show
      lispm-agents-main-window-function #'lispm-desktop-platinum-main-window
      lispm-agents-confirm-function #'lispm-desktop-agent--confirm
      lispm-agents-context-display-action '(lispm-desktop-main-area-display-file))
(dolist (mode lispm-desktop-terminal-major-modes)
  (add-to-list 'lispm-agents-terminal-modes mode))
;; The servers showing agents Emacs's state stop with their terminals.
(add-hook 'lispm-desktop-term-exit-functions #'lispm-agents-context-terminal-exited)
(keymap-set lispm-agents-map "n" #'lispm-desktop-agent-next-waiting)

(defun lispm-desktop-setup-ai ()
  "Set up the editor's agents: their windows wear the agent's mark and
the session's title, and \\`C-c a' holds their commands."
  (add-hook 'lispm-desktop-platinum-buffer-icon-functions #'lispm-desktop-agent--buffer-icon)
  (add-hook 'lispm-desktop-platinum-buffer-title-functions #'lispm-agents-buffer-label)
  (keymap-global-set "C-c a" lispm-agents-map))

;;;; Names of old

(defconst lispm-desktop-ai--moved
  '(("lispm-desktop-agent-context-" "lispm-agents-context-" arguments bind
     content display environment error handle parse path record-selection
     release release-all result selection selection-functions server stop tools
     visible-p)
    ("lispm-desktop-agent-mcp-" "lispm-agents-mcp-" start tools)
    ("lispm-desktop-agent-ide-" "lispm-agents-ide-" accept-diff connection
     diff-mode diff-mode-map diffs directory lock-directory p port reject-diff
     server start token tools)
    ("lispm-desktop-agent-claude-" "lispm-agents-claude-" icon new
     projects-directory sessions)
    ("lispm-desktop-agent-codex-" "lispm-agents-codex-" chats-directory
     index-file mcp-p new sessions sessions-directory token-variable)
    ("lispm-desktop-local-http-" "lispm-local-http-" headers max-request serve)
    ("lispm-desktop-websocket-" "lispm-websocket-" close guid max-message send
     serve)
    ("lispm-desktop-agents-" "lispm-agents-" changed-hook)
    ("lispm-desktop-setup-" "lispm-agents-setup-" claude codex)
    ("lispm-desktop-agent-" "lispm-agents-" add-key-map add-session-reader age
     append-json-line archived-p available available-p buffer-agent
     buffer-label buffer-launched buffer-p buffer-session-id buffer-title
     cached-session claim-sessions claim-slack content-text default-directory
     define delete-session emacs-buffer-agent file-sessions file-time
     fresh-name group-by-directory human-text-p icon image-icon
     independent-environment key-maps label-faces location-label map name new
     of-buffer project-directory props read-json-region read-transcript
     refresh-buffer-titles remove-session-reader rename-session
     reread-sessions-hook resume-session running-buffers running-ids running-p
     send-region session-act session-buffer session-copy-id session-embark-map
     session-marker-regexp session-open-directory session-open-transcript
     session-resume sessions sessions-arrived sessions-arrived-hook
     sessions-changed-hook sessions-list sessions-snapshot show state-file
     terminal-agent title-width toggle-archive-session transcript-head-bytes
     transcript-tail-bytes undefine uuid))
  "Names LispM Agent gave what moved there: (FROM TO NAME...), FROM and TO
prefixes, NAME the rest.  Each FROM NAME stays, obsolete, for TO NAME.")

(cl-defstruct (lispm-desktop-agent-mcp (:include lispm-agents-mcp) (:constructor nil))
  "Obsolete: `lispm-agents-mcp', for agents that include it by its old name.")

(define-obsolete-variable-alias 'lispm-desktop-agents 'lispm-agents-defined "0.9.0")
(pcase-dolist (`(,from ,to . ,names) lispm-desktop-ai--moved)
  (dolist (name names)
    (let ((old (intern (concat from (symbol-name name))))
          (new (intern (concat to (symbol-name name)))))
      (when (fboundp new)
        (define-obsolete-function-alias old new "0.9.0"))
      (when (boundp new)
        (define-obsolete-variable-alias old new "0.9.0"))
      (when (facep new)
        (put old 'face-alias new)
        (put old 'obsolete-face "0.9.0")))))

(provide 'lispm-desktop-ai)
;; The files that moved, required by their old names.
(provide 'lispm-desktop-agent-context)
(provide 'lispm-desktop-agent-mcp)
(provide 'lispm-desktop-agent-ide)
(provide 'lispm-desktop-agent-claude)
(provide 'lispm-desktop-agent-codex)
(provide 'lispm-desktop-local-http)
(provide 'lispm-desktop-websocket)
;;; lispm-desktop-ai.el ends here
