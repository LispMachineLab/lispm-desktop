;;; lispm-desktop-agent-codex.el --- Codex, an agent of the editor  -*- lexical-binding: t; -*-

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

;; Codex as one of the editor's agents, built in: it takes part while
;; `codex' is on PATH (`lispm-desktop-agent-define').
;;
;; - Its sessions are the rollout transcripts under ~/.codex/sessions/,
;;   named by the thread names of ~/.codex/session_index.jsonl, else by
;;   their first prompt.  Codex assigns a session's id itself, so a fresh
;;   terminal learns it from the transcripts.
;; - It sees Emacs's state through an MCP server per terminal
;;   (lispm-desktop-agent-mcp.el), named on its command line for that run
;;   only:
;;
;;     codex -c mcp_servers.emacs.url=... -c ...bearer_token_env_var=VAR
;;           --> POST /mcp with "Authorization: Bearer TOKEN"
;;
;;   The token reaches it in an environment variable that no file holds
;;   and that Codex is told to withhold from the commands it runs.
;; - Its worktrees, ~/.codex/worktrees/ID/NAME, join the project named
;;   NAME in the sidebar; the Codex app's scratch tree holds chats.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-ai)
(require 'lispm-desktop-agent-mcp)
(require 'lispm-desktop-i18n)
;; `parse-iso8601-time-string', for when a session began: not autoloaded.
(require 'parse-time)

(defconst lispm-desktop-agent-codex-sessions-directory
  (expand-file-name "~/.codex/sessions/")
  "Directory where Codex stores its dated rollout transcripts.")

(defconst lispm-desktop-agent-codex-index-file
  (expand-file-name "~/.codex/session_index.jsonl")
  "Codex index mapping session ids to their thread names.")

(defconst lispm-desktop-agent-codex-chats-directory
  (expand-file-name "~/Documents/Codex/")
  "The Codex app's dated scratch tree, whose sessions are chats.")

;;;; Sessions

(defun lispm-desktop-agent-codex--prompt (object)
  "Return the human prompt carried by Codex transcript OBJECT, if any."
  (let ((payload (plist-get object :payload)))
    (pcase (list (plist-get object :type) (plist-get payload :type))
      (`("event_msg" "user_message")
       (let ((text (plist-get payload :message)))
         (and (lispm-desktop-agent-human-text-p text) text)))
      (`("response_item" "message")
       (when (equal (plist-get payload :role) "user")
         (let ((text (lispm-desktop-agent-content-text (plist-get payload :content))))
           (and (lispm-desktop-agent-human-text-p text) text)))))))

(defun lispm-desktop-agent-codex--session (file objects)
  "Return the Codex session recorded in FILE given its head/tail OBJECTS.
Its title is the first prompt, or nil when there is none; a thread name from
the Codex index overrides it later, see `lispm-desktop-agent-codex--named',
since the index changes without the transcript and so is no part of what is
memoized per transcript.  Its :started time, float seconds, is when the
session began, which ties a freshly started terminal to it, see
`lispm-desktop-agent-claim-sessions'.  Return nil for a transcript without
metadata."
  (when-let* ((meta (seq-find (lambda (object)
                                (equal (plist-get object :type) "session_meta"))
                              (car objects)))
              (payload (plist-get meta :payload))
              (id (plist-get payload :id))
              (cwd (plist-get payload :cwd)))
    (list :agent 'codex :id id :cwd cwd
          :title (seq-some #'lispm-desktop-agent-codex--prompt (car objects))
          :started (when-let* ((stamp (plist-get payload :timestamp)))
                     (ignore-errors (float-time (parse-iso8601-time-string stamp))))
          :file file)))

(defun lispm-desktop-agent-codex--thread-names ()
  "Return a hash table from Codex session id to its latest thread name."
  (let ((names (make-hash-table :test #'equal)))
    (when (file-readable-p lispm-desktop-agent-codex-index-file)
      (dolist (entry (lispm-desktop-agent-read-json-region
                      lispm-desktop-agent-codex-index-file 0
                      (file-attribute-size
                       (file-attributes lispm-desktop-agent-codex-index-file))))
        (when-let* ((id (plist-get entry :id))
                    (name (plist-get entry :thread_name)))
          (puthash id name names))))
    names))

(defun lispm-desktop-agent-codex--named (session names)
  "Return SESSION titled by its thread name in NAMES, if it has one.
SESSION is not modified: it is shared with the memo.  Return nil when
SESSION is nil or has neither a name nor a title of its own to show."
  (when-let* ((session)
              (title (or (gethash (plist-get session :id) names)
                         (plist-get session :title))))
    (plist-put (copy-sequence session) :title title)))

(defun lispm-desktop-agent-codex--files ()
  "Return every Codex rollout transcript file."
  (when (file-directory-p lispm-desktop-agent-codex-sessions-directory)
    (directory-files-recursively lispm-desktop-agent-codex-sessions-directory
                                 "\\`rollout-.*\\.jsonl\\'")))

(defun lispm-desktop-agent-codex-sessions ()
  "Return every recorded Codex session.
Forked threads share their session's id; the desktop keeps the newest."
  (let ((names (lispm-desktop-agent-codex--thread-names)))
    (lispm-desktop-agent-file-sessions (lispm-desktop-agent-codex--files)
                                       #'lispm-desktop-agent-codex--session
                                       (lambda (session)
                                         (lispm-desktop-agent-codex--named session names)))))

(defun lispm-desktop-agent-codex--rename (session title)
  "Give Codex SESSION the TITLE: a thread name appended to its index."
  (lispm-desktop-agent-append-json-line
   lispm-desktop-agent-codex-index-file
   (list :id (plist-get session :id) :thread_name title
         :updated_at (format-time-string "%FT%T.%6NZ" nil t))))

(defun lispm-desktop-agent-codex--session-files (session)
  "Return the files recording Codex SESSION: every rollout sharing its id."
  (directory-files-recursively lispm-desktop-agent-codex-sessions-directory
                               (regexp-quote (plist-get session :id))))

(defun lispm-desktop-agent-codex--worktree (directory)
  "Return where DIRECTORY belongs when it is in a Codex worktree.
Codex places worktrees under ~/.codex/worktrees/ID/NAME, which does not name
the main checkout, so the sidebar matches NAME against known projects."
  (when (string-match "/\\.codex/worktrees/[^/]+/\\([^/]+\\)" directory)
    (list :name (match-string 1 directory) :label (concat "⎇ " (lispm-desktop-tr "codex worktree")))))

;;;; Emacs's state, over MCP

(defconst lispm-desktop-agent-codex-token-variable "LISPM_DESKTOP_EMACS_MCP_TOKEN"
  "Environment variable that hands Codex its server's token.")

(cl-defstruct (lispm-desktop-agent-codex-mcp (:include lispm-desktop-agent-mcp)
                                             (:constructor lispm-desktop-agent-codex-mcp--make))
  "The MCP server of a Codex terminal.")

(defun lispm-desktop-agent-codex--bypass-proxy (variable)
  "Return the entry of the proxy exclusion VARIABLE with loopback added.
Codex's HTTP client follows the system proxy, which otherwise receives
the requests to this server too and refuses them."
  (format "%s=%s" variable
          (string-join (delete-dups (append (split-string (or (getenv variable) "") "," t)
                                            '("127.0.0.1")))
                       ",")))

(cl-defmethod lispm-desktop-agent-context-environment ((server lispm-desktop-agent-codex-mcp))
  "Hand Codex SERVER's token, and reach SERVER without a proxy."
  (list (format "%s=%s" lispm-desktop-agent-codex-token-variable (lispm-desktop-agent-mcp-token server))
        (lispm-desktop-agent-codex--bypass-proxy "NO_PROXY")
        (lispm-desktop-agent-codex--bypass-proxy "no_proxy")))

(cl-defmethod lispm-desktop-agent-context-arguments ((server lispm-desktop-agent-codex-mcp))
  "Configure SERVER as Codex's MCP server `emacs' for this run only.
Codex passes its whole environment to the commands it runs unless told
otherwise, so the token variable is excluded from them.  The exclusion
replaces any `shell_environment_policy.exclude' of config.toml."
  (list "-c" (format "mcp_servers.emacs.url=\"http://127.0.0.1:%d/mcp\""
                     (lispm-desktop-agent-mcp-port server))
        "-c" (format "mcp_servers.emacs.bearer_token_env_var=\"%s\""
                     lispm-desktop-agent-codex-token-variable)
        "-c" (format "shell_environment_policy.exclude=[\"%s\"]"
                     lispm-desktop-agent-codex-token-variable)))

(defun lispm-desktop-agent-codex--context (directory)
  "Serve Emacs's state to a Codex terminal running in DIRECTORY."
  (lispm-desktop-agent-mcp-start "codex-mcp" #'lispm-desktop-agent-codex-mcp--make directory))

;;;; The agent

(defun lispm-desktop-agent-codex-new ()
  "Start a new Codex session at the current project root."
  (interactive)
  (lispm-desktop-agent-new 'codex))

(defun lispm-desktop-setup-codex ()
  "Make Codex one of the editor's agents."
  (lispm-desktop-agent-define
   'codex
   :name "Codex"
   ;; OpenAI's mark, the maker of Codex, in the surrounding text's colour.
   :icon '(nerd-icons-codicon "nf-cod-openai" nil)
   :text-icon "◎"
   :command #'lispm-desktop-agent-codex-new
   :key "x"
   :built-in t
   :program "codex"
   :sessions #'lispm-desktop-agent-codex-sessions
   :rename #'lispm-desktop-agent-codex--rename
   :files #'lispm-desktop-agent-codex--session-files
   :worktree #'lispm-desktop-agent-codex--worktree
   :chats (list lispm-desktop-agent-codex-chats-directory)
   :resume-arguments (lambda (id) (list "resume" id))
   :context #'lispm-desktop-agent-codex--context))

(provide 'lispm-desktop-agent-codex)
;;; lispm-desktop-agent-codex.el ends here
