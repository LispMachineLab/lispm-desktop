;;; lispm-desktop-ai.el --- Coding agents, in terminals and in Emacs  -*- lexical-binding: t; -*-

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

;; Coding agents run as their native CLIs inside terminals, or inside
;; Emacs.  The editor ships with three: LispM's (lispm-desktop-lispm.el),
;; Claude Code (lispm-desktop-agent-claude.el) and Codex
;; (lispm-desktop-agent-codex.el); the App Store adds others as
;; extensions, as Hermes (LispMachineLab/lispm-hermes).  Every agent tells
;; `lispm-desktop-agent-define' how to list, start, resume, rename and
;; delete its sessions, and then takes part in all this module adds
;; around it:
;;
;; - a unified, newest-first list of every recorded session of the agents,
;;   resuming the chosen one in its original working directory;
;; - a left sidebar view that groups sessions and running terminals by
;;   project and shows the chosen one in the main window;
;; - commands that start a fresh agent at the current project root, under
;;   C-c a and in the λ menu;
;; - sending the active region (with its location) to a running agent;
;; - a server per terminal showing the agent Emacs's state
;;   (`lispm-desktop-agent-context').
;;
;; An agent run as a CLI takes no part anywhere while its program is not
;; on PATH: no menu, button or key offers it and none of its sessions are
;; listed.
;;
;; A session is a plist (:agent :id :cwd :title :time :file).  Agents that
;; keep JSONL transcripts read them with `lispm-desktop-agent-cached-session',
;; which memoizes the parse per (file . mtime), so reopening the list
;; rereads only files that changed since the last listing:
;;
;;   transcript file --(head/tail bytes)--> JSON objects --(parser)--> session

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-agent-context)

(declare-function lispm-desktop-terminal-make "lispm-desktop-term" (name program &rest args))
(declare-function lispm-desktop-terminal-live-p "lispm-desktop-term" (buffer))
(declare-function lispm-desktop-terminal-paste "lispm-desktop-term" (buffer text))
(declare-function lispm-desktop-term-waiting-buffers "lispm-desktop-term" ())
(declare-function project-root "project" (project))
(declare-function nerd-icons-mdicon "nerd-icons" (icon-name &rest args))
(declare-function lispm-desktop-platinum-main-window "lispm-desktop-platinum-titlebar" ())
(declare-function lispm-desktop-sidebar-one-line "lispm-desktop-sidebar" (text width))
(declare-function lispm-desktop-sidebar-show-in-main "lispm-desktop-sidebar" (buffer))
(declare-function lispm-desktop-workspace-enter "lispm-desktop-interface" (name &optional directory))
(declare-function lispm-desktop-workspace-show-alone "lispm-desktop-interface" (buffer))
(declare-function lispm-desktop-popup-alert "lispm-desktop-popup" (icon message buttons &optional detail))
(declare-function lispm-desktop-popup-menu "lispm-desktop-popup" (items &optional x y align))
(declare-function lispm-desktop-popup-frame-xy "lispm-desktop-popup" (position))
(defvar tabspaces-default-tab)
(defvar eat-terminal)
(defvar embark-keymap-alist)
(defvar lispm-desktop-agent--root-cache)

;;;; Agents

(defvar lispm-desktop-agents nil
  "The agents, in the order they were defined: (AGENT . PROPERTIES).
See `lispm-desktop-agent-define'.")

(defvar-keymap lispm-desktop-agent-map
  :doc "Commands for coding agents, bound under \\`C-c a'.
Each agent adds the key starting it, see `lispm-desktop-agent-define'."
  "s" #'lispm-desktop-agent-sessions
  "n" #'lispm-desktop-agent-next-waiting
  "r" #'lispm-desktop-agent-send-region)

(defun lispm-desktop-agent-define (agent &rest properties)
  "Make AGENT, a symbol, one of the desktop's agents, as PROPERTIES say.
Every agent has these:

  :name        its name, \"Claude Code\"
  :icon        (FUNCTION ICON FACE): FUNCTION makes a one-line icon of
               ICON, as `nerd-icons-codicon' or `lispm-desktop-agent-image-icon'
               do; FACE, when non-nil, is layered under it
  :command     the command starting a session at the current project root
  :key         the key of :command under \\`C-c a' and in the agents sidebar
  :sessions    function returning its recorded sessions, as plists
               (:agent :id :cwd :title :time :file); an agent that
               assigns its own ids adds :started, see
               `lispm-desktop-agent-claim-sessions'
  :rename      function of a session and a title

and these when they apply:

  :built-in    non-nil for an agent the editor ships with; those come
               first wherever agents are offered
  :files       function of a session returning the files and directories
               recording it, which deleting it moves to the Trash
  :delete      function deleting a session from the agent's own store,
               for an agent whose sessions have no files of their own
  :transcript  function of a session returning a file of its transcript;
               by default, its :file
  :worktree    function of a directory returning nil, or where that
               worktree of the agent's belongs: (:root REPOSITORY :label
               LABEL), or (:name NAME :label LABEL) when only the
               repository's name is known
  :chats       directories whose sessions are chats, not project work
  :short-id    function of a session's id returning the short form that
               tells it from others; by default its first eight characters

An agent run as a CLI in a terminal has these:

  :program     its executable; while it is not on PATH, the agent takes
               part nowhere
  :new-arguments     function of no argument returning (ID . ARGUMENTS)
                     starting a new session; ID is the session's id when
                     the agent takes one it is given, else nil
  :resume-arguments  function of a session's id returning ARGUMENTS
                     resuming it
  :arguments   function of a directory returning the arguments of every
               run there
  :context     function of a directory returning a server showing the
               agent Emacs's state, see `lispm-desktop-agent-context', or nil

An agent running inside Emacs has these instead:

  :new         function of a directory starting a session there; returns
               its buffer
  :resume      function of a session resuming it; returns its buffer
  :buffer-p    predicate on buffers holding one of its sessions
  :send        function of a buffer and text putting the text into the
               session's input, without sending it

Defining AGENT again replaces what it was.  Return AGENT."
  (if-let* ((entry (assq agent lispm-desktop-agents)))
      (progn (lispm-desktop-agent--unbind-key agent (cdr entry))
             (setcdr entry properties))
    (setq lispm-desktop-agents (append lispm-desktop-agents (list (cons agent properties)))))
  (lispm-desktop-agent--bind-key agent properties)
  (lispm-desktop-agent--changed)
  agent)

(defun lispm-desktop-agent-undefine (agent)
  "Make AGENT none of the desktop's agents any more.
Its terminals go on running, but the servers showing them Emacs's state
stop now, while the code stopping them is still loaded: an extension
undefines its agent as it is unloaded.  Nothing offers AGENT any more,
nor lists its sessions."
  (when-let* ((entry (assq agent lispm-desktop-agents)))
    (dolist (buffer (buffer-list))
      (when (eq (buffer-local-value 'lispm-desktop-agent-buffer-agent buffer) agent)
        (lispm-desktop-agent-context-release buffer)))
    (setq lispm-desktop-agents (delq entry lispm-desktop-agents))
    (lispm-desktop-agent--unbind-key agent (cdr entry))
    (lispm-desktop-agent--changed)))

(defvar lispm-desktop-agents-changed-hook nil
  "Hook run when what the agents list shows may have changed by itself: an
agent was defined or removed, or an agent changed its sessions, as a LispM
agent does.  The agents sidebar shows them anew once Emacs is idle.")

(defvar lispm-desktop-agent-sessions-changed-hook nil
  "Hook run once a session was shown, renamed, archived or deleted.
The agents sidebar shows it anew at once.")

(defun lispm-desktop-agent--changed ()
  "Forget what was worked out from the agents, and tell who shows them."
  (clrhash lispm-desktop-agent--root-cache)
  (run-hooks 'lispm-desktop-agents-changed-hook))

(defun lispm-desktop-agent--key-binding (agent properties)
  "Return the binding of AGENT's key, of PROPERTIES: its command while it
can run, nothing otherwise."
  `(menu-item ,(format "New %s session" (plist-get properties :name))
              ,(plist-get properties :command)
              :filter ,(lambda (command) (and (lispm-desktop-agent-available-p agent) command))))

(defvar lispm-desktop-agent--keys nil
  "The keys bound to start agents, as (KEY . AGENT).")

(defvar lispm-desktop-agent-key-maps '(lispm-desktop-agent-map)
  "Variables holding the keymaps agents' keys are bound in: \\`C-c a''s, and
those added with `lispm-desktop-agent-add-key-map', as the agents
sidebar's is.")

(defun lispm-desktop-agent--key-maps ()
  "Return the keymaps agents' keys are bound in, see
`lispm-desktop-agent-key-maps'."
  (mapcar #'symbol-value lispm-desktop-agent-key-maps))

(defun lispm-desktop-agent--key-taken-p (key)
  "Return non-nil when KEY starts a command of its own, not an agent, in a
keymap of `lispm-desktop-agent--key-maps'; what a keymap's parent binds
does not count."
  (let ((event (aref (key-parse key) 0))
        (taken nil))
    (dolist (map (lispm-desktop-agent--key-maps))
      (map-keymap-internal (lambda (bound binding)
                             (when (and binding (eq bound event))
                               (setq taken t)))
                           map))
    taken))

(defun lispm-desktop-agent--bind-key (agent properties)
  "Bind AGENT's key, of PROPERTIES, under \\`C-c a' and in the agents
sidebar; one another agent has is taken over, one starting another
command is left to it."
  (when-let* ((key (plist-get properties :key)))
    (if (and (not (assoc key lispm-desktop-agent--keys))
             (lispm-desktop-agent--key-taken-p key))
        (message "%s gets no key: %s is taken" (plist-get properties :name) key)
      (setf (alist-get key lispm-desktop-agent--keys nil nil #'equal) agent)
      (dolist (map (lispm-desktop-agent--key-maps))
        (keymap-set map key (lispm-desktop-agent--key-binding agent properties))))))

(defun lispm-desktop-agent--unbind-key (agent properties)
  "Unbind the key of AGENT, of PROPERTIES, when it is AGENT's; an agent
still defined that wants it gets it."
  (when-let* ((key (plist-get properties :key))
              ((eq (alist-get key lispm-desktop-agent--keys nil nil #'equal) agent)))
    (setq lispm-desktop-agent--keys (assoc-delete-all key lispm-desktop-agent--keys))
    (dolist (map (lispm-desktop-agent--key-maps))
      (keymap-unset map key t))
    (when-let* ((other (seq-find (lambda (entry)
                                   (and (not (eq (car entry) agent))
                                        (equal (plist-get (cdr entry) :key) key)))
                                 lispm-desktop-agents)))
      (lispm-desktop-agent--bind-key (car other) (cdr other)))))

(defun lispm-desktop-agent-add-key-map (variable)
  "Bind the agents' keys in the keymap VARIABLE holds too, from now on, as
the agents sidebar does as it is loaded."
  (add-to-list 'lispm-desktop-agent-key-maps variable t)
  (pcase-dolist (`(,key . ,agent) lispm-desktop-agent--keys)
    (keymap-set (symbol-value variable) key
                (lispm-desktop-agent--key-binding agent (lispm-desktop-agent-props agent)))))

(defun lispm-desktop-agent-props (agent)
  "Return the properties of AGENT, or nil when it is no agent."
  (alist-get agent lispm-desktop-agents))

(defun lispm-desktop-agent-name (agent)
  "Return the name of AGENT."
  (or (plist-get (lispm-desktop-agent-props agent) :name)
      (capitalize (symbol-name agent))))

(defun lispm-desktop-agent-available-p (agent)
  "Return non-nil when AGENT is an agent that can run here.
One run as a CLI can while its program is on PATH."
  (when-let* ((props (lispm-desktop-agent-props agent)))
    (let ((program (plist-get props :program)))
      (or (null program) (and (executable-find program) t)))))

(defun lispm-desktop-agent--ordered ()
  "Return the agents, as (AGENT . PROPERTIES): those built in first, then
the others, each in the order they were defined."
  (append (seq-filter (lambda (entry) (plist-get (cdr entry) :built-in)) lispm-desktop-agents)
          (seq-remove (lambda (entry) (plist-get (cdr entry) :built-in)) lispm-desktop-agents)))

(defun lispm-desktop-agent-available ()
  "Return the agents that can run here, as (AGENT . PROPERTIES): those
built in first, then the others in the order they were defined."
  (seq-filter (lambda (entry) (lispm-desktop-agent-available-p (car entry)))
              (lispm-desktop-agent--ordered)))

(defun lispm-desktop-agent--check-available (agent)
  "Return the properties of AGENT; signal a `user-error' unless it can run."
  (let ((props (or (lispm-desktop-agent-props agent)
                   (user-error "There is no agent %s; the App Store adds agents" agent))))
    (unless (lispm-desktop-agent-available-p agent)
      (user-error "%s is not installed: `%s' is not on PATH"
                  (plist-get props :name) (plist-get props :program)))
    props))

(defun lispm-desktop-agent-emacs-buffer-agent (buffer)
  "Return the in-Emacs agent whose session BUFFER holds, or nil."
  (car (seq-find (lambda (entry)
                   (when-let* ((buffer-p (plist-get (cdr entry) :buffer-p)))
                     (funcall buffer-p buffer)))
                 lispm-desktop-agents)))

(defconst lispm-desktop-agent-transcript-head-bytes 65536
  "Bytes read from the start of a transcript to find its metadata and first prompt.")

(defconst lispm-desktop-agent-transcript-tail-bytes 16384
  "Bytes read from the end of a transcript to find a title set late in the session.")

(defconst lispm-desktop-agent-title-width 72
  "Maximum width of a session title in the session list.")

;;;; Transcript reading (effects)

(defun lispm-desktop-agent-read-json-region (file beg end)
  "Return the JSON objects on the complete lines of FILE between BEG and END.
A line cut by BEG or END (when END stops short of the file's size) is skipped,
as is any line that is not valid JSON.  Objects are plists with list arrays."
  (let ((truncated (< end (file-attribute-size (file-attributes file)))))
    (with-temp-buffer
      (insert-file-contents file nil beg end)
      (goto-char (point-min))
      (when (> beg 0) (forward-line 1))
      (lispm-desktop-agent--parse-json-lines truncated))))

(defun lispm-desktop-agent--parse-json-lines (truncated)
  "Return the JSON objects on the lines from point to the end of the buffer.
When TRUNCATED, the last line is partial and is skipped."
  (let (objects)
    (while (not (eobp))
      (let ((line (unless (and truncated (= (line-end-position) (point-max)))
                    (buffer-substring-no-properties (point) (line-end-position)))))
        (when-let* ((object (and line
                                 (ignore-errors
                                   (json-parse-string line
                                                      :object-type 'plist
                                                      :array-type 'list
                                                      :null-object nil
                                                      :false-object nil)))))
          (push object objects)))
      (forward-line 1))
    (nreverse objects)))

(defun lispm-desktop-agent-read-transcript (file)
  "Return the JSON objects at the head and tail of transcript FILE as a cons."
  (let ((size (file-attribute-size (file-attributes file))))
    (cons (lispm-desktop-agent-read-json-region
           file 0 (min size lispm-desktop-agent-transcript-head-bytes))
          (when (> size lispm-desktop-agent-transcript-head-bytes)
            (lispm-desktop-agent-read-json-region
             file (max lispm-desktop-agent-transcript-head-bytes
                       (- size lispm-desktop-agent-transcript-tail-bytes))
             size)))))

(defun lispm-desktop-agent-file-time (file)
  "Return the modification time of FILE as a float."
  (float-time (file-attribute-modification-time (file-attributes file))))

(defun lispm-desktop-agent-append-json-line (file object)
  "Append OBJECT, a plist, to JSON Lines FILE as one UTF-8 line."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (concat (decode-coding-string (json-serialize object) 'utf-8) "\n")
                  nil file t 'silent)))

;;;; Transcript parsing (pure)

(defun lispm-desktop-agent-human-text-p (text)
  "Return non-nil when TEXT reads as a prompt a person typed.
Agents inject context, command echoes and reminders as tagged blocks that
start with `<', and Codex injects project guidance headed `# AGENTS.md';
those are not prompts."
  (and (stringp text)
       (let ((trimmed (string-trim-left text)))
         (not (or (string-empty-p trimmed)
                  (string-prefix-p "<" trimmed)
                  (string-prefix-p "# AGENTS.md" trimmed))))))

(defun lispm-desktop-agent-content-text (content)
  "Return the text of message CONTENT, a string or a list of typed parts."
  (if (stringp content)
      content
    (mapconcat (lambda (part) (or (plist-get part :text) "")) content "")))

;;;; Session discovery

(defvar lispm-desktop-agent--session-cache (make-hash-table :test #'equal)
  "Memoized sessions keyed by transcript file, valued (MTIME . SESSION-OR-NIL).")

(defun lispm-desktop-agent-cached-session (file parse)
  "Return the session of transcript FILE, calling PARSE when it changed.
PARSE maps the head and tail objects of FILE, as
`lispm-desktop-agent-read-transcript' returns them, to a session plist or
nil; the session's :time is FILE's modification time."
  (let ((time (lispm-desktop-agent-file-time file))
        (cached (gethash file lispm-desktop-agent--session-cache)))
    (if (and cached (= (car cached) time))
        (cdr cached)
      (let ((session (when-let* ((parsed (funcall parse (lispm-desktop-agent-read-transcript file))))
                       (plist-put parsed :time time))))
        (puthash file (cons time session) lispm-desktop-agent--session-cache)
        session))))

(defun lispm-desktop-agent--newest-per-id (sessions)
  "Return SESSIONS keeping only the most recent transcript of each agent id.
Codex writes forked threads to separate files that share the session id."
  (let ((latest (make-hash-table :test #'equal)))
    (dolist (session sessions)
      (let* ((key (cons (plist-get session :agent) (plist-get session :id)))
             (seen (gethash key latest)))
        (when (or (null seen) (> (plist-get session :time) (plist-get seen :time)))
          (puthash key session latest))))
    (hash-table-values latest)))

(defun lispm-desktop-agent-sessions-list ()
  "Return every recorded session of the agents that can run here, newest
first, see `lispm-desktop-agent-available'.  An agent whose sessions cannot
be read lists none, and says so."
  (sort (lispm-desktop-agent--newest-per-id
         (mapcan (pcase-lambda (`(,_ . ,props))
                   (condition-case err
                       (delq nil (copy-sequence (funcall (plist-get props :sessions))))
                     (error (message "%s sessions unavailable: %s" (plist-get props :name)
                                     (error-message-string err))
                            nil)))
                 (lispm-desktop-agent-available)))
        (lambda (a b) (> (plist-get a :time) (plist-get b :time)))))

;;;; Presentation (pure)

(defun lispm-desktop-agent-age (time now)
  "Return a short human age for TIME relative to NOW, both float seconds.
Past a month it is the date, short as Mac OS 9's Finder wrote it: 5/13/26."
  (let ((seconds (- now time)))
    (cond ((< seconds 3600) (format "%dm" (/ seconds 60)))
          ((< seconds 86400) (format "%dh" (/ seconds 3600)))
          ((< seconds (* 30 86400)) (format "%dd" (/ seconds 86400)))
          (t (format-time-string "%-m/%-d/%y" time)))))

(defun lispm-desktop-agent--short-id (session)
  "Return the short form of SESSION's id that tells sessions apart: what
its agent's :short-id makes of it, else its first eight characters, as
of a UUID."
  (let ((id (plist-get session :id)))
    (if-let* ((short (plist-get (lispm-desktop-agent-props (plist-get session :agent)) :short-id)))
        (funcall short id)
      (substring id 0 (min 8 (length id))))))

(defconst lispm-desktop-agent-label-faces
  '(font-lock-function-name-face font-lock-keyword-face font-lock-type-face
    font-lock-constant-face font-lock-variable-name-face)
  "Faces telling the agents apart in the session list, one per agent in turn.")

(defun lispm-desktop-agent--label-face (agent)
  "Return the face of AGENT's name in the session list: the agents built
in keep theirs, whichever extensions were loaded before them."
  (let ((index (or (seq-position (mapcar #'car (lispm-desktop-agent--ordered)) agent) 0)))
    (nth (mod index (length lispm-desktop-agent-label-faces)) lispm-desktop-agent-label-faces)))

(defun lispm-desktop-agent--session-label (session now)
  "Return the completion label for SESSION, with ages relative to NOW.
The short id suffix keeps labels unique across sessions with equal titles."
  (format "%s %s %s %s %s"
          (propertize (format "%-6s" (plist-get session :agent))
                      'face (lispm-desktop-agent--label-face (plist-get session :agent)))
          (propertize (format "%-18s" (truncate-string-to-width
                                       (file-name-nondirectory
                                        (directory-file-name (plist-get session :cwd)))
                                       18))
                      'face 'font-lock-constant-face)
          (propertize (format "%-10s" (lispm-desktop-agent-age (plist-get session :time) now))
                      'face 'shadow)
          (lispm-desktop-sidebar-one-line (plist-get session :title) lispm-desktop-agent-title-width)
          (propertize (lispm-desktop-agent--short-id session) 'face 'shadow)))

;;;; Terminals (effects)

(defvar-local lispm-desktop-agent-buffer-agent nil
  "The agent this terminal runs, a key of `lispm-desktop-agents'.")
(put 'lispm-desktop-agent-buffer-agent 'permanent-local t)

(defun lispm-desktop-agent-terminal-agent (buffer)
  "Return the agent the terminal BUFFER runs, or nil when it runs none.
A terminal an agent was started in says so, see `lispm-desktop-agent--launch';
any other is taken by its name, \"*AGENT:...*\", as agent terminals are named."
  (and (memq (buffer-local-value 'major-mode buffer) '(eat-mode lispm-desktop-term-mode))
       (let ((agent (or (buffer-local-value 'lispm-desktop-agent-buffer-agent buffer)
                        ;; Asked while tab labels are drawn: the match data stays.
                        (save-match-data
                          (and (string-match "\\`\\*\\([a-z]+\\):" (buffer-name buffer))
                               (intern (match-string 1 (buffer-name buffer))))))))
         (and (plist-get (lispm-desktop-agent-props agent) :program) agent))))

(defun lispm-desktop-agent-buffer-p (buffer)
  "Return non-nil when BUFFER holds an agent session.
That is a terminal an agent runs in, or the buffer of a session of an
agent running inside Emacs."
  (and (buffer-live-p buffer)
       (or (lispm-desktop-agent-terminal-agent buffer)
           (lispm-desktop-agent-emacs-buffer-agent buffer))
       t))

(defun lispm-desktop-agent-running-p (buffer)
  "Return non-nil when BUFFER exists and still runs a live process.
The session of an agent running inside Emacs runs while its buffer lives."
  (if (and buffer (lispm-desktop-agent-emacs-buffer-agent buffer))
      (buffer-live-p buffer)
    (and buffer (lispm-desktop-terminal-live-p buffer))))

(defvar-local lispm-desktop-agent-buffer-session-id nil
  "Id of the session this agent terminal runs, when known.")
(put 'lispm-desktop-agent-buffer-session-id 'permanent-local t)

(defvar-local lispm-desktop-agent-buffer-launched nil
  "Time, float seconds, this agent terminal was started.")
(put 'lispm-desktop-agent-buffer-launched 'permanent-local t)

(defvar-local lispm-desktop-agent-buffer-title nil
  "Human-readable title of the session this agent terminal runs.
Tab labels show it instead of the terminal's machine-oriented buffer name.")
(put 'lispm-desktop-agent-buffer-title 'permanent-local t)

(defun lispm-desktop-agent-uuid ()
  "Return a random version 4 UUID string."
  (let ((bytes (mapcar (lambda (_) (random 256)) (number-sequence 1 16))))
    (setf (nth 6 bytes) (logior #x40 (logand (nth 6 bytes) #x0f))
          (nth 8 bytes) (logior #x80 (logand (nth 8 bytes) #x3f)))
    (apply #'format "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x"
           bytes)))

(defconst lispm-desktop-agent-session-marker-regexp
  (concat "\\`\\(CLAUDECODE\\|CLAUDE_PID\\|CLAUDE_EFFORT"
          "\\|CLAUDE_CODE_[A-Z0-9_]*\\|CLAUDE_AGENT_SDK_[A-Z0-9_]*"
          "\\|CLAUDE_PREVIEW_[A-Z0-9_]*\\)=")
  "Environment entries that tie a process to an enclosing Claude Code session.
An Emacs started from inside Claude Code inherits them; a Claude launched with
them believes it is that session's child, disables its own transcript and
reuses the parent's session id and messaging socket.")

(defun lispm-desktop-agent-independent-environment (environment)
  "Return ENVIRONMENT without enclosing Claude Code session markers.
Agents and shells started from Emacs are independent top-level sessions."
  (seq-remove (lambda (entry) (string-match-p lispm-desktop-agent-session-marker-regexp entry))
              environment))

(defun lispm-desktop-agent--launch (agent directory name args &optional id title)
  "Run AGENT with ARGS in DIRECTORY inside the terminal NAME; return it.
The terminal is a lispm-desktop-term once its module is built, see
`lispm-desktop-terminal-make', else an Eat terminal.
The terminal records the session ID and TITLE for labels and the sidebar.
The agent runs as an independent session whatever environment Emacs itself
inherited, and with its :context server showing it Emacs's state and its
:arguments, see `lispm-desktop-agent-define'.  A running terminal with the
same NAME is returned instead of restarted."
  (let ((props (lispm-desktop-agent--check-available agent)))
    (unless (file-directory-p directory)
      (user-error "Working directory no longer exists: %s" directory))
    (let* ((default-directory (file-name-as-directory directory))
           ;; A new agent gets a server showing it Emacs's state; a running
           ;; terminal returned as is keeps the one it has.
           (server (unless (lispm-desktop-agent-running-p (get-buffer (format "*%s*" name)))
                     (when-let* ((context (plist-get props :context)))
                       (funcall context default-directory))))
           (process-environment (append (and server (lispm-desktop-agent-context-environment server))
                                        (lispm-desktop-agent-independent-environment process-environment)))
           (buffer (apply #'lispm-desktop-terminal-make name (plist-get props :program)
                          (append (and server (lispm-desktop-agent-context-arguments server))
                                  (when-let* ((arguments (plist-get props :arguments)))
                                    (funcall arguments default-directory))
                                  args))))
      (when server
        (lispm-desktop-agent-context-bind server buffer))
      (with-current-buffer buffer
        (setq lispm-desktop-agent-buffer-agent agent
              lispm-desktop-agent-buffer-session-id (or id lispm-desktop-agent-buffer-session-id)
              lispm-desktop-agent-buffer-title (or title lispm-desktop-agent-buffer-title)
              lispm-desktop-agent-buffer-launched (or lispm-desktop-agent-buffer-launched (float-time))))
      ;; Agents record their sessions as they go, so closing the terminal
      ;; loses nothing a resume cannot restore: kill it without asking.
      (when-let* ((process (get-buffer-process buffer)))
        (set-process-query-on-exit-flag process nil))
      buffer)))

(defun lispm-desktop-agent-buffer-label (buffer)
  "Return the tab label of BUFFER when it is an agent terminal, else nil."
  (when (lispm-desktop-agent-buffer-p buffer)
    (or (buffer-local-value 'lispm-desktop-agent-buffer-title buffer)
        (string-trim (buffer-name buffer) "\\*" "\\*"))))

(defconst lispm-desktop-agent-claim-slack 5
  "Seconds a session may seem to start before its terminal did.
The terminal's launch is stamped by Emacs and the session's start by the
agent; the slack absorbs their clocks' rounding.")

(defun lispm-desktop-agent-claim-sessions (sessions)
  "Tie running agent terminals that do not know their session to SESSIONS.
Codex assigns its session id itself, so a terminal started fresh learns it
from the transcripts: its session is the earliest one of its agent that
began in the terminal's directory at or after its launch and that no other
terminal runs.  Terminals claim in launch order, so two started in one
directory take the sessions in the order they began."
  (let* ((running (lispm-desktop-agent-running-buffers))
         (claimed (delq nil (mapcar (lambda (buffer)
                                      (buffer-local-value 'lispm-desktop-agent-buffer-session-id buffer))
                                    running)))
         (candidates (sort (seq-filter (lambda (session) (plist-get session :started))
                                       sessions)
                           (lambda (a b) (< (plist-get a :started) (plist-get b :started))))))
    (dolist (buffer (sort (seq-filter
                           (lambda (buffer)
                             (and (null (buffer-local-value 'lispm-desktop-agent-buffer-session-id buffer))
                                  (buffer-local-value 'lispm-desktop-agent-buffer-launched buffer)))
                           running)
                          (lambda (a b) (< (buffer-local-value 'lispm-desktop-agent-buffer-launched a)
                                           (buffer-local-value 'lispm-desktop-agent-buffer-launched b)))))
      (with-current-buffer buffer
        (let ((directory (directory-file-name (expand-file-name default-directory)))
              (agent (lispm-desktop-agent-of-buffer buffer)))
          (when-let* ((session
                       (seq-find
                        (lambda (session)
                          (and (eq (plist-get session :agent) agent)
                               (not (member (plist-get session :id) claimed))
                               (>= (plist-get session :started)
                                   (- lispm-desktop-agent-buffer-launched lispm-desktop-agent-claim-slack))
                               (equal (directory-file-name
                                       (expand-file-name (plist-get session :cwd)))
                                      directory)))
                        candidates)))
            (setq lispm-desktop-agent-buffer-session-id (plist-get session :id))
            (push (plist-get session :id) claimed)))))))

(defun lispm-desktop-agent-refresh-buffer-titles (sessions)
  "Update the titles of running agent terminals from SESSIONS.
Terminals first claim their sessions, see
`lispm-desktop-agent-claim-sessions'.  A new session gains its real title
once its first prompt is recorded, and a renamed session follows its new name."
  (lispm-desktop-agent-claim-sessions sessions)
  (let ((titles (make-hash-table :test #'equal)))
    (dolist (session sessions)
      (puthash (plist-get session :id) (plist-get session :title) titles))
    (dolist (buffer (lispm-desktop-agent-running-buffers))
      (with-current-buffer buffer
        (when-let* ((title (and lispm-desktop-agent-buffer-session-id
                                (gethash lispm-desktop-agent-buffer-session-id titles))))
          (setq lispm-desktop-agent-buffer-title title))))))

(defun lispm-desktop-agent--workspace-name (directory)
  "Return the workspace owning sessions run in DIRECTORY.
Sessions belong to their project root, so worktrees and subdirectories share
the repository's workspace; project-less chats share the default workspace."
  (let ((root (lispm-desktop-agent--project-root directory)))
    (if (eq root 'chats)
        (cons tabspaces-default-tab nil)
      (cons (file-name-nondirectory root) root))))

(defun lispm-desktop-agent-show (buffer)
  "Show agent terminal BUFFER in its project's workspace and select it.
In a workspace just created for it, BUFFER is shown alone; otherwise it is
placed as `lispm-desktop-sidebar-show-in-main' places terminals.  The agent
sidebar is refreshed, so the terminal appears in its running list."
  (pcase-let ((`(,name . ,root) (lispm-desktop-agent--workspace-name
                                 (buffer-local-value 'default-directory buffer))))
    (if (lispm-desktop-workspace-enter name root)
        (lispm-desktop-workspace-show-alone buffer)
      (lispm-desktop-sidebar-show-in-main buffer)))
  (run-hooks 'lispm-desktop-agent-sessions-changed-hook)
  buffer)

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
      (user-error "No terminal is waiting for you"))
    (if (lispm-desktop-agent-buffer-p next)
        (lispm-desktop-agent-show next)
      (lispm-desktop-sidebar-show-in-main next))))

(defun lispm-desktop-agent-project-directory ()
  "Return the current project root, or `default-directory' outside projects."
  (if-let* ((project (project-current)))
      (project-root project)
    default-directory))

(defun lispm-desktop-agent-default-directory ()
  "Return where a new session or shell starts: the project root of the main
window's buffer."
  (with-selected-window (lispm-desktop-platinum-main-window)
    (lispm-desktop-agent-project-directory)))

(defun lispm-desktop-agent-fresh-name (agent directory)
  "Return an unused terminal name for a new AGENT session in DIRECTORY."
  (let* ((base (format "%s:%s" agent
                       (file-name-nondirectory (directory-file-name directory))))
         (name base)
         (n 1))
    (while (lispm-desktop-agent-running-p (get-buffer (concat "*" name "*")))
      (setq n (1+ n)
            name (format "%s<%d>" base n)))
    name))

;;;; Commands

(defvar lispm-desktop-agent--candidates nil
  "Session plists of the last session listing, keyed by their labels.")

(defun lispm-desktop-agent--session-at (label)
  "Return the session plist listed under LABEL."
  (or (and lispm-desktop-agent--candidates (gethash label lispm-desktop-agent--candidates))
      (user-error "Unknown agent session: %s" label)))

(defun lispm-desktop-agent--read-session (prompt)
  "Read a recorded agent session with PROMPT and return its plist."
  (let* ((now (float-time))
         (table (make-hash-table :test #'equal))
         (labels (mapcar (lambda (session)
                           (let ((label (lispm-desktop-agent--session-label session now)))
                             (puthash label session table)
                             label))
                         (lispm-desktop-agent-sessions-list))))
    (setq lispm-desktop-agent--candidates table)
    (lispm-desktop-agent--session-at
     (completing-read prompt
                      (lambda (string pred action)
                        (if (eq action 'metadata)
                            '(metadata (category . lispm-desktop-agent-session)
                                       (display-sort-function . identity))
                          (complete-with-action action labels string pred)))
                      nil t))))

(defun lispm-desktop-agent-resume-session (session)
  "Resume SESSION in its original working directory.
A session already running in a terminal is shown there instead: resuming it
again would run a second agent on the same session."
  (let ((agent (plist-get session :agent))
        (id (plist-get session :id))
        (cwd (plist-get session :cwd)))
    (lispm-desktop-agent-show
     (or (lispm-desktop-agent-session-buffer id)
         (let ((props (lispm-desktop-agent--check-available agent)))
           (if-let* ((resume (plist-get props :resume)))
               (funcall resume session)
             (lispm-desktop-agent--launch agent cwd
                                          (format "%s:%s:%s" agent
                                                  (file-name-nondirectory (directory-file-name cwd))
                                                  (lispm-desktop-agent--short-id session))
                                          (funcall (plist-get props :resume-arguments) id)
                                          id
                                          (plist-get session :title))))))))

(defun lispm-desktop-agent-sessions ()
  "Choose a recorded session of any agent and resume it."
  (interactive)
  (lispm-desktop-agent-resume-session (lispm-desktop-agent--read-session "Agent session: ")))

(defun lispm-desktop-agent-new (agent &optional directory)
  "Start a new AGENT session in DIRECTORY.
DIRECTORY defaults to the project root of the main window's buffer."
  (let* ((props (lispm-desktop-agent--check-available agent))
         (directory (or directory (lispm-desktop-agent-default-directory))))
    (lispm-desktop-agent-show
     (if-let* ((new (plist-get props :new)))
         (funcall new directory)
       (pcase-let ((`(,id . ,args) (when-let* ((new-arguments (plist-get props :new-arguments)))
                                     (funcall new-arguments))))
         (lispm-desktop-agent--launch agent directory (lispm-desktop-agent-fresh-name agent directory)
                                      args id
                                      (format "New %s · %s" (plist-get props :name)
                                              (file-name-nondirectory (directory-file-name directory)))))))))

(defun lispm-desktop-agent--pick-terminal ()
  "Return the agent terminal to send to, preferring the current project.
Ask when several running agent terminals qualify."
  (let* ((root (lispm-desktop-agent-project-directory))
         (running (seq-filter (lambda (buffer)
                                (and (lispm-desktop-agent-buffer-p buffer)
                                     (lispm-desktop-agent-running-p buffer)))
                              (buffer-list)))
         (local (seq-filter (lambda (buffer)
                              (file-in-directory-p
                               (buffer-local-value 'default-directory buffer) root))
                            running))
         (candidates (or local running)))
    (pcase candidates
      ('nil (user-error "No running agent; start one from the λ menu%s"
                        (mapconcat (lambda (entry)
                                     (if-let* ((key (plist-get (cdr entry) :key)))
                                         (format " or with C-c a %s" key)
                                       ""))
                                   (lispm-desktop-agent-available) "")))
      (`(,only) only)
      (_ (get-buffer (completing-read "Send to agent: "
                                      (mapcar #'buffer-name candidates) nil t))))))

(defun lispm-desktop-agent--region-reference (beg end)
  "Return a fenced reference to the text between BEG and END of this buffer.
The header names the file relative to the project root and the line range."
  (let* ((file (if buffer-file-name
                   (file-relative-name buffer-file-name (lispm-desktop-agent-project-directory))
                 (buffer-name)))
         (first (line-number-at-pos beg))
         (last (line-number-at-pos (if (and (> end beg) (= (char-before end) ?\n))
                                       (1- end)
                                     end)))
         (lang (string-remove-suffix
                "-ts" (string-remove-suffix "-mode" (symbol-name major-mode)))))
    (format "%s:%d-%d\n```%s\n%s\n```\n"
            file first last lang
            (string-trim-right (buffer-substring-no-properties beg end)))))

(defun lispm-desktop-agent-send-region (beg end)
  "Send the region between BEG and END, with its location, to an agent terminal.
Without an active region, send a reference to the current file and line.
The text is pasted into the agent's composer without submitting it."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let ((text (if beg
                  (lispm-desktop-agent--region-reference beg end)
                (format "%s:%d "
                        (if buffer-file-name
                            (file-relative-name buffer-file-name
                                                (lispm-desktop-agent-project-directory))
                          (buffer-name))
                        (line-number-at-pos))))
        (target (lispm-desktop-agent--pick-terminal)))
    (deactivate-mark)
    (if-let* ((send (plist-get (lispm-desktop-agent-props (lispm-desktop-agent-emacs-buffer-agent target))
                               :send)))
        (funcall send target text)
      (lispm-desktop-terminal-paste target text))
    (pop-to-buffer target)))

;;;; Session management

(defconst lispm-desktop-agent-state-file (locate-user-emacs-file "agent-sessions.eld")
  "File holding Emacs-side session state, currently the archived sessions.
Archiving hides a session in Emacs only; it never alters agent data.")

(defvar lispm-desktop-agent--archived 'unloaded
  "Keys of archived sessions, or `unloaded' before the state file is read.")

(defun lispm-desktop-agent--session-key (session)
  "Return the key identifying SESSION across agents, \"AGENT:ID\"."
  (format "%s:%s" (plist-get session :agent) (plist-get session :id)))

(defun lispm-desktop-agent--read-state ()
  "Return the plist the state file holds, or nil without one.
A file holding no such plist is moved aside, to the same name ending in
.unreadable, and said so, for the next write would replace it."
  (let ((file lispm-desktop-agent-state-file))
    (when (file-readable-p file)
      (let ((state (with-temp-buffer
                     (insert-file-contents file)
                     (condition-case nil (read (current-buffer)) (error 'unreadable)))))
        (if (and (plistp state) (listp (plist-get state :archived)))
            state
          (let ((aside (concat file ".unreadable")))
            (rename-file file aside t)
            (message "Cannot read %s; moved to %s"
                     (abbreviate-file-name file) (abbreviate-file-name aside)))
          nil)))))

(defun lispm-desktop-agent--archived-keys ()
  "Return the archived session keys, reading the state file on first use."
  (when (eq lispm-desktop-agent--archived 'unloaded)
    (setq lispm-desktop-agent--archived
          (plist-get (lispm-desktop-agent--read-state) :archived)))
  lispm-desktop-agent--archived)

(defun lispm-desktop-agent-archived-p (session)
  "Return non-nil when SESSION is archived."
  (member (lispm-desktop-agent--session-key session) (lispm-desktop-agent--archived-keys)))

(defun lispm-desktop-agent--set-archived (session archived)
  "Mark SESSION ARCHIVED or not and persist the archived set."
  (let ((key (lispm-desktop-agent--session-key session))
        (keys (lispm-desktop-agent--archived-keys)))
    (setq lispm-desktop-agent--archived (if archived
                                            (cons key (delete key keys))
                                         (delete key keys)))
    (let ((part (concat lispm-desktop-agent-state-file ".part")))
      (with-temp-file part
        (let ((print-length nil) (print-level nil))
          (prin1 (list :archived lispm-desktop-agent--archived) (current-buffer))))
      ;; Renamed into place, so the file is there whole or not at all.
      (rename-file part lispm-desktop-agent-state-file t))))

(defun lispm-desktop-agent--rename (session title)
  "Rename SESSION to TITLE the way its own agent records names, see the
:rename of `lispm-desktop-agent-define'.  This list then shows TITLE."
  (let ((agent (plist-get session :agent)))
    (funcall (or (plist-get (lispm-desktop-agent-props agent) :rename)
                 (user-error "%s sessions cannot be renamed" (lispm-desktop-agent-name agent)))
             session title)))

(defun lispm-desktop-agent--session-files (session)
  "Return every file and directory recording SESSION, as its agent's :files
says; none for an agent keeping its sessions in a store of its own."
  (when-let* ((files (plist-get (lispm-desktop-agent-props (plist-get session :agent)) :files)))
    (funcall files session)))

(defun lispm-desktop-agent--session-transcript (session)
  "Return a file holding SESSION's transcript: what its agent's :transcript
makes, else its :file."
  (if-let* ((transcript (plist-get (lispm-desktop-agent-props (plist-get session :agent)) :transcript)))
      (funcall transcript session)
    (plist-get session :file)))

(defun lispm-desktop-agent--session-running-p (session)
  "Return non-nil when SESSION runs in a live agent terminal."
  (lispm-desktop-agent-session-buffer (plist-get session :id)))

(defun lispm-desktop-agent-rename-session (session)
  "Prompt for a new title of SESSION and rename it."
  (let ((title (string-trim (read-string "Rename session: "
                                         (plist-get session :title)))))
    (when (string-empty-p title)
      (user-error "Title must not be empty"))
    (lispm-desktop-agent--rename session title)
    (run-hooks 'lispm-desktop-agent-sessions-changed-hook)))

(defun lispm-desktop-agent-toggle-archive-session (session)
  "Archive SESSION, or unarchive it when it is archived."
  (lispm-desktop-agent--set-archived session (not (lispm-desktop-agent-archived-p session)))
  (run-hooks 'lispm-desktop-agent-sessions-changed-hook))

(defun lispm-desktop-agent-delete-session (session)
  "Delete SESSION after confirmation.
The files recording it go to the system trash; an agent keeping its
sessions in a store of its own deletes it there itself, see the :delete of
`lispm-desktop-agent-define'.  A session that is running in a terminal
cannot be deleted."
  (when (lispm-desktop-agent--session-running-p session)
    (user-error "Session is running; exit its terminal first"))
  (let* ((agent (plist-get session :agent))
         (name (lispm-desktop-agent-name agent))
         (delete (plist-get (lispm-desktop-agent-props agent) :delete))
         (files (unless delete (lispm-desktop-agent--session-files session)))
         (title (lispm-desktop-sidebar-one-line (plist-get session :title) 40)))
    (when (eq 'delete
              (lispm-desktop-popup-alert
               'stop
               (if delete
                   (format "Delete the %s session “%s”?" name title)
                 (format "Move the %s session “%s” to the Trash?" name title))
               `(("Cancel" cancel cancel)
                 (,(if delete "Delete" "Move to Trash") delete default))
               (if delete
                   (format "%s removes it from its store; this cannot be undone." name)
                 (format "Its %d file%s go%s to the Trash." (length files)
                         (if (cdr files) "s" "") (if (cdr files) "" "es")))))
      (if delete
          (funcall delete session)
        (dolist (file files)
          (move-file-to-trash file)
          (remhash file lispm-desktop-agent--session-cache)))
      (lispm-desktop-agent--set-archived session nil)
      (run-hooks 'lispm-desktop-agent-sessions-changed-hook))))

(defun lispm-desktop-agent--in-main-window (function &rest args)
  "Call FUNCTION with ARGS in the main window of this frame."
  (select-window (lispm-desktop-platinum-main-window))
  (apply function args))

(defun lispm-desktop-agent-session-act (action session)
  "Perform the context menu ACTION on SESSION."
  (pcase action
    ('resume (lispm-desktop-agent-resume-session session))
    ('rename (lispm-desktop-agent-rename-session session))
    ('archive (lispm-desktop-agent-toggle-archive-session session))
    ('delete (lispm-desktop-agent-delete-session session))
    ('copy (kill-new (plist-get session :id))
           (message "Copied %s" (plist-get session :id)))
    ('transcript (lispm-desktop-agent--in-main-window #'find-file (lispm-desktop-agent--session-transcript session)))
    ('directory (lispm-desktop-agent--in-main-window #'dired (plist-get session :cwd)))))

;;;; Embark actions on listed sessions

(defun lispm-desktop-agent-session-copy-id (label)
  "Copy the id of the agent session listed under LABEL."
  (lispm-desktop-agent-session-act 'copy (lispm-desktop-agent--session-at label)))

(defun lispm-desktop-agent-session-open-transcript (label)
  "Open the transcript of the agent session listed under LABEL."
  (lispm-desktop-agent-session-act 'transcript (lispm-desktop-agent--session-at label)))

(defun lispm-desktop-agent-session-open-directory (label)
  "Open the working directory of the agent session listed under LABEL."
  (lispm-desktop-agent-session-act 'directory (lispm-desktop-agent--session-at label)))

(defun lispm-desktop-agent-session-resume (label)
  "Resume the agent session listed under LABEL."
  (lispm-desktop-agent-session-act 'resume (lispm-desktop-agent--session-at label)))

(defvar-keymap lispm-desktop-agent-session-embark-map
  :doc "Embark actions for agent sessions in the session list."
  "RET" #'lispm-desktop-agent-session-resume
  "w" #'lispm-desktop-agent-session-copy-id
  "f" #'lispm-desktop-agent-session-open-transcript
  "d" #'lispm-desktop-agent-session-open-directory)

;;;; Projects, running sessions and their marks

(defun lispm-desktop-agent--directory-key (directory)
  "Return DIRECTORY expanded, without trailing slash, abbreviated for display."
  (abbreviate-file-name (directory-file-name (expand-file-name directory))))

(defun lispm-desktop-agent--chat-directory-p (directory)
  "Return non-nil when DIRECTORY holds project-less chat sessions.
The home directory itself is a chat location; so is anything under an
agent's :chats directories, see `lispm-desktop-agent-define'."
  (let ((dir (directory-file-name (expand-file-name directory))))
    (or (string= dir (directory-file-name (expand-file-name "~")))
        (seq-some (lambda (entry)
                    (seq-some (lambda (chats)
                                (string-prefix-p (file-name-as-directory (expand-file-name chats))
                                                 (file-name-as-directory dir)))
                              (plist-get (cdr entry) :chats)))
                  lispm-desktop-agents))))

(defun lispm-desktop-agent--worktree (directory)
  "Return where DIRECTORY belongs when it is in one of an agent's worktrees,
as that agent's :worktree says, see `lispm-desktop-agent-define'; else nil."
  (seq-some (lambda (entry)
              (when-let* ((worktree (plist-get (cdr entry) :worktree)))
                (funcall worktree directory)))
            lispm-desktop-agents))

(defun lispm-desktop-agent--git-main-root (directory)
  "Return the main checkout root of the git repository containing DIRECTORY.
A linked worktree's `.git' is a file naming MAIN/.git/worktrees/NAME; follow
it to MAIN.  Return nil when DIRECTORY is missing or outside any repository."
  (when-let* (((file-directory-p directory))
              (root (locate-dominating-file directory ".git")))
    (let ((dotgit (expand-file-name ".git" root)))
      (or (and (file-regular-p dotgit)
               (with-temp-buffer
                 (insert-file-contents dotgit)
                 (when (re-search-forward "^gitdir: \\(.+?\\)/\\.git/worktrees/" nil t)
                   (match-string 1))))
          root))))

(defvar lispm-desktop-agent--root-cache (make-hash-table :test #'equal)
  "Memoized project roots keyed by working directory.")

(defun lispm-desktop-agent--project-root (directory)
  "Return the project root of working DIRECTORY, or `chats' for chat sessions.
An agent's worktree known to belong to a repository resolves to it, git
checkouts and their subdirectories to the main checkout root; other
directories stand alone."
  (let ((dir (directory-file-name (expand-file-name directory))))
    (with-memoization (gethash dir lispm-desktop-agent--root-cache)
      (if (lispm-desktop-agent--chat-directory-p dir)
          'chats
        (let ((base (or (plist-get (lispm-desktop-agent--worktree dir) :root) dir)))
          (directory-file-name (or (lispm-desktop-agent--git-main-root base) base)))))))

(defun lispm-desktop-agent-location-label (directory root)
  "Return where DIRECTORY sits inside project ROOT, or nil at the root itself.
Worktrees are named by their worktree, subdirectories by their relative path."
  (let ((dir (directory-file-name (expand-file-name directory))))
    (cond ((or (eq root 'chats) (string-equal-ignore-case dir root)) nil)
          ((plist-get (lispm-desktop-agent--worktree dir) :label))
          ((string-prefix-p (file-name-as-directory root) (file-name-as-directory dir))
           (file-relative-name dir root))
          (t (abbreviate-file-name dir)))))

(defun lispm-desktop-agent--resolve-roots (directories)
  "Return an alist mapping each of DIRECTORIES to its project root.
Roots come from `lispm-desktop-agent--project-root'.  An agent's worktree
known only by its repository's name, as a Codex worktree that no longer
exists and so cannot be followed through git, joins the known project
whose directory name is that name, when there is one."
  (let* ((roots (mapcar (lambda (dir) (cons dir (lispm-desktop-agent--project-root dir)))
                        directories))
         (by-name (make-hash-table :test #'equal)))
    (pcase-dolist (`(,_ . ,root) roots)
      (when (and (stringp root) (not (plist-get (lispm-desktop-agent--worktree root) :name)))
        (unless (gethash (file-name-nondirectory root) by-name)
          (puthash (file-name-nondirectory root) root by-name))))
    (mapcar (pcase-lambda (`(,dir . ,root))
              (cons dir (or (and (stringp root)
                                 (when-let* ((name (plist-get (lispm-desktop-agent--worktree root) :name)))
                                   (gethash name by-name)))
                            root)))
            roots)))

(defun lispm-desktop-agent-group-by-directory (sessions running)
  "Group SESSIONS and RUNNING terminals by project.
A project is the root from `lispm-desktop-agent--resolve-roots', so worktrees
and subdirectories join their repository and chat locations share one `chats'
group.  Return a list of plists
  (:key KEY :dir DIR :chats CHATS :running BUFFERS :sessions SESSIONS :time T)
where KEY folds case (macOS paths are case-insensitive), DIR is the root for
display, and T is the latest activity.  Projects with running terminals come
first, then by latest activity, and the chats group last.  SESSIONS are
newest first and keep that order inside each group."
  (let* ((directory-of (lambda (item)
                         (if (bufferp item)
                             (buffer-local-value 'default-directory item)
                           (plist-get item :cwd))))
         (roots (lispm-desktop-agent--resolve-roots
                 (delete-dups (mapcar directory-of (append running sessions)))))
         (groups (make-hash-table :test #'equal))
         (order nil))
    (cl-flet ((group (item)
                (let* ((root (cdr (assoc (funcall directory-of item) roots)))
                       (key (if (eq root 'chats) 'chats (downcase root))))
                  (or (gethash key groups)
                      (progn
                        (push key order)
                        (puthash key
                                 (list :key key
                                       :dir (if (eq root 'chats) "~"
                                              (lispm-desktop-agent--directory-key root))
                                       :chats (eq root 'chats)
                                       :running nil :sessions nil :time 0)
                                 groups))))))
      (dolist (buffer running)
        (let ((group (group buffer)))
          (plist-put group :running (cons buffer (plist-get group :running)))))
      (dolist (session sessions)
        (let ((group (group session)))
          (plist-put group :sessions (cons session (plist-get group :sessions)))
          (plist-put group :time (max (plist-get group :time)
                                      (plist-get session :time))))))
    (sort (mapcar (lambda (key)
                    (let ((group (gethash key groups)))
                      (plist-put group :running (nreverse (plist-get group :running)))
                      (plist-put group :sessions (nreverse (plist-get group :sessions)))))
                  order)
          (lambda (a b)
            (let ((ca (plist-get a :chats)) (cb (plist-get b :chats))
                  (ra (and (plist-get a :running) t))
                  (rb (and (plist-get b :running) t)))
              (cond ((not (eq ca cb)) cb)
                    ((not (eq ra rb)) ra)
                    (t (> (plist-get a :time) (plist-get b :time)))))))))

(defun lispm-desktop-agent-running-buffers ()
  "Return the live agent terminals, most recently used first."
  (seq-filter (lambda (buffer)
                (and (lispm-desktop-agent-buffer-p buffer) (lispm-desktop-agent-running-p buffer)))
              (buffer-list)))

(defun lispm-desktop-agent-running-ids (buffers)
  "Return the ids of the sessions agent terminal BUFFERS run, where known.
Ids are compared whole: Codex ids are time-ordered, so sessions begun within
about a minute of each other share their leading characters."
  (delq nil (mapcar (lambda (buffer)
                      (buffer-local-value 'lispm-desktop-agent-buffer-session-id buffer))
                    buffers)))

(defun lispm-desktop-agent-session-buffer (id)
  "Return the running agent terminal of the session with ID, or nil."
  (seq-find (lambda (buffer)
              (equal (buffer-local-value 'lispm-desktop-agent-buffer-session-id buffer) id))
            (lispm-desktop-agent-running-buffers)))

(defconst lispm-desktop-agent-icon-directory
  (expand-file-name "icons" lispm-desktop-directory)
  "Directory of the desktop's image icons of agents.")

(defun lispm-desktop-agent-image-icon (file &rest args)
  "Return FILE, an image, as a one-line icon of an agent.
FILE is absolute, or relative to `lispm-desktop-agent-icon-directory'.
ARGS are the nerd-icons keyword arguments: :height scales the icon relative
to the frame's line height, :face and :v-adjust are accepted for the sake of
callers that pass them to every icon function.  Where images cannot be
shown, a feather stands in for the image."
  (require 'nerd-icons)
  (let ((path (expand-file-name file lispm-desktop-agent-icon-directory)))
    (if (and (display-images-p) (file-readable-p path))
        (propertize " " 'display
                    (create-image path nil nil
                                  :height (round (* (or (plist-get args :height) 1.0)
                                                    (frame-char-height)))
                                  :ascent 'center))
      (apply #'nerd-icons-mdicon "nf-md-feather" args))))

(defun lispm-desktop-agent--icon-spec (agent)
  "Return (FUNCTION ICON FACE) of AGENT's icon, its :icon, see
`lispm-desktop-agent-define'; a robot for no agent known."
  (or (plist-get (lispm-desktop-agent-props agent) :icon)
      '(nerd-icons-codicon "nf-cod-robot" nil)))

(defun lispm-desktop-agent-icon (agent)
  "Return the brand icon of AGENT."
  (require 'nerd-icons)
  (pcase-let* ((`(,function ,name ,face) (lispm-desktop-agent--icon-spec agent))
               (icon (copy-sequence (funcall function name))))
    ;; Layer the colour under the icon's own face so its Nerd Font family
    ;; survives; replacing the face would fall back to an arbitrary CJK font.
    (when face
      (add-face-text-property 0 (length icon) face t icon))
    icon))

(defun lispm-desktop-agent-of-buffer (buffer)
  "Return the agent running in agent BUFFER, or nil when it is none.
A terminal knows its agent; an in-Emacs agent recognizes its own."
  (or (lispm-desktop-agent-emacs-buffer-agent buffer)
      (lispm-desktop-agent-terminal-agent buffer)))

(defun lispm-desktop-agent--buffer-icon (buffer)
  "Return the mark of the agent whose session BUFFER holds, or nil.
This is one of `lispm-desktop-platinum-buffer-icon-functions'."
  (when (lispm-desktop-agent-buffer-p buffer)
    (lispm-desktop-agent-icon (lispm-desktop-agent-of-buffer buffer))))

(defun lispm-desktop-setup-ai ()
  "Set up AI assistants."
  ;; A session's window shows its agent's mark, and its title.
  (add-hook 'lispm-desktop-platinum-buffer-icon-functions #'lispm-desktop-agent--buffer-icon)
  (add-hook 'lispm-desktop-platinum-buffer-title-functions #'lispm-desktop-agent-buffer-label)
  (keymap-global-set "C-c a" lispm-desktop-agent-map)
  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist
                 '(lispm-desktop-agent-session . lispm-desktop-agent-session-embark-map))))

(provide 'lispm-desktop-ai)
;;; lispm-desktop-ai.el ends here
