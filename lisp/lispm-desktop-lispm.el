;;; lispm-desktop-lispm.el --- LispM agents, built into the editor  -*- lexical-binding: t; -*-

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

;; LispM runs agents inside Emacs rather than as CLIs in terminals.  You
;; talk to one in a chat buffer laid out like Claude Code's; its session is
;; stored as an org document out of sight (/transcript shows it).  This
;; module makes it one of the editor's agents, built in
;; (`lispm-agents-define'), so it works as the others do:
;;
;; - C-c a l (and l in the agents sidebar) starts a LispM agent at the
;;   current project root; C-u C-c a l asks for its role;
;; - its sessions join the session list (C-c a s) and the agents sidebar,
;;   grouped by project, and resume from there;
;; - C-c a r puts a reference to the region into its input, as for any
;;   running agent;
;; - its chats are laid out as terminals, on the left of the main area,
;;   with the files they open on the right.
;;
;; LispM is lispm-agent, found or installed by lispm-desktop-deps.el: a
;; copy already on `load-path', else a checkout beside the desktop's, else
;; one package-vc installs from its repository.

;;; Code:

(require 'lispm-desktop-ai)
(require 'lispm-desktop-deps)
(require 'lispm-desktop-i18n)

(defvar lispm-desktop-conversation-modes)
(defvar lispm-interface)
(defvar lispm-chat-agent)
(defvar lispm-session-agent)
(declare-function lispm-sessions "lispm-runtime" ())
(declare-function lispm-agent-display-buffer "lispm" (agent))
(declare-function lispm-start "lispm-runtime" (&optional role root))
(declare-function lispm-resume-file "lispm-runtime" (file))
(declare-function lispm-rename-session "lispm-runtime" (file title))
(declare-function lispm-session-files "lispm-runtime" (id))
(declare-function lispm-language-changed "lispm-i18n" ())
(declare-function lispm-agent-id "lispm-core" (agent))
(declare-function lispm-agent-name "lispm-core" (agent))
(declare-function lispm-agent-view "lispm-core" (agent))
(declare-function lispm--read-role "lispm" (prompt))
(declare-function lispm-mode-line-mode "lispm" (&optional arg))

;;;; Sessions as lispm-desktop-ai sees them

(defun lispm-desktop-lispm--session (session)
  "Return the LispM SESSION description as a lispm-desktop-ai session plist."
  (list :agent 'lispm
        :id (plist-get session :id)
        :cwd (plist-get session :root)
        :title (plist-get session :title)
        :time (plist-get session :time)
        :file (plist-get session :file)))

(defun lispm-desktop-lispm-sessions ()
  "Return every recorded LispM session as lispm-desktop-ai session plists."
  (mapcar #'lispm-desktop-lispm--session (lispm-sessions)))

(defun lispm-desktop-lispm--compose (buffer)
  "Put point where BUFFER's next message is written; return BUFFER."
  (with-current-buffer buffer
    (goto-char (point-max)))
  buffer)

(defun lispm-desktop-lispm-new (directory &optional role)
  "Start a LispM agent of ROLE in DIRECTORY; return its chat."
  (lispm-desktop-lispm--compose (lispm-agent-display-buffer (lispm-start role directory))))

(defun lispm-desktop-lispm-resume (session)
  "Resume the LispM SESSION; return its chat."
  (lispm-desktop-lispm--compose
   (lispm-agent-display-buffer (lispm-resume-file (plist-get session :file)))))

(defun lispm-desktop-lispm-buffer-p (buffer)
  "Return non-nil when BUFFER is where a live LispM agent is talked to."
  (and (buffer-live-p buffer)
       (if (eq lispm-interface 'chat)
           (and (eq (buffer-local-value 'major-mode buffer) 'lispm-chat-mode)
                (buffer-local-value 'lispm-chat-agent buffer))
         (and (buffer-local-value 'lispm-session-mode buffer)
              (buffer-local-value 'lispm-session-agent buffer)))
       t))

(defun lispm-desktop-lispm-send (buffer text)
  "Put TEXT at the end of the input of LispM BUFFER; RET sends it."
  (with-current-buffer buffer
    (goto-char (point-max))
    (unless (or (bolp) (eq major-mode 'lispm-chat-mode)) (insert "\n"))
    (insert text)))

(defun lispm-desktop-lispm-rename (session title)
  "Give the LispM SESSION the TITLE shown in session lists."
  (lispm-rename-session (plist-get session :file) title))

(defun lispm-desktop-lispm-files (session)
  "Return the files recording the LispM SESSION."
  (lispm-session-files (plist-get session :id)))

;;;; Buffers

(defun lispm-desktop-lispm--buffer-agent ()
  "The LispM agent of the current buffer, if it is where one is talked to."
  (and (lispm-desktop-lispm-buffer-p (current-buffer))
       (if (eq major-mode 'lispm-chat-mode) lispm-chat-agent lispm-session-agent)))

(defun lispm-desktop-lispm--tag-buffer ()
  "Tag a LispM chat or session buffer: its session id, title and layout.
lispm-desktop-ai reads them."
  (when-let* ((agent (lispm-desktop-lispm--buffer-agent)))
    (setq lispm-agents-buffer-session-id (lispm-agent-id agent)
          lispm-agents-buffer-title (or lispm-agents-buffer-title
                                        (symbol-name (lispm-agent-name agent))))
    (setq-local lispm-desktop-conversation-buffer t)))

(defun lispm-desktop-lispm--changed (agent)
  "Keep AGENT's buffer tagged, and say its sessions changed."
  (let ((view (lispm-agent-view agent)))
    ;; /clear gives a standing agent a new session in the same chat.
    (when (buffer-live-p view)
      (with-current-buffer view (lispm-desktop-lispm--tag-buffer))))
  (run-hooks 'lispm-agents-changed-hook))

;;;; Commands

(defun lispm-desktop-agent-lispm-new (&optional role)
  "Start a new LispM agent at the current project root.
With a prefix argument, choose its ROLE."
  (interactive (list (and current-prefix-arg (lispm--read-role (lispm-desktop-tr "Role")))))
  (if role
      (lispm-agents-show (lispm-desktop-lispm-new (lispm-agents-default-directory) role))
    (lispm-agents-new 'lispm)))

(defun lispm-desktop-setup-lispm ()
  "Load LispM and register its agents with the agent tools."
  (lispm-desktop-deps-ensure 'lispm)
  (require 'lispm)
  (lispm-agents-define
   'lispm
   :name "LispM"
   :icon `(lispm-agents-image-icon
           ,(expand-file-name "lispm-lambda.png" lispm-desktop-agent-icon-directory) nil)
   :text-icon "λ"
   :command #'lispm-desktop-agent-lispm-new
   :key "l"
   :built-in t
   :sessions #'lispm-desktop-lispm-sessions
   :new #'lispm-desktop-lispm-new
   :resume #'lispm-desktop-lispm-resume
   :buffer-p #'lispm-desktop-lispm-buffer-p
   :send #'lispm-desktop-lispm-send
   :rename #'lispm-desktop-lispm-rename
   :files #'lispm-desktop-lispm-files
   ;; Its ids are dated and end in random digits.
   :short-id (lambda (id) (car (last (split-string id "-")))))

  ;; A chat keeps to the session column from the moment it is created.
  (add-to-list 'lispm-desktop-conversation-modes 'lispm-chat-mode)
  (add-hook 'lispm-chat-mode-hook #'lispm-desktop-lispm--tag-buffer)
  (add-hook 'lispm-session-mode-hook #'lispm-desktop-lispm--tag-buffer)
  (add-hook 'lispm-state-change-functions #'lispm-desktop-lispm--changed)
  (add-hook 'lispm-title-change-functions #'lispm-desktop-lispm--changed)
  ;; LispM speaks the desktop's language, unless `lispm-language' says
  ;; another; it knows nothing of the desktop, so the desktop tells it.
  (when (boundp 'lispm-language-functions)
    (add-hook 'lispm-language-functions #'lispm-desktop-current-language)
    (add-hook 'lispm-desktop-language-changed-hook #'lispm-language-changed))
  (lispm-mode-line-mode 1))

(provide 'lispm-desktop-lispm)
;;; lispm-desktop-lispm.el ends here
