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
;; LispM's terminal, LispM Agent's lispm-terminal.el, the sessions, a
;; listener and a who line, used from the keyboard, is an application of
;; the desktop's too, LispM Terminal: in the Applications folder, on the
;; desktop, in the λ menu's Applications, the Special menu (Enter
;; Terminal) and the LispM bar, and on C-c T; LispM enters it too when
;; asked to.  It is entered at once, in a frame of its own over the
;; desktop, a new one on a text terminal; C-c C-q, or asking LispM, leaves
;; it and the frame goes, the desktop as it was.  To start LispM as the
;; terminal instead, see lispm-desktop-start-as.el.
;;
;; LispM is lispm-agent, found or installed by lispm-desktop-deps.el: a
;; copy already on `load-path', else a checkout beside the desktop's, else
;; one package-vc installs from its repository.

;;; Code:

(require 'lispm-desktop-ai)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-deps)
(require 'lispm-desktop-i18n)
(require 'lispm-desktop-start-as)

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
(declare-function lispm-desktop-store-skill-sources "lispm-desktop-store" ())
(defvar lispm-skill-sources)
(declare-function lispm-desktop-setup-bar "lispm-desktop-bar" ())
(declare-function lispm-terminal "lispm-terminal" (&optional agent))
(declare-function lispm-terminal-soon "lispm-terminal" (function))
(defvar lispm-terminal-mode)
(defvar lispm-terminal-keys-mode)
(defvar lispm-terminal-enter-function)

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

;;;; LispM's terminal, entered from the desktop

(defvar lispm-desktop-lispm-terminal--frame nil
  "The frame LispM's terminal was entered in from the desktop, or nil.")

(defvar lispm-desktop-lispm-terminal--from nil
  "The window selected as LispM's terminal was entered, given the
keyboard back as it is left.")

(defun lispm-desktop-lispm-terminal-open-p ()
  "Return non-nil while LispM's terminal is entered from the desktop."
  (frame-live-p lispm-desktop-lispm-terminal--frame))

(defun lispm-desktop-lispm-terminal--parameters (screen)
  "Return the parameters of a frame for LispM's terminal over SCREEN, the
frame the desktop is drawn in: where SCREEN is and as large, or full
screen as it is, with none of the desktop's bars or window dividers, and
no screen of the desktop's, see `lispm-desktop-apps-screen-p'.  On a
text terminal a frame fills the terminal anyway."
  `((name . ,(lispm-desktop-tr "LispM Terminal"))
    (lispm-desktop-own-frame . lispm-terminal)
    (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
    (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
    (right-divider-width . 0) (bottom-divider-width . 0)
    ,@(when (display-graphic-p screen)
        (if-let* ((fullscreen (frame-parameter screen 'fullscreen)))
            `((fullscreen . ,fullscreen))
          (let ((position (frame-position screen)))
            `((left . ,(car position)) (top . ,(cdr position))
              (undecorated . ,(frame-parameter screen 'undecorated))
              (width . (text-pixels . ,(frame-text-width screen)))
              ;; The menu bar, in the tab bar, is the screen's own.
              (height . (text-pixels . ,(+ (frame-text-height screen)
                                           (if (fboundp 'tab-bar-height) (tab-bar-height screen t) 0))))))))))

(defun lispm-desktop-lispm-terminal (&optional agent)
  "Enter LispM's terminal now: the sessions, a listener and a who line,
used from the keyboard, in a frame of its own over the desktop; the
listener talks to AGENT, by default to LispM or the agent it talked to
last.  \`C-c C-q' leaves it, the desktop as it was.  Entered already,
the terminal comes in front."
  (interactive)
  (unless (lispm-desktop-lispm-terminal-available-p)
    (user-error (lispm-desktop-tr "This LispM Agent has no terminal")))
  (if (lispm-desktop-lispm-terminal-open-p)
      (let ((frame lispm-desktop-lispm-terminal--frame))
        (select-frame-set-input-focus frame)
        (when agent
          (lispm-terminal agent)
          (set-window-parameter (frame-selected-window frame) 'tab-line-format 'none)))
    (require 'lispm-terminal)
    (let* ((from (selected-window))
           ;; None of the desktop's ways with a new frame: a screen's menu
           ;; bar, glass.
           (frame (let ((after-make-frame-functions nil)
                        (before-make-frame-hook nil))
                    (make-frame (lispm-desktop-lispm-terminal--parameters (lispm-desktop-apps-screen))))))
      (setq lispm-desktop-lispm-terminal--frame frame
            lispm-desktop-lispm-terminal--from from)
      (select-frame-set-input-focus frame)
      (condition-case err
          (if agent (lispm-terminal agent) (lispm-terminal))
        (error (delete-frame frame t)
               (signal (car err) (cdr err))))
      ;; The listener has no title bar, as the windows beside it have
      ;; none: the desktop gives every window one.
      (set-window-parameter (frame-selected-window frame) 'tab-line-format 'none))))

(defun lispm-desktop-lispm-terminal-enter (agent)
  "Enter LispM's terminal for AGENT, asked by the tool terminal_enter, in a
frame of its own as `lispm-desktop-lispm-terminal' does, once the tool
call is over: the desktop's frame stays as it is.  This is
`lispm-terminal-enter-function' on the desktop; nil, for the usual words."
  (let ((enter (lambda () (lispm-desktop-lispm-terminal agent))))
    (if (fboundp 'lispm-terminal-soon)
        (lispm-terminal-soon enter)
      (run-at-time 0 nil enter)))
  nil)

(defvar-local lispm-desktop-lispm-terminal--line-numbers nil
  "Non-nil in a buffer whose line numbers LispM's terminal hides.")

(defun lispm-desktop-lispm-terminal--keys-changed ()
  "Hide the line numbers the desktop gives every buffer while LispM's
terminal shows it, as its keys are on there, and show them again as it
leaves.  Turned off as the buffer's mode is set, as that of the
sessions is, the desktop's global mode leaves them off.  This is a
`lispm-terminal-keys-mode-hook' function."
  (if lispm-terminal-keys-mode
      (when (fboundp 'display-line-numbers-mode)
        (setq lispm-desktop-lispm-terminal--line-numbers (bound-and-true-p display-line-numbers-mode))
        (display-line-numbers-mode -1))
    (when lispm-desktop-lispm-terminal--line-numbers
      (kill-local-variable 'lispm-desktop-lispm-terminal--line-numbers)
      (display-line-numbers-mode 1))))

(defun lispm-desktop-lispm-terminal--left ()
  "Close the frame LispM's terminal was entered in once the terminal
leaves it: by \`C-c C-q', or for another frame.  The frame goes before
the terminal would give it back as it was.  This is a
`lispm-terminal-mode-hook' function."
  (when (and (not (bound-and-true-p lispm-terminal-mode)) (lispm-desktop-lispm-terminal-open-p))
    (delete-frame lispm-desktop-lispm-terminal--frame t)))

(defun lispm-desktop-lispm-terminal--deleted (frame)
  "Give the keyboard back to the window the terminal was entered from as
FRAME, the terminal's, goes, however it goes, unless that window is gone
or hidden in the Dock.  This is a `delete-frame-functions' function, run
before Emacs chooses a frame to select."
  (when (eq frame lispm-desktop-lispm-terminal--frame)
    (let ((from lispm-desktop-lispm-terminal--from))
      (setq lispm-desktop-lispm-terminal--frame nil
            lispm-desktop-lispm-terminal--from nil)
      (when (and (eq frame (selected-frame)) (window-live-p from)
                 (eq (frame-visible-p (window-frame from)) t))
        (select-frame-set-input-focus (window-frame from))
        (select-window from)))))

(defun lispm-desktop-lispm-setup-terminal ()
  "Make LispM's terminal an application of the desktop's, built in, when
LispM Agent has it: LispM Terminal, entered on \`C-c T', see
`lispm-desktop-lispm-terminal'."
  (when (lispm-desktop-lispm-terminal-available-p)
    (lispm-desktop-define-app 'lispm-terminal :name (lispm-desktop-tr-noop "LispM Terminal")
                              :icon "terminal" :folder "terminal-32"
                              :activate #'lispm-desktop-lispm-terminal
                              :running #'lispm-desktop-lispm-terminal-open-p)
    (keymap-global-set "C-c T" #'lispm-desktop-lispm-terminal)
    ;; Asked of LispM, the terminal is entered so too, never in the
    ;; desktop's own frame; it is left as by C-c C-q.
    (setq lispm-terminal-enter-function #'lispm-desktop-lispm-terminal-enter)
    (add-hook 'lispm-terminal-mode-hook #'lispm-desktop-lispm-terminal--left)
    (add-hook 'lispm-terminal-keys-mode-hook #'lispm-desktop-lispm-terminal--keys-changed)
    ;; Before the terminal's own, which leaves it as its frame goes.
    (add-hook 'delete-frame-functions #'lispm-desktop-lispm-terminal--deleted -90)))

(defun lispm-desktop--system-skills ()
  "Return where the desktop's own skills are, as LispM Agent's
`lispm-skill-sources' are told: the manuals of the desktop, its Control
Panels and its App Store, `lispm-desktop-skill-directory'."
  (list (list :directory lispm-desktop-skill-directory :origin "LispM Desktop")))

(defun lispm-desktop-setup-lispm ()
  "Load LispM and register its agents with the agent tools."
  (lispm-desktop-deps-ensure 'lispm)
  (require 'lispm)
  ;; LispM's agents find the desktop's skills, and those the App Store
  ;; installed with its applications; LispM knows nothing of the desktop,
  ;; so the desktop tells it.  The desktop's come last: a skill of a later
  ;; source replaces one of the same name, and no catalog's replaces one
  ;; of the desktop's own.
  (when (boundp 'lispm-skill-sources)
    (add-hook 'lispm-skill-sources #'lispm-desktop-store-skill-sources)
    (add-hook 'lispm-skill-sources #'lispm-desktop--system-skills 90))
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
  (lispm-mode-line-mode 1)
  ;; LispM itself, the system's agent, has a bar of its own in the menu
  ;; bar, and the desktop enters LispM's terminal; started as the
  ;; terminal, LispM has neither, and talks to it in its listener.
  (unless (lispm-desktop-started-as-terminal-p)
    (lispm-desktop-setup-bar)
    (lispm-desktop-lispm-setup-terminal)))

(provide 'lispm-desktop-lispm)
;;; lispm-desktop-lispm.el ends here
