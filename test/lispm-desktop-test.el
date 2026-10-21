;;; lispm-desktop-test.el --- Tests for lispm-desktop  -*- lexical-binding: t; -*-

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

;; Run with `make test'.  They need none of the packages lispm-desktop
;; installs: setup functions are not called, only what they decide with.

;;; Code:

(require 'ert)
(require 'lispm-desktop)

(defmacro lispm-desktop-test--with-agents (agents &rest body)
  "Run BODY with AGENTS, (AGENT . PROPERTIES), the only agents defined,
their keys bound in keymaps of the test's own."
  (declare (indent 1))
  `(let ((lispm-desktop-agents nil)
         (lispm-desktop-agent--keys nil)
         (lispm-desktop-agent-map (make-sparse-keymap))
         (lispm-desktop-agent-list-mode-map (make-sparse-keymap)))
     (pcase-dolist (`(,agent . ,props) ,agents)
       (apply #'lispm-desktop-agent-define agent props))
     ,@body))

(ert-deftest lispm-desktop-test-agent-available-p ()
  "An agent run as a CLI can run exactly when its program is on PATH; one
running inside Emacs always can; one never defined cannot."
  (lispm-desktop-test--with-agents
      '((claude :name "Claude Code" :program "lispm-desktop-test-no-such-program")
        (codex :name "Codex" :program "emacs")
        (lispm :name "LispM" :new ignore))
    (should-not (lispm-desktop-agent-available-p 'claude))
    (should (lispm-desktop-agent-available-p 'codex))
    (should (lispm-desktop-agent-available-p 'lispm))
    (should-not (lispm-desktop-agent-available-p 'hermes))
    (should (equal (mapcar #'car (lispm-desktop-agent-available)) '(codex lispm)))))

(ert-deftest lispm-desktop-test-agents-built-in-first ()
  "Agents are offered those built in first, then in the order defined."
  (lispm-desktop-test--with-agents
      '((hermes :name "Hermes" :new ignore)
        (lispm :name "LispM" :new ignore :built-in t)
        (claude :name "Claude Code" :new ignore :built-in t))
    (should (equal (mapcar #'car (lispm-desktop-agent-available)) '(lispm claude hermes)))
    (lispm-desktop-agent-define 'hermes :name "Hermes, again" :new #'ignore)
    (should (equal (mapcar #'car lispm-desktop-agents) '(hermes lispm claude)))
    (lispm-desktop-agent-undefine 'lispm)
    (should (equal (mapcar #'car (lispm-desktop-agent-available)) '(claude hermes)))))

(ert-deftest lispm-desktop-test-agent-keys-follow-the-agent ()
  "An agent's key starts it, under C-c a and in the agents sidebar, while
it can run; it does nothing once its program is gone or it is undefined."
  (let ((program "emacs"))
    (lispm-desktop-test--with-agents
        `((codex :name "Codex" :program "emacs" :key "x" :command lispm-desktop-agent-codex-new))
      (should (eq (keymap-lookup lispm-desktop-agent-map "x") 'lispm-desktop-agent-codex-new))
      (should (eq (keymap-lookup lispm-desktop-agent-list-mode-map "x") 'lispm-desktop-agent-codex-new))
      (setf (plist-get (alist-get 'codex lispm-desktop-agents) :program) "lispm-desktop-test-no-such-program")
      (should-not (keymap-lookup lispm-desktop-agent-map "x"))
      (setf (plist-get (alist-get 'codex lispm-desktop-agents) :program) program)
      (lispm-desktop-agent-undefine 'codex)
      (should-not (keymap-lookup lispm-desktop-agent-map "x"))
      (should-not (keymap-lookup lispm-desktop-agent-list-mode-map "x")))))

(ert-deftest lispm-desktop-test-agent-keys-leave-other-commands-be ()
  "An agent's key taken by another command stays that command's, and its
parent's bindings do not count; an agent taking another's key gives it
back when it goes."
  (lispm-desktop-test--with-agents nil
    (keymap-set lispm-desktop-agent-map "r" #'lispm-desktop-agent-send-region)
    (set-keymap-parent lispm-desktop-agent-list-mode-map special-mode-map)
    (let ((inhibit-message t))
      (lispm-desktop-agent-define 'greedy :name "Greedy" :new #'ignore :key "r" :command #'ignore))
    (should (eq (keymap-lookup lispm-desktop-agent-map "r") 'lispm-desktop-agent-send-region))
    (lispm-desktop-agent-undefine 'greedy)
    (should (eq (keymap-lookup lispm-desktop-agent-map "r") 'lispm-desktop-agent-send-region))
    ;; h is the sidebar's parent's, describe-mode: Hermes may have it.
    (lispm-desktop-agent-define 'hermes :name "Hermes" :new #'ignore :key "h" :command 'lispm-hermes-new)
    (should (eq (keymap-lookup lispm-desktop-agent-list-mode-map "h") 'lispm-hermes-new))
    (lispm-desktop-agent-define 'other :name "Other" :new #'ignore :key "h" :command 'other-new)
    (should (eq (keymap-lookup lispm-desktop-agent-map "h") 'other-new))
    (lispm-desktop-agent-undefine 'other)
    (should (eq (keymap-lookup lispm-desktop-agent-map "h") 'lispm-hermes-new))
    (should (eq (keymap-lookup lispm-desktop-agent-list-mode-map "h") 'lispm-hermes-new))
    (lispm-desktop-agent-undefine 'hermes)
    (should-not (keymap-lookup lispm-desktop-agent-map "h"))
    (should (eq (keymap-lookup lispm-desktop-agent-list-mode-map "h") 'describe-mode))))

(ert-deftest lispm-desktop-test-launch-requires-installed-cli ()
  "Starting an agent whose CLI is missing, or no agent at all, is a user
error, before anything runs."
  (lispm-desktop-test--with-agents
      '((claude :name "Claude Code" :program "lispm-desktop-test-no-such-program"))
    (should-error (lispm-desktop-agent--launch 'claude temporary-file-directory "x" nil)
                  :type 'user-error)
    (should-error (lispm-desktop-agent-new 'hermes temporary-file-directory) :type 'user-error)))

(ert-deftest lispm-desktop-test-claude-follows-emacs-theme ()
  "Claude Code starts with its theme \"auto\", asking the terminal for
Emacs's colours, unless its user chose a theme: in its user, project or
local settings, or in its old global configuration other than the default
\"dark\".  Settings it cannot read choose nothing."
  (let* ((home (make-temp-file "lispm-desktop-test-home" t))
         (project (expand-file-name "project/" home))
         (process-environment (cons (concat "HOME=" home) process-environment))
         (auto '("--settings" "{\"theme\":\"auto\"}")))
    (cl-flet ((put-json (file text)
                (make-directory (file-name-directory file) t)
                (with-temp-file file (insert text))))
      (unwind-protect
          (progn
            (make-directory project)
            (should (equal (lispm-desktop-agent-claude--arguments project) auto))
            (put-json (expand-file-name ".claude.json" home) "{\"numStartups\":3,\"theme\":\"dark\"}")
            (put-json (expand-file-name ".claude/settings.json" home) "{\"theme\":")
            (should (equal (lispm-desktop-agent-claude--arguments project) auto))
            (dolist (file (list (expand-file-name ".claude.json" home)
                                (expand-file-name ".claude/settings.json" home)
                                (expand-file-name ".claude/settings.json" project)
                                (expand-file-name ".claude/settings.local.json" project)))
              (put-json file "{\"permissions\":{},\"theme\":\"light-daltonized\"}")
              (should-not (lispm-desktop-agent-claude--arguments project))
              (put-json file "[]"))
            ;; Launched, Claude gets the arguments before its own.
            (let (command)
              (lispm-desktop-test--with-agents nil
                (lispm-desktop-setup-claude)
                (cl-letf (((symbol-function 'lispm-desktop-agent-available-p) #'always)
                          ((symbol-function 'lispm-desktop-agent-ide-start) #'ignore)
                          ((symbol-function 'lispm-desktop-terminal-make)
                           (lambda (_name &rest program)
                             (setq command program)
                             (get-buffer-create " *lispm-desktop-test-launch*"))))
                  (unwind-protect
                      (lispm-desktop-agent--launch 'claude project "lispm-desktop-test-launch"
                                                   '("--session-id" "x"))
                    (kill-buffer " *lispm-desktop-test-launch*"))))
              (should (equal command `("claude" ,@auto "--session-id" "x")))))
        (delete-directory home t)))))

(ert-deftest lispm-desktop-test-sessions-skip-missing-agents ()
  "No record of an agent whose CLI is missing is read or listed."
  (let ((read nil))
    (lispm-desktop-test--with-agents
        `((claude :name "Claude Code" :program "lispm-desktop-test-no-such-program"
                  :sessions ,(lambda () (setq read t) nil)))
      (should (null (lispm-desktop-agent-sessions-list)))
      (should-not read))))

(ert-deftest lispm-desktop-test-sessions-of-every-agent ()
  "Every agent's sessions are listed, newest first; an agent whose
sessions cannot be read lists none, and the others still list theirs."
  (lispm-desktop-test--with-agents
      `((lispm :name "LispM" :new ignore
               :sessions ,(lambda ()
                            (list (list :agent 'lispm :id "a" :time 1.0)
                                  (list :agent 'lispm :id "b" :time 3.0))))
        (codex :name "Codex" :program "emacs"
               :sessions ,(lambda () (list (list :agent 'codex :id "c" :time 2.0))))
        (broken :name "Broken" :new ignore :sessions ,(lambda () (error "No store"))))
    (let ((inhibit-message t))
      (should (equal (mapcar (lambda (s) (plist-get s :id)) (lispm-desktop-agent-sessions-list))
                     '("b" "c" "a"))))))

(ert-deftest lispm-desktop-test-agent-terminals-know-their-agent ()
  "A terminal is an agent's when started for it, or named after it while
that agent is defined; a shell is no agent's."
  (lispm-desktop-test--with-agents
      '((codex :name "Codex" :program "emacs"))
    (let ((named (get-buffer-create "*codex:lispm-desktop-test*"))
          (tagged (get-buffer-create "*lispm-desktop-test-tagged*"))
          (shell (get-buffer-create "*zsh:lispm-desktop-test*")))
      (unwind-protect
          (progn
            (dolist (buffer (list named tagged shell))
              (with-current-buffer buffer (setq major-mode 'lispm-desktop-term-mode)))
            (with-current-buffer tagged (setq lispm-desktop-agent-buffer-agent 'codex))
            (should (eq (lispm-desktop-agent-of-buffer named) 'codex))
            (should (eq (lispm-desktop-agent-of-buffer tagged) 'codex))
            (should-not (lispm-desktop-agent-buffer-p shell))
            (lispm-desktop-agent-undefine 'codex)
            (should-not (lispm-desktop-agent-buffer-p named)))
        (mapc #'kill-buffer (list named tagged shell))))))

(cl-defstruct (lispm-desktop-test--server (:constructor lispm-desktop-test--server-make))
  "A server bound to a terminal in a test; STOPPED once it was stopped."
  stopped)

(cl-defmethod lispm-desktop-agent-context-stop ((server lispm-desktop-test--server))
  "Note that SERVER was stopped."
  (setf (lispm-desktop-test--server-stopped server) t))

(ert-deftest lispm-desktop-test-undefined-agent-stops-its-servers ()
  "Undefining an agent stops the servers of its terminals then, while its
code is loaded, and leaves the terminals and other agents' servers be;
the match data a caller holds survives telling terminals apart."
  (lispm-desktop-test--with-agents
      '((codex :name "Codex" :program "emacs") (claude :name "Claude Code" :program "emacs"))
    (let ((codex (get-buffer-create "*codex:lispm-desktop-test*"))
          (claude (get-buffer-create "*claude:lispm-desktop-test*"))
          (codex-server (lispm-desktop-test--server-make))
          (claude-server (lispm-desktop-test--server-make)))
      (unwind-protect
          (progn
            (with-current-buffer codex (setq lispm-desktop-agent-buffer-agent 'codex))
            (with-current-buffer claude (setq lispm-desktop-agent-buffer-agent 'claude))
            (lispm-desktop-agent-context-bind codex-server codex)
            (lispm-desktop-agent-context-bind claude-server claude)
            (with-temp-buffer
              (setq major-mode 'lispm-desktop-term-mode)
              (rename-buffer "*claude:lispm-desktop-test-2*")
              (string-match "\\(test\\)" "a test")
              (lispm-desktop-agent-buffer-p (current-buffer))
              (should (equal (match-beginning 1) 2)))
            (lispm-desktop-agent-undefine 'codex)
            (should (lispm-desktop-test--server-stopped codex-server))
            (should-not (lispm-desktop-test--server-stopped claude-server))
            (should (buffer-live-p codex)))
        (lispm-desktop-agent-context-release-all)
        (mapc #'kill-buffer (list codex claude))))))

(ert-deftest lispm-desktop-test-agent-worktrees-join-their-projects ()
  "A Claude worktree joins its repository and a Codex worktree the known
project of its name; the Codex app's scratch tree holds chats."
  (lispm-desktop-test--with-agents nil
    (lispm-desktop-setup-claude)
    (lispm-desktop-setup-codex)
    (let* ((repo (make-temp-file "lispm-desktop-test-repo" t))
           (claude (expand-file-name ".claude/worktrees/feature" repo))
           (codex (expand-file-name (format "~/.codex/worktrees/1a2b/%s" (file-name-nondirectory repo))))
           (scratch (expand-file-name "2026-10-20/chat" lispm-desktop-agent-codex-chats-directory)))
      (unwind-protect
          (progn
            (make-directory claude t)
            (let ((roots (lispm-desktop-agent--resolve-roots (list repo claude codex scratch))))
              (should (equal (cdr (assoc claude roots)) (directory-file-name repo)))
              (should (equal (cdr (assoc codex roots)) (directory-file-name repo)))
              (should (eq (cdr (assoc scratch roots)) 'chats)))
            (should (equal (lispm-desktop-agent-location-label claude (directory-file-name repo))
                           "⎇ feature"))
            (should (equal (lispm-desktop-agent-location-label codex (directory-file-name repo))
                           "⎇ codex worktree")))
        (clrhash lispm-desktop-agent--root-cache)
        (delete-directory repo t)))))

(ert-deftest lispm-desktop-test-agent-archive-survives-an-unreadable-state ()
  "Archived sessions are kept across starts; a state file that does not
read is moved aside, not replaced by the next archiving."
  (let* ((directory (make-temp-file "lispm-desktop-test-state" t))
         (lispm-desktop-agent-state-file (expand-file-name "agent-sessions.eld" directory))
         (lispm-desktop-agent--archived 'unloaded)
         (cut-short "(:archived (\"claude:1\" \"cod")
         (one '(:agent claude :id "1"))
         (two '(:agent codex :id "2")))
    (unwind-protect
        (progn
          (lispm-desktop-agent--set-archived one t)
          (should-not (file-exists-p (concat lispm-desktop-agent-state-file ".part")))
          (setq lispm-desktop-agent--archived 'unloaded)
          (should (lispm-desktop-agent-archived-p one))
          (should-not (lispm-desktop-agent-archived-p two))
          ;; A write cut short leaves a file that does not read.
          (with-temp-file lispm-desktop-agent-state-file (insert cut-short))
          (setq lispm-desktop-agent--archived 'unloaded)
          (let ((inhibit-message t))
            (should-not (lispm-desktop-agent-archived-p one)))
          (lispm-desktop-agent--set-archived two t)
          (should (equal (with-temp-buffer
                           (insert-file-contents (concat lispm-desktop-agent-state-file ".unreadable"))
                           (buffer-string))
                         cut-short))
          (setq lispm-desktop-agent--archived 'unloaded)
          (should (equal (lispm-desktop-agent--archived-keys) '("codex:2"))))
      (delete-directory directory t))))

(ert-deftest lispm-desktop-test-codex-sessions-named-by-the-index ()
  "A Codex session is titled by its thread name in the index, else by its
first prompt; context Codex injects is no prompt."
  (let* ((home (make-temp-file "lispm-desktop-test-codex" t))
         (lispm-desktop-agent-codex-sessions-directory (expand-file-name "sessions/" home))
         (lispm-desktop-agent-codex-index-file (expand-file-name "session_index.jsonl" home))
         (file (expand-file-name "2026/10/20/rollout-1.jsonl" lispm-desktop-agent-codex-sessions-directory)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "{\"type\":\"session_meta\",\"payload\":{\"id\":\"0199\",\"cwd\":\"/tmp\",\"timestamp\":\"2026-10-20T08:00:00Z\"}}\n"
                    "{\"type\":\"response_item\",\"payload\":{\"type\":\"message\",\"role\":\"user\",\"content\":[{\"type\":\"input_text\",\"text\":\"# AGENTS.md instructions\"}]}}\n"
                    "{\"type\":\"event_msg\",\"payload\":{\"type\":\"user_message\",\"message\":\"Fix the build\"}}\n"))
          (let ((session (car (lispm-desktop-agent-codex-sessions))))
            (should (equal (plist-get session :title) "Fix the build"))
            (should (equal (plist-get session :id) "0199"))
            (should (numberp (plist-get session :started))))
          (lispm-desktop-agent-codex--rename (list :id "0199") "Build fix")
          (should (equal (plist-get (car (lispm-desktop-agent-codex-sessions)) :title) "Build fix")))
      (remhash file lispm-desktop-agent--session-cache)
      (delete-directory home t))))

(ert-deftest lispm-desktop-test-claude-sessions-titled-by-their-name ()
  "A Claude session is titled by its first prompt until it is named.  The
name lives beside the transcript, which Claude rewrites, and a blank one
names nothing.  A transcript without a prompt is no session."
  (let* ((lispm-desktop-agent-claude-projects-directory (make-temp-file "lispm-desktop-test-claude" t))
         (file (expand-file-name "-tmp/0199.jsonl" lispm-desktop-agent-claude-projects-directory))
         (empty (expand-file-name "-tmp/0200.jsonl" lispm-desktop-agent-claude-projects-directory))
         (title (lambda () (plist-get (car (lispm-desktop-agent-claude-sessions)) :title))))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "{\"type\":\"user\",\"cwd\":\"/tmp\",\"message\":{\"role\":\"user\",\"content\":\"Fix the build\"}}\n"))
          (with-temp-file empty
            (insert "{\"type\":\"system\",\"cwd\":\"/tmp\"}\n"))
          (should (= (length (lispm-desktop-agent-claude-sessions)) 1))
          (should (equal (funcall title) "Fix the build"))
          (lispm-desktop-agent-claude--rename (car (lispm-desktop-agent-claude-sessions)) "Build fix")
          (should (equal (funcall title) "Build fix"))
          (lispm-desktop-agent-claude--rename (car (lispm-desktop-agent-claude-sessions)) " ")
          (should (equal (funcall title) "Fix the build")))
      (remhash file lispm-desktop-agent--session-cache)
      (remhash empty lispm-desktop-agent--session-cache)
      (delete-directory lispm-desktop-agent-claude-projects-directory t))))

(ert-deftest lispm-desktop-test-first-start-in-batch ()
  "A batch Emacs settles on Emacs keys and Dracula without asking.
Choices already made are kept, and the start that finds them made is
not a first start."
  (let ((lispm-desktop-editing-style nil)
        (lispm-desktop-theme nil))
    (cl-letf (((symbol-function 'lispm-desktop-assistant-ask)
               (lambda (_questions) (error "Asked in batch")))
              ((symbol-function 'customize-save-variable)
               (lambda (&rest _) (error "Saved in batch"))))
      (should (lispm-desktop--first-start))
      (should (eq lispm-desktop-editing-style 'emacs))
      (should (eq lispm-desktop-theme 'dracula))))
  (let ((lispm-desktop-editing-style 'vim)
        (lispm-desktop-theme 'platinum))
    (should-not (lispm-desktop--first-start))
    (should (eq lispm-desktop-editing-style 'vim))
    (should (eq lispm-desktop-theme 'platinum))))

(ert-deftest lispm-desktop-test-first-start-saves-answers ()
  "The first start asks only what is not chosen yet and saves the answers;
questions put off take the first choices, saved nowhere."
  (let ((lispm-desktop-editing-style nil)
        (lispm-desktop-theme nil)
        (noninteractive nil)
        (asked nil)
        (saved nil)
        (answers '((lispm-desktop-editing-style . vim) (lispm-desktop-theme . platinum))))
    (cl-letf (((symbol-function 'lispm-desktop-assistant-ask)
               (lambda (questions) (setq asked (mapcar #'car questions)) answers))
              ((symbol-function 'customize-save-variable)
               (lambda (variable value &rest _)
                 (push (cons variable value) saved)
                 (set-default variable value))))
      (should (lispm-desktop--first-start))
      (should (equal asked '(lispm-desktop-editing-style lispm-desktop-theme)))
      (should (equal (reverse saved) answers))
      (should (eq lispm-desktop-editing-style 'vim))
      (should (eq lispm-desktop-theme 'platinum))
      ;; The editing style chosen already, only the appearance is asked;
      ;; put off, it is Dracula for now.
      (setq lispm-desktop-theme nil saved nil answers nil)
      (should (lispm-desktop--first-start))
      (should (equal asked '(lispm-desktop-theme)))
      (should-not saved)
      (should (eq lispm-desktop-theme 'dracula)))))

(defmacro lispm-desktop-test--with-assistant (events &rest body)
  "Run BODY with the assistant's page reading EVENTS, a list, in turn.
Nothing is drawn: the page is laid out on a window 1000 by 600 pixels.
Bind `shown' to what was shown, in order, each as (STEP FOCUSED PRESSED)."
  (declare (indent 1))
  `(let ((events ,events)
         (shown nil))
     (cl-letf (((symbol-function 'lispm-desktop-assistant--show)
                (lambda (page focused pressed)
                  (setq shown (append shown (list (list (plist-get page :step) focused pressed))))
                  (lispm-desktop-assistant--layout 1000 600 page)))
               ((symbol-function 'lispm-desktop-assistant--read)
                (lambda () (if events (pop events) (error "No more events"))))
               ((symbol-function 'sit-for) #'ignore))
       ,@body)))

(defun lispm-desktop-test--assistant-click (kind layout index)
  "Return a mouse event of KIND over the INDEXth card of LAYOUT, or Go Back
for INDEX `back', on the assistant's image."
  (pcase-let ((`(,x ,y ,w ,h) (if (eq index 'back)
                                  (plist-get layout :back)
                                (nth index (plist-get layout :cards)))))
    (list kind (list (selected-window) 1 '(0 . 0) 0 nil 1 '(0 . 0)
                     '(image :type svg) (cons (+ x (/ w 2)) (+ y (/ h 2))) '(1000 . 600)))))

(ert-deftest lispm-desktop-test-assistant-keys ()
  "A choice's letter chooses it, in either case; Tab and C-f, C-n move the
ring on and C-b, C-p back, round the choices, and Return or Space chooses
the one ringed; DEL goes back a question, ringing the choice made there;
Escape and C-g put the questions off."
  (let ((questions lispm-desktop-assistant-questions))
    (lispm-desktop-test--with-assistant '(?v ?P)
      (should (equal (lispm-desktop-assistant--ask-on-page questions)
                     '((lispm-desktop-editing-style . vim) (lispm-desktop-theme . platinum)))))
    (lispm-desktop-test--with-assistant '(tab return ?\C-f ?\C-n ?\s)
      (should (equal (lispm-desktop-assistant--ask-on-page questions)
                     '((lispm-desktop-editing-style . vim) (lispm-desktop-theme . dracula)))))
    (lispm-desktop-test--with-assistant '(?\C-b ?\C-p return backtab return)
      (should (equal (lispm-desktop-assistant--ask-on-page questions)
                     '((lispm-desktop-editing-style . emacs) (lispm-desktop-theme . platinum)))))
    ;; DEL on the first question does nothing; on the second it goes back
    ;; to the first, Vim keys ringed, which Return chooses again.  A
    ;; button chosen blinks.
    (lispm-desktop-test--with-assistant '(?\d ?v ?\d return ?d)
      (should (equal (lispm-desktop-assistant--ask-on-page questions)
                     '((lispm-desktop-editing-style . vim) (lispm-desktop-theme . dracula))))
      (should (equal shown '(("Step 1 of 2" 0 nil) ("Step 1 of 2" 0 nil) ("Step 1 of 2" 0 1)
                             ("Step 2 of 2" 0 nil)
                             ("Step 1 of 2" 1 nil) ("Step 1 of 2" 1 1)
                             ("Step 2 of 2" 0 nil) ("Step 2 of 2" 0 0)))))
    (dolist (put-off '(escape ?\C-g))
      (lispm-desktop-test--with-assistant (list ?e put-off)
        (should-not (lispm-desktop-assistant--ask-on-page questions))))
    ;; Keys meaning nothing here, and a while without any, are passed over.
    (lispm-desktop-test--with-assistant '(nil ?x ?\M-v (switch-frame nil) ?e ?d)
      (should (equal (lispm-desktop-assistant--ask-on-page questions)
                     '((lispm-desktop-editing-style . emacs) (lispm-desktop-theme . dracula)))))))

(ert-deftest lispm-desktop-test-assistant-mouse ()
  "A choice is chosen by pressing its button and letting go over it; let go
elsewhere, nothing is chosen.  Go Back goes back."
  (let* ((questions lispm-desktop-assistant-questions)
         (first (lispm-desktop-assistant--layout
                 1000 600 (lispm-desktop-assistant--page (nth 0 questions) 0 2)))
         (second (lispm-desktop-assistant--layout
                  1000 600 (lispm-desktop-assistant--page (nth 1 questions) 1 2))))
    (cl-flet ((click (kind layout index) (lispm-desktop-test--assistant-click kind layout index)))
      (lispm-desktop-test--with-assistant
          (list (click 'down-mouse-1 first 0) (click 'mouse-1 first 1)
                (click 'down-mouse-1 first 1) (click 'mouse-1 first 1)
                (click 'down-mouse-1 second 'back) (click 'mouse-1 second 'back)
                (click 'down-mouse-1 first 0) (click 'mouse-1 first 0)
                (click 'down-mouse-1 second 1) (click 'drag-mouse-1 second 1))
        (should (equal (lispm-desktop-assistant--ask-on-page questions)
                       '((lispm-desktop-editing-style . emacs) (lispm-desktop-theme . platinum))))
        ;; A button is drawn held down while pressed over it.
        (should (equal shown '(("Step 1 of 2" 0 nil) ("Step 1 of 2" 0 0)
                               ("Step 1 of 2" 0 nil) ("Step 1 of 2" 0 1)
                               ("Step 2 of 2" 0 nil) ("Step 2 of 2" 0 back)
                               ("Step 1 of 2" 1 nil) ("Step 1 of 2" 1 0)
                               ("Step 2 of 2" 0 nil) ("Step 2 of 2" 0 1))))))))

(ert-deftest lispm-desktop-test-assistant-layout ()
  "The window stands in the middle of the page, its choices side by side
inside it, as wide as one another, and Go Back at its bottom right."
  (let* ((page (lispm-desktop-assistant--page (nth 1 lispm-desktop-assistant-questions) 1 2))
         (layout (lispm-desktop-assistant--layout 1000 600 page)))
    (pcase-let ((`(,x ,y ,w ,h) (plist-get layout :window))
                (`(,cx ,cy ,cw ,ch) (plist-get layout :content))
                (`((,ax ,ay ,aw ,ah) (,bx ,by ,bw ,bh)) (plist-get layout :cards))
                (`(,gx ,gy ,gw ,gh) (plist-get layout :back)))
      (should (= x (/ (- 1000 w) 2)))
      (should (= y (/ (- 600 h) 2)))
      (should (and (= ay by) (= aw bw) (= ah bh) (< (+ ax aw) bx)))
      (should (= (- ax cx) (- (+ cx cw) (+ bx bw))))
      (should (<= cy ay (+ ay ah) gy (+ gy gh) (+ cy ch)))
      (should (<= (+ cx (/ cw 2)) gx (+ gx gw) (+ cx cw)))
      (should (eq (lispm-desktop-assistant--target-at layout (+ ax 5) (+ ay 5)) 0))
      (should (eq (lispm-desktop-assistant--target-at layout (+ bx bw -1) (+ by bh -1)) 1))
      (should (eq (lispm-desktop-assistant--target-at layout (1+ gx) (1+ gy)) 'back))
      (should-not (lispm-desktop-assistant--target-at layout (+ ax aw 1) ay))
      (should-not (lispm-desktop-assistant--target-at layout 0 0)))
    ;; A page too small for the window keeps the window's top left on it.
    (should (equal (seq-take (plist-get (lispm-desktop-assistant--layout 300 200 page) :window) 2)
                   '(0 0)))))

(ert-deftest lispm-desktop-test-assistant-draw ()
  "The page is a Platinum window on the desktop, titled Welcome to LispM:
the question, both choices with their keys, how to choose; the last
page, while LispM is set up, says so and what was chosen."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (lispm-desktop-platinum-current-palette palette)
         (questions lispm-desktop-assistant-questions)
         (page (lispm-desktop-assistant--page (nth 1 questions) 1 2))
         (svg (lispm-desktop-assistant-draw palette 1000 600 page 0 nil))
         (texts (mapcar #'dom-text (dom-by-tag svg 'text))))
    (dolist (text '("Welcome to LispM" "Which appearance?" "Dracula" "Platinum" "D" "P"
                    "Step 2 of 2" "Go Back"))
      (should (member text texts)))
    (should (seq-find (lambda (text) (string-prefix-p "Press D or P" text)) texts))
    (pcase-let ((`(,cx ,cy ,_ ,_) (plist-get (lispm-desktop-assistant--layout 1000 600 page) :content)))
      (should (equal (lispm-desktop-test--pixel svg 1 1) (upcase (plist-get palette :desktop))))
      (should (equal (lispm-desktop-test--pixel svg (+ cx 2) (+ cy 80)) "#DDDDDD")))
    (let* ((done (lispm-desktop-assistant--done-page
                  questions '((lispm-desktop-editing-style . vim) (lispm-desktop-theme . platinum))))
           (texts (mapcar #'dom-text (dom-by-tag (lispm-desktop-assistant-draw
                                                  palette 1000 600 (plist-put done :status "Installing…")
                                                  nil nil)
                                                 'text))))
      ;; svg.el writes characters past ASCII as references: … is &#8230;.
      (should (seq-find (lambda (text) (string-prefix-p "Setting LispM up" text)) texts))
      (should (seq-find (lambda (text) (string-prefix-p "Installing" text)) texts))
      (should (seq-find (lambda (text) (string-search "Vim keys, Platinum." text)) texts)))
    (should (string-search "Emacs keys, Dracula for now"
                           (plist-get (lispm-desktop-assistant--done-page questions nil) :detail)))))

(ert-deftest lispm-desktop-test-assistant-in-minibuffer ()
  "Where the page cannot be drawn, the questions are asked in the
minibuffer, by the same keys; C-g puts them off."
  (should-not (lispm-desktop-assistant-available-p))
  (let ((keys (list ?v ?p)))
    (cl-letf (((symbol-function 'read-multiple-choice)
               (lambda (_prompt choices &rest _) (assq (pop keys) choices))))
      (should (equal (lispm-desktop-assistant-ask lispm-desktop-assistant-questions)
                     '((lispm-desktop-editing-style . vim) (lispm-desktop-theme . platinum))))))
  (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) (signal 'quit nil))))
    (should-not (lispm-desktop-assistant-ask lispm-desktop-assistant-questions))))

(defun lispm-desktop-test--path-points (path)
  "Return the points of PATH, path data of moves and lines, as (X . Y)."
  (let ((points nil) (start 0))
    (while (string-match "\\([-0-9.]+\\) \\([-0-9.]+\\)" path start)
      (push (cons (string-to-number (match-string 1 path)) (string-to-number (match-string 2 path)))
            points)
      (setq start (match-end 0)))
    (nreverse points)))

(ert-deftest lispm-desktop-test-coach-mark-loop ()
  "A coach mark is a loop filling its box, going once round and a little
further, its two ends side by side; its box is an oval's through the
corners of what it circles, clear of them by the margin asked."
  (let* ((points (lispm-desktop-test--path-points (lispm-desktop-platinum-coach-mark-path 10 20 100 40 2)))
         (first (car points))
         (last (car (last points))))
    (should (string-prefix-p "M" (lispm-desktop-platinum-coach-mark-path 10 20 100 40)))
    (dolist (point points)
      (should (<= 9 (car point) 111))
      (should (<= 19 (cdr point) 61)))
    ;; Round: points on each side of the box.
    (should (seq-find (lambda (point) (< (car point) 15)) points))
    (should (seq-find (lambda (point) (> (car point) 105)) points))
    (should (seq-find (lambda (point) (> (cdr point) 55)) points))
    ;; A little further than once round: the ends are near each other at
    ;; the top, the last one outside the first.
    (should (< (cdr first) 30))
    (should (< (abs (- (car first) (car last))) 25))
    (should (< (cdr last) (cdr first))))
  (pcase-let ((`(,x ,y ,w ,h) (lispm-desktop-platinum-coach-mark-box 100 100 40 20 5)))
    (should (< x 95))
    (should (< y 95))
    (should (> (+ x w) 145))
    (should (> (+ y h) 125))
    (should (= (+ x (/ w 2)) 120))))

(ert-deftest lispm-desktop-test-coach-mark-menu-title ()
  "The menu bar title the coach mark circles is drawn with its loop, in
red; the others are not, and the mark is part of the title's image."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (lispm-desktop-platinum-current-palette palette)
         (red lispm-desktop-platinum-coach-mark-color))
    (cl-flet ((red-p (svg) (seq-find (lambda (path) (equal (dom-attr path 'stroke) red))
                                     (dom-by-tag svg 'path))))
      (should (red-p (lispm-desktop-platinum-draw-menu-title palette 60 :label "File" :marked t)))
      (should-not (red-p (lispm-desktop-platinum-draw-menu-title palette 60 :label "File"))))
    (let ((lispm-desktop-platinum-menus-function nil)
          (lispm-desktop-platinum-coach-mark nil))
      (let ((plain (nth 2 (car (lispm-desktop-platinum-menu-bar)))))
        (setq lispm-desktop-platinum-coach-mark '(menu . lispm-desktop-lambda))
        (should-not (equal (get-text-property 0 'display plain)
                           (get-text-property 0 'display (nth 2 (car (lispm-desktop-platinum-menu-bar))))))
        (should (lispm-desktop-platinum-coach-marked-p '(menu . lispm-desktop-lambda)))
        (should-not (lispm-desktop-platinum-coach-marked-p '(menu . lispm-desktop-application)))))))

(ert-deftest lispm-desktop-test-coach-mark-on-the-desktop ()
  "The desktop draws the coach mark round one of its icons, or round a
place on the screen, counted from the desktop's own top left; the loop
stays on the desktop.  Setting the mark draws again what shows it."
  (cl-letf (((symbol-function 'lispm-browser-available-p) #'ignore))
    (with-current-buffer (lispm-desktop-buffer)
      (let ((red (regexp-quote (format "stroke=\"%s\"" lispm-desktop-platinum-coach-mark-color)))
            (lispm-desktop--selected nil))
        (let ((lispm-desktop-platinum-coach-mark nil))
          (should-not (string-match-p red (lispm-desktop--svg 800 600))))
        (let ((lispm-desktop-platinum-coach-mark '(icon . home)))
          (let ((svg (lispm-desktop--svg 800 600)))
            (should (string-match-p red svg))
            (pcase-let ((`(,_ ,left ,top ,right ,_) (assq 'home lispm-desktop--hits))
                        (points (lispm-desktop-test--path-points
                                 (progn (string-match "<path d=\"\\([^\"]*\\)\"[^>]*stroke=" svg)
                                        (match-string 1 svg)))))
              (dolist (point points)
                (should (<= 0 (car point) 800))
                (should (<= 0 (cdr point) 600)))
              (should (seq-find (lambda (point) (< (car point) left)) points))
              (should (seq-find (lambda (point) (> (car point) (min right 790))) points))
              (should (seq-find (lambda (point) (< (cdr point) (+ top 10))) points)))))
        (let ((lispm-desktop-platinum-coach-mark (cons 'area (lambda () '(200 320 100 30)))))
          (let ((points (lispm-desktop-test--path-points
                         (let ((svg (lispm-desktop--svg 800 600 nil '(0 . 20))))
                           (string-match "<path d=\"\\([^\"]*\\)\"[^>]*stroke=" svg)
                           (match-string 1 svg)))))
            ;; 320 on the screen is 300 on a desktop under a 20-pixel menu bar.
            (should (seq-find (lambda (point) (< (cdr point) 300)) points))
            (should (seq-find (lambda (point) (> (cdr point) 330)) points))
            (should-not (seq-find (lambda (point) (> (cdr point) 350)) points))))
        (let ((lispm-desktop-platinum-coach-mark (cons 'area #'ignore)))
          (should-not (string-match-p red (lispm-desktop--svg 800 600))))))
    (let* ((drawn nil)
           (lispm-desktop-platinum-coach-mark nil)
           (lispm-desktop-platinum-coach-mark-functions (list (lambda (mark) (push mark drawn)))))
      (lispm-desktop-platinum-set-coach-mark '(icon . store))
      (should (equal drawn '((icon . store))))
      (lispm-desktop-platinum-set-coach-mark nil)
      (should (equal drawn '(nil (icon . store)))))))

(ert-deftest lispm-desktop-test-guide-steps ()
  "Every step of the guide has a title, words, an icon that is there and
keys Emacs can read; what it circles is a menu bar title, a desktop
icon or a place a function returns, and what Show Me calls is a function."
  (dolist (step lispm-desktop-guide-steps)
    (let ((properties (cdr step)))
      (should (stringp (plist-get properties :title)))
      (should (stringp (plist-get properties :text)))
      (should (file-readable-p (lispm-desktop-platinum-icon-file (plist-get properties :icon))))
      (pcase (plist-get properties :mark)
        ('nil)
        (`(menu . ,key) (should (memq key '(lispm-desktop-lambda lispm-desktop-application))))
        (`(icon . ,key) (should (assq key lispm-desktop-items)))
        (`(area . ,function) (should (fboundp function)))
        (mark (ert-fail (list "Unknown mark" mark))))
      (when-let* ((show (plist-get properties :show)))
        (should (fboundp show)))
      (dolist (entry (plist-get properties :keys))
        (should (stringp (nth 1 entry)))
        (dolist (keys (split-string (car entry) ", *" t))
          (should (key-valid-p keys))))))
  (should (file-readable-p (lispm-desktop-platinum-icon-file "guide"))))

(ert-deftest lispm-desktop-test-guide-shows-what-is-here ()
  "Steps whose subject is missing are left out, and keys whose predicate
says no; the guide's place stays within the steps left."
  (cl-letf (((symbol-function 'lispm-browser-available-p) #'ignore)
            ((symbol-function 'lispm-desktop-strip-edges) #'ignore)
            ((symbol-function 'lispm-desktop-coherence-available-p) #'ignore))
    (let ((keys (mapcar #'car (lispm-desktop-guide--steps))))
      (should-not (memq 'browser keys))
      (should-not (memq 'strip keys))
      (should (equal (car keys) 'welcome))
      (should (equal (car (last keys)) 'keys)))
    (should-not (lispm-desktop-guide--keys (assq 'windows lispm-desktop-guide-steps)))
    (let ((lispm-desktop-guide--index 99))
      (should (eq (car (lispm-desktop-guide--step)) 'keys))))
  (cl-letf (((symbol-function 'lispm-desktop-coherence-available-p) #'always))
    (should (equal (mapcar #'car (lispm-desktop-guide--keys (assq 'windows lispm-desktop-guide-steps)))
                   '("C-c h")))))

(ert-deftest lispm-desktop-test-guide-goes-step-by-step ()
  "Next and Back go from step to step, each circling what it speaks of;
Back does nothing on the first step, and Next on the last puts the guide
away, to start over the next time."
  (let ((lispm-desktop-guide--index 0)
        (lispm-desktop-guide--playing nil)
        (lispm-desktop-guide--undo nil)
        (marks nil)
        (closed nil))
    (cl-letf (((symbol-function 'lispm-desktop-guide--render) #'ignore)
              ((symbol-function 'lispm-desktop-guide--frame) #'ignore)
              ((symbol-function 'lispm-desktop-platinum-set-coach-mark) (lambda (mark) (push mark marks)))
              ((symbol-function 'lispm-desktop-guide-close) (lambda () (setq closed t))))
      (lispm-desktop-guide-previous)
      (should (= lispm-desktop-guide--index 0))
      (should-not marks)
      (lispm-desktop-guide-next)
      (should (= lispm-desktop-guide--index 1))
      (should (equal (car marks) '(menu . lispm-desktop-lambda)))
      (lispm-desktop-guide-previous)
      (should (= lispm-desktop-guide--index 0))
      (should (null (car marks)))
      (setq lispm-desktop-guide--index (1- (length (lispm-desktop-guide--steps))))
      (should-not closed)
      (lispm-desktop-guide-next)
      (should closed)
      (should (= lispm-desktop-guide--index 0)))))

(ert-deftest lispm-desktop-test-guide-plays-by-itself ()
  "Playing by itself, the guide does a step's Show Me a moment after the
step comes, if it is still on it; what Show Me opened is put back as the
guide goes on, is skipped or is closed, and the guide stops playing once
closed.  Skip starts the guide over the next time."
  (let ((lispm-desktop-guide--index 0)
        (lispm-desktop-guide--playing t)
        (lispm-desktop-guide--undo nil)
        (lispm-desktop-guide--timer nil)
        (timers nil)
        (put-back 0)
        (closed nil))
    (cl-letf (((symbol-function 'lispm-desktop-guide--render) #'ignore)
              ((symbol-function 'lispm-desktop-platinum-set-coach-mark) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest args) (push (cons function args) timers) nil))
              ((symbol-function 'lispm-desktop-guide-close)
               (lambda () (setq closed t) (lispm-desktop-guide--closed))))
      ;; The welcome shows nothing; the λ menu's step shows its menu.
      (lispm-desktop-guide--go 0)
      (should-not timers)
      (lispm-desktop-guide--go 1)
      (should (equal (mapcar #'car timers) '(lispm-desktop-guide--play)))
      (should (eq (car (nth 1 (car timers))) 'lambda-menu))
      ;; Played once the guide is on another step, it does nothing.
      (let ((shown nil))
        (cl-letf (((symbol-function 'lispm-desktop-guide--frame) #'always)
                  ((symbol-function 'lispm-desktop-guide-show-me) (lambda () (setq shown t))))
          (lispm-desktop-guide--play (assq 'welcome lispm-desktop-guide-steps))
          (should-not shown)
          (lispm-desktop-guide--play (lispm-desktop-guide--step))
          (should shown)))
      ;; What was opened is put back as the guide goes on.
      (setq lispm-desktop-guide--undo (lambda () (cl-incf put-back)))
      (lispm-desktop-guide-next)
      (should (= put-back 1))
      (should-not lispm-desktop-guide--undo)
      (setq lispm-desktop-guide--undo (lambda () (cl-incf put-back)))
      (lispm-desktop-guide-skip)
      (should closed)
      (should (= put-back 2))
      (should (= lispm-desktop-guide--index 0))
      (should-not lispm-desktop-guide--playing))))

(ert-deftest lispm-desktop-test-guide-shows-a-menu-for-a-while ()
  "A menu Show Me opens gets the keys it is to be typed first; playing by
itself, the guide closes it after a while with Escape, unless something
was typed meanwhile, and takes back an Escape left unread."
  (let ((lispm-desktop-guide--playing t)
        (unread-command-events nil)
        (tick nil)
        (seen nil))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest _) (setq tick function) (timer-create))))
      (defalias 'lispm-desktop-test--modal
        (lambda ()
          (interactive)
          ;; Reading the key typed first counts as an event read.
          (setq seen (pop unread-command-events))
          (cl-incf num-nonmacro-input-events)
          ;; Time passes, nothing typed: Escape is typed into the menu.
          (funcall tick)
          (push (copy-sequence unread-command-events) seen)
          ;; The menu closes otherwise, its Escape unread.
          ))
      (unwind-protect
          (progn
            (should-not (lispm-desktop-guide--modal 'lispm-desktop-test--modal '(return)))
            (should (equal seen '((escape) . return)))
            (should-not unread-command-events)
            ;; Something typed meanwhile: the menu is left to the user.
            (setq seen nil)
            (cl-letf (((symbol-function 'lispm-desktop-test--modal)
                       (lambda ()
                         (interactive)
                         (cl-incf num-nonmacro-input-events 2)
                         (funcall tick)
                         (setq seen (copy-sequence unread-command-events)))))
              (lispm-desktop-guide--modal 'lispm-desktop-test--modal)
              (should-not seen))
            ;; Not playing: no Escape at all.
            (let ((lispm-desktop-guide--playing nil))
              (setq tick nil)
              (cl-letf (((symbol-function 'lispm-desktop-test--modal) (lambda () (interactive) nil)))
                (lispm-desktop-guide--modal 'lispm-desktop-test--modal))
              (should-not tick)))
        (fmakunbound 'lispm-desktop-test--modal)))))

(ert-deftest lispm-desktop-test-guide-window ()
  "The guide's window shows the step's title, words and keys as key caps,
and its buttons: Show Me where the step shows something, Back but on the
first step, Next, or Done on the last; its bottom says how far along it
is.  The keys go from step to step."
  (cl-letf (((symbol-function 'lispm-browser-available-p) #'ignore))
    (with-temp-buffer
      (lispm-desktop-guide-mode)
      (let ((lispm-desktop-guide-buffer-name (buffer-name))
            (lispm-desktop-guide--index 0))
        (cl-flet ((labels ()
                    (let ((found nil))
                      (save-excursion
                        (goto-char (point-min))
                        (while (< (point) (point-max))
                          (when-let* ((label (get-text-property (point) 'lispm-desktop-platinum-label)))
                            (push (cons label (and (button-at (point)) t)) found))
                          (forward-char 1)))
                      (nreverse found))))
          (lispm-desktop-guide--render)
          (should (string-search "Welcome to LispM" (buffer-string)))
          (should (string-search "the next step" (buffer-string)))
          (should (equal (labels) '(("Skip" . t) ("Back") ("Next" . t))))
          (should (equal lispm-desktop-platinum-info
                         (format "Step 1 of %d" (length (lispm-desktop-guide--steps)))))
          (setq lispm-desktop-guide--index 1)
          (lispm-desktop-guide--render)
          (should (string-search "The λ menu" (buffer-string)))
          (should (equal (labels) '(("Skip" . t) ("Show Me" . t) ("Back" . t) ("Next" . t))))
          (setq lispm-desktop-guide--index (1- (length (lispm-desktop-guide--steps))))
          (lispm-desktop-guide--render)
          (should (equal (labels) '(("Back" . t) ("Done" . t))))))
      (dolist (key '("n" "SPC" "C-f" "C-n"))
        (should (eq (keymap-lookup lispm-desktop-guide-mode-map key) 'lispm-desktop-guide-next)))
      (dolist (key '("p" "DEL" "C-b" "C-p"))
        (should (eq (keymap-lookup lispm-desktop-guide-mode-map key) 'lispm-desktop-guide-previous)))
      (should (eq (keymap-lookup lispm-desktop-guide-mode-map "q") 'lispm-desktop-guide-close)))))

(ert-deftest lispm-desktop-test-guide-is-an-application ()
  "The guide is an application built in: the Applications folder holds it
after the App Store, the λ menu's Applications lists it, and the
Application menu while it is open; in front, its menus are File and Go,
Go listing every step, the one the guide is on checked."
  (let ((lispm-desktop-apps lispm-desktop-apps)
        (lispm-desktop-store-installed '((calculator :kind app :name "Calculator" :command calc)))
        (lispm-desktop-guide--index 1))
    (lispm-desktop-guide--define-app)
    (should (equal (mapcar #'car (lispm-desktop-apps-built-in)) '(guide)))
    (should (equal (seq-take (lispm-desktop-store--folder-items) 3)
                   (list '("App Store" "app-store-32" lispm-desktop-store-show)
                         (list "LispM Guide" "guide-32" (nth 2 (nth 1 (lispm-desktop-store--folder-items))))
                         (nth 2 (lispm-desktop-store--folder-items)))))
    (should (equal (mapcar #'car (lispm-desktop-store--folder-items)) '("App Store" "LispM Guide" "Calculator")))
    (should (equal (mapcar (lambda (item) (plist-get item :label)) (lispm-desktop-store-application-items))
                   '("LispM Guide" "Calculator")))
    (should (file-readable-p (lispm-desktop-platinum-icon-file "guide-32")))
    (cl-letf (((symbol-function 'lispm-desktop-guide-open-p) #'ignore))
      (should-not (assq 'guide (lispm-desktop-apps--available))))
    (cl-letf (((symbol-function 'lispm-desktop-guide-open-p) #'always))
      (should (assq 'guide (lispm-desktop-apps--available))))
    (let ((lispm-desktop-apps--frontmost 'guide))
      (should (equal (mapcar #'cadr (lispm-desktop-apps-menus)) '("File" "Go")))
      (should (equal (cdr (lispm-desktop-apps-application)) "LispM Guide")))
    (let* ((items (lispm-desktop-guide--go-items))
           (steps (seq-filter #'consp (cdr (memq 'separator items)))))
      (should (= (length steps) (length (lispm-desktop-guide--steps))))
      (should (equal (seq-keep (lambda (item) (plist-get item :checked)) steps) '(t)))
      (should (plist-get (nth 1 steps) :checked))
      (should (plist-get (car items) :enabled)))))

(ert-deftest lispm-desktop-test-guide-opens-centred ()
  "The guide's window stands in the middle of the desktop, its frame's
margins counted, and never past the desktop's top left."
  (should (equal (lispm-desktop-guide--centre '(0 20 1000 620) 400 300 '(6 1 7 7))
                 (cons (+ 6 (/ (- 1000 400 13) 2)) (+ 20 1 (/ (- 600 300 8) 2)))))
  (should (equal (lispm-desktop-guide--centre '(0 20 300 200) 400 300 '(6 1 7 7)) '(6 . 21))))

(ert-deftest lispm-desktop-test-guide-first-time ()
  "The guide shows itself once, and says so for every later start; where
it cannot be drawn, a line says how to start instead.  A batch Emacs
does neither."
  (let ((lispm-desktop-guide-shown nil)
        (saved nil)
        (shown nil)
        (said nil))
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (variable value &rest _) (push (cons variable value) saved) (set variable value)))
              ((symbol-function 'run-with-idle-timer) (lambda (&rest _) (setq shown t)))
              ((symbol-function 'lispm-desktop-guide--welcome) (lambda () (setq said t))))
      (lispm-desktop-guide-first-time)
      (should-not (or saved shown said))
      (let ((noninteractive nil))
        (cl-letf (((symbol-function 'display-graphic-p) #'ignore))
          (lispm-desktop-guide-first-time)
          (should (equal saved '((lispm-desktop-guide-shown . t))))
          (should said)
          (should-not shown)
          (setq said nil saved nil)
          (lispm-desktop-guide-first-time)
          (should-not (or saved said)))
        (setq lispm-desktop-guide-shown nil)
        (cl-letf (((symbol-function 'display-graphic-p) #'always)
                  ((symbol-function 'image-type-available-p) #'always))
          (lispm-desktop-guide-first-time)
          (should shown)
          (should-not said))))))

(ert-deftest lispm-desktop-test-guide-in-the-help-menu ()
  "The desktop's menus end with Help once the guide offers itself there."
  (let ((lispm-desktop-help-items-function nil))
    (should-not (assq 'lispm-desktop-menu-help (lispm-desktop-menus)))
    (setq lispm-desktop-help-items-function #'lispm-desktop-guide-help-items)
    (should (equal (car (last (lispm-desktop-menus))) '(lispm-desktop-menu-help "Help" lispm-desktop--help-items)))
    (should (eq (plist-get (car (lispm-desktop--help-items)) :action) 'lispm-desktop-guide))))

(defmacro lispm-desktop-test--with-themes (&rest body)
  "Run BODY with two empty themes, `lispm-desktop-test-light' enabled and
saved with Customize as `lispm-desktop-theme', and
`lispm-desktop-test-dark'; Emacs set up, the variable watched.  Themes
loaded are pushed onto `loads', and timers onto `timers' instead of run:
`lispm-desktop-test--run-timers' runs them, as Emacs would once the
command is done."
  (declare (indent 0))
  `(let ((properties (list (get 'user 'theme-settings)
                           (get 'lispm-desktop-theme 'theme-value)
                           (get 'lispm-desktop-theme 'saved-value)
                           (get 'lispm-desktop-theme 'customized-value)))
         (lispm-desktop-theme nil)
         (custom-enabled-themes nil)
         (custom-known-themes custom-known-themes)
         (enable-theme-functions nil)
         (disable-theme-functions nil)
         (lispm-desktop--set-up t)
         (loads nil)
         (timers nil))
     (dolist (theme '(lispm-desktop-test-light lispm-desktop-test-dark))
       (custom-declare-theme theme (intern (format "%s-theme" theme))))
     (unwind-protect
         (cl-letf (((symbol-function 'load-theme)
                    (lambda (theme &rest _) (push theme loads) (enable-theme theme)))
                   ((symbol-function 'run-at-time)
                    (lambda (_time _repeat function &rest args) (push (cons function args) timers)))
                   ((symbol-function 'custom-save-all) #'ignore))
           (customize-set-variable 'lispm-desktop-theme 'lispm-desktop-test-light)
           (enable-theme 'lispm-desktop-test-light)
           (add-variable-watcher 'lispm-desktop-theme #'lispm-desktop--theme-changed)
           ,@body)
       (remove-variable-watcher 'lispm-desktop-theme #'lispm-desktop--theme-changed)
       (mapc #'disable-theme custom-enabled-themes)
       (pcase-let ((`(,settings ,value ,saved ,customized) properties))
         (put 'user 'theme-settings settings)
         (put 'lispm-desktop-theme 'theme-value value)
         (put 'lispm-desktop-theme 'saved-value saved)
         (put 'lispm-desktop-theme 'customized-value customized)))))

(defmacro lispm-desktop-test--run-timers (timers)
  "Run the functions the variable TIMERS holds, (FUNCTION . ARGS) newest
first, and those they leave in turn, ten rounds at most; return what is
left after that."
  `(let ((rounds 0))
     (while (and ,timers (< (setq rounds (1+ rounds)) 10))
       (let ((due (reverse ,timers)))
         (setq ,timers nil)
         (dolist (timer due)
           (apply (car timer) (cdr timer)))))
     ,timers))

(ert-deftest lispm-desktop-test-theme-switch-loads-once ()
  "Switching themes loads the new theme once, and only it, though the value
saved with Customize names the old one until the switch saves the new:
enabling a theme sets the variable back to that saved value meanwhile,
which once loaded the old theme again and then the new one."
  (lispm-desktop-test--with-themes
    (lispm-desktop-set-theme 'lispm-desktop-test-dark)
    (should-not (lispm-desktop-test--run-timers timers))
    (should (equal loads '(lispm-desktop-test-dark)))
    (should (equal custom-enabled-themes '(lispm-desktop-test-dark)))
    (should (eq lispm-desktop-theme 'lispm-desktop-test-dark))))

(ert-deftest lispm-desktop-test-theme-set-loads-it ()
  "Setting `lispm-desktop-theme' once Emacs is set up loads that theme when
the command is done, and the variable keeps the value set, though another
was saved with Customize."
  (lispm-desktop-test--with-themes
    (setq lispm-desktop-theme 'lispm-desktop-test-dark)
    (should-not loads)
    (should-not (lispm-desktop-test--run-timers timers))
    (should (equal loads '(lispm-desktop-test-dark)))
    (should (equal custom-enabled-themes '(lispm-desktop-test-dark)))
    (should (eq lispm-desktop-theme 'lispm-desktop-test-dark))
    ;; Set to the theme enabled, nothing is loaded again.
    (setq lispm-desktop-theme 'lispm-desktop-test-dark)
    (should-not (lispm-desktop-test--run-timers timers))
    (should (equal loads '(lispm-desktop-test-dark)))))

(ert-deftest lispm-desktop-test-theme-loads-in-redisplay ()
  "On NS, a theme is loaded by the redisplay run for it, as it starts and
before it draws; it is loaded all the same when no redisplay runs, and
an error loading it is signalled once the redisplay is done."
  (lispm-desktop-test--with-themes
    (let ((events nil))
      ;; Only LispM Desktop is told it runs on NS: Emacs told so would treat
      ;; the batch frame as a window-system frame as a theme is disabled,
      ;; and CI's Emacs builds have no window system.
      (cl-letf (((symbol-function 'lispm-desktop--theme-in-redisplay-p) (lambda () t))
                ((symbol-function 'load-theme)
                 (lambda (theme &rest _) (push (list 'load theme) events) (enable-theme theme)))
                ((symbol-function 'redisplay)
                 (lambda (&rest _)
                   (push 'redisplay events)
                   (funcall pre-redisplay-function t)
                   (push 'drawn events))))
        (lispm-desktop-load-theme 'lispm-desktop-test-dark)
        (should (equal (reverse events) '(redisplay (load lispm-desktop-test-dark) drawn)))
        (should (equal custom-enabled-themes '(lispm-desktop-test-dark)))
        (should-not (advice-function-member-p #'lispm-desktop--load-pending-theme
                                              pre-redisplay-function))
        ;; A redisplay that does not run, as while a keyboard macro runs.
        (setq events nil)
        (cl-letf (((symbol-function 'redisplay) #'ignore))
          (lispm-desktop-load-theme 'lispm-desktop-test-light))
        (should (equal events '((load lispm-desktop-test-light))))
        (should (equal custom-enabled-themes '(lispm-desktop-test-light)))
        (should-not (advice-function-member-p #'lispm-desktop--load-pending-theme
                                              pre-redisplay-function))
        (cl-letf (((symbol-function 'load-theme)
                   (lambda (&rest _) (error "No such theme"))))
          (should (equal (should-error (lispm-desktop-load-theme 'lispm-desktop-test-dark))
                         '(error "No such theme"))))
        (should-not lispm-desktop--pending-theme)
        (should-not (advice-function-member-p #'lispm-desktop--load-pending-theme
                                              pre-redisplay-function))))))

(ert-deftest lispm-desktop-test-deps-requirements ()
  "Of a package's requirements, those not installed are returned; Emacs never."
  (let ((file (make-temp-file "lispm-desktop-test-" nil ".el"
                              (concat ";;; lispm-desktop-test-pkg.el --- A test  -*- lexical-binding: t; -*-\n"
                                      ";; Version: 0.1\n"
                                      ";; Package-Requires: ((emacs \"30.1\") (lispm-desktop-test-missing \"1.0\"))\n"
                                      ";;; Commentary:\n;;; Code:\n"
                                      "(provide 'lispm-desktop-test-pkg)\n;;; lispm-desktop-test-pkg.el ends here\n"))))
    (unwind-protect
        (should (equal (mapcar #'car (lispm-desktop-deps--requirements file)) '(lispm-desktop-test-missing)))
      (delete-file file))))

(ert-deftest lispm-desktop-test-deps-found-in-order ()
  "A project already on `load-path' is used as it is; else a checkout beside
the desktop's joins `load-path'; else package-vc installs its latest
release from its repository, lisp/ holding its sources."
  (let ((installed nil)
        (found nil)
        (load-path '("/elsewhere")))
    (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) found))
              ((symbol-function 'lispm-desktop-deps--requirements) #'ignore)
              ((symbol-function 'package-vc-install) (lambda (spec rev) (push (list spec rev) installed))))
      (setq found "/elsewhere/lispm-browser.el")
      (lispm-desktop-deps-ensure 'lispm-browser)
      (should (equal load-path '("/elsewhere")))
      (setq found nil)
      (cl-letf (((symbol-function 'lispm-desktop-deps--checkout)
                 (lambda (name) (and (equal name "lispm-agent") "/beside/lispm-agent/lisp"))))
        (lispm-desktop-deps-ensure 'lispm)
        (should (equal load-path '("/beside/lispm-agent/lisp" "/elsewhere")))
        (should-not installed)
        (lispm-desktop-deps-ensure 'lispm-browser))
      (should (equal installed '(((lispm-browser :url "https://github.com/LispMachineLab/lispm-browser"
                                                 :lisp-dir "lisp")
                                  :last-release)))))))

(ert-deftest lispm-desktop-test-directory ()
  "`lispm-desktop-directory' is the checkout, holding the agents' icons."
  (should (file-exists-p (expand-file-name "lisp/lispm-desktop.el" lispm-desktop-directory)))
  (should (file-directory-p lispm-desktop-agent-icon-directory)))


;;;; Platinum

(ert-deftest lispm-desktop-test-title-layout-centres ()
  "A short title sits in the middle of its bar, with ridges either side,
the boxes spaced as Mac OS 9 spaced them."
  (let* ((layout (lispm-desktop-platinum-title-layout 600 100))
         (s lispm-desktop-platinum-box-size)
         (ridges (plist-get layout :ridges)))
    (should (= (plist-get layout :title) 100))
    (should (= (plist-get layout :label-x) 250))
    ;; Close box three pixels in, four to the ridges.
    (should (= (plist-get layout :start) (+ 3 s)))
    (should (= (car (nth 0 ridges)) (+ 3 s 4)))
    ;; Four to the zoom box, three to the collapse box, three to the edge.
    (should (= (plist-get layout :end) (- 600 3 s 3 s)))
    (should (= (cdr (nth 1 ridges)) (- 600 3 s 3 s 5)))))

(ert-deftest lispm-desktop-test-title-layout-narrows-long-titles ()
  "A title too wide for its bar is narrowed, keeping ridges on both sides."
  (pcase-let* ((layout (lispm-desktop-platinum-title-layout 300 1000))
               (`((,a . ,b) (,c . ,d)) (plist-get layout :ridges)))
    (should (< (plist-get layout :title) 300))
    (should (>= (- b a) 12))
    (should (>= (- d c) 12))))

(ert-deftest lispm-desktop-test-title-layout-without-boxes ()
  "A bar lacking boxes gives their room to the title."
  (let ((all (lispm-desktop-platinum-title-layout 300 1000))
        (close-only (lispm-desktop-platinum-title-layout 300 1000 '(on nil nil))))
    (should (> (plist-get close-only :end) (plist-get all :end)))
    (should (> (plist-get close-only :title) (plist-get all :title)))))

(ert-deftest lispm-desktop-test-title-layout-margins ()
  "Margins stand the boxes nearest the bar's ends that far in, the title
stretch between giving up the room; an end without boxes keeps Mac OS
9's margin."
  (let* ((s lispm-desktop-platinum-box-size)
         (margin lispm-desktop-platinum-box-margin)
         (layout (lispm-desktop-platinum-title-layout 600 100 nil '(12 . 15)))
         (close-only (lispm-desktop-platinum-title-layout 600 100 '(on nil nil) '(12 . 15))))
    (should (equal (plist-get layout :margins) '(12 . 15)))
    (should (= (plist-get layout :start) (+ 12 s)))
    (should (= (plist-get layout :end) (- 600 15 s 3 s)))
    (should (equal (plist-get close-only :margins) (cons 12 margin)))
    (should (= (plist-get close-only :end) (- 600 margin)))
    (should (equal (plist-get (lispm-desktop-platinum-title-layout 600 100) :margins)
                   (cons margin margin)))))

(defun lispm-desktop-test--box-in-corner-p (frame-width frame-height box)
  "Return non-nil when BOX, (X0 Y0 X1 Y1) in pixels, crosses a corner macOS
resizes a frame FRAME-WIDTH by FRAME-HEIGHT pixels from."
  (pcase-let ((`(,x0 ,y0 ,x1 ,y1) box))
    (seq-some (pcase-lambda (`(,side ,end . ,square))
                (let ((x (if (eq side 'left) 0 (- frame-width square)))
                      (y (if (eq end 'top) 0 (- frame-height square))))
                  (and (< x0 (+ x square)) (< x x1) (< y0 (+ y square)) (< y y1))))
              lispm-desktop-platinum-resize-corners)))

(defun lispm-desktop-test--outer-boxes (left right top margins)
  "Return the boxes nearest the ends of a title bar from LEFT to RIGHT, its
top at TOP, standing them at MARGINS: the close box and the last box, as
\(X0 Y0 X1 Y1) in pixels."
  (pcase-let ((s lispm-desktop-platinum-box-size)
              (y (+ top lispm-desktop-platinum-box-top))
              (`(,lead . ,trail) margins))
    (list (list (+ left lead) y (+ left lead s) (+ y s))
          (list (- right trail s) y (- right trail) (+ y s)))))

(ert-deftest lispm-desktop-test-box-margins-clear-resize-corners ()
  "A title bar's outermost boxes stand clear of every corner macOS resizes
their frame from, and no further in than that takes, Mac OS 9's margin
the least.  A floating window's bar runs along its frame's top; rolled
up, the bar is the whole frame."
  (let ((margin lispm-desktop-platinum-box-margin))
    (pcase-dolist (`(,geometry ,expected)
                   '(((400 300 0 400 0) (12 . 12))         ; a floating window
                     ((400 22 0 400 0) (12 . 15))          ; rolled up
                     ((400 300 0 400 8) (12 . 12))         ; the boxes' top row in the corners'
                     ((400 300 0 400 9) (3 . 3))           ; the boxes under the corners
                     ((400 300 5 390 0) (7 . 3))           ; the bar in from the frame's sides
                     ((1440 900 300 900 40) (3 . 3))))     ; a window amid the screen
      (pcase-let* ((`(,width ,height ,left ,right ,top) geometry)
                   (margins (apply #'lispm-desktop-platinum--box-margins geometry))
                   (`(,lead . ,trail) margins))
        (should (equal margins expected))
        (dolist (box (lispm-desktop-test--outer-boxes left right top margins))
          (should-not (lispm-desktop-test--box-in-corner-p width height box)))
        ;; A pixel further out, a box lies in a corner, unless it stood at
        ;; Mac OS 9's margin already.
        (pcase-let ((`(,close ,last) (lispm-desktop-test--outer-boxes
                                      left right top (cons (1- lead) (1- trail)))))
          (should (or (= lead margin) (lispm-desktop-test--box-in-corner-p width height close)))
          (should (or (= trail margin) (lispm-desktop-test--box-in-corner-p width height last))))))))

(ert-deftest lispm-desktop-test-resized-from-corners ()
  "macOS resizes an undecorated frame from its corners unless it is full
screen; a decorated frame, or one of another window system, it does not."
  (cl-flet ((resized (system parameters)
              (cl-letf (((symbol-function 'framep) (lambda (_) system))
                        ((symbol-function 'frame-parameter)
                         (lambda (_ parameter) (alist-get parameter parameters))))
                (lispm-desktop-platinum--resized-from-corners-p 'frame))))
    (should (resized 'ns '((undecorated . t))))
    (should (resized 'ns '((undecorated . t) (fullscreen . maximized))))
    (should-not (resized 'ns '((undecorated . t) (fullscreen . fullboth))))
    (should-not (resized 'ns nil))
    (should-not (resized 'x '((undecorated . t))))))

(ert-deftest lispm-desktop-test-window-box-margins ()
  "A window's title bar keeps Mac OS 9's margins unless the window system
resizes its frame from the corners; then they come of where the bar runs
in the frame, its right end before the window's divider and scroll bar."
  (let ((margin lispm-desktop-platinum-box-margin))
    (should (equal (lispm-desktop-platinum--window-box-margins (selected-window))
                   (cons margin margin)))
    (cl-letf (((symbol-function 'lispm-desktop-platinum--resized-from-corners-p) #'always)
              ((symbol-function 'frame-native-width) (lambda (&optional _) 410))
              ((symbol-function 'frame-native-height) (lambda (&optional _) 300))
              ((symbol-function 'window-pixel-edges) (lambda (&optional _) '(0 0 410 300)))
              ((symbol-function 'window-right-divider-width) (lambda (&optional _) 4))
              ((symbol-function 'window-scroll-bar-width) (lambda (&optional _) 6)))
      ;; The bar ends ten pixels in from the frame's right, past where
      ;; the corner reaches less Mac OS 9's margin.
      (should (equal (lispm-desktop-platinum--window-box-margins (selected-window))
                     (cons 12 margin))))))

(defun lispm-desktop-test--bar-pieces (bar)
  "Return the pieces of title bar BAR in order, as (TARGET . WIDTH): TARGET
the box a piece is, or nil for the bar's ground, WIDTH its image's in
pixels."
  (mapcar (lambda (position)
            (let ((data (plist-get (cdr (get-text-property position 'display bar)) :data)))
              (cons (get-text-property position 'lispm-desktop-platinum-target bar)
                    (string-to-number
                     (dom-attr (with-temp-buffer
                                 (insert data)
                                 (libxml-parse-xml-region (point-min) (point-max)))
                               'width)))))
          (number-sequence 0 (1- (length bar)))))

(ert-deftest lispm-desktop-test-title-bar-stands-boxes-at-margins ()
  "A title bar stands its close box and its last box at the margins given,
its pieces filling its width."
  ;; Drawn as SVG: an Emacs without librsvg cannot.
  (skip-unless (image-type-available-p 'svg))
  (let* ((lispm-desktop-platinum-current-palette (lispm-desktop-platinum-derive-palette "#FFFFFF" "#000000"
                                                                                  "#777777" "#6363CE"))
         (s lispm-desktop-platinum-box-size)
         (pieces (lispm-desktop-test--bar-pieces
                  (lispm-desktop-platinum-draw-title-bar 600 t nil "codex" nil '(on on on)
                                                          nil nil nil '(12 . 15)))))
    (should (= (apply #'+ (mapcar #'cdr pieces)) 600))
    (should (equal (seq-take pieces 2) `((nil . 12) (close . ,s))))
    (should (equal (last pieces 2) `((collapse . ,s) (nil . 15))))))

(ert-deftest lispm-desktop-test-palette-derives-from-theme ()
  "The chrome is the content darkened; bevels lean to the text and to black."
  (let ((palette (lispm-desktop-platinum-derive-palette "#282a36" "#f8f8f2" "#6272a4" "#bd93f9")))
    (should (equal (plist-get palette :content) "#282a36"))
    (should (equal (plist-get palette :accent) "#bd93f9"))
    (cl-flet ((lum (color) (apply #'+ (lispm-desktop-platinum-rgb color))))
      (should (< (lum (plist-get palette :chrome)) (lum "#282a36")))
      (should (> (lum (plist-get palette :light)) (lum (plist-get palette :chrome))))
      (should (< (lum (plist-get palette :shade)) (lum (plist-get palette :chrome)))))))

(ert-deftest lispm-desktop-test-boxes-draw ()
  "Every box kind draws, plain, modified and pressed, in Dracula too."
  (let ((palette (lispm-desktop-platinum-derive-palette "#282a36" "#f8f8f2" "#6272a4" "#bd93f9")))
    (dolist (kind '(close zoom collapse))
      (should (eq (dom-tag (lispm-desktop-platinum-draw-box palette kind)) 'svg))
      (should (eq (dom-tag (lispm-desktop-platinum-draw-box palette kind t)) 'svg))
      (should (eq (dom-tag (lispm-desktop-platinum-draw-box palette kind nil t)) 'svg)))
    (should (eq (dom-tag (lispm-desktop-platinum-draw-title-part palette 50 :active t :ridges '((0 . 49))))
                'svg))
    (should (eq (dom-tag (lispm-desktop-platinum-draw-triangle palette t)) 'svg))))

(ert-deftest lispm-desktop-test-title-box-pixels ()
  "The close box is a recessed well round a dark frame, a bevel and a
square shaded from grey to white; the zoom and collapse boxes add their
glyphs; the bar is lit along its top, shaded along its bottom."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (close (lispm-desktop-platinum-draw-box palette 'close))
         (zoom (lispm-desktop-platinum-draw-box palette 'zoom))
         (collapse (lispm-desktop-platinum-draw-box palette 'collapse)))
    (should (equal (lispm-desktop-test--size close) '(13 . 21)))
    (cl-flet ((at (svg x y) (lispm-desktop-test--pixel svg x y)))
      (should (equal (at close 5 0) "#FFFFFF"))
      (should (equal (at close 5 1) "#CCCCCC"))
      (should (equal (at close 5 19) "#999999"))
      (should (equal (at close 5 20) "#000000"))
      (should (equal (at close 0 3) "#888888"))
      (should (equal (at close 12 15) "#FFFFFF"))
      (should (equal (at close 1 4) "#222222"))
      (should (equal (at close 2 5) "#FFFFFF"))
      (should (equal (at close 3 6) "#999999"))
      (should (equal (at close 9 12) "#FFFFFF"))
      (should (equal (at close 10 8) "#888888"))
      (should (equal (at zoom 7 6) "#222222"))
      (should (equal (at zoom 4 10) "#222222"))
      (should (equal (at collapse 6 8) "#222222"))
      (should (equal (at collapse 6 10) "#222222")))
    (let ((part (lispm-desktop-platinum-draw-title-part palette 40 :active t :ridges '((2 . 30)))))
      (cl-flet ((at (x y) (lispm-desktop-test--pixel part x y)))
        ;; Six ridges, their ends engraved.
        (should (equal (at 2 3) "#FFFFFF"))
        (should (equal (at 2 4) "#CCCCCC"))
        (should (equal (at 3 4) "#777777"))
        (should (equal (at 30 4) "#777777"))
        (should (equal (at 30 3) "#CCCCCC"))
        (should (equal (at 10 13) "#FFFFFF"))
        (should (equal (at 10 15) "#CCCCCC"))))
    (let ((inactive (lispm-desktop-platinum-draw-title-part palette 40)))
      (should (equal (lispm-desktop-test--pixel inactive 10 0) "#DDDDDD"))
      (should (equal (lispm-desktop-test--pixel inactive 10 20) "#555555")))))

(ert-deftest lispm-desktop-test-rgb-reads-hex-without-a-display ()
  "A #RRGGBB colour is read exactly, even in batch."
  (should (equal (lispm-desktop-platinum-rgb "#ff0080") (list 1.0 0.0 (/ 128 255.0))))
  (should (equal (lispm-desktop-platinum-blend "#000000" "#ff0080" 1.0) "#ff0080")))

(ert-deftest lispm-desktop-test-window-controls ()
  "A lone window zooms but cannot roll up; a side window only closes."
  (save-window-excursion
    (delete-other-windows)
    (should (equal (lispm-desktop-platinum--window-controls (selected-window)) '(on on nil)))
    (let ((below (split-window-below)))
      (should (equal (lispm-desktop-platinum--window-controls below) '(on on on))))
    (let ((side (display-buffer-in-side-window (get-buffer-create " lispm-desktop-test-side")
                                               '((side . left)))))
      (should (equal (lispm-desktop-platinum--window-controls side) '(on nil nil))))))

(ert-deftest lispm-desktop-test-chrome-controls ()
  "Chrome, such as the browser's address window, only closes, and its close
box runs the buffer's own close function."
  (save-window-excursion
    (delete-other-windows)
    (let ((chrome (split-window-below))
          (closed nil))
      (set-window-parameter chrome 'lispm-desktop-chrome t)
      (should (equal (lispm-desktop-platinum--window-controls chrome) '(on nil nil)))
      (with-temp-buffer
        (setq-local lispm-desktop-platinum-box-functions (list :close (lambda (window) (setq closed window))))
        (set-window-buffer chrome (current-buffer))
        (lispm-desktop-platinum-close-window chrome)
        (should (eq closed chrome))
        (should (buffer-live-p (current-buffer)))))))

(defun lispm-desktop-test--menu-bar-titles ()
  "Return the keys of the menu bar's left end."
  (mapcar #'car (lispm-desktop-platinum-menu-bar)))

(ert-deftest lispm-desktop-test-desktop-darkens-selected-icons ()
  "A selected icon is its SVG with every colour halved."
  (should (equal (lispm-desktop--darken "<rect fill=\"#FF8040\"/><rect fill=\"#000000\"/>")
                 "<rect fill=\"#7F4020\"/><rect fill=\"#000000\"/>")))

(ert-deftest lispm-desktop-test-desktop-items ()
  "The browser's icon shows where the browser can run; the Trash is last."
  (cl-letf (((symbol-function 'lispm-browser-available-p) #'ignore))
    (should (equal (mapcar #'car (lispm-desktop--items))
                   '(home editor applications store control-panels trash))))
  (cl-letf (((symbol-function 'lispm-browser-available-p) #'always))
    (should (equal (mapcar #'car (lispm-desktop--items))
                   '(home editor browser applications store control-panels trash)))))

(ert-deftest lispm-desktop-test-desktop-icons-stand-where-put ()
  "Icons with no place stand in the column at the right edge, the Trash in
the bottom corner; one put elsewhere stands there, measured from the edges
nearest it, kept whole on the desktop; the others keep to free cells."
  (cl-letf (((symbol-function 'lispm-browser-available-p) #'ignore)
            ((symbol-function 'lispm-desktop--font) (lambda () '("Geneva" 12 10))))
    (pcase-let ((`(,cell-width . ,cell-height) (lispm-desktop--cell-size)))
      (let ((lispm-desktop-icon-places nil))
        (let ((layout (lispm-desktop--layout 800 600)))
          (should (equal (mapcar #'cdr layout)
                         `((,(- 800 lispm-desktop-margin cell-width) ,lispm-desktop-margin)
                           (,(- 800 lispm-desktop-margin cell-width)
                            ,(+ lispm-desktop-margin cell-height lispm-desktop-gap))
                           (,(- 800 lispm-desktop-margin cell-width)
                            ,(+ lispm-desktop-margin (* 2 (+ cell-height lispm-desktop-gap))))
                           (,(- 800 lispm-desktop-margin cell-width)
                            ,(+ lispm-desktop-margin (* 3 (+ cell-height lispm-desktop-gap))))
                           (,(- 800 lispm-desktop-margin cell-width)
                            ,(+ lispm-desktop-margin (* 4 (+ cell-height lispm-desktop-gap))))
                           (,(- 800 lispm-desktop-margin cell-width)
                            ,(- 600 lispm-desktop-margin cell-height)))))))
      ;; Home put at the top left; Emacs keeps its cell, the second.
      (let ((lispm-desktop-icon-places '((home :left 20 :top 30))))
        (let ((layout (lispm-desktop--layout 800 600)))
          (should (equal (cdr (seq-find (lambda (entry) (eq (car (car entry)) 'home)) layout))
                         '(20 30)))
          (should (equal (nth 2 (seq-find (lambda (entry) (eq (car (car entry)) 'editor)) layout))
                         lispm-desktop-margin))))
      ;; An icon put on Emacs's cell pushes it down the column.
      (let ((lispm-desktop-icon-places `((home :right ,lispm-desktop-margin :top ,lispm-desktop-margin)
                                         (trash :right ,lispm-desktop-margin :top ,lispm-desktop-margin))))
        (should (> (nth 2 (seq-find (lambda (entry) (eq (car (car entry)) 'editor))
                                    (lispm-desktop--layout 800 600)))
                   lispm-desktop-margin)))
      ;; A place past the edge is kept whole on the desktop.
      (should (equal (lispm-desktop--place-xy '(:left 2000 :top -5) 800 600)
                     (cons (- 800 cell-width) 0)))
      ;; Measured from the nearest edges, so it follows them.
      (should (equal (lispm-desktop--xy-place 700 500 800 600)
                     (list :right (- 800 700 cell-width) :bottom (- 600 500 cell-height))))
      (should (equal (lispm-desktop--xy-place 10 20 800 600) '(:left 10 :top 20)))
      (should (equal (lispm-desktop--place-xy (lispm-desktop--xy-place 700 20 800 600) 1000 600)
                     '(900 . 20))))))

(ert-deftest lispm-desktop-test-desktop-drop-keeps-the-others ()
  "Dropping an icon keeps every other where it stands and saves them all."
  (cl-letf (((symbol-function 'lispm-browser-available-p) #'ignore)
            ((symbol-function 'lispm-desktop--font) (lambda () '("Geneva" 12 10)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 800))
            ((symbol-function 'window-body-height) (lambda (&rest _) 601))
            ((symbol-function 'lispm-desktop--render) #'ignore)
            ((symbol-function 'customize-save-variable)
             (lambda (variable value) (set variable value))))
    (let ((lispm-desktop-icon-places nil)
          (pitch (+ (cdr (lispm-desktop--cell-size)) lispm-desktop-gap)))
      ;; Dropped near the top left, it lands on the grid cell nearest.
      (lispm-desktop--drop 'home 100 120 nil)
      (should (equal (alist-get 'home lispm-desktop-icon-places)
                     (list :left (+ lispm-desktop-margin lispm-desktop-column)
                           :top (+ lispm-desktop-margin (* 2 pitch)))))
      (should (equal (alist-get 'editor lispm-desktop-icon-places)
                     (lispm-desktop--xy-place (- 800 lispm-desktop-margin lispm-desktop-column)
                                              (+ lispm-desktop-margin pitch)
                                              800 600)))
      ;; Dropped on Emacs's cell, it takes the free cell nearest instead.
      (lispm-desktop--drop 'home (- 800 lispm-desktop-margin lispm-desktop-column)
                           (+ lispm-desktop-margin pitch 5) nil)
      (let ((home (lispm-desktop--place-xy (alist-get 'home lispm-desktop-icon-places) 800 600))
            (editor (lispm-desktop--place-xy (alist-get 'editor lispm-desktop-icon-places) 800 600)))
        (should-not (equal home editor))
        (should (lispm-desktop--cell-free-p home (list (list (car editor) (cdr editor))))))
      ;; Near the bottom right corner, it lines up with the Trash's row.
      (lispm-desktop--drop 'home (- 800 200) (- 600 lispm-desktop-margin 60) nil)
      (should (equal (plist-get (alist-get 'home lispm-desktop-icon-places) :bottom)
                     lispm-desktop-margin))
      (should (zerop (mod (- (plist-get (alist-get 'home lispm-desktop-icon-places) :right)
                             lispm-desktop-margin)
                          lispm-desktop-column)))
      (lispm-desktop-arrange-icons)
      (should-not lispm-desktop-icon-places))))

(ert-deftest lispm-desktop-test-desktop-drag-survives-empty-reads ()
  "Reads that come back empty, as the mouse is looked at between moves, do
not end the drag, nor do moves Emacs reports: the icon is still dropped
where it is let go."
  (cl-letf (((symbol-function 'lispm-browser-available-p) #'ignore)
            ((symbol-function 'lispm-desktop--font) (lambda () '("Geneva" 12 10)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 800))
            ((symbol-function 'window-body-height) (lambda (&rest _) 601))
            ((symbol-function 'window-inside-pixel-edges) (lambda (&rest _) '(0 0 800 601)))
            ((symbol-function 'lispm-desktop--copy-frames) (lambda (&rest _) '(icon . name)))
            ((symbol-function 'lispm-desktop--place-copy) #'ignore)
            ((symbol-function 'lispm-desktop-popup-dispose) #'ignore)
            ((symbol-function 'lispm-desktop--render) #'ignore)
            ((symbol-function 'customize-save-variable)
             (lambda (variable value) (set variable value))))
    (let* ((lispm-desktop-icon-places nil)
           (cell (lispm-desktop--cell-at 'home nil))
           (press (cons (+ (car cell) 40) (+ (cadr cell) 16)))
           (reads 0)
           (pointer press)
           (unread-command-events nil))
      (cl-letf (((symbol-function 'lispm-desktop--pointer) (lambda (_) pointer))
                ((symbol-function 'read-event)
                 (lambda (&rest _)
                   (setq reads (1+ reads))
                   (cond ((> reads 10) '(drag-mouse-1 (nil) (nil)))
                         ;; Every other read times out with nothing.
                         ((cl-oddp reads)
                          (setq pointer (cons (- (car pointer) 20) (+ (cdr pointer) 10)))
                          nil)
                         (t '(mouse-movement (nil)))))))
        (should (lispm-desktop--drag 'home nil press)))
      (should (equal (alist-get 'home lispm-desktop-icon-places)
                     (lispm-desktop--snap (- (car cell) 100) (+ (cadr cell) 50) 800 600
                                          (mapcar #'cdr (seq-remove
                                                         (lambda (entry) (eq (car (car entry)) 'home))
                                                         (lispm-desktop--layout 800 600))))))
      (should-not unread-command-events))))

(ert-deftest lispm-desktop-test-desktop-copy-follows-the-pointer ()
  "The dragged copy follows the pointer pixel by pixel, keeping it where it
grabbed the icon, and only the drop snaps the icon to the grid; Escape
gives the drag up, the icon left where it was."
  (cl-letf (((symbol-function 'lispm-browser-available-p) #'ignore)
            ((symbol-function 'lispm-desktop--font) (lambda () '("Geneva" 12 10)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 800))
            ((symbol-function 'window-body-height) (lambda (&rest _) 601))
            ((symbol-function 'window-inside-pixel-edges) (lambda (&rest _) '(0 0 800 601)))
            ((symbol-function 'lispm-desktop--copy-frames) (lambda (&rest _) '(icon . name)))
            ((symbol-function 'lispm-desktop-popup-dispose) #'ignore)
            ((symbol-function 'lispm-desktop--render) #'ignore)
            ((symbol-function 'customize-save-variable)
             (lambda (variable value) (set variable value))))
    (let* ((lispm-desktop-icon-places nil)
           (cell (lispm-desktop--cell-at 'home nil))
           (press (cons (+ (car cell) 40) (+ (cadr cell) 16)))
           (pointer press)
           (script nil)
           (placed nil)
           (unread-command-events nil))
      (cl-letf (((symbol-function 'lispm-desktop--pointer) (lambda (_) pointer))
                ((symbol-function 'lispm-desktop--place-copy)
                 (lambda (_frames _key x y _window) (push (cons x y) placed)))
                ((symbol-function 'read-event)
                 (lambda (&rest _)
                   (pcase-let ((`(,dx ,dy . ,event) (pop script)))
                     (setq pointer (cons (+ (car press) dx) (+ (cdr press) dy)))
                     event))))
        ;; A jitter shows no copy; then the copy goes wherever the pointer
        ;; does, by the pixel.
        (setq script '((-3 2) (-57 23) (-101 47) (-101 47 drag-mouse-1 (nil) (nil))))
        (should (lispm-desktop--drag 'home nil press))
        (should (equal placed (list (cons (- (car cell) 101) (+ (cadr cell) 47))
                                    (cons (- (car cell) 57) (+ (cadr cell) 23)))))
        ;; Let go, the icon is put on the grid cell nearest.
        (let ((saved (lispm-desktop--place-xy (alist-get 'home lispm-desktop-icon-places) 800 600)))
          (should (equal (lispm-desktop--xy-place (car saved) (cdr saved) 800 600)
                         (lispm-desktop--snap (- (car cell) 101) (+ (cadr cell) 47) 800 600
                                              (mapcar #'cdr (seq-remove
                                                             (lambda (entry) (eq (car (car entry)) 'home))
                                                             (lispm-desktop--layout 800 600))))))
          (should-not (equal saved (car placed)))
          ;; Escape, and nothing more moves.
          (setq placed nil
                script '((-30 40) (-30 40 . 27)))
          (lispm-desktop--drag 'home nil press)
          (should placed)
          (should (equal (lispm-desktop--place-xy (alist-get 'home lispm-desktop-icon-places) 800 600)
                         saved)))))))

(ert-deftest lispm-desktop-test-desktop-right-click-menu ()
  "A right click on the desktop offers Arrange Icons and the picture; on an
icon, Open first."
  (cl-letf (((symbol-function 'lispm-browser-available-p) #'ignore))
    (let ((shown nil)
          (key nil)
          (lispm-desktop-icon-places '((home :left 10 :top 10))))
      (cl-letf (((symbol-function 'lispm-desktop-mouse-select)
                 (lambda (_) (with-current-buffer (lispm-desktop-buffer)
                               (setq lispm-desktop--selected key))))
                ((symbol-function 'lispm-desktop-popup-frame-xy) (lambda (_) '(0 . 0)))
                ((symbol-function 'lispm-desktop-popup-menu) (lambda (items &rest _) (setq shown items))))
        (lispm-desktop-context-menu nil)
        (should (equal (plist-get (car shown) :label) "Arrange Icons"))
        (should (plist-get (car shown) :enabled))
        (setq key 'home)
        (lispm-desktop-context-menu nil)
        (should (equal (plist-get (car shown) :label) "Open"))
        (should (member "Desktop Picture…"
                        (mapcar (lambda (item) (and (consp item) (plist-get item :label))) shown)))))))

(ert-deftest lispm-desktop-test-desktop-double-click-opens-not-drags ()
  "A double click opens the icon, even when its first click strayed a few
pixels; a press let go after a jitter is a click, and a drop back on the
icon's own cell saves nothing."
  (cl-letf (((symbol-function 'lispm-browser-available-p) #'ignore)
            ((symbol-function 'lispm-desktop--font) (lambda () '("Geneva" 12 10)))
            ((symbol-function 'window-body-width) (lambda (&rest _) 800))
            ((symbol-function 'window-body-height) (lambda (&rest _) 601))
            ((symbol-function 'window-inside-pixel-edges) (lambda (&rest _) '(0 0 800 601)))
            ((symbol-function 'lispm-desktop--copy-frames) (lambda (&rest _) '(icon . name)))
            ((symbol-function 'lispm-desktop--place-copy) #'ignore)
            ((symbol-function 'lispm-desktop-popup-dispose) #'ignore)
            ((symbol-function 'lispm-desktop--render) #'ignore))
    (let* ((lispm-desktop-icon-places nil)
           (lispm-desktop--last-click nil)
           (saved nil)
           (opened nil)
           (cell (lispm-desktop--cell-at 'home nil))
           (press (cons (+ (car cell) 40) (+ (cadr cell) 16)))
           (pointer press)
           (moves nil)
           (unread-command-events nil))
      (cl-letf (((symbol-function 'customize-save-variable)
                 (lambda (variable value) (setq saved t) (set variable value)))
                ((symbol-function 'lispm-desktop-open) (lambda () (setq opened t)))
                ((symbol-function 'select-window) #'ignore)
                ((symbol-function 'lispm-desktop-apps-set-frontmost) #'ignore)
                ((symbol-function 'lispm-desktop--select) #'ignore)
                ((symbol-function 'lispm-desktop--item-at) (lambda (_) 'home))
                ((symbol-function 'lispm-desktop--pointer) (lambda (_) pointer))
                ((symbol-function 'read-event)
                 (lambda (&rest _)
                   (if moves
                       (progn (setq pointer (pop moves)) nil)
                     '(mouse-1 (nil))))))
        (let ((event `(down-mouse-1 (nil 1 ,press 0))))
          ;; A first click that jitters 3 and 4 pixels is a click.
          (setq moves (list (cons (+ (car press) 3) (+ (cdr press) 4))))
          (lispm-desktop-mouse-select event)
          (should-not saved)
          (should (eq (car lispm-desktop--last-click) 'home))
          ;; The second press, quick and close by, opens without dragging.
          (setq moves (list (cons (+ (car press) 40) (cdr press))))
          (lispm-desktop-mouse-select event)
          (should opened)
          (should-not saved)
          (should moves)
          ;; A drag that comes back to the icon's own cell saves nothing.
          (setq moves (list (cons (+ (car press) 10) (+ (cdr press) 9)))
                opened nil)
          (lispm-desktop-mouse-select event)
          (should-not opened)
          (should-not saved))))))

(ert-deftest lispm-desktop-test-desktop-draws-the-dragged-copy ()
  "The dragged copy is the icon alone and its name inverted, as Mac OS 9's
drag image was, no piece of the desktop about them; the icon stands
centred at the top of its cell, its name under it."
  (cl-letf (((symbol-function 'lispm-browser-available-p) #'ignore)
            ((symbol-function 'lispm-desktop--font) (lambda () '("Geneva" 12 10)))
            ((symbol-function 'string-pixel-width) (lambda (&rest _) 40)))
    (let ((lispm-desktop-picture nil))
      (pcase-let ((`(,icon . ,name) (lispm-desktop--copy-svgs 'home)))
        (should (string-match-p "width=\"32\" height=\"32\" viewBox=\"0 0 32 32\"" icon))
        (should-not (string-match-p (regexp-quote (lispm-desktop--color)) icon))
        (should-not (string-match-p "opacity" icon))
        (should (string-match-p (format "<rect width=\"48\" height=\"16\" fill=\"%s\"/>"
                                        (or (lispm-desktop-platinum-color :text) "#000000"))
                                name))
        (should (string-match-p (format "fill=\"%s\">Home</text></svg>\\'"
                                        (or (lispm-desktop-platinum-color :content) "#FFFFFF"))
                                name)))
      (should (equal (lispm-desktop--copy-place 'home 100 50)
                     (list (cons (+ 100 40 -16) 50) (cons (+ 100 40 -24) (+ 50 32 4))))))))

(ert-deftest lispm-desktop-test-desktop-picture-scaled-copy ()
  "A picture filling the desktop is drawn from a copy scaled to it once one
is made; until then, and for a picture at its own size, from itself."
  (let* ((lispm-desktop-picture (expand-file-name "icons/os9/globe-32.svg" lispm-desktop-directory))
         (file lispm-desktop-picture)
         (time (file-attribute-modification-time (file-attributes file)))
         (started nil))
    (cl-letf (((symbol-function 'lispm-desktop--picture-size) (lambda (_) '(4000 . 3000)))
              ((symbol-function 'frame-scale-factor) (lambda (&rest _) 2.0))
              ((symbol-function 'lispm-desktop--scale-picture)
               (lambda (&rest args) (setq started args))))
      (let ((lispm-desktop-picture-position 'fill)
            (lispm-desktop--scaled nil))
        (should (equal (lispm-desktop--picture-file 800 600) file))
        (should (equal started (list file time 1600 1200)))
        (setq lispm-desktop--scaled (list file time 1600 1200 file))
        (setq started nil)
        (should (equal (lispm-desktop--picture-file 800 600) file))
        (should-not started)
        ;; Grown past the copy, the copy is made again.
        (lispm-desktop--picture-file 1000 600)
        (should started))
      (let ((lispm-desktop-picture-position 'center)
            (lispm-desktop--scaled nil))
        (setq started nil)
        (should (equal (lispm-desktop--picture-file 800 600) file))
        (should-not started)))))

(ert-deftest lispm-desktop-test-desktop-selects-and-names ()
  "The arrow keys step through the icons, wrapping; the selected one's name
is inverted, and a click finds the icon drawn under it, the Trash in the
bottom corner."
  (cl-letf (((symbol-function 'lispm-browser-available-p) #'ignore))
    (with-current-buffer (lispm-desktop-buffer)
      (setq lispm-desktop--selected nil)
      (lispm-desktop-select-next)
      (should (eq lispm-desktop--selected 'home))
      (lispm-desktop-select-previous)
      (should (eq lispm-desktop--selected 'trash))
      (lispm-desktop-select-previous)
      (should (eq lispm-desktop--selected 'control-panels))
      (setq lispm-desktop--selected 'home)
      (let ((svg (lispm-desktop--svg 800 600)))
        (should (string-match-p "fill=\"#000000\"/><text[^>]*fill=\"#FFFFFF\">Home</text>" svg))
        (should (string-match-p "fill=\"#FFFFFF\"/><text[^>]*fill=\"#000000\">Trash</text>" svg))
        (pcase-let ((`(,_ ,left ,top ,right ,bottom) (assq 'home lispm-desktop--hits)))
          (should (eq (lispm-desktop--key-at (/ (+ left right) 2) (/ (+ top bottom) 2)) 'home)))
        (should (> (nth 2 (assq 'trash lispm-desktop--hits)) 400))
        (should-not (lispm-desktop--key-at 10 10)))
      (lispm-desktop-deselect)
      (should-not lispm-desktop--selected))))

(ert-deftest lispm-desktop-test-desktop-picture ()
  "A desktop picture lies over the desktop's colour as its position says."
  (let ((lispm-desktop-picture (expand-file-name "icons/os9/globe-32.svg" lispm-desktop-directory)))
    (let ((lispm-desktop-picture-position 'fill))
      (should (string-match-p "<image href=\"globe-32.svg\"[^>]*xMidYMid slice"
                              (lispm-desktop--svg 800 600))))
    (let ((lispm-desktop-picture-position 'fit))
      (should (string-match-p "xMidYMid meet" (lispm-desktop--svg 800 600))))
    (let ((lispm-desktop-picture-position 'tile))
      (should (string-match-p "<pattern id=\"lispm-desktop-tile\"" (lispm-desktop--svg 800 600)))))
  (let ((lispm-desktop-picture nil)
        (lispm-desktop-color "#123456"))
    (let ((svg (lispm-desktop--svg 800 600)))
      (should (string-match-p "fill=\"#123456\"" svg))
      (should-not (string-match-p "<image" svg)))))

(ert-deftest lispm-desktop-test-desktop-color-is-hex ()
  "Any colour Emacs takes reaches the desktop's SVG written #RRGGBB: the
12-digit hex `read-color' returns, short hex, rgb:, and names, which a
graphic display resolves."
  (dolist (case '(("#483D8B" . "#483d8b")
                  ("#48483D3D8B8B" . "#483d8b")
                  ("#F00" . "#ff0000")
                  ("rgb:ff/80/00" . "#ff8000")
                  ("dark slate blue" . "#483d8b")
                  ("DarkSlateBlue" . "#483d8b")
                  ("gray50" . "#7f7f7f")))
    (should (equal (lispm-desktop--hex-color (car case)) (cdr case))))
  (should-not (lispm-desktop--hex-color "no such colour"))
  (should-not (lispm-desktop--hex-color ""))
  (should-not (lispm-desktop--hex-color nil))
  (cl-letf (((symbol-function 'display-graphic-p) #'always)
            ((symbol-function 'color-values)
             (lambda (color &optional _) (and (equal color "SystemWindow") '(65535 0 0)))))
    (should (equal (lispm-desktop--hex-color "SystemWindow") "#ff0000")))
  (let ((lispm-desktop-picture nil))
    (let ((lispm-desktop-color "dark slate blue"))
      (should (equal (lispm-desktop--color) "#483d8b"))
      (should (string-match-p "fill=\"#483d8b\"" (lispm-desktop--svg 800 600))))
    (let ((lispm-desktop-color "#48483D3D8B8B"))
      (should (equal (lispm-desktop--color) "#483d8b")))
    ;; A colour Emacs does not know leaves the desktop the theme's, not black.
    (let ((lispm-desktop-color "no such colour"))
      (should (equal (lispm-desktop--color)
                     (let ((lispm-desktop-color nil)) (lispm-desktop--color)))))))

(ert-deftest lispm-desktop-test-settings-choose-color-keeps-hex ()
  "The colour chosen is kept written #RRGGBB, as a name typed comes back
from `read-color' 12-digit; nothing, or no colour, is not kept."
  (let (saved)
    (cl-letf (((symbol-function 'lispm-desktop-settings--save)
               (lambda (variable value) (push (cons variable value) saved))))
      (dolist (answer '("#48483D3D8B8B" "" "no such colour"))
        (cl-letf (((symbol-function 'read-color) (lambda (&rest _) answer)))
          (lispm-desktop-settings--choose-color))))
    (should (equal saved '((lispm-desktop-color . "#483d8b"))))))

(ert-deftest lispm-desktop-test-trash-full ()
  "The Trash is full when it holds anything but Finder's .DS_Store: counted
where it can be read, measured by its link count where it cannot."
  (cl-letf (((symbol-function 'directory-files)
             (lambda (&rest _) '(".DS_Store" "old.txt"))))
    (should (lispm-desktop-trash-full-p)))
  (cl-letf (((symbol-function 'directory-files) (lambda (&rest _) '(".DS_Store"))))
    (should-not (lispm-desktop-trash-full-p)))
  (dolist (case '((3 t nil) (4 t t) (2 nil nil) (3 nil t)))
    (pcase-let ((`(,links ,ds-store ,full) case))
      (cl-letf (((symbol-function 'directory-files)
                 (lambda (&rest _) (signal 'file-error '("Operation not permitted"))))
                ((symbol-function 'file-attributes)
                 (lambda (&rest _) (list t links 501 20 nil nil nil 64)))
                ((symbol-function 'file-exists-p) (lambda (_) ds-store)))
        (should (eq (and (lispm-desktop-trash-full-p) t) full))))))

(ert-deftest lispm-desktop-test-closing-the-last-window-shows-the-desktop ()
  "The close box takes a window away while others remain in the main area;
the last one uncovers the desktop, as the desktop's setup has it."
  (save-window-excursion
    (delete-other-windows)
    (let ((lispm-desktop-platinum-last-window-function #'lispm-desktop-show)
          (first (generate-new-buffer " lispm-desktop-test-first"))
          (second (generate-new-buffer " lispm-desktop-test-second")))
      (set-window-buffer (selected-window) first)
      (let ((other (split-window-right)))
        (set-window-buffer other second)
        (lispm-desktop-platinum-close-window other)
        (should-not (window-live-p other))
        (should-not (buffer-live-p second))
        (lispm-desktop-platinum-close-window (selected-window))
        (should-not (buffer-live-p first))
        (should (lispm-desktop-p (selected-window)))))))

(ert-deftest lispm-desktop-test-editor-opens-on-the-startup-screen ()
  "In the editor, the last window closed shows Emacs's own startup screen."
  (save-window-excursion
    (delete-other-windows)
    (let ((lispm-desktop-empty-function #'lispm-desktop-editor--show-splash)
          (lispm-desktop-platinum-last-window-function #'lispm-desktop-show)
          (buffer (generate-new-buffer " lispm-desktop-test-last")))
      (set-window-buffer (selected-window) buffer)
      (lispm-desktop-platinum-close-window (selected-window))
      (should (equal (buffer-name (window-buffer)) lispm-desktop-editor-splash-buffer-name)))))

(ert-deftest lispm-desktop-test-editor-strip-never-scrolls ()
  "The title strip is never scrolled sideways: drawn, its point is before
the tabs and its window unscrolled, as a strip scrolled to the tabs' end
while it was narrower than they for a moment showed no tabs."
  (save-window-excursion
    (delete-other-windows)
    (let ((buffer (lispm-desktop-editor--strip-buffer)))
      (unwind-protect
          (cl-letf (((symbol-function 'lispm-desktop-editor-frame) #'selected-frame)
                    ((symbol-function 'lispm-desktop-sidebar-current-view) (lambda () 'files))
                    ((symbol-function 'lispm-desktop-platinum-tabs)
                     (lambda (_tabs width) (make-string (/ width (frame-char-width)) ?=))))
            (with-current-buffer buffer
              (should-not auto-hscroll-mode))
            (set-window-buffer (selected-window) buffer)
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (insert (make-string 200 ?=))))
            (set-window-point (selected-window) (with-current-buffer buffer (point-max)))
            (set-window-hscroll (selected-window) 40)
            (lispm-desktop-editor--render-strip)
            (should (= (window-hscroll (selected-window)) 0))
            (should (= (window-point (selected-window)) 1))
            (should (= (with-current-buffer buffer (point)) 1)))
        (kill-buffer buffer)))))

(ert-deftest lispm-desktop-test-new-workspace-opens-on-the-desktop ()
  "A workspace opened onto *scratch* shows the desktop instead."
  (save-window-excursion
    (delete-other-windows)
    (set-window-buffer (selected-window) (get-buffer-create "*scratch*"))
    (lispm-desktop--on-new-workspace)
    (should (lispm-desktop-p))))

(defmacro lispm-desktop-test--with-folder (&rest body)
  "Run BODY with `folder' bound to a fresh folder of a few items."
  (declare (indent 0))
  `(let ((folder (make-temp-file "lispm-desktop-finder" t)))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "zeta" folder))
           (make-directory (expand-file-name "Alpha/.git" folder) t)
           (dolist (name '("notes.org" "init.el" "beta.txt" ".hidden"))
             (write-region "" nil (expand-file-name name folder)))
           ,@body)
       (delete-directory folder t))))

(ert-deftest lispm-desktop-test-finder-reads-folders ()
  "A folder's items come by name, case aside, hidden ones left out, each
with its large icon."
  (lispm-desktop-test--with-folder
    (let ((items (lispm-desktop-finder--read folder)))
      (should (equal (mapcar #'car items) '("Alpha" "beta.txt" "init.el" "notes.org" "zeta")))
      (should (cdr (assoc "zeta" items)))
      (should-not (cdr (assoc "init.el" items)))
      (should (equal (lispm-desktop-finder--icon-name folder (assoc "Alpha" items)) "folder-git-32"))
      (should (equal (lispm-desktop-finder--icon-name folder (assoc "zeta" items)) "folder-32"))
      (should (equal (lispm-desktop-finder--icon-name folder (assoc "init.el" items)) "document-lisp-32"))
      (should (equal (lispm-desktop-finder--icon-name folder (assoc "notes.org" items)) "document-org-32"))
      (should (equal (lispm-desktop-finder--icon-name folder (assoc "beta.txt" items)) "document-32")))))

(ert-deftest lispm-desktop-test-finder-history ()
  "Opening a folder records the one left: back, forward and up move through
them, up selecting the folder it came from."
  (lispm-desktop-test--with-folder
    (let ((root (file-name-as-directory folder)))
      (with-current-buffer (lispm-desktop-finder--buffer)
        (setq lispm-desktop-finder--directory nil lispm-desktop-finder--back nil lispm-desktop-finder--forward nil))
      (lispm-desktop-finder--go folder)
      (lispm-desktop-finder--go (expand-file-name "zeta" folder) t)
      (lispm-desktop-finder--in
        (should (equal lispm-desktop-finder--back (list root)))
        (should (equal (lispm-desktop-finder--title lispm-desktop-finder--directory) "zeta")))
      (lispm-desktop-finder-back)
      (lispm-desktop-finder--in
        (should (equal lispm-desktop-finder--directory root))
        (should (equal lispm-desktop-finder--forward (list (file-name-as-directory
                                                            (expand-file-name "zeta" folder))))))
      (lispm-desktop-finder-forward)
      (lispm-desktop-finder-up)
      (lispm-desktop-finder--in
        (should (equal lispm-desktop-finder--directory root))
        (should (equal lispm-desktop-finder--selected "zeta"))
        (should (string-match-p "\\`5 items" (lispm-desktop-finder--info)))))
    (kill-buffer lispm-desktop-finder-buffer-name)))

(ert-deftest lispm-desktop-test-finder-moves-to-trash ()
  "Command-Delete sends the selected item to the Trash and selects the next."
  (lispm-desktop-test--with-folder
    (let ((trashed nil))
      (cl-letf (((symbol-function 'move-file-to-trash)
                 (lambda (file) (push file trashed) (delete-file file))))
        (lispm-desktop-finder--go folder)
        (lispm-desktop-finder--select "beta.txt")
        (lispm-desktop-finder-move-to-trash)
        (should (equal trashed (list (expand-file-name "beta.txt" folder))))
        (lispm-desktop-finder--in
          (should-not (assoc "beta.txt" lispm-desktop-finder--items))
          (should (equal lispm-desktop-finder--selected "init.el")))))
    (kill-buffer lispm-desktop-finder-buffer-name)))

(ert-deftest lispm-desktop-test-window-rolls-up-its-chrome ()
  "A floating window whose kind rolls its chrome, as the Finder window,
Control Panels and the App Store do, rolls up to its title bar, its header
and its bottom line put away, and down to its height again, its buffer
untouched."
  (save-window-excursion
    (delete-other-windows)
    (let ((frame (selected-frame))
          (window (selected-window))
          (lispm-desktop-app-window-kinds '((kind :roll chrome)))
          (heights nil))
      (with-temp-buffer
        (setq-local header-line-format "Header")
        (set-window-buffer window (current-buffer))
        (set-frame-parameter frame 'lispm-desktop-window-kind 'kind)
        (unwind-protect
            (cl-letf (((symbol-function 'set-frame-height)
                       (lambda (_frame height &optional _pretend _pixelwise) (push height heights))))
              (let ((height (frame-text-height frame)))
                (lispm-desktop-app-window-collapse-box window)
                (should (lispm-desktop-app-window-rolled-p frame))
                (should (eq (window-parameter window 'header-line-format) 'none))
                (should (eq (window-parameter window 'mode-line-format) 'none))
                (should (equal header-line-format "Header"))
                (lispm-desktop-app-window-collapse-box window)
                (should-not (lispm-desktop-app-window-rolled-p frame))
                (should (= (car heights) height))
                (should-not (window-parameter window 'header-line-format))
                (should-not (window-parameter window 'mode-line-format))))
          (set-frame-parameter frame 'lispm-desktop-window-kind nil)
          (set-frame-parameter frame 'lispm-desktop-platinum-rolled-up nil))))))

(ert-deftest lispm-desktop-test-sidebar-knows-no-view ()
  "The sidebar draws lists and shows one view at a time; the views add
themselves, so its file names no agent and no shell."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "lisp/lispm-desktop-sidebar.el" lispm-desktop-directory))
    (should-not (re-search-forward "lispm-desktop-\\(?:agent\\|shell\\)" nil t))))

(ert-deftest lispm-desktop-test-sidebar-list-views ()
  "A list view gives only its buffer, mode and drawing: the sidebar shows it
drawn anew, draws it again with point on the same line, and hides it.  A
view defined again keeps its place."
  (let ((lispm-desktop-sidebar-views nil)
        (lispm-desktop-sidebar-prepare-function nil)
        (lispm-desktop-sidebar-change-hook nil)
        (name " *lispm-desktop-test list*")
        (renders 0))
    (cl-flet ((render ()
                (setq renders (1+ renders))
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert "first\nsecond\nthird\n"))))
      (lispm-desktop-sidebar-define-view 'one :title "One" :buffer name :mode #'special-mode :render #'ignore)
      (lispm-desktop-sidebar-define-view 'two :title "Two" :visible #'ignore :show #'ignore :hide #'ignore)
      (lispm-desktop-sidebar-define-view 'one :title "One" :buffer name :mode #'special-mode :render #'render)
      (should (equal (mapcar #'car lispm-desktop-sidebar-views) '(one two)))
      (save-window-excursion
        (delete-other-windows)
        (unwind-protect
            (progn
              (lispm-desktop-sidebar-select 'one)
              (should (eq (lispm-desktop-sidebar-current-view) 'one))
              (should (= renders 1))
              (with-current-buffer name
                (should (derived-mode-p 'special-mode))
                (goto-char (point-min))
                (forward-line 1))
              (lispm-desktop-sidebar-refresh-list 'one)
              (should (= renders 2))
              (with-current-buffer name
                (should (= (line-number-at-pos) 2)))
              (lispm-desktop-sidebar-select 'one)
              (should-not (lispm-desktop-sidebar-current-view)))
          (kill-buffer name))))))

(ert-deftest lispm-desktop-test-views-ask-the-editor-first ()
  "A view is asked for through the editor, which comes forward first."
  (let ((asked nil))
    (cl-letf (((symbol-function 'lispm-desktop-editor--prepare-view) (lambda () (setq asked t)))
              (lispm-desktop-sidebar-views '((test :visible ignore :show ignore :hide ignore)))
              (lispm-desktop-sidebar-prepare-function #'lispm-desktop-editor--prepare-view))
      (lispm-desktop-sidebar-select 'test)
      (should asked))))

(ert-deftest lispm-desktop-test-views-asked-for-elsewhere-show ()
  "A view asked for while the editor is out of sight is shown, though the
editor, coming forward, brings back the view it showed before; asked for
in the editor in front, it hides."
  (let* ((in-front nil)
         ;; The view the editor shows, seen or not: Agents, from startup.
         (shown 'agents)
         (define (lambda (view)
                   (list view
                         :visible (lambda () (and in-front (eq shown view)))
                         :show (lambda () (setq shown view))
                         :hide (lambda () (setq shown nil))))))
    (cl-letf (((symbol-function 'lispm-desktop-editor--prepare-view) (lambda () (setq in-front t)))
              (lispm-desktop-sidebar-views (list (funcall define 'files) (funcall define 'agents)))
              (lispm-desktop-sidebar-prepare-function #'lispm-desktop-editor--prepare-view)
              (lispm-desktop-sidebar-change-hook nil))
      (lispm-desktop-sidebar-select 'agents)
      (should (eq shown 'agents))
      (lispm-desktop-sidebar-select 'agents)
      (should-not shown)
      ;; Another view asked for from the desktop replaces the one brought back.
      (lispm-desktop-sidebar-select 'agents)
      (setq in-front nil)
      (lispm-desktop-sidebar-select 'files)
      (should (eq shown 'files)))))

(ert-deftest lispm-desktop-test-menu-bar-follows-the-application ()
  "The menu bar holds the menus of the application in front."
  (let ((lispm-desktop-platinum-menus-function #'lispm-desktop-apps-menus))
    (let ((lispm-desktop-apps--frontmost 'editor))
      (should (equal (lispm-desktop-test--menu-bar-titles)
                     '(lispm-desktop-lambda lispm-desktop-editor-file lispm-desktop-editor-edit lispm-desktop-editor-view
                                            lispm-desktop-editor-project lispm-desktop-editor-window))))
    (let ((lispm-desktop-apps--frontmost 'desktop))
      (should (equal (lispm-desktop-test--menu-bar-titles)
                     '(lispm-desktop-lambda lispm-desktop-menu-file lispm-desktop-menu-view lispm-desktop-menu-go
                                            lispm-desktop-menu-special))))
    (let ((lispm-desktop-apps--frontmost 'browser))
      (cl-letf (((symbol-function 'lispm-browser-extensions-supported-p) #'ignore))
        (should (equal (lispm-desktop-test--menu-bar-titles)
                       '(lispm-desktop-lambda lispm-desktop-browser-file lispm-desktop-browser-view lispm-desktop-browser-go
                                              lispm-desktop-browser-bookmarks lispm-desktop-browser-window))))
      ;; Where pages can run Chrome extensions, they have a menu.
      (cl-letf (((symbol-function 'lispm-browser-extensions-supported-p) #'always))
        (should (equal (lispm-desktop-test--menu-bar-titles)
                       '(lispm-desktop-lambda lispm-desktop-browser-file lispm-desktop-browser-view lispm-desktop-browser-go
                                              lispm-desktop-browser-bookmarks lispm-desktop-browser-extensions-menu
                                              lispm-desktop-browser-window)))))))

(ert-deftest lispm-desktop-test-application-follows-its-selected-window ()
  "Selecting a floating window puts its application in front; the screen,
selected by a click on the menu bar, leaves the one in front."
  (let ((lispm-desktop-apps--frontmost 'editor)
        (lispm-desktop-apps-frontmost-functions nil)
        (app 'browser))
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (_frame parameter) (and (eq parameter 'lispm-desktop-app) app))))
      (lispm-desktop-apps--follow-window)
      (should (eq (lispm-desktop-apps-frontmost) 'browser))
      (setq app nil)
      (lispm-desktop-apps--follow-window)
      (should (eq (lispm-desktop-apps-frontmost) 'browser)))))

(ert-deftest lispm-desktop-test-browser-boxes ()
  "The browser's zoom box moves it between full screen and floating; only
the floating browser rolls up."
  (cl-letf (((symbol-function 'lispm-desktop-browser-floating-p) #'ignore))
    (should (equal (lispm-desktop-browser--boxes)
                   '(:close lispm-desktop-browser--close-window :zoom lispm-desktop-browser--zoom-box))))
  (cl-letf (((symbol-function 'lispm-desktop-browser-floating-p) #'always))
    (should (eq (plist-get (lispm-desktop-browser--boxes) :collapse) 'lispm-desktop-browser--collapse-box))))

(ert-deftest lispm-desktop-test-application-menu-lists-applications ()
  "The Application menu lists the applications, the one in front checked;
the browser only where it can run."
  (let ((lispm-desktop-apps--frontmost 'editor))
    (cl-letf (((symbol-function 'lispm-browser-available-p) #'ignore))
      (let ((items (lispm-desktop-apps-application-items)))
        (should (equal (mapcar (lambda (item) (plist-get item :label)) items)
                       '("Desktop" "Emacs")))
        (should (plist-get (nth 1 items) :checked))
        (should (equal (cdr (lispm-desktop-apps-application)) "Emacs"))))
    (cl-letf (((symbol-function 'lispm-browser-available-p) #'always))
      (should (= 3 (length (lispm-desktop-apps-application-items)))))))

(ert-deftest lispm-desktop-test-documents-go-to-the-editor ()
  "Outside the editor, documents are sent to it; the browser's pages, the
desktop and internal buffers are not."
  (with-current-buffer (get-buffer-create "lispm-desktop-test-document.txt")
    (setq buffer-file-name "/tmp/lispm-desktop-test-document.txt")
    (let ((document (current-buffer)))
      (cl-letf (((symbol-function 'lispm-desktop-editor--in-workspace-p) #'ignore))
        (should (lispm-desktop-editor--astray-p document nil))
        (should-not (lispm-desktop-editor--astray-p (lispm-desktop-buffer) nil))
        (let ((lispm-desktop-editor--displaying t))
          (should-not (lispm-desktop-editor--astray-p document nil))))
      (cl-letf (((symbol-function 'lispm-desktop-editor--in-workspace-p) #'always))
        (should-not (lispm-desktop-editor--astray-p document nil))))
    (setq buffer-file-name nil)
    (kill-buffer)))

(ert-deftest lispm-desktop-test-documents-leave-the-floating-browser ()
  "A document opened from the floating browser, an application's window
of its own, goes to the editor; popups are left be."
  (with-current-buffer (get-buffer-create "lispm-desktop-test-document.txt")
    (setq buffer-file-name "/tmp/lispm-desktop-test-document.txt")
    (let ((document (current-buffer))
          (parameters '((lispm-desktop-window . t))))
      (cl-letf (((symbol-function 'frame-parent) #'always)
                ((symbol-function 'frame-parameter)
                 (lambda (_frame parameter) (alist-get parameter parameters))))
        (should (lispm-desktop-editor--astray-p document nil))
        (setq parameters nil)
        (should-not (lispm-desktop-editor--astray-p document nil))))
    (setq buffer-file-name nil)
    (kill-buffer)))

(ert-deftest lispm-desktop-test-new-workspace-leaves-sessions-behind ()
  "A workspace made while a session's window is selected starts on its own
first buffer alone: the session is not carried along, as it was when
the first buffer went through the routing that keeps sessions uncovered."
  (let ((session (get-buffer-create "lispm-desktop-test-session"))
        (display-buffer-alist '((lispm-desktop-main-area--session-p lispm-desktop-main-area-display-session)
                                (lispm-desktop-main-area--astray-p lispm-desktop-main-area-display-file)))
        (switch-to-buffer-obey-display-actions t)
        (tab-bar-mode-was tab-bar-mode))
    (cl-letf (((symbol-function 'lispm-desktop-terminal-buffer-p)
               (lambda (buffer) (eq (get-buffer buffer) session))))
      (unwind-protect
          (progn
            (delete-other-windows)
            (set-window-buffer (selected-window) session)
            (let ((tab-bar-new-tab-choice #'lispm-desktop--new-tab-empty))
              (tab-bar-new-tab))
            (should (equal (mapcar (lambda (w) (buffer-name (window-buffer w)))
                                   (window-list nil 'no-mini))
                           (list lispm-desktop-empty-buffer-name))))
        (tab-bar-close-tab)
        (unless tab-bar-mode-was (tab-bar-mode -1))
        (kill-buffer session)))))

(ert-deftest lispm-desktop-test-new-workspace-starts-on-startup-screen ()
  "A new workspace shows Emacs's startup screen itself, handing tab-bar no
buffer: one shown through `switch-to-buffer' would obey the editor's
routing while the old workspace is still in front."
  (save-window-excursion
    (should-not (lispm-desktop-editor--new-workspace-buffer))
    (should (equal (buffer-name (window-buffer)) lispm-desktop-editor-splash-buffer-name))))

(ert-deftest lispm-desktop-test-workspaces-chooser-keys ()
  "The workspaces' panel answers from the keyboard: Return or Space chooses
the workspace ringed, Tab, C-Tab, Command-` and C-f ring the next, C-n
the one a row down, a digit chooses at once; Escape gives up, and any
other key closes the panel and goes on to Emacs."
  (let ((tabs (mapcar (lambda (name) `(tab (name . ,name))) '("A" "B" "C" "D" "E" "F")))
        (lispm-desktop-workspaces-columns 4))
    (cl-letf (((symbol-function 'lispm-desktop-editor-frame) #'selected-frame)
              ((symbol-function 'lispm-desktop-apps-hidden-p) #'ignore)
              ((symbol-function 'lispm-desktop-apps-screen) #'selected-frame)
              ((symbol-function 'lispm-desktop-editor--workspace-tab-p) #'always)
              ((symbol-function 'lispm-desktop-editor--current-workspace) (lambda () "A"))
              ((symbol-function 'lispm-desktop-workspaces--layout) #'ignore)
              ((symbol-function 'lispm-desktop-workspaces--width) (lambda (_) 100))
              ((symbol-function 'lispm-desktop-workspaces--text) (lambda (&rest _) ""))
              ((symbol-function 'lispm-desktop-popup-make-frame) (lambda (_) 'panel))
              ((symbol-function 'lispm-desktop-popup-fill) #'ignore)
              ((symbol-function 'lispm-desktop-popup-set-size) #'ignore)
              ((symbol-function 'lispm-desktop-popup-show) #'ignore)
              ((symbol-function 'lispm-desktop-popup-dispose) #'ignore)
              ((symbol-function 'frame-root-window) #'ignore)
              ((symbol-function 'window-text-pixel-size) (lambda (&rest _) '(100 . 100)))
              ((symbol-function 'redisplay) #'ignore))
      (let ((tab-bar-tabs-function (lambda (&optional _) tabs)))
        (cl-flet ((choose (events &optional step)
                    (let ((unread-command-events events))
                      (list (lispm-desktop-workspaces-choose step) unread-command-events))))
          (should (equal (choose '(return)) '((workspace . "A") nil)))
          (should (equal (choose '(tab return)) '((workspace . "B") nil)))
          (should (equal (choose '(?\C-f ?\C-f ?\s)) '((workspace . "C") nil)))
          (should (equal (choose '(backtab return)) '((workspace . "F") nil)))
          (should (equal (choose '(S-iso-lefttab return)) '((workspace . "F") nil)))
          (should (equal (choose '(C-tab ?\M-` return)) '((workspace . "C") nil)))
          (should (equal (choose '(?\C-n return)) '((workspace . "E") nil)))
          (should (equal (choose '(?\C-p return)) '((workspace . "C") nil)))
          (should (equal (choose '(?3)) '((workspace . "C") nil)))
          (should (equal (choose '(escape)) '(nil nil)))
          (should (equal (choose '(?x)) '(nil (?x))))
          (should (equal (choose '(return) 1) '((workspace . "B") nil))))))))

(ert-deftest lispm-desktop-test-editor-workspaces-leave-out-desktop-and-browser ()
  "The desktop's and the browser's workspaces are not the editor's."
  (should-not (lispm-desktop-editor--workspace-tab-p '(tab (name . "Desktop"))))
  (should-not (lispm-desktop-editor--workspace-tab-p '(tab (name . "Browser"))))
  (should (lispm-desktop-editor--workspace-tab-p '(tab (name . "lispm-desktop")))))

(ert-deftest lispm-desktop-test-buffer-manager-counts-process-trees ()
  "A buffer's process is counted with every process under it."
  (let ((processes (make-hash-table)))
    (puthash 10 '(1 1.5 1000) processes)
    (puthash 11 '(10 2.0 500) processes)
    (puthash 12 '(11 0.5 250) processes)
    (puthash 20 '(1 9.0 9000) processes)
    (let ((children (lispm-desktop-buffer-manager--children processes)))
      (should (equal (lispm-desktop-buffer-manager--tree-cost 10 processes children) '(4.0 . 1750)))
      (should (equal (lispm-desktop-buffer-manager--tree-cost 12 processes children) '(0.5 . 250))))))

(ert-deftest lispm-desktop-test-buffer-manager-rows ()
  "Every buffer but internal ones has a row, its text measured."
  (with-current-buffer (get-buffer-create "lispm-desktop-test-row")
    (erase-buffer)
    (insert "hello")
    (cl-letf (((symbol-function 'lispm-desktop-buffer-manager--processes) (lambda () (make-hash-table))))
      (let* ((entries (lispm-desktop-buffer-manager--entries))
             (row (assq (current-buffer) entries)))
        (should row)
        (should (equal (get-text-property 0 'lispm-desktop-value (aref (cadr row) 2)) 5))
        (should-not (seq-find (lambda (entry) (string-prefix-p " " (buffer-name (car entry))))
                              entries))))
    (kill-buffer)))

(ert-deftest lispm-desktop-test-palette-takes-named-colours ()
  "Colours a theme names replace the derived ones; the rest stay derived."
  (let* ((derived (lispm-desktop-platinum-derive-palette "#282a36" "#f8f8f2" "#6272a4" "#bd93f9"))
         (palette (lispm-desktop-platinum--merge-palette derived '(:chrome "#DDDDDD" :accent "#6363CE"))))
    (should (equal (plist-get palette :chrome) "#DDDDDD"))
    (should (equal (plist-get palette :accent) "#6363CE"))
    (should (equal (plist-get palette :shade) (plist-get derived :shade)))
    ;; The derived palette itself is left untouched.
    (should (equal (plist-get derived :accent) "#bd93f9"))))

(ert-deftest lispm-desktop-test-platinum-theme ()
  "The platinum theme loads, and names Platinum's greys for the chrome."
  (let ((custom-theme-load-path (cons (expand-file-name "themes/" lispm-desktop-directory)
                                      custom-theme-load-path)))
    (unwind-protect
        (progn
          (load-theme 'platinum t)
          (should (memq 'platinum custom-enabled-themes))
          (should (equal (plist-get lispm-desktop-platinum-colors :chrome) "#DDDDDD"))
          (should (equal (plist-get lispm-desktop-platinum-colors :accent) "#6363CE")))
      (disable-theme 'platinum))
    (should-not (plist-get lispm-desktop-platinum-colors :chrome))))

(ert-deftest lispm-desktop-test-window-bottom ()
  "A window's bottom edge counts a list's items, or shows the buffer."
  ;; Drawn as SVG: an Emacs without librsvg cannot.
  (skip-unless (image-type-available-p 'svg))
  (let ((lispm-desktop-platinum-current-palette (lispm-desktop-platinum-derive-palette "#282a36" "#f8f8f2"
                                                                                 "#6272a4" "#bd93f9")))
    (with-temp-buffer
      (rename-buffer "lispm-desktop-test 100% done" t)
      (let ((left (lispm-desktop-platinum-mode-line-left)))
        ;; A % in the buffer name is escaped, as the bottom is a mode line.
        (should (string-search "100%% done" left)))
      (setq-local lispm-desktop-platinum-info "3 sessions")
      (should (string-search "3 sessions" (lispm-desktop-platinum-mode-line-left)))
      (should (string-empty-p (string-trim (substring-no-properties
                                            (lispm-desktop-platinum-mode-line-right))))))))

(ert-deftest lispm-desktop-test-sidebar-count ()
  "List windows count their items in the singular or the plural."
  (should (equal (lispm-desktop-sidebar-count 1 "session") "1 session"))
  (should (equal (lispm-desktop-sidebar-count 3 "session" "1 running") "3 sessions, 1 running")))

(ert-deftest lispm-desktop-test-sidebar-keeps-width ()
  "The frame narrowing, the main area gives up the columns and the sidebar
keeps its own, also once its text narrowed, as its scroll bar narrows it
by widening the fringe; the width dragged to is the one kept after."
  (save-window-excursion
    (delete-other-windows)
    (let* ((frame (selected-frame))
           (columns (window-total-width (frame-root-window frame)))
           (lines (frame-height frame))
           (buffer (get-buffer-create " lispm-desktop-test sidebar")))
      (unwind-protect
          (let* ((sidebar (lispm-desktop-sidebar--display buffer))
                 (main (window-main-window frame))
                 (width (window-total-width sidebar)))
            (should (memq #'lispm-desktop-sidebar--keep-width
                          (buffer-local-value 'window-size-change-functions buffer)))
            ;; The text narrowing lifts what Emacs preserved.  The scroll
            ;; bar narrows it by the fringe, which a batch Emacs's frame
            ;; has none of: margins do here.
            (set-window-margins sidebar 0 2)
            ;; As redisplay runs it.
            (lispm-desktop-sidebar--keep-width sidebar)
            (set-frame-size frame (- columns 10) lines)
            (should (= (window-total-width sidebar) width))
            (should (= (window-total-width main) (- columns 10 width)))
            (adjust-window-trailing-edge sidebar 4 t)
            (lispm-desktop-sidebar--keep-width sidebar)
            (set-frame-size frame columns lines)
            (should (= (window-total-width sidebar) (+ width 4))))
        (set-frame-size frame columns lines)
        (kill-buffer buffer)))))

(ert-deftest lispm-desktop-test-pressed-box-is-inverted ()
  "A box held down is lit from below, its square darkened."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (up (lispm-desktop-platinum-draw-box palette 'close))
         (down (lispm-desktop-platinum-draw-box palette 'close nil t)))
    (should (equal (lispm-desktop-test--pixel up 9 12) "#FFFFFF"))
    (should (equal (lispm-desktop-test--pixel down 9 12) "#BBBBBB"))
    (should (equal (lispm-desktop-test--pixel down 10 8) "#CCCCCC"))))

(ert-deftest lispm-desktop-test-toolbar-buttons-with-image-icons-are-apart ()
  "Two toolbar buttons whose icons are images read alike, \" +\", but are
different buttons: pressing one draws only it pushed in."
  (let* ((icon (lambda (file) (propertize " " 'display (list 'image :type 'png :file file))))
         (hermes (lispm-desktop-sidebar-toolbar-button (concat (funcall icon "hermes.png") "+")
                                                        #'lispm-desktop-agent-codex-new "Hermes" 'header-line))
         (lispm (lispm-desktop-sidebar-toolbar-button (concat (funcall icon "lispm.png") "+")
                                                       #'lispm-desktop-agent-lispm-new "LispM" 'header-line)))
    (should (equal hermes lispm))
    (should-not (equal (get-text-property 0 'lispm-desktop-platinum-target hermes)
                       (get-text-property 0 'lispm-desktop-platinum-target lispm)))))

(ert-deftest lispm-desktop-test-press-runs-on-release-over-button ()
  "A pressed button runs only when let go over it, and is drawn pushed in
while held."
  (save-window-excursion
    (let* ((window (selected-window))
           (press (list window 'tab-line '(0 . 0) 0))
           (ran 0)
           (held nil))
      (cl-letf (((symbol-function 'redisplay)
                 (lambda (&rest _) (setq held (window-parameter window 'lispm-desktop-platinum-pressed))))
                ((symbol-function 'lispm-desktop-platinum--press-target) (lambda (_) 'close)))
        ;; Let go over the button: it runs.
        (cl-letf (((symbol-function 'read-event) (lambda (&rest _) (list 'mouse-1 press))))
          (lispm-desktop-platinum-press press (lambda (_) t) (lambda () (cl-incf ran))))
        (should (= ran 1))
        (should (eq held 'close))
        (should-not (window-parameter window 'lispm-desktop-platinum-pressed))
        ;; Moved off before letting go: nothing runs.
        (cl-letf (((symbol-function 'read-event) (lambda (&rest _) (list 'mouse-1 press))))
          (lispm-desktop-platinum-press press (lambda (_) nil) (lambda () (cl-incf ran))))
        (should (= ran 1))))))

(ert-deftest lispm-desktop-test-drag-swaps-windows ()
  "A title bar dropped on another window swaps the two, columns permitting."
  (lispm-desktop-test--with-layout
   (let* ((session (lispm-desktop-test--session "lispm-desktop-test session 8"))
          (a (lispm-desktop-test--file "lispm-desktop-test-g.txt"))
          (b (lispm-desktop-test--file "lispm-desktop-test-h.txt")))
     (switch-to-buffer session)
     (switch-to-buffer a)
     (let* ((file-window (selected-window))
            (lower (split-window-below)))
       (set-window-buffer lower b)
       (cl-flet ((drag (from to)
                   (lispm-desktop-platinum-drag-window
                    (list 'drag-mouse-1 (list from 'tab-line '(0 . 0) 0)
                          (list to 'tab-line '(0 . 0) 0)))))
         ;; Two files trade places.
         (drag file-window lower)
         (should (eq (window-buffer file-window) b))
         (should (eq (window-buffer lower) a))
         ;; A file dropped on the session does not move.
         (drag lower (get-buffer-window session))
         (should (eq (window-buffer (get-buffer-window session)) session)))))))

(ert-deftest lispm-desktop-test-file-icons ()
  "Files are shown with the document of their kind."
  (should (equal (lispm-desktop-platinum-file-icon-name "a.el") "document-lisp"))
  (should (equal (lispm-desktop-platinum-file-icon-name "a.rs") "document-code"))
  (should (equal (lispm-desktop-platinum-file-icon-name "README.md") "document-markdown"))
  (should (equal (lispm-desktop-platinum-file-icon-name "notes.org") "document-org"))
  (should (equal (lispm-desktop-platinum-file-icon-name "a.png") "document-image"))
  (should (equal (lispm-desktop-platinum-file-icon-name "LICENSE") "document"))
  (dolist (name '("document-lisp" "document-code" "document-markdown" "document-org"
                  "folder-git" "lispm" "robot"))
    (should (file-exists-p (expand-file-name (format "icons/os9/%s.svg" name) lispm-desktop-directory)))))

(ert-deftest lispm-desktop-test-scroll-thumb ()
  "The thumb is as long as the share shown, and as far down as the start;
the arrows take the bar's bottom."
  (let ((arrows (1+ (* 2 lispm-desktop-platinum-scroll-arrow))))
    ;; Everything shows: no thumb.
    (should-not (lispm-desktop-platinum-scroll-thumb 400 1 1000 1 1000))
    ;; The top of a buffer ten times the window: a tenth of the track, at its top.
    (pcase-let ((`(,top . ,length) (lispm-desktop-platinum-scroll-thumb (+ 300 arrows) 1 101 1 1001)))
      (should (= top 0))
      (should (= length 30)))
    ;; The end: the thumb's bottom meets the arrows.
    (pcase-let ((`(,top . ,length) (lispm-desktop-platinum-scroll-thumb (+ 300 arrows) 901 1001 1 1001)))
      (should (= (+ top length) 300)))
    ;; A short thumb is never shorter than a box.
    (should (= (cdr (lispm-desktop-platinum-scroll-thumb 400 1 2 1 100000)) 16))))

(ert-deftest lispm-desktop-test-scroll-bar-draws ()
  "Scroll bars draw active, inactive, and with nothing to scroll."
  (let ((palette (lispm-desktop-platinum-derive-palette "#FFFFFF" "#000000" "#777777" "#6363CE")))
    (should (eq (dom-tag (lispm-desktop-platinum-draw-scroll-bar palette 300 '(40 . 30) t)) 'svg))
    (should (eq (dom-tag (lispm-desktop-platinum-draw-scroll-bar palette 300 nil t)) 'svg))
    (should (eq (dom-tag (lispm-desktop-platinum-draw-scroll-bar palette 300 '(40 . 30) nil)) 'svg))))

;;;; Controls

(defun lispm-desktop-test--platinum-palette ()
  "Return the palette the platinum theme gives: Mac OS 9's own colours."
  (let ((custom-theme-load-path (cons (expand-file-name "themes/" lispm-desktop-directory)
                                      custom-theme-load-path)))
    (unwind-protect
        (progn
          (load-theme 'platinum t)
          (lispm-desktop-platinum--merge-palette
           (lispm-desktop-platinum-derive-palette "#FFFFFF" "#000000" "#777777" "#6363CE")
           lispm-desktop-platinum-colors))
      (disable-theme 'platinum))))

(defun lispm-desktop-test--dracula-palette ()
  "Return the palette derived from Dracula's colours."
  (lispm-desktop-platinum-derive-palette "#282a36" "#f8f8f2" "#6272a4" "#bd93f9"))

(defun lispm-desktop-test--pixel (svg x y)
  "Return the colour SVG paints the pixel at X, Y with, upcased, or nil.
Only rectangles count, the last painted over the pixel winning, as the
drawing functions draw their pixels as rectangles."
  (let ((color nil))
    (dolist (node (dom-children svg))
      (when (and (consp node) (eq (dom-tag node) 'rect))
        (let ((x0 (dom-attr node 'x)) (y0 (dom-attr node 'y))
              (fill (dom-attr node 'fill)))
          (when (and (numberp x0) (stringp fill) (not (equal fill "none"))
                     (<= x0 (+ x 0.5) (+ x0 (dom-attr node 'width)))
                     (<= y0 (+ y 0.5) (+ y0 (dom-attr node 'height))))
            (setq color (upcase fill))))))
    color))

(defun lispm-desktop-test--size (svg)
  "Return the width and height of SVG as a cons."
  (cons (dom-attr svg 'width) (dom-attr svg 'height)))

(ert-deftest lispm-desktop-test-images-without-svg ()
  "An Emacs built without SVG gets the chrome's images all the same, rather
than an error: the text under them shows instead."
  (cl-letf* ((available (symbol-function 'image-type-available-p))
             ((symbol-function 'image-type-available-p)
              (lambda (type) (and (not (eq type 'svg)) (funcall available type)))))
    (let ((image (lispm-desktop-platinum--svg-image (svg-create 4 4))))
      (should (eq (car image) 'image))
      (should (eq (image-property image :type) 'svg))
      (should (string-prefix-p "<svg" (image-property image :data))))))

(ert-deftest lispm-desktop-test-grey-ramp ()
  "Platinum's greys come back as themselves; another theme's keep their order."
  (let ((platinum (lispm-desktop-test--platinum-palette))
        (dracula (lispm-desktop-test--dracula-palette)))
    (dolist (level '(#x00 #x22 #x77 #x88 #xAA #xCC #xDD #xEE #xFF))
      (should (equal (upcase (lispm-desktop-platinum-grey platinum level))
                     (format "#%02X%02X%02X" level level level))))
    (cl-flet ((lum (level) (lispm-desktop-platinum--luminance (lispm-desktop-platinum-grey dracula level))))
      (should (< (lum #x22) (lum #x77) (lum #xAA) (lum #xDD) (lum #xFF))))))

(ert-deftest lispm-desktop-test-palette-new-keys ()
  "Every theme gets the ramp of the accent, the outline and the Control
Strip's greys; Platinum names Mac OS 9's."
  (let ((dracula (lispm-desktop-test--dracula-palette))
        (platinum (lispm-desktop-test--platinum-palette)))
    (dolist (key '(:outline :highlight :highlight-dark :highlight-text :thumb :thumb-light
                   :thumb-dark :focus :alert :alert-dark :strip :strip-light :strip-shade :strip-dark))
      (should (stringp (plist-get dracula key)))
      (should (stringp (plist-get platinum key))))
    (should (equal (plist-get platinum :highlight) "#333399"))
    (should (equal (plist-get platinum :thumb) "#9999FF"))
    (should (equal (plist-get platinum :focus) "#6666CC"))
    (should (equal (plist-get platinum :strip) "#C0C0C0"))
    ;; Text on a highlighted Dracula menu stays legible.
    (should (equal (plist-get dracula :highlight-text) "#f8f8f2"))))

(ert-deftest lispm-desktop-test-push-button ()
  "A push button is drawn as Mac OS 9's: twenty pixels tall, its corners
rounded, two lit lines and two shaded ones, darkened when pressed and
flat when disabled."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (font '(:family "Geneva" :size 12 :weight "normal" :synthetic nil))
         (normal (lispm-desktop-platinum-draw-push-button palette "Cancel" 60 :font font))
         (pressed (lispm-desktop-platinum-draw-push-button palette "Cancel" 60 :state 'pressed :font font))
         (disabled (lispm-desktop-platinum-draw-push-button palette "Eject" 80 :state 'disabled :font font)))
    (should (equal (lispm-desktop-test--size normal) '(60 . 20)))
    (cl-flet ((at (svg x y) (lispm-desktop-test--pixel svg x y)))
      ;; The corners are cut, the outline steps in.
      (should-not (at normal 0 0))
      (should (equal (at normal 2 0) "#222222"))
      (should (equal (at normal 3 0) "#000000"))
      (should (equal (at normal 1 1) "#000000"))
      (should (equal (at normal 0 10) "#000000"))
      ;; Lit, face, shaded.
      (should (equal (at normal 10 1) "#DDDDDD"))
      (should (equal (at normal 10 2) "#FFFFFF"))
      (should (equal (at normal 2 10) "#FFFFFF"))
      (should (equal (at normal 30 10) "#DDDDDD"))
      (should (equal (at normal 57 10) "#AAAAAA"))
      (should (equal (at normal 58 10) "#777777"))
      (should (equal (at normal 10 17) "#AAAAAA"))
      (should (equal (at normal 10 18) "#777777"))
      ;; Pressed: dark, lit from below.
      (should (equal (at pressed 10 1) "#444444"))
      (should (equal (at pressed 10 2) "#555555"))
      (should (equal (at pressed 30 10) "#666666"))
      (should (equal (at pressed 58 10) "#888888"))
      ;; Disabled: a grey outline round a flat face.
      (should (equal (at disabled 0 10) "#888888"))
      (should (equal (at disabled 2 10) "#DDDDDD"))
      (should (equal (at disabled 78 10) "#DDDDDD")))
    ;; Labels: black, white when pressed, dimmed when disabled.
    (should (equal (dom-attr (car (dom-by-tag normal 'text)) 'fill) "#000000"))
    (should (equal (upcase (dom-attr (car (dom-by-tag pressed 'text)) 'fill)) "#FFFFFF"))
    (should (equal (upcase (dom-attr (car (dom-by-tag disabled 'text)) 'fill)) "#777777"))))

(ert-deftest lispm-desktop-test-push-button-rings ()
  "The default button is ringed three pixels out, 26 tall in all; the
focus ring stands two pixels further, in the focus colour; a padded
button keeps its size whichever rings it wears."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (font '(:family "Geneva" :size 12 :weight "normal" :synthetic nil))
         (default (lispm-desktop-platinum-draw-push-button palette "Save" 60 :default t :font font))
         (plain (lispm-desktop-platinum-draw-push-button palette "Save" 60 :pad 5 :font font))
         (both (lispm-desktop-platinum-draw-push-button palette "Save" 60 :default t :focused t
                                                        :pad 5 :font font)))
    (should (equal (lispm-desktop-test--size default) '(66 . 26)))
    (should (equal (lispm-desktop-test--size plain) '(70 . 30)))
    (should (equal (lispm-desktop-test--size both) '(70 . 30)))
    (cl-flet ((at (x y) (lispm-desktop-test--pixel default x y)))
      ;; Outline, then lit and shaded bands, then the button's own outline.
      (should (equal (at 0 13) "#000000"))
      (should (equal (at 1 13) "#DDDDDD"))
      (should (equal (at 2 13) "#AAAAAA"))
      (should (equal (at 3 13) "#000000"))
      (should (equal (at 63 13) "#AAAAAA"))
      (should (equal (at 64 13) "#777777"))
      (should (equal (at 65 13) "#000000")))
    (should-not (string-search "#6666CC" (format "%S" plain)))
    (should (string-search "#6666CC" (format "%S" both)))))

(ert-deftest lispm-desktop-test-bevel-button ()
  "A bevel button is square, lit along its top and left, darker pressed."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (up (lispm-desktop-platinum-draw-bevel-button palette 48 48))
         (down (lispm-desktop-platinum-draw-bevel-button palette 48 48 :state 'pressed)))
    (cl-flet ((at (svg x y) (lispm-desktop-test--pixel svg x y)))
      (should (equal (at up 0 0) "#666666"))
      (should (equal (at up 2 10) "#FFFFFF"))
      (should (equal (at up 20 20) "#CCCCCC"))
      (should (equal (at up 45 20) "#999999"))
      (should (equal (at up 47 20) "#333333"))
      (should (equal (at down 0 10) "#111111"))
      (should (equal (at down 20 20) "#888888"))
      (should (equal (at down 46 20) "#BBBBBB")))))

(ert-deftest lispm-desktop-test-tabs ()
  "Tabs stand on the pane's edge: the one in front opens into the pane in
its colour, those behind are darker with the pane's outline under them."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (font '(:family "Geneva" :size 12 :weight "normal" :synthetic nil))
         (front (lispm-desktop-platinum-draw-tab palette "Desktop" 70 :front t))
         (back (lispm-desktop-platinum-draw-tab palette "Fonts" 50))
         (edge (lispm-desktop-platinum-draw-tab-edge palette 8)))
    (should (equal (lispm-desktop-test--size front) '(70 . 24)))
    (should (equal (lispm-desktop-test--size edge) '(8 . 24)))
    (cl-flet ((at (svg x y) (lispm-desktop-test--pixel svg x y)))
      ;; Rounded top corners, slanted sides.
      (should (equal (at back 0 0) "#DDDDDD"))
      (should (equal (at back 11 0) "#000000"))
      (should (equal (at back 0 20) "#222222"))
      (should (equal (at back 6 4) "#000000"))
      ;; Faces, and the pane's outline under the tab behind only.
      (should (equal (at back 25 10) "#CCCCCC"))
      (should (equal (at front 35 10) "#EEEEEE"))
      (should (equal (at back 25 21) "#000000"))
      (should (equal (at front 35 21) "#EEEEEE"))
      (should (equal (at edge 4 21) "#000000"))
      (should (equal (at edge 4 23) "#FFFFFF")))
    ;; Every label is black and none bold.
    (should (equal (dom-attr (car (dom-by-tag front 'text)) 'fill) "#000000"))
    (should (equal (dom-attr (car (dom-by-tag back 'text)) 'fill) "#000000"))))

(ert-deftest lispm-desktop-test-disclosure-triangle ()
  "The disclosure triangle is outlined, in the variation colour, shadowed."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (right (lispm-desktop-platinum-draw-triangle palette nil))
         (down (lispm-desktop-platinum-draw-triangle palette t)))
    (cl-flet ((at (svg x y) (lispm-desktop-test--pixel svg x y)))
      (should (equal (at right 3 0) "#000000"))
      (should (equal (at right 4 3) "#CCCCFF"))
      (should (equal (at right 5 4) "#9999FF"))
      (should (equal (at right 7 5) "#6666CC"))
      (should (equal (at right 9 6) "#BBBBBB"))
      ;; Pointing down it is the same triangle turned over its diagonal.
      (should (equal (at down 3 4) "#CCCCFF"))
      (should (equal (at down 0 3) "#000000")))))

(ert-deftest lispm-desktop-test-small-controls ()
  "Radio buttons, check boxes and placards draw at Mac OS 9's sizes."
  (let ((palette (lispm-desktop-test--dracula-palette)))
    (should (equal (lispm-desktop-test--size (lispm-desktop-platinum-draw-radio palette t)) '(12 . 12)))
    (should (string-search "url(#sphere)" (format "%S" (lispm-desktop-platinum-draw-radio palette nil))))
    (should (equal (lispm-desktop-test--size (lispm-desktop-platinum-draw-check-box palette t)) '(14 . 14)))
    (should (dom-by-tag (lispm-desktop-platinum-draw-check-box palette t) 'polyline))
    (should-not (dom-by-tag (lispm-desktop-platinum-draw-check-box palette nil) 'polyline))
    (let ((placard (lispm-desktop-platinum-draw-placard palette 100 20 :label "9 items"
                                                        :font '(:family "Geneva" :size 10
                                                                :weight "normal" :synthetic nil))))
      (should (equal (lispm-desktop-test--size placard) '(100 . 20)))
      (should (equal (lispm-desktop-test--pixel placard 0 0)
                     (upcase (plist-get palette :outline)))))))

(ert-deftest lispm-desktop-test-window-frame ()
  "An active window's frame is outlined, lit from the top left, lined round
its content, and casts a shadow; beside the title bar it takes the bar's
lines; an inactive one lies flat in a dark grey outline."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (active (lispm-desktop-platinum-draw-window-frame palette 101 81 :active t))
         (inactive (lispm-desktop-platinum-draw-window-frame palette 101 81)))
    (should (equal (lispm-desktop-test--size active) '(101 . 81)))
    (cl-flet ((at (svg x y) (lispm-desktop-test--pixel svg x y)))
      ;; Left and right of the content: outline, band, the content's line.
      (should (equal (mapcar (lambda (x) (at active x 40)) '(0 1 2 3 4 5))
                     '("#000000" "#FFFFFF" "#CCCCCC" "#CCCCCC" "#999999" "#000000")))
      (should (equal (mapcar (lambda (x) (at active x 40)) '(94 95 96 97 98 99 100))
                     '("#000000" "#FFFFFF" "#CCCCCC" "#CCCCCC" "#999999" "#000000" "#000000")))
      ;; The bottom, and the shadow under it.
      (should (equal (mapcar (lambda (y) (at active 50 y)) '(74 75 76 77 78 79 80))
                     '("#000000" "#FFFFFF" "#CCCCCC" "#CCCCCC" "#999999" "#000000" "#000000")))
      ;; Beside the title bar: its lit top, its shade and its black line.
      (should (equal (at active 3 1) "#FFFFFF"))
      (should (equal (at active 3 10) "#CCCCCC"))
      (should (equal (at active 3 20) "#999999"))
      (should (equal (at active 3 21) "#000000"))
      (should (equal (at inactive 0 40) "#555555"))
      (should (equal (at inactive 2 40) "#DDDDDD")))))

(defmacro lispm-desktop-test--with-frames (frames &rest body)
  "Run BODY among fake frames: FRAMES evaluates to a list of
\(NAME . PARAMETERS), symbols standing for frames, whose parameters the frame primitives read
and set; `parent-frame', `position', `size' and `visibility' stand for a
frame's parent, place, size and visibility.  In BODY, `moves' holds the
moves made, (FRAME X Y), the latest first, and `raised' the frames
raised, the latest first."
  (declare (indent 1))
  `(let ((parameters (mapcar (lambda (frame) (cons (car frame) (copy-tree (cdr frame)))) ,frames))
         (moves nil)
         (raised nil))
     (cl-flet ((frame-get (frame name) (alist-get name (alist-get frame parameters)))
               (frame-put (frame name value) (setf (alist-get name (alist-get frame parameters)) value)))
       (cl-letf (((symbol-function 'frame-live-p) (lambda (frame) (and (assq frame parameters) t)))
                 ((symbol-function 'frame-parameter) (lambda (frame name) (frame-get frame name)))
                 ((symbol-function 'set-frame-parameter) (lambda (frame name value) (frame-put frame name value)))
                 ((symbol-function 'modify-frame-parameters)
                  (lambda (frame alist) (pcase-dolist (`(,name . ,value) alist) (frame-put frame name value))))
                 ((symbol-function 'frame-parent) (lambda (&optional frame) (frame-get frame 'parent-frame)))
                 ((symbol-function 'frame-position) (lambda (&optional frame) (frame-get frame 'position)))
                 ((symbol-function 'set-frame-position)
                  (lambda (frame x y) (push (list frame x y) moves) (frame-put frame 'position (cons x y))))
                 ((symbol-function 'frame-visible-p) (lambda (frame) (frame-get frame 'visibility)))
                 ((symbol-function 'make-frame-visible) (lambda (&optional frame) (frame-put frame 'visibility t)))
                 ((symbol-function 'make-frame-invisible)
                  (lambda (&optional frame _force) (frame-put frame 'visibility nil)))
                 ((symbol-function 'raise-frame) (lambda (&optional frame) (push frame raised)))
                 ((symbol-function 'frame-pixel-width) (lambda (&optional frame) (car (frame-get frame 'size))))
                 ((symbol-function 'frame-pixel-height) (lambda (&optional frame) (cdr (frame-get frame 'size))))
                 ((symbol-function 'frame-text-width) (lambda (&optional frame) (car (frame-get frame 'size))))
                 ((symbol-function 'frame-text-height) (lambda (&optional frame) (cdr (frame-get frame 'size))))
                 ((symbol-function 'frame-edges)
                  (lambda (&optional frame _type)
                    (pcase-let ((`(,x . ,y) (frame-get frame 'position))
                                (`(,w . ,h) (frame-get frame 'size)))
                      (list x y (+ x w) (+ y h)))))
                 ((symbol-function 'frame-list) (lambda () (mapcar #'car parameters)))
                 ((symbol-function 'frame-list-z-order)
                  (lambda (&optional _) (seq-remove (lambda (frame) (frame-get frame 'parent-frame))
                                                    (mapcar #'car parameters))))
                 ((symbol-function 'lispm-desktop-platinum--frameable-p) #'ignore)
                 ((symbol-function 'lispm-desktop-platinum-frame-update) #'ignore))
         ,@body))))

(ert-deftest lispm-desktop-test-window-draws-its-own-frame ()
  "A floating window is framed unless it draws its own frame, as an
application's window may, Stickies' notes an edge of their colour."
  (let ((lispm-desktop-platinum-window-frames t)
        (lispm-desktop-platinum-current-palette (lispm-desktop-test--platinum-palette))
        (parameters '((window (lispm-desktop-window . t))
                      (note (lispm-desktop-window . t) (lispm-desktop-platinum-own-frame . t))
                      (screen))))
    (cl-letf (((symbol-function 'frame-live-p) #'always)
              ((symbol-function 'display-graphic-p) #'always)
              ((symbol-function 'frame-parameter)
               (lambda (frame parameter) (alist-get parameter (alist-get frame parameters)))))
      (should (lispm-desktop-platinum--frameable-p 'window))
      (should-not (lispm-desktop-platinum--frameable-p 'note))
      (should-not (lispm-desktop-platinum--frameable-p 'screen)))))

(defconst lispm-desktop-test--window-frames
  '((screen (position 0 . 0) (size 1200 . 800) (visibility . t))
    (outer (lispm-desktop-platinum-frames . window) (parent-frame . screen)
           (position 94 . 49) (size 413 . 308) (visibility . t))
    (window (lispm-desktop-window . t) (parent-frame . outer) (lispm-desktop-platinum-frame . outer)
            (position 6 . 1) (size 400 . 300) (visibility . t)))
  "A floating window in its frame over the screen, for
`lispm-desktop-test--with-frames'.")

(ert-deftest lispm-desktop-test-window-in-its-frame ()
  "A framed window stands in its frame, its parent: moved, raised, shown or
hidden, the frame is, the window going with it, as Mac OS 9 kept a
window's structure and content as one."
  (lispm-desktop-test--with-frames lispm-desktop-test--window-frames
      (should (eq (lispm-desktop-platinum-window-outer 'window) 'outer))
      (should (eq (lispm-desktop-platinum-window-outer 'screen) 'screen))
      (should (eq (lispm-desktop-platinum-window-frame-p 'outer) 'window))
      (should (lispm-desktop-platinum-chrome-frame-p 'outer))
      (should-not (lispm-desktop-apps-screen-p 'outer))
      (should (eq (lispm-desktop-platinum-window-parent 'window) 'screen))
      (should (eq (lispm-desktop-apps-window-screen 'window) 'screen))
      (should (equal (lispm-desktop-platinum-window-position 'window) '(100 . 50)))
      (lispm-desktop-platinum-move-window 'window 300 200)
      (should (equal moves '((outer 294 199))))
      (should (equal (frame-position 'window) '(6 . 1)))
      (should (equal (lispm-desktop-platinum-window-position 'window) '(300 . 200)))
      (should (equal (lispm-desktop-apps--geometry 'window) '(300 200 400 300)))
      (lispm-desktop-platinum-raise-window 'window)
      (should (equal raised '(window outer)))
      (lispm-desktop-platinum-hide-window 'window)
      (should-not (frame-visible-p 'outer))
      (should-not (frame-visible-p 'window))
      (lispm-desktop-platinum-show-window 'window)
      (should (frame-visible-p 'outer))
      (should (frame-visible-p 'window))))

(ert-deftest lispm-desktop-test-desktop-edges ()
  "The desktop lies under the menu bar, across the screen, down to the echo
area."
  (pcase-let ((`(,left ,top ,right ,bottom) (lispm-desktop-apps-desktop-edges (selected-frame)))
              (`(,_ ,echo-top ,_ ,_) (window-pixel-edges (minibuffer-window))))
    (should (= left 0))
    (should (= top (nth 1 (window-pixel-edges (frame-root-window)))))
    (should (= right (frame-native-width)))
    (should (= bottom echo-top))))

(defmacro lispm-desktop-test--with-desktop (frames &rest body)
  "Run BODY among the fake FRAMES, as `lispm-desktop-test--with-frames' does,
over a desktop from 0, 25 to 1200, 780: a menu bar 25 pixels high above
it, an echo area 20 pixels high below.  Frames take the sizes given them."
  (declare (indent 1))
  `(lispm-desktop-test--with-frames ,frames
       (cl-letf (((symbol-function 'lispm-desktop-apps-desktop-edges) (lambda (_) '(0 25 1200 780)))
                 ((symbol-function 'set-frame-size)
                  (lambda (frame width height &optional _)
                    (set-frame-parameter frame 'size (cons width height)))))
         ,@body)))

(ert-deftest lispm-desktop-test-window-kept-on-desktop-in-its-frame ()
  "A floating window is kept whole on the desktop with the frame drawn round
it, its outline, band and shadow: grown past the desktop, it shrinks
until its frame fills the desktop, under the menu bar and above the echo
area; let past an edge, it comes back until its frame is in.  A window
not framed keeps to its own edges, and one out on the computer's screen
is kept on that screen instead."
  (lispm-desktop-test--with-desktop lispm-desktop-test--window-frames
      (cl-flet ((outer-edges ()
                  ;; The frame is as large as its window and the margins.
                  (pcase-let ((`(,x . ,y) (frame-position 'outer))
                              (`(,width . ,height) (frame-parameter 'window 'size)))
                    (list x y (+ x width 13) (+ y height 8)))))
        (should (equal (lispm-desktop-platinum-window-margins 'window) '(6 1 7 7)))
        ;; Grown by its grow box past the desktop's bottom right.
        (lispm-desktop-apps--put 'window '(100 50 1300 900))
        (should (equal (frame-parameter 'window 'size) '(1187 . 747)))
        (should (equal (outer-edges) '(0 25 1200 780)))
        (should (equal (lispm-desktop-platinum-window-position 'window) '(6 . 26)))
        ;; Moved past the top left, and past the bottom right.
        (lispm-desktop-apps--put 'window '(-50 0 400 300))
        (should (equal (frame-parameter 'window 'size) '(400 . 300)))
        (should (equal (outer-edges) '(0 25 413 333)))
        (lispm-desktop-apps--put 'window '(1000 700 400 300))
        (should (equal (outer-edges) '(787 472 1200 780)))
        ;; Where it is kept, it stays.
        (setq moves nil)
        (lispm-desktop-apps--put 'window '(793 473 400 300))
        (should-not moves)))
  (lispm-desktop-test--with-desktop '((screen (position 0 . 0) (size 1200 . 800) (visibility . t))
                                      (window (lispm-desktop-window . t) (parent-frame . screen)
                                              (position 100 . 50) (size 400 . 300) (visibility . t)))
      (should (equal (lispm-desktop-platinum-window-margins 'window) '(0 0 0 0)))
      (lispm-desktop-apps--put 'window '(100 50 1300 900))
      (should (equal (frame-parameter 'window 'size) '(1200 . 755)))
      (should (equal (frame-position 'window) '(0 . 25))))
  (lispm-desktop-test--with-desktop lispm-desktop-test--window-frames
      (let ((kept nil))
        (cl-letf (((symbol-function 'lispm-desktop-apps--keep-on-screen)
                   (lambda (frame) (push frame kept))))
          (modify-frame-parameters 'outer '((parent-frame) (lispm-desktop-out . screen)))
          (set-frame-parameter 'window 'lispm-desktop-out 'screen)
          (lispm-desktop-apps--put 'window '(-50 0 1300 900))
          (lispm-desktop-apps-keep-on-desktop 'window)
          (should (equal kept '(window)))
          (should-not moves)
          (should (equal (frame-parameter 'window 'size) '(400 . 300)))))))

(defmacro lispm-desktop-test--with-edges (&rest body)
  "Run BODY with the floating window of `lispm-desktop-test--window-frames'
in its frame, the frame last given its size, 413 by 308 pixels.  In
BODY, `sizes' holds the sizes given the window, the latest first."
  (declare (indent 0))
  `(lispm-desktop-test--with-frames lispm-desktop-test--window-frames
       (let ((sizes nil))
         (set-frame-parameter 'outer 'lispm-desktop-platinum-size '(413 . 308))
         (set-frame-parameter 'outer 'lispm-desktop-platinum-sizes '((413 . 308)))
         (cl-letf (((symbol-function 'set-frame-size)
                    (lambda (frame width height &optional _)
                      (push (list frame width height) sizes)
                      (frame-put frame 'size (cons width height)))))
           ,@body))))

(ert-deftest lispm-desktop-test-window-frame-follows-edges ()
  "On macOS the window system resizes a window or its frame dragged by an
edge, behind Emacs's back.  Dragged by its left or top edge, the window
moves in its frame, which follows it there; dragged by its edge, the
frame changes alone, and the window takes the size it leaves it and
stands in it again."
  (lispm-desktop-test--with-edges
    (should-not (lispm-desktop-platinum--frame-follow 'window 'outer))
    (should-not moves)
    ;; The window's top left corner, dragged in by 40 and 30 pixels.
    (frame-put 'window 'position '(46 . 31))
    (frame-put 'window 'size '(360 . 270))
    (should (lispm-desktop-platinum--frame-follow 'window 'outer))
    (should (equal (frame-position 'outer) '(134 . 79)))
    (should (equal (frame-position 'window) '(6 . 1)))
    (should-not sizes))
  (lispm-desktop-test--with-edges
    ;; The frame's top left corner, dragged out by 60 and 40 pixels.
    (frame-put 'outer 'position '(34 . 9))
    (frame-put 'outer 'size '(473 . 348))
    (should (lispm-desktop-platinum--frame-follow 'window 'outer))
    (should (equal sizes '((window 460 340))))
    (should (equal (frame-position 'outer) '(34 . 9)))
    ;; Emacs takes the window to stand in it still: it is moved anew.
    (should (equal moves '((window 6 1) (window 7 1)))))
  (lispm-desktop-test--with-edges
    ;; Dragged in past the least a window grows to.
    (frame-put 'outer 'size '(113 . 58))
    (should (lispm-desktop-platinum--frame-follow 'window 'outer))
    (should (equal sizes (list (list 'window (car lispm-desktop-platinum-window-min-size)
                                     (cdr lispm-desktop-platinum-window-min-size))))))
  (lispm-desktop-test--with-edges
    ;; Rolled up, the window takes the frame's width only.
    (set-frame-parameter 'window 'lispm-desktop-platinum-rolled-up '(buffer . 300))
    (frame-put 'outer 'size '(463 . 348))
    (should (lispm-desktop-platinum--frame-follow 'window 'outer))
    (should (equal sizes '((window 450 300))))))

(ert-deftest lispm-desktop-test-window-frame-keeps-its-own-sizes ()
  "The frame's size changing is no drag of its edge when it is a size it
was given lately, which the window system tells of late, when the
window's own size changed, or when the frame's font did, as with a theme:
the frame is then only given its size again."
  (lispm-desktop-test--with-edges
    (set-frame-parameter 'outer 'lispm-desktop-platinum-sizes '((413 . 308) (473 . 348)))
    (frame-put 'outer 'size '(473 . 348))
    (should-not (lispm-desktop-platinum--frame-follow 'window 'outer))
    (should-not sizes))
  (lispm-desktop-test--with-edges
    (frame-put 'outer 'size '(473 . 348))
    (frame-put 'window 'size '(420 . 310))
    (should-not (lispm-desktop-platinum--frame-follow 'window 'outer))
    (should-not sizes))
  (lispm-desktop-test--with-edges
    (set-frame-parameter 'outer 'lispm-desktop-platinum-font "Monaco 12")
    (set-frame-parameter 'outer 'font "Monaco 16")
    (frame-put 'outer 'size '(530 . 400))
    (should-not (lispm-desktop-platinum--frame-follow 'window 'outer))
    (should-not sizes)
    (should-not moves))
  (lispm-desktop-test--with-edges
    ;; Not yet standing in it, as while it is being framed.
    (frame-put 'window 'parent-frame 'screen)
    (frame-put 'window 'position '(100 . 50))
    (should-not (lispm-desktop-platinum--frame-follow 'window 'outer))
    (should-not moves)))

(ert-deftest lispm-desktop-test-outline-geometry ()
  "Moving, a window's outline is a band two pixels thick round its outer
rectangle; growing, a line a pixel thick round it and, within, the
title bar's line, the content's lines and the grow box, where Mac OS 9's
GrowWindow drew them."
  (should (equal (lispm-desktop-platinum-outline-lines 'move 10 20 300 200)
                 '((10 20 300 2 nil) (10 218 300 2 nil) (10 22 196 2 t) (308 22 196 2 t))))
  (let ((lines (lispm-desktop-platinum-outline-lines 'grow 10 20 300 200 21)))
    (should (= (length lines) 10))
    (dolist (line '((10 20 300 1 nil) (10 219 300 1 nil) (10 21 198 1 t) (309 21 198 1 t)
                    ;; Under the title bar, from the content's left line to
                    ;; its right one, and round the content.
                    (15 41 289 1 nil) (15 42 171 1 t) (303 42 171 1 t) (15 213 289 1 nil)
                    ;; The grow box in the content's bottom right corner.
                    (288 198 15 1 nil) (288 199 14 1 t)))
      (should (member line lines)))
    (dolist (line lines)
      (pcase-let ((`(,x ,y ,length ,thickness ,vertical) line))
        (should (<= 10 x (+ x (if vertical thickness length)) 310))
        (should (<= 20 y (+ y (if vertical length thickness)) 220)))))
  (should (= 4 (length (lispm-desktop-platinum-outline-lines 'grow 10 20 300 200)))))

(ert-deftest lispm-desktop-test-outline-dots ()
  "The outline's dots are the 50% grey pattern lined up with the screen's
pixels: a checkerboard whose phase follows where the line is."
  (should (equal (lispm-desktop-platinum-dots 4 2 0)
                 (vector (bool-vector t nil t nil) (bool-vector nil t nil t))))
  (should (equal (lispm-desktop-platinum-dots 3 1 1) (vector (bool-vector nil t nil)))))

(ert-deftest lispm-desktop-test-outline-dots-image ()
  "The outline's dots are an XBM image of the bits, as large as they are."
  ;; CI's Emacs builds have no window system, and so no XBM images.
  (skip-unless (image-type-available-p 'xbm))
  ;; Bits are sized by `:data-width' and `:data-height': `:width' and
  ;; `:height' would scale the image, and make the spec invalid.
  (let ((image (lispm-desktop-platinum--dots-image 6 2 1)))
    (should (eq (plist-get (cdr image) :type) 'xbm))
    (should (equal (list (plist-get (cdr image) :data-width) (plist-get (cdr image) :data-height)) '(6 2)))
    (should-not (plist-member (cdr image) :width))))

(ert-deftest lispm-desktop-test-outline-keeps-thin ()
  "A frame kept to draw an outline's lines gets its font a pixel tall
back once a theme, or a change to the default face, gave it the default
font again, and is sized again in the desktop's colour; while it has
that font, it is left as it is."
  (lispm-desktop-test--with-frames '((line (font . "Monaco-1")
                                           (lispm-desktop-platinum-thin-font . "Monaco-1")
                                           (lispm-desktop-platinum-line . (1 14 0 nil 3))))
    (let ((sizes nil))
      (cl-letf (((symbol-function 'set-frame-font)
                 (lambda (font _keep frames)
                   (push (font-get font :size) sizes)
                   (dolist (frame frames)
                     (frame-put frame 'font (format "Monaco-%d" (font-get font :size))))))
                ((symbol-function 'lispm-desktop-platinum-desktop-color) (lambda () "#6666CC")))
        (lispm-desktop-platinum--keep-thin 'line)
        (should-not sizes)
        (should (frame-get 'line 'lispm-desktop-platinum-line))
        ;; A theme enabled: the default face's font, a line 16 pixels
        ;; tall, and the theme's background.
        (frame-put 'line 'font "Monaco-12")
        (frame-put 'line 'background-color "#FFFFFF")
        (lispm-desktop-platinum--keep-thin 'line)
        (should (equal sizes '(1)))
        (should (equal (frame-get 'line 'font) "Monaco-1"))
        (should (equal (frame-get 'line 'background-color) "#6666CC"))
        (should-not (frame-get 'line 'lispm-desktop-platinum-line))
        ;; A font that cannot be a pixel tall is not set again and again.
        (cl-letf (((symbol-function 'set-frame-font)
                   (lambda (_font _keep _frames) (push 'failed sizes))))
          (frame-put 'line 'font "Bitmap-13")
          (lispm-desktop-platinum--keep-thin 'line)
          (lispm-desktop-platinum--keep-thin 'line)
          (should (equal sizes '(failed 1))))))))

(ert-deftest lispm-desktop-test-drag-keeps-in-front ()
  "What a drag draws comes back over whatever came in front of it, and is
left where it is while it is in front."
  (let ((order '(window outer line-a line-b))
        (raised nil))
    (cl-letf (((symbol-function 'frame-list-z-order) (lambda (&optional _) order))
              ((symbol-function 'raise-frame) (lambda (frame) (push frame raised))))
      (lispm-desktop-platinum-keep-in-front '(line-a line-b) 'screen)
      (should (equal raised '(line-b line-a)))
      (setq raised nil order '(line-b window outer line-a))
      (lispm-desktop-platinum-keep-in-front '(line-a line-b) 'screen)
      (should-not raised)
      ;; On the computer's screen, the outline floats above by itself.
      (setq order '(window line-a))
      (lispm-desktop-platinum-keep-in-front '(line-a) nil)
      (should-not raised))))

(defmacro lispm-desktop-test--with-outline (script &rest body)
  "Run BODY with the framed window of `lispm-desktop-test--window-frames'
and a mouse following SCRIPT, a list of ((X . Y) . EVENT): each read
puts the mouse at X, Y on the computer's screen and gives EVENT, nil for
a read that waited in vain.  In BODY, `ran' counts the runs of
`lispm-desktop-platinum-frame-moved-functions', `outlines' holds the
outlines drawn, the latest first, and `sizes' the sizes given the window."
  (declare (indent 1))
  `(lispm-desktop-test--with-frames lispm-desktop-test--window-frames
       (let* ((script ,script)
              (pointer (car (car script)))
              (ran 0)
              (outlines nil)
              (sizes nil)
              (unread-command-events nil)
              (lispm-desktop-platinum-frame-moved-functions (list (lambda (_) (setq ran (1+ ran))))))
         (cl-letf (((symbol-function 'mouse-absolute-pixel-position) (lambda () pointer))
                   ((symbol-function 'read-event)
                    (lambda (&rest _)
                      (pcase-let ((`(,place . ,event) (pop script)))
                        (setq pointer place)
                        event)))
                   ((symbol-function 'lispm-desktop-platinum-show-outline)
                    (lambda (_parent lines _origin) (push lines outlines)))
                   ((symbol-function 'lispm-desktop-platinum-hide-outline) #'ignore)
                   ((symbol-function 'lispm-desktop-platinum-origin) (lambda (_) '(0 . 0)))
                   ((symbol-function 'select-frame-set-input-focus) #'ignore)
                   ((symbol-function 'set-frame-size)
                    (lambda (frame width height &optional _) (push (list frame width height) sizes))))
           ,@body))))

(ert-deftest lispm-desktop-test-outline-moves-the-window-once ()
  "Dragged by its outline, a window stays where it is while the outline
follows the mouse, and moves once the button rises, by as far as the
mouse went; the moved functions run once.  Escape or C-g leaves it where
it was; a press let go where it was is handed on as a click."
  (lispm-desktop-test--with-outline (list (cons '(500 . 300) nil) (cons '(510 . 305) nil)
                                          (cons '(530 . 320) '(mouse-movement (nil)))
                                          (cons '(560 . 340) nil)
                                          (cons '(560 . 340) '(drag-mouse-1 (nil) (nil))))
    (should (lispm-desktop-platinum-drag-outline 'window 'move))
    (should (equal moves '((outer 154 89))))
    (should (= ran 1))
    (should (= (length outlines) 3))
    (should (equal (car (car outlines)) '(154 89 413 2 nil)))
    (should (equal (car (car (last outlines))) '(104 54 413 2 nil))))
  (dolist (stop '(27 escape ?\C-g))
    (lispm-desktop-test--with-outline (list (cons '(500 . 300) nil) (cons '(580 . 360) nil)
                                            (cons '(580 . 360) stop))
      (should-not (lispm-desktop-platinum-drag-outline 'window 'move))
      (should outlines)
      (should-not moves)
      (should (= ran 0))))
  (lispm-desktop-test--with-outline (list (cons '(500 . 300) nil) (cons '(502 . 301) nil)
                                          (cons '(502 . 301) '(mouse-1 (nil))))
    (should-not (lispm-desktop-platinum-drag-outline 'window 'move))
    (should-not moves)
    (should (= ran 0))
    (should (equal unread-command-events '((mouse-1 (nil)))))))

(ert-deftest lispm-desktop-test-outline-grows-the-window-once ()
  "Grown by its outline, a window is given its new size once, when the
button rises, and grows no smaller than its least."
  (lispm-desktop-test--with-outline (list (cons '(500 . 300) nil) (cons '(520 . 310) nil)
                                          (cons '(540 . 330) '(drag-mouse-1 (nil) (nil))))
    (should (lispm-desktop-platinum-drag-outline 'window 'grow))
    (should (equal sizes '((window 440 330))))
    (should-not moves)
    (should (= ran 1))
    (should (= (length (car outlines)) 10))
    (should (member '(94 49 453 1 nil) (car outlines))))
  (lispm-desktop-test--with-outline (list (cons '(500 . 300) nil)
                                          (cons '(0 . 0) '(drag-mouse-1 (nil) (nil))))
    (lispm-desktop-platinum-drag-outline 'window 'grow)
    (should (equal sizes (list (list 'window (car lispm-desktop-platinum-window-min-size)
                                     (cdr lispm-desktop-platinum-window-min-size)))))))

(ert-deftest lispm-desktop-test-scroll-bar-parts ()
  "A scroll bar is outlined down both sides, its track recessed, its thumb
in the variation colour with four ridges, its arrows together at the
bottom; with nothing to scroll it lies flat."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (bar (lispm-desktop-platinum-draw-scroll-bar palette 200 '(40 . 60) t))
         (flat (lispm-desktop-platinum-draw-scroll-bar palette 200 nil t))
         (track (lispm-desktop-platinum--scroll-track 200)))
    (should (equal (lispm-desktop-test--size bar) '(16 . 200)))
    (cl-flet ((at (svg x y) (lispm-desktop-test--pixel svg x y)))
      (should (equal (at bar 0 10) "#000000"))
      (should (equal (at bar 15 10) "#000000"))
      (should (equal (at bar 1 10) "#777777"))
      (should (equal (at bar 2 10) "#888888"))
      (should (equal (at bar 8 10) "#AAAAAA"))
      ;; The thumb, parted from the track by lines.
      (should (equal (at bar 8 39) "#000000"))
      (should (equal (at bar 8 40) "#CCCCFF"))
      (should (equal (at bar 8 45) "#9999FF"))
      (should (equal (at bar 14 45) "#6666CC"))
      (should (equal (at bar 8 100) "#000000"))
      (should (= 4 (seq-count (lambda (y) (equal (at bar 8 y) "#333399"))
                              (number-sequence 40 99))))
      ;; The arrow boxes, lit and outlined.
      (should (equal (at bar 8 track) "#000000"))
      (should (equal (at bar 8 (1+ track)) "#FFFFFF"))
      (should (equal (at bar 3 (+ track 5)) "#DDDDDD"))
      (should (equal (at bar 14 (+ track 5)) "#BBBBBB"))
      ;; Nothing to scroll: flat, grey arrows.
      (should (equal (at flat 8 10) "#EEEEEE"))
      (should (equal (at flat 8 track) "#888888")))))

(ert-deftest lispm-desktop-test-grow-box ()
  "The grow box is sixteen pixels square, outlined at top and left, ridged."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (box (lispm-desktop-platinum-draw-grow-box palette 16)))
    (should (equal (lispm-desktop-test--size box) '(16 . 16)))
    (cl-flet ((at (x y) (lispm-desktop-test--pixel box x y)))
      (should (equal (at 0 8) "#000000"))
      (should (equal (at 8 0) "#000000"))
      (should (equal (at 1 8) "#FFFFFF"))
      (should (equal (at 3 3) "#CCCCCC"))
      (should (equal (at 10 5) "#777777")))))

(ert-deftest lispm-desktop-test-header-strip ()
  "A window's header strip, under its title bar, is lit along its top,
platinum, and shaded along its bottom over a black line."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (strip (lispm-desktop-platinum-draw-header-strip palette 200 "3 items")))
    (should (equal (lispm-desktop-test--size strip) '(200 . 22)))
    (cl-flet ((at (x y) (lispm-desktop-test--pixel strip x y)))
      (should (equal (at 5 0) "#FFFFFF"))
      (should (equal (at 5 10) "#DDDDDD"))
      (should (equal (at 5 20) "#AAAAAA"))
      (should (equal (at 5 21) "#000000")))
    (should (dom-by-tag strip 'text))
    (should (eq (dom-tag (lispm-desktop-platinum-draw-header-strip
                          (lispm-desktop-test--dracula-palette) 200 "3 items"))
                'svg))))

(ert-deftest lispm-desktop-test-header-button ()
  "A header strip may open with a bevel button standing on its ground, six
pixels in; the rest of the strip keeps its label centred on the whole,
clear of the button."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (width (lispm-desktop-platinum-header-button-width "▲ home"))
         (button (lispm-desktop-platinum-draw-header-button palette "▲ home"))
         (pressed (lispm-desktop-platinum-draw-header-button palette "▲ home" 'pressed))
         (disabled (lispm-desktop-platinum-draw-header-button palette "▲" 'disabled)))
    (should (equal (lispm-desktop-test--size button) (cons width 22)))
    (cl-flet ((at (svg x y) (lispm-desktop-test--pixel svg x y)))
      (should (equal (at button 2 0) "#FFFFFF"))
      (should (equal (at button 2 10) "#DDDDDD"))
      (should (equal (at button 2 21) "#000000"))
      (should (equal (at button 6 2) "#666666"))
      (should (equal (at button 8 10) "#FFFFFF"))
      (should (equal (at button (1- width) 10) "#333333"))
      (should (equal (at button 10 18) "#333333"))
      (should (equal (at button 10 19) "#DDDDDD"))
      (should (equal (at pressed 6 10) "#111111"))
      (should (equal (at disabled 6 10) "#999999")))
    (should (dom-by-tag button 'text)))
  (let ((palette (lispm-desktop-test--platinum-palette)))
    (cl-flet ((label-x (svg) (dom-attr (car (dom-by-tag svg 'text)) 'x)))
      (should (= (label-x (lispm-desktop-platinum-draw-header-strip palette 200 "3 items" 100))
                 (- (label-x (lispm-desktop-platinum-draw-header-strip palette 300 "3 items")) 100)))
      (should (= (label-x (lispm-desktop-platinum-draw-header-strip palette 40 "3 items" 100)) 8)))))

(ert-deftest lispm-desktop-test-title-menu-from-keyboard ()
  "The title menu pops up under the mouse from a Command click, and from
the keyboard under the selected window's title."
  (let ((calls nil))
    (with-temp-buffer
      (setq-local lispm-desktop-platinum-title-menu-function
                  (lambda (_window) '((:label "Here" :action ignore))))
      (cl-letf (((symbol-function 'lispm-desktop-popup-menu)
                 (lambda (items &optional x y _align) (push (list items x y) calls))))
        (save-window-excursion
          (switch-to-buffer (current-buffer))
          (call-interactively #'lispm-desktop-platinum-title-menu)
          (lispm-desktop-platinum-title-menu (selected-window)))))
    (pcase-let ((`((,_ ,mouse-x ,mouse-y) (,items ,x ,y)) calls))
      (should (equal (plist-get (car items) :label) "Here"))
      (should (integerp x))
      (should (integerp y))
      (should-not mouse-x)
      (should-not mouse-y))))

(ert-deftest lispm-desktop-test-group-box-edge ()
  "A group box's top edge is an engraved line, grey over lit, running down
its sides; its title breaks the line on the panel's grey."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (edge (lispm-desktop-platinum-draw-group-edge palette 200))
         (titled (lispm-desktop-platinum-draw-group-edge palette 200 "Appearance")))
    (should (equal (lispm-desktop-test--size edge) '(200 . 18)))
    (cl-flet ((at (svg x y) (lispm-desktop-test--pixel svg x y)))
      (should (equal (at edge 50 9) "#888888"))
      (should (equal (at edge 50 10) "#FFFFFF"))
      (should (equal (at edge 0 14) "#888888"))
      (should (equal (at edge 1 14) "#FFFFFF"))
      (should (equal (at edge 198 14) "#888888"))
      (should (equal (at edge 199 14) "#FFFFFF"))
      (should (equal (at titled 10 9) "#EEEEEE")))
    (should (dom-by-tag titled 'text))))

(ert-deftest lispm-desktop-test-finder-path-menu ()
  "The Finder window counts its items in its header, and its title pops up
its folder's path, the folder first, each going there."
  (lispm-desktop-test--with-folder
    (unwind-protect
        (let ((root (file-name-as-directory folder)))
          (lispm-desktop-finder--go (expand-file-name "zeta" folder))
          (lispm-desktop-finder--in
            (should (eq lispm-desktop-platinum-title-menu-function #'lispm-desktop-finder--path-items))
            (should (equal header-line-format '(:eval (lispm-desktop-finder--header))))
            (let ((items (lispm-desktop-finder--path-items nil)))
              (should (equal (plist-get (car items) :label) "zeta"))
              (should (equal (plist-get (cadr items) :label) (lispm-desktop-finder--title root)))
              (funcall (plist-get (cadr items) :action)))
            (should (equal lispm-desktop-finder--directory root))))
      (kill-buffer lispm-desktop-finder-buffer-name))))

(ert-deftest lispm-desktop-test-finder-up-button ()
  "The Finder's header opens with a button naming the enclosing folder,
pressed to go there; at the top it is dimmed and does nothing."
  (lispm-desktop-test--with-folder
    (unwind-protect
        (let ((root (file-name-as-directory folder)))
          (lispm-desktop-finder--go (expand-file-name "zeta" folder))
          (lispm-desktop-finder--in
            (let* ((label (lispm-desktop-finder--up-label))
                   (button (lispm-desktop-finder--up-button label)))
              (should (equal (substring-no-properties label)
                             (concat "▲ " (lispm-desktop-finder--title root))))
              (should (eq (keymap-lookup (get-text-property 0 'keymap button)
                                         "<header-line> <down-mouse-1>")
                          #'lispm-desktop-finder--press-up)))
            (cl-letf (((symbol-function 'lispm-desktop-platinum-press)
                       (lambda (_position _pressed-p command) (funcall command))))
              (lispm-desktop-finder--press-up
               `(down-mouse-1 (,(selected-window) header-line (10 . 10) 0 nil nil (0 . 0) nil
                               (0 . 0) (0 . 0)))))
            (should (equal lispm-desktop-finder--directory root))
            (should (equal lispm-desktop-finder--selected "zeta")))
          (lispm-desktop-finder--go "/")
          (lispm-desktop-finder--in
            (should (equal (lispm-desktop-finder--up-label) "▲"))
            (should-not (get-text-property 0 'keymap (lispm-desktop-finder--up-button "▲")))))
      (kill-buffer lispm-desktop-finder-buffer-name))))

(ert-deftest lispm-desktop-test-control-strip-segments ()
  "The Control Strip's segments are 24 pixels tall in its own greys, each
parted from the next by a black line; a pressed module is darkened."
  (let ((palette (lispm-desktop-test--platinum-palette)))
    (dolist (kind '(collapse left right module tab))
      (should (equal (lispm-desktop-test--size (lispm-desktop-platinum-draw-strip-segment palette kind))
                     (cons (lispm-desktop-platinum-strip-segment-width kind) 24))))
    (let ((module (lispm-desktop-platinum-draw-strip-segment
                   palette 'module :menu t
                   :icon (expand-file-name "icons/os9/mail.svg" lispm-desktop-directory)))
          (pressed (lispm-desktop-platinum-draw-strip-segment palette 'module :state 'pressed))
          (disabled (lispm-desktop-platinum-draw-strip-segment palette 'left :state 'disabled)))
      (cl-flet ((at (svg x y) (lispm-desktop-test--pixel svg x y)))
        (should (equal (at module 10 0) "#000000"))
        (should (equal (at module 0 10) "#C0C0C0"))
        (should (equal (at module 1 10) "#FFFFFF"))
        (should (equal (at module 25 10) "#A0A0A0"))
        (should (equal (at module 26 10) "#808080"))
        (should (equal (at module 27 10) "#000000"))
        ;; The menu's triangle.
        (should (equal (at module 21 11) "#000000"))
        (should (equal (at pressed 10 12) "#808080"))
        (should (equal (at pressed 0 12) "#404040"))
        (should (equal (at disabled 0 12) "#808080")))
      (when (fboundp 'libxml-parse-xml-region)
        (should (dom-by-tag module 'svg))))))

;;;; Controls on lines

(defun lispm-desktop-test--image-width (string position)
  "Return the width of the SVG image STRING displays at POSITION."
  (let* ((spec (get-text-property position 'display string))
         (data (plist-get (cdr spec) :data)))
    (and (string-match "width=\"\\([0-9]+\\)\"" data)
         (string-to-number (match-string 1 data)))))

(ert-deftest lispm-desktop-test-tab-row ()
  "A row of tabs is as wide as asked, the pane's edge running on after the
tabs, which carry their properties."
  (let* ((lispm-desktop-platinum-current-palette (lispm-desktop-test--platinum-palette))
         (map (make-sparse-keymap))
         (row (lispm-desktop-platinum-tabs `(("One" t keymap ,map help-echo "First") ("Two" nil)) 400)))
    (should (= (length row) 6))
    (should (= (apply #'+ (mapcar (lambda (i) (lispm-desktop-test--image-width row i))
                                  (number-sequence 0 5)))
               400))
    (should (eq (get-text-property 1 'keymap row) map))
    (should (equal (get-text-property 1 'help-echo row) "First"))
    ;; The tab in front and the one behind are drawn apart.
    (should-not (equal (get-text-property 1 'display row) (get-text-property 3 'display row)))))

(ert-deftest lispm-desktop-test-content-runs ()
  "A label drawn into a control keeps its runs: words in the system face,
icon glyphs in their own family and colour, images as images."
  (let* ((svg (svg-create 100 20))
         (icon (propertize "x" 'face '((:family "Symbols Nerd Font Mono") (:foreground "#D97757"))
                           'display '(raise 0.0)))
         (image (propertize " " 'display (list 'image :type 'svg :width 16 :height 16
                                               :file (expand-file-name "icons/os9/folder.svg"
                                                                       lispm-desktop-directory)))))
    (lispm-desktop-platinum-svg-content svg (concat icon " New" image) 0 0 20 "#000000"
                                          'lispm-desktop-platinum-list)
    (let ((texts (dom-by-tag svg 'text)))
      (should (seq-find (lambda (text) (string-prefix-p "Symbols Nerd Font Mono"
                                                        (dom-attr text 'font-family)))
                        texts))
      (should (seq-find (lambda (text) (equal (dom-attr text 'fill) "#D97757")) texts))
      (should (seq-find (lambda (text) (string-search "New" (dom-text text))) texts)))
    (when (fboundp 'libxml-parse-xml-region)
      (should (dom-by-tag svg 'svg)))))

(ert-deftest lispm-desktop-test-toolbar-label-keeps-image-icons ()
  "Trimming a toolbar label drops plain spaces but keeps a space showing an icon."
  (let ((icon (propertize " " 'display '(image :type png :file "x.png"))))
    (should (equal (lispm-desktop-sidebar--trim (concat " " icon "+ ")) (concat icon "+")))
    (should (get-text-property 0 'display (lispm-desktop-sidebar--trim (concat " " icon "+ "))))))

(ert-deftest lispm-desktop-test-focus-ring-follows-point ()
  "The control point is on is drawn focused; leaving it draws it plain again."
  (let ((lispm-desktop-platinum-current-palette (lispm-desktop-test--platinum-palette)))
    (with-temp-buffer
      (lispm-desktop-platinum-insert-push-button "OK" #'ignore)
      (insert " ")
      (lispm-desktop-platinum-insert-push-button "Cancel" #'ignore)
      (let ((plain (get-text-property 1 'lispm-desktop-platinum-plain))
            (focused (get-text-property 1 'lispm-desktop-platinum-focused)))
        (should-not (equal plain focused))
        (goto-char 1)
        (lispm-desktop-platinum-show-focus)
        (should (equal (get-text-property 1 'display) focused))
        (goto-char 3)
        (lispm-desktop-platinum-show-focus)
        (should (equal (get-text-property 1 'display) plain))
        (should (equal (get-text-property 3 'display) (get-text-property 3 'lispm-desktop-platinum-focused)))))))

(ert-deftest lispm-desktop-test-push-button-pushes ()
  "Return on a push button runs its action, drawn pushed in meanwhile."
  (let ((lispm-desktop-platinum-current-palette (lispm-desktop-test--platinum-palette))
        (ran 0)
        (shown nil))
    (with-temp-buffer
      (lispm-desktop-platinum-insert-push-button "OK" (lambda () (cl-incf ran)))
      (cl-letf (((symbol-function 'sit-for)
                 (lambda (&rest _) (setq shown (get-text-property 1 'display)) t))
                ((symbol-function 'redisplay) #'ignore))
        (let ((last-input-event ?\r))
          (lispm-desktop-platinum-push 1)))
      (should (= ran 1))
      (should (equal shown (get-text-property 1 'lispm-desktop-platinum-pressed)))
      (should-not (equal (get-text-property 1 'display) shown)))))

(ert-deftest lispm-desktop-test-scroll-bar-windows ()
  "Files get scroll bars; sessions and side windows other than lists do not."
  (save-window-excursion
    (delete-other-windows)
    (let ((lispm-desktop-platinum-scroll-bar-functions (list #'lispm-desktop--session-scroll-bar))
          (file (lispm-desktop-test--file "lispm-desktop-test-i.txt"))
          (session (lispm-desktop-test--session "lispm-desktop-test session 9")))
      (set-window-buffer (selected-window) file)
      (should (lispm-desktop-platinum--scroll-window-p (selected-window)))
      (set-window-buffer (selected-window) session)
      (should-not (lispm-desktop-platinum--scroll-window-p (selected-window)))
      (let ((side (display-buffer-in-side-window (get-buffer-create " lispm-desktop-test popup")
                                                 '((side . bottom)))))
        (should-not (lispm-desktop-platinum--scroll-window-p side))))))

(ert-deftest lispm-desktop-test-platinum-knows-no-application ()
  "Platinum draws the chrome and asks the desktop what it needs to know of
the applications: none of its files names the agents, the terminals, the
sidebar, the browser, the App Store, the editor or the desktop itself."
  (dolist (file (directory-files (expand-file-name "lisp" lispm-desktop-directory) t
                                 "\\`lispm-desktop-platinum.*\\.el\\'"))
    (with-temp-buffer
      (insert-file-contents file)
      (should-not (re-search-forward
                   (concat "lispm-desktop-\\(?:agent\\|sidebar\\|term-[a-ln-z]\\|browser\\|store\\|settings"
                           "\\|main-area\\|show\\_>\\|editor\\|finder\\|apps\\|surface\\|interface\\)"
                           "\\|lispm-desktop--")
                   nil t)))))

(ert-deftest lispm-desktop-test-platinum-asks-the-desktop ()
  "What platinum shows of the applications, the applications tell it: a
buffer's icon, title and status, whether its window scrolls or is listed,
what a side window's close box does and which windows trade places."
  (let ((lispm-desktop-platinum-buffer-icon-functions (list (lambda (_) "ICON")))
        (lispm-desktop-platinum-buffer-title-functions (list (lambda (_) "A title")))
        (lispm-desktop-platinum-scroll-bar-functions (list (lambda (_) 'yes)))
        (lispm-desktop-platinum-running-buffers-functions (list (lambda () (list (current-buffer)))))
        (closed nil))
    (with-temp-buffer
      (should (equal (lispm-desktop-platinum--choose-icon (current-buffer)) "ICON"))
      (should (equal (lispm-desktop-platinum-buffer-title (current-buffer)) "A title"))
      (should (member "A title" (lispm-desktop-platinum--running-programs))))
    (save-window-excursion
      (delete-other-windows)
      (let ((side (display-buffer-in-side-window (get-buffer-create " lispm-desktop-test side")
                                                 '((side . bottom))))
            (lispm-desktop-platinum-side-window-close-function (lambda (window) (setq closed window))))
        (should (lispm-desktop-platinum--scroll-window-p side))
        (lispm-desktop-platinum-close-window side)
        (should (eq closed side))
        (let ((lispm-desktop-platinum-scroll-bar-functions nil))
          (should-not (lispm-desktop-platinum--scroll-window-p side))
          (should (lispm-desktop-platinum--scroll-window-p (selected-window))))))
    (with-temp-buffer
      (setq-local lispm-desktop-platinum-listed nil)
      (let ((hidden (current-buffer)))
        (should-not (memq hidden (lispm-desktop-platinum-window-buffers (selected-window))))))))

(ert-deftest lispm-desktop-test-menu-text ()
  "A menu is drawn as Mac OS 9's: lit and shaded edges, items sixteen pixels
tall, separators engraved, checks marked, the item under the mouse in the
variation colour, disabled items greyed."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (lispm-desktop-platinum-current-palette palette)
         (items '((:label "One" :action ignore :checked t)
                  separator
                  (:label "Two" :action ignore :enabled nil)
                  (:label "Three" :action ignore)
                  (:label "More" :submenu (( :label "Four" :action ignore)))))
         (svg (lispm-desktop-popup-draw-menu palette items 200 3))
         (texts (dom-by-tag svg 'text)))
    (should (equal (car (lispm-desktop-popup--rows items)) '(0 16 22 38 54)))
    (should (equal (lispm-desktop-test--size svg) '(200 . 70)))
    (cl-flet ((at (x y) (lispm-desktop-test--pixel svg x y))
              (ink (label) (dom-attr (seq-find (lambda (text) (string-search label (dom-text text))) texts)
                                     'fill)))
      (should (equal (at 50 0) "#FFFFFF"))
      (should (equal (at 0 8) "#FFFFFF"))
      (should (equal (at 199 8) "#999999"))
      (should (equal (at 50 69) "#999999"))
      ;; The separator, engraved across the whole width.
      (should (equal (at 0 18) "#888888"))
      (should (equal (at 199 19) "#FFFFFF"))
      ;; Three, under the mouse.
      (should (equal (at 50 45) "#333399"))
      (should (equal (at 0 45) "#6666CC"))
      (should (equal (at 199 45) "#000088"))
      (should (equal (upcase (ink "Three")) "#FFFFFF"))
      (should (equal (upcase (ink "Two")) "#777777"))
      ;; SVG text carries non-ASCII characters as references.
      (should (ink (format "&#%d;" ?✓)))
      ;; More's submenu triangle.
      (should (equal (at 190 61) "#000000")))
    ;; The item under a y.
    (should (= (lispm-desktop-popup--index-at items 40) 3))
    (should-not (lispm-desktop-popup--index-at items 18))
    (should-not (lispm-desktop-popup--index-at items 25))
    ;; Moving down from One skips the separator and the disabled Two.
    (should (= (lispm-desktop-popup--step items 0 1) 3))
    (should (= (lispm-desktop-popup--step items 3 1) 4))
    ;; A dark theme greys disabled items in its own dim colour.
    (let* ((dracula (lispm-desktop-test--dracula-palette))
           (dark (lispm-desktop-popup-draw-menu dracula items 200 nil)))
      (should (equal (dom-attr (seq-find (lambda (text) (string-search "Two" (dom-text text)))
                                         (dom-by-tag dark 'text))
                               'fill)
                     (plist-get dracula :dim))))))

(ert-deftest lispm-desktop-test-small-menu ()
  "A small menu, as the Control Strip's, has items 13 pixels tall in the
small system font, labels 15 pixels in."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (lispm-desktop-platinum-current-palette palette)
         (lispm-desktop-popup-small t)
         (items '((:label "One" :action ignore) (:label "Two" :action ignore)))
         (svg (lispm-desktop-popup-draw-menu palette items 100 1)))
    (should (equal (lispm-desktop-test--size svg) '(100 . 26)))
    (should (equal (lispm-desktop-test--pixel svg 50 20) "#333399"))
    (should (= (lispm-desktop-popup--menu-width items)
               (+ 15 (lispm-desktop-platinum-content-width "Two" 'lispm-desktop-platinum-small) 11)))
    (should (seq-every-p (lambda (text) (= (dom-attr text 'x) 15))
                         (seq-remove (lambda (text) (string-search "&#" (dom-text text)))
                                     (dom-by-tag svg 'text))))))

(ert-deftest lispm-desktop-test-menu-icons ()
  "A menu whose items have icons shows each before its label, as Mac OS 9's
Apple menu did: the labels move over to make room."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (lispm-desktop-platinum-current-palette palette)
         (icon `(image :type png :data ,(base64-decode-string "iVBORw0KGgo=")))
         (items `((:label "Blocker" :action ignore :icon ,icon) (:label "Store" :action ignore)))
         (svg (lispm-desktop-popup-draw-menu palette items 200 nil))
         (images (dom-by-tag svg 'image)))
    (should (= (length images) 1))
    (should (= (dom-attr (car images) 'x) (lispm-desktop-popup--label-x)))
    (should (seq-every-p (lambda (text) (= (dom-attr text 'x) (+ (lispm-desktop-popup--label-x) 20)))
                         (dom-by-tag svg 'text)))
    (should (= (lispm-desktop-popup--menu-width items)
               (+ 20 (lispm-desktop-popup--menu-width (mapcar (lambda (item) (plist-put (copy-sequence item) :icon nil))
                                                              items)))))))

(ert-deftest lispm-desktop-test-submenu-turns-at-the-edge ()
  "A submenu opens beside its item, or on the menu's left when the right
has no room, as Mac OS 9's did at the screen's edge."
  (let* ((lispm-desktop-platinum-current-palette (lispm-desktop-test--platinum-palette))
         (items '((:label "One" :action ignore) (:label "More" :submenu ((:label "Two" :action ignore)))))
         (submenu (lispm-desktop-popup--submenu (nth 1 items)))
         (level (list :items items :x 100 :y 20 :width 80))
         (width (lispm-desktop-popup--menu-width submenu)))
    (should (equal (lispm-desktop-popup--submenu-place level 1 submenu 1000) '(178 35 nil)))
    (should (equal (lispm-desktop-popup--submenu-place level 1 submenu (+ 178 width 1)) '(102 35 right)))))

(ert-deftest lispm-desktop-test-menu-keys ()
  "Key equivalents are keyboard sequences only."
  (should (lispm-desktop-popup-keyboard-p (kbd "C-c a l")))
  (should-not (lispm-desktop-popup-keyboard-p [C-M-drag-mouse-1]))
  (should-not (lispm-desktop-popup-keyboard-p [menu-bar file open])))

(ert-deftest lispm-desktop-test-keymap-items ()
  "A menu keymap becomes menu items: labels, conditions, checks, submenus."
  (let ((map (make-sparse-keymap "Test"))
        (sub (make-sparse-keymap "Sub")))
    (define-key sub [inner] '(menu-item "Inner" ignore))
    ;; Bindings added later come first, as Emacs shows them.
    (define-key map [last] '(menu-item "Last" ignore :enable nil))
    (define-key map [sub] `(menu-item "More" ,sub))
    (define-key map [sep] '(menu-item "--"))
    (define-key map [hidden] '(menu-item "Hidden" ignore :visible nil))
    (define-key map [toggle] '(menu-item "Toggle" ignore :button (:toggle . t)))
    (define-key map [first] '(menu-item "First" ignore :keys "C-x f"))
    (let ((items (lispm-desktop-popup-keymap-items map)))
      (should (equal (mapcar (lambda (item) (if (eq item 'separator) item (plist-get item :label)))
                             items)
                     '("First" "Toggle" separator "More" "Last")))
      (should (equal (plist-get (nth 0 items) :key) "C-x f"))
      (should (plist-get (nth 1 items) :checked))
      (should-not (lispm-desktop-popup--enabled-p (nth 4 items)))
      (should (equal (mapcar (lambda (item) (plist-get item :label))
                             (lispm-desktop-popup--submenu (nth 3 items)))
                     '("Inner"))))))

(defun lispm-desktop-test--choose (label)
  "Return a stand-in for `lispm-desktop-popup-choose' that picks the item LABEL.
It follows submenus, as a click through a hierarchical menu does."
  (lambda (items &rest _)
    (named-let pick ((items items))
      (seq-some (lambda (item)
                  (when (consp item)
                    (cond ((equal (plist-get item :label) label) item)
                          ((plist-get item :submenu) (pick (lispm-desktop-popup--submenu item))))))
                items))))

(ert-deftest lispm-desktop-test-x-popup-menu-keymap ()
  "A keymap popped up by `x-popup-menu' answers with the chosen key path,
as the project tree's right-click menu looks its command up by it."
  (let ((map (make-sparse-keymap "Tree"))
        (sub (make-sparse-keymap "New")))
    (define-key sub [file] '(menu-item "File" find-file))
    (define-key map [new] `(menu-item "New" ,sub))
    (define-key map [rename] '(menu-item "Rename" rename-file))
    (cl-letf (((symbol-function 'lispm-desktop-popup-available-p) #'always)
              ((symbol-function 'lispm-desktop-popup--x-position) (lambda (_) '(10 . 20)))
              ((symbol-function 'lispm-desktop-popup-choose) (lispm-desktop-test--choose "File")))
      (let ((choice (lispm-desktop-popup--x-popup-menu #'ignore t map)))
        (should (equal choice '(new file)))
        (should (eq (lookup-key map (apply #'vector choice)) 'find-file))))
    (cl-letf (((symbol-function 'lispm-desktop-popup-available-p) #'always)
              ((symbol-function 'lispm-desktop-popup--x-position) (lambda (_) '(10 . 20)))
              ((symbol-function 'lispm-desktop-popup-choose) #'ignore))
      (should-not (lispm-desktop-popup--x-popup-menu #'ignore t map)))))

(ert-deftest lispm-desktop-test-x-popup-menu-panes ()
  "A menu of panes answers with the chosen item's value; lone strings
cannot be chosen, and panes are parted by separators."
  (let ((menu '("Spelling" ("" ("teh" . teh) "Suggestions") ("Actions" ("Save" . save) "--"))))
    (should (equal (mapcar (lambda (item) (if (consp item) (plist-get item :label) item))
                           (lispm-desktop-popup--pane-items menu))
                   '("teh" "Suggestions" separator "Actions" "Save" separator)))
    (should-not (lispm-desktop-popup--enabled-p (nth 1 (lispm-desktop-popup--pane-items menu))))
    (cl-letf (((symbol-function 'lispm-desktop-popup-available-p) #'always)
              ((symbol-function 'lispm-desktop-popup--x-position) (lambda (_) '(10 . 20)))
              ((symbol-function 'lispm-desktop-popup-choose) (lispm-desktop-test--choose "Save")))
      (should (eq (lispm-desktop-popup--x-popup-menu #'ignore t menu) 'save)))))

(ert-deftest lispm-desktop-test-x-popup-menu-passes-through ()
  "Without a position, or a display to draw on, the original menu runs."
  (let ((map (make-sparse-keymap "Tree")))
    (define-key map [rename] '(menu-item "Rename" rename-file))
    (should (eq (lispm-desktop-popup--x-popup-menu (lambda (&rest _) 'native) nil map) 'native))
    (should (eq (lispm-desktop-popup--x-popup-menu (lambda (&rest _) 'native) t map) 'native))))

(ert-deftest lispm-desktop-test-right-click-defers-to-local-command ()
  "A buffer with a right-click command of its own keeps it."
  (with-temp-buffer
    (should-not (lispm-desktop-popup--local-right-click-p nil))
    (let ((map (make-sparse-keymap)))
      (keymap-set map "<mouse-3>" #'rename-file)
      (use-local-map map)
      (should (lispm-desktop-popup--local-right-click-p nil)))))

(ert-deftest lispm-desktop-test-popups-fall-back ()
  "Without a graphical display, menus and alerts ask in the minibuffer."
  (should-not (lispm-desktop-popup-available-p))
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Two")))
    (should (equal (plist-get (lispm-desktop-popup-menu '((:label "One" :action ignore)
                                                          (:label "Two" :action ignore)))
                              :label)
                   "Two")))
  (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) '(?s "Save"))))
    (should (eq (lispm-desktop-popup-alert 'caution "Save?" '(("Cancel" cancel cancel)
                                                             ("Save" save default)))
                'save))))

(ert-deftest lispm-desktop-test-popup-holds-size ()
  "A popup frame keeps the size it was given as its font changes, when
Emacs sizes a frame anew to whole lines and columns of the new font, no
less than its window's minimum: the popup's buffer asks its window for a
line and a column, not four lines and ten columns, and has its frame
sized back."
  (with-temp-buffer
    (setq-local mode-line-format nil)
    (lispm-desktop-popup-hold-size)
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (should (= (window-min-size nil nil nil t) (frame-char-height)))
      (should (= (window-min-size nil t nil t) (frame-char-width)))))
  (let ((frame (selected-frame))
        (text '(80 . 25))
        (sized nil))
    (cl-letf (((symbol-function 'set-frame-size)
               (lambda (_frame width height &optional _pixelwise)
                 (push (cons width height) sized)
                 (setq text (cons width height))))
              ((symbol-function 'frame-text-width) (lambda (&optional _frame) (car text)))
              ((symbol-function 'frame-text-height) (lambda (&optional _frame) (cdr text))))
      (unwind-protect
          (progn
            ;; A frame `lispm-desktop-popup-set-size' never sized is left alone.
            (lispm-desktop-popup--keep-size (selected-window))
            (should-not sized)
            (lispm-desktop-popup-set-size frame 256 24)
            (should (equal sized '((256 . 24))))
            ;; A larger font: Emacs made it whole columns, and four lines.
            (setq text '(288 . 76) sized nil)
            (lispm-desktop-popup--keep-size (selected-window))
            (should (equal sized '((256 . 24))))
            (setq sized nil)
            (lispm-desktop-popup--keep-size (selected-window))
            (should-not sized))
        (set-frame-parameter frame 'lispm-desktop-popup-size nil)))))

(ert-deftest lispm-desktop-test-alert-text ()
  "An alert shows its message, its detail and a button per choice, framed
as Mac OS 9 framed its alerts."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (lispm-desktop-platinum-current-palette palette)
         (buttons '(("Don’t Save" discard nil) ("Cancel" cancel cancel) ("Save" save default)))
         (svg (lispm-desktop-popup-draw-alert palette 'caution "Save?" "Changes will be lost."
                                              buttons nil nil))
         (texts (mapcar #'dom-text (dom-by-tag svg 'text))))
    (should (seq-find (lambda (text) (string-search "Save?" text)) texts))
    (should (seq-find (lambda (text) (string-search "Changes will be lost." text)) texts))
    (cl-flet ((at (x y) (lispm-desktop-test--pixel svg x y)))
      (should (equal (at 0 30) "#FF9999"))
      (should (equal (at 1 30) "#FFFFFF"))
      (should (equal (at 30 1) "#FFFFFF"))
      (should (equal (at 50 40) "#DDDDDD"))
      (should (equal (at (- lispm-desktop-popup-alert-width 2) 30) "#999999"))
      (should (equal (at (- lispm-desktop-popup-alert-width 1) 30) "#FF6666")))))

(ert-deftest lispm-desktop-test-alert-layout ()
  "Don't Save stands apart under the message; Cancel and the default button
stand at the right, ten pixels apart, the default's ring twelve pixels
from the edge."
  (let* ((lispm-desktop-platinum-current-palette (lispm-desktop-test--platinum-palette))
         (layout (lispm-desktop-popup--alert-layout
                  "Save changes to the document before closing?" nil
                  '(("Don’t Save" discard nil) ("Cancel" cancel cancel) ("Save" save default))))
         (buttons (plist-get layout :buttons))
         (w lispm-desktop-popup-alert-width))
    (should (equal (mapcar #'cadr buttons) '(discard cancel save)))
    (pcase-let ((`(,_ ,_ ,_ ,dont ,_ ,_) (nth 0 buttons))
                (`(,_ ,_ ,_ ,cancel ,_ ,cancel-width) (nth 1 buttons))
                (`(,_ ,_ ,_ ,save ,_ ,save-width) (nth 2 buttons)))
      (should (= dont (- lispm-desktop-popup-alert-text-x 4)))
      (should (= (+ save save-width 3) (- w 12)))
      (should (= (- (- save 3) (+ cancel cancel-width)) lispm-desktop-popup-button-gap)))
    (cl-letf (((symbol-function 'lispm-desktop-modal-over)
               (lambda (event _frame) (event-start event)))
              ((symbol-function 'frame-parameter) (lambda (&rest _) buttons)))
      (pcase-let ((`(,_ ,_ ,_ ,x ,y ,_) (nth 1 buttons)))
        (should (eq (lispm-desktop-popup--button-at
                     (list 'mouse-1 (list (selected-window) 1 (cons (+ x 5) (+ y 5)) 0))
                     nil)
                    'cancel))))))

;;;; lispm-desktop-modal

(ert-deftest lispm-desktop-test-modal-key-roles ()
  "Every role of a key in a modal panel has a key the HHKB types: C-i for
Tab, C-m for Return, C-[ and C-g for Escape, DEL for Back, and C-f, C-b,
C-n and C-p for the arrows, which stay as well."
  (should (eq (lispm-desktop-modal-role ?\C-i) 'next))
  (should (eq (lispm-desktop-modal-role 'backtab) 'previous))
  (should (eq (lispm-desktop-modal-role ?\C-m) 'confirm))
  (should (eq (lispm-desktop-modal-role ?\C-\[) 'cancel))
  (should (eq (lispm-desktop-modal-role ?\C-g) 'cancel))
  (should (eq (lispm-desktop-modal-role ?\M-.) 'cancel))
  (should (eq (lispm-desktop-modal-role ?\d) 'back))
  (should (eq (lispm-desktop-modal-role ?\s) 'press))
  (should (equal (mapcar #'lispm-desktop-modal-step '(?\C-f ?\C-n tab right ?\C-b ?\C-p backtab up ?a))
                 '(1 1 1 1 -1 -1 -1 -1 nil)))
  (should-not (lispm-desktop-modal-role ?a)))

(ert-deftest lispm-desktop-test-modal-cycle ()
  "A focus ring wraps around; from no control it goes to the first or the
last, and it passes over the controls it is told to."
  (should (= (lispm-desktop-modal-cycle 2 1 3) 0))
  (should (= (lispm-desktop-modal-cycle 0 -1 3) 2))
  (should (= (lispm-desktop-modal-cycle nil 1 3) 0))
  (should (= (lispm-desktop-modal-cycle nil -1 3) 2))
  (should (= (lispm-desktop-modal-cycle 1 4 6) 5))
  (should (= (lispm-desktop-modal-cycle 0 1 4 (lambda (index) (/= index 1))) 2))
  (should-not (lispm-desktop-modal-cycle 0 1 4 #'ignore))
  (should-not (lispm-desktop-modal-cycle nil 1 0)))

(ert-deftest lispm-desktop-test-modal-hit ()
  "A click is on a control when it lies in its rectangle, the left and top
edges in, the right and bottom out."
  (let ((layout '((a 10 10 20 10) (b 30 10 20 10) (c 0 0 0 0))))
    (should (eq (lispm-desktop-modal-hit layout 10 10) 'a))
    (should (eq (lispm-desktop-modal-hit layout 29 19) 'a))
    (should (eq (lispm-desktop-modal-hit layout 30 10) 'b))
    (should-not (lispm-desktop-modal-hit layout 50 10))
    (should-not (lispm-desktop-modal-hit layout 20 20))
    (should-not (lispm-desktop-modal-hit layout 0 0))
    (should-not (lispm-desktop-modal-inside-p nil 0 0))))

(ert-deftest lispm-desktop-test-modal-flash ()
  "A control chosen blinks for its kind's time: a button eight ticks, a
menu item three, drawn back as it was when asked."
  (let ((log nil))
    (cl-letf (((symbol-function 'sit-for) (lambda (seconds) (push (list 'wait seconds) log)))
              ((symbol-function 'redisplay) #'ignore))
      (lispm-desktop-modal-flash 'button (lambda () (push 'pressed log)))
      (should (equal (reverse log) '(pressed (wait 0.133))))
      (setq log nil)
      (lispm-desktop-modal-flash 'menu-item (lambda () (push 'off log)) (lambda () (push 'on log)))
      (should (equal (reverse log) '(off (wait 0.05) on))))))

(ert-deftest lispm-desktop-test-modal-needs-no-platinum ()
  "The modal kit stands on its own: it loads none of the desktop's files."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "lisp/lispm-desktop-modal.el" lispm-desktop-directory))
    (should-not (re-search-forward "(require 'lispm-desktop" nil t))))

(ert-deftest lispm-desktop-test-modal-hold ()
  "A control held with the mouse is drawn pushed in only while the mouse
is over it, and chosen only when the button rises over it; Escape gives
up."
  (let ((draws nil))
    (cl-flet ((hold (events)
                (setq draws nil)
                (let ((unread-command-events events))
                  (lispm-desktop-modal-hold (lambda (event) (eq (cadr event) 'on))
                                            (lambda (in) (push in draws))))))
      (should (hold '((mouse-movement off) (mouse-movement on) (mouse-1 on))))
      (should (equal (reverse draws) '(t nil t nil)))
      (should-not (hold '((mouse-movement off) (mouse-1 off))))
      (should (equal (reverse draws) '(t nil)))
      (should-not (hold '(escape)))
      (should (equal (reverse draws) '(t nil))))))

(ert-deftest lispm-desktop-test-store-grid-buttons-press-on-release ()
  "The App Store's grid buttons are held pushed in from the press, as
Mac OS 9's buttons were, and pressed as the mouse rises over them."
  (should (eq (keymap-lookup lispm-desktop-store-grid-map "<down-mouse-1>")
              #'lispm-desktop-store--grid-press)))

(ert-deftest lispm-desktop-test-modal-moved-p ()
  "A move of the mouse by more than `double-click-fuzz' is a drag; less is
a hand's jitter in a click."
  (let ((double-click-fuzz 3))
    (should-not (lispm-desktop-modal-moved-p '(3 . -3)))
    (should (lispm-desktop-modal-moved-p '(4 . 0)))
    (should (lispm-desktop-modal-moved-p '(0 . -4)))
    (should-not (lispm-desktop-modal-moved-p nil))))

(defmacro lispm-desktop-test--with-alert-keys (&rest body)
  "Run BODY where `lispm-desktop-popup-alert' reads the events queued in
`unread-command-events', its popup drawn nowhere.  In BODY, `draws' holds
the (PRESSED FOCUSED) of each drawing, the latest first."
  (declare (indent 0))
  `(let ((draws nil))
     (cl-letf (((symbol-function 'lispm-desktop-popup-available-p) (lambda () t))
               ((symbol-function 'lispm-desktop-popup-open-alert) (lambda (&rest _) 'alert))
               ((symbol-function 'lispm-desktop-popup--draw-alert)
                (lambda (_frame _icon _message _detail _buttons pressed focused)
                  (push (list pressed focused) draws)))
               ((symbol-function 'lispm-desktop-popup-dispose) #'ignore)
               ((symbol-function 'sit-for) #'ignore)
               ((symbol-function 'redisplay) #'ignore))
       ,@body)))

(ert-deftest lispm-desktop-test-alert-keys ()
  "An alert answers from the keyboard: Return presses its default button,
Escape its Cancel, or its default when it has none; Tab and Shift-Tab,
C-f and C-b move the focus ring from button to button, and Space presses
the button ringed; a button's own keys press it."
  (let ((buttons '(("Don’t Save" discard nil ?\M-d) ("Cancel" cancel cancel) ("Save" save default))))
    (lispm-desktop-test--with-alert-keys
      (cl-flet ((answer (events &optional given)
                  (let ((unread-command-events events))
                    (lispm-desktop-popup-alert 'caution "Save?" (or given buttons)))))
        (should (eq (answer '(return)) 'save))
        (should (eq (answer '(?\r)) 'save))
        (should (eq (answer '(escape)) 'cancel))
        (should (eq (answer '(?\C-g)) 'cancel))
        (should (eq (answer '(escape) '(("OK" ok default))) 'ok))
        (should (eq (answer '(?\M-d)) 'discard))
        (should (eq (answer '(tab ?\s)) 'discard))
        (should (eq (answer '(tab tab ?\s)) 'cancel))
        (should (eq (answer '(?\C-f ?\C-f ?\C-f ?\s)) 'save))
        (should (eq (answer '(backtab ?\s)) 'save))
        (should (eq (answer '(?\C-b ?\C-b ?\s)) 'cancel))
        ;; Space with no button ringed presses nothing.
        (should (eq (answer '(?\s return)) 'save))
        ;; C-n and C-p ring the buttons as Tab and Shift-Tab do.
        (should (eq (answer '(?\C-n ?\s)) 'discard))
        (should (eq (answer '(?\C-p ?\s)) 'save))
        (should (eq (answer '(S-iso-lefttab ?\s)) 'save))))))

(ert-deftest lispm-desktop-test-title-bar-clicks ()
  "A title bar brings its window forward, rolls up on a double click, and
pops up its buffers on a Meta click."
  (should (keymap-lookup lispm-desktop-platinum--title-map "<tab-line> <mouse-1>"))
  (should (keymap-lookup lispm-desktop-platinum--title-map "<tab-line> <double-mouse-1>"))
  (should (keymap-lookup lispm-desktop-platinum--title-map "<tab-line> M-<mouse-1>")))

(ert-deftest lispm-desktop-test-shade-or-zoom ()
  "A window alone in its column zooms; one sharing it rolls up."
  (save-window-excursion
    (delete-other-windows)
    (let ((zoomed nil) (shaded nil))
      (cl-letf (((symbol-function 'lispm-desktop-platinum-zoom-window) (lambda (_) (setq zoomed t)))
                ((symbol-function 'lispm-desktop-platinum-collapse-window) (lambda (_) (setq shaded t))))
        (lispm-desktop-platinum-shade-or-zoom (selected-window))
        (should (and zoomed (not shaded)))
        (setq zoomed nil)
        (lispm-desktop-platinum-shade-or-zoom (split-window-below))
        (should (and shaded (not zoomed)))))))

(ert-deftest lispm-desktop-test-roll-up ()
  "A floating window rolls up to its title bar alone, as Mac OS 9's
WindowShade rolled it: no minimum height of Emacs's holds it taller, and
no scroll function runs on it while none of its text is in sight.  It
rolls down to the height it is given, its buffer as it was."
  (save-window-excursion
    (delete-other-windows)
    (with-temp-buffer
      (setq-local tab-line-format "Title" header-line-format nil mode-line-format nil)
      (let ((frame (selected-frame))
            (window (selected-window))
            (heights nil))
        (set-window-buffer window (current-buffer))
        (let ((title (window-tab-line-height window))
              (minimum (window-min-size window nil nil t))
              (unrolled (frame-text-height frame)))
          (should (> minimum title))
          (cl-letf (((symbol-function 'set-frame-height)
                     (lambda (_frame height &optional _pretend pixelwise)
                       (should pixelwise)
                       (push height heights))))
            (lispm-desktop-platinum-roll-up frame title)
            (should (= (window-min-size window nil nil t) title))
            ;; The title bar, unless a line of the font is as tall: Emacs
            ;; shows a tab line only in a window taller than a line.
            (should (= (car heights) (max title (1+ (frame-char-height frame)))))
            (should (= (lispm-desktop-platinum-rolled-up-height frame) unrolled))
            (should (local-variable-p 'window-scroll-functions))
            (should-not window-scroll-functions)
            (lispm-desktop-platinum-roll-down frame 300)
            (should (= (car heights) 300))
            (should (= (window-min-size window nil nil t) minimum))
            (should-not (local-variable-p 'window-scroll-functions))
            (should-not (lispm-desktop-platinum-rolled-up-height frame))))))))

(ert-deftest lispm-desktop-test-rolled-up-window-kept-on-desktop ()
  "A window rolled up keeps to its title bar when put back on the desktop,
as after it was moved or its screen changed size, and is remembered at
the height it rolls down to."
  (let* ((frame (selected-frame))
         (rolled (frame-text-height frame))
         (sizes nil))
    (cl-letf (((symbol-function 'frame-parent) (lambda (&optional _) frame))
              ((symbol-function 'set-frame-size)
               (lambda (_frame width height &optional _pixelwise) (push (cons width height) sizes)))
              ((symbol-function 'set-frame-position) #'ignore)
              ;; CI's Emacs builds have no window system, and so no tab bar.
              ((symbol-function 'tab-bar-height) (lambda (&rest _) 0)))
      (unwind-protect
          (progn
            (set-frame-parameter frame 'lispm-desktop-platinum-rolled-up (cons (current-buffer) 300))
            (should (= (nth 3 (lispm-desktop-apps--geometry frame)) 300))
            (lispm-desktop-apps--put frame (list 0 0 (frame-text-width frame) 300))
            (should (seq-every-p (lambda (size) (= (cdr size) rolled)) sizes))
            (set-frame-parameter frame 'lispm-desktop-platinum-rolled-up nil)
            (should (= (nth 3 (lispm-desktop-apps--geometry frame)) rolled))
            (lispm-desktop-apps--put frame (list 0 0 (frame-text-width frame) 300))
            (should (/= (cdar sizes) rolled)))
        (set-frame-parameter frame 'lispm-desktop-platinum-rolled-up nil)))))

(ert-deftest lispm-desktop-test-status-drawings ()
  "The battery and the load meter, the Control Strip's module icons, are
sixteen pixels square and draw at any level."
  (let ((palette (lispm-desktop-test--platinum-palette)))
    (dolist (percent '(0 5 50 100))
      (should (equal (lispm-desktop-test--size (lispm-desktop-platinum-draw-battery palette percent t))
                     '(16 . 16)))
      (should (eq (dom-tag (lispm-desktop-platinum-draw-battery palette percent nil)) 'svg)))
    (should (equal (lispm-desktop-test--pixel (lispm-desktop-platinum-draw-battery palette 100 nil) 5 7)
                   "#000000"))
    (should (equal (lispm-desktop-test--pixel (lispm-desktop-platinum-draw-battery palette 0 nil) 5 7)
                   "#C0C0C0"))
    (dolist (fraction '(0.0 0.5 1.5))
      (should (equal (lispm-desktop-test--size (lispm-desktop-platinum-draw-meter palette fraction))
                     '(16 . 16))))
    (cl-flet ((bar (fraction x) (lispm-desktop-test--pixel (lispm-desktop-platinum-draw-meter palette fraction)
                                                           x 13)))
      (should (equal (bar 0.5 2) "#000000"))
      (should (equal (bar 0.5 12) "#808080")))))

(require 'time)
(defvar load)
(defvar mail)

(ert-deftest lispm-desktop-test-status-notes ()
  "What Emacs computes for the time and the battery is noted for the menu bar."
  (let ((load " 1.25") (mail t) (lispm-desktop-platinum--load nil) (lispm-desktop-platinum--mail nil))
    (lispm-desktop-platinum--note-time)
    (should (equal lispm-desktop-platinum--load "1.25"))
    (should (eq lispm-desktop-platinum--mail t)))
  (let ((lispm-desktop-platinum--battery nil))
    (lispm-desktop-platinum--note-battery '((?p . "87")))
    (should (equal (alist-get ?p lispm-desktop-platinum--battery) "87"))))

(ert-deftest lispm-desktop-test-menu-bar-lines ()
  "The menu bar is drawn: a lit top line, platinum, a shaded line and a black
line, the screen's corners rounded over its ends, an open menu's title
in the variation colour; the bar runs from edge to edge."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (plain (lispm-desktop-platinum-draw-menu-title palette 40 :corner 'left))
         (open (lispm-desktop-platinum-draw-menu-title palette 40 :highlighted t :corner 'right)))
    (cl-flet ((at (svg x y) (lispm-desktop-test--pixel svg x y)))
      (should (equal (mapcar (lambda (y) (at plain 20 y)) '(0 1 10 18 19))
                     '("#FFFFFF" "#DDDDDD" "#DDDDDD" "#999999" "#000000")))
      (should (equal (at plain 0 0) "#000000"))
      (should (equal (at plain 0 10) "#FFFFFF"))
      (should (equal (at open 39 0) "#000000"))
      (should (equal (mapcar (lambda (y) (at open 20 y)) '(0 1 10 18 19))
                     '("#6666CC" "#333399" "#333399" "#000088" "#000000")))))
  (let ((lispm-desktop-platinum-current-palette (lispm-desktop-test--platinum-palette))
        (lispm-desktop-platinum-menus-function nil))
    (cl-letf (((symbol-function 'lispm-desktop-platinum-menu-status) #'ignore)
              ((symbol-function 'frame-inner-width) (lambda (&rest _) 900)))
      ;; The bar's own stretch takes what its titles leave.
      (let* ((items (lispm-desktop-platinum-menu-bar-line))
             (filler (assq 'lispm-desktop-menu-filler items))
             (others (remq filler items)))
        (should (= (lispm-desktop-test--image-width (nth 2 filler) 0)
                   (- 900 (apply #'+ (mapcar (lambda (item) (string-pixel-width (nth 2 item)))
                                             others)))))))))

(ert-deftest lispm-desktop-test-menu-status ()
  "The menu bar keeps other status text and the time, as Mac OS 9's held
its clock alone; the load, the mail and the battery are in the Control
Strip."
  (let ((labels nil)
        (lispm-desktop-platinum-current-palette (lispm-desktop-platinum-derive-palette "#FFFFFF" "#000000"
                                                                                 "#777777" "#6363CE"))
        (global-mode-string '("" "λ 2" display-time-string))
        (display-time-string "9:41AM")
        (display-time-mode t)
        (display-time-format "%-I:%M %p")
        (lispm-desktop-platinum--load "")
        (lispm-desktop-platinum--mail nil))
    ;; A batch Emacs formats every mode line construct as "".
    (cl-letf (((symbol-function 'format-mode-line)
               (lambda (construct &rest _)
                 (mapconcat (lambda (part) (if (stringp part) part "")) (ensure-list construct) ""))))
      (setq labels (mapcar (lambda (item)
                             (substring-no-properties
                              (get-text-property 0 'lispm-desktop-platinum-label (nth 2 item))))
                           (lispm-desktop-platinum-menu-status))))
    (progn
      (should (string-search "λ 2" (nth 0 labels)))
      (should-not (seq-some (lambda (label) (string-search "9:41AM" label)) labels))
      (should (= (length labels) 2))))
  (let ((global-mode-string nil) (display-time-mode nil))
    (should-not (lispm-desktop-platinum-menu-status))))

(ert-deftest lispm-desktop-test-age-short-date ()
  "Old sessions are dated as Finder dated files; recent ones by their age."
  (let ((now (float-time (encode-time '(0 0 12 13 10 2026 nil -1 nil)))))
    (should (equal (lispm-desktop-agent-age (- now 120) now) "2m"))
    (should (equal (lispm-desktop-agent-age (float-time (encode-time '(0 0 12 13 5 2026 nil -1 nil))) now)
                   "5/13/26"))))

(ert-deftest lispm-desktop-test-row-shades-sorted-column ()
  "A row's right-hand column carries the sorted column's face to the edge."
  (cl-letf (((symbol-function 'lispm-desktop-sidebar--column-pixels) (lambda () 100)))
    (let ((row (lispm-desktop-sidebar--row "" "name" "3h" 300)))
      (should (string-search "3h" row))
      (should (equal (get-text-property (1- (length row)) 'face row) 'lispm-desktop-platinum-sorted)))))

;;;; Main area columns

(defmacro lispm-desktop-test--with-layout (&rest body)
  "Run BODY in a frame of one main window, with the columns' rules on."
  `(save-window-excursion
     (let ((display-buffer-alist
            '((lispm-desktop-main-area--astray-p lispm-desktop-main-area-display-file)
              (lispm-desktop-main-area--session-p lispm-desktop-main-area-display-session)))
           (switch-to-buffer-obey-display-actions t)
           (window-min-width 4))
       (delete-other-windows)
       ,@body)))

(defun lispm-desktop-test--session (name)
  "Return a fresh buffer NAME laid out as a session."
  (with-current-buffer (get-buffer-create name)
    (setq lispm-desktop-conversation-buffer t)
    (current-buffer)))

(defun lispm-desktop-test--file (name)
  "Return a buffer visiting a temporary file NAME."
  (find-file-noselect (expand-file-name name temporary-file-directory)))

(defun lispm-desktop-test--left-p (a b)
  "Return non-nil when the window of buffer A is left of B's."
  (< (car (window-edges (get-buffer-window a))) (car (window-edges (get-buffer-window b)))))

(ert-deftest lispm-desktop-test-file-from-session-goes-right ()
  "A file opened from a session window opens to its right; the session stays."
  (lispm-desktop-test--with-layout
   (let ((session (lispm-desktop-test--session "lispm-desktop-test session"))
         (file (lispm-desktop-test--file "lispm-desktop-test-a.txt")))
     (switch-to-buffer session)
     (switch-to-buffer file)
     (should (get-buffer-window session))
     (should (lispm-desktop-test--left-p session file))
     (should (eq (window-buffer (selected-window)) file)))))

(ert-deftest lispm-desktop-test-session-from-file-goes-left ()
  "A session shown from a file window goes to the left of the file."
  (lispm-desktop-test--with-layout
   (let ((session (lispm-desktop-test--session "lispm-desktop-test session 2"))
         (file (lispm-desktop-test--file "lispm-desktop-test-b.txt")))
     (switch-to-buffer file)
     (switch-to-buffer session)
     (should (get-buffer-window file))
     (should (lispm-desktop-test--left-p session file)))))

(ert-deftest lispm-desktop-test-session-reuses-session-column ()
  "With both columns open, a session from the file side replaces the session."
  (lispm-desktop-test--with-layout
   (let ((one (lispm-desktop-test--session "lispm-desktop-test session 3"))
         (two (lispm-desktop-test--session "lispm-desktop-test session 4"))
         (file (lispm-desktop-test--file "lispm-desktop-test-c.txt")))
     (switch-to-buffer one)
     (switch-to-buffer file)
     (switch-to-buffer two)
     (should (= (length (window-list)) 2))
     (should-not (get-buffer-window one))
     (should (lispm-desktop-test--left-p two file)))))

(ert-deftest lispm-desktop-test-other-buffer-from-session-goes-right ()
  "Any buffer switched to from a session window goes right, not over it."
  (lispm-desktop-test--with-layout
   (let ((session (lispm-desktop-test--session "lispm-desktop-test session 6"))
         (other (get-buffer-create "lispm-desktop-test other")))
     (switch-to-buffer session)
     (switch-to-buffer other)
     (should (get-buffer-window session))
     (should (lispm-desktop-test--left-p session other)))))

(ert-deftest lispm-desktop-test-popups-are-left-alone ()
  "A buffer displayed as a popup is not taken over by the columns."
  (lispm-desktop-test--with-layout
   (let ((session (lispm-desktop-test--session "lispm-desktop-test session 7")))
     (switch-to-buffer session)
     (should-not (lispm-desktop-main-area--astray-p (get-buffer-create "lispm-desktop-test popup")
                                                    '(display-buffer-pop-up-window))))))

(define-derived-mode lispm-desktop-test-chat-mode special-mode "Chat"
  "A stand-in for an agent's chat mode.")

(ert-deftest lispm-desktop-test-chat-mode-is-a-session ()
  "A buffer in a registered chat mode keeps to the session column at once."
  (lispm-desktop-test--with-layout
   (let ((lispm-desktop-conversation-modes '(lispm-desktop-test-chat-mode))
         (file (lispm-desktop-test--file "lispm-desktop-test-f.txt"))
         (chat (with-current-buffer (get-buffer-create "lispm-desktop-test chat")
                 (lispm-desktop-test-chat-mode)
                 (current-buffer))))
     (switch-to-buffer file)
     (pop-to-buffer chat)
     (should (lispm-desktop-test--left-p chat file)))))

(ert-deftest lispm-desktop-test-second-file-stays-right ()
  "A second file opened from the session replaces the file, not the session."
  (lispm-desktop-test--with-layout
   (let ((session (lispm-desktop-test--session "lispm-desktop-test session 5"))
         (a (lispm-desktop-test--file "lispm-desktop-test-d.txt"))
         (b (lispm-desktop-test--file "lispm-desktop-test-e.txt")))
     (switch-to-buffer session)
     (switch-to-buffer a)
     (select-window (get-buffer-window session))
     (switch-to-buffer b)
     (should (= (length (window-list)) 2))
     (should (lispm-desktop-test--left-p session b)))))

(ert-deftest lispm-desktop-test-menu-bar-only-outside-the-frame ()
  "A graphical frame keeps its menu bar only where NS draws it, outside."
  (let (set)
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (frame) (not (eq frame 'tty))))
              ((symbol-function 'framep)
               (lambda (frame) (if (eq frame 'tty) t frame)))
              ((symbol-function 'set-frame-parameter)
               (lambda (frame parameter value)
                 (push (list frame parameter value) set))))
      (lispm-desktop-frame-menu-bar 'ns)
      (lispm-desktop-frame-menu-bar 'tty)
      (lispm-desktop-frame-menu-bar 'x))
    (should (equal set '((x menu-bar-lines 0))))))

(ert-deftest lispm-desktop-test-default-font-first-installed ()
  "The default face takes the first preferred family that is installed,
unless the user customized it."
  (let (set)
    (cl-letf (((symbol-function 'font-family-list)
               (lambda (&optional _frame) '("Andale Mono" "Menlo")))
              ((symbol-function 'set-face-attribute)
               (lambda (face _frame &rest attributes)
                 (push (cons face attributes) set))))
      (lispm-desktop-pin-default-font)
      (should (equal set '((default :family "Menlo" :height 120))))
      (setq set nil)
      (cl-letf (((get 'default 'saved-face) '((t :family "Iosevka"))))
        (lispm-desktop-pin-default-font))
      (should-not set))))

(defun lispm-desktop-test--x-properties (checks)
  "Stand in for `x-window-property', reading CHECKS.
CHECKS maps a window (0 for the root) to the window its
_NET_SUPPORTING_WM_CHECK names, or to `gone' for a destroyed window."
  (lambda (_property _frame _type source &rest _)
    (pcase (alist-get source checks)
      ('nil nil)
      ('gone (error "BadWindow"))
      (check check))))

(ert-deftest lispm-desktop-test-exwm-only-without-a-window-manager ()
  "Emacs manages an X display only when no window manager runs on it."
  (let ((lispm-desktop-window-manager 'auto))
    (cl-letf (((symbol-function 'framep) (lambda (frame) frame)))
      (cl-letf (((symbol-function 'x-window-property)
                 (lispm-desktop-test--x-properties nil)))
        (should-not (lispm-desktop-exwm-wanted-p 'ns))
        (should (lispm-desktop-exwm-wanted-p 'x))
        (let ((lispm-desktop-window-manager nil))
          (should-not (lispm-desktop-exwm-wanted-p 'x))))
      ;; A running window manager: the root names a window naming itself.
      (cl-letf (((symbol-function 'x-window-property)
                 (lispm-desktop-test--x-properties '((0 . 7) (7 . 7)))))
        (should-not (lispm-desktop-exwm-wanted-p 'x)))
      ;; A window manager that quit left its check window's name behind.
      (cl-letf (((symbol-function 'x-window-property)
                 (lispm-desktop-test--x-properties '((0 . 7) (7 . gone)))))
        (should (lispm-desktop-exwm-wanted-p 'x))))))

;;;; The browser

(ert-deftest lispm-desktop-test-browser-keys-show-in-menus ()
  "The browser's Command keys show beside their menu items."
  (should (equal (lispm-desktop-browser--key 'lispm-browser-add-bookmark) "M-d"))
  (let ((lispm-desktop-browser--navigation '(:page t :back t :forward t)))
    (should (equal (plist-get (car (lispm-desktop-browser--go-items)) :key) "M-["))))

(defconst lispm-desktop-test--png
  (base64-decode-string
   "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8DwHwAFBQIAX8jx0gAAAABJRU5ErkJggg==")
  "A PNG one pixel square, standing in for an extension's icon.")

(ert-deftest lispm-desktop-test-browser-extension-buttons-draw ()
  "An extension's toolbar button is a small bevel holding its icon, greyed
when it does nothing on the page, its badge at the lower right, pushed in
while its popup is up; one without an icon shows its name's initial.  The
Extensions button is a puzzle piece in a bevel."
  (let* ((lispm-desktop-platinum-current-palette (lispm-desktop-test--platinum-palette))
         (icon `(image :type png :data ,lispm-desktop-test--png :width 16 :height 16))
         (button (list :id "a" :name "Blocker" :icon icon :badge "12345"
                       :badge-color "#123456" :badge-text-color "#FFFFFF" :enabled t))
         (svg (lispm-desktop-browser--draw-extension button)))
    (should (string-search (format "xlink:href='data:image/png;base64,%s'"
                                   (base64-encode-string lispm-desktop-test--png t))
                           svg))
    (should (string-search "opacity='1'" svg))
    ;; The badge: four characters at most, in its colours.
    (should (string-search ">1234</text>" svg))
    (should (string-search "fill='#123456'" svg))
    (should (string-search "opacity='0.45'"
                           (lispm-desktop-browser--draw-extension (plist-put (copy-sequence button) :enabled nil))))
    ;; Pushed in: the bevel's top edge is shaded, not lit.
    (let ((shade (lispm-desktop-platinum-color :shade)))
      (should-not (string-search (format "<rect width='24' height='1' fill='%s'/>" shade) svg))
      (should (string-search (format "<rect width='24' height='1' fill='%s'/>" shade)
                             (lispm-desktop-browser--draw-extension (plist-put (copy-sequence button) :open t)))))
    (should (string-search ">B</text>" (lispm-desktop-browser--draw-extension '(:id "b" :name "blocker" :enabled t))))
    (should (string-search "<path d='M4.5 8 H8.4" (lispm-desktop-browser--draw-puzzle nil)))))

(ert-deftest lispm-desktop-test-browser-extension-buttons-end-the-toolbar ()
  "The location toolbar ends with a button per extension lispm-browser puts
on the toolbar, then the Extensions button, each marked as the button it
is; the address field stops short of them."
  (let ((buttons '((:id "a" :name "Blocker" :enabled t) (:id "b" :name "Wallet" :enabled t)))
        (drawn nil))
    (cl-letf (((symbol-function 'display-graphic-p) #'always)
              ((symbol-function 'lispm-browser-extensions-supported-p) #'always)
              ((symbol-function 'lispm-browser-extensions-buttons) (lambda () buttons))
              ((symbol-function 'lispm-desktop-browser--image)
               (lambda (key _draw) (push key drawn) `(image :type svg :data ,(format "%S" key)))))
      (pcase-let ((`(,string . ,width) (lispm-desktop-browser--extension-buttons)))
        (should (= width (+ (* 2 (+ lispm-desktop-browser-extension-size lispm-desktop-browser-extension-gap))
                            lispm-desktop-browser-extension-size)))
        (should (equal (delq nil (mapcar (lambda (i) (get-text-property i 'lispm-browser-extensions-button string))
                                         (number-sequence 0 (1- (length string)))))
                       '("a" "b" menu)))
        (should (equal (mapcar #'car drawn) '(puzzle extension extension))))
      (with-temp-buffer
        (setq-local lispm-desktop-browser--address-start (copy-marker (point-min)))
        (insert "https://example.com/")
        (lispm-desktop-browser--draw-field)
        (let ((after (overlay-get lispm-desktop-browser--field 'after-string)))
          (should (equal (plist-get (cdr (get-text-property 0 'display after)) :align-to)
                         `(- right (,(+ 12 8 (+ (* 2 26) 24))))))
          (should (string-search " " (substring after 1))))))
    ;; Where nothing is drawn, the field reaches across.
    (cl-letf (((symbol-function 'lispm-desktop-browser--extension-buttons) #'ignore))
      (with-temp-buffer
        (setq-local lispm-desktop-browser--address-start (copy-marker (point-min)))
        (lispm-desktop-browser--draw-field)
        (should (equal (plist-get (cdr (get-text-property 0 'display (overlay-get lispm-desktop-browser--field 'after-string)))
                                  :align-to)
                       '(- right (12))))))))

(ert-deftest lispm-desktop-test-browser-extension-buttons-anchor ()
  "A popup pressed from the keyboard hangs under its button, found on the
location toolbar's line; the toolbar's menus open under their button,
flush with its right edge."
  (let ((asked nil)
        (shown nil))
    (cl-letf (((symbol-function 'lispm-desktop-browser--address-window) #'selected-window)
              ((symbol-function 'window-pixel-edges) (lambda (&rest _) '(0 100 800 200)))
              ((symbol-function 'window-inside-pixel-edges) (lambda (&rest _) '(0 180 800 200)))
              ((symbol-function 'lispm-browser-extensions-button-anchor)
               (lambda (&rest args) (setq asked args) 'anchor))
              ((symbol-function 'lispm-desktop-popup-menu)
               (lambda (items x y align) (push (list items x y align lispm-desktop-popup-parent) shown))))
      (should (eq (lispm-desktop-browser--extension-anchor "a") 'anchor))
      (should (equal asked (list (selected-window) (+ 80 3) 24 "a")))
      (lispm-desktop-browser--popup-at '(items) (list 'frame 700 83 24 24))
      (should (equal shown '(((items) 724 107 right frame)))))))

(ert-deftest lispm-desktop-test-browser-address-typing-replaces ()
  "Typing or pasting in the address field replaces the address Open
Location marks, though `delete-selection-mode' stays off: elsewhere text
typed still goes in beside the region, and other keys keep their ways."
  (let ((transient-mark-mode t)
        (delete-selection-mode nil)
        (kill-ring '("lispm.org"))
        (kill-ring-yank-pointer nil)
        (interprogram-cut-function nil)
        (interprogram-paste-function nil)
        (global (current-global-map))
        (buffer (lispm-desktop-browser--address-buffer)))
    (cl-letf (((symbol-function 'lispm-browser-shown-buffer) #'ignore)
              ((symbol-function 'lispm-desktop-browser--bookmarks-button) (lambda () "Bookmarks"))
              ((symbol-function 'lispm-desktop-platinum-icon) (lambda (_) "*"))
              ((symbol-function 'lispm-desktop-clipboard-text) (lambda () "example.com")))
      (unwind-protect
          (save-window-excursion
            ;; Command-V pastes, as `lispm-desktop-setup-keys' binds it.
            (use-global-map (define-keymap :parent global "M-v" #'lispm-desktop-paste))
            (switch-to-buffer buffer)
            (cl-flet ((type (keys)
                        (lispm-desktop-browser--render-address "https://duckduckgo.com/")
                        (lispm-desktop-browser-focus-address)
                        (execute-kbd-macro (kbd keys))
                        (lispm-desktop-browser--edited-address)))
              (should (equal (lispm-browser-input-url (type "github.com/LispMachineLab/lispm-desktop"))
                             "https://github.com/LispMachineLab/lispm-desktop"))
              (should (equal (type "C-y") "lispm.org"))
              (should (equal (type "M-v") "example.com"))
              ;; Moving and copying leave the address as it was.
              (should (equal (type "C-b M-w") "https://duckduckgo.com/"))
              (should (equal (car kill-ring) "https://duckduckgo.com"))
              ;; Once the mark is gone, text typed goes in at point.
              (should (equal (type "C-SPC C-SPC ?q=1") "https://duckduckgo.com/?q=1")))
            (with-temp-buffer
              (switch-to-buffer (current-buffer))
              (insert "abc")
              (push-mark (point-min) t t)
              (execute-kbd-macro "x")
              (should (equal (buffer-string) "abcx"))
              ;; Text pasted into a page leaves the buffer's region alone.
              (setq-local lispm-desktop-paste-function #'ignore)
              (should-not (lispm-desktop--paste-delete-selection))))
        (use-global-map global)
        (kill-buffer buffer)))))

(ert-deftest lispm-desktop-test-throbber-turns-while-loading ()
  "The throbber turns while the shown page loads and rests once it is done."
  (let ((lispm-desktop-browser--throbber-frame nil)
        (lispm-desktop-browser--throbber-timer nil)
        (loading t))
    (with-temp-buffer
      (setq-local xwidget-webkit--loading-p t)
      (cl-letf (((symbol-function 'lispm-browser-shown-buffer)
                 (let ((buffer (current-buffer)))
                   (lambda () (setq-local xwidget-webkit--loading-p loading) buffer))))
        (lispm-desktop-browser--turn-throbber)
        (lispm-desktop-browser--turn-throbber)
        (should (eql lispm-desktop-browser--throbber-frame 1))
        (setq loading nil)
        (lispm-desktop-browser--turn-throbber)
        (should-not lispm-desktop-browser--throbber-frame)))))

;;;; Settings

(ert-deftest lispm-desktop-test-settings-panels-draw ()
  "Each panel draws its settings: the themes, the desktop's picture and its
positions, the keys, the browser's pages."
  ;; Drawn as SVG: an Emacs without librsvg cannot.
  (skip-unless (image-type-available-p 'svg))
  (cl-letf (((symbol-function 'lispm-browser-available-p) #'always)
            ((symbol-function 'lispm-browser-history) (lambda () '(("u" "t" 0 1))))
            ((symbol-function 'lispm-browser-bookmarks) (lambda () nil)))
    (with-current-buffer (lispm-desktop-settings--buffer)
      (pcase-dolist (`(,panel . ,labels)
                     '((themes "Dracula" "Platinum")
                       (desktop "From the theme" "Fill" "Tile" "Arrange Icons")
                       (keyboard "Vim keys" "Emacs keys" "next start")
                       (browser "Home Page" "Clear History" "Edit Bookmarks")
                       (network "Proxy" "None" "HTTP" "SOCKS5" "Direct Connections")))
        (lispm-desktop-settings-select panel)
        (let ((text (concat (buffer-string)
                            ;; Push buttons draw their labels in their images.
                            (mapconcat #'identity
                                       (delq nil (mapcar (lambda (position)
                                                           (get-text-property position 'lispm-desktop-platinum-label))
                                                         (number-sequence (point-min) (1- (point-max)))))
                                       " "))))
          (dolist (label labels)
            (should (string-search label text))))
        (should (equal (nth 1 (assq panel lispm-desktop-settings-panels)) lispm-desktop-platinum-title))))))

(ert-deftest lispm-desktop-test-settings-radio-saves ()
  "Pressing a radio button saves its choice and draws the panel again."
  ;; Drawn as SVG: an Emacs without librsvg cannot.
  (skip-unless (image-type-available-p 'svg))
  (let ((saved nil)
        (lispm-desktop-editing-style 'vim))
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (variable value) (push (cons variable value) saved))))
      (with-current-buffer (lispm-desktop-settings--buffer)
        (lispm-desktop-settings-select 'keyboard)
        (let ((button (next-button (point-min))))
          (while (and button (not (string-search "Emacs keys" (button-label button))))
            (setq button (next-button (button-end button))))
          (should button)
          (button-activate button))
        (should (equal (car saved) '(lispm-desktop-editing-style . emacs)))))))

(ert-deftest lispm-desktop-test-settings-proxy-asks-for-its-server ()
  "Choosing a proxy without a server asks for one first, and keeps neither
when none is given; with one, the choice is saved."
  ;; Drawn as SVG: an Emacs without librsvg cannot.
  (skip-unless (image-type-available-p 'svg))
  (let ((saved nil)
        (answer "")
        (lispm-desktop-proxy-type nil)
        (lispm-desktop-proxy-server nil))
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (variable value) (push (cons variable value) saved) (set variable value)))
              ((symbol-function 'read-string)
               (lambda (&rest _) (if (functionp answer) (funcall answer) answer))))
      (with-current-buffer (lispm-desktop-settings--buffer)
        (lispm-desktop-settings-select 'network)
        (let ((press (lambda (label)
                       (let ((button (next-button (point-min))))
                         (while (and button (not (string-search label (button-label button))))
                           (setq button (next-button (button-end button))))
                         (button-activate button)))))
          (funcall press "SOCKS5")
          (should-not saved)
          (setq answer "nonsense")
          (should-error (funcall press "SOCKS5") :type 'user-error)
          (should-not saved)
          (setq answer "127.0.0.1:7890")
          (funcall press "SOCKS5")
          (should (equal (reverse saved) '((lispm-desktop-proxy-server . "127.0.0.1:7890")
                                           (lispm-desktop-proxy-type . socks5))))
          (setq answer (lambda () (error "Asked again")))
          (funcall press "HTTP")
          (should (equal (car saved) '(lispm-desktop-proxy-type . http))))))))

(ert-deftest lispm-desktop-test-control-panels-folder ()
  "The Control Panels folder is a Finder folder of a panel each: opening an
item opens its panel, and nothing is made, renamed or thrown away there."
  (let ((opened nil)
        (lispm-desktop-finder-folders nil))
    (setf (alist-get 'control-panels lispm-desktop-finder-folders)
          '(:title "Control Panels" :icon "control-panels" :items lispm-desktop-settings--folder-items))
    (cl-letf (((symbol-function 'lispm-desktop-settings-show) (lambda (panel) (setq opened panel)))
              ((symbol-function 'lispm-desktop-finder--render) #'ignore))
      (lispm-desktop-finder--go 'control-panels)
      (with-current-buffer (lispm-desktop-finder--buffer)
        (should (member "Appearance" (mapcar #'car lispm-desktop-finder--items)))
        (should (equal (lispm-desktop-finder--icon-name lispm-desktop-finder--directory '("Keyboard"))
                       "keyboard-32"))
        (should (equal (lispm-desktop-finder--title lispm-desktop-finder--directory) "Control Panels"))
        (should-not (lispm-desktop-finder-real-p))
        (should-not (lispm-desktop-finder--parent lispm-desktop-finder--directory))
        (setq lispm-desktop-finder--selected "Appearance"))
      (lispm-desktop-finder-open)
      (should (eq opened 'themes))
      (should-error (lispm-desktop-finder-new-folder "x") :type 'user-error)
      (should-error (lispm-desktop-finder-move-to-trash) :type 'user-error)
      (lispm-desktop-finder--go temporary-file-directory)
      (should (lispm-desktop-finder-real-p)))))

(ert-deftest lispm-desktop-test-settings-in-menus ()
  "The λ menu opens each panel; the Application menu lists Settings while
its window is open."
  (let ((settings (seq-find (lambda (item) (and (consp item) (equal (plist-get item :label) "Control Panels")))
                            (lispm-desktop-lambda-items))))
    (should settings)
    (should (member "Appearance" (mapcar (lambda (item) (plist-get item :label))
                                         (funcall (plist-get settings :submenu))))))
  (cl-letf (((symbol-function 'lispm-browser-available-p) #'ignore))
    (cl-letf (((symbol-function 'lispm-desktop-settings-open-p) #'ignore))
      (should-not (member "Control Panels" (mapcar (lambda (item) (plist-get item :label))
                                                   (lispm-desktop-apps-application-items)))))
    (cl-letf (((symbol-function 'lispm-desktop-settings-open-p) #'always))
      (should (member "Control Panels" (mapcar (lambda (item) (plist-get item :label))
                                               (lispm-desktop-apps-application-items)))))))

;;;; lispm-desktop-coherence

(defvar exwm-wm-mode)

(ert-deftest lispm-desktop-test-coherence-inside-fraction ()
  "How much of a window lies over the desktop decides where it belongs."
  (should (= 1.0 (lispm-desktop-coherence-inside-fraction '(10 10 100 100) '(0 0 1000 800))))
  (should (= 0.25 (lispm-desktop-coherence-inside-fraction '(950 0 200 100) '(0 0 1000 800))))
  (should (= 0.0 (lispm-desktop-coherence-inside-fraction '(1200 0 200 100) '(0 0 1000 800))))
  (should (= 0.0 (lispm-desktop-coherence-inside-fraction '(0 0 0 100) '(0 0 1000 800)))))

(defmacro lispm-desktop-test--with-drop (spec &rest body)
  "Run BODY with a window dropped as SPEC says: :child, a child of the
screen rather than out; :inside, its fraction over the screen; :hidden,
the screen hidden; :full, the screen full screen; :drag, windows going
out when dropped, t when left out.  `out' and `in' are bound to whether
the window was taken out or brought in."
  (declare (indent 1))
  `(let ((out nil) (in nil)
         (lispm-desktop-coherence-drag-out ,(if (plist-member spec :drag) (plist-get spec :drag) t)))
     (cl-letf (((symbol-function 'lispm-desktop-coherence-available-p) #'always)
               ((symbol-function 'frame-parameter)
                (lambda (_frame parameter) (eq parameter 'lispm-desktop-window)))
               ((symbol-function 'frame-parent) (lambda (_) (and ,(plist-get spec :child) 'screen)))
               ((symbol-function 'lispm-desktop-apps-window-screen) (lambda (_) 'screen))
               ((symbol-function 'lispm-desktop-apps-hidden-p) (lambda (&rest _) ,(plist-get spec :hidden)))
               ((symbol-function 'lispm-desktop-coherence--full-screen-p) (lambda (_) ,(plist-get spec :full)))
               ((symbol-function 'frame-live-p) #'always)
               ((symbol-function 'lispm-desktop-coherence--outer-rectangle) #'ignore)
               ((symbol-function 'lispm-desktop-coherence--screen-rectangle) #'ignore)
               ((symbol-function 'lispm-desktop-coherence-inside-fraction)
                (lambda (&rest _) ,(plist-get spec :inside)))
               ((symbol-function 'lispm-desktop-coherence-take-out) (lambda (_) (setq out t)))
               ((symbol-function 'lispm-desktop-coherence-bring-in) (lambda (_) (setq in t))))
       (lispm-desktop-coherence--dropped 'window)
       ,@body)))

(ert-deftest lispm-desktop-test-coherence-drop-out-and-in ()
  "A window dropped mostly past the desktop's edge goes out; one out dropped
mostly over the desktop shown comes in; nothing else moves a window."
  (lispm-desktop-test--with-drop (:child t :inside 0.2) (should out) (should-not in))
  (lispm-desktop-test--with-drop (:child t :inside 0.8) (should-not out) (should-not in))
  (lispm-desktop-test--with-drop (:inside 0.8) (should in) (should-not out))
  (lispm-desktop-test--with-drop (:inside 0.8 :hidden t) (should-not in) (should-not out))
  (lispm-desktop-test--with-drop (:inside 0.2) (should-not in) (should-not out)))

(ert-deftest lispm-desktop-test-coherence-drop-rules ()
  "Full screen, the desktop moves no window out or in; with dragging out
turned off, a window dropped past its edge stays, and one out still
comes back in."
  (lispm-desktop-test--with-drop (:child t :inside 0.2 :full t) (should-not out))
  (lispm-desktop-test--with-drop (:inside 0.8 :full t) (should-not in))
  (lispm-desktop-test--with-drop (:child t :inside 0.2 :drag nil) (should-not out))
  (lispm-desktop-test--with-drop (:inside 0.8 :drag nil) (should in)))

(ert-deftest lispm-desktop-test-coherence-unsupported ()
  "Windows go out on macOS only; elsewhere the desktop says why it cannot
hide, and Hide Desktop is not offered."
  (should (lispm-desktop-coherence-unsupported (selected-frame)))
  (let ((noninteractive nil))
    (cl-letf (((symbol-function 'display-graphic-p) #'always))
      (cl-letf (((symbol-function 'framep) (lambda (_) 'ns)))
        (should-not (lispm-desktop-coherence-unsupported 'screen)))
      (cl-letf (((symbol-function 'framep) (lambda (_) 'x)))
        (should (string-match-p "macOS" (lispm-desktop-coherence-unsupported 'screen)))
        (let ((exwm-wm-mode t))
          (ignore exwm-wm-mode)
          (should (string-match-p "manages" (lispm-desktop-coherence-unsupported 'screen))))))
    (cl-letf (((symbol-function 'display-graphic-p) #'ignore))
      (should (string-match-p "terminal" (lispm-desktop-coherence-unsupported 'screen)))))
  (should-error (lispm-desktop-hide-desktop) :type 'user-error)
  (cl-letf (((symbol-function 'lispm-browser-available-p) #'ignore))
    (should-not (member "Hide Desktop" (mapcar (lambda (item) (and (consp item) (plist-get item :label)))
                                               (lispm-desktop-apps-application-items))))))

(ert-deftest lispm-desktop-test-coherence-application-menu ()
  "The Application menu begins with Hide Desktop, or Show Desktop while the
desktop is hidden, where windows can go out."
  (let ((lispm-desktop-apps--frontmost 'desktop))
    (cl-letf (((symbol-function 'lispm-browser-available-p) #'ignore)
              ((symbol-function 'lispm-desktop-coherence-available-p) #'always))
      (cl-letf (((symbol-function 'lispm-desktop-apps-hidden-p) #'ignore))
        (let ((items (lispm-desktop-apps-application-items)))
          (should (equal (plist-get (car items) :label) "Hide Desktop"))
          (should (eq (cadr items) 'separator))))
      (cl-letf (((symbol-function 'lispm-desktop-apps-hidden-p) #'always))
        (should (equal (plist-get (car (lispm-desktop-apps-application-items)) :label) "Show Desktop"))))))

(ert-deftest lispm-desktop-test-coherence-geometry-counts-from-the-screen ()
  "A window out remembers its place over its screen, as one floating there:
it opens there again, a child of the screen."
  (cl-letf (((symbol-function 'frame-position) (lambda (_) '(500 . 300)))
            ((symbol-function 'frame-parent) #'ignore)
            ((symbol-function 'lispm-desktop-apps-out-p) (lambda (_) 'screen))
            ((symbol-function 'lispm-desktop-apps--screen-origin) (lambda (_) '(40 . 100)))
            ((symbol-function 'frame-text-width) (lambda (_) 700))
            ((symbol-function 'frame-text-height) (lambda (_) 400))
            ((symbol-function 'lispm-desktop-platinum-rolled-up-height) #'ignore))
    (should (equal (lispm-desktop-apps--geometry 'window) '(460 200 700 400)))))

(ert-deftest lispm-desktop-test-coherence-keep-on-screen ()
  "A window out keeps its title bar under the menu bar and part of it on the
computer's screen."
  (let ((moved nil))
    (cl-letf (((symbol-function 'lispm-desktop-apps--workarea) (lambda (_) '(0 25 1728 1000)))
              ((symbol-function 'lispm-desktop-platinum-window-outer) #'identity)
              ((symbol-function 'frame-position) (lambda (_) '(2000 . 5)))
              ((symbol-function 'frame-pixel-width) (lambda (_) 700))
              ((symbol-function 'set-frame-position) (lambda (_ x y) (setq moved (list x y)))))
      (lispm-desktop-apps--keep-on-screen 'window)
      (should (equal moved '(1648 25))))))

(ert-deftest lispm-desktop-test-coherence-window-out-keeps-its-frame ()
  "A window taken out goes out in its frame: the frame carrying it becomes a
frame of its own where it was, the window still standing in it, framed;
brought back in, the frame is a child of the screen again where it is."
  (lispm-desktop-test--with-frames lispm-desktop-test--window-frames
      (cl-letf (((symbol-function 'lispm-desktop-coherence--minibuffer) (lambda (_) 'minibuffer))
                ((symbol-function 'minibuffer-window) (lambda (&optional frame) (list 'mini frame)))
                ((symbol-function 'lispm-desktop-apps--screen-origin) (lambda (_) '(40 . 100)))
                ((symbol-function 'lispm-desktop-apps--keep-on-screen) #'ignore)
                ((symbol-function 'lispm-desktop-apps-keep-on-desktop) #'ignore))
        (lispm-desktop-coherence-take-out 'window)
        (should-not (frame-parent 'outer))
        (should (eq (frame-parent 'window) 'outer))
        (should (lispm-desktop-platinum--framed-p 'window))
        (should (equal (frame-position 'outer) '(134 . 149)))
        (should (equal (frame-position 'window) '(6 . 1)))
        (should (eq (lispm-desktop-apps-out-p 'window) 'screen))
        (should (eq (lispm-desktop-apps-out-p 'outer) 'screen))
        (should-not (lispm-desktop-apps-screen-p 'outer))
        (should (eq (lispm-desktop-apps-window-screen 'window) 'screen))
        (should-not (lispm-desktop-platinum-window-parent 'window))
        (should (equal (frame-parameter 'window 'minibuffer) '(mini minibuffer)))
        (should (equal (frame-parameter 'outer 'minibuffer) '(mini minibuffer)))
        (should (equal (lispm-desktop-apps-windows-out-in-order 'screen) '(window)))
        (lispm-desktop-coherence-bring-in 'window)
        (should (eq (frame-parent 'outer) 'screen))
        (should (eq (frame-parent 'window) 'outer))
        (should (equal (frame-position 'outer) '(94 . 49)))
        (should-not (lispm-desktop-apps-out-p 'window))
        (should-not (lispm-desktop-apps-out-p 'outer))
        (should (equal (frame-parameter 'window 'minibuffer) '(mini screen))))))

(ert-deftest lispm-desktop-test-coherence-screen-of-a-window-out ()
  "The screen is the desktop's frame, also while a window out is selected."
  (cl-letf (((symbol-function 'selected-frame) (lambda () 'window))
            ((symbol-function 'frame-parent) #'ignore)
            ((symbol-function 'frame-parameter) (lambda (_ parameter) (and (eq parameter 'lispm-desktop-out) 'screen)))
            ((symbol-function 'frame-live-p) #'always))
    (should (eq (lispm-desktop-apps-screen) 'screen))))

;;;; lispm-desktop-term

(defmacro lispm-desktop-test--with-term (bindings &rest body)
  "Run BODY with BINDINGS of terminal buffers made by `lispm-desktop-term-make', each
(VAR NAME PROGRAM ARGS), killed afterwards; skipped where the module is
not built."
  (declare (indent 1))
  `(progn
     (skip-unless (lispm-desktop-term-ready-p))
     (let ,(mapcar (pcase-lambda (`(,var ,name ,program ,args))
                     `(,var (lispm-desktop-term-make ,name ,program ,args temporary-file-directory)))
                   bindings)
       (unwind-protect (progn ,@body)
         ,@(mapcar (lambda (b) `(let ((kill-buffer-query-functions nil)) (kill-buffer ,(car b))))
                   bindings)))))

(defun lispm-desktop-test--term-wait (buffer &optional seconds)
  "Wait until BUFFER's terminal program exited, at most SECONDS."
  (let ((deadline (+ (float-time) (or seconds 5))))
    (while (and (null (buffer-local-value 'lispm-desktop-term--exit-code buffer))
                (< (float-time) deadline))
      (accept-process-output nil 0.02)
      (lispm-desktop-term--tick))))

(ert-deftest lispm-desktop-test-terminals-are-one-family ()
  "A terminal is a live buffer of Eat or of lispm-desktop-term; what is laid
out as one takes in agents' conversations too."
  (let ((buffers (mapcar (lambda (mode)
                           (with-current-buffer (generate-new-buffer " lispm-desktop-test terminal")
                             (setq major-mode mode)
                             (current-buffer)))
                         '(eat-mode lispm-desktop-term-mode fundamental-mode))))
    (unwind-protect
        (progn
          (should (equal (mapcar #'lispm-desktop-terminal-p buffers) '(t t nil)))
          (with-current-buffer (car buffers)
            (should (lispm-desktop-terminal-p)))
          (should (lispm-desktop-terminal-buffer-p (nth 1 buffers)))
          (with-current-buffer (nth 2 buffers)
            (setq lispm-desktop-conversation-buffer t))
          (should-not (lispm-desktop-terminal-p (nth 2 buffers)))
          (should (lispm-desktop-terminal-buffer-p (nth 2 buffers))))
      (mapc #'kill-buffer buffers))
    (should-not (lispm-desktop-terminal-p (car buffers)))))

(ert-deftest lispm-desktop-test-terminals-lay-out-alike ()
  "Eat's terminals and lispm-desktop-term's are laid out by one function:
lines unwrapped and left to right, no line highlight, output no edit, and
symbols too wide for a cell shown as look-alikes."
  (let ((emulation-mode-map-alists emulation-mode-map-alists))
    (dolist (setup (list #'lispm-desktop-terminal-display #'lispm-desktop-term-mode))
      (with-temp-buffer
        (visual-line-mode 1)
        (add-hook 'first-change-hook #'ignore nil t)
        (funcall setup)
        (should truncate-lines)
        (should-not visual-line-mode)
        (should-not bidi-display-reordering)
        (should (eq bidi-paragraph-direction 'left-to-right))
        (should bidi-inhibit-bpa)
        (should (local-variable-p 'global-hl-line-mode))
        (should-not first-change-hook)
        (should (equal (aref buffer-display-table ?⏺) [?●]))))))

(ert-deftest lispm-desktop-test-clipboard-keys-everywhere ()
  "Command-C and Command-V copy and paste wherever they are bound, M-x
stays Emacs's, and in a terminal M-w and C-y copy and paste too."
  (let ((map (make-sparse-keymap)))
    (lispm-desktop-bind-clipboard-keys map)
    (should (eq (keymap-lookup map "M-c") #'lispm-desktop-copy-region))
    (should (eq (keymap-lookup map "M-v") #'lispm-desktop-paste))
    (should (eq (keymap-lookup map "M-x") #'execute-extended-command))
    (should-not (keymap-lookup map "C-y")))
  (dolist (map (list lispm-desktop-eat-keys-mode-map lispm-desktop-term-keys-mode-map))
    (pcase-dolist (`(,key . ,command) '(("M-c" . lispm-desktop-copy-region)
                                        ("M-v" . lispm-desktop-paste)
                                        ("M-w" . lispm-desktop-copy-region)
                                        ("C-y" . lispm-desktop-paste)
                                        ("M-x" . execute-extended-command)))
      (should (eq (keymap-lookup map key) command)))))

(ert-deftest lispm-desktop-test-interface-sets-up-only-itself ()
  "The interface's setup leaves the browser and the languages to their own
setups."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "lisp/lispm-desktop-interface.el" lispm-desktop-directory))
    (should-not (re-search-forward "lispm-\\(?:desktop-\\)?browser\\|c-basic-offset\\|js-indent-level" nil t))))

(ert-deftest lispm-desktop-test-term-draws-colours-and-input ()
  "A terminal draws its program's text with the cells' colours, takes what
is typed, keeps its last screen when the program exits, and reports the
exit code."
  (lispm-desktop-test--with-term ((term "lispm-desktop-test-term" "/bin/sh"
                                        '("-c" "printf 'a\\033[1;32mgreen\\033[0mb\\n'; read line; echo \"got:$line\"; exit 3")))
    (with-current-buffer term
      (accept-process-output nil 0.2)
      (lispm-desktop-term-send-string "hello\r")
      (lispm-desktop-test--term-wait term)
      (goto-char (point-min))
      (should (search-forward "agreenb" nil t))
      (should (equal (get-text-property (+ (point-min) 1) 'face)
                     '(:foreground "#00ff00" :weight bold)))
      (should (search-forward "got:hello" nil t))
      (should (search-forward "[Process exited with code 3]" nil t))
      (should (eql lispm-desktop-term--exit-code 3))
      (should-not (lispm-desktop-term-live-p)))))

(ert-deftest lispm-desktop-test-term-environment-is-emacs-own ()
  "The program sees `process-environment', not the environment Emacs was
started with."
  (let ((process-environment (cons "LISPM_DESKTOP_TEST_VALUE=from-lisp"
                                   (seq-remove (lambda (e) (string-prefix-p "HOME=" e))
                                               process-environment))))
    (lispm-desktop-test--with-term ((term "lispm-desktop-test-env" "/bin/sh"
                                          '("-c" "echo \"[$LISPM_DESKTOP_TEST_VALUE][${HOME:-none}][$TERM]\"")))
      (lispm-desktop-test--term-wait term)
      (with-current-buffer term
        (goto-char (point-min))
        (should (search-forward "[from-lisp][none][xterm-256color]" nil t))))))

(ert-deftest lispm-desktop-test-term-hidden-terminal-does-not-wake-emacs ()
  "A terminal no window shows never wakes Emacs for its output, only for
its exit: its program's writing costs Emacs's thread nothing."
  (skip-unless (lispm-desktop-term-ready-p))
  (let ((woken nil))
    (cl-letf (((symbol-function 'lispm-desktop-term--schedule) #'ignore))
      (setq lispm-desktop-term--pending nil)
      (let ((term (lispm-desktop-term-make "lispm-desktop-test-hidden" "/bin/sh"
                                           '("-c" "for i in 1 2 3 4 5; do echo line $i; sleep 0.05; done; sleep 0.3")
                                           temporary-file-directory)))
        (unwind-protect
            (with-current-buffer term
              (let ((deadline (+ (float-time) 0.25)))
                (while (< (float-time) deadline)
                  (accept-process-output nil 0.02)))
              (setq woken (memq lispm-desktop-term--id lispm-desktop-term--pending))
              (should-not woken)
              (let ((deadline (+ (float-time) 3)))
                (while (and (not (memq lispm-desktop-term--id lispm-desktop-term--pending)) (< (float-time) deadline))
                  (accept-process-output nil 0.02)))
              (should (memq lispm-desktop-term--id lispm-desktop-term--pending))
              (should (string-match-p "line 5" (lispm-desktop-term-module-text lispm-desktop-term--terminal nil))))
          (let ((kill-buffer-query-functions nil)) (kill-buffer term)))))))

(ert-deftest lispm-desktop-test-term-resizes ()
  "Resizing a terminal tells its program the new size."
  (lispm-desktop-test--with-term ((term "lispm-desktop-test-size" "/bin/sh" '("-c" "sleep 0.3; stty size")))
    (with-current-buffer term
      (lispm-desktop-term-module-resize lispm-desktop-term--terminal 57 13)
      (lispm-desktop-test--term-wait term)
      (should (string-match-p "13 57" (lispm-desktop-term-module-text lispm-desktop-term--terminal nil))))))

(ert-deftest lispm-desktop-test-term-starts-at-selected-window-size ()
  "A terminal no window shows yet starts the size of the selected window,
so the program sees no resize when the terminal is shown there."
  (let ((size (lispm-desktop-term--window-size (selected-window))))
    (lispm-desktop-test--with-term ((term "lispm-desktop-test-start-size" "/bin/sh" '("-c" "stty size")))
      (lispm-desktop-test--term-wait term)
      (with-current-buffer term
        (should (equal lispm-desktop-term--size size))
        (should (string-match-p (format "%d %d" (cdr size) (car size))
                                (lispm-desktop-term-module-text lispm-desktop-term--terminal nil)))))))

(ert-deftest lispm-desktop-test-term-kill-running-program ()
  "Killing a terminal whose program still runs hangs the program up, and
Emacs keeps running when it exits.  Its exit once raised SIGPIPE, which
ends a batch Emacs such as the one running these tests."
  (lispm-desktop-test--with-term ((term "lispm-desktop-test-kill" "sleep" '("30")))
    (let ((pid (with-current-buffer term (lispm-desktop-term-module-pid lispm-desktop-term--terminal)))
          (deadline (+ (float-time) 5)))
      (accept-process-output nil 0.2)
      (should (zerop (signal-process pid 0)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer term))
      ;; Once the program is reaped, the module hears of its exit.
      (while (and (zerop (signal-process pid 0)) (< (float-time) deadline))
        (accept-process-output nil 0.02)
        (lispm-desktop-term--tick))
      (should-not (zerop (signal-process pid 0)))
      (dotimes (_ 10)
        (accept-process-output nil 0.02)
        (lispm-desktop-term--tick)))))

(ert-deftest lispm-desktop-test-term-channel-closes-and-reopens ()
  "The module stops writing to its channel before Emacs exits and closes
it, as a write to a closed pipe raises SIGPIPE; a channel made after that
wakes Emacs instead."
  (skip-unless (lispm-desktop-term-ready-p))
  (should (memq #'lispm-desktop-term--close-channel kill-emacs-hook))
  (let ((old lispm-desktop-term--channel))
    (lispm-desktop-term--close-channel)
    (delete-process old)
    (lispm-desktop-test--with-term ((term "lispm-desktop-test-channel" "/bin/sh" '("-c" "exit 4")))
      (should-not (eq lispm-desktop-term--channel old))
      (lispm-desktop-test--term-wait term)
      (should (eql (buffer-local-value 'lispm-desktop-term--exit-code term) 4)))))

(ert-deftest lispm-desktop-test-agent-list-follows-the-agents ()
  "The agents sidebar draws a session the user acted on at once, and what
the agents changed themselves once Emacs is idle, and only when it is up."
  (dlet ((lispm-desktop-agents-changed-hook nil)
         (lispm-desktop-agent-sessions-changed-hook nil)
         (eat-exit-hook nil)
         (lispm-desktop-term-exit-functions nil)
         (lispm-desktop-term-status-functions nil))
    (let ((lispm-desktop-agent-list--refresh-timer nil)
          (refreshed 0)
          (scheduled 0))
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (&rest _) (setq scheduled (1+ scheduled)) 'timer))
                ((symbol-function 'lispm-desktop-agent-list-refresh)
                 (lambda () (setq refreshed (1+ refreshed)))))
        (lispm-desktop-setup-agent-list)
        (run-hooks 'lispm-desktop-agent-sessions-changed-hook)
        (should (= refreshed 1))
        (should-not (get-buffer lispm-desktop-agent-list-buffer-name))
        (run-hooks 'lispm-desktop-agents-changed-hook)
        (should (= scheduled 0))
        (with-current-buffer (get-buffer-create lispm-desktop-agent-list-buffer-name)
          (unwind-protect
              (progn
                (run-hooks 'lispm-desktop-agents-changed-hook)
                (should (= scheduled 1)))
            (kill-buffer)))
        (should (= refreshed 1))))))

(ert-deftest lispm-desktop-test-agent-list-refreshes-after-agents-only ()
  "Only an agent terminal's exit refreshes the agent sidebar, and only once
Emacs is idle: reading transcripts must not stall the terminal scheduler."
  (let ((lispm-desktop-agent-list--refresh-timer nil)
        (scheduled 0))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (&rest _) (setq scheduled (1+ scheduled)) 'timer))
              ((symbol-function 'lispm-desktop-agent-list-refresh)
               (lambda () (error "Refreshed synchronously"))))
      (with-temp-buffer
        (lispm-desktop-agent-list-refresh-on-exit (current-buffer) 0)
        (should (= scheduled 0)))
      (with-current-buffer (get-buffer-create "*claude:lispm-desktop-test*")
        (unwind-protect
            (lispm-desktop-test--with-agents '((claude :name "Claude Code" :program "emacs"))
              (setq major-mode 'lispm-desktop-term-mode)
              (lispm-desktop-agent-list-refresh-on-exit (current-buffer) 0)
              (lispm-desktop-agent-list-refresh-on-exit (current-buffer) 0)
              (should (= scheduled 1)))
          (kill-buffer))))))

;;;;; Program status (OSC 7501)

(defun lispm-desktop-test--term-until (buffer predicate &optional seconds)
  "Run the terminal scheduler until PREDICATE holds in BUFFER, at most
SECONDS; return PREDICATE's value."
  (let ((deadline (+ (float-time) (or seconds 5)))
        (value nil))
    (while (and (not (setq value (with-current-buffer buffer (funcall predicate))))
                (< (float-time) deadline))
      (accept-process-output nil 0.02)
      (lispm-desktop-term--tick))
    value))

(ert-deftest lispm-desktop-test-term-program-status ()
  "What a program reports of itself (OSC 7501) reaches its terminal's
buffer; what it finished is dismissed once the user types, and what it
was still doing ends with it."
  (lispm-desktop-test--with-term
      ((term "lispm-desktop-test-status" "/bin/sh"
             (list "-c" (concat "printf '\\033]7501;state=working:id=build:progress=40:app=make\\007'; "
                                "printf '\\033]7501;state=blocked:kind=permission:msg="
                                (base64-encode-string "Run tests?") "\\033\\\\'; "
                                "read line; printf '\\033]7501;state=done\\007'; read line; read line"))))
    (should (lispm-desktop-test--term-until
             term (lambda () (eq (plist-get (lispm-desktop-term-status) :state) 'blocked))))
    (with-current-buffer term
      (should (equal lispm-desktop-term-status-records
                     '((:id nil :state blocked :kind permission :progress nil :app nil :title nil
                            :msg "Run tests?")
                       (:id "build" :state working :kind nil :progress 40 :app "make" :title nil
                            :msg nil))))
      (should (equal (lispm-desktop-term-status-label (lispm-desktop-term-status)) "needs approval"))
      (should (memq term (lispm-desktop-term-waiting-buffers)))
      (lispm-desktop-term-send-string "\r"))
    (should (lispm-desktop-test--term-until
             term (lambda () (eq (plist-get (car lispm-desktop-term-status-records) :state) 'done))))
    ;; A task still working outweighs the program's own done.
    (should (eq (plist-get (lispm-desktop-term-status term) :state) 'working))
    (with-current-buffer term (lispm-desktop-term-send-string "\r"))
    (should (lispm-desktop-test--term-until
             term (lambda () (equal (mapcar (lambda (r) (plist-get r :id)) lispm-desktop-term-status-records)
                                    '("build")))))
    (should-not (memq term (lispm-desktop-term-waiting-buffers)))
    (with-current-buffer term (lispm-desktop-term-send-string "\r"))
    (lispm-desktop-test--term-wait term)
    (should-not (buffer-local-value 'lispm-desktop-term-status-records term))))

(ert-deftest lispm-desktop-test-term-program-status-query ()
  "A program asking whether the terminal takes status reports is answered,
before the answer to what it asks next."
  (lispm-desktop-test--with-term
      ((term "lispm-desktop-test-status-query" "/bin/sh"
             '("-c" "stty -icanon -echo min 0 time 10; printf '\\033]7501;?\\033\\\\\\033[c'; sleep 0.3; reply=$(dd bs=256 count=1 2>/dev/null); printf '%s\\n' \"$reply\" | tr '\\033' E")))
    (lispm-desktop-test--term-wait term)
    ;; Joined: the answers are longer than the terminal is wide.
    (let ((text (string-replace "\n" "" (with-current-buffer term
                                          (lispm-desktop-term-module-text lispm-desktop-term--terminal nil)))))
      (should (string-match-p
               (regexp-quote "E]7501;?:states=idle,working,done,blocked,error:kinds=permission,question,authE\\E[?")
               text)))))

(ert-deftest lispm-desktop-test-term-colour-scheme ()
  "A program asking whether the background is dark (DSR 996) is answered,
and one that asked to hear of it (DEC mode 2031) is told when the colours
Emacs gives turn the background from light to dark, and only then."
  (lispm-desktop-test--with-term
      ((term "lispm-desktop-test-scheme" "/bin/sh"
             '("-c" "read go; stty -icanon -echo min 0 time 30; printf '\\033[?996n\\033[?2031h'; a=$(dd bs=64 count=1 2>/dev/null); echo ready; b=$(dd bs=64 count=1 2>/dev/null); printf '[%s][%s]\\n' \"$a\" \"$b\" | tr '\\033' E")))
    (with-current-buffer term
      (let ((colours (lambda (background)
                       (lispm-desktop-term-module-set-colors
                        lispm-desktop-term--terminal
                        (vconcat (make-vector 16 "#808080") (list "#000000" background)))))
            (text (lambda ()
                    (string-replace "\n" "" (lispm-desktop-term-module-text lispm-desktop-term--terminal nil)))))
        (funcall colours "#ffffff")
        (lispm-desktop-term-send-string "\r")
        (should (lispm-desktop-test--term-until term (lambda () (string-match-p "ready" (funcall text)))))
        (funcall colours "#e8e8e8")
        (funcall colours "#1e1e1e")
        (lispm-desktop-test--term-wait term)
        (should (string-match-p (regexp-quote "[E[?997;2n][E[?997;1n]") (funcall text)))))))

(ert-deftest lispm-desktop-test-term-status-picks-what-matters ()
  "A terminal's status is the record that matters most to the user, and
its label says what the program waits for."
  (with-temp-buffer
    (setq lispm-desktop-term-status-records
          '((:id nil :state working :progress 30)
            (:id "a" :state done)
            (:id "b" :state blocked :kind question :msg "Which branch?")))
    (let ((record (lispm-desktop-term-status)))
      (should (equal (plist-get record :id) "b"))
      (should (equal (lispm-desktop-term-status-label record) "has a question"))
      (should (equal (get-text-property 0 'help-echo (lispm-desktop-term-status-label record))
                     "Which branch?")))
    (setq lispm-desktop-term-status-records '((:id nil :state working :progress 30) (:id "a" :state done)))
    (should (equal (lispm-desktop-term-status-label (lispm-desktop-term-status)) "working 30%"))
    (should (equal (lispm-desktop-term-status-label (lispm-desktop-term-status) t) "working"))
    (setq lispm-desktop-term-status-records nil)
    (should-not (lispm-desktop-term-status))))

(ert-deftest lispm-desktop-test-agent-next-waiting ()
  "\\`C-c a n' goes to the terminals waiting for the user, those blocked
first, then those failed or done, and round again."
  (should (eq (keymap-lookup lispm-desktop-agent-map "n") #'lispm-desktop-agent-next-waiting))
  (should-error (lispm-desktop-agent-next-waiting) :type 'user-error)
  (let ((buffers nil)
        (shown nil))
    (unwind-protect
        (progn
          (pcase-dolist (`(,name . ,state) '(("busy" . working) ("done" . done) ("asks" . blocked) ("quiet")))
            (with-current-buffer (generate-new-buffer (format " *lispm-desktop-test-%s*" name))
              (setq lispm-desktop-term--terminal 'fake)
              (when state
                (setq lispm-desktop-term-status-records (list (list :state state))))
              (push (current-buffer) buffers)))
          (setq buffers (nreverse buffers))
          (should (equal (lispm-desktop-term-waiting-buffers) (list (nth 2 buffers) (nth 1 buffers))))
          (cl-letf (((symbol-function 'lispm-desktop-agent-show) (lambda (buffer) (push buffer shown)))
                    ((symbol-function 'lispm-desktop-sidebar-show-in-main) (lambda (buffer) (push buffer shown))))
            (lispm-desktop-agent-next-waiting)
            (should (eq (car shown) (nth 2 buffers)))
            (with-current-buffer (nth 2 buffers) (lispm-desktop-agent-next-waiting))
            (should (eq (car shown) (nth 1 buffers)))
            (with-current-buffer (nth 1 buffers) (lispm-desktop-agent-next-waiting))
            (should (eq (car shown) (nth 2 buffers)))))
      (mapc #'kill-buffer buffers))))

(ert-deftest lispm-desktop-test-agent-list-refreshes-on-status-changes ()
  "An agent terminal's status refreshes the agent sidebar, once Emacs is
idle, when its label there changes; progress alone does not."
  (let ((lispm-desktop-agent-list--refresh-timer nil)
        (scheduled 0))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (&rest _) (setq scheduled (1+ scheduled)) nil)))
      (with-current-buffer (get-buffer-create "*codex:lispm-desktop-test*")
        (unwind-protect
            (lispm-desktop-test--with-agents '((codex :name "Codex" :program "emacs"))
              (setq major-mode 'lispm-desktop-term-mode)
              (setq lispm-desktop-term-status-records '((:state working :progress 10)))
              (lispm-desktop-agent-list-refresh-on-status (current-buffer))
              (setq lispm-desktop-term-status-records '((:state working :progress 20)))
              (lispm-desktop-agent-list-refresh-on-status (current-buffer))
              (should (= scheduled 1))
              (should (equal (lispm-desktop-agent-list--running-label (current-buffer)) "working"))
              (setq lispm-desktop-term-status-records '((:state blocked :kind permission)))
              (lispm-desktop-agent-list-refresh-on-status (current-buffer))
              (should (= scheduled 2))
              (should (equal (lispm-desktop-agent-list--running-label (current-buffer)) "needs approval"))
              (setq lispm-desktop-term-status-records nil)
              (should (equal (lispm-desktop-agent-list--running-label (current-buffer)) "running")))
          (kill-buffer))))))

(ert-deftest lispm-desktop-test-title-bar-shows-program-status ()
  "A terminal's title bar shows what its program says it is doing."
  ;; Drawn as SVG: an Emacs without librsvg cannot.
  (skip-unless (image-type-available-p 'svg))
  (let* ((lispm-desktop-platinum-current-palette (lispm-desktop-platinum-derive-palette "#FFFFFF" "#000000"
                                                                                  "#777777" "#6363CE"))
         (bar (lispm-desktop-platinum-draw-title-bar 600 t nil "codex" nil '(on on nil) nil
                                                      (propertize "needs approval" 'face '(:foreground "#CE3131"))))
         (title (seq-find (lambda (position)
                            (equal (get-text-property position 'keymap bar) lispm-desktop-platinum--title-map))
                          (number-sequence 0 (1- (length bar)))))
         (data (plist-get (cdr (get-text-property title 'display bar)) :data))
         (svg (with-temp-buffer (insert data) (libxml-parse-xml-region (point-min) (point-max))))
         (texts (dom-by-tag svg 'text)))
    ;; The title is drawn in the bar, the status after it in its own colour.
    (should (seq-find (lambda (text) (string-search "codex" (dom-text text))) texts))
    (let ((status (seq-find (lambda (text) (string-search "needs approval" (dom-text text))) texts)))
      (should status)
      (should (equal (dom-attr status 'fill) "#CE3131")))))

(ert-deftest lispm-desktop-test-term-module-stale-after-source-change ()
  "The module counts as stale once a source is newer than its library."
  (let* ((root (make-temp-file "lispm-desktop-term-src" t))
         (lispm-desktop-term-source-directory root)
         (library (expand-file-name "target/release/liblispm_desktop_term.dylib" root))
         (source (expand-file-name "src/lib.rs" root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory library) t)
          (make-directory (file-name-directory source) t)
          (dolist (file (list source (expand-file-name "Cargo.toml" root)
                              (expand-file-name "Cargo.lock" root)))
            (write-region "" nil file)
            (set-file-times file (time-subtract nil 60)))
          (write-region "" nil library)
          (should (equal (lispm-desktop-term--module-file) library))
          (should-not (lispm-desktop-term--stale-p))
          (set-file-times source (time-add nil 60))
          (should (lispm-desktop-term--stale-p)))
      (delete-directory root t))))

(defmacro lispm-desktop-test--with-term-sources (&rest body)
  "Run BODY with the module's sources in a fresh directory, a minute old,
`root' bound to it, and nothing built, downloaded or loaded: the module
counts as not loaded, even after a test loaded the one built here."
  (declare (indent 0))
  `(let* ((root (make-temp-file "lispm-desktop-term-src" t))
          (lispm-desktop-term-source-directory root)
          (lispm-desktop-term--building nil)
          (lispm-desktop-term--downloading nil)
          (lispm-desktop-term--load-failure nil)
          (featurep (symbol-function 'featurep)))
     (unwind-protect
         (cl-letf (((symbol-function 'featurep)
                    (lambda (feature &rest subfeatures)
                      (and (not (eq feature 'lispm-desktop-term-module))
                           (apply featurep feature subfeatures)))))
           (dolist (name '("Cargo.toml" "Cargo.lock" "src/lib.rs" "src/pty.rs"))
             (let ((file (expand-file-name name root)))
               (make-directory (file-name-directory file) t)
               (write-region name nil file)
               (set-file-times file (time-subtract nil 60))))
           ,@body)
       (delete-directory root t))))

(defun lispm-desktop-test--term-file (file &optional age)
  "Write FILE, AGE seconds old (by default new), and return it."
  (make-directory (file-name-directory file) t)
  (write-region "library" nil file)
  (set-file-times file (time-subtract nil (or age 0)))
  file)

(ert-deftest lispm-desktop-test-term-target ()
  "A prebuilt module is chosen by the processor and system Emacs was built
for, and none is published for the others."
  (should (equal (lispm-desktop-term--target "aarch64-apple-darwin24.1.0") "aarch64-apple-darwin"))
  (should (equal (lispm-desktop-term--target "arm64-apple-darwin23.0.0") "aarch64-apple-darwin"))
  (should (equal (lispm-desktop-term--target "x86_64-apple-darwin21.6.0") "x86_64-apple-darwin"))
  (should (equal (lispm-desktop-term--target "x86_64-pc-linux-gnu") "x86_64-unknown-linux-gnu"))
  (should (equal (lispm-desktop-term--target "x86_64-unknown-linux-gnu") "x86_64-unknown-linux-gnu"))
  (should-not (lispm-desktop-term--target "aarch64-unknown-linux-gnu"))
  (should-not (lispm-desktop-term--target "x86_64-pc-linux-musl"))
  (should-not (lispm-desktop-term--target "x86_64-w64-mingw32"))
  (should (equal (lispm-desktop-term--asset "aarch64-apple-darwin")
                 "liblispm_desktop_term-aarch64-apple-darwin.dylib"))
  (should (equal (lispm-desktop-term--asset "x86_64-unknown-linux-gnu")
                 "liblispm_desktop_term-x86_64-unknown-linux-gnu.so")))

(ert-deftest lispm-desktop-test-term-source-version ()
  "The sources' version follows their contents, not their times: it is the
digest of the lines sha256sum writes for them."
  (lispm-desktop-test--with-term-sources
    (let ((version (lispm-desktop-term-source-version))
          (lib (expand-file-name "src/lib.rs" root)))
      (should (string-match-p "\\`[0-9a-f]\\{16\\}\\'" version))
      (should (equal (lispm-desktop-term-source-digest)
                     (secure-hash 'sha256
                                  (mapconcat (lambda (name)
                                               (format "%s  %s\n" (secure-hash 'sha256 name) name))
                                             '("Cargo.lock" "Cargo.toml" "src/lib.rs" "src/pty.rs") ""))))
      (set-file-times lib)
      (should (equal (lispm-desktop-term-source-version) version))
      (write-region "changed" nil lib)
      (should-not (equal (lispm-desktop-term-source-version) version))
      (write-region "src/lib.rs" nil lib)
      (should (equal (lispm-desktop-term-source-version) version))
      ;; A new file counts, in a directory of its own too.
      (lispm-desktop-test--term-file (expand-file-name "src/pty/keys.rs" root))
      (should-not (equal (lispm-desktop-term-source-version) version)))))

(ert-deftest lispm-desktop-test-term-plan-without-cargo ()
  "Without Cargo, the module built for the sources is downloaded unless a
library current for them is there; one downloaded stays current until the
sources change, whatever their times."
  (lispm-desktop-test--with-term-sources
    (cl-letf (((symbol-function 'executable-find) (lambda (program &rest _) (equal program "curl"))))
      (let ((system-configuration "aarch64-apple-darwin24.1.0")
            (lispm-desktop-term-download-enabled t))
        (should (eq (lispm-desktop-term--plan) 'download))
        (should-not (lispm-desktop-term--module-file))
        (let ((lispm-desktop-term-download-enabled nil))
          (should-not (lispm-desktop-term--plan)))
        (let ((system-configuration "aarch64-unknown-linux-gnu"))
          (should-not (lispm-desktop-term--plan)))
        (cl-letf (((symbol-function 'lispm-desktop-needs-command-line-tools-p) #'always)
                  ((symbol-function 'lispm-desktop-app-ui-needs) #'ignore)
                  ((symbol-function 'executable-find) #'ignore))
          (should-not (lispm-desktop-term--plan)))
        (let ((prebuilt (lispm-desktop-test--term-file (lispm-desktop-term--prebuilt-file) 120)))
          (should (string-prefix-p (expand-file-name "prebuilt/" root) prebuilt))
          (should (equal (lispm-desktop-term--current-file) prebuilt))
          (should (equal (lispm-desktop-term--module-file) prebuilt))
          (should-not (lispm-desktop-term--plan))
          ;; Sources newer than the library, as a checkout leaves them.
          (set-file-times (expand-file-name "src/lib.rs" root))
          (should (equal (lispm-desktop-term--current-file) prebuilt))
          (should-not (lispm-desktop-term--plan))
          ;; Changed sources want the library built from them; until it
          ;; is there, the old one is still loaded.
          (write-region "changed" nil (expand-file-name "src/lib.rs" root))
          (should-not (lispm-desktop-term--current-file))
          (should (eq (lispm-desktop-term--plan) 'download))
          (should (equal (lispm-desktop-term--module-file) prebuilt))
          ;; A library Cargo built from the sources is current, so nothing
          ;; is downloaded; once out of date, a current download wins.
          (let ((built (lispm-desktop-test--term-file
                        (expand-file-name "target/release/liblispm_desktop_term.dylib" root))))
            (should (equal (lispm-desktop-term--module-file) built))
            (should-not (lispm-desktop-term--plan))
            (set-file-times built (time-subtract nil 3600))
            (should (eq (lispm-desktop-term--plan) 'download))
            (let ((current (lispm-desktop-test--term-file (lispm-desktop-term--prebuilt-file) 7200)))
              (should (equal (lispm-desktop-term--module-file) current))
              (should-not (lispm-desktop-term--plan)))))))))

(ert-deftest lispm-desktop-test-term-plan-with-cargo ()
  "Where Cargo is, it builds the module whenever its own library is missing
or out of date, a download current or not, and its library is loaded
before a downloaded one."
  (lispm-desktop-test--with-term-sources
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) t)))
      (let ((system-configuration "x86_64-apple-darwin21.6.0")
            (lispm-desktop-term-download-enabled t))
        (should (eq (lispm-desktop-term--plan) 'build))
        (let ((prebuilt (lispm-desktop-test--term-file (lispm-desktop-term--prebuilt-file))))
          (should (eq (lispm-desktop-term--plan) 'build))
          (should (equal (lispm-desktop-term--module-file) prebuilt))
          (let ((built (lispm-desktop-test--term-file
                        (expand-file-name "target/x86_64-apple-darwin/release/liblispm_desktop_term.dylib" root)
                        30)))
            (should-not (lispm-desktop-term--plan))
            (should (equal (lispm-desktop-term--module-file) built))
            (set-file-times (expand-file-name "Cargo.lock" root))
            (should (eq (lispm-desktop-term--plan) 'build))))))))

(ert-deftest lispm-desktop-test-term-install-prebuilt ()
  "A downloaded library is kept only when its SHA-256 is the one published
for it, and then replaces those kept for other versions."
  (lispm-desktop-test--with-term-sources
    (let* ((system-configuration "x86_64-pc-linux-gnu")
           (download (make-temp-file "lispm-desktop-term-download" t))
           (library (expand-file-name "lib.so" download))
           (sums (expand-file-name "SHA256SUMS" download))
           (file (lispm-desktop-term--prebuilt-file))
           (name (file-name-nondirectory file))
           (old (lispm-desktop-test--term-file
                 (expand-file-name (concat "0123456789abcdef/" name) (lispm-desktop-term--prebuilt-directory)))))
      (unwind-protect
          (progn
            (write-region "the library" nil library)
            (write-region (format "%s  %s\n" (make-string 64 ?0) name) nil sums)
            (should-error (lispm-desktop-term--install-prebuilt library sums file))
            (write-region (format "%s *other.so\n" (secure-hash 'sha256 "the library")) nil sums)
            (should-error (lispm-desktop-term--install-prebuilt library sums file))
            (should-not (file-exists-p file))
            (should (file-exists-p old))
            (write-region (format "%s  liblispm_desktop_term-x86_64-apple-darwin.dylib\n%s  %s\n"
                                  (make-string 64 ?1) (secure-hash 'sha256 "the library") name)
                          nil sums)
            (should (equal (lispm-desktop-term--install-prebuilt library sums file) file))
            (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) "the library"))
            (should-not (file-exists-p (concat file ".part")))
            (should-not (file-exists-p (file-name-directory old)))
            (should (equal (lispm-desktop-term--prebuilt-files) (list file))))
        (delete-directory download t)))))

(ert-deftest lispm-desktop-test-term-load-failure ()
  "A library that does not load leaves terminals in Eat, and is not tried
again; nothing loads while the module is being downloaded."
  (lispm-desktop-test--with-term-sources
    (let ((system-configuration "aarch64-apple-darwin24.1.0")
          (tries 0))
      (cl-letf (((symbol-function 'module-load)
                 (lambda (_file) (setq tries (1+ tries)) (error "Not a module")))
                (inhibit-message t))
        (should-not (lispm-desktop-term-load))
        (should (= tries 0))
        (let ((library (lispm-desktop-test--term-file (lispm-desktop-term--prebuilt-file))))
          (let ((lispm-desktop-term--downloading (make-process :name "lispm-desktop-test" :command '("sleep" "5"))))
            (unwind-protect
                (should-not (lispm-desktop-term-load))
              (delete-process lispm-desktop-term--downloading)))
          (should (= tries 0))
          (should-not (lispm-desktop-term-load))
          (should-not (lispm-desktop-term-load))
          (should (= tries 1))
          (should (equal (car lispm-desktop-term--load-failure) library)))))))

(ert-deftest lispm-desktop-test-term-download ()
  "The module is downloaded from where `lispm-desktop-term-prebuilt-url'
says, under the sources' version, kept when its checksum holds, then
loaded; a library that is not the one published is dropped.  The release
is a local directory: curl reads it as it reads the web."
  (skip-unless (executable-find "curl"))
  (lispm-desktop-test--with-term-sources
    (let* ((system-configuration "aarch64-apple-darwin24.1.0")
           (releases (make-temp-file "lispm-desktop-term-releases" t))
           (lispm-desktop-term-prebuilt-url (concat "file://" releases "/"))
           (release (expand-file-name (concat "term-module-" (lispm-desktop-term-source-version)) releases))
           (asset "liblispm_desktop_term-aarch64-apple-darwin.dylib")
           (file (lispm-desktop-term--prebuilt-file))
           loaded called)
      (unwind-protect
          (cl-letf (((symbol-function 'lispm-desktop-term-load) (lambda () (setq loaded t)))
                    (inhibit-message t))
            (make-directory release t)
            (write-region "the library" nil (expand-file-name asset release))
            (write-region (format "%s  %s\n" (secure-hash 'sha256 "the library") asset)
                          nil (expand-file-name "SHA256SUMS" release))
            (lispm-desktop-term-download (lambda () (setq called t)))
            (while lispm-desktop-term--downloading
              (accept-process-output nil 0.05))
            (should (file-exists-p file))
            (should (and loaded called))
            (should (equal (lispm-desktop-term--current-file) file))
            ;; Tampered with: the checksum no longer holds.
            (delete-directory (lispm-desktop-term--prebuilt-directory) t)
            (setq loaded nil called nil)
            (write-region "another library" nil (expand-file-name asset release))
            (lispm-desktop-term-download (lambda () (setq called t)))
            (while lispm-desktop-term--downloading
              (accept-process-output nil 0.05))
            (should-not (file-exists-p file))
            (should-not (or loaded called))
            (should (string-search "SHA-256 differs"
                                   (with-current-buffer "*lispm-desktop-term download*" (buffer-string)))))
        (delete-directory releases t)))))

(ert-deftest lispm-desktop-test-term-wheel ()
  "The wheel is reported to a program that asked for the mouse, turned into
cursor keys for one holding the whole screen, and else scrolls the
history, as Alacritty does; with Shift it always scrolls the history."
  (let ((window (selected-window))
        (old (window-buffer (selected-window)))
        sent scrolled)
    (with-temp-buffer
      (setq-local lispm-desktop-term--size '(80 . 24))
      (set-window-buffer window (current-buffer))
      (unwind-protect
          (cl-letf (((symbol-function 'lispm-desktop-term--send) (lambda (string) (push string sent)))
                    ((symbol-function 'lispm-desktop-term-module-scroll)
                     (lambda (_ lines) (push lines scrolled)))
                    ((symbol-function 'lispm-desktop-term--render) #'ignore))
            (let ((event (lambda (type)
                           (list type (list window 1 '(10 . 5) 0 nil 1 '(10 . 5) nil nil '(1 . 1))))))
              (cl-letf (((symbol-function 'lispm-desktop-term-modes) (lambda (&rest _) (logior 4 8 16))))
                (lispm-desktop-term-wheel-up (funcall event 'wheel-up))
                (lispm-desktop-term-wheel-down (funcall event 'wheel-down))
                (should (equal (reverse sent) '("\e[<64;11;6M" "\e[<65;11;6M")))
                (setq sent nil)
                (lispm-desktop-term-wheel-up (funcall event 'S-wheel-up))
                (should-not sent)
                (should (equal scrolled '(3))))
              (cl-letf (((symbol-function 'lispm-desktop-term-modes) (lambda (&rest _) 8)))
                (lispm-desktop-term-wheel-up (funcall event 'wheel-up))
                (should (equal sent '("\e[M`+&"))))
              (setq sent nil scrolled nil)
              (cl-letf (((symbol-function 'lispm-desktop-term-modes) (lambda (&rest _) 4)))
                (lispm-desktop-term-wheel-down (funcall event 'wheel-down))
                (should (equal sent '("\eOB\eOB\eOB"))))
              (cl-letf (((symbol-function 'lispm-desktop-term-modes) (lambda (&rest _) 0)))
                (lispm-desktop-term-wheel-up (funcall event 'wheel-up))
                (should (equal scrolled '(3))))))
        (set-window-buffer window old)))))

(ert-deftest lispm-desktop-test-term-clicks ()
  "A program that takes the mouse is told of a press, of the moves while
the button is held when it asked for them, and of the release; without
mouse reports, and with Shift, clicks stay Emacs's."
  (let ((window (selected-window))
        (old (window-buffer (selected-window)))
        sent)
    (with-temp-buffer
      (setq-local lispm-desktop-term--size '(80 . 24))
      (lispm-desktop-term-keys-mode 1)
      (set-window-buffer window (current-buffer))
      (unwind-protect
          (cl-letf (((symbol-function 'lispm-desktop-term--send) (lambda (string) (push string sent))))
            (let ((posn (lambda (x y) (list window 1 (cons x y) 0 nil 1 (cons x y) nil nil '(1 . 1)))))
              (cl-letf (((symbol-function 'lispm-desktop-term-modes) (lambda (&rest _) (logior 8 16 128))))
                (should (eq (key-binding [down-mouse-1]) #'lispm-desktop-term-mouse-press))
                (should (eq (key-binding [mouse-1]) #'ignore))
                (should (eq (key-binding [S-down-mouse-1]) #'lispm-desktop-term-mouse-emacs))
                (let ((unread-command-events
                       (list (list 'mouse-movement (funcall posn 4 2))
                             (list 'mouse-movement (funcall posn 4 2))
                             (list 'drag-mouse-1 (funcall posn 1 1) (funcall posn 6 3)))))
                  (lispm-desktop-term-mouse-press (list 'down-mouse-1 (funcall posn 1 1)))
                  (should (equal (reverse sent)
                                 '("\e[<0;2;2M" "\e[<32;5;3M" "\e[<0;7;4m")))))
              (setq sent nil)
              (cl-letf (((symbol-function 'lispm-desktop-term-modes) (lambda (&rest _) 8)))
                (let ((unread-command-events
                       (list (list 'mouse-movement (funcall posn 4 2))
                             (list 'mouse-3 (funcall posn 2 0)))))
                  (lispm-desktop-term-mouse-press (list 'down-mouse-3 (funcall posn 2 0)))
                  (should (equal (reverse sent) '("\e[M\"#!" "\e[M##!")))))
              (cl-letf (((symbol-function 'lispm-desktop-term-modes) (lambda (&rest _) 0)))
                (should-not (eq (key-binding [down-mouse-1]) #'lispm-desktop-term-mouse-press))
                (should-not (eq (key-binding [mouse-1]) #'ignore)))))
        (set-window-buffer window old)))))

(defmacro lispm-desktop-test--with-quit (answers documents programs &rest body)
  "Run BODY quitting through alerts answered by ANSWERS in turn, with
DOCUMENTS unsaved and PROGRAMS running.  Bind `asked' to the alerts'
messages and details, `killed', `saved' to what was killed and saved."
  (declare (indent 3))
  `(let ((answers ,answers) (asked nil) (killed nil) (saved nil)
         (kill-emacs-query-functions nil) (save-some-buffers-functions nil)
         (confirm-kill-processes t))
     (cl-letf (((symbol-function 'lispm-desktop-popup-available-p) (lambda () t))
               ((symbol-function 'lispm-desktop-popup-alert)
                (lambda (_icon message buttons &optional detail)
                  (push (list message detail (mapcar #'car buttons)) asked)
                  (pop answers)))
               ((symbol-function 'lispm-desktop-platinum--unsaved-documents) (lambda () ,documents))
               ((symbol-function 'lispm-desktop-platinum--running-programs) (lambda () ,programs))
               ((symbol-function 'save-buffer) (lambda (&rest _) (push (current-buffer) saved)))
               ((symbol-function 'kill-emacs) (lambda (&rest args) (push args killed))))
       ,@body)))

(ert-deftest lispm-desktop-test-quit-asks-in-alerts ()
  "Quitting asks of each unsaved document and of the programs running in
alerts; Cancel keeps Emacs, Don't Save and Quit end it."
  (with-temp-buffer
    (let ((document (current-buffer))
          (confirm-kill-emacs nil))
      (lispm-desktop-test--with-quit '(cancel) (list document) nil
        (lispm-desktop-platinum--quit #'ignore)
        (should-not killed)
        (should (= (length asked) 1))
        (should (string-match-p "Save changes to the document .* before quitting\\?"
                                (car (car asked)))))
      (lispm-desktop-test--with-quit '(discard quit) (list document) '("claude" "zsh")
        (lispm-desktop-platinum--quit #'ignore)
        (should (equal killed '((nil nil))))
        (should-not saved)
        (should (equal (cadr (car asked)) "Quitting stops “claude” and “zsh”."))
        (should (equal (nth 2 (car asked)) '("Cancel" "Quit"))))
      (lispm-desktop-test--with-quit '(save cancel) (list document) '("zsh")
        (lispm-desktop-platinum--quit #'ignore nil t)
        (should (equal saved (list document)))
        (should-not killed)
        (should (equal (car (car asked)) "Restart Emacs while programs are running?"))))))

(ert-deftest lispm-desktop-test-quit-yes-or-no-questions-are-alerts ()
  "The questions of `kill-emacs-query-functions' and `confirm-kill-emacs'
are alerts too, the latter asked only when nothing else was."
  (lispm-desktop-test--with-quit '(yes no) nil nil
    (let ((confirm-kill-emacs #'y-or-n-p))
      (add-hook 'kill-emacs-query-functions
                (lambda () (yes-or-no-p "This Emacs session has clients; exit anyway? ")))
      (lispm-desktop-platinum--quit #'ignore)
      (should-not killed)
      (should (equal (mapcar #'car (reverse asked))
                     '("This Emacs session has clients; exit anyway?" "Really quit Emacs?")))
      (should (equal (nth 2 (car asked)) '("No" "Yes")))))
  (lispm-desktop-test--with-quit '() nil nil
    (let ((confirm-kill-emacs nil))
      (lispm-desktop-platinum--quit #'ignore)
      (should (equal killed '((nil nil))))
      (should-not asked)))
  (should (equal (lispm-desktop-platinum-quoted-names '("a" "b" "c" "d" "e")) "“a”, “b”, “c” and 2 others"))
  (should (equal (lispm-desktop-platinum-quoted-names '("*shell:a*")) "“shell:a”")))

(ert-deftest lispm-desktop-test-typed-events ()
  "Characters and function keys are typed; mouse events and the area
symbols `read-event' returns before them are not, so a panel dropped
from the menu bar outlives the release of the click that dropped it."
  (should (lispm-desktop-typed-event-p ?a))
  (should (lispm-desktop-typed-event-p 'f1))
  (should (lispm-desktop-typed-event-p 'escape))
  (should-not (lispm-desktop-typed-event-p 'tab-bar))
  (should-not (lispm-desktop-typed-event-p 'mode-line))
  (should-not (lispm-desktop-typed-event-p '(mouse-1 nil)))
  (should-not (lispm-desktop-typed-event-p nil)))

(ert-deftest lispm-desktop-test-menu-typed-keys ()
  "Menus are walked from the keyboard: a typed letter goes to the next
enabled item it starts, and the keys shown beside items can be typed:
no function keys, and no Super keys where no key is Super."
  (let ((items '((:label "Undo" :action ignore) separator (:label "Paste" :action ignore)
                 (:label "Print" :action ignore :enabled nil)
                 (:label " paste again" :action ignore))))
    (should (= (lispm-desktop-popup--item-starting items nil ?p) 2))
    (should (= (lispm-desktop-popup--item-starting items 2 ?P) 4))
    (should (= (lispm-desktop-popup--item-starting items 4 ?p) 2))
    (should-not (lispm-desktop-popup--item-starting items nil ?z)))
  (should (lispm-desktop-popup-keyboard-p (kbd "C-x h")))
  (should (lispm-desktop-popup-keyboard-p (kbd "M-RET")))
  (should-not (lispm-desktop-popup-keyboard-p (kbd "<f10>")))
  (should-not (lispm-desktop-popup-keyboard-p (kbd "<open>")))
  (should-not (lispm-desktop-popup-keyboard-p (kbd "M-<down>")))
  (should-not (lispm-desktop-popup-keyboard-p (kbd "<mouse-1>")))
  (let ((window-system 'ns) (ns-command-modifier 'meta) (ns-alternate-modifier 'none))
    (should-not (lispm-desktop-popup-keyboard-p (kbd "s-a"))))
  (should (lispm-desktop-popup-keyboard-p (kbd "M-c"))))

(ert-deftest lispm-desktop-test-term-keys ()
  "Keys send the bytes terminals expect: cursor keys follow the program's
mode, C-c C-c sends C-c, and Escape reaches the program."
  (should (equal (cdr (assoc "C-c C-c" lispm-desktop-term-keys)) "\C-c"))
  (should (equal (cdr (assoc "<escape>" lispm-desktop-term-keys)) "\e"))
  (should (equal (cdr (assoc "S-<return>" lispm-desktop-term-keys)) "\e[13;2u"))
  (cl-letf (((symbol-function 'lispm-desktop-term-modes) (lambda (&rest _) 1)))
    (should (equal (lispm-desktop-term--cursor-key "A") "\eOA")))
  (cl-letf (((symbol-function 'lispm-desktop-term-modes) (lambda (&rest _) 0)))
    (should (equal (lispm-desktop-term--cursor-key "A") "\e[A")))
  (should (eq (lookup-key lispm-desktop-term-keys-mode-map (kbd "C-c C-c")) #'lispm-desktop-term-send-key))
  (should (eq (lookup-key lispm-desktop-term-keys-mode-map (kbd "M-x")) #'execute-extended-command)))

;;;; Control Strip

(defmacro lispm-desktop-test--with-strip (&rest body)
  "Run BODY with the Control Strip's state fresh, every module shown, and
nothing saved or drawn."
  (declare (indent 0))
  `(let ((lispm-desktop-strip-modules '(appearance desktop keyboard internet battery load mail))
         (lispm-desktop-strip-collapsed nil)
         (lispm-desktop-strip-edge 'left)
         (lispm-desktop-strip-raised 0)
         (lispm-desktop-strip-shown t)
         (lispm-desktop-strip-width nil)
         (lispm-desktop-strip--first 0)
         (lispm-desktop-strip--pressed nil)
         (lispm-desktop-strip--focus nil)
         (saved nil))
     (ignore saved)
     (cl-letf (((symbol-function 'lispm-desktop-settings--browser-p) #'always)
               ((symbol-function 'lispm-desktop-platinum-battery-percent) (lambda () 80))
               ((symbol-function 'lispm-desktop-strip--time-p) #'always)
               ((symbol-function 'lispm-desktop-strip--render) #'ignore)
               ((symbol-function 'lispm-desktop-settings--save)
                (lambda (variable value) (push (cons variable value) saved) (set variable value))))
       ,@body)))

(ert-deftest lispm-desktop-test-strip-parts ()
  "The strip runs collapse box, left arrow, modules, right arrow and tab; the
tab drags how many modules show, the arrows scroll to the rest; collapsed,
the tab alone is left."
  (lispm-desktop-test--with-strip
    (should (equal (lispm-desktop-strip--parts)
                   '(collapse left (module . appearance) (module . desktop) (module . keyboard)
                              (module . internet) (module . battery) (module . load) (module . mail)
                              right tab)))
    (should (= (lispm-desktop-strip--width) (+ 15 14 (* 7 28) 14 17)))
    (should (eq (lispm-desktop-strip--state 'left) 'disabled))
    (should (eq (lispm-desktop-strip--state 'right) 'disabled))
    (setq lispm-desktop-strip-width 3)
    (lispm-desktop-strip-scroll 1)
    (should (equal (seq-filter #'consp (lispm-desktop-strip--parts))
                   '((module . desktop) (module . keyboard) (module . internet))))
    (should-not (lispm-desktop-strip--state 'left))
    (should-not (lispm-desktop-strip--state 'right))
    (should (= (lispm-desktop-strip--x '(module . keyboard)) (+ 15 14 28)))
    (lispm-desktop-strip-scroll 10)
    (should (equal (car (seq-filter #'consp (lispm-desktop-strip--parts))) '(module . battery)))
    (should (eq (lispm-desktop-strip--state 'right) 'disabled))
    (lispm-desktop-strip-collapse)
    (should (equal saved '((lispm-desktop-strip-collapsed . t))))
    (should (equal (lispm-desktop-strip--parts) '(tab)))
    (lispm-desktop-strip-collapse t)
    (should-not lispm-desktop-strip-collapsed)))

(ert-deftest lispm-desktop-test-strip-modules ()
  "Each module has a sixteen-pixel icon, its help, and a menu ending in its
panel where it has one; modules that cannot work are left out."
  (require 'battery)
  (lispm-desktop-test--with-strip
    (let ((lispm-desktop-platinum-current-palette (lispm-desktop-test--platinum-palette))
          (lispm-desktop-theme 'platinum)
          (lispm-desktop-platinum--battery '((?p . "80") (?L . "AC") (?t . "N/A")))
          (lispm-desktop-platinum--load "1.25")
          (battery-echo-area-format "Power %L, battery %p%%"))
      (dolist (key (lispm-desktop-strip--modules))
        (let ((icon (lispm-desktop-strip--icon key)))
          (should (if (stringp icon) (file-readable-p icon) (equal (lispm-desktop-test--size icon) '(16 . 16)))))
        (should (stringp (lispm-desktop-strip--help (cons 'module key))))
        (should (lispm-desktop-strip--items key)))
      (pcase-dolist (`(,key . ,panel) '((appearance . "Open Appearance…") (desktop . "Open Desktop…")
                                        (keyboard . "Open Keyboard…") (internet . "Open Internet…")))
        (should (equal (plist-get (car (last (lispm-desktop-strip--items key))) :label) panel)))
      (should (plist-get (seq-find (lambda (item) (equal (plist-get item :label) "Platinum"))
                                   (lispm-desktop-strip--items 'appearance))
                         :checked)))
    (cl-letf (((symbol-function 'lispm-desktop-settings--browser-p) #'ignore)
              ((symbol-function 'lispm-desktop-platinum-battery-percent) #'ignore))
      (should (equal (lispm-desktop-strip--modules) '(appearance desktop keyboard load mail))))))

(ert-deftest lispm-desktop-test-strip-segment-focus ()
  "A module the keyboard is on is ringed in the focus colour; a drawn icon
is embedded as it is; the tab's end and the triangles stand out on any
theme."
  (let* ((palette (lispm-desktop-test--platinum-palette))
         (icon (lispm-desktop-platinum-draw-meter palette 1.0))
         (module (lispm-desktop-platinum-draw-strip-segment palette 'module :icon icon :focused t)))
    (should (equal (lispm-desktop-test--pixel module 0 10) "#6666CC"))
    (should (equal (lispm-desktop-test--pixel module 1 10) "#6666CC"))
    (should (equal (lispm-desktop-test--pixel module 13 1) "#6666CC"))
    (should (equal (lispm-desktop-test--pixel module 27 10) "#000000"))
    (should (= (length (dom-by-tag module 'svg)) 2)))
  ;; The tab's pointed end is set off against what the strip lies on.
  (let ((tab (lispm-desktop-platinum-draw-strip-segment (lispm-desktop-test--platinum-palette) 'tab
                                                        :behind "#6666AA")))
    (should (equal (lispm-desktop-test--pixel tab 16 0) "#6666AA"))
    (should (equal (lispm-desktop-test--pixel tab 0 10) "#000000")))
  ;; Its triangle stands out on a dark theme's greys too.
  (let ((dracula (lispm-desktop-test--dracula-palette)))
    (should (equal (lispm-desktop-test--pixel
                    (lispm-desktop-platinum-draw-strip-segment dracula 'module :menu t) 21 11)
                   (upcase (plist-get dracula :text))))))

(ert-deftest lispm-desktop-test-strip-holds-size ()
  "The strip's frame keeps the strip's size as the font changes, where it
grew to the four lines Emacs keeps a window to, over the echo area."
  (when-let* ((old (get-buffer lispm-desktop-strip-buffer-name)))
    (kill-buffer old))
  (let ((buffer (lispm-desktop-strip--buffer)))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer nil buffer)
          (should (= (window-min-size nil nil nil t) (frame-char-height)))
          (with-current-buffer buffer
            (should (memq #'lispm-desktop-popup--keep-size window-size-change-functions))))
      (kill-buffer buffer))))

(ert-deftest lispm-desktop-test-strip-menu-place ()
  "A module's menu pops up small, its outline on the module's left edge and
its bottom two pixels into the strip, as Mac OS 9's did."
  (lispm-desktop-test--with-strip
    (let ((items '((:label "One" :action ignore) separator (:label "Two" :action ignore))))
      (let ((lispm-desktop-popup-small t))
        (should (= (cdr (lispm-desktop-popup--rows items)) (+ 13 6 13))))
      (should (= (cdr (lispm-desktop-popup--rows items)) (+ 16 6 16)))
      (cl-letf (((symbol-function 'frame-position) (lambda (&rest _) '(0 . 500))))
        (should (equal (lispm-desktop-strip--menu-place 'desktop items)
                       (cons (+ 15 14 28 -1) (- 501 32))))))))

(ert-deftest lispm-desktop-test-strip-place-split ()
  "The strip goes in its screen's bottom-left corner when the screen is
split in windows too, whose root window is no live window; never above
the root window, under the menu bar."
  (lispm-desktop-test--with-strip
    (save-window-excursion
      (delete-other-windows)
      (split-window-right)
      (split-window-below)
      (let ((frame (selected-frame)) placed)
        (cl-letf (((symbol-function 'lispm-desktop-strip--live-p) #'always)
                  ((symbol-function 'frame-parent) (lambda (&rest _) frame))
                  ((symbol-function 'set-frame-position)
                   (lambda (_frame x y) (setq placed (cons x y)))))
          (lispm-desktop-strip--place)
          (pcase-let ((`(,_ ,top ,_ ,bottom) (window-pixel-edges (frame-root-window frame))))
            (should (equal placed
                           (cons 0 (max top (- bottom lispm-desktop-platinum-strip-height)))))))))))

(ert-deftest lispm-desktop-test-strip-right-edge-parts ()
  "On the right edge the tab is the strip's left end and the collapse box
its right end, as Mac OS 9 drew it; collapsed, the tab alone is left."
  (lispm-desktop-test--with-strip
    (let ((lispm-desktop-strip-edge 'right)
          (lispm-desktop-strip-width 2))
      (should (equal (lispm-desktop-strip--parts)
                     '(tab left (module . appearance) (module . desktop) right collapse)))
      (should (= (lispm-desktop-strip--x '(module . appearance)) (+ 17 14)))
      (let ((lispm-desktop-strip-collapsed t))
        (should (equal (lispm-desktop-strip--parts) '(tab)))))))

(ert-deftest lispm-desktop-test-strip-position ()
  "The strip lies on its edge, raised from the bottom, never above the
bounds' top, under the menu bar."
  (let ((bounds '(0 20 800 600))
        (h lispm-desktop-platinum-strip-height))
    (should (equal (lispm-desktop-strip--position bounds 200 'left 0) (cons 0 (- 600 h))))
    (should (equal (lispm-desktop-strip--position bounds 200 'right 100) (cons 600 (- 600 h 100))))
    (should (equal (lispm-desktop-strip--position bounds 200 'right 9999) '(600 . 20)))))

(ert-deftest lispm-desktop-test-strip-moved ()
  "Dragged by its tab, the strip goes to the edge whose half of the screen
the mouse is in, as high as the mouse took it, kept on the screen."
  (let* ((bounds '(0 20 800 600))
         (h lispm-desktop-platinum-strip-height)
         (start (- 600 h)))
    (should (equal (lispm-desktop-strip--moved bounds start '(100 . -200)) '(left . 200)))
    (should (equal (lispm-desktop-strip--moved bounds start '(400 . -200)) '(right . 200)))
    (should (equal (lispm-desktop-strip--moved bounds start '(399 . 50)) '(left . 0)))
    (should (equal (lispm-desktop-strip--moved bounds start '(700 . -9999)) (cons 'right (- 600 h 20))))))

(ert-deftest lispm-desktop-test-strip-moves-and-hides-from-the-keyboard ()
  "In the strip, C-p and C-n move it up and down its edge, C-a and C-e to
the left and the right edge; C-c B hides it and shows it again, for the
next start too."
  (lispm-desktop-test--with-strip
    (let ((frame (selected-frame))
          (h lispm-desktop-platinum-strip-height))
      (cl-letf (((symbol-function 'lispm-desktop-strip--live-p) #'always)
                ((symbol-function 'lispm-desktop-strip--open) #'ignore)
                ((symbol-function 'frame-parent) (lambda (&rest _) frame))
                ((symbol-function 'lispm-desktop-strip--bounds) (lambda (_) '(0 20 800 600))))
        (let ((unread-command-events (list ?\C-p ?\C-p ?\C-n ?\C-e ?\C-g)))
          (lispm-desktop-strip-select))
        (should (eq lispm-desktop-strip-edge 'right))
        (should (= lispm-desktop-strip-raised h))
        (let ((unread-command-events (list ?\C-n ?\C-n ?\C-a ?\C-g)))
          (lispm-desktop-strip-select))
        (should (eq lispm-desktop-strip-edge 'left))
        (should (= lispm-desktop-strip-raised 0))
        (should (equal (assq 'lispm-desktop-strip-edge saved) '(lispm-desktop-strip-edge . left))))
      (let ((live t) (calls nil))
        (cl-letf (((symbol-function 'lispm-desktop-strip--live-p) (lambda () live))
                  ((symbol-function 'lispm-desktop-strip-show) (lambda () (push 'show calls) (setq live t)))
                  ((symbol-function 'lispm-desktop-strip-hide) (lambda () (push 'hide calls) (setq live nil))))
          (lispm-desktop-strip-toggle)
          (should-not lispm-desktop-strip-shown)
          (lispm-desktop-strip-toggle)
          (should lispm-desktop-strip-shown)
          (should (equal (reverse calls) '(hide show))))))))

(ert-deftest lispm-desktop-test-strip-outline ()
  "Moving the strip draws Mac OS 9's outline: a solid band two pixels thick
round it, stepping round the tab's slanted end; on the right edge its
mirror."
  (let* ((width 200)
         (tab (- width 17))
         (left (lispm-desktop-platinum-strip-outline-lines 0 100 width 'left "#666633"))
         (right (lispm-desktop-platinum-strip-outline-lines 0 100 width 'right "#666633")))
    ;; Over and along its top, to the tab's top corner; down its left edge.
    (should (member (list -1 99 (+ tab 8 2) 1 nil "#666633") left))
    (should (member (list -1 100 (+ tab 8 3) 1 nil "#666633") left))
    (should (member (list -1 101 22 2 t "#666633") left))
    ;; The tab's straight end, rows 9 to 14, two pixels thick.
    (should (member (list (+ tab 16) 109 6 2 t "#666633") left))
    ;; Where it slants, four: its first slanted row.
    (should (member (list (+ tab 7) 101 4 1 nil "#666633") left))
    ;; The right edge's outline is the left's mirror.
    (cl-flet ((mirror (line)
                (pcase-let ((`(,x ,y ,length ,thickness ,vertical ,color) line))
                  (list (- width 1 (+ x (if vertical thickness length) -1)) y length thickness vertical color))))
      (should (equal (sort (mapcar #'mirror left) (lambda (a b) (string< (prin1-to-string a) (prin1-to-string b))))
                     (sort (copy-sequence right) (lambda (a b) (string< (prin1-to-string a) (prin1-to-string b)))))))))

(ert-deftest lispm-desktop-test-strip-tab-right ()
  "The right edge's tab is the left one's outline mirrored, Mac OS 9's light
kept: lit at its left, shaded at its right."
  (let ((left lispm-desktop-platinum--strip-tab)
        (right lispm-desktop-platinum--strip-tab-right))
    (should (= (length left) (length right)))
    (cl-loop for l in left for r in right
             do (should (= (length l) (length r) 17))
             do (dotimes (i 17)
                  (should (eq (eq (aref l i) ?.) (eq (aref r (- 16 i)) ?.)))))
    (should (equal (lispm-desktop-platinum--strip-tab-ends right) (make-list 24 16)))
    (dolist (row (seq-subseq right 2 21))
      (should (eq (aref row 15) ?8)))))

(ert-deftest lispm-desktop-test-strip-inverse ()
  "The outline is drawn in the inverse of what it lies on, as Mac OS 9's XOR."
  (should (equal (lispm-desktop-platinum-inverse "#9999CC") "#666633"))
  (should (equal (lispm-desktop-platinum-inverse "black") "#ffffff")))

(ert-deftest lispm-desktop-test-strip-keyboard ()
  "C-c b goes to the strip: C-f and C-b move between the modules, Return or
Space opens the ringed one's menu, and C-g leaves; a collapsed strip shows
whole meanwhile."
  (lispm-desktop-test--with-strip
    (let ((opened nil)
          (rings nil)
          (lispm-desktop-strip-collapsed t))
      (cl-letf (((symbol-function 'lispm-desktop-strip--live-p) #'always)
                ((symbol-function 'lispm-desktop-strip--render)
                 (lambda () (when lispm-desktop-strip--focus
                              (push (cons lispm-desktop-strip--focus lispm-desktop-strip-collapsed) rings))))
                ((symbol-function 'lispm-desktop-strip--open)
                 (lambda (key) (push key opened) '(:label "Chosen"))))
        (let ((unread-command-events (list ?\C-f ?\C-f ?\C-b ?\C-f ?
)))
          (lispm-desktop-strip-select))
        (should (equal opened '(keyboard)))
        (should (equal (mapcar #'car (reverse rings)) '(appearance desktop keyboard desktop keyboard keyboard)))
        (should-not (cdr (car rings)))
        (should lispm-desktop-strip-collapsed)
        (should-not lispm-desktop-strip--focus)
        (setq opened nil)
        (let ((unread-command-events (list ?\C-b ?\C-g)))
          (lispm-desktop-strip-select))
        (should-not opened)
        ;; Tab and Shift-Tab ring the modules too, as in any panel.
        (setq rings nil)
        (let ((unread-command-events (list ?\t ?\t 'backtab ?\C-g)))
          (lispm-desktop-strip-select))
        (should (equal (mapcar #'car (reverse rings)) '(appearance desktop keyboard desktop)))))))

(ert-deftest lispm-desktop-test-strip-background-set-when-changed ()
  "Drawing the strip again sets its frame's background only when the
colour changed: NS Emacs clears a frame whenever its background is set."
  (let ((lispm-desktop-strip--frame (selected-frame))
        (color "#C0C0C0")
        (background "#C0C0C0")
        (sets nil))
    (cl-letf (((symbol-function 'lispm-desktop-strip--live-p) #'always)
              ((symbol-function 'lispm-desktop-strip--line) (lambda () "strip"))
              ((symbol-function 'lispm-desktop-platinum-color) (lambda (_key) color))
              ((symbol-function 'lispm-desktop-popup-set-size) #'ignore)
              ((symbol-function 'lispm-desktop-strip--width) (lambda () 100))
              ((symbol-function 'lispm-desktop-strip--place) #'ignore)
              ((symbol-function 'frame-parameter)
               (lambda (_frame parameter) (and (eq parameter 'background-color) background)))
              ((symbol-function 'set-frame-parameter)
               (lambda (_frame parameter value)
                 (push (cons parameter value) sets)
                 (when (eq parameter 'background-color) (setq background value)))))
      (unwind-protect
          (progn
            (lispm-desktop-strip--render)
            (lispm-desktop-strip--render)
            (should-not sets)
            (setq color "#1b1d24")
            (lispm-desktop-strip--render)
            (lispm-desktop-strip--render)
            (should (equal sets '((background-color . "#1b1d24")))))
        (when (get-buffer lispm-desktop-strip-buffer-name)
          (kill-buffer lispm-desktop-strip-buffer-name))))))

(ert-deftest lispm-desktop-test-strip-walks-menus ()
  "In a module's menu opened from the keyboard, Right and Left go on to the
menu of the module beside, as along the menu bar."
  (lispm-desktop-test--with-strip
    (let ((opened nil))
      (cl-letf (((symbol-function 'lispm-desktop-strip--open)
                 (lambda (key)
                   (push key opened)
                   (when (< (length opened) 3)
                     (throw 'lispm-desktop-popup-menu-bar-step (if (eq key 'appearance) -1 1)))
                   nil)))
        (should (equal (lispm-desktop-strip--walk (lispm-desktop-strip--modules) 0) '(nil . 0)))
        (should (equal opened '(appearance mail appearance)))))))

(ert-deftest lispm-desktop-test-library-path-takes-the-chosen-sdk ()
  "Only the command line tools' SDK on LIBRARY_PATH gives way to the chosen one."
  (require 'lispm-desktop-early)
  (let ((path-separator ":")
        (xcode "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/lib"))
    (should (equal (lispm-desktop-early--library-path
                    "/a/gcc/lib/:/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib/:/opt/lib" xcode)
                   (concat "/a/gcc/lib/:" xcode ":/opt/lib")))
    (should (equal (lispm-desktop-early--library-path "/a/gcc/lib" xcode) "/a/gcc/lib"))))

;;;; Applications and the App Store

(require 'package-vc)

(ert-deftest lispm-desktop-test-define-app ()
  "An application defined comes after the others in the Application menu;
defined again it keeps its place; removed, it goes, and the desktop comes in
front if it was there.  The applications' list is never changed in place."
  (let* ((original lispm-desktop-apps)
         (copy (copy-tree original))
         (lispm-desktop-apps original)
         (lispm-desktop-apps--frontmost nil))
    (lispm-desktop-define-app 'elfeed :name "Elfeed" :activate #'ignore)
    (should (eq (car (car (last lispm-desktop-apps))) 'elfeed))
    (lispm-desktop-define-app 'other :name "Other" :activate #'ignore)
    (lispm-desktop-define-app 'elfeed :name "Feeds" :activate #'ignore)
    (should (equal (last (mapcar #'car lispm-desktop-apps) 2) '(elfeed other)))
    (should (equal (plist-get (alist-get 'elfeed lispm-desktop-apps) :name) "Feeds"))
    (setq lispm-desktop-apps--frontmost 'elfeed)
    (lispm-desktop-remove-app 'elfeed)
    (should-not (assq 'elfeed lispm-desktop-apps))
    (should (eq (lispm-desktop-apps-frontmost) 'desktop))
    (should (equal original copy))))

(ert-deftest lispm-desktop-test-apps-functions-may-be-lambdas ()
  "An application's :visible, :running and :menus may be lambdas as well as
named functions; one whose function is not defined does not run."
  (let ((lispm-desktop-apps `((desktop :name "Desktop" :activate ignore)
                              (shown :name "Shown" :activate ignore :visible ,(lambda () t)
                                     :menus ,(lambda () '((m "File" ignore))))
                              (hidden :name "Hidden" :activate ignore :visible ,(lambda () nil))
                              (missing :name "Missing" :activate ignore
                                       :visible lispm-desktop-test-no-such-function)))
        (lispm-desktop-apps--frontmost 'shown))
    (should (equal (mapcar #'car (lispm-desktop-apps--available)) '(desktop shown)))
    (should (equal (lispm-desktop-apps-menus) '((m "File" ignore))))))

(ert-deftest lispm-desktop-test-apps-full-screen ()
  "The floating windows hide only behind the editor or the browser filling
the screen; any other application in front leaves them shown."
  (let ((lispm-desktop-browser-floating nil))
    (cl-letf (((symbol-function 'lispm-desktop-editor-floating-p) #'ignore))
      (should (lispm-desktop-apps-full-screen-p 'editor))
      (should (lispm-desktop-apps-full-screen-p 'browser))
      (should-not (lispm-desktop-apps-full-screen-p 'desktop))
      (should-not (lispm-desktop-apps-full-screen-p 'elfeed)))
    (let ((lispm-desktop-browser-floating t))
      (cl-letf (((symbol-function 'lispm-desktop-editor-floating-p) #'always))
        (should-not (lispm-desktop-apps-full-screen-p 'editor))
        (should-not (lispm-desktop-apps-full-screen-p 'browser))))))

(ert-deftest lispm-desktop-test-app-window-follows-frontmost ()
  "An application's window hides behind the full-screen editor and shows
again with the desktop in front."
  (let ((lispm-desktop-app-window--frames `((elfeed . ,(selected-frame))))
        (visible t)
        (calls nil))
    (cl-letf (((symbol-function 'frame-visible-p) (lambda (_frame) visible))
              ((symbol-function 'lispm-desktop-platinum-show-window)
               (lambda (_frame) (push 'show calls) (setq visible t)))
              ((symbol-function 'lispm-desktop-platinum-hide-window)
               (lambda (_frame) (push 'hide calls) (setq visible nil)))
              ((symbol-function 'lispm-desktop-platinum-raise-window) #'ignore)
              ((symbol-function 'lispm-desktop-editor-floating-p) #'ignore))
      (lispm-desktop-app-window--follow-frontmost 'editor)
      (lispm-desktop-app-window--follow-frontmost 'desktop)
      (lispm-desktop-app-window--follow-frontmost 'elfeed)
      (should (equal (nreverse calls) '(hide show))))))

(ert-deftest lispm-desktop-test-app-window-close-box ()
  "The close box of an application's window closes the window alone while
others share the application's frame, else the application's window."
  (let ((closed nil))
    (cl-letf (((symbol-function 'lispm-desktop-app-window-close) (lambda (id) (setq closed id))))
      (save-window-excursion
        (delete-other-windows)
        (let ((frame (selected-frame)))
          (set-frame-parameter frame 'lispm-desktop-window-kind 'elfeed)
          (unwind-protect
              (let ((other (split-window)))
                (lispm-desktop-app-window-close-box other)
                (should-not (window-live-p other))
                (should-not closed)
                (lispm-desktop-app-window-close-box (selected-window))
                (should (eq closed 'elfeed)))
            (set-frame-parameter frame 'lispm-desktop-window-kind nil)))))))

(defmacro lispm-desktop-test--with-window-kinds (frames &rest body)
  "Run BODY among the fake FRAMES, as `lispm-desktop-test--with-frames' does,
each a floating window of the kind its parameter `lispm-desktop-window-kind'
names, registered as open.  In BODY, `selected' is the frame selected,
`focused' the frames given the keyboard, the latest first, `after' the
frames whose keyboard went on, `behind' the window it went on to, its
application in front, or nil for the screen, `activated' how often the
desktop came in front, and `hidden' whether the desktop is hidden in the
Dock.  A window is at (1 2 300 200), or at its parameter `geometry'."
  (declare (indent 1))
  `(lispm-desktop-test--with-frames ,frames
       (let ((lispm-desktop-app-window--frames nil)
             (lispm-desktop-app-window--geometry nil)
             (lispm-desktop-apps--frontmost nil)
             (lispm-desktop-apps-frontmost-functions (list #'lispm-desktop-app-window--follow-frontmost))
             (selected 'screen)
             (focused nil)
             (after nil)
             (behind nil)
             (activated 0)
             (hidden nil))
         (dolist (frame (mapcar #'car parameters))
           (when-let* ((id (frame-parameter frame 'lispm-desktop-window-kind)))
             (push (cons id frame) lispm-desktop-app-window--frames)))
         (setq lispm-desktop-app-window--frames (nreverse lispm-desktop-app-window--frames))
         (cl-letf (((symbol-function 'selected-frame) (lambda () selected))
                   ((symbol-function 'select-frame-set-input-focus)
                    (lambda (frame &rest _) (setq selected frame) (push frame focused)))
                   ((symbol-function 'lispm-desktop-apps-focus-after)
                    (lambda (frame)
                      (push frame after)
                      (setq selected (or behind 'screen))
                      (when behind
                        (lispm-desktop-apps-set-frontmost (frame-parameter behind 'lispm-desktop-app)))
                      behind))
                   ((symbol-function 'lispm-desktop-activate)
                    (lambda () (cl-incf activated) (lispm-desktop-apps-set-frontmost 'desktop)))
                   ((symbol-function 'lispm-desktop-apps-hidden-p) (lambda (&optional _) hidden))
                   ((symbol-function 'lispm-desktop-apps-screen) (lambda () 'screen))
                   ((symbol-function 'lispm-desktop-apps--geometry)
                    (lambda (frame) (or (frame-parameter frame 'geometry) '(1 2 300 200))))
                   ((symbol-function 'lispm-desktop-apps-keep-on-desktop) #'ignore)
                   ((symbol-function 'lispm-desktop-platinum-show-window)
                    (lambda (frame) (set-frame-parameter frame 'visibility t)))
                   ((symbol-function 'lispm-desktop-platinum-hide-window)
                    (lambda (frame) (set-frame-parameter frame 'visibility nil)))
                   ((symbol-function 'lispm-desktop-platinum-raise-window) (lambda (frame) (push frame raised)))
                   ((symbol-function 'lispm-desktop-platinum-rolled-up-height)
                    (lambda (frame) (frame-parameter frame 'rolled)))
                   ((symbol-function 'lispm-desktop-platinum-roll-down)
                    (lambda (frame _height) (set-frame-parameter frame 'rolled nil)))
                   ((symbol-function 'frame-root-window) (lambda (&optional _) (selected-window)))
                   ((symbol-function 'delete-frame)
                    (lambda (&optional frame _force)
                      (run-hook-with-args 'delete-frame-functions frame)
                      (setq parameters (assq-delete-all frame parameters)))))
           (let ((delete-frame-functions (list #'lispm-desktop-app-window--forget)))
             ,@body)))))

(defconst lispm-desktop-test--window-kinds
  '((screen (visibility . t))
    (finder (lispm-desktop-window-kind . finder) (lispm-desktop-app . desktop) (visibility . t))
    (settings (lispm-desktop-window-kind . settings) (lispm-desktop-app . settings) (visibility . t))
    (store (lispm-desktop-window-kind . store) (lispm-desktop-app . store) (visibility . t))
    (editor (lispm-desktop-window-kind . editor) (lispm-desktop-app . editor) (visibility . t))
    (browser (lispm-desktop-window-kind . browser) (lispm-desktop-app . browser) (visibility . t)))
  "The fake floating windows of `lispm-desktop-test--with-window-kinds':
the Finder window, Control Panels, the App Store and the floating editor
and browser, all shown.")

(ert-deftest lispm-desktop-test-app-window-kinds-follow-frontmost ()
  "The floating windows hide from the full-screen editor, the floating
browser only from it; they show again with the desktop, and the window of
the application in front comes over the others, the Finder window
excepted, and the floating editor too, whose own code shows and hides it."
  (lispm-desktop-test--with-window-kinds lispm-desktop-test--window-kinds
    (let ((lispm-desktop-browser-floating t)
          (floating nil))
      (cl-letf (((symbol-function 'lispm-desktop-editor-floating-p) (lambda () floating)))
        (cl-flet ((shown () (seq-filter #'frame-visible-p '(finder settings store editor browser))))
          (lispm-desktop-apps-set-frontmost 'editor)
          (should (equal (shown) '(editor)))
          (setq raised nil)
          (lispm-desktop-apps-set-frontmost 'desktop)
          (should (equal (shown) '(finder settings store editor browser)))
          (should-not raised)
          (lispm-desktop-apps-set-frontmost 'store)
          (should (equal raised '(store)))
          (setq floating t raised nil)
          (lispm-desktop-apps-set-frontmost 'editor)
          (should (equal (shown) '(finder settings store editor browser)))
          (should (equal raised '(editor)))
          (setq lispm-desktop-browser-floating nil)
          (lispm-desktop-apps-set-frontmost 'store)
          (should (equal (shown) '(finder settings store editor browser))))))))

(ert-deftest lispm-desktop-test-app-window-close-bookkeeping ()
  "A floating window closed, by its close box or deleted as by C-x 5 0, is
remembered where it was, at the height it rolls down to; the keyboard goes
on only when it had it, and the desktop comes in front only when the
window's application was in front.  A window closed twice is closed once."
  (lispm-desktop-test--with-window-kinds lispm-desktop-test--window-kinds
    (set-frame-parameter 'settings 'rolled 500)
    (lispm-desktop-apps-set-frontmost 'store)
    ;; Control Panels, behind the App Store and without the keyboard.
    (lispm-desktop-app-window-close 'settings)
    (should-not (lispm-desktop-app-window-open-p 'settings))
    (should-not (frame-parameter 'settings 'rolled))
    (should (equal (alist-get 'settings lispm-desktop-app-window--geometry) '(1 2 300 200)))
    (should-not after)
    (should (eq (lispm-desktop-apps-frontmost) 'store))
    ;; The App Store, in front and with the keyboard, deleted as by C-x 5 0.
    (setq selected 'store)
    (set-frame-parameter 'store 'geometry '(10 20 640 640))
    (delete-frame 'store)
    (should-not (lispm-desktop-app-window-open-p 'store))
    (should (equal (alist-get 'store lispm-desktop-app-window--geometry) '(10 20 640 640)))
    (should (equal after '(store)))
    (should (eq (lispm-desktop-apps-frontmost) 'desktop))
    (lispm-desktop-app-window--retire 'store)
    (should (equal after '(store)))))

(defconst lispm-desktop-test--windows-behind
  '((screen (visibility . t))
    (outer-a (lispm-desktop-platinum-frames . a) (parent-frame . screen) (visibility . t))
    (a (lispm-desktop-window . t) (parent-frame . outer-a) (lispm-desktop-platinum-frame . outer-a)
       (lispm-desktop-app . settings) (visibility . t))
    (b (lispm-desktop-window . t) (parent-frame . screen) (lispm-desktop-app . editor))
    (outer-c (lispm-desktop-platinum-frames . c) (parent-frame . screen) (visibility . t))
    (c (lispm-desktop-window . t) (parent-frame . outer-c) (lispm-desktop-platinum-frame . outer-c)
       (lispm-desktop-app . desktop) (visibility . t)))
  "Floating windows over the screen for `lispm-desktop-test--with-frames':
A framed in front, B hidden, C framed at the back.")

(ert-deftest lispm-desktop-test-focus-after-goes-to-the-window-behind ()
  "A floating window going away gives the keyboard to the window shown
right behind it, as Mac OS 9 made the next window active, and puts that
window's application in front; with none behind, the screen takes the
keyboard.  While the screen is hidden in the Dock, the window after is
the one in front of those out on the computer's screen, and with none
left the keyboard stays where it is."
  (lispm-desktop-test--with-frames lispm-desktop-test--windows-behind
    (let ((lispm-desktop-apps--frontmost 'settings)
          (lispm-desktop-apps-frontmost-functions nil)
          (order '(outer-a a b outer-c c))
          (top '(screen))
          (focused nil))
      (cl-letf (((symbol-function 'frame-list-z-order)
                 (lambda (&optional parent) (if (eq parent 'screen) order top)))
                ((symbol-function 'select-frame-set-input-focus)
                 (lambda (frame &rest _) (push frame focused))))
        (should (equal (lispm-desktop-apps-windows-in-order 'screen) '(a b c)))
        (should (eq (lispm-desktop-apps-focus-after 'a) 'c))
        (should (equal focused '(c)))
        (should (eq (lispm-desktop-apps-frontmost) 'desktop))
        (setq order '(outer-c c) lispm-desktop-apps--frontmost 'settings)
        (should-not (lispm-desktop-apps-focus-after 'c))
        (should (equal focused '(screen c)))
        (should (eq (lispm-desktop-apps-frontmost) 'settings))
        ;; Hidden in the Dock, with C out on the computer's screen.
        (set-frame-parameter 'screen 'lispm-desktop-hidden t)
        (set-frame-parameter 'outer-c 'parent-frame nil)
        (set-frame-parameter 'c 'lispm-desktop-out 'screen)
        (setq top '(outer-c c screen) focused nil)
        (should (eq (lispm-desktop-apps-focus-after 'a) 'c))
        (should (equal focused '(c)))
        (should (eq (lispm-desktop-apps-frontmost) 'desktop))
        (should-not (lispm-desktop-apps-focus-after 'c))
        (should (equal focused '(c)))))))

(ert-deftest lispm-desktop-test-app-window-close-focuses-the-window-behind ()
  "A window closed with the keyboard gives it to the window behind, whose
application comes in front rather than the desktop.  A window closed from
its application's menu had the keyboard, which the menu bar holds only
for the menu; the Finder window only when it had it last."
  (lispm-desktop-test--with-window-kinds lispm-desktop-test--window-kinds
    (let ((lispm-desktop-browser-floating t)
          (lispm-desktop-finder--had-focus nil))
      (cl-letf (((symbol-function 'lispm-desktop-editor-floating-p) #'always))
        ;; The App Store in front, closed by its close box.
        (lispm-desktop-apps-set-frontmost 'store)
        (setq selected 'store behind 'editor)
        (lispm-desktop-app-window-close 'store)
        (should (equal after '(store)))
        (should (eq selected 'editor))
        (should (eq (lispm-desktop-apps-frontmost) 'editor))
        ;; Control Panels in front, closed from its File menu.
        (lispm-desktop-apps-set-frontmost 'settings)
        (setq selected 'screen behind 'browser)
        (lispm-desktop-app-window-close 'settings)
        (should (equal after '(settings store)))
        (should (eq selected 'browser))
        (should (eq (lispm-desktop-apps-frontmost) 'browser))
        ;; The Finder window, closed from the desktop's File menu while the
        ;; desktop had the keyboard.
        (lispm-desktop-apps-set-frontmost 'desktop)
        (setq selected 'screen)
        (lispm-desktop-app-window-close 'finder)
        (should (equal after '(settings store)))
        (should (eq selected 'screen))))))

(ert-deftest lispm-desktop-test-editor-close-focuses-the-window-behind ()
  "The floating editor closed gives the keyboard to the window behind it,
the desktop left behind; with no window behind, the desktop comes in
front."
  (let ((lispm-desktop-editor--closed nil)
        (lispm-desktop-editor--frame 'editor)
        (behind 'finder)
        (hidden nil)
        (activated 0))
    (cl-letf (((symbol-function 'lispm-desktop-editor-floating-p) #'always)
              ((symbol-function 'lispm-desktop-apps-focus-after) (lambda (_) behind))
              ((symbol-function 'lispm-desktop-platinum-hide-window) (lambda (frame) (push frame hidden)))
              ((symbol-function 'lispm-desktop-apps-hidden-p) #'ignore)
              ((symbol-function 'lispm-desktop-activate) (lambda () (cl-incf activated))))
      (lispm-desktop-editor-close)
      (should lispm-desktop-editor--closed)
      (should (equal hidden '(editor)))
      (should (= activated 0))
      (setq behind nil)
      (lispm-desktop-editor-close)
      (should (= activated 1)))))

(ert-deftest lispm-desktop-test-browser-window-passes-the-keyboard ()
  "The floating browser's window deleted with the keyboard gives it to the
window behind; going full screen, to the screen it then fills."
  (lispm-desktop-test--with-window-kinds
      (cons '(stand-in (lispm-desktop-app . browser) (visibility . t))
            lispm-desktop-test--window-kinds)
    (let ((lispm-desktop-browser--frame 'browser)
          (lispm-desktop-browser-floating t))
      (cl-letf (((symbol-function 'lispm-desktop-browser-floating-p) (lambda () lispm-desktop-browser--frame))
                ((symbol-function 'lispm-desktop-browser--unshade) #'ignore)
                ((symbol-function 'lispm-desktop-apps-window-screen) (lambda (_) 'screen)))
        (lispm-desktop-apps-set-frontmost 'browser)
        (setq selected 'browser)
        (lispm-desktop-browser--delete-frame t)
        (should-not (frame-live-p 'browser))
        (should (eq selected 'screen))
        (should-not after)
        (setq lispm-desktop-browser--frame 'stand-in selected 'stand-in behind 'finder)
        (lispm-desktop-browser--delete-frame)
        (should-not (frame-live-p 'stand-in))
        (should (equal after '(stand-in)))
        (should (eq selected 'finder))
        (should (eq (lispm-desktop-apps-frontmost) 'desktop))))))

(ert-deftest lispm-desktop-test-app-window-released ()
  "The floating editor or browser going full screen is forgotten as its
kind's window, its place remembered, the application in front and the
keyboard left to its own code."
  (lispm-desktop-test--with-window-kinds lispm-desktop-test--window-kinds
    (lispm-desktop-apps-set-frontmost 'editor)
    (setq selected 'editor)
    (lispm-desktop-app-window-release 'editor)
    (delete-frame 'editor)
    (should-not (lispm-desktop-app-window-open-p 'editor))
    (should (equal (alist-get 'editor lispm-desktop-app-window--geometry) '(1 2 300 200)))
    (should (eq (lispm-desktop-apps-frontmost) 'editor))
    (should-not after)))

(ert-deftest lispm-desktop-test-app-window-opens-out-with-the-desktop-hidden ()
  "A floating window opening brings the desktop in front first, unless the
desktop is hidden in the Dock: the window then opens out on the computer's
screen.  So does the browser quitting while in front; once another
application came in front, as the window behind the floating browser
took the keyboard, the desktop stays behind."
  (lispm-desktop-test--with-window-kinds lispm-desktop-test--window-kinds
    (lispm-desktop-app-window-ensure 'settings)
    (should (= activated 1))
    (setq hidden t)
    (lispm-desktop-app-window-ensure 'settings)
    (should (= activated 1))
    (cl-letf (((symbol-function 'lispm-desktop-browser--close-workspace) #'ignore))
      (lispm-desktop-apps-set-frontmost 'browser)
      (lispm-desktop-browser--quit)
      (should (= activated 1))
      (setq hidden nil)
      (lispm-desktop-browser--quit)
      (should (= activated 2))
      (lispm-desktop-apps-set-frontmost 'settings)
      (lispm-desktop-browser--quit)
      (should (= activated 2)))))

(ert-deftest lispm-desktop-test-app-window-refocus ()
  "After a menu bar menu, the window of the application in front takes the
keyboard back; the Finder window only when it had it."
  (lispm-desktop-test--with-window-kinds lispm-desktop-test--window-kinds
    (let ((lispm-desktop-finder--had-focus nil))
      (lispm-desktop-apps-set-frontmost 'settings)
      (lispm-desktop-app-window--refocus)
      (should (equal focused '(settings)))
      (lispm-desktop-apps-set-frontmost 'desktop)
      (setq selected 'screen)
      (lispm-desktop-app-window--refocus)
      (should (equal focused '(settings)))
      (setq lispm-desktop-finder--had-focus t)
      (lispm-desktop-app-window--refocus)
      (should (equal focused '(finder settings))))))

(ert-deftest lispm-desktop-test-app-window-first-places ()
  "Each kind's window opens first where its kind puts it, under the menu
bar, a window system or not; only windows that cascade count for the
cascade of an application's window."
  (lispm-desktop-test--with-window-kinds lispm-desktop-test--window-kinds
    (let ((made nil))
      (cl-letf (((symbol-function 'make-frame) (lambda (params) (setq made params) 'new))
                ((symbol-function 'minibuffer-window) #'ignore)
                ((symbol-function 'set-face-attribute) #'ignore)
                ((symbol-function 'set-window-buffer) #'ignore)
                ((symbol-function 'set-window-dedicated-p) #'ignore)
                ((symbol-function 'lispm-desktop-finder--buffer) #'ignore)
                ((symbol-function 'lispm-desktop-settings--buffer) #'ignore)
                ((symbol-function 'frame-inner-width) (lambda (&optional _) 1200))
                ((symbol-function 'frame-inner-height) (lambda (&optional _) 800))
                ;; As in an Emacs without a window system.
                ((symbol-function 'tab-bar-height) nil))
        (cl-flet ((place (id)
                    (setq lispm-desktop-app-window--frames
                          (assq-delete-all id lispm-desktop-app-window--frames))
                    (lispm-desktop-app-window-make-frame id 'screen)
                    (list (alist-get 'left made) (alist-get 'top made))))
          (should (equal (place 'finder) '(40 30)))
          (should (equal (place 'settings) '(340 50)))
          (should (equal (place 'editor) '(72 20)))
          (should (equal (place 'browser) '(120 30)))
          ;; The App Store, the only window open that cascades, is not counted
          ;; for itself; an application's window cascades from it.
          (setq lispm-desktop-app-window--frames
                (seq-filter (lambda (entry) (eq (car entry) 'store)) lispm-desktop-app-window--frames))
          (lispm-desktop-app-window-make-frame 'elfeed 'screen)
          (should (equal (list (alist-get 'left made) (alist-get 'top made)) '(82 62)))
          (should (eq (alist-get 'lispm-desktop-app-window made) 'elfeed))
          (should (eq (alist-get 'lispm-desktop-close-function made) 'lispm-desktop-app-window-close-box))
          (lispm-desktop-app-window-make-frame 'editor 'screen)
          (should-not (assq 'lispm-desktop-app-window made))
          (should-not (assq 'lispm-desktop-close-function made)))))))

(ert-deftest lispm-desktop-test-app-window-modes ()
  "The Finder window's, Control Panels' and the App Store's buffers derive
from the floating windows' mode: no cursor, no fringes, no wrapped lines."
  (dolist (mode '(lispm-desktop-finder-mode lispm-desktop-settings-mode lispm-desktop-store-mode))
    (with-temp-buffer
      (cl-letf (((symbol-function 'lispm-desktop-store--refit) #'ignore))
        (funcall mode))
      (should (derived-mode-p 'lispm-desktop-app-window-mode))
      (should-not cursor-type)
      (should truncate-lines)
      (should (eql left-fringe-width 0)))))

(ert-deftest lispm-desktop-test-list-check-box-pixels ()
  "Extensions Manager's check boxes: twelve pixels square, outlined; checked,
crossed from corner to corner; for a group partly on, crossed by a bar."
  (let ((palette (lispm-desktop-test--platinum-palette)))
    (cl-flet ((at (state x y)
                (lispm-desktop-test--pixel (lispm-desktop-platinum-draw-list-check-box palette state) x y)))
      (should (equal (lispm-desktop-test--size (lispm-desktop-platinum-draw-list-check-box palette 'on))
                     '(12 . 12)))
      (should (equal (at 'on 0 0) "#000000"))
      (should (equal (at 'on 1 1) "#FFFFFF"))
      (should (equal (at 'on 3 2) "#000000"))
      (should (equal (at 'on 8 2) "#000000"))
      (should (equal (at 'off 3 2) "#DDDDDD"))
      (should (equal (at 'mixed 4 5) "#000000"))
      (should (equal (at 'mixed 4 3) "#DDDDDD"))
      (should (equal (at 'off 10 6) "#888888")))))

(defmacro lispm-desktop-test--with-store (&rest body)
  "Run BODY with an App Store of its own: no catalog, nothing installed,
its clones in a temporary directory, Customize only setting what it saves."
  (declare (indent 0))
  `(let* ((lispm-desktop-store-directory (file-name-as-directory (make-temp-file "lispm-desktop-test-store" t)))
          (lispm-desktop-store-catalogs nil)
          (lispm-desktop-store--catalogs nil)
          (lispm-desktop-store-installed nil)
          (lispm-desktop-store--checking nil)
          (lispm-desktop-store--checked t)
          (lispm-desktop-apps lispm-desktop-apps)
          (lispm-desktop-apps--frontmost nil)
          (lispm-desktop-app-window--frames nil)
          (inhibit-message t))
     (cl-letf (((symbol-function 'customize-save-variable)
                (lambda (variable value &rest _) (set variable value))))
       (unwind-protect
           (progn ,@body)
         (when (get-buffer lispm-desktop-store-buffer-name)
           (kill-buffer lispm-desktop-store-buffer-name))
         (delete-directory lispm-desktop-store-directory t)))))

(defun lispm-desktop-test--store-catalog (url items &optional files name)
  "Put a catalog at URL's clone holding ITEMS, and FILES, (FILE . TEXT)s;
NAME it.  The catalog is listed and read."
  (let ((directory (lispm-desktop-store--catalog-directory url)))
    (make-directory directory t)
    (with-temp-file (expand-file-name "catalog.eld" directory)
      (prin1 `(:name ,(or name "Test Store") :items ,items) (current-buffer)))
    (pcase-dolist (`(,file . ,text) files)
      (let ((path (expand-file-name file directory)))
        (make-directory (file-name-directory path) t)
        (with-temp-file path (insert text))))
    (setq lispm-desktop-store-catalogs (append lispm-desktop-store-catalogs (list url)))
    (lispm-desktop-store-read-catalogs)))

(ert-deftest lispm-desktop-test-store-catalog-directory ()
  "A catalog is cloned in a directory named after its repository."
  (let ((lispm-desktop-store-directory "/tmp/store/"))
    (should (equal (lispm-desktop-store--catalog-directory "https://github.com/LispMachineLab/lispm-store")
                   "/tmp/store/LispMachineLab-lispm-store/"))
    (should (equal (lispm-desktop-store--catalog-directory "https://example.org/a/b.git/")
                   "/tmp/store/a-b/"))))

(defun lispm-desktop-test--git (directory &rest args)
  "Run git with ARGS in DIRECTORY, unsigned, as the tests' own author."
  (apply #'lispm-desktop-store--git directory
         "-c" "user.name=LispM Test" "-c" "user.email=test@example.org"
         "-c" "commit.gpgsign=false" "-c" "tag.gpgsign=false"
         args))

(ert-deftest lispm-desktop-test-store-fetches-catalogs ()
  "Checking for updates clones a catalog the first time and brings it to its
newest commit after, in the background; then the list shows what it holds."
  (skip-unless (executable-find "git"))
  (lispm-desktop-test--with-store
    (let ((source (make-temp-file "lispm-desktop-test-catalog" t)))
      (unwind-protect
          (cl-flet ((commit (items)
                      (with-temp-file (expand-file-name "catalog.eld" source)
                        (prin1 `(:name "Fetched" :items ,items) (current-buffer)))
                      (lispm-desktop-test--git source "add" "catalog.eld")
                      (lispm-desktop-test--git source "commit" "--quiet" "-m" "Catalog"))
                    (check ()
                      (let ((done nil))
                        (lispm-desktop-store-check-for-updates (lambda () (setq done t)))
                        (with-timeout (30 (error "The catalogs were not fetched"))
                          (while (not done)
                            (accept-process-output nil 0.05))))))
            (lispm-desktop-test--git source "init" "--quiet")
            (commit '((calculator :kind app :name "Calculator" :command calc)))
            (setq lispm-desktop-store-catalogs (list source))
            (check)
            (should (equal (mapcar #'car (lispm-desktop-store-items)) '(calculator)))
            (should (equal (lispm-desktop-store--catalog-name source) "Fetched"))
            (should (stringp (plist-get (car lispm-desktop-store--catalogs) :commit)))
            (commit '((calculator :kind app :name "Calculator" :command calc)
                      (calendar :kind app :name "Calendar" :command calendar)))
            (check)
            (should (equal (mapcar #'car (lispm-desktop-store-items)) '(calculator calendar))))
        (delete-directory source t)))))

(ert-deftest lispm-desktop-test-store-git-ignores-callers-repository ()
  "Git runs for the App Store in the directory given, whatever repository
an inherited GIT_DIR names."
  (let ((process-environment (cons "GIT_DIR=/nonexistent/.git" process-environment)))
    (should-not (member "GIT_DIR=/nonexistent/.git"
                        (seq-filter (lambda (entry) (string-prefix-p "GIT_DIR=" entry))
                                    (let ((env (lispm-desktop-store--git-environment)))
                                      ;; The first entry naming a variable wins.
                                      (seq-take env (seq-position env "GIT_DIR=/nonexistent/.git"))))))
    (should (equal (car (lispm-desktop-store--git-environment)) "GIT_DIR"))))

(ert-deftest lispm-desktop-test-store-main-file ()
  "A repository installed from its URL is the package of its main file:
named after the repository, at the top or in lisp/, else the one file with
a Version header."
  (let ((repository (make-temp-file "lispm-desktop-test-repo" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "lisp" repository))
          (with-temp-file (expand-file-name "lisp/foo.el" repository) (insert ";;; foo.el\n"))
          (with-temp-file (expand-file-name "lisp/foo-extra.el" repository) (insert ";;; extra\n"))
          (should (equal (file-name-nondirectory (lispm-desktop-store--main-file repository "emacs-foo"))
                         "foo.el"))
          (with-temp-file (expand-file-name "bar.el" repository)
            (insert ";;; bar.el\n;; Version: 1.0\n;;;###autoload\n(defun bar-open () (interactive))\n"))
          (should (equal (file-name-nondirectory (lispm-desktop-store--main-file repository "unrelated"))
                         "bar.el"))
          (should (equal (lispm-desktop-store--autoloaded-commands (expand-file-name "bar.el" repository))
                         '("bar-open"))))
      (delete-directory repository t))))

(ert-deftest lispm-desktop-test-store-item-validation ()
  "An item is listed only when well formed: a name, a kind the App Store
installs, files inside its catalog, a package pinned and built by no
command; an application opened by a command or a feature, an extension
loading a feature from a package of its own."
  (cl-flet ((valid (item) (lispm-desktop-store--item-valid-p item)))
    (should (valid '(calc :kind app :name "Calculator" :command calc :category utilities :subtitle "Calc")))
    (should (valid '(e :kind app :name "E" :command e :package e)))
    (should (valid '(e :kind app :name "E" :feature e :package (e :url "u" :rev "r" :lisp-dir "lisp"))))
    (should (valid '(x :kind extension :name "X" :feature x :package (x :url "u" :rev "r")
                       :program "x" :install "brew install x")))
    (should-not (valid '(x :kind extension :name "X" :package (x :url "u" :rev "r"))))
    (should-not (valid '(x :kind extension :name "X" :feature x)))
    (should-not (valid '(x :kind extension :name "X" :feature x :package x :program x)))
    (should-not (valid '(e :kind app :name "E")))
    (should-not (valid '(e :kind app :name "E" :command e :subtitle 3)))
    (should-not (valid '(e :kind app :name "E" :command e :category "news")))
    (should-not (valid '(e :kind app :name "E" :command e :package (e :url "u"))))
    (should-not (valid '(e :kind app :name "E" :command e :package (e :url "u" :rev "r" :make "all"))))
    (should-not (valid '(e :kind app :name "E" :command e :package (e :url "u" :rev "r" :shell-command "x"))))
    (should-not (valid '(e :kind app :name "E" :command e :icon "../x")))
    (should-not (valid '(r :kind role :name "R" :file "roles/r.org")))
    (should-not (valid '("e" :kind app :name "E" :command e)))))

(ert-deftest lispm-desktop-test-store-catalogs-read ()
  "The catalogs are read as data: each item carries its catalog, an earlier
catalog's item hides a later one's of the same name, items wrongly given
are left out, items of other kinds, as LispM's roles, passed over without
a word, and those installed no catalog lists stay listed."
  (lispm-desktop-test--with-store
    (lispm-desktop-test--store-catalog
     "https://example.org/first"
     '((calculator :kind app :name "Calculator" :command calc)
       (broken :kind app :name "Broken")))
    (let ((said nil))
      (cl-letf (((symbol-function 'message) (lambda (&rest args) (push (apply #'format args) said))))
        (lispm-desktop-test--store-catalog
         "https://example.org/second"
         '((calculator :kind app :name "Other Calculator" :command calc)
           (reviewer :kind role :name "Reviewer" :file "roles/reviewer.org")
           (hermes :kind extension :name "Hermes" :feature lispm-hermes
                   :package (lispm-hermes :url "u" :rev "r")))))
      (should-not (seq-find (lambda (text) (string-search "reviewer" text)) said)))
    (setq lispm-desktop-store-installed '((gone :kind app :name "Gone" :command gone)
                                          (old-role :kind role :name "Old" :file "roles/old.org")))
    (let ((items (lispm-desktop-store-items)))
      (should (equal (mapcar #'car items) '(calculator hermes gone)))
      (should (equal (plist-get (cdr (assq 'calculator items)) :name) "Calculator"))
      (should (equal (plist-get (cdr (assq 'calculator items)) :catalog) "https://example.org/first"))
      (should (equal (plist-get (cdr (assq 'hermes items)) :catalog) "https://example.org/second"))
      (should (equal (lispm-desktop-store--catalog-name "https://example.org/second") "Test Store")))))

(defvar lispm-desktop-test--extension-events nil
  "What the test extension's library did: `loaded', `unloaded', newest first.")

(defmacro lispm-desktop-test--with-extension (feature &rest body)
  "Run BODY with `extension-directory', a temporary directory, first on the
load path, where BODY may write FEATURE's library with
`lispm-desktop-test--write-extension'.  FEATURE is unloaded after."
  (declare (indent 1))
  `(let* ((extension-directory (make-temp-file "lispm-desktop-test-extension" t))
          (load-path (cons extension-directory load-path)))
     (unwind-protect
         (progn ,@body)
       (when (featurep ,feature)
         (unload-feature ,feature t))
       (delete-directory extension-directory t))))

(defun lispm-desktop-test--write-extension (directory feature)
  "Write FEATURE's library in DIRECTORY: it pushes `loaded' to
`lispm-desktop-test--extension-events' when loaded and `unloaded' when
unloaded."
  (with-temp-file (expand-file-name (format "%s.el" feature) directory)
    (insert (format "(push 'loaded lispm-desktop-test--extension-events)
(defun %s-unload-function () (push 'unloaded lispm-desktop-test--extension-events) nil)
(provide '%s)\n" feature feature))))

(ert-deftest lispm-desktop-test-store-install-requirements-first ()
  "Installing an extension installs the application it requires first,
both asked about at once; each is kept with its package pinned to its
commit, the application made one of the desktop's and the extension
loaded."
  (lispm-desktop-test--with-store
    (lispm-desktop-test--with-extension 'lispm-desktop-test-ext
      (setq lispm-desktop-test--extension-events nil)
      (lispm-desktop-test--store-catalog
       "https://example.org/store"
       '((elfeed :kind app :name "Elfeed" :command elfeed :category news
                 :package (elfeed :url "https://example.org/elfeed" :rev "0123456789abcdef"))
         (ext :kind extension :name "Ext" :feature lispm-desktop-test-ext :requires (elfeed)
              :package (lispm-desktop-test-ext :url "https://example.org/ext" :rev "fedcba9876543210"))))
      (let ((asked nil) (packages nil) (pinned nil))
        (cl-letf (((symbol-function 'lispm-desktop-store--confirm)
                   (lambda (question &rest _) (push question asked) t))
                  ((symbol-function 'package-installed-p) (lambda (name &rest _) (memq name packages)))
                  ((symbol-function 'package-vc-install)
                   (lambda (spec rev)
                     (push (list spec rev) pinned)
                     (push (car spec) packages)
                     (when (eq (car spec) 'lispm-desktop-test-ext)
                       (lispm-desktop-test--write-extension extension-directory 'lispm-desktop-test-ext)))))
          (lispm-desktop-store-install 'ext)
          (should (equal asked '("Install “Elfeed” and “Ext”?")))
          (should (equal pinned '(((lispm-desktop-test-ext :url "https://example.org/ext") "fedcba9876543210")
                                  ((elfeed :url "https://example.org/elfeed") "0123456789abcdef"))))
          (should (equal (mapcar #'car lispm-desktop-store-installed) '(elfeed ext)))
          (dolist (id '(elfeed ext))
            (should (plist-get (cdr (assq id lispm-desktop-store-installed)) :owned))
            (should-not (lispm-desktop-store-update-available-p id)))
          (should (equal (plist-get (alist-get 'elfeed lispm-desktop-apps) :name) "Elfeed"))
          (should-not (assq 'ext lispm-desktop-apps))
          (should (featurep 'lispm-desktop-test-ext))
          (should (equal lispm-desktop-test--extension-events '(loaded))))))))

(ert-deftest lispm-desktop-test-store-extension-needs-its-program ()
  "An extension whose program is not on PATH is not installed, and nothing
asked: its button does nothing, and what it needs is said with the
command installing it."
  (lispm-desktop-test--with-store
    (lispm-desktop-test--store-catalog
     "https://example.org/store"
     '((ext :kind extension :name "Ext" :feature lispm-desktop-test-ext
            :program "lispm-desktop-test-no-such-program" :install "brew install ext"
            :package (lispm-desktop-test-ext :url "u" :rev "r"))))
    (cl-letf (((symbol-function 'lispm-desktop-store--confirm) (lambda (&rest _) (error "Asked"))))
      (let ((props (cdr (assq 'ext (lispm-desktop-store-items)))))
        (should (equal (lispm-desktop-store--action 'ext props) '("Get")))
        (should (string-search "brew install ext" (lispm-desktop-store--description props))))
      (let ((err (should-error (lispm-desktop-store-install 'ext) :type 'user-error)))
        (should (string-search "brew install ext" (cadr err))))
      (should-error (lispm-desktop-store-act 'ext) :type 'user-error)
      (should-not lispm-desktop-store-installed))))

(ert-deftest lispm-desktop-test-store-extension-from-a-checkout ()
  "An extension whose library is on the load path already, as a
checkout's, is loaded from there: no package is installed, nor deleted
when it is removed, which unloads it."
  (lispm-desktop-test--with-store
    (lispm-desktop-test--with-extension 'lispm-desktop-test-ext
      (setq lispm-desktop-test--extension-events nil)
      (lispm-desktop-test--write-extension extension-directory 'lispm-desktop-test-ext)
      (lispm-desktop-test--store-catalog
       "https://example.org/store"
       '((ext :kind extension :name "Ext" :feature lispm-desktop-test-ext
              :package (lispm-desktop-test-ext :url "u" :rev "abc"))))
      (cl-letf (((symbol-function 'lispm-desktop-store--confirm) (lambda (&rest _) t))
                ((symbol-function 'package-vc-install) (lambda (&rest _) (error "Installed")))
                ((symbol-function 'lispm-desktop-store--delete-package) (lambda (&rest _) (error "Deleted"))))
        (lispm-desktop-store-install 'ext)
        (should-not (plist-get (cdr (assq 'ext lispm-desktop-store-installed)) :owned))
        (should (equal (car (lispm-desktop-store--action 'ext (cdr (assq 'ext (lispm-desktop-store-items)))))
                       "Installed"))
        ;; The catalog pinning another commit is no update of the checkout's.
        (setf (plist-get (cdr (plist-get (cdr (assq 'ext (plist-get (car lispm-desktop-store--catalogs) :items)))
                                         :package))
                         :rev)
              "def")
        (should-not (lispm-desktop-store-update-available-p 'ext))
        (should (equal (lispm-desktop-store--version 'ext (cdr (assq 'ext (lispm-desktop-store-items))))
                       "load path"))
        (lispm-desktop-store-remove 'ext)
        (should-not (featurep 'lispm-desktop-test-ext))
        (should (equal lispm-desktop-test--extension-events '(unloaded loaded)))
        (should-not lispm-desktop-store-installed)))))

(ert-deftest lispm-desktop-test-store-app-making-itself ()
  "An application whose package makes it itself, as Stickies does, is
loaded when installed and unloaded when removed, its windows going with
it."
  (lispm-desktop-test--with-store
    (lispm-desktop-test--with-extension 'lispm-desktop-test-ext
      (setq lispm-desktop-test--extension-events nil)
      (lispm-desktop-test--write-extension extension-directory 'lispm-desktop-test-ext)
      (lispm-desktop-test--store-catalog
       "https://example.org/store"
       '((notes :kind app :name "Notes" :category productivity :feature lispm-desktop-test-ext
                :package (lispm-desktop-test-ext :url "u" :rev "abc"))))
      (cl-letf (((symbol-function 'lispm-desktop-store--confirm) (lambda (&rest _) t)))
        (lispm-desktop-store-install 'notes)
        (should (featurep 'lispm-desktop-test-ext))
        (lispm-desktop-store-remove 'notes)
        (should-not (featurep 'lispm-desktop-test-ext))
        (should (equal lispm-desktop-test--extension-events '(unloaded loaded)))
        (should-not lispm-desktop-store-installed)))))

(ert-deftest lispm-desktop-test-store-extension-that-cannot-load ()
  "An extension whose feature cannot be loaded is not kept as installed,
and the package installed for it is deleted again."
  (lispm-desktop-test--with-store
    (lispm-desktop-test--store-catalog
     "https://example.org/store"
     '((ext :kind extension :name "Ext" :feature lispm-desktop-test-no-such-feature
            :package (lispm-desktop-test-no-such-feature :url "u" :rev "r"))))
    (let ((packages nil) (deleted nil))
      (cl-letf (((symbol-function 'lispm-desktop-store--confirm) (lambda (&rest _) t))
                ((symbol-function 'package-installed-p) (lambda (name &rest _) (memq name packages)))
                ((symbol-function 'package-vc-install) (lambda (spec _rev) (push (car spec) packages)))
                ((symbol-function 'lispm-desktop-store--delete-package) (lambda (package) (push package deleted))))
        (should-error (lispm-desktop-store-install 'ext) :type 'user-error)
        (should-not lispm-desktop-store-installed)
        (should (equal deleted '((lispm-desktop-test-no-such-feature :url "u" :rev "r"))))))))

(ert-deftest lispm-desktop-test-store-space-on-the-disclosure ()
  "Space on the disclosure of the item's information shows or hides it,
rather than pressing the selected item's button."
  (lispm-desktop-test--with-store
    (lispm-desktop-test--store-catalog "https://example.org/store"
                                       '((calculator :kind app :name "Calculator" :command calc)))
    (with-current-buffer (lispm-desktop-store--buffer)
      (lispm-desktop-store--render)
      (goto-char (point-min))
      (while (not (equal (get-text-property (point) 'lispm-desktop-platinum-label) "Item Information"))
        (forward-char 1))
      (should (eq (key-binding (kbd "SPC")) 'push-button))
      (should lispm-desktop-store--information)
      (call-interactively (key-binding (kbd "SPC")))
      (should-not lispm-desktop-store--information))))

(ert-deftest lispm-desktop-test-store-built-in-asks-nothing ()
  "An application built into Emacs is only made an application: nothing is
asked and no package installed."
  (lispm-desktop-test--with-store
    (lispm-desktop-test--store-catalog "https://example.org/store"
                                       '((calculator :kind app :name "Calculator" :command calc)))
    (cl-letf (((symbol-function 'lispm-desktop-store--confirm) (lambda (&rest _) (error "Asked")))
              ((symbol-function 'package-install) (lambda (&rest _) (error "Installed"))))
      (lispm-desktop-store-install 'calculator)
      (should (lispm-desktop-store-installed-p 'calculator))
      (should-not (plist-get (cdr (assq 'calculator lispm-desktop-store-installed)) :owned))
      (should (assq 'calculator lispm-desktop-apps))
      (should (equal (mapcar (lambda (item) (plist-get item :label))
                             (lispm-desktop-store-application-items))
                     '("Calculator"))))))

(ert-deftest lispm-desktop-test-store-declined-installs-nothing ()
  "Declining the alert installs nothing."
  (lispm-desktop-test--with-store
    (lispm-desktop-test--store-catalog "https://example.org/store"
                                       '((emms :kind app :name "EMMS" :command emms :package emms)))
    (cl-letf (((symbol-function 'lispm-desktop-store--confirm) #'ignore)
              ((symbol-function 'package-install) (lambda (&rest _) (error "Installed"))))
      (lispm-desktop-store-install 'emms)
      (should-not lispm-desktop-store-installed))))

(ert-deftest lispm-desktop-test-store-remove ()
  "Removing an application deletes the package the App Store installed and
leaves one there before; each is forgotten."
  (lispm-desktop-test--with-store
    (setq lispm-desktop-store-installed
          '((elfeed :kind app :name "Elfeed" :command elfeed :package (elfeed :url "u" :rev "r")
                    :installed "r" :owned t)
            (emms :kind app :name "EMMS" :command emms :package emms :installed "1.0")))
    (lispm-desktop-store--register-installed)
    (should (assq 'emms lispm-desktop-apps))
    (let ((deleted nil))
      (cl-letf (((symbol-function 'lispm-desktop-store--confirm) (lambda (&rest _) t))
                ((symbol-function 'lispm-desktop-store--delete-package) (lambda (package) (push package deleted))))
        (dolist (id '(elfeed emms))
          (lispm-desktop-store-remove id))
        (should (equal deleted '((elfeed :url "u" :rev "r"))))
        (should-not lispm-desktop-store-installed)
        (should-not (assq 'elfeed lispm-desktop-apps))
        (should-not (assq 'emms lispm-desktop-apps))))))

(ert-deftest lispm-desktop-test-store-remove-says-who-needs-it ()
  "Removing an item says which installed items require it."
  (let ((lispm-desktop-store-installed
         '((elfeed :kind app :name "Elfeed" :command elfeed :package elfeed)
           (ext :kind extension :name "Ext" :feature ext :package ext :requires (elfeed)))))
    (should (string-search "“Ext” needs it"
                           (lispm-desktop-store--remove-detail
                            'elfeed (cdr (assq 'elfeed lispm-desktop-store-installed)))))
    (should (string-search "leaves the editor"
                           (lispm-desktop-store--remove-detail
                            'ext (cdr (assq 'ext lispm-desktop-store-installed)))))))

(ert-deftest lispm-desktop-test-store-updates ()
  "An update is ready once the catalog pins a package to another commit;
updating checks it out again, loads an extension anew, and then none is
ready."
  (lispm-desktop-test--with-store
    (lispm-desktop-test--with-extension 'lispm-desktop-test-ext
      (setq lispm-desktop-test--extension-events nil)
      (lispm-desktop-test--write-extension extension-directory 'lispm-desktop-test-ext)
      (lispm-desktop-test--store-catalog
       "https://example.org/store"
       '((elfeed :kind app :name "Elfeed" :command elfeed :package (elfeed :url "u" :rev "new"))
         (ext :kind extension :name "Ext" :feature lispm-desktop-test-ext
              :package (lispm-desktop-test-ext :url "v" :rev "new"))))
      (setq lispm-desktop-store-installed
            '((elfeed :kind app :name "Elfeed" :command elfeed :package (elfeed :url "u" :rev "old")
                      :catalog "https://example.org/store" :installed "old" :owned t)
              (ext :kind extension :name "Ext" :feature lispm-desktop-test-ext
                   :package (lispm-desktop-test-ext :url "v" :rev "old")
                   :catalog "https://example.org/store" :installed "old" :owned t)))
      (lispm-desktop-store--register-installed)
      (should (lispm-desktop-store-update-available-p 'elfeed))
      (should (equal (lispm-desktop-store--action 'ext (cdr (assq 'ext (lispm-desktop-store-items))))
                     '("Update" . update)))
      (let ((repinned nil))
        (cl-letf (((symbol-function 'lispm-desktop-store--confirm) (lambda (&rest _) t))
                  ((symbol-function 'lispm-desktop-store--repin) (lambda (name rev) (push (cons name rev) repinned))))
          (lispm-desktop-store-update 'elfeed)
          (lispm-desktop-store-act 'ext)
          (should (equal repinned '((lispm-desktop-test-ext . "new") (elfeed . "new"))))
          (should (equal lispm-desktop-test--extension-events '(loaded unloaded loaded)))
          (should (plist-get (cdr (assq 'elfeed lispm-desktop-store-installed)) :owned))
          (should-not (lispm-desktop-store-update-available-p 'elfeed))
          (should-not (lispm-desktop-store-update-available-p 'ext))
          (should-error (lispm-desktop-store-update 'ext) :type 'user-error))))))

(ert-deftest lispm-desktop-test-store-requirement-missing ()
  "An item requiring one no catalog lists is not installed."
  (lispm-desktop-test--with-store
    (lispm-desktop-test--store-catalog "https://example.org/store"
                                       '((ext :kind extension :name "Ext" :feature ext
                                              :package ext :requires (elfeed))))
    (should-error (lispm-desktop-store-install 'ext) :type 'user-error)))

(ert-deftest lispm-desktop-test-store-categories ()
  "The categories down the left are All, those holding an item, in their
order, and Updates, each with its count; an extension is in Extensions,
whatever category it names; a search keeps the items it matches."
  (lispm-desktop-test--with-store
    (lispm-desktop-test--store-catalog
     "https://example.org/store"
     '((tetris :kind app :name "Tetris" :command tetris :category games :subtitle "Falling blocks")
       (snake :kind app :name "Snake" :command snake :category games)
       (calculator :kind app :name "Calculator" :command calc :category utilities)
       (ext :kind extension :name "Ext" :feature ext :package ext :category games)))
    (should (equal (lispm-desktop-store--categories)
                   '((all "All" 4) (utilities "Utilities" 1) (games "Games" 2)
                     (extensions "Extensions" 1) (updates "Updates" 0))))
    (with-temp-buffer
      (setq lispm-desktop-store--category 'games)
      (should (equal (mapcar #'car (lispm-desktop-store--shown)) '(snake tetris)))
      (setq lispm-desktop-store--category 'all
            lispm-desktop-store--filter "BLOCK")
      (should (equal (mapcar #'car (lispm-desktop-store--shown)) '(tetris)))
      (setq lispm-desktop-store--filter "nothing")
      (should (equal (lispm-desktop-store--empty-text) "Nothing matches “nothing”."))
      (setq lispm-desktop-store--filter nil
            lispm-desktop-store--category 'updates)
      (should (equal (lispm-desktop-store--empty-text) "Everything is up to date.")))))

(ert-deftest lispm-desktop-test-store-buttons ()
  "An item's button reads Get until it is installed, then Open for an
application and Installed, doing nothing, for an extension; Update while
an update is ready."
  (let ((lispm-desktop-store--catalogs
         '((:url "c" :items ((elfeed :kind app :name "Elfeed" :command elfeed :package (elfeed :url "u" :rev "new"))
                             (calendar :kind app :name "Calendar" :command calendar)
                             (ext :kind extension :name "Ext" :feature ext :package ext)))))
        (lispm-desktop-store-installed
         '((elfeed :kind app :name "Elfeed" :command elfeed :package (elfeed :url "u" :rev "old") :installed "old")
           (ext :kind extension :name "Ext" :feature ext :package ext :installed "1"))))
    (cl-flet ((action (id) (lispm-desktop-store--action id (cdr (assq id (lispm-desktop-store-items))))))
      (should (equal (action 'calendar) '("Get" . install)))
      (should (equal (action 'elfeed) '("Update" . update)))
      (should (equal (action 'ext) '("Installed")))
      (push '(calendar :kind app :name "Calendar" :command calendar :installed "built-in")
            lispm-desktop-store-installed)
      (should (equal (action 'calendar) '("Open" . open))))))

(ert-deftest lispm-desktop-test-store-grid-hits ()
  "A click on the grid lands on a category of the list at its left, on an
item, or on the item's button; the list goes on down the grid's lines."
  (let* ((width 640)
         (categories '((all "All" 5) (games "Games" 5) (updates "Updates" 0)))
         (items (mapcar (lambda (n) (list (intern (format "game%d" n)) :kind 'app :name (format "Game %d" n)
                                          :command 'ignore))
                        (number-sequence 1 5)))
         (lispm-desktop-store-installed nil)
         (lispm-desktop-store--catalogs nil))
    (pcase-let ((`(,gx ,columns ,tw) (lispm-desktop-store--grid width)))
      (should (= columns 4))
      (should (= (lispm-desktop-store--lines 5 columns 3) 2))
      (should (= (lispm-desktop-store--lines 0 columns 3) 1))
      (should (equal (lispm-desktop-store--hit width 0 20 25 categories items) '(category . games)))
      (should (equal (lispm-desktop-store--hit width 0 20 60 categories items) '(category . updates)))
      (should-not (lispm-desktop-store--hit width 0 20 90 categories items))
      (should (equal (lispm-desktop-store--hit width 0 (+ gx 10) 20 categories items) '(item . game1)))
      (should (equal (lispm-desktop-store--hit width 1 (+ gx 10) 20 categories items) '(item . game5)))
      (should-not (lispm-desktop-store--hit width 1 (+ gx tw 10) 20 categories items))
      (pcase-let ((`(,bx ,by ,_) (lispm-desktop-store--button-box (+ gx tw) tw "Get")))
        (should (equal (lispm-desktop-store--hit width 0 (+ bx 2) (+ by 2) categories items)
                       '(button . game2)))))))

(ert-deftest lispm-desktop-test-store-in-menus-and-folder ()
  "The λ menu opens the App Store and lists the applications installed; the
Applications folder holds the App Store and each application, not the
extensions."
  (let ((lispm-desktop-store-installed
         '((calculator :kind app :name "Calculator" :command calc)
           (ext :kind extension :name "Ext" :feature ext :package ext))))
    (let ((items (lispm-desktop-lambda-items)))
      (should (seq-find (lambda (item) (and (consp item) (equal (plist-get item :label) "App Store")))
                        items))
      (should (seq-find (lambda (item) (and (consp item) (equal (plist-get item :label) "Applications")))
                        items)))
    (should (equal (mapcar #'car (lispm-desktop-store--folder-items)) '("App Store" "Calculator")))))

(ert-deftest lispm-desktop-test-store-window-keys ()
  "The App Store's window shows its categories and items, the push
buttons under them; the keyboard goes from item to item and category to
category, and RET presses the selected item's button."
  (lispm-desktop-test--with-store
    (lispm-desktop-test--store-catalog
     "https://example.org/store"
     (append '((calculator :kind app :name "Calculator" :command calc :category utilities)
               (ext :kind extension :name "Ext" :feature ext :package ext))
             (mapcar (lambda (n) (list (intern (format "game%d" n)) :kind 'app :name (format "Game %d" n)
                                       :command 'ignore :category 'games))
                     (number-sequence 1 5))))
    (let ((pressed nil))
      (cl-letf (((symbol-function 'lispm-desktop-store-install) (lambda (id) (setq pressed id))))
        (with-current-buffer (lispm-desktop-store--buffer)
          (lispm-desktop-store--render)
          (let ((labels (delq nil (mapcar (lambda (position)
                                            (get-text-property position 'lispm-desktop-platinum-label))
                                          (number-sequence (point-min) (1- (point-max)))))))
            (dolist (label '("Check for Updates" "Install from URL…" "Remove" "Get"))
              (should (member label labels))))
          (should (equal (lispm-desktop-store--placard) "7 items, 0 installed"))
          (should (eq lispm-desktop-store--category 'all))
          (should (eq lispm-desktop-store--selected 'calculator))
          (lispm-desktop-store-forward)
          (should (eq lispm-desktop-store--selected 'ext))
          (lispm-desktop-store-down)
          (should (eq lispm-desktop-store--selected 'game4))
          (lispm-desktop-store-down)
          (should (eq lispm-desktop-store--selected 'game5))
          (lispm-desktop-store-up)
          (should (eq lispm-desktop-store--selected 'game1))
          (lispm-desktop-store-next-category)
          (should (eq lispm-desktop-store--category 'utilities))
          (should (eq lispm-desktop-store--selected 'calculator))
          (lispm-desktop-store-next-category)
          (should (eq lispm-desktop-store--category 'games))
          (should (eq lispm-desktop-store--selected 'game1))
          (lispm-desktop-store-previous-category)
          (lispm-desktop-store-previous-category)
          (lispm-desktop-store-previous-category)
          (should (eq lispm-desktop-store--category 'updates))
          (should-not lispm-desktop-store--selected)
          (lispm-desktop-store-show-category 'games)
          (lispm-desktop-store-act-selected)
          (should (eq pressed 'game1)))))))

;;;; Software Update

(defmacro lispm-desktop-test--with-update (&rest body)
  "Run BODY with a Software Update of its own: nothing checked, found or
logged, its file a temporary one, installing as LispM quits."
  (declare (indent 0))
  `(let* ((lispm-desktop-update-file (make-temp-file "lispm-desktop-test-update" nil ".eld"))
          (lispm-desktop-update--checking nil)
          (lispm-desktop-update--checked nil)
          (lispm-desktop-update--waiting nil)
          (lispm-desktop-update--results nil)
          (lispm-desktop-update--log nil)
          (lispm-desktop-update--announced nil)
          (lispm-desktop-update--requested nil)
          (lispm-desktop-update--reopen nil)
          (lispm-desktop-update--installed nil)
          (lispm-desktop-update-check-automatically t)
          (lispm-desktop-update-schedule 'daily)
          (lispm-desktop-update-install-automatically t)
          (lispm-desktop-update-changed-hook nil)
          (inhibit-message t))
     (unwind-protect
         (progn ,@body)
       (delete-file lispm-desktop-update-file))))

(defun lispm-desktop-test--update-commit (directory file subject)
  "Commit FILE, made in the checkout DIRECTORY, with SUBJECT."
  (with-temp-file (expand-file-name file directory)
    (insert subject "\n"))
  (lispm-desktop-test--git directory "add" file)
  (lispm-desktop-test--git directory "commit" "--quiet" "-m" subject))

(defun lispm-desktop-test--update-push (directory file subject)
  "Commit FILE with SUBJECT in the clone DIRECTORY, and push it to its origin."
  (lispm-desktop-test--update-commit directory file subject)
  (lispm-desktop-test--git directory "push" "--quiet" "origin" "HEAD:main"))

(defun lispm-desktop-test--update-release (directory tag)
  "Release what the clone DIRECTORY holds as TAG, and push the tag."
  (lispm-desktop-test--git directory "tag" "-a" tag "-m" tag)
  (lispm-desktop-test--git directory "push" "--quiet" "origin" tag))

(defun lispm-desktop-test--update-repositories ()
  "Return (ROOT CHECKOUT WORK) of new repositories in ROOT: a bare origin
with a first commit on main, CHECKOUT a clone of it following main, as
install.sh clones, and WORK another, whose pushes are the updates."
  (let* ((root (file-name-as-directory (make-temp-file "lispm-desktop-test-update-repos" t)))
         (origin (concat root "origin.git"))
         (checkout (concat root "checkout/"))
         (work (concat root "work/")))
    (lispm-desktop-test--git root "init" "--quiet" "--bare" origin)
    (lispm-desktop-test--git origin "symbolic-ref" "HEAD" "refs/heads/main")
    (make-directory work)
    (lispm-desktop-test--git work "init" "--quiet")
    (lispm-desktop-test--git work "remote" "add" "origin" origin)
    (lispm-desktop-test--update-push work "a.txt" "First")
    (lispm-desktop-test--git root "clone" "--quiet" "--depth" "1" (concat "file://" origin) checkout)
    (list root checkout work)))

(defun lispm-desktop-test--update-check (directory)
  "Check the checkout DIRECTORY and return what was found."
  (let ((result 'pending))
    (lispm-desktop-update--check-checkout "Test" directory (lambda (found) (setq result found)))
    (with-timeout (30 (error "The checkout was not checked"))
      (while (eq result 'pending)
        (accept-process-output nil 0.05)))
    result))

(ert-deftest lispm-desktop-test-update-checks-and-installs ()
  "A check fetches the branch a checkout follows and its tags, and tells
what its latest release brings, changing nothing; commits not released
yet are no update.  Installing moves the checkout on to the release and
logs it, and the log is read back at the next start."
  (skip-unless (executable-find "git"))
  (lispm-desktop-test--with-update
    (pcase-let ((`(,root ,checkout ,work) (lispm-desktop-test--update-repositories)))
      (unwind-protect
          (let ((before (lispm-desktop-update--git checkout "rev-parse" "HEAD")))
            ;; Before the first release, there is nothing to update to.
            (lispm-desktop-test--update-push work "b.txt" "Second")
            (let ((result (lispm-desktop-test--update-check checkout)))
              (should (eq (plist-get result :state) 'current))
              (should-not (plist-get result :release)))
            (should (string-match-p "\\`[0-9a-f]+, [0-9-]+\\'" (lispm-desktop-update-version checkout)))
            (lispm-desktop-test--update-push work "c.txt" "Third")
            (lispm-desktop-test--update-release work "v0.2.0")
            (lispm-desktop-test--update-release work "v0.2.0-rc1")
            (lispm-desktop-test--update-push work "d.txt" "Fourth")
            (let ((result (lispm-desktop-test--update-check checkout)))
              (should (eq (plist-get result :state) 'update))
              (should (equal (plist-get result :release) "v0.2.0"))
              (should (equal (lispm-desktop-update-title result) "Test 0.2.0"))
              (should (equal (plist-get result :changes) '("Third" "Second")))
              (should (equal (plist-get result :from) before))
              (should (equal (plist-get result :to) (lispm-desktop-update--git work "rev-parse" "v0.2.0^{commit}")))
              (should-not (plist-get result :app))
              ;; Fetched only: the files are as they were.
              (should (equal (lispm-desktop-update--git checkout "rev-parse" "HEAD") before))
              (should-not (file-exists-p (expand-file-name "b.txt" checkout)))
              (should (equal (lispm-desktop-update-install (list result)) (list result)))
              (should (equal (lispm-desktop-update--git checkout "rev-parse" "HEAD") (plist-get result :to)))
              (should (file-exists-p (expand-file-name "c.txt" checkout)))
              (should-not (file-exists-p (expand-file-name "d.txt" checkout)))
              (should (equal (lispm-desktop-update-version checkout) "0.2.0"))
              (pcase-let ((`(,_time ,name ,from ,to ,changes) (car lispm-desktop-update--log)))
                (should (equal (list name from to changes)
                               (list "Test 0.2.0" before (plist-get result :to) '("Third" "Second")))))
              (let ((log lispm-desktop-update--log))
                (setq lispm-desktop-update--log nil)
                (lispm-desktop-update--load)
                (should (equal lispm-desktop-update--log log)))
              (should (eq (plist-get (lispm-desktop-test--update-check checkout) :state) 'current))
              ;; Past the release, as a developer's checkout on main is, it
              ;; is up to date too.
              (lispm-desktop-test--git checkout "merge" "--quiet" "--ff-only" "origin/main")
              (let ((result (lispm-desktop-test--update-check checkout)))
                (should (eq (plist-get result :state) 'current))
                (should (equal (plist-get result :release) "v0.2.0")))
              (should (string-match-p "\\`0\\.2\\.0\\+1, [0-9a-f]+\\'" (lispm-desktop-update-version checkout)))))
        (delete-directory root t)))))

(defun lispm-desktop-test--update-install (root tag)
  "Return a checkout of ROOT's origin made as install.sh makes one: cloned
shallow at the release TAG, on main following origin's main."
  (let ((checkout (concat root "installed/")))
    (lispm-desktop-test--git root "clone" "--quiet" "--depth" "1" "--branch" tag
                             (concat "file://" root "origin.git") checkout)
    (lispm-desktop-test--git checkout "remote" "set-branches" "origin" "main")
    (lispm-desktop-test--git checkout "fetch" "--quiet" "origin")
    (lispm-desktop-test--git checkout "checkout" "--quiet" "-B" "main")
    (lispm-desktop-test--git checkout "branch" "--quiet" "--set-upstream-to=origin/main" "main")
    checkout))

(ert-deftest lispm-desktop-test-update-changes-by-pull-request ()
  "An update lists what was merged since along the branch's own line: a
pull request by its title and number, a commit pushed straight by its
subject.  In a checkout cloned shallow at a release, as install.sh clones,
a pull request branched before the release brings no older history in."
  (skip-unless (executable-find "git"))
  (should (equal (lispm-desktop-update--changes
                  "Merge pull request #3 from a/b\x1f\x1e\nPlain\x1f\x1e\n\x1f\x1e")
                 '("Merge pull request #3 from a/b" "Plain")))
  (should-not (lispm-desktop-update--changes nil))
  (lispm-desktop-test--with-update
    (pcase-let ((`(,root ,_checkout ,work) (lispm-desktop-test--update-repositories)))
      (unwind-protect
          (let ((first (lispm-desktop-update--git work "rev-parse" "HEAD"))
                (branch (lispm-desktop-update--git work "rev-parse" "--abbrev-ref" "HEAD")))
            (lispm-desktop-test--update-push work "b.txt" "Second")
            (lispm-desktop-test--update-release work "v0.2.0")
            (let ((installed (lispm-desktop-test--update-install root "v0.2.0")))
              (should (equal (lispm-desktop-update-version installed) "0.2.0"))
              ;; A pull request branched from First, before the release.
              (lispm-desktop-test--git work "checkout" "--quiet" "-b" "fix" first)
              (lispm-desktop-test--update-commit work "fix.txt" "Mend the box")
              (lispm-desktop-test--git work "checkout" "--quiet" branch)
              (lispm-desktop-test--git work "merge" "--quiet" "--no-ff" "fix"
                                       "-m" "Merge pull request #26 from LispMachineLab/fix"
                                       "-m" "Stand the boxes clear of the corners")
              (lispm-desktop-test--update-push work "c.txt" "Straight to main")
              (lispm-desktop-test--update-release work "v0.2.1")
              (let ((result (lispm-desktop-test--update-check installed)))
                (should (eq (plist-get result :state) 'update))
                (should (equal (plist-get result :changes)
                               '("Straight to main" "Stand the boxes clear of the corners (#26)")))
                (should (lispm-desktop-update-install (list result)))
                (should (equal (lispm-desktop-update-version installed) "0.2.1")))))
        (delete-directory root t)))))

(ert-deftest lispm-desktop-test-update-leaves-checkouts-alone ()
  "A checkout with changes of its own, commits of its own, or on no branch
is left as it is; an update found is not installed once the checkout has
changes since."
  (skip-unless (executable-find "git"))
  (lispm-desktop-test--with-update
    (pcase-let ((`(,root ,checkout ,work) (lispm-desktop-test--update-repositories)))
      (unwind-protect
          (cl-flet ((reason () (let ((result (lispm-desktop-test--update-check checkout)))
                                 (should (eq (plist-get result :state) 'kept))
                                 (plist-get result :reason))))
            (lispm-desktop-test--update-push work "b.txt" "Second")
            (lispm-desktop-test--update-release work "v0.2.0")
            (with-temp-file (expand-file-name "a.txt" checkout) (insert "mine\n"))
            (should (equal (reason) "has changes of its own"))
            (lispm-desktop-test--git checkout "checkout" "--quiet" "--" "a.txt")
            (let ((result (lispm-desktop-test--update-check checkout))
                  (before (lispm-desktop-update--git checkout "rev-parse" "HEAD")))
              (should (eq (plist-get result :state) 'update))
              (with-temp-file (expand-file-name "a.txt" checkout) (insert "mine\n"))
              (should-not (lispm-desktop-update-install (list result)))
              (should (equal (lispm-desktop-update--git checkout "rev-parse" "HEAD") before))
              (should-not lispm-desktop-update--log))
            (lispm-desktop-test--git checkout "checkout" "--quiet" "--" "a.txt")
            (lispm-desktop-test--update-commit checkout "mine.txt" "Mine")
            (should (equal (reason) "has commits of its own"))
            (lispm-desktop-test--git checkout "checkout" "--quiet" "--detach")
            (should (equal (reason) "follows no branch")))
        (delete-directory root t)))))

(ert-deftest lispm-desktop-test-update-check-failure ()
  "A checkout whose repository cannot be reached is told of; the last check
then is not moved, so the next hour tries again.  A check that reached
every repository is kept for the next start."
  (skip-unless (executable-find "git"))
  (lispm-desktop-test--with-update
    (pcase-let ((`(,root ,checkout ,_work) (lispm-desktop-test--update-repositories)))
      (unwind-protect
          (cl-letf (((symbol-function 'lispm-desktop-update-checkouts)
                     (lambda () (list (cons "Test" checkout)))))
            (cl-flet ((check ()
                        (let ((done 'pending) (changes 0))
                          (add-hook 'lispm-desktop-update-changed-hook (lambda () (cl-incf changes)))
                          (lispm-desktop-update-check (lambda (updates) (setq done updates)))
                          (should (lispm-desktop-update-checking-p))
                          ;; Asked again meanwhile: told once the check running ends.
                          (lispm-desktop-update-check (lambda (_updates) (cl-incf changes 10)))
                          (with-timeout (30 (error "Software Update did not check"))
                            (while (eq done 'pending)
                              (accept-process-output nil 0.05)))
                          (should (= changes 12))
                          (setq lispm-desktop-update-changed-hook nil)
                          done)))
              (lispm-desktop-test--git checkout "remote" "set-url" "origin" (concat root "missing.git"))
              (should-not (check))
              (should (eq (plist-get (car (lispm-desktop-update-results)) :state) 'failed))
              (should (stringp (plist-get (car (lispm-desktop-update-results)) :reason)))
              (should-not (lispm-desktop-update-last-check))
              (lispm-desktop-test--git checkout "remote" "set-url" "origin" (concat root "origin.git"))
              (should-not (check))
              (should (lispm-desktop-update-last-check))
              (let ((checked (lispm-desktop-update-last-check)))
                (setq lispm-desktop-update--checked nil)
                (lispm-desktop-update--load)
                (should (equal (lispm-desktop-update-last-check) checked)))))
        (delete-directory root t)))))

(ert-deftest lispm-desktop-test-update-app-needs-installer ()
  "An update changing install.sh, in the desktop's checkout, needs install.sh
while LispM.app runs: it is installed as LispM quits only when Restart to
Update asked for it, and install.sh is then run, after."
  (skip-unless (executable-find "git"))
  (lispm-desktop-test--with-update
    (pcase-let ((`(,root ,checkout ,work) (lispm-desktop-test--update-repositories)))
      (unwind-protect
          (let ((ran nil))
            (cl-letf (((symbol-function 'lispm-desktop-update--desktop-p) #'always)
                      ((symbol-function 'lispm-desktop-update--app) (lambda () "/Applications/LispM.app"))
                      ((symbol-function 'lispm-desktop-update--run-installer)
                       (lambda (app reopen) (setq ran (list app reopen)))))
              (lispm-desktop-test--update-push work "install.sh" "Use a newer Emacs")
              (lispm-desktop-test--update-release work "v0.2.0")
              (let ((result (lispm-desktop-test--update-check checkout)))
                (should (plist-get result :app))
                (setq lispm-desktop-update--results (list result))
                (should (lispm-desktop-update-needs-installer-p))
                (should-not (lispm-desktop-update-installs-on-quit-p))
                (lispm-desktop-update--install-on-quit)
                (should-not ran)
                (should (equal (lispm-desktop-update--git checkout "rev-parse" "HEAD") (plist-get result :from)))
                (setq lispm-desktop-update--requested t
                      lispm-desktop-update--reopen t)
                (should (lispm-desktop-update-installs-on-quit-p))
                (lispm-desktop-update--install-on-quit)
                (should (equal ran '("/Applications/LispM.app" t)))
                (should (equal (lispm-desktop-update--git checkout "rev-parse" "HEAD") (plist-get result :to)))
                (should-not (lispm-desktop-update-updates))))
            ;; Out of LispM.app, as in a developer's own Emacs, the files are all.
            (cl-letf (((symbol-function 'lispm-desktop-update--app) #'ignore))
              (setq lispm-desktop-update--results (list '(:state update :app t)))
              (should-not (lispm-desktop-update-needs-installer-p))))
        (delete-directory root t)))))

(ert-deftest lispm-desktop-test-update-asking-waits ()
  "With Ask me before installing updates on, nothing installs as LispM
quits until Restart to Update asked for it; a batch Emacs never installs."
  (lispm-desktop-test--with-update
    (let ((installed 0))
      (cl-letf (((symbol-function 'lispm-desktop-update-install) (lambda (&rest _) (cl-incf installed)))
                ((symbol-function 'lispm-desktop-update--app) #'ignore))
        (setq lispm-desktop-update--results (list '(:name "Test" :state update))
              lispm-desktop-update-install-automatically nil)
        (lispm-desktop-update--install-on-quit)
        (should (= installed 0))
        (setq lispm-desktop-update--requested t)
        (lispm-desktop-update--install-on-quit)
        (should (= installed 1))
        (lispm-desktop-update--quit)
        (should (= installed 1))
        (setq lispm-desktop-update--results (list '(:name "Test" :state kept)))
        (should-not (lispm-desktop-update-installs-on-quit-p))))))

(ert-deftest lispm-desktop-test-update-schedule ()
  "A check by itself is due once a day, or a week, has passed since the
last; with checking by itself off, none is made."
  (lispm-desktop-test--with-update
    (let ((checks 0))
      (cl-letf (((symbol-function 'lispm-desktop-update-check) (lambda (&rest _) (cl-incf checks))))
        (should (lispm-desktop-update--due-p))
        (setq lispm-desktop-update--checked (current-time))
        (should-not (lispm-desktop-update--due-p))
        (setq lispm-desktop-update--checked (time-subtract nil (* 2 24 60 60)))
        (should (lispm-desktop-update--due-p))
        (setq lispm-desktop-update-schedule 'weekly)
        (should-not (lispm-desktop-update--due-p))
        (setq lispm-desktop-update--checked (time-subtract nil (* 8 24 60 60)))
        (should (lispm-desktop-update--due-p))
        (lispm-desktop-update--tick)
        (should (= checks 1))
        (setq lispm-desktop-update-check-automatically nil)
        (lispm-desktop-update--tick)
        (should (= checks 1))))))

(ert-deftest lispm-desktop-test-update-announces-once ()
  "Updates a check of its own found are told of once: in a line when they
install as LispM quits, else by an alert, once Emacs is idle."
  (lispm-desktop-test--with-update
    (let ((said nil) (idle nil)
          (updates (list '(:name "LispM Desktop" :state update :release "v0.3.0" :to "abc"
                                 :changes ("A" "B")))))
      (cl-letf (((symbol-function 'message) (lambda (format &rest args) (push (apply #'format format args) said)))
                ((symbol-function 'run-with-idle-timer) (lambda (&rest args) (push args idle)))
                ((symbol-function 'lispm-desktop-update--app) #'ignore))
        (setq lispm-desktop-update--results updates)
        (lispm-desktop-update--announce updates)
        (lispm-desktop-update--announce updates)
        (should (= (length said) 1))
        (should (string-search "LispM Desktop 0.3.0, 2 changes" (car said)))
        (should-not idle)
        (setq lispm-desktop-update--announced nil
              lispm-desktop-update-install-automatically nil)
        (lispm-desktop-update--announce updates)
        (should (= (length said) 1))
        (should (equal (cddr (car idle)) '(lispm-desktop-update--offer nil)))))))

(ert-deftest lispm-desktop-test-update-offer ()
  "The alert offering to restart has Restart as its default only when the
check was asked for; without its window, a check of its own only says so."
  (lispm-desktop-test--with-update
    (let ((buttons nil) (restarted 0) (said nil))
      (cl-letf (((symbol-function 'lispm-desktop-popup-alert)
                 (lambda (_icon _message choices &optional _detail) (setq buttons choices) 'restart))
                ((symbol-function 'lispm-desktop-popup-available-p) #'always)
                ((symbol-function 'lispm-desktop-update-restart) (lambda () (cl-incf restarted)))
                ((symbol-function 'lispm-desktop-update--app) #'ignore)
                ((symbol-function 'message) (lambda (&rest args) (push args said))))
        (setq lispm-desktop-update--results (list '(:name "LispM Agent" :state update :changes ("A"))))
        (lispm-desktop-update--offer t)
        (should (= restarted 1))
        (should (eq (nth 2 (assoc "Restart" buttons)) 'default))
        (lispm-desktop-update--offer nil)
        (should (= restarted 2))
        (should-not (nth 2 (assoc "Restart" buttons)))
        (cl-letf (((symbol-function 'lispm-desktop-popup-available-p) #'ignore))
          (setq buttons nil)
          (lispm-desktop-update--offer nil)
          (should-not buttons)
          (should said))))))

(ert-deftest lispm-desktop-test-update-checkouts ()
  "The checkouts are the projects' git checkouts, each once; a project
installed as a package is left out."
  (skip-unless (executable-find "git"))
  (let* ((root (file-name-as-directory (make-temp-file "lispm-desktop-test-update-projects" t)))
         (package-user-dir (concat root "elpa/"))
         (lispm-desktop-update-projects '(("One" . "one.el") ("Two" . "two.el") ("Package" . "three.el")))
         (files `(("one.el" . ,(concat root "repo/lisp/one.el"))
                  ("two.el" . ,(concat root "repo/lisp/two.el"))
                  ("three.el" . ,(concat root "elpa/three/three.el")))))
    (unwind-protect
        (progn
          (dolist (file files)
            (make-directory (file-name-directory (cdr file)) t)
            (write-region "" nil (cdr file)))
          (lispm-desktop-test--git (concat root "repo") "init" "--quiet")
          (lispm-desktop-test--git (concat root "elpa/three") "init" "--quiet")
          (cl-letf (((symbol-function 'locate-library)
                     (lambda (file &rest _) (cdr (assoc file files)))))
            (should (equal (lispm-desktop-update-checkouts)
                           (list (cons "One" (file-name-as-directory (file-truename (concat root "repo")))))))))
      (delete-directory root t))))

(ert-deftest lispm-desktop-test-update-installer-command ()
  "install.sh runs once this Emacs has quit, for the checkouts' home and
the application's folder, opening LispM after only when restarting."
  (let* ((lispm-desktop-directory "/Users/me/.lispm/lispm-desktop/")
         (command (lispm-desktop-update--installer-command "/Applications/LispM.app" t)))
    (should (string-search (format "kill -0 %d" (emacs-pid)) command))
    (should (string-search "LISPM_HOME=/Users/me/.lispm " command))
    (should (string-search "LISPM_APP_DIR=/Applications " command))
    (should (string-search "LISPM_OPEN=1" command))
    (should (string-search "/Users/me/.lispm/lispm-desktop/install.sh" command))
    (should (string-search "LISPM_OPEN=0" (lispm-desktop-update--installer-command "/Applications/LispM.app" nil)))))

(ert-deftest lispm-desktop-test-settings-update-panel ()
  "The Software Update panel offers Update Now, what the check found and
Restart to Update, how it checks and installs by itself, and what is
installed; its check boxes save their choices."
  ;; Drawn as SVG: an Emacs without librsvg cannot.
  (skip-unless (image-type-available-p 'svg))
  (lispm-desktop-test--with-update
    (let ((saved nil))
      (cl-letf (((symbol-function 'lispm-desktop-update-installed)
                 (lambda () '(("LispM Desktop" "/x/" "0.2.0+3, 626d068"))))
                ((symbol-function 'lispm-desktop-update--app) #'ignore)
                ((symbol-function 'customize-save-variable)
                 (lambda (variable value) (push (cons variable value) saved))))
        (setq lispm-desktop-update--checked (current-time)
              lispm-desktop-update--results
              (list '(:name "LispM Desktop" :directory "/x/" :state update :release "v0.3.0"
                      :changes ("Mend the menu" "Draw it"))))
        (with-current-buffer (lispm-desktop-settings--buffer)
          (lispm-desktop-settings-select 'update)
          (let ((text (concat (buffer-string)
                              (mapconcat #'identity
                                         (delq nil (mapcar (lambda (position)
                                                             (get-text-property position 'lispm-desktop-platinum-label))
                                                           (number-sequence (point-min) (1- (point-max)))))
                                         " "))))
            (dolist (label '("Update Now" "LispM Desktop 0.3.0, 2 changes" "Mend the menu" "Restart to Update"
                             "or as LispM quits" "Check for updates automatically"
                             "Every day" "Every week" "Ask me before installing updates" "0.2.0+3, 626d068"
                             "Show Log"))
              (should (string-search label text))))
          (should (equal lispm-desktop-platinum-title "Software Update"))
          (let ((button (next-button (point-min))))
            (while (and button (not (string-search "Ask me" (button-label button))))
              (setq button (next-button (button-end button))))
            (should button)
            (button-activate button))
          (should (equal (car saved) '(lispm-desktop-update-install-automatically))))))))

;;;; Applications' interface

(defun lispm-desktop-test-app-start-game () "Start." (interactive))
(defun lispm-desktop-test-app-pause-game () "Pause." (interactive))
(defun lispm-desktop-test-app-undo () "Undo." (interactive))
(defun lispm-desktop-test-app-left () "Left." (interactive))
(defun lispm-desktop-test-app-click () "Click." (interactive))
(defvar lispm-desktop-test-app-score 1234)

(defvar-keymap lispm-desktop-test-app-mode-map
  "n" #'lispm-desktop-test-app-start-game
  "p" #'lispm-desktop-test-app-pause-game
  "u" #'lispm-desktop-test-app-undo
  "<left>" #'lispm-desktop-test-app-left
  "<mouse-1>" #'lispm-desktop-test-app-click
  "?" #'describe-mode)

(define-derived-mode lispm-desktop-test-app-mode special-mode "Test App"
  "A game's mode, for the applications' interface's tests.")

(define-derived-mode lispm-desktop-test-app-derived-mode lispm-desktop-test-app-mode "Derived"
  "A mode derived from the test game's.")

(ert-deftest lispm-desktop-test-app-ui-finds-commands ()
  "An application's own commands are found in its keymap, with the keys
typing them; Emacs's own of its parent keymap and those of the mouse
alone are not."
  (with-temp-buffer
    (lispm-desktop-test-app-mode)
    (let ((commands (lispm-desktop-app-ui-commands)))
      (should (seq-set-equal-p (mapcar #'car commands)
                               '(lispm-desktop-test-app-start-game lispm-desktop-test-app-pause-game
                                 lispm-desktop-test-app-undo lispm-desktop-test-app-left)))
      (should (equal (cdr (assq 'lispm-desktop-test-app-start-game commands)) (list [?n])))
      (should (equal (cdr (assq 'lispm-desktop-test-app-left commands)) '([left]))))))

(ert-deftest lispm-desktop-test-app-ui-labels ()
  "Commands are named as a menu names them: the application's prefix off,
words capitalized but the small ones, acronyms in capitals."
  (with-temp-buffer
    (lispm-desktop-test-app-mode)
    (should (equal (lispm-desktop-app-ui-command-label 'lispm-desktop-test-app-start-game) "Start Game")))
  (should (equal (lispm-desktop-app-ui-label "browse-url") "Browse URL"))
  (should (equal (lispm-desktop-app-ui-label "move-to-the-left") "Move to the Left"))
  (should (equal (lispm-desktop-app-ui-label "5x5-new") "5x5 New")))

(ert-deftest lispm-desktop-test-app-ui-found-buttons-and-status ()
  "Without a skin, an application gets buttons for its commands starting,
pausing and undoing, and its score on its status, found by their names."
  (let ((lispm-desktop-app-ui-skins nil))
    (with-temp-buffer
      (lispm-desktop-test-app-mode)
      (should (equal (lispm-desktop-app-ui-buttons)
                     '(("New Game" lispm-desktop-test-app-start-game t)
                       ("Pause" lispm-desktop-test-app-pause-game t)
                       ("Undo" lispm-desktop-test-app-undo t))))
      (should (equal (lispm-desktop-app-ui-status) '("Score 1,234"))))))

(ert-deftest lispm-desktop-test-app-ui-skins ()
  "A skin says what is not found out; a mode derived from its mode takes it,
labels and status are read as the window is drawn, and one failing, as
before its game is set up, says nothing rather than signalling."
  (let ((lispm-desktop-app-ui-skins nil)
        (paused nil))
    (lispm-desktop-app-ui-define 'lispm-desktop-test-app-mode
      :status (lambda () (list "Fine" nil ""))
      :buttons `((,(lambda () (if paused "Resume" "Pause")) ignore ,(lambda () paused))
                 (,(lambda () (error "Not yet")) ignore)))
    (with-temp-buffer
      (lispm-desktop-test-app-derived-mode)
      (should (equal (lispm-desktop-app-ui-status) '("Fine")))
      (should (equal (lispm-desktop-app-ui-buttons) '(("Pause" ignore nil) ("…" ignore t))))
      (setq paused t)
      (should (equal (car (lispm-desktop-app-ui-buttons)) '("Resume" ignore t))))
    (lispm-desktop-app-ui-define 'lispm-desktop-test-app-mode :status (lambda () (error "Not yet")))
    (with-temp-buffer
      (lispm-desktop-test-app-mode)
      (should-not (lispm-desktop-app-ui-status)))))

(ert-deftest lispm-desktop-test-app-ui-skin-of-an-application ()
  "An application whose buffer has no mode of its own is described by its
key, and :run, said by a skin naming it with :app, opens it."
  (let ((lispm-desktop-app-ui-skins nil)
        (lispm-desktop-app-ui--buffers nil)
        (ran nil))
    (lispm-desktop-app-ui-define 'pong :title "Pong!")
    (lispm-desktop-app-ui-define 'lispm-desktop-test-app-mode :app 'test :run (lambda () (setq ran 'run)))
    (with-temp-buffer
      (setq lispm-desktop-app-ui--app 'pong)
      (should (equal (lispm-desktop-app-ui-get :title) "Pong!")))
    (lispm-desktop-app-ui-run 'test 'ignore)
    (should (eq ran 'run))
    (setq ran nil)
    (lispm-desktop-app-ui-run 'other (lambda () (interactive) (setq ran 'command)))
    (should (eq ran 'command))))

(ert-deftest lispm-desktop-test-app-ui-reopens-on-its-buffer ()
  "An application opened again shows the buffer its window showed when it
closed, as a game is found where it was left; once that buffer is gone,
its command runs again."
  (let ((lispm-desktop-app-ui-skins nil)
        (ran nil))
    (with-temp-buffer
      (let ((lispm-desktop-app-ui--buffers (list (cons 'test (current-buffer)))))
        (save-window-excursion
          (lispm-desktop-app-ui-run 'test (lambda () (interactive) (setq ran t)))
          (should (eq (window-buffer) (current-buffer))))
        (should-not ran)))
    (let ((lispm-desktop-app-ui--buffers (list (cons 'test (generate-new-buffer "gone")))))
      (kill-buffer (cdar lispm-desktop-app-ui--buffers))
      (lispm-desktop-app-ui-run 'test (lambda () (interactive) (setq ran t)))
      (should ran))))

(ert-deftest lispm-desktop-test-app-ui-keys-window ()
  "An application's keys window lists each key cap and what it does, and
has no bottom saying where point is in the list."
  (with-temp-buffer
    (lispm-desktop-app-ui-keys-mode)
    (should-not mode-line-format)
    (let ((inhibit-read-only t))
      (lispm-desktop-app-ui--insert-keys '(("p" . "Pause or resume") ("←  →" . "Move"))))
    (should (string-search "Pause or resume" (buffer-string)))
    (should (= (count-lines (point-min) (point-max)) 3))))

(ert-deftest lispm-desktop-test-app-ui-questions-in-alerts ()
  "A question asked in an application's window is asked in an alert,
without its (y or n); asked elsewhere, as Emacs asks it."
  (let ((asked nil))
    (cl-letf (((symbol-function 'lispm-desktop-popup-available-p) #'always)
              ((symbol-function 'lispm-desktop-popup-alert)
               (lambda (icon message buttons &optional _detail)
                 (setq asked (list icon message (mapcar #'car buttons)))
                 t)))
      (should (eq (lispm-desktop-app-ui--asking (lambda (&rest _) 'emacs) "Start a new game? (y or n) ")
                  'emacs))
      (should-not asked)
      (cl-letf (((symbol-function 'lispm-desktop-app-ui-app) (lambda (&optional _) 'test)))
        (should (lispm-desktop-app-ui--asking #'ignore "Start a new game? (y or n) "))))
    (should (equal asked '(caution "Start a new game?" ("Yes" "No"))))))

(ert-deftest lispm-desktop-test-app-ui-letters-for-arrows ()
  "Where an application binds the arrows and not C-b, C-f, C-p, C-n, the
letters do what its arrows do, as an HHKB has no arrows."
  (with-temp-buffer
    (lispm-desktop-test-app-mode)
    (let ((map (lispm-desktop-app-ui--letters-map)))
      (should (eq (keymap-lookup map "C-b") 'lispm-desktop-test-app-left))
      (should-not (keymap-lookup map "C-f")))))

(ert-deftest lispm-desktop-test-app-ui-menus ()
  "An application's menus are File, its own, here one of its commands under
Game, a game's, and Help; their items run in its window, showing the keys
typing them there."
  (let ((lispm-desktop-app-ui-skins nil)
        (lispm-desktop-apps (list (list 'test :name "Test" :category 'games)))
        (ran nil))
    (save-window-excursion
      (with-temp-buffer
        (lispm-desktop-test-app-mode)
        (switch-to-buffer (current-buffer))
        (let ((window (selected-window)))
          (cl-letf (((symbol-function 'lispm-desktop-app-ui-window) (lambda (_) window))
                    ((symbol-function 'lispm-desktop-test-app-start-game)
                     (lambda () (interactive) (setq ran (current-buffer)))))
            (let* ((menus (lispm-desktop-app-ui-menus 'test))
                   (game (funcall (nth 2 (nth 1 menus))))
                   (file (funcall (nth 2 (nth 0 menus)))))
              (should (equal (mapcar #'cadr menus) '("File" "Game" "Help")))
              (should (equal (plist-get (car game) :label) "Start Game"))
              (should (equal (plist-get (car game) :key) "n"))
              (should (equal (plist-get (car file) :label) "New Game"))
              (with-temp-buffer
                (funcall (plist-get (car game) :action)))
              (should (eq ran (window-buffer window))))))))))

(ert-deftest lispm-desktop-test-app-ui-store-applications ()
  "An application the App Store installs gets the interface's menus, its
category and its description, from which the interface tells a game."
  (let ((lispm-desktop-apps nil))
    (lispm-desktop-store--register-app
     'calc '(:kind app :name "Calculator" :command calc :category utilities :description "Calc."))
    (let ((app (alist-get 'calc lispm-desktop-apps)))
      (should (eq (plist-get app :category) 'utilities))
      (should (equal (plist-get app :description) "Calc."))
      (should (equal (mapcar #'cadr (funcall (plist-get app :menus))) '("File" "Help"))))
    (should-not (lispm-desktop-app-ui-game-p 'calc))))

(ert-deftest lispm-desktop-test-platinum-info-placards ()
  "A window's bottom shows each string its info returns on a placard of its
own, and none when it returns nil, not the buffer's name."
  (with-temp-buffer
    (rename-buffer "lispm-desktop-test-info" t)
    (setq-local lispm-desktop-platinum-info (lambda () '("Score 1" "Rows 2")))
    (let ((left (lispm-desktop-platinum-mode-line-left)))
      (should (string-search "Score 1" left))
      (should (string-search "Rows 2" left)))
    (setq-local lispm-desktop-platinum-info #'ignore)
    (should-not (string-search "lispm-desktop-test-info" (lispm-desktop-platinum-mode-line-left)))))

(ert-deftest lispm-desktop-test-app-board-grid-hits ()
  "A board's grid is drawn as wide and tall as its cells and edges, and a
click plays the cell under it, nothing outside the cells."
  (let* ((played nil)
         (board (list :size 20 :click (lambda (row column) (setq played (list row column)))))
         (drawn (lispm-desktop-app-board-draw-grid board lispm-desktop-platinum-current-palette
                                                   '((a b c) (d e f)) '(1 . 2)))
         (svg (car drawn))
         (hit (cdr drawn))
         (left (+ lispm-desktop-app-board-margin 6)))
    (should (= (dom-attr svg 'width) (+ (* 2 left) 60)))
    (should (= (dom-attr svg 'height) (+ (* 2 left) 40)))
    (funcall (funcall hit (+ left 45) (+ left 25)))
    (should (equal played '(1 2)))
    (should-not (funcall hit 2 2))
    (should-not (funcall hit (+ left 61) (+ left 5)))))

(ert-deftest lispm-desktop-test-app-board-waits-for-its-game ()
  "A board whose game cannot say yet what it shows is not drawn, and its
text stays in sight, rather than its commands failing."
  (with-temp-buffer
    (insert "text")
    (setq-local lispm-desktop-app-board (list :cells (lambda () (error "No board yet"))))
    (should-not (lispm-desktop-app-board--model))
    (lispm-desktop-app-board-refresh)
    (should-not lispm-desktop-app-board--overlay)))

(ert-deftest lispm-desktop-test-app-board-reads-text ()
  "Boards read their games' text by line and column, counted from 0."
  (with-temp-buffer
    (insert "abc\n  de\n")
    (should (eq (lispm-desktop-app-board-char 1 3) ?e))
    (should-not (lispm-desktop-app-board-char 1 9))
    (should-not (lispm-desktop-app-board-char 5 0))
    (should (equal (lispm-desktop-app-board-line-column (lispm-desktop-app-board-position 1 2)) '(1 . 2)))))

(ert-deftest lispm-desktop-test-app-games-5x5-cells ()
  "5x5's board shows its filled squares, and those a solution presses
while it holds."
  (require '5x5)
  (let ((5x5-grid (vector (vector t nil) (vector nil t)))
        (5x5-grid-size 2)
        (5x5-moves 3)
        (5x5-solver-output nil))
    (should (equal (lispm-desktop-app-games--5x5-cells) '((on off) (off on))))
    (setq 5x5-solver-output (list 3 (cons 0 (vector (vector nil t) (vector nil nil)))))
    (should (equal (lispm-desktop-app-games--5x5-cells) '((on off-hint) (off on))))
    (setq 5x5-moves 4)
    (should (equal (lispm-desktop-app-games--5x5-cells) '((on off) (off on))))))

(ert-deftest lispm-desktop-test-app-games-gomoku-cells ()
  "Gomoku's board shows the stones of both players, the last one marked."
  (require 'gomoku)
  (let* ((gomoku-board-width 3)
         (gomoku-board-height 2)
         (gomoku-board (make-vector (* 4 4) 0))
         (gomoku-game-history (list (cons (gomoku-xy-to-index 3 2) 0))))
    (aset gomoku-board (gomoku-xy-to-index 1 1) 1)
    (aset gomoku-board (gomoku-xy-to-index 3 2) 6)
    (should (equal (lispm-desktop-app-games--gomoku-cells) '((x empty empty) (empty empty o-last))))))

(ert-deftest lispm-desktop-test-app-games-blackbox-cells ()
  "Blackbox's board is its box in its edges: what came out where it was
shot into, the balls guessed, the corners blank."
  (require 'blackbox)
  (with-temp-buffer
    (let ((bb-board nil) (bb-balls-placed nil))
      (bb-insert-board)
      (let ((inhibit-read-only t))
        (bb-goto '(-1 . 2))
        (delete-char 1)
        (insert "H")
        (bb-goto '(3 . 4))
        (delete-char 1)
        (insert "O"))
      (let ((cells (lispm-desktop-app-games--blackbox-cells)))
        (should (eq (nth 0 (nth 0 cells)) 'corner))
        (should (equal (nth 0 (nth 3 cells)) '(edge . "H")))
        (should (equal (nth 1 (nth 0 cells)) '(edge)))
        (should (eq (nth 4 (nth 5 cells)) 'ball))
        (should (eq (nth 1 (nth 1 cells)) 'empty))))))

(ert-deftest lispm-desktop-test-app-games-mpuz-cells ()
  "Mpuz's multiplication shows each square's letter, or its digit once
found, the times sign beside the multiplier."
  (require 'mpuz)
  (let ((mpuz-board (make-vector 10 nil))
        (mpuz-digit-to-letter (vector 9 8 7 6 5 4 3 2 1 0))
        (mpuz-found-digits (make-bool-vector 10 nil))
        (mpuz-trivial-digits (make-bool-vector 10 nil)))
    (aset mpuz-board 3 '((2 . 9) (10 . 1)))
    (aset mpuz-found-digits 5 t)
    (aset mpuz-board 5 '((4 . 7)))
    (let ((cells (lispm-desktop-app-games--mpuz-cells)))
      (should (equal (nth 4 (nth 0 cells)) '("G" unsolved)))
      (should (equal (nth 0 (nth 4 cells)) '("G" unsolved)))
      (should (equal (nth 3 (nth 1 cells)) '("5" solved)))
      (should (equal (nth 2 (nth 1 cells)) '("×" sign))))))

(ert-deftest lispm-desktop-test-app-skins-calculator-entry ()
  "The Calculator's keypad types a number into its display: one point at
most, the sign changed in front, the last digit taken back."
  (with-temp-buffer
    (dolist (key '("1" "." "." "5"))
      (lispm-desktop-app-skins-calc-press key))
    (should (equal lispm-desktop-app-skins--calc-entry "1.5"))
    (lispm-desktop-app-skins-calc-press 'sign)
    (should (equal lispm-desktop-app-skins--calc-entry "-1.5"))
    (lispm-desktop-app-skins-calc-press 'delete)
    (should (equal lispm-desktop-app-skins--calc-entry "-1."))
    (lispm-desktop-app-skins-calc-press 'clear)
    (should (equal lispm-desktop-app-skins--calc-entry ""))))

;;;; The chat applications

(ert-deftest lispm-desktop-test-app-chat-count ()
  "A conversation's count of messages unread: none without, bold while
unread, in red where they name you, as many as name you where that is
known, else as many as are unread."
  (should-not (lispm-desktop-app-chat--count '(:unread 0)))
  (should-not (lispm-desktop-app-chat--count '(:unread t :mentions 0)))
  (let ((count (lispm-desktop-app-chat--count '(:unread 3))))
    (should (equal count "3"))
    (should (eq (get-text-property 0 'face count) 'lispm-desktop-app-chat-unread)))
  (let ((count (lispm-desktop-app-chat--count '(:unread 3 :mentions 1))))
    (should (equal count "1"))
    (should (eq (get-text-property 0 'face count) 'lispm-desktop-app-chat-mentions)))
  (should (equal (lispm-desktop-app-chat--count '(:unread 2 :mentions t)) "2"))
  (should (equal (lispm-desktop-app-chat--count '(:unread t :mentions 4)) "4")))

(ert-deftest lispm-desktop-test-app-chat-line ()
  "A network's line holds its name in bold, and what it says of it; a
conversation's is indented under it, its name bold while it is unread."
  (let ((heading (lispm-desktop-app-chat-line '(:name "Libera.Chat" :heading t :state "connecting…")))
        (read (lispm-desktop-app-chat-line '(:name "#emacs" :unread 0)))
        (unread (lispm-desktop-app-chat-line '(:name "#lispm" :unread 2))))
    (should (equal (substring-no-properties heading) " Libera.Chat  connecting…"))
    (should (eq (get-text-property 1 'face heading) 'lispm-desktop-app-chat-heading))
    (should (equal (substring-no-properties read) "    #emacs"))
    (should-not (get-text-property 4 'face read))
    (should (string-prefix-p "    #lispm " (substring-no-properties unread)))
    (should (eq (get-text-property 4 'face unread) 'lispm-desktop-app-chat-unread))
    (should (string-suffix-p "2" (substring-no-properties unread)))))

(defvar lispm-desktop-test--conversations nil
  "The conversations of the list of `lispm-desktop-test-chat-list-mode'.")

(define-derived-mode lispm-desktop-test-chat-list-mode lispm-desktop-app-chat-list-mode "Test"
  "A list of conversations for the tests."
  (setq-local lispm-desktop-app-chat-conversations-function (lambda () lispm-desktop-test--conversations)))

(ert-deftest lispm-desktop-test-app-chat-refresh ()
  "The list is drawn again only where its conversations changed, point
staying on the conversation it was on; a buffer that is no list is left
alone."
  (let ((lispm-desktop-test--conversations
         '((:key net :name "Net" :heading t) (:key a :name "#a" :unread 0) (:key b :name "#b" :unread 1))))
    (with-temp-buffer
      (lispm-desktop-test-chat-list-mode)
      (lispm-desktop-app-chat-refresh)
      (should (equal (count-lines (point-min) (point-max)) 3))
      (forward-line 2)
      (should (eq (plist-get (lispm-desktop-app-chat-conversation) :key) 'b))
      (let ((tick (buffer-modified-tick)))
        (lispm-desktop-app-chat-refresh)
        (should (= tick (buffer-modified-tick))))
      (setq lispm-desktop-test--conversations
            (append (seq-take lispm-desktop-test--conversations 1)
                    '((:key c :name "#c" :unread 0))
                    (cdr lispm-desktop-test--conversations)))
      (lispm-desktop-app-chat-refresh)
      (should (equal (count-lines (point-min) (point-max)) 4))
      (should (eq (plist-get (lispm-desktop-app-chat-conversation) :key) 'b))
      (should (equal (lispm-desktop-app-chat-unread-status lispm-desktop-test--conversations) "1 unread"))))
  (with-temp-buffer
    (insert "not a list")
    (let ((lispm-desktop-app-chat-conversations-function (lambda () '((:key x :name "x")))))
      (lispm-desktop-app-chat-refresh t))
    (should (equal (buffer-string) "not a list"))))

(defcustom lispm-desktop-test-chat-option 'standard
  "An option of a chat application, for the tests."
  :type 'symbol
  :group 'lispm-desktop)

(ert-deftest lispm-desktop-test-app-chat-set-options ()
  "A chat application's option is set as its window wants it while it is
as the package ships it, never where you set it or customized it."
  (let ((options '((lispm-desktop-test-chat-option desktop shipped))))
    (unwind-protect
        (progn
          (dolist (value '(standard shipped))
            (set-default 'lispm-desktop-test-chat-option value)
            (lispm-desktop-app-chat-set-options options)
            (should (eq lispm-desktop-test-chat-option 'desktop)))
          (set-default 'lispm-desktop-test-chat-option 'yours)
          (lispm-desktop-app-chat-set-options options)
          (should (eq lispm-desktop-test-chat-option 'yours))
          (set-default 'lispm-desktop-test-chat-option 'standard)
          (put 'lispm-desktop-test-chat-option 'saved-value '('standard))
          (lispm-desktop-app-chat-set-options options)
          (should (eq lispm-desktop-test-chat-option 'standard)))
      (put 'lispm-desktop-test-chat-option 'saved-value nil)
      (set-default 'lispm-desktop-test-chat-option 'standard))))

(ert-deftest lispm-desktop-test-app-erc-channel-name ()
  "A channel's name typed gets its # unless it has a channel's prefix."
  (should (equal (lispm-desktop-app-erc-channel-name "emacs") "#emacs"))
  (should (equal (lispm-desktop-app-erc-channel-name " #emacs ") "#emacs"))
  (should (equal (lispm-desktop-app-erc-channel-name "##lispm") "##lispm"))
  (should (equal (lispm-desktop-app-erc-channel-name "&local") "&local"))
  (should-error (lispm-desktop-app-erc-channel-name "  ") :type 'user-error))

(ert-deftest lispm-desktop-test-app-erc-keeps-networks ()
  "A network connected to is kept, and its channels as they are joined and
left, whatever their case; connecting to it again keeps its channels."
  (let ((lispm-desktop-app-erc-networks nil)
        (saves 0))
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (variable value) (cl-incf saves) (set variable value))))
      (lispm-desktop-app-erc--remember '(:server "irc.libera.chat" :port 6697 :tls t :nick "me"))
      (lispm-desktop-app-erc--keep-channel "IRC.libera.chat" "#emacs" t)
      (lispm-desktop-app-erc--keep-channel "irc.libera.chat" "#lispm" t)
      (should (= saves 3))
      (lispm-desktop-app-erc--keep-channel "irc.libera.chat" "#EMACS" t)
      (lispm-desktop-app-erc--keep-channel "irc.oftc.net" "#debian" t)
      (should (= saves 3))
      (lispm-desktop-app-erc--keep-channel "irc.libera.chat" "#Emacs" nil)
      (lispm-desktop-app-erc--remember '(:server "irc.libera.chat" :port 6697 :tls t :nick "me2"))
      (should (equal lispm-desktop-app-erc-networks
                     '((:server "irc.libera.chat" :port 6697 :tls t :nick "me2" :channels ("#lispm"))))))))

(ert-deftest lispm-desktop-test-app-erc-unread ()
  "A conversation's messages unread are as ERC tracks them, named where
the face ERC tracks them with is your nickname's."
  (require 'erc-track)
  (with-temp-buffer
    (let ((plain (current-buffer)))
      (with-temp-buffer
        (let ((named (current-buffer)))
          (dlet ((erc-modified-channels-alist `((,plain 4 . erc-default-face)
                                                (,named 2 erc-current-nick-face erc-default-face))))
            (should (equal (lispm-desktop-app-erc--unread plain) '(4 . nil)))
            (should (equal (lispm-desktop-app-erc--unread named) '(2 . t)))
            (with-temp-buffer
              (should (equal (lispm-desktop-app-erc--unread (current-buffer)) '(0 . nil))))))))))

(ert-deftest lispm-desktop-test-app-slack-keeps-secrets ()
  "A workspace's token and cookie are kept with auth-source, as
emacs-slack's manual keeps them, and read back; one kept already is left
as it is."
  (let* ((file (make-temp-file "lispm-desktop-test-authinfo"))
         (auth-sources (list file))
         (auth-source-save-behavior t)
         (workspace '(:name "lispm" :user "me@example.com"))
         (host (lispm-desktop-app-slack--host workspace)))
    (unwind-protect
        (progn
          (auth-source-forget-all-cached)
          (should (equal host "lispm.slack.com"))
          (should-not (lispm-desktop-app-slack--secret host "me@example.com"))
          (lispm-desktop-app-slack--keep-secret host "me@example.com" "xoxc-token")
          (lispm-desktop-app-slack--keep-secret host "me@example.com^cookie" "xoxd-cookie")
          (lispm-desktop-app-slack--keep-secret host "me@example.com" "xoxc-other")
          (should (equal (lispm-desktop-app-slack--secret host "me@example.com") "xoxc-token"))
          (should (equal (lispm-desktop-app-slack--secret host "me@example.com^cookie") "xoxd-cookie")))
      (auth-source-forget-all-cached)
      (delete-file file))))

(ert-deftest lispm-desktop-test-app-telega-missing ()
  "Telega's window says what it lacks first: TDLib, a TDLib new enough, a C
compiler, then telega-server built; nothing once it is built, or when
telega-server runs in Docker."
  (let* ((root (make-temp-file "lispm-desktop-test-telega" t))
         (prefix (expand-file-name "prefix" root))
         (version-file (expand-file-name "lib/cmake/Td/TdConfigVersion.cmake" prefix))
         (programs '("cc" "make")))
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (name &optional _remote)
                     (cond ((member name programs) (concat "/usr/bin/" name))
                           ((string-match-p "telega-server" name)
                            (let ((file (expand-file-name name (expand-file-name "telega" root))))
                              (and (file-exists-p file) file)))))))
          (dlet ((telega-use-docker nil)
                 (telega-directory (expand-file-name "telega" root))
                 (telega-server-command "telega-server")
                 (telega-server-libs-prefix (expand-file-name "nowhere" root))
                 (telega-tdlib-min-version "1.8.66")
                 (lispm-desktop-app-telega-tdlib-prefixes (list prefix)))
            (should (eq (lispm-desktop-app-telega-missing) 'tdlib))
            (should (string-search lispm-desktop-app-telega-tdlib-command (lispm-desktop-app-telega--page-text)))
            (should (equal (car (car (lispm-desktop-app-telega--page-buttons))) "Copy Command"))
            ;; As the catalog declares TDLib, the Installer installs it.
            (cl-letf (((symbol-function 'lispm-desktop-app-ui-needs)
                       (lambda (_app) '((library "TDLib" :for "Telegram" :brew "tdlib" :head t
                                                 :file "include/td/telegram/td_json_client.h")
                                        (program "cc" :for "building telega-server" :xcode t))))
                      ((symbol-function 'lispm-desktop-needs--brew-prefix) (lambda () root)))
              (should (equal (lispm-desktop-app-telega--page-buttons)
                             '(("Install TDLib…" lispm-desktop-app-telega-install)
                               ("Check Again" lispm-desktop-app-telega-check-again)))))
            (make-directory (expand-file-name "include/td/telegram" prefix) t)
            (write-region "" nil (expand-file-name "include/td/telegram/td_json_client.h" prefix))
            (make-directory (file-name-directory version-file) t)
            (write-region "set(PACKAGE_VERSION \"1.8.0\")\n" nil version-file)
            (should (equal (lispm-desktop-app-telega-tdlib-version prefix) "1.8.0"))
            (should (eq (lispm-desktop-app-telega-missing) 'old-tdlib))
            (write-region "set(PACKAGE_VERSION \"1.8.70\")\n" nil version-file)
            (setq programs nil)
            (should (eq (lispm-desktop-app-telega-missing) 'compiler))
            (setq programs '("cc" "make"))
            (should (eq (lispm-desktop-app-telega-missing) 'server))
            (should (equal (lispm-desktop-app-telega--page-buttons)
                           '(("Build telega-server" lispm-desktop-app-telega-build))))
            (make-directory telega-directory t)
            (write-region "" nil (expand-file-name "telega-server" telega-directory))
            (should-not (lispm-desktop-app-telega-missing))
            (delete-file (expand-file-name "telega-server" telega-directory))
            (setq telega-use-docker t)
            (should-not (lispm-desktop-app-telega-missing))))
      (delete-directory root t))))

(ert-deftest lispm-desktop-test-app-ui-every-application-described ()
  "Every application of the App Store's catalog is described where what is
found out of it is not enough, each board's functions defined."
  (dolist (key '(tetris-mode snake-mode pong 5x5-mode gomoku-mode solitaire-mode blackbox-mode
                 mpuz-mode bubbles-mode life-mode hanoi dun-mode doctor-mode calc-mode calendar-mode
                 elfeed-search-mode emms-browser-mode erc-mode slack-mode slack-buffer-mode
                 telega-root-mode telega-chat-mode lispm-desktop-app-erc-list-mode
                 lispm-desktop-app-slack-list-mode lispm-desktop-app-telega-setup-mode))
    (let ((skin (alist-get key lispm-desktop-app-ui-skins)))
      (should skin)
      (when-let* ((board (plist-get skin :board)))
        (dolist (property '(:cells :tiles :cursor :click :draw :model))
          (let ((function (plist-get board property)))
            (when (and function (symbolp function))
              (should (fboundp function)))))))))

;;;; Needs

(defconst lispm-desktop-test--needs
  '((program "lispm-test-player" :for "playing music" :brew "lispm-test-player"
             :instead ("lispm-test-stand-in") :more "seeking")
    (program "lispm-test-tagger" :for "reading tags" :brew "lispm-test-tagger"))
  "What an app's catalog says it needs, for the tests.")

(ert-deftest lispm-desktop-test-needs-declared ()
  "A catalog declares what an app needs: each of a kind, with what it is
for, what installs it and what may stand in for it; those malformed are
not taken."
  (should (lispm-desktop-needs-list-valid-p lispm-desktop-test--needs))
  (should (lispm-desktop-needs-list-valid-p nil))
  (should (lispm-desktop-needs-valid-p '(application "Spotify" :for "playing" :cask "spotify"
                                                     :bundle "com.spotify.client")))
  (should (lispm-desktop-needs-valid-p '(library "libvterm" :for "terminals" :brew "libvterm"
                                                 :file "lib/libvterm.dylib")))
  (should-not (lispm-desktop-needs-valid-p '(program "mpv" :brew "mpv")))
  (should-not (lispm-desktop-needs-valid-p '(widget "mpv" :for "playing")))
  (should-not (lispm-desktop-needs-valid-p '("mpv" :for "playing")))
  (should-not (lispm-desktop-needs-valid-p '(program "mpv" :for "playing" :instead "afplay")))
  (should-not (lispm-desktop-needs-valid-p '(program "mpv" :for "playing" :commands ("play"))))
  (should (lispm-desktop-needs-valid-p '(library "TDLib" :for "Telegram" :brew "tdlib" :head t)))
  (should-not (lispm-desktop-needs-valid-p '(library "TDLib" :for "Telegram" :brew "tdlib" :head "yes")))
  (should-not (lispm-desktop-store--item-valid-p
               '(emms :kind app :name "EMMS" :command emms :needs (("mpv"))))))

(ert-deftest lispm-desktop-test-needs-stand ()
  "A need is present, missing with a stand-in installed, or missing; what
is said of it says which, and the App Store installs those missing or
copies the commands installing them."
  (cl-letf (((symbol-function 'executable-find) (lambda (name &rest _) (equal name "lispm-test-stand-in"))))
    (pcase-let ((`(,player ,tagger) lispm-desktop-test--needs))
      (should (eq (lispm-desktop-needs-state player) 'instead))
      (should (eq (lispm-desktop-needs-state tagger) 'missing))
      (should (equal (lispm-desktop-needs-missing lispm-desktop-test--needs) (list tagger)))
      (should (equal (lispm-desktop-needs-missing lispm-desktop-test--needs t) lispm-desktop-test--needs))
      (should (equal (lispm-desktop-needs-text "EMMS" player)
                     "EMMS uses lispm-test-stand-in for playing music; lispm-test-player would add seeking."))
      (should (equal (lispm-desktop-needs-text "EMMS" tagger) "EMMS needs lispm-test-tagger for reading tags."))
      (should (equal (lispm-desktop-needs-install-command tagger) "brew install lispm-test-tagger")))
    (let ((props `(:kind app :name "EMMS" :description "Music." :needs ,lispm-desktop-test--needs)))
      (should (equal (lispm-desktop-store--install-commands props)
                     "brew install lispm-test-player && brew install lispm-test-tagger"))
      (should (equal (lispm-desktop-store--needs-to-install props) lispm-desktop-test--needs))
      (should (string-search "It needs lispm-test-tagger for reading tags." (lispm-desktop-store--description props)))
      (should (string-search "With it, lispm-test-tagger, for reading tags: brew install lispm-test-tagger."
                             (lispm-desktop-store--preview (cons 'emms props))))))
  (cl-letf (((symbol-function 'executable-find) #'always))
    (should-not (lispm-desktop-store--install-commands
                 `(:kind app :name "EMMS" :needs ,lispm-desktop-test--needs)))))

(ert-deftest lispm-desktop-test-needs-command-line-tools ()
  "A program of Apple's command line tools is missing while they are,
though its name on PATH is found: it is Apple's, offering the tools."
  (cl-letf (((symbol-function 'executable-find) #'always)
            ((symbol-function 'lispm-desktop-needs-command-line-tools-p) #'ignore))
    (should-not (lispm-desktop-needs-present-p '(program "git" :for "keeping history" :xcode t)))
    (should (lispm-desktop-needs-present-p '(program "git" :for "keeping history"))))
  (should (equal (lispm-desktop-needs-install-command '(program "git" :for "keeping history" :xcode t))
                 "xcode-select --install"))
  (should (equal (lispm-desktop-needs-install-command '(library "TDLib" :for "Telegram" :brew "tdlib" :head t))
                 "brew install --HEAD tdlib")))

(ert-deftest lispm-desktop-test-store-needs-from-the-catalog ()
  "An installed app needs what its catalog declares now, though it was
installed before the catalog declared it; the commands a need names are
guarded while the app is installed."
  (let ((lispm-desktop-store-installed '((emms :kind app :name "EMMS" :command emms-browser)))
        (lispm-desktop-store--catalogs
         `((:url "https://example.org/store"
            :items ((emms :kind app :name "EMMS" :command emms-browser
                          :needs ((program "lispm-test-player" :for "playing music" :brew "lispm-test-player"
                                           :commands (lispm-desktop-test--play))))))))
        (lispm-desktop-apps nil)
        (lispm-desktop-needs--guards nil))
    (defalias 'lispm-desktop-test--play #'ignore)
    (unwind-protect
        (progn
          (lispm-desktop-store--register-app 'emms (cdr (assq 'emms (lispm-desktop-store-items))))
          (should (equal (mapcar #'lispm-desktop-needs-name (lispm-desktop-app-ui-needs 'emms))
                         '("lispm-test-player")))
          (should (assq 'lispm-desktop-test--play lispm-desktop-needs--guards))
          (lispm-desktop-needs-unguard-all (lispm-desktop-store--needs 'emms))
          (should-not (assq 'lispm-desktop-test--play lispm-desktop-needs--guards)))
      (lispm-desktop-needs-unguard 'lispm-desktop-test--play)
      (fmakunbound 'lispm-desktop-test--play))))

(ert-deftest lispm-desktop-test-app-needs-in-help ()
  "An app's Help menu offers to install each need missing; one with
nothing standing in is offered as it opens, once a session."
  (let ((lispm-desktop-apps (list (list 'emms :name "EMMS" :needs (lambda () lispm-desktop-test--needs))))
        (lispm-desktop-app-ui--told nil)
        (asked nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (name &rest _) (equal name "lispm-test-stand-in")))
              ((symbol-function 'lispm-desktop-popup-alert)
               (lambda (_icon message &rest _) (push message asked) 'cancel)))
      (should (equal (seq-filter #'stringp (mapcar (lambda (item) (and (consp item) (plist-get item :label)))
                                                   (lispm-desktop-app-ui--help-items 'emms)))
                     '("EMMS Keys" "About EMMS" "Install lispm-test-player…" "Install lispm-test-tagger…")))
      (lispm-desktop-app-ui--offer-missing 'emms)
      (lispm-desktop-app-ui--offer-missing 'emms)
      (should (equal asked '("EMMS needs lispm-test-tagger.  Install it?"))))))

(ert-deftest lispm-desktop-test-needs-guard ()
  "A guarded command run while its need is missing offers to install it,
then runs once it is; called from code, it runs as it is."
  (let ((lispm-desktop-needs--guards nil)
        (ran 0)
        (installs nil))
    (defalias 'lispm-desktop-test--guarded (lambda () (interactive) (cl-incf ran)))
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find) #'ignore)
                  ((symbol-function 'lispm-desktop-popup-alert) (lambda (&rest _) 'install))
                  ((symbol-function 'lispm-desktop-needs-install)
                   (lambda (needs who then) (push (list (mapcar #'lispm-desktop-needs-name needs) who) installs)
                     (funcall then t))))
          (lispm-desktop-needs-guard 'lispm-desktop-test--guarded "EMMS" (list (cadr lispm-desktop-test--needs)))
          (lispm-desktop-test--guarded)
          (should (= ran 1))
          (should-not installs)
          (call-interactively #'lispm-desktop-test--guarded)
          (should (equal installs '((("lispm-test-tagger") "EMMS"))))
          (should (= ran 2)))
      (lispm-desktop-needs-unguard 'lispm-desktop-test--guarded)
      (fmakunbound 'lispm-desktop-test--guarded))))

;; A package's option set from what it found as it loaded, for the test.
(defvar lispm-desktop-test--found nil)

(ert-deftest lispm-desktop-test-needs-refresh ()
  "Once a need is there, the options holding it that a package left nil
as it loaded are set again from their definitions; others are left."
  (let ((need '(program "lispm-test-player" :for "playing music" :brew "lispm-test-player"
                        :variables (lispm-desktop-test--option lispm-desktop-test--found))))
    (cl-letf (((symbol-function 'executable-find) #'ignore))
      (custom-declare-variable 'lispm-desktop-test--option
                               '(and (executable-find "lispm-test-player") "lispm-test-player")
                               "An option holding a program." :type 'sexp :group 'lispm-desktop))
    (unwind-protect
        (progn
          (should-not lispm-desktop-test--option)
          (lispm-desktop-needs-refresh (list need))
          (should-not lispm-desktop-test--option)
          (cl-letf (((symbol-function 'executable-find) #'always))
            (lispm-desktop-needs-refresh (list need)))
          (should (equal lispm-desktop-test--option "lispm-test-player"))
          (should (equal lispm-desktop-test--found "lispm-test-player")))
      (setq lispm-desktop-test--found nil)
      (makunbound 'lispm-desktop-test--option))))

(defmacro lispm-desktop-test--with-brew (script &rest body)
  "Run BODY with a brew of SCRIPT, a shell script, alone on PATH beside the
programs it installs; BIN is its directory.  A program it installs is a
script it writes into BIN."
  (declare (indent 1))
  `(let* ((bin (make-temp-file "lispm-test-brew" t))
          (exec-path (list bin))
          (process-environment (cons (concat "PATH=" bin ":/bin:/usr/bin") process-environment))
          (lispm-desktop-needs--jobs nil)
          (lispm-desktop-needs--requests nil)
          (lispm-desktop-needs--homebrew nil)
          (lispm-desktop-needs--process nil))
     (ignore bin)
     (with-temp-file (expand-file-name "brew" bin)
       (insert "#!/bin/sh\nBIN=" (shell-quote-argument bin) "\n" ,script))
     (set-file-modes (expand-file-name "brew" bin) #o755)
     (unwind-protect
         (cl-letf (((symbol-function 'lispm-desktop-app-window-close) #'ignore))
           ,@body)
       (when (process-live-p lispm-desktop-needs--process)
         (delete-process lispm-desktop-needs--process))
       (delete-directory bin t))))

(defun lispm-desktop-test--wait (predicate)
  "Wait until PREDICATE returns non-nil, ten seconds at most."
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))))

(ert-deftest lispm-desktop-test-needs-install-one-at-a-time ()
  "Installs run one at a time, in order, with Homebrew; once they all
end, what waited for them is called back."
  (lispm-desktop-test--with-brew
      "echo \"start $3\" >> $BIN/log; sleep 0.2
printf '#!/bin/sh\\n' > $BIN/$3; chmod +x $BIN/$3; echo \"end $3\" >> $BIN/log; echo \"Pouring $3\"\n"
    (let (answer)
      (lispm-desktop-needs-install lispm-desktop-test--needs "EMMS" (lambda (present) (setq answer (list present))))
      (lispm-desktop-test--wait (lambda () answer))
      (should (equal answer '(t)))
      (should (equal (with-temp-buffer (insert-file-contents (expand-file-name "log" bin))
                                       (split-string (buffer-string) "\n" t))
                     '("start lispm-test-player" "end lispm-test-player"
                       "start lispm-test-tagger" "end lispm-test-tagger")))
      (should (equal (lispm-desktop-needs--status) "Installed lispm-test-player and lispm-test-tagger."))
      (should-not (lispm-desktop-needs-running-p)))))

(ert-deftest lispm-desktop-test-needs-install-waits-for-locks ()
  "An install another brew's lock stops tries again after a while."
  (lispm-desktop-test--with-brew
      "if [ ! -e $BIN/tried ]; then touch $BIN/tried
echo 'Error: A `brew install` process has already locked /opt/homebrew/Cellar/openssl@3.'; exit 1; fi
printf '#!/bin/sh\\n' > $BIN/$3; chmod +x $BIN/$3\n"
    (let ((lispm-desktop-needs-lock-wait 0.1)
          answer)
      (lispm-desktop-needs-install (list (cadr lispm-desktop-test--needs)) "EMMS"
                                   (lambda (present) (setq answer (list present))))
      (lispm-desktop-test--wait (lambda () answer))
      (should (equal answer '(t))))))

(ert-deftest lispm-desktop-test-needs-install-fails-and-retries ()
  "An install that fails says Homebrew's error and answers nil; tried
again, it installs."
  (lispm-desktop-test--with-brew
      "if [ ! -e $BIN/tried ]; then touch $BIN/tried; echo 'Error: No available formula'; exit 1; fi
printf '#!/bin/sh\\n' > $BIN/$3; chmod +x $BIN/$3\n"
    (let (answer)
      (lispm-desktop-needs-install (list (cadr lispm-desktop-test--needs)) "EMMS"
                                   (lambda (present) (setq answer (list present))))
      (lispm-desktop-test--wait (lambda () answer))
      (should (equal answer '(nil)))
      (should (equal (lispm-desktop-needs--job-error (car lispm-desktop-needs--jobs)) "Error: No available formula"))
      (should (equal (lispm-desktop-needs--status) "lispm-test-tagger could not be installed."))
      (lispm-desktop-needs-retry)
      (lispm-desktop-test--wait (lambda () (not (lispm-desktop-needs-running-p))))
      (should (eq (lispm-desktop-needs--job-state (car lispm-desktop-needs--jobs)) 'installed)))))

(ert-deftest lispm-desktop-test-needs-without-homebrew ()
  "Without Homebrew, installing offers it first; declined, what Homebrew
would install fails, and what waited is answered nil."
  (let ((exec-path nil)
        (lispm-desktop-needs-brew-places nil)
        (lispm-desktop-needs--jobs nil)
        (lispm-desktop-needs--requests nil)
        (lispm-desktop-needs--homebrew nil)
        (asked nil)
        answer)
    (cl-letf (((symbol-function 'lispm-desktop-popup-alert)
               (lambda (_icon message &rest _) (push message asked) 'cancel))
              ((symbol-function 'lispm-desktop-app-window-close) #'ignore))
      (lispm-desktop-needs-install lispm-desktop-test--needs "EMMS" (lambda (present) (setq answer (list present))))
      (lispm-desktop-test--wait (lambda () answer))
      (should (equal asked '("Install Homebrew first?")))
      (should (equal answer '(nil)))
      (should (seq-every-p (lambda (job) (equal (lispm-desktop-needs--job-error job) "Homebrew is not installed."))
                           lispm-desktop-needs--jobs)))))

(ert-deftest lispm-desktop-test-app-emms-mdls ()
  "Spotlight's tags, as mdls prints them, are read: strings, the first of
a list, numbers as written, nothing for null."
  (with-temp-buffer
    (insert "kMDItemAlbum            = \"Abbey Road\"\n"
            "kMDItemAudioTrackNumber = 3\n"
            "kMDItemAuthors          = (\n    \"The Beatles\",\n    \"Someone\"\n)\n"
            "kMDItemComposer         = (null)\n"
            "kMDItemTitle            = \"Say \\\"Something\\\"\"\n")
    (should (equal (lispm-desktop-app-emms--mdls-value "kMDItemAlbum") "Abbey Road"))
    (should (equal (lispm-desktop-app-emms--mdls-value "kMDItemAudioTrackNumber") "3"))
    (should (equal (lispm-desktop-app-emms--mdls-value "kMDItemAuthors") "The Beatles"))
    (should-not (lispm-desktop-app-emms--mdls-value "kMDItemComposer"))
    (should (equal (lispm-desktop-app-emms--mdls-value "kMDItemTitle") "Say \"Something\""))
    (should-not (lispm-desktop-app-emms--mdls-value "kMDItemGenre"))))

(ert-deftest lispm-desktop-test-app-emms-import-music ()
  "The Music library's songs kept as files are cached with Music's tags,
its playlists kept of them; songs without a file, or protected, are
counted as left to Music."
  (let* ((file (make-temp-file "lispm-music-playlists"))
         (lispm-desktop-app-emms-music-playlists-file file)
         (lispm-desktop-app-emms--music-playlists 'unread)
         (cache nil)
         (emms-cache-dirty nil))
    (unwind-protect
        (cl-letf (((symbol-function 'emms-track) (lambda (type name) (list type name)))
                  ((symbol-function 'emms-track-set)
                   (lambda (track field value) (nconc track (list (cons field value)))))
                  ((symbol-function 'emms-cache-set) (lambda (_type path track) (push (cons path track) cache)))
                  ((symbol-function 'emms-cache-save) #'ignore))
          (let ((result (lispm-desktop-app-emms--import
                         '(:total 5
                           :tracks ((:id "A" :path "/m/a.m4a" :name "Song A" :artist "Band" :trackNumber 2
                                     :year 0 :duration 181.6 :kind "AAC audio file")
                                    (:id "B" :path "/m/b.m4p" :name "Song B" :kind "Protected AAC audio file")
                                    (:id "C" :path nil :name "Song C" :kind "MPEG audio file")
                                    (:id "D" :path "/m/d.mp3" :name "Song D" :kind "MPEG audio file")
                                    (:id "E" :path "/m/e.m4a" :name "Song E" :kind "AAC audio file"
                                     :protected t))
                           :playlists ((:name "Mix" :ids ("D" "B" "A")) (:name "Gone" :ids ("C")))))))
            (should (equal (take 3 result) '(2 1 3)))
            (should (equal (sort (copy-sequence (nth 3 result)) #'string<) '("/m/a.m4a" "/m/d.mp3"))))
          (let ((a (cdr (assoc "/m/a.m4a" cache))))
            (should (equal (alist-get 'info-title (cddr a)) "Song A"))
            (should (equal (alist-get 'info-tracknumber (cddr a)) "2"))
            (should (= (alist-get 'info-playing-time (cddr a)) 182))
            (should-not (assq 'info-year (cddr a))))
          (should (equal (lispm-desktop-app-emms-music-playlists) '(("Mix" "/m/d.mp3" "/m/a.m4a"))))
          (setq lispm-desktop-app-emms--music-playlists 'unread)
          (should (equal (lispm-desktop-app-emms-music-playlists) '(("Mix" "/m/d.mp3" "/m/a.m4a")))))
      (delete-file file))))

(ert-deftest lispm-desktop-test-app-emms-reader-rebuilt ()
  "The reader of the Music library is built once, and again only when its
sources change."
  (let ((lispm-desktop-app-emms-reader-directory (make-temp-file "lispm-music-library" t)))
    (unwind-protect
        (let ((stamp (lispm-desktop-app-emms--reader-stamp)))
          (should-not (lispm-desktop-app-emms--reader-built-p))
          (make-directory (file-name-directory stamp) t)
          (with-temp-file stamp (insert (lispm-desktop-app-emms--reader-sources-hash)))
          (should (lispm-desktop-app-emms--reader-built-p))
          (with-temp-file stamp (insert "sources of before"))
          (should-not (lispm-desktop-app-emms--reader-built-p)))
      (delete-directory lispm-desktop-app-emms-reader-directory t))))

(ert-deftest lispm-desktop-test-app-emms-not-read ()
  "When the Music library could not be read, the alert says why and its
Open Privacy Settings opens Media & Apple Music in System Settings."
  (let ((asked nil) (opened nil))
    (cl-letf (((symbol-function 'lispm-desktop-popup-alert)
               (lambda (_icon message buttons &optional detail)
                 (setq asked (list message (mapcar #'car buttons) detail))
                 'settings))
              ((symbol-function 'call-process)
               (lambda (program &rest arguments) (setq opened (cons program (last arguments))) 0)))
      (lispm-desktop-app-emms--not-read '(:error "Access denied.")))
    (should (equal (car asked) "The Music library could not be read."))
    (should (equal (nth 1 asked) '("Open Privacy Settings" "OK")))
    (should (string-prefix-p "Access denied." (nth 2 asked)))
    (should (equal opened (list "open" lispm-desktop-app-emms-media-settings)))))

(ert-deftest lispm-desktop-test-app-emms-readable ()
  "Emacs may read the files imported, or macOS keeps them from it, as it
keeps ~/Music from an application not let read the media library, or
none of them is there."
  (let ((file (make-temp-file "lispm-song")))
    (unwind-protect
        (progn
          (should (eq (lispm-desktop-app-emms-readable-p (list "/no/such/song.m4a" file)) t))
          (should-not (lispm-desktop-app-emms-readable-p (list "/no/such/song.m4a")))
          (cl-letf (((symbol-function 'insert-file-contents-literally)
                     (lambda (name &rest _)
                       (signal 'file-error (list "Opening input file" "Operation not permitted" name)))))
            (should (eq (lispm-desktop-app-emms-readable-p (list file)) 'denied))))
      (delete-file file)))
  (should (equal (lispm-desktop-app-emms--application "/Applications/LispM.app/Contents/MacOS/") "LispM"))
  (should (equal (lispm-desktop-app-emms--application "/usr/local/bin/") "Emacs")))

(ert-deftest lispm-desktop-test-app-emms-imported-unreadable ()
  "Songs imported that Emacs may not read are said so, naming the
application to let read the media library, and Open Privacy Settings
opens Media & Apple Music; songs it may read are counted."
  (let ((asked nil) (opened nil) (readable 'denied))
    (cl-letf (((symbol-function 'lispm-desktop-app-emms-readable-p) (lambda (_) readable))
              ((symbol-function 'lispm-desktop-app-emms--application) (lambda (&rest _) "LispM"))
              ((symbol-function 'lispm-desktop-popup-alert)
               (lambda (_icon message buttons &optional detail)
                 (setq asked (list message (mapcar #'car buttons) detail))
                 'settings))
              ((symbol-function 'call-process)
               (lambda (program &rest arguments) (setq opened (cons program (last arguments))) 0)))
      (lispm-desktop-app-emms--imported nil '(426 9 31 ("/m/a.m4a")))
      (should (equal (car asked) "426 songs and 9 playlists came from Music, but LispM may not read them."))
      (should (equal (nth 1 asked) '("Open Privacy Settings" "OK")))
      (should (string-search "Turn LispM on in Privacy & Security, Media & Apple Music" (nth 2 asked)))
      (should (equal opened (list "open" lispm-desktop-app-emms-media-settings)))
      (setq readable t opened nil)
      (lispm-desktop-app-emms--imported nil '(426 9 31 ("/m/a.m4a")))
      (should (equal (car asked) "426 songs and 9 playlists came from Music."))
      (should (equal (nth 1 asked) '("OK"))))))

(ert-deftest lispm-desktop-test-app-emms-players ()
  "EMMS plays with its default players whose programs are installed, mpv
only when it is, then macOS's afplay where it is."
  (skip-unless (locate-library "emms-setup"))
  (require 'emms-setup)
  (cl-letf (((symbol-function 'executable-find) (lambda (name &rest _) (equal name "afplay"))))
    (let ((players (lispm-desktop-app-emms-players)))
      (should-not (memq 'emms-player-mpv players))
      (should (eq (car (last players)) 'emms-player-afplay))
      (should (fboundp 'emms-player-afplay-playable-p))))
  (cl-letf (((symbol-function 'executable-find) (lambda (name &rest _) (equal name "mpv"))))
    (should (memq 'emms-player-mpv (lispm-desktop-app-emms-players)))
    (should-not (memq 'emms-player-afplay (lispm-desktop-app-emms-players)))))

(ert-deftest lispm-desktop-test-store-archive-updates ()
  "An item from the package archives can be updated when the archives, as
last read, hold a newer package than installed; one pinned, when its
catalog pins another commit; one built in, never."
  (lispm-desktop-test--with-store
    (let ((package-alist (list (list 'emms (package-desc-create :name 'emms :version '(20260414 1505)))))
          (package-archive-contents
           (list (list 'emms (package-desc-create :name 'emms :version '(20261001 900)))))
          (lispm-desktop-store-installed
           '((emms :kind app :name "EMMS" :command emms-browser :package emms :installed "20260414.1505")
             (elfeed :kind app :name "Elfeed" :command elfeed :installed "aaa"
                     :package (elfeed :url "https://example.org/elfeed" :rev "aaa"))
             (calculator :kind app :name "Calculator" :command calc :installed "built-in"))))
      (should (lispm-desktop-store-update-available-p 'emms))
      (should-not (lispm-desktop-store-update-available-p 'elfeed))
      (should-not (lispm-desktop-store-update-available-p 'calculator))
      (should (equal (lispm-desktop-store-updates) '(emms)))
      (should (string-search "to the package archives' newest, 20261001.900"
                             (lispm-desktop-store--changes (lispm-desktop-store--installed 'emms)
                                                           (lispm-desktop-store--installed 'emms))))
      (setq package-archive-contents
            (list (list 'emms (package-desc-create :name 'emms :version '(20260414 1505)))))
      (should-not (lispm-desktop-store-update-available-p 'emms)))))

(ert-deftest lispm-desktop-test-store-updates-quietly ()
  "An update installing by itself asks nothing: a pinned package goes to
the commit its catalog pins now, and is kept as installed so."
  (lispm-desktop-test--with-store
    (let ((repinned nil))
      (setq lispm-desktop-store-installed
            '((elfeed :kind app :name "Elfeed" :command elfeed :installed "aaa"
                      :package (elfeed :url "https://example.org/elfeed" :rev "aaa")))
            lispm-desktop-store--catalogs
            '((:url "https://example.org/store"
               :items ((elfeed :kind app :name "Elfeed" :command elfeed
                               :package (elfeed :url "https://example.org/elfeed" :rev "bbb"))))))
      (cl-letf (((symbol-function 'lispm-desktop-store--confirm)
                 (lambda (&rest _) (error "Asked")))
                ((symbol-function 'lispm-desktop-store--repin)
                 (lambda (name rev) (setq repinned (list name rev))))
                ((symbol-function 'lispm-desktop-store--register) #'ignore))
        (lispm-desktop-store-update 'elfeed t))
      (should (equal repinned '(elfeed "bbb")))
      (should (equal (plist-get (lispm-desktop-store--installed 'elfeed) :installed) "bbb"))
      (should-not (lispm-desktop-store-update-available-p 'elfeed)))))

(ert-deftest lispm-desktop-test-store-updates-install-by-themselves ()
  "Updates found install by themselves once LispM is idle, quietly, but an
application open waits for its window to close; told not to, nothing
installs and the updates wait."
  (lispm-desktop-test--with-store
    (let ((updated nil) (scheduled nil) (lispm-desktop-store--update-timer nil)
          (lispm-desktop-store-update-automatically t))
      (cl-letf (((symbol-function 'lispm-desktop-store-updates) (lambda () '(elfeed emms hermes)))
                ((symbol-function 'lispm-desktop-app-window-open-p) (lambda (id) (eq id 'elfeed)))
                ((symbol-function 'lispm-desktop-store-update)
                 (lambda (id &optional quietly) (push (cons id quietly) updated)))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (seconds _repeat function) (setq scheduled (list seconds function)) 'timer)))
        (lispm-desktop-store--checked-updates)
        (should (equal scheduled '(60 lispm-desktop-store--install-updates)))
        (setq scheduled nil)
        (lispm-desktop-store--install-updates)
        (should (equal (nreverse updated) '((emms . t) (hermes . t))))
        (should (equal scheduled '(60 lispm-desktop-store--install-updates)))
        (setq updated nil scheduled nil lispm-desktop-store--update-timer nil
              lispm-desktop-store-update-automatically nil)
        (lispm-desktop-store--checked-updates)
        (lispm-desktop-store--install-updates)
        (should-not updated)
        (should-not scheduled)))))

(ert-deftest lispm-desktop-test-store-checks-along-software-update ()
  "The App Store looks for updates whenever Software Update checks, unless
it is checking already."
  (lispm-desktop-test--with-update
    (let ((checked 0)
          (lispm-desktop-update-check-hook (list #'lispm-desktop-store--check-along))
          (lispm-desktop-store--checking nil))
      (cl-letf (((symbol-function 'lispm-desktop-update-checkouts) #'ignore)
                ((symbol-function 'lispm-desktop-store-check-for-updates)
                 (lambda (&rest _) (setq checked (1+ checked)))))
        (lispm-desktop-update-check)
        (should (= checked 1))
        (setq lispm-desktop-store--checking t)
        (lispm-desktop-update-check)
        (should (= checked 1))))))

(ert-deftest lispm-desktop-test-settings-applications-updates ()
  "Software Update's panel says whether the App Store's applications update
by themselves, with a check box saving the choice, and which can be
updated, with a button showing them in the App Store."
  ;; Drawn as SVG: an Emacs without librsvg cannot.
  (skip-unless (image-type-available-p 'svg))
  (lispm-desktop-test--with-store
    (let ((saved nil) (lispm-desktop-store-update-automatically t))
      (setq lispm-desktop-store-installed '((elfeed :kind app :name "Elfeed" :command elfeed)))
      (cl-letf (((symbol-function 'lispm-desktop-store-updates) (lambda () '(elfeed)))
                ((symbol-function 'customize-save-variable)
                 (lambda (variable value) (push (cons variable value) saved))))
        (with-temp-buffer
          (lispm-desktop-settings--applications)
          (let ((text (concat (buffer-string)
                              (mapconcat #'identity
                                         (delq nil (mapcar (lambda (position)
                                                             (get-text-property position 'lispm-desktop-platinum-label))
                                                           (number-sequence (point-min) (1- (point-max)))))
                                         " "))))
            (dolist (label '("Update applications automatically" "Elfeed" "can be updated" "Show Updates"))
              (should (string-search label text)))))))))

(ert-deftest lispm-desktop-test-platinum-check-box-focus ()
  "A check box the keyboard is on is ringed, in the room left about it."
  (let* ((focus "#6666CC")
         (palette (plist-put (copy-sequence lispm-desktop-platinum-current-palette) :focus focus))
         (plain (lispm-desktop-platinum-draw-check-box palette t :pad 2))
         (focused (lispm-desktop-platinum-draw-check-box palette t :focused t)))
    (should (equal (dom-attr plain 'width) 18))
    (should (equal (dom-attr focused 'width) 18))
    (should-not (string-search focus (prin1-to-string plain)))
    (should (string-search focus (prin1-to-string focused)))))

;;;; The proxy

(ert-deftest lispm-desktop-test-proxy-server-and-url ()
  "The proxy's address is HOST:PORT, a scheme before it passed over, an IPv6
host in brackets; its URL tells programs its kind, a SOCKS5 proxy looking
names up."
  (should (equal (lispm-desktop-proxy-parse-server "127.0.0.1:7890") '("127.0.0.1" . 7890)))
  (should (equal (lispm-desktop-proxy-parse-server " http://proxy.example:8080/ ") '("proxy.example" . 8080)))
  (should (equal (lispm-desktop-proxy-parse-server "[::1]:1080") '("::1" . 1080)))
  (dolist (bad '(nil "" "127.0.0.1" "127.0.0.1:" ":7890" "host:0" "host:65536" "a b:1" "::1:1080"))
    (should-not (lispm-desktop-proxy-parse-server bad)))
  (should (equal (lispm-desktop-proxy--url 'http "127.0.0.1:7890") "http://127.0.0.1:7890"))
  (should (equal (lispm-desktop-proxy--url 'socks5 "[::1]:1080") "socks5h://[::1]:1080"))
  (should-not (lispm-desktop-proxy--url nil "127.0.0.1:7890"))
  (should-not (lispm-desktop-proxy--url 'http nil)))

(ert-deftest lispm-desktop-test-proxy-direct-hosts ()
  "This computer is always reached directly, and so are the exceptions and
the hosts below them; \"*\" is every host."
  (let ((lispm-desktop-proxy-exceptions '("example.com" ".intranet" "*.corp.example")))
    (dolist (host '("localhost" "127.0.0.1" "127.0.0.2" "::1" "[::1]" "example.com" "www.Example.com"
                    "wiki.intranet" "a.corp.example"))
      (should (lispm-desktop-proxy-direct-p host)))
    (dolist (host '("notexample.com" "example.org" "intranet.example" "github.com"))
      (should-not (lispm-desktop-proxy-direct-p host)))
    (should (equal (lispm-desktop-proxy-direct-hosts)
                   '("localhost" "127.0.0.1" "::1" "example.com" ".intranet" "*.corp.example"))))
  (let ((lispm-desktop-proxy-exceptions '("*")))
    (should (lispm-desktop-proxy-direct-p "github.com"))))

(defmacro lispm-desktop-test--with-proxy-environment (&rest body)
  "Run BODY with an environment of its own, set with no proxy of LispM's."
  (declare (indent 0))
  `(let ((process-environment (copy-sequence process-environment))
         (lispm-desktop-proxy--original-environment nil)
         (lispm-desktop-proxy-change-functions nil))
     (dolist (variable (append lispm-desktop-proxy-variables lispm-desktop-proxy-exception-variables))
       (setenv variable nil))
     ,@body))

(ert-deftest lispm-desktop-test-proxy-environment ()
  "The programs Emacs starts are told the proxy, and the hosts reached
directly, those the environment named before kept; without a proxy, the
environment is as it was."
  (lispm-desktop-test--with-proxy-environment
    (setenv "https_proxy" "http://old:1")
    (setenv "NO_PROXY" "corp.example")
    (lispm-desktop-proxy--apply 'socks5 "127.0.0.1:7890" '("intranet"))
    (dolist (variable lispm-desktop-proxy-variables)
      (should (equal (getenv variable) "socks5h://127.0.0.1:7890")))
    (should (equal (getenv "no_proxy") "localhost,127.0.0.1,::1,intranet"))
    (should (equal (getenv "NO_PROXY") "localhost,127.0.0.1,::1,intranet,corp.example"))
    ;; Another proxy, then none: what was there before is back.
    (lispm-desktop-proxy--apply 'http "proxy.example:8080" nil)
    (should (equal (getenv "https_proxy") "http://proxy.example:8080"))
    (should (equal (getenv "NO_PROXY") "localhost,127.0.0.1,::1,corp.example"))
    (lispm-desktop-proxy--apply nil "proxy.example:8080" nil)
    (should (equal (getenv "https_proxy") "http://old:1"))
    (should (equal (getenv "NO_PROXY") "corp.example"))
    (should-not (getenv "ALL_PROXY"))
    (should-not (getenv "no_proxy"))
    ;; No proxy, nothing was set: nothing is touched.
    (setenv "ALL_PROXY" "socks5://mine:1")
    (lispm-desktop-proxy--apply nil nil nil)
    (should (equal (getenv "ALL_PROXY") "socks5://mine:1"))))

(ert-deftest lispm-desktop-test-proxy-follows-its-settings ()
  "A setting changed, by the panel, Customize or `setq', is followed at
once, and the browser is told; a binding with `let' changes nothing."
  (lispm-desktop-test--with-proxy-environment
    (let ((lispm-desktop-proxy-type nil)
          (lispm-desktop-proxy-server "127.0.0.1:7890")
          (lispm-desktop-proxy-exceptions nil)
          (told nil))
      (add-hook 'lispm-desktop-proxy-change-functions (lambda (url hosts) (push (list url hosts) told)))
      (lispm-desktop-proxy--changed 'lispm-desktop-proxy-type 'http 'set nil)
      (should (equal (car told) '("http://127.0.0.1:7890" ("localhost" "127.0.0.1" "::1"))))
      (should (equal (getenv "http_proxy") "http://127.0.0.1:7890"))
      (lispm-desktop-proxy--changed 'lispm-desktop-proxy-type 'socks5 'let nil)
      (lispm-desktop-proxy--changed 'lispm-desktop-proxy-type 'socks5 'set (current-buffer))
      (should (= (length told) 1))
      (lispm-desktop-proxy--changed 'lispm-desktop-proxy-type nil 'set nil)
      (should (equal (car told) '(nil ("localhost" "127.0.0.1" "::1"))))
      (should-not (getenv "http_proxy")))))

(ert-deftest lispm-desktop-test-proxy-url-locator ()
  "url.el goes through an HTTP proxy, but not to a host reached directly;
under a SOCKS5 proxy its streams go through socks.el, and without one,
url.el finds proxies as it did."
  (let ((lispm-desktop-proxy-type 'http)
        (lispm-desktop-proxy-server "127.0.0.1:7890")
        (lispm-desktop-proxy-exceptions nil)
        (lispm-desktop-proxy--url-locator (lambda (_ host) (concat "BEFORE " host)))
        (url (url-generic-parse-url "https://example.com/")))
    (should (equal (lispm-desktop-proxy-find-for-url url "example.com") "PROXY 127.0.0.1:7890"))
    (should (equal (lispm-desktop-proxy-find-for-url url "localhost") "DIRECT"))
    (let ((lispm-desktop-proxy-type 'socks5))
      (should (equal (lispm-desktop-proxy-find-for-url url "example.com") "DIRECT")))
    (let ((lispm-desktop-proxy-type nil))
      (should (equal (lispm-desktop-proxy-find-for-url url "example.com") "BEFORE example.com")))))

(ert-deftest lispm-desktop-test-proxy-socks-streams ()
  "Under a SOCKS5 proxy, url.el's streams are opened through it, HTTPS's
negotiated over it and its certificate checked; others open as before."
  ;; Loaded before they are stubbed, as opening a stream loads them.
  (mapc #'require '(socks gnutls nsm puny))
  (let ((lispm-desktop-proxy-type 'socks5)
        (lispm-desktop-proxy-server "127.0.0.1:1080")
        (lispm-desktop-proxy-exceptions nil)
        (url-gateway-unplugged nil)
        (url-gateway-method 'native)
        (calls nil))
    (cl-letf (((symbol-function 'socks-open-network-stream)
               (lambda (_name _buffer host service)
                 (push (list 'socks socks-server host service) calls)
                 'stream))
              ((symbol-function 'gnutls-negotiate)
               (lambda (&rest spec) (push (list 'tls (plist-get spec :hostname)) calls) 'tls-stream))
              ((symbol-function 'nsm-verify-connection)
               (lambda (stream host port) (push (list 'check stream host port) calls) stream)))
      (let ((open (lambda (&rest args) (push (cons 'direct args) calls) 'direct-stream)))
        (should (eq (lispm-desktop-proxy--url-open-stream open "n" nil "example.com" 80) 'stream))
        (should (equal (car calls) '(socks ("LispM" "127.0.0.1" 1080 5) "example.com" 80)))
        (setq calls nil)
        (should (eq (lispm-desktop-proxy--url-open-stream open "n" nil "example.com" 443 'tls) 'tls-stream))
        (should (equal (reverse calls) '((socks ("LispM" "127.0.0.1" 1080 5) "example.com" 443)
                                         (tls "example.com")
                                         (check tls-stream "example.com" 443))))
        (setq calls nil)
        (should (eq (lispm-desktop-proxy--url-open-stream open "n" nil "127.0.0.1" 8080) 'direct-stream))
        (let ((lispm-desktop-proxy-type 'http))
          (should (eq (lispm-desktop-proxy--url-open-stream open "n" nil "example.com" 80 'tls)
                      'direct-stream)))
        (should (equal (mapcar #'car calls) '(direct direct)))))))

(defun lispm-desktop-test--proxy-server (kind)
  "Start a proxy of KIND, `http' or `socks5', on a free loopback port; return
its process.  It answers each request itself, with a page naming the host
asked for, and keeps the hosts in its `hosts' property."
  (let ((server
         (make-network-process
          :name "lispm-desktop-test-proxy" :server t :host 'local :service t
          :coding 'binary :noquery t
          :filter
          (lambda (connection string)
            (let* ((server (process-get connection 'server))
                   (input (concat (or (process-get connection 'input) "") string))
                   (state (or (process-get connection 'state) (if (eq kind 'socks5) 'greeting 'request)))
                   (answer (lambda (host)
                             (process-put server 'hosts (cons host (process-get server 'hosts)))
                             (let ((body (format "proxied %s" host)))
                               (process-send-string
                                connection
                                (format "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
                                        (length body) body))))))
              (pcase state
                ((and 'greeting (guard (>= (length input) 3)))
                 (process-send-string connection (unibyte-string 5 0))
                 (setq input "" state 'connect))
                ((and 'connect (guard (and (>= (length input) 5)
                                           (>= (length input) (+ 7 (aref input 4))))))
                 (process-put connection 'host (substring input 5 (+ 5 (aref input 4))))
                 (process-send-string connection (unibyte-string 5 0 0 1 0 0 0 0 0 0))
                 (setq input "" state 'request))
                ((and 'request (guard (string-search "\r\n\r\n" input)))
                 (funcall answer (or (process-get connection 'host)
                                     (and (string-match "\\`GET http://\\([^/:]+\\)" input)
                                          (match-string 1 input))))
                 (setq input "" state 'answered)))
              (process-put connection 'input input)
              (process-put connection 'state state)))
          :log (lambda (server connection _message)
                 (process-put connection 'server server)))))
    server))

(ert-deftest lispm-desktop-test-proxy-url-retrieves-through-it ()
  "url.el retrieves through an HTTP proxy and through a SOCKS5 one."
  (dolist (kind '(http socks5))
    (let* ((server (lispm-desktop-test--proxy-server kind))
           (lispm-desktop-proxy-type kind)
           (lispm-desktop-proxy-server (format "127.0.0.1:%d" (process-contact server :service)))
           (lispm-desktop-proxy-exceptions nil)
           (url-proxy-locator #'lispm-desktop-proxy-find-for-url)
           (url-request-noninteractive t))
      (advice-add 'url-open-stream :around #'lispm-desktop-proxy--url-open-stream)
      (unwind-protect
          (let ((buffer (url-retrieve-synchronously "http://lispm-test.invalid/page" t t 10)))
            (should buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (should (search-forward "proxied lispm-test.invalid" nil t)))
            (kill-buffer buffer)
            (should (equal (process-get server 'hosts) '("lispm-test.invalid"))))
        (advice-remove 'url-open-stream #'lispm-desktop-proxy--url-open-stream)
        (delete-process server)))))

;;;; Startup

(ert-deftest lispm-desktop-test-startup-login-command ()
  "LispM opens at login as it runs now: its application through open(1)
where it runs in one, else its Emacs, with its init directory either way."
  (let ((user-emacs-directory "/Users/u/.lispm/lispm-desktop/")
        (invocation-name "Emacs"))
    (let ((system-type 'darwin)
          (invocation-directory "/Applications/LispM.app/Contents/MacOS/"))
      (should (equal (lispm-desktop-startup-login-command)
                     '("/usr/bin/open" "-a" "/Applications/LispM.app"
                       "--args" "--init-directory=/Users/u/.lispm/lispm-desktop/"))))
    (let ((system-type 'gnu/linux)
          (invocation-name "emacs")
          (invocation-directory "/usr/local/bin/"))
      (should (equal (lispm-desktop-startup-login-command)
                     '("/usr/local/bin/emacs" "--init-directory=/Users/u/.lispm/lispm-desktop/"))))))

(ert-deftest lispm-desktop-test-startup-login-files ()
  "The launch agent is a property list running the command at load,
naming the application it opens; the autostart entry quotes and escapes
the command as the desktop entry specification asks."
  (let* ((agent (lispm-desktop-startup--launch-agent
                 '("/usr/bin/open" "-a" "/Applications/A & B.app" "--args" "--init-directory=/x/")
                 "org.lispmachinelab.lispm"))
         (plist (with-temp-buffer
                  (insert agent)
                  (car (xml-parse-region (point-min) (point-max)))))
         (dict (car (xml-get-children plist 'dict)))
         (strings (mapcar (lambda (node) (car (xml-node-children node)))
                          (xml-get-children (car (xml-get-children dict 'array)) 'string))))
    (should (eq (xml-node-name plist) 'plist))
    (should (equal strings '("/usr/bin/open" "-a" "/Applications/A & B.app" "--args" "--init-directory=/x/")))
    (should (string-search (concat "<string>" lispm-desktop-startup-login-label "</string>") agent))
    (should (string-search "<key>RunAtLoad</key>\n  <true/>" agent))
    (should (string-search "<string>org.lispmachinelab.lispm</string>" agent))
    (should-not (string-search "AssociatedBundleIdentifiers"
                               (lispm-desktop-startup--launch-agent '("/usr/bin/open" "-a" "/x.app")))))
  (let ((entry (lispm-desktop-startup--autostart-entry
                '("/usr/bin/emacs" "--init-directory=/home/u/my dir/" "100%" "a$b\\c"))))
    (should (string-search "[Desktop Entry]\nType=Application\n" entry))
    (should (string-search "Exec=/usr/bin/emacs \"--init-directory=/home/u/my dir/\" 100%% \"a\\\\$b\\\\\\\\c\"\n"
                           entry))))

(ert-deftest lispm-desktop-test-startup-open-at-login ()
  "Opening at login writes the login file, only when it changed, and
turning it off deletes it; where there is none, it cannot be turned on."
  (let* ((directory (make-temp-file "lispm-desktop-test-login" t))
         (file (expand-file-name "LaunchAgents/lispm.plist" directory))
         (text "first"))
    (cl-letf (((symbol-function 'lispm-desktop-startup-login-file) (lambda () file))
              ((symbol-function 'lispm-desktop-startup--login-text) (lambda () text)))
      (unwind-protect
          (progn
            (should-not (lispm-desktop-startup-login-p))
            (lispm-desktop-open-at-login t)
            (should (lispm-desktop-startup-login-p))
            (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) "first"))
            ;; The same again: left as it is.
            (set-file-times file 0)
            (lispm-desktop-open-at-login t)
            (should (time-equal-p (file-attribute-modification-time (file-attributes file)) 0))
            (setq text "second")
            (lispm-desktop-open-at-login t)
            (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) "second"))
            (lispm-desktop-open-at-login nil)
            (should-not (file-exists-p file))
            (should-not (lispm-desktop-startup-login-p))
            (lispm-desktop-open-at-login nil))
        (delete-directory directory t))
      (cl-letf (((symbol-function 'lispm-desktop-startup-login-file) #'ignore))
        (should-not (lispm-desktop-startup-login-p))
        (should-error (lispm-desktop-open-at-login t) :type 'user-error)))))

(ert-deftest lispm-desktop-test-startup-item-names ()
  "A startup item is named by its :name, else after what it opens; a copy
with a key changed leaves the item as it was."
  (let ((lispm-desktop-apps '((browser :name "Browser"))))
    (pcase-dolist (`(,item ,kind ,name)
                   '(((:app browser) app "Browser")
                     ((:app stickies) app "Stickies")
                     ((:file "~/notes/todo.org") file "todo.org")
                     ((:file "~/projects/") file "projects")
                     ((:program "OLLAMA_HOST=0.0.0.0 /opt/bin/ollama serve") program "ollama")
                     ((:program "ollama serve" :name "Models") program "Models")
                     ((:command magit-status) command "magit-status")
                     ((:name "Odd") nil "Odd")))
      (should (eq (lispm-desktop-startup-item-kind item) kind))
      (should (equal (lispm-desktop-startup-item-name item) name))))
  (let ((item (list :program "a" :disabled t)))
    (should (equal (lispm-desktop-startup-item-with item :disabled nil) '(:program "a")))
    (should (equal (lispm-desktop-startup-item-with item :name "A") '(:program "a" :disabled t :name "A")))
    (should (equal item '(:program "a" :disabled t)))))

(defvar lispm-desktop-test--startup-opened nil
  "What the startup items opened in a test, last first.")

(defun lispm-desktop-test-startup-command ()
  "Note that a startup item called this command."
  (interactive)
  (push 'command lispm-desktop-test--startup-opened))

(ert-deftest lispm-desktop-test-startup-items-open ()
  "The startup items open in their order: applications, documents in the
editor, folders in the Finder window, commands; those turned off do not,
and one that cannot open is told of, the others opening still."
  (let* ((directory (make-temp-file "lispm-desktop-test-startup" t))
         (document (expand-file-name "todo.org" directory))
         (lispm-desktop-test--startup-opened nil)
         (lispm-desktop-apps '((browser :name "Browser")))
         (told nil)
         (lispm-desktop-startup-items
          `((:app browser)
            (:file ,document)
            (:app nowhere)
            (:file ,directory :disabled t)
            (:file ,directory)
            (:command lispm-desktop-test-startup-command)
            (:command lispm-desktop-test-startup-command :disabled t))))
    (write-region "" nil document nil 'silent)
    (cl-letf (((symbol-function 'lispm-desktop-apps-activate)
               (lambda (app) (push (list 'app app) lispm-desktop-test--startup-opened)))
              ((symbol-function 'lispm-desktop-editor-activate) #'ignore)
              ((symbol-function 'find-file)
               (lambda (file) (push (list 'file file) lispm-desktop-test--startup-opened)))
              ((symbol-function 'lispm-desktop-finder-open-directory)
               (lambda (folder) (push (list 'folder folder) lispm-desktop-test--startup-opened)))
              ((symbol-function 'message)
               (lambda (format &rest args) (setq told (apply #'format-message format args)))))
      (unwind-protect
          (lispm-desktop-startup-open-items)
        (delete-directory directory t)))
    (should (equal (reverse lispm-desktop-test--startup-opened)
                   `((app browser) (file ,document) (folder ,directory) command)))
    (should (string-search "nowhere" told))))

(ert-deftest lispm-desktop-test-startup-program ()
  "A program runs in the background, in its directory, its output in its
own buffer; quitting does not ask about it, a second open leaves it
running alone, and its stop is noted with its exit code."
  (let* ((directory (make-temp-file "lispm-desktop-test-program" t))
         (item `(:program "pwd; echo started; read line; exit 3" :name "lispm-test-service"
                          :directory ,directory))
         (changes 0)
         (lispm-desktop-startup-changed-hook (list (lambda () (setq changes (1+ changes))))))
    (unwind-protect
        (let ((process (progn (lispm-desktop-startup-open-item item)
                              (lispm-desktop-startup-program-process item))))
          (should (process-live-p process))
          (should-not (process-query-on-exit-flag process))
          (should (eq (lispm-desktop-startup-program-status item) 'running))
          (lispm-desktop-startup-open-item item)
          (should (eq (lispm-desktop-startup-program-process item) process))
          (process-send-string process "go\n")
          (while (process-live-p process)
            (accept-process-output process 1))
          (with-current-buffer (lispm-desktop-startup-program-buffer-name item)
            (should (string-search (directory-file-name (file-truename directory))
                                   (file-truename (buffer-string))))
            (should (string-search "started" (buffer-string)))
            (should (string-search "exit code 3" (buffer-string))))
          (should (equal (lispm-desktop-startup-program-status item) '(exited . 3)))
          (should (>= changes 2)))
      (when-let* ((buffer (get-buffer (lispm-desktop-startup-program-buffer-name item))))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer)))
      (delete-directory directory t))))

(ert-deftest lispm-desktop-test-startup-after-start ()
  "The startup items open once Emacs waits for input, never in a batch
Emacs."
  (let ((scheduled nil))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest _) (setq scheduled function))))
      (lispm-desktop-startup--after-start)
      (should-not scheduled)
      (let ((noninteractive nil))
        (lispm-desktop-startup--after-start))
      (should (eq scheduled #'lispm-desktop-startup-open-items)))))

(ert-deftest lispm-desktop-test-settings-startup-panel ()
  "The Startup panel shows whether LispM opens at login and the startup
items, each turned on or off by its check box, and adds and removes them."
  ;; Drawn as SVG: an Emacs without librsvg cannot.
  (skip-unless (image-type-available-p 'svg))
  (let ((saved nil)
        (login nil)
        (answers nil)
        (lispm-desktop-startup-items '((:app browser) (:program "ollama serve" :disabled t))))
    (cl-letf (((symbol-function 'lispm-desktop-startup-login-file) (lambda () "/nowhere/lispm.plist"))
              ((symbol-function 'lispm-desktop-open-at-login) (lambda (open) (setq login (list open))))
              ((symbol-function 'customize-save-variable)
               (lambda (variable value) (push (cons variable value) saved) (set variable value)))
              ((symbol-function 'completing-read) (lambda (&rest _) (pop answers)))
              ((symbol-function 'read-string) (lambda (&rest _) (pop answers)))
              ((symbol-function 'read-directory-name) (lambda (&rest _) (pop answers))))
      (with-current-buffer (lispm-desktop-settings--buffer)
        (lispm-desktop-settings-select 'startup)
        (let ((text (concat (buffer-string)
                            (mapconcat #'identity
                                       (delq nil (mapcar (lambda (position)
                                                           (get-text-property position 'lispm-desktop-platinum-label))
                                                         (number-sequence (point-min) (1- (point-max)))))
                                       " ")))
              (press (lambda (label)
                       ;; A push button draws its label in its image.
                       (let ((button (next-button (point-min))))
                         (while (and button
                                     (not (string-search label (button-label button)))
                                     (not (equal label (get-text-property (button-start button)
                                                                          'lispm-desktop-platinum-label))))
                           (setq button (next-button (button-end button))))
                         (should button)
                         (button-activate button)))))
          (dolist (label '("Login" "Open LispM when you log in" "Startup Items" "Browser"
                           "ollama" "ollama serve" "Add…" "Remove…" "Open…"))
            (should (string-search label text)))
          (should (equal lispm-desktop-platinum-title "Startup"))
          (funcall press "Open LispM when you log in")
          (should (equal login '(t)))
          ;; The program's check box turns it on.
          (funcall press "ollama")
          (should (equal lispm-desktop-startup-items '((:app browser) (:program "ollama serve"))))
          (setq answers '("Program, run in the background" "make serve" "~/site/" "Site"))
          (funcall press "Add…")
          (should (equal (car (last lispm-desktop-startup-items))
                         '(:program "make serve" :directory "~/site/" :name "Site")))
          (setq answers '("Program, run in the background" "make serve" "~/" "Site"))
          (should-error (funcall press "Add…") :type 'user-error)
          (setq answers '("Browser"))
          (funcall press "Remove…")
          (should (equal (mapcar #'lispm-desktop-startup-item-name lispm-desktop-startup-items)
                         '("ollama" "Site")))
          (should (= (length saved) 3)))))))

(provide 'lispm-desktop-test)
;;; lispm-desktop-test.el ends here
