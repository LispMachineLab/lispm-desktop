;;; lispm-desktop-start-as.el --- What LispM starts as: the desktop or Focus Mode  -*- lexical-binding: t; -*-

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

;; LispM starts as the desktop, or in Focus Mode, LispM's terminal: no
;; desktop, menu bar or windows' chrome, only the sessions on the left, a
;; listener on the right and a who line at the bottom, all used from the
;; keyboard, as a Lisp Machine's console was.  The terminal is LispM
;; Agent's, see its lispm-terminal.el; the desktop enters it at once too,
;; see lispm-desktop-lispm.el.  The setting's value for it is `terminal'.
;;
;;   the setting       `lispm-desktop-start-as', in the Startup panel of
;;                     Control Panels
;;   the command line  --start-as=terminal or --start-as=desktop, for one
;;                     start:
;;
;;                       open -a LispM --args --start-as=terminal
;;                       emacs -nw --start-as=terminal
;;
;;                     Emacs's own --terminal names the device a text
;;                     terminal is on, hence --start-as.
;;   switching         M-x lispm-desktop-start-as-terminal, and Start in
;;                     Focus Mode… in the desktop's Special menu; from
;;                     Focus Mode, M-x lispm-desktop-start-as-desktop, or
;;                     asking LispM for desktop mode.  The setting changes
;;                     and an alert offers to restart.
;;
;; As the terminal, `lispm-desktop-setup' leaves the desktop out, see
;; lispm-desktop.el, and the first frame becomes the terminal once Emacs
;; has made it.

;;; Code:

(require 'lispm-desktop-core)
(require 'lispm-desktop-i18n)

(declare-function lispm-ui-alert "lispm-desktop-popup" (icon message buttons &optional detail))
(declare-function lispm-terminal "lispm-terminal" (&optional agent))
(declare-function lispm-terminal-soon "lispm-terminal" (function))
(declare-function lispm-desktop-standalone-started-hidden-p "lispm-desktop-standalone" ())
(defvar lispm-turn-end-functions)

(defcustom lispm-desktop-start-as 'desktop
  "What LispM starts as: `desktop', the desktop with its menu bar, windows,
Finder and applications; or `terminal', Focus Mode, LispM's terminal, the
sessions, a listener and a who line, used from the keyboard alone.  It takes
effect at the next start, read as LispM is set up: before personal.el
loads, so it is set with Customize, as Control Panels do.
--start-as=desktop or --start-as=terminal on the command line chooses
for one start."
  :type '(choice (const :tag "The desktop" desktop)
                 (const :tag "Focus Mode, LispM's terminal" terminal))
  :group 'lispm-desktop)

(defconst lispm-desktop-start-as-choices '(("desktop" . desktop) ("terminal" . terminal))
  "What LispM can start as, as the command line names it.")

(defvar lispm-desktop-started-as nil
  "What this LispM started as, `desktop' or `terminal'; nil until set up.")

(defvar lispm-desktop-start-as-restart-file (locate-user-emacs-file "lispm-restart-as.eld")
  "Where LispM notes what to start as when it restarts, see
`lispm-desktop-restart-as'.")

(defun lispm-desktop-started-as-terminal-p ()
  "Return non-nil when LispM started as its terminal, without the desktop."
  (eq lispm-desktop-started-as 'terminal))

(defun lispm-desktop-start-as--argument (arguments)
  "Return what ARGUMENTS, a command line without the program, say LispM
starts as: the value of --start-as, given as --start-as=VALUE or
--start-as VALUE, `desktop' or `terminal'; nil when they say neither.
The last one given counts; the arguments after -- are files."
  (let ((as nil))
    (while arguments
      (let ((argument (pop arguments)))
        (cond ((equal argument "--") (setq arguments nil))
              ((string-match "\\`--?start-as\\(?:=\\(.*\\)\\)?\\'" argument)
               (let ((value (if (match-beginning 1) (match-string 1 argument) (pop arguments))))
                 (when-let* ((choice (assoc value lispm-desktop-start-as-choices)))
                   (setq as (cdr choice))))))))
    as))

(defun lispm-desktop-start-as--switch (_switch)
  "Take the value of --start-as off the command line, which
`lispm-desktop-setup' has followed already.  This is a function of
`command-switch-alist'."
  (let ((value (pop command-line-args-left)))
    (cond ((null value)
           (message (lispm-desktop-tr "--start-as needs desktop or terminal")))
          ((not (assoc value lispm-desktop-start-as-choices))
           (message (lispm-desktop-tr "--start-as is desktop or terminal, not %s") value)))))

(defun lispm-desktop-start-as--restart ()
  "Return what the LispM that restarted into this start asked it to be, or
nil, and forget it.  A restart is the same process with the same command
line, so its process id tells it from any other start."
  (let ((file lispm-desktop-start-as-restart-file))
    (when (file-exists-p file)
      (let ((note (ignore-errors
                    (with-temp-buffer
                      (insert-file-contents file)
                      (read (current-buffer))))))
        (ignore-errors (delete-file file))
        (and (consp note) (eql (car note) (emacs-pid))
             (rassq (cdr note) lispm-desktop-start-as-choices)
             (cdr note))))))

(defun lispm-desktop-lispm-terminal-available-p ()
  "Return non-nil when LispM Agent has its terminal, lispm-terminal.el."
  (locate-library "lispm-terminal"))

(defun lispm-desktop-start-as--choose ()
  "Settle what this start is, `lispm-desktop-started-as', and return it.
It is what a restart asked for, else what the command line says, else
`lispm-desktop-start-as'.  A batch Emacs, as LispM.app's installer runs to
install every package, sets the desktop up; so does LispM started out of
sight for an application of the computer's own, opened from the Finder
or the Dock, whose window the desktop draws, see
lispm-desktop-standalone.el; and so does a LispM Agent without the
terminal, which says so."
  (setq lispm-desktop-started-as
        (if (and (not noninteractive)
                 (not (and (fboundp 'lispm-desktop-standalone-started-hidden-p)
                           (lispm-desktop-standalone-started-hidden-p)))
                 (eq (or (lispm-desktop-start-as--restart)
                         (lispm-desktop-start-as--argument (cdr command-line-args))
                         lispm-desktop-start-as)
                     'terminal)
                 (or (lispm-desktop-lispm-terminal-available-p)
                     (ignore (message (lispm-desktop-tr "This LispM Agent has no Focus Mode: LispM starts as the desktop")))))
            'terminal
          'desktop)))

(defun lispm-desktop-start-as--open-terminal ()
  "Make the selected frame LispM's terminal, see `lispm-terminal'.  This is
a `window-setup-hook' function, run once Emacs has made its frame and
opened the files the command line names."
  (condition-case err
      (progn (require 'lispm-terminal)
             (lispm-terminal))
    (error (message (lispm-desktop-tr "Focus Mode could not open: %s") (error-message-string err)))))

(defun lispm-desktop-setup-start-as ()
  "Settle what LispM starts as and return it, `desktop' or `terminal', see
`lispm-desktop-start-as--choose'; Emacs is told --start-as, so as to take
its value off the command line rather than open it as a file."
  (add-to-list 'command-switch-alist '("-start-as" . lispm-desktop-start-as--switch))
  (lispm-desktop-start-as--choose))

;;;; Switching

(defun lispm-desktop-restart-as (as)
  "Restart LispM as AS, `desktop' or `terminal', whatever its command line
says: Emacs restarts with the same one."
  (with-temp-file lispm-desktop-start-as-restart-file
    (prin1 (cons (emacs-pid) as) (current-buffer)))
  (restart-emacs)
  ;; Still here: quitting was called off.
  (ignore-errors (delete-file lispm-desktop-start-as-restart-file)))

(defun lispm-desktop-start-as-switch (as)
  "Have LispM start as AS, `desktop' or `terminal', from now on, and offer
to restart into it unless it runs as AS already."
  (customize-save-variable 'lispm-desktop-start-as as)
  (cond ((eq as lispm-desktop-started-as)
         (message (if (eq as 'terminal)
                      (lispm-desktop-tr "LispM starts in Focus Mode, as it runs now")
                    (lispm-desktop-tr "LispM starts as the desktop, as it runs now"))))
        ((eq 'restart (lispm-ui-alert
                       'note (if (eq as 'terminal)
                                 (lispm-desktop-tr "LispM starts in Focus Mode from its next start.")
                               (lispm-desktop-tr "LispM starts as the desktop from its next start."))
                       `((,(lispm-desktop-tr "Later") later cancel)
                         (,(lispm-desktop-tr "Restart") restart default))
                       (lispm-desktop-tr "Restart now to switch at once; agents stop, and their sessions can be resumed.")))
         (lispm-desktop-restart-as as))))

(defun lispm-desktop-start-as-terminal ()
  "Have LispM start in Focus Mode from now on: its terminal, the sessions,
a listener and a who line, without the desktop.  An alert offers to
restart now."
  (interactive)
  (lispm-desktop-start-as-switch 'terminal))

(defun lispm-desktop-start-as-desktop ()
  "Have LispM start as the desktop from now on.  An alert offers to
restart now."
  (interactive)
  (lispm-desktop-start-as-switch 'desktop))

(defvar lispm-desktop-start-as--waiting nil
  "The `lispm-turn-end-functions' function waiting to offer a restart, or nil.")

(defun lispm-desktop-start-as--after-turn (agent function)
  "Call FUNCTION once AGENT's turn is over and its session saved, and
the command then under way is done, the LispM bar closed.  Asked again
before, as by a second call of the tool in the same turn, it is called
once."
  (unless lispm-desktop-start-as--waiting
    (letrec ((once (lambda (ended _reply)
                     (when (eq ended agent)
                       (remove-hook 'lispm-turn-end-functions once)
                       (setq lispm-desktop-start-as--waiting nil)
                       (lispm-terminal-soon function)))))
      (setq lispm-desktop-start-as--waiting once)
      (add-hook 'lispm-turn-end-functions once))))

(defun lispm-desktop-start-as-leave-terminal (agent)
  "Leave LispM's terminal for AGENT, asked by the tool terminal_leave,
as LispM started as the terminal: there is no desktop to go back to, so
LispM starts as the desktop from now on, and once AGENT's turn is over
and saved, the alert of `lispm-desktop-start-as-desktop' offers to
restart into it.  Return what to tell AGENT.  This is
`lispm-terminal-leave-function' while LispM runs as its terminal."
  (let ((inhibit-message t))
    (customize-save-variable 'lispm-desktop-start-as 'desktop))
  (lispm-desktop-start-as--after-turn agent #'lispm-desktop-start-as-desktop)
  "LispM started in Focus Mode, its terminal, so there is no desktop to go back to \
yet: from now on it starts as the desktop, and once this reply is done \
the user is asked whether to restart into it now (agents stop, and \
their sessions can be resumed).  Tell them so in one short line.")

(provide 'lispm-desktop-start-as)
;;; lispm-desktop-start-as.el ends here
