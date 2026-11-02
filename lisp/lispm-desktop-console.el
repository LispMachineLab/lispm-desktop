;;; lispm-desktop-console.el --- The errors and warnings LispM reported  -*- lexical-binding: t; -*-

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

;; The Console is LispM's, as Console.app is the Mac's: the errors,
;; warnings and notes LispM and its applications reported, kept newest
;; first, each with what it said and what led to it.  What the echo area
;; shows and takes away again stays here, to be read later:
;;
;;   warnings   every warning `display-warning' shows, its type the source
;;   errors     every LispM command that failed, with the command that did
;;   notes      every message in the echo area, once the user asks for
;;              them with `lispm-desktop-console-capture-messages'
;;   reports    what the App Store and the Installer say of an install or
;;              an update that failed, and what LispM says where something
;;              of its own could not be done
;;
;; The list filters by severity and by source, says how many of each it
;; holds, saves what it shows as text, and empties.  An event's details
;; are a Return away.
;;
;; The Console records quietly: reporting an event does not show it, and
;; `lispm-desktop-console-error' shows it as LispM would, without
;; recording it twice.  The desktop's other ways of showing a message
;; stay as they were.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'lispm-desktop-core)
(require 'lispm-desktop-i18n)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-app-window)
(require 'lispm-desktop-app-ui)

(defconst lispm-desktop-console-buffer-name " *Console*"
  "Name of the buffer of the Console's list.")

(defconst lispm-desktop-console-event-buffer-name " *Console Event*"
  "Name of the buffer showing one event of the Console.")

(defcustom lispm-desktop-console-limit 1000
  "How many events the Console keeps, newest first.
The oldest are dropped as new ones come; 0 keeps none."
  :type 'natnum
  :group 'lispm-desktop)

(defcustom lispm-desktop-console-refresh-interval 2
  "Seconds between the Console's looks at the events reported to it."
  :type 'natnum
  :group 'lispm-desktop)

(defcustom lispm-desktop-console-capture-messages nil
  "Non-nil makes the Console record every message in the echo area.
Off by default: messages come constantly, as isearch's and the
progress of what LispM does.  Warnings, failed commands, and what LispM,
the App Store and the Installer report are recorded either way."
  :type 'boolean
  :group 'lispm-desktop)

(cl-defstruct (lispm-desktop-console-event (:constructor lispm-desktop-console--make-event))
  "One thing the Console recorded.
ID orders the events as they came, TIME when, SEVERITY `error',
`warning' or `note', SOURCE who reported it, MESSAGE what it said, and
DETAILS more about it, or nil."
  (id 0)
  (time 0)
  (severity 'note)
  (source "")
  (message "")
  (details nil))

(defvar lispm-desktop-console--events nil
  "The events the Console holds, newest first.")

(defvar lispm-desktop-console--next-id 0
  "The id the next event gets.")

(defvar lispm-desktop-console--dirty nil
  "Non-nil while events were recorded that the Console has not shown yet.")

(defvar lispm-desktop-console--quiet nil
  "Non-nil while LispM shows a message the Console recorded already.")

(defvar lispm-desktop-console--timer nil
  "The timer looking for events to show while the Console's window is open.")

(defvar-local lispm-desktop-console--severity nil
  "The severity the Console shows, or nil for all of them.")

(defvar-local lispm-desktop-console--source nil
  "The source the Console shows, or nil for all of them.")

;;;; Recording

(defun lispm-desktop-console-report (severity source message &optional details)
  "Record an event of SEVERITY from SOURCE saying MESSAGE, with DETAILS.
Nothing is shown; see `lispm-desktop-console-say'.  Return the event."
  (let ((event (lispm-desktop-console--make-event
                :id (setq lispm-desktop-console--next-id (1+ lispm-desktop-console--next-id))
                :time (float-time)
                :severity severity
                :source source
                :message message
                :details details)))
    (if (> lispm-desktop-console-limit 0)
        (setq lispm-desktop-console--events
              (cons event (seq-take lispm-desktop-console--events
                                    (1- lispm-desktop-console-limit))))
      (setq lispm-desktop-console--events nil))
    (setq lispm-desktop-console--dirty t)
    event))

(defun lispm-desktop-console--show (message)
  "Show MESSAGE in the echo area, as LispM would, without recording it twice."
  (let ((lispm-desktop-console--quiet t))
    (message "%s" message)))

(defun lispm-desktop-console-say (severity source message &optional details)
  "Record an event, then show MESSAGE in the echo area, as LispM would.
SEVERITY, SOURCE and DETAILS are as `lispm-desktop-console-report' takes
them.  Return the event."
  (prog1 (lispm-desktop-console-report severity source message details)
    (lispm-desktop-console--show message)))

(defun lispm-desktop-console-error (source message &optional details)
  "Record an error from SOURCE saying MESSAGE, then show MESSAGE.
DETAILS say more about it."
  (lispm-desktop-console-say 'error source message details))

;;;; What the Console hears

(defun lispm-desktop-console--warning-source (type)
  "Return the source of a warning of TYPE, an English name."
  (let ((type (if (consp type) (car type) type)))
    (cond ((eq type 'lispm-desktop) "LispM")
          ((symbolp type) (symbol-name type))
          (t "Emacs"))))

(defun lispm-desktop-console--warning-level (level)
  "Return the severity of a warning shown at LEVEL."
  (cond ((memq level '(:error :emergency)) 'error)
        ((eq level :debug) 'note)
        (t 'warning)))

(defun lispm-desktop-console--display-warning (function &rest arguments)
  "Record the warning ARGUMENTS show, then show it.
This is `display-warning' advised."
  (when-let* (((not lispm-desktop-console--quiet))
              ((nth 1 arguments)))
    (lispm-desktop-console-report
     (lispm-desktop-console--warning-level (nth 2 arguments))
     (lispm-desktop-console--warning-source (nth 0 arguments))
     (format "%s" (nth 1 arguments))))
  (let ((lispm-desktop-console--quiet t))
    (apply function arguments)))

(defun lispm-desktop-console--command-error (function &rest arguments)
  "Record the command that failed, then say so as Emacs does.
This is `command-error-default-function' advised.  A quit and a
`user-error' are what the user did, not failures, and are left out."
  (let ((data (nth 0 arguments)))
    (when (and (not lispm-desktop-console--quiet)
               (consp data)
               (not (memq (car data) '(quit user-error))))
      (lispm-desktop-console-report
       'error "Emacs" (error-message-string data)
       (and (boundp 'this-command) this-command
            (lispm-desktop-tr "Command: %s" this-command)))))
  (let ((lispm-desktop-console--quiet t))
    (apply function arguments)))

(defun lispm-desktop-console--message (text)
  "Record TEXT, a message the echo area is about to show, when asked to.
This is one of `set-message-functions'.  Return nil, that the message
shows as it would."
  (when (and lispm-desktop-console-capture-messages
             (not lispm-desktop-console--quiet)
             (stringp text)
             (not (string-empty-p text)))
    (lispm-desktop-console-report 'note "Emacs" text))
  nil)

;;;; What the Console shows

(defconst lispm-desktop-console--columns 4
  "How many columns the Console's list has.")

(defun lispm-desktop-console--format ()
  "Return the Console's columns, named in the language LispM speaks."
  (vector (list (lispm-desktop-tr "Time") 19 t)
          (list (lispm-desktop-tr "Type") 8 t)
          (list (lispm-desktop-tr "Source") 12 t)
          (list (lispm-desktop-tr (lispm-desktop-tr-noop "Message" "console")) 0 t)))

(defun lispm-desktop-console--severity-name (severity)
  "Return SEVERITY, `error', `warning' or `note', in the language LispM speaks."
  (pcase severity
    ('error (lispm-desktop-tr "Error"))
    ('warning (lispm-desktop-tr "Warning"))
    (_ (lispm-desktop-tr "Note"))))

(defun lispm-desktop-console--one-line (text)
  "Return TEXT as one line, as the list shows it."
  (string-trim (replace-regexp-in-string "[\r\n]+" " " text)))

(defun lispm-desktop-console--time (event)
  "Return EVENT's time, written so that sorting the list sorts in time."
  (format-time-string "%Y-%m-%d %H:%M:%S" (lispm-desktop-console-event-time event)))

(defun lispm-desktop-console--events-shown ()
  "Return the events the Console shows now, newest first."
  (seq-filter (lambda (event)
                (and (or (null lispm-desktop-console--severity)
                         (eq (lispm-desktop-console-event-severity event)
                             lispm-desktop-console--severity))
                     (or (null lispm-desktop-console--source)
                         (equal (lispm-desktop-console-event-source event)
                                lispm-desktop-console--source))))
              lispm-desktop-console--events))

(defun lispm-desktop-console--entries ()
  "Return the Console's rows, as `tabulated-list-entries' takes them."
  (mapcar (lambda (event)
            (list event (vector (lispm-desktop-console--time event)
                                (lispm-desktop-console--severity-name
                                 (lispm-desktop-console-event-severity event))
                                (lispm-desktop-tr (lispm-desktop-console-event-source event))
                                (lispm-desktop-console--one-line
                                 (lispm-desktop-console-event-message event)))))
          (lispm-desktop-console--events-shown)))

(defun lispm-desktop-console-refresh ()
  "Draw the Console's list anew, from the events it holds."
  (interactive)
  (let ((buffer (get-buffer lispm-desktop-console-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'lispm-desktop-console-mode)
          (setq tabulated-list-entries (lispm-desktop-console--entries))
          (tabulated-list-print t)
          (setq lispm-desktop-console--dirty nil))))))

(defun lispm-desktop-console--tick ()
  "Draw the Console again when events were recorded since it was drawn."
  (when lispm-desktop-console--dirty
    (lispm-desktop-console-refresh)))

(defun lispm-desktop-console--started ()
  "Look for events to show, while the Console's window is open."
  (unless lispm-desktop-console--timer
    (let ((interval (max 1 lispm-desktop-console-refresh-interval)))
      (setq lispm-desktop-console--timer
            (run-with-timer interval interval #'lispm-desktop-console--tick)))))

(defun lispm-desktop-console--stopped ()
  "Stop looking for events to show."
  (when lispm-desktop-console--timer
    (cancel-timer lispm-desktop-console--timer)
    (setq lispm-desktop-console--timer nil)))

(defun lispm-desktop-console--closed (frame)
  "Stop looking for events once the Console's window FRAME closed."
  (when (eq (lispm-desktop-app-window-id frame) 'console)
    (lispm-desktop-console--stopped)))

;;;; Filtering

(defun lispm-desktop-console--filtered ()
  "Filter the list anew, and put it in front."
  (lispm-desktop-console-refresh)
  (force-mode-line-update t))

(defun lispm-desktop-console-show-all ()
  "Show every event the Console holds."
  (interactive)
  (setq lispm-desktop-console--severity nil
        lispm-desktop-console--source nil)
  (lispm-desktop-console--filtered))

(defun lispm-desktop-console-show-errors ()
  "Show the errors the Console holds."
  (interactive)
  (setq lispm-desktop-console--severity 'error)
  (lispm-desktop-console--filtered))

(defun lispm-desktop-console-show-warnings ()
  "Show the warnings the Console holds."
  (interactive)
  (setq lispm-desktop-console--severity 'warning)
  (lispm-desktop-console--filtered))

(defun lispm-desktop-console-show-notes ()
  "Show the notes the Console holds."
  (interactive)
  (setq lispm-desktop-console--severity 'note)
  (lispm-desktop-console--filtered))

(defun lispm-desktop-console-filter-source ()
  "Show only the events reported by a source chosen."
  (interactive)
  (let* ((sources (delete-dups
                   (mapcar #'lispm-desktop-console-event-source
                           lispm-desktop-console--events)))
         (choices (mapcar (lambda (source)
                            (cons (lispm-desktop-tr source) source))
                          sources))
         (choice (and sources
                      (completing-read (lispm-desktop-tr "Filter by Source…")
                                       choices nil t))))
    (when choice
      (setq lispm-desktop-console--source (cdr (assoc choice choices)))
      (lispm-desktop-console--filtered))))

(defun lispm-desktop-console-show-every-source ()
  "Show the events of every source."
  (interactive)
  (setq lispm-desktop-console--source nil)
  (lispm-desktop-console--filtered))

(defun lispm-desktop-console-toggle-filter ()
  "Show only the errors, or everything when something is filtered out."
  (interactive)
  (if (or lispm-desktop-console--severity lispm-desktop-console--source)
      (lispm-desktop-console-show-all)
    (lispm-desktop-console-show-errors)))

;;;; One event

(defun lispm-desktop-console--render-event (event)
  "Show EVENT in the current buffer, with its details."
  (let ((inhibit-read-only t)
        (severity (lispm-desktop-console--severity-name
                   (lispm-desktop-console-event-severity event)))
        (source (lispm-desktop-tr (lispm-desktop-console-event-source event))))
    (erase-buffer)
    (insert (propertize (lispm-desktop-tr "Event") 'face 'bold) "\n\n")
    (insert (propertize (lispm-desktop-console-event-message event) 'face 'bold) "\n\n")
    (insert (lispm-desktop-tr "%s: %s" (lispm-desktop-tr "Time")
                              (lispm-desktop-console--time event))
            "\n"
            (lispm-desktop-tr "%s: %s" (lispm-desktop-tr "Type") severity)
            "\n"
            (lispm-desktop-tr "%s: %s" (lispm-desktop-tr "Source") source)
            "\n")
    (when-let* ((details (lispm-desktop-console-event-details event)))
      (insert "\n" details "\n"))
    (goto-char (point-min))))

(defun lispm-desktop-console-show-event (event)
  "Show EVENT, with what led to it, in the Console's window."
  (interactive (list (tabulated-list-get-id)))
  (when (lispm-desktop-console-event-p event)
    (let ((buffer (get-buffer-create lispm-desktop-console-event-buffer-name)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'lispm-desktop-console-event-mode)
          (lispm-desktop-console-event-mode))
        (lispm-desktop-console--render-event event))
      (lispm-desktop-console--show-buffer buffer))))

;;;; Saving and emptying

(defun lispm-desktop-console--text ()
  "Return the events shown, oldest first, as text to save."
  (let ((text nil))
    (dolist (event (reverse (lispm-desktop-console--events-shown)))
      (push (concat (format "%s  %s  %s  %s\n"
                            (lispm-desktop-console--time event)
                            (lispm-desktop-console--severity-name
                             (lispm-desktop-console-event-severity event))
                            (lispm-desktop-tr (lispm-desktop-console-event-source event))
                            (lispm-desktop-console-event-message event))
                    (when-let* ((details (lispm-desktop-console-event-details event)))
                      (concat (replace-regexp-in-string
                               "^" "    " details nil nil)
                              "\n")))
            text))
    (apply #'concat (nreverse text))))

(defun lispm-desktop-console-save ()
  "Save the events the Console shows as text."
  (interactive)
  (let ((file (read-file-name (lispm-desktop-tr "Save the console: ")
                              nil nil nil "~/Console.txt")))
    (when (and file (not (string-empty-p file)))
      (write-region (lispm-desktop-console--text) nil file nil 'silent)
      (lispm-desktop-console--show
       (lispm-desktop-tr "Saved the console to %s" (abbreviate-file-name file))))))

(defun lispm-desktop-console-clear ()
  "Empty the Console."
  (interactive)
  (setq lispm-desktop-console--events nil)
  (lispm-desktop-console-refresh))

;;;; The list's interface

(defun lispm-desktop-console--toggle-label ()
  "Return what the button filtering the list says."
  (if (or lispm-desktop-console--severity lispm-desktop-console--source)
      (lispm-desktop-tr "Show All")
    (lispm-desktop-tr "Errors Only")))

(defun lispm-desktop-console--buttons ()
  "Return the push buttons of the Console's window."
  `((,(lispm-desktop-console--toggle-label) lispm-desktop-console-toggle-filter)
    (,(lispm-desktop-tr "Save…") lispm-desktop-console-save)
    (,(lispm-desktop-tr "Clear") lispm-desktop-console-clear)))

(defun lispm-desktop-console--count (severity)
  "Return how many events the Console holds of SEVERITY."
  (seq-count (lambda (event)
               (eq (lispm-desktop-console-event-severity event) severity))
             lispm-desktop-console--events))

(defun lispm-desktop-console--status ()
  "Return what the bottom of the Console's window says."
  (let ((severity lispm-desktop-console--severity)
        (source lispm-desktop-console--source))
    (delq nil
          (list (lispm-desktop-tr-n "%d error" "%d errors"
                                    (lispm-desktop-console--count 'error))
                (lispm-desktop-tr-n "%d warning" "%d warnings"
                                    (lispm-desktop-console--count 'warning))
                (lispm-desktop-tr-n "%d note" "%d notes"
                                    (lispm-desktop-console--count 'note))
                (pcase severity
                  ('error (lispm-desktop-tr "Errors Only"))
                  ('warning (lispm-desktop-tr "Warnings Only"))
                  ('note (lispm-desktop-tr "Notes Only")))
                (and source (lispm-desktop-tr "Source: %s" (lispm-desktop-tr source)))))))

(defun lispm-desktop-console--menu-items ()
  "Return the items of the Console's menu."
  (let ((severity lispm-desktop-console--severity)
        (source lispm-desktop-console--source))
    `((:label ,(lispm-desktop-tr "Show All") :action lispm-desktop-console-show-all
              :checked ,(and (null severity) (null source)))
      (:label ,(lispm-desktop-tr "Errors Only") :action lispm-desktop-console-show-errors
              :checked ,(eq severity 'error))
      (:label ,(lispm-desktop-tr "Warnings Only") :action lispm-desktop-console-show-warnings
              :checked ,(eq severity 'warning))
      (:label ,(lispm-desktop-tr "Notes Only") :action lispm-desktop-console-show-notes
              :checked ,(eq severity 'note))
      separator
      (:label ,(lispm-desktop-tr "Filter by Source…") :action lispm-desktop-console-filter-source
              :enabled ,(and lispm-desktop-console--events t))
      (:label ,(lispm-desktop-tr "All Sources") :action lispm-desktop-console-show-every-source
              :checked ,(null source) :enabled ,(and source t))
      separator
      (:label ,(lispm-desktop-tr "Save…") :action lispm-desktop-console-save)
      (:label ,(lispm-desktop-tr "Clear") :action lispm-desktop-console-clear))))

(defvar-keymap lispm-desktop-console-mode-map
  :doc "Keymap of the Console's list."
  :parent tabulated-list-mode-map
  "RET" #'lispm-desktop-console-show-event
  "<double-mouse-1>" #'lispm-desktop-console-show-event
  "a" #'lispm-desktop-console-show-all
  "e" #'lispm-desktop-console-show-errors
  "w" #'lispm-desktop-console-show-warnings
  "n" #'lispm-desktop-console-show-notes
  "f" #'lispm-desktop-console-filter-source
  "s" #'lispm-desktop-console-save
  "C" #'lispm-desktop-console-clear
  "g" #'lispm-desktop-console-refresh)

(define-derived-mode lispm-desktop-console-mode tabulated-list-mode "Console"
  "The errors, warnings and notes LispM reported, newest first.
\\{lispm-desktop-console-mode-map}"
  (let ((format (lispm-desktop-console--format)))
    ;; The columns are named in the buffer, its window's strip of buttons
    ;; taking the header line a list would have (see
    ;; `lispm-desktop-app-ui--list-header'); said here, they are named
    ;; from the first drawing on, before any event was recorded.
    (setq-local tabulated-list-use-header-line nil
                tabulated-list-format format
                tabulated-list-padding 1
                tabulated-list-sort-key (cons (car (aref format 0)) t)))
  (setq-local truncate-lines t
              revert-buffer-function (lambda (&rest _) (lispm-desktop-console-refresh))
              lispm-desktop-console--severity nil
              lispm-desktop-console--source nil)
  (add-hook 'kill-buffer-hook #'lispm-desktop-console--stopped nil t)
  (tabulated-list-init-header))

;;;; One event's buffer

(defvar-keymap lispm-desktop-console-event-mode-map
  :doc "Keymap of the Console's event."
  :parent special-mode-map
  "q" #'lispm-desktop-console
  "b" #'lispm-desktop-console)

(define-derived-mode lispm-desktop-console-event-mode special-mode "Console Event"
  "One event the Console recorded, with what led to it.
\\{lispm-desktop-console-event-mode-map}")

;;;; The window and its application

(defun lispm-desktop-console--buffer ()
  "Return the Console's list buffer, made when missing."
  (or (get-buffer lispm-desktop-console-buffer-name)
      (with-current-buffer (get-buffer-create lispm-desktop-console-buffer-name)
        (lispm-desktop-console-mode)
        (lispm-desktop-console-refresh)
        (current-buffer))))

(defun lispm-desktop-console--show-buffer (buffer)
  "Show BUFFER in the Console's window, or the selected window at a terminal."
  (if (and (display-graphic-p) (not noninteractive))
      (if (lispm-desktop-app-window-open-p 'console)
          (let ((window (frame-selected-window
                         (lispm-desktop-app-window-frame 'console))))
            ;; The window is dedicated, as an application window is:
            ;; showing another buffer in it means undoing that first.
            (set-window-dedicated-p window nil)
            (set-window-buffer window buffer)
            (set-window-dedicated-p window t)
            (lispm-desktop-app-window-present 'console))
        (lispm-desktop-app-window-show 'console :buffer buffer))
    (pop-to-buffer buffer)))

(defun lispm-desktop-console-open-p ()
  "Return non-nil while the Console's window is open."
  (lispm-desktop-app-window-open-p 'console))

(defun lispm-desktop-console ()
  "Show the Console, or bring it back to its list while it is open."
  (interactive)
  (let ((buffer (lispm-desktop-console--buffer)))
    (lispm-desktop-console-refresh)
    (lispm-desktop-console--started)
    (lispm-desktop-console--show-buffer buffer)
    buffer))

(defun lispm-desktop-setup-console ()
  "Make the Console an application built in, and have it hear LispM."
  (lispm-desktop-app-window-define 'console
    :app 'console :name (lispm-desktop-tr-noop "Console") :size '(640 . 400)
    :documents 'keep :close #'lispm-desktop-app-window-close-box)
  (lispm-desktop-define-app 'console
    :name (lispm-desktop-tr-noop "Console")
    :icon "console" :folder "console-32"
    :menus (lambda () (lispm-desktop-app-ui-menus 'console))
    :activate #'lispm-desktop-console
    :visible #'lispm-desktop-console-open-p)
  (lispm-desktop-app-ui-define 'lispm-desktop-console-mode
    :app 'console
    :restore #'lispm-desktop-console
    :title (lispm-desktop-tr-noop "Console")
    :list t
    :cursor nil
    :buttons #'lispm-desktop-console--buttons
    :status #'lispm-desktop-console--status
    :menus (list (cons (lispm-desktop-tr-noop "Console")
                       #'lispm-desktop-console--menu-items))
    :keys `(("Return" . ,(lispm-desktop-tr-noop "Show the event's details"))
            ("a" . ,(lispm-desktop-tr-noop "Show All"))
            ("e" . ,(lispm-desktop-tr-noop "Errors Only"))
            ("w" . ,(lispm-desktop-tr-noop "Warnings Only"))
            ("n" . ,(lispm-desktop-tr-noop "Notes Only"))
            ("f" . ,(lispm-desktop-tr-noop "Filter by Source…"))
            ("s" . ,(lispm-desktop-tr-noop "Save…"))
            ("C" . ,(lispm-desktop-tr-noop "Clear"))
            ("g" . ,(lispm-desktop-tr-noop "Draw the list again"))))
  (lispm-desktop-app-ui-define 'lispm-desktop-console-event-mode
    :app 'console
    :title (lispm-desktop-tr-noop "Event")
    :prose t
    :cursor nil
    :bottom nil
    :buttons `((,(lispm-desktop-tr-noop "Back") lispm-desktop-console))
    :keys `(("q  b" . ,(lispm-desktop-tr-noop "Back"))))
  (unless (advice-member-p #'lispm-desktop-console--display-warning 'display-warning)
    (advice-add 'display-warning :around #'lispm-desktop-console--display-warning))
  (unless (advice-member-p #'lispm-desktop-console--command-error
                           'command-error-default-function)
    (advice-add 'command-error-default-function :around
                #'lispm-desktop-console--command-error))
  (add-to-list 'set-message-functions #'lispm-desktop-console--message)
  (add-hook 'delete-frame-functions #'lispm-desktop-console--closed)
  (add-hook 'lispm-desktop-language-changed-hook
            #'lispm-desktop-console--language-changed))

(defun lispm-desktop-console--language-changed ()
  "Name the Console's columns in the language LispM speaks now."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'lispm-desktop-console-mode)
        (let* ((columns (mapcar #'car (append tabulated-list-format nil)))
               (key (car tabulated-list-sort-key))
               (index (or (seq-position columns key #'equal) 0))
               (flip (cdr tabulated-list-sort-key)))
          (setq tabulated-list-format (lispm-desktop-console--format)
                tabulated-list-sort-key (cons (car (aref tabulated-list-format index))
                                              flip))
          (tabulated-list-init-header)
          (lispm-desktop-console-refresh))))))

(provide 'lispm-desktop-console)
;;; lispm-desktop-console.el ends here
