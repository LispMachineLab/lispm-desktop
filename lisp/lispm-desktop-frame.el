;;; lispm-desktop-frame.el --- One frame clock for work drawn in pieces  -*- lexical-binding: t; -*-

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

;; Emacs runs Lisp, process output and redisplay on one thread, one thing
;; at a time.  What is done there in pieces as it arrives, terminals drawn
;; as their programs write, results of work done elsewhere taken in, is
;; done a frame at a time, and every such client shares one clock, so that
;; together they never take more of a frame than its budget, and give way
;; to the keyboard:
;;
;;   client --`lispm-desktop-frame-request'--> due
;;   one timer: a frame no sooner than `lispm-desktop-frame-interval'
;;              after the last
;;   frame: DEADLINE, its start plus `lispm-desktop-frame-budget'; the
;;          due clients by depth, each called with DEADLINE: the first
;;          always, the others while no key waits and DEADLINE is ahead
;;
;; A client is added with `lispm-desktop-frame-add-client'.  Terminals
;; are the client `terminals', at depth 0, see lispm-desktop-term.el.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'lispm-desktop-core)
(require 'lispm-desktop-i18n)

(defgroup lispm-desktop-frame nil
  "One frame clock for the work done in pieces, giving way to the keyboard."
  :group 'lispm-desktop)

;; The clock was the terminals' first; their settings are its own now.
(define-obsolete-variable-alias 'lispm-desktop-term-frame-interval
  'lispm-desktop-frame-interval "0.9.0")
(define-obsolete-variable-alias 'lispm-desktop-term-frame-budget
  'lispm-desktop-frame-budget "0.9.0")

(defcustom lispm-desktop-frame-interval 0.016
  "Seconds between two frames, at the least."
  :type 'number
  :group 'lispm-desktop-frame)

(defcustom lispm-desktop-frame-budget 0.008
  "Seconds one frame may take before its clients give way."
  :type 'number
  :group 'lispm-desktop-frame)

(defvar lispm-desktop-frame--clients nil
  "The clock's clients in the order they run, as (NAME FUNCTION DEPTH).
They are sorted by ascending DEPTH, those of one depth in the order they
were first added.")

(defvar lispm-desktop-frame--due nil
  "Names of the clients due in the next frame.")

(defvar lispm-desktop-frame--timer nil
  "Timer of the next frame, while one is due.")

(defvar lispm-desktop-frame--last 0
  "When the last frame started, a `float-time'.")

(defvar lispm-desktop-frame--running nil
  "Non-nil while a frame calls its clients.")

(defvar lispm-desktop-frame-last-error nil
  "The last error a client signalled, as (NAME . ERROR), or nil.")

(defun lispm-desktop-frame-add-client (name function &optional depth)
  "Make FUNCTION the clock's client NAME, a symbol, run at DEPTH.
In a frame where NAME is due, see `lispm-desktop-frame-request', FUNCTION
is called with one argument, DEADLINE, the `float-time' by which the
frame should be done; it returns non-nil when it has work left, and NAME
is then due again in the next frame.

Clients run by ascending DEPTH, 0 by default, those of one depth in the
order they were first added.  Adding NAME again replaces its FUNCTION and
DEPTH, and keeps it due when it was.  Return NAME."
  (let ((entry (list name function (or depth 0))))
    (setq lispm-desktop-frame--clients
          ;; A fresh list, sorted stably: a frame running goes on with the
          ;; clients it started with.
          (sort (if (assq name lispm-desktop-frame--clients)
                    (mapcar (lambda (client) (if (eq (car client) name) entry client))
                            lispm-desktop-frame--clients)
                  (append lispm-desktop-frame--clients (list entry)))
                (lambda (a b) (< (nth 2 a) (nth 2 b))))))
  name)

(defun lispm-desktop-frame-remove-client (name)
  "Make NAME no client of the clock any more, its request forgotten.
The timer of the next frame goes when no client is left due."
  (setq lispm-desktop-frame--clients
        (seq-remove (lambda (client) (eq (car client) name)) lispm-desktop-frame--clients)
        lispm-desktop-frame--due (delq name lispm-desktop-frame--due))
  (when (and lispm-desktop-frame--timer (null lispm-desktop-frame--due))
    (cancel-timer lispm-desktop-frame--timer)
    (setq lispm-desktop-frame--timer nil)))

(defun lispm-desktop-frame-request (name)
  "Make client NAME due, and schedule a frame.
The frame comes no sooner than `lispm-desktop-frame-interval' after the
last; one timer serves every client.  A name that is no client, see
`lispm-desktop-frame-add-client', is ignored.  Return NAME when it is due."
  (when (assq name lispm-desktop-frame--clients)
    (cl-pushnew name lispm-desktop-frame--due)
    ;; A frame running schedules the next itself once its clients ran.
    (unless lispm-desktop-frame--running
      (lispm-desktop-frame--schedule))
    name))

(defun lispm-desktop-frame--schedule ()
  "Schedule the next frame while a client is due and none is scheduled.
It comes no sooner than `lispm-desktop-frame-interval' after the last."
  (when (and lispm-desktop-frame--due (not lispm-desktop-frame--timer))
    (setq lispm-desktop-frame--timer
          (run-with-timer (max 0 (- (+ lispm-desktop-frame--last lispm-desktop-frame-interval)
                                    (float-time)))
                          nil #'lispm-desktop-frame--tick))))

(defun lispm-desktop-frame--run (name function deadline)
  "Call client NAME's FUNCTION with DEADLINE and return what it returns.
An error it signals is reported and kept in `lispm-desktop-frame-last-error';
NAME is then not due, so that it does not fail again every frame."
  (condition-case err
      (funcall function deadline)
    (error
     (setq lispm-desktop-frame-last-error (cons name err)
           lispm-desktop-frame--due (delq name lispm-desktop-frame--due))
     (message (lispm-desktop-tr "The frame clock's client %s failed: %s")
              name (error-message-string err))
     nil)))

(defun lispm-desktop-frame--tick ()
  "Run a frame: call each due client, by depth, with the frame's deadline.
The first due client always runs, as it gives way to the keyboard itself;
each after it runs only while no key is waiting and the deadline is
ahead, and stays due otherwise, for the next frame.  A client is no
longer due once called, unless it returns non-nil or asks again."
  (unless lispm-desktop-frame--running
    (when lispm-desktop-frame--timer
      (cancel-timer lispm-desktop-frame--timer)
      (setq lispm-desktop-frame--timer nil))
    (setq lispm-desktop-frame--last (float-time))
    (let ((deadline (+ lispm-desktop-frame--last lispm-desktop-frame-budget))
          (first t))
      (unwind-protect
          (let ((lispm-desktop-frame--running t))
            (pcase-dolist (`(,name ,function ,_) lispm-desktop-frame--clients)
              (when (and (memq name lispm-desktop-frame--due)
                         (or first (not (or (input-pending-p) (> (float-time) deadline)))))
                (setq first nil
                      lispm-desktop-frame--due (delq name lispm-desktop-frame--due))
                ;; A client that removed itself while it ran stays removed.
                (when (and (lispm-desktop-frame--run name function deadline)
                           (assq name lispm-desktop-frame--clients))
                  (cl-pushnew name lispm-desktop-frame--due)))))
        (lispm-desktop-frame--schedule)))))

(provide 'lispm-desktop-frame)
;;; lispm-desktop-frame.el ends here
