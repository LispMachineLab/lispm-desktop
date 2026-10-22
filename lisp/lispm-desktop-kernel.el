;;; lispm-desktop-kernel.el --- Background work in lispm-kernel, once asked for  -*- lexical-binding: t; -*-

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

;; lispm-kernel is LispM's runtime for work Emacs should not wait on: a
;; Common Lisp program, SBCL's runtime and the kernel's core in one
;; executable, that Emacs runs as a process.  The two talk in
;; S-expressions; it does the work on threads of its own and Emacs takes
;; the results in a little at a time, on the desktop's frame clock
;; (lispm-desktop-frame.el).  It reads the agents' sessions for the
;; sidebar, through the desktop's session readers, and makes the downloads
;; package.el makes in the background, as the App Store's refresh of the
;; package archives, which url.el would make on Emacs's thread; see
;; lispm-kernel-desktop.el and lispm-kernel-package.el in its repository.
;;
;; It is on unless `lispm-desktop-kernel' is nil: Use LispM Kernel in
;; Control Panels' Kernel panel, `setopt', or Custom, where toggling it
;; applies it.  Either takes effect at once, and nothing needs
;; restarting: turned on, the kernel starts; turned off, it stops; one
;; that exits is started again by itself.  Its Lisp is found or installed
;; as the other LispM projects are, see lispm-desktop-deps.el, and its
;; program is downloaded as built for this computer, and kept only when
;; its SHA-256 is the one pinned in the kernel's own repository
;; (lisp/lispm-kernel-program-sums.eld); nobody is made to build it,
;; though `lispm-kernel-program-source' can ask SBCL to build it.  Turned
;; on by the user, what it lacks is got in the Installer, which shows how
;; far it is (lispm-desktop-needs.el); on by default, it is got quietly.
;; Until the program is there, and wherever it cannot be, the sidebar
;; reads sessions as it does without the kernel, url.el downloads for
;; package.el, and the kernel says why: in *Messages* alone, unless it
;; was asked for, and in the Kernel panel.

;;; Code:

(require 'lispm-desktop-core)
(require 'lispm-desktop-deps)
(require 'lispm-desktop-i18n)
(require 'lispm-desktop-loading)
(require 'lispm-desktop-needs)

(defvar lispm-kernel-desktop-mode)
(defvar lispm-kernel-package-mode)
(defvar lispm-kernel-desktop-program)
(defvar lispm-kernel-desktop-last-error)
(defvar lispm-kernel-program-problem)
(defvar lispm-kernel-program-progress)
(defvar lispm-kernel-program-source)
(defvar lispm-kernel-program-obtained-functions)
(defvar lispm-kernel-program-progress-functions)
(declare-function lispm-kernel-desktop-mode "ext:lispm-kernel-desktop" (&optional arg))
(declare-function lispm-kernel-package-mode "ext:lispm-kernel-package" (&optional arg))
(declare-function lispm-kernel-desktop-status "ext:lispm-kernel-desktop" ())
(declare-function lispm-kernel-desktop-restart "ext:lispm-kernel-desktop" ())
(declare-function lispm-kernel-program-file "ext:lispm-kernel-program" ())
(declare-function lispm-kernel-program-obtain "ext:lispm-kernel-program" ())
(declare-function lispm-kernel-program-stop "ext:lispm-kernel-program" ())
(declare-function lispm-kernel-program--busy-p "ext:lispm-kernel-program" ())
(declare-function custom-variable-save "cus-edit" (widget))
(declare-function widget-default-notify "wid-edit" (widget child &optional event))

(defcustom lispm-desktop-kernel t
  "Non-nil reads the agents' sessions in the background with lispm-kernel,
and has it make package.el's downloads in the background.
It takes effect at once, set from Control Panels' Kernel panel, with
`setopt', or in Custom, where toggling it applies and saves it.  What the
kernel lacks, its Lisp and its program, is got: in the Installer, which
shows how far it is, once it was asked for; quietly while it is on by
default.  Until then, and where it cannot be, the sidebar reads the
sessions, and url.el downloads, as with nil."
  :type '(boolean :notify lispm-desktop-kernel--toggled)
  :group 'lispm-desktop)

(defun lispm-desktop-kernel--asked-p ()
  "Return non-nil when `lispm-desktop-kernel' was set with Custom or
`setopt', rather than left as it is by default."
  (or (get 'lispm-desktop-kernel 'saved-value)
      (get 'lispm-desktop-kernel 'customized-value)))

(defun lispm-desktop-kernel--toggled (widget child &optional event)
  "Apply and save `lispm-desktop-kernel' once Custom's toggle changed it,
as a check box does, rather than when Apply is pressed.
This is the :notify of the option's value WIDGET; CHILD and EVENT are
passed on to Custom."
  (widget-default-notify widget child event)
  (let ((variable (widget-get widget :parent)))
    ;; A widget is a list, its type first.
    (when (and variable (eq (car variable) 'custom-variable))
      ;; Once the toggle is done with: saving draws the option anew.
      (run-at-time 0 nil (lambda ()
                           (when (and (widget-get variable :custom-state)
                                      (not (eq (widget-get variable :custom-state) 'saved)))
                             (custom-variable-save variable)))))))

;;;; What it needs

(defun lispm-desktop-kernel--program-p ()
  "Return non-nil when lispm-kernel's program is there, or one is named."
  (or (bound-and-true-p lispm-kernel-desktop-program)
      (and (fboundp 'lispm-kernel-program-file) (lispm-kernel-program-file))))

(defun lispm-desktop-kernel--load ()
  "Load the kernel's Lisp, found where it is, see
`lispm-desktop-deps-ensure', installed when it is not."
  (lispm-desktop-deps-ensure 'lispm-kernel)
  (require 'lispm-kernel-desktop))

(defun lispm-desktop-kernel-installed-p ()
  "Return non-nil when lispm-kernel can run here as it is: its Lisp is
found, nothing installed for it, and its program is there."
  (and (lispm-desktop-deps-present-p 'lispm-kernel)
       (progn (lispm-desktop-kernel--load)
              (lispm-desktop-kernel--program-p))))

(defun lispm-desktop-kernel--getting-p ()
  "Return non-nil while lispm-kernel's program is downloaded or built."
  (and (fboundp 'lispm-kernel-program--busy-p) (lispm-kernel-program--busy-p)))

(defun lispm-desktop-kernel--problem ()
  "Return why lispm-kernel's program could not be had, a sentence."
  (or (bound-and-true-p lispm-kernel-program-problem)
      (lispm-desktop-tr "Its program could not be had.")))

(defun lispm-desktop-kernel-need ()
  "Return lispm-kernel as the Installer gets it, a need of
lispm-desktop-needs.el: its Lisp, then its program."
  (list 'program "lispm-kernel"
        :for (lispm-desktop-tr "background work, as reading the agents' sessions")
        :present #'lispm-desktop-kernel-installed-p
        :install #'lispm-desktop-kernel--install))

(defun lispm-desktop-kernel--follow (report done)
  "Call REPORT as lispm-kernel's program is downloaded or built, and DONE
once it is there, or not; return what stops it.  REPORT and DONE are as
a need's :install takes them."
  (let* ((note (if (eq (bound-and-true-p lispm-kernel-program-source) 'build)
                   (lispm-desktop-tr "Building its program with SBCL…")
                 (lispm-desktop-tr "Downloading its program…")))
         (progress nil)
         (obtained nil)
         (forget (lambda ()
                   (remove-hook 'lispm-kernel-program-progress-functions progress)
                   (remove-hook 'lispm-kernel-program-obtained-functions obtained))))
    (setq progress (lambda (fraction) (funcall report note fraction))
          obtained (lambda (there)
                     (funcall forget)
                     (funcall done (unless there (lispm-desktop-kernel--problem)))))
    (add-hook 'lispm-kernel-program-progress-functions progress)
    (add-hook 'lispm-kernel-program-obtained-functions obtained)
    (funcall report note (bound-and-true-p lispm-kernel-program-progress))
    (lambda ()
      (funcall forget)
      (when (fboundp 'lispm-kernel-program-stop)
        (lispm-kernel-program-stop)))))

(defun lispm-desktop-kernel--install (report done)
  "Get what lispm-kernel lacks, for the Installer: its Lisp, then its
program, downloaded, or built when `lispm-kernel-program-source' says so.
REPORT and DONE are as a need's :install takes them, see
lispm-desktop-needs.el; return what stops the program's download, or nil
once it is done."
  (unless (lispm-desktop-deps-present-p 'lispm-kernel)
    (funcall report (lispm-desktop-tr "Getting its Lisp…"))
    ;; package-vc installs it here, Emacs waiting: shown first.
    (redisplay t))
  (lispm-desktop-with-loading lispm-desktop-needs-buffer-name
    (lispm-desktop-kernel--load))
  (if (lispm-desktop-kernel--program-p)
      (ignore (funcall done nil))
    (pcase (lispm-kernel-program-obtain)
      ('ready (ignore (funcall done nil)))
      ('pending (lispm-desktop-kernel--follow report done))
      (_ (ignore (funcall done (lispm-desktop-kernel--problem)))))))

(defun lispm-desktop-kernel--when-started (function)
  "Call FUNCTION now, or once Emacs has started, while it starts: the
Installer's window opens over the desktop."
  (if after-init-time
      (funcall function)
    (letrec ((once (lambda ()
                     (remove-hook 'window-setup-hook once)
                     (funcall function))))
      (add-hook 'window-setup-hook once))))

(defun lispm-desktop-kernel--get ()
  "Have the Installer get what lispm-kernel lacks, then turn it on, unless
`lispm-desktop-kernel' was set to nil meanwhile."
  (lispm-desktop-kernel--when-started
   (lambda ()
     (lispm-desktop-needs-install
      (list (lispm-desktop-kernel-need)) "LispM"
      (lambda (present)
        (when (and present lispm-desktop-kernel)
          (lispm-desktop-kernel-apply t t)))))))

;;;; On and off

(defun lispm-desktop-kernel--turn-on ()
  "Turn on `lispm-kernel-desktop-mode', which gets the kernel's program in
the background, then `lispm-kernel-package-mode', where the kernel's Lisp
has it."
  (lispm-desktop-kernel--load)
  (unless lispm-kernel-desktop-mode
    (lispm-kernel-desktop-mode 1))
  (when (and (require 'lispm-kernel-package nil t) (not lispm-kernel-package-mode))
    (lispm-kernel-package-mode 1)))

(defun lispm-desktop-kernel-apply (on &optional asked)
  "Turn lispm-kernel on when ON is non-nil, else off, at once.
On, it starts as soon as it has what it needs.  When it was ASKED for,
by default when `lispm-desktop-kernel' was set rather than left on by
default, the Installer gets what it lacks, showing how far it is, see
`lispm-desktop-kernel-need'; else that is got quietly.  Until then the
sidebar reads the sessions itself.  What goes wrong is said, in
*Messages* alone unless it was asked for, and leaves the desktop as it
was.  A batch Emacs, as an installer's, gets the Lisp and leaves the
modes off."
  (if on
      (let ((asked (or asked (lispm-desktop-kernel--asked-p))))
        (condition-case err
            (cond
             (noninteractive (lispm-desktop-deps-ensure 'lispm-kernel))
             ((or (not asked) (lispm-desktop-deps-present-p 'lispm-kernel))
              (lispm-desktop-kernel--turn-on)
              ;; Its program comes in the background: the Installer shows
              ;; how far.
              (when (and asked (lispm-desktop-kernel--getting-p))
                (lispm-desktop-kernel--get)))
             (t (lispm-desktop-kernel--get)))
          ;; On by default, it says so in *Messages* alone, as where no
          ;; program is published for the computer it would at every start.
          (error (let ((inhibit-message (or inhibit-message (not asked))))
                   (message (lispm-desktop-tr "lispm-kernel could not be turned on: %s")
                            (error-message-string err))))))
    (when (bound-and-true-p lispm-kernel-package-mode)
      (lispm-kernel-package-mode -1))
    (when (bound-and-true-p lispm-kernel-desktop-mode)
      (lispm-kernel-desktop-mode -1))))

(defun lispm-desktop-kernel-restart ()
  "Start lispm-kernel again at once, or try again to turn it on."
  (interactive)
  (cond ((not lispm-desktop-kernel)
         (user-error (lispm-desktop-tr "lispm-kernel is off")))
        ((not (bound-and-true-p lispm-kernel-desktop-mode))
         (lispm-desktop-kernel-apply t t))
        ((fboundp 'lispm-kernel-desktop-restart)
         (lispm-kernel-desktop-restart))
        (t (lispm-kernel-desktop-mode -1)
           (lispm-kernel-desktop-mode 1))))

(defun lispm-desktop-kernel--why (error)
  "Return ERROR, a sentence, an error or a summary, as a sentence."
  (cond ((stringp error) error)
        ((and (consp error) (symbolp (car error)) (get (car error) 'error-conditions))
         (error-message-string error))
        (error (format "%s" error))))

(defun lispm-desktop-kernel-status ()
  "Return how lispm-kernel stands, for Control Panels to say:
  (off)                   `lispm-desktop-kernel' is nil
  (installing FRACTION)   what it lacks is got, FRACTION of it, or nil
                          while that is not known
  (running PID SINCE)     it runs as the process PID since SINCE, a
                          `float-time'; both nil when not known
  (exited WHY AT)         it exited, WHY says why, and starts again by
                          itself at AT, a `float-time'
  (stopped WHY)           it is on, but does not run, WHY says why"
  (let ((job (lispm-desktop-needs-job-state (lispm-desktop-kernel-need))))
    (cond ((not lispm-desktop-kernel) '(off))
          ((memq (car job) '(waiting installing)) (list 'installing (nth 1 job)))
          ((not (bound-and-true-p lispm-kernel-desktop-mode))
           (list 'stopped (or (nth 2 job)
                              (bound-and-true-p lispm-kernel-program-problem)
                              (lispm-desktop-kernel--why (bound-and-true-p lispm-kernel-desktop-last-error)))))
          ((fboundp 'lispm-kernel-desktop-status)
           (pcase (lispm-kernel-desktop-status)
             (`(running ,process ,since) (list 'running (process-id process) since))
             (`(exited ,why ,at) (list 'exited why at))
             (`(getting ,fraction) (list 'installing fraction))
             (_ (list 'stopped nil))))
          ((lispm-desktop-kernel--getting-p) (list 'installing nil))
          (t (list 'running nil nil)))))

(defun lispm-desktop-kernel--changed (_symbol value operation _where)
  "Apply VALUE, set to `lispm-desktop-kernel' by OPERATION.
This watches the variable once LispM is set up; binding it does nothing."
  (when (eq operation 'set)
    (lispm-desktop-kernel-apply value)))

(defun lispm-desktop-setup-kernel ()
  "Turn lispm-kernel on when `lispm-desktop-kernel' says so, and follow it.
While it is nil nothing is loaded, nor turned off."
  (when lispm-desktop-kernel
    (lispm-desktop-kernel-apply t))
  (add-variable-watcher 'lispm-desktop-kernel #'lispm-desktop-kernel--changed))

(provide 'lispm-desktop-kernel)
;;; lispm-desktop-kernel.el ends here
