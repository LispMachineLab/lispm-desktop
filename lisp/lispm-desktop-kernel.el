;;; lispm-desktop-kernel.el --- Background work in lispm-kernel  -*- lexical-binding: t; -*-

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
;; (lispm-desktop-frame.el).  For now it reads the agents' sessions for the
;; sidebar, through the desktop's session readers; see
;; lispm-kernel-desktop.el in its repository.
;;
;; It is on unless `lispm-desktop-kernel' is nil.  Its Lisp is found or
;; installed as the other LispM projects are, see lispm-desktop-deps.el,
;; and it gets its program itself, in the background: downloaded as built
;; for this computer, and kept only when its SHA-256 is the one pinned in
;; the kernel's own repository (lisp/lispm-kernel-program-sums.eld);
;; nobody is made to build it, though `lispm-kernel-program-source' can
;; ask SBCL to build it.  Until the program is there, and wherever it
;; cannot be, the sidebar reads sessions as it does without the kernel,
;; and the kernel says why: in *Messages* alone, unless it was asked for.

;;; Code:

(require 'lispm-desktop-core)
(require 'lispm-desktop-deps)
(require 'lispm-desktop-i18n)

(defvar lispm-kernel-desktop-mode)
(declare-function lispm-kernel-desktop-mode "ext:lispm-kernel-desktop" (&optional arg))

(defcustom lispm-desktop-kernel t
  "Non-nil reads the agents' sessions in the background with lispm-kernel.
Its Lisp is found beside the desktop's checkout or installed with
package-vc, and its program downloaded for this computer, checked against
the SHA-256 its Lisp pins; until then, and where it cannot be, the
sidebar reads the sessions as it does with nil.  Setting it once LispM is
set up takes effect at once."
  :type 'boolean
  :group 'lispm-desktop)

(defun lispm-desktop-kernel--asked-p ()
  "Return non-nil when `lispm-desktop-kernel' was set with Custom or
`setopt', rather than left as it is by default."
  (or (get 'lispm-desktop-kernel 'saved-value)
      (get 'lispm-desktop-kernel 'customized-value)))

(defun lispm-desktop-kernel-apply (on)
  "Turn lispm-kernel on when ON is non-nil, else off.
Turning it on gets the kernel's Lisp, see `lispm-desktop-deps-ensure', and
turns `lispm-kernel-desktop-mode' on, which gets its program in the
background; what goes wrong is said, and leaves the desktop as it was.
A batch Emacs, as an installer's, gets the Lisp and leaves the mode off."
  (if on
      (condition-case err
          (progn
            (lispm-desktop-deps-ensure 'lispm-kernel)
            (require 'lispm-kernel-desktop)
            (unless (or noninteractive lispm-kernel-desktop-mode)
              (lispm-kernel-desktop-mode 1)))
        ;; On by default, it says so in *Messages* alone, as where no
        ;; program is published for the computer it would at every start.
        (error (let ((inhibit-message (or inhibit-message (not (lispm-desktop-kernel--asked-p)))))
                 (message (lispm-desktop-tr "lispm-kernel could not be turned on: %s")
                          (error-message-string err)))))
    (when (bound-and-true-p lispm-kernel-desktop-mode)
      (lispm-kernel-desktop-mode -1))))

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
