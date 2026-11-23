;;; lispm-desktop-dir-locals.el --- Safe .dir-locals.el auto-mode-alist in Emacs 31.1  -*- lexical-binding: t; -*-

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

;; In GNU Emacs 31.1 and unpatched builds of 32.0.50 (CVE-2026-131959,
;; bug#82213), `set-auto-mode--find-matching-alist-entry' runs the function
;; of a `(REGEXP FUNCTION t)' entry in `.dir-locals.el''s `auto-mode-alist'
;; as soon as an earlier entry matches the stripped file name, before
;; `set-auto-mode--apply-alist' asks `set-auto-mode--dir-local-valid-p'
;; whether `.dir-locals.el' may name it.  Visiting any file under an
;; untrusted directory can therefore run arbitrary Lisp without asking.
;;
;; Upstream commit 205ae0b0583dbfe09e258164adbc49e130645221 replaces
;; `set-auto-mode--find-matching-alist-entry' with
;; `set-auto-mode--from-matching-alist-entry', checking every mode from
;; `.dir-locals.el' before calling `set-auto-mode-0'.
;; `lispm-desktop-early.el', `lispm-desktop.el' and the App Store's batch
;; installer mend an unpatched Emacs at once; in Emacs 30 and in builds that
;; carry the upstream fix already, `lispm-desktop-dir-locals-mend' does
;; nothing.

;;; Code:

(defvar auto-mode-case-fold)
(declare-function set-auto-mode-0 "files" (mode &optional keep-mode-if-same))
(declare-function set-auto-mode--dir-local-valid-p "files" (mode))
(declare-function set-auto-mode--from-matching-alist-entry "files"
                  (alist name case-insensitive keep-mode-if-same dir-local))

(defun lispm-desktop-dir-locals-vulnerable-p ()
  "Return non-nil when `files' carries unpatched CVE-2026-131959 helpers."
  (and (fboundp 'set-auto-mode--find-matching-alist-entry)
       (not (fboundp 'set-auto-mode--from-matching-alist-entry))))

(defun lispm-desktop-dir-locals--alist-match (alist name case-insensitive)
  "Return first matching entry in ALIST for file NAME.
When CASE-INSENSITIVE is non-nil, match ignoring case."
  (if case-insensitive
      (let ((case-fold-search t))
        (assoc-default name alist #'string-match))
    (or (let ((case-fold-search nil))
          (assoc-default name alist #'string-match))
        (and auto-mode-case-fold
             (let ((case-fold-search t))
               (assoc-default name alist #'string-match))))))

(defun lispm-desktop-dir-locals--compound-entry-p (entry)
  "Return non-nil when ENTRY has the compound `(MODE NON-NIL)' alist shape."
  (and (not (functionp entry))
       (consp entry)
       (consp (cdr entry))
       (not (null (cadr entry)))))

(defun lispm-desktop-dir-locals--from-matching-alist-entry
    (alist name case-insensitive keep-mode-if-same dir-local)
  "Apply matching modes in ALIST for file NAME.
CASE-INSENSITIVE, KEEP-MODE-IF-SAME and DIR-LOCAL follow `files.el':
when DIR-LOCAL is non-nil, every candidate mode must satisfy
`set-auto-mode--dir-local-valid-p' before `set-auto-mode-0' runs it."
  (let (was-set)
    (while
        (let* ((newmode (lispm-desktop-dir-locals--alist-match
                         alist name case-insensitive))
               (mode-t (when (lispm-desktop-dir-locals--compound-entry-p newmode)
                         (setq name (substring name 0 (match-beginning 0)))
                         (car newmode)))
               (mode (or mode-t newmode)))
          (when newmode
            (if (and dir-local
                     (not (set-auto-mode--dir-local-valid-p mode)))
                (message "Ignoring invalid mode `%S'" mode)
              (set-auto-mode-0 mode keep-mode-if-same)
              (setq was-set t))
            mode-t)))
    was-set))

(defun lispm-desktop-dir-locals--apply-alist (alist keep-mode-if-same dir-local)
  "Apply `auto-mode-alist'-shaped ALIST to the current buffer safely.
KEEP-MODE-IF-SAME and DIR-LOCAL follow `set-auto-mode--apply-alist'."
  (when buffer-file-name
    (save-match-data
      (let ((name (file-name-sans-versions buffer-file-name))
            (remote-id (file-remote-p buffer-file-name))
            (case-insensitive-p (file-name-case-insensitive-p buffer-file-name)))
        (when (and (stringp remote-id)
                   (string-match (regexp-quote remote-id) name))
          (setq name (substring name (match-end 0))))
        (set-auto-mode--from-matching-alist-entry
         alist name case-insensitive-p keep-mode-if-same dir-local)))))

(defun lispm-desktop-dir-locals-mend ()
  "Mend CVE-2026-131959 in an unpatched Emacs 31.1 or 32.0.50."
  (when (lispm-desktop-dir-locals-vulnerable-p)
    (defalias 'set-auto-mode--from-matching-alist-entry
      #'lispm-desktop-dir-locals--from-matching-alist-entry)
    (defalias 'set-auto-mode--apply-alist
      #'lispm-desktop-dir-locals--apply-alist)
    (fmakunbound 'set-auto-mode--find-matching-alist-entry)))

(provide 'lispm-desktop-dir-locals)
;;; lispm-desktop-dir-locals.el ends here
