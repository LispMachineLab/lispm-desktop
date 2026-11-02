;;; lispm-desktop-update-safety.el --- Check a Desktop release before switching -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Software Update and install.sh must check the incoming Desktop's
;; dependencies before changing its checkout.  A dependency skipped because
;; it has local changes is still a dependency.  Read the incoming declaration
;; as data, without loading the incoming Desktop or its applications.
;; This module also runs in a batch Emacs with the user's packages activated.

;;; Code:

(require 'cl-lib)
(require 'package)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-store-installer)

(defun lispm-desktop-update-safety--manifest (directory revision)
  "Read DIRECTORY's dependency declaration at REVISION without evaluating it.
Only the literal `lispm-desktop-deps' defconst is accepted.  An unreadable
or computed declaration stops the update instead of silently skipping it."
  (with-temp-buffer
    (insert (lispm-desktop-store--git
             directory "show" (concat revision ":lisp/lispm-desktop-deps.el")))
    (goto-char (point-min))
    (let (found)
      (condition-case nil
          (while (not found)
            (pcase (read (current-buffer))
              (`(defconst lispm-desktop-deps (quote ,entries) . ,_)
               (unless (and (consp entries)
                            (seq-every-p
                             (lambda (entry)
                               (and (consp entry) (symbolp (car entry))
                                    (stringp (plist-get (cdr entry) :checkout))))
                             entries))
                 (user-error "Desktop update stopped: invalid dependency declaration"))
               (setq found entries))))
        (end-of-file
         (user-error "Desktop update stopped: no literal dependency declaration in %s" revision)))
      found)))

(defun lispm-desktop-update-safety--main (feature spec directory)
  "Find FEATURE's effective source using SPEC and Desktop DIRECTORY.
Use the active load path first, then a sibling checkout, then an inactive
installed package, matching Desktop startup's precedence.  Do not activate
or install packages, and do not substitute a new sibling for an old package
already on the load path."
  (let* ((file (concat (symbol-name feature) ".el"))
         (lisp (plist-get spec :lisp-dir))
         (sibling (expand-file-name
                   (concat "../" (plist-get spec :checkout) "/"
                           (when lisp (concat lisp "/")) file)
                   (file-name-as-directory directory)))
         (desc (cadr (assq feature package-alist))))
    (or (locate-library file t)
        (and (file-readable-p sibling) sibling)
        (when desc
          (seq-find #'file-readable-p
                    (list (expand-file-name file (package-desc-dir desc))
                          (expand-file-name (concat (when lisp (concat lisp "/")) file)
                                            (package-desc-dir desc))))))))

(defun lispm-desktop-update-safety--planned (main updates)
  "Return the entry in UPDATES changing the repository that owns MAIN.
Match repository roots, not path prefixes: Desktop's checkout can contain
elpa repositories of its own, which a Desktop update does not change."
  (when-let* ((root (ignore-errors
                     (lispm-desktop-store--git (file-name-directory main)
                                              "rev-parse" "--show-toplevel"))))
    (seq-find
     (lambda (update)
       (and (plist-get update :to)
            (equal (file-truename root)
                   (directory-file-name
                    (file-truename (plist-get update :directory))))))
     updates)))

(defun lispm-desktop-update-safety--version (text)
  "Read a package version from source TEXT, or nil if it is not declared."
  (ignore-errors
    (with-temp-buffer
      (insert text)
      (package-desc-version (package-buffer-info)))))

(defun lispm-desktop-update-check-dependencies (directory revision &optional updates)
  "Check Desktop DIRECTORY at REVISION against effective dependencies.
UPDATES is Software Update's proposed list of plists with :directory and
:to; those source trees are checked in place of their current files.  With
nil UPDATES, verify only what the next startup will actually find on disk.
Return t on success, or signal `user-error' naming the unmet dependency.
No checkout, package build, application load, or candidate code evaluation
takes place.  Call again with nil UPDATES immediately before switching the
Desktop, after dependency updates have completed."
  (dolist (entry (lispm-desktop-update-safety--manifest directory revision))
    (pcase-let* ((`(,feature . ,spec) entry)
                 (minimum (plist-get spec :version))
                 (needs (plist-get spec :needs)))
      ;; Kernel is optional.  The running host binds this option; the batch
      ;; installer may not load host options, so absence alone is allowed for
      ;; an unconstrained dependency which startup can install normally.
      (unless (and (eq feature 'lispm-kernel)
                   (boundp 'lispm-desktop-kernel)
                   (not (symbol-value 'lispm-desktop-kernel)))
        (let ((main (lispm-desktop-update-safety--main feature spec directory)))
          (when (or main minimum needs)
            (unless main
              (user-error "Desktop update stopped: %s is missing; install it before updating Desktop" feature))
            (let* ((planned (lispm-desktop-update-safety--planned main updates))
                   (root (and planned (plist-get planned :directory)))
                   (target (and planned (plist-get planned :to)))
                   (relative (and root (file-relative-name (file-truename main) (file-truename root))))
                   (text (if planned
                             (lispm-desktop-store--git root "show" (concat target ":" relative))
                           (with-temp-buffer (insert-file-contents main) (buffer-string))))
                   (version (lispm-desktop-update-safety--version text)))
              (when (and minimum
                         (or (null version) (version-list-< version (version-to-list minimum))))
                (user-error
                 "Desktop update stopped: %s in %s needs version %s or later (found %s); update that dependency first. Desktop was kept unchanged"
                 feature (abbreviate-file-name (file-name-directory main)) minimum
                 (if version (package-version-join version) "no version")))
              (when needs
                (let ((library (concat needs ".el")))
                  (unless (if planned
                              (ignore-errors
                                (lispm-desktop-store--git
                                 root "cat-file" "-e"
                                 (concat target ":" (or (file-name-directory relative) "") library))
                                t)
                            (file-readable-p (expand-file-name library (file-name-directory main))))
                    (user-error "Desktop update stopped: %s in %s lacks %s; update that dependency first. Desktop was kept unchanged"
                                feature (abbreviate-file-name (file-name-directory main)) needs))))))))))
  t)

(provide 'lispm-desktop-update-safety)
;;; lispm-desktop-update-safety.el ends here
