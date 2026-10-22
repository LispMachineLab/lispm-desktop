;;; lispm-desktop-deps.el --- The LispM projects the desktop runs  -*- lexical-binding: t; -*-

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

;; LispM Desktop runs other LispM projects, each a repository of its own:
;; lispm-agent, LispM's agents, lispm-browser, its browser, and, once
;; `lispm-desktop-kernel' asks for it, lispm-kernel, its runtime for
;; background work.  Each is found where it already is, else in a checkout
;; beside the desktop's, else its latest release is installed from its
;; repository with package-vc:
;;
;;   on `load-path'        a copy the user's own init file put there
;;   ../lispm-agent/lisp   a checkout beside this one, as a developer
;;   ../lispm-browser/lisp   keeps them
;;   ../lispm-kernel/lisp
;;   package-vc            the first start of everyone else, once
;;
;; The packages their main files require are installed as package.el
;; would install them for the package.
;;
;; A batch Emacs installing packages, as LispM.app's installer runs one,
;; can say each it installs, and how many there are, with
;; `lispm-desktop-deps-batch-progress'.

;;; Code:

(require 'package)
(require 'seq)
(require 'lispm-desktop-core)

(defconst lispm-desktop-deps
  '((lispm :url "https://github.com/LispMachineLab/lispm-agent" :checkout "lispm-agent")
    (lispm-browser :url "https://github.com/LispMachineLab/lispm-browser" :checkout "lispm-browser")
    (lispm-kernel :url "https://github.com/LispMachineLab/lispm-kernel" :checkout "lispm-kernel"))
  "The LispM projects the desktop runs: (FEATURE :url URL :checkout NAME).
FEATURE is the project's main file, in lisp/ of its repository at URL;
NAME is the directory of a checkout of it beside the desktop's.")

(defun lispm-desktop-deps--checkout (name)
  "Return lisp/ of the checkout NAME beside the desktop's, or nil."
  (let ((directory (expand-file-name (concat "../" name "/lisp") lispm-desktop-directory)))
    (and (file-directory-p directory) directory)))

(defun lispm-desktop-deps--requirements (file)
  "Return the packages FILE, a package's main file, requires and lacks."
  (seq-remove (lambda (dep)
                (or (eq (car dep) 'emacs)
                    (package-installed-p (car dep) (cadr dep))))
              (package-desc-reqs
               (with-temp-buffer
                 (insert-file-contents file)
                 (package-buffer-info)))))

(defun lispm-desktop-deps-ensure (feature)
  "Make FEATURE, a project of `lispm-desktop-deps', loadable.
A copy already on `load-path' is used as it is; else a checkout beside the
desktop's joins it, see `lispm-desktop-deps--checkout'; else the latest
release of the project is installed from its repository with package-vc,
the commit that last changed its Version header.  The packages its main
file requires, and lacks, are installed."
  (pcase-let* ((`(,url ,checkout) (let ((spec (alist-get feature lispm-desktop-deps)))
                                    (list (plist-get spec :url) (plist-get spec :checkout))))
               (file (concat (symbol-name feature) ".el")))
    (unless (locate-library file t)
      (if-let* ((directory (lispm-desktop-deps--checkout checkout)))
          (add-to-list 'load-path directory)
        (package-vc-install (list feature :url url :lisp-dir "lisp") :last-release)))
    (when-let* ((main (locate-library file t))
                (missing (lispm-desktop-deps--requirements main)))
      (package-refresh-contents)
      (dolist (dep missing)
        (package-install (car dep))))))

;;;; Saying what installs

(defconst lispm-desktop-deps-progress-tag "LISPM-PACKAGE "
  "What begins a line in which a batch Emacs says the package it installs.")

(defvar lispm-desktop-deps--progress nil
  "While a batch Emacs says the packages it installs: (DONE . TOTAL), how
many it has installed and how many it has to, nil before the first.")

(defun lispm-desktop-deps--required ()
  "Return what the desktop, and the projects of `lispm-desktop-deps' on
`load-path', require and lack: their main files' requirements."
  (seq-mapcat (lambda (feature)
                (when-let* ((main (locate-library (concat (symbol-name feature) ".el") t)))
                  (lispm-desktop-deps--requirements main)))
              (cons 'lispm-desktop (mapcar #'car lispm-desktop-deps))))

(defun lispm-desktop-deps--installing (desc)
  "Say that the package DESC installs now, and the how many-th it is of how
many: at the first, those `lispm-desktop-deps--required' needs installed,
theirs counted; a package beyond them counts one more."
  (let ((progress (or lispm-desktop-deps--progress
                      (setq lispm-desktop-deps--progress
                            (cons 0 (length (ignore-errors
                                              (package-compute-transaction
                                               nil (lispm-desktop-deps--required)))))))))
    (setcar progress (1+ (car progress)))
    (setcdr progress (max (car progress) (cdr progress)))
    ;; Unlike `princ', which batch Emacs buffers until it exits, this is
    ;; read at once.
    (send-string-to-terminal
     (format "\n%s%s (%d/%d)\n" lispm-desktop-deps-progress-tag
             (package-desc-name desc) (car progress) (cdr progress)))))

(defun lispm-desktop-deps-batch-progress ()
  "Make this batch Emacs say each package it installs from the archives,
a line each on its standard output: `lispm-desktop-deps-progress-tag',
the package, and how many it has installed of how many, as
\"magit (12/33)\".  LispM.app's installer calls it before the desktop's
init.el, which installs whatever is missing, and shows those lines."
  (advice-add 'package-install-from-archive :before #'lispm-desktop-deps--installing))

(provide 'lispm-desktop-deps)
;;; lispm-desktop-deps.el ends here
