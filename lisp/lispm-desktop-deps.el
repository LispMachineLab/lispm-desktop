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

;; LispM Desktop runs two other LispM projects, each a repository of its
;; own: lispm-agent, LispM's agents, and lispm-browser, its browser.  Each
;; is found where it already is, else in a checkout beside the desktop's,
;; else its latest release is installed from its repository with package-vc:
;;
;;   on `load-path'        a copy the user's own init file put there
;;   ../lispm-agent/lisp   a checkout beside this one, as a developer
;;   ../lispm-browser/lisp   keeps the three
;;   package-vc            the first start of everyone else, once
;;
;; The packages their main files require are installed as package.el
;; would install them for the package.

;;; Code:

(require 'package)
(require 'seq)
(require 'lispm-desktop-core)

(defconst lispm-desktop-deps
  '((lispm :url "https://github.com/LispMachineLab/lispm-agent" :checkout "lispm-agent")
    (lispm-browser :url "https://github.com/LispMachineLab/lispm-browser" :checkout "lispm-browser"))
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

(provide 'lispm-desktop-deps)
;;; lispm-desktop-deps.el ends here
