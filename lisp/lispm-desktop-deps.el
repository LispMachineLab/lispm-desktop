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
;; lispm-agent, LispM's agents, lispm-browser, its browser, and, unless
;; `lispm-desktop-kernel' is nil, lispm-kernel, its runtime for
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
;; package-vc installs a project's latest release, the newest tag vX.Y.Z,
;; as install.sh checks it out: package-vc's own :last-release finds none
;; for a main file under lisp/, and would install the main branch.  The
;; packages their main files require are installed as package.el would
;; install them for the package.
;;
;; A project may have to be new enough: the desktop needs lispm-agent's
;; editor's agents, lispm-agents.el, which releases before 0.4.0 lack.
;; A copy installed as a package, which Software Update moved on only
;; from 0.9.1, is then updated to its latest release before anything of
;; it loads, in an Emacs of its own (lispm-desktop-store-installer.el);
;; of a checkout, the desktop says what to update.

;;; Code:

(require 'package)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-i18n)
(require 'lispm-desktop-store-installer)

(defconst lispm-desktop-deps
  '((lispm :url "https://github.com/LispMachineLab/lispm-agent" :checkout "lispm-agent"
           :needs "lispm-agents")
    (lispm-browser :url "https://github.com/LispMachineLab/lispm-browser" :checkout "lispm-browser")
    (lispm-kernel :url "https://github.com/LispMachineLab/lispm-kernel" :checkout "lispm-kernel"))
  "The LispM projects the desktop runs: (FEATURE :url URL :checkout NAME
:needs LIBRARY).  FEATURE is the project's main file, in lisp/ of its
repository at URL; NAME is the directory of a checkout of it beside the
desktop's; LIBRARY, when given, a library of it the desktop needs, which
older releases lack.")

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

(defun lispm-desktop-deps--git (directory &rest args)
  "Run git with ARGS in DIRECTORY and wait; return what it printed, trimmed,
or nil when it failed."
  (let ((default-directory (file-name-as-directory directory))
        (process-environment (lispm-desktop-store--git-environment)))
    (with-temp-buffer
      (when (eql 0 (apply #'call-process "git" nil '(t nil) nil args))
        (string-trim (buffer-string))))))

(defun lispm-desktop-deps--release (directory)
  "Return the commit of the latest release of the git checkout DIRECTORY,
its tags fetched first: of the highest vX.Y.Z tag on its origin's main
branch.  Return nil when there is none, or the fetch failed."
  (when-let* (((lispm-desktop-deps--git directory "fetch" "--quiet" "--tags" "origin"))
              (tags (lispm-desktop-deps--git directory "tag" "--list" "v*" "--sort=-v:refname"
                                             "--merged" "origin/HEAD"))
              (tag (seq-find (lambda (tag) (string-match-p "\\`v[0-9]+\\.[0-9]+\\.[0-9]+\\'" tag))
                             (split-string tags "\n" t))))
    (lispm-desktop-deps--git directory "rev-parse" (concat tag "^{commit}"))))

(defun lispm-desktop-deps--release-tag (url)
  "Return the tag of the latest release of the repository at URL, as
\"v0.2.0\", or nil when it has none or cannot be reached."
  (with-temp-buffer
    (let ((process-environment (lispm-desktop-store--git-environment)))
      (when (eql 0 (ignore-errors
                     (call-process "git" nil '(t nil) nil "ls-remote" "--tags" "--refs" url "v*")))
        (car (sort (seq-keep (lambda (line)
                               (and (string-match "refs/tags/\\(v[0-9]+\\.[0-9]+\\.[0-9]+\\)\\'" line)
                                    (match-string 1 line)))
                             (split-string (buffer-string) "\n" t))
                   (lambda (a b) (version< (substring b 1) (substring a 1)))))))))

(defun lispm-desktop-deps-present-p (feature)
  "Return non-nil when FEATURE, a project of `lispm-desktop-deps', is here
without installing anything: on `load-path', or in a checkout beside the
desktop's."
  (or (locate-library (concat (symbol-name feature) ".el") t)
      (lispm-desktop-deps--checkout (plist-get (alist-get feature lispm-desktop-deps) :checkout))))

(defun lispm-desktop-deps--package (main)
  "Return the package installed from git that holds MAIN, a project's main
file, or nil when it is in no such package."
  (seq-some (lambda (entry)
              (let ((desc (cadr entry)))
                (and (package-vc-p desc)
                     (string-prefix-p (file-name-as-directory (file-truename (package-desc-dir desc)))
                                      (file-truename main))
                     desc)))
            package-alist))

(defun lispm-desktop-deps--catch-up (main needs)
  "Make the project whose main file is MAIN have NEEDS, a library of it
the desktop needs: a package installed from git is updated to its
latest release, in an Emacs of its own; anything else is an error saying
what to update."
  (let* ((desc (lispm-desktop-deps--package main))
         (directory (and desc (package-desc-dir desc)))
         (release (and directory (lispm-desktop-deps--release directory))))
    (unless release
      (error (lispm-desktop-tr "%s lacks %s, which this LispM Desktop needs; update it to its latest release")
             (abbreviate-file-name (file-name-directory main)) needs))
    (message (lispm-desktop-tr "Updating %s to its latest release, which this LispM Desktop needs…")
             (abbreviate-file-name directory))
    (lispm-desktop-store-installer-call
     `(update ,(package-desc-name desc) ,release ,(lispm-desktop-deps--git directory "rev-parse" "HEAD")))
    ;; Built again: what it autoloads now.
    (load (expand-file-name (format "%s-autoloads" (package-desc-name desc)) directory) t t)
    (unless (locate-library needs)
      (error (lispm-desktop-tr "%s lacks %s, which this LispM Desktop needs; update it to its latest release")
             (abbreviate-file-name directory) needs))))

(defun lispm-desktop-deps-ensure (feature)
  "Make FEATURE, a project of `lispm-desktop-deps', loadable.
A copy already on `load-path' is used as it is; else a checkout beside the
desktop's joins it, see `lispm-desktop-deps--checkout'; else the latest
release of the project is installed from its repository with package-vc,
its newest tag vX.Y.Z, or the commit that last changed its Version header
before it has one.  The packages its main file requires, and lacks, are
installed.  A copy lacking the library the desktop needs of it, its
:needs, is brought up to date, see `lispm-desktop-deps--catch-up'."
  (pcase-let* ((spec (alist-get feature lispm-desktop-deps))
               (`(,url ,checkout ,needs) (list (plist-get spec :url) (plist-get spec :checkout)
                                               (plist-get spec :needs)))
               (file (concat (symbol-name feature) ".el")))
    (unless (locate-library file t)
      (if-let* ((directory (lispm-desktop-deps--checkout checkout)))
          (add-to-list 'load-path directory)
        (package-vc-install (list feature :url url :lisp-dir "lisp")
                            (or (lispm-desktop-deps--release-tag url) :last-release))))
    (when-let* ((main (locate-library file t)))
      (when (and needs (not (locate-library needs)))
        (lispm-desktop-deps--catch-up main needs))
      (when-let* ((missing (lispm-desktop-deps--requirements main)))
        (package-refresh-contents)
        (dolist (dep missing)
          (package-install (car dep)))))))

(provide 'lispm-desktop-deps)
;;; lispm-desktop-deps.el ends here
