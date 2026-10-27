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
;; lispm-kit, the engine its words are translated with, lispm-ui, the
;; library it draws forms and asks with, lispm-agent, LispM's agents,
;; lispm-browser, its browser, and, unless `lispm-desktop-kernel' is nil,
;; lispm-kernel, its runtime for background work.  Each is found where
;; it already is, else in a checkout beside the desktop's, else its
;; latest release is installed from its repository with package-vc:
;;
;;   on `load-path'        a copy the user's own init file put there
;;   ../lispm-agent/lisp   a checkout beside this one, as a developer
;;   ../lispm-browser/lisp   keeps them; a project whose sources are at
;;   ../lispm-kernel/lisp    the top of its repository, as a library's,
;;                           is the checkout itself
;;   package-vc            the first start of everyone else, once
;;
;; package-vc installs a project's latest release, the newest tag vX.Y.Z,
;; as install.sh checks it out: package-vc's own :last-release finds none
;; for a main file under lisp/, and would install the main branch.  The
;; packages their main files require are installed as package.el would
;; install them for the package, but for those found already: on
;; `load-path', or a project of the desktop's here without installing.
;;
;; A project may have to be new enough: the desktop needs lispm-agent's
;; editor's agents, lispm-agents.el, which releases before 0.4.0 lack,
;; and a project may have to be of a version at least.  A copy installed
;; as a package, which Software Update moved on only from 0.9.1, is then
;; updated to its latest release before anything of it loads, in an Emacs
;; of its own (lispm-desktop-store-installer.el); of a checkout, the
;; desktop says what to update.
;;
;; A batch Emacs installing packages, as LispM.app's installer runs one,
;; can say each it installs, and how many there are, with
;; `lispm-desktop-deps-batch-progress'.
;;
;; lispm-desktop-i18n.el finds LispM Kit with this file before the desktop
;; has words to show, so it requires nothing that shows them: what it
;; says is in English until they are loaded.

;;; Code:

(require 'package)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-store-installer)

(declare-function lispm-desktop-tr "lispm-desktop-i18n" (message &rest args))

(defun lispm-desktop-deps--tr (message &rest args)
  "Return MESSAGE formatted with ARGS, translated as `lispm-desktop-tr'
translates it once the desktop's words are loaded, else as written: as
while LispM Kit, which translates them, is being installed."
  (apply (if (fboundp 'lispm-desktop-tr) #'lispm-desktop-tr #'format) message args))

(defconst lispm-desktop-deps--directory
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "The desktop's directory, a checkout of lispm-desktop, as
`lispm-desktop-directory' says once the desktop's core is loaded.")

(defconst lispm-desktop-deps
  '((lispm-kit :url "https://github.com/LispMachineLab/lispm-kit" :checkout "lispm-kit"
               ;; Newer than 0.1.0, whose engine translates nothing.
               :needs "lispm-kit-i18n")
    (lispm-ui :url "https://github.com/LispMachineLab/lispm-ui" :checkout "lispm-ui"
              :version "0.1.0")
    (lispm :url "https://github.com/LispMachineLab/lispm-agent" :checkout "lispm-agent"
           :lisp-dir "lisp"
           ;; Newer than lispm-agents, which the desktop needs too.
           :needs "lispm-exec")
    (lispm-browser :url "https://github.com/LispMachineLab/lispm-browser" :checkout "lispm-browser"
                   :lisp-dir "lisp")
    (lispm-kernel :url "https://github.com/LispMachineLab/lispm-kernel" :checkout "lispm-kernel"
                  :lisp-dir "lisp"))
  "The LispM projects the desktop runs: (FEATURE :url URL :checkout NAME
:lisp-dir DIRECTORY :needs LIBRARY :version VERSION).  FEATURE is the
project's main file, in DIRECTORY of its repository at URL, or at its
top without one; NAME is the directory of a checkout of it beside the
desktop's; LIBRARY, when given, a library of it the desktop needs, and
VERSION the version at least its main file says, which older releases
lack.")

(defun lispm-desktop-deps--checkout (name)
  "Return where the sources of the checkout NAME beside the desktop's are,
its :lisp-dir in `lispm-desktop-deps', or nil when there is none."
  (let* ((spec (cdr (seq-find (lambda (entry) (equal (plist-get (cdr entry) :checkout) name))
                              lispm-desktop-deps)))
         (lisp (plist-get spec :lisp-dir))
         (directory (expand-file-name (concat "../" name (and lisp (concat "/" lisp)))
                                      lispm-desktop-deps--directory)))
    (and (file-directory-p directory) directory)))

(defun lispm-desktop-deps--requirements (file)
  "Return the packages FILE, a package's main file, requires and lacks.
A requirement found already is no lack: on `load-path', as a checkout
of a LispM project the user keeps, or a project of the desktop's here
without installing, see `lispm-desktop-deps-present-p'."
  (seq-remove (lambda (dep)
                (or (eq (car dep) 'emacs)
                    (package-installed-p (car dep) (cadr dep))
                    (locate-library (concat (symbol-name (car dep)) ".el") t)
                    (and (assq (car dep) lispm-desktop-deps)
                         (lispm-desktop-deps-present-p (car dep)))))
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
  (lispm-desktop-store--release-tag url))

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

(defun lispm-desktop-deps--version (main)
  "Return the version MAIN, a project's main file, says, as a list, or nil
when it says none."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents main)
      (package-desc-version (package-buffer-info)))))

(defun lispm-desktop-deps--behind (main spec)
  "Return what the project whose main file is MAIN lacks that the desktop
needs of it, SPEC its plist in `lispm-desktop-deps', or nil: an error
message saying what to update."
  (let ((needs (plist-get spec :needs))
        (version (plist-get spec :version))
        (where (abbreviate-file-name (file-name-directory main))))
    (cond ((and needs (not (locate-library needs)))
           (lispm-desktop-deps--tr "%s lacks %s, which this LispM Desktop needs; update it to its latest release"
                                   where needs))
          ((when-let* ((version)
                       (has (lispm-desktop-deps--version main)))
             (version-list-< has (version-to-list version)))
           (lispm-desktop-deps--tr "%s is older than %s, which this LispM Desktop needs; update it to its latest release"
                                   where version)))))

(defun lispm-desktop-deps--catch-up (main spec)
  "Make the project whose main file is MAIN have what the desktop needs of
it, SPEC its plist in `lispm-desktop-deps': a package installed from
git is updated to its latest release, in an Emacs of its own; anything
else is an error saying what to update."
  (let* ((desc (lispm-desktop-deps--package main))
         (directory (and desc (package-desc-dir desc)))
         (release (and directory (lispm-desktop-deps--release directory))))
    (unless release
      (error "%s" (lispm-desktop-deps--behind main spec)))
    (message "%s" (lispm-desktop-deps--tr "Updating %s to its latest release, which this LispM Desktop needs…"
                                          (abbreviate-file-name directory)))
    (lispm-desktop-store-installer-call
     `(update ,(package-desc-name desc) ,release ,(lispm-desktop-deps--git directory "rev-parse" "HEAD")))
    ;; Built again: what it autoloads now.
    (load (expand-file-name (format "%s-autoloads" (package-desc-name desc)) directory) t t)
    (when-let* ((behind (lispm-desktop-deps--behind main spec)))
      (error "%s" behind))))

(defun lispm-desktop-deps-ensure (feature)
  "Make FEATURE, a project of `lispm-desktop-deps', loadable.
A copy already on `load-path' is used as it is; else a checkout beside the
desktop's joins it, see `lispm-desktop-deps--checkout'; else the latest
release of the project is installed from its repository with package-vc,
its newest tag vX.Y.Z, or the commit that last changed its Version header
before it has one.  The packages its main file requires, and lacks, are
installed.  A copy lacking the library the desktop needs of it, its
:needs, or older than its :version, is brought up to date, see
`lispm-desktop-deps--catch-up'."
  (pcase-let* ((spec (alist-get feature lispm-desktop-deps))
               (`(,url ,checkout ,lisp) (list (plist-get spec :url) (plist-get spec :checkout)
                                              (plist-get spec :lisp-dir)))
               (file (concat (symbol-name feature) ".el")))
    (unless (locate-library file t)
      (if-let* ((directory (lispm-desktop-deps--checkout checkout)))
          (add-to-list 'load-path directory)
        (package-vc-install `(,feature :url ,url ,@(and lisp (list :lisp-dir lisp)))
                            (or (lispm-desktop-deps--release-tag url) :last-release))))
    (when-let* ((main (locate-library file t)))
      (when (lispm-desktop-deps--behind main spec)
        (lispm-desktop-deps--catch-up main spec))
      (when-let* ((missing (lispm-desktop-deps--requirements main)))
        (package-refresh-contents)
        (dolist (dep missing)
          (package-install (car dep)))))))

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
