;;; lispm-desktop-store-installer.el --- What the App Store does to a package  -*- lexical-binding: t; -*-

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

;; What the App Store does to an item's package, see
;; lispm-desktop-store.el: install it, from git pinned to its commit or
;; from the package archives, and check it out at another commit for an
;; update.
;;
;; The App Store does it in an Emacs of its own, in the background, so
;; that the desktop goes on while a package is cloned and compiled, and
;; its window shows how far it is.  That Emacs runs in batch and loads
;; this file alone, which needs nothing of the desktop's:
;;
;;   emacs -Q --batch -l JOB
;;
;; JOB sets what the App Store's Emacs installs packages with, as
;; `package-user-dir' and `package-archives', then calls
;; `lispm-desktop-store-installer-run' to install a package, to check
;; one from git out at another commit, or to upgrade one from the
;; package archives.  It says how far it is on its
;; standard output, a line each beginning with
;; `lispm-desktop-store-installer-tag':
;;
;;   (stage STAGE)       preparing, archives, download, dependencies, build
;;   (progress FRACTION) how much of the stage is done, from 0 to 1
;;   (done INSTALLED)    what is installed: a commit, a version
;;   (failed MESSAGE)    why it was not
;;
;; Everything else it says, on its standard error, is the job's log.
;;
;; A package from git may come with files its releases build, as a secret
;; kept out of its source: its :assets, downloaded beside it once it is
;; installed or checked out, each kept only when its SHA-256 is the one
;; the catalog pins, see `lispm-desktop-store--fetch-assets'.
;;
;; A package may require one of LispM's libraries that no package archive
;; holds, as lispm-gl requires lispm-prebuilt: it is installed first, from
;; git at its latest release, see `lispm-desktop-store-installer-libraries'.
;;
;; `lispm-desktop-store-installer-program' makes JOB's program in the
;; Emacs asking, and `lispm-desktop-store-installer-call' runs it there
;; and waits, as LispM Desktop does to keep a LispM project installed as
;; a package at the release it needs (lispm-desktop-deps.el,
;; lispm-desktop-update.el).

;;; Code:

(require 'cl-lib)
(require 'package)
(require 'seq)
(require 'subr-x)

(declare-function package-vc-install "package-vc" (package &optional rev backend name))
(declare-function package-vc-rebuild "package-vc" (pkg-desc))
(declare-function url-retrieve-synchronously "url" (url &optional silent inhibit-cookies timeout))
(defvar url-http-response-status)
(defvar lispm-desktop-store-installer--reporting)

;;;; Git

(defun lispm-desktop-store--git-environment ()
  "Return the environment git runs in: no repository of Emacs's own, as an
inherited GIT_DIR would name, and no prompt for a password."
  (append '("GIT_DIR" "GIT_WORK_TREE" "GIT_INDEX_FILE" "GIT_TERMINAL_PROMPT=0")
          process-environment))

(defun lispm-desktop-store--release-tag (url)
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

(defun lispm-desktop-store--git (directory &rest args)
  "Run git with ARGS in DIRECTORY; return what it printed, or signal it."
  (let ((default-directory (file-name-as-directory directory))
        (process-environment (lispm-desktop-store--git-environment)))
    (with-temp-buffer
      (let ((status (apply #'process-file "git" nil t nil args)))
        (unless (eql status 0)
          (error "git %s failed: %s" (car args) (string-trim (buffer-string))))
        (string-trim (buffer-string))))))

;;;; Packages

(defun lispm-desktop-store--plist-without (plist &rest keys)
  "Return PLIST without KEYS."
  (let ((result nil))
    (while plist
      (unless (memq (car plist) keys)
        (setq result (nconc result (list (car plist) (cadr plist)))))
      (setq plist (cddr plist)))
    result))

(defun lispm-desktop-store--package-name (package)
  "Return the name of PACKAGE, an item's :package."
  (if (consp package) (car package) package))

(defun lispm-desktop-store--package-version (name)
  "Return the version of the package NAME installed, or nil."
  (when-let* ((desc (cadr (assq name package-alist))))
    (package-version-join (package-desc-version desc))))

(defun lispm-desktop-store--install-package (package)
  "Install PACKAGE, an item's :package; return what is installed: the
commit of a package from git, the version of one from the archives, or
\"built-in\".  A package installed already is left as it is, but for
the :assets of one from git, downloaded again."
  (pcase package
    ('nil "built-in")
    ((pred symbolp)
     (unless (package-installed-p package)
       (unless (assq package package-archive-contents)
         (package-refresh-contents))
       (package-install package))
     (or (lispm-desktop-store--package-version package) "installed"))
    (`(,name . ,spec)
     (let ((rev (plist-get spec :rev)))
       (unless (package-installed-p name)
         (let ((directory (expand-file-name (symbol-name name) package-user-dir)))
           (when (file-exists-p directory)
             (user-error "%s is in the way of %s; move it away first"
                         (abbreviate-file-name directory) name)))
         (require 'package-vc)
         (package-vc-install (cons name (lispm-desktop-store--plist-without spec :rev :assets)) rev))
       (lispm-desktop-store--fetch-assets name (plist-get spec :assets))
       rev))))

(defun lispm-desktop-store--download (url)
  "Return what is at URL, unibyte, or signal why it was not got."
  (require 'url)
  (let ((buffer (or (url-retrieve-synchronously url t t 120)
                    (error "%s did not answer" url))))
    (unwind-protect
        (with-current-buffer buffer
          (let ((status (bound-and-true-p url-http-response-status)))
            (unless (eql status 200)
              (error "%s answered %s" url status)))
          (set-buffer-multibyte nil)
          ;; The headers end at the first empty line.
          (goto-char (point-min))
          (re-search-forward "^\r?\n")
          (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer buffer))))

(defun lispm-desktop-store--fetch-assets (name assets)
  "Put ASSETS, the :assets of a package from git, beside the package NAME:
each (FILE :url URL :sha256 SHA) is downloaded from URL into the
package's directory as FILE, and kept only when its SHA-256 is SHA."
  (when assets
    (let ((directory (package-desc-dir (or (cadr (assq name package-alist))
                                           (error "%s is not installed" name)))))
      (when lispm-desktop-store-installer--reporting
        (lispm-desktop-store-installer--stage 'download))
      (pcase-dolist (`(,file . ,props) assets)
        (unless (and (stringp file) (equal file (file-name-nondirectory file))
                     (not (member file '("" "." ".."))))
          (error "%S is not a file's name" file))
        (let* ((url (plist-get props :url))
               (data (lispm-desktop-store--download url))
               (sha256 (secure-hash 'sha256 data)))
          (unless (equal sha256 (plist-get props :sha256))
            (error "%s is not the file the catalog pins: its SHA-256 is %s" url sha256))
          (let ((temporary (make-temp-file (expand-file-name (concat "." file) directory)))
                (coding-system-for-write 'no-conversion))
            (write-region data nil temporary nil 'silent)
            (rename-file temporary (expand-file-name file directory) t)))))))

(defun lispm-desktop-store--repin (name rev &optional assets)
  "Check the package NAME, installed from git, out at commit REV, and build
it again, as `package-vc-upgrade' would build it; then download ASSETS,
its :assets, beside it."
  (let* ((desc (or (cadr (assq name package-alist)) (user-error "%s is not installed" name)))
         (directory (package-desc-dir desc))
         (present (lambda ()
                    (ignore-errors
                      (lispm-desktop-store--git directory "cat-file" "-e" (concat rev "^{commit}"))
                      t))))
    (unless (funcall present)
      (lispm-desktop-store-installer--fetch directory "origin")
      (unless (funcall present)
        (lispm-desktop-store-installer--fetch directory "origin" rev)))
    (lispm-desktop-store--git directory "checkout" "--quiet" "--detach" rev)
    (require 'package-vc)
    (package-vc-rebuild desc)
    (lispm-desktop-store--fetch-assets name assets)))

;;;; In an Emacs of its own

(defconst lispm-desktop-store-installer-tag "LISPM-STORE "
  "What begins a line in which the installing Emacs says how far it is.")

(defconst lispm-desktop-store-installer-handed-variables
  '(user-emacs-directory exec-path
    package-user-dir package-directory-list package-archives package-archive-priorities
    package-pinned-packages package-load-list package-check-signature package-unsigned-archives
    package-gnupghome-dir package-selected-packages package-install-upgrade-built-in
    package-vc-selected-packages package-vc-default-backend package-quickstart package-quickstart-file
    url-proxy-services url-using-proxy gnutls-trustfiles network-security-level)
  "What an Emacs installing in the background takes from this one: how
packages are installed, and how the network is reached.")

(defconst lispm-desktop-store-installer--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory this file is in, which an Emacs of its own loads it from.")

(defun lispm-desktop-store-installer--readable-p (value)
  "Return non-nil when VALUE, printed, reads back as itself."
  (condition-case nil
      (let ((print-length nil) (print-level nil))
        (equal (car (read-from-string (prin1-to-string value))) value))
    (error nil)))

(defun lispm-desktop-store-installer-program (job)
  "Return the program an Emacs of its own runs for JOB, as
`lispm-desktop-store-installer-run' takes it: this Emacs's settings for
packages and the network, then JOB."
  `(progn
     ,@(mapcan (lambda (variable)
                 (when (and (boundp variable)
                            (lispm-desktop-store-installer--readable-p (symbol-value variable)))
                   (list `(setq ,variable ',(symbol-value variable)))))
               lispm-desktop-store-installer-handed-variables)
     (push ,lispm-desktop-store-installer--directory load-path)
     ;; The editor's agents, which an extension adding one requires.
     ,@(when-let* ((agents (locate-library "lispm-agents")))
         `((push ,(file-name-directory agents) load-path)))
     (require 'lispm-desktop-store-installer)
     (lispm-desktop-store-installer-run ',job)))

(defun lispm-desktop-store-installer-call (job)
  "Do JOB in an Emacs of its own, as `lispm-desktop-store-installer-run'
takes it, and wait until it is done; return what it installed.  Signal an
error saying why, when it was not."
  (let ((file (make-temp-file "lispm-desktop-store-job" nil ".el"))
        (process-environment (lispm-desktop-store--git-environment)))
    (unwind-protect
        (with-temp-buffer
          (with-temp-file file
            (insert ";;; -*- lexical-binding: t -*-\n")
            (let ((print-length nil) (print-level nil) (print-escape-newlines t))
              (prin1 (lispm-desktop-store-installer-program job) (current-buffer))))
          ;; Nothing it asks is answered: it fails instead of waiting.
          (call-process (expand-file-name invocation-name invocation-directory) nil '(t nil) nil
                        "-Q" "--batch" "-l" file)
          (goto-char (point-min))
          (let ((ended nil))
            (while (search-forward lispm-desktop-store-installer-tag nil t)
              (pcase (ignore-errors (read (current-buffer)))
                ((and `(,(or 'done 'failed) ,_) event) (setq ended event))))
            (pcase ended
              (`(done ,installed) installed)
              (`(failed ,message) (error "%s" message))
              (_ (error "The Emacs installing stopped without saying why")))))
      (delete-file file))))

(defvar lispm-desktop-store-installer--reporting nil
  "Non-nil in the Emacs installing for the App Store, which says how far
it is on its standard output.")

(defvar package-vc-register-as-project)

(defvar lispm-desktop-store-installer--stage nil
  "The stage the installing Emacs said it is at last.")

(defvar lispm-desktop-store-installer--compiling nil
  "While a package compiles: (DONE FILES SAID), its files compiled so far,
all of them, and how far that was said to be last.")

(defun lispm-desktop-store-installer--say (&rest event)
  "Say EVENT, a list, to the App Store, when installing for it."
  (when lispm-desktop-store-installer--reporting
    (let ((print-length nil)
          (print-level nil)
          (print-escape-newlines t))
      ;; Unlike `princ', which batch Emacs buffers until it exits, this
      ;; reaches the App Store at once.
      (send-string-to-terminal (concat "\n" lispm-desktop-store-installer-tag (prin1-to-string event) "\n")))))

(defun lispm-desktop-store-installer--stage (stage)
  "Say that the job is at STAGE now."
  (unless (eq stage lispm-desktop-store-installer--stage)
    (setq lispm-desktop-store-installer--stage stage)
    (lispm-desktop-store-installer--say 'stage stage)))

(defun lispm-desktop-store-installer--git-progress (text)
  "Return how far git's progress TEXT says a download is, from 0 to 1, or
nil: receiving the objects is most of it, resolving the deltas the rest."
  (let ((fraction nil)
        (start 0))
    (while (string-match "\\(Receiving objects\\|Resolving deltas\\): *\\([0-9]+\\)%" text start)
      (let ((percent (/ (string-to-number (match-string 2 text)) 100.0)))
        (setq fraction (if (equal (match-string 1 text) "Receiving objects")
                           (* 0.9 percent)
                         (+ 0.9 (* 0.1 percent)))
              start (match-end 0))))
    fraction))

(defun lispm-desktop-store-installer--git-with-progress (directory &rest args)
  "Run git with ARGS in DIRECTORY as `lispm-desktop-store--git' does, saying
how far the download it makes is, from what git prints with --progress."
  (let* ((default-directory (file-name-as-directory directory))
         (process-environment (lispm-desktop-store--git-environment))
         (buffer (generate-new-buffer " *lispm-desktop-store git*"))
         (said 0)
         (process (make-process
                   :name "git" :buffer buffer :command (cons "git" args)
                   :connection-type 'pipe :noquery t
                   :filter (lambda (process text)
                             (with-current-buffer (process-buffer process)
                               (goto-char (point-max))
                               (insert text))
                             (when-let* ((fraction (lispm-desktop-store-installer--git-progress text))
                                         ((>= (- fraction said) 0.01)))
                               (setq said fraction)
                               (lispm-desktop-store-installer--say 'progress fraction))))))
    (while (process-live-p process)
      (accept-process-output process 0.1))
    (unwind-protect
        (unless (eql (process-exit-status process) 0)
          (error "git %s failed: %s" (car args)
                 (car (last (split-string (with-current-buffer buffer (buffer-string)) "[\r\n]+" t)))))
      (kill-buffer buffer))))

(defun lispm-desktop-store-installer--fetch (directory &rest args)
  "Fetch with ARGS in the clone DIRECTORY, saying how far, when installing."
  (if lispm-desktop-store-installer--reporting
      (progn (lispm-desktop-store-installer--stage 'download)
             (apply #'lispm-desktop-store-installer--git-with-progress
                    directory "fetch" "--progress" args))
    (apply #'lispm-desktop-store--git directory "fetch" "--quiet" args)))

(defun lispm-desktop-store-installer--clone (remote directory rev)
  "Clone REMOTE into DIRECTORY at REV, as `vc-git-clone' does, saying how
far the download is; return DIRECTORY.  REV, a commit, is checked out
after the clone; any other is tried as the branch to clone first."
  (lispm-desktop-store-installer--stage 'download)
  (let ((parent (file-name-directory (directory-file-name directory))))
    (cl-flet ((clone (&rest args)
                (apply #'lispm-desktop-store-installer--git-with-progress
                       parent "clone" "--progress" (append args (list remote directory)))))
      (cond ((null rev) (clone))
            ((and (not (string-match-p "\\`[0-9a-f]\\{7,40\\}\\'" rev))
                  (ignore-errors (clone "--branch" rev) t)))
            (t (clone)
               (lispm-desktop-store--git directory "checkout" "--quiet" rev)))))
  directory)

(defun lispm-desktop-store-installer--compile (compile desc)
  "Call COMPILE on DESC, a package, saying how many of its files are done:
all its Lisp files are counted, as `byte-recompile-directory' goes through
each, those it leaves uncompiled too."
  (lispm-desktop-store-installer--stage 'build)
  (let ((lispm-desktop-store-installer--compiling
         (list 0 (max 1 (length (directory-files-recursively
                                 (package-desc-dir desc) "\\`[^.].*\\.el\\'" nil
                                 (lambda (directory)
                                   (not (string-prefix-p "." (file-name-nondirectory directory)))))))
               0)))
    (funcall compile desc)))

(defun lispm-desktop-store-installer--compiled (&rest _)
  "Count a file of the package compiled, and say how far that is."
  (when-let* ((counts lispm-desktop-store-installer--compiling))
    (setcar counts (1+ (car counts)))
    (let ((fraction (min 1.0 (/ (car counts) (float (nth 1 counts))))))
      (when (>= (- fraction (nth 2 counts)) 0.01)
        (setcar (nthcdr 2 counts) fraction)
        (lispm-desktop-store-installer--say 'progress fraction)))))

(defun lispm-desktop-store-installer--refuse (prompt &rest _)
  "Refuse to answer PROMPT: nobody is there to; the job fails, saying it."
  (user-error "%s" (string-trim-right (substring-no-properties prompt) "[ ?:]+")))

(defconst lispm-desktop-store-installer-libraries
  '((lispm-prebuilt :url "https://github.com/LispMachineLab/lispm-prebuilt"))
  "LispM's libraries a package may require that no package archive holds,
\(NAME :url URL) each.  A package requiring one, in its Package-Requires,
has it installed first, from git at its latest release.")

(defun lispm-desktop-store-installer--libraries (install deps)
  "Install DEPS with INSTALL, `package-vc-install-dependencies', then those
of them it could not that are LispM's libraries, from git at their latest
release; return those still missing.
This is around `package-vc-install-dependencies' in the App Store's Emacs."
  (seq-remove
   (pcase-lambda (`(,name ,version))
     (when-let* ((spec (alist-get name lispm-desktop-store-installer-libraries)))
       (package-vc-install (cons name spec)
                           (or (lispm-desktop-store--release-tag (plist-get spec :url)) :last-release))
       (package-installed-p name version)))
   (funcall install deps)))

(defun lispm-desktop-store-installer--prepare ()
  "Make this Emacs install for the App Store: say how far it is, ask
nothing, and keep no customization, which the App Store's Emacs keeps."
  (setq lispm-desktop-store-installer--reporting t)
  (when (boundp 'native-comp-jit-compilation)
    (setq native-comp-jit-compilation nil))
  ;; Advice on primitives, below, compiles trampolines at once; this Emacs
  ;; runs with -Q, so LispM.app's mending of native compilation is not done.
  (when (require 'lispm-desktop-native-comp nil t)
    (lispm-desktop-native-comp-mend))
  ;; The App Store's Emacs remembers the clone as a project, if it does:
  ;; this one's list of projects would be written over by that one's.
  (setq package-vc-register-as-project nil)
  (advice-add 'customize-save-variable :override
              (lambda (variable value &rest _) (set-default variable value)))
  (dolist (asking '(yes-or-no-p y-or-n-p read-string read-from-minibuffer completing-read))
    (advice-add asking :override #'lispm-desktop-store-installer--refuse))
  (advice-add 'vc-git-clone :override #'lispm-desktop-store-installer--clone)
  (dolist (refresh '(package-refresh-contents package-vc--download-and-read-archives))
    (advice-add refresh :before (lambda (&rest _) (lispm-desktop-store-installer--stage 'archives))))
  (advice-add 'package-install-from-archive :before
              (lambda (&rest _)
                (unless (eq lispm-desktop-store-installer--stage 'dependencies)
                  (lispm-desktop-store-installer--stage 'download))))
  (advice-add 'package-vc-install-dependencies :before
              (lambda (&rest _) (lispm-desktop-store-installer--stage 'dependencies)))
  (advice-add 'package-vc-install-dependencies :around #'lispm-desktop-store-installer--libraries)
  (advice-add 'package--compile :around #'lispm-desktop-store-installer--compile)
  (advice-add 'byte-compile-file :after #'lispm-desktop-store-installer--compiled))

(defun lispm-desktop-store-installer--upgrade (name)
  "Install the package archives' newest NAME beside the one installed, then
delete that one; return the new version.  One that fails, or is stopped,
leaves the one installed as it was."
  (let ((old (cadr (assq name package-alist)))
        (new (or (cadr (assq name package-archive-contents))
                 (error "The package archives hold no %s" name))))
    (package-install new 'dont-select)
    (when (and old (not (equal (package-desc-version old) (package-desc-version new))))
      (package-delete old 'force 'dont-unselect))
    (package-version-join (package-desc-version new))))

(defun lispm-desktop-store-installer--update (name rev former &optional assets)
  "Check the package NAME out at commit REV, build it again and download
ASSETS, its :assets, beside it; return REV.  When that fails, it is
checked out at FORMER again."
  (condition-case err
      (progn (lispm-desktop-store--repin name rev assets)
             rev)
    (error
     (when-let* ((desc (cadr (assq name package-alist))))
       (ignore-errors
         (lispm-desktop-store--git (package-desc-dir desc) "checkout" "--quiet" "--detach" former)))
     (signal (car err) (cdr err)))))

(defun lispm-desktop-store-installer-run (job)
  "Do JOB for the App Store, in this Emacs run in batch, then exit.
JOB is (install PACKAGE), PACKAGE an item's :package; (update NAME REV
FORMER ASSETS), the package NAME from git to go from commit FORMER to
REV, with ASSETS, its :assets, beside it; or
\(upgrade NAME), the package NAME to the package archives' newest.
How far it is is said on the standard output; Emacs exits with 0 once
done, else with 1."
  (lispm-desktop-store-installer--prepare)
  (lispm-desktop-store-installer--stage 'preparing)
  ;; Loaded first, package-vc reads the archives' specifications from
  ;; where they are kept as package.el starts, rather than download them.
  (require 'package-vc)
  (condition-case err
      (let ((installed (pcase job
                         (`(install ,package)
                          (package-initialize t)
                          (lispm-desktop-store--install-package package))
                         (`(update ,name ,rev ,former . ,assets)
                          (package-initialize t)
                          (lispm-desktop-store-installer--update name rev former (car assets)))
                         (`(upgrade ,name)
                          (package-initialize t)
                          (lispm-desktop-store-installer--upgrade name))
                         (_ (error "No such job: %S" job)))))
        (lispm-desktop-store-installer--say 'done installed)
        (kill-emacs 0))
    (error
     (lispm-desktop-store-installer--say 'failed (error-message-string err))
     (kill-emacs 1))))

(provide 'lispm-desktop-store-installer)
;;; lispm-desktop-store-installer.el ends here
