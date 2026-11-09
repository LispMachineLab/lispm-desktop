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

(require 'cl-lib)
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
              ;; Its palette, drawing, icons, alerts, menus and fields.
              :version "0.7.2")
    (lispm :url "https://github.com/LispMachineLab/lispm-agent" :checkout "lispm-agent"
           :lisp-dir "lisp"
           ;; Agent owns project views and their optional renderer dispatch.
           :needs "lispm-agents-work" :version "0.17.3")
    (lispm-browser :url "https://github.com/LispMachineLab/lispm-browser" :checkout "lispm-browser"
                   :lisp-dir "lisp")
    (lispm-kernel :url "https://github.com/LispMachineLab/lispm-kernel" :checkout "lispm-kernel"
                  :lisp-dir "lisp")
    (lispm-auth :url "https://github.com/LispMachineLab/lispm-auth" :checkout "lispm-auth"))
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

(defun lispm-desktop-deps--shallow-exclude (directory)
  "Return what git fetch takes to keep the git checkout DIRECTORY shallow,
when it was cloned so, see `lispm-desktop-store--shallow-exclude'."
  (when-let* (((equal (lispm-desktop-deps--git directory "rev-parse" "--is-shallow-repository") "true"))
              (own (apply #'lispm-desktop-deps--git directory lispm-desktop-store--own-release))
              (listed (lispm-desktop-deps--git directory "ls-remote" "--tags" "--refs" "origin")))
    (lispm-desktop-store--shallow-exclude own listed)))

(defun lispm-desktop-deps--release (directory)
  "Return the commit of the latest release of the git checkout DIRECTORY,
its origin's main branch fetched first, with the tags of its commits: of
the highest vX.Y.Z tag on that branch.  Not every tag is fetched, the
history of each, and a checkout cloned shallow stays so, see
`lispm-desktop-deps--shallow-exclude'.  Return nil when there is none,
or the fetch failed."
  (when-let* (((apply #'lispm-desktop-deps--git directory "fetch" "--quiet"
                      (append (lispm-desktop-deps--shallow-exclude directory) '("origin"))))
              (tags (lispm-desktop-deps--git directory "tag" "--list" "v*" "--sort=-v:refname"
                                             "--merged" "origin/HEAD"))
              (tag (seq-find (lambda (tag) (string-match-p "\\`v[0-9]+\\.[0-9]+\\.[0-9]+\\'" tag))
                             (split-string tags "\n" t))))
    (lispm-desktop-deps--git directory "rev-parse" (concat tag "^{commit}"))))

(defvar lispm-desktop-deps--prefetched-tags nil
  "Alist of (URL . TAG) resolved concurrently for project repositories.")

(defvar lispm-desktop-deps--fast-install-active nil
  "Non-nil when parallel prefetch and deferred compile are active.")

(defvar lispm-desktop-deps--prefetch-dir nil
  "Directory where prefetched ELPA archive files are cached.")

(defvar lispm-desktop-deps--deferred-compile-queue nil
  "List of `package-desc' objects queued for parallel batch compile.")

(defvar lispm-desktop-deps-compile-jobs
  (min 8 (max 2 (or (and (fboundp 'num-processors) (ignore-errors (num-processors))) 4)))
  "Max concurrent batch Emacs processes used to compile packages.")

(defun lispm-desktop-deps--parse-latest-release-tag (output)
  "Extract the highest vX.Y.Z tag from `git ls-remote' OUTPUT, or nil."
  (when (and (stringp output) (not (string-empty-p output)))
    (car (sort (delq nil
                     (mapcar (lambda (line)
                               (and (string-match "refs/tags/\\(v[0-9]+\\.[0-9]+\\.[0-9]+\\)\\'" line)
                                    (match-string 1 line)))
                             (split-string output "\n" t)))
               (lambda (a b) (version< (substring b 1) (substring a 1)))))))

(defun lispm-desktop-deps--release-tag (url)
  "Return the tag of the latest release of the repository at URL, as
\"v0.2.0\", or nil when it has none or cannot be reached."
  (or (cdr (assoc url lispm-desktop-deps--prefetched-tags))
      (lispm-desktop-store--release-tag url)))

(defun lispm-desktop-deps-prefetch-projects (&optional features)
  "Concurrently resolve tags and shallow-clone missing projects in FEATURES.
When FEATURES is nil, defaults to the core startup projects
`(lispm-kit lispm-ui lispm-browser lispm)'."
  (unless package-alist
    (ignore-errors (package-load-all-descriptors)))
  (let* ((targets (or features '(lispm-kit lispm-ui lispm-browser lispm)))
         (missing (seq-filter (lambda (feature)
                                (and (assq feature lispm-desktop-deps)
                                     (not (lispm-desktop-deps-present-p feature))
                                     (not (assq feature package-alist))))
                              targets)))
    (when (>= (length missing) 2)
      ;; Phase 1: Resolve release tags concurrently via `git ls-remote'.
      (let ((tag-procs nil)
            (process-environment (lispm-desktop-store--git-environment)))
        (dolist (feature missing)
          (let* ((spec (alist-get feature lispm-desktop-deps))
                 (url (plist-get spec :url))
                 (pinned (plist-get spec :tag)))
            (cond
             (pinned
              (setf (alist-get url lispm-desktop-deps--prefetched-tags nil nil #'equal) pinned))
             ((and url (not (assoc url lispm-desktop-deps--prefetched-tags)))
              (let ((buf (generate-new-buffer " *lispm-deps-tag*")))
                (condition-case nil
                    (let ((proc (make-process
                                 :name (format "lispm-deps-tag-%s" feature)
                                 :buffer buf
                                 :command (list "git" "ls-remote" "--tags" "--refs" url "v*")
                                 :connection-type 'pipe
                                 :noquery t)))
                      (push (list proc buf url) tag-procs))
                  (error (kill-buffer buf))))))))
        (unwind-protect
            (while (seq-some (lambda (item) (process-live-p (car item))) tag-procs)
              (accept-process-output nil 0.05))
          (dolist (item tag-procs)
            (pcase-let ((`(,proc ,buf ,url) item))
              (when (process-live-p proc) (delete-process proc))
              (when (buffer-live-p buf)
                (when (and (eq (process-status proc) 'exit)
                           (zerop (process-exit-status proc)))
                  (when-let* ((tag (with-current-buffer buf
                                     (lispm-desktop-deps--parse-latest-release-tag (buffer-string)))))
                    (setf (alist-get url lispm-desktop-deps--prefetched-tags nil nil #'equal) tag)))
                (kill-buffer buf)))))
        ;; Phase 2: Concurrently shallow-clone all missing projects with resolved tags,
        ;; and prefetch the LispM Auth release archive in the same parallel wave.
        (let ((stage-root (expand-file-name ".prefetch-clones" user-emacs-directory))
              (cache-dir (or lispm-desktop-deps--prefetch-dir
                             (setq lispm-desktop-deps--prefetch-dir
                                   (expand-file-name ".cache/elpa-prefetch" user-emacs-directory))))
              (clone-procs nil)
              (auth-proc nil)
              (auth-part nil)
              (auth-dest nil)
              (pcts (make-hash-table :test 'eq)))
          (make-directory stage-root t)
          (make-directory cache-dir t)
          (when (and (executable-find "curl")
                     (boundp 'lispm-desktop-deps-auth-version)
                     (not (locate-library "lispm-auth.el" t)))
            (setq auth-dest (expand-file-name
                             (format "lispm-auth-client-%s.tar.gz" lispm-desktop-deps-auth-version)
                             cache-dir)
                  auth-part (concat auth-dest ".part"))
            (unless (file-regular-p auth-dest)
              (condition-case nil
                  (setq auth-proc
                        (make-process
                         :name "lispm-deps-prefetch-auth"
                         :buffer nil
                         :command (list "curl" "-fsSL" "--retry" "2" "--connect-timeout" "10"
                                        "-o" auth-part
                                        (format "https://github.com/LispMachineLab/lispm-auth/releases/download/v%s/lispm-auth-client-%s.tar.gz"
                                                lispm-desktop-deps-auth-version
                                                lispm-desktop-deps-auth-version))
                         :connection-type 'pipe
                         :noquery t))
                (error (setq auth-proc nil)))))
          (dolist (feature missing)
            (let* ((spec (alist-get feature lispm-desktop-deps))
                   (url (plist-get spec :url))
                   (tag (or (plist-get spec :tag)
                            (cdr (assoc url lispm-desktop-deps--prefetched-tags))))
                   (key (and url tag (cons url tag))))
              (when (and key (not (assoc key lispm-desktop-store-installer--prefetched-clones)))
                (let* ((holder (make-temp-file (expand-file-name (format "%s-" feature) stage-root) t))
                       (target (expand-file-name "repo" holder)))
                  (puthash feature 0 pcts)
                  (condition-case nil
                      (let ((proc (make-process
                                   :name (format "lispm-deps-clone-%s" feature)
                                   :buffer nil
                                   :command (list "git" "clone" "--quiet" "--progress"
                                                  "--depth" "1" "--branch" tag "--" url target)
                                   :connection-type 'pipe
                                   :noquery t
                                   :filter (lambda (_proc chunk)
                                             (when (string-match "\\([0-9]+\\)%" chunk)
                                               (puthash feature (string-to-number (match-string 1 chunk)) pcts)
                                               (when (and (bound-and-true-p lispm-desktop-installer-screen--active)
                                                          (fboundp 'lispm-desktop-installer-screen-report))
                                                 (let ((sum 0) (cnt 0))
                                                   (maphash (lambda (_k v)
                                                              (setq sum (+ sum v)
                                                                    cnt (1+ cnt)))
                                                            pcts)
                                                   (when (> cnt 0)
                                                     (lispm-desktop-installer-screen-report
                                                      (format "Downloading (%d%%)…" (/ sum cnt)))))))))))
                        (push (list proc key target holder) clone-procs))
                    (error (ignore-errors (delete-directory holder t))))))))
          (unwind-protect
              (while (or (and auth-proc (process-live-p auth-proc))
                         (seq-some (lambda (item) (process-live-p (car item))) clone-procs))
                (accept-process-output nil 0.05))
            (when auth-proc
              (when (process-live-p auth-proc) (delete-process auth-proc))
              (if (and (eq (process-status auth-proc) 'exit)
                       (zerop (process-exit-status auth-proc))
                       (file-regular-p auth-part))
                  (ignore-errors (rename-file auth-part auth-dest t))
                (ignore-errors (delete-file auth-part))))
            (dolist (item clone-procs)
              (pcase-let ((`(,proc ,key ,target ,holder) item))
                (when (process-live-p proc) (delete-process proc))
                (if (and (eq (process-status proc) 'exit)
                         (zerop (process-exit-status proc))
                         (file-directory-p (expand-file-name ".git" target)))
                    (push (cons key target) lispm-desktop-store-installer--prefetched-clones)
                  (ignore-errors (delete-directory holder t)))))))))))

(defun lispm-desktop-deps--archive-cache-dir (url)
  "Return the local prefetch directory for the archive at URL, or nil."
  (when (and lispm-desktop-deps--prefetch-dir (stringp url))
    (when-let* ((entry (seq-find (lambda (a) (equal (cdr a) url)) package-archives)))
      (let ((dir (expand-file-name (concat "archives/" (car entry))
                                   lispm-desktop-deps--prefetch-dir)))
        (and (file-directory-p dir) (file-name-as-directory dir))))))

(defun lispm-desktop-deps-prefetch-archives ()
  "Download `archive-contents' for `package-archives' concurrently via curl."
  (when (and lispm-desktop-deps--fast-install-active
             (executable-find "curl"))
    (let* ((cache-root (or lispm-desktop-deps--prefetch-dir
                           (setq lispm-desktop-deps--prefetch-dir
                                 (expand-file-name ".cache/elpa-prefetch" user-emacs-directory))))
           (procs nil))
      (dolist (archive package-archives)
        (let* ((name (car archive))
               (url (cdr archive))
               (dir (expand-file-name (concat "archives/" name) cache-root)))
          (when (and (stringp url) (string-match-p "\\`https?://" url))
            (make-directory dir t)
            (dolist (file (cons "archive-contents"
                                (when (and package-check-signature
                                           (not (member name package-unsigned-archives))
                                           (not (string-match-p "melpa" url)))
                                  '("archive-contents.sig"))))
              (let* ((dest (expand-file-name file dir))
                     (part (concat dest ".part")))
                (unless (file-regular-p dest)
                  (condition-case nil
                      (let ((proc (make-process
                                   :name (format "lispm-archive-%s-%s" name file)
                                   :buffer nil
                                   :command (list "curl" "-fsSL" "--compressed"
                                                  "--retry" "2" "--connect-timeout" "10"
                                                  "-o" part (concat url file))
                                   :connection-type 'pipe
                                   :noquery t)))
                        (push (list proc part dest) procs))
                    (error (ignore-errors (delete-file part))))))))))
      (when procs
        (unwind-protect
            (while (seq-some (lambda (item) (process-live-p (car item))) procs)
              (accept-process-output nil 0.03))
          (dolist (item procs)
            (pcase-let ((`(,proc ,part ,dest) item))
              (when (process-live-p proc) (delete-process proc))
              (if (and (eq (process-status proc) 'exit)
                       (zerop (process-exit-status proc))
                       (file-regular-p part))
                  (ignore-errors (rename-file part dest t))
                (ignore-errors (delete-file part))))))))))

(defun lispm-desktop-deps--cached-response-buffer (orig url fn &rest args)
  "Serve prefetched ELPA archive files and skip redundant network probes."
  (let ((file (plist-get args :file))
        (noerror (plist-get args :noerror)))
    (cond
     ((not lispm-desktop-deps--fast-install-active)
      (apply orig url fn args))
     ;; Skip optional `<pkg>-readme.txt' HTTP probes during startup batch/GUI installation.
     ((and (stringp file)
           noerror
           (string-suffix-p "-readme.txt" file))
      (when-let* ((err-fn (plist-get args :error-function)))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (funcall err-fn)))
      nil)
     ;; MELPA does not publish `.sig' files; skip guaranteed-404 `.sig' probes on MELPA
     ;; when signature checking allows unsigned packages.
     ((and (stringp file)
           noerror
           (string-suffix-p ".sig" file)
           (eq (package-check-signature) 'allow-unsigned)
           (stringp url)
           (string-match-p "melpa" url))
      (when-let* ((err-fn (plist-get args :error-function)))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (funcall err-fn)))
      nil)
     ;; Serve prefetched `archive-contents' (downloaded in parallel with `--compressed').
     ((and (stringp file)
           (member file '("archive-contents" "archive-contents.sig"))
           (when-let* ((arch-dir (lispm-desktop-deps--archive-cache-dir url)))
             (and (file-regular-p (expand-file-name file arch-dir))
                  (progn
                    (apply orig arch-dir fn args)
                    t)))))
     ;; Serve prefetched package tarballs and `.el' files directly from local cache.
     ((and (stringp file)
           lispm-desktop-deps--prefetch-dir
           (file-regular-p (expand-file-name file lispm-desktop-deps--prefetch-dir)))
      (let ((local-dir (file-name-as-directory (expand-file-name lispm-desktop-deps--prefetch-dir))))
        (apply orig local-dir fn args)))
     (t
      (apply orig url fn args)))))

(defun lispm-desktop-deps-prefetch-packages (requirements)
  "Compute the transaction for REQUIREMENTS and download files in parallel."
  (when (and requirements (executable-find "curl"))
    (let* ((transaction (ignore-errors (package-compute-transaction nil requirements)))
           (cache-dir (or lispm-desktop-deps--prefetch-dir
                          (setq lispm-desktop-deps--prefetch-dir
                                (expand-file-name ".cache/elpa-prefetch" user-emacs-directory))))
           (queue nil))
      (when transaction
        (make-directory cache-dir t)
        (dolist (desc transaction)
          (let* ((loc (ignore-errors (package-desc-archive-location desc)))
                 (file (ignore-errors (concat (package-desc-full-name desc) (package-desc-suffix desc))))
                 (dest (and file (expand-file-name file cache-dir))))
            (when (and (stringp loc)
                       (string-match-p "\\`https?://" loc)
                       file
                       (not (file-regular-p dest))
                       (not (assoc file queue)))
              (push (list file (concat loc file) dest) queue))))
        (setq queue (nreverse queue))
        (when (>= (length queue) 2)
          (let ((total (length queue))
                (done 0)
                (active nil)
                (max-jobs 8))
            (unwind-protect
                (while (or queue active)
                  (while (and queue (< (length active) max-jobs))
                    (pcase-let* ((`(,file ,url ,dest) (pop queue))
                                 (part (concat dest ".part")))
                      (condition-case nil
                          (let ((proc (make-process
                                       :name (format "lispm-prefetch-%s" file)
                                       :buffer nil
                                       :command (list "curl" "-fsSL" "--retry" "2"
                                                      "--connect-timeout" "10"
                                                      "-o" part url)
                                       :connection-type 'pipe
                                       :noquery t)))
                            (push (list proc part dest) active))
                        (error
                         (setq done (1+ done))))))
                  (when active
                    (accept-process-output nil 0.03)
                    (let ((still-active nil))
                      (dolist (item active)
                        (pcase-let ((`(,proc ,part ,dest) item))
                          (if (process-live-p proc)
                              (push item still-active)
                            (if (and (eq (process-status proc) 'exit)
                                     (zerop (process-exit-status proc))
                                     (file-regular-p part))
                                (ignore-errors (rename-file part dest t))
                              (ignore-errors (delete-file part)))
                            (setq done (1+ done))
                            (when (and (bound-and-true-p lispm-desktop-installer-screen--active)
                                       (fboundp 'lispm-desktop-installer-screen-report))
                              (lispm-desktop-installer-screen-report
                               (format "Downloading (%d%%)…" (/ (* 100 done) (max 1 total))))))))
                      (setq active still-active))))
              (dolist (item active)
                (pcase-let ((`(,proc ,part ,_dest) item))
                  (when (process-live-p proc) (delete-process proc))
                  (ignore-errors (delete-file part)))))))))))

(defun lispm-desktop-deps--defer-compile (orig pkg-desc)
  "Defer `package--compile' for PKG-DESC when fast-install is active."
  (if lispm-desktop-deps--fast-install-active
      (let ((dir (package-desc-dir pkg-desc)))
        (when (and (stringp dir)
                   (not (seq-some (lambda (d) (equal (package-desc-dir d) dir))
                                  lispm-desktop-deps--deferred-compile-queue)))
          (push pkg-desc lispm-desktop-deps--deferred-compile-queue)))
    (funcall orig pkg-desc)))

(defun lispm-desktop-deps--collect-compile-files (descs)
  "Return all `.el' files in DESCS that are eligible for byte-compilation."
  (let ((result nil))
    (dolist (desc descs)
      (let* ((root (ignore-errors (package-desc-dir desc)))
             (ignores (ignore-errors (package--parse-elpaignore desc)))
             (ignore-re (if ignores
                            (mapconcat #'identity ignores "\\|")
                          regexp-unmatchable)))
        (when (and (stringp root) (file-directory-p root))
          (let ((dirs (list (expand-file-name root))))
            (while dirs
              (let ((dir (pop dirs)))
                (dolist (entry (ignore-errors (directory-files dir t)))
                  (let ((base (file-name-nondirectory entry)))
                    (cond
                     ((file-directory-p entry)
                      (when (and (not (member base '("." ".." "RCS" "CVS")))
                                 (not (string-prefix-p "." base))
                                 (not (file-symlink-p entry))
                                 (not (string-match-p ignore-re entry)))
                        (setq dirs (nconc dirs (list entry)))))
                     ((and (string-match-p "\\.el\\'" base)
                           (file-readable-p entry)
                           (not (string-prefix-p ".#" base))
                           (not (auto-save-file-name-p entry))
                           (not (string-match-p "\\(?:-autoloads\\|-pkg\\|\\.dir-locals.*\\)\\.el\\'" base))
                           (not (string-match-p ignore-re entry)))
                      (push entry result)))))))))))
    (delete-dups (nreverse result))))

(defun lispm-desktop-deps-compile-files-parallel (files &optional sync on-progress)
  "Byte-compile FILES in parallel across worker processes.
Each worker compiles individual `.el' files into atomic temporary `.elc' files
before renaming them into place, preventing race conditions when files require
siblings during compilation.  When SYNC or `noninteractive' is non-nil, wait
non-blockingly via `accept-process-output' while updating UI progress."
  (let ((unique-files (seq-filter #'file-readable-p (delete-dups (copy-sequence files)))))
    (when unique-files
      (let* ((emacs (expand-file-name invocation-name invocation-directory))
             (total (length unique-files))
             (done 0)
             (jobs (max 1 (min (or lispm-desktop-deps-compile-jobs 4) total)))
             (buckets (make-vector jobs nil))
             (lp (delete-dups
                  (append (mapcar #'file-name-directory unique-files)
                          (seq-filter #'stringp load-path))))
             (procs nil)
             (idx 0))
        (dolist (file unique-files)
          (aset buckets (mod idx jobs) (cons file (aref buckets (mod idx jobs))))
          (setq idx (1+ idx)))
        (dotimes (i jobs)
          (when-let* ((bucket (nreverse (aref buckets i))))
            (let ((expr (prin1-to-string
                         `(progn
                            (require 'bytecomp)
                            (when (boundp 'native-comp-enable-subr-trampolines)
                              (setq native-comp-enable-subr-trampolines nil))
                            (setq load-path ',lp
                                  load-prefer-newer t
                                  warning-minimum-level :error
                                  byte-compile-warnings nil
                                  inhibit-message t)
                            (dolist (f ',bucket)
                              (condition-case nil
                                  (let* ((dest (byte-compile-dest-file f))
                                         (tmp (concat dest "." (number-to-string (emacs-pid)) ".tmp"))
                                         (byte-compile-dest-file-function (lambda (_) tmp)))
                                    (unwind-protect
                                        (when (and (eq (byte-recompile-file f t 0) t)
                                                   (file-regular-p tmp))
                                          (rename-file tmp dest t))
                                      (when (file-exists-p tmp)
                                        (ignore-errors (delete-file tmp)))))
                                (error nil))
                              (princ "LISPM-COMPILED\n"))))))
              (condition-case nil
                  (let ((proc (make-process
                               :name (format "lispm-byte-compile-%d" (1+ i))
                               :buffer nil
                               :command (list emacs "-Q" "--batch" "--eval" expr)
                               :connection-type 'pipe
                               :noquery t
                               :filter (lambda (_proc chunk)
                                         (let ((pos 0)
                                               (hits 0))
                                           (while (string-match "LISPM-COMPILED" chunk pos)
                                             (setq hits (1+ hits)
                                                   pos (match-end 0)))
                                           (when (> hits 0)
                                             (setq done (min total (+ done hits)))
                                             (when on-progress
                                               (funcall on-progress done total))
                                             (when (and (bound-and-true-p lispm-desktop-installer-screen--active)
                                                        (fboundp 'lispm-desktop-installer-screen-report))
                                               (lispm-desktop-installer-screen-report
                                                (format "Compiling (%d%%)…" (/ (* 100 done) (max 1 total)))))
                                             (when (fboundp 'lispm-desktop-assistant-compile-progress)
                                               (lispm-desktop-assistant-compile-progress done total))))))))
                    (push proc procs))
                (error
                 (dolist (f bucket)
                   (ignore-errors (byte-recompile-file f t 0))
                   (setq done (min total (1+ done)))
                   (when on-progress
                     (funcall on-progress done total))))))))
        (when (or sync noninteractive)
          (unwind-protect
              (while (seq-some #'process-live-p procs)
                (accept-process-output nil 0.02))
            (dolist (proc procs)
              (when (process-live-p proc)
                (delete-process proc)))))
        procs))))

(defun lispm-desktop-deps-flush-deferred-compiles (&optional sync)
  "Compile queued packages in parallel across worker processes at the file level.
When SYNC or `noninteractive' is non-nil, wait for all workers to finish;
otherwise run them asynchronously in the background."
  (let* ((descs (nreverse lispm-desktop-deps--deferred-compile-queue))
         (files (lispm-desktop-deps--collect-compile-files descs)))
    (setq lispm-desktop-deps--deferred-compile-queue nil)
    (when files
      (prog1 (lispm-desktop-deps-compile-files-parallel files sync)
        (when (or sync noninteractive)
          (dolist (desc descs)
            (ignore-errors (package--reload-previously-loaded desc))))))))

(defun lispm-desktop-deps-recompile-all-parallel ()
  "Recompile all activated packages in `package-alist' in parallel."
  (let* ((descs (mapcar #'cadr package-alist))
         (files (lispm-desktop-deps--collect-compile-files descs)))
    (lispm-desktop-deps-compile-files-parallel files t)))

(defun lispm-desktop-deps--prefetch-vc-deps (deps)
  "Refresh archives and prefetch archive tarballs for DEPS."
  (when lispm-desktop-deps--fast-install-active
    (when-let* ((missing (seq-remove
                          (lambda (dep)
                            (or (eq (car dep) 'emacs)
                                (package-installed-p (car dep) (cadr dep))
                                (locate-library (concat (symbol-name (car dep)) ".el") t)
                                (assq (car dep) lispm-desktop-deps)))
                          deps)))
      (lispm-desktop-deps--refresh missing)
      (lispm-desktop-deps-prefetch-packages missing))))

(defvar lispm-desktop-deps--refreshed nil
  "Archive configuration and contents last refreshed for project dependencies.")

(defvar lispm-desktop-deps--refresh-failed nil
  "Non-nil when an archive failed during the current refresh.")

(defun lispm-desktop-deps--before-refresh (&rest _)
  "Prefetch archive indices via curl before `package-refresh-contents'."
  (when lispm-desktop-deps--fast-install-active
    (lispm-desktop-deps-prefetch-archives)))

(defun lispm-desktop-deps--after-refresh (&rest _)
  "Record `lispm-desktop-deps--refreshed' after refreshing archives."
  (when (and package-archive-contents
             (not lispm-desktop-deps--refresh-failed))
    (setq lispm-desktop-deps--refreshed
          (cons (lispm-desktop-deps--archive-settings)
                (copy-tree package-archive-contents)))))

(defun lispm-desktop-deps--skip-vc-archives (orig &rest args)
  "Skip downloading `elpa-packages.eld' during fast startup VC installation."
  (unless lispm-desktop-deps--fast-install-active
    (apply orig args)))

(defun lispm-desktop-deps-enable-fast-install ()
  "Enable parallel package prefetch and deferred byte-compilation."
  (setq lispm-desktop-deps--fast-install-active t)
  (advice-add 'package-refresh-contents :before #'lispm-desktop-deps--before-refresh)
  (advice-add 'package-refresh-contents :after #'lispm-desktop-deps--after-refresh)
  (advice-add 'package-vc--download-and-read-archives :around #'lispm-desktop-deps--skip-vc-archives)
  (advice-add 'package--with-response-buffer-1 :around #'lispm-desktop-deps--cached-response-buffer)
  (advice-add 'package-vc-install-dependencies :before #'lispm-desktop-deps--prefetch-vc-deps)
  (advice-add 'package--compile :around #'lispm-desktop-deps--defer-compile))

(defun lispm-desktop-deps-disable-fast-install (&optional sync-compile)
  "Disable fast-install hooks and flush any deferred byte-compilations."
  (setq lispm-desktop-deps--fast-install-active nil)
  (advice-remove 'package-refresh-contents #'lispm-desktop-deps--before-refresh)
  (advice-remove 'package-refresh-contents #'lispm-desktop-deps--after-refresh)
  (advice-remove 'package-vc--download-and-read-archives #'lispm-desktop-deps--skip-vc-archives)
  (advice-remove 'package--with-response-buffer-1 #'lispm-desktop-deps--cached-response-buffer)
  (advice-remove 'package-vc-install-dependencies #'lispm-desktop-deps--prefetch-vc-deps)
  (advice-remove 'package--compile #'lispm-desktop-deps--defer-compile)
  (lispm-desktop-deps-flush-deferred-compiles sync-compile))

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

(defun lispm-desktop-deps--checkout-root (main)
  "Return the git repository root containing MAIN when it is a clean checkout
on a branch, or nil."
  (let ((dir (file-name-directory main)))
    (when (and (file-regular-p main)
               (file-directory-p dir))
      (when-let* ((root (lispm-desktop-deps--git dir "rev-parse" "--show-toplevel"))
                  ((not (string-empty-p root)))
                  ((file-exists-p (expand-file-name ".git" root)))
                  ((lispm-desktop-deps--git root "rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{upstream}"))
                  ((equal (lispm-desktop-deps--git root "status" "--porcelain" "--untracked-files=no") "")))
        (file-name-as-directory root)))))

(defun lispm-desktop-deps--updatable-p (main)
  "Return non-nil when MAIN belongs to a package or clean git checkout that
`lispm-desktop-deps--catch-up' can update."
  (or (lispm-desktop-deps--package main)
      (lispm-desktop-deps--checkout-root main)))

(defun lispm-desktop-deps--catch-up (main spec)
  "Make the project whose main file is MAIN have what the desktop needs of
it, SPEC its plist in `lispm-desktop-deps': a package installed from
git is updated to its latest release, in an Emacs of its own, and a
clean git checkout on a branch is fast-forwarded to its latest release;
anything else is an error saying what to update."
  (let* ((desc (lispm-desktop-deps--package main))
         (directory (if desc
                        (package-desc-dir desc)
                      (lispm-desktop-deps--checkout-root main)))
         (tag (plist-get spec :tag))
         (release (and directory
                       (if tag
                           (and (lispm-desktop-deps--git directory "fetch" "--quiet" "origin"
                                                        (concat "refs/tags/" tag ":refs/tags/" tag))
                                (lispm-desktop-deps--git directory "rev-parse" (concat tag "^{commit}")))
                         (lispm-desktop-deps--release directory)))))
    (unless release
      (error "%s" (lispm-desktop-deps--behind main spec)))
    (message "%s" (lispm-desktop-deps--tr "Updating %s to its latest release, which this LispM Desktop needs…"
                                          (abbreviate-file-name directory)))
    (if desc
        (progn
          (lispm-desktop-store-installer-call
           `(update ,(package-desc-name desc) ,release ,(lispm-desktop-deps--git directory "rev-parse" "HEAD")))
          ;; Built again: what it autoloads now.
          (load (expand-file-name (format "%s-autoloads" (package-desc-name desc)) directory) t t))
      (lispm-desktop-store-checkout-preflight
       directory release (lispm-desktop-deps--git directory "rev-parse" "HEAD"))
      (lispm-desktop-store--git directory "merge" "--ff-only" "--quiet" "--no-autostash"
                                "--no-overwrite-ignore" release))
    (when-let* ((behind (lispm-desktop-deps--behind main spec)))
      (error "%s" behind))))

(defun lispm-desktop-deps--watch-archive (original archive file &rest args)
  "Run ORIGINAL for ARCHIVE and FILE, recording unsuccessful downloads."
  (let* ((path (expand-file-name (concat "archives/" (car archive) "/" file) package-user-dir))
         (before (file-attributes path)))
    (condition-case err
        (prog1 (apply original archive file args)
          ;; package.el can return normally without writing a malformed response.
          ;; An unchanged cache is conservatively treated as a failure too.
          (when (equal before (file-attributes path))
            (setq lispm-desktop-deps--refresh-failed t)))
      (error (setq lispm-desktop-deps--refresh-failed t)
             (signal (car err) (cdr err))))))

(defun lispm-desktop-deps--archive-settings ()
  "Return the archive configuration relevant to refreshing package metadata."
  (copy-tree (list package-archives package-archive-priorities package-pinned-packages
                   package-user-dir package-check-signature package-unsigned-archives
                   package-gnupghome-dir package-refresh-contents-hook)))

(defun lispm-desktop-deps--refresh (missing)
  "Refresh archives when MISSING cannot use this process's last refresh."
  (let ((settings (lispm-desktop-deps--archive-settings)))
    (unless (and lispm-desktop-deps--refreshed
                 (equal settings (car lispm-desktop-deps--refreshed))
                 (equal package-archive-contents (cdr lispm-desktop-deps--refreshed))
                 (cl-every (lambda (dep)
                             (seq-some (lambda (desc)
                                         (version-list-<= (cadr dep) (package-desc-version desc)))
                                       (cdr (assq (car dep) package-archive-contents))))
                           missing))
      (setq lispm-desktop-deps--refreshed nil)
      (when lispm-desktop-deps--fast-install-active
        (lispm-desktop-deps-prefetch-archives))
      (let ((lispm-desktop-deps--refresh-failed nil))
        (advice-add 'package--download-one-archive :around #'lispm-desktop-deps--watch-archive)
        (unwind-protect (package-refresh-contents)
          (advice-remove 'package--download-one-archive #'lispm-desktop-deps--watch-archive))
        (unless lispm-desktop-deps--refresh-failed
          (setq lispm-desktop-deps--refreshed
                (cons settings (copy-tree package-archive-contents))))))))

(defconst lispm-desktop-deps-auth-version "0.2.8")
(defconst lispm-desktop-deps-auth-sha256
  "6fe48147cd8b883c2c11e71eadc151fe44a46e6ebf81aac6998ca77d421f8746")
(defconst lispm-desktop-deps-auth-source-sha256
  "3b59cb5d489394a4e0d90f0a151f32dd381301cb28dd375eb4a114df9129bac1")

(defun lispm-desktop-deps--notify-progress (installer-status &optional assistant-status)
  "Ensure a visible progress UI when an installation or update begins."
  (cond
   ((and (not (bound-and-true-p lispm-desktop-installer-screen--active))
         (fboundp 'lispm-desktop-assistant-ensure-progress))
    (lispm-desktop-assistant-ensure-progress (or assistant-status installer-status)))
   (t
    (unless (fboundp 'lispm-desktop-installer-screen-ensure)
      (require 'lispm-desktop-installer-screen nil t))
    (if (fboundp 'lispm-desktop-installer-screen-ensure)
        (lispm-desktop-installer-screen-ensure installer-status)
      (when (fboundp 'lispm-desktop-installer-screen-report)
        (lispm-desktop-installer-screen-report installer-status))))))

(defun lispm-desktop-deps--existing-auth-dir (root)
  "Return the newest installed Auth client directory under ROOT, or nil."
  (when (file-directory-p root)
    (let ((dirs (sort (directory-files root nil "\\`lispm-auth-[0-9]" t)
                      (lambda (a b)
                        (version-list-<
                         (or (ignore-errors
                               (version-to-list (string-remove-prefix "lispm-auth-" b)))
                             '(0))
                         (or (ignore-errors
                               (version-to-list (string-remove-prefix "lispm-auth-" a)))
                             '(0)))))))
      (seq-some (lambda (name)
                  (let ((dir (expand-file-name name root)))
                    (and (file-regular-p (expand-file-name "lispm-auth.el" dir))
                         dir)))
                dirs))))

(defun lispm-desktop-deps--confirm-update (template &rest args)
  "Confirm updating a component with TEMPLATE and ARGS."
  (or noninteractive
      (progn
        (unless (fboundp 'lispm-desktop-assistant-confirm-update)
          (require 'lispm-desktop-assistant nil t))
        (if (fboundp 'lispm-desktop-assistant-confirm-update)
            (apply #'lispm-desktop-assistant-confirm-update template args)
          (y-or-n-p (apply #'format template args))))))

(defun lispm-desktop-deps--auth-install ()
  "Install verified Auth client beside existing packages without replacing them."
  (require 'url)
  (let* ((root (expand-file-name "lispm-desktop-components" user-emacs-directory))
          (directory (expand-file-name (concat "lispm-auth-" lispm-desktop-deps-auth-version) root))
          (main (expand-file-name "lispm-auth.el" directory))
          (existing (lispm-desktop-deps--existing-auth-dir root))
          (prefetched (and lispm-desktop-deps--prefetch-dir
                           (expand-file-name
                            (format "lispm-auth-client-%s.tar.gz" lispm-desktop-deps-auth-version)
                            lispm-desktop-deps--prefetch-dir))))
    (cond
     ((and (file-regular-p main)
           (equal lispm-desktop-deps-auth-source-sha256
                  (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally main)
                    (secure-hash 'sha256 (current-buffer)))))
      (add-to-list 'load-path directory))
     ((and existing
           (not (lispm-desktop-deps--confirm-update
                 "Update LispM Auth client to %s? "
                 lispm-desktop-deps-auth-version)))
      (add-to-list 'load-path existing))
     (t
      (when (file-exists-p directory)
        (error "Auth client directory already exists but is not the verified release: %s" directory))
      (let ((updating (and existing t)))
        (lispm-desktop-deps--notify-progress
         (format "%s: lispm-auth" (if updating "Updating project" "Installing project"))
         (format "%s: lispm-auth…" (if updating "Updating component" "Installing component"))))
      (make-directory root t)
      (let ((archive (make-temp-file "lispm-auth-client-" nil ".tar.gz"))
            (stage (make-temp-file (expand-file-name ".auth-" root) t)))
        (unwind-protect
            (progn
              (if (and prefetched (file-regular-p prefetched))
                  (copy-file prefetched archive t)
                (url-copy-file
                 (format "https://github.com/LispMachineLab/lispm-auth/releases/download/v%s/lispm-auth-client-%s.tar.gz"
                         lispm-desktop-deps-auth-version lispm-desktop-deps-auth-version)
                 archive t))
              (unless (equal lispm-desktop-deps-auth-sha256
                             (with-temp-buffer
                               (set-buffer-multibyte nil)
                               (insert-file-contents-literally archive)
                               (secure-hash 'sha256 (current-buffer))))
                (error "LispM Auth client archive SHA256 mismatch"))
              (with-temp-buffer
                (set-buffer-multibyte nil)
                (let ((coding-system-for-read 'no-conversion))
                  (unless (zerop (call-process "tar" nil (current-buffer) nil "-xOf" archive
                                              (format "lispm-auth-client-%s/lispm-auth.el"
                                                      lispm-desktop-deps-auth-version)))
                    (error "Cannot read LispM Auth client archive")))
                (unless (equal lispm-desktop-deps-auth-source-sha256 (secure-hash 'sha256 (current-buffer)))
                  (error "LispM Auth client source SHA256 mismatch"))
                (let ((coding-system-for-write 'no-conversion))
                  (write-region (point-min) (point-max) (expand-file-name "lispm-auth.el" stage) nil 'silent)))
              (rename-file stage directory))
          (delete-file archive)
          (when (file-directory-p stage) (delete-directory stage t))))
      (add-to-list 'load-path directory)))))

(defun lispm-desktop-deps-ensure-auth ()
  "Install the pinned Auth client unless a user-managed copy takes precedence."
  (let* ((main (locate-library "lispm-auth.el" t))
         (checkout (lispm-desktop-deps--checkout "lispm-auth")))
    (cond
     ((and main (file-in-directory-p main package-user-dir)
           (equal (lispm-desktop-deps--git (file-name-directory main) "rev-parse" "HEAD")
                  "7f56c31de638c084aaba409a1f3630324180cd9b")
           (equal (lispm-desktop-deps--git (file-name-directory main) "remote" "get-url" "origin")
                  "https://github.com/LispMachineLab/lispm-auth")
           (equal (lispm-desktop-deps--git (file-name-directory main) "status" "--porcelain" "--untracked-files=no") ""))
      ;; The old default package is left intact; the new client takes precedence.
      (lispm-desktop-deps--auth-install))
     ((and main (not (file-in-directory-p main (expand-file-name "lispm-desktop-components" user-emacs-directory))))
      (when (version-list-< (or (lispm-desktop-deps--version main) '(0))
                            (version-to-list lispm-desktop-deps-auth-version))
        (display-warning 'lispm-desktop
                         (format "User-managed LispM Auth in %s is older than %s; update it to enable all supported platforms."
                                 (file-name-directory main) lispm-desktop-deps-auth-version) :warning)))
     ((and (not main) checkout) (add-to-list 'load-path checkout))
     (t (lispm-desktop-deps--auth-install)))))

(defun lispm-desktop-deps-ensure (feature)
  "Make FEATURE, a project of `lispm-desktop-deps', loadable.
A copy already on `load-path' is used as it is; else a checkout beside the
desktop's joins it, see `lispm-desktop-deps--checkout'; else the latest
release of the project is installed from its repository with package-vc,
its newest tag vX.Y.Z, or the commit that last changed its Version header
before it has one, unless it is installed already, inactive.  The
packages its main file requires, and lacks, are installed.  A copy
lacking the library the desktop needs of it, its :needs, or older than
its :version, is brought up to date, see `lispm-desktop-deps--catch-up'."
  (pcase-let* ((spec (alist-get feature lispm-desktop-deps))
               (`(,url ,checkout ,lisp) (list (plist-get spec :url) (plist-get spec :checkout)
                                              (plist-get spec :lisp-dir)))
               (file (concat (symbol-name feature) ".el")))
    (unless (locate-library file t)
      (if-let* ((directory (lispm-desktop-deps--checkout checkout)))
          (add-to-list 'load-path directory)
        ;; An Emacs that has not read which packages are installed, as one
        ;; run with -Q, reads it: else it would install one again.
        (unless package-alist
          (package-load-all-descriptors))
        (unless (assq feature package-alist)
          (lispm-desktop-deps--notify-progress
           (format "Installing project: %s" feature)
           (format "Installing component: %s…" feature))
          (advice-add 'vc-git-clone :override #'lispm-desktop-store-installer--clone)
          (advice-add 'package-vc--release-rev :before #'lispm-desktop-store-installer--whole-history)
          (unwind-protect
              (condition-case err
                  (package-vc-install `(,feature :url ,url ,@(and lisp (list :lisp-dir lisp)))
                                      (or (plist-get spec :tag) (lispm-desktop-deps--release-tag url) :last-release))
                (wrong-type-argument
                 (if (equal (cdr err) '(stringp nil))
                     (error "Could not install %s from %s; check Git and network access, then retry" feature url)
                   (signal (car err) (cdr err)))))
            (advice-remove 'package-vc--release-rev #'lispm-desktop-store-installer--whole-history)
            (advice-remove 'vc-git-clone #'lispm-desktop-store-installer--clone)))
        ;; package.el activates a package only once each package it
        ;; requires is installed as one, and so leaves inactive a project
        ;; requiring another the desktop has as a checkout, as LispM UI
        ;; requires lispm-kit: it is activated here, its requirements
        ;; being here already.
        (when-let* (((not (locate-library file t)))
                    (desc (cadr (assq feature package-alist))))
          (package-activate-1 desc))))
    (when-let* ((main (locate-library file t)))
      (when-let* ((behind (lispm-desktop-deps--behind main spec)))
        (if (not (lispm-desktop-deps--updatable-p main))
            (error "%s" behind)
          (if (lispm-desktop-deps--confirm-update
               "Update component %s to its latest release? " feature)
              (progn
                (lispm-desktop-deps--notify-progress
                 (format "Updating project: %s" feature)
                 (format "Updating component: %s…" feature))
                (lispm-desktop-deps--catch-up main spec))
            (display-warning 'lispm-desktop behind :warning))))
      (when-let* ((missing (lispm-desktop-deps--requirements main)))
        (lispm-desktop-deps--notify-progress
         (format "Preparing: %s" feature)
         (format "Installing component: %s…" feature))
        (lispm-desktop-deps--refresh missing)
        (when lispm-desktop-deps--fast-install-active
          (lispm-desktop-deps-prefetch-packages missing))
        (dolist (dep missing)
          (unless (package-installed-p (car dep) (cadr dep))
            (package-install (car dep))))))))

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
