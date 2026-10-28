;;; lispm-desktop-update.el --- Software Update: keeping LispM up to date  -*- lexical-binding: t; -*-

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

;; LispM runs from git checkouts: lispm-desktop, lispm-agent,
;; lispm-browser and lispm-kit, and lispm-kernel once asked for, side by
;; side in ~/.lispm as install.sh made them, or wherever a developer keeps
;; them; or from packages package-vc installed from git, as the desktop
;; installs a project it finds no checkout of (lispm-desktop-deps.el).  Each
;; project releases its versions as tags of its main branch, v0.2.0 and
;; on, and Software Update keeps each at the latest release, as running
;; install.sh again does:
;;
;;   checking    fetches each checkout's branch, with the tags of its
;;               commits, in the background and lists what the latest
;;               release brings; nothing changes yet.  A checkout cloned
;;               shallow stays so: only the commits since the release
;;               before its own are fetched
;;   installing  moves each checkout on to its latest release, as LispM
;;               quits, so that the code running never mixes old files
;;               and new; restarting LispM installs at once.  A package
;;               is checked out at its release and built again in an
;;               Emacs of its own, as the App Store updates its own;
;;               packages go first, and while one of them fails the
;;               checkouts wait, for a release of one project may need
;;               another's, as the desktop's 0.9.0 needs the agent's 0.4.0
;;
;; A checkout holding its latest release, or newer commits, as a
;; developer's on the tip of main, is up to date.  One with changes or
;; commits of its own, or on no branch, is left as it is, as install.sh
;; leaves it; a package on no branch follows its origin's main.  A
;; package from the package archives is the package manager's to update.
;; An update to LispM.app
;; itself (install.sh, or macos/ of lispm-desktop) is installed by
;; install.sh, run once LispM has quit; it opens LispM again when done.
;;
;; git runs in the background, in lispm-kernel when it runs (`lispm-exec'),
;; but for installing, which LispM waits for as it quits.
;;
;; Software Update checks by itself every day or every week
;; (`lispm-desktop-update-check-automatically',
;; `lispm-desktop-update-schedule').  What it
;; finds is installed the next time LispM quits
;; (`lispm-desktop-update-install-automatically'); when that is off, or
;; when the update needs install.sh, it asks first.  Its control panel is
;; Software Update, in Control Panels; `lispm-desktop-update-now' checks
;; at once and offers to restart into what it finds.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'lispm-desktop-core)
(require 'lispm-desktop-i18n)
(require 'lispm-desktop-store-installer)
(require 'lispm-exec)

(declare-function lispm-desktop-popup-alert "lispm-desktop-popup" (icon message buttons &optional detail))
(declare-function lispm-desktop-popup-available-p "lispm-desktop-popup" ())

(defgroup lispm-desktop-update nil
  "Software Update: keeping LispM up to date."
  :group 'lispm-desktop
  :prefix "lispm-desktop-update-")

(defcustom lispm-desktop-update-check-automatically t
  "Non-nil means Software Update checks for updates by itself, as often as
`lispm-desktop-update-schedule' says."
  :type 'boolean
  :group 'lispm-desktop-update)

(defcustom lispm-desktop-update-schedule 'daily
  "How often Software Update checks by itself: `daily' or `weekly'."
  :type '(choice (const :tag "Every day" daily)
                 (const :tag "Every week" weekly))
  :group 'lispm-desktop-update)

(defcustom lispm-desktop-update-install-automatically t
  "Non-nil means updates found are installed when LispM quits, without
asking, and run from its next start.  nil means Software Update asks
first, and installs when told to."
  :type 'boolean
  :group 'lispm-desktop-update)

(defcustom lispm-desktop-update-file (locate-user-emacs-file "lispm-update.eld")
  "File Software Update keeps when it last checked and its log in."
  :type 'file
  :group 'lispm-desktop-update)

(defconst lispm-desktop-update-projects
  '(("LispM Desktop" . "lispm-desktop-core.el")
    ("LispM Agent" . "lispm.el")
    ("LispM Browser" . "lispm-browser.el")
    ("LispM Kit" . "lispm-kit-i18n.el")
    ("LispM UI" . "lispm-ui.el")
    ("LispM Kernel" . "lispm-kernel.el"))
  "The LispM projects Software Update keeps up to date: (NAME . FILE), FILE
a file of the project's sources, in its lisp/ directory or at its top,
as `locate-library' finds it.")

(defconst lispm-desktop-update-release-regexp
  "\\`v\\([0-9]+\\.[0-9]+\\.[0-9]+\\)\\'"
  "What the tag of a release is: v and its version, as v0.2.0.
A tag with more, as v0.3.0-rc1, is no release.")

(defconst lispm-desktop-update-log-size 100
  "How many installs the log keeps.")

(defvar lispm-desktop-update--checking nil
  "Non-nil while Software Update checks.")

(defvar lispm-desktop-update--waiting nil
  "Functions to call with the updates found once the check running ends.")

(defvar lispm-desktop-update--checked nil
  "When Software Update last checked, or nil.")

(defvar lispm-desktop-update--results nil
  "What the last check found, a plist a checkout, see
`lispm-desktop-update--check-checkout'.")

(defvar lispm-desktop-update--log nil
  "The installs, the latest first: (TIME NAME FROM TO CHANGES).")

(defvar lispm-desktop-update--announced nil
  "The commits the last announcement told of, so that it is made once.")

(defvar lispm-desktop-update--requested nil
  "Non-nil once installing as LispM quits was asked for, even with
`lispm-desktop-update-install-automatically' off.")

(defvar lispm-desktop-update--reopen nil
  "Non-nil when install.sh, run as LispM quits, opens LispM after.")

(defvar lispm-desktop-update--installed nil
  "The checkouts and what each holds, once worked out, see
`lispm-desktop-update-installed'.")

(defvar lispm-desktop-update--installed-generation 0
  "Counts the times `lispm-desktop-update--installed' was forgotten, so
that what was worked out before is not kept after.")

(defvar lispm-desktop-update--working-out nil
  "Non-nil while what the checkouts hold is worked out in the background.")

(defvar lispm-desktop-update--timer nil
  "The timer of the checks Software Update makes by itself.")

(defvar lispm-desktop-update-changed-hook nil
  "Hook run once what Software Update knows changed: a check started or
ended, updates installed.  Its control panel draws itself again.")

(defvar lispm-desktop-update-check-hook nil
  "Hook run as Software Update begins a check, by itself or asked: what
else is kept up to date checks along, as the App Store's applications.")

;;;; Git

(defun lispm-desktop-update--environment ()
  "Return the environment git runs in: no repository of Emacs's own, as an
inherited GIT_DIR would name, and no prompt for a password."
  (append '("GIT_DIR" "GIT_WORK_TREE" "GIT_INDEX_FILE" "GIT_TERMINAL_PROMPT=0")
          process-environment))

(defun lispm-desktop-update--git (directory &rest args)
  "Run git with ARGS in DIRECTORY and wait; return what it printed, trimmed,
or nil when it failed."
  (let ((default-directory (file-name-as-directory directory))
        (process-environment (lispm-desktop-update--environment)))
    (with-temp-buffer
      (when (eql 0 (apply #'call-process "git" nil '(t nil) nil args))
        (string-trim (buffer-string))))))

(defun lispm-desktop-update--git-async (directory args callback)
  "Run git with ARGS in DIRECTORY in the background, then call CALLBACK with
what it printed, trimmed, or nil when it failed, and what it said then.
lispm-kernel runs it when it runs, see `lispm-exec'."
  (let ((default-directory (file-name-as-directory directory))
        (process-environment (lispm-desktop-update--environment)))
    (lispm-exec "git" args
                (lambda (exit output errors)
                  (if (eql exit 0)
                      (funcall callback (string-trim output) nil)
                    (let ((said (string-trim (concat errors (if (string-empty-p errors) output "")))))
                      (funcall callback nil (cond ((stringp exit) exit)
                                                  ((string-empty-p said) (lispm-desktop-tr-noop "git failed"))
                                                  (t said)))))))))

(defun lispm-desktop-update--git-steps (directory steps callback)
  "Run STEPS of git in DIRECTORY in the background, one after another.
Each step is (ARGS . NEXT): ARGS, or a function returning them once the
steps before have run, or nil when the step is not needed then; NEXT is
called with what git printed, or nil when it failed, and what it said
then, and returns t to go on with the steps, or a result to stop with.
CALLBACK is called with the result, or nil when every step went on."
  (if (null steps)
      (funcall callback nil)
    (pcase-let* ((`(,args . ,next) (car steps))
                 (args (if (functionp args) (funcall args) args)))
      (if (null args)
          (lispm-desktop-update--git-steps directory (cdr steps) callback)
        (lispm-desktop-update--git-async
         directory args
         (lambda (output errors)
           (let ((result (funcall next output errors)))
             (if (eq result t)
                 (lispm-desktop-update--git-steps directory (cdr steps) callback)
               (funcall callback result)))))))))

;;;; Checkouts

(defun lispm-desktop-update--top-of (top)
  "Return TOP, what git said the top of a checkout is, as a directory, or nil."
  (and top (not (string-empty-p top))
       (file-name-as-directory (file-truename top))))

(defun lispm-desktop-update--top (directory &optional callback)
  "Return the top of the git checkout holding DIRECTORY, or nil.
With CALLBACK, call it with that instead, git running in the background."
  (cond ((not (file-directory-p directory))
         (if callback (funcall callback nil) nil))
        (callback
         (lispm-desktop-update--git-async directory '("rev-parse" "--show-toplevel")
                                          (lambda (top _errors)
                                            (funcall callback (lispm-desktop-update--top-of top)))))
        (t (lispm-desktop-update--top-of
            (lispm-desktop-update--git directory "rev-parse" "--show-toplevel")))))

(defun lispm-desktop-update--package (directory)
  "Return the name of the package whose directory DIRECTORY is, the top of
a checkout package-vc made in `package-user-dir', or nil when it is none."
  (let ((directory (directory-file-name (file-truename directory))))
    (and (boundp 'package-user-dir)
         (equal (file-name-directory directory)
                (file-name-as-directory (file-truename package-user-dir)))
         (intern (file-name-nondirectory directory)))))

(defun lispm-desktop-update--checkouts-of (projects tops)
  "Return the checkouts of PROJECTS, (NAME . DIRECTORY) each, whose tops
of their checkouts TOPS are, each or nil, a checkout once."
  (let ((seen nil) (checkouts nil))
    (cl-mapc (lambda (project top)
               (when (and top (not (member top seen)))
                 (push top seen)
                 (push (cons (car project) top) checkouts)))
             projects tops)
    (nreverse checkouts)))

(defun lispm-desktop-update-checkouts (&optional callback)
  "Return the checkouts of the LispM projects this Emacs runs, (NAME .
DIRECTORY) each, DIRECTORY the top of the checkout: a project's own, or
the package package-vc installed it as, see
`lispm-desktop-update--package'.  A project not in a git checkout, as a
package from the archives, is left out; so is a project in the checkout
of one before it, as when the projects were one.  With CALLBACK, call it
with them instead, git running in the background, and return nil."
  (let* ((projects (seq-keep (pcase-lambda (`(,name . ,file))
                               (when-let* ((library (locate-library file t)))
                                 (cons name (file-truename (file-name-directory library)))))
                             lispm-desktop-update-projects))
         (left (length projects))
         (tops (make-vector left nil)))
    (cond ((not callback)
           (lispm-desktop-update--checkouts-of
            projects (mapcar (lambda (project) (lispm-desktop-update--top (cdr project))) projects)))
          ((zerop left) (funcall callback nil))
          (t (seq-do-indexed
              (lambda (project index)
                (lispm-desktop-update--top
                 (cdr project)
                 (lambda (top)
                   (aset tops index top)
                   (when (zerop (setq left (1- left)))
                     (funcall callback (lispm-desktop-update--checkouts-of projects (append tops nil)))))))
              projects)
             nil))))

(defun lispm-desktop-update--release-version (tag)
  "Return the version TAG releases, as \"0.2.0\" for v0.2.0, or nil when
TAG is no release, see `lispm-desktop-update-release-regexp'."
  (and tag (string-match lispm-desktop-update-release-regexp tag)
       (match-string 1 tag)))

(defconst lispm-desktop-update--describe
  '("describe" "--tags" "--long" "--match" "v[0-9]*" "--exclude" "*-*")
  "How git describes a checkout's commit by its latest release.")

(defconst lispm-desktop-update--last-commit '("log" "-1" "--format=%h, %cs")
  "How git tells a checkout's commit and its date.")

(defun lispm-desktop-update--described (described)
  "Return what a checkout holds, as `lispm-desktop-update-version' tells
it, from DESCRIBED, what git describe said of it; nil when it said
nothing of a release."
  (when (and described
             (string-match "\\`\\(v.+\\)-\\([0-9]+\\)-g\\([0-9a-f]+\\)\\'" described))
    (let ((tag (match-string 1 described))
          (since (string-to-number (match-string 2 described)))
          (commit (match-string 3 described)))
      (if (zerop since)
          (lispm-desktop-update--release-version tag)
        (format "%s+%d, %s" (lispm-desktop-update--release-version tag) since commit)))))

(defun lispm-desktop-update-version (directory &optional callback)
  "Return what the checkout DIRECTORY holds, or nil: its release, as
\"0.2.0\"; past it, the release, how many commits since and the commit,
as \"0.2.0+3, 626d068\"; before the first release, the commit and its
date, as \"626d068, 2026-10-20\".  With CALLBACK, call it with that
instead, git running in the background."
  (if (not callback)
      (or (lispm-desktop-update--described
           (apply #'lispm-desktop-update--git directory lispm-desktop-update--describe))
          (apply #'lispm-desktop-update--git directory lispm-desktop-update--last-commit))
    (lispm-desktop-update--git-async
     directory lispm-desktop-update--describe
     (lambda (described _errors)
       (if-let* ((version (lispm-desktop-update--described described)))
           (funcall callback version)
         (lispm-desktop-update--git-async directory lispm-desktop-update--last-commit
                                          (lambda (commit _errors) (funcall callback commit))))))))

(defun lispm-desktop-update--changes (log)
  "Return the changes LOG tells of, the newest first; LOG is what
`git log --first-parent --format=%s%x1f%b%x1e' printed of them.
The changes are read along the branch's own line, a merged pull request
being one: its title and number, as \"Mend the close box (#26)\", from a
GitHub merge commit's body and subject; any other commit is its subject.
The commits a pull request brought are not listed: in a checkout cloned
shallow, as install.sh clones them, those of a branch older than the
checkout's commit would list most of the history again."
  (delq nil
        (mapcar (lambda (entry)
                  (pcase-let* ((`(,subject ,body) (split-string entry "\x1f"))
                               (subject (string-trim subject))
                               (number (and (string-match "\\`Merge pull request #\\([0-9]+\\) from "
                                                          subject)
                                            (match-string 1 subject)))
                               (title (car (split-string (or body "") "\n" t "[ \t]+"))))
                    (cond ((string-empty-p subject) nil)
                          ((and number title) (format "%s (#%s)" title number))
                          (t subject))))
                (split-string (or log "") "\x1e" t "[ \t\n]+"))))

(defun lispm-desktop-update-title (update)
  "Return what UPDATE is called: its project, and the version it brings,
as \"LispM Desktop 0.3.0\"."
  (string-join (delq nil (list (plist-get update :name)
                               (lispm-desktop-update--release-version (plist-get update :release))))
               " "))

(defun lispm-desktop-update--forget-installed ()
  "Forget what the checkouts hold, to be worked out anew."
  (setq lispm-desktop-update--installed nil
        lispm-desktop-update--installed-generation (1+ lispm-desktop-update--installed-generation)))

(defun lispm-desktop-update--work-out-installed ()
  "Work out what the checkouts hold in the background, git running for
each at once; then keep it and run `lispm-desktop-update-changed-hook'."
  (unless lispm-desktop-update--working-out
    (setq lispm-desktop-update--working-out t)
    (let ((generation lispm-desktop-update--installed-generation))
      (cl-flet ((done (installed)
                  (setq lispm-desktop-update--working-out nil)
                  (when (= generation lispm-desktop-update--installed-generation)
                    (setq lispm-desktop-update--installed installed))
                  (run-hooks 'lispm-desktop-update-changed-hook)))
        (lispm-desktop-update-checkouts
         (lambda (checkouts)
           (let* ((left (length checkouts))
                  (installed (mapcar (pcase-lambda (`(,name . ,directory)) (list name directory nil))
                                     checkouts)))
             (if (zerop left)
                 (done nil)
               (dolist (entry installed)
                 (lispm-desktop-update-version
                  (nth 1 entry)
                  (lambda (version)
                    (setf (nth 2 entry) version)
                    (when (zerop (setq left (1- left)))
                      (done installed)))))))))))))

(defun lispm-desktop-update-installed ()
  "Return the checkouts of LispM and what each holds, (NAME DIRECTORY
VERSION) each, VERSION as `lispm-desktop-update-version' tells it.  It is
worked out once, and again after a check or an install, in the
background: until it is, this returns nil, and once it is,
`lispm-desktop-update-changed-hook' runs."
  (or lispm-desktop-update--installed
      (progn (lispm-desktop-update--work-out-installed)
             nil)))

(defun lispm-desktop-update--desktop-p (directory)
  "Return non-nil when DIRECTORY is the top of the desktop's own checkout."
  (equal directory (file-name-as-directory (file-truename lispm-desktop-directory))))

(defconst lispm-desktop-update-app-files '("install.sh" "macos")
  "The files of lispm-desktop LispM.app is made of: install.sh, and macos/,
its launcher and icon.")

(defun lispm-desktop-update--check-checkout (name directory callback)
  "Fetch the branch the checkout DIRECTORY of the project NAME follows, and
the tags of its commits, in the background, then call CALLBACK with what
was found, a plist:

  :name       NAME
  :directory  DIRECTORY
  :state      `current', up to date; `update', a release is ready;
              `kept', left as it is; `failed', the check failed
  :reason     why it was kept, or what failed: in English, translated
              where shown, or as git said it
  :release    the tag of the branch's latest release
  :from, :to  the commit the checkout holds, and the release's
  :changes    the subjects of the commits the update brings
  :app        non-nil when the update changes LispM.app
  :package    the name of the package DIRECTORY is, see
              `lispm-desktop-update--package'

The latest release is the tag of the highest version on the branch, see
`lispm-desktop-update-release-regexp'.  A checkout holding it, or newer
commits of the branch, is current, and so is one of a branch with no
release yet.  A package on no branch, as one updated before is, follows
its origin's main.

Only what the check needs is fetched: not the tags of every release, the
history of each, which in a checkout cloned shallow, as install.sh and
the App Store clone them, would be most of the repository's history.
Such a checkout is fetched down to the release before its own, see
`lispm-desktop-store--shallow-exclude'."
  (let* ((package (lispm-desktop-update--package directory))
         (result (list :name name :directory directory :state 'current :package package))
         (branch "@{upstream}")
         (shallow nil)
         (own nil)
         (exclude nil)
         (release nil))
    (cl-flet ((stop (state reason)
                (plist-put (plist-put result :state state) :reason reason)))
      (lispm-desktop-update--git-steps
       directory
       `((("rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{upstream}")
          . ,(lambda (output _errors)
               (cond (output t)
                     (package (setq branch "origin/HEAD") t)
                     (t (stop 'kept (lispm-desktop-tr-noop "follows no branch"))))))
         (("rev-parse" "--is-shallow-repository")
          . ,(lambda (output _errors) (setq shallow (equal output "true")) t))
         (,(lambda () (and shallow lispm-desktop-store--own-release))
          . ,(lambda (output _errors) (setq own output) t))
         (,(lambda () (and own `("ls-remote" "--tags" "--refs" ,@(and package '("origin")))))
          . ,(lambda (output _errors)
               (setq exclude (and output (lispm-desktop-store--shallow-exclude own output)))
               t))
         ;; Tags come along with the commits they name.
         (,(lambda () `("fetch" "--quiet" ,@exclude ,@(and package '("origin"))))
          . ,(lambda (output errors)
               (or (and output t)
                   (stop 'failed (string-remove-prefix "fatal: " (car (split-string errors "\n")))))))
         (,(lambda () (list "tag" "--list" "v*" "--sort=-v:refname" "--merged" branch))
          . ,(lambda (output _errors)
               (setq release (seq-find #'lispm-desktop-update--release-version
                                       (and output (split-string output "\n" t))))
               (when release
                 (setq result (plist-put result :release release)))
               (or (and release t) result)))
         (,(lambda () (list "rev-parse" "HEAD" (concat release "^{commit}")))
          . ,(lambda (output _errors)
               (pcase (and output (split-string output "\n"))
                 (`(,from ,to)
                  (setq result (plist-put (plist-put result :from from) :to to))
                  (or (not (equal from to)) result))
                 (_ (stop 'failed (lispm-desktop-tr-noop "its commits do not read"))))))
         ;; Newer than the release, as a developer's checkout is.
         (,(lambda () (list "merge-base" "--is-ancestor" release "HEAD"))
          . ,(lambda (output _errors) (or (not output) result)))
         (,(lambda () (list "merge-base" "--is-ancestor" "HEAD" release))
          . ,(lambda (output _errors)
               (or (and output t) (stop 'kept (lispm-desktop-tr-noop "has commits of its own")))))
         (("status" "--porcelain" "--untracked-files=no")
          . ,(lambda (output _errors)
               (or (equal output "") (stop 'kept (lispm-desktop-tr-noop "has changes of its own")))))
         (,(lambda () (list "log" "--first-parent" "--format=%s%x1f%b%x1e" (concat "HEAD.." release)))
          . ,(lambda (output _errors)
               (setq result (plist-put result :changes (lispm-desktop-update--changes output)))
               t))
         (,(lambda () `("diff" "--name-only" "HEAD" ,release "--" ,@lispm-desktop-update-app-files))
          . ,(lambda (output _errors)
               (plist-put (plist-put result :state 'update)
                          :app (and (lispm-desktop-update--desktop-p directory)
                                    (not (member output '(nil ""))))))))
       (lambda (stopped) (funcall callback (or stopped result)))))))

;;;; Checking

(defun lispm-desktop-update-updates ()
  "Return the updates the last check found, one plist a checkout."
  (seq-filter (lambda (result) (eq (plist-get result :state) 'update))
              lispm-desktop-update--results))

(defun lispm-desktop-update-checking-p ()
  "Return non-nil while Software Update checks."
  lispm-desktop-update--checking)

(defun lispm-desktop-update-last-check ()
  "Return when Software Update last checked, or nil."
  lispm-desktop-update--checked)

(defun lispm-desktop-update-results ()
  "Return what the last check found, a plist a checkout."
  lispm-desktop-update--results)

(defun lispm-desktop-update-check (&optional callback)
  "Check for updates of LispM in the background.
CALLBACK, when given, is called with the updates found once done; while
a check runs already, once it ends."
  (interactive)
  (when callback
    (setq lispm-desktop-update--waiting (append lispm-desktop-update--waiting (list callback))))
  (if lispm-desktop-update--checking
      (when (called-interactively-p 'any)
        (message (lispm-desktop-tr "Software Update is checking already")))
    (setq lispm-desktop-update--checking t)
    (run-hooks 'lispm-desktop-update-check-hook)
    (run-hooks 'lispm-desktop-update-changed-hook)
    (lispm-desktop-update-checkouts
     (lambda (checkouts)
       (lispm-desktop-update--check-checkouts checkouts)))))

(defun lispm-desktop-update--check-checkouts (checkouts)
  "Check CHECKOUTS for updates in the background, as
`lispm-desktop-update-check' does, which found them."
  (let* ((left (length checkouts))
         (results (make-vector left nil)))
    (cl-flet ((finish ()
                (lispm-desktop-update--forget-installed)
                (setq lispm-desktop-update--checking nil
                      lispm-desktop-update--results (append results nil))
                (unless (seq-find (lambda (result) (eq (plist-get result :state) 'failed))
                                  lispm-desktop-update--results)
                  (setq lispm-desktop-update--checked (current-time))
                  (lispm-desktop-update--save))
                (run-hooks 'lispm-desktop-update-changed-hook)
                (let ((waiting lispm-desktop-update--waiting))
                  (setq lispm-desktop-update--waiting nil)
                  (dolist (function waiting)
                    (funcall function (lispm-desktop-update-updates))))))
      (if (null checkouts)
          (finish)
        (seq-do-indexed
         (pcase-lambda (`(,name . ,directory) index)
           (lispm-desktop-update--check-checkout
            name directory
            (lambda (result)
              (aset results index result)
              (when (zerop (setq left (1- left)))
                (finish)))))
         checkouts)))))

(defun lispm-desktop-update--interval ()
  "Return the seconds between the checks Software Update makes by itself."
  (pcase lispm-desktop-update-schedule
    ('weekly (* 7 24 60 60))
    (_ (* 24 60 60))))

(defun lispm-desktop-update--due-p ()
  "Return non-nil when a check by itself is due."
  (or (null lispm-desktop-update--checked)
      (>= (float-time (time-since lispm-desktop-update--checked))
          (lispm-desktop-update--interval))))

(defun lispm-desktop-update--tick ()
  "Check, when checking by itself is on and a check is due; then tell of
what was found.  This runs from `lispm-desktop-update--timer'."
  (when (and lispm-desktop-update-check-automatically
             (not lispm-desktop-update--checking)
             (lispm-desktop-update--due-p))
    (lispm-desktop-update-check #'lispm-desktop-update--announce)))

(defun lispm-desktop-update--summary (updates)
  "Return a line telling what UPDATES bring, by project, in the language
LispM speaks."
  (mapconcat (lambda (update)
               (let ((count (length (plist-get update :changes))))
                 (lispm-desktop-tr-n "%s, %d change" "%s, %d changes" count
                                     (lispm-desktop-update-title update) count)))
             updates "; "))

(defun lispm-desktop-update--announce (updates)
  "Tell of UPDATES, found by a check of Software Update's own, once.
When they install by themselves, as LispM quits, a line says so;
otherwise an alert asks, once Emacs is idle."
  (let ((commits (mapcar (lambda (update) (plist-get update :to)) updates)))
    (when (and updates (not (equal commits lispm-desktop-update--announced)))
      (setq lispm-desktop-update--announced commits)
      (if (lispm-desktop-update-installs-on-quit-p)
          (message (lispm-desktop-tr "Software Update: %s; installed when LispM quits.")
                   (lispm-desktop-update--summary updates))
        (run-with-idle-timer 2 nil #'lispm-desktop-update--offer nil)))))

;;;; Installing

(defun lispm-desktop-update-needs-installer-p (&optional updates)
  "Return non-nil when UPDATES, by default those found, change LispM.app
and this Emacs runs in it, so that install.sh installs them."
  (and (lispm-desktop-update--app)
       (seq-find (lambda (update) (plist-get update :app))
                 (or updates (lispm-desktop-update-updates)))))

(defun lispm-desktop-update-installs-on-quit-p ()
  "Return non-nil when the updates found are installed as LispM quits,
asked or not: only those install.sh need not install."
  (and (lispm-desktop-update-updates)
       (or lispm-desktop-update-install-automatically lispm-desktop-update--requested)
       (or lispm-desktop-update--requested (not (lispm-desktop-update-needs-installer-p)))))

(defun lispm-desktop-update--install-checkout (update)
  "Move the checkout of UPDATE on to the commit fetched for it; return
non-nil once it was, nil when it no longer could, as with changes of its
own made since.  A package is checked out at the commit and built again,
in an Emacs of its own, as the App Store updates one; when that fails,
it is left as it was, and saying so."
  (let ((directory (plist-get update :directory))
        (package (plist-get update :package)))
    (and (file-directory-p directory)
         (equal (lispm-desktop-update--git directory "rev-parse" "HEAD") (plist-get update :from))
         (equal (lispm-desktop-update--git directory "status" "--porcelain" "--untracked-files=no") "")
         (if package
             (condition-case err
                 (progn (lispm-desktop-store-installer-call
                         `(update ,package ,(plist-get update :to) ,(plist-get update :from)))
                        t)
               (error (message (lispm-desktop-tr "Software Update: %s could not be installed: %s")
                               (lispm-desktop-update-title update) (error-message-string err))
                      nil))
           (lispm-desktop-update--git directory "merge" "--ff-only" "--quiet" (plist-get update :to))))))

(defun lispm-desktop-update-install (&optional updates)
  "Install UPDATES, by default those found, at once; return those installed.
Packages go first, and while one of them fails the checkouts wait: a
release of one project may need another's.  Each install is logged.
Code already loaded keeps running: install as Emacs quits, or before it
loads any of the files changed."
  (let* ((updates (or updates (lispm-desktop-update-updates)))
         (packages (seq-filter (lambda (update) (plist-get update :package)) updates))
         (done (seq-filter #'lispm-desktop-update--install-checkout packages))
         (installed (append done
                            (when (= (length done) (length packages))
                              (seq-filter #'lispm-desktop-update--install-checkout
                                          (seq-difference updates packages))))))
    (dolist (update installed)
      (push (list (current-time) (lispm-desktop-update-title update) (plist-get update :from)
                  (plist-get update :to) (plist-get update :changes))
            lispm-desktop-update--log))
    (when installed
      (lispm-desktop-update--forget-installed)
      (setq lispm-desktop-update--log (seq-take lispm-desktop-update--log lispm-desktop-update-log-size)
            lispm-desktop-update--results
            (seq-remove (lambda (result) (memq result installed)) lispm-desktop-update--results))
      (lispm-desktop-update--save)
      (run-hooks 'lispm-desktop-update-changed-hook))
    installed))

(defun lispm-desktop-update--app ()
  "Return LispM.app, when this Emacs runs in it, else nil.
Only an application install.sh made carries LispMEmacsBuild."
  (when (and (eq system-type 'darwin) invocation-directory)
    (let* ((contents (file-name-directory (directory-file-name invocation-directory)))
           (plist (expand-file-name "Info.plist" contents))
           (app (directory-file-name (file-name-directory (directory-file-name contents)))))
      (and (string-suffix-p ".app" app)
           (file-readable-p plist)
           (with-temp-buffer
             (insert-file-contents plist)
             (search-forward "<key>LispMEmacsBuild</key>" nil t))
           app))))

(defun lispm-desktop-update--installer-command (app reopen)
  "Return the shell command running install.sh once this Emacs has quit,
to make APP again, opening it after when REOPEN."
  (let ((home (directory-file-name (file-name-directory (directory-file-name lispm-desktop-directory)))))
    (format (concat "while kill -0 %d 2>/dev/null; do sleep 0.5; done; "
                    "LISPM_HOME=%s LISPM_APP_DIR=%s LISPM_OPEN=%d /bin/sh %s >%s 2>&1")
            (emacs-pid)
            (shell-quote-argument home)
            (shell-quote-argument (directory-file-name (file-name-directory app)))
            (if reopen 1 0)
            (shell-quote-argument (expand-file-name "install.sh" lispm-desktop-directory))
            (shell-quote-argument (expand-file-name "update.log" home)))))

(defun lispm-desktop-update--run-installer (app reopen)
  "Run install.sh once this Emacs has quit, apart from it, to make APP
again; it opens APP after when REOPEN."
  (let ((process-environment (lispm-desktop-update--environment)))
    (call-process "/bin/sh" nil 0 nil "-c"
                  (format "(%s) </dev/null >/dev/null 2>&1 &"
                          (lispm-desktop-update--installer-command app reopen)))))

(defun lispm-desktop-update--install-on-quit ()
  "Install the updates found, as Emacs quits, when they are to be.
An update to LispM.app is installed by install.sh, once Emacs is gone."
  (when (lispm-desktop-update-installs-on-quit-p)
    (let ((app (lispm-desktop-update-needs-installer-p)))
      (lispm-desktop-update-install)
      (when app
        (lispm-desktop-update--run-installer (lispm-desktop-update--app) lispm-desktop-update--reopen)))))

(defun lispm-desktop-update--quit ()
  "Install the updates found as Emacs quits, when they are to be; a batch
Emacs never does.  This runs from `kill-emacs-hook'."
  (unless noninteractive
    (lispm-desktop-update--install-on-quit)))

(defun lispm-desktop-update-restart ()
  "Restart LispM into the updates found, installing them as it quits.
An update to LispM.app quits LispM; install.sh opens it again when done."
  (interactive)
  (unless (lispm-desktop-update-updates)
    (user-error (lispm-desktop-tr "No update is ready; check first")))
  (setq lispm-desktop-update--requested t)
  (if (lispm-desktop-update-needs-installer-p)
      (progn (setq lispm-desktop-update--reopen t)
             (save-buffers-kill-emacs))
    (restart-emacs))
  ;; Still here: quitting was called off; the updates wait for the next.
  (setq lispm-desktop-update--reopen nil))

(defun lispm-desktop-update--offer (asked)
  "Ask whether to restart into the updates found, ASKED for or not.
Restarting is the default only when asked for: a check of Software
Update's own may come up while you type.  This runs from a timer, where
C-g is let quit the alert."
  (when-let* ((updates (lispm-desktop-update-updates)))
    (with-local-quit
      (if (or (active-minibuffer-window) (not (fboundp 'lispm-desktop-popup-available-p))
              ;; Without its window, the alert asks in the minibuffer, where
              ;; a key typed meanwhile would answer.
              (and (not asked) (not (lispm-desktop-popup-available-p))))
          (message (lispm-desktop-tr "Software Update: %s; restart LispM to install it (M-x lispm-desktop-update-restart).")
                   (lispm-desktop-update--summary updates))
        (when (eq 'restart
                  (lispm-desktop-popup-alert
                   'note (lispm-desktop-tr-n "An update to LispM is ready." "Updates to LispM are ready."
                                             (length updates))
                   `((,(lispm-desktop-tr "Later") later cancel)
                     (,(lispm-desktop-tr "Restart") restart ,(and asked 'default)))
                   (if (lispm-desktop-update-needs-installer-p)
                       (lispm-desktop-tr "%s.  LispM quits for install.sh to make LispM.app again, and opens when it is done."
                                         (lispm-desktop-update--summary updates))
                     (lispm-desktop-tr-n
                      "%s.  Restarting installs it; terminals and agents stop, and their sessions can be resumed."
                      "%s.  Restarting installs them; terminals and agents stop, and their sessions can be resumed."
                      (length updates) (lispm-desktop-update--summary updates)))))
          (lispm-desktop-update-restart))))))

(defun lispm-desktop-update-now ()
  "Check for updates at once, then offer to restart into those found."
  (interactive)
  (lispm-desktop-update-check
   (lambda (updates)
     (if updates
         ;; Out of the process sentinel calling this, which reads no input.
         (run-at-time 0 nil #'lispm-desktop-update--offer t)
       (message (if (seq-find (lambda (result) (eq (plist-get result :state) 'failed))
                              lispm-desktop-update--results)
                    (lispm-desktop-tr "Software Update could not check every part of LispM")
                  (lispm-desktop-tr "LispM is up to date")))))))

;;;; The log

(defun lispm-desktop-update--save ()
  "Write when Software Update last checked, and its log, to
`lispm-desktop-update-file'."
  (condition-case err
      (with-temp-file lispm-desktop-update-file
        (let ((print-length nil) (print-level nil))
          (insert ";; Software Update: when it checked last, and what it installed.\n")
          (prin1 (list :checked lispm-desktop-update--checked :log lispm-desktop-update--log)
                 (current-buffer))
          (insert "\n")))
    (error (message (lispm-desktop-tr "Software Update could not write %s: %s")
                    lispm-desktop-update-file (error-message-string err)))))

(defun lispm-desktop-update--load ()
  "Read when Software Update last checked, and its log."
  (when (file-readable-p lispm-desktop-update-file)
    (condition-case nil
        (let ((data (with-temp-buffer
                      (insert-file-contents lispm-desktop-update-file)
                      (read (current-buffer)))))
          (when (plistp data)
            (setq lispm-desktop-update--checked (plist-get data :checked)
                  lispm-desktop-update--log (seq-filter #'consp (plist-get data :log)))))
      (error nil))))

(defconst lispm-desktop-update-log-buffer-name "*Software Update Log*"
  "Name of the buffer showing what Software Update installed.")

(defun lispm-desktop-update-show-log ()
  "Show the updates ready, every change each brings, then what Software
Update installed, the latest first."
  (interactive)
  (with-current-buffer (get-buffer-create lispm-desktop-update-log-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (when-let* ((updates (lispm-desktop-update-updates)))
        (insert (propertize (concat (lispm-desktop-tr "Ready to install") "\n\n") 'face 'bold))
        (dolist (update updates)
          (insert (propertize (format "%s  %s..%s\n" (lispm-desktop-update-title update)
                                      (substring (plist-get update :from) 0 7)
                                      (substring (plist-get update :to) 0 7))
                              'face 'bold))
          (dolist (change (plist-get update :changes))
            (insert "  " change "\n"))
          (insert "\n"))
        (insert (propertize (concat (lispm-desktop-tr "Installed") "\n\n") 'face 'bold)))
      (if (null lispm-desktop-update--log)
          (insert (lispm-desktop-tr "Software Update has installed nothing yet.") "\n")
        (pcase-dolist (`(,time ,name ,from ,to ,changes) lispm-desktop-update--log)
          (insert (propertize (format "%s  %s  %s..%s\n" (format-time-string "%Y-%m-%d %H:%M" time)
                                      name (substring from 0 (min 7 (length from)))
                                      (substring to 0 (min 7 (length to))))
                              'face 'bold))
          (dolist (change changes)
            (insert "  " change "\n"))
          (insert "\n")))
      (goto-char (point-min)))
    (special-mode)
    (pop-to-buffer (current-buffer))))

;;;; Setting up

(defun lispm-desktop-setup-update ()
  "Make Software Update check by itself, and install as Emacs quits.
A batch Emacs, as install.sh runs, does neither."
  (unless noninteractive
    (lispm-desktop-update--load)
    ;; Late, so that what runs after loads none of the files changed.
    (add-hook 'kill-emacs-hook #'lispm-desktop-update--quit 90)
    (when (timerp lispm-desktop-update--timer)
      (cancel-timer lispm-desktop-update--timer))
    ;; A while after the start, so as not to slow it; then every hour,
    ;; checking only once a check is due, so that a Mac waking from sleep
    ;; catches up.
    (setq lispm-desktop-update--timer (run-at-time 120 3600 #'lispm-desktop-update--tick))))

(provide 'lispm-desktop-update)
;;; lispm-desktop-update.el ends here
