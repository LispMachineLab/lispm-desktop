;;; lispm-desktop-emacs.el --- The Emacs LispM.app runs on, and switching it  -*- lexical-binding: t; -*-

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

;; LispM.app is GNU Emacs, made by install.sh of a build of
;; jimeh/emacs-builds.  install.sh offers Emacs 31, the default, and Emacs
;; 32, still in development, for what only it can do (OpenGL drawn in
;; buffers, see lispm-gl).  The Kernel panel switches LispM.app from one to
;; the other as install.sh does, in two steps:
;;
;;   stage    in the background, LispM running on: install.sh makes
;;            LispM.app of the other Emacs in ~/.lispm/staging, downloaded
;;            and checked, or copied from the one kept in ~/.lispm/backup
;;   restart  LispM quits; install.sh then keeps LispM.app in
;;            ~/.lispm/backup, puts the staged one in its place, compiles
;;            the packages again for its Emacs, and opens it
;;
;; The builds offered are those install.sh of this checkout pins, so that
;; Software Update brings new ones.  Only a LispM.app install.sh made is
;; switched: in an Emacs of your own, the panel says which Emacs it is.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-i18n)
(require 'lispm-desktop-update)

(defvar lispm-desktop-emacs-changed-hook nil
  "Hook run when switching Emacs moves on: a download's progress, the
staged LispM.app ready, or a problem.")

(defvar lispm-desktop-emacs-problem nil
  "Why the last switch could not be staged, a sentence, or nil.")

(defvar lispm-desktop-emacs--process nil
  "install.sh staging LispM.app, while it runs.")

(defvar lispm-desktop-emacs--target nil
  "The major version of Emacs being staged, or staged last.")

(defvar lispm-desktop-emacs--progress nil
  "How much of the download is done, a percentage, or nil.")

(defvar lispm-desktop-emacs--told 0
  "When `lispm-desktop-emacs-changed-hook' last ran for a progress, the
time `float-time' says.")

(defvar lispm-desktop-emacs--switch-on-quit nil
  "Non-nil once Restart to Switch was asked for: install.sh switches as
LispM quits.")

(defconst lispm-desktop-emacs-buffer-name " *LispM Emacs*"
  "Buffer of install.sh's output while it stages LispM.app.")

;;;; The builds install.sh offers

(defun lispm-desktop-emacs--installer ()
  "Return install.sh of the desktop's checkout."
  (expand-file-name "install.sh" lispm-desktop-directory))

(defun lispm-desktop-emacs-builds (&optional installer)
  "Return the builds of Emacs INSTALLER, by default install.sh, offers, by
major version: plists of :major, :version, :release and :build, and
:development, non-nil for an Emacs not released, as 32.0.50."
  (let ((file (or installer (lispm-desktop-emacs--installer)))
        (builds nil))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (while (re-search-forward "^EMACS_\\([0-9]+\\)_\\(RELEASE\\|BUILD\\|VERSION\\)=\\([^ \t\n]+\\)$" nil t)
          (let* ((major (string-to-number (match-string 1)))
                 (key (intern (concat ":" (downcase (match-string 2)))))
                 (entry (or (assq major builds) (car (push (list major) builds)))))
            (setcdr entry (plist-put (cdr entry) key (match-string 3)))))))
    (mapcar (lambda (entry)
              (let ((build (append (list :major (car entry)) (cdr entry))))
                (plist-put build :development
                           (and (string-suffix-p ".50" (or (plist-get build :version) "")) t))))
            (sort (seq-filter (lambda (entry) (plist-get (cdr entry) :build)) builds)
                  (lambda (a b) (< (car a) (car b)))))))

(defun lispm-desktop-emacs-build (major &optional builds)
  "Return the build of Emacs MAJOR among BUILDS, by default those offered."
  (seq-find (lambda (build) (eql (plist-get build :major) major))
            (or builds (lispm-desktop-emacs-builds))))

(defun lispm-desktop-emacs-name (major &optional version)
  "Return the name of Emacs MAJOR, whose version is VERSION, for people."
  (let ((version (or version (plist-get (lispm-desktop-emacs-build major) :version))))
    (if (and version (string-suffix-p ".50" version))
        (lispm-desktop-tr "GNU Emacs %d, in development" major)
      (lispm-desktop-tr "GNU Emacs %s" (or version major)))))

(defun lispm-desktop-emacs-build-date (build)
  "Return the day BUILD, a plist of `lispm-desktop-emacs-builds', was built
on, as 2026-10-21, or nil."
  (let ((name (or (plist-get build :build) "")))
    (and (string-match "\\`Emacs\\.\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\." name)
         (match-string 1 name))))

;;;; LispM.app, its backup, and what is staged

(defun lispm-desktop-emacs-app ()
  "Return LispM.app, when this Emacs runs in it, else nil."
  (lispm-desktop-update--app))

(defun lispm-desktop-emacs--home ()
  "Return the directory LispM's checkouts are in, ~/.lispm by default."
  (directory-file-name (file-name-directory (directory-file-name lispm-desktop-directory))))

(defun lispm-desktop-emacs--staged-app ()
  "Return where install.sh stages LispM.app."
  (expand-file-name "staging/LispM.app" (lispm-desktop-emacs--home)))

(defun lispm-desktop-emacs--backup-app ()
  "Return where install.sh keeps the LispM.app it replaced."
  (expand-file-name "backup/LispM.app" (lispm-desktop-emacs--home)))

(defun lispm-desktop-emacs-app-build (app)
  "Return the build of Emacs the LispM.app APP was made of, or nil."
  (let ((plist (expand-file-name "Contents/Info.plist" app)))
    (when (file-readable-p plist)
      (with-temp-buffer
        (insert-file-contents plist)
        (and (search-forward "<key>LispMEmacsBuild</key>" nil t)
             (re-search-forward "<string>\\([^<]*\\)</string>" nil t)
             (match-string 1))))))

(defun lispm-desktop-emacs-build-major (build)
  "Return the major version of Emacs BUILD, a LispMEmacsBuild, is of: that
of the build offered it begins with, else the version in its name; a
build of master is of the version master is, the highest offered."
  (when build
    (or (plist-get (seq-find (lambda (offered) (string-prefix-p (plist-get offered :build) build))
                             (lispm-desktop-emacs-builds))
                   :major)
        (and (string-match "\\.emacs-\\([0-9]+\\)-" build)
             (string-to-number (match-string 1 build)))
        (and (string-match-p "\\.master\\." build)
             (plist-get (car (last (lispm-desktop-emacs-builds))) :major)))))

(defun lispm-desktop-emacs--kept (app)
  "Return the LispM.app at APP, when it is there, as a plist of :app,
:build and :major; else nil."
  (when-let* ((build (lispm-desktop-emacs-app-build app)))
    (list :app app :build build :major (lispm-desktop-emacs-build-major build))))

(defun lispm-desktop-emacs-backup ()
  "Return the LispM.app install.sh kept when it switched last, a plist of
:app, :build and :major, or nil."
  (lispm-desktop-emacs--kept (lispm-desktop-emacs--backup-app)))

(defun lispm-desktop-emacs-staged ()
  "Return the LispM.app staged, ready to switch to, a plist of :app,
:build and :major, or nil: none while one is staged, nor one of the Emacs
running."
  (unless (lispm-desktop-emacs-staging-p)
    (when-let* ((staged (lispm-desktop-emacs--kept (lispm-desktop-emacs--staged-app))))
      (and (not (eql (plist-get staged :major) emacs-major-version)) staged))))

(defun lispm-desktop-emacs-staging-p ()
  "Return non-nil while LispM.app is being staged."
  (process-live-p lispm-desktop-emacs--process))

(defun lispm-desktop-emacs-target ()
  "Return the major version of Emacs being staged, or staged and ready."
  (cond ((lispm-desktop-emacs-staging-p) lispm-desktop-emacs--target)
        ((lispm-desktop-emacs-staged) (plist-get (lispm-desktop-emacs-staged) :major))))

(defun lispm-desktop-emacs-progress ()
  "Return how much of the download is done, a percentage, or nil."
  (and (lispm-desktop-emacs-staging-p) lispm-desktop-emacs--progress))

;;;; Staging

(defun lispm-desktop-emacs--changed (&optional progress)
  "Run `lispm-desktop-emacs-changed-hook'; for a PROGRESS, twice a second
at most."
  (if (not progress)
      (run-hooks 'lispm-desktop-emacs-changed-hook)
    (when (>= (- (float-time) lispm-desktop-emacs--told) 0.5)
      (setq lispm-desktop-emacs--told (float-time))
      (run-hooks 'lispm-desktop-emacs-changed-hook))))

(defun lispm-desktop-emacs--filter (process output)
  "Keep OUTPUT of install.sh, PROCESS, and read the download's progress."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert output)))
  ;; curl's progress bar ends each update in a percentage.
  (let ((start 0) (last nil))
    (while (string-match "\\([0-9]+\\(?:\\.[0-9]\\)?\\)%" output start)
      (setq last (string-to-number (match-string 1 output))
            start (match-end 0)))
    (when last
      (setq lispm-desktop-emacs--progress (min 100.0 last))
      (lispm-desktop-emacs--changed t))))

(defun lispm-desktop-emacs--failure (buffer)
  "Return why install.sh failed, as it said in BUFFER, or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (goto-char (point-max))
      (and (re-search-backward "^LispM: \\(.+\\)$" nil t)
           (match-string 1)))))

(defun lispm-desktop-emacs--sentinel (process _event)
  "Say how staging went once install.sh, PROCESS, has ended."
  (unless (process-live-p process)
    (setq lispm-desktop-emacs--process nil
          lispm-desktop-emacs--progress nil)
    (let ((staged (lispm-desktop-emacs-staged)))
      (cond ((eq (process-status process) 'signal)
             (setq lispm-desktop-emacs-problem nil))
            ((and (= (process-exit-status process) 0) staged
                  (eql (plist-get staged :major) lispm-desktop-emacs--target))
             (setq lispm-desktop-emacs-problem nil)
             (message (lispm-desktop-tr "%s is ready: restart LispM to switch to it.")
                      (lispm-desktop-emacs-name lispm-desktop-emacs--target)))
            (t
             (setq lispm-desktop-emacs-problem
                   (or (lispm-desktop-emacs--failure (process-buffer process))
                       (lispm-desktop-tr "install.sh could not make LispM.app")))
             (message (lispm-desktop-tr "%s could not be made ready: %s")
                      (lispm-desktop-emacs-name lispm-desktop-emacs--target)
                      lispm-desktop-emacs-problem))))
    (lispm-desktop-emacs--changed)))

(defun lispm-desktop-emacs--environment (&rest variables)
  "Return the environment install.sh runs in, with VARIABLES set, strings
of NAME=VALUE: LispM's places, and no repository of Emacs's own."
  (append variables
          (list (concat "LISPM_HOME=" (lispm-desktop-emacs--home))
                (concat "LISPM_APP_DIR=" (directory-file-name
                                          (file-name-directory (lispm-desktop-emacs-app)))))
          (lispm-desktop-update--environment)))

(defun lispm-desktop-emacs-stage (major)
  "Make LispM.app of Emacs MAJOR ready to switch to, in the background.
install.sh downloads it, checks it and makes LispM.app of it in
~/.lispm/staging, or copies the one kept in ~/.lispm/backup; LispM runs on.
Once it is ready, `lispm-desktop-emacs-restart' switches to it."
  (interactive
   (list (let ((choices (mapcar (lambda (build)
                                  (cons (lispm-desktop-emacs-name (plist-get build :major)) (plist-get build :major)))
                                (lispm-desktop-emacs-builds))))
           (cdr (assoc (completing-read (lispm-desktop-tr "Switch LispM to: ") choices nil t) choices)))))
  (unless (lispm-desktop-emacs-app)
    (user-error (lispm-desktop-tr "Only LispM.app switches its Emacs; this Emacs is one of your own")))
  (unless (lispm-desktop-emacs-build major)
    (user-error (lispm-desktop-tr "install.sh offers no Emacs %d") major))
  (cond
   ((eql major emacs-major-version)
    (lispm-desktop-emacs-cancel))
   ;; Staged already, or on its way.
   ((eql major (lispm-desktop-emacs-target)))
   (t
    (lispm-desktop-emacs-cancel)
    (let ((buffer (get-buffer-create lispm-desktop-emacs-buffer-name)))
      (with-current-buffer buffer (erase-buffer))
      (setq lispm-desktop-emacs--target major
            lispm-desktop-emacs--progress nil
            lispm-desktop-emacs-problem nil)
      (let ((process-environment (lispm-desktop-emacs--environment
                                  "LISPM_STEP=stage" (format "LISPM_EMACS=%d" major))))
        (setq lispm-desktop-emacs--process
              (make-process :name "lispm-emacs-stage" :buffer buffer :noquery t
                            :command (list "/bin/sh" (lispm-desktop-emacs--installer))
                            :connection-type 'pipe
                            :filter #'lispm-desktop-emacs--filter
                            :sentinel #'lispm-desktop-emacs--sentinel))))
    (lispm-desktop-emacs--changed))))

(defun lispm-desktop-emacs-cancel ()
  "Stop staging LispM.app, and forget what was staged."
  (interactive)
  (when (process-live-p lispm-desktop-emacs--process)
    (delete-process lispm-desktop-emacs--process))
  (setq lispm-desktop-emacs--process nil
        lispm-desktop-emacs--progress nil
        lispm-desktop-emacs-problem nil)
  (let ((staged (lispm-desktop-emacs--staged-app)))
    (when (file-directory-p staged)
      (delete-directory staged t)))
  (lispm-desktop-emacs--changed))

;;;; Switching

(defun lispm-desktop-emacs--switch-command ()
  "Return the shell command switching LispM.app once this Emacs has quit."
  (format (concat "while kill -0 %d 2>/dev/null; do sleep 0.5; done; "
                  "/bin/sh %s >>%s 2>&1")
          (emacs-pid)
          (shell-quote-argument (lispm-desktop-emacs--installer))
          (shell-quote-argument (expand-file-name "switch.log" (lispm-desktop-emacs--home)))))

(defun lispm-desktop-emacs--switch ()
  "Switch LispM.app once this Emacs has quit, when that was asked for; a
batch Emacs never does.  This runs from `kill-emacs-hook'."
  (when (and lispm-desktop-emacs--switch-on-quit (not noninteractive) (lispm-desktop-emacs-staged))
    (let ((process-environment (lispm-desktop-emacs--environment "LISPM_STEP=switch")))
      (call-process "/bin/sh" nil 0 nil "-c"
                    (format "(%s) </dev/null >/dev/null 2>&1 &" (lispm-desktop-emacs--switch-command))))))

(defun lispm-desktop-emacs-restart ()
  "Restart LispM on the Emacs staged: LispM quits, install.sh keeps
LispM.app in ~/.lispm/backup, puts the staged one in its place and opens
it."
  (interactive)
  (unless (lispm-desktop-emacs-staged)
    (user-error (lispm-desktop-tr "No other Emacs is ready to switch to")))
  (setq lispm-desktop-emacs--switch-on-quit t)
  (save-buffers-kill-emacs)
  ;; Still here: quitting was called off; the switch waits for Restart.
  (setq lispm-desktop-emacs--switch-on-quit nil))

(defun lispm-desktop-setup-emacs ()
  "Switch LispM.app as LispM quits, when Restart to Switch asked for it."
  (add-hook 'kill-emacs-hook #'lispm-desktop-emacs--switch 80))

(provide 'lispm-desktop-emacs)
;;; lispm-desktop-emacs.el ends here
