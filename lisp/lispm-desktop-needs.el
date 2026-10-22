;;; lispm-desktop-needs.el --- What the desktop and its applications need from outside Emacs  -*- lexical-binding: t; -*-

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

;; Applications use things Emacs does not have: programs run from PATH,
;; as Yeetube plays videos in mpv; libraries, as a terminal's libvterm;
;; and applications of macOS, as a Spotify client's Spotify.  Each is a
;; need, a list of its kind, its name and what is known of it:
;;
;;   (program "mpv" :for "playing videos" :brew "mpv"
;;            :commands (yeetube-play yeetube-replay))
;;   (library "libvterm" :for "drawing terminals" :brew "libvterm"
;;            :file "lib/libvterm.dylib")
;;   (application "Spotify" :for "playing Spotify" :cask "spotify"
;;                :bundle "com.spotify.client")
;;
;; :for       what it is needed for, said wherever it is missing
;; :brew      the Homebrew formula installing it; :cask, the cask
;; :head      t to install the formula's HEAD, built from its latest
;;            sources, where its release is too old, as TDLib's is for
;;            Telega
;; :xcode     t for a program of Apple's command line tools, as git or
;;            swift: their names on PATH are Apple's stand-ins until the
;;            tools are installed, by Apple's own window
;; :instead   programs doing its work while it is missing, as macOS's
;;            afplay plays music without mpv; :more, what it adds to them
;; :commands  commands needing it: run while it is missing, they offer
;;            to install it first, then run, see `lispm-desktop-needs-guard'
;; :variables options holding it, which a package set as it loaded, as
;;            Yeetube's `yeetube-mpv-program' is nil where mpv was not:
;;            once it is there, they are set again from their definitions
;; :file      a library's file, under its formula's prefix, or absolute
;; :bundle    an application's bundle identifier
;;
;; What LispM gets itself, not Homebrew, as lispm-kernel's program, is a
;; need made in code, never in a catalog, with two more:
;;
;; :install   a function the Installer calls with REPORT and DONE: it
;;            calls REPORT with a line saying how far it is and, when it
;;            knows, the fraction done, and DONE once, with nil when it
;;            went well, else why not, a sentence.  It may return a
;;            function stopping it.
;; :present   a function saying whether the need is there, in place of
;;            looking for it by its name
;;
;; A need is `present', `instead' while missing but a stand-in is there,
;; or `missing' (`lispm-desktop-needs-state').  Nothing is installed
;; unasked: `lispm-desktop-needs-ensure' asks in one alert whether to
;; install what an application lacks, then installs it;
;; `lispm-desktop-needs-install' installs for a caller that asked
;; already, as the App Store's Get does.
;;
;; Installs wait in one queue and run one at a time, as Homebrew locks
;; what it installs and two of them at once trip over each other's
;; locks.  They run in the Installer, a window of its own, as Mac OS 9's
;; Installer did: what is installed, what installs now, the last line
;; Homebrew wrote, a Stop button, and a Retry button for what failed,
;; with the whole log a click away.  Without Homebrew, the Installer
;; offers to install it first, in a terminal, where its installer asks
;; for the password; Apple's command line tools are installed by Apple's
;; window, which the Installer waits for.
;;
;; What Homebrew installed is uninstalled the same way, in the same
;; queue, once an application needing it is uninstalled and nothing else
;; needs it: `lispm-desktop-needs-uninstall'.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'svg)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-app-window)
(require 'lispm-desktop-i18n)
(require 'lispm-desktop-catalog)

(declare-function lispm-desktop-popup-alert "lispm-desktop-popup" (icon message buttons &optional detail))
(declare-function lispm-desktop-app-ui-define "lispm-desktop-app-ui" (key &rest properties))
(declare-function lispm-desktop-app-ui-menus "lispm-desktop-app-ui" (app))
(declare-function lispm-desktop-terminal-make "lispm-desktop-term" (name program &rest args))
(declare-function lispm-desktop-terminal-live-p "lispm-desktop-term" (buffer))
(declare-function lispm-desktop-sidebar-show-in-main "lispm-desktop-sidebar" (buffer))

(defgroup lispm-desktop-needs nil
  "What the desktop and its applications need from outside Emacs."
  :group 'lispm-desktop)

(defcustom lispm-desktop-needs-homebrew-installer
  "/bin/bash -c \"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\""
  "The shell command installing Homebrew, as brew.sh gives it."
  :type 'string
  :group 'lispm-desktop-needs)

(defconst lispm-desktop-needs-kinds lispm-desktop-catalog-need-kinds
  "The kinds of need, as a catalog names them.")

(defconst lispm-desktop-needs-brew-places '("/opt/homebrew/bin/brew" "/usr/local/bin/brew")
  "Where Homebrew puts brew, on Apple silicon and on Intel Macs.")

(defconst lispm-desktop-needs-lock-regexp
  "already locked\\|Another active Homebrew\\|Please wait for it to finish"
  "What Homebrew says when another brew holds a lock it needs.")

(defconst lispm-desktop-needs-lock-tries 20
  "How many times an install waits for another brew before it fails.")

(defconst lispm-desktop-needs-lock-wait 15
  "Seconds an install waits for another brew before trying again.")

(defconst lispm-desktop-needs-poll 2
  "Seconds between looks for what another program is installing: Homebrew
in its terminal, Apple's command line tools in Apple's window.")

(defconst lispm-desktop-needs-poll-limit 3600
  "Seconds the Installer waits for another program to install a need.")

(defconst lispm-desktop-needs-log-buffer-name "*Installer Log*"
  "Name of the buffer holding what the installs wrote.")

(defconst lispm-desktop-needs-buffer-name " *Installer*"
  "Name of the Installer window's buffer.")

;;;; Needs

(defun lispm-desktop-needs-kind (need)
  "Return the kind of NEED."
  (car need))

(defun lispm-desktop-needs-name (need)
  "Return the name of NEED."
  (cadr need))

(defun lispm-desktop-needs-get (need property)
  "Return PROPERTY of NEED."
  (plist-get (cddr need) property))

(defun lispm-desktop-needs-key (need)
  "Return what tells NEED from another: its kind and name."
  (cons (lispm-desktop-needs-kind need) (lispm-desktop-needs-name need)))

(defun lispm-desktop-needs-valid-p (need)
  "Return non-nil when NEED is well formed, as a catalog may give it, see
`lispm-desktop-catalog-need-problems'."
  (null (lispm-desktop-catalog-errors (lispm-desktop-catalog-need-problems need))))

(defun lispm-desktop-needs-list-valid-p (needs)
  "Return non-nil when NEEDS is a list of well-formed needs."
  (null (lispm-desktop-catalog-errors (lispm-desktop-catalog-needs-problems needs))))

;;;; Where things are

(defun lispm-desktop-needs-brew ()
  "Return the brew program, or nil when Homebrew is not installed.
Found where Homebrew puts it, its directory goes on PATH: Emacs started
from the Dock may not have it there."
  (let ((brew (or (executable-find "brew")
                  (seq-find #'file-executable-p lispm-desktop-needs-brew-places))))
    (when brew
      (let ((directory (file-name-directory brew)))
        (unless (member (directory-file-name directory) (mapcar #'directory-file-name exec-path))
          (push directory exec-path)
          (setenv "PATH" (concat directory path-separator (getenv "PATH"))))))
    brew))

(defun lispm-desktop-needs--brew-prefix ()
  "Return Homebrew's prefix, the directory above its bin, or nil."
  (when-let* ((brew (lispm-desktop-needs-brew)))
    (file-name-directory (directory-file-name (file-name-directory brew)))))

(defvar lispm-desktop-needs--command-line-tools nil
  "Non-nil once Apple's command line tools were found installed.")

(defun lispm-desktop-needs-command-line-tools-p ()
  "Return non-nil when Apple's command line tools are installed.
Their programs' names on PATH, as git, are found without them: they are
Apple's, offering to install the tools."
  (or lispm-desktop-needs--command-line-tools
      (setq lispm-desktop-needs--command-line-tools
            (and (eq system-type 'darwin)
                 (eq 0 (ignore-errors (call-process "xcode-select" nil nil nil "-p")))))))

(defvar lispm-desktop-needs--applications (make-hash-table :test #'equal)
  "Applications found, by need key: (TIME . FOUND).")

(defun lispm-desktop-needs--application-p (need)
  "Return non-nil when the application of NEED is installed: NAME.app in
an Applications folder, or found by its :bundle with Spotlight.  Looks are
kept a while, as the App Store asks often."
  (let* ((key (lispm-desktop-needs-key need))
         (seen (gethash key lispm-desktop-needs--applications)))
    (if (and seen (< (- (float-time) (car seen)) 10))
        (cdr seen)
      (let ((found (or (seq-some (lambda (directory)
                                   (file-directory-p (expand-file-name
                                                      (concat (lispm-desktop-needs-name need) ".app")
                                                      directory)))
                                 '("/Applications" "~/Applications" "/System/Applications"))
                       (when-let* ((bundle (lispm-desktop-needs-get need :bundle))
                                   ((executable-find "mdfind")))
                         (not (string-blank-p
                               (with-output-to-string
                                 (ignore-errors
                                   (call-process "mdfind" nil standard-output nil
                                                 (format "kMDItemCFBundleIdentifier == '%s'" bundle))))))))))
        (puthash key (cons (float-time) found) lispm-desktop-needs--applications)
        found))))

(defun lispm-desktop-needs--library-p (need)
  "Return non-nil when the library of NEED is installed: its :file, under
its formula's prefix when relative, or else its formula's keg."
  (let ((file (lispm-desktop-needs-get need :file))
        (formula (lispm-desktop-needs-get need :brew)))
    (cond ((and file (file-name-absolute-p file)) (file-exists-p file))
          ((and formula (lispm-desktop-needs--brew-prefix))
           (file-exists-p (expand-file-name (or file "")
                                            (expand-file-name formula (expand-file-name
                                                                       "opt" (lispm-desktop-needs--brew-prefix)))))))))

(defun lispm-desktop-needs-present-p (need)
  "Return non-nil when NEED is installed here."
  (pcase (lispm-desktop-needs-kind need)
    ((guard (lispm-desktop-needs-get need :present))
     (funcall (lispm-desktop-needs-get need :present)))
    ('program (and (executable-find (lispm-desktop-needs-name need))
                   (or (not (lispm-desktop-needs-get need :xcode))
                       (lispm-desktop-needs-command-line-tools-p))))
    ('library (lispm-desktop-needs--library-p need))
    ('application (lispm-desktop-needs--application-p need))))

(defun lispm-desktop-needs-refresh (needs)
  "Set again the :variables of those of NEEDS present that are nil: a
package set them from what it found as it loaded, before they were."
  (dolist (need needs)
    (when-let* ((variables (seq-filter (lambda (variable) (and (boundp variable) (null (symbol-value variable))))
                                       (lispm-desktop-needs-get need :variables)))
                ((lispm-desktop-needs-present-p need)))
      (dolist (variable variables)
        (if (custom-variable-p variable)
            (custom-reevaluate-setting variable)
          (set-default variable (lispm-desktop-needs-name need)))))))

(defun lispm-desktop-needs-stand-in (need)
  "Return the program standing in for NEED, one of its :instead found, or nil."
  (seq-find #'executable-find (lispm-desktop-needs-get need :instead)))

(defun lispm-desktop-needs-state (need)
  "Return how NEED stands: `present', `instead' while missing but a
stand-in is there, or `missing'."
  (cond ((lispm-desktop-needs-present-p need) 'present)
        ((lispm-desktop-needs-stand-in need) 'instead)
        (t 'missing)))

(defun lispm-desktop-needs-missing (needs &optional stand-ins)
  "Return those of NEEDS that are missing; with STAND-INS, also those a
stand-in does the work of."
  (seq-filter (lambda (need) (memq (lispm-desktop-needs-state need) (if stand-ins '(missing instead) '(missing))))
              needs))

;;;; How a need installs

(defun lispm-desktop-needs-provider (need)
  "Return how NEED installs: (lisp FUNCTION), (brew FORMULA), (cask CASK),
(xcode), or nil."
  (cond ((lispm-desktop-needs-get need :install) (list 'lisp (lispm-desktop-needs-get need :install)))
        ((lispm-desktop-needs-get need :brew) (list 'brew (lispm-desktop-needs-get need :brew)))
        ((lispm-desktop-needs-get need :cask) (list 'cask (lispm-desktop-needs-get need :cask)))
        ((lispm-desktop-needs-get need :xcode) (list 'xcode))))

(defun lispm-desktop-needs-installable-p (need)
  "Return non-nil when the Installer can install NEED: LispM's own
installs and Homebrew run on macOS and Linux, Apple's command line tools
only on macOS."
  (pcase (lispm-desktop-needs-provider need)
    ('nil nil)
    ('(xcode) (eq system-type 'darwin))
    (_ t)))

(defun lispm-desktop-needs-install-command (need)
  "Return the command installing NEED, as a shell would read it, or nil."
  (pcase (lispm-desktop-needs-provider need)
    (`(brew ,formula) (format "brew install %s%s" (if (lispm-desktop-needs-get need :head) "--HEAD " "") formula))
    (`(cask ,cask) (format "brew install --cask %s" cask))
    ('(xcode) "xcode-select --install")))

(defun lispm-desktop-needs-brewed-p (need)
  "Return non-nil when Homebrew installed NEED: its formula's keg or its
cask is there, whoever had Homebrew install it.  Only such a need is
uninstalled: Apple's command line tools never are."
  (when-let* ((prefix (lispm-desktop-needs--brew-prefix)))
    (pcase (lispm-desktop-needs-provider need)
      (`(brew ,formula)
       (file-directory-p (expand-file-name (file-name-nondirectory formula) (expand-file-name "Cellar" prefix))))
      (`(cask ,cask)
       (file-directory-p (expand-file-name (file-name-nondirectory cask) (expand-file-name "Caskroom" prefix)))))))

(defun lispm-desktop-needs-text (who need)
  "Return what is said of NEED of WHO, a name, or of it when WHO is nil, as
it stands, in the language LispM speaks."
  (let ((name (lispm-desktop-needs-name need))
        (for (lispm-desktop-needs-get need :for)))
    (pcase (lispm-desktop-needs-state need)
      ('present (if who
                    (lispm-desktop-tr "%s uses %s for %s." who name for)
                  (lispm-desktop-tr "It uses %s for %s." name for)))
      ('instead (let ((stand-in (lispm-desktop-needs-stand-in need))
                      (more (lispm-desktop-needs-get need :more)))
                  (cond ((and who more)
                         (lispm-desktop-tr "%s uses %s for %s; %s would add %s." who stand-in for name more))
                        (who (lispm-desktop-tr "%s uses %s for %s; %s would add more." who stand-in for name))
                        (more (lispm-desktop-tr "It uses %s for %s; %s would add %s." stand-in for name more))
                        (t (lispm-desktop-tr "It uses %s for %s; %s would add more." stand-in for name)))))
      (_ (if who
             (lispm-desktop-tr "%s needs %s for %s." who name for)
           (lispm-desktop-tr "It needs %s for %s." name for))))))

(defun lispm-desktop-needs--names (needs)
  "Return the names of NEEDS as a sentence lists them, in the language
LispM speaks."
  (let ((names (mapcar #'lispm-desktop-needs-name needs)))
    (pcase (length names)
      (1 (car names))
      (2 (lispm-desktop-tr "%s and %s" (car names) (cadr names)))
      (_ (lispm-desktop-tr "%s and %s" (string-join (butlast names) ", ") (car (last names)))))))

(defun lispm-desktop-needs-detail (needs &optional also)
  "Return sentences saying what NEEDS are for and what installs them, in
the language LispM speaks.  ALSO non-nil says they are installed too,
besides what the caller installs."
  (let ((listed (mapconcat (lambda (need)
                             (lispm-desktop-tr "%s, for %s" (lispm-desktop-needs-name need)
                                               (lispm-desktop-needs-get need :for)))
                           needs "; "))
        (brewed (seq-some (lambda (need) (memq (car (lispm-desktop-needs-provider need)) '(brew cask)))
                          needs)))
    (concat (if also
                (lispm-desktop-tr "It also installs %s." listed)
              (lispm-desktop-tr "%s." listed))
            (cond ((and brewed (not (lispm-desktop-needs-brew)))
                   (concat "  " (lispm-desktop-tr-n
                                 "Homebrew installs it, and is installed first: its installer asks for your password."
                                 "Homebrew installs them, and is installed first: its installer asks for your password."
                                 (length needs))))
                  (brewed
                   (concat "  " (lispm-desktop-tr-n "Homebrew installs it." "Homebrew installs them."
                                                    (length needs))))))))

;;;; Asking

(defun lispm-desktop-needs-ask (needs who &optional then)
  "Ask in one alert whether to install NEEDS, which WHO, a name, uses;
install them if so.  THEN, if given, is called with non-nil once all of
NEEDS are present, at once when they are, or with nil when they are not
installed.  NEEDS nothing installs are said, with how to install them."
  (let ((needs (seq-remove #'lispm-desktop-needs-present-p needs)))
    (cond
     ((null needs) (when then (funcall then t)))
     ((not (seq-every-p #'lispm-desktop-needs-installable-p needs))
      (let ((commands (delq nil (mapcar #'lispm-desktop-needs-install-command needs))))
        (when (eq 'copy (lispm-desktop-popup-alert
                         'caution (lispm-desktop-tr "%s needs %s." who (lispm-desktop-needs--names needs))
                         (if commands
                             `((,(lispm-desktop-tr "Copy Command") copy) (,(lispm-desktop-tr "OK") ok default))
                           `((,(lispm-desktop-tr "OK") ok default)))
                         (concat (lispm-desktop-needs-detail needs)
                                 (when commands
                                   (concat "  " (lispm-desktop-tr "Install with: %s"
                                                                  (string-join commands " && ")))))))
          (kill-new (string-join commands " && "))
          (message (lispm-desktop-tr "Copied: %s") (string-join commands " && "))))
      (when then (funcall then nil)))
     ((eq 'install (lispm-desktop-popup-alert
                    'caution (lispm-desktop-tr-n "%s needs %s.  Install it?" "%s needs %s.  Install them?"
                                                 (length needs) who (lispm-desktop-needs--names needs))
                    `((,(lispm-desktop-tr "Not Now") cancel cancel) (,(lispm-desktop-tr "Install") install default))
                    (lispm-desktop-needs-detail needs)))
      (lispm-desktop-needs-install needs who then))
     (then (funcall then nil)))))

(defun lispm-desktop-needs-ensure (needs who &optional then)
  "Make sure WHO, a name, has its NEEDS: ask whether to install those
missing with nothing standing in for them, see `lispm-desktop-needs-ask'.
THEN is called as there."
  (lispm-desktop-needs-ask (lispm-desktop-needs-missing needs) who then))

;;;; Commands needing a need

(defvar lispm-desktop-needs--guards nil
  "The commands guarded: (COMMAND WHO . NEEDS).")

(defun lispm-desktop-needs-guard (command who needs)
  "Make COMMAND, which WHO, a name, has, need NEEDS: called interactively
while one is missing with nothing standing in for it, it offers to
install them first, then runs once they are."
  (setf (alist-get command lispm-desktop-needs--guards) (cons who needs))
  (advice-remove command 'lispm-desktop-needs-guard)
  (advice-add command :around
              (lambda (function &rest arguments)
                (let ((missing (and (called-interactively-p 'any)
                                    (lispm-desktop-needs-missing (cddr (assq command lispm-desktop-needs--guards))))))
                  (if (not missing)
                      (progn (lispm-desktop-needs-refresh (cddr (assq command lispm-desktop-needs--guards)))
                             (apply function arguments))
                    (let ((window (selected-window))
                          (buffer (current-buffer)))
                      (lispm-desktop-needs-ask
                       missing (cadr (assq command lispm-desktop-needs--guards))
                       ;; Run as it was called, past its guard.
                       (lambda (present)
                         (when (and present (buffer-live-p buffer))
                           (if (window-live-p window)
                               (with-selected-window window
                                 (with-current-buffer buffer
                                   (apply #'funcall-interactively function arguments)))
                             (with-current-buffer buffer
                               (apply #'funcall-interactively function arguments))))))))))
              '((name . lispm-desktop-needs-guard))))

(defun lispm-desktop-needs-unguard (command)
  "Let COMMAND run without its needs again."
  (advice-remove command 'lispm-desktop-needs-guard)
  (setq lispm-desktop-needs--guards (assq-delete-all command lispm-desktop-needs--guards)))

(defun lispm-desktop-needs-guard-all (needs who)
  "Guard the :commands of each of NEEDS, which WHO, a name, has."
  (let (commands)
    (dolist (need needs)
      (dolist (command (lispm-desktop-needs-get need :commands))
        (push need (alist-get command commands))))
    (pcase-dolist (`(,command . ,needs) commands)
      (lispm-desktop-needs-guard command who (nreverse needs)))))

(defun lispm-desktop-needs-unguard-all (needs)
  "Let the :commands of each of NEEDS run without them again."
  (dolist (need needs)
    (mapc #'lispm-desktop-needs-unguard (lispm-desktop-needs-get need :commands))))

(defun lispm-desktop-needs-guarded-p (need &optional except)
  "Return non-nil when a command guarded needs NEED, one not of EXCEPT, a
list of commands: something still uses it."
  (seq-some (pcase-lambda (`(,command ,_ . ,needs))
              (and (not (memq command except))
                   (seq-find (lambda (other) (equal (lispm-desktop-needs-key other) (lispm-desktop-needs-key need)))
                             needs)))
            lispm-desktop-needs--guards))

;;;; Installing

(cl-defstruct (lispm-desktop-needs--job (:constructor lispm-desktop-needs--make-job)
                                         (:copier nil))
  "An install or uninstall the Installer runs or has run: ACTION is
`install' or `uninstall'.  STATE is `waiting', `installing' while either
runs, then `installed' or `removed', `failed' or `cancelled'."
  need who (action 'install) (state 'waiting) note error (tries 0) since
  ;; How much of an install of LispM's own is done, from 0 to 1, when it
  ;; says, and what stops it.
  fraction stop)

(defvar lispm-desktop-needs--jobs nil
  "The installs of the Installer's run, in order.")

(defvar lispm-desktop-needs--requests nil
  "What waits for installs to end: (KEYS . CALLBACK).")

(defvar lispm-desktop-needs--process nil
  "The install running, a process, or nil.")

(defvar lispm-desktop-needs--timer nil
  "The Installer's timer: its next look, try, or frame of its bar.")

(defvar lispm-desktop-needs--homebrew nil
  "Where installing Homebrew stands: nil, or (TERMINAL . SINCE) while its
installer runs, or `declined' when the user said not to install it.")

(defvar lispm-desktop-needs--frame 0
  "The frame of the progress bar's stripes, as they move.")

(defun lispm-desktop-needs--active-p (job)
  "Return non-nil while JOB waits or runs."
  (memq (lispm-desktop-needs--job-state job) '(waiting installing)))

(defun lispm-desktop-needs--done-p (job)
  "Return non-nil when JOB did what it was for: installed, or uninstalled."
  (memq (lispm-desktop-needs--job-state job) '(installed removed)))

(defun lispm-desktop-needs--uninstall-p (job)
  "Return non-nil when JOB uninstalls rather than installs."
  (eq (lispm-desktop-needs--job-action job) 'uninstall))

(defun lispm-desktop-needs-job-state (need)
  "Return where the Installer is with NEED, the same kind and name: nil
when its run has nothing to do with it, else (STATE FRACTION ERROR),
STATE `waiting', `installing', `installed', `removed', `failed' or
`cancelled', FRACTION how much is done when known, ERROR why it failed."
  (when-let* ((job (seq-find (lambda (job) (equal (lispm-desktop-needs-key (lispm-desktop-needs--job-need job))
                                                  (lispm-desktop-needs-key need)))
                             lispm-desktop-needs--jobs)))
    (list (lispm-desktop-needs--job-state job) (lispm-desktop-needs--job-fraction job)
          (lispm-desktop-needs--job-error job))))

(defun lispm-desktop-needs-running-p ()
  "Return non-nil while the Installer has installs or uninstalls to do."
  (seq-some #'lispm-desktop-needs--active-p lispm-desktop-needs--jobs))

(defun lispm-desktop-needs--queue (needs who action then)
  "Queue ACTION, `install' or `uninstall', of each of NEEDS, of WHO, a
name, in the Installer, and start; THEN is called as
`lispm-desktop-needs-install' says.  A need that cannot be done fails at
once; one already waiting or running the same way is left as it is."
  (let ((keys (mapcar #'lispm-desktop-needs-key needs)))
    (unless (lispm-desktop-needs-running-p)
      ;; A new run: what the last one did is put away.
      (setq lispm-desktop-needs--jobs nil
            lispm-desktop-needs--homebrew (and (consp lispm-desktop-needs--homebrew)
                                               lispm-desktop-needs--homebrew)))
    (dolist (need needs)
      (unless (seq-find (lambda (job) (and (lispm-desktop-needs--active-p job)
                                           (eq (lispm-desktop-needs--job-action job) action)
                                           (equal (lispm-desktop-needs-key (lispm-desktop-needs--job-need job))
                                                  (lispm-desktop-needs-key need))))
                        lispm-desktop-needs--jobs)
        (let ((possible (or (eq action 'uninstall) (lispm-desktop-needs-installable-p need))))
          (setq lispm-desktop-needs--jobs
                (append (seq-remove (lambda (job) (and (not (lispm-desktop-needs--active-p job))
                                                       (equal (lispm-desktop-needs-key (lispm-desktop-needs--job-need job))
                                                              (lispm-desktop-needs-key need))))
                                    lispm-desktop-needs--jobs)
                        (list (lispm-desktop-needs--make-job
                               :need need :who who :action action
                               :state (if possible 'waiting 'failed)
                               :error (unless possible (lispm-desktop-tr "Nothing here installs it.")))))))))
    (if (null needs)
        (when then (funcall then t))
      (when then (push (cons keys then) lispm-desktop-needs--requests))
      ;; What nothing installs has failed already.
      (lispm-desktop-needs--answer)
      (lispm-desktop-needs-show)
      (lispm-desktop-needs--next))))

(defun lispm-desktop-needs-install (needs who &optional then)
  "Install NEEDS, which WHO, a name, uses, unasked: the caller asked.
Those present or installing already are left; the others wait their turn
in the Installer.  THEN, if given, is called with non-nil once all of
NEEDS are present, or with nil once one could not be installed."
  (lispm-desktop-needs--queue (seq-remove #'lispm-desktop-needs-present-p needs) who 'install then))

(defun lispm-desktop-needs-uninstall (needs who &optional then)
  "Uninstall NEEDS, which WHO, a name, used, with Homebrew, unasked: the
caller asked, and made sure nothing else needs them.  Only those Homebrew
installed are, see `lispm-desktop-needs-brewed-p'; they wait their turn
in the Installer, as installs do.  THEN, if given, is called with non-nil
once all of them are gone, or with nil once one could not be."
  (lispm-desktop-needs--queue (seq-filter #'lispm-desktop-needs-brewed-p needs) who 'uninstall then))

(defun lispm-desktop-needs--next ()
  "Start the next install or uninstall waiting, unless one runs; else end
the run."
  (unless (or (process-live-p lispm-desktop-needs--process)
              (seq-find (lambda (job) (eq (lispm-desktop-needs--job-state job) 'installing))
                        lispm-desktop-needs--jobs))
    (if-let* ((job (seq-find (lambda (job) (eq (lispm-desktop-needs--job-state job) 'waiting))
                             lispm-desktop-needs--jobs)))
        (cond ((lispm-desktop-needs--uninstall-p job)
               (if (lispm-desktop-needs-brewed-p (lispm-desktop-needs--job-need job))
                   (lispm-desktop-needs--brew-run job)
                 (lispm-desktop-needs--end job 'removed)))
              ((lispm-desktop-needs-present-p (lispm-desktop-needs--job-need job))
               (lispm-desktop-needs--end job 'installed))
              (t (pcase (lispm-desktop-needs-provider (lispm-desktop-needs--job-need job))
                   (`(lisp ,function) (lispm-desktop-needs--lisp-run job function))
                   (`(,(or 'brew 'cask) . ,_) (lispm-desktop-needs--brew-run job))
                   ('(xcode) (lispm-desktop-needs--xcode-install job)))))
      (lispm-desktop-needs--finish)))
  (lispm-desktop-needs--render))

(defun lispm-desktop-needs--end (job state &optional error)
  "End JOB in STATE, `installed', `removed', `failed' or `cancelled', with
ERROR."
  (setf (lispm-desktop-needs--job-state job) state
        (lispm-desktop-needs--job-error job) error
        (lispm-desktop-needs--job-note job) nil
        (lispm-desktop-needs--job-fraction job) nil
        (lispm-desktop-needs--job-stop job) nil)
  (clrhash lispm-desktop-needs--applications)
  (when (eq state 'installed)
    (lispm-desktop-needs-refresh (list (lispm-desktop-needs--job-need job))))
  (lispm-desktop-needs--answer)
  (run-at-time 0 nil #'lispm-desktop-needs--next))

(defun lispm-desktop-needs--answer ()
  "Call back what waited for installs that have all ended."
  (let ((ended (lambda (key)
                 (not (seq-find (lambda (job)
                                  (and (lispm-desktop-needs--active-p job)
                                       (equal (lispm-desktop-needs-key (lispm-desktop-needs--job-need job)) key)))
                                lispm-desktop-needs--jobs))))
        (done (lambda (key)
                (seq-find (lambda (job)
                            (and (lispm-desktop-needs--done-p job)
                                 (equal (lispm-desktop-needs-key (lispm-desktop-needs--job-need job)) key)))
                          lispm-desktop-needs--jobs))))
    (dolist (request lispm-desktop-needs--requests)
      (when (seq-every-p ended (car request))
        (setq lispm-desktop-needs--requests (delq request lispm-desktop-needs--requests))
        (funcall (cdr request) (seq-every-p done (car request)))))))

(defun lispm-desktop-needs--finish ()
  "End the run: say so, and close the Installer after a while when all
went well, as nothing is left to read."
  (when (timerp lispm-desktop-needs--timer)
    (cancel-timer lispm-desktop-needs--timer))
  (setq lispm-desktop-needs--timer nil)
  (when (and lispm-desktop-needs--jobs
             (seq-every-p #'lispm-desktop-needs--done-p lispm-desktop-needs--jobs))
    (message "%s" (lispm-desktop-needs--status))
    (run-at-time 4 nil (lambda ()
                         (unless (or (lispm-desktop-needs-running-p)
                                     (seq-remove #'lispm-desktop-needs--done-p lispm-desktop-needs--jobs))
                           (lispm-desktop-app-window-close 'installer))))))

;;;;; Homebrew

(defun lispm-desktop-needs--log (text)
  "Add TEXT to the Installer's log."
  (with-current-buffer (get-buffer-create lispm-desktop-needs-log-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert text))))

(defun lispm-desktop-needs--brew-run (job)
  "Install or uninstall JOB's need with Homebrew; to install, Homebrew
first if need be."
  (let ((brew (lispm-desktop-needs-brew)))
    (cond
     (brew
      (pcase-let* ((need (lispm-desktop-needs--job-need job))
                   (`(,provider ,name) (lispm-desktop-needs-provider need))
                   (uninstall (lispm-desktop-needs--uninstall-p job))
                   (head (and (not uninstall) (eq provider 'brew) (lispm-desktop-needs-get need :head)))
                   (arguments `(,(if uninstall "uninstall" "install") ,(if (eq provider 'cask) "--cask" "--formula")
                                ,@(and head '("--HEAD")) ,name))
                   (process-environment (append '("HOMEBREW_NO_AUTO_UPDATE=1" "HOMEBREW_NO_ENV_HINTS=1"
                                                  "NONINTERACTIVE=1")
                                                process-environment))
                   (start (with-current-buffer (get-buffer-create lispm-desktop-needs-log-buffer-name)
                            (point-max))))
        (setf (lispm-desktop-needs--job-state job) 'installing
              (lispm-desktop-needs--job-since job) (float-time)
              (lispm-desktop-needs--job-note job) (if uninstall
                                                      (lispm-desktop-tr "Uninstalling with Homebrew…")
                                                    (lispm-desktop-tr "Installing with Homebrew…")))
        (lispm-desktop-needs--log (format "\n$ brew %s\n" (string-join arguments " ")))
        (setq lispm-desktop-needs--process
              (make-process
               :name "lispm-desktop-needs" :noquery t :connection-type 'pipe
               :command (cons brew arguments)
               :filter (lambda (_process output)
                         (lispm-desktop-needs--log output)
                         (when-let* ((line (car (last (split-string output "\n" t "[ \t\r]+")))))
                           (setf (lispm-desktop-needs--job-note job) line))
                         (lispm-desktop-needs--render))
               :sentinel (lambda (process _event)
                           (unless (process-live-p process)
                             (lispm-desktop-needs--brew-ended job process start)))))
        (lispm-desktop-needs--tick)))
     ;; Without Homebrew, nothing it installed is left to uninstall.
     ((lispm-desktop-needs--uninstall-p job)
      (lispm-desktop-needs--end job 'removed))
     ((eq lispm-desktop-needs--homebrew 'declined)
      (lispm-desktop-needs--end job 'failed (lispm-desktop-tr "Homebrew is not installed.")))
     ((consp lispm-desktop-needs--homebrew)
      (setf (lispm-desktop-needs--job-note job)
            (lispm-desktop-tr "Waiting for Homebrew's installer in the terminal…")))
     (t (lispm-desktop-needs--offer-homebrew job)))))

(defun lispm-desktop-needs--brew-ended (job process start)
  "End JOB, whose PROCESS installed or uninstalled it, its log since START."
  (setq lispm-desktop-needs--process nil)
  (let* ((log (with-current-buffer (get-buffer-create lispm-desktop-needs-log-buffer-name)
                (buffer-substring-no-properties (min start (point-max)) (point-max))))
         (need (lispm-desktop-needs--job-need job)))
    (cond
     ((eq (lispm-desktop-needs--job-state job) 'cancelled))
     ((and (eq (process-exit-status process) 0) (lispm-desktop-needs--uninstall-p job))
      (lispm-desktop-needs--end job 'removed))
     ((and (eq (process-exit-status process) 0) (lispm-desktop-needs-present-p need))
      (lispm-desktop-needs--end job 'installed))
     ((eq (process-exit-status process) 0)
      (lispm-desktop-needs--end job 'failed (lispm-desktop-tr "Homebrew installed it, but %s is not to be found."
                                                              (lispm-desktop-needs-name need))))
     ((and (string-match-p lispm-desktop-needs-lock-regexp log)
           (< (lispm-desktop-needs--job-tries job) lispm-desktop-needs-lock-tries))
      (cl-incf (lispm-desktop-needs--job-tries job))
      (setf (lispm-desktop-needs--job-state job) 'installing
            (lispm-desktop-needs--job-note job) (lispm-desktop-tr "Waiting for another Homebrew to finish…"))
      (run-at-time lispm-desktop-needs-lock-wait nil
                   (lambda ()
                     (when (eq (lispm-desktop-needs--job-state job) 'installing)
                       (setf (lispm-desktop-needs--job-state job) 'waiting)
                       (lispm-desktop-needs--next)))))
     (t (lispm-desktop-needs--end job 'failed (lispm-desktop-needs--error log))))))

(defun lispm-desktop-needs--error (log)
  "Return what went wrong, as LOG, an install's or uninstall's output,
says it."
  (let ((lines (split-string log "\n" t "[ \t\r]+")))
    (or (car (last (seq-filter (lambda (line) (string-prefix-p "Error:" line)) lines)))
        (car (last lines))
        (lispm-desktop-tr "Homebrew failed."))))

(defun lispm-desktop-needs--offer-homebrew (job)
  "Ask whether to install Homebrew for JOB; install it in a terminal if so."
  (if (eq 'install (lispm-desktop-popup-alert
                    'caution (lispm-desktop-tr "Install Homebrew first?")
                    `((,(lispm-desktop-tr "Cancel") cancel cancel) (,(lispm-desktop-tr "Install Homebrew") install default))
                    (lispm-desktop-tr "%s comes from Homebrew, the package manager most Mac programs come from.  Its installer runs in a terminal, where it asks for your password."
                                      (lispm-desktop-needs-name (lispm-desktop-needs--job-need job)))))
      (let ((terminal (lispm-desktop-terminal-make "Homebrew Installer" "/bin/bash" "-c"
                                                   lispm-desktop-needs-homebrew-installer)))
        (lispm-desktop-sidebar-show-in-main terminal)
        (setq lispm-desktop-needs--homebrew (cons terminal (float-time)))
        (setf (lispm-desktop-needs--job-note job)
              (lispm-desktop-tr "Waiting for Homebrew's installer in the terminal…"))
        (lispm-desktop-needs--tick))
    (setq lispm-desktop-needs--homebrew 'declined)
    (dolist (other lispm-desktop-needs--jobs)
      (when (and (eq (lispm-desktop-needs--job-state other) 'waiting)
                 (memq (car (lispm-desktop-needs-provider (lispm-desktop-needs--job-need other))) '(brew cask)))
        (setf (lispm-desktop-needs--job-state other) 'failed
              (lispm-desktop-needs--job-error other) (lispm-desktop-tr "Homebrew is not installed."))))
    (lispm-desktop-needs--answer)
    (run-at-time 0 nil #'lispm-desktop-needs--next)))

(defun lispm-desktop-needs--look-for-homebrew ()
  "Go on once Homebrew's installer has installed it; fail what needs it
once the installer ended without, or after too long."
  (pcase-let ((`(,terminal . ,since) lispm-desktop-needs--homebrew))
    (cond ((lispm-desktop-needs-brew)
           (setq lispm-desktop-needs--homebrew nil)
           (lispm-desktop-needs--next))
          ((or (not (lispm-desktop-terminal-live-p terminal))
               (> (- (float-time) since) lispm-desktop-needs-poll-limit))
           (setq lispm-desktop-needs--homebrew nil)
           (dolist (job lispm-desktop-needs--jobs)
             (when (and (eq (lispm-desktop-needs--job-state job) 'waiting)
                        (memq (car (lispm-desktop-needs-provider (lispm-desktop-needs--job-need job))) '(brew cask)))
               (setf (lispm-desktop-needs--job-state job) 'failed
                     (lispm-desktop-needs--job-error job)
                     (lispm-desktop-tr "Homebrew's installer ended without installing it."))))
           (lispm-desktop-needs--answer)
           (lispm-desktop-needs--next)))))

;;;;; LispM's own

(defun lispm-desktop-needs--lisp-run (job function)
  "Install JOB's need with FUNCTION, its :install: see the Commentary."
  (let* ((need (lispm-desktop-needs--job-need job))
         (running (lambda () (eq (lispm-desktop-needs--job-state job) 'installing)))
         (report (lambda (note &optional fraction)
                   (when (funcall running)
                     (unless (equal note (lispm-desktop-needs--job-note job))
                       (lispm-desktop-needs--log (concat note "\n")))
                     (setf (lispm-desktop-needs--job-note job) note
                           (lispm-desktop-needs--job-fraction job) fraction)
                     (lispm-desktop-needs--render))))
         (done (lambda (&optional error)
                 (when (funcall running)
                   (cond (error
                          (lispm-desktop-needs--log (concat error "\n"))
                          (lispm-desktop-needs--end job 'failed error))
                         ((lispm-desktop-needs-present-p need) (lispm-desktop-needs--end job 'installed))
                         (t (lispm-desktop-needs--end
                             job 'failed (lispm-desktop-tr "It was installed, but %s is not to be found."
                                                           (lispm-desktop-needs-name need)))))))))
    (setf (lispm-desktop-needs--job-state job) 'installing
          (lispm-desktop-needs--job-since job) (float-time)
          (lispm-desktop-needs--job-note job) (lispm-desktop-tr "Installing…")
          (lispm-desktop-needs--job-fraction job) nil)
    (lispm-desktop-needs--log (format "\n%s\n" (lispm-desktop-needs-name need)))
    (lispm-desktop-needs--tick)
    (condition-case err
        (let ((stop (funcall function report done)))
          ;; It may have ended already.
          (when (and (functionp stop) (funcall running))
            (setf (lispm-desktop-needs--job-stop job) stop)))
      (error (funcall done (error-message-string err))))))

;;;;; Apple's command line tools

(defun lispm-desktop-needs--xcode-install (job)
  "Have Apple's window install its command line tools for JOB, and wait."
  (setf (lispm-desktop-needs--job-state job) 'installing
        (lispm-desktop-needs--job-since job) (float-time)
        (lispm-desktop-needs--job-note job) (lispm-desktop-tr "Finish installing in Apple's window…"))
  (lispm-desktop-needs--log "\n$ xcode-select --install\n")
  (ignore-errors (call-process "xcode-select" nil (get-buffer-create lispm-desktop-needs-log-buffer-name) nil
                               "--install"))
  (lispm-desktop-needs--tick))

(defun lispm-desktop-needs--look-for-xcode (job)
  "End JOB once Apple's command line tools are installed, or after too long."
  (setq lispm-desktop-needs--command-line-tools nil)
  (cond ((lispm-desktop-needs-command-line-tools-p) (lispm-desktop-needs--end job 'installed))
        ((> (- (float-time) (lispm-desktop-needs--job-since job)) lispm-desktop-needs-poll-limit)
         (lispm-desktop-needs--end job 'failed (lispm-desktop-tr "Apple's installer did not finish.")))))

;;;;; The Installer's clock

(defun lispm-desktop-needs--tick ()
  "Move the progress bar, and look for what another program installs."
  (when (timerp lispm-desktop-needs--timer)
    (cancel-timer lispm-desktop-needs--timer))
  (setq lispm-desktop-needs--timer
        (run-at-time 0.25 nil
                     (lambda ()
                       (setq lispm-desktop-needs--timer nil)
                       (when (lispm-desktop-needs-running-p)
                         (cl-incf lispm-desktop-needs--frame)
                         (when (zerop (mod lispm-desktop-needs--frame (* 4 lispm-desktop-needs-poll)))
                           (if (consp lispm-desktop-needs--homebrew)
                               (lispm-desktop-needs--look-for-homebrew)
                             (when-let* ((job (seq-find (lambda (job)
                                                          (and (eq (lispm-desktop-needs--job-state job) 'installing)
                                                               (equal (lispm-desktop-needs-provider
                                                                       (lispm-desktop-needs--job-need job))
                                                                      '(xcode))))
                                                        lispm-desktop-needs--jobs)))
                               (lispm-desktop-needs--look-for-xcode job))))
                         (lispm-desktop-needs--render)
                         (lispm-desktop-needs--tick))))))

;;;; Stopping and trying again

(defun lispm-desktop-needs-stop ()
  "Stop the installs: the one running, and those waiting."
  (interactive)
  (when (process-live-p lispm-desktop-needs--process)
    (delete-process lispm-desktop-needs--process))
  (setq lispm-desktop-needs--process nil
        lispm-desktop-needs--homebrew nil)
  (let ((stops nil))
    (dolist (job lispm-desktop-needs--jobs)
      (when (lispm-desktop-needs--active-p job)
        (when (lispm-desktop-needs--job-stop job)
          (push (lispm-desktop-needs--job-stop job) stops))
        (setf (lispm-desktop-needs--job-state job) 'cancelled
              (lispm-desktop-needs--job-note job) nil
              (lispm-desktop-needs--job-fraction job) nil
              (lispm-desktop-needs--job-stop job) nil
              (lispm-desktop-needs--job-error job) (lispm-desktop-tr "Stopped."))))
    ;; Stopped once they are, what they then say is not heard.
    (mapc (lambda (stop) (ignore-errors (funcall stop))) stops))
  (lispm-desktop-needs--answer)
  (lispm-desktop-needs--next))

(defun lispm-desktop-needs-failed-p ()
  "Return non-nil when an install of the Installer's run failed or stopped."
  (seq-find (lambda (job) (memq (lispm-desktop-needs--job-state job) '(failed cancelled)))
            lispm-desktop-needs--jobs))

(defun lispm-desktop-needs-retry ()
  "Try again what failed or was stopped."
  (interactive)
  (let ((jobs (seq-filter (lambda (job) (and (memq (lispm-desktop-needs--job-state job) '(failed cancelled))
                                             (or (lispm-desktop-needs--uninstall-p job)
                                                 (lispm-desktop-needs-installable-p (lispm-desktop-needs--job-need job)))))
                          lispm-desktop-needs--jobs)))
    (unless jobs
      (user-error (lispm-desktop-tr "Nothing to try again")))
    (when (eq lispm-desktop-needs--homebrew 'declined)
      (setq lispm-desktop-needs--homebrew nil))
    (dolist (job jobs)
      (setf (lispm-desktop-needs--job-state job) 'waiting
            (lispm-desktop-needs--job-error job) nil
            (lispm-desktop-needs--job-tries job) 0))
    (lispm-desktop-needs--next)))

(defun lispm-desktop-needs-show-log ()
  "Show what the installs wrote."
  (interactive)
  (let ((buffer (get-buffer-create lispm-desktop-needs-log-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'special-mode) (special-mode))
      (goto-char (point-max)))
    (pop-to-buffer buffer)))

;;;; The Installer's window

(defvar-keymap lispm-desktop-needs-installer-mode-map
  :doc "Keys of the Installer's window."
  "s" #'lispm-desktop-needs-stop
  "r" #'lispm-desktop-needs-retry
  "l" #'lispm-desktop-needs-show-log
  "g" #'lispm-desktop-needs--render)

(define-derived-mode lispm-desktop-needs-installer-mode special-mode "Installer"
  "The Installer's window: what it installs, and how far it is."
  (setq-local truncate-lines t
              cursor-type nil))

(defun lispm-desktop-needs--buffer ()
  "Return the Installer's buffer, made when missing."
  (or (get-buffer lispm-desktop-needs-buffer-name)
      (with-current-buffer (get-buffer-create lispm-desktop-needs-buffer-name)
        (lispm-desktop-needs-installer-mode)
        (current-buffer))))

(defun lispm-desktop-needs--status ()
  "Return what the Installer's run has come to, a sentence or two in the
language LispM speaks."
  (let* ((jobs lispm-desktop-needs--jobs)
         (installing (seq-find (lambda (job) (eq (lispm-desktop-needs--job-state job) 'installing)) jobs))
         (failed (seq-filter (lambda (job) (memq (lispm-desktop-needs--job-state job) '(failed cancelled))) jobs))
         (done (seq-count (lambda (job) (not (lispm-desktop-needs--active-p job))) jobs))
         (names (lambda (jobs) (lispm-desktop-needs--names (mapcar #'lispm-desktop-needs--job-need jobs))))
         ;; Installs and uninstalls each said in a sentence of their own.
         (say (lambda (jobs install uninstall)
                (let ((removed (seq-filter #'lispm-desktop-needs--uninstall-p jobs))
                      (installed (seq-remove #'lispm-desktop-needs--uninstall-p jobs)))
                  (string-join (delq nil (list (and installed (funcall install installed))
                                               (and removed (funcall uninstall removed))))
                               "  ")))))
    (cond (installing (if (lispm-desktop-needs--uninstall-p installing)
                          (lispm-desktop-tr "Uninstalling %s (%d of %d)…"
                                            (lispm-desktop-needs-name (lispm-desktop-needs--job-need installing))
                                            (1+ done) (length jobs))
                        (lispm-desktop-tr "Installing %s (%d of %d)…"
                                          (lispm-desktop-needs-name (lispm-desktop-needs--job-need installing))
                                          (1+ done) (length jobs))))
          ((lispm-desktop-needs-running-p) (lispm-desktop-tr "Waiting…"))
          (failed (funcall say failed
                           (lambda (jobs) (lispm-desktop-tr-n "%s could not be installed." "%s could not be installed."
                                                              (length jobs) (funcall names jobs)))
                           (lambda (jobs) (lispm-desktop-tr-n "%s could not be uninstalled." "%s could not be uninstalled."
                                                              (length jobs) (funcall names jobs)))))
          (jobs (funcall say jobs
                         (lambda (jobs) (lispm-desktop-tr-n "Installed %s." "Installed %s." (length jobs) (funcall names jobs)))
                         (lambda (jobs) (lispm-desktop-tr-n "Uninstalled %s." "Uninstalled %s." (length jobs)
                                                            (funcall names jobs)))))
          (t (lispm-desktop-tr "Nothing to install.")))))

(defun lispm-desktop-needs--bar (width)
  "Return the Installer's progress bar, WIDTH pixels wide, as Mac OS 9 drew
one: a sunken track filled as far as the installs have come, the part
installing now filled as far as it says it is, else striped and moving."
  (let* ((jobs lispm-desktop-needs--jobs)
         (count (max 1 (length jobs)))
         (part (seq-some (lambda (job) (and (eq (lispm-desktop-needs--job-state job) 'installing)
                                            (lispm-desktop-needs--job-fraction job)))
                         jobs))
         ;; In hundredths, so that the images kept stay few.
         (part (and part (/ (round (* 100 (min 1.0 (max 0.0 part)))) 100.0)))
         (done (+ (seq-count #'lispm-desktop-needs--done-p jobs) (or part 0)))
         (running (and (lispm-desktop-needs-running-p) (not part)))
         (frame (if running (mod lispm-desktop-needs--frame 6) 0)))
    (lispm-desktop-platinum-image
     (list 'lispm-desktop-needs-bar width count done running frame)
     (lambda ()
       (let* ((palette lispm-desktop-platinum-current-palette)
              (height 12)
              (svg (svg-create width height))
              (inner (- width 2))
              (filled (round (* inner (/ done (float count)))))
              (step (round (* inner (/ 1.0 count))))
              (fill (or (plist-get palette :highlight) "#6666CC"))
              (light (or (plist-get palette :thumb-light) "#CCCCFF")))
         (lispm-desktop-platinum-svg-fill svg 0 0 width height (lispm-desktop-platinum-grey palette #x88))
         (lispm-desktop-platinum-svg-fill svg 1 1 inner (- height 2) (lispm-desktop-platinum-grey palette #xEE))
         (lispm-desktop-platinum-svg-fill svg 1 1 filled (- height 2) fill)
         (when running
           (let ((from (+ 1 filled))
                 (to (min (+ 1 inner) (+ 1 filled step))))
             (lispm-desktop-platinum-svg-fill svg from 1 (- to from) (- height 2) light)
             (cl-loop for x from (+ (- from 12) (* 2 frame)) below to by 12
                      do (svg-polygon svg (list (cons (max from x) 1) (cons (max from (min to (+ x 6))) 1)
                                                (cons (max from (min to (+ x 6 (- height 2)))) (1- height))
                                                (cons (max from (min to (+ x (- height 2)))) (1- height)))
                                      :fill fill :stroke "none"))))
         svg)))))

(defun lispm-desktop-needs--render ()
  "Draw the Installer's window again."
  (when-let* ((buffer (get-buffer lispm-desktop-needs-buffer-name)))
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
             (window (get-buffer-window buffer t))
             (width (max 200 (- (if window (window-body-width window t) 420) 40)))
             (columns (max 20 (- (if window (window-body-width window) 60) 8))))
        (erase-buffer)
        (insert "\n")
        (dolist (job lispm-desktop-needs--jobs)
          (let ((need (lispm-desktop-needs--job-need job))
                (fit (lambda (text) (truncate-string-to-width text columns nil nil "…"))))
            (insert "  "
                    (pcase (lispm-desktop-needs--job-state job)
                      ((or 'installed 'removed) "✓") ('installing "▸") ('waiting "·") (_ "✕"))
                    "  "
                    (propertize (lispm-desktop-needs-name need) 'face 'bold)
                    (propertize (concat "  " (funcall fit (lispm-desktop-tr "for %s" (lispm-desktop-needs--job-who job))))
                                'face 'shadow)
                    "\n     "
                    (propertize (funcall fit (lispm-desktop-needs-get need :for)) 'face 'shadow)
                    "\n")
            (when-let* ((said (pcase (lispm-desktop-needs--job-state job)
                                ('installing
                                 (let ((note (lispm-desktop-needs--job-note job))
                                       (fraction (lispm-desktop-needs--job-fraction job)))
                                   (if (and note fraction)
                                       (lispm-desktop-tr "%s %d%%" note (floor (* 100 fraction)))
                                     note)))
                                ((or 'failed 'cancelled) (lispm-desktop-needs--job-error job)))))
              (insert "     " (propertize (funcall fit said) 'face 'italic) "\n"))))
        (insert "\n  ")
        (insert-image (lispm-desktop-needs--bar width) "[progress]")
        (insert "\n\n  " (lispm-desktop-needs--status) "\n")
        (goto-char (point-min))
        ;; The buttons' states and the bottom's placard follow the run.
        (force-mode-line-update t)))))

(defun lispm-desktop-needs-show ()
  "Open the Installer's window, or bring it in front."
  (interactive)
  (lispm-desktop-needs--render)
  (if (or noninteractive (not (display-graphic-p)))
      (message "%s" (lispm-desktop-needs--status))
    (lispm-desktop-app-window-show 'installer :buffer (lispm-desktop-needs--buffer))
    (lispm-desktop-needs--render)))

;;;; Setup

(defun lispm-desktop-setup-needs ()
  "Make the Installer an application with its window and interface."
  (lispm-desktop-app-window-define 'installer
    :app 'installer :name (lispm-desktop-tr-noop "Installer") :size '(480 . 240) :documents 'keep
    :close #'lispm-desktop-app-window-close-box)
  (lispm-desktop-define-app 'installer :name (lispm-desktop-tr-noop "Installer") :icon "software-update"
                            :menus (lambda () (lispm-desktop-app-ui-menus 'installer))
                            :activate #'lispm-desktop-needs-show
                            :visible (lambda () (lispm-desktop-app-window-open-p 'installer)))
  (lispm-desktop-app-ui-define 'lispm-desktop-needs-installer-mode
    :title (lispm-desktop-tr-noop "Installer")
    :cursor nil
    :status (lambda () (lispm-desktop-needs--status))
    :buttons `((,(lispm-desktop-tr-noop "Stop") lispm-desktop-needs-stop lispm-desktop-needs-running-p)
               (,(lispm-desktop-tr-noop "Try Again") lispm-desktop-needs-retry lispm-desktop-needs-failed-p)
               (,(lispm-desktop-tr-noop "Show Log") lispm-desktop-needs-show-log))
    :keys `(("s" . ,(lispm-desktop-tr-noop "Stop the installs")) ("r" . ,(lispm-desktop-tr-noop "Try again what failed"))
            ("l" . ,(lispm-desktop-tr-noop "Show what the installs wrote")))))

(provide 'lispm-desktop-needs)
;;; lispm-desktop-needs.el ends here
