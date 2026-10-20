;;; lispm-desktop-store.el --- The App Store: applications, agents and skills  -*- lexical-binding: t; -*-

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

;; The App Store installs what LispM runs beyond what it ships with:
;;
;;   Applications  Emacs packages, each in a window of its own on the
;;                 desktop, see lispm-desktop-app-window.el
;;   Agents        LispM's roles, the kinds of agent it starts
;;   Skills        what LispM's agents know how to do with Emacs
;;
;; It lists them from catalogs: git repositories holding a catalog.eld,
;; LispMachineLab/lispm-store by default (`lispm-desktop-store-catalogs').
;; Each is cloned under `lispm-desktop-store-directory' and its catalog
;; read as data, never evaluated:
;;
;;   (:name "LispM Store"
;;    :items
;;    ((elfeed :kind app :name "Elfeed" :description "Read web feeds."
;;             :icon "elfeed" :command elfeed
;;             :package (elfeed :url "https://github.com/skeeto/elfeed"
;;                              :rev "<commit>"))
;;     (calculator :kind app :name "Calculator" :command calc)
;;     (reviewer :kind role :name "Reviewer" :file "roles/reviewer.org")
;;     (feeds :kind skill :name "Feeds" :file "skills/feeds.org"
;;            :requires (elfeed))))
;;
;; An application's :package is absent for one built into Emacs, the name
;; of a package of the package archives, or a spec as `package-vc-install'
;; takes one, pinned to the commit :rev.  :command opens the application
;; in its window; or :feature names a package that makes its application
;; itself, with `lispm-desktop-define-app' and the item's name as its key.
;; :icon names icons/NAME.svg and icons/NAME-32.svg of the catalog.  A role
;; or a skill is a file of the catalog, which lispm-agent installs
;; (`lispm-install-role', `lispm-install-skill').  :requires names the
;; items installed before.
;;
;; Nothing is installed unasked: what an item brings, a role's or a
;; skill's text and code, or where a package comes from, is shown under
;; the list while an alert asks, as before an update, with what changes.
;; What was installed is kept with Customize in
;; `lispm-desktop-store-installed', so the applications are back at the
;; next start without a look at the network.
;;
;; The window is laid out as Mac OS 9's Extensions Manager: the items in
;; groups, Applications, Agents and Skills, each checked while installed,
;; with its version and the catalog listing it; push buttons under the
;; list; the selected item's information below, shown or hidden.  The
;; keyboard does all of it: C-n and C-p go from row to row, Space installs
;; or removes, RET opens an application, i shows or hides the information,
;; u updates, g checks the catalogs, + installs from a git URL, Tab goes to
;; the push buttons and q closes.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'package)
(require 'lispm-desktop-core)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-app-window)

(declare-function lispm-desktop-popup-alert "lispm-desktop-popup" (icon message buttons &optional detail))
(declare-function lispm-desktop-deps-ensure "lispm-desktop-deps" (feature))
(declare-function lispm-desktop-finder-open-directory "lispm-desktop-finder" (directory))
(declare-function package-vc-install "package-vc" (package &optional rev backend name))
(declare-function package-vc-rebuild "package-vc" (pkg-desc))
(declare-function lispm-skills "lispm-skills" (&optional agent))
(defvar lispm-role-directories)
(defvar package-vc-selected-packages)
(defvar lispm-desktop-finder-folders)
(defvar evil-emacs-state-cursor)
(defvar evil-normal-state-cursor)

(defgroup lispm-desktop-store nil
  "The App Store: applications, agents and skills to install."
  :group 'lispm-desktop)

(defcustom lispm-desktop-store-catalogs '("https://github.com/LispMachineLab/lispm-store")
  "The catalogs the App Store lists: git repositories holding a catalog.eld.
An item of an earlier catalog hides one of the same name in a later one."
  :type '(repeat string)
  :group 'lispm-desktop-store)

(defcustom lispm-desktop-store-directory (locate-user-emacs-file "lispm-store/")
  "Directory the catalogs are cloned in, a directory each."
  :type 'directory
  :group 'lispm-desktop-store)

(defcustom lispm-desktop-store-installed nil
  "What the App Store installed, in the order installed: (ID . PROPERTIES).
PROPERTIES are the item's, as its catalog listed it, with :catalog, the
catalog's repository; :installed, what is installed: a package's commit
or version, or the hash of a role's or a skill's file; :as, the name of
the role or skill; :commit, the catalog's commit it came from; and
:owned, non-nil when the App Store installed the package of an
application itself, so that it deletes it too."
  :type '(alist :key-type symbol :value-type plist)
  :group 'lispm-desktop-store)

(defconst lispm-desktop-store-kinds
  '((app "Applications" "Application")
    (role "Agents" "Agent")
    (skill "Skills" "Skill"))
  "The kinds of item, in the list's order: (KIND GROUP NAME).")

(defconst lispm-desktop-store-buffer-name " *App Store*"
  "Name of the App Store's window's buffer.")

(defvar-local lispm-desktop-store--closed nil
  "The groups folded up: kinds of `lispm-desktop-store-kinds'.")

(defvar-local lispm-desktop-store--selected nil
  "The row selected: an item's ID, or a group's kind as (group . KIND).")

(defvar-local lispm-desktop-store--information t
  "Non-nil while the selected item's information shows under the list.")

(defvar-local lispm-desktop-store--status-text nil
  "What the App Store is doing, for its placard, or nil.")

(defvar-local lispm-desktop-store--width nil
  "The width in pixels the window was drawn for.")

(defvar-local lispm-desktop-store--face-cookie nil
  "The remapping giving the window its platinum.")

(defconst lispm-desktop-store-margin 12
  "Pixels between the window's edges and the list, the buttons and the information.")

(defconst lispm-desktop-store-columns '(56 80 120)
  "Widths in pixels of the On/Off, Version and Source columns; Name has the rest.")

(defvar lispm-desktop-store-row-map)

;;;; Catalogs

(defvar lispm-desktop-store--catalogs nil
  "The catalogs as read, in `lispm-desktop-store-catalogs' order: plists of
:url, :directory, :name, :commit and :items.")

(defvar lispm-desktop-store--checking nil
  "Non-nil while the catalogs are fetched.")

(defvar lispm-desktop-store--checked nil
  "Non-nil once the catalogs were fetched since Emacs started.")

(defun lispm-desktop-store--catalog-directory (url)
  "Return the directory the catalog at URL is cloned in, named after it."
  (let ((name (replace-regexp-in-string
               "[^A-Za-z0-9._-]+" "-"
               (replace-regexp-in-string "\\`[a-z+]+://[^/]*/" ""
                                         (string-remove-suffix ".git" (directory-file-name url))))))
    (file-name-as-directory (expand-file-name (string-trim name "-+" "-+") lispm-desktop-store-directory))))

(defun lispm-desktop-store--git-environment ()
  "Return the environment git runs in: no repository of Emacs's own, as an
inherited GIT_DIR would name, and no prompt for a password."
  (append '("GIT_DIR" "GIT_WORK_TREE" "GIT_INDEX_FILE" "GIT_TERMINAL_PROMPT=0")
          process-environment))

(defun lispm-desktop-store--git (directory &rest args)
  "Run git with ARGS in DIRECTORY; return what it printed, or signal it."
  (let ((default-directory (file-name-as-directory directory))
        (process-environment (lispm-desktop-store--git-environment)))
    (with-temp-buffer
      (let ((status (apply #'process-file "git" nil t nil args)))
        (unless (eql status 0)
          (error "git %s failed: %s" (car args) (string-trim (buffer-string))))
        (string-trim (buffer-string))))))

(defun lispm-desktop-store--git-steps (steps callback)
  "Run STEPS in the background one after another, each (DIRECTORY . ARGS) of
git; then call CALLBACK with nil, or with what the step that failed said."
  (if (null steps)
      (funcall callback nil)
    (pcase-let* ((`(,directory . ,args) (car steps))
                 (buffer (generate-new-buffer " *lispm-desktop-store git*"))
                 (default-directory (file-name-as-directory directory))
                 (process-environment (lispm-desktop-store--git-environment)))
      (make-process
       :name "lispm-desktop-store-git" :buffer buffer :command (cons "git" args)
       :connection-type 'pipe :noquery t
       :sentinel (lambda (process _event)
                   (unless (process-live-p process)
                     (let ((output (with-current-buffer buffer (string-trim (buffer-string)))))
                       (kill-buffer buffer)
                       (if (eql (process-exit-status process) 0)
                           (lispm-desktop-store--git-steps (cdr steps) callback)
                         (funcall callback (if (string-empty-p output) "git failed" output))))))))))

(defun lispm-desktop-store--fetch-steps (url)
  "Return the git steps bringing the clone of the catalog at URL up to date:
cloned the first time, the newest commit of its default branch after."
  (let ((directory (lispm-desktop-store--catalog-directory url)))
    (if (file-directory-p (expand-file-name ".git" directory))
        `((,directory "fetch" "--quiet" "--depth" "1" "origin" "HEAD")
          (,directory "checkout" "--quiet" "--detach" "FETCH_HEAD"))
      (make-directory lispm-desktop-store-directory t)
      `((,lispm-desktop-store-directory "clone" "--quiet" "--depth" "1" ,url
                                        ,(directory-file-name directory))))))

(defun lispm-desktop-store-check-for-updates (&optional callback)
  "Fetch the catalogs in the background, then list what they hold now.
CALLBACK, when given, is called after."
  (interactive)
  (if lispm-desktop-store--checking
      (message "The App Store is checking its catalogs already")
    (setq lispm-desktop-store--checking t)
    (lispm-desktop-store--status "Checking the catalogs…")
    (let ((urls lispm-desktop-store-catalogs)
          (failed nil))
      (cl-labels ((next ()
                    (if-let* ((url (pop urls)))
                        (lispm-desktop-store--git-steps
                         (lispm-desktop-store--fetch-steps url)
                         (lambda (error)
                           (when error
                             (push url failed)
                             (message "App Store: %s: %s" url error))
                           (next)))
                      (setq lispm-desktop-store--checking nil
                            lispm-desktop-store--checked t)
                      (lispm-desktop-store-read-catalogs)
                      (lispm-desktop-store--status
                       (and failed (format "Could not reach %s" (string-join (nreverse failed) ", "))))
                      (lispm-desktop-store--render)
                      (when callback
                        (funcall callback)))))
        (next)))))

(defun lispm-desktop-store--safe-file-p (file)
  "Return non-nil when FILE names a file inside a catalog: relative, and
never out of it with `..'."
  (and (stringp file) (not (string-empty-p file))
       (not (file-name-absolute-p file)) (not (string-prefix-p "~" file))
       (not (member ".." (split-string file "/")))))

(defun lispm-desktop-store--package-valid-p (package)
  "Return non-nil when PACKAGE is an application's :package as catalogs
give it: nil, a package's name, or a spec pinned to a commit that runs no
command to build it."
  (pcase package
    ('nil t)
    ((pred symbolp) t)
    (`(,(and name (pred symbolp)) . ,(and spec (pred plistp)))
     (and name (stringp (plist-get spec :url)) (stringp (plist-get spec :rev))
          (not (plist-member spec :make)) (not (plist-member spec :shell-command))
          (or (null (plist-get spec :lisp-dir))
              (lispm-desktop-store--safe-file-p (plist-get spec :lisp-dir)))))))

(defun lispm-desktop-store--item-valid-p (item)
  "Return non-nil when ITEM, (ID . PROPERTIES) of a catalog, is well formed."
  (pcase item
    (`(,(and id (pred symbolp)) . ,(and props (pred plistp)))
     (and id (stringp (plist-get props :name))
          (let ((description (plist-get props :description)))
            (or (null description) (stringp description)))
          (let ((requires (plist-get props :requires)))
            (and (listp requires) (seq-every-p #'symbolp requires)))
          (pcase (plist-get props :kind)
            ('app (let ((command (plist-get props :command))
                        (feature (plist-get props :feature))
                        (icon (plist-get props :icon)))
                    (and (lispm-desktop-store--package-valid-p (plist-get props :package))
                         (symbolp command) (symbolp feature) (or command feature)
                         (or (null icon) (lispm-desktop-store--safe-file-p icon)))))
            ((or 'role 'skill) (lispm-desktop-store--safe-file-p (plist-get props :file))))))))

(defun lispm-desktop-store--read-catalog (url)
  "Return the catalog at URL as its clone holds it, or nil without a clone.
A catalog that cannot be read is said so and left out, and so are the
items of one that are not well formed."
  (let* ((directory (lispm-desktop-store--catalog-directory url))
         (file (expand-file-name "catalog.eld" directory)))
    (when (file-readable-p file)
      (condition-case err
          (let ((data (with-temp-buffer
                        (insert-file-contents file)
                        (read (current-buffer)))))
            (unless (and (plistp data) (listp (plist-get data :items)))
              (error "It is no catalog"))
            (list :url url :directory directory
                  :name (let ((name (plist-get data :name))) (if (stringp name) name url))
                  :commit (ignore-errors (lispm-desktop-store--git directory "rev-parse" "HEAD"))
                  :items (seq-filter (lambda (item)
                                       (or (lispm-desktop-store--item-valid-p item)
                                           (progn (message "App Store: %s lists %S wrongly; left out"
                                                           url (car-safe item))
                                                  nil)))
                                     (plist-get data :items))))
        (error (message "App Store: cannot read the catalog of %s: %s" url (error-message-string err))
               nil)))))

(defun lispm-desktop-store-read-catalogs ()
  "Read the catalogs as their clones hold them; return them."
  (setq lispm-desktop-store--catalogs
        (delq nil (mapcar #'lispm-desktop-store--read-catalog lispm-desktop-store-catalogs))))

(defun lispm-desktop-store--catalog (url)
  "Return the catalog read from URL, or nil."
  (seq-find (lambda (catalog) (equal (plist-get catalog :url) url)) lispm-desktop-store--catalogs))

(defun lispm-desktop-store--catalog-name (url)
  "Return the name of the catalog at URL, or of the repository there."
  (cond ((null url) "")
        ((lispm-desktop-store--catalog url) (plist-get (lispm-desktop-store--catalog url) :name))
        (t (file-name-nondirectory (string-remove-suffix ".git" (directory-file-name url))))))

;;;; Items

(defun lispm-desktop-store-items ()
  "Return the items the App Store lists: (ID . PROPERTIES), :catalog the
repository of the catalog listing each.  Installed items no catalog
lists any more come too, as they were installed."
  (let ((items nil))
    (dolist (catalog lispm-desktop-store--catalogs)
      (pcase-dolist (`(,id . ,props) (plist-get catalog :items))
        (unless (assq id items)
          (push `(,id :catalog ,(plist-get catalog :url) ,@props) items))))
    (pcase-dolist (`(,id . ,props) lispm-desktop-store-installed)
      (unless (assq id items)
        (push (cons id props) items)))
    (nreverse items)))

(defun lispm-desktop-store-installed-p (id)
  "Return non-nil when the App Store installed the item ID."
  (and (assq id lispm-desktop-store-installed) t))

(defun lispm-desktop-store--installed (id)
  "Return the properties the item ID was installed with, or nil."
  (cdr (assq id lispm-desktop-store-installed)))

(defun lispm-desktop-store--plist-without (plist &rest keys)
  "Return PLIST without KEYS."
  (let ((result nil))
    (while plist
      (unless (memq (car plist) keys)
        (setq result (nconc result (list (car plist) (cadr plist)))))
      (setq plist (cddr plist)))
    result))

(defun lispm-desktop-store--file (props &optional file)
  "Return FILE, by default the :file of PROPS, in its catalog's clone."
  (expand-file-name (or file (plist-get props :file))
                    (lispm-desktop-store--catalog-directory (plist-get props :catalog))))

(defun lispm-desktop-store--icon (props &optional large)
  "Return the icon of the item of PROPS, its 32-pixel one when LARGE, as
`lispm-desktop-platinum-icon-file' takes it: its catalog's drawing, or
one standing for its kind."
  (or (when-let* ((name (plist-get props :icon))
                  ((plist-get props :catalog))
                  (file (lispm-desktop-store--file props (format "icons/%s%s.svg" name (if large "-32" ""))))
                  ((file-readable-p file)))
        file)
      (pcase (plist-get props :kind)
        ('app "lispm")
        ('role "robot")
        (_ "document-org"))))

(defun lispm-desktop-store--package-name (package)
  "Return the name of PACKAGE, an application's :package."
  (if (consp package) (car package) package))

(defun lispm-desktop-store--package-version (name)
  "Return the version of the package NAME installed, or nil."
  (when-let* ((desc (cadr (assq name package-alist))))
    (package-version-join (package-desc-version desc))))

(defun lispm-desktop-store--short (commit)
  "Return COMMIT shortened, as git shows commits, or nil."
  (and (stringp commit) (substring commit 0 (min 7 (length commit)))))

(defun lispm-desktop-store--version (id props)
  "Return the version of the item ID, of PROPS, for the Version column."
  (let ((installed (lispm-desktop-store--installed id)))
    (or (pcase (plist-get props :kind)
          ('app (let ((package (plist-get props :package)))
                  (cond ((null package) (format "%d.%d" emacs-major-version emacs-minor-version))
                        ((and installed (lispm-desktop-store--package-version
                                         (lispm-desktop-store--package-name package))))
                        ((consp package) (lispm-desktop-store--short (plist-get (cdr package) :rev))))))
          (_ (lispm-desktop-store--short
              (if installed
                  (plist-get installed :commit)
                (plist-get (lispm-desktop-store--catalog (plist-get props :catalog)) :commit)))))
        "–")))

(defun lispm-desktop-store--origin (props)
  "Return where the item of PROPS comes from, in words."
  (pcase (plist-get props :kind)
    ('app (pcase (plist-get props :package)
            ('nil "Emacs")
            ((pred symbolp) "the package archives")
            (`(,_ . ,spec) (plist-get spec :url))))
    (_ (or (plist-get props :catalog) "–"))))

(defun lispm-desktop-store--hash (path)
  "Return the sha256 of PATH: of a file, or of a directory's files and names."
  (if (file-directory-p path)
      (secure-hash 'sha256
                   (mapconcat (lambda (file)
                                (concat (file-relative-name file path) "\0"
                                        (with-temp-buffer
                                          (insert-file-contents-literally file)
                                          (buffer-string))
                                        "\0"))
                              (sort (directory-files-recursively path "") #'string<)
                              ""))
    (with-temp-buffer
      (insert-file-contents-literally path)
      (secure-hash 'sha256 (current-buffer)))))

(defun lispm-desktop-store-update-available-p (id)
  "Return non-nil when the item ID is installed as its catalog no longer
lists it: a package pinned to another commit, or a role or skill changed."
  (when-let* ((installed (lispm-desktop-store--installed id))
              (props (cdr (assq id (lispm-desktop-store-items)))))
    (pcase (plist-get props :kind)
      ('app (let ((package (plist-get props :package)))
              (and (consp package)
                   (not (equal (plist-get (cdr package) :rev) (plist-get installed :installed))))))
      (_ (let ((file (and (plist-get props :catalog) (lispm-desktop-store--file props))))
           (and file (file-exists-p file)
                (not (equal (lispm-desktop-store--hash file) (plist-get installed :installed)))))))))

(defun lispm-desktop-store--requirers (id)
  "Return the names of the installed items requiring the item ID."
  (mapcan (pcase-lambda (`(,_ . ,props))
            (when (memq id (plist-get props :requires))
              (list (plist-get props :name))))
          lispm-desktop-store-installed))

;;;; Installing, removing, updating

(defun lispm-desktop-store--status (text)
  "Say TEXT on the App Store's placard, and in the echo area; nil for none.
The window is drawn at once, as what follows may keep Emacs busy."
  (when-let* ((buffer (get-buffer lispm-desktop-store-buffer-name)))
    (with-current-buffer buffer
      (setq lispm-desktop-store--status-text text)
      (force-mode-line-update))
    (when text
      (redisplay t)))
  (when text
    (message "%s" text)))

(defun lispm-desktop-store--names (items)
  "Return the names of ITEMS, (ID . PROPERTIES), as a phrase."
  (lispm-desktop-platinum--names (mapcar (lambda (item) (plist-get (cdr item) :name)) items)))

(defun lispm-desktop-store--confirm (question detail preview action)
  "Ask QUESTION in an alert, DETAIL under it, and return non-nil to go on.
PREVIEW, a text, shows under the list meanwhile; ACTION names the button
going on."
  (lispm-desktop-store--call-with-preview
   preview
   (lambda ()
     (eq 'go (lispm-desktop-popup-alert 'caution question
                                        `(("Cancel" cancel cancel) (,action go default))
                                        detail)))))

(defconst lispm-desktop-store-preview-buffer-name " *App Store Preview*"
  "Name of the buffer showing what an item brings while an alert asks.")

(defun lispm-desktop-store--call-with-preview (text function)
  "Call FUNCTION with TEXT, when non-nil, shown under the App Store's list
meanwhile, in a pane of the window, or in a window of its own."
  (if (null text)
      (funcall function)
    (let ((buffer (get-buffer-create lispm-desktop-store-preview-buffer-name)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert text)
          (goto-char (point-min)))
        (unless (derived-mode-p 'org-mode)
          (delay-mode-hooks (org-mode)))
        (setq buffer-read-only t
              truncate-lines nil
              word-wrap t)
        (setq-local lispm-desktop-platinum-title "What It Brings"))
      (unwind-protect
          (if-let* ((frame (lispm-desktop-app-window-frame 'store)))
              (let ((window (split-window (frame-root-window frame) nil 'below)))
                (set-window-buffer window buffer)
                (redisplay t)
                (unwind-protect (funcall function)
                  (when (window-live-p window)
                    (delete-window window))))
            (save-window-excursion
              (pop-to-buffer buffer)
              (redisplay t)
              (funcall function)))
        (kill-buffer buffer)))))

(defun lispm-desktop-store--read-file (file)
  "Return the text of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun lispm-desktop-store--file-text (path)
  "Return the text of PATH, a file, or of every file of PATH, a directory."
  (if (file-directory-p path)
      (mapconcat (lambda (file)
                   (format "** %s\n\n%s\n" (file-relative-name file path) (lispm-desktop-store--read-file file)))
                 (sort (directory-files-recursively path "") #'string<)
                 "\n")
    (lispm-desktop-store--read-file path)))

(defun lispm-desktop-store--preview (item)
  "Return what installing ITEM brings, as text to read first."
  (pcase-let ((`(,_ . ,props) item))
    (concat
     (format "* %s, %s\n\n" (plist-get props :name)
             (downcase (nth 2 (assq (plist-get props :kind) lispm-desktop-store-kinds))))
     (pcase (plist-get props :kind)
       ('app (pcase (plist-get props :package)
               ('nil "Built into Emacs.\n")
               ((and package (pred symbolp))
                (format "The package %s, from the package archives (%s).\n"
                        package (mapconcat #'car package-archives ", ")))
               (`(,name . ,spec)
                (format "The package %s, from %s at commit %s.\n"
                        name (plist-get spec :url) (plist-get spec :rev)))))
       (_ (lispm-desktop-store--file-text (lispm-desktop-store--file props)))))))

(defun lispm-desktop-store--built-in-p (props)
  "Return non-nil when PROPS describe an application built into Emacs."
  (and (eq (plist-get props :kind) 'app) (null (plist-get props :package))))

(defun lispm-desktop-store--confirm-install (items)
  "Show what ITEMS bring and ask whether to install them; non-nil if so.
Applications built into Emacs are only made applications: nothing asks."
  (or (seq-every-p (lambda (item) (lispm-desktop-store--built-in-p (cdr item))) items)
      (lispm-desktop-store--confirm
       (format "Install %s?" (lispm-desktop-store--names items))
       "Its code will run in Emacs as your own does; what it brings is shown under the list."
       (mapconcat #'lispm-desktop-store--preview items "\n")
       "Install")))

(defun lispm-desktop-store--to-install (id)
  "Return the items installing the item ID brings, those it requires first
and those installed left out."
  (let ((items (lispm-desktop-store-items))
        (order nil)
        (visiting nil))
    (cl-labels ((visit (id from)
                  (unless (or (assq id order) (lispm-desktop-store-installed-p id))
                    (when (memq id visiting)
                      (user-error "The App Store's items %s require each other" id))
                    (push id visiting)
                    (let ((item (or (assq id items)
                                    (user-error "%s requires %s, which no catalog lists" from id))))
                      (dolist (required (plist-get (cdr item) :requires))
                        (visit required id))
                      (push item order)))))
      (visit id nil))
    (nreverse order)))

(defun lispm-desktop-store--agent-ready (function)
  "Make sure lispm-agent's FUNCTION can be called, LispM loaded first;
signal that it cannot when this LispM has no such function."
  (unless (fboundp function)
    (when (fboundp 'lispm-desktop-deps-ensure)
      (lispm-desktop-deps-ensure 'lispm))
    (require 'lispm nil t))
  (unless (fboundp function)
    (user-error "This LispM installs no agents or skills: update lispm-agent")))

(defun lispm-desktop-store--agent-call (function &rest args)
  "Call lispm-agent's FUNCTION with ARGS, LispM loaded first."
  (lispm-desktop-store--agent-ready function)
  (apply function args))

(defun lispm-desktop-store--keep (id props &rest installed)
  "Keep that the item ID, of PROPS, is installed as INSTALLED, a plist, says."
  (customize-save-variable
   'lispm-desktop-store-installed
   (append (assq-delete-all id (copy-alist lispm-desktop-store-installed))
           (list (cons id (append installed (lispm-desktop-store--plist-without
                                             props :installed :as :owned :commit)))))))

(defun lispm-desktop-store--forget (id)
  "Forget that the item ID is installed."
  (customize-save-variable 'lispm-desktop-store-installed
                           (assq-delete-all id (copy-alist lispm-desktop-store-installed))))

(defun lispm-desktop-store--install-package (package)
  "Install PACKAGE, an application's :package; return what is installed:
the commit of a package from git, the version of one from the archives,
or \"built-in\".  A package installed already is left as it is."
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
         (package-vc-install (cons name (lispm-desktop-store--plist-without spec :rev)) rev))
       rev))))

(defmacro lispm-desktop-store--quietly (&rest body)
  "Run BODY showing none of the buffers it would show, as the log of the
package's compilation, which would split the App Store's window."
  (declare (indent 0))
  `(save-window-excursion
     (let ((display-buffer-overriding-action '(display-buffer-no-window (allow-no-window . t))))
       ,@body)))

(defun lispm-desktop-store--install-1 (item)
  "Install ITEM, unasked, and keep what was installed."
  (pcase-let* ((`(,id . ,props) item)
               (kind (plist-get props :kind)))
    (lispm-desktop-store--status (format "Installing %s…" (plist-get props :name)))
    (if (eq kind 'app)
        (let* ((package (plist-get props :package))
               (owned (and package
                           (not (package-installed-p (lispm-desktop-store--package-name package)))))
               (installed (lispm-desktop-store--quietly
                            (lispm-desktop-store--install-package package))))
          (lispm-desktop-store--keep id props :installed installed :owned owned)
          (lispm-desktop-store--register-app id (lispm-desktop-store--installed id)))
      (let* ((file (lispm-desktop-store--file props))
             (as (lispm-desktop-store--agent-call
                  (if (eq kind 'role) 'lispm-install-role 'lispm-install-skill) file t)))
        (lispm-desktop-store--keep id props :installed (lispm-desktop-store--hash file) :as as
                                   :commit (plist-get (lispm-desktop-store--catalog
                                                       (plist-get props :catalog))
                                                      :commit))))))

(defun lispm-desktop-store-install (id)
  "Install the item ID, and the items it requires first, once confirmed."
  (interactive (list (lispm-desktop-store--read-item
                      "Install: " (lambda (id) (not (lispm-desktop-store-installed-p id))))))
  (let ((items (lispm-desktop-store--to-install id)))
    ;; Nothing is asked about what this LispM could not install.
    (dolist (item items)
      (pcase (plist-get (cdr item) :kind)
        ('role (lispm-desktop-store--agent-ready 'lispm-install-role))
        ('skill (lispm-desktop-store--agent-ready 'lispm-install-skill))))
    (when (and items (lispm-desktop-store--confirm-install items))
      (unwind-protect
          (dolist (item items)
            (lispm-desktop-store--install-1 item))
        (lispm-desktop-store--status nil)
        (lispm-desktop-store--render)))))

(defun lispm-desktop-store--delete-package (package)
  "Delete PACKAGE, an application's :package, as package.el deletes one.
One that other packages need stays, as said in the echo area."
  (let ((name (lispm-desktop-store--package-name package)))
    (when-let* ((desc (cadr (assq name package-alist))))
      (condition-case err
          (package-delete desc)
        (error (message "App Store: %s stays: %s" name (error-message-string err)))))
    (when (and (consp package) (boundp 'package-vc-selected-packages)
               (assq name package-vc-selected-packages))
      (customize-save-variable 'package-vc-selected-packages
                               (assq-delete-all name (copy-alist package-vc-selected-packages))))))

(defun lispm-desktop-store--remove-detail (id props)
  "Return what removing the item ID, of PROPS, does, in words."
  (let ((requirers (lispm-desktop-store--requirers id)))
    (concat
     (pcase (plist-get props :kind)
       ('app (cond ((null (plist-get props :package))
                    "It stays in Emacs; only its window and its place among the applications go.")
                   ((plist-get props :owned)
                    (format "Its package %s is deleted."
                            (lispm-desktop-store--package-name (plist-get props :package))))
                   (t (format "Its package %s stays, as it was there before."
                              (lispm-desktop-store--package-name (plist-get props :package))))))
       ('role "Agents started from it keep going; no new one starts from it.")
       (_ "Agents no longer find it."))
     (when requirers
       (format "  %s %s it." (lispm-desktop-platinum--names requirers)
               (if (cdr requirers) "need" "needs"))))))

(defun lispm-desktop-store--remove-1 (id props)
  "Remove the item ID, of PROPS, unasked, and forget it."
  (pcase (plist-get props :kind)
    ('app (lispm-desktop-app-window-close id)
          (lispm-desktop-remove-app id)
          (when (plist-get props :owned)
            (lispm-desktop-store--quietly
              (lispm-desktop-store--delete-package (plist-get props :package)))))
    ('role (lispm-desktop-store--agent-call 'lispm-delete-role (plist-get props :as)))
    (_ (lispm-desktop-store--agent-call 'lispm-delete-skill (plist-get props :as))))
  (lispm-desktop-store--forget id))

(defun lispm-desktop-store-remove (id)
  "Remove the item ID the App Store installed, once confirmed."
  (interactive (list (lispm-desktop-store--read-item "Remove: " #'lispm-desktop-store-installed-p)))
  (let ((props (or (lispm-desktop-store--installed id)
                   (user-error "The App Store did not install %s" id))))
    (when (lispm-desktop-store--confirm (format "Remove “%s”?" (plist-get props :name))
                                        (lispm-desktop-store--remove-detail id props)
                                        nil "Remove")
      (lispm-desktop-store--remove-1 id props)
      (lispm-desktop-store--render))))

(defun lispm-desktop-store--installed-path (kind as)
  "Return where lispm-agent keeps the role or skill AS of KIND, or nil."
  (pcase kind
    ('role (and (boundp 'lispm-role-directories)
                (seq-some (lambda (directory)
                            (let ((file (expand-file-name (format "%s.org" as) directory)))
                              (and (file-exists-p file) file)))
                          lispm-role-directories)))
    ('skill (when-let* (((fboundp 'lispm-skills))
                        (skill (seq-find (lambda (skill) (equal (plist-get skill :name) as))
                                         (lispm-skills))))
              (let ((file (plist-get skill :file)))
                (if (equal (file-name-nondirectory file) "SKILL.org")
                    (file-name-directory file)
                  file))))))

(defun lispm-desktop-store--diff (old new)
  "Return the differences from OLD to NEW, files or directories, unified;
two files are named as installed and as the catalog has it."
  (with-temp-buffer
    (apply #'call-process "diff" nil t nil "-ru"
           `(,@(unless (or (file-directory-p old) (file-directory-p new))
                 '("--label" "installed" "--label" "catalog"))
             ,(expand-file-name old) ,(expand-file-name new)))
    (buffer-string)))

(defun lispm-desktop-store--changes (installed props)
  "Return what updating the item installed as INSTALLED to PROPS changes."
  (concat
   (format "* %s\n\n" (plist-get props :name))
   (pcase (plist-get props :kind)
     ('app (pcase-let ((`(,name . ,spec) (plist-get props :package)))
             (format "The package %s goes from commit %s to commit %s, of %s.\n"
                     name (plist-get installed :installed) (plist-get spec :rev) (plist-get spec :url))))
     (kind (let ((old (lispm-desktop-store--installed-path kind (plist-get installed :as)))
                 (new (lispm-desktop-store--file props)))
             (if old
                 (lispm-desktop-store--diff old new)
               (lispm-desktop-store--file-text new)))))))

(defun lispm-desktop-store--repin (name rev)
  "Check the package NAME, installed from git, out at commit REV, and build
it again, as `package-vc-upgrade' would build it."
  (let* ((desc (or (cadr (assq name package-alist)) (user-error "%s is not installed" name)))
         (directory (package-desc-dir desc))
         (present (lambda ()
                    (ignore-errors
                      (lispm-desktop-store--git directory "cat-file" "-e" (concat rev "^{commit}"))
                      t))))
    (unless (funcall present)
      (lispm-desktop-store--git directory "fetch" "--quiet" "origin")
      (unless (funcall present)
        (lispm-desktop-store--git directory "fetch" "--quiet" "origin" rev)))
    (lispm-desktop-store--git directory "checkout" "--quiet" "--detach" rev)
    (require 'package-vc)
    (package-vc-rebuild desc)))

(defun lispm-desktop-store-update (id)
  "Bring the item ID to what its catalog lists now, once confirmed."
  (interactive (list (lispm-desktop-store--read-item "Update: " #'lispm-desktop-store-update-available-p)))
  (let* ((installed (or (lispm-desktop-store--installed id)
                        (user-error "The App Store did not install %s" id)))
         (props (cdr (assq id (lispm-desktop-store-items)))))
    (unless (lispm-desktop-store-update-available-p id)
      (user-error "%s is up to date" (plist-get installed :name)))
    (when (lispm-desktop-store--confirm (format "Update “%s”?" (plist-get props :name))
                                        "What changes is shown under the list."
                                        (lispm-desktop-store--changes installed props)
                                        "Update")
      (lispm-desktop-store--status (format "Updating %s…" (plist-get props :name)))
      (unwind-protect
          (pcase (plist-get props :kind)
            ('app (let* ((package (plist-get props :package))
                         (rev (plist-get (cdr package) :rev)))
                    (lispm-desktop-store--quietly
                      (lispm-desktop-store--repin (car package) rev))
                    (lispm-desktop-store--keep id props :installed rev :owned (plist-get installed :owned))
                    (lispm-desktop-store--register-app id (lispm-desktop-store--installed id))))
            (kind (let* ((file (lispm-desktop-store--file props))
                         (as (lispm-desktop-store--agent-call
                              (if (eq kind 'role) 'lispm-install-role 'lispm-install-skill) file t)))
                    (lispm-desktop-store--keep id props :installed (lispm-desktop-store--hash file) :as as
                                               :commit (plist-get (lispm-desktop-store--catalog
                                                                   (plist-get props :catalog))
                                                                  :commit)))))
        (lispm-desktop-store--status nil)
        (lispm-desktop-store--render)))))

;;;; Installing from a git URL

(defun lispm-desktop-store--main-file (repository name)
  "Return the main Lisp file of the package REPOSITORY holds, named after
NAME, its repository's name: NAME.el or the file NAME names without an
Emacs prefix or suffix, at the top or in lisp/, else the one file there
with a Version header.  Return nil when there is none."
  (let* ((stem (replace-regexp-in-string "\\`emacs-\\|\\(?:[.-]el\\|-mode\\)\\'" "" name))
         (files (append (directory-files repository t "\\.el\\'")
                        (let ((lisp (expand-file-name "lisp" repository)))
                          (and (file-directory-p lisp) (directory-files lisp t "\\.el\\'"))))))
    (or (seq-find (lambda (file) (member (file-name-base file) (list name stem))) files)
        (let ((headed (seq-filter (lambda (file)
                                    (with-temp-buffer
                                      (insert-file-contents file nil 0 4096)
                                      (re-search-forward "^;+ *\\(?:Package-\\)?Version:" nil t)))
                                  files)))
          (and (not (cdr headed)) (car headed))))))

(defun lispm-desktop-store--autoloaded-commands (file)
  "Return the names of the functions FILE autoloads, as strings."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((names nil))
      (while (re-search-forward "^;;;###autoload\n(\\(?:cl-\\)?defun\\*? +\\([^ \t\n()]+\\)" nil t)
        (push (match-string 1) names))
      (nreverse names))))

(defun lispm-desktop-store--add-catalog (url)
  "Add the catalog at URL to the App Store's, once confirmed."
  (when (member url lispm-desktop-store-catalogs)
    (user-error "The App Store lists %s already" url))
  (when (lispm-desktop-store--confirm (format "Add the catalog at %s?" url)
                                      "Its items show in the App Store; nothing is installed unasked."
                                      nil "Add")
    (customize-save-variable 'lispm-desktop-store-catalogs (append lispm-desktop-store-catalogs (list url)))
    (lispm-desktop-store-check-for-updates)))

(defun lispm-desktop-store--install-repository (url repository)
  "Install the Emacs package REPOSITORY, a clone of URL, as an application.
Its command opening it is read; it is pinned to the clone's commit."
  (let* ((repo-name (file-name-base (string-remove-suffix ".git" (directory-file-name url))))
         (main (or (lispm-desktop-store--main-file repository repo-name)
                   (user-error "%s holds no Emacs package or catalog" url)))
         (name (intern (file-name-base main)))
         (lisp-dir (let ((directory (file-relative-name (file-name-directory main) repository)))
                     (unless (equal directory "./") (directory-file-name directory))))
         (rev (lispm-desktop-store--git repository "rev-parse" "HEAD"))
         (command (completing-read (format "Command opening %s: " name)
                                   (lispm-desktop-store--autoloaded-commands main)))
         (props `(:kind app :name ,(capitalize (symbol-name name))
                  :description ,(format "Installed from %s." url)
                  :command ,(intern command)
                  :package (,name :url ,url :rev ,rev ,@(and lisp-dir (list :lisp-dir lisp-dir))))))
    (when (string-blank-p command)
      (user-error "No command opens %s" name))
    (when (lispm-desktop-store-installed-p name)
      (user-error "%s is installed already" name))
    (when (lispm-desktop-store--confirm-install (list (cons name props)))
      (unwind-protect
          (lispm-desktop-store--install-1 (cons name props))
        (lispm-desktop-store--status nil)
        (lispm-desktop-store--render)))))

(defun lispm-desktop-store-install-from-url (url)
  "Install from the git repository at URL, once confirmed.
A repository holding a catalog.eld is a catalog, which joins the App
Store's; any other is an Emacs package, installed as an application
pinned to its newest commit."
  (interactive (list (read-string "Install from git URL: ")))
  (when (string-blank-p url)
    (user-error "No URL"))
  (let ((directory (make-temp-file "lispm-desktop-store-" t)))
    (unwind-protect
        (progn
          (lispm-desktop-store--status (format "Looking at %s…" url))
          (lispm-desktop-store--git directory "clone" "--quiet" "--depth" "1" url "repository")
          (let ((repository (expand-file-name "repository" directory)))
            (lispm-desktop-store--status nil)
            (if (file-exists-p (expand-file-name "catalog.eld" repository))
                (lispm-desktop-store--add-catalog url)
              (lispm-desktop-store--install-repository url repository))))
      (lispm-desktop-store--status nil)
      (delete-directory directory t))))

;;;; Applications

(defun lispm-desktop-store-open (id)
  "Open the installed application ID, in its window."
  (interactive (list (lispm-desktop-store--read-item
                      "Open: " (lambda (id) (eq (plist-get (lispm-desktop-store--installed id) :kind) 'app)))))
  (let ((props (lispm-desktop-store--installed id)))
    (unless (eq (plist-get props :kind) 'app)
      (user-error "%s is no application installed" id))
    (if (plist-get props :feature)
        (lispm-desktop-apps-activate id)
      (lispm-desktop-app-window-show
       id :name (plist-get props :name)
       :setup (lambda () (call-interactively (plist-get props :command)))))))

(defun lispm-desktop-store--register-app (id props)
  "Make the installed application ID, of PROPS, one of the desktop's."
  (if-let* ((feature (plist-get props :feature)))
      (require feature nil t)
    (let ((menu (intern (format "lispm-desktop-store-%s-file" id))))
      (lispm-desktop-define-app
       id :name (plist-get props :name) :icon (lispm-desktop-store--icon props)
       :menus (lambda () `((,menu "File" ,(lambda () (lispm-desktop-app-window-file-items id)))))
       :activate (lambda () (lispm-desktop-store-open id))
       :visible (lambda () (lispm-desktop-app-window-open-p id))))))

(defun lispm-desktop-store--register-installed ()
  "Make the applications installed the desktop's, as at every start."
  (pcase-dolist (`(,id . ,props) lispm-desktop-store-installed)
    (when (eq (plist-get props :kind) 'app)
      (condition-case err
          (lispm-desktop-store--register-app id props)
        (error (message "App Store: %s: %s" id (error-message-string err)))))))

(defun lispm-desktop-store-application-items ()
  "Return a menu item opening each application installed, for the λ menu."
  (mapcan (pcase-lambda (`(,id . ,props))
            (when (eq (plist-get props :kind) 'app)
              (list (list :label (plist-get props :name)
                          :action (lambda () (lispm-desktop-apps-activate id))))))
          lispm-desktop-store-installed))

(defun lispm-desktop-store--folder-items ()
  "Return the Applications folder's items: the App Store, then the
applications installed, see `lispm-desktop-finder-folders'."
  (cons (list "App Store" "app-store-32" #'lispm-desktop-store-show)
        (mapcan (pcase-lambda (`(,id . ,props))
                  (when (eq (plist-get props :kind) 'app)
                    (list (list (plist-get props :name) (lispm-desktop-store--icon props t)
                                (lambda () (lispm-desktop-apps-activate id))))))
                lispm-desktop-store-installed)))

(defun lispm-desktop-store-open-applications ()
  "Open the Applications folder in the Finder window."
  (interactive)
  (lispm-desktop-finder-open-directory 'applications))

;;;; The window

(defun lispm-desktop-store-open-p ()
  "Return non-nil while the App Store's window is open."
  (lispm-desktop-app-window-open-p 'store))

(defun lispm-desktop-store--buffer ()
  "Return the App Store's buffer, made when missing."
  (or (get-buffer lispm-desktop-store-buffer-name)
      (with-current-buffer (get-buffer-create lispm-desktop-store-buffer-name)
        (lispm-desktop-store-mode)
        (current-buffer))))

(defun lispm-desktop-store--selected-id ()
  "Return the ID of the item selected in the App Store's window, or nil."
  (when-let* ((buffer (get-buffer lispm-desktop-store-buffer-name)))
    (let ((selected (buffer-local-value 'lispm-desktop-store--selected buffer)))
      (and selected (symbolp selected) selected))))

(defun lispm-desktop-store--read-item (prompt &optional predicate)
  "Return the ID of an item PREDICATE accepts: the one selected in the App
Store's window when that is current, else one read with PROMPT."
  (or (and (derived-mode-p 'lispm-desktop-store-mode)
           (let ((id (lispm-desktop-store--selected-id)))
             (and id (or (null predicate) (funcall predicate id)) id)))
      (let* ((items (seq-filter (lambda (item) (or (null predicate) (funcall predicate (car item))))
                                (lispm-desktop-store-items)))
             (names (mapcar (lambda (item) (cons (plist-get (cdr item) :name) (car item))) items)))
        (unless names
          (user-error "There is nothing to choose from"))
        (cdr (assoc (completing-read prompt names nil t) names)))))

;;;;; Drawing

(defun lispm-desktop-store--geometry (width)
  "Return where the list's columns start in a list WIDTH pixels wide:
\(NAME VERSION SOURCE END), END where the sort button, closing the row of
headings, begins."
  (pcase-let ((`(,on ,version ,source) lispm-desktop-store-columns))
    (let ((end (- width 16)))
      (list on (- end source version) (- end source) end))))

(defun lispm-desktop-store--to (x face)
  "Return a blank reaching X pixels into the list, in FACE.
The list starts past the window's margin and its own outline."
  (propertize " " 'display `(space :align-to (,(+ lispm-desktop-store-margin 1 x))) 'face face))

(defun lispm-desktop-store--outline ()
  "Return the list's outline down one side, a pixel wide."
  (propertize " " 'display '(space :width (1))
              'face `(:background ,(lispm-desktop-platinum--color :outline))))

(defun lispm-desktop-store--margin ()
  "Return the window's margin left of the list, and the list's outline."
  (concat (propertize " " 'display `(space :width (,lispm-desktop-store-margin)))
          (lispm-desktop-store--outline)))

(defun lispm-desktop-store--end (width face)
  "Return the end of a list row WIDTH pixels wide: the rest in FACE, then
the outline; the window's margin is left blank."
  (concat (lispm-desktop-store--to width face)
          (lispm-desktop-store--outline)))

(defun lispm-desktop-store--fit (text width face)
  "Return TEXT in FACE, shortened to WIDTH pixels."
  (lispm-desktop-platinum--fit (propertize text 'face face) (max 0 width)))

(defun lispm-desktop-store--selected-face ()
  "Return the face of the selected row's name: on the light highlight
colour, as Mac OS 9 drew a selected name, in whichever of the theme's text
and background colours reads on it."
  (let ((highlight (or (lispm-desktop-platinum--color :thumb-light)
                       (lispm-desktop-platinum--color :highlight))))
    `(:background ,highlight
      :foreground ,(lispm-desktop-platinum--legible highlight
                                                    (lispm-desktop-platinum--color :text)
                                                    (lispm-desktop-platinum--color :content)))))

(defun lispm-desktop-store--group-state (items)
  "Return the check box of a group of ITEMS: `on' when all are installed,
`off' when none is, else `mixed'."
  (let ((on (seq-count (lambda (item) (lispm-desktop-store-installed-p (car item))) items)))
    (cond ((zerop on) 'off)
          ((= on (length items)) 'on)
          (t 'mixed))))

(defun lispm-desktop-store--row (key line)
  "Return LINE as the list's row of KEY, an item's ID or (group . KIND)."
  (propertize line 'lispm-desktop-store-row key 'keymap lispm-desktop-store-row-map
              'pointer 'arrow))

(defun lispm-desktop-store--group-line (kind group items width)
  "Return the row of the group of KIND, named GROUP, holding ITEMS, in a list
WIDTH pixels wide: a disclosure triangle and the group's check box, its
folder and name, as Extensions Manager drew a folder."
  (pcase-let* ((`(,name-x ,version-x ,source-x ,_) (lispm-desktop-store--geometry width))
               (body 'lispm-desktop-platinum-list-body)
               (sorted '(lispm-desktop-platinum-sorted lispm-desktop-platinum-list))
               (open (not (memq kind lispm-desktop-store--closed)))
               (selected (equal lispm-desktop-store--selected (cons 'group kind))))
    (concat
     (lispm-desktop-store--margin)
     (lispm-desktop-store--to 2 body)
     (propertize (lispm-desktop-platinum-triangle open) 'lispm-desktop-store-triangle kind
                 'help-echo (if open "Fold this group up" "Unfold this group"))
     (lispm-desktop-store--to 22 body)
     (lispm-desktop-platinum-list-check-box (lispm-desktop-store--group-state items))
     (lispm-desktop-store--to name-x body)
     (lispm-desktop-store--to (+ name-x 6) sorted)
     (lispm-desktop-platinum-icon "folder")
     (lispm-desktop-store--to (+ name-x 35) sorted)
     (lispm-desktop-store--fit group (- version-x name-x 39)
                               `(,@(and selected (list (lispm-desktop-store--selected-face)))
                                 :weight bold :inherit ,sorted))
     (lispm-desktop-store--to version-x sorted)
     (lispm-desktop-store--to (+ version-x 6) body)
     (propertize "–" 'face body)
     (lispm-desktop-store--to (+ source-x 6) body)
     (propertize "–" 'face body)
     (lispm-desktop-store--end width body))))

(defun lispm-desktop-store--item-line (id props width)
  "Return the row of the item ID, of PROPS, in a list WIDTH pixels wide: its
check box, on while installed, its icon and name, version and catalog."
  (pcase-let* ((`(,name-x ,version-x ,source-x ,end) (lispm-desktop-store--geometry width))
               (body 'lispm-desktop-platinum-list-body)
               (sorted '(lispm-desktop-platinum-sorted lispm-desktop-platinum-list))
               (selected (eq lispm-desktop-store--selected id))
               (update (lispm-desktop-store-update-available-p id)))
    (concat
     (lispm-desktop-store--margin)
     (lispm-desktop-store--to 36 body)
     (propertize (lispm-desktop-platinum-list-check-box
                  (if (lispm-desktop-store-installed-p id) 'on 'off))
                 'lispm-desktop-store-check id
                 'help-echo (if (lispm-desktop-store-installed-p id) "Remove it" "Install it"))
     (lispm-desktop-store--to name-x body)
     (lispm-desktop-store--to (+ name-x 31) sorted)
     (lispm-desktop-platinum-icon (lispm-desktop-store--icon props))
     (lispm-desktop-store--to (+ name-x 62) sorted)
     (lispm-desktop-store--fit (plist-get props :name) (- version-x name-x 66)
                               (if selected
                                   `(,(lispm-desktop-store--selected-face) ,@sorted)
                                 sorted))
     (lispm-desktop-store--to version-x sorted)
     (lispm-desktop-store--to (+ version-x 6) body)
     (lispm-desktop-store--fit (concat (lispm-desktop-store--version id props) (if update " •" ""))
                               (- source-x version-x 10) body)
     (lispm-desktop-store--to (+ source-x 6) body)
     (lispm-desktop-store--fit (lispm-desktop-store--catalog-name (plist-get props :catalog))
                               (- end source-x 10) body)
     (lispm-desktop-store--end width body))))

(defun lispm-desktop-store--grouped ()
  "Return the items by group: (KIND GROUP ITEMS), each group's by name."
  (let ((items (lispm-desktop-store-items)))
    (mapcar (pcase-lambda (`(,kind ,group . ,_))
              (list kind group
                    (sort (seq-filter (lambda (item) (eq (plist-get (cdr item) :kind) kind)) items)
                          (lambda (a b) (string-lessp (downcase (plist-get (cdr a) :name))
                                                      (downcase (plist-get (cdr b) :name)))))))
            lispm-desktop-store-kinds)))

(defun lispm-desktop-store--insert-line (line)
  "Insert LINE, a list's row, and a newline in the row's own face."
  (let ((start (point)))
    (insert line)
    (add-face-text-property start (point) 'lispm-desktop-platinum-row t)
    (insert "\n")))

(defun lispm-desktop-store--rule (width &optional bottom)
  "Return a line WIDTH pixels wide closing the list, as an image: its black
edge, under the list when BOTTOM, etched below it."
  (propertize
   " " 'display
   (lispm-desktop-platinum--image
    (list 'store-rule width bottom)
    (lambda ()
      (let* ((palette lispm-desktop-platinum--palette)
             (svg (svg-create (+ width 2) 2)))
        (lispm-desktop-platinum--fill svg 0 0 (+ width 2) 2 (plist-get palette :chrome))
        (if bottom
            (progn (lispm-desktop-platinum--row svg 0 (1+ width) 0 (plist-get palette :outline))
                   (lispm-desktop-platinum--row svg 0 (1+ width) 1 (lispm-desktop-platinum-grey palette #xFF)))
          (lispm-desktop-platinum--row svg 0 (1+ width) 1 (lispm-desktop-platinum-grey palette #xAA)))
        svg)))))

(defun lispm-desktop-store--insert-top (width)
  "Insert the row above the list: the catalogs listed, and Check for Updates."
  (insert (propertize " " 'display '(space :height (8))) "\n")
  (let ((label "Check for Updates"))
    (insert (propertize " " 'display `(space :width (,lispm-desktop-store-margin)))
            (propertize "Catalogs: " 'face '(:inherit lispm-desktop-platinum-list :weight bold))
            (lispm-desktop-store--fit (if lispm-desktop-store--catalogs
                                          (mapconcat (lambda (catalog) (plist-get catalog :name))
                                                     lispm-desktop-store--catalogs ", ")
                                        "none read yet")
                                      (- width 140 (lispm-desktop-platinum-push-button-width label))
                                      'lispm-desktop-platinum-list)
            (propertize " " 'display `(space :align-to (- right (,(+ lispm-desktop-store-margin
                                                                    (lispm-desktop-store--button-width label)))))))
    (lispm-desktop-platinum-insert-push-button label #'lispm-desktop-store-check-for-updates
                                               :help "Fetch the catalogs again"
                                               :disabled lispm-desktop-store--checking)
    (insert "\n" (propertize " " 'display '(space :height (6))) "\n")))

(defun lispm-desktop-store--insert-list (width)
  "Insert the list WIDTH pixels wide: its headings, then each group and,
unfolded, its items."
  (let ((inner (- width (* 2 lispm-desktop-store-margin) 2)))
    (pcase-let ((`(,name-x ,version-x ,source-x ,end) (lispm-desktop-store--geometry inner)))
      (insert (propertize " " 'display `(space :width (,lispm-desktop-store-margin)))
              (lispm-desktop-store--rule inner)
              "\n"
              (propertize " " 'display `(space :width (,lispm-desktop-store-margin)))
              (lispm-desktop-store--outline)
              (propertize " " 'display
                          (lispm-desktop-platinum--image
                           (list 'store-header inner)
                           (lambda ()
                             (lispm-desktop-platinum-draw-list-header
                              lispm-desktop-platinum--palette
                              `(("On/Off" ,name-x nil)
                                ("Name" ,(- version-x name-x) t)
                                ("Version" ,(- source-x version-x) nil)
                                ("Source" ,(- end source-x) nil))))))
              (lispm-desktop-store--outline)
              "\n"))
    (pcase-dolist (`(,kind ,group ,items) (lispm-desktop-store--grouped))
      (lispm-desktop-store--insert-line
       (lispm-desktop-store--row (cons 'group kind)
                                 (lispm-desktop-store--group-line kind group items inner)))
      (unless (memq kind lispm-desktop-store--closed)
        (pcase-dolist (`(,id . ,props) items)
          (lispm-desktop-store--insert-line
           (lispm-desktop-store--row id (lispm-desktop-store--item-line id props inner))))))
    (insert (propertize " " 'display `(space :width (,lispm-desktop-store-margin)))
            (lispm-desktop-store--rule inner t)
            "\n")))

(defun lispm-desktop-store--button-width (label)
  "Return the width in pixels a push button reading LABEL takes on its line,
its rings' room included."
  (or (ignore-errors
        (car (image-size (lispm-desktop-platinum--image-of (lispm-desktop-platinum-push-button label)) t)))
      (+ (lispm-desktop-platinum-push-button-width label) 10)))

(defun lispm-desktop-store--insert-buttons (width)
  "Insert the push buttons under the list, flush with its right edge:
Install from URL…, Update and Remove, of the item selected."
  (let* ((id (lispm-desktop-store--selected-id))
         (buttons `(("Install from URL…" lispm-desktop-store-install-from-url nil)
                    ("Update" lispm-desktop-store-update-selected
                     ,(not (and id (lispm-desktop-store-update-available-p id))))
                    ("Remove" lispm-desktop-store-remove-selected
                     ,(not (and id (lispm-desktop-store-installed-p id))))))
         (gap 10)
         (room (+ (apply #'+ (mapcar (lambda (button) (lispm-desktop-store--button-width (car button)))
                                     buttons))
                  (* gap (1- (length buttons))))))
    (ignore width)
    (insert (propertize " " 'display '(space :height (8))) "\n"
            (propertize " " 'display `(space :align-to (- right (,(+ lispm-desktop-store-margin room))))))
    (seq-do-indexed
     (pcase-lambda (`(,label ,command ,disabled) index)
       (when (> index 0)
         (insert (propertize " " 'display `(space :width (,gap)))))
       (lispm-desktop-platinum-insert-push-button label (lambda () (call-interactively command))
                                                  :disabled disabled))
     buttons)
    (insert "\n")))

(defun lispm-desktop-store--wrap (text width face)
  "Return TEXT in FACE broken into lines of at most WIDTH pixels."
  (let ((lines nil)
        (line ""))
    (dolist (word (split-string (or text "") "[ \t\n]+" t))
      (let ((longer (if (string-empty-p line) word (concat line " " word))))
        (if (or (string-empty-p line)
                (<= (string-pixel-width (propertize longer 'face face)) width))
            (setq line longer)
          (push line lines)
          (setq line word))))
    (unless (string-empty-p line)
      (push line lines))
    (nreverse lines)))

(defun lispm-desktop-store--fields (key)
  "Return the information shown of KEY, the row selected, as (LABEL VALUE)."
  (if (consp key)
      (let ((items (nth 2 (assq (cdr key) (lispm-desktop-store--grouped)))))
        `(("Kind:" "Group")
          ("Items:" ,(format "%d" (length items)))
          ("Installed:" ,(format "%d" (seq-count (lambda (item) (lispm-desktop-store-installed-p (car item)))
                                                  items)))))
    (let* ((props (cdr (assq key (lispm-desktop-store-items))))
           (installed (lispm-desktop-store--installed key))
           (kind (plist-get props :kind)))
      `(("Kind:" ,(nth 2 (assq kind lispm-desktop-store-kinds)))
        ("Version:" ,(lispm-desktop-store--version key props))
        ("Status:" ,(cond ((lispm-desktop-store-update-available-p key) "Installed; an update is ready")
                          (installed "Installed")
                          (t "Not installed")))
        ("From:" ,(lispm-desktop-store--origin props))
        ("Where:" ,(or (and installed
                            (pcase kind
                              ('app (if-let* ((package (plist-get props :package))
                                              (desc (cadr (assq (lispm-desktop-store--package-name package)
                                                                package-alist))))
                                        (abbreviate-file-name (package-desc-dir desc))
                                      "Emacs"))
                              (_ (when-let* ((path (lispm-desktop-store--installed-path
                                                    kind (plist-get installed :as))))
                                   (abbreviate-file-name path)))))
                       "–"))))))

(defun lispm-desktop-store--draw-information (palette width key)
  "Return an SVG of the information of KEY, the row selected, WIDTH pixels
wide, in PALETTE, laid out as Extensions Manager's: the icon and name, the
fields under them, the description in a box at the right."
  (let* ((height 128)
         (svg (svg-create width height))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
         (text (plist-get palette :text))
         (small 'lispm-desktop-platinum-small)
         (props (and (symbolp key) (cdr (assq key (lispm-desktop-store-items)))))
         (name (if props (plist-get props :name) (nth 1 (assq (cdr-safe key) lispm-desktop-store-kinds))))
         (box-x (max 220 (/ width 2)))
         (box-w (- width box-x lispm-desktop-store-margin)))
    (lispm-desktop-platinum--fill svg 0 0 width height (plist-get palette :chrome))
    (when key
      (let ((icon (lispm-desktop-platinum-icon-file (if props (lispm-desktop-store--icon props t) "folder-32"))))
        (if (string-suffix-p ".svg" icon)
            (lispm-desktop-platinum--embed-icon svg icon lispm-desktop-store-margin 4 32)
          (lispm-desktop-platinum--draw-image svg (create-image icon) lispm-desktop-store-margin 4 32 32))))
    (when key
      (lispm-desktop-platinum--draw-content svg name (+ lispm-desktop-store-margin 40) 6 14 text small)
      (lispm-desktop-platinum--draw-content
       svg (if props (lispm-desktop-store--catalog-name (plist-get props :catalog)) "")
       (+ lispm-desktop-store-margin 40) 20 14 text small)
      (let ((y 44)
            (label-end (+ lispm-desktop-store-margin 66)))
        (pcase-dolist (`(,label ,value) (lispm-desktop-store--fields key))
          (let ((bold (propertize label 'face 'bold)))
            (lispm-desktop-platinum--draw-content
             svg bold (- label-end (lispm-desktop-platinum--content-width bold small)) y 13 text small)
            (lispm-desktop-platinum--draw-content
             svg (lispm-desktop-platinum--fit (propertize value 'face small) (- box-x label-end 18))
             (+ label-end 4) y 13 text small))
          (setq y (+ y 13)))))
    ;; The box, sunk in the platinum: etched, outlined, white.
    (lispm-desktop-platinum--fill svg (1- box-x) 3 (+ box-w 2) (- height 7) (funcall grey #xAA))
    (lispm-desktop-platinum--fill svg box-x 4 (1+ box-w) (- height 7) (funcall grey #xFF))
    (lispm-desktop-platinum--fill svg box-x 4 box-w (- height 8) (plist-get palette :outline))
    (lispm-desktop-platinum--fill svg (1+ box-x) 5 (- box-w 2) (- height 10) (plist-get palette :content))
    (when key
      (let ((y 8))
        (dolist (line (cons (propertize name 'face 'bold)
                            (lispm-desktop-store--wrap (and props (plist-get props :description))
                                                       (- box-w 12) small)))
          (when (< y (- height 20))
            (lispm-desktop-platinum--draw-content svg line (+ box-x 6) y 13 text small)
            (setq y (+ y 13))))))
    svg))

(defun lispm-desktop-store--insert-information (width)
  "Insert the disclosure of the item's information and, shown, the
information, WIDTH pixels wide."
  (let ((start (point)))
    (insert (propertize " " 'display `(space :width (,lispm-desktop-store-margin)))
            (lispm-desktop-platinum-triangle lispm-desktop-store--information)
            (propertize " " 'display '(space :width (6)))
            (propertize (if lispm-desktop-store--information "Hide Item Information" "Show Item Information")
                        'face '(:inherit lispm-desktop-platinum-small :weight bold)))
    (make-text-button start (point) 'action (lambda (_) (lispm-desktop-store-toggle-information))
                      'face '(:inherit lispm-desktop-platinum-small :weight bold)
                      'help-echo "Show or hide the item's information"
                      'lispm-desktop-platinum-label "Item Information")
    (insert "\n"))
  (insert (propertize " " 'display `(space :width (,lispm-desktop-store-margin)))
          (lispm-desktop-store--rule (- width (* 2 lispm-desktop-store-margin) 2))
          "\n")
  (when lispm-desktop-store--information
    (let ((key lispm-desktop-store--selected))
      (insert (propertize " " 'display
                          (lispm-desktop-platinum--image
                           (list 'store-information width key (lispm-desktop-store--fields-key key))
                           (lambda ()
                             (lispm-desktop-store--draw-information lispm-desktop-platinum--palette width key))))
              "\n"))))

(defun lispm-desktop-store--fields-key (key)
  "Return what the information of KEY depends on, to draw it again when it changes."
  (and key (list (lispm-desktop-store--fields key)
                 (when (symbolp key) (cdr (assq key (lispm-desktop-store-items)))))))

(defun lispm-desktop-store--placard ()
  "Return what the placard says: the App Store's doing, or its count."
  (or lispm-desktop-store--status-text
      (let* ((items (lispm-desktop-store-items))
             (installed (seq-count (lambda (item) (lispm-desktop-store-installed-p (car item))) items)))
        (format "%d item%s, %d installed" (length items) (if (= (length items) 1) "" "s") installed))))

(defun lispm-desktop-store--keys ()
  "Return the keys of the rows in the window's order."
  (let ((keys nil))
    (pcase-dolist (`(,kind ,_ ,items) (lispm-desktop-store--grouped))
      (push (cons 'group kind) keys)
      (unless (memq kind lispm-desktop-store--closed)
        (dolist (item items)
          (push (car item) keys))))
    (nreverse keys)))

(defun lispm-desktop-store--goto (key)
  "Put point on the row of KEY; return non-nil when there is one."
  (let ((position (point-min))
        (found nil))
    (while (and (not found) (setq position (next-single-property-change position 'lispm-desktop-store-row)))
      (when (equal (get-text-property position 'lispm-desktop-store-row) key)
        (setq found position)))
    (when found
      (goto-char found))))

(defun lispm-desktop-store--render ()
  "Draw the App Store's window again, the row or button point was on kept."
  (when-let* ((buffer (get-buffer lispm-desktop-store-buffer-name)))
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
             (window (get-buffer-window buffer t))
             (width (if window (window-body-width window t) 540))
             (button (get-text-property (point) 'lispm-desktop-platinum-label)))
        (setq lispm-desktop-store--width width)
        (unless (or (consp lispm-desktop-store--selected)
                    (memq lispm-desktop-store--selected (lispm-desktop-store--keys)))
          (setq lispm-desktop-store--selected (car (lispm-desktop-store--keys))))
        (when lispm-desktop-store--face-cookie
          (face-remap-remove-relative lispm-desktop-store--face-cookie))
        (setq lispm-desktop-store--face-cookie
              (face-remap-add-relative 'default `(:background ,(lispm-desktop-platinum--color :chrome))))
        (erase-buffer)
        (lispm-desktop-store--insert-top width)
        (lispm-desktop-store--insert-list width)
        (lispm-desktop-store--insert-buttons width)
        (lispm-desktop-store--insert-information width)
        (goto-char (point-min))
        (if button
            (let ((position (point-min)))
              (while (and (setq position (next-single-property-change position 'lispm-desktop-platinum-label))
                          (not (equal (get-text-property position 'lispm-desktop-platinum-label) button))))
              (goto-char (or position (point-min))))
          (lispm-desktop-store--goto lispm-desktop-store--selected))
        (when window
          (set-window-point window (point)))
        (setq lispm-desktop-platinum--focus nil)
        (lispm-desktop-platinum-show-focus)
        (force-mode-line-update)))))

(defun lispm-desktop-store--refit (window)
  "Draw the window again once WINDOW's width changed.
This is a buffer-local `window-size-change-functions' function."
  (unless (eql (window-body-width window t) lispm-desktop-store--width)
    (lispm-desktop-store--render)))

;;;;; Commands of the window

(defun lispm-desktop-store--in-window (function)
  "Call FUNCTION in the App Store's buffer, as from its menus."
  (with-current-buffer (lispm-desktop-store--buffer)
    (funcall function)))

(defun lispm-desktop-store--select (key)
  "Select the row of KEY and draw the window again."
  (with-current-buffer (lispm-desktop-store--buffer)
    (unless (equal key lispm-desktop-store--selected)
      (setq lispm-desktop-store--selected key)
      (lispm-desktop-store--render))))

(defun lispm-desktop-store--follow-point ()
  "Select the row point went to.  This is a buffer-local `post-command-hook'."
  (when-let* ((key (get-text-property (point) 'lispm-desktop-store-row)))
    (unless (equal key lispm-desktop-store--selected)
      (setq lispm-desktop-store--selected key)
      (lispm-desktop-store--render))))

(defun lispm-desktop-store-next-row (&optional back)
  "Select the next row of the list, or the previous one when BACK; from a
push button, the row selected."
  (interactive)
  (lispm-desktop-store--in-window
   (lambda ()
     (let* ((keys (lispm-desktop-store--keys))
            (here (get-text-property (point) 'lispm-desktop-store-row))
            (index (seq-position keys lispm-desktop-store--selected))
            (key (cond ((not here) lispm-desktop-store--selected)
                       ((null index) (car keys))
                       (t (nth (max 0 (min (1- (length keys)) (+ index (if back -1 1)))) keys)))))
       (setq lispm-desktop-store--selected key)
       (lispm-desktop-store--render)
       (lispm-desktop-store--goto key)))))

(defun lispm-desktop-store-previous-row ()
  "Select the previous row of the list."
  (interactive)
  (lispm-desktop-store-next-row t))

(defun lispm-desktop-store-toggle-group (kind &optional open)
  "Fold the group of KIND up, or unfold it; OPEN, `open' or `close', says which."
  (lispm-desktop-store--in-window
   (lambda ()
     (let ((closed (memq kind lispm-desktop-store--closed)))
       (when (pcase open ('open closed) ('close (not closed)) (_ t))
         (setq lispm-desktop-store--closed (if closed
                                               (delq kind lispm-desktop-store--closed)
                                             (cons kind lispm-desktop-store--closed)))
         (when (and (not closed) (symbolp lispm-desktop-store--selected)
                    (eq (plist-get (cdr (assq lispm-desktop-store--selected (lispm-desktop-store-items))) :kind)
                        kind))
           (setq lispm-desktop-store--selected (cons 'group kind)))
         (lispm-desktop-store--render))))))

(defun lispm-desktop-store-toggle ()
  "Install the item selected, or remove it when installed; on a group's row,
fold the group up or unfold it."
  (interactive)
  (lispm-desktop-store--in-window
   (lambda ()
     (let ((key lispm-desktop-store--selected))
       (cond ((consp key) (lispm-desktop-store-toggle-group (cdr key)))
             ((null key))
             ((lispm-desktop-store-installed-p key) (lispm-desktop-store-remove key))
             (t (lispm-desktop-store-install key)))))))

(defun lispm-desktop-store-open-selected ()
  "Open the application selected when it is installed; on a group's row,
fold the group up or unfold it; else show the item's information."
  (interactive)
  (lispm-desktop-store--in-window
   (lambda ()
     (let ((key lispm-desktop-store--selected))
       (cond ((consp key) (lispm-desktop-store-toggle-group (cdr key)))
             ((and key (eq (plist-get (lispm-desktop-store--installed key) :kind) 'app))
              (lispm-desktop-store-open key))
             ((not lispm-desktop-store--information)
              (lispm-desktop-store-toggle-information)))))))

(defun lispm-desktop-store-update-selected ()
  "Update the item selected."
  (interactive)
  (if-let* ((id (lispm-desktop-store--selected-id)))
      (lispm-desktop-store-update id)
    (user-error "Select an item first")))

(defun lispm-desktop-store-remove-selected ()
  "Remove the item selected."
  (interactive)
  (if-let* ((id (lispm-desktop-store--selected-id)))
      (lispm-desktop-store-remove id)
    (user-error "Select an item first")))

(defun lispm-desktop-store-toggle-information ()
  "Show or hide the information of the item selected."
  (interactive)
  (lispm-desktop-store--in-window
   (lambda ()
     (setq lispm-desktop-store--information (not lispm-desktop-store--information))
     (lispm-desktop-store--render))))

(defun lispm-desktop-store-unfold ()
  "Unfold the group selected."
  (interactive)
  (when (consp lispm-desktop-store--selected)
    (lispm-desktop-store-toggle-group (cdr lispm-desktop-store--selected) 'open)))

(defun lispm-desktop-store-fold ()
  "Fold up the group selected, or the group of the item selected."
  (interactive)
  (let ((kind (if (consp lispm-desktop-store--selected)
                  (cdr lispm-desktop-store--selected)
                (plist-get (cdr (assq lispm-desktop-store--selected (lispm-desktop-store-items))) :kind))))
    (when kind
      (lispm-desktop-store-toggle-group kind 'close))))

(defun lispm-desktop-store--click (event)
  "Select the row EVENT clicked; on its check box, install or remove the
item, on its triangle, fold the group up or unfold it."
  (interactive "e")
  (let* ((position (event-start event))
         (point (posn-point position)))
    (with-selected-window (posn-window position)
      (when-let* ((key (get-text-property point 'lispm-desktop-store-row)))
        (lispm-desktop-store--select key)
        (cond ((get-text-property point 'lispm-desktop-store-triangle)
               (lispm-desktop-store-toggle-group (cdr key)))
              ((get-text-property point 'lispm-desktop-store-check)
               (lispm-desktop-store-toggle)))))))

(defun lispm-desktop-store--double-click (event)
  "Open the application whose row EVENT clicked twice."
  (interactive "e")
  (lispm-desktop-store--click event)
  (lispm-desktop-store-open-selected))

(defvar-keymap lispm-desktop-store-row-map
  :doc "Keymap of the App Store's rows, for the mouse."
  "<mouse-1>" #'lispm-desktop-store--click
  "<double-mouse-1>" #'lispm-desktop-store--double-click)

(defun lispm-desktop-store-close ()
  "Close the App Store's window."
  (interactive)
  (lispm-desktop-app-window-close 'store))

(defun lispm-desktop-store--close-box (_window)
  "Close the App Store's window, from its close box."
  (lispm-desktop-store-close))

(defun lispm-desktop-store--collapse-box (_window)
  "Roll the App Store's window up into its title bar, or down again."
  (when-let* ((frame (lispm-desktop-app-window-frame 'store)))
    (if-let* ((height (lispm-desktop-platinum-rolled-up-height frame)))
        (lispm-desktop-platinum-roll-down frame height)
      (lispm-desktop-platinum-roll-up frame (window-tab-line-height (frame-root-window frame))))))

(defvar-keymap lispm-desktop-store-mode-map
  :doc "Keymap of the App Store's window."
  :parent button-buffer-map
  "C-n" #'lispm-desktop-store-next-row
  "C-p" #'lispm-desktop-store-previous-row
  "n" #'lispm-desktop-store-next-row
  "p" #'lispm-desktop-store-previous-row
  "<down>" #'lispm-desktop-store-next-row
  "<up>" #'lispm-desktop-store-previous-row
  "C-f" #'lispm-desktop-store-unfold
  "C-b" #'lispm-desktop-store-fold
  "<right>" #'lispm-desktop-store-unfold
  "<left>" #'lispm-desktop-store-fold
  "SPC" #'lispm-desktop-store-toggle
  "RET" #'lispm-desktop-store-open-selected
  "o" #'lispm-desktop-store-open-selected
  "i" #'lispm-desktop-store-toggle-information
  "u" #'lispm-desktop-store-update-selected
  "D" #'lispm-desktop-store-remove-selected
  "+" #'lispm-desktop-store-install-from-url
  "g" #'lispm-desktop-store-check-for-updates
  "q" #'lispm-desktop-store-close
  "M-w" #'lispm-desktop-store-close)

(define-derived-mode lispm-desktop-store-mode special-mode "App Store"
  "Major mode of the App Store's window, laid out as Extensions Manager.
\\{lispm-desktop-store-mode-map}"
  (visual-line-mode -1)
  (setq-local cursor-type nil
              evil-emacs-state-cursor '(nil)
              evil-normal-state-cursor '(nil)
              truncate-lines t
              line-spacing 0
              left-fringe-width 0
              right-fringe-width 0
              show-trailing-whitespace nil
              lispm-desktop-platinum-info #'lispm-desktop-store--placard
              lispm-desktop-platinum-title "App Store"
              lispm-desktop-platinum-icon (lispm-desktop-platinum-icon "app-store")
              lispm-desktop-platinum-box-functions (list :close #'lispm-desktop-store--close-box
                                                         :collapse #'lispm-desktop-store--collapse-box))
  (add-hook 'window-size-change-functions #'lispm-desktop-store--refit nil t)
  (add-hook 'post-command-hook #'lispm-desktop-store--follow-point nil t)
  (add-hook 'post-command-hook #'lispm-desktop-platinum-show-focus nil t)
  (hl-line-mode -1)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1)))

(defun lispm-desktop-store-show ()
  "Open the App Store's window over the desktop.
The catalogs are fetched the first time it opens after Emacs starts."
  (interactive)
  (unless lispm-desktop-store--catalogs
    (lispm-desktop-store-read-catalogs))
  (let ((buffer (lispm-desktop-store--buffer)))
    (lispm-desktop-app-window-show 'store :name "App Store" :buffer buffer :size '(560 . 500))
    (lispm-desktop-store--render)
    (unless (or lispm-desktop-store--checked lispm-desktop-store--checking)
      (lispm-desktop-store-check-for-updates))))

;;;;; Menus

(defun lispm-desktop-store--file-items ()
  "Return the items of the App Store's File menu."
  (let* ((id (lispm-desktop-store--selected-id))
         (installed (and id (lispm-desktop-store--installed id))))
    `((:label "Open" :action lispm-desktop-store-open-selected :key "RET"
              :enabled ,(eq (plist-get installed :kind) 'app))
      (:label ,(if installed "Remove" "Install") :action lispm-desktop-store-toggle :key "SPC"
              :enabled ,(and id t))
      (:label "Update" :action lispm-desktop-store-update-selected :key "u"
              :enabled ,(and id (lispm-desktop-store-update-available-p id) t))
      separator
      (:label "Install from URL…" :action lispm-desktop-store-install-from-url :key "+")
      (:label "Check for Updates" :action lispm-desktop-store-check-for-updates :key "g"
              :enabled ,(not lispm-desktop-store--checking))
      separator
      (:label "Close Window" :action lispm-desktop-store-close :key "q"))))

(defun lispm-desktop-store--view-items ()
  "Return the items of the App Store's View menu: a group each, checked
while unfolded, and the item's information."
  (with-current-buffer (lispm-desktop-store--buffer)
    `(,@(mapcar (pcase-lambda (`(,kind ,group . ,_))
                  (list :label group :action (lambda () (lispm-desktop-store-toggle-group kind))
                        :checked (not (memq kind lispm-desktop-store--closed))))
                lispm-desktop-store-kinds)
      separator
      (:label ,(if lispm-desktop-store--information "Hide Item Information" "Show Item Information")
              :action lispm-desktop-store-toggle-information :key "i"))))

(defun lispm-desktop-store-menus ()
  "Return the App Store's menus, for the menu bar while it is in front."
  '((lispm-desktop-store-file "File" lispm-desktop-store--file-items)
    (lispm-desktop-store-view "View" lispm-desktop-store--view-items)))

;;;; Setup

(defun lispm-desktop-setup-store ()
  "Make the App Store an application, the Applications folder a folder of
the Finder, and the applications installed the desktop's."
  (lispm-desktop-define-app 'store :name "App Store" :icon "app-store"
                            :menus #'lispm-desktop-store-menus
                            :activate #'lispm-desktop-store-show
                            :visible #'lispm-desktop-store-open-p)
  (setf (alist-get 'applications lispm-desktop-finder-folders)
        '(:title "Applications" :icon "applications" :items lispm-desktop-store--folder-items))
  (lispm-desktop-store--register-installed)
  (add-hook 'enable-theme-functions (lambda (&rest _) (lispm-desktop-store--render)) 95))

(provide 'lispm-desktop-store)
;;; lispm-desktop-store.el ends here
