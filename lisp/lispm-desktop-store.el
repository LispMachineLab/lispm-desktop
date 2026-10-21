;;; lispm-desktop-store.el --- The App Store: applications and extensions  -*- lexical-binding: t; -*-

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
;;                 desktop, see lispm-desktop-app-window.el, with the
;;                 desktop's interface, see lispm-desktop-app-ui.el: a
;;                 reader of feeds, a music player, Emacs's calculator,
;;                 its games
;;   Extensions    packages adding to the editor, as an agent its CLI
;;                 runs (`lispm-desktop-agent-define'): Hermes
;;
;; It lists them from catalogs: git repositories holding a catalog.eld,
;; LispMachineLab/lispm-store by default (`lispm-desktop-store-catalogs').
;; Each is cloned under `lispm-desktop-store-directory' and its catalog
;; read as data, never evaluated:
;;
;;   (:name "LispM Store"
;;    :items
;;    ((elfeed :kind app :name "Elfeed" :subtitle "Web feeds"
;;             :category news :description "Read web feeds."
;;             :icon "elfeed" :command elfeed
;;             :package (elfeed :url "https://github.com/skeeto/elfeed"
;;                              :rev "<commit>"))
;;     (calculator :kind app :name "Calculator" :category utilities
;;                 :command calc)
;;     (hermes :kind extension :name "Hermes" :icon "hermes"
;;             :program "hermes" :install "<command installing hermes>"
;;             :feature lispm-hermes
;;             :package (lispm-hermes :url "<repository>" :rev "<commit>"))))
;;
;; An application's :package is absent for one built into Emacs, the name
;; of a package of the package archives, or a spec as `package-vc-install'
;; takes one, pinned to the commit :rev.  :command opens the application
;; in its window; or :feature names a package that makes its application
;; itself, with `lispm-desktop-define-app' and the item's name as its key,
;; and that is unloaded (`unload-feature') when the application is removed.
;; An extension's :package is required, and its :feature, loaded, makes
;; the extension take part; unloaded (`unload-feature') it takes part no
;; more.  An extension needing a :program can be installed only while
;; that program is on PATH; :install says how to install the program.
;; An app's :programs are those it uses beyond Emacs, as EMMS a player,
;; each (NAME :for WHAT :install COMMAND :instead STAND-INS :more
;; WHAT-MORE): one missing does not keep it from being installed, but
;; its information says so, Copy Install Command copies the command,
;; and its window tells, see lispm-desktop-app-ui.el.
;; :icon names icons/NAME.svg and icons/NAME-32.svg of the catalog;
;; :category, one of `lispm-desktop-store-categories', and :subtitle, a
;; few words, place an application in the window.  :requires names the
;; items installed before.  Items of other kinds, as LispM's roles and
;; skills, are not the App Store's: it passes them over.
;;
;; Nothing is installed unasked: where a package comes from is shown
;; under the window while an alert asks, as before an update, with what
;; changes.  What was installed is kept with Customize in
;; `lispm-desktop-store-installed', so the applications and extensions
;; are back at the next start without a look at the network.
;;
;; Packages are installed and updated in an Emacs of their own, in the
;; background, one after another (`lispm-desktop-store-install-in-background',
;; lispm-desktop-store-installer.el), so the desktop goes on meanwhile.
;; The item's button gives way to a progress bar, Mac OS 9's, its
;; subtitle to what is being done: waiting, downloading, building; its
;; information says how far, and its button under the grid, Stop, stops
;; it, what it installed undone.  One that fails says why in its
;; information, its button reads Try Again, and Show Log shows what its
;; Emacs said.
;;
;; The window is laid out as the Mac App Store's, in Mac OS 9's forms:
;; the categories down the left, the items of the one chosen as icons in
;; a grid, each with its button, Get, Open or Update; push buttons under
;; the grid, and the selected item's information below them, shown or
;; hidden.  The keyboard does all of it: C-f, C-b, C-n and C-p go from
;; icon to icon, M-n and M-p from category to category, RET or Space
;; press the selected item's button, / searches, i shows or hides the
;; information, u updates, D removes, . stops an install, l shows the
;; log of one that failed, g checks the catalogs, + installs from a git
;; URL, Tab goes to the push buttons and q closes.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'package)
(require 'lispm-desktop-core)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-modal)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-app-window)
(require 'lispm-desktop-app-ui)
(require 'lispm-desktop-store-installer)

(declare-function lispm-desktop-popup-alert "lispm-desktop-popup" (icon message buttons &optional detail))
(declare-function lispm-desktop-finder-open-directory "lispm-desktop-finder" (directory))
(declare-function lispm-desktop--darken "lispm-desktop-surface" (svg))
(declare-function project-remember-projects-under "project" (dir &optional recursive))
(defvar package-vc-selected-packages)
(defvar lispm-desktop-finder-folders)
(defvar evil-emacs-state-cursor)
(defvar evil-normal-state-cursor)

(defgroup lispm-desktop-store nil
  "The App Store: applications and extensions to install."
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
or version; and :owned, non-nil when the App Store installed the item's
package itself, so that it deletes it too."
  :type '(alist :key-type symbol :value-type plist)
  :group 'lispm-desktop-store)

(defconst lispm-desktop-store-kinds
  '((app "Applications" "Application")
    (extension "Extensions" "Extension"))
  "The kinds of item the App Store installs: (KIND GROUP NAME).")

(defconst lispm-desktop-store-categories
  '((all "All")
    (news "News")
    (music "Music")
    (productivity "Productivity")
    (utilities "Utilities")
    (games "Games")
    (extensions "Extensions")
    (updates "Updates"))
  "The categories down the App Store's left side, in order: (CATEGORY NAME).
An application names its :category in its catalog; every extension is in
Extensions.  All holds every item and Updates those an update is ready
for; a category holding nothing is left out.")

(defconst lispm-desktop-store-buffer-name " *App Store*"
  "Name of the App Store's window's buffer.")

(defvar-local lispm-desktop-store--category 'all
  "The category shown, a key of `lispm-desktop-store-categories'.")

(defvar-local lispm-desktop-store--filter nil
  "What the items shown match, see `lispm-desktop-store-search', or nil.")

(defvar-local lispm-desktop-store--selected nil
  "The ID of the item selected, or nil.")

(defvar-local lispm-desktop-store--information t
  "Non-nil while the selected item's information shows under the grid.")

(defvar-local lispm-desktop-store--status-text nil
  "What the App Store is doing, for its placard, or nil.")

(defvar-local lispm-desktop-store--pressed nil
  "The item whose button the mouse holds pushed in, or nil.")

(defvar-local lispm-desktop-store--width nil
  "The width in pixels the window was drawn for.")

(defvar-local lispm-desktop-store--face-cookie nil
  "The remapping giving the window its platinum.")

(defconst lispm-desktop-store-margin 12
  "Pixels between the window's edges and what it holds.")

(defconst lispm-desktop-store-sidebar-width 132
  "Width in pixels of the list of categories, its outline included.")

(defconst lispm-desktop-store-category-height 20
  "Height in pixels of a category's row, as of a row of Mac OS 9's lists.")

(defconst lispm-desktop-store-tile-width 116
  "Narrowest an item of the grid is, in pixels: its icon, name and button.")

(defconst lispm-desktop-store-tile-height 112
  "Height in pixels of an item of the grid, and of a line of the grid.")

(defvar lispm-desktop-store-grid-map)
(defvar lispm-desktop-store-disclosure-map)
(defvar lispm-desktop-store--jobs)

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

(defun lispm-desktop-store--string-p (object)
  "Return non-nil when OBJECT is absent or a string."
  (or (null object) (stringp object)))

(defun lispm-desktop-store--programs-valid-p (programs)
  "Return non-nil when PROGRAMS, an app's :programs, are well formed: each
\(NAME :for WHAT :install COMMAND :instead STAND-INS :more WHAT-MORE),
NAME and the stand-ins programs' names, the rest strings, :for needed."
  (and (proper-list-p programs)
       (seq-every-p (lambda (program)
                      (pcase program
                        (`(,(and name (pred stringp)) . ,(and props (pred plistp)))
                         (and (not (string-empty-p name))
                              (stringp (plist-get props :for))
                              (lispm-desktop-store--string-p (plist-get props :install))
                              (lispm-desktop-store--string-p (plist-get props :more))
                              (let ((instead (plist-get props :instead)))
                                (and (proper-list-p instead) (seq-every-p #'stringp instead)))))))
                    programs)))

(defun lispm-desktop-store--item-valid-p (item)
  "Return non-nil when ITEM, (ID . PROPERTIES) of a catalog, is well formed."
  (pcase item
    (`(,(and id (pred symbolp)) . ,(and props (pred plistp)))
     (and id (stringp (plist-get props :name))
          (lispm-desktop-store--string-p (plist-get props :description))
          (lispm-desktop-store--string-p (plist-get props :subtitle))
          (symbolp (plist-get props :category))
          (let ((requires (plist-get props :requires)))
            (and (listp requires) (seq-every-p #'symbolp requires)))
          (let ((icon (plist-get props :icon)))
            (or (null icon) (lispm-desktop-store--safe-file-p icon)))
          (lispm-desktop-store--package-valid-p (plist-get props :package))
          (lispm-desktop-store--programs-valid-p (plist-get props :programs))
          (pcase (plist-get props :kind)
            ('app (let ((command (plist-get props :command))
                        (feature (plist-get props :feature)))
                    (and (symbolp command) (symbolp feature) (or command feature))))
            ('extension (and (plist-get props :package)
                             (plist-get props :feature) (symbolp (plist-get props :feature))
                             (lispm-desktop-store--string-p (plist-get props :program))
                             (lispm-desktop-store--string-p (plist-get props :install)))))))))

(defun lispm-desktop-store--kind-p (item)
  "Return non-nil when ITEM, (ID . PROPERTIES), is of a kind the App Store
installs, see `lispm-desktop-store-kinds'."
  (and (consp item) (plistp (cdr item))
       (assq (plist-get (cdr item) :kind) lispm-desktop-store-kinds)
       t))

(defun lispm-desktop-store--read-catalog (url)
  "Return the catalog at URL as its clone holds it, or nil without a clone.
A catalog that cannot be read is said so and left out, and so are the
items of one that are not well formed.  Items of kinds the App Store does
not install are passed over without a word."
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
                                       (and (lispm-desktop-store--kind-p item)
                                            (or (lispm-desktop-store--item-valid-p item)
                                                (progn (message "App Store: %s lists %S wrongly; left out"
                                                                url (car-safe item))
                                                       nil))))
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
lists any more come too, as they were installed, and those being
installed from elsewhere, as from a git URL."
  (let ((items nil))
    (dolist (catalog lispm-desktop-store--catalogs)
      (pcase-dolist (`(,id . ,props) (plist-get catalog :items))
        (unless (assq id items)
          (push `(,id :catalog ,(plist-get catalog :url) ,@props) items))))
    (pcase-dolist (`(,id . ,props) lispm-desktop-store-installed)
      (unless (or (assq id items) (not (lispm-desktop-store--kind-p (cons id props))))
        (push (cons id props) items)))
    (dolist (job lispm-desktop-store--jobs)
      (unless (assq (lispm-desktop-store-job-id job) items)
        (push (lispm-desktop-store-job-item job) items)))
    (nreverse items)))

(defun lispm-desktop-store-installed-p (id)
  "Return non-nil when the App Store installed the item ID."
  (and (assq id lispm-desktop-store-installed) t))

(defun lispm-desktop-store--installed (id)
  "Return the properties the item ID was installed with, or nil."
  (cdr (assq id lispm-desktop-store-installed)))

(defun lispm-desktop-store--file (props file)
  "Return FILE of the catalog of the item of PROPS, in its clone."
  (expand-file-name file (lispm-desktop-store--catalog-directory (plist-get props :catalog))))

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
        ('extension "robot")
        (_ (if large "applications-32" "lispm")))))

(defun lispm-desktop-store--short (commit)
  "Return COMMIT shortened, as git shows commits, or nil."
  (and (stringp commit) (substring commit 0 (min 7 (length commit)))))

(defun lispm-desktop-store--version (id props)
  "Return the version of the item ID, of PROPS, for its information."
  (let ((package (plist-get props :package)))
    (or (cond ((null package) (format "%d.%d" emacs-major-version emacs-minor-version))
              ((equal (plist-get (lispm-desktop-store--installed id) :installed) "load-path")
               "load path")
              ((and (lispm-desktop-store-installed-p id)
                    (lispm-desktop-store--package-version (lispm-desktop-store--package-name package))))
              ((consp package) (lispm-desktop-store--short (plist-get (cdr package) :rev))))
        "–")))

(defun lispm-desktop-store--origin (props)
  "Return where the item of PROPS comes from, in words."
  (pcase (plist-get props :package)
    ('nil "Emacs")
    ((pred symbolp) "the package archives")
    (`(,_ . ,spec) (plist-get spec :url))))

(defun lispm-desktop-store-update-available-p (id)
  "Return non-nil when the item ID is installed as its catalog no longer
lists it: its package pinned to another commit.  One loaded from the load
path, as a checkout's, is updated there, not by the App Store."
  (when-let* ((installed (lispm-desktop-store--installed id))
              ((not (equal (plist-get installed :installed) "load-path")))
              (props (cdr (assq id (lispm-desktop-store-items))))
              (package (plist-get props :package))
              ((consp package)))
    (not (equal (plist-get (cdr package) :rev) (plist-get installed :installed)))))

(defun lispm-desktop-store--requirers (id)
  "Return the names of the installed items requiring the item ID."
  (mapcan (pcase-lambda (`(,_ . ,props))
            (when (memq id (plist-get props :requires))
              (list (plist-get props :name))))
          lispm-desktop-store-installed))

(defun lispm-desktop-store--missing-program (props)
  "Return the program the item of PROPS needs and PATH lacks, or nil."
  (let ((program (plist-get props :program)))
    (and (stringp program) (not (executable-find program)) program)))

(defun lispm-desktop-store--category-of (props)
  "Return the category of the item of PROPS: Extensions for an extension,
else its :category."
  (if (eq (plist-get props :kind) 'extension) 'extensions (plist-get props :category)))

(defun lispm-desktop-store--action (id props)
  "Return what the button of the item ID, of PROPS, does, as (LABEL .
ACTION): ACTION is `install', `open', `update' or `stop', or nil for a
button that does nothing: an extension installed, one whose program is
not on PATH, or an update past stopping.  An item whose install or
update failed is tried again."
  (cond ((lispm-desktop-store--busy-p id)
         (if (lispm-desktop-store--stoppable-p (lispm-desktop-store--job id)) '("Stop" . stop) '("Stop")))
        ((when-let* ((job (lispm-desktop-store--job id)))
           (cons "Try Again" (lispm-desktop-store-job-action job))))
        ((lispm-desktop-store-update-available-p id) '("Update" . update))
        ((lispm-desktop-store-installed-p id)
         (if (eq (plist-get props :kind) 'app) '("Open" . open) '("Installed")))
        ((lispm-desktop-store--missing-program props) '("Get"))
        (t '("Get" . install))))

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
  (lispm-desktop-platinum-quoted-names (mapcar (lambda (item) (plist-get (cdr item) :name)) items)))

(defun lispm-desktop-store--confirm (question detail preview action)
  "Ask QUESTION in an alert, DETAIL under it, and return non-nil to go on.
PREVIEW, a text, shows under the grid meanwhile; ACTION names the button
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
  "Call FUNCTION with TEXT, when non-nil, shown under the App Store's grid
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

(defun lispm-desktop-store--preview (item)
  "Return what installing ITEM brings, as text to read first."
  (pcase-let ((`(,_ . ,props) item))
    (concat
     (format "* %s, %s\n\n" (plist-get props :name)
             (downcase (nth 2 (assq (plist-get props :kind) lispm-desktop-store-kinds))))
     (pcase (plist-get props :package)
       ('nil "Built into Emacs.\n")
       ((and package (pred symbolp))
        (format "The package %s, from the package archives (%s).\n"
                package (mapconcat #'car package-archives ", ")))
       (`(,name . ,spec)
        (format "The package %s, from %s at commit %s.\n"
                name (plist-get spec :url) (plist-get spec :rev))))
     (when-let* ((program (plist-get props :program)))
       (format "It runs %s, found at %s.\n" program (executable-find program))))))

(defun lispm-desktop-store--built-in-p (props)
  "Return non-nil when PROPS describe an application built into Emacs."
  (and (eq (plist-get props :kind) 'app) (null (plist-get props :package))))

(defun lispm-desktop-store--confirm-install (items)
  "Show what ITEMS bring and ask whether to install them; non-nil if so.
Applications built into Emacs are only made applications: nothing asks."
  (or (seq-every-p (lambda (item) (lispm-desktop-store--built-in-p (cdr item))) items)
      (lispm-desktop-store--confirm
       (format "Install %s?" (lispm-desktop-store--names items))
       "Its code will run in Emacs as your own does; where it comes from is shown under the window."
       (mapconcat #'lispm-desktop-store--preview items "\n")
       "Install")))

(defun lispm-desktop-store--to-install (id)
  "Return the items installing the item ID brings, those it requires first
and those installed, or being installed, left out."
  (let ((items (lispm-desktop-store-items))
        (order nil)
        (visiting nil))
    (cl-labels ((visit (id from)
                  (unless (or (assq id order) (lispm-desktop-store-installed-p id)
                              ;; Asked for before: waiting, or installing.
                              (and from (lispm-desktop-store--busy-p id)))
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

(defun lispm-desktop-store--check-program (props)
  "Signal a `user-error' when the item of PROPS needs a program PATH lacks,
saying how to install it."
  (when-let* ((program (lispm-desktop-store--missing-program props)))
    (user-error "%s needs `%s', which is not on PATH%s" (plist-get props :name) program
                (if-let* ((install (plist-get props :install)))
                    (format "; install it with: %s" install)
                  ""))))

(defun lispm-desktop-store--keep (id props &rest installed)
  "Keep that the item ID, of PROPS, is installed as INSTALLED, a plist, says.
Saving it is not said in the echo area: the window shows it."
  (let ((inhibit-message t))
    (customize-save-variable
     'lispm-desktop-store-installed
     (append (assq-delete-all id (copy-alist lispm-desktop-store-installed))
             (list (cons id (append installed (lispm-desktop-store--plist-without
                                               props :installed :owned))))))))

(defun lispm-desktop-store--forget (id)
  "Forget that the item ID is installed."
  (customize-save-variable 'lispm-desktop-store-installed
                           (assq-delete-all id (copy-alist lispm-desktop-store-installed))))

(defun lispm-desktop-store--package-present (props)
  "Return where the package of the item of PROPS is already: `built-in' for
none to install, `installed' for a package installed, `load-path' for an
item's feature on the load path without a package, as a checkout's; nil
when it is to be installed."
  (let ((package (plist-get props :package))
        (feature (plist-get props :feature)))
    (cond ((null package) 'built-in)
          ((package-installed-p (lispm-desktop-store--package-name package)) 'installed)
          ((and feature (locate-library (symbol-name feature))) 'load-path))))

(defmacro lispm-desktop-store--quietly (&rest body)
  "Run BODY showing none of the buffers it would show, as the log of the
package's compilation, which would split the App Store's window."
  (declare (indent 0))
  `(save-window-excursion
     (let ((display-buffer-overriding-action '(display-buffer-no-window (allow-no-window . t))))
       ,@body)))

(defun lispm-desktop-store--install-here (job present)
  "Do JOB, an install, in this Emacs, which waits meanwhile: its package is
installed unless PRESENT, as `lispm-desktop-store--package-present' says."
  (pcase-let* ((`(,_ . ,props) (lispm-desktop-store-job-item job))
               (package (plist-get props :package))
               (installed (pcase present
                            ('nil
                             (lispm-desktop-store--status (format "Installing %s…" (plist-get props :name)))
                             (unwind-protect
                                 (lispm-desktop-store--quietly
                                   (lispm-desktop-store--install-package package))
                               (lispm-desktop-store--status nil)))
                            ;; A checkout's: no package of the App Store's
                            ;; to update or delete.
                            ('load-path "load-path")
                            ('built-in "built-in")
                            ;; Installed already: taken as the catalog pins it.
                            (_ (if (consp package)
                                   (plist-get (cdr package) :rev)
                                 (or (lispm-desktop-store--package-version package) "installed"))))))
    (lispm-desktop-store--installed-as job installed (not present))))

(defun lispm-desktop-store--installed-as (job installed owned)
  "End JOB, an install, its package installed as INSTALLED, OWNED when the
App Store installed it: keep it, and make it take part, an application
one of the desktop's, an extension loaded.  It is kept only once it takes
part: a package that cannot be loaded is not left installed."
  (pcase-let ((`(,id . ,props) (lispm-desktop-store-job-item job)))
    (if-let* ((failure (lispm-desktop-store--failure-of (lambda () (lispm-desktop-store--register id props)))))
        (progn
          (when owned
            (lispm-desktop-store--quietly
              (lispm-desktop-store--delete-package (plist-get props :package))))
          (lispm-desktop-store--fail job (format "It cannot be loaded: %s" failure)))
      (lispm-desktop-store--keep id props :installed installed :owned owned)
      (lispm-desktop-store--done job))))

(defun lispm-desktop-store--failure-of (function)
  "Call FUNCTION; return nil, or what the error it signalled says."
  (condition-case err
      (progn (funcall function) nil)
    (error (error-message-string err))))

(defun lispm-desktop-store-install (id)
  "Install the item ID, and the items it requires first, once confirmed.
They are installed one after another, in the background, see
`lispm-desktop-store-install-in-background', each item's progress shown
in its place in the App Store's window."
  (interactive (list (lispm-desktop-store--read-item
                      "Install: " (lambda (id) (not (lispm-desktop-store-installed-p id))))))
  (when (lispm-desktop-store--busy-p id)
    (user-error "%s is being installed already" (plist-get (cdr (assq id (lispm-desktop-store-items))) :name)))
  (let ((items (lispm-desktop-store--to-install id)))
    ;; Nothing is asked about what could not run here.
    (dolist (item items)
      (lispm-desktop-store--check-program (cdr item)))
    (when (and items (lispm-desktop-store--confirm-install items))
      (lispm-desktop-store--queue items 'install))))

;;;; Installing in the background

(defcustom lispm-desktop-store-install-in-background t
  "Non-nil to install and update packages in an Emacs of their own.
The desktop goes on meanwhile, and the App Store's window shows how far
each is, as Mac OS 9 showed it, in a progress bar; one can be stopped.
Nil installs them in this Emacs, which waits until each is done."
  :type 'boolean
  :group 'lispm-desktop-store)

(cl-defstruct (lispm-desktop-store-job (:constructor lispm-desktop-store--make-job)
                                       (:copier nil))
  "An install or an update the App Store was asked for."
  id item action group
  (state 'waiting) (stage 'waiting) (fraction 0.0)
  process log file before pending installed error stopped)

(defvar lispm-desktop-store--jobs nil
  "The installs and updates asked for, in order, as `lispm-desktop-store-job's.
Each waits, runs, one at a time, or failed; those done are gone.  A job
is installing or updating ITEM, (ID . PROPERTIES), as ACTION says; those
asked for together share a GROUP.")

(defconst lispm-desktop-store-stages
  '((waiting "Waiting" 0.0 0.0)
    (preparing "Preparing" 0.0 0.05)
    (archives "Reading the archives" 0.05 0.1)
    (download "Downloading" 0.1 0.6)
    (dependencies "Getting what it needs" 0.6 0.7)
    (build "Building" 0.7 0.95)
    (finishing "Finishing" 0.95 1.0))
  "The stages of an install or an update, in order: (STAGE NAME FROM TO),
FROM and TO the part of its progress bar each fills.")

(defconst lispm-desktop-store--handed-variables
  '(user-emacs-directory exec-path
    package-user-dir package-directory-list package-archives package-archive-priorities
    package-pinned-packages package-load-list package-check-signature package-unsigned-archives
    package-gnupghome-dir package-selected-packages package-install-upgrade-built-in
    package-vc-selected-packages package-vc-default-backend package-quickstart package-quickstart-file
    url-proxy-services url-using-proxy gnutls-trustfiles network-security-level)
  "What an Emacs installing in the background takes from this one: how
packages are installed, and how the network is reached.")

(defvar lispm-desktop-store--redraw-timer nil
  "Timer drawing the App Store's window again with the jobs' progress.")

(defun lispm-desktop-store--job (id)
  "Return the job of the item ID, or nil."
  (seq-find (lambda (job) (eq (lispm-desktop-store-job-id job) id)) lispm-desktop-store--jobs))

(defun lispm-desktop-store--busy-p (id)
  "Return non-nil while the item ID waits to be installed or updated, or is."
  (when-let* ((job (lispm-desktop-store--job id)))
    (memq (lispm-desktop-store-job-state job) '(waiting running))))

(defun lispm-desktop-store--background-p ()
  "Return non-nil when packages are installed in an Emacs of their own."
  (and lispm-desktop-store-install-in-background t))

(defun lispm-desktop-store--stoppable-p (job)
  "Return non-nil when JOB can be stopped: while it waits, an install
while it runs in the background, an update until it builds, as Mac OS
9's Installer no longer stopped once it changed what it had installed."
  (pcase (lispm-desktop-store-job-state job)
    ('waiting t)
    ('running (and (lispm-desktop-store-job-process job)
                   (or (eq (lispm-desktop-store-job-action job) 'install)
                       (memq (lispm-desktop-store-job-stage job) '(preparing archives download)))))))

(defun lispm-desktop-store--queue (items action)
  "Install or update, as ACTION says, ITEMS, (ID . PROPERTIES), one after
another; an earlier attempt that failed is forgotten."
  (let ((group (list action)))
    (dolist (item items)
      (when-let* ((old (lispm-desktop-store--job (car item))))
        (lispm-desktop-store--forget-job old))
      (setq lispm-desktop-store--jobs
            (append lispm-desktop-store--jobs
                    (list (lispm-desktop-store--make-job :id (car item) :item item
                                                         :action action :group group))))))
  (lispm-desktop-store--render)
  (lispm-desktop-store--next))

(defun lispm-desktop-store--forget-job (job)
  "Forget JOB, and its log."
  (setq lispm-desktop-store--jobs (delq job lispm-desktop-store--jobs))
  (when-let* ((log (lispm-desktop-store-job-log job))
              ((buffer-live-p log)))
    (when-let* ((pipe (get-buffer-process log)))
      (delete-process pipe))
    (kill-buffer log)))

(defun lispm-desktop-store--next ()
  "Start the first job waiting, unless one runs."
  (unless (seq-find (lambda (job) (eq (lispm-desktop-store-job-state job) 'running))
                    lispm-desktop-store--jobs)
    (when-let* ((job (seq-find (lambda (job) (eq (lispm-desktop-store-job-state job) 'waiting))
                               lispm-desktop-store--jobs)))
      (lispm-desktop-store--start job))))

(defun lispm-desktop-store--start (job)
  "Start JOB: in an Emacs of its own, or here, as
`lispm-desktop-store-install-in-background' says.  An item whose package
is there already, or that has none, is installed at once."
  (setf (lispm-desktop-store-job-state job) 'running
        (lispm-desktop-store-job-stage job) 'preparing)
  (pcase-let ((`(,id . ,props) (lispm-desktop-store-job-item job)))
    (condition-case err
        (progn
          ;; What it requires must be there by now: it failed, else.
          (when-let* ((missing (seq-find (lambda (required) (not (lispm-desktop-store-installed-p required)))
                                         (plist-get props :requires))))
            (error "It needs “%s”, which is not installed"
                   (or (plist-get (cdr (assq missing (lispm-desktop-store-items))) :name) missing)))
          (pcase (lispm-desktop-store-job-action job)
            ('install
             (let ((present (lispm-desktop-store--package-present props)))
               (if (or present (not (lispm-desktop-store--background-p)))
                   (lispm-desktop-store--install-here job present)
                 (lispm-desktop-store--spawn job `(install ,(plist-get props :package))))))
            ('update
             (let ((package (plist-get props :package))
                   (former (plist-get (lispm-desktop-store--installed id) :installed)))
               (if (lispm-desktop-store--background-p)
                   (lispm-desktop-store--spawn
                    job `(update ,(car package) ,(plist-get (cdr package) :rev) ,former))
                 (lispm-desktop-store--update-here job))))))
      (error (lispm-desktop-store--fail job (error-message-string err))))))

(defun lispm-desktop-store--done (job)
  "End JOB, which went well."
  (lispm-desktop-store--forget-job job)
  (unless (lispm-desktop-store-open-p)
    (message "%s is %s" (plist-get (cdr (lispm-desktop-store-job-item job)) :name)
             (if (eq (lispm-desktop-store-job-action job) 'install) "installed" "up to date")))
  (lispm-desktop-store--render)
  (lispm-desktop-store--next))

(defun lispm-desktop-store--fail (job reason)
  "End JOB, which failed for REASON, a sentence: it stays, saying so, with
its log, until it is tried again.  The jobs asked for with it, waiting,
are not started."
  (setf (lispm-desktop-store-job-state job) 'failed
        (lispm-desktop-store-job-process job) nil
        (lispm-desktop-store-job-error job) reason)
  (dolist (other lispm-desktop-store--jobs)
    (when (and (eq (lispm-desktop-store-job-group other) (lispm-desktop-store-job-group job))
               (eq (lispm-desktop-store-job-state other) 'waiting))
      (lispm-desktop-store--forget-job other)))
  (message "App Store: %s could not be %s: %s" (plist-get (cdr (lispm-desktop-store-job-item job)) :name)
           (if (eq (lispm-desktop-store-job-action job) 'install) "installed" "updated")
           reason)
  (lispm-desktop-store--render)
  (lispm-desktop-store--next))

(defun lispm-desktop-store--readable-p (value)
  "Return non-nil when VALUE, printed, reads back as itself."
  (condition-case nil
      (let ((print-length nil) (print-level nil))
        (equal (car (read-from-string (prin1-to-string value))) value))
    (error nil)))

(defun lispm-desktop-store--job-program (task)
  "Return the program an Emacs of its own runs for TASK, as
`lispm-desktop-store-installer-run' takes it: this Emacs's settings for
packages and the network, then TASK."
  (let ((library (or (locate-library "lispm-desktop-store-installer")
                     (error "lispm-desktop-store-installer is not on the load path"))))
    `(progn
       ,@(mapcan (lambda (variable)
                   (when (and (boundp variable) (lispm-desktop-store--readable-p (symbol-value variable)))
                     (list `(setq ,variable ',(symbol-value variable)))))
                 lispm-desktop-store--handed-variables)
       (push ,(file-name-directory library) load-path)
       (require 'lispm-desktop-store-installer)
       (lispm-desktop-store-installer-run ',task))))

(defun lispm-desktop-store--package-directories ()
  "Return the packages' directories in `package-user-dir', the archives'
and the keys' left out."
  (when (file-directory-p package-user-dir)
    (seq-filter (lambda (directory)
                  (and (file-directory-p directory)
                       (not (member (file-name-nondirectory directory) '("archives" "gnupg")))))
                (directory-files package-user-dir t "\\`[^.]"))))

(defun lispm-desktop-store--spawn (job task)
  "Run JOB in an Emacs of its own, in batch, doing TASK; see
`lispm-desktop-store-installer-run'."
  (pcase-let* ((`(,_ . ,props) (lispm-desktop-store-job-item job))
               (file (make-temp-file "lispm-desktop-store-job" nil ".el"))
               (log (generate-new-buffer (format " *App Store: %s*" (plist-get props :name))))
               (process-environment (lispm-desktop-store--git-environment)))
    (with-temp-file file
      (insert ";;; -*- lexical-binding: t -*-\n")
      (let ((print-length nil) (print-level nil) (print-escape-newlines t))
        (prin1 (lispm-desktop-store--job-program task) (current-buffer))))
    (setf (lispm-desktop-store-job-file job) file
          (lispm-desktop-store-job-log job) log
          (lispm-desktop-store-job-before job) (lispm-desktop-store--package-directories))
    (add-hook 'kill-emacs-hook #'lispm-desktop-store--abandon)
    (let ((process (make-process
                    :name "lispm-desktop-store-install"
                    :command (list (expand-file-name invocation-name invocation-directory)
                                   "-Q" "--batch" "-l" file)
                    :connection-type 'pipe
                    ;; Its log goes when the job does, unasked.
                    :stderr (make-pipe-process :name "lispm-desktop-store-install log"
                                               :buffer log :noquery t)
                    :filter (lambda (_process text) (lispm-desktop-store--heard job text))
                    :sentinel (lambda (process _event)
                                (unless (process-live-p process)
                                  ;; Alerts and loading are no sentinel's work.
                                  (run-at-time 0 nil #'lispm-desktop-store--ended job))))))
      (setf (lispm-desktop-store-job-process job) process)
      ;; Nothing it asks is answered: it fails instead of waiting.
      (process-send-eof process))))

(defun lispm-desktop-store--stage-bounds (stage)
  "Return the part of the progress bar STAGE fills, as (FROM TO)."
  (or (cddr (assq stage lispm-desktop-store-stages)) '(0.0 0.0)))

(defun lispm-desktop-store--heard (job text)
  "Take what JOB's Emacs said, TEXT: how far it is, see
lispm-desktop-store-installer.el, else what goes in its log."
  (let* ((lines (split-string (concat (lispm-desktop-store-job-pending job) text) "\n"))
         (tag lispm-desktop-store-installer-tag))
    (setf (lispm-desktop-store-job-pending job) (car (last lines)))
    (dolist (line (butlast lines))
      (if (string-prefix-p tag line)
          (pcase (ignore-errors (car (read-from-string line (length tag))))
            (`(stage ,(and stage (guard (assq stage lispm-desktop-store-stages))))
             (setf (lispm-desktop-store-job-stage job) stage
                   (lispm-desktop-store-job-fraction job)
                   (max (lispm-desktop-store-job-fraction job) (car (lispm-desktop-store--stage-bounds stage)))))
            (`(progress ,(and fraction (pred numberp)))
             (pcase-let ((`(,from ,to) (lispm-desktop-store--stage-bounds (lispm-desktop-store-job-stage job))))
               (setf (lispm-desktop-store-job-fraction job)
                     (max (lispm-desktop-store-job-fraction job) (+ from (* (- to from) (min 1 fraction)))))))
            (`(done ,installed) (setf (lispm-desktop-store-job-installed job) (list installed)))
            (`(failed ,message) (setf (lispm-desktop-store-job-error job) message)))
        (when (and (not (string-blank-p line)) (buffer-live-p (lispm-desktop-store-job-log job)))
          (with-current-buffer (lispm-desktop-store-job-log job)
            (goto-char (point-max))
            (insert line "\n")))))
    (lispm-desktop-store--redraw-soon)))

(defun lispm-desktop-store--last-said (job)
  "Return the last line JOB's Emacs said in its log, or nil."
  (when (buffer-live-p (lispm-desktop-store-job-log job))
    (car (last (split-string (with-current-buffer (lispm-desktop-store-job-log job) (buffer-string))
                             "[\r\n]+" t "[ \t]+")))))

(defun lispm-desktop-store--ended (job)
  "Take that JOB's Emacs ended: the job is done, failed or was stopped."
  (let ((process (lispm-desktop-store-job-process job)))
    (when (lispm-desktop-store-job-file job)
      (ignore-errors (delete-file (lispm-desktop-store-job-file job)))
      (setf (lispm-desktop-store-job-file job) nil))
    (cond
     ((lispm-desktop-store-job-stopped job)
      (lispm-desktop-store--undo job)
      (dolist (other lispm-desktop-store--jobs)
        (when (and (eq (lispm-desktop-store-job-group other) (lispm-desktop-store-job-group job))
                   (memq (lispm-desktop-store-job-state other) '(waiting running)))
          (lispm-desktop-store--forget-job other)))
      (lispm-desktop-store--render)
      (lispm-desktop-store--next))
     ((and (eql (process-exit-status process) 0) (lispm-desktop-store-job-installed job))
      (setf (lispm-desktop-store-job-stage job) 'finishing
            (lispm-desktop-store-job-fraction job) 0.95)
      (let ((installed (car (lispm-desktop-store-job-installed job))))
        (if-let* ((failure (lispm-desktop-store--failure-of (lambda () (lispm-desktop-store--adopt job)))))
            (progn (lispm-desktop-store--undo job)
                   (lispm-desktop-store--fail job failure))
          (pcase (lispm-desktop-store-job-action job)
            ('install (lispm-desktop-store--installed-as job installed t))
            ('update (lispm-desktop-store--updated-as job installed))))))
     (t
      (lispm-desktop-store--undo job)
      (lispm-desktop-store--fail job (or (lispm-desktop-store-job-error job)
                                         (lispm-desktop-store--last-said job)
                                         "Its Emacs stopped before it was done"))))))

(defun lispm-desktop-store--undo (job)
  "Undo what JOB, failed or stopped in the background, left: the packages
it put in `package-user-dir', and an update's checkout, back at the
commit it was at."
  (let ((before (lispm-desktop-store-job-before job)))
    (dolist (directory (lispm-desktop-store--package-directories))
      (unless (member directory before)
        (delete-directory directory t))))
  (when (eq (lispm-desktop-store-job-action job) 'update)
    (pcase-let* ((`(,id . ,props) (lispm-desktop-store-job-item job))
                 (desc (cadr (assq (lispm-desktop-store--package-name (plist-get props :package))
                                   package-alist))))
      (when desc
        (ignore-errors
          (lispm-desktop-store--git (package-desc-dir desc) "checkout" "--quiet" "--detach"
                                    (plist-get (lispm-desktop-store--installed id) :installed)))))))

(defun lispm-desktop-store--adopt (job)
  "Make what JOB's Emacs installed known to this one and take part, as
installing it here would have: its package and those it brought, active,
and its package one of those chosen, as package.el keeps them.  An
update's package is described anew, and what of it was loaded is loaded
again."
  (pcase-let* ((`(,_ . ,props) (lispm-desktop-store-job-item job))
               (package (plist-get props :package))
               (name (lispm-desktop-store--package-name package))
               (before (lispm-desktop-store-job-before job))
               (former (assq name package-alist))
               (again (and former (eq (lispm-desktop-store-job-action job) 'update)
                           (package-desc-dir (cadr former))))
               ;; What Customize saves and project.el finds is not said
               ;; in the echo area: the window shows what happens.
               (inhibit-message t))
    (when again
      (setq package-alist (delq former package-alist)))
    (dolist (directory (lispm-desktop-store--package-directories))
      (when (or (not (member directory before))
                (and again (file-equal-p directory again)))
        (package-load-descriptor directory)))
    (if again
        (let ((desc (or (cadr (assq name package-alist)) (error "%s is gone" name))))
          (package-activate-1 desc 'reload 'deps))
      (unless (package-activate name)
        (error "Its package %s cannot be activated" name))
      (unless (memq name package-selected-packages)
        (customize-save-variable 'package-selected-packages (cons name package-selected-packages)))
      (when (consp package)
        (require 'package-vc)
        (unless (assq name package-vc-selected-packages)
          (customize-save-variable
           'package-vc-selected-packages
           (cons (cons name (lispm-desktop-store--plist-without (cdr package) :rev))
                 package-vc-selected-packages)))
        (when (bound-and-true-p package-vc-register-as-project)
          (require 'project)
          (project-remember-projects-under (package-desc-dir (cadr (assq name package-alist)))))))))

(defun lispm-desktop-store--abandon ()
  "Stop the jobs running, as Emacs exits, and undo what they did."
  (dolist (job lispm-desktop-store--jobs)
    (when-let* ((process (lispm-desktop-store-job-process job))
                ((process-live-p process)))
      (setf (lispm-desktop-store-job-stopped job) t)
      (delete-process process)
      (ignore-errors (lispm-desktop-store--undo job))
      (when (lispm-desktop-store-job-file job)
        (ignore-errors (delete-file (lispm-desktop-store-job-file job)))))))

(defun lispm-desktop-store-stop (id)
  "Stop installing or updating the item ID, and those asked for with it.
What it installed so far is undone."
  (interactive (list (lispm-desktop-store--read-item "Stop: " #'lispm-desktop-store--busy-p)))
  (let ((job (lispm-desktop-store--job id)))
    (unless (lispm-desktop-store--busy-p id)
      (user-error "Nothing is being installed for that"))
    (unless (lispm-desktop-store--stoppable-p job)
      (user-error "%s can no longer be stopped" (plist-get (cdr (lispm-desktop-store-job-item job)) :name)))
    (if (eq (lispm-desktop-store-job-state job) 'running)
        (progn (setf (lispm-desktop-store-job-stopped job) t)
               (delete-process (lispm-desktop-store-job-process job)))
      (let ((group (lispm-desktop-store-job-group job)))
        (dolist (other (memq job lispm-desktop-store--jobs))
          (when (and (eq (lispm-desktop-store-job-group other) group)
                     (eq (lispm-desktop-store-job-state other) 'waiting))
            (lispm-desktop-store--forget-job other))))
      (lispm-desktop-store--render))))

(defun lispm-desktop-store-show-log (id)
  "Show what the Emacs that failed to install or update the item ID said."
  (interactive (list (lispm-desktop-store--read-item
                      "Log of: " (lambda (id) (when-let* ((job (lispm-desktop-store--job id)))
                                                (buffer-live-p (lispm-desktop-store-job-log job)))))))
  (let ((log (and (lispm-desktop-store--job id) (lispm-desktop-store-job-log (lispm-desktop-store--job id)))))
    (unless (buffer-live-p log)
      (user-error "There is no log of that"))
    (with-current-buffer log
      (special-mode)
      (goto-char (point-max)))
    (pop-to-buffer log)))

(defun lispm-desktop-store--progress (job)
  "Return how far JOB is, in words, as \"Installing: downloading, 45%\"."
  (let ((install (eq (lispm-desktop-store-job-action job) 'install)))
    (pcase (lispm-desktop-store-job-state job)
      ('waiting (if install "Waiting to install" "Waiting to update"))
      ('failed (if install "Could not be installed" "Could not be updated"))
      (_ (format "%s: %s, %d%%" (if install "Installing" "Updating")
                 (downcase (nth 1 (assq (lispm-desktop-store-job-stage job) lispm-desktop-store-stages)))
                 (round (* 100 (lispm-desktop-store-job-fraction job))))))))

(defun lispm-desktop-store--redraw-soon ()
  "Draw the App Store's window again shortly, its jobs' progress shown."
  (unless (timerp lispm-desktop-store--redraw-timer)
    (setq lispm-desktop-store--redraw-timer
          (run-at-time 0.1 nil (lambda ()
                                 (setq lispm-desktop-store--redraw-timer nil)
                                 (lispm-desktop-store--render))))))

(defun lispm-desktop-store--delete-package (package)
  "Delete PACKAGE, an item's :package, as package.el deletes one.
One that other packages need stays, as said in the echo area."
  (let ((name (lispm-desktop-store--package-name package)))
    (when-let* ((desc (cadr (assq name package-alist))))
      (condition-case err
          (progn
            (package-delete desc)
            ;; Activated from package.el's start, before it read the
            ;; archives, `package-installed-p' asks this list; the
            ;; package would be there still, and not installed again.
            (unless (assq name package-alist)
              (setq package-activated-list (delq name package-activated-list))))
        (error (message "App Store: %s stays: %s" name (error-message-string err)))))
    (when (and (consp package) (boundp 'package-vc-selected-packages)
               (assq name package-vc-selected-packages))
      (customize-save-variable 'package-vc-selected-packages
                               (assq-delete-all name (copy-alist package-vc-selected-packages))))))

(defun lispm-desktop-store--remove-detail (id props)
  "Return what removing the item ID, of PROPS, does, in words."
  (let ((requirers (lispm-desktop-store--requirers id))
        (package (lispm-desktop-store--package-name (plist-get props :package))))
    (concat
     (pcase (plist-get props :kind)
       ('extension "It leaves the editor at once.  ")
       (_ (if package "" "It stays in Emacs; only its window and its place among the applications go.")))
     (cond ((null package) "")
           ((plist-get props :owned) (format "Its package %s is deleted." package))
           (t (format "Its package %s stays, as it was there before." package)))
     (when requirers
       (format "  %s %s it." (lispm-desktop-platinum-quoted-names requirers)
               (if (cdr requirers) "need" "needs"))))))

(defun lispm-desktop-store--unload (feature)
  "Unload FEATURE when it is loaded, as `unload-feature' does by force;
what cannot be unloaded is said, and stays until Emacs restarts."
  (when (featurep feature)
    (condition-case err
        (unload-feature feature t)
      (error (message "App Store: %s stays loaded until Emacs restarts: %s"
                      feature (error-message-string err))))))

(defun lispm-desktop-store--remove-1 (id props)
  "Remove the item ID, of PROPS, unasked, and forget it."
  (pcase (plist-get props :kind)
    ('app (lispm-desktop-store--unload (plist-get props :feature))
          (lispm-desktop-app-window-close id)
          (lispm-desktop-remove-app id))
    ('extension (lispm-desktop-store--unload (plist-get props :feature))))
  (when (plist-get props :owned)
    (lispm-desktop-store--quietly
      (lispm-desktop-store--delete-package (plist-get props :package))))
  (lispm-desktop-store--forget id))

(defun lispm-desktop-store-remove (id)
  "Remove the item ID the App Store installed, once confirmed."
  (interactive (list (lispm-desktop-store--read-item "Remove: " #'lispm-desktop-store-installed-p)))
  (let ((props (or (lispm-desktop-store--installed id)
                   (user-error "The App Store did not install %s" id))))
    (when (lispm-desktop-store--busy-p id)
      (user-error "%s is being updated; stop that first" (plist-get props :name)))
    (when (lispm-desktop-store--confirm (format "Remove “%s”?" (plist-get props :name))
                                        (lispm-desktop-store--remove-detail id props)
                                        nil "Remove")
      (lispm-desktop-store--remove-1 id props)
      (lispm-desktop-store--render))))

(defun lispm-desktop-store--changes (installed props)
  "Return what updating the item installed as INSTALLED to PROPS changes."
  (pcase-let ((`(,name . ,spec) (plist-get props :package)))
    (format "* %s\n\nThe package %s goes from commit %s to commit %s, of %s.\n"
            (plist-get props :name) name (plist-get installed :installed)
            (plist-get spec :rev) (plist-get spec :url))))

(defun lispm-desktop-store-update (id)
  "Bring the item ID to what its catalog lists now, once confirmed.
It is updated in the background, as it is installed, see
`lispm-desktop-store-install'; an extension is loaded again, so the new
code takes part at once."
  (interactive (list (lispm-desktop-store--read-item "Update: " #'lispm-desktop-store-update-available-p)))
  (let* ((installed (or (lispm-desktop-store--installed id)
                        (user-error "The App Store did not install %s" id)))
         (props (cdr (assq id (lispm-desktop-store-items)))))
    (unless (lispm-desktop-store-update-available-p id)
      (user-error "%s is up to date" (plist-get installed :name)))
    (when (lispm-desktop-store--busy-p id)
      (user-error "%s is being updated already" (plist-get installed :name)))
    (when (lispm-desktop-store--confirm (format "Update “%s”?" (plist-get props :name))
                                        "What changes is shown under the window."
                                        (lispm-desktop-store--changes installed props)
                                        "Update")
      (lispm-desktop-store--queue (list (cons id props)) 'update))))

(defun lispm-desktop-store--update-here (job)
  "Do JOB, an update, in this Emacs, which waits meanwhile."
  (pcase-let* ((`(,_ . ,props) (lispm-desktop-store-job-item job))
               (package (plist-get props :package))
               (rev (plist-get (cdr package) :rev)))
    (lispm-desktop-store--status (format "Updating %s…" (plist-get props :name)))
    (unwind-protect
        (lispm-desktop-store--quietly
          (lispm-desktop-store--repin (car package) rev))
      (lispm-desktop-store--status nil))
    (lispm-desktop-store--updated-as job rev)))

(defun lispm-desktop-store--updated-as (job rev)
  "End JOB, an update, its package checked out at REV: keep it, and load
an extension again."
  (pcase-let ((`(,id . ,props) (lispm-desktop-store-job-item job)))
    (lispm-desktop-store--keep id props :installed rev
                               :owned (plist-get (lispm-desktop-store--installed id) :owned))
    (when (eq (plist-get props :kind) 'extension)
      (lispm-desktop-store--unload (plist-get props :feature)))
    (if-let* ((failure (lispm-desktop-store--failure-of
                        (lambda () (lispm-desktop-store--register id (lispm-desktop-store--installed id))))))
        (lispm-desktop-store--fail job (format "It cannot be loaded: %s" failure))
      (lispm-desktop-store--done job))))

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
    (when (or (lispm-desktop-store-installed-p name) (lispm-desktop-store--busy-p name))
      (user-error "%s is installed already" name))
    (when (lispm-desktop-store--confirm-install (list (cons name props)))
      (lispm-desktop-store--queue (list (cons name props)) 'install))))

(defun lispm-desktop-store-install-from-url (url)
  "Install from the git repository at URL, once confirmed.
A repository holding a catalog.eld is a catalog, which joins the App
Store's; any other is an Emacs package, installed as an application
pinned to its newest commit.  The repository is looked at in the
background; what goes wrong is said in an alert."
  (interactive (list (read-string "Install from git URL: ")))
  (when (string-blank-p url)
    (user-error "No URL"))
  (let ((directory (make-temp-file "lispm-desktop-store-" t)))
    (lispm-desktop-store--status (format "Looking at %s…" url))
    (lispm-desktop-store--git-steps
     `((,directory "clone" "--quiet" "--depth" "1" ,url "repository"))
     (lambda (failure)
       (lispm-desktop-store--status nil)
       (run-at-time
        0 nil
        (lambda ()
          (unwind-protect
              (with-local-quit
                (condition-case err
                    (let ((repository (expand-file-name "repository" directory)))
                      (cond (failure (user-error "%s" failure))
                            ((file-exists-p (expand-file-name "catalog.eld" repository))
                             (lispm-desktop-store--add-catalog url))
                            (t (lispm-desktop-store--install-repository url repository))))
                  (user-error
                   (lispm-desktop-popup-alert 'stop (format "Nothing could be installed from %s." url)
                                              '(("OK" ok default))
                                              (error-message-string err)))))
            (delete-directory directory t))))))))

;;;; Applications and extensions taking part

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
       :setup (lambda () (lispm-desktop-app-ui-run id (plist-get props :command)))))))

(defun lispm-desktop-store--programs (id)
  "Return the programs the app ID uses, as its catalog declares them now,
else as it was installed with: a catalog may come to declare them after
the app was installed.  The catalogs are read from their clones, if they
were not yet."
  (unless lispm-desktop-store--catalogs
    (ignore-errors (lispm-desktop-store-read-catalogs)))
  (plist-get (cdr (assq id (lispm-desktop-store-items))) :programs))

(defun lispm-desktop-store--register-app (id props)
  "Make the installed application ID, of PROPS, one of the desktop's: its
window gets the desktop's interface, see lispm-desktop-app-ui.el, and its
menus with it."
  (if-let* ((feature (plist-get props :feature)))
      (require feature nil t)
    (lispm-desktop-define-app
     id :name (plist-get props :name) :icon (lispm-desktop-store--icon props)
     :category (lispm-desktop-store--category-of props) :description (plist-get props :description)
     :programs (lambda () (lispm-desktop-store--programs id)) :store t
     :menus (lambda () (lispm-desktop-app-ui-menus id))
     :activate (lambda () (lispm-desktop-store-open id))
     :visible (lambda () (lispm-desktop-app-window-open-p id)))))

(defun lispm-desktop-store--register (id props)
  "Make the installed item ID, of PROPS, take part: an application one of
the desktop's, an extension loaded."
  (pcase (plist-get props :kind)
    ('app (lispm-desktop-store--register-app id props))
    ('extension (require (plist-get props :feature)))))

(defun lispm-desktop-store--register-installed ()
  "Make the applications and extensions installed take part, as at every start."
  (pcase-dolist (`(,id . ,props) lispm-desktop-store-installed)
    (when (lispm-desktop-store--kind-p (cons id props))
      (condition-case err
          (lispm-desktop-store--register id props)
        (error (message "App Store: %s: %s" id (error-message-string err)))))))

(defun lispm-desktop-store-application-items ()
  "Return a menu item opening each application built in, see
`lispm-desktop-apps-built-in', then each installed, for the λ menu."
  (append
   (mapcar (pcase-lambda (`(,id . ,props))
             (list :label (plist-get props :name) :action (lambda () (lispm-desktop-apps-activate id))))
           (lispm-desktop-apps-built-in))
   (mapcan (pcase-lambda (`(,id . ,props))
             (when (eq (plist-get props :kind) 'app)
               (list (list :label (plist-get props :name)
                           :action (lambda () (lispm-desktop-apps-activate id))))))
           lispm-desktop-store-installed)))

(defun lispm-desktop-store--folder-items ()
  "Return the Applications folder's items: the App Store, the applications
built in, see `lispm-desktop-apps-built-in', then those installed, see
`lispm-desktop-finder-folders'."
  `(("App Store" "app-store-32" lispm-desktop-store-show)
    ,@(mapcar (pcase-lambda (`(,id . ,props))
                (list (plist-get props :name) (plist-get props :folder)
                      (lambda () (lispm-desktop-apps-activate id))))
              (lispm-desktop-apps-built-in))
    ,@(mapcan (pcase-lambda (`(,id . ,props))
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
    (buffer-local-value 'lispm-desktop-store--selected buffer)))

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

;;;;; What shows

(defun lispm-desktop-store--in-category-p (item category)
  "Return non-nil when ITEM, (ID . PROPERTIES), is one of CATEGORY's."
  (pcase category
    ('all t)
    ('updates (lispm-desktop-store-update-available-p (car item)))
    (_ (eq (lispm-desktop-store--category-of (cdr item)) category))))

(defun lispm-desktop-store--matches-p (item filter)
  "Return non-nil when FILTER is nil, or ITEM's name, subtitle or
description holds it, whatever its case."
  (or (null filter)
      (seq-some (lambda (key)
                  (let ((text (plist-get (cdr item) key)))
                    (and (stringp text) (string-search (downcase filter) (downcase text)))))
                '(:name :subtitle :description))))

(defun lispm-desktop-store--categories ()
  "Return the categories listed down the window's left: All, those holding
an item, and Updates, as (CATEGORY NAME COUNT)."
  (let ((items (lispm-desktop-store-items)))
    (delq nil (mapcar (pcase-lambda (`(,category ,name))
                        (let ((count (seq-count (lambda (item) (lispm-desktop-store--in-category-p item category))
                                                items)))
                          (when (or (memq category '(all updates)) (> count 0))
                            (list category name count))))
                      lispm-desktop-store-categories))))

(defun lispm-desktop-store--category-name (category)
  "Return the name of CATEGORY, or nil."
  (cadr (assq category lispm-desktop-store-categories)))

(defun lispm-desktop-store--shown ()
  "Return the items the window shows: its category's matching its search,
by name."
  (sort (seq-filter (lambda (item)
                      (and (lispm-desktop-store--in-category-p item lispm-desktop-store--category)
                           (lispm-desktop-store--matches-p item lispm-desktop-store--filter)))
                    (lispm-desktop-store-items))
        (lambda (a b) (string-lessp (downcase (plist-get (cdr a) :name))
                                    (downcase (plist-get (cdr b) :name))))))

(defun lispm-desktop-store--empty-text ()
  "Return what the grid says when it shows nothing."
  (cond (lispm-desktop-store--filter (format "Nothing matches “%s”." lispm-desktop-store--filter))
        ((eq lispm-desktop-store--category 'updates) "Everything is up to date.")
        (t "Nothing here yet.")))

;;;;; Geometry (pure)

(defun lispm-desktop-store--grid (width)
  "Return the grid's layout in a window WIDTH pixels wide: (X COLUMNS
TILE-WIDTH), X where it starts, right of the categories."
  (let* ((x (+ lispm-desktop-store-margin lispm-desktop-store-sidebar-width lispm-desktop-store-margin))
         (room (max lispm-desktop-store-tile-width (- width x lispm-desktop-store-margin)))
         (columns (max 1 (/ room lispm-desktop-store-tile-width))))
    (list x columns (/ room columns))))

(defun lispm-desktop-store--categories-height (count)
  "Return the height in pixels of the list of COUNT categories."
  (+ 2 (* count lispm-desktop-store-category-height)))

(defun lispm-desktop-store--lines (count columns categories)
  "Return how many lines the grid of COUNT items in COLUMNS takes: at least
one, and enough for the list of CATEGORIES, a count, beside it."
  (max 1
       (ceiling count (float columns))
       (ceiling (lispm-desktop-store--categories-height categories)
                (float lispm-desktop-store-tile-height))))

(defun lispm-desktop-store--button-box (x width label)
  "Return where the button reading LABEL of the item drawn at X, WIDTH
pixels wide, stands: (X Y WIDTH), Y from the top of the item's line."
  (let ((w (lispm-desktop-platinum-push-button-width label)))
    (list (+ x (/ (- width w) 2)) 84 w)))

(defun lispm-desktop-store--hit (width line x y categories items)
  "Return what stands at X, Y of the grid's LINE, the window WIDTH pixels
wide, listing CATEGORIES beside ITEMS: (category . CATEGORY), (button .
ID), the button of the item ID, (item . ID), or nil.  The progress bar
standing in place of a button is the item's."
  (pcase-let* ((`(,gx ,columns ,tw) (lispm-desktop-store--grid width))
               (top (+ (* line lispm-desktop-store-tile-height) y)))
    (cond
     ((and (>= x lispm-desktop-store-margin)
           (< x (+ lispm-desktop-store-margin lispm-desktop-store-sidebar-width)))
      (let ((index (floor (1- top) lispm-desktop-store-category-height)))
        (when (and (>= top 1) (< index (length categories)))
          (cons 'category (car (nth index categories))))))
     ((>= x gx)
      (let* ((column (floor (- x gx) tw))
             (item (and (< column columns) (nth (+ (* line columns) column) items))))
        (when item
          (pcase-let ((`(,bx ,by ,bw) (lispm-desktop-store--button-box
                                       (+ gx (* column tw)) tw
                                       (car (lispm-desktop-store--action (car item) (cdr item))))))
            (if (and (not (lispm-desktop-store--busy-p (car item)))
                     (lispm-desktop-modal-inside-p (list bx by bw lispm-desktop-platinum-button-height) x y))
                (cons 'button (car item))
              (cons 'item (car item))))))))))

;;;;; Drawing

(defun lispm-desktop-store--selected-face ()
  "Return the colours of what is selected: the light highlight colour, as
Mac OS 9 drew a selected name, and whichever of the theme's text and
background colours reads on it, as (:background COLOR :foreground COLOR)."
  (let ((highlight (or (lispm-desktop-platinum-color :thumb-light)
                       (lispm-desktop-platinum-color :highlight))))
    `(:background ,highlight
      :foreground ,(lispm-desktop-platinum-legible highlight
                                                    (lispm-desktop-platinum-color :text)
                                                    (lispm-desktop-platinum-color :content)))))

(defun lispm-desktop-store--fit (text width face)
  "Return TEXT in FACE, shortened to WIDTH pixels."
  (lispm-desktop-platinum-fit (propertize text 'face face) (max 0 width)))

(defun lispm-desktop-store--darkened (file)
  "Return the SVG document of the icon FILE darkened, as Mac OS 9 darkened
a selected icon."
  (lispm-desktop-platinum-svg-dom
   nil (lispm-desktop--darken (with-temp-buffer
                                (insert-file-contents file)
                                (buffer-string)))))

(defun lispm-desktop-store--draw-categories (svg palette categories current top)
  "Draw on SVG the list of CATEGORIES, CURRENT highlighted, from TOP down.
The list is drawn whole on every line of the grid, each line moving it
up by the lines above, so that together they show it once."
  (let* ((x lispm-desktop-store-margin)
         (w lispm-desktop-store-sidebar-width)
         (rh lispm-desktop-store-category-height)
         (face 'lispm-desktop-platinum-list)
         (small 'lispm-desktop-platinum-small)
         (selected (lispm-desktop-store--selected-face)))
    (lispm-desktop-platinum-svg-fill svg x top w (lispm-desktop-store--categories-height (length categories))
                                  (plist-get palette :outline))
    (lispm-desktop-platinum-svg-fill svg (1+ x) (1+ top) (- w 2) (* rh (length categories))
                                  (plist-get palette :content))
    (seq-do-indexed
     (pcase-lambda (`(,category ,name ,count) index)
       (let* ((y (+ top 1 (* index rh)))
              (on (eq category current))
              (ink (if on (plist-get selected :foreground) (plist-get palette :text)))
              (number (format "%d" count)))
         (when on
           (lispm-desktop-platinum-svg-fill svg (1+ x) y (- w 2) rh (plist-get selected :background)))
         (lispm-desktop-platinum-svg-content svg (propertize name 'face face) (+ x 8) y rh ink face)
         (lispm-desktop-platinum-svg-content
          svg number (- (+ x w) 8 (lispm-desktop-platinum-content-width number small)) y rh
          (if on ink (lispm-desktop-platinum-grey palette #x77)) small)))
     categories)))

(defun lispm-desktop-store--tile (item)
  "Return what the grid draws of ITEM, (ID . PROPERTIES), as a plist: its
:icon, :name, :subtitle, its button's :label, whether it is :enabled and
:pressed, and whether it is :selected.  While it is installed or
updated, or waits to be, :progress is how far, and its button is not
drawn; the subtitle says what it is doing, or that it failed."
  (pcase-let* ((`(,id . ,props) item)
               (`(,label . ,action) (lispm-desktop-store--action id props))
               (job (lispm-desktop-store--job id)))
    (list :id id :icon (lispm-desktop-store--icon props t) :name (plist-get props :name)
          :subtitle (cond ((null job)
                           (or (plist-get props :subtitle)
                               (lispm-desktop-store--category-name (lispm-desktop-store--category-of props))))
                          ((eq (lispm-desktop-store-job-state job) 'failed)
                           (if (eq (lispm-desktop-store-job-action job) 'install)
                               "Could not install"
                             "Could not update"))
                          (t (concat (nth 1 (assq (lispm-desktop-store-job-stage job) lispm-desktop-store-stages))
                                     "…")))
          :progress (and (lispm-desktop-store--busy-p id)
                         ;; Steps of a hundredth: no more images than that.
                         (/ (round (* 100 (lispm-desktop-store-job-fraction job))) 100.0))
          :label label :enabled (and action t)
          :pressed (eq id lispm-desktop-store--pressed)
          :selected (eq id lispm-desktop-store--selected))))

(defun lispm-desktop-store--progress-box (x width)
  "Return where the progress bar of the item drawn at X, WIDTH pixels wide,
stands, in place of its button: (X Y WIDTH), Y from the top of the line."
  (let ((w (min 120 (- width 20))))
    (list (+ x (/ (- width w) 2))
          (+ 84 (/ (- lispm-desktop-platinum-button-height lispm-desktop-platinum-progress-height) 2))
          w)))

(defun lispm-desktop-store--draw-tile (svg palette tile x width)
  "Draw TILE, as `lispm-desktop-store--tile' makes it, on SVG at X, WIDTH
pixels wide: its icon, darkened when selected, its name, highlighted
when selected, its subtitle and its button, or its progress bar."
  (let* ((center (+ x (/ width 2)))
         (file (lispm-desktop-platinum-icon-file (plist-get tile :icon)))
         (selected (plist-get tile :selected))
         (highlight (lispm-desktop-store--selected-face))
         (face 'lispm-desktop-platinum-list)
         (small 'lispm-desktop-platinum-small))
    (cond ((not (file-readable-p file)))
          ((not (string-suffix-p ".svg" file))
           (lispm-desktop-platinum-svg-emacs-image svg (create-image file) (- center 16) 8 32 32))
          (selected
           (when-let* ((dom (lispm-desktop-store--darkened file)))
             (lispm-desktop-platinum-svg-embed svg dom (- center 16) 8 32 32)))
          (t (lispm-desktop-platinum-svg-icon svg file (- center 16) 8 32)))
    (let* ((name (lispm-desktop-store--fit (plist-get tile :name) (- width 10) face))
           (w (lispm-desktop-platinum-content-width name face))
           (nx (- center (/ w 2))))
      (when selected
        (lispm-desktop-platinum-svg-fill svg (- nx 3) 44 (+ w 6) 16 (plist-get highlight :background)))
      (lispm-desktop-platinum-svg-content svg name nx 44 16
                                            (if selected (plist-get highlight :foreground) (plist-get palette :text))
                                            face))
    (when-let* ((subtitle (plist-get tile :subtitle)))
      (let* ((text (lispm-desktop-store--fit subtitle (- width 10) small))
             (w (lispm-desktop-platinum-content-width text small)))
        (lispm-desktop-platinum-svg-content svg text (- center (/ w 2)) 61 14
                                              (lispm-desktop-platinum-grey palette #x66) small)))
    (if-let* ((progress (plist-get tile :progress)))
        (pcase-let ((`(,px ,py ,pw) (lispm-desktop-store--progress-box x width)))
          (lispm-desktop-platinum-svg-progress-bar svg px py pw progress palette))
      (pcase-let ((`(,bx ,by ,bw) (lispm-desktop-store--button-box x width (plist-get tile :label))))
        (lispm-desktop-platinum-svg-push-button svg bx by palette (plist-get tile :label) bw
                                                  :state (cond ((not (plist-get tile :enabled)) 'disabled)
                                                               ((plist-get tile :pressed) 'pressed)))))))

(defun lispm-desktop-store--draw-line (palette width line categories current tiles empty)
  "Return an SVG of the grid's LINE, WIDTH pixels wide, in PALETTE: its
part of the list of CATEGORIES, CURRENT highlighted, and TILES, its
items as `lispm-desktop-store--tile' makes them; EMPTY, when non-nil,
says why there are none."
  (pcase-let* ((`(,gx ,_ ,tw) (lispm-desktop-store--grid width))
               (height lispm-desktop-store-tile-height)
               (svg (svg-create width height)))
    (lispm-desktop-platinum-svg-fill svg 0 0 width height (plist-get palette :chrome))
    (lispm-desktop-store--draw-categories svg palette categories current (- (* line height)))
    (seq-do-indexed (lambda (tile column)
                      (lispm-desktop-store--draw-tile svg palette tile (+ gx (* column tw)) tw))
                    tiles)
    (when empty
      (let* ((face 'lispm-desktop-platinum-list)
             (w (lispm-desktop-platinum-content-width empty face)))
        (lispm-desktop-platinum-svg-content
         svg empty (+ gx (/ (- width gx lispm-desktop-store-margin w) 2)) 40 16
         (lispm-desktop-platinum-grey palette #x66) face)))
    svg))

(defun lispm-desktop-store--insert-grid (width)
  "Insert the categories and the grid of the items shown, a window WIDTH
pixels wide: a line of items each, drawn as one image."
  (pcase-let* ((`(,_ ,columns ,_) (lispm-desktop-store--grid width))
               (categories (lispm-desktop-store--categories))
               (items (lispm-desktop-store--shown))
               (empty (and (null items) (lispm-desktop-store--empty-text)))
               (current lispm-desktop-store--category))
    (dotimes (line (lispm-desktop-store--lines (length items) columns (length categories)))
      (let ((tiles (mapcar #'lispm-desktop-store--tile
                           (seq-take (nthcdr (* line columns) items) columns)))
            (empty (and (= line 0) empty)))
        (insert (propertize " " 'display
                            (lispm-desktop-platinum-image
                             (list 'store-line width line categories current tiles empty)
                             (lambda ()
                               (lispm-desktop-store--draw-line lispm-desktop-platinum-current-palette width line
                                                               categories current tiles empty)))
                            'lispm-desktop-store-line line
                            'keymap lispm-desktop-store-grid-map
                            'pointer 'arrow)
                "\n")))))

(defun lispm-desktop-store--rule (width &optional bottom)
  "Return a line WIDTH pixels wide, as an image: black, lit below when
BOTTOM, else etched."
  (propertize
   " " 'display
   (lispm-desktop-platinum-image
    (list 'store-rule width bottom)
    (lambda ()
      (let* ((palette lispm-desktop-platinum-current-palette)
             (svg (svg-create (+ width 2) 2)))
        (lispm-desktop-platinum-svg-fill svg 0 0 (+ width 2) 2 (plist-get palette :chrome))
        (if bottom
            (progn (lispm-desktop-platinum-svg-row svg 0 (1+ width) 0 (plist-get palette :outline))
                   (lispm-desktop-platinum-svg-row svg 0 (1+ width) 1 (lispm-desktop-platinum-grey palette #xFF)))
          (lispm-desktop-platinum-svg-row svg 0 (1+ width) 1 (lispm-desktop-platinum-grey palette #xAA)))
        svg)))))

(defun lispm-desktop-store--button-width (label &optional default)
  "Return the width in pixels a push button reading LABEL takes on its line,
its rings' room included: the default button's ring too when DEFAULT."
  (or (ignore-errors
        (car (image-size (lispm-desktop-platinum-image-of
                          (lispm-desktop-platinum-push-button label :default default))
                         t)))
      (+ (lispm-desktop-platinum-push-button-width label) (if default 16 10))))

(defun lispm-desktop-store--insert-top (width)
  "Insert the row above the grid: the catalogs listed, or what the items
shown match, and Check for Updates."
  (insert (propertize " " 'display '(space :height (8))) "\n")
  (let ((label "Check for Updates")
        (bold '(:inherit lispm-desktop-platinum-list :weight bold)))
    (insert (propertize " " 'display `(space :width (,lispm-desktop-store-margin)))
            (if lispm-desktop-store--filter
                (concat (propertize "Search: " 'face bold)
                        (lispm-desktop-store--fit (format "“%s”" lispm-desktop-store--filter)
                                                  (- width 140 (lispm-desktop-platinum-push-button-width label))
                                                  'lispm-desktop-platinum-list))
              (concat (propertize "Catalogs: " 'face bold)
                      (lispm-desktop-store--fit (if lispm-desktop-store--catalogs
                                                    (mapconcat (lambda (catalog) (plist-get catalog :name))
                                                               lispm-desktop-store--catalogs ", ")
                                                  "none read yet")
                                                (- width 140 (lispm-desktop-platinum-push-button-width label))
                                                'lispm-desktop-platinum-list)))
            (propertize " " 'display `(space :align-to (- right (,(+ lispm-desktop-store-margin
                                                                    (lispm-desktop-store--button-width label)))))))
    (lispm-desktop-platinum-insert-push-button label #'lispm-desktop-store-check-for-updates
                                               :help "Fetch the catalogs again"
                                               :disabled lispm-desktop-store--checking)
    (insert "\n" (propertize " " 'display '(space :height (8))) "\n")))

(defun lispm-desktop-store--insert-buttons (width)
  "Insert the push buttons under the grid, flush with its right edge:
Install from URL…, Show Log for an item that failed to install or
update, Copy Install Command for an extension whose program is missing,
Remove, and the selected item's button, Get, Open, Update, Stop or Try
Again."
  (let* ((id lispm-desktop-store--selected)
         (props (and id (cdr (assq id (lispm-desktop-store-items)))))
         (action (and props (lispm-desktop-store--action id props)))
         (job (and id (lispm-desktop-store--job id)))
         (buttons `(("Install from URL…" lispm-desktop-store-install-from-url nil)
                    ,@(when (and job (eq (lispm-desktop-store-job-state job) 'failed)
                                 (buffer-live-p (lispm-desktop-store-job-log job)))
                        '(("Show Log" lispm-desktop-store-show-log-selected nil)))
                    ,@(when (and props (lispm-desktop-store--install-commands props))
                        '(("Copy Install Command" lispm-desktop-store-copy-install-command nil)))
                    ("Remove" lispm-desktop-store-remove-selected
                     ,(not (and id (lispm-desktop-store-installed-p id) (not (lispm-desktop-store--busy-p id)))))
                    (,(or (car action) "Get") lispm-desktop-store-act-selected
                     ,(not (cdr action)) t)))
         (gap 10)
         (room (+ (apply #'+ (mapcar (lambda (button)
                                       (lispm-desktop-store--button-width (car button) (nth 3 button)))
                                     buttons))
                  (* gap (1- (length buttons))))))
    (ignore width)
    (insert (propertize " " 'display '(space :height (6))) "\n"
            (propertize " " 'display `(space :align-to (- right (,(+ lispm-desktop-store-margin room))))))
    (seq-do-indexed
     (pcase-lambda (`(,label ,command ,disabled ,default) index)
       (when (> index 0)
         (insert (propertize " " 'display `(space :width (,gap)))))
       (lispm-desktop-platinum-insert-push-button label (lambda () (call-interactively command))
                                                  :disabled disabled :default default))
     buttons)
    (insert "\n")))

(defun lispm-desktop-store--pieces (word width face)
  "Return WORD in FACE cut into pieces of at most WIDTH pixels, after a
slash or a hyphen where one is: a file name or a URL, as an error names.
A word that fits is its own piece."
  (let ((pieces nil))
    (cl-flet ((fits (text) (<= (string-pixel-width (propertize text 'face face)) width)))
      (while (not (fits word))
        (let* ((fit (let ((n (1- (length word))))
                      (while (and (> n 1) (not (fits (substring word 0 n))))
                        (setq n (1- n)))
                      n))
               (cut (or (when-let* ((at (string-match-p "[/-][^/-]*\\'" (substring word 0 fit)))
                                    ((> at 0)))
                          (1+ at))
                        fit)))
          (push (substring word 0 cut) pieces)
          (setq word (substring word cut)))))
    (nreverse (cons word pieces))))

(defun lispm-desktop-store--wrap (text width face)
  "Return TEXT in FACE broken into lines of at most WIDTH pixels; a word
too long for a line is cut, see `lispm-desktop-store--pieces'."
  (let ((lines nil)
        (line ""))
    (dolist (word (mapcan (lambda (word) (lispm-desktop-store--pieces word width face))
                          (split-string (or text "") "[ \t\n]+" t)))
      (let ((longer (if (string-empty-p line) word (concat line " " word))))
        (if (or (string-empty-p line)
                (<= (string-pixel-width (propertize longer 'face face)) width))
            (setq line longer)
          (push line lines)
          (setq line word))))
    (unless (string-empty-p line)
      (push line lines))
    (nreverse lines)))

(defun lispm-desktop-store--where (props)
  "Return where the item of PROPS, installed, lives, in words."
  (let ((package (lispm-desktop-store--package-name (plist-get props :package)))
        (feature (plist-get props :feature)))
    (cond ((when-let* ((desc (and package (cadr (assq package package-alist)))))
             (abbreviate-file-name (package-desc-dir desc))))
          ((when-let* ((library (and feature (locate-library (symbol-name feature)))))
             (abbreviate-file-name (file-name-directory library))))
          ((null package) "Emacs")
          (t "–"))))

(defun lispm-desktop-store--fields (id)
  "Return the information shown of the item ID, as (LABEL VALUE)."
  (let* ((props (cdr (assq id (lispm-desktop-store-items))))
         (installed (lispm-desktop-store--installed id))
         (program (lispm-desktop-store--missing-program props)))
    `(("Category:" ,(or (lispm-desktop-store--category-name (lispm-desktop-store--category-of props)) "–"))
      ("Version:" ,(lispm-desktop-store--version id props))
      ("Status:" ,(cond ((lispm-desktop-store--job id) (lispm-desktop-store--progress (lispm-desktop-store--job id)))
                        ((lispm-desktop-store-update-available-p id) "Installed; an update is ready")
                        (installed "Installed")
                        (program (format "Needs %s, not on PATH" program))
                        (t "Not installed")))
      ("From:" ,(lispm-desktop-store--origin props))
      ("Where:" ,(if installed (lispm-desktop-store--where installed) "–")))))

(defun lispm-desktop-store--description (props)
  "Return the description of the item of PROPS, with how to install the
program it needs when PATH lacks it, and what of the programs an app uses
is missing."
  (concat (or (plist-get props :description) "")
          (when-let* ((program (lispm-desktop-store--missing-program props)))
            (format "  It needs %s, which is not on PATH%s" program
                    (if-let* ((install (plist-get props :install)))
                        (format "; install it with: %s" install)
                      ".")))
          (mapconcat (pcase-lambda (`(,name ,state ,properties))
                       (unless (eq state 'present)
                         (concat "  " (lispm-desktop-app-ui-program-text "It" name state properties))))
                     (lispm-desktop-app-ui-program-states (plist-get props :programs)))
          (when-let* (((not (lispm-desktop-store--missing-program props)))
                      (install (lispm-desktop-store--install-commands props)))
            (format "  Install them with: %s" install))))

(defun lispm-desktop-store--draw-information (palette width id)
  "Return an SVG of the information of the item ID, WIDTH pixels wide, in
PALETTE, laid out as Extensions Manager's: the icon and name, the fields
under them, the description in a box at the right."
  (let* ((height 128)
         (svg (svg-create width height))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level)))
         (text (plist-get palette :text))
         (small 'lispm-desktop-platinum-small)
         (props (and id (cdr (assq id (lispm-desktop-store-items)))))
         (box-x (max 220 (/ width 2)))
         (box-w (- width box-x lispm-desktop-store-margin)))
    (lispm-desktop-platinum-svg-fill svg 0 0 width height (plist-get palette :chrome))
    (when props
      (let ((icon (lispm-desktop-platinum-icon-file (lispm-desktop-store--icon props t))))
        (if (string-suffix-p ".svg" icon)
            (lispm-desktop-platinum-svg-icon svg icon lispm-desktop-store-margin 4 32)
          (lispm-desktop-platinum-svg-emacs-image svg (create-image icon) lispm-desktop-store-margin 4 32 32)))
      (lispm-desktop-platinum-svg-content svg (plist-get props :name) (+ lispm-desktop-store-margin 40) 6 14 text small)
      (lispm-desktop-platinum-svg-content
       svg (lispm-desktop-store--catalog-name (plist-get props :catalog))
       (+ lispm-desktop-store-margin 40) 20 14 text small)
      (let ((y 44)
            (label-end (+ lispm-desktop-store-margin 66)))
        (pcase-dolist (`(,label ,value) (lispm-desktop-store--fields id))
          (let ((bold (propertize label 'face 'bold)))
            (lispm-desktop-platinum-svg-content
             svg bold (- label-end (lispm-desktop-platinum-content-width bold small)) y 13 text small)
            (lispm-desktop-platinum-svg-content
             svg (lispm-desktop-platinum-fit (propertize value 'face small) (- box-x label-end 18))
             (+ label-end 4) y 13 text small))
          (setq y (+ y 13)))))
    ;; The box, sunk in the platinum: etched, outlined, white.
    (lispm-desktop-platinum-svg-fill svg (1- box-x) 3 (+ box-w 2) (- height 7) (funcall grey #xAA))
    (lispm-desktop-platinum-svg-fill svg box-x 4 (1+ box-w) (- height 7) (funcall grey #xFF))
    (lispm-desktop-platinum-svg-fill svg box-x 4 box-w (- height 8) (plist-get palette :outline))
    (lispm-desktop-platinum-svg-fill svg (1+ box-x) 5 (- box-w 2) (- height 10) (plist-get palette :content))
    (when props
      (let ((y 8))
        (dolist (line (cons (propertize (plist-get props :name) 'face 'bold)
                            (lispm-desktop-store--wrap (concat (lispm-desktop-store--failure id)
                                                               (lispm-desktop-store--description props))
                                                       (- box-w 12) small)))
          (when (< y (- height 20))
            (lispm-desktop-platinum-svg-content svg line (+ box-x 6) y 13 text small)
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
                      'keymap lispm-desktop-store-disclosure-map
                      'face '(:inherit lispm-desktop-platinum-small :weight bold)
                      'help-echo "Show or hide the item's information"
                      'lispm-desktop-platinum-label "Item Information")
    (insert "\n"))
  (insert (propertize " " 'display `(space :width (,lispm-desktop-store-margin)))
          (lispm-desktop-store--rule (- width (* 2 lispm-desktop-store-margin) 2))
          "\n")
  (when lispm-desktop-store--information
    (let ((id lispm-desktop-store--selected))
      (insert (propertize " " 'display
                          (lispm-desktop-platinum-image
                           (list 'store-information width id (lispm-desktop-store--information-key id))
                           (lambda ()
                             (lispm-desktop-store--draw-information lispm-desktop-platinum-current-palette width id))))
              "\n"))))

(defun lispm-desktop-store--information-key (id)
  "Return what the information of the item ID depends on, to draw it again
when it changes."
  (and id (let ((props (cdr (assq id (lispm-desktop-store-items)))))
            (list (lispm-desktop-store--fields id) props (lispm-desktop-store--description props)
                  (lispm-desktop-store--failure id)
                  (lispm-desktop-store--catalog-name (plist-get props :catalog))))))

(defun lispm-desktop-store--failure (id)
  "Return why installing or updating the item ID failed, as sentences to
put before its description, or \"\"."
  (let ((job (lispm-desktop-store--job id)))
    (if (and job (eq (lispm-desktop-store-job-state job) 'failed))
        (format "It could not be %s: %s.  "
                (if (eq (lispm-desktop-store-job-action job) 'install) "installed" "updated")
                (string-trim-right (lispm-desktop-store-job-error job) "[ .]+"))
      "")))

(defun lispm-desktop-store--placard ()
  "Return what the placard says: the App Store's doing, what it installs
or updates, or its count."
  (or lispm-desktop-store--status-text
      (when-let* ((job (seq-find (lambda (job) (eq (lispm-desktop-store-job-state job) 'running))
                                 lispm-desktop-store--jobs)))
        (let ((waiting (seq-count (lambda (job) (eq (lispm-desktop-store-job-state job) 'waiting))
                                  lispm-desktop-store--jobs)))
          (format "%s “%s”… %d%%%s"
                  (if (eq (lispm-desktop-store-job-action job) 'install) "Installing" "Updating")
                  (plist-get (cdr (lispm-desktop-store-job-item job)) :name)
                  (round (* 100 (lispm-desktop-store-job-fraction job)))
                  (if (> waiting 0) (format ", %d waiting" waiting) ""))))
      (let* ((items (lispm-desktop-store-items))
             (installed (seq-count (lambda (item) (lispm-desktop-store-installed-p (car item))) items)))
        (format "%d item%s, %d installed" (length items) (if (= (length items) 1) "" "s") installed))))

(defun lispm-desktop-store--render ()
  "Draw the App Store's window again, the button point was on kept."
  (when-let* ((buffer (get-buffer lispm-desktop-store-buffer-name)))
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
             (window (get-buffer-window buffer t))
             (width (if window (window-body-width window t) 640))
             (button (get-text-property (if window (window-point window) (point))
                                        'lispm-desktop-platinum-label)))
        (setq lispm-desktop-store--width width)
        (unless (assq lispm-desktop-store--category (lispm-desktop-store--categories))
          (setq lispm-desktop-store--category 'all))
        (let ((shown (lispm-desktop-store--shown)))
          (unless (assq lispm-desktop-store--selected shown)
            (setq lispm-desktop-store--selected (caar shown))))
        (when lispm-desktop-store--face-cookie
          (face-remap-remove-relative lispm-desktop-store--face-cookie))
        (setq lispm-desktop-store--face-cookie
              (face-remap-add-relative 'default `(:background ,(lispm-desktop-platinum-color :chrome))))
        (erase-buffer)
        (lispm-desktop-store--insert-top width)
        (lispm-desktop-store--insert-grid width)
        (lispm-desktop-store--insert-buttons width)
        (lispm-desktop-store--insert-information width)
        (goto-char (point-min))
        (when button
          (let ((position (point-min)))
            (while (and (setq position (next-single-property-change position 'lispm-desktop-platinum-label))
                        (not (equal (get-text-property position 'lispm-desktop-platinum-label) button))))
            (goto-char (or position (point-min)))))
        (when window
          (set-window-point window (point)))
        (setq lispm-desktop-platinum-focus nil)
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

(defun lispm-desktop-store--select (id)
  "Select the item ID and draw the window again."
  (lispm-desktop-store--in-window
   (lambda ()
     (unless (eq id lispm-desktop-store--selected)
       (setq lispm-desktop-store--selected id)
       (lispm-desktop-store--render)))))

(defun lispm-desktop-store--columns ()
  "Return how many items a line of the grid holds, as last drawn."
  (nth 1 (lispm-desktop-store--grid (or lispm-desktop-store--width 640))))

(defun lispm-desktop-store--move (step)
  "Select the item STEP places from the one selected in the grid, the
first or the last past either end."
  (lispm-desktop-store--in-window
   (lambda ()
     (when-let* ((ids (mapcar #'car (lispm-desktop-store--shown))))
       (let ((index (seq-position ids lispm-desktop-store--selected)))
         (lispm-desktop-store--select
          (nth (if index (max 0 (min (1- (length ids)) (+ index step))) 0) ids)))))))

(defun lispm-desktop-store-forward ()
  "Select the next item of the grid."
  (interactive)
  (lispm-desktop-store--move 1))

(defun lispm-desktop-store-backward ()
  "Select the previous item of the grid."
  (interactive)
  (lispm-desktop-store--move -1))

(defun lispm-desktop-store-down ()
  "Select the item under the one selected in the grid."
  (interactive)
  (lispm-desktop-store--move (lispm-desktop-store--in-window #'lispm-desktop-store--columns)))

(defun lispm-desktop-store-up ()
  "Select the item over the one selected in the grid."
  (interactive)
  (lispm-desktop-store--move (- (lispm-desktop-store--in-window #'lispm-desktop-store--columns))))

(defun lispm-desktop-store-show-category (category)
  "Show the items of CATEGORY, a key of `lispm-desktop-store-categories'."
  (interactive (list (lispm-desktop-store--in-window
                      (lambda ()
                        (let ((names (mapcar (pcase-lambda (`(,category ,name . ,_)) (cons name category))
                                             (lispm-desktop-store--categories))))
                          (cdr (assoc (completing-read "Category: " names nil t) names)))))))
  (lispm-desktop-store--in-window
   (lambda ()
     (setq lispm-desktop-store--category category)
     (lispm-desktop-store--render))))

(defun lispm-desktop-store-next-category (&optional back)
  "Show the next category's items, or the previous one's when BACK."
  (interactive)
  (lispm-desktop-store--in-window
   (lambda ()
     (let* ((categories (mapcar #'car (lispm-desktop-store--categories)))
            (index (or (seq-position categories lispm-desktop-store--category) 0)))
       (lispm-desktop-store-show-category
        (nth (mod (+ index (if back -1 1)) (length categories)) categories))))))

(defun lispm-desktop-store-previous-category ()
  "Show the previous category's items."
  (interactive)
  (lispm-desktop-store-next-category t))

(defun lispm-desktop-store-search (text)
  "Show only the items whose name, subtitle or description holds TEXT;
an empty TEXT shows every item again."
  (interactive (list (read-string "Search the App Store: "
                                  (lispm-desktop-store--in-window (lambda () lispm-desktop-store--filter)))))
  (lispm-desktop-store--in-window
   (lambda ()
     (setq lispm-desktop-store--filter (unless (string-blank-p text) (string-trim text)))
     (lispm-desktop-store--render))))

(defun lispm-desktop-store-act (id)
  "Press the button of the item ID: get it, open it, update it, or stop
installing or updating it."
  (interactive (list (lispm-desktop-store--read-item "Item: ")))
  (let* ((props (cdr (assq id (lispm-desktop-store-items)))))
    (pcase (cdr (lispm-desktop-store--action id props))
      ('install (lispm-desktop-store-install id))
      ('open (lispm-desktop-store-open id))
      ('update (lispm-desktop-store-update id))
      ('stop (lispm-desktop-store-stop id))
      (_ (lispm-desktop-store--check-program props)
         (when (lispm-desktop-store--busy-p id)
           (user-error "%s can no longer be stopped" (plist-get props :name)))
         (user-error "%s is installed" (plist-get props :name))))))

(defun lispm-desktop-store-act-selected ()
  "Press the button of the item selected: Get, Open or Update."
  (interactive)
  (if-let* ((id (lispm-desktop-store--selected-id)))
      (lispm-desktop-store-act id)
    (user-error "Select an item first")))

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

(defun lispm-desktop-store-stop-selected ()
  "Stop installing or updating the item selected, as Command-period
stopped what Mac OS 9 was doing."
  (interactive)
  (if-let* ((id (lispm-desktop-store--selected-id)))
      (lispm-desktop-store-stop id)
    (user-error "Select an item first")))

(defun lispm-desktop-store-show-log-selected ()
  "Show the log of the item selected, which failed to install or update."
  (interactive)
  (if-let* ((id (lispm-desktop-store--selected-id)))
      (lispm-desktop-store-show-log id)
    (user-error "Select an item first")))

(defun lispm-desktop-store--install-commands (props)
  "Return the command installing what the item of PROPS uses and PATH lacks,
or nil: an extension's program, an app's programs, theirs joined."
  (let ((commands (if (lispm-desktop-store--missing-program props)
                      (ensure-list (plist-get props :install))
                    (delq nil (mapcar (pcase-lambda (`(,_ ,state ,properties))
                                        (unless (eq state 'present) (plist-get properties :install)))
                                      (lispm-desktop-app-ui-program-states (plist-get props :programs)))))))
    (and commands (string-join commands " && "))))

(defun lispm-desktop-store-copy-install-command ()
  "Copy the command installing what the item selected uses and PATH lacks."
  (interactive)
  (let* ((id (lispm-desktop-store--selected-id))
         (install (and id (lispm-desktop-store--install-commands (cdr (assq id (lispm-desktop-store-items)))))))
    (unless install
      (user-error "Nothing this item uses is missing"))
    (kill-new install)
    (message "Copied: %s" install)))

(defun lispm-desktop-store-toggle-information ()
  "Show or hide the information of the item selected."
  (interactive)
  (lispm-desktop-store--in-window
   (lambda ()
     (setq lispm-desktop-store--information (not lispm-desktop-store--information))
     (lispm-desktop-store--render))))

(defun lispm-desktop-store--grid-event (event)
  "Return what the mouse EVENT on the grid is on, see `lispm-desktop-store--hit'."
  (let* ((position (event-start event))
         (buffer (window-buffer (posn-window position)))
         (xy (posn-object-x-y position))
         (line (and (posn-point position)
                    (get-text-property (posn-point position) 'lispm-desktop-store-line buffer))))
    (when (and line xy)
      (with-current-buffer buffer
        (lispm-desktop-store--hit lispm-desktop-store--width line (car xy) (cdr xy)
                                  (lispm-desktop-store--categories) (lispm-desktop-store--shown))))))

(defun lispm-desktop-store--grid-click (event)
  "Act on the click EVENT on the grid: show the category clicked, select
the item clicked, or press the button clicked."
  (interactive "e")
  (pcase (lispm-desktop-store--grid-event event)
    (`(category . ,category) (lispm-desktop-store-show-category category))
    (`(item . ,id) (lispm-desktop-store--select id))
    (`(button . ,id) (lispm-desktop-store--select id)
     (lispm-desktop-store-act id))))

(defun lispm-desktop-store--grid-press (event)
  "Hold the button of an item pressed with the mouse EVENT pushed in while
the mouse stays over it, and press it when the mouse button rises there,
as Mac OS 9's buttons were pressed.  A press elsewhere on the grid waits
for its click."
  (interactive "e")
  (pcase (lispm-desktop-store--grid-event event)
    ((and `(button . ,id) target)
     (let ((buffer (window-buffer (posn-window (event-start event)))))
       (lispm-desktop-store--select id)
       (when (lispm-desktop-modal-hold
              (lambda (moved) (and (windowp (posn-window (event-start moved)))
                                   (equal (lispm-desktop-store--grid-event moved) target)))
              (lambda (in)
                (with-current-buffer buffer
                  (setq lispm-desktop-store--pressed (and in id))
                  (lispm-desktop-store--render))
                (redisplay t)))
         (lispm-desktop-store-act id))))))

(defun lispm-desktop-store--grid-double-click (event)
  "Press the button of the item EVENT clicked twice."
  (interactive "e")
  (pcase (lispm-desktop-store--grid-event event)
    (`(item . ,id) (lispm-desktop-store-act id))))

(defvar-keymap lispm-desktop-store-disclosure-map
  :doc "Keymap of the disclosure of the item's information: Space shows or
hides it, as Return does, rather than pressing the item's button."
  :parent button-map
  "SPC" #'push-button)

(defvar-keymap lispm-desktop-store-grid-map
  :doc "Keymap of the App Store's grid, for the mouse."
  "<down-mouse-1>" #'lispm-desktop-store--grid-press
  "<mouse-1>" #'lispm-desktop-store--grid-click
  "<double-mouse-1>" #'lispm-desktop-store--grid-double-click)

(defun lispm-desktop-store-close ()
  "Close the App Store's window."
  (interactive)
  (lispm-desktop-app-window-close 'store))

(defun lispm-desktop-store--close-box (_window)
  "Close the App Store's window, from its close box."
  (lispm-desktop-store-close))

(defvar-keymap lispm-desktop-store-mode-map
  :doc "Keymap of the App Store's window."
  :parent button-buffer-map
  "C-f" #'lispm-desktop-store-forward
  "C-b" #'lispm-desktop-store-backward
  "C-n" #'lispm-desktop-store-down
  "C-p" #'lispm-desktop-store-up
  "<right>" #'lispm-desktop-store-forward
  "<left>" #'lispm-desktop-store-backward
  "<down>" #'lispm-desktop-store-down
  "<up>" #'lispm-desktop-store-up
  "n" #'lispm-desktop-store-forward
  "p" #'lispm-desktop-store-backward
  "M-n" #'lispm-desktop-store-next-category
  "M-p" #'lispm-desktop-store-previous-category
  "c" #'lispm-desktop-store-show-category
  "RET" #'lispm-desktop-store-act-selected
  "SPC" #'lispm-desktop-store-act-selected
  "/" #'lispm-desktop-store-search
  "i" #'lispm-desktop-store-toggle-information
  "u" #'lispm-desktop-store-update-selected
  "D" #'lispm-desktop-store-remove-selected
  "." #'lispm-desktop-store-stop-selected
  "l" #'lispm-desktop-store-show-log-selected
  "w" #'lispm-desktop-store-copy-install-command
  "+" #'lispm-desktop-store-install-from-url
  "g" #'lispm-desktop-store-check-for-updates
  "q" #'lispm-desktop-store-close
  "M-w" #'lispm-desktop-store-close)

(define-derived-mode lispm-desktop-store-mode lispm-desktop-app-window-mode "App Store"
  "Major mode of the App Store's window: categories, a grid of icons and
the selected item's information.
\\{lispm-desktop-store-mode-map}"
  (setq-local line-spacing 0
              lispm-desktop-platinum-info #'lispm-desktop-store--placard
              lispm-desktop-platinum-title "App Store"
              lispm-desktop-platinum-icon (lispm-desktop-platinum-icon "app-store")
              lispm-desktop-platinum-box-functions (list :close #'lispm-desktop-store--close-box
                                                         :collapse #'lispm-desktop-app-window-collapse-box))
  (add-hook 'window-size-change-functions #'lispm-desktop-store--refit nil t)
  (add-hook 'post-command-hook #'lispm-desktop-platinum-show-focus nil t)
  (hl-line-mode -1))

(lispm-desktop-app-window-define 'store
  :name "App Store" :size '(640 . 640) :roll 'chrome
  :documents 'keep :close #'lispm-desktop-app-window-close-box)

(defun lispm-desktop-store-show ()
  "Open the App Store's window over the desktop.
The catalogs are fetched the first time it opens after Emacs starts."
  (interactive)
  (unless lispm-desktop-store--catalogs
    (lispm-desktop-store-read-catalogs))
  (let ((buffer (lispm-desktop-store--buffer)))
    (lispm-desktop-app-window-show 'store :buffer buffer)
    (lispm-desktop-store--render)
    (unless (or lispm-desktop-store--checked lispm-desktop-store--checking)
      (lispm-desktop-store-check-for-updates))))

;;;;; Menus

(defun lispm-desktop-store--file-items ()
  "Return the items of the App Store's File menu."
  (let* ((id (lispm-desktop-store--selected-id))
         (props (and id (cdr (assq id (lispm-desktop-store-items)))))
         (action (and props (lispm-desktop-store--action id props))))
    `((:label ,(or (car action) "Get") :action lispm-desktop-store-act-selected :key "RET"
              :enabled ,(and (cdr action) t))
      (:label "Remove" :action lispm-desktop-store-remove-selected :key "D"
              :enabled ,(and id (lispm-desktop-store-installed-p id) (not (lispm-desktop-store--busy-p id))))
      (:label "Stop" :action lispm-desktop-store-stop-selected :key "."
              :enabled ,(and id (lispm-desktop-store--busy-p id)
                             (lispm-desktop-store--stoppable-p (lispm-desktop-store--job id)) t))
      (:label "Show Log" :action lispm-desktop-store-show-log-selected :key "l"
              :enabled ,(and id (lispm-desktop-store--job id)
                             (buffer-live-p (lispm-desktop-store-job-log (lispm-desktop-store--job id)))))
      (:label "Copy Install Command" :action lispm-desktop-store-copy-install-command :key "w"
              :enabled ,(and props (plist-get props :install) t))
      separator
      (:label "Install from URL…" :action lispm-desktop-store-install-from-url :key "+")
      (:label "Check for Updates" :action lispm-desktop-store-check-for-updates :key "g"
              :enabled ,(not lispm-desktop-store--checking))
      separator
      (:label "Close Window" :action lispm-desktop-store-close :key "q"))))

(defun lispm-desktop-store--view-items ()
  "Return the items of the App Store's View menu: a category each, the one
shown checked, Search and the item's information."
  (with-current-buffer (lispm-desktop-store--buffer)
    `(,@(mapcar (pcase-lambda (`(,category ,name . ,_))
                  (list :label name :action (lambda () (lispm-desktop-store-show-category category))
                        :checked (eq category lispm-desktop-store--category)))
                (lispm-desktop-store--categories))
      separator
      (:label "Search…" :action lispm-desktop-store-search :key "/")
      (:label ,(if lispm-desktop-store--information "Hide Item Information" "Show Item Information")
              :action lispm-desktop-store-toggle-information :key "i"))))

(defun lispm-desktop-store-menus ()
  "Return the App Store's menus, for the menu bar while it is in front."
  '((lispm-desktop-store-file "File" lispm-desktop-store--file-items)
    (lispm-desktop-store-view "View" lispm-desktop-store--view-items)))

;;;; Setup

(defun lispm-desktop-setup-store ()
  "Make the App Store an application, the Applications folder a folder of
the Finder, and the applications and extensions installed take part."
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
