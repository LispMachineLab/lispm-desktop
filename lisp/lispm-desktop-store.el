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
;;                 runs (`lispm-agents-define'): Hermes
;;
;; It lists them from catalogs: git repositories holding a catalog.eld,
;; LispMachineLab/lispm-store by default (`lispm-desktop-store-catalogs').
;; Each is cloned under `lispm-desktop-store-directory' and its catalog
;; read as data, never evaluated, in the format lispm-desktop-catalog.el
;; describes and checks, the one LispMachineLab/lispm-store's own check
;; loads too: an item it finds an error in is left out, saying why.
;; `lispm-desktop-store-check-catalog' lists what it finds wrong with a
;; catalog, for those writing one.
;;
;; An application becomes the desktop's in one way, at its install and at
;; every start (`lispm-desktop-store--register-app'): its catalog says
;; what it is called, its icon, category and description, and what it
;; needs (`lispm-desktop-store-app-metadata'); how it opens is the App
;; Store's for one with a :command, run in the application's window, or
;; its library's for one with a :feature, running windows of its own as
;; Stickies does: loaded first, the library says what the application
;; does with `lispm-desktop-define-app' under the item's ID, and is
;; unloaded (`unload-feature') when the application is uninstalled.  An
;; extension's :feature, loaded, makes it take part in the editor;
;; unloaded, it takes part no more.  An extension needing a :program can
;; be installed only while that program is on PATH; :install says how to
;; install the program.  An app's :needs are the programs, libraries and
;; applications it uses beyond Emacs, as EMMS a player, each as
;; lispm-desktop-needs.el says: Get installs those missing with the app,
;; in the Installer, the alert asking for both; its information says what
;; is missing, Install Missing installs it, and its window offers it, see
;; lispm-desktop-app-ui.el; the commands a need names offer it too.
;; :category, one of `lispm-desktop-store-categories', as Classic AI, the
;; programs of AI's first decades, ELIZA's Doctor, SHRDLU and MYCIN, and
;; :subtitle, a few words, place an application in the window.  :requires names the
;; items installed before.  Items of other kinds, as LispM's roles and
;; skills, are not the App Store's: it passes them over.
;;
;; Nothing new is installed unasked: where a package comes from is shown
;; under the window while an alert asks, as before an update asked for,
;; with what changes.  The updates of what was installed install by
;; themselves, unless `lispm-desktop-store-update-automatically' says
;; otherwise, in Control Panels' Software Update: the App Store looks
;; for them whenever Software Update checks, and installs them when
;; LispM is idle.  What was installed is kept with Customize in
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
;; a search field over the categories down the left, the items of the
;; one chosen as icons in a grid, each with its button, Get, Open or
;; Update, in pages of as many as fit, Previous and Next under them.
;; What is typed in the field is looked for in every category as typing
;; pauses, the best matches first.  An item clicked opens its page in
;; place of the grid: its icon and name large, its banner, its
;; introduction, Markdown its catalog gives (lispm-desktop-markdown.el),
;; whose links open as clicked, and its information, who made it and
;; under what licence among it; its push buttons under them; Go Back
;; goes back to the grid, on the page it was.  The window never scrolls:
;; an item's page longer than the grid's page shows in pages too, turned
;; as the grid's are.  An item a catalog lists without the whole of its
;; listing, its banner and introduction among it, is not shown.  The
;; keyboard does all of it: C-f, C-b, C-n and C-p go from icon to icon,
;; on an item's page C-n and C-p from page to page, ] and [, or C-v and
;; M-v, from page to page, M-n and M-p from category to category, RET or
;; i opens the selected item's page, DEL or M-[ goes back, Space presses
;; its button, / or C-s goes to the search field, which RET or C-n leaves
;; and C-g empties, u updates, D uninstalls, . stops an install, l shows
;; the log of one that failed, o opens a link of an item's introduction,
;; g checks the catalogs, + installs from a git URL, Tab goes to the push
;; buttons and q closes.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'package)
(require 'delsel)
(require 'lispm-desktop-core)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-modal)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-app-window)
(require 'lispm-desktop-app-ui)
(require 'lispm-desktop-store-installer)
(require 'lispm-desktop-needs)
(require 'lispm-desktop-update)
(require 'lispm-desktop-loading)
(require 'lispm-desktop-i18n)
(require 'lispm-desktop-catalog)
(require 'lispm-desktop-markdown)

(declare-function lispm-desktop-popup-alert "lispm-desktop-popup" (icon message buttons &optional detail))
(declare-function lispm-desktop-popup-menu "lispm-desktop-popup" (items &optional x y align))
(declare-function lispm-desktop-popup-frame-xy "lispm-desktop-popup" (position))
(declare-function lispm-desktop-finder-open-directory "lispm-desktop-finder" (directory))
(declare-function lispm-desktop-finder-folder-changed "lispm-desktop-finder" (key))
(declare-function lispm-desktop--darken "lispm-desktop-surface" (svg))
(declare-function project-remember-projects-under "project" (dir &optional recursive))
(defvar package-vc-selected-packages)
(defvar lispm-desktop-finder-folders)
(defvar lispm-desktop-context-menu-function)
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

(defcustom lispm-desktop-store-update-automatically t
  "Non-nil means the updates of what the App Store installed install by
themselves once found: when LispM has been idle a minute, an application
once its window is closed.  nil means they wait in the App Store's
Updates.  The App Store looks for them whenever Software Update checks
for LispM's own, and as it opens."
  :type 'boolean
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
  `((app ,(lispm-desktop-tr-noop "Applications") ,(lispm-desktop-tr-noop "Application"))
    (extension ,(lispm-desktop-tr-noop "Extensions") ,(lispm-desktop-tr-noop "Extension")))
  "The kinds of item the App Store installs: (KIND GROUP NAME), GROUP and
NAME in English, translated where shown.")

(defconst lispm-desktop-store-categories
  `((all ,(lispm-desktop-tr-noop "All"))
    (news ,(lispm-desktop-tr-noop "News"))
    (music ,(lispm-desktop-tr-noop "Music"))
    (social ,(lispm-desktop-tr-noop "Social"))
    (productivity ,(lispm-desktop-tr-noop "Productivity"))
    (utilities ,(lispm-desktop-tr-noop "Utilities"))
    (games ,(lispm-desktop-tr-noop "Games"))
    (classic-ai ,(lispm-desktop-tr-noop "Classic AI"))
    (extensions ,(lispm-desktop-tr-noop "Extensions"))
    (updates ,(lispm-desktop-tr-noop "Updates")))
  "The categories down the App Store's left side, in order: (CATEGORY NAME),
NAME in English, translated where shown.
An application names its :category in its catalog; every extension is in
Extensions.  All holds every item and Updates those an update is ready
for; a category holding nothing is left out.")

(defconst lispm-desktop-store-buffer-name " *App Store*"
  "Name of the App Store's window's buffer.")

(defvar-local lispm-desktop-store--category 'all
  "The category shown, a key of `lispm-desktop-store-categories'.")

(defvar-local lispm-desktop-store--filter nil
  "What the search field holds, which the items shown match, see
`lispm-desktop-store-search', or nil.")

(defvar-local lispm-desktop-store--selected nil
  "The ID of the item selected, or nil.")

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
                         (funcall callback (if (string-empty-p output)
                                               (lispm-desktop-tr "git failed")
                                             output))))))))))

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
  "Fetch the catalogs in the background, and read the package archives
anew when an item installed comes from them; then list what they hold
now, and install the updates found or say they are there, see
`lispm-desktop-store-update-automatically'.  CALLBACK, when given, is
called after.  Emacs waits on none of it, so the pointer keeps its
arrow; the App Store's status line says it checks."
  (interactive)
  (if lispm-desktop-store--checking
      (message (lispm-desktop-tr "The App Store is checking its catalogs already"))
    (setq lispm-desktop-store--checking t)
    (lispm-desktop-store--status (lispm-desktop-tr "Checking the catalogs…"))
    (let ((urls lispm-desktop-store-catalogs)
          (failed nil))
      (cl-labels ((next ()
                    (if-let* ((url (pop urls)))
                        (lispm-desktop-store--git-steps
                         (lispm-desktop-store--fetch-steps url)
                         (lambda (error)
                           (when error
                             (push url failed)
                             (message (lispm-desktop-tr "App Store: %s: %s") url error))
                           (next)))
                      (lispm-desktop-store-read-catalogs)
                      (lispm-desktop-store--refresh-archives
                       (lambda ()
                         (setq lispm-desktop-store--checking nil
                               lispm-desktop-store--checked t)
                         (lispm-desktop-store--status
                          (and failed (lispm-desktop-tr "Could not reach %s" (string-join (nreverse failed) ", "))))
                         (lispm-desktop-store--render)
                         (lispm-desktop-store--checked-updates)
                         (when callback
                           (funcall callback)))))))
        (condition-case err
            (next)
          (error (setq lispm-desktop-store--checking nil)
                 (signal (car err) (cdr err))))))))

(defun lispm-desktop-store--item-valid-p (item)
  "Return non-nil when ITEM, (ID . PROPERTIES) of a catalog, is an item the
App Store lists: of a kind it installs, and well formed as
lispm-desktop-catalog.el says."
  (lispm-desktop-catalog-item-valid-p item))

(defun lispm-desktop-store--kind-p (item)
  "Return non-nil when ITEM, (ID . PROPERTIES), is of a kind the App Store
installs, see `lispm-desktop-store-kinds'."
  (and (consp item) (plistp (cdr item))
       (assq (plist-get (cdr item) :kind) lispm-desktop-store-kinds)
       t))

(defun lispm-desktop-store--read-catalog (url)
  "Return the catalog at URL as its clone holds it, or nil without a clone.
A catalog that cannot be read is said so and left out, and so is an item
of it lispm-desktop-catalog.el finds an error in, saying which: one short
of its listing, as its banner or its introduction, is.  What it only
warns of, as a subtitle too long, is the catalog's to mend, and does not
keep an item out.  Items of kinds the App Store does not install are
passed over without a word."
  (let* ((directory (lispm-desktop-store--catalog-directory url))
         (file (expand-file-name "catalog.eld" directory)))
    (when (file-readable-p file)
      (condition-case err
          (let ((data (lispm-desktop-catalog-read directory)))
            (list :url url :directory directory
                  :name (let ((name (plist-get data :name))) (if (stringp name) name url))
                  :commit (ignore-errors (lispm-desktop-store--git directory "rev-parse" "HEAD"))
                  :items (seq-filter
                          (lambda (item)
                            (and (lispm-desktop-store--kind-p item)
                                 (if-let* ((errors (lispm-desktop-catalog-errors
                                                    (lispm-desktop-catalog-item-problems item directory))))
                                     (progn (message (lispm-desktop-tr "App Store: %s lists %S wrongly, left out: %s")
                                                     url (car-safe item) (string-join errors "; "))
                                            nil)
                                   t)))
                          (plist-get data :items))))
        (error (message (lispm-desktop-tr "App Store: cannot read the catalog of %s: %s")
                        url (error-message-string err))
               nil)))))

(defun lispm-desktop-store-read-catalogs ()
  "Read the catalogs as their clones hold them; return them."
  (setq lispm-desktop-store--catalogs
        (delq nil (mapcar #'lispm-desktop-store--read-catalog lispm-desktop-store-catalogs))))

(defconst lispm-desktop-store-check-buffer-name "*Catalog Check*"
  "Name of the buffer `lispm-desktop-store-check-catalog' lists problems in.")

(defun lispm-desktop-store-check-catalog (directory)
  "List what is wrong with the catalog DIRECTORY holds, its catalog.eld and
the icons it names, as the App Store reads it and as LispMachineLab's
lispm-store checks its own, see lispm-desktop-catalog.el; return the
problems, (SEVERITY . TEXT)s.  An error keeps an item out of the App
Store; a warning says what the catalog should say besides."
  (interactive (list (read-directory-name (lispm-desktop-tr "Check the catalog in: ") nil nil t)))
  (let ((problems (condition-case err
                      (lispm-desktop-catalog-problems (lispm-desktop-catalog-read directory) directory)
                    (error (list (cons 'error (error-message-string err)))))))
    (when (called-interactively-p 'any)
      (with-current-buffer (get-buffer-create lispm-desktop-store-check-buffer-name)
        (special-mode)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (abbreviate-file-name (expand-file-name "catalog.eld" directory)) "\n\n")
          (pcase-dolist (`(,severity . ,text) problems)
            (insert (if (eq severity 'error) (lispm-desktop-tr "Error: %s" text) (lispm-desktop-tr "Warning: %s" text))
                    "\n"))
          (insert (if problems
                      (concat "\n" (lispm-desktop-tr-n "%d problem." "%d problems." (length problems)) "  "
                              (lispm-desktop-tr "An error keeps an item out of the App Store; a warning says what the catalog should say besides."))
                    (lispm-desktop-tr "Nothing is wrong with it."))
                  "\n")
          (goto-char (point-min)))
        (pop-to-buffer (current-buffer))))
    problems))

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

(defun lispm-desktop-store--catalog-icon (props &optional large)
  "Return the file of the icon the catalog of the item of PROPS draws it
with, its 32-pixel one when LARGE, or nil when its clone holds none."
  (when-let* ((name (plist-get props :icon))
              ((plist-get props :catalog))
              (file (lispm-desktop-store--file props (format "icons/%s%s.svg" name (if large "-32" ""))))
              ((file-readable-p file)))
    file))

(defun lispm-desktop-store--icon (props &optional large)
  "Return the icon of the item of PROPS, its 32-pixel one when LARGE, as
`lispm-desktop-platinum-icon-file' takes it: its catalog's drawing, or
one standing for its kind."
  (or (lispm-desktop-store--catalog-icon props large)
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
               (lispm-desktop-tr "load path"))
              ((and (lispm-desktop-store-installed-p id)
                    (lispm-desktop-store--package-version (lispm-desktop-store--package-name package))))
              ((consp package) (lispm-desktop-store--short (plist-get (cdr package) :rev))))
        "–")))

(defun lispm-desktop-store--origin (props)
  "Return where the item of PROPS comes from, in words of the language
LispM speaks."
  (pcase (plist-get props :package)
    ('nil "Emacs")
    ((pred symbolp) (lispm-desktop-tr "the package archives"))
    (`(,_ . ,spec) (plist-get spec :url))))

(defun lispm-desktop-store--archive-newer-p (name)
  "Return non-nil when the package archives, as last read, hold a newer
package NAME than the one installed."
  (when-let* ((installed (cadr (assq name package-alist))))
    (seq-some (lambda (available)
                (version-list-< (package-desc-version installed) (package-desc-version available)))
              (cdr (assq name package-archive-contents)))))

(defun lispm-desktop-store-update-available-p (id)
  "Return non-nil when the item ID installed can be updated: its package
pinned by its catalog to another commit than installed, or, from the
package archives, newer there.  One loaded from the load path, as a
checkout's, is updated there, not by the App Store."
  (when-let* ((installed (lispm-desktop-store--installed id))
              ((not (equal (plist-get installed :installed) "load-path")))
              (props (cdr (assq id (lispm-desktop-store-items)))))
    (pcase (plist-get props :package)
      ('nil nil)
      ((and package (pred consp))
       (not (equal (plist-get (cdr package) :rev) (plist-get installed :installed))))
      (package (lispm-desktop-store--archive-newer-p package)))))

(defun lispm-desktop-store-updates ()
  "Return the IDs of the items installed that can be updated."
  (seq-filter #'lispm-desktop-store-update-available-p (mapcar #'car lispm-desktop-store-installed)))

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
ACTION): LABEL is in the language LispM speaks; ACTION is `install',
`open', `update' or `stop', or nil for a button that does nothing: an
extension installed, one whose program is not on PATH, or an update past
stopping.  An item whose install or update failed is tried again."
  (cond ((lispm-desktop-store--busy-p id)
         (if (lispm-desktop-store--stoppable-p (lispm-desktop-store--job id))
             (cons (lispm-desktop-tr "Stop") 'stop)
           (list (lispm-desktop-tr "Stop"))))
        ((when-let* ((job (lispm-desktop-store--job id)))
           (cons (lispm-desktop-tr "Try Again") (lispm-desktop-store-job-action job))))
        ((lispm-desktop-store-update-available-p id) (cons (lispm-desktop-tr "Update") 'update))
        ((lispm-desktop-store-installed-p id)
         (if (eq (plist-get props :kind) 'app)
             (cons (lispm-desktop-tr "Open") 'open)
           (list (lispm-desktop-tr "Installed"))))
        ((lispm-desktop-store--missing-program props) (list (lispm-desktop-tr "Get")))
        (t (cons (lispm-desktop-tr "Get") 'install))))

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
                                        `((,(lispm-desktop-tr "Cancel") cancel cancel) (,action go default))
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
        (setq-local word-wrap-by-category t
                    lispm-desktop-platinum-title (lambda () (lispm-desktop-tr "What It Brings"))))
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
  "Return what installing ITEM brings, as text to read first, in the
language LispM speaks."
  (pcase-let ((`(,_ . ,props) item))
    (concat
     "* " (if (eq (plist-get props :kind) 'extension)
              (lispm-desktop-tr "%s, extension" (plist-get props :name))
            (lispm-desktop-tr "%s, application" (plist-get props :name)))
     "\n\n"
     (pcase (plist-get props :package)
       ('nil (concat (lispm-desktop-tr "Built into Emacs.") "\n"))
       ((and package (pred symbolp))
        (concat (lispm-desktop-tr "The package %s, from the package archives (%s)."
                                  package (mapconcat #'car package-archives ", "))
                "\n"))
       (`(,name . ,spec)
        (concat (lispm-desktop-tr "The package %s, from %s at commit %s."
                                  name (plist-get spec :url) (plist-get spec :rev))
                "\n")))
     (when-let* ((program (plist-get props :program)))
       (concat (lispm-desktop-tr "It runs %s, found at %s." program (executable-find program)) "\n"))
     (mapconcat (lambda (need)
                  (concat (lispm-desktop-tr "With it, %s, for %s: %s." (lispm-desktop-needs-name need)
                                            (lispm-desktop-needs-get need :for)
                                            (lispm-desktop-needs-install-command need))
                          "\n"))
                (lispm-desktop-store--needs-to-install props)))))

(defun lispm-desktop-store--items-needs (items)
  "Return what ITEMS need that is missing and the Installer installs, each
need once."
  (seq-uniq (mapcan (lambda (item) (lispm-desktop-store--needs-to-install (cdr item))) items)
            (lambda (need other) (equal (lispm-desktop-needs-key need) (lispm-desktop-needs-key other)))))

(defun lispm-desktop-store--needs-to-install (props)
  "Return what the item of PROPS needs that is missing and the Installer
installs, a stand-in there or not."
  (seq-filter #'lispm-desktop-needs-installable-p
              (lispm-desktop-needs-missing (plist-get props :needs) t)))

(defun lispm-desktop-store--built-in-p (props)
  "Return non-nil when PROPS describe an application built into Emacs."
  (and (eq (plist-get props :kind) 'app) (null (plist-get props :package))))

(defun lispm-desktop-store--confirm-install (items)
  "Show what ITEMS bring and ask whether to install them; non-nil if so.
Applications built into Emacs are only made applications: nothing asks."
  (or (seq-every-p (lambda (item) (lispm-desktop-store--built-in-p (cdr item))) items)
      (lispm-desktop-store--confirm
       (lispm-desktop-tr "Install %s?" (lispm-desktop-store--names items))
       (concat (lispm-desktop-tr
                "Its code will run in Emacs as your own does; where it comes from is shown under the window.")
               (when-let* ((needs (lispm-desktop-store--items-needs items)))
                 (concat "  " (lispm-desktop-needs-detail needs t))))
       (mapconcat #'lispm-desktop-store--preview items "\n")
       (lispm-desktop-tr "Install"))))

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
                      (user-error (lispm-desktop-tr "The App Store's items %s require each other") id))
                    (push id visiting)
                    (let ((item (or (assq id items)
                                    (user-error (lispm-desktop-tr "%s requires %s, which no catalog lists") from id))))
                      (dolist (required (plist-get (cdr item) :requires))
                        (visit required id))
                      (push item order)))))
      (visit id nil))
    (nreverse order)))

(defun lispm-desktop-store--check-program (props)
  "Signal a `user-error' when the item of PROPS needs a program PATH lacks,
saying how to install it."
  (when-let* ((program (lispm-desktop-store--missing-program props)))
    (if-let* ((install (plist-get props :install)))
        (user-error (lispm-desktop-tr "%s needs `%s', which is not on PATH; install it with: %s")
                    (plist-get props :name) program install)
      (user-error (lispm-desktop-tr "%s needs `%s', which is not on PATH") (plist-get props :name) program))))

(defun lispm-desktop-store--keep (id props &rest installed)
  "Keep that the item ID, of PROPS, is installed as INSTALLED, a plist, says.
Saving it is not said in the echo area: the window shows it."
  (let ((inhibit-message t))
    (customize-save-variable
     'lispm-desktop-store-installed
     (append (assq-delete-all id (copy-alist lispm-desktop-store-installed))
             (list (cons id (append installed (lispm-desktop-store--plist-without
                                               props :installed :owned))))))
    (lispm-desktop-store--applications-changed)))

(defun lispm-desktop-store--forget (id)
  "Forget that the item ID is installed."
  (customize-save-variable 'lispm-desktop-store-installed
                           (assq-delete-all id (copy-alist lispm-desktop-store-installed)))
  (lispm-desktop-store--applications-changed))

(defun lispm-desktop-store--applications-changed ()
  "Show the applications installed as they are now in the Applications
folder, when the Finder window shows it."
  (when (fboundp 'lispm-desktop-finder-folder-changed)
    (lispm-desktop-finder-folder-changed 'applications)))

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
  "Do JOB, an install, in this Emacs, which waits meanwhile, the pointer a
watch over the App Store: its package is installed unless PRESENT, as
`lispm-desktop-store--package-present' says."
  (pcase-let* ((`(,_ . ,props) (lispm-desktop-store-job-item job))
               (package (plist-get props :package))
               (installed (pcase present
                            ('nil
                             (lispm-desktop-store--status (lispm-desktop-tr "Installing %s…" (plist-get props :name)))
                             (unwind-protect
                                 (lispm-desktop-with-loading lispm-desktop-store-buffer-name
                                   (lispm-desktop-store--quietly
                                     (lispm-desktop-store--install-package package)))
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
          (lispm-desktop-store--fail job (lispm-desktop-tr "It cannot be loaded: %s" failure)))
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
in its place in the App Store's window; then what they need beyond
Emacs, in the Installer, the alert having asked for it too."
  (interactive (list (lispm-desktop-store--read-item
                      (lispm-desktop-tr "Install: ") (lambda (id) (not (lispm-desktop-store-installed-p id))))))
  (when (lispm-desktop-store--busy-p id)
    (user-error (lispm-desktop-tr "%s is being installed already")
                (plist-get (cdr (assq id (lispm-desktop-store-items))) :name)))
  (let ((items (lispm-desktop-store--to-install id)))
    ;; Nothing is asked about what could not run here.
    (dolist (item items)
      (lispm-desktop-store--check-program (cdr item)))
    (when (and items (lispm-desktop-store--confirm-install items))
      (lispm-desktop-store--queue
       items 'install
       (when-let* ((needs (lispm-desktop-store--items-needs items)))
         (lambda ()
           (lispm-desktop-needs-install needs (string-join (mapcar (lambda (item) (plist-get (cdr item) :name))
                                                                   items)
                                                           ", ")
                                        (lambda (_) (lispm-desktop-store--render)))))))))

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
  process log file before pending installed error stopped then)

(defvar lispm-desktop-store--jobs nil
  "The installs and updates asked for, in order, as `lispm-desktop-store-job's.
Each waits, runs, one at a time, or failed; those done are gone.  A job
is installing or updating ITEM, (ID . PROPERTIES), as ACTION says; those
asked for together share a GROUP.")

(defconst lispm-desktop-store-stages
  `((waiting ,(lispm-desktop-tr-noop "Waiting") ,(lispm-desktop-tr-noop "waiting") 0.0 0.0)
    (preparing ,(lispm-desktop-tr-noop "Preparing") ,(lispm-desktop-tr-noop "preparing") 0.0 0.05)
    (archives ,(lispm-desktop-tr-noop "Reading the archives") ,(lispm-desktop-tr-noop "reading the archives")
              0.05 0.1)
    (download ,(lispm-desktop-tr-noop "Downloading") ,(lispm-desktop-tr-noop "downloading") 0.1 0.6)
    (dependencies ,(lispm-desktop-tr-noop "Getting what it needs") ,(lispm-desktop-tr-noop "getting what it needs")
                  0.6 0.7)
    (build ,(lispm-desktop-tr-noop "Building") ,(lispm-desktop-tr-noop "building") 0.7 0.95)
    (finishing ,(lispm-desktop-tr-noop "Finishing") ,(lispm-desktop-tr-noop "finishing") 0.95 1.0))
  "The stages of an install or an update, in order: (STAGE NAME PHRASE FROM
TO).  NAME stands alone, as an item's subtitle, PHRASE in a sentence, as
its status; both are in English, translated where shown.  FROM and TO are
the part of its progress bar each fills.")

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

(defun lispm-desktop-store--queue (items action &optional then)
  "Install or update, as ACTION says, ITEMS, (ID . PROPERTIES), one after
another; an earlier attempt that failed is forgotten.  THEN, when given,
is called once the last is done, unless one fails."
  (let ((group (list action)))
    (dolist (item items)
      (when-let* ((old (lispm-desktop-store--job (car item))))
        (lispm-desktop-store--forget-job old))
      (setq lispm-desktop-store--jobs
            (append lispm-desktop-store--jobs
                    (list (lispm-desktop-store--make-job :id (car item) :item item
                                                         :action action :group group
                                                         :then (and (eq item (car (last items))) then)))))))
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
            (error (lispm-desktop-tr "It needs “%s”, which is not installed")
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
               (cond ((not (lispm-desktop-store--background-p))
                      (lispm-desktop-store--update-here job))
                     ((consp package)
                      (lispm-desktop-store--spawn
                       job `(update ,(car package) ,(plist-get (cdr package) :rev) ,former)))
                     (t (lispm-desktop-store--spawn job `(upgrade ,package))))))))
      (error (lispm-desktop-store--fail job (error-message-string err))))))

(defun lispm-desktop-store--done (job)
  "End JOB, which went well, then do what was to follow it."
  (lispm-desktop-store--forget-job job)
  (unless (lispm-desktop-store-open-p)
    (message (if (eq (lispm-desktop-store-job-action job) 'install)
                 (lispm-desktop-tr "App Store: installed %s")
               (lispm-desktop-tr "App Store: updated %s"))
             (plist-get (cdr (lispm-desktop-store-job-item job)) :name)))
  (lispm-desktop-store--ended-job job)
  (when-let* ((then (lispm-desktop-store-job-then job)))
    (funcall then)))

(defun lispm-desktop-store--ended-job (job)
  "Draw the App Store again once JOB ended, tell Software Update when it
was an update, and start the next."
  (lispm-desktop-store--render)
  (when (eq (lispm-desktop-store-job-action job) 'update)
    (run-hooks 'lispm-desktop-update-changed-hook))
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
  (message (if (eq (lispm-desktop-store-job-action job) 'install)
               (lispm-desktop-tr "App Store: %s could not be installed: %s")
             (lispm-desktop-tr "App Store: %s could not be updated: %s"))
           (plist-get (cdr (lispm-desktop-store-job-item job)) :name)
           reason)
  (lispm-desktop-store--ended-job job))

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
                     (error (lispm-desktop-tr "lispm-desktop-store-installer is not on the load path")))))
    `(progn
       ,@(mapcan (lambda (variable)
                   (when (and (boundp variable) (lispm-desktop-store--readable-p (symbol-value variable)))
                     (list `(setq ,variable ',(symbol-value variable)))))
                 lispm-desktop-store--handed-variables)
       (push ,(file-name-directory library) load-path)
       ;; The editor's agents, which an extension adding one requires.
       ,@(when-let* ((agents (locate-library "lispm-agents")))
           `((push ,(file-name-directory agents) load-path)))
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
  (or (nthcdr 3 (assq stage lispm-desktop-store-stages)) '(0.0 0.0)))

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
                                         (lispm-desktop-tr "Its Emacs stopped before it was done")))))))

(defun lispm-desktop-store--undo (job)
  "Undo what JOB, failed or stopped in the background, left: the packages
it put in `package-user-dir', and an update's checkout, back at the
commit it was at.  An update from the package archives deletes the
version it replaces only once the new one is installed."
  (let ((before (lispm-desktop-store-job-before job)))
    (dolist (directory (lispm-desktop-store--package-directories))
      (unless (member directory before)
        (delete-directory directory t))))
  (when (eq (lispm-desktop-store-job-action job) 'update)
    (pcase-let* ((`(,id . ,props) (lispm-desktop-store-job-item job))
                 (package (plist-get props :package))
                 (desc (cadr (assq (lispm-desktop-store--package-name package) package-alist))))
      (when (and desc (consp package))
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
        (let ((desc (or (cadr (assq name package-alist)) (error (lispm-desktop-tr "%s is gone") name))))
          (package-activate-1 desc 'reload 'deps))
      (unless (package-activate name)
        (error (lispm-desktop-tr "Its package %s cannot be activated") name))
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
  (interactive (list (lispm-desktop-store--read-item (lispm-desktop-tr "Stop: ") #'lispm-desktop-store--busy-p)))
  (let ((job (lispm-desktop-store--job id)))
    (unless (lispm-desktop-store--busy-p id)
      (user-error (lispm-desktop-tr "Nothing is being installed for that")))
    (unless (lispm-desktop-store--stoppable-p job)
      (user-error (lispm-desktop-tr "%s can no longer be stopped")
                  (plist-get (cdr (lispm-desktop-store-job-item job)) :name)))
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
                      (lispm-desktop-tr "Log of: ")
                      (lambda (id) (when-let* ((job (lispm-desktop-store--job id)))
                                     (buffer-live-p (lispm-desktop-store-job-log job)))))))
  (let ((log (and (lispm-desktop-store--job id) (lispm-desktop-store-job-log (lispm-desktop-store--job id)))))
    (unless (buffer-live-p log)
      (user-error (lispm-desktop-tr "There is no log of that")))
    (with-current-buffer log
      (special-mode)
      (goto-char (point-max)))
    (pop-to-buffer log)))

(defun lispm-desktop-store--progress (job)
  "Return how far JOB is, in words of the language LispM speaks, as
\"Installing: downloading, 45%\"."
  (let ((install (eq (lispm-desktop-store-job-action job) 'install)))
    (pcase (lispm-desktop-store-job-state job)
      ('waiting (if install (lispm-desktop-tr "Waiting to install") (lispm-desktop-tr "Waiting to update")))
      ('failed (if install (lispm-desktop-tr "Could not be installed") (lispm-desktop-tr "Could not be updated")))
      (_ (let ((stage (lispm-desktop-tr (nth 2 (assq (lispm-desktop-store-job-stage job) lispm-desktop-store-stages))))
               (percent (round (* 100 (lispm-desktop-store-job-fraction job)))))
           (if install
               (lispm-desktop-tr "Installing: %s, %d%%" stage percent)
             (lispm-desktop-tr "Updating: %s, %d%%" stage percent)))))))

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
        (error (message (lispm-desktop-tr "App Store: %s stays: %s") name (error-message-string err)))))
    (when (and (consp package) (boundp 'package-vc-selected-packages)
               (assq name package-vc-selected-packages))
      (customize-save-variable 'package-vc-selected-packages
                               (assq-delete-all name (copy-alist package-vc-selected-packages))))))

(defun lispm-desktop-store--remove-detail (id props)
  "Return what uninstalling the item ID, of PROPS, does, in words of the
language LispM speaks."
  (let ((requirers (lispm-desktop-store--requirers id))
        (package (lispm-desktop-store--package-name (plist-get props :package))))
    (concat
     (pcase (plist-get props :kind)
       ('extension (concat (lispm-desktop-tr "It leaves the editor at once.") "  "))
       (_ (if package
              ""
            (lispm-desktop-tr "It stays in Emacs; only its window and its place among the applications go."))))
     (cond ((null package) "")
           ((plist-get props :owned) (lispm-desktop-tr "Its package %s is deleted." package))
           (t (lispm-desktop-tr "Its package %s stays, as it was there before." package)))
     (when requirers
       (concat "  " (lispm-desktop-tr-n "%s needs it." "%s need it." (length requirers)
                                        (lispm-desktop-platinum-quoted-names requirers)))))))

(defun lispm-desktop-store--unload (feature)
  "Unload FEATURE when it is loaded, as `unload-feature' does by force;
what cannot be unloaded is said, and stays until Emacs restarts."
  (when (featurep feature)
    (condition-case err
        (unload-feature feature t)
      (error (message (lispm-desktop-tr "App Store: %s stays loaded until Emacs restarts: %s")
                      feature (error-message-string err))))))

(defun lispm-desktop-store--remove-1 (id props)
  "Uninstall the item ID, of PROPS, unasked, and forget it."
  (pcase (plist-get props :kind)
    ('app (lispm-desktop-needs-unguard-all (lispm-desktop-store--needs id))
          (lispm-desktop-store--unload (plist-get props :feature))
          (lispm-desktop-app-window-close id)
          (lispm-desktop-remove-app id))
    ('extension (lispm-desktop-store--unload (plist-get props :feature))))
  (when (plist-get props :owned)
    (lispm-desktop-store--quietly
      (lispm-desktop-store--delete-package (plist-get props :package))))
  (lispm-desktop-store--forget id))

;;;;; What an item leaves

(defun lispm-desktop-store--same-need-p (need other)
  "Return non-nil when NEED and OTHER are the same need."
  (equal (lispm-desktop-needs-key need) (lispm-desktop-needs-key other)))

(defun lispm-desktop-store--needs-leftovers (id)
  "Return what the app ID needs that Homebrew installed and nothing else
uses once it is uninstalled: no other application, installed or built
in, and no command but the app's own, see `lispm-desktop-needs-guarded-p'."
  (let* ((needs (lispm-desktop-store--needs id))
         (own (mapcan (lambda (need) (copy-sequence (lispm-desktop-needs-get need :commands))) needs))
         (others (append (mapcan (pcase-lambda (`(,app . ,_))
                                   (unless (eq app id)
                                     (copy-sequence (ignore-errors (lispm-desktop-app-ui-needs app)))))
                                 lispm-desktop-apps)
                         (mapcan (pcase-lambda (`(,other . ,_))
                                   (unless (eq other id)
                                     (copy-sequence (lispm-desktop-store--needs other))))
                                 lispm-desktop-store-installed))))
    (seq-uniq (seq-filter (lambda (need)
                            (and (lispm-desktop-needs-brewed-p need)
                                 (not (seq-find (lambda (other) (lispm-desktop-store--same-need-p need other))
                                                others))
                                 (not (lispm-desktop-needs-guarded-p need own))))
                          needs)
              #'lispm-desktop-store--same-need-p)))

(defun lispm-desktop-store--package-leftovers (id props)
  "Return the names of the packages the package of the item ID, of PROPS,
brought along that nothing else needs once it is deleted, those needing
another first: none chosen, as package.el keeps the packages installed
by hand, none another item's, none outside `package-user-dir', and none
that a package staying requires.  Only for a package the App Store
installed, which it deletes."
  (when-let* (((plist-get props :owned))
              (name (lispm-desktop-store--package-name (plist-get props :package)))
              ((assq name package-alist)))
    (let ((wanted (append package-selected-packages
                          (mapcar #'car (bound-and-true-p package-vc-selected-packages))
                          (mapcan (pcase-lambda (`(,other . ,other-props))
                                    (when-let* (((not (eq other id)))
                                                (package (plist-get other-props :package)))
                                      (list (lispm-desktop-store--package-name package))))
                                  lispm-desktop-store-installed)))
          (gone (list name))
          (more t))
      ;; A package goes once every package requiring it goes: the
      ;; packages requiring it come before it in GONE, reversed.
      (while more
        (setq more nil)
        (dolist (candidate (mapcan (lambda (package)
                                     (when-let* ((desc (cadr (assq package package-alist))))
                                       (mapcar #'car (package-desc-reqs desc))))
                                   gone))
          (when-let* (((not (memq candidate gone)))
                      ((not (memq candidate wanted)))
                      (desc (cadr (assq candidate package-alist)))
                      ((file-in-directory-p (package-desc-dir desc) package-user-dir))
                      ((seq-every-p (pcase-lambda (`(,other ,other-desc . ,_))
                                      (or (memq other gone)
                                          (not (assq candidate (package-desc-reqs other-desc)))))
                                    package-alist)))
            (push candidate gone)
            (setq more t))))
      (cdr (reverse gone)))))

(defun lispm-desktop-store--and (words)
  "Return WORDS, strings, as a sentence lists them, in the language LispM
speaks."
  (if (cdr words)
      (lispm-desktop-tr "%s and %s" (string-join (butlast words) (lispm-desktop-tr-in "list" ", "))
                        (car (last words)))
    (car words)))

(defun lispm-desktop-store--leftovers-text (needs packages)
  "Return what the alert says of NEEDS and PACKAGES, which uninstalling an
item leaves and nothing else uses, in the language LispM speaks: a
sentence of its own for programs, packages, or both, each whole."
  (let ((programs (and needs (lispm-desktop-store--and (mapcar #'lispm-desktop-needs-name needs))))
        (names (and packages (lispm-desktop-store--and (mapcar #'symbol-name packages)))))
    (cond ((null packages)
           (lispm-desktop-tr-n
            "It brought %s from Homebrew, which nothing else uses: Uninstall leaves it, Uninstall All uninstalls it too."
            "It brought %s from Homebrew, which nothing else uses: Uninstall leaves them, Uninstall All uninstalls them too."
            (length needs) programs))
          ((null needs)
           (lispm-desktop-tr-n
            "It brought the package %s, which nothing else uses: Uninstall leaves it, Uninstall All deletes it too."
            "It brought the packages %s, which nothing else uses: Uninstall leaves them, Uninstall All deletes them too."
            (length packages) names))
          (t (lispm-desktop-tr-n
              "It brought %s from Homebrew and the package %s, which nothing else uses: Uninstall leaves them, Uninstall All removes them too."
              "It brought %s from Homebrew and the packages %s, which nothing else uses: Uninstall leaves them, Uninstall All removes them too."
              (length packages) programs names)))))

(defun lispm-desktop-store--confirm-uninstall (id props needs packages)
  "Ask whether to uninstall the item ID, of PROPS, which leaves NEEDS and
PACKAGES that nothing else uses.  Return nil to keep it, `item' to
uninstall it alone, Uninstall, the default, or `all' for them too,
Uninstall All: an alert with three buttons asks, as Mac OS 9's asked
whether to save, Uninstall All standing apart at the left.  The alert
is as wide as Mac OS 9's, so the buttons do not name what is left; its
words do."
  (let ((question (lispm-desktop-tr "Uninstall “%s”?" (plist-get props :name)))
        (detail (lispm-desktop-store--remove-detail id props)))
    (if (not (or needs packages))
        (and (lispm-desktop-store--confirm question detail nil (lispm-desktop-tr "Uninstall")) 'item)
      (pcase (lispm-desktop-popup-alert
              'caution question
              `((,(lispm-desktop-tr "Uninstall All") all nil ?\M-a) (,(lispm-desktop-tr "Cancel") cancel cancel)
                (,(lispm-desktop-tr "Uninstall") item default))
              (string-trim (concat detail "  " (lispm-desktop-store--leftovers-text needs packages))))
        ((and choice (or 'item 'all)) choice)))))

(defun lispm-desktop-store-uninstall (id)
  "Uninstall the item ID the App Store installed, once confirmed.
What it alone brought, programs Homebrew installed for it and packages
its package brought along, are named in the alert: Uninstall, the
default, leaves them; Uninstall All removes them too, the programs in
the Installer.
Return non-nil when it was uninstalled."
  (interactive (list (lispm-desktop-store--read-item (lispm-desktop-tr "Uninstall: ")
                                                     #'lispm-desktop-store-installed-p)))
  (let ((props (or (lispm-desktop-store--installed id)
                   (user-error (lispm-desktop-tr "The App Store did not install %s") id))))
    (when (lispm-desktop-store--busy-p id)
      (user-error (lispm-desktop-tr "%s is being updated; stop that first") (plist-get props :name)))
    (let* ((needs (and (eq (plist-get props :kind) 'app) (lispm-desktop-store--needs-leftovers id)))
           (packages (lispm-desktop-store--package-leftovers id props))
           (choice (lispm-desktop-store--confirm-uninstall id props needs packages)))
      (when choice
        (lispm-desktop-store--remove-1 id props)
        (when (eq choice 'all)
          (lispm-desktop-store--quietly
            (mapc #'lispm-desktop-store--delete-package packages))
          (when needs
            (lispm-desktop-needs-uninstall needs (plist-get props :name))))
        (lispm-desktop-store--render)
        t))))

(define-obsolete-function-alias 'lispm-desktop-store-remove #'lispm-desktop-store-uninstall "0.9.0")

(defun lispm-desktop-store--changes (installed props)
  "Return what updating the item installed as INSTALLED to PROPS changes,
in the language LispM speaks."
  (concat
   "* " (plist-get props :name) "\n\n"
   (pcase (plist-get props :package)
     (`(,name . ,spec)
      (lispm-desktop-tr "The package %s goes from commit %s to commit %s, of %s."
                        name (plist-get installed :installed) (plist-get spec :rev) (plist-get spec :url)))
     (name
      (lispm-desktop-tr "The package %s goes from version %s to the package archives' newest, %s."
                        name (plist-get installed :installed)
                        (if-let* ((available (cadr (assq name package-archive-contents))))
                            (package-version-join (package-desc-version available))
                          "?"))))
   "\n"))

(defun lispm-desktop-store-update (id &optional quietly)
  "Bring the item ID to what its catalog lists now, once confirmed: its
package to the commit the catalog pins, or to the package archives'
newest.  It is updated in the background, as it is installed, see
`lispm-desktop-store-install'; an extension is loaded again, so the new
code takes part at once.  QUIETLY, as when updates install by
themselves, asks nothing."
  (interactive (list (lispm-desktop-store--read-item (lispm-desktop-tr "Update: ")
                                                     #'lispm-desktop-store-update-available-p)))
  (let* ((installed (or (lispm-desktop-store--installed id)
                        (user-error (lispm-desktop-tr "The App Store did not install %s") id)))
         (props (cdr (assq id (lispm-desktop-store-items)))))
    (unless (lispm-desktop-store-update-available-p id)
      (user-error (lispm-desktop-tr "%s is up to date") (plist-get installed :name)))
    (when (lispm-desktop-store--busy-p id)
      (user-error (lispm-desktop-tr "%s is being updated already") (plist-get installed :name)))
    (when (or quietly
              (lispm-desktop-store--confirm (lispm-desktop-tr "Update “%s”?" (plist-get props :name))
                                            (lispm-desktop-tr "What changes is shown under the window.")
                                            (lispm-desktop-store--changes installed props)
                                            (lispm-desktop-tr "Update")))
      (lispm-desktop-store--queue (list (cons id props)) 'update))))

(defun lispm-desktop-store--update-here (job)
  "Do JOB, an update, in this Emacs, which waits meanwhile, the pointer a
watch over the App Store: its package checked out at the commit its
catalog pins, or the package archives' newest installed."
  (pcase-let* ((`(,_ . ,props) (lispm-desktop-store-job-item job))
               (package (plist-get props :package)))
    (lispm-desktop-store--status (lispm-desktop-tr "Updating %s…" (plist-get props :name)))
    (let ((now (unwind-protect
                   (lispm-desktop-with-loading lispm-desktop-store-buffer-name
                     (if (consp package)
                         (let ((rev (plist-get (cdr package) :rev)))
                           (lispm-desktop-store--quietly
                             (lispm-desktop-store--repin (car package) rev))
                           rev)
                       (lispm-desktop-store--quietly
                         (package-upgrade package))
                       (or (lispm-desktop-store--package-version package) "installed")))
                 (lispm-desktop-store--status nil))))
      (lispm-desktop-store--updated-as job now))))

(defun lispm-desktop-store--updated-as (job now)
  "End JOB, an update, its package now as NOW says, a commit or a version:
keep it, and load an extension again."
  (pcase-let ((`(,id . ,props) (lispm-desktop-store-job-item job)))
    (lispm-desktop-store--keep id props :installed now
                               :owned (plist-get (lispm-desktop-store--installed id) :owned))
    (when (eq (plist-get props :kind) 'extension)
      (lispm-desktop-store--unload (plist-get props :feature)))
    (if-let* ((failure (lispm-desktop-store--failure-of
                        (lambda () (lispm-desktop-store--register id (lispm-desktop-store--installed id))))))
        (lispm-desktop-store--fail job (lispm-desktop-tr "It cannot be loaded: %s" failure))
      (lispm-desktop-store--done job))))

;;;; Updates by themselves
;;
;; The App Store looks for updates whenever Software Update checks for
;; LispM's own, by itself or asked, and as its window opens: its
;; catalogs fetched, and the package archives the items installed come
;; from read anew.  With `lispm-desktop-store-update-automatically'
;; the updates found install once LispM has been idle a minute, without
;; asking, an application only while its window is closed; without it,
;; they wait in the App Store's Updates, and Software Update's control
;; panel says so.

(defvar lispm-desktop-store--update-timer nil
  "Idle timer installing the updates found, while they wait to.")

(defun lispm-desktop-store--update-names (ids)
  "Return the names of the items installed IDS, as a phrase in the
language LispM speaks, see `lispm-desktop-platinum-quoted-names'."
  (lispm-desktop-store--names
   (mapcar (lambda (id)
             (list id :name (or (plist-get (lispm-desktop-store--installed id) :name) (symbol-name id))))
           ids)))

(defun lispm-desktop-store--install-updates ()
  "Install the updates found, quietly, in the background as installs go;
an application open waits for its window to close, and an update
failing is said and left for later."
  (setq lispm-desktop-store--update-timer nil)
  (when lispm-desktop-store-update-automatically
    (let ((waiting nil))
      (dolist (id (lispm-desktop-store-updates))
        (cond ((lispm-desktop-app-window-open-p id) (push id waiting))
              ;; Being updated already, or waiting to be.
              ((lispm-desktop-store--busy-p id))
              ;; Each says when it is done, or why it failed.
              (t (condition-case err
                     (lispm-desktop-store-update id t)
                   (error (message (lispm-desktop-tr "App Store: %s could not be updated: %s")
                                   id (error-message-string err)))))))
      (run-hooks 'lispm-desktop-update-changed-hook)
      (when waiting
        (lispm-desktop-store--install-updates-later)))))

(defun lispm-desktop-store--install-updates-later ()
  "Install the updates found once LispM has been idle a minute, when they
install by themselves."
  (when (and lispm-desktop-store-update-automatically
             (not lispm-desktop-store--update-timer)
             (lispm-desktop-store-updates))
    (setq lispm-desktop-store--update-timer
          (run-with-idle-timer 60 nil #'lispm-desktop-store--install-updates))))

(defun lispm-desktop-store--checked-updates ()
  "Once the catalogs were checked: install the updates found later, by
themselves, or say they are there."
  (if lispm-desktop-store-update-automatically
      (lispm-desktop-store--install-updates-later)
    (when-let* ((ids (lispm-desktop-store-updates)))
      (message "%s" (lispm-desktop-tr-n "App Store: %s can be updated, in its Updates"
                                        "App Store: %s can be updated, in its Updates"
                                        (length ids) (lispm-desktop-store--update-names ids)))))
  (run-hooks 'lispm-desktop-update-changed-hook))

(defun lispm-desktop-store--archive-packages ()
  "Return the packages from the package archives of the items installed."
  (delq nil (mapcar (pcase-lambda (`(,_ . ,props))
                      (let ((package (plist-get props :package)))
                        (and package (symbolp package) package)))
                    lispm-desktop-store-installed)))

(defun lispm-desktop-store--archives-to-refresh ()
  "Return the entries of `package-archives' that hold, as they were read
last, the packages of the items installed from the archives; all of them
when one such package is in none."
  (unless package-archive-contents
    (package-read-all-archive-contents))
  (let ((names nil))
    (catch 'all
      (dolist (package (lispm-desktop-store--archive-packages))
        (let ((descs (cdr (assq package package-archive-contents))))
          (unless descs
            (throw 'all package-archives))
          (dolist (desc descs)
            (cl-pushnew (package-desc-archive desc) names :test #'equal))))
      (seq-filter (lambda (archive) (member (car archive) names)) package-archives))))

(defun lispm-desktop-store--refresh-archives (then)
  "Read the package archives the items installed come from anew, in the
background, then call THEN; at most two minutes go by.  Those archives
are downloaded, see `lispm-desktop-store--archives-to-refresh', the
others read again as they were."
  (if (null (lispm-desktop-store--archive-packages))
      (funcall then)
    (condition-case nil
        (let ((started (current-time))
              (timer nil))
          (let ((package-archives (lispm-desktop-store--archives-to-refresh)))
            (package-refresh-contents t))
          (setq timer (run-at-time 1 1 (lambda ()
                                         (when (or (not (bound-and-true-p package--downloads-in-progress))
                                                   (> (float-time (time-since started)) 120))
                                           (cancel-timer timer)
                                           (funcall then))))))
      (error (funcall then)))))

(defun lispm-desktop-store--check-along ()
  "Check for updates as Software Update checks for LispM's own.
This is one of `lispm-desktop-update-check-hook'."
  (unless lispm-desktop-store--checking
    (lispm-desktop-store-check-for-updates)))

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
    (user-error (lispm-desktop-tr "The App Store lists %s already") url))
  (when (lispm-desktop-store--confirm (lispm-desktop-tr "Add the catalog at %s?" url)
                                      (lispm-desktop-tr "Its items show in the App Store; nothing is installed unasked.")
                                      nil (lispm-desktop-tr "Add"))
    (customize-save-variable 'lispm-desktop-store-catalogs (append lispm-desktop-store-catalogs (list url)))
    (lispm-desktop-store-check-for-updates)))

(defun lispm-desktop-store--install-repository (url repository)
  "Install the Emacs package REPOSITORY, a clone of URL, as an application.
Its command opening it is read; it is pinned to the clone's commit."
  (let* ((repo-name (file-name-base (string-remove-suffix ".git" (directory-file-name url))))
         (main (or (lispm-desktop-store--main-file repository repo-name)
                   (user-error (lispm-desktop-tr "%s holds no Emacs package or catalog") url)))
         (name (intern (file-name-base main)))
         (lisp-dir (let ((directory (file-relative-name (file-name-directory main) repository)))
                     (unless (equal directory "./") (directory-file-name directory))))
         (rev (lispm-desktop-store--git repository "rev-parse" "HEAD"))
         (command (completing-read (lispm-desktop-tr "Command opening %s: " name)
                                   (lispm-desktop-store--autoloaded-commands main)))
         (props `(:kind app :name ,(capitalize (symbol-name name))
                  :description ,(lispm-desktop-tr "Installed from %s." url)
                  :command ,(intern command)
                  :package (,name :url ,url :rev ,rev ,@(and lisp-dir (list :lisp-dir lisp-dir))))))
    (when (string-blank-p command)
      (user-error (lispm-desktop-tr "No command opens %s") name))
    (when (or (lispm-desktop-store-installed-p name) (lispm-desktop-store--busy-p name))
      (user-error (lispm-desktop-tr "%s is installed already") name))
    (when (lispm-desktop-store--confirm-install (list (cons name props)))
      (lispm-desktop-store--queue (list (cons name props)) 'install))))

(defun lispm-desktop-store-install-from-url (url)
  "Install from the git repository at URL, once confirmed.
A repository holding a catalog.eld is a catalog, which joins the App
Store's; any other is an Emacs package, installed as an application
pinned to its newest commit.  The repository is looked at in the
background; what goes wrong is said in an alert."
  (interactive (list (read-string (lispm-desktop-tr "Install from git URL: "))))
  (when (string-blank-p url)
    (user-error (lispm-desktop-tr "No URL")))
  (let ((directory (make-temp-file "lispm-desktop-store-" t))
        (loading (lispm-desktop-loading-start lispm-desktop-store-buffer-name)))
    (lispm-desktop-store--status (lispm-desktop-tr "Looking at %s…" url))
    (condition-case err
        (lispm-desktop-store--git-steps
         `((,directory "clone" "--quiet" "--depth" "1" ,url "repository"))
         (lambda (failure)
           (lispm-desktop-loading-stop loading)
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
                       (lispm-desktop-popup-alert 'stop (lispm-desktop-tr "Nothing could be installed from %s." url)
                                                  `((,(lispm-desktop-tr "OK") ok default))
                                                  (error-message-string err)))))
                (delete-directory directory t))))))
      (error (lispm-desktop-loading-stop loading)
             (lispm-desktop-store--status nil)
             (signal (car err) (cdr err))))))

;;;; Applications and extensions taking part

(defun lispm-desktop-store-open (id)
  "Open the installed application ID, in its window."
  (interactive (list (lispm-desktop-store--read-item
                      (lispm-desktop-tr "Open: ")
                      (lambda (id) (eq (plist-get (lispm-desktop-store--installed id) :kind) 'app)))))
  (let ((props (lispm-desktop-store--installed id)))
    (unless (eq (plist-get props :kind) 'app)
      (user-error (lispm-desktop-tr "%s is no application installed") id))
    (if (plist-get props :feature)
        (lispm-desktop-apps-activate id)
      (lispm-desktop-app-window-show
       id :name (plist-get props :name)
       :setup (lambda () (lispm-desktop-app-ui-run id (plist-get props :command)))))))

(defun lispm-desktop-store--needs (id)
  "Return what the app ID needs beyond Emacs, as its catalog declares it
now, else as it was installed with: a catalog may come to declare it
after the app was installed.  The catalogs are read from their clones,
if they were not yet."
  (unless lispm-desktop-store--catalogs
    (ignore-errors (lispm-desktop-store-read-catalogs)))
  (plist-get (or (cdr (assq id (lispm-desktop-store-items)))
                 (lispm-desktop-store--installed id))
             :needs))

(defconst lispm-desktop-store-app-metadata '(:name :icon :category :description :needs :store)
  "The properties of an installed application, see `lispm-desktop-apps',
that its catalog gives, and nothing else: what it is called, how it
looks, where the App Store files it, what it says of itself and needs.")

(defun lispm-desktop-store--app-behaviour (id props)
  "Return what the installed application ID, of PROPS, does, as properties
of `lispm-desktop-apps' other than `lispm-desktop-store-app-metadata'.
One opened by its :command opens in its window, from the App Store,
with the menus its window works out; one of a :feature, running windows
of its own, does what its library says with `lispm-desktop-define-app'
under ID, the library loaded first, the window's menus where it names
none.  A library that defines no application ID, or none that it
activates, is an error."
  (if-let* ((feature (plist-get props :feature)))
      (progn
        (require feature)
        (let ((own (cdr (assq id lispm-desktop-apps))))
          (unless (plist-get own :activate)
            (error (lispm-desktop-tr "Its library %s defines no application %s") feature id))
          `(,@(apply #'lispm-desktop-store--plist-without own lispm-desktop-store-app-metadata)
            ,@(unless (plist-member own :menus)
                (list :menus (lambda () (lispm-desktop-app-ui-menus id)))))))
    (list :menus (lambda () (lispm-desktop-app-ui-menus id))
          :activate (lambda () (lispm-desktop-store-open id))
          :visible (lambda () (lispm-desktop-app-window-open-p id)))))

(defun lispm-desktop-store--register-app (id props)
  "Make the installed application ID, of PROPS, one of the desktop's: what
it does, see `lispm-desktop-store--app-behaviour', with what its catalog
says of it, `lispm-desktop-store-app-metadata', the icon its library drew
standing in where its catalog's clone holds none.  Its window gets the
desktop's interface, see lispm-desktop-app-ui.el, and what it needs is
offered where it is missing.  This is the one way an application of the
App Store becomes the desktop's, at its install and at every start."
  (let* ((behaviour (lispm-desktop-store--app-behaviour id props))
         (icon (or (lispm-desktop-store--catalog-icon props)
                   (and (plist-get props :feature) (plist-get (cdr (assq id lispm-desktop-apps)) :icon))
                   (lispm-desktop-store--icon props))))
    (apply #'lispm-desktop-define-app
           id :name (plist-get props :name) :icon icon
           :category (lispm-desktop-store--category-of props) :description (plist-get props :description)
           :needs (lambda () (lispm-desktop-store--needs id)) :store t
           behaviour))
  (lispm-desktop-needs-guard-all (plist-get props :needs) (plist-get props :name)))

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
        (error (message (lispm-desktop-tr "App Store: %s: %s") id (error-message-string err)))))))

(defun lispm-desktop-store-application-items ()
  "Return a menu item opening each application built in, see
`lispm-desktop-apps-built-in', named in the language LispM speaks, then
each installed, as its catalog names it, for the λ menu."
  (append
   (mapcar (pcase-lambda (`(,id . ,props))
             (list :label (lispm-desktop-tr (plist-get props :name))
                   :action (lambda () (lispm-desktop-apps-activate id))))
           (lispm-desktop-apps-built-in))
   (mapcan (pcase-lambda (`(,id . ,props))
             (when (eq (plist-get props :kind) 'app)
               (list (list :label (plist-get props :name)
                           :action (lambda () (lispm-desktop-apps-activate id))))))
           lispm-desktop-store-installed)))

(defun lispm-desktop-store--folder-items ()
  "Return the Applications folder's items: the App Store, the applications
built in, see `lispm-desktop-apps-built-in', named in the language LispM
speaks, then those installed, as their catalogs name them, see
`lispm-desktop-finder-folders'.  Those installed are uninstalled when
thrown away, from the folder's menu or onto the Trash."
  `(("App Store" "app-store-32" lispm-desktop-store-show)
    ,@(mapcar (pcase-lambda (`(,id . ,props))
                (list (lispm-desktop-tr (plist-get props :name)) (plist-get props :folder)
                      (lambda () (lispm-desktop-apps-activate id))))
              (lispm-desktop-apps-built-in))
    ,@(mapcan (pcase-lambda (`(,id . ,props))
                (when (eq (plist-get props :kind) 'app)
                  (list (list (plist-get props :name) (lispm-desktop-store--icon props t)
                              (lambda () (lispm-desktop-apps-activate id))
                              (lambda () (lispm-desktop-store-uninstall id))))))
              lispm-desktop-store-installed)))

(defun lispm-desktop-store-open-applications ()
  "Open the Applications folder in the Finder window."
  (interactive)
  (lispm-desktop-finder-open-directory 'applications))

;;;; The window

(defvar-local lispm-desktop-store--view 'grid
  "What the window shows: `grid', a page of the items of its category or of
its search, or `item', the page of the item selected.")

(defvar-local lispm-desktop-store--drawn nil
  "What the window showed when last drawn, as `lispm-desktop-store--page'
says it, or nil before.")

(defun lispm-desktop-store--page ()
  "Return what the window shows, telling one page from another: `grid', or
\(item . ID) for the page of the item ID."
  (if (eq lispm-desktop-store--view 'item)
      (cons 'item lispm-desktop-store--selected)
    lispm-desktop-store--view))

(defvar-local lispm-desktop-store--height nil
  "The height in pixels the window was drawn for.")

(defvar-local lispm-desktop-store--rows-height nil
  "The height in pixels of the rows over and under the grid, as measured
once drawn: the fonts set it.")

(defvar-local lispm-desktop-store--field nil
  "The overlay over the search field's text, once the window is drawn.")

(defvar-local lispm-desktop-store--field-focused nil
  "Non-nil while the search field was last drawn with the keyboard.")

(defvar-local lispm-desktop-store--search-timer nil
  "The timer showing what the search field holds matches, once typing pauses.")

(defconst lispm-desktop-store-search-delay 0.15
  "Seconds typing in the search field pauses before the grid shows what
the search matches.")

(defvar lispm-desktop-store-field-map)
(defvar lispm-desktop-store-clear-map)
(defvar lispm-desktop-store-page-map)

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
          (user-error (lispm-desktop-tr "There is nothing to choose from")))
        (cdr (assoc (completing-read prompt names nil t) names)))))

;;;;; What shows

(defun lispm-desktop-store--in-category-p (item category)
  "Return non-nil when ITEM, (ID . PROPERTIES), is one of CATEGORY's."
  (pcase category
    ('all t)
    ('updates (lispm-desktop-store-update-available-p (car item)))
    (_ (eq (lispm-desktop-store--category-of (cdr item)) category))))

(defun lispm-desktop-store--words (filter)
  "Return the words of FILTER, what is searched, in lower case, or nil."
  (and filter (split-string (downcase filter) "[ \t\n]+" t)))

(defun lispm-desktop-store--rank (item words)
  "Return how well ITEM, (ID . PROPERTIES), matches WORDS, each of which it
must hold somewhere: 0 when its name starts with the first, 1 when its
name holds them all, 2 otherwise; nil when one of them is nowhere in its
name, subtitle, description or category, named in English or in the
language LispM speaks."
  (let* ((props (cdr item))
         (name (downcase (plist-get props :name)))
         (category (lispm-desktop-store--category-of props))
         (text (downcase (string-join (delq nil (list name (plist-get props :subtitle)
                                                      (plist-get props :description)
                                                      (cadr (assq category lispm-desktop-store-categories))
                                                      (lispm-desktop-store--category-name category)))
                                      "\n"))))
    (when (seq-every-p (lambda (word) (string-search word text)) words)
      (cond ((string-prefix-p (car words) name) 0)
            ((seq-every-p (lambda (word) (string-search word name)) words) 1)
            (t 2)))))

(defun lispm-desktop-store--matches-p (item filter)
  "Return non-nil when FILTER is nil, or ITEM holds each of its words, see
`lispm-desktop-store--rank'."
  (let ((words (lispm-desktop-store--words filter)))
    (or (null words) (and (lispm-desktop-store--rank item words) t))))

(defun lispm-desktop-store--searching-p ()
  "Return non-nil while the window shows what a search matches."
  (and (lispm-desktop-store--words lispm-desktop-store--filter) t))

(defun lispm-desktop-store--categories ()
  "Return the categories listed down the window's left: All, those holding
an item, and Updates, as (CATEGORY NAME COUNT), NAME in the language
LispM speaks."
  (let ((items (lispm-desktop-store-items)))
    (delq nil (mapcar (pcase-lambda (`(,category ,name))
                        (let ((count (seq-count (lambda (item) (lispm-desktop-store--in-category-p item category))
                                                items)))
                          (when (or (memq category '(all updates)) (> count 0))
                            (list category (lispm-desktop-tr name) count))))
                      lispm-desktop-store-categories))))

(defun lispm-desktop-store--category-name (category)
  "Return the name of CATEGORY in the language LispM speaks, or nil."
  (when-let* ((name (cadr (assq category lispm-desktop-store-categories))))
    (lispm-desktop-tr name)))

(defun lispm-desktop-store--current-category ()
  "Return the category the list highlights: the one shown, or nil while a
search is."
  (unless (lispm-desktop-store--searching-p)
    lispm-desktop-store--category))

(defun lispm-desktop-store--shown ()
  "Return the items the window shows: those its search matches, the best
first, then by name; without a search, its category's, by name.  A
search looks through every category."
  (let ((words (lispm-desktop-store--words lispm-desktop-store--filter))
        (by-name (lambda (a b) (string-lessp (downcase (plist-get (cdr a) :name))
                                             (downcase (plist-get (cdr b) :name))))))
    (if words
        (mapcar #'cdr (sort (delq nil (mapcar (lambda (item)
                                                (when-let* ((rank (lispm-desktop-store--rank item words)))
                                                  (cons rank item)))
                                              (lispm-desktop-store-items)))
                            (lambda (a b) (if (= (car a) (car b))
                                              (funcall by-name (cdr a) (cdr b))
                                            (< (car a) (car b))))))
      (sort (seq-filter (lambda (item) (lispm-desktop-store--in-category-p item lispm-desktop-store--category))
                        (lispm-desktop-store-items))
            by-name))))

(defun lispm-desktop-store--empty-text ()
  "Return what the grid says when it shows nothing."
  (cond ((lispm-desktop-store--searching-p)
         (lispm-desktop-tr "Nothing matches “%s”." (string-trim lispm-desktop-store--filter)))
        ((eq lispm-desktop-store--category 'updates) (lispm-desktop-tr "Everything is up to date."))
        (t (lispm-desktop-tr "Nothing here yet."))))

(defun lispm-desktop-store--title ()
  "Return what the row over the grid names: the category shown, or Search
Results while a search shows."
  (if (lispm-desktop-store--searching-p)
      (lispm-desktop-tr "Search Results")
    (or (lispm-desktop-store--category-name lispm-desktop-store--category) "")))

;;;;; Geometry (pure)

(defconst lispm-desktop-store-text-categories-width 22
  "Width in cells of the list of categories on a text terminal.")

(defun lispm-desktop-store--tile-height ()
  "Return the height in pixels of a line of the grid: a line of text on a
text terminal, where the items are listed a line each."
  (if (lispm-desktop-tty-p) 1 lispm-desktop-store-tile-height))

(defun lispm-desktop-store--grid (width)
  "Return the grid's layout in a window WIDTH pixels wide: (X COLUMNS
TILE-WIDTH), X where it starts, right of the categories.  On a text
terminal the items are listed a line each, see
`lispm-desktop-store--insert-text-grid'."
  (if (lispm-desktop-tty-p)
      (list (+ lispm-desktop-store-text-categories-width 2) 1
            (- width lispm-desktop-store-text-categories-width 2))
    (let* ((x (+ lispm-desktop-store-margin lispm-desktop-store-sidebar-width lispm-desktop-store-margin))
           (room (max lispm-desktop-store-tile-width (- width x lispm-desktop-store-margin)))
           (columns (max 1 (/ room lispm-desktop-store-tile-width))))
      (list x columns (/ room columns)))))

(defun lispm-desktop-store--categories-height (count)
  "Return the height in pixels of the list of COUNT categories, a line each
on a text terminal."
  (if (lispm-desktop-tty-p)
      count
    (+ 2 (* count lispm-desktop-store-category-height))))

(defconst lispm-desktop-store-rows-height 100
  "Height in pixels of the window's rows over and under the grid, their
gaps included: the search field's and the push buttons', until they are
drawn and measured, see `lispm-desktop-store--rows-height'.")

(defun lispm-desktop-store--page-lines (height categories &optional rows)
  "Return how many lines a page of the grid takes in a window HEIGHT pixels
tall: as many as fit between the rows over and under it, ROWS pixels
tall, by default `lispm-desktop-store-rows-height'; at least one, and
enough for the list of CATEGORIES, a count, beside them."
  (let ((tile (lispm-desktop-store--tile-height))
        ;; Until they are measured, the rows take some lines of text.
        (rows (or rows (if (lispm-desktop-tty-p) 6 lispm-desktop-store-rows-height))))
    (max 1
         (ceiling (lispm-desktop-store--categories-height categories) (float tile))
         (/ (- height rows) tile))))

(defun lispm-desktop-store--pages (count size)
  "Return how many pages COUNT items take, SIZE of them a page; one at least."
  (max 1 (ceiling count (float size))))

(defun lispm-desktop-store--button-box (x width label)
  "Return where the button reading LABEL of the item drawn at X, WIDTH
pixels wide, stands: (X Y WIDTH), Y from the top of the item's line."
  (let ((w (lispm-desktop-platinum-push-button-width label)))
    (list (+ x (/ (- width w) 2)) 84 w)))

(defun lispm-desktop-store--category-hit (x y categories)
  "Return (category . CATEGORY) when X, Y falls on one of CATEGORIES, as the
list down the window's left shows them, Y from the list's top; else nil."
  (when (and (>= x lispm-desktop-store-margin)
             (< x (+ lispm-desktop-store-margin lispm-desktop-store-sidebar-width)))
    (let ((index (floor (1- y) lispm-desktop-store-category-height)))
      (when (and (>= y 1) (< index (length categories)))
        (cons 'category (car (nth index categories)))))))

(defun lispm-desktop-store--hit (width line x y categories items)
  "Return what stands at X, Y of the grid's LINE, the window WIDTH pixels
wide, listing CATEGORIES beside ITEMS, those of the page shown: (category
. CATEGORY), (button . ID), the button of the item ID, (item . ID), or
nil.  The progress bar standing in place of a button is the item's."
  (pcase-let* ((`(,gx ,columns ,tw) (lispm-desktop-store--grid width)))
    (cond
     ((< x (+ lispm-desktop-store-margin lispm-desktop-store-sidebar-width))
      (lispm-desktop-store--category-hit x (+ (* line lispm-desktop-store-tile-height) y) categories))
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

;;;;; Pages of the grid

(defun lispm-desktop-store--page-size ()
  "Return how many items a page of the grid holds, in the window as last
drawn."
  (* (nth 1 (lispm-desktop-store--grid (or lispm-desktop-store--width 640)))
     (lispm-desktop-store--page-lines (or lispm-desktop-store--height 600)
                                      (length (lispm-desktop-store--categories))
                                      lispm-desktop-store--rows-height)))

(defun lispm-desktop-store--page-of (items size)
  "Return which page of ITEMS, SIZE of them a page, holds the item
selected, the first counted 0: 0 when none does."
  (/ (or (seq-position (mapcar #'car items) lispm-desktop-store--selected) 0) size))

(defun lispm-desktop-store--page-items ()
  "Return the items of the grid's page shown: the one holding the item
selected."
  (let* ((items (lispm-desktop-store--shown))
         (size (lispm-desktop-store--page-size)))
    (seq-take (nthcdr (* size (lispm-desktop-store--page-of items size)) items) size)))

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
         ;; Shortened to leave room for its count, as a translation may be long.
         (lispm-desktop-platinum-svg-content
          svg (lispm-desktop-store--fit name (- w 22 (lispm-desktop-platinum-content-width number small)) face)
          (+ x 8) y rh ink face)
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
                               (lispm-desktop-tr "Could not install")
                             (lispm-desktop-tr "Could not update")))
                          (t (concat (lispm-desktop-tr
                                      (nth 1 (assq (lispm-desktop-store-job-stage job) lispm-desktop-store-stages)))
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

(defun lispm-desktop-store--draw-icon (svg file x y size &optional darkened)
  "Draw the icon FILE on SVG at X, Y, SIZE pixels square, DARKENED as Mac OS
9 darkened an icon selected."
  (cond ((not (file-readable-p file)))
        ((not (string-suffix-p ".svg" file))
         (lispm-desktop-platinum-svg-emacs-image svg (create-image file) x y size size))
        (darkened
         (when-let* ((dom (lispm-desktop-store--darkened file)))
           (lispm-desktop-platinum-svg-embed svg dom x y size size)))
        (t (lispm-desktop-platinum-svg-icon svg file x y size))))

(defun lispm-desktop-store--draw-tile (svg palette tile x width)
  "Draw TILE, as `lispm-desktop-store--tile' makes it, on SVG at X, WIDTH
pixels wide: its icon, darkened when selected, its name, highlighted
when selected, its subtitle and its button, or its progress bar."
  (let* ((center (+ x (/ width 2)))
         (selected (plist-get tile :selected))
         (highlight (lispm-desktop-store--selected-face))
         (face 'lispm-desktop-platinum-list)
         (small 'lispm-desktop-platinum-small))
    (lispm-desktop-store--draw-icon svg (lispm-desktop-platinum-icon-file (plist-get tile :icon))
                                    (- center 16) 8 32 selected)
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

(defun lispm-desktop-store--text-category (category)
  "Return CATEGORY, as `lispm-desktop-store--categories' lists it, as a line
of the list of categories on a text terminal: its name and count, the
one shown highlighted, carrying (category . KEY) as its
`lispm-desktop-store-hit' property; nil a blank as wide, then the rule
between the list and what is beside it."
  (concat (if-let* ((category category))
              (pcase-let* ((`(,key ,name ,count) category)
                           (number (format "%d" count))
                           (left lispm-desktop-store-text-categories-width))
                (propertize (concat " " (truncate-string-to-width
                                         name (- left 3 (length number)) nil ?\s "…")
                                    number " ")
                            'face (if (eq key (lispm-desktop-store--current-category))
                                      (lispm-desktop-store--selected-face)
                                    'lispm-desktop-platinum-list)
                            'lispm-desktop-store-hit (cons 'category key)))
            (make-string (1- lispm-desktop-store-text-categories-width) ?\s))
          (propertize " │ " 'face `(:foreground ,(lispm-desktop-platinum-color :muted)))))

(defun lispm-desktop-store--text-progress (fraction)
  "Return a progress bar filled to FRACTION, as text eleven cells wide."
  (let ((done (round (* 6 (max 0 (min 1 fraction))))))
    (concat (make-string done ?█) (make-string (- 6 done) ?░)
            (format " %3d%%" (round (* 100 fraction))))))

(defun lispm-desktop-store--insert-text-grid (width height)
  "Insert the categories and the page of the grid on a text terminal, as
text WIDTH cells wide and as many lines as a page of HEIGHT lines takes:
the categories down the left and beside them an item a line, its icon,
name and subtitle, and at the right its button, or how far it is
installed; the item selected is highlighted.  Each part carries what it
is as its `lispm-desktop-store-hit' property, as
`lispm-desktop-store--hit' says it: (category . CATEGORY), (item . ID)
or (button . ID)."
  (let* ((categories (lispm-desktop-store--categories))
         (items (lispm-desktop-store--page-items))
         (empty (and (null items) (lispm-desktop-store--empty-text)))
         (dim `(:foreground ,(lispm-desktop-platinum-color :muted))))
    (dotimes (line (lispm-desktop-store--page-lines height (length categories)
                                                    lispm-desktop-store--rows-height))
      (let ((start (point))
            (item (nth line items)))
        (insert (lispm-desktop-store--text-category (nth line categories)))
        (cond
         (item
          (let* ((tile (lispm-desktop-store--tile item))
                 (id (plist-get tile :id))
                 (button (if-let* ((progress (plist-get tile :progress)))
                             (lispm-desktop-store--text-progress progress)
                           (lispm-desktop-platinum-text-button
                            (concat " " (plist-get tile :label) " ")
                            (cond ((not (plist-get tile :enabled)) 'disabled)
                                  ((plist-get tile :pressed) 'pressed)))))
                 (room (- width lispm-desktop-store-text-categories-width 6 (string-width button)))
                 (subtitle (plist-get tile :subtitle))
                 (text (truncate-string-to-width
                        (concat (substring-no-properties
                                 (lispm-desktop-platinum-icon (plist-get tile :icon)))
                                " " (plist-get tile :name)
                                (when subtitle (concat "  " (propertize subtitle 'face dim))))
                        (max 0 room) nil nil "…")))
            (insert (propertize (concat text " ")
                                'face (if (plist-get tile :selected)
                                          (lispm-desktop-store--selected-face)
                                        'lispm-desktop-platinum-list)
                                'lispm-desktop-store-hit (cons 'item id))
                    (propertize " " 'display `(space :align-to ,(lispm-desktop-align-right
                                                                 (string-width button)))
                                'lispm-desktop-store-hit (cons 'item id))
                    (propertize button 'lispm-desktop-store-hit (cons 'button id)))))
         ((and empty (= line 0))
          (insert (propertize empty 'face dim))))
        (add-text-properties start (point) (list 'lispm-desktop-store-line line
                                                 'keymap lispm-desktop-store-grid-map
                                                 'pointer 'arrow))
        (insert "\n")))))

(defun lispm-desktop-store--insert-grid (width height)
  "Insert the categories and the page of the grid holding the item
selected, the window WIDTH by HEIGHT pixels: a line of items each, drawn
as one image, as many lines as a page takes, so that the push buttons
under them stay where they are from page to page.  On a text terminal
it is text, see `lispm-desktop-store--insert-text-grid'."
  (if (lispm-desktop-tty-p)
      (lispm-desktop-store--insert-text-grid width height)
    (lispm-desktop-store--insert-image-grid width height)))

(defun lispm-desktop-store--insert-image-grid (width height)
  "Insert the categories and the page of the grid as images, the window
WIDTH by HEIGHT pixels, see `lispm-desktop-store--insert-grid'."
  (pcase-let* ((`(,_ ,columns ,_) (lispm-desktop-store--grid width))
               (categories (lispm-desktop-store--categories))
               (items (lispm-desktop-store--page-items))
               (empty (and (null items) (lispm-desktop-store--empty-text)))
               (current (lispm-desktop-store--current-category)))
    (dotimes (line (lispm-desktop-store--page-lines height (length categories)
                                                    lispm-desktop-store--rows-height))
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

(defun lispm-desktop-store--button-width (label &optional default)
  "Return the width in pixels a push button reading LABEL takes on its line,
its rings' room included: the default button's ring too when DEFAULT.
On a text terminal, the cells it takes."
  (or (and (lispm-desktop-tty-p)
           (string-width (lispm-desktop-platinum-image-of (lispm-desktop-platinum-push-button label))))
      (ignore-errors
        (car (image-size (lispm-desktop-platinum-image-of
                          (lispm-desktop-platinum-push-button label :default default))
                         t)))
      (+ (lispm-desktop-platinum-push-button-width label) (if default 16 10))))

;;;;; The rows over and under the grid

(defun lispm-desktop-store--insert-field ()
  "Insert the search field's text, the search, at point; return where it
stands, (START . END).  Its overlay is put over it once the whole window
is drawn, see `lispm-desktop-store--protect'."
  (let ((start (point)))
    (insert (or lispm-desktop-store--filter ""))
    (cons start (point))))

(defun lispm-desktop-store--insert-top (width)
  "Insert the row over the grid, the window WIDTH pixels wide: the search
field, over the categories; then what the grid shows, or on an item's
page Go Back; and Check for Updates.  Return where the field's text is, as
`lispm-desktop-store--insert-field' does."
  (unless (lispm-desktop-tty-p)
    (insert (propertize " " 'display '(space :height (8))) "\n"))
  (let* ((label (lispm-desktop-tr "Check for Updates"))
         (gx (car (lispm-desktop-store--grid width)))
         (margin (lispm-desktop-px lispm-desktop-store-margin))
         (right (+ margin (lispm-desktop-store--button-width label)))
         field)
    (insert (propertize " " 'display `(space :width (,margin))))
    (setq field (lispm-desktop-store--insert-field))
    (insert (propertize " " 'display `(space :align-to (,gx))))
    (if (eq lispm-desktop-store--view 'item)
        (lispm-desktop-platinum-insert-push-button (lispm-desktop-tr "Go Back") #'lispm-desktop-store-back
                                                   :help (lispm-desktop-tr "Go back to the items"))
      (insert (lispm-desktop-store--fit (lispm-desktop-store--title) (- width gx right (lispm-desktop-px 12))
                                        '(:inherit lispm-desktop-platinum-list :weight bold))))
    (insert (propertize " " 'display `(space :align-to ,(lispm-desktop-align-right right))))
    (lispm-desktop-platinum-insert-push-button label #'lispm-desktop-store-check-for-updates
                                               :help (lispm-desktop-tr "Fetch the catalogs again")
                                               :disabled lispm-desktop-store--checking)
    (insert "\n" (if (lispm-desktop-tty-p) "" (concat (propertize " " 'display '(space :height (8))) "\n")))
    field))

(defun lispm-desktop-store--insert-row (left right)
  "Insert a row of push buttons, LEFT from the window's left edge and RIGHT
flush with its right edge, each (LABEL COMMAND DISABLED DEFAULT), or a
string, said between them."
  (let* ((gap (lispm-desktop-px 10))
         (margin (lispm-desktop-px lispm-desktop-store-margin))
         (face 'lispm-desktop-platinum-list)
         (width (lambda (button)
                  (if (stringp button)
                      (lispm-desktop-platinum-content-width button face)
                    (lispm-desktop-store--button-width (car button) (nth 3 button)))))
         (room (+ (apply #'+ (mapcar width right)) (* gap (max 0 (1- (length right))))))
         (insert-all
          (lambda (buttons)
            (seq-do-indexed
             (lambda (button index)
               (when (> index 0)
                 (insert (lispm-desktop-platinum-space gap)))
               (pcase button
                 ((pred stringp) (insert (propertize button 'face face)))
                 (`(,label ,command ,disabled . ,default)
                  (lispm-desktop-platinum-insert-push-button label (lambda () (call-interactively command))
                                                             :disabled disabled :default (car default)))))
             buttons))))
    (unless (lispm-desktop-tty-p)
      (insert (propertize " " 'display '(space :height (6))) "\n"))
    (insert (lispm-desktop-platinum-space margin))
    (funcall insert-all left)
    (insert (propertize " " 'display `(space :align-to ,(lispm-desktop-align-right (+ margin room)))))
    (funcall insert-all right)
    (insert "\n")))

(defun lispm-desktop-store--pager (page pages)
  "Return Previous and Next with the page between, as
`lispm-desktop-store--insert-row' takes them, PAGE of PAGES shown, the
first 0; nil when there is one page."
  (when (> pages 1)
    `((,(lispm-desktop-tr-in "page" "Previous") lispm-desktop-store-previous-page ,(= page 0))
      ,(lispm-desktop-tr "Page %d of %d" (1+ page) pages)
      (,(lispm-desktop-tr-in "page" "Next") lispm-desktop-store-next-page ,(= page (1- pages))))))

(defun lispm-desktop-store--insert-buttons ()
  "Insert the push buttons under the grid: Install from URL…, and, while
there are pages, Previous and Next with the page between; under an
item's page, its buttons, see `lispm-desktop-store--item-buttons', and
Previous and Next while it takes pages."
  (if (eq lispm-desktop-store--view 'item)
      (lispm-desktop-store--insert-row
       (lispm-desktop-store--pager lispm-desktop-store--page-number lispm-desktop-store--page-count)
       (lispm-desktop-store--item-buttons))
    (let* ((items (lispm-desktop-store--shown))
           (size (lispm-desktop-store--page-size)))
      (lispm-desktop-store--insert-row
       `((,(lispm-desktop-tr "Install from URL…") lispm-desktop-store-install-from-url nil))
       (lispm-desktop-store--pager (lispm-desktop-store--page-of items size)
                                   (lispm-desktop-store--pages (length items) size))))))

(defun lispm-desktop-store--item-buttons ()
  "Return the push buttons under the selected item's page, as
`lispm-desktop-store--insert-row' takes them: Show Log for an item that
failed to install or update, Install Missing for one whose needs the
Installer installs, Copy Install Command for one whose program it does
not, Uninstall…, and the item's button, Get, Open, Update, Stop or Try
Again, the default."
  (let* ((id lispm-desktop-store--selected)
         (props (and id (cdr (assq id (lispm-desktop-store-items)))))
         (action (and props (lispm-desktop-store--action id props)))
         (job (and id (lispm-desktop-store--job id))))
    `(,@(when (and job (eq (lispm-desktop-store-job-state job) 'failed)
                   (buffer-live-p (lispm-desktop-store-job-log job)))
          `((,(lispm-desktop-tr "Show Log") lispm-desktop-store-show-log-selected nil)))
      ,@(cond ((and props (lispm-desktop-store--needs-to-install props))
               `((,(lispm-desktop-tr "Install Missing") lispm-desktop-store-install-missing nil)))
              ((and props (lispm-desktop-store--install-commands props))
               `((,(lispm-desktop-tr "Copy Install Command") lispm-desktop-store-copy-install-command nil))))
      (,(lispm-desktop-tr "Uninstall…") lispm-desktop-store-uninstall-selected
       ,(not (and id (lispm-desktop-store-installed-p id) (not (lispm-desktop-store--busy-p id)))))
      (,(or (car action) (lispm-desktop-tr "Get")) lispm-desktop-store-act-selected ,(not (cdr action)) t))))

;;;;; An item's page

(defconst lispm-desktop-store-head-height 92
  "Height in pixels of the head of an item's page: its icon, name and more.")

(defconst lispm-desktop-store-row-height 16
  "Height in pixels of a line of text of an item's page.")

(defun lispm-desktop-store--pieces (word width face)
  "Return WORD in FACE, under the faces of its own, cut into pieces of at
most WIDTH pixels, after a slash or a hyphen where one is: a file name
or a URL, as an error names.  A word that fits is its own piece; so is a
character, however narrow WIDTH, so that cutting ends."
  (let ((pieces nil))
    (cl-flet ((fits (text) (<= (lispm-desktop-platinum-content-width text face) width)))
      (while (and (> (length word) 1) (not (fits word)))
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
  "Return TEXT in FACE, under the faces of its own, broken into lines of
at most WIDTH pixels, between words or the characters of Chinese and
Japanese, and where it holds a newline; a word too long for a line is
cut, see `lispm-desktop-store--pieces'.  The lines keep TEXT's
properties."
  (unless (member text '(nil ""))
    (mapcan (lambda (paragraph)
              (mapcan (lambda (line) (lispm-desktop-store--pieces line width face))
                      (or (lispm-desktop-i18n-wrap
                           paragraph
                           (lambda (line) (<= (lispm-desktop-platinum-content-width line face) width)))
                          (list ""))))
            (split-string text "\n"))))

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

(defun lispm-desktop-store--license (license)
  "Return LICENSE, an SPDX expression, as an item's information shows it: a
licence SPDX does not list, LicenseRef-NAME, in words."
  (if license
      (replace-regexp-in-string "LicenseRef-\\([-.A-Za-z0-9]+\\)"
                                (lambda (ref) (string-replace "-" " " (match-string 1 ref)))
                                license t t)
    "–"))

(defun lispm-desktop-store--fields (id)
  "Return the information shown of the item ID, as (LABEL VALUE), in the
language LispM speaks."
  (let* ((props (cdr (assq id (lispm-desktop-store-items))))
         (installed (lispm-desktop-store--installed id))
         (program (lispm-desktop-store--missing-program props)))
    `((,(lispm-desktop-tr "Category:")
       ,(or (lispm-desktop-store--category-name (lispm-desktop-store--category-of props)) "–"))
      (,(lispm-desktop-tr "Version:") ,(lispm-desktop-store--version id props))
      (,(lispm-desktop-tr "Status:")
       ,(cond ((lispm-desktop-store--job id) (lispm-desktop-store--progress (lispm-desktop-store--job id)))
              ((lispm-desktop-store-update-available-p id) (lispm-desktop-tr "Installed; an update is ready"))
              (installed (lispm-desktop-tr "Installed"))
              (program (lispm-desktop-tr "Needs %s, not on PATH" program))
              (t (lispm-desktop-tr "Not installed"))))
      (,(lispm-desktop-tr "Developer:") ,(or (plist-get props :author) "–"))
      (,(lispm-desktop-tr "Licence:") ,(lispm-desktop-store--license (plist-get props :license)))
      ,@(when-let* ((requires (plist-get props :requires)))
          `((,(lispm-desktop-tr "Requires:")
             ,(mapconcat (lambda (other)
                           (or (plist-get (cdr (assq other (lispm-desktop-store-items))) :name)
                               (symbol-name other)))
                         requires ", "))))
      ,@(when-let* ((needs (append (ensure-list (plist-get props :program))
                                   (mapcar #'cadr (plist-get props :needs)))))
          `((,(lispm-desktop-tr "Needs:") ,(string-join needs ", "))))
      (,(lispm-desktop-tr "From:") ,(lispm-desktop-store--origin props))
      (,(lispm-desktop-tr "Where:") ,(if installed (lispm-desktop-store--where installed) "–")))))

(defun lispm-desktop-store--notes (props)
  "Return what the item of PROPS lacks, in sentences of the language LispM
speaks, each after two spaces, or \"\": how to install the program it
needs when PATH lacks it, and what of an app's needs is missing."
  (concat (when-let* ((program (lispm-desktop-store--missing-program props)))
            (concat "  " (if-let* ((install (plist-get props :install)))
                             (lispm-desktop-tr "It needs %s, which is not on PATH; install it with: %s"
                                               program install)
                           (lispm-desktop-tr "It needs %s, which is not on PATH." program))))
          (mapconcat (lambda (need)
                       (unless (lispm-desktop-needs-present-p need)
                         (concat "  " (lispm-desktop-needs-text nil need))))
                     (plist-get props :needs))
          (when-let* (((not (lispm-desktop-store--missing-program props)))
                      ((not (lispm-desktop-store--needs-to-install props)))
                      (install (lispm-desktop-store--install-commands props)))
            (concat "  " (lispm-desktop-tr "Install them with: %s" install)))))

(defun lispm-desktop-store--description (props)
  "Return the description of the item of PROPS, as its catalog gives it,
with what it lacks, see `lispm-desktop-store--notes'."
  (concat (or (plist-get props :description) "") (lispm-desktop-store--notes props)))

(defvar lispm-desktop-store--abouts (make-hash-table :test #'equal)
  "The introductions read, by (FILE . MODIFIED): their blocks.")

(defun lispm-desktop-store--about-file (props)
  "Return the file of the introduction of the item of PROPS in its
catalog's clone, in the language LispM speaks where the catalog has it
translated, or nil when the clone holds none."
  (when-let* ((name (plist-get props :about))
              ((lispm-desktop-catalog-safe-file-p name))
              ((plist-get props :catalog)))
    (seq-find #'file-readable-p
              (list (lispm-desktop-store--file
                     props (format "about/%s.%s.md" name (lispm-desktop-current-language)))
                    (lispm-desktop-store--file props (format "about/%s.md" name))))))

(defun lispm-desktop-store--about (props)
  "Return the blocks of the introduction of the item of PROPS, Markdown
read as lispm-desktop-markdown.el reads it, or nil when it has none.  It
is read again once its file changed."
  (when-let* ((file (lispm-desktop-store--about-file props)))
    (with-memoization (gethash (cons file (file-attribute-modification-time (file-attributes file)))
                               lispm-desktop-store--abouts)
      (condition-case err
          (lispm-desktop-catalog--about-blocks file)
        (error (message (lispm-desktop-tr "App Store: cannot read %s: %s") file (error-message-string err))
               nil)))))

(defun lispm-desktop-store--banner (props)
  "Return the file of the banner of the item of PROPS in its catalog's
clone, or nil when the clone holds none."
  (when-let* ((name (plist-get props :banner))
              ((lispm-desktop-catalog-safe-file-p name))
              ((plist-get props :catalog))
              (file (lispm-desktop-store--file props (format "banners/%s.svg" name)))
              ((file-readable-p file)))
    file))

(defconst lispm-desktop-store-picture-scale 3
  "Most times its own pixels an SVG of pixels, as a banner, is drawn, in
a window wide enough, unless it says it is larger.")

(defun lispm-desktop-store--picture-size (file room)
  "Return the size to draw the picture FILE at on an item's page, ROOM
pixels wide, as (WIDTH . HEIGHT), or nil when it cannot be read.  An SVG
of pixels, as a banner, is drawn at a whole multiple of its pixels, as
large as fits, up to `lispm-desktop-store-picture-scale' times them, but
never smaller than its size, unless narrower to fit; another picture at
its size, or narrower to fit."
  (when-let* ((size (if (string-suffix-p ".svg" file)
                        (when-let* ((dom (lispm-desktop-platinum-svg-dom file)))
                          (let* ((box (mapcar #'string-to-number
                                              (split-string (or (dom-attr dom 'viewBox) "") "[ ,]+" t)))
                                 (width (string-to-number (or (dom-attr dom 'width) "0")))
                                 (height (string-to-number (or (dom-attr dom 'height) "0"))))
                            (cond ((and (= (length box) 4) (> (nth 2 box) 0) (> (nth 3 box) 0))
                                   (list (nth 2 box) (nth 3 box) (max width (nth 2 box))))
                                  ((and (> width 0) (> height 0)) (list width height width)))))
                      (ignore-errors (pcase-let ((`(,w . ,h) (image-size (create-image file) t)))
                                       (list w h w)))))
              ((> (car size) 0)))
    (pcase-let* ((`(,unit ,unit-height ,own) size)
                 (width (cond ((< room own) room)
                              ((string-suffix-p ".svg" file)
                               (* unit (min (floor room unit)
                                            (max lispm-desktop-store-picture-scale (floor own unit)))))
                              (t own))))
      (cons (round width) (round (* width (/ (float unit-height) unit)))))))

(defun lispm-desktop-store--failure (id)
  "Return why installing or updating the item ID failed, as sentences to
put before its description, in the language LispM speaks, or \"\"."
  (let ((job (lispm-desktop-store--job id)))
    (if (and job (eq (lispm-desktop-store-job-state job) 'failed))
        (let ((why (string-trim-right (lispm-desktop-store-job-error job) "[ .]+")))
          (concat (if (eq (lispm-desktop-store-job-action job) 'install)
                      (lispm-desktop-tr "It could not be installed: %s." why)
                    (lispm-desktop-tr "It could not be updated: %s." why))
                  "  "))
      "")))

(defun lispm-desktop-store--head (id)
  "Return what the head of the page of the item ID shows, as a plist: its
:icon, :name, :subtitle, the name of its :catalog, and while it is
installed or updated, how far, :progress, as its tile shows it."
  (let* ((item (assq id (lispm-desktop-store-items)))
         (tile (lispm-desktop-store--tile item)))
    (list :icon (plist-get tile :icon) :name (plist-get tile :name) :subtitle (plist-get tile :subtitle)
          :catalog (lispm-desktop-store--catalog-name (plist-get (cdr item) :catalog))
          :progress (plist-get tile :progress))))

(defconst lispm-desktop-store-markdown-indent 18
  "Pixels a list's items and a quote's lines stand in from what holds them.")

(defconst lispm-desktop-store-heading-heights '((1 . 28) (2 . 22) (3 . 18))
  "Height in pixels of a line of a heading of each level of an item's
introduction; one deeper is a third level's.")

(defconst lispm-desktop-store-code-height 15
  "Height in pixels of a line of code of an item's introduction.")

(defun lispm-desktop-store--in-face (text face)
  "Return TEXT with FACE under the faces of its own."
  (let ((text (copy-sequence text)))
    (add-face-text-property 0 (length text) face t text)
    text))

(defun lispm-desktop-store--links-of (line x face)
  "Return where the links of LINE, drawn in FACE from X, stand on its row,
as (FROM TO URL)s."
  (let ((links nil) (at 0))
    (while (setq at (text-property-not-all at (length line) 'lispm-desktop-markdown-link nil line))
      (let ((end (next-single-property-change at 'lispm-desktop-markdown-link line (length line))))
        (push (list (+ x (lispm-desktop-platinum-content-width (substring line 0 at) face))
                    (+ x (lispm-desktop-platinum-content-width (substring line 0 end) face))
                    (get-text-property at 'lispm-desktop-markdown-link line))
              links)
        (setq at end)))
    (nreverse links)))

(defun lispm-desktop-store--text-rows (text x room face &optional height bars)
  "Return the rows of TEXT broken into lines ROOM pixels wide in FACE, from
X, HEIGHT pixels tall, by default `lispm-desktop-store-row-height', BARS
the quotes' bars beside them: a row a line, its links where they stand."
  (mapcar (lambda (line)
            (list :height (or height lispm-desktop-store-row-height) :text line :face face :x x
                  :links (lispm-desktop-store--links-of line x face) :bars bars))
          (lispm-desktop-store--wrap text room face)))

(defun lispm-desktop-store--markdown-rows (blocks props x room face &optional bars)
  "Return the rows of an item's page showing BLOCKS, of the introduction
of the item of PROPS, from X, ROOM pixels wide, in FACE, as
`lispm-desktop-store--page-rows' makes rows: a line a row, a space
between blocks, a list's items and a quote's blocks further in, a
quote's bars beside them, BARS those of the quotes holding BLOCKS."
  (let ((rows nil)
        (indent lispm-desktop-store-markdown-indent))
    (cl-flet ((add (more) (setq rows (append (reverse more) rows)))
              (gap (height) (push (list :height height :bars bars) rows)))
      (dolist (block blocks)
        (when rows
          (gap 6))
        (pcase block
          (`(heading ,level ,text)
           (when (and rows (<= level 2))
             (gap 6))
           (add (mapcar (lambda (row) (append row '(:heading t)))
                        (lispm-desktop-store--text-rows
                         (lispm-desktop-store--in-face text (lispm-desktop-markdown-heading-face level))
                         x room face (alist-get (min level 3) lispm-desktop-store-heading-heights) bars))))
          (`(paragraph ,text)
           (add (lispm-desktop-store--text-rows text x room face nil bars)))
          (`(code ,_ ,text)
           (let ((band (cons x room)))
             (push (list :height 4 :code band :bars bars) rows)
             (dolist (line (split-string text "\n"))
               (push (list :height lispm-desktop-store-code-height :code band :bars bars :x (+ x 8) :face face
                           :text (lispm-desktop-platinum-fit
                                  (propertize line 'face (list 'lispm-desktop-markdown-code face))
                                  (- room 16)))
                     rows))
             (push (list :height 4 :code band :bars bars) rows)))
          (`(blockquote ,inner)
           (add (lispm-desktop-store--markdown-rows inner props (+ x indent) (- room indent) face
                                                    (append bars (list x)))))
          (`(list ,start ,items)
           (let ((number (or start 1)))
             (dolist (item items)
               (let* ((more (lispm-desktop-store--markdown-rows item props (+ x indent) (- room indent)
                                                                face bars))
                      (first (seq-find (lambda (row) (plist-get row :text)) more)))
                 (when first
                   (nconc first (list :marker (if start (format "%d." number) "•") :marker-x x)))
                 (add more)
                 (gap 2)
                 (cl-incf number)))))
          (`(rule)
           (push (list :height 13 :rule t :x x :room room :bars bars) rows))
          (`(image ,alt ,source)
           (let* ((file (and (lispm-desktop-catalog-safe-file-p source)
                             (not (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" source))
                             (plist-get props :catalog)
                             (lispm-desktop-store--file props source)))
                  (size (and file (file-readable-p file) (lispm-desktop-store--picture-size file room))))
             (if size
                 (push (list :height (cdr size) :picture file :size size :x x :bars bars) rows)
               (add (lispm-desktop-store--text-rows
                     (propertize (format "[%s]" alt) 'face 'lispm-desktop-markdown-emphasis)
                     x room face nil bars))))))))
    (nreverse rows)))

(defun lispm-desktop-store--page-rows (id width)
  "Return the rows of the page of the item ID, the window WIDTH pixels
wide: its head, its banner, a rule, what it lacks, its introduction, or
its description where it has none, its information a field each.  They
are shown in pages, see `lispm-desktop-store--item-pages'.  Each row is
a plist: its :height, and :head, :banner, :rule, :picture, :text or
:label and :value; :x, the text's left; its :links, where they stand,
as (FROM TO URL)s; a list item's :marker at :marker-x; a quote's :bars,
the x of each; :code, the band behind a block of code, (X . WIDTH);
:heading, on a heading's line."
  (let* ((props (cdr (assq id (lispm-desktop-store-items))))
         (gx (car (lispm-desktop-store--grid width)))
         (room (- width gx lispm-desktop-store-margin))
         (face 'lispm-desktop-platinum-list)
         (small 'lispm-desktop-platinum-small)
         (fields (lispm-desktop-store--fields id))
         ;; The labels' right edge: past the widest, as a translation may
         ;; be wider than the room English takes.
         (label-end (apply #'max 72 (mapcar (lambda (field)
                                               (+ 4 (lispm-desktop-platinum-content-width
                                                     (propertize (car field) 'face 'bold) small)))
                                             fields)))
         (about (lispm-desktop-store--about props))
         (banner (lispm-desktop-store--banner props))
         ;; A banner runs into the margin on the right: a pixel banner
         ;; is drawn twice its pixels in a window of the default width.
         (size (and banner (lispm-desktop-store--picture-size banner (- width gx 4))))
         (notes (string-trim (concat (lispm-desktop-store--failure id) (lispm-desktop-store--notes props))))
         (rows `((:height ,lispm-desktop-store-head-height :head ,(lispm-desktop-store--head id))
                 ,@(when size
                     (list (list :height (+ (cdr size) 8) :banner banner :size size
                                 :modified (file-attribute-modification-time (file-attributes banner)))))
                 (:height 13 :rule t)
                 ,@(if (not about)
                       (lispm-desktop-store--text-rows (concat (lispm-desktop-store--failure id)
                                                               (lispm-desktop-store--description props))
                                                       gx room face)
                     (append (unless (string-empty-p notes)
                               (append (lispm-desktop-store--text-rows notes gx room face)
                                       (list (list :height 10))))
                             (lispm-desktop-store--markdown-rows about props gx room face)))
                 (:height 14)
                 (:height ,(alist-get 2 lispm-desktop-store-heading-heights) :x ,gx :face ,face
                          :text ,(propertize (lispm-desktop-tr "Information")
                                             'face (lispm-desktop-markdown-heading-face 2))
                          :heading t)
                 (:height 4)
                 ,@(mapcar (pcase-lambda (`(,label ,value))
                             (list :height lispm-desktop-store-row-height :label label :label-end label-end
                                   :value (lispm-desktop-platinum-fit (propertize value 'face small)
                                                                      (- room label-end 8))))
                           fields))))
    rows))

;;;;; An item's page in pages
;; The window never scrolls: an item's page longer than the grid's page
;; shows in pages as tall as the grid's, turned as the grid's are, so
;; that the push buttons under it stay where they are.

(defvar-local lispm-desktop-store--page-number 0
  "Which page of an item's page shows, the first 0.")

(defvar-local lispm-desktop-store--page-count 1
  "How many pages the item's page shown takes, as last drawn.")

(defun lispm-desktop-store--gap-p (row)
  "Return non-nil when ROW of an item's page shows nothing of its own: a
space between blocks, a quote's bars beside it maybe."
  (and row (cl-loop for (key _) on row by #'cddr always (memq key '(:height :bars)))))

(defun lispm-desktop-store--fit-row (row height)
  "Return ROW of an item's page no taller than HEIGHT pixels: a picture
taller is drawn smaller, as wide as keeps its shape."
  (let ((size (plist-get row :size))
        (tall (plist-get row :height)))
    (if (not (and size (> tall height)))
        row
      (let* ((around (- tall (cdr size)))
             (h (max 1 (- height around)))
             (w (max 1 (round (* (car size) (/ (float h) (cdr size))))))
             (row (copy-sequence row)))
        (plist-put (plist-put row :size (cons w h)) :height (+ h around))))))

(defun lispm-desktop-store--paginate (rows height)
  "Return ROWS of an item's page in pages HEIGHT pixels tall, each a list
of rows ending in a blank row as tall as it lacks: as many rows a page
as fit, a picture taller than a page drawn smaller.  No page but the
first starts with a space, and a heading goes over to the next page
with the row it heads."
  (let ((pages nil) (page nil) (used 0))
    (cl-flet ((add (row)
                (push row page)
                (cl-incf used (plist-get row :height)))
              (drop ()
                (cl-decf used (plist-get (car page) :height))
                (pop page))
              (close ()
                (push (nreverse (if (< used height) (cons (list :height (- height used)) page) page)) pages)
                (setq page nil used 0)))
      (dolist (row rows)
        (let* ((row (lispm-desktop-store--fit-row row height))
               (tall (plist-get row :height)))
          (when (and page (> (+ used tall) height))
            (let ((carried nil))
              (while (lispm-desktop-store--gap-p (car page))
                (drop))
              (while (and (cdr page) (plist-get (car page) :heading)
                          (<= (+ (plist-get (car page) :height) tall
                                 (apply #'+ (mapcar (lambda (row) (plist-get row :height)) carried)))
                              height))
                (push (drop) carried))
              (close)
              (mapc #'add carried)))
          (unless (and (null page) pages (lispm-desktop-store--gap-p row))
            (add row))))
      (when (or page (null pages))
        (close))
      (nreverse pages))))

(defun lispm-desktop-store--item-pages (id width height categories)
  "Return the pages of the page of the item ID, the window WIDTH by HEIGHT
pixels, listing CATEGORIES, a count, at its left: its rows, see
`lispm-desktop-store--page-rows', in pages that end where the grid's page
ends, see `lispm-desktop-store--paginate'."
  (lispm-desktop-store--paginate
   (lispm-desktop-store--page-rows id width)
   (* lispm-desktop-store-tile-height
      (lispm-desktop-store--page-lines height categories lispm-desktop-store--rows-height))))

(defun lispm-desktop-store--page-of-pages (pages)
  "Return the page of PAGES, an item's page's, to show: the one
`lispm-desktop-store--page-number' says, the last when there are fewer;
count them in `lispm-desktop-store--page-count'."
  (setq lispm-desktop-store--page-count (max 1 (length pages))
        lispm-desktop-store--page-number (min lispm-desktop-store--page-number
                                              (1- lispm-desktop-store--page-count)))
  (nth lispm-desktop-store--page-number pages))

(defun lispm-desktop-store--text-pages (lines size)
  "Return LINES of an item's page on a text terminal in pages of SIZE
lines; no page but the first starts with a blank line."
  (let ((pages nil))
    (while lines
      (when pages
        (while (and lines (string-empty-p (car lines)))
          (pop lines)))
      (when lines
        (push (seq-take lines size) pages)
        (setq lines (nthcdr size lines))))
    (nreverse pages)))

(defun lispm-desktop-store--draw-head (svg palette x width head)
  "Draw HEAD, as `lispm-desktop-store--head' makes it, on SVG from X, the
page WIDTH pixels wide: the icon twice its size, the name large, the
subtitle and the catalog under it, and the progress bar while the item
is installed or updated."
  (let* ((tx (+ x 80))
         (room (- width tx lispm-desktop-store-margin))
         (text (plist-get palette :text))
         (grey (lispm-desktop-platinum-grey palette #x66))
         (small 'lispm-desktop-platinum-small))
    (lispm-desktop-store--draw-icon svg (lispm-desktop-platinum-icon-file (plist-get head :icon)) x 10 64)
    (lispm-desktop-platinum-svg-content
     svg (lispm-desktop-store--fit (plist-get head :name) room '(:weight bold :height 1.5))
     tx 10 26 text 'lispm-desktop-platinum-list)
    (when-let* ((subtitle (plist-get head :subtitle)))
      (lispm-desktop-platinum-svg-content svg (lispm-desktop-store--fit subtitle room small)
                                          tx 38 14 grey small))
    (lispm-desktop-platinum-svg-content svg (lispm-desktop-store--fit (plist-get head :catalog) room small)
                                        tx 54 14 grey small)
    (when-let* ((progress (plist-get head :progress)))
      (lispm-desktop-platinum-svg-progress-bar svg tx 72 (min 160 room) progress palette))))

(defun lispm-desktop-store--draw-picture (svg file x y size)
  "Draw the picture FILE on SVG at X, Y, SIZE, (WIDTH . HEIGHT), pixels."
  (lispm-desktop-platinum-svg-emacs-image
   svg (list 'image :file file :type (if (string-suffix-p ".svg" file) 'svg (image-supported-file-p file)))
   x y (car size) (cdr size)))

(defun lispm-desktop-store--draw-row (palette width top row categories current)
  "Return an SVG of ROW of an item's page, as
`lispm-desktop-store--page-rows' makes it, TOP pixels down the page,
WIDTH pixels wide, in PALETTE: its part of the list of CATEGORIES,
CURRENT highlighted, and what it shows."
  (let* ((height (plist-get row :height))
         (svg (svg-create width height))
         (x (car (lispm-desktop-store--grid width)))
         (text (plist-get palette :text))
         (small 'lispm-desktop-platinum-small))
    (lispm-desktop-platinum-svg-fill svg 0 0 width height (plist-get palette :chrome))
    (lispm-desktop-store--draw-categories svg palette categories current (- top))
    ;; A quote's bars, and the band behind code, run on from row to row.
    (dolist (bar (plist-get row :bars))
      (lispm-desktop-platinum-svg-fill svg (+ bar 4) 0 3 height (lispm-desktop-platinum-grey palette #xAA)))
    (when-let* ((band (plist-get row :code)))
      (lispm-desktop-platinum-svg-fill svg (car band) 0 (cdr band) height (lispm-desktop-platinum-grey palette #xEE)))
    (when-let* ((marker (plist-get row :marker)))
      (lispm-desktop-platinum-svg-content svg marker (plist-get row :marker-x) 0 height text (plist-get row :face)))
    (cond
     ((plist-get row :head)
      (lispm-desktop-store--draw-head svg palette x width (plist-get row :head)))
     ;; Inset in a well, as Mac OS 9 framed a picture.
     ((plist-get row :banner)
      (pcase-let ((`(,w . ,h) (plist-get row :size)))
        (lispm-desktop-platinum-svg-fill svg (1- x) 3 (+ w 2) (+ h 2) (lispm-desktop-platinum-grey palette #xFF))
        (lispm-desktop-platinum-svg-fill svg (1- x) 3 (1+ w) (1+ h) (lispm-desktop-platinum-grey palette #x88))
        (lispm-desktop-store--draw-picture svg (plist-get row :banner) x 4 (plist-get row :size))))
     ((plist-get row :picture)
      (lispm-desktop-store--draw-picture svg (plist-get row :picture) (plist-get row :x) 0 (plist-get row :size)))
     ;; Etched, as Mac OS 9's separators were.
     ((plist-get row :rule)
      (let* ((from (or (plist-get row :x) x))
             (end (if (plist-get row :room) (+ from (plist-get row :room) -1) (- width lispm-desktop-store-margin 1))))
        (lispm-desktop-platinum-svg-row svg from end 6 (lispm-desktop-platinum-grey palette #x88))
        (lispm-desktop-platinum-svg-row svg from end 7 (lispm-desktop-platinum-grey palette #xFF))))
     ((plist-get row :text)
      (lispm-desktop-platinum-svg-content svg (plist-get row :text) (or (plist-get row :x) x) 0 height text
                                          (plist-get row :face)))
     ((plist-get row :label)
      (let* ((label (propertize (plist-get row :label) 'face 'bold))
             (end (+ x (plist-get row :label-end))))
        (lispm-desktop-platinum-svg-content
         svg label (- end 4 (lispm-desktop-platinum-content-width label small)) 0 height text small)
        (lispm-desktop-platinum-svg-content svg (plist-get row :value) (+ end 4) 0 height text small))))
    svg))

(defun lispm-desktop-store--markdown-lines (blocks room dim)
  "Return the lines of a text terminal showing BLOCKS, Markdown's, ROOM
cells wide, DIM the face of what only frames them: a blank line between
blocks, a list's items after their bullets or numbers, a quote's lines
after a bar, code further in, a rule across, an image by its words."
  (let ((lines nil)
        (face 'lispm-desktop-platinum-list))
    (cl-flet ((add (more) (setq lines (append (reverse more) lines)))
              (prefixed (first then more)
                (seq-map-indexed (lambda (line index) (concat (if (= index 0) first then) line)) more)))
      (dolist (block blocks)
        (when lines
          (push "" lines))
        (pcase block
          (`(heading ,level ,text)
           (add (lispm-desktop-store--wrap
                 (lispm-desktop-store--in-face text (lispm-desktop-markdown-heading-face level)) room face)))
          (`(paragraph ,text)
           (add (lispm-desktop-store--wrap text room face)))
          (`(code ,_ ,text)
           (add (mapcar (lambda (line)
                          (concat "  " (truncate-string-to-width
                                        (propertize line 'face 'lispm-desktop-markdown-code)
                                        (max 1 (- room 2)) nil nil "…")))
                        (split-string text "\n"))))
          (`(blockquote ,inner)
           (let ((bar (propertize "│ " 'face dim)))
             (add (prefixed bar bar (lispm-desktop-store--markdown-lines inner (max 4 (- room 2)) dim)))))
          (`(list ,start ,items)
           (let ((number (or start 1)))
             (dolist (item items)
               (let* ((marker (if start (format "%d. " number) "• "))
                      (more (lispm-desktop-store--markdown-lines
                             item (max 4 (- room (string-width marker))) dim)))
                 (add (prefixed marker (make-string (string-width marker) ?\s)
                                (seq-remove #'string-empty-p more)))
                 (cl-incf number)))))
          (`(rule)
           (push (propertize (make-string room ?─) 'face dim) lines))
          (`(image ,alt ,_)
           (push (propertize (format "[%s]" alt) 'face 'lispm-desktop-markdown-emphasis) lines)))))
    (nreverse lines)))

(defun lispm-desktop-store--hits (line hit)
  "Return LINE, a link's words where they stand saying they are, (link
. URL), and the rest HIT, for the mouse."
  (let ((line (copy-sequence line)) (at 0))
    (while (< at (length line))
      (let ((end (next-single-property-change at 'lispm-desktop-markdown-link line (length line)))
            (url (get-text-property at 'lispm-desktop-markdown-link line)))
        (put-text-property at end 'lispm-desktop-store-hit (if url (cons 'link url) hit) line)
        (setq at end)))
    line))

(defun lispm-desktop-store--insert-text-page (width height)
  "Insert the page of the item selected on a text terminal, as text WIDTH
cells wide and as many lines as a page of the grid takes in HEIGHT
lines, the categories still at its left: its icon, name, subtitle and
catalog, how far it is installed, a rule, what it lacks and its
introduction, or its description where it has none, and its information
a field a line; in pages of that many lines, the one
`lispm-desktop-store--page-number' says."
  (let* ((id lispm-desktop-store--selected)
         (props (cdr (assq id (lispm-desktop-store-items))))
         (head (lispm-desktop-store--head id))
         (categories (lispm-desktop-store--categories))
         (room (max 10 (- width lispm-desktop-store-text-categories-width 5)))
         (dim `(:foreground ,(lispm-desktop-platinum-color :muted)))
         (fields (lispm-desktop-store--fields id))
         (label-width (apply #'max 0 (mapcar (lambda (field) (string-width (car field))) fields)))
         (about (lispm-desktop-store--about props))
         (notes (string-trim (concat (lispm-desktop-store--failure id) (lispm-desktop-store--notes props))))
         (lines `(,(concat (substring-no-properties (lispm-desktop-platinum-icon (plist-get head :icon)))
                           " " (propertize (plist-get head :name) 'face 'bold)
                           (when-let* ((subtitle (plist-get head :subtitle)))
                             (concat "  " (propertize subtitle 'face dim)))
                           "  " (propertize (plist-get head :catalog) 'face dim))
                  ,@(when-let* ((progress (plist-get head :progress)))
                      (list (lispm-desktop-store--text-progress progress)))
                  ,(propertize (make-string room ?─) 'face dim)
                  ,@(if (not about)
                        (lispm-desktop-store--wrap (concat (lispm-desktop-store--failure id)
                                                           (lispm-desktop-store--description props))
                                                   room 'lispm-desktop-platinum-list)
                      (append (unless (string-empty-p notes)
                                (append (lispm-desktop-store--wrap notes room 'lispm-desktop-platinum-list)
                                        (list "")))
                              (lispm-desktop-store--markdown-lines about room dim)))
                  ""
                  ,(propertize (lispm-desktop-tr "Information") 'face (lispm-desktop-markdown-heading-face 2))
                  ,@(mapcar (pcase-lambda (`(,label ,value))
                              (concat (propertize (string-pad label label-width nil t) 'face 'bold)
                                      " " value))
                            fields)))
         (size (lispm-desktop-store--page-lines height (length categories) lispm-desktop-store--rows-height))
         (lines (lispm-desktop-store--page-of-pages (lispm-desktop-store--text-pages lines size))))
    (dotimes (index size)
      (let ((start (point)))
        (insert (lispm-desktop-store--text-category (nth index categories))
                (lispm-desktop-store--hits (truncate-string-to-width (or (nth index lines) "") room nil nil "…")
                                           (cons 'item id)))
        (add-text-properties start (point) (list 'lispm-desktop-store-top index
                                                 'keymap lispm-desktop-store-page-map
                                                 'pointer 'arrow))
        (insert "\n")))))

(defun lispm-desktop-store--insert-page (width height)
  "Insert the page of the item selected, the window WIDTH by HEIGHT pixels,
the categories still at its left: a row each, drawn as one image; on a
text terminal as text, see `lispm-desktop-store--insert-text-page'."
  (if (lispm-desktop-tty-p)
      (lispm-desktop-store--insert-text-page width height)
    (lispm-desktop-store--insert-image-page width height)))

(defun lispm-desktop-store--insert-image-page (width height)
  "Insert the page of the item selected as images, the window WIDTH by
HEIGHT pixels, see `lispm-desktop-store--insert-page': of its pages, the
one `lispm-desktop-store--page-number' says."
  (let ((categories (lispm-desktop-store--categories))
        (current (lispm-desktop-store--current-category))
        (top 0))
    (dolist (row (lispm-desktop-store--page-of-pages
                  (lispm-desktop-store--item-pages lispm-desktop-store--selected width height
                                                   (length categories))))
      (let* ((at top)
             (image (copy-sequence
                     (lispm-desktop-platinum-image
                      (list 'store-row width at categories current (lispm-desktop-store--row-key row))
                      (lambda ()
                        (lispm-desktop-store--draw-row lispm-desktop-platinum-current-palette width at
                                                       row categories current))))))
        ;; A row is as tall as its image, though shorter than a line of
        ;; text, as a picture tiled: its pages end where the grid's would.
        (setf (image-property image :ascent) 100)
        (insert (propertize " " 'display image
                            'lispm-desktop-store-top at
                            'lispm-desktop-store-row row
                            'keymap lispm-desktop-store-page-map
                            'pointer 'arrow)
                (propertize "\n" 'line-height t)))
      (setq top (+ top (plist-get row :height))))))

(defun lispm-desktop-store--row-key (row)
  "Return what tells ROW of an item's page apart from other rows, for
memoizing its image: its words with their faces, which `equal' alone
ignores."
  (mapcar (lambda (value)
            (if (stringp value) (cons (substring-no-properties value) (object-intervals value)) value))
          row))

;;;;; Drawing the window

(defun lispm-desktop-store--placard ()
  "Return what the placard says: the App Store's doing, what it installs
or updates, or its count."
  (or lispm-desktop-store--status-text
      (when-let* ((job (seq-find (lambda (job) (eq (lispm-desktop-store-job-state job) 'running))
                                 lispm-desktop-store--jobs)))
        (let* ((waiting (seq-count (lambda (job) (eq (lispm-desktop-store-job-state job) 'waiting))
                                   lispm-desktop-store--jobs))
               (name (plist-get (cdr (lispm-desktop-store-job-item job)) :name))
               (percent (round (* 100 (lispm-desktop-store-job-fraction job))))
               (doing (if (eq (lispm-desktop-store-job-action job) 'install)
                          (lispm-desktop-tr "Installing “%s”… %d%%" name percent)
                        (lispm-desktop-tr "Updating “%s”… %d%%" name percent))))
          (if (> waiting 0)
              (lispm-desktop-tr-n "%s, %d waiting" "%s, %d waiting" waiting doing waiting)
            doing)))
      (let* ((items (lispm-desktop-store-items))
             (installed (seq-count (lambda (item) (lispm-desktop-store-installed-p (car item))) items)))
        (lispm-desktop-tr-n "%d item, %d installed" "%d items, %d installed" (length items)
                            (length items) installed))))

(defun lispm-desktop-store--keep-selection ()
  "Keep the item selected one the window shows: on the grid, one of its
items, the first otherwise; on an item's page, one listed still, else
the grid shows again."
  (when (and (eq lispm-desktop-store--view 'item)
             (not (assq lispm-desktop-store--selected (lispm-desktop-store-items))))
    (setq lispm-desktop-store--view 'grid))
  (when (eq lispm-desktop-store--view 'grid)
    (let ((shown (lispm-desktop-store--shown)))
      (unless (assq lispm-desktop-store--selected shown)
        (setq lispm-desktop-store--selected (caar shown))))))

(defun lispm-desktop-store--protect (field)
  "Make the window's text read-only, but the search field's, FIELD, (START
. END), and put the field's overlay over it.  Text typed in the field
takes none of the properties of the text around it."
  (pcase-let ((`(,start . ,end) field))
    (add-text-properties (point-min) start '(read-only t))
    (add-text-properties end (point-max) '(read-only t))
    ;; Nothing goes before the window's first line either.
    (put-text-property (point-min) (1+ (point-min)) 'front-sticky '(read-only))
    (put-text-property (1- start) start 'rear-nonsticky t)
    (unless lispm-desktop-store--field
      (setq lispm-desktop-store--field (make-overlay start end nil nil t))
      (overlay-put lispm-desktop-store--field 'field 'lispm-desktop-store-search)
      (overlay-put lispm-desktop-store--field 'keymap lispm-desktop-store-field-map)
      (overlay-put lispm-desktop-store--field 'pointer 'text))
    (move-overlay lispm-desktop-store--field start end)))

(defun lispm-desktop-store--render ()
  "Draw the App Store's window again, what has the keyboard kept: a push
button, or the search field, where in it point and the mark were.  On
the page drawn before, point stays too; a page shown anew shows its
first page.  The window shows its buffer from its start: it never
scrolls.  While an input method composes in the field, it is drawn once
the composing ends."
  (when-let* ((buffer (get-buffer lispm-desktop-store-buffer-name)))
    (with-current-buffer buffer
      (if (lispm-desktop-store--composing-p)
          (run-with-timer lispm-desktop-store-search-delay nil #'lispm-desktop-store--render)
        (let* ((inhibit-read-only t)
               (inhibit-modification-hooks t)
               (window (get-buffer-window buffer t))
               (width (if window (window-body-width window t) 640))
               (height (if window (window-body-height window t) 600))
               (here (if window (window-point window) (point)))
               (button (get-text-property here 'lispm-desktop-platinum-label))
               (bounds (lispm-desktop-store--field-bounds))
               (in-field (and bounds (<= (car bounds) here (cdr bounds))
                              (list (- here (car bounds))
                                    (and (mark t) (<= (car bounds) (mark t) (cdr bounds))
                                         (- (mark t) (car bounds))))))
               field same)
          (setq lispm-desktop-store--width width
                lispm-desktop-store--height height)
          (unless (assq lispm-desktop-store--category (lispm-desktop-store--categories))
            (setq lispm-desktop-store--category 'all))
          (lispm-desktop-store--keep-selection)
          (setq same (equal (lispm-desktop-store--page) lispm-desktop-store--drawn))
          (unless same
            (setq lispm-desktop-store--page-number 0))
          (when lispm-desktop-store--face-cookie
            (face-remap-remove-relative lispm-desktop-store--face-cookie))
          (setq lispm-desktop-store--face-cookie
                (face-remap-add-relative 'default `(:background ,(lispm-desktop-platinum-color :chrome))))
          (when lispm-desktop-store--field
            (delete-overlay lispm-desktop-store--field))
          (erase-buffer)
          (setq field (lispm-desktop-store--insert-top width))
          (if (eq lispm-desktop-store--view 'item)
              (lispm-desktop-store--insert-page width height)
            (lispm-desktop-store--insert-grid width height))
          (lispm-desktop-store--insert-buttons)
          (lispm-desktop-store--protect field)
          (goto-char (point-min))
          (cond
           (in-field
            (pcase-let ((`(,point ,mark) in-field)
                        (`(,from . ,to) field))
              (goto-char (min to (+ from point)))
              (when mark
                (set-marker (mark-marker) (min to (+ from mark))))))
           ((and button same)
            (let ((position (point-min)))
              (while (and (setq position (next-single-property-change position 'lispm-desktop-platinum-label))
                          (not (equal (get-text-property position 'lispm-desktop-platinum-label) button))))
              (goto-char (or position (point-min)))))
           (same
            (goto-char (min here (point-max)))))
          (when window
            (set-window-start window (point-min) t)
            (set-window-point window (point)))
          (setq lispm-desktop-store--drawn (lispm-desktop-store--page))
          (lispm-desktop-store--dress-field)
          (setq lispm-desktop-platinum-focus nil)
          (lispm-desktop-platinum-show-focus)
          (force-mode-line-update)
          (when (and window (eq lispm-desktop-store--view 'grid))
            (lispm-desktop-store--measure window height)))))))

(defun lispm-desktop-store--measure (window height)
  "Measure the rows over and under the grid as WINDOW, HEIGHT pixels tall,
shows them, and draw the window again when the page should hold more
lines or fewer, so that its push buttons show whole."
  (let* ((categories (length (lispm-desktop-store--categories)))
         (lines (lispm-desktop-store--page-lines height categories lispm-desktop-store--rows-height))
         (rows (- (cdr (window-text-pixel-size window nil nil nil nil))
                  (* lines (lispm-desktop-store--tile-height)))))
    (unless (eql rows lispm-desktop-store--rows-height)
      (setq lispm-desktop-store--rows-height rows)
      (unless (= lines (lispm-desktop-store--page-lines height categories rows))
        (lispm-desktop-store--render)))))

(defun lispm-desktop-store--refit (window)
  "Draw the window again once WINDOW's size changed: the grid's pages hold
what fits.  This is a buffer-local `window-size-change-functions' function."
  (unless (and (eql (window-body-width window t) lispm-desktop-store--width)
               (eql (window-body-height window t) lispm-desktop-store--height))
    (lispm-desktop-store--render)))

;;;;; The search field

(defun lispm-desktop-store--field-bounds ()
  "Return where the search field's text is, (START . END), or nil."
  (when-let* ((field lispm-desktop-store--field)
              ((overlay-buffer field)))
    (cons (overlay-start field) (overlay-end field))))

(defun lispm-desktop-store--in-field-p (&optional position)
  "Return non-nil when POSITION, by default point, is in the search field."
  (when-let* ((bounds (lispm-desktop-store--field-bounds)))
    (<= (car bounds) (or position (point)) (cdr bounds))))

(defun lispm-desktop-store--field-focused-p ()
  "Return non-nil while the search field has the keyboard: point in it, in
the window selected."
  (and (eq (window-buffer) (current-buffer))
       (lispm-desktop-store--in-field-p (window-point))))

(defun lispm-desktop-store--dress-field ()
  "Draw the search field as Mac OS 9's edit text field, as wide as the list
of categories under it: framed, ringed while it has the keyboard, and
the cursor in it then; empty, it reads Search, dimmed, else a × at its
end empties it."
  (when-let* ((field lispm-desktop-store--field)
              ((overlay-buffer field)))
    (let* ((focused (lispm-desktop-store--field-focused-p))
           (face (if focused 'lispm-desktop-platinum-field-focused 'lispm-desktop-platinum-field))
           (empty (= (overlay-start field) (overlay-end field)))
           (end (if (lispm-desktop-tty-p)
                    (1- lispm-desktop-store-text-categories-width)
                  (+ lispm-desktop-store-margin lispm-desktop-store-sidebar-width)))
           (pad (propertize " " 'face face 'display `(space :width (,(lispm-desktop-px 4)))))
           (cursor (and focused '(bar . 1))))
      (setq lispm-desktop-store--field-focused focused)
      (overlay-put field 'face face)
      (overlay-put field 'help-echo (lispm-desktop-tr "Search the App Store"))
      ;; Each string over the field starts a frame of its own: while it
      ;; is empty, one string draws it all.
      (overlay-put field 'before-string (unless empty pad))
      ;; The cursor at the text's end stands on what follows it.
      (overlay-put field 'after-string
                   (concat (if empty
                               (concat pad
                                       (propertize (lispm-desktop-tr "Search") 'cursor t
                                                   'face `(:inherit ,face :foreground
                                                                    ,(lispm-desktop-platinum-grey
                                                                      lispm-desktop-platinum-current-palette #x88))))
                             (propertize " " 'face face 'cursor t))
                           (propertize " " 'face face 'display
                                       `(space :align-to (,(- end (lispm-desktop-px (if empty 4 18))))))
                           (if empty
                               ""
                             (propertize "×" 'face face 'keymap lispm-desktop-store-clear-map
                                         'pointer 'hand 'help-echo (lispm-desktop-tr "Clear the search")))
                           (propertize " " 'face face 'display `(space :align-to (,end)))))
      (setq-local cursor-type cursor
                  evil-emacs-state-cursor (list cursor)
                  evil-normal-state-cursor (list cursor)))))

(defun lispm-desktop-store--follow-focus (&rest _)
  "Draw the search field again when it took or lost the keyboard, from the
buffer's `post-command-hook' and `window-selection-change-functions'."
  (when-let* ((buffer (get-buffer lispm-desktop-store-buffer-name)))
    (with-current-buffer buffer
      (unless (eq (lispm-desktop-store--field-focused-p) lispm-desktop-store--field-focused)
        (lispm-desktop-store--dress-field)))))

(defun lispm-desktop-store--composing-p ()
  "Return non-nil while an input method composes text in the current
buffer, the text it shows there not yet typed."
  (seq-some (lambda (symbol)
              (and (boundp symbol)
                   (let ((overlay (symbol-value symbol)))
                     (and (overlayp overlay) (eq (overlay-buffer overlay) (current-buffer))
                          (< (overlay-start overlay) (overlay-end overlay))))))
            '(ns-working-overlay quail-overlay)))

(defun lispm-desktop-store--field-changed (start end _length)
  "Take the search field's text as what is searched once it changed; the
grid shows what it matches as typing pauses.  This is a buffer-local
`after-change-functions' function."
  (when-let* ((bounds (lispm-desktop-store--field-bounds))
              ((<= (car bounds) start end (cdr bounds))))
    (let ((text (buffer-substring-no-properties (car bounds) (cdr bounds))))
      (setq lispm-desktop-store--filter (unless (string-empty-p text) text)))
    (lispm-desktop-store--dress-field)
    (lispm-desktop-store--search-soon)))

(defun lispm-desktop-store--search-soon ()
  "Show what the search matches once typing pauses, from the first page."
  (when (timerp lispm-desktop-store--search-timer)
    (cancel-timer lispm-desktop-store--search-timer))
  (let ((buffer (current-buffer)))
    (setq lispm-desktop-store--search-timer
          (run-with-timer lispm-desktop-store-search-delay nil
                          (lambda ()
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (if (lispm-desktop-store--composing-p)
                                    (lispm-desktop-store--search-soon)
                                  (lispm-desktop-store--searched)))))))))

(defun lispm-desktop-store--searched ()
  "Show the grid of what the search matches, the best selected, or the
category's again after the search was emptied."
  (when (timerp lispm-desktop-store--search-timer)
    (cancel-timer lispm-desktop-store--search-timer))
  (setq lispm-desktop-store--search-timer nil
        lispm-desktop-store--view 'grid
        lispm-desktop-store--selected nil)
  (lispm-desktop-store--render))

(defun lispm-desktop-store--leave-field ()
  "Take the keyboard from the search field for the grid or the item's page."
  (when (lispm-desktop-store--in-field-p)
    (goto-char (point-min))
    (deactivate-mark))
  (when-let* ((window (get-buffer-window (current-buffer) t))
              ((lispm-desktop-store--in-field-p (window-point window))))
    (set-window-point window (point-min))))

(defun lispm-desktop-store--replace-marked ()
  "Delete the text marked in the search field before a command that types
or pastes there, as `delete-selection-mode' does, from the buffer's
`pre-command-hook': the desktop leaves that mode off, and the field
behaves so all the same, its text marked as it takes the keyboard."
  (when (and (not delete-selection-mode) (use-region-p)
             (lispm-desktop-store--in-field-p (region-beginning))
             (lispm-desktop-store--in-field-p (region-end)))
    (let ((delete-selection-mode t))
      (delete-selection-pre-hook))))

(defun lispm-desktop-store-search (&optional text)
  "Search the App Store: the search field takes the keyboard, its text
marked, so that what is typed replaces it, and the grid shows what
matches as typing pauses.  From Lisp, search for TEXT at once; an empty
TEXT shows the category again."
  (interactive)
  (if text
      (lispm-desktop-store--in-window
       (lambda ()
         (setq lispm-desktop-store--filter (unless (string-blank-p text) (string-trim text)))
         (lispm-desktop-store--searched)))
    (let ((buffer (lispm-desktop-store--buffer)))
      (unless (get-buffer-window buffer t)
        (lispm-desktop-store-show))
      (let ((window (get-buffer-window buffer t)))
        (unless (eq (selected-window) window)
          (lispm-desktop-app-window-present 'store)
          (select-window window))
        (with-current-buffer buffer
          (pcase-let ((`(,start . ,end) (lispm-desktop-store--field-bounds)))
            (goto-char end)
            (set-window-point window end)
            (when (< start end)
              (push-mark start t t))
            (lispm-desktop-store--dress-field)))))))

(defun lispm-desktop-store-search-done ()
  "Leave the search field for the grid, which shows what the search
matches at once."
  (interactive)
  (when (timerp lispm-desktop-store--search-timer)
    (lispm-desktop-store--searched))
  (lispm-desktop-store--leave-field)
  (lispm-desktop-store--dress-field))

(defun lispm-desktop-store-clear-search ()
  "Empty the search field: the category searched from shows again."
  (interactive)
  (lispm-desktop-store--in-window
   (lambda ()
     (setq lispm-desktop-store--filter nil)
     (lispm-desktop-store--searched))))

(defun lispm-desktop-store-search-cancel ()
  "Empty the search field, the category showing again; empty already,
leave it for the grid."
  (interactive)
  (if lispm-desktop-store--filter
      (lispm-desktop-store-clear-search)
    (lispm-desktop-store--leave-field)
    (lispm-desktop-store--dress-field)))

(defun lispm-desktop-store-field-forward ()
  "Move point a character on in the search field, never past its end."
  (interactive)
  (when (< (point) (cdr (lispm-desktop-store--field-bounds)))
    (forward-char 1)))

(defun lispm-desktop-store-field-backward ()
  "Move point a character back in the search field, never before its start."
  (interactive)
  (when (> (point) (car (lispm-desktop-store--field-bounds)))
    (backward-char 1)))

(defvar lispm-desktop-store-field-map
  (let ((map (define-keymap :full t
               "RET" #'lispm-desktop-store-search-done
               "C-n" #'lispm-desktop-store-search-done
               "<down>" #'lispm-desktop-store-search-done
               "C-g" #'lispm-desktop-store-search-cancel
               "DEL" #'delete-backward-char
               "C-f" #'lispm-desktop-store-field-forward
               "C-b" #'lispm-desktop-store-field-backward
               "<right>" #'lispm-desktop-store-field-forward
               "<left>" #'lispm-desktop-store-field-backward)))
    ;; What is typed goes in the field, the window's letter keys aside.
    (set-char-table-range (nth 1 map) '(32 . 126) #'self-insert-command)
    (lispm-desktop-bind-clipboard-keys map)
    map)
  "Keymap of the App Store's search field: what is typed goes in it, and
Emacs's keys edit it, the window's own letter keys left aside.  Return,
C-n and Tab leave it for the grid; C-g empties it, then leaves it.")

(defvar-keymap lispm-desktop-store-clear-map
  :doc "Keymap of the × at the end of the search field, which empties it."
  "<down-mouse-1>" #'ignore
  "<mouse-1>" #'lispm-desktop-store-clear-search)

;;;;; Commands of the window

(defun lispm-desktop-store--in-window (function)
  "Call FUNCTION in the App Store's buffer, as from its menus."
  (with-current-buffer (lispm-desktop-store--buffer)
    (funcall function)))

(defun lispm-desktop-store--select (id)
  "Select the item ID and draw the window again: the grid turns to the page
holding it; an item's page shows its page."
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

(defun lispm-desktop-store--item-page-p ()
  "Return non-nil while the App Store's window shows an item's page."
  (eq (buffer-local-value 'lispm-desktop-store--view (lispm-desktop-store--buffer)) 'item))

(defun lispm-desktop-store--turn-item-page (step)
  "Show the page STEP pages on of the item's page shown, STEP negative
back; past either end, nothing."
  (lispm-desktop-store--in-window
   (lambda ()
     (let ((number (+ lispm-desktop-store--page-number step)))
       (when (< -1 number lispm-desktop-store--page-count)
         (setq lispm-desktop-store--page-number number)
         (lispm-desktop-store--render))))))

(defun lispm-desktop-store-forward ()
  "Select the next item of the grid; on an item's page, show the next's."
  (interactive)
  (lispm-desktop-store--move 1))

(defun lispm-desktop-store-backward ()
  "Select the previous item of the grid; on an item's page, show the
previous one's."
  (interactive)
  (lispm-desktop-store--move -1))

(defun lispm-desktop-store-down ()
  "Select the item under the one selected in the grid; on an item's page,
show its next page, as the window never scrolls."
  (interactive)
  (if (lispm-desktop-store--item-page-p)
      (lispm-desktop-store--turn-item-page 1)
    (lispm-desktop-store--move (lispm-desktop-store--in-window #'lispm-desktop-store--columns))))

(defun lispm-desktop-store-up ()
  "Select the item over the one selected in the grid; on an item's page,
show its previous page."
  (interactive)
  (if (lispm-desktop-store--item-page-p)
      (lispm-desktop-store--turn-item-page -1)
    (lispm-desktop-store--move (- (lispm-desktop-store--in-window #'lispm-desktop-store--columns)))))

(defun lispm-desktop-store-next-page (&optional back)
  "Show the grid's next page, its first item selected, or the previous
page when BACK; on an item's page, its next page or its previous one."
  (interactive)
  (if (lispm-desktop-store--item-page-p)
      (lispm-desktop-store--turn-item-page (if back -1 1))
    (lispm-desktop-store--in-window
     (lambda ()
       (let* ((items (lispm-desktop-store--shown))
              (size (lispm-desktop-store--page-size))
              (page (+ (lispm-desktop-store--page-of items size) (if back -1 1))))
         (when (< -1 page (lispm-desktop-store--pages (length items) size))
           (lispm-desktop-store--select (car (nth (* page size) items)))))))))

(defun lispm-desktop-store-previous-page ()
  "Show the grid's previous page, its first item selected; on an item's
page, its previous page."
  (interactive)
  (lispm-desktop-store-next-page t))

(defun lispm-desktop-store-show-category (category)
  "Show the items of CATEGORY, a key of `lispm-desktop-store-categories',
in the grid; the search field empties."
  (interactive (list (lispm-desktop-store--in-window
                      (lambda ()
                        (let ((names (mapcar (pcase-lambda (`(,category ,name . ,_)) (cons name category))
                                             (lispm-desktop-store--categories))))
                          (cdr (assoc (completing-read (lispm-desktop-tr "Category: ") names nil t) names)))))))
  (lispm-desktop-store--in-window
   (lambda ()
     (setq lispm-desktop-store--category category
           lispm-desktop-store--filter nil
           lispm-desktop-store--view 'grid)
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

(defun lispm-desktop-store-show-item (&optional id)
  "Show the page of the item ID, by default the one selected, in place of
the grid: its icon, name and description, its information, and its push
buttons under them.  Go Back, \\`DEL' or \\`M-[', goes back to the grid."
  (interactive)
  (lispm-desktop-store--in-window
   (lambda ()
     (setq lispm-desktop-store--selected
           (or id lispm-desktop-store--selected (user-error (lispm-desktop-tr "Select an item first")))
           lispm-desktop-store--view 'item)
     (lispm-desktop-store--leave-field)
     (lispm-desktop-store--render))))

(defun lispm-desktop-store-back ()
  "Go back from an item's page to the grid, on the page holding the item;
from the grid of a search, back to the category searched from."
  (interactive)
  (lispm-desktop-store--in-window
   (lambda ()
     (cond ((eq lispm-desktop-store--view 'item)
            (setq lispm-desktop-store--view 'grid)
            (lispm-desktop-store--render))
           ((lispm-desktop-store--searching-p)
            (lispm-desktop-store-clear-search))))))

(defun lispm-desktop-store-toggle-item ()
  "Show the selected item's page, as Get Info showed an item's
information; on its page, go back to the grid."
  (interactive)
  (if (lispm-desktop-store--item-page-p)
      (lispm-desktop-store-back)
    (lispm-desktop-store-show-item)))

(define-obsolete-function-alias 'lispm-desktop-store-toggle-information
  #'lispm-desktop-store-toggle-item "0.9.0")

(defun lispm-desktop-store-enter ()
  "Show the selected item's page; on it, press the item's button, as Return
pressed the default button."
  (interactive)
  (if (lispm-desktop-store--item-page-p)
      (lispm-desktop-store-act-selected)
    (lispm-desktop-store-show-item)))

(defun lispm-desktop-store-act (id)
  "Press the button of the item ID: get it, open it, update it, or stop
installing or updating it."
  (interactive (list (lispm-desktop-store--read-item (lispm-desktop-tr "Item: "))))
  (let* ((props (cdr (assq id (lispm-desktop-store-items)))))
    (pcase (cdr (lispm-desktop-store--action id props))
      ('install (lispm-desktop-store-install id))
      ('open (lispm-desktop-store-open id))
      ('update (lispm-desktop-store-update id))
      ('stop (lispm-desktop-store-stop id))
      (_ (lispm-desktop-store--check-program props)
         (when (lispm-desktop-store--busy-p id)
           (user-error (lispm-desktop-tr "%s can no longer be stopped") (plist-get props :name)))
         (user-error (lispm-desktop-tr "%s is installed") (plist-get props :name))))))

(defun lispm-desktop-store-act-selected ()
  "Press the button of the item selected: Get, Open or Update."
  (interactive)
  (if-let* ((id (lispm-desktop-store--selected-id)))
      (lispm-desktop-store-act id)
    (user-error (lispm-desktop-tr "Select an item first"))))

(defun lispm-desktop-store-update-selected ()
  "Update the item selected."
  (interactive)
  (if-let* ((id (lispm-desktop-store--selected-id)))
      (lispm-desktop-store-update id)
    (user-error (lispm-desktop-tr "Select an item first"))))

(defun lispm-desktop-store-uninstall-selected ()
  "Uninstall the item selected."
  (interactive)
  (if-let* ((id (lispm-desktop-store--selected-id)))
      (lispm-desktop-store-uninstall id)
    (user-error (lispm-desktop-tr "Select an item first"))))

(define-obsolete-function-alias 'lispm-desktop-store-remove-selected
  #'lispm-desktop-store-uninstall-selected "0.9.0")

(defun lispm-desktop-store-stop-selected ()
  "Stop installing or updating the item selected, as Command-period
stopped what Mac OS 9 was doing."
  (interactive)
  (if-let* ((id (lispm-desktop-store--selected-id)))
      (lispm-desktop-store-stop id)
    (user-error (lispm-desktop-tr "Select an item first"))))

(defun lispm-desktop-store-show-log-selected ()
  "Show the log of the item selected, which failed to install or update."
  (interactive)
  (if-let* ((id (lispm-desktop-store--selected-id)))
      (lispm-desktop-store-show-log id)
    (user-error (lispm-desktop-tr "Select an item first"))))

(defun lispm-desktop-store--install-commands (props)
  "Return the command installing what the item of PROPS uses and lacks,
or nil: an extension's program, an app's needs, theirs joined."
  (let ((commands (if (lispm-desktop-store--missing-program props)
                      (ensure-list (plist-get props :install))
                    (delq nil (mapcar #'lispm-desktop-needs-install-command
                                      (lispm-desktop-needs-missing (plist-get props :needs) t))))))
    (and commands (string-join commands " && "))))

(defun lispm-desktop-store--open-url (url)
  "Open URL, a link of an item's introduction, as `browse-url' opens it:
a page or an address only, see `lispm-desktop-catalog-link-schemes'."
  (if (and (string-match "\\`\\([a-zA-Z][a-zA-Z0-9+.-]*\\):" url)
           (member (downcase (match-string 1 url)) lispm-desktop-catalog-link-schemes))
      (browse-url url)
    (user-error (lispm-desktop-tr "The App Store opens no link to %s" url))))

(defun lispm-desktop-store--page-links ()
  "Return the links of the introduction of the item whose page shows, as
\(WORDS . URL)s, or nil."
  (lispm-desktop-store--in-window
   (lambda ()
     (and (eq lispm-desktop-store--view 'item) lispm-desktop-store--selected
          (lispm-desktop-markdown-links
           (lispm-desktop-store--about (cdr (assq lispm-desktop-store--selected (lispm-desktop-store-items)))))))))

(defun lispm-desktop-store-open-link ()
  "Open a link of the introduction of the item whose page shows: its only
one, or the one chosen of them."
  (interactive)
  (let ((links (or (lispm-desktop-store--page-links)
                   (user-error (lispm-desktop-tr "This page has no links")))))
    (lispm-desktop-store--open-url
     (if (cdr links)
         (let ((choices (mapcar (pcase-lambda (`(,words . ,url)) (cons (format "%s  %s" words url) url)) links)))
           (cdr (assoc (completing-read (lispm-desktop-tr "Open link: ") choices nil t) choices)))
       (cdar links)))))

(defun lispm-desktop-store-install-missing ()
  "Install what the item selected needs and lacks, once confirmed."
  (interactive)
  (let* ((id (or (lispm-desktop-store--selected-id) (user-error (lispm-desktop-tr "Select an item first"))))
         (props (cdr (assq id (lispm-desktop-store-items))))
         (needs (or (lispm-desktop-store--needs-to-install props)
                    (user-error (lispm-desktop-tr "Nothing %s needs is missing") (plist-get props :name)))))
    (lispm-desktop-needs-ask needs (plist-get props :name) (lambda (_) (lispm-desktop-store--render)))))

(defun lispm-desktop-store-copy-install-command ()
  "Copy the command installing what the item selected uses and PATH lacks."
  (interactive)
  (let* ((id (lispm-desktop-store--selected-id))
         (install (and id (lispm-desktop-store--install-commands (cdr (assq id (lispm-desktop-store-items)))))))
    (unless install
      (user-error (lispm-desktop-tr "Nothing this item uses is missing")))
    (kill-new install)
    (message (lispm-desktop-tr "Copied: %s") install)))

;;;;; The mouse

(defun lispm-desktop-store--grid-event (event)
  "Return what the mouse EVENT on the grid is on, see `lispm-desktop-store--hit'."
  (let* ((position (event-start event))
         (buffer (window-buffer (posn-window position)))
         (xy (posn-object-x-y position))
         (line (and (posn-point position)
                    (get-text-property (posn-point position) 'lispm-desktop-store-line buffer))))
    (cond
     ;; Drawn in text, what stands there says what it is.
     ((lispm-desktop-tty-p (window-frame (posn-window position)))
      (and line (get-text-property (posn-point position) 'lispm-desktop-store-hit buffer)))
     ((and line xy)
      (with-current-buffer buffer
        (lispm-desktop-store--hit lispm-desktop-store--width line (car xy) (cdr xy)
                                  (lispm-desktop-store--categories) (lispm-desktop-store--page-items)))))))

(defun lispm-desktop-store--grid-click (event)
  "Act on the click EVENT on the grid: show the category clicked, the page
of the item clicked, or press the button clicked."
  (interactive "e")
  (pcase (lispm-desktop-store--grid-event event)
    (`(category . ,category) (lispm-desktop-store-show-category category))
    (`(item . ,id) (lispm-desktop-store-show-item id))
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

(defun lispm-desktop-store--page-event (event)
  "Return what the mouse EVENT on an item's page is on: (category .
CATEGORY), a category of the list at its left, (link . URL), a link of
its introduction, (item . ID), the item whose page it is, or nil."
  (let* ((position (event-start event))
         (buffer (window-buffer (posn-window position)))
         (xy (posn-object-x-y position))
         (top (and (posn-point position)
                   (get-text-property (posn-point position) 'lispm-desktop-store-top buffer))))
    (if (lispm-desktop-tty-p (window-frame (posn-window position)))
        (and top (get-text-property (posn-point position) 'lispm-desktop-store-hit buffer))
    (when (and top xy)
      (with-current-buffer buffer
        (or (lispm-desktop-store--page-hit top (car xy) (cdr xy) (lispm-desktop-store--categories))
            (when-let* ((row (get-text-property (posn-point position) 'lispm-desktop-store-row))
                        (link (seq-find (pcase-lambda (`(,from ,to ,_)) (<= from (car xy) to))
                                        (plist-get row :links))))
              (cons 'link (nth 2 link)))
            (and (>= (car xy) (car (lispm-desktop-store--grid lispm-desktop-store--width)))
                 (cons 'item lispm-desktop-store--selected))))))))

(defun lispm-desktop-store--page-hit (top x y categories)
  "Return what stands at X, Y of the row of an item's page TOP pixels down
it, listing CATEGORIES at its left: (category . CATEGORY), or nil."
  (lispm-desktop-store--category-hit x (+ top y) categories))

(defun lispm-desktop-store--page-click (event)
  "Act on the click EVENT on an item's page: show the category clicked, or
open the link."
  (interactive "e")
  (pcase (lispm-desktop-store--page-event event)
    (`(category . ,category) (lispm-desktop-store-show-category category))
    (`(link . ,url) (lispm-desktop-store--open-url url))))

(defun lispm-desktop-store--item-items ()
  "Return the menu of the item selected: the File menu's items acting on
it, its button's action first, then its page and Uninstall…."
  (seq-take-while (lambda (item) (not (eq item 'separator))) (lispm-desktop-store--file-items)))

(defun lispm-desktop-store--context-menu (event)
  "Pop up the menu of the item right-clicked by EVENT, on the grid or its
page, which it selects, as Mac OS 9's contextual menus came up on a
Control click."
  (interactive "e")
  (pcase (or (lispm-desktop-store--page-event event) (lispm-desktop-store--grid-event event))
    (`(,(or 'item 'button) . ,id)
     (lispm-desktop-store--select id)
     (pcase-let ((`(,x . ,y) (lispm-desktop-popup-frame-xy event)))
       (lispm-desktop-popup-menu (lispm-desktop-store--item-items) x y)))))

(defun lispm-desktop-store--keyboard-menu ()
  "Pop up the menu of the item selected from the keyboard, see
`lispm-desktop-context-menu-function': under its button on the grid,
under its icon on its page."
  (lispm-desktop-store--in-window
   (lambda ()
     (pcase-let ((`(,gx ,columns ,tw) (lispm-desktop-store--grid (or lispm-desktop-store--width 640))))
       (cond
        ;; On a text terminal, under the item's line, at its text.
        ((lispm-desktop-tty-p)
         (let ((line (if (eq lispm-desktop-store--view 'item)
                         (text-property-any (point-min) (point-max) 'lispm-desktop-store-top 0)
                       (text-property-any (point-min) (point-max) 'lispm-desktop-store-line
                                          (or (seq-position (mapcar #'car (lispm-desktop-store--page-items))
                                                            lispm-desktop-store--selected)
                                              (user-error (lispm-desktop-tr "Select an item first")))))))
           (pcase-let ((`(,x . ,y) (lispm-desktop-popup-frame-xy line)))
             (lispm-desktop-popup-menu (lispm-desktop-store--item-items) (+ x gx) (1+ y)))))
        ((eq lispm-desktop-store--view 'item)
           (pcase-let ((`(,x . ,y) (lispm-desktop-popup-frame-xy
                                    (text-property-not-all (point-min) (point-max) 'lispm-desktop-store-top nil))))
             (lispm-desktop-popup-menu (lispm-desktop-store--item-items) (+ x gx) (+ y 76))))
        (t
         (let ((index (or (seq-position (mapcar #'car (lispm-desktop-store--page-items))
                                        lispm-desktop-store--selected)
                          (user-error (lispm-desktop-tr "Select an item first")))))
           (pcase-let* ((line (text-property-any (point-min) (point-max) 'lispm-desktop-store-line (/ index columns)))
                        (`(,x . ,y) (lispm-desktop-popup-frame-xy line))
                        (`(,bx ,by ,_) (lispm-desktop-store--button-box
                                        (+ gx (* (% index columns) tw)) tw
                                        (car (lispm-desktop-store--action
                                              lispm-desktop-store--selected
                                              (cdr (assq lispm-desktop-store--selected (lispm-desktop-store-items))))))))
             (lispm-desktop-popup-menu (lispm-desktop-store--item-items)
                                       (+ x bx) (+ y by lispm-desktop-platinum-button-height))))))))))

(defvar-keymap lispm-desktop-store-grid-map
  :doc "Keymap of the App Store's grid, for the mouse.  The second click of
a double click lands on the item's page the first opened: it does nothing."
  "<down-mouse-1>" #'lispm-desktop-store--grid-press
  "<mouse-1>" #'lispm-desktop-store--grid-click
  "<double-down-mouse-1>" #'ignore
  "<double-mouse-1>" #'ignore
  "<triple-down-mouse-1>" #'ignore
  "<triple-mouse-1>" #'ignore
  "<down-mouse-3>" #'lispm-desktop-store--context-menu
  "<mouse-3>" #'ignore)

(defvar-keymap lispm-desktop-store-page-map
  :doc "Keymap of an item's page, for the mouse: its categories show their
items, the right button pops up the item's menu, and the click after
the one that opened the page does nothing."
  "<down-mouse-1>" #'ignore
  "<mouse-1>" #'lispm-desktop-store--page-click
  "<double-down-mouse-1>" #'ignore
  "<double-mouse-1>" #'ignore
  "<triple-down-mouse-1>" #'ignore
  "<triple-mouse-1>" #'ignore
  "<down-mouse-3>" #'lispm-desktop-store--context-menu
  "<mouse-3>" #'ignore)

;;;;; The window's keys and mode

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
  "C-v" #'lispm-desktop-store-next-page
  "M-v" #'lispm-desktop-store-previous-page
  "]" #'lispm-desktop-store-next-page
  "[" #'lispm-desktop-store-previous-page
  "<next>" #'lispm-desktop-store-next-page
  "<prior>" #'lispm-desktop-store-previous-page
  "M-n" #'lispm-desktop-store-next-category
  "M-p" #'lispm-desktop-store-previous-category
  "c" #'lispm-desktop-store-show-category
  "RET" #'lispm-desktop-store-enter
  "SPC" #'lispm-desktop-store-act-selected
  "i" #'lispm-desktop-store-toggle-item
  "DEL" #'lispm-desktop-store-back
  "M-[" (lispm-desktop-graphic-binding #'lispm-desktop-store-back)
  "/" #'lispm-desktop-store-search
  "C-s" #'lispm-desktop-store-search
  "u" #'lispm-desktop-store-update-selected
  "D" #'lispm-desktop-store-uninstall-selected
  "." #'lispm-desktop-store-stop-selected
  "l" #'lispm-desktop-store-show-log-selected
  "w" #'lispm-desktop-store-copy-install-command
  "I" #'lispm-desktop-store-install-missing
  "o" #'lispm-desktop-store-open-link
  "+" #'lispm-desktop-store-install-from-url
  "g" #'lispm-desktop-store-check-for-updates
  "q" #'lispm-desktop-store-close
  "M-w" #'lispm-desktop-store-close)

(define-derived-mode lispm-desktop-store-mode lispm-desktop-app-window-mode "App Store"
  "Major mode of the App Store's window: the search field, the categories,
a grid of icons in pages, or an item's page in its place.
\\{lispm-desktop-store-mode-map}"
  ;; Read-only but for the search field, whose text is typed in.
  (setq-local buffer-read-only nil
              line-spacing 0
              lispm-desktop-context-menu-function #'lispm-desktop-store--keyboard-menu
              lispm-desktop-platinum-info #'lispm-desktop-store--placard
              lispm-desktop-platinum-title "App Store"
              lispm-desktop-platinum-icon (lispm-desktop-platinum-icon "app-store")
              lispm-desktop-platinum-box-functions (list :close #'lispm-desktop-store--close-box
                                                         :collapse #'lispm-desktop-app-window-collapse-box))
  (add-hook 'window-size-change-functions #'lispm-desktop-store--refit nil t)
  (add-hook 'window-selection-change-functions #'lispm-desktop-store--follow-focus nil t)
  (add-hook 'after-change-functions #'lispm-desktop-store--field-changed nil t)
  (add-hook 'pre-command-hook #'lispm-desktop-store--replace-marked nil t)
  (add-hook 'post-command-hook #'lispm-desktop-platinum-show-focus nil t)
  (add-hook 'post-command-hook #'lispm-desktop-store--follow-focus nil t)
  ;; The window holds what it shows whole: the wheel turns nothing, and
  ;; what a command scrolled goes back.
  (dolist (function '(mwheel-scroll-up-function mwheel-scroll-down-function
                      mwheel-scroll-left-function mwheel-scroll-right-function))
    (set (make-local-variable function) #'ignore))
  (add-hook 'post-command-hook #'lispm-desktop-store--hold-still nil t)
  (hl-line-mode -1))

(defun lispm-desktop-store--hold-still ()
  "Show the App Store's buffer from its start in each window showing it,
never scrolled down or sideways, as a command such as \\`C-l' may
have scrolled it; a window rolled up is left as it is.  This is a
buffer-local `post-command-hook' function."
  (dolist (window (get-buffer-window-list nil nil t))
    (unless (lispm-desktop-platinum-rolled-up-height (window-frame window))
      (unless (= (window-start window) (point-min))
        (set-window-start window (point-min)))
      (unless (zerop (window-hscroll window))
        (set-window-hscroll window 0))
      (unless (zerop (window-vscroll window t))
        (set-window-vscroll window 0 t)))))

(defun lispm-desktop-store--scroll-bar (buffer)
  "Say no scroll bar for BUFFER, the App Store's, which never scrolls.
This is one of `lispm-desktop-platinum-scroll-bar-functions'."
  (and (provided-mode-derived-p (buffer-local-value 'major-mode buffer) 'lispm-desktop-store-mode)
       'no))

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
    `((:label ,(or (car action) (lispm-desktop-tr "Get")) :action lispm-desktop-store-act-selected :key "SPC"
              :enabled ,(and (cdr action) t))
      (:label ,(lispm-desktop-tr "Get Info") :action lispm-desktop-store-show-item :key "i"
              :enabled ,(and id (not (lispm-desktop-store--item-page-p))))
      (:label ,(lispm-desktop-tr "Uninstall…") :action lispm-desktop-store-uninstall-selected :key "D"
              :enabled ,(and id (lispm-desktop-store-installed-p id) (not (lispm-desktop-store--busy-p id))))
      (:label ,(lispm-desktop-tr "Stop") :action lispm-desktop-store-stop-selected :key "."
              :enabled ,(and id (lispm-desktop-store--busy-p id)
                             (lispm-desktop-store--stoppable-p (lispm-desktop-store--job id)) t))
      (:label ,(lispm-desktop-tr "Show Log") :action lispm-desktop-store-show-log-selected :key "l"
              :enabled ,(and id (lispm-desktop-store--job id)
                             (buffer-live-p (lispm-desktop-store-job-log (lispm-desktop-store--job id)))))
      (:label ,(lispm-desktop-tr "Install Missing") :action lispm-desktop-store-install-missing :key "I"
              :enabled ,(and props (lispm-desktop-store--needs-to-install props) t))
      (:label ,(lispm-desktop-tr "Copy Install Command") :action lispm-desktop-store-copy-install-command :key "w"
              :enabled ,(and props (lispm-desktop-store--install-commands props) t))
      (:label ,(lispm-desktop-tr "Open Link…") :action lispm-desktop-store-open-link :key "o"
              :enabled ,(and (lispm-desktop-store--page-links) t))
      separator
      (:label ,(lispm-desktop-tr "Install from URL…") :action lispm-desktop-store-install-from-url :key "+")
      (:label ,(lispm-desktop-tr "Check for Updates") :action lispm-desktop-store-check-for-updates :key "g"
              :enabled ,(not lispm-desktop-store--checking))
      separator
      (:label ,(lispm-desktop-tr "Close Window") :action lispm-desktop-store-close :key "q"))))

(defun lispm-desktop-store--view-items ()
  "Return the items of the App Store's View menu: a category each, the one
shown checked; Search, Back, and the pages, the grid's or an item's
page's."
  (with-current-buffer (lispm-desktop-store--buffer)
    (let* ((grid (eq lispm-desktop-store--view 'grid))
           (items (lispm-desktop-store--shown))
           (size (lispm-desktop-store--page-size))
           (page (if grid (lispm-desktop-store--page-of items size) lispm-desktop-store--page-number))
           (pages (if grid (lispm-desktop-store--pages (length items) size) lispm-desktop-store--page-count)))
      `(,@(mapcar (pcase-lambda (`(,category ,name . ,_))
                    (list :label name :action (lambda () (lispm-desktop-store-show-category category))
                          :checked (eq category (lispm-desktop-store--current-category))))
                  (lispm-desktop-store--categories))
        separator
        (:label ,(lispm-desktop-tr "Search…") :action lispm-desktop-store-search :key "/")
        (:label ,(lispm-desktop-tr "Go Back") :action lispm-desktop-store-back :key "DEL"
                :enabled ,(or (not grid) (lispm-desktop-store--searching-p)))
        separator
        (:label ,(lispm-desktop-tr "Previous Page") :action lispm-desktop-store-previous-page :key "M-v"
                :enabled ,(> page 0))
        (:label ,(lispm-desktop-tr "Next Page") :action lispm-desktop-store-next-page :key "C-v"
                :enabled ,(< page (1- pages)))))))

(defun lispm-desktop-store-menus ()
  "Return the App Store's menus, for the menu bar while it is in front."
  `((lispm-desktop-store-file ,(lispm-desktop-tr "File") lispm-desktop-store--file-items)
    (lispm-desktop-store-view ,(lispm-desktop-tr "View") lispm-desktop-store--view-items)))

;;;; Setup

(defun lispm-desktop-setup-store ()
  "Make the App Store an application, the Applications folder a folder of
the Finder, and the applications and extensions installed take part."
  (lispm-desktop-define-app 'store :name "App Store" :icon "app-store"
                            :menus #'lispm-desktop-store-menus
                            :activate #'lispm-desktop-store-show
                            :visible #'lispm-desktop-store-open-p)
  (setf (alist-get 'applications lispm-desktop-finder-folders)
        `(:title ,(lispm-desktop-tr-noop "Applications") :icon "applications"
                 :items lispm-desktop-store--folder-items :trash ,(lispm-desktop-tr-noop "Uninstall…")))
  (lispm-desktop-store--register-installed)
  (add-hook 'lispm-desktop-update-check-hook #'lispm-desktop-store--check-along)
  (add-hook 'lispm-desktop-platinum-scroll-bar-functions #'lispm-desktop-store--scroll-bar)
  (add-hook 'enable-theme-functions (lambda (&rest _) (lispm-desktop-store--render)) 95))

(provide 'lispm-desktop-store)
;;; lispm-desktop-store.el ends here
