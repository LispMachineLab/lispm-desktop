;;; lispm-desktop-catalog.el --- The format of the App Store's catalogs  -*- lexical-binding: t; -*-

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

;; What a catalog of the App Store holds, said once, for the App Store
;; reading it and for those writing one: LispMachineLab/lispm-store's
;; `make check' loads this file too, and so checks its catalog by the
;; rules the App Store reads it with.  It needs nothing of the desktop's
;; and runs in `emacs -Q --batch'.
;;
;; A catalog is a git repository holding catalog.eld, one property list,
;; read as data and never evaluated, and the icons it names:
;;
;;   (:name "LispM Store"
;;    :items
;;    ((elfeed :kind app :name "Elfeed" :subtitle "Web feeds"
;;             :category news :description "Read web feeds."
;;             :icon "elfeed" :command elfeed
;;             :package (elfeed :url "https://github.com/skeeto/elfeed"
;;                              :rev "<full commit SHA>"))
;;     (hermes :kind extension :name "Hermes" :subtitle "An agent"
;;             :description "Hermes in the editor." :icon "hermes"
;;             :program "hermes" :install "<command installing hermes>"
;;             :feature lispm-hermes
;;             :package (lispm-hermes :url "<repository>" :rev "<SHA>"))))
;;
;; An item is (ID . PROPERTIES), its properties as
;; `lispm-desktop-catalog-properties' lists them for its kind.  The App
;; Store installs applications and extensions; items of other kinds, as
;; LispM Agent's roles and skills, are not its own, and it passes them
;; over.  :icon names icons/NAME.svg, 16 pixels, and icons/NAME-32.svg.
;; :package is absent for a part of Emacs, the name of a package of the
;; package archives, or (NAME :url URL :rev SHA), a git repository
;; pinned to a commit, :lisp-dir naming its Lisp's directory when it is
;; not at the top.  Nothing builds with a command: no :make nor
;; :shell-command anywhere.  An application opens with :command, a
;; command its package autoloads, which the App Store runs in the
;; application's window; or, running windows of its own, as Stickies,
;; with :feature, a library that, loaded, says what the application does
;; with `lispm-desktop-define-app' under the item's ID.  An extension's
;; :feature, loaded, makes it take part in the editor; its :program is
;; what it needs on PATH, :install how to install that.  :needs are what
;; an application uses beyond Emacs, each (KIND NAME :for WHAT ...), see
;; `lispm-desktop-catalog-need-properties'.  :requires names the items
;; installed before it.
;;
;; `lispm-desktop-catalog-item-problems' and
;; `lispm-desktop-catalog-problems' say what is wrong, each problem an
;; error or a warning.  An error makes an item unsafe or unreadable: the
;; App Store leaves it out.  A warning leaves it listed, but short of
;; what a catalog should say, as a subtitle or an icon: a catalog's check
;; fails on either.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'cl-lib)

(defconst lispm-desktop-catalog-kinds '(app extension)
  "The kinds of item the App Store installs.")

(defconst lispm-desktop-catalog-properties
  '((app :required (:kind :name :subtitle :category :description :icon)
         :optional (:package :command :feature :needs :requires))
    (extension :required (:kind :name :subtitle :description :icon :package :feature)
               :optional (:program :install :requires)))
  "The properties an item of each kind has, as (KIND :required KEYS
:optional KEYS).  An application has :command or :feature besides.")

(defconst lispm-desktop-catalog-categories '(news music social productivity utilities games)
  "The categories an application names; every extension is in Extensions.")

(defconst lispm-desktop-catalog-subtitle-length 24
  "Most characters a subtitle has: it stands under an icon in the grid.")

(defconst lispm-desktop-catalog-forbidden '(:make :shell-command)
  "Properties no item holds anywhere: they would build with a command.")

(defconst lispm-desktop-catalog-package-properties '(:url :rev :lisp-dir)
  "The properties of a package pinned to a commit of its repository.")

(defconst lispm-desktop-catalog-need-kinds '(program library application)
  "The kinds of what an application needs beyond Emacs: a program on
PATH, a library, or an application, NAME.app.")

(defconst lispm-desktop-catalog-need-properties
  '(:for :brew :cask :head :xcode :instead :more :commands :variables :file :bundle)
  "The properties of a need, (KIND NAME :for WHAT ...):

:for       what the application needs it for
:brew      the Homebrew formula installing it; :cask for an application
:head      t to install the formula's HEAD, where its release is too old
:xcode     t for a program of Apple's command line tools
:instead   programs doing its work while it is missing
:more      what it does that they do not
:commands  the application's commands needing it
:variables the application's options holding it, set again once installed
:file      a library's file, under its formula's prefix
:bundle    an application's bundle identifier")

;;;; Pieces

(defun lispm-desktop-catalog--plist-p (object)
  "Return non-nil when OBJECT is a property list of keywords."
  (and (proper-list-p object) (cl-evenp (length object))
       (cl-loop for (key) on object by #'cddr always (keywordp key))))

(defun lispm-desktop-catalog--keys (plist)
  "Return the keys of PLIST, in order."
  (cl-loop for (key) on plist by #'cddr collect key))

(defun lispm-desktop-catalog--symbol-p (object)
  "Return non-nil when OBJECT can name a command, a feature or a package."
  (and object (symbolp object) (not (keywordp object)) (not (eq object t))))

(defun lispm-desktop-catalog--id-p (object)
  "Return non-nil when OBJECT names an item, an icon or a package as a
catalog should: lower-case letters, digits and hyphens."
  (and (lispm-desktop-catalog--symbol-p object)
       (string-match-p "\\`[a-z0-9][a-z0-9-]*\\'" (symbol-name object))))

(defun lispm-desktop-catalog--line-p (object)
  "Return non-nil when OBJECT is a string of one line, trimmed, not empty."
  (and (stringp object) (not (string-empty-p object))
       (not (string-search "\n" object)) (equal object (string-trim object))))

(defun lispm-desktop-catalog-safe-file-p (file)
  "Return non-nil when FILE names a file inside a catalog: relative, and
never out of it with `..'."
  (and (stringp file) (not (string-empty-p file))
       (not (file-name-absolute-p file)) (not (string-prefix-p "~" file))
       (not (member ".." (split-string file "/")))))

(defun lispm-desktop-catalog--holds (tree keys)
  "Return the first of KEYS found anywhere in TREE, or nil."
  (cond ((memq tree keys) tree)
        ((consp tree) (or (lispm-desktop-catalog--holds (car tree) keys)
                          (lispm-desktop-catalog--holds (cdr tree) keys)))))

(defmacro lispm-desktop-catalog--saying (&rest body)
  "Run BODY, where (wrong FORMAT ARGS...) says an error and (short FORMAT
ARGS...) a warning, each formatted as `format' does; return what was
said, in order, as (SEVERITY . TEXT)s."
  (declare (indent 0) (debug t))
  (let ((problems (make-symbol "problems")))
    `(let ((,problems nil))
       (cl-flet ((wrong (format &rest args) (push (cons 'error (apply #'format format args)) ,problems))
                 (short (format &rest args) (push (cons 'warning (apply #'format format args)) ,problems)))
         ,@body)
       (nreverse ,problems))))

(defun lispm-desktop-catalog-errors (problems)
  "Return the texts of the errors among PROBLEMS, (SEVERITY . TEXT)s."
  (mapcan (lambda (problem) (and (eq (car problem) 'error) (list (cdr problem)))) problems))

;;;; Needs

(defun lispm-desktop-catalog-need-problems (need)
  "Return what is wrong with NEED, (KIND NAME :for WHAT ...), as a list of
\(SEVERITY . TEXT), SEVERITY `error' or `warning'.  Homebrew's names go
into the shell command an application's information offers to copy, so
a name Homebrew would not give is an error."
  (lispm-desktop-catalog--saying
    (pcase need
      (`(,kind ,name . ,properties)
       (cond
        ((not (memq kind lispm-desktop-catalog-need-kinds))
         (wrong "a need is (KIND NAME ...), KIND one of %s"
                (mapconcat #'symbol-name lispm-desktop-catalog-need-kinds ", ")))
        ((not (and (stringp name) (not (string-empty-p name))))
         (wrong "a need is (KIND NAME ...), NAME a string"))
        ((not (lispm-desktop-catalog--plist-p properties))
         (wrong "%s: not a property list" name))
        (t
         (dolist (key (lispm-desktop-catalog--keys properties))
           (unless (memq key lispm-desktop-catalog-need-properties)
             (short "%s: %s is not a property of a need" name key)))
         (unless (stringp (plist-get properties :for))
           (wrong "%s: needs :for, what it is needed for" name))
         (dolist (key '(:for :more :file :bundle))
           (when (plist-member properties key)
             (let ((value (plist-get properties key)))
               (cond ((not (stringp value)) (wrong "%s: %s is a string" name key))
                     ((not (lispm-desktop-catalog--line-p value))
                      (short "%s: %s is a string of one line" name key))))))
         (dolist (key '(:brew :cask))
           (when (plist-member properties key)
             (unless (let ((value (plist-get properties key)))
                       (and (stringp value) (string-match-p "\\`[a-z0-9][a-z0-9@+._/-]*\\'" value)))
               (wrong "%s: %s is the name of what Homebrew installs" name key))))
         (unless (memq (plist-get properties :head) '(nil t))
           (wrong "%s: :head is t" name))
         (when (and (plist-member properties :xcode) (not (eq (plist-get properties :xcode) t)))
           (short "%s: :xcode is t" name))
         (when (and (plist-get properties :head) (not (plist-get properties :brew)))
           (short "%s: :head is a :brew formula's" name))
         (let ((instead (plist-get properties :instead)))
           (unless (and (proper-list-p instead) (seq-every-p #'stringp instead))
             (wrong "%s: :instead is a list of programs' names" name)))
         (dolist (key '(:commands :variables))
           (let ((symbols (plist-get properties key)))
             (unless (and (proper-list-p symbols) (seq-every-p #'lispm-desktop-catalog--symbol-p symbols))
               (wrong "%s: %s is a list of symbols" name key)))))))
      (_ (wrong "a need is (KIND NAME :for WHAT ...)")))))

(defun lispm-desktop-catalog-needs-problems (needs)
  "Return what is wrong with NEEDS, an application's :needs, as
`lispm-desktop-catalog-need-problems' says it of each."
  (if (proper-list-p needs)
      (mapcan #'lispm-desktop-catalog-need-problems needs)
    (list (cons 'error ":needs is a list of (KIND NAME :for WHAT ...)"))))

;;;; Items

(defun lispm-desktop-catalog--package-problems (package kind)
  "Return what is wrong with PACKAGE, the :package of an item of KIND."
  (lispm-desktop-catalog--saying
    (pcase package
      ('nil (when (eq kind 'extension)
              (wrong "an extension needs :package")))
      ((pred symbolp)
       (cond ((not (lispm-desktop-catalog--symbol-p package))
              (wrong ":package %S is not a package's name" package))
             ((eq kind 'extension)
              (short "an extension's :package is (NAME :url URL :rev SHA), from git"))
             ((not (lispm-desktop-catalog--id-p package))
              (short ":package %s is not a package's name of lower-case letters" package))))
      (`(,name . ,spec)
       (if (not (and (lispm-desktop-catalog--symbol-p name) (lispm-desktop-catalog--plist-p spec)))
           (wrong ":package is a package's name or (NAME :url URL :rev SHA)")
         (dolist (key (lispm-desktop-catalog--keys spec))
           (unless (memq key lispm-desktop-catalog-package-properties)
             (short ":package has %s; only :url, :rev and :lisp-dir are allowed" key)))
         (let ((url (plist-get spec :url))
               (rev (plist-get spec :rev))
               (dir (plist-get spec :lisp-dir)))
           (cond ((not (stringp url)) (wrong ":package needs :url, its repository"))
                 ((not (string-match-p "\\`https://[^ ]+\\'" url))
                  (short ":package's :url is an https URL")))
           (cond ((not (stringp rev)) (wrong ":package needs :rev, the commit it is pinned to"))
                 ((not (string-match-p "\\`[0-9a-f]\\{40\\}\\'" rev))
                  (short ":package's :rev is a full 40-character commit SHA")))
           (when (and dir (not (lispm-desktop-catalog-safe-file-p dir)))
             (wrong ":lisp-dir is a directory inside the package")))))
      (_ (wrong ":package is a package's name or (NAME :url URL :rev SHA)")))))

(defun lispm-desktop-catalog--application-problems (props)
  "Return what is wrong with PROPS, an application's properties, in what
only an application has: how it opens, its category, its needs."
  (append
   (lispm-desktop-catalog--saying
     (let ((command (plist-get props :command))
           (feature (plist-get props :feature)))
       (cond ((not (or command feature))
              (wrong "an application needs :command, or :feature"))
             ((and command (not (lispm-desktop-catalog--symbol-p command)))
              (wrong ":command is a command's name"))
             ((and feature (not (lispm-desktop-catalog--symbol-p feature)))
              (wrong ":feature is a library's name"))
             ((and command feature)
              (short "an application has :command or :feature, not both"))))
     (let ((category (plist-get props :category)))
       (cond ((not (symbolp category)) (wrong ":category is a symbol"))
             ((and category (not (memq category lispm-desktop-catalog-categories)))
              (short ":category is one of %s"
                     (mapconcat #'symbol-name lispm-desktop-catalog-categories ", "))))))
   (lispm-desktop-catalog-needs-problems (plist-get props :needs))))

(defun lispm-desktop-catalog--extension-problems (props)
  "Return what is wrong with PROPS, an extension's properties, in what only
an extension has: its feature, and the program it needs."
  (lispm-desktop-catalog--saying
    (unless (lispm-desktop-catalog--symbol-p (plist-get props :feature))
      (wrong "an extension needs :feature, the library its package provides"))
    (dolist (key '(:program :install))
      (when (plist-member props key)
        (let ((value (plist-get props key)))
          (cond ((not (stringp value)) (wrong "%s is a string" key))
                ((not (lispm-desktop-catalog--line-p value)) (short "%s is a string of one line" key))))))
    (when (and (plist-member props :install) (not (plist-member props :program)))
      (short ":install says how to install its :program, which it lacks"))))

(defun lispm-desktop-catalog--common-problems (id kind props)
  "Return what is wrong with PROPS, the properties of the item ID of KIND,
in what every item has."
  (let* ((keys (cdr (assq kind lispm-desktop-catalog-properties)))
         (allowed (append (plist-get keys :required) (plist-get keys :optional)
                          (and (eq kind 'app) '(:command :feature)))))
    (lispm-desktop-catalog--saying
      (unless (lispm-desktop-catalog--id-p id)
        (short "an item's ID is of lower-case letters, digits and hyphens"))
      (when-let* ((key (lispm-desktop-catalog--holds props lispm-desktop-catalog-forbidden)))
        (wrong "it holds %s: the App Store builds nothing with a command" key))
      ;; Those an extension cannot do without are its errors, below.
      (dolist (key (plist-get keys :required))
        (unless (or (plist-member props key) (memq key '(:package :feature)))
          (short "it needs %s" key)))
      (let ((seen nil))
        (dolist (key (lispm-desktop-catalog--keys props))
          (cond ((memq key seen) (short "%s is given twice" key))
                ((not (memq key allowed)) (short "%s is not a property of an %s" key kind)))
          (push key seen)))
      (let ((name (plist-get props :name)))
        (cond ((not (and (stringp name) (not (string-empty-p name))))
               (wrong "it needs :name, a string"))
              ((not (lispm-desktop-catalog--line-p name))
               (short ":name is a string of one line"))))
      (dolist (key '(:description :subtitle))
        (when-let* ((value (plist-get props key)))
          (cond ((not (stringp value)) (wrong "%s is a string" key))
                ((not (lispm-desktop-catalog--line-p value)) (short "%s is a string of one line" key))
                ((and (eq key :subtitle) (> (length value) lispm-desktop-catalog-subtitle-length))
                 (short ":subtitle is at most %d characters" lispm-desktop-catalog-subtitle-length)))))
      (let ((icon (plist-get props :icon)))
        (cond ((null icon))
              ((not (lispm-desktop-catalog-safe-file-p icon))
               (wrong ":icon names icons/NAME.svg inside the catalog"))
              ((not (string-match-p "\\`[a-z0-9][a-z0-9-]*\\'" icon))
               (short ":icon is NAME of icons/NAME.svg, lower-case letters, digits and hyphens"))))
      (let ((requires (plist-get props :requires)))
        (unless (and (proper-list-p requires) (seq-every-p #'lispm-desktop-catalog--symbol-p requires))
          (wrong ":requires is a list of items' IDs"))))))

(defun lispm-desktop-catalog-item-problems (item)
  "Return what is wrong with ITEM, (ID . PROPERTIES) of a catalog, as a
list of (SEVERITY . TEXT): SEVERITY `error' when the App Store cannot
list it safely, `warning' when it lists it short of what a catalog
should say.  An item of a kind the App Store does not install, as a
role or a skill, is not its own: nothing is said of it."
  (pcase item
    (`(,id . ,props)
     (cond
      ((not (lispm-desktop-catalog--symbol-p id))
       (list (cons 'error "an item is (ID . PROPERTIES), ID a symbol")))
      ((not (lispm-desktop-catalog--plist-p props))
       (list (cons 'error "its properties are no property list")))
      (t
       (let ((kind (plist-get props :kind)))
         (when (memq kind lispm-desktop-catalog-kinds)
           (append (lispm-desktop-catalog--common-problems id kind props)
                   (lispm-desktop-catalog--package-problems (plist-get props :package) kind)
                   (if (eq kind 'app)
                       (lispm-desktop-catalog--application-problems props)
                     (lispm-desktop-catalog--extension-problems props))))))))
    (_ (list (cons 'error "an item is (ID . PROPERTIES)")))))

(defun lispm-desktop-catalog-item-valid-p (item)
  "Return non-nil when ITEM is an item the App Store installs and lists:
of one of its kinds, without an error."
  (and (consp item) (lispm-desktop-catalog--plist-p (cdr item))
       (memq (plist-get (cdr item) :kind) lispm-desktop-catalog-kinds)
       (null (lispm-desktop-catalog-errors (lispm-desktop-catalog-item-problems item)))))

;;;; Catalogs

(defun lispm-desktop-catalog-read (directory)
  "Return what DIRECTORY's catalog.eld holds, read as data, never
evaluated; signal an error when it is no catalog."
  (let ((data (with-temp-buffer
                (insert-file-contents (expand-file-name "catalog.eld" directory))
                (prog1 (read (current-buffer))
                  (forward-comment (buffer-size))
                  (unless (eobp)
                    (error "catalog.eld holds more than one form"))))))
    (unless (and (lispm-desktop-catalog--plist-p data) (proper-list-p (plist-get data :items)))
      (error "catalog.eld is no (:name NAME :items ITEMS)"))
    data))

(defun lispm-desktop-catalog--requires-problems (items)
  "Return what is wrong with the :requires of ITEMS, the (ID . PROPERTIES)s
a catalog lists of the App Store's kinds: each names another of them,
and they leave an order to install them in."
  (let ((done nil))
    (append
     (lispm-desktop-catalog--saying
       (pcase-dolist (`(,id . ,props) items)
         (dolist (other (plist-get props :requires))
           (cond ((eq other id) (short "%s: it requires itself" id))
                 ((not (assq other items))
                  (short "%s: it requires %s, which is no application or extension of the catalog"
                         id other))))))
     (lispm-desktop-catalog--saying
       (cl-labels ((visit (id path)
                     (cond ((memq id path)
                            (short "%s: its :requires go round in a circle" id))
                           ((not (memq id done))
                            (dolist (other (plist-get (cdr (assq id items)) :requires))
                              (when (and (not (eq other id)) (assq other items))
                                (visit other (cons id path))))
                            (push id done)))))
         (dolist (item items)
           (visit (car item) nil)))))))

(defun lispm-desktop-catalog-problems (catalog &optional directory)
  "Return what is wrong with CATALOG, the form a catalog.eld holds, in the
items the App Store installs, as a list of (SEVERITY . TEXT), each TEXT
naming the item: what `lispm-desktop-catalog-item-problems' says of
each, IDs given twice, :requires naming no such item, and, DIRECTORY
being the catalog's clone, the icons it names and lacks."
  (if (not (and (lispm-desktop-catalog--plist-p catalog) (proper-list-p (plist-get catalog :items))))
      (list (cons 'error "the catalog is (:name NAME :items ITEMS)"))
    (let ((problems nil)
          (items nil)
          (ids nil))
      (unless (lispm-desktop-catalog--line-p (plist-get catalog :name))
        (push (cons 'warning "the catalog's :name is a string of one line") problems))
      (dolist (entry (plist-get catalog :items))
        (let ((id (car-safe entry)))
          (if (memq id ids)
              (push (cons 'warning (format "%s: two items have this ID; the first is listed" id)) problems)
            (push id ids)
            (dolist (problem (lispm-desktop-catalog-item-problems entry))
              (push (cons (car problem) (format "%s: %s" (if (consp entry) id entry) (cdr problem))) problems))
            (when (lispm-desktop-catalog-item-valid-p entry)
              (push entry items)
              (when-let* ((icon (plist-get (cdr entry) :icon))
                          (directory)
                          ((lispm-desktop-catalog-safe-file-p icon)))
                (dolist (file (list (format "icons/%s.svg" icon) (format "icons/%s-32.svg" icon)))
                  (unless (file-regular-p (expand-file-name file directory))
                    (push (cons 'warning (format "%s: %s is missing" id file)) problems))))))))
      (append (nreverse problems) (lispm-desktop-catalog--requires-problems (nreverse items))))))

(provide 'lispm-desktop-catalog)
;;; lispm-desktop-catalog.el ends here
