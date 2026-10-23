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
;;             :icon "elfeed" :banner "elfeed" :about "elfeed"
;;             :author "Christopher Wellons" :license "Unlicense"
;;             :command elfeed
;;             :package (elfeed :url "https://github.com/skeeto/elfeed"
;;                              :rev "<full commit SHA>"))
;;     (hermes :kind extension :name "Hermes" :subtitle "An agent"
;;             :description "Hermes in the editor." :icon "hermes"
;;             :banner "hermes" :about "hermes" :author "LispMachineLab"
;;             :license "AGPL-3.0-or-later"
;;             :program "hermes" :install "<command installing hermes>"
;;             :feature lispm-hermes
;;             :package (lispm-hermes :url "<repository>" :rev "<SHA>"))))
;;
;; An item is (ID . PROPERTIES), its properties as
;; `lispm-desktop-catalog-properties' lists them for its kind.  The App
;; Store installs applications and extensions, and the skills that come
;; with them; items of other kinds, as LispM Agent's roles, are not its
;; own, and it passes them over.  Each application and extension gives
;; its whole listing, what its page in the App Store shows
;; (`lispm-desktop-catalog-listing'): lacking any of it, it is not
;; listed.  :icon names icons/NAME.svg, 16 pixels, and
;; icons/NAME-32.svg; :banner banners/NAME.svg, the picture over its page; :about about/NAME.md, its introduction, in the Markdown
;; lispm-desktop-markdown.el reads, about/NAME.LANGUAGE.md in a language
;; of `lispm-desktop-languages' where there is one; :author is who made
;; what it installs, :license its licence, an SPDX expression.
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
;; what it needs on PATH, :install how to install that by hand; its
;; :category, when it names one, files it there, as AI Harness one
;; bringing an agent into the editor, and one naming none is in
;; Extensions.  :needs are what an application or an extension uses
;; beyond Emacs, each (KIND NAME :for WHAT ...), see
;; `lispm-desktop-catalog-need-properties': the App Store installs them
;; with it, so a program Homebrew or Cargo installs is an extension's
;; need, not its :program, which its user installs.  :requires names
;; the items installed before it.  :emacs is the lowest major version of
;; Emacs it runs in, as 32 for what only Emacs 32's canvas images can
;; show: an older Emacs's App Store lists it, but does not install it.
;;
;; A skill is LispM Agent's manual of what applications and extensions
;; let agents do, an org file of instructions and of code defining tools:
;;
;;     (music :kind skill :name "Play music"
;;            :description "Lets agents find, play, pause and skip music."
;;            :file "skills/music.org" :requires (emms))
;;
;; It is never listed nor installed on its own: the App Store installs it
;; with what it :requires, the applications and extensions of the catalog
;; it is the manual of, once each of them is, and uninstalls it with
;; them.  Its :file is skills/ID.org, or skills/ID/ holding SKILL.org and
;; files of its own; the org file says its #+title: and #+description:
;; on a line each, and its code, in blocks marked :lispm load, reads
;; whole: it is read here, never evaluated.  See
;; `lispm-desktop-catalog-skill-properties'.
;;
;; `lispm-desktop-catalog-item-problems' and
;; `lispm-desktop-catalog-problems' say what is wrong, each problem an
;; error or a warning.  An error makes an item unsafe, unreadable or
;; short of its listing, its subtitle, banner or introduction, or the
;; files its catalog lacks: the App Store leaves it out.  A warning
;; leaves it listed, but not as a catalog should list it, as a subtitle
;; too long or an introduction with a heading of its own: a catalog's
;; check fails on either.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'lispm-desktop-markdown)

(defconst lispm-desktop-catalog-kinds '(app extension)
  "The kinds of item the App Store installs.")

(defconst lispm-desktop-catalog-listing
  '(:name :subtitle :description :icon :banner :about :author :license)
  "What every application and extension gives of itself, which its page in
the App Store shows: lacking any of it, the App Store does not list it.
An application gives its :category too; an extension may.")

(defconst lispm-desktop-catalog-properties
  `((app :required (:kind ,@lispm-desktop-catalog-listing :category)
         :optional (:package :command :feature :needs :requires :emacs))
    (extension :required (:kind ,@lispm-desktop-catalog-listing :package :feature)
               :optional (:category :program :install :needs :requires :emacs)))
  "The properties an item of each kind has, as (KIND :required KEYS
:optional KEYS).  An application has :command or :feature besides.")

(defconst lispm-desktop-catalog-skill-properties
  '(:required (:kind :name :description :file :requires) :optional nil)
  "The properties of a skill, LispM Agent's, which the App Store installs
with the applications and extensions it :requires, never on its own: its
:name and a :description, a line each, saying what it lets agents do,
and its :file, skills/ID.org or skills/ID/.")

(defconst lispm-desktop-catalog-categories '(news music social productivity utilities games classic-ai)
  "The categories an application names.")

(defconst lispm-desktop-catalog-extension-categories '(ai-harness)
  "The categories an extension may name: AI Harness, for one bringing an
agent into the editor.  An extension naming none is in Extensions.")

(defconst lispm-desktop-catalog-files
  '((:icon "icons/%s.svg" "icons/%s-32.svg")
    (:banner "banners/%s.svg")
    (:about "about/%s.md"))
  "The files of its catalog an item names, as (KEY FORMAT...): KEY's value,
NAME, formatted with each FORMAT.")

(defconst lispm-desktop-catalog-author-length 48
  "Most characters an author has: it stands on a line of an item's page.")

(defconst lispm-desktop-catalog-subtitle-length 24
  "Most characters a subtitle has: it stands under an icon in the grid.")

(defconst lispm-desktop-catalog-forbidden '(:make :shell-command)
  "Properties no item holds anywhere: they would build with a command.")

(defconst lispm-desktop-catalog-package-properties '(:url :rev :lisp-dir)
  "The properties of a package pinned to a commit of its repository.")

(defconst lispm-desktop-catalog-need-kinds '(program library application)
  "The kinds of what an item needs beyond Emacs: a program on
PATH, a library, or an application, NAME.app.")

(defconst lispm-desktop-catalog-need-properties
  '(:for :brew :cask :head :xcode :cargo :instead :more :commands :variables :file :bundle)
  "The properties of a need, (KIND NAME :for WHAT ...):

:for       what the application or extension needs it for
:brew      the Homebrew formula installing it; :cask for an application
:head      t to install the formula's HEAD, where its release is too old
:xcode     t for a program of Apple's command line tools
:cargo     the crate Cargo builds a program from, for one Homebrew lacks
:instead   programs doing its work while it is missing
:more      what it does that they do not
:commands  its commands needing it
:variables its options holding it, set again once installed
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
       (let ((case-fold-search nil))
         (string-match-p "\\`[a-z0-9][a-z0-9-]*\\'" (symbol-name object)))))

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
         (when (plist-member properties :cargo)
           (unless (let ((value (plist-get properties :cargo)))
                     (and (stringp value) (string-match-p "\\`[A-Za-z0-9][A-Za-z0-9_-]*\\'" value)))
             (wrong "%s: :cargo is the name of a crate Cargo installs" name))
           (unless (eq kind 'program)
             (wrong "%s: :cargo builds a program" name)))
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
  "Return what is wrong with NEEDS, an application's or an extension's
:needs, as `lispm-desktop-catalog-need-problems' says it of each."
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

(defun lispm-desktop-catalog-category-problems (kind category)
  "Return what is wrong with CATEGORY, the :category of an item of KIND, as
a list of (SEVERITY . TEXT), see `lispm-desktop-catalog-item-problems'."
  (let ((categories (if (eq kind 'extension)
                        lispm-desktop-catalog-extension-categories
                      lispm-desktop-catalog-categories)))
    (lispm-desktop-catalog--saying
      (cond ((not (symbolp category)) (wrong ":category is a symbol"))
            ((and category (not (memq category categories)))
             (short ":category is one of %s" (mapconcat #'symbol-name categories ", ")))))))

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
              (short "an application has :command or :feature, not both")))))
   (lispm-desktop-catalog-category-problems 'app (plist-get props :category))
   (lispm-desktop-catalog-needs-problems (plist-get props :needs))))

(defun lispm-desktop-catalog--extension-problems (props)
  "Return what is wrong with PROPS, an extension's properties, in what only
an extension has: its feature, the program it needs, its category; and
in its needs, as an application's."
  (append
   (lispm-desktop-catalog--saying
     (unless (lispm-desktop-catalog--symbol-p (plist-get props :feature))
       (wrong "an extension needs :feature, the library its package provides"))
     (dolist (key '(:program :install))
       (when (plist-member props key)
         (let ((value (plist-get props key)))
           (cond ((not (stringp value)) (wrong "%s is a string" key))
                 ((not (lispm-desktop-catalog--line-p value)) (short "%s is a string of one line" key))))))
     (when (and (plist-member props :install) (not (plist-member props :program)))
       (short ":install says how to install its :program, which it lacks"))
     ;; Its :program missing, it is not installed: the need would never be.
     (when-let* ((program (plist-get props :program))
                 ((proper-list-p (plist-get props :needs)))
                 ((seq-find (lambda (need) (equal (car-safe (cdr-safe need)) program))
                            (plist-get props :needs))))
       (short ":program %s is one of its :needs, which install it: the need is enough" program)))
   (lispm-desktop-catalog-category-problems 'extension (plist-get props :category))
   (lispm-desktop-catalog-needs-problems (plist-get props :needs))))

(defun lispm-desktop-catalog--spdx-p (license)
  "Return non-nil when LICENSE reads as an SPDX expression: identifiers,
as GPL-3.0-or-later, joined by AND, OR and WITH, in brackets or not."
  (let ((words (split-string (replace-regexp-in-string "[()]" " " license) " " t)))
    (and words
         (cl-loop for word in words
                  for index from 0
                  always (if (cl-oddp index)
                             (member word '("AND" "OR" "WITH"))
                           (string-match-p "\\`[A-Za-z0-9][A-Za-z0-9.+-]*\\'" word)))
         (cl-oddp (length words)))))

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
      ;; Its listing is what its page shows: without it, it is not listed.
      ;; Its name, and what an extension cannot do without, are said below.
      (dolist (key (plist-get keys :required))
        (unless (or (plist-member props key) (memq key '(:name :package :feature)))
          (wrong "it needs %s" key)))
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
      (dolist (key '(:description :subtitle :author :license))
        (when-let* ((value (plist-get props key)))
          (cond ((not (stringp value)) (wrong "%s is a string" key))
                ((not (lispm-desktop-catalog--line-p value)) (short "%s is a string of one line" key))
                ((and (eq key :subtitle) (> (length value) lispm-desktop-catalog-subtitle-length))
                 (short ":subtitle is at most %d characters" lispm-desktop-catalog-subtitle-length))
                ((and (eq key :author) (> (length value) lispm-desktop-catalog-author-length))
                 (short ":author is at most %d characters" lispm-desktop-catalog-author-length))
                ((and (eq key :license) (not (lispm-desktop-catalog--spdx-p value)))
                 (short ":license is an SPDX expression, as GPL-3.0-or-later")))))
      (pcase-dolist (`(,key ,format . ,_) lispm-desktop-catalog-files)
        (let ((name (plist-get props key))
              (file (format format "NAME")))
          (cond ((null name))
                ((not (lispm-desktop-catalog-safe-file-p name))
                 (wrong "%s names %s inside the catalog" key file))
                ((let ((case-fold-search nil))
                   (not (string-match-p "\\`[a-z0-9][a-z0-9-]*\\'" name)))
                 (short "%s is NAME of %s, lower-case letters, digits and hyphens" key file)))))
      (let ((requires (plist-get props :requires)))
        (unless (and (proper-list-p requires) (seq-every-p #'lispm-desktop-catalog--symbol-p requires))
          (wrong ":requires is a list of items' IDs")))
      (when-let* ((emacs (plist-get props :emacs)))
        (unless (and (natnump emacs) (<= 30 emacs 99))
          (wrong ":emacs is the lowest major version of Emacs it runs in, as 32"))))))

(defun lispm-desktop-catalog-item-files (props &optional directory)
  "Return the files of its catalog the item of PROPS names, relative to
the catalog, as `lispm-desktop-catalog-files' says: its icons, its banner
and its introduction; DIRECTORY being the catalog's clone, the
introduction's translations there, about/NAME.LANGUAGE.md, and the
images the introductions show from the catalog too."
  (let ((files (cl-loop for (key . formats) in lispm-desktop-catalog-files
                        for name = (plist-get props key)
                        when (lispm-desktop-catalog-safe-file-p name)
                        append (mapcar (lambda (format) (format format name)) formats))))
    (when-let* ((directory)
                (name (plist-get props :about))
                ((lispm-desktop-catalog-safe-file-p name))
                (about (expand-file-name "about" directory))
                ((file-directory-p about)))
      (let ((translations (mapcar (lambda (file) (concat "about/" file))
                                  (directory-files about nil (format "\\`%s\\.[A-Za-z]+\\(?:-[A-Za-z]+\\)*\\.md\\'"
                                                                     (regexp-quote name))))))
        (setq files (append files translations))
        (dolist (file (cons (format "about/%s.md" name) translations))
          (when (file-readable-p (expand-file-name file directory))
            (pcase-dolist (`(,_ ,source) (lispm-desktop-catalog--about-images (expand-file-name file directory)))
              (when (and (lispm-desktop-catalog-safe-file-p source)
                         (not (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" source))
                         (not (member source files)))
                (setq files (append files (list source)))))))))
    files))

(defun lispm-desktop-catalog--about-blocks (file)
  "Return the blocks of the introduction FILE, Markdown."
  (lispm-desktop-markdown-parse (with-temp-buffer (insert-file-contents file) (buffer-string))))

(defun lispm-desktop-catalog--about-images (file)
  "Return the images the introduction FILE shows, as (ALT SOURCE)."
  (lispm-desktop-markdown-images (lispm-desktop-catalog--about-blocks file)))

(defun lispm-desktop-catalog--prose (blocks)
  "Return the words of BLOCKS outside code, as Markdown's text: code, in a
line or a block, may hold what reads as HTML, as Emacs's <up>."
  (mapconcat (lambda (text)
               (let ((at 0) (parts nil))
                 (while (< at (length text))
                   (let ((end (next-single-property-change at 'face text (length text))))
                     (unless (memq 'lispm-desktop-markdown-code (ensure-list (get-text-property at 'face text)))
                       (push (substring-no-properties text at end) parts))
                     (setq at end)))
                 (apply #'concat (nreverse parts))))
             (lispm-desktop-markdown-texts blocks) "\n"))

(defconst lispm-desktop-catalog-link-schemes '("https" "http" "mailto")
  "The schemes of the links an introduction's page opens.")

(defun lispm-desktop-catalog--about-problems (file name)
  "Return what is wrong with the introduction FILE, named NAME, of an item:
it says nothing, has a heading of the first level, as its page shows the
name, or HTML, which shows as written; an image outside its catalog, or
from the network, which its page does not fetch; a link to something
else than a page or an address."
  (let ((blocks (lispm-desktop-catalog--about-blocks file)))
    (lispm-desktop-catalog--saying
      (when (null blocks)
        (wrong "%s says nothing" name))
      (when (memq 1 (lispm-desktop-markdown-headings blocks))
        (short "%s has a heading of the first level; its page shows the name above it" name))
      (when (string-match-p "</?[A-Za-z][A-Za-z0-9-]*\\(?:[ \t\n][^>]*\\)?/?>"
                            (lispm-desktop-catalog--prose blocks))
        (short "%s holds HTML, which its page shows as written" name))
      (pcase-dolist (`(,_ ,source) (lispm-desktop-markdown-images blocks))
        (cond ((string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" source)
               (short "%s shows %s from the network, which its page does not fetch" name source))
              ((not (lispm-desktop-catalog-safe-file-p source))
               (wrong "%s shows %s, which is not inside the catalog" name source))
              ((not (string-match-p "\\.\\(?:svg\\|png\\|jpe?g\\|gif\\)\\'" source))
               (short "%s shows %s, which is no SVG, PNG, JPEG or GIF image" name source))))
      (pcase-dolist (`(,_ . ,url) (lispm-desktop-markdown-links blocks))
        (unless (and (string-match "\\`\\([a-zA-Z][a-zA-Z0-9+.-]*\\):" url)
                     (member (downcase (match-string 1 url)) lispm-desktop-catalog-link-schemes))
          (short "%s links to %s; its page opens only %s links" name url
                 (string-join lispm-desktop-catalog-link-schemes ", ")))))))

(defun lispm-desktop-catalog--file-problems (props directory)
  "Return what is wrong with the files the item of PROPS names in its
catalog's clone DIRECTORY: one missing, which its page would lack, is an
error; so is an introduction's image missing, and what
`lispm-desktop-catalog--about-problems' says of its introductions."
  (let ((files (lispm-desktop-catalog-item-files props directory)))
    (append
     (lispm-desktop-catalog--saying
       (dolist (file files)
         (unless (file-regular-p (expand-file-name file directory))
           (wrong "%s is missing" file))))
     (mapcan (lambda (file)
               (when (and (string-prefix-p "about/" file) (string-suffix-p ".md" file)
                          (file-regular-p (expand-file-name file directory)))
                 (lispm-desktop-catalog--about-problems (expand-file-name file directory) file)))
             files))))

(defun lispm-desktop-catalog-item-problems (item &optional directory)
  "Return what is wrong with ITEM, (ID . PROPERTIES) of a catalog, as a
list of (SEVERITY . TEXT): SEVERITY `error' when the App Store cannot
list it, unsafe, unreadable or short of its listing, `warning' when it
lists it, but not as a catalog should.  DIRECTORY being the catalog's
clone, the files the item names there are looked at too, see
`lispm-desktop-catalog--file-problems'.  A skill's are said by
`lispm-desktop-catalog--skill-problems', its file read in DIRECTORY.  An
item of a kind the App Store does not install, as a role, is not its
own: nothing is said of it."
  (pcase item
    (`(,id . ,props)
     (cond
      ((not (lispm-desktop-catalog--symbol-p id))
       (list (cons 'error "an item is (ID . PROPERTIES), ID a symbol")))
      ((not (lispm-desktop-catalog--plist-p props))
       (list (cons 'error "its properties are no property list")))
      (t
       (let ((kind (plist-get props :kind)))
         (cond
          ((memq kind lispm-desktop-catalog-kinds)
           (append (lispm-desktop-catalog--common-problems id kind props)
                   (lispm-desktop-catalog--package-problems (plist-get props :package) kind)
                   (if (eq kind 'app)
                       (lispm-desktop-catalog--application-problems props)
                     (lispm-desktop-catalog--extension-problems props))
                   (and directory (lispm-desktop-catalog--file-problems props directory))))
          ((eq kind 'skill) (lispm-desktop-catalog--skill-problems id props directory)))))))
    (_ (list (cons 'error "an item is (ID . PROPERTIES)")))))

(defun lispm-desktop-catalog-item-valid-p (item &optional directory)
  "Return non-nil when ITEM is an item the App Store installs and lists:
of one of its kinds, without an error; DIRECTORY being its catalog's
clone, the files it names there too."
  (and (consp item) (lispm-desktop-catalog--plist-p (cdr item))
       (memq (plist-get (cdr item) :kind) lispm-desktop-catalog-kinds)
       (null (lispm-desktop-catalog-errors (lispm-desktop-catalog-item-problems item directory)))))

;;;; Skills

(defun lispm-desktop-catalog-skill-p (item)
  "Return non-nil when ITEM, (ID . PROPERTIES), is a skill."
  (and (consp item) (lispm-desktop-catalog--plist-p (cdr item))
       (eq (plist-get (cdr item) :kind) 'skill)))

(defun lispm-desktop-catalog-skill-file (id props)
  "Return the org file of the skill ID, of PROPS, relative to its catalog:
its :file, skills/ID.org, or SKILL.org in it, skills/ID/; nil when :file
names neither, or ID names no file."
  (let ((file (plist-get props :file)))
    (when (and (lispm-desktop-catalog--id-p id) (lispm-desktop-catalog-safe-file-p file))
      (cond ((equal file (format "skills/%s.org" id)) file)
            ((equal file (format "skills/%s/" id)) (concat file "SKILL.org"))))))

(defun lispm-desktop-catalog--org-keyword (text key)
  "Return the value of the #+KEY: line of the org TEXT, as LispM Agent
reads a skill's title and description, or nil."
  (let ((case-fold-search t))
    (when (string-match (format "^#\\+%s:[ \t]*\\(.*?\\)[ \t]*$" (regexp-quote key)) text)
      (match-string 1 text))))

(defun lispm-desktop-catalog-skill-blocks (text)
  "Return the source blocks of Emacs Lisp of the skill TEXT, as (LINE
CODE LOAD): LINE where it starts, CODE as LispM Agent reads it, org's
escapes undone, nil when the block is not closed; LOAD non-nil for a
block marked :lispm load, whose code is evaluated as the skill loads."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((case-fold-search t) (blocks nil))
      (while (re-search-forward "^[ \t]*#\\+begin_src[ \t]+emacs-lisp\\([^\n]*\\)\n" nil t)
        (let ((args (match-string 1))
              (line (line-number-at-pos (match-beginning 0)))
              (start (point)))
          (push (list line
                      (and (re-search-forward "^[ \t]*#\\+end_src" nil t)
                           (replace-regexp-in-string
                            "^[ \t]*\\(,\\)\\(?:,*[*]\\|#\\+\\)" ""
                            (buffer-substring-no-properties start (match-beginning 0)) nil nil 1))
                      (string-match-p ":lispm[ \t]+load" args))
                blocks)))
      (nreverse blocks))))

(defun lispm-desktop-catalog--unread (code)
  "Return why CODE, Emacs Lisp, does not read whole, or nil when it does.
It is read, never evaluated."
  (with-temp-buffer
    (with-syntax-table emacs-lisp-mode-syntax-table
      (insert code)
      (goto-char (point-min))
      (let ((line 1))
        (condition-case err
            (progn
              (while (progn (forward-comment (buffer-size)) (not (eobp)))
                (setq line (line-number-at-pos))
                (read (current-buffer)))
              nil)
          (end-of-file (format "the form at its line %d is not closed" line))
          (error (format "at its line %d, %s" (line-number-at-pos) (error-message-string err))))))))

(defun lispm-desktop-catalog--skill-text-problems (text name)
  "Return what is wrong with TEXT, the org file NAME of a skill: a title
or a description missing, a block not closed, code that does not read."
  (lispm-desktop-catalog--saying
    (dolist (key '("title" "description"))
      (unless (lispm-desktop-catalog--line-p (lispm-desktop-catalog--org-keyword text key))
        (wrong "%s needs #+%s:, a line" name key)))
    (pcase-dolist (`(,line ,code ,load) (lispm-desktop-catalog-skill-blocks text))
      (cond ((null code) (wrong "%s:%d: the block is not closed with #+end_src" name line))
            ((not load))
            ((when-let* ((why (lispm-desktop-catalog--unread code)))
               (wrong "%s:%d: the code does not read: %s" name line why)))))))

(defun lispm-desktop-catalog--skill-problems (id props &optional directory)
  "Return what is wrong with PROPS, the properties of the skill ID: as a
list of (SEVERITY . TEXT), see `lispm-desktop-catalog-item-problems'.
DIRECTORY being its catalog's clone, its file is read there too."
  (let ((allowed (append (plist-get lispm-desktop-catalog-skill-properties :required)
                         (plist-get lispm-desktop-catalog-skill-properties :optional))))
    (append
     (lispm-desktop-catalog--saying
       ;; It names its file, and the file of its copy.
       (unless (lispm-desktop-catalog--id-p id)
         (wrong "a skill's ID is of lower-case letters, digits and hyphens: it names its file"))
       (when-let* ((key (lispm-desktop-catalog--holds props lispm-desktop-catalog-forbidden)))
         (wrong "it holds %s: the App Store builds nothing with a command" key))
       (let ((seen nil))
         (dolist (key (lispm-desktop-catalog--keys props))
           (cond ((memq key seen) (short "%s is given twice" key))
                 ((not (memq key allowed)) (short "%s is not a property of a skill" key)))
           (push key seen)))
       (dolist (key '(:name :description))
         (let ((value (plist-get props key)))
           (cond ((not (and (stringp value) (not (string-empty-p value)))) (wrong "it needs %s, a string" key))
                 ((not (lispm-desktop-catalog--line-p value)) (short "%s is a string of one line" key)))))
       (let ((requires (plist-get props :requires)))
         (cond ((not (and (proper-list-p requires) (seq-every-p #'lispm-desktop-catalog--symbol-p requires)))
                (wrong ":requires is a list of items' IDs"))
               ((null requires)
                (wrong "it needs :requires, the applications or extensions it comes with"))))
       (when (and (lispm-desktop-catalog--id-p id) (not (lispm-desktop-catalog-skill-file id props)))
         (wrong ":file is \"skills/%s.org\" or \"skills/%s/\"" id id)))
     (when-let* ((directory)
                 (file (lispm-desktop-catalog-skill-file id props)))
       (if (file-regular-p (expand-file-name file directory))
           (lispm-desktop-catalog--skill-text-problems
            (with-temp-buffer
              (insert-file-contents (expand-file-name file directory))
              (buffer-string))
            file)
         (list (cons 'error (format "%s is missing" file))))))))

(defun lispm-desktop-catalog-skill-valid-p (item &optional directory)
  "Return non-nil when ITEM is a skill the App Store installs with what it
requires: without an error; DIRECTORY being its catalog's clone, in its
file there too."
  (and (lispm-desktop-catalog-skill-p item)
       (null (lispm-desktop-catalog-errors (lispm-desktop-catalog-item-problems item directory)))))

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

(defun lispm-desktop-catalog--skill-requires-problems (skills items)
  "Return what is wrong with the :requires of SKILLS, given ITEMS, the
applications and extensions of their catalog: a skill requiring another
item would never be installed."
  (lispm-desktop-catalog--saying
    (pcase-dolist (`(,id . ,props) skills)
      (dolist (other (plist-get props :requires))
        (unless (assq other items)
          (wrong "%s: it requires %s, which is no application or extension of the catalog: it would never be installed"
                 id other))))))

(defun lispm-desktop-catalog-problems (catalog &optional directory)
  "Return what is wrong with CATALOG, the form a catalog.eld holds, in the
items the App Store installs, as a list of (SEVERITY . TEXT), each TEXT
naming the item: what `lispm-desktop-catalog-item-problems' says of
each, DIRECTORY being the catalog's clone, of the files it names there
too; IDs given twice, :requires naming no such item, and a skill's
naming what is no application or extension of the catalog."
  (if (not (and (lispm-desktop-catalog--plist-p catalog) (proper-list-p (plist-get catalog :items))))
      (list (cons 'error "the catalog is (:name NAME :items ITEMS)"))
    (let ((problems nil)
          (items nil)
          (skills nil)
          (ids nil))
      (unless (lispm-desktop-catalog--line-p (plist-get catalog :name))
        (push (cons 'warning "the catalog's :name is a string of one line") problems))
      (dolist (entry (plist-get catalog :items))
        (let ((id (car-safe entry)))
          (if (memq id ids)
              (push (cons 'warning (format "%s: two items have this ID; the first is listed" id)) problems)
            (push id ids)
            (let ((found (lispm-desktop-catalog-item-problems entry directory)))
              (dolist (problem found)
                (push (cons (car problem) (format "%s: %s" (if (consp entry) id entry) (cdr problem))) problems))
              (when (and (consp entry) (lispm-desktop-catalog--plist-p (cdr entry))
                         (null (lispm-desktop-catalog-errors found)))
                (cond ((memq (plist-get (cdr entry) :kind) lispm-desktop-catalog-kinds)
                       (push entry items))
                      ((lispm-desktop-catalog-skill-p entry)
                       (push entry skills))))))))
      (setq items (nreverse items))
      (append (nreverse problems) (lispm-desktop-catalog--requires-problems items)
              (lispm-desktop-catalog--skill-requires-problems (nreverse skills) items)))))

(provide 'lispm-desktop-catalog)
;;; lispm-desktop-catalog.el ends here
