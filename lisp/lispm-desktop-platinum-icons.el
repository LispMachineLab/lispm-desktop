;;; lispm-desktop-platinum-icons.el --- Icons of files, folders and buffers  -*- lexical-binding: t; -*-

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

;; The icons the chrome shows: Mac OS 9's own, a file's by its extension,
;; a folder's, and a buffer's by its mode.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-platinum-draw)

(declare-function lispm-desktop-agent--buffer-agent "lispm-desktop-agent-list" (buffer))
(declare-function lispm-desktop-agent--buffer-p "lispm-desktop-ai" (buffer))
(declare-function lispm-desktop-agent--icon "lispm-desktop-agent-list" (agent))


;;;; Icons
;;
;; Finder's small icons, drawn as sixteen-pixel SVGs in icons/os9: a
;; folder (badged when it holds a git repository), a hard disk, documents
;; (text, code, Lisp, Markdown, Org, picture, blank), a compact Macintosh
;; standing for a terminal, a small robot for the agents and LispM's own
;; application diamond.
;; Unlike the chrome, icons keep their own colours, as Mac OS 9's did
;; whatever the appearance.

(defvar lispm-desktop-platinum--icons (make-hash-table :test #'equal)
  "Icon images by name.")

(defun lispm-desktop-platinum-icon-file (name)
  "Return the file of icon NAME: a file of icons/os9 without its extension,
`lambda' for LispM's rainbow λ, `emacs' for the icon Emacs ships with,
read from this Emacs's own data directory, or an absolute file name, as
of an application's icon installed from the App Store."
  (pcase name
    ((pred (lambda (name) (and (stringp name) (file-name-absolute-p name)))) name)
    ('lambda (expand-file-name "icons/lispm-lambda.png" lispm-desktop-directory))
    ('emacs (let ((file (expand-file-name "images/icons/hicolor/scalable/apps/emacs.svg"
                                          data-directory)))
              (if (file-readable-p file)
                  file
                (lispm-desktop-platinum-icon-file "document"))))
    (_ (expand-file-name (format "icons/os9/%s.svg" name) lispm-desktop-directory))))

(defun lispm-desktop-platinum-icon (name)
  "Return the Finder icon NAME as a string one icon wide.
NAME is as `lispm-desktop-platinum-icon-file' takes it.  Where images cannot be
shown, a blank stands in."
  (let ((file (lispm-desktop-platinum-icon-file name)))
    ;; The icon carries a face of its own, so a button it sits in, such as
    ;; a list row, does not underline it.
    (if (and (display-images-p) (file-readable-p file))
        (propertize " " 'face 'lispm-desktop-platinum-list
                    'display (or (gethash file lispm-desktop-platinum--icons)
                                 (puthash file (create-image file nil nil :height 16
                                                             :ascent 'center)
                                          lispm-desktop-platinum--icons)))
      (propertize " " 'face 'lispm-desktop-platinum-list 'display '(space :width (16))))))

(defconst lispm-desktop-platinum-lisp-extensions '("el" "lisp" "lsp" "cl" "scm" "ss" "rkt" "clj" "cljs"
                                                   "cljc" "edn" "fnl")
  "Extensions of files shown with the Lisp document icon.")

(defconst lispm-desktop-platinum-code-extensions
  '("c" "h" "cc" "cpp" "hpp" "m" "mm" "swift" "rs" "go" "zig" "py" "rb" "js" "jsx" "ts" "tsx"
    "java" "kt" "scala" "hs" "ml" "ex" "exs" "erl" "lua" "php" "sh" "bash" "zsh" "fish" "nix"
    "sol" "sql" "css" "scss" "html" "xml" "json" "yaml" "yml" "toml" "proto" "mk" "cmake")
  "Extensions of files shown with the code document icon.")

(defun lispm-desktop-platinum-file-icon-name (file)
  "Return the name of the document icon of FILE, by its kind."
  (let ((extension (downcase (or (file-name-extension file) ""))))
    (cond ((string-match-p (image-file-name-regexp) file) "document-image")
          ((member extension lispm-desktop-platinum-lisp-extensions) "document-lisp")
          ((member extension '("md" "markdown" "mkd")) "document-markdown")
          ((equal extension "org") "document-org")
          ((member extension lispm-desktop-platinum-code-extensions) "document-code")
          (t "document"))))

(defun lispm-desktop-platinum-folder-icon (directory)
  "Return the folder icon of DIRECTORY: badged when it is a git repository."
  (lispm-desktop-platinum-icon (if (and directory
                                        (file-exists-p (expand-file-name ".git" directory)))
                                   "folder-git"
                                 "folder")))

(defvar lispm-directory)

(defvar-local lispm-desktop-platinum--icon-cache nil
  "This buffer's icon as (KEY PALETTE . ICON): what it was chosen from.")

(defun lispm-desktop-platinum-buffer-icon (buffer)
  "Return the Finder icon of BUFFER.
An agent shows its own mark; a terminal, a compact Macintosh; a
directory, a folder; LispM's own records, LispM's diamond; a file, the
document of its kind; any other buffer, a blank document.  Title bars
ask at every redisplay, so the icon is kept until what it was chosen
from changes."
  (with-current-buffer buffer
    (let ((key (list major-mode buffer-file-name default-directory (buffer-name))))
      (if (and (equal (car lispm-desktop-platinum--icon-cache) key)
               (eq (cadr lispm-desktop-platinum--icon-cache) lispm-desktop-platinum-current-palette))
          (cddr lispm-desktop-platinum--icon-cache)
        (cddr (setq lispm-desktop-platinum--icon-cache
                    (cons key (cons lispm-desktop-platinum-current-palette
                                    (lispm-desktop-platinum--choose-icon buffer)))))))))

(defun lispm-desktop-platinum--choose-icon (buffer)
  "Return the Finder icon of BUFFER, see `lispm-desktop-platinum-buffer-icon'."
  (with-current-buffer buffer
    (cond ((and (fboundp 'lispm-desktop-agent--buffer-p) (lispm-desktop-agent--buffer-p buffer))
           (lispm-desktop-agent--icon (lispm-desktop-agent--buffer-agent buffer)))
          ((derived-mode-p 'eat-mode 'lispm-desktop-term-mode 'term-mode 'shell-mode 'eshell-mode)
           (lispm-desktop-platinum-icon "computer"))
          ((derived-mode-p 'dired-mode) (lispm-desktop-platinum-folder-icon default-directory))
          ((and buffer-file-name (boundp 'lispm-directory)
                (file-in-directory-p buffer-file-name lispm-directory))
           (lispm-desktop-platinum-icon "lispm"))
          (buffer-file-name
           (lispm-desktop-platinum-icon
            (let ((name (lispm-desktop-platinum-file-icon-name buffer-file-name)))
              (cond ((not (equal name "document")) name)
                    ((derived-mode-p 'lisp-data-mode) "document-lisp")
                    ((derived-mode-p 'prog-mode) "document-code")
                    (t name)))))
          (t (lispm-desktop-platinum-icon "document-blank")))))

(provide 'lispm-desktop-platinum-icons)
;;; lispm-desktop-platinum-icons.el ends here
