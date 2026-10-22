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
(require 'lispm-desktop-platinum-raster)



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
or of the look's own icons, which take their place where the look draws
one (see `lispm-desktop-look-define'); `lambda' for LispM's rainbow λ,
`emacs' for the icon Emacs ships with, read from this Emacs's own data
directory, or an absolute file name, as of an application's icon
installed from the App Store."
  (pcase name
    ((pred (lambda (name) (and (stringp name) (file-name-absolute-p name)))) name)
    ('lambda (expand-file-name "icons/lispm-lambda.png" lispm-desktop-directory))
    ('emacs (let ((file (expand-file-name "images/icons/hicolor/scalable/apps/emacs.svg"
                                          data-directory)))
              (if (file-readable-p file)
                  file
                (lispm-desktop-platinum-icon-file "document"))))
    (_ (let ((icons (expand-file-name "icons" lispm-desktop-directory))
             (file (format "%s.svg" name)))
         (or (seq-some (lambda (directory)
                         (let ((file (expand-file-name file (expand-file-name directory icons))))
                           (and (file-readable-p file) file)))
                       (lispm-desktop-look-get :icons))
             (expand-file-name file (expand-file-name "os9" icons)))))))

(defcustom lispm-desktop-platinum-text-icons
  '(("folder" . "▪") ("disk" . "▬") ("hard-disk" . "▬") ("computer" . "▣")
    ("document-lisp" . "λ") ("document" . "▫") ("note" . "▫")
    ("robot" . "◉") ("lispm" . "◆") ("lambda" . "λ") ("emacs" . "◆")
    ("applications" . "◆") ("app-store" . "◈") ("control-panels" . "▤")
    ("globe" . "○") ("netscape" . "○") ("trash" . "▿") ("guide" . "?")
    ("mail" . "@") ("caution" . "!") ("stop" . "×")
    ("appearance" . "◐") ("desktop-picture" . "▦") ("keyboard" . "▥") ("language" . "¶")
    ("network" . "⇄") ("sandbox" . "▧") ("software-update" . "↻") ("startup" . "▸")
    ("find" . "⌕"))
  "What stands for each icon on a text terminal, one cell wide.
Each entry is (NAME . STRING): NAME is an icon's name as
`lispm-desktop-platinum-icon' takes it, or the start of one, so that
\"folder\" also stands for \"folder-git\", and the longest start that
matches is the one used.  An icon matched by none is a middle dot.  A
terminal whose font is a Nerd Font can be given its icons here."
  :type '(alist :key-type string :value-type string)
  :group 'lispm-desktop)

(defvar lispm-desktop-platinum--icon-colors (make-hash-table :test #'equal)
  "The colour each icon is drawn in most, by name, see
`lispm-desktop-platinum--text-icon'.")

(defun lispm-desktop-platinum--text-icon (name)
  "Return what stands for icon NAME on a text terminal, see
`lispm-desktop-platinum-text-icons', in the colour the icon is drawn in
most, see `lispm-desktop-platinum-raster-color'."
  (let* ((color (with-memoization (gethash name lispm-desktop-platinum--icon-colors)
                  (when-let* ((pixels (lispm-desktop-platinum-raster
                                       (lispm-desktop-platinum-icon-file name) 16)))
                    (lispm-desktop-platinum-raster-color pixels))))
         (name (if (symbolp name) (symbol-name name) (file-name-base name)))
         (match (car (sort (seq-filter (lambda (entry) (string-prefix-p (car entry) name))
                                       lispm-desktop-platinum-text-icons)
                           (lambda (a b) (> (length (car a)) (length (car b))))))))
    (propertize (truncate-string-to-width (or (cdr match) "·") 1 nil ?\s)
                'face (if color
                          `(:inherit lispm-desktop-platinum-list :foreground ,color)
                        'lispm-desktop-platinum-list))))

(defun lispm-desktop-platinum--icon-source (name size)
  "Return the file to draw icon NAME from at SIZE pixels, 16 or less: its
own sixteen-pixel drawing where it has one beside the 32-pixel NAME-32,
so that it comes out pixel for pixel, else NAME's.  An icon drawn at a
size that is no whole part of its own would smear."
  (let ((file (lispm-desktop-platinum-icon-file name)))
    (or (when-let* (((<= size 16))
                    ((string-match "-32\\.svg\\'" file))
                    (small (replace-match ".svg" t t file))
                    ((file-readable-p small)))
          small)
        (when-let* (((<= size 16))
                    ((stringp name))
                    ((string-suffix-p "-32" name))
                    (small (lispm-desktop-platinum-icon-file (string-remove-suffix "-32" name)))
                    ((file-readable-p small)))
          small)
        file)))

(defvar lispm-desktop-platinum--icon-pictures (make-hash-table :test #'equal)
  "Icons drawn in text, by file, size, selection and background, see
`lispm-desktop-platinum-icon-picture'.")

(defun lispm-desktop-platinum-icon-picture (name size &optional selected background)
  "Return icon NAME drawn on a text terminal SIZE pixels a side, best 16, as
lines of text, see `lispm-desktop-platinum-text-picture', darkened when
SELECTED, over BACKGROUND, the colour where it is drawn; or nil where it
cannot be drawn, as an icon that is no SVG.  NAME is as
`lispm-desktop-platinum-icon-file' takes it; it is drawn from its own
drawing of that size or twice it, see
`lispm-desktop-platinum--icon-source'.  Each is drawn once."
  (let ((file (lispm-desktop-platinum--icon-source name size)))
    (with-memoization (gethash (list file size (and selected t) background)
                               lispm-desktop-platinum--icon-pictures)
      (when-let* ((pixels (lispm-desktop-platinum-raster file size)))
        (lispm-desktop-platinum-text-picture pixels selected background)))))

(defun lispm-desktop-platinum-icon (name)
  "Return the Finder icon NAME as a string one icon wide.
NAME is as `lispm-desktop-platinum-icon-file' takes it.  On a text
terminal a character stands in, see `lispm-desktop-platinum-text-icons';
elsewhere where images cannot be shown, a blank."
  (let ((file (lispm-desktop-platinum-icon-file name)))
    ;; The icon carries a face of its own, so a button it sits in, such as
    ;; a list row, does not underline it.
    (cond
     ((lispm-desktop-tty-p) (lispm-desktop-platinum--text-icon name))
     ((and (display-images-p) (file-readable-p file))
        (propertize " " 'face 'lispm-desktop-platinum-list
                    'display (or (gethash file lispm-desktop-platinum--icons)
                                 (puthash file (create-image file nil nil :height 16
                                                             :ascent 'center)
                                          lispm-desktop-platinum--icons))))
     (t (propertize " " 'face 'lispm-desktop-platinum-list 'display '(space :width (16)))))))

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

(defvar lispm-desktop-platinum-buffer-icon-functions nil
  "Functions giving a buffer an icon of their own, tried in turn before its
kind's: each is called with the buffer and returns an icon, as
`lispm-desktop-platinum-icon' makes one, or nil.  An agent's session
shows its agent's mark this way.")

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
    (cond ((run-hook-with-args-until-success 'lispm-desktop-platinum-buffer-icon-functions buffer))
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
