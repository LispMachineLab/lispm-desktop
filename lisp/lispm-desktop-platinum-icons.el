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

;; LispM UI draws icons and raster pictures.  The desktop supplies its
;; look's files, keeps the user's text-icon option, and chooses each
;; folder's and buffer's icon for the chrome.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-platinum-draw)
(require 'lispm-ui-icons)

(defun lispm-desktop-platinum-resolve-icon (name)
  "Return the desktop's file for icon NAME, or nil for LispM UI's own.
The look's icons take precedence over icons/os9; `lambda' names LispM's
rainbow mark.  Absolute file names and Emacs's icon are resolved by
`lispm-ui-icon-file' before this hook is called."
  (if (eq name 'lambda)
      (expand-file-name "icons/lispm-lambda.png" lispm-desktop-directory)
    (let ((icons (expand-file-name "icons" lispm-desktop-directory))
          (file (format "%s.svg" name)))
      (seq-some (lambda (directory)
                  (let ((file (expand-file-name file (expand-file-name directory icons))))
                    (and (file-readable-p file) file)))
                (append (lispm-ui-look-get :icons) '("os9"))))))

(add-hook 'lispm-ui-icon-functions #'lispm-desktop-platinum-resolve-icon)

(defcustom lispm-desktop-platinum-text-icons
  '(("folder" . "▪") ("disk" . "▬") ("hard-disk" . "▬") ("computer" . "▣")
    ("document-lisp" . "λ") ("document" . "▫") ("note" . "▫")
    ("robot" . "◉") ("lispm" . "◆") ("lambda" . "λ") ("emacs" . "◆")
    ("applications" . "◆") ("app-store" . "◈") ("control-panels" . "▤")
    ("globe" . "○") ("netscape" . "○") ("trash" . "▿") ("guide" . "?")
    ("mail" . "@") ("caution" . "!") ("stop" . "×")
    ("appearance" . "◐") ("desktop-picture" . "▦") ("keyboard" . "▥") ("language" . "¶")
    ("network" . "⇄") ("sandbox" . "▧") ("software-update" . "↻") ("startup" . "▸")
    ("find" . "⌕") ("terminal" . "λ"))
  "What stands for each icon on a text terminal, one cell wide.
Each entry is (NAME . STRING): NAME is an icon's name as
`lispm-ui-icon' takes it, or the start of one, so that
\"folder\" also stands for \"folder-git\", and the longest start that
matches is the one used.  An icon matched by none is a middle dot.  A
terminal whose font is a Nerd Font can be given its icons here."
  :type '(alist :key-type string :value-type string)
  :group 'lispm-desktop)

;; Preserve the option loaded from custom.el before the toolkit, as for
;; `lispm-desktop-look'.  LispM UI reads the same variable thereafter.
(set-default 'lispm-ui-text-icons lispm-desktop-platinum-text-icons)
(with-no-warnings
  (defvaralias 'lispm-ui-text-icons 'lispm-desktop-platinum-text-icons))

(defun lispm-desktop-platinum-folder-icon (directory)
  "Return the folder icon of DIRECTORY: badged when it is a git repository."
  (lispm-ui-icon (if (and directory
                          (file-exists-p (expand-file-name ".git" directory)))
                     "folder-git"
                   "folder")))

(defvar lispm-directory)

(defvar lispm-desktop-platinum-buffer-icon-functions nil
  "Functions giving a buffer an icon of their own, tried in turn before its
kind's: each is called with the buffer and returns an icon, as
`lispm-ui-icon' makes one, or nil.  An agent's session
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
               (eq (cadr lispm-desktop-platinum--icon-cache) lispm-ui-current-palette))
          (cddr lispm-desktop-platinum--icon-cache)
        (cddr (setq lispm-desktop-platinum--icon-cache
                    (cons key (cons lispm-ui-current-palette
                                    (lispm-desktop-platinum--choose-icon buffer)))))))))

(defun lispm-desktop-platinum--choose-icon (buffer)
  "Return the Finder icon of BUFFER, see `lispm-desktop-platinum-buffer-icon'."
  (with-current-buffer buffer
    (cond ((run-hook-with-args-until-success 'lispm-desktop-platinum-buffer-icon-functions buffer))
          ((derived-mode-p 'eat-mode 'lispm-desktop-term-mode 'term-mode 'shell-mode 'eshell-mode)
           (lispm-ui-icon "computer"))
          ((derived-mode-p 'dired-mode) (lispm-desktop-platinum-folder-icon default-directory))
          ((and buffer-file-name (boundp 'lispm-directory)
                (file-in-directory-p buffer-file-name lispm-directory))
           (lispm-ui-icon "lispm"))
          (buffer-file-name
           (lispm-ui-icon
            (let ((name (lispm-ui-file-icon-name buffer-file-name)))
              (cond ((not (equal name "document")) name)
                    ((derived-mode-p 'lisp-data-mode) "document-lisp")
                    ((derived-mode-p 'prog-mode) "document-code")
                    (t name)))))
          (t (lispm-ui-icon "document-blank")))))

(provide 'lispm-desktop-platinum-icons)
;;; lispm-desktop-platinum-icons.el ends here
