;;; lispm-ui-rename.el --- The desktop's names, renamed as they move to LispM UI  -*- lexical-binding: t; -*-

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

;; The desktop's controls, panels and forms move to LispM UI, a library
;; of their own, part by part, and their names change from the
;; desktop's to LispM UI's.  The desktop's own code calls them by their
;; new names: each move renames them throughout lisp/ and test/ with the
;; table scripts/lispm-ui-renames.eld, the renames of every move so far,
;;
;;   emacs -Q --batch -l scripts/lispm-ui-rename.el \
;;     -f lispm-desktop-rename-batch lisp/*.el test/*.el
;;
;; as a commit of its own, which a reviewer makes again and finds no
;; difference.  The table is a list of renames, each
;;
;;   ("OLD" . "NEW")            the symbol OLD becomes NEW
;;   (:prefix "OLD" . "NEW")    every symbol starting with OLD starts with
;;                              NEW instead
;;
;; A symbol is renamed wherever its whole name is written: in code, in
;; quoted names of docstrings and comments, in strings.  A line of code
;; indented as Emacs Lisp indents it is indented so again, as a name
;; renamed before it on the line above moves where it lines up.  The
;; files of `lispm-desktop-rename-kept' keep the old names: they alias
;; them to the new.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defconst lispm-desktop-rename-table
  (expand-file-name "lispm-ui-renames.eld" (file-name-directory (or load-file-name buffer-file-name)))
  "The table of renames, beside this file.")

(defconst lispm-desktop-rename-kept '("lispm-desktop-ui-compat.el")
  "Files the renames leave as they are, by name: those defining the old
names as aliases of the new.")

(defconst lispm-desktop-rename--constituents "-[:alnum:]_*+/<>=!?$%&:~^"
  "The characters of a symbol's name, as `skip-chars-forward' takes them.")

(defun lispm-desktop-rename--constituent-p (char)
  "Return non-nil when CHAR may be part of a symbol's name."
  (and char (string-match-p (concat "[" lispm-desktop-rename--constituents "]") (string char))))

(defun lispm-desktop-rename-read (&optional file)
  "Return the renames of FILE, by default `lispm-desktop-rename-table'."
  (with-temp-buffer
    (insert-file-contents (or file lispm-desktop-rename-table))
    (read (current-buffer))))

(defun lispm-desktop-rename-buffer (renames)
  "Apply RENAMES, as the table holds them, to the current buffer.
Return how many names were renamed."
  (let ((count 0))
    (dolist (rename renames)
      (pcase-let* ((prefix (eq (car rename) :prefix))
                   (`(,old . ,new) (if prefix (cdr rename) rename)))
        (save-excursion
          (goto-char (point-min))
          (let ((case-fold-search nil))
            (while (search-forward old nil t)
              (let ((start (match-beginning 0))
                    (end (match-end 0)))
                (when (and (not (lispm-desktop-rename--constituent-p (char-before start)))
                           (or prefix (not (lispm-desktop-rename--constituent-p (char-after end)))))
                  (replace-match new t t)
                  (setq count (1+ count)))))))))
    count))

(defun lispm-desktop-rename--indentation (&optional indent)
  "Return the indentation of the current buffer's lines, a vector; with
INDENT, the indentation Emacs Lisp would give them."
  (let ((text (buffer-string)))
    (with-temp-buffer
      (insert text)
      (when indent
        (delay-mode-hooks (emacs-lisp-mode))
        (let ((indent-tabs-mode nil)
              (inhibit-message t))
          (indent-region (point-min) (point-max))))
      (goto-char (point-min))
      (let ((indents nil))
        (while (not (eobp))
          (push (current-indentation) indents)
          (forward-line 1))
        (vconcat (nreverse indents))))))

(defun lispm-desktop-rename--indented ()
  "Return which lines of the current buffer are indented as Emacs Lisp
indents them, a vector of booleans."
  (cl-map 'vector #'= (lispm-desktop-rename--indentation) (lispm-desktop-rename--indentation t)))

(defun lispm-desktop-rename--indent-again (indented)
  "Indent the lines INDENTED says were indented as Emacs Lisp indents
them, see `lispm-desktop-rename--indented', as it now indents them."
  (let ((now (lispm-desktop-rename--indentation))
        (wanted (lispm-desktop-rename--indentation t))
        (indent-tabs-mode nil))
    (save-excursion
      (goto-char (point-min))
      (dotimes (line (length now))
        (when (and (aref indented line) (/= (aref now line) (aref wanted line)))
          (indent-line-to (aref wanted line)))
        (forward-line 1)))))

(defun lispm-desktop-rename-kept-p (file)
  "Return non-nil when FILE keeps the old names, see `lispm-desktop-rename-kept'."
  (member (file-name-nondirectory file) lispm-desktop-rename-kept))

(defun lispm-desktop-rename-files (files &optional renames)
  "Apply RENAMES, by default the table's, to FILES, saving each changed,
but for those of `lispm-desktop-rename-kept'.
Return how many names were renamed."
  (let ((renames (or renames (lispm-desktop-rename-read)))
        (count 0))
    (dolist (file (seq-remove #'lispm-desktop-rename-kept-p files))
      (with-temp-buffer
        (insert-file-contents file)
        (let* ((indented (lispm-desktop-rename--indented))
               (renamed (lispm-desktop-rename-buffer renames)))
          (when (> renamed 0)
            (lispm-desktop-rename--indent-again indented)
            (write-region nil nil file nil 'silent)
            (setq count (+ count renamed))))))
    count))

(defun lispm-desktop-rename-batch ()
  "Apply the table's renames to the files named on the command line."
  (let ((count (lispm-desktop-rename-files command-line-args-left)))
    (setq command-line-args-left nil)
    (message "%d names renamed" count)))

(provide 'lispm-ui-rename)
;;; lispm-ui-rename.el ends here
