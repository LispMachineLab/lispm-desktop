;;; lispm-desktop-shells.el --- The shells sidebar  -*- lexical-binding: t; -*-

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

;; The shells view of the left sidebar: the user's running shells, each a
;; terminal of its own, and a button starting another.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-ai)
(require 'lispm-desktop-sidebar)

(declare-function lispm-desktop-terminal-make "lispm-desktop-term" (name program &rest args))
(declare-function lispm-desktop-term-ready-p "lispm-desktop-term" ())
(declare-function eat "eat" (&optional program arg))
(defvar explicit-shell-file-name)

(defconst lispm-desktop-shell-list-buffer-name "*Shells*"
  "Name of the shell sidebar buffer.")

(defun lispm-desktop-shell--buffer-p (buffer)
  "Return non-nil when BUFFER is a live shell terminal, not an agent's."
  (and (lispm-desktop-terminal-p buffer)
       (not (lispm-desktop-agent-buffer-p buffer))
       (lispm-desktop-agent-running-p buffer)))

(defun lispm-desktop-shell-new (&optional directory)
  "Start a new shell in DIRECTORY, by default the main window's project root.
The shell is the user's, run as an independent process in a terminal, see
`lispm-desktop-terminal-make'."
  (interactive)
  (let* ((directory (or directory (lispm-desktop-agent-default-directory)))
         (default-directory (file-name-as-directory directory))
         (process-environment (lispm-desktop-agent-independent-environment process-environment))
         (program (or explicit-shell-file-name (getenv "ESHELL") shell-file-name)))
    (lispm-desktop-sidebar-show-in-main
     (lispm-desktop-terminal-make (lispm-desktop-agent-fresh-name 'shell directory) program "-l")))
  (lispm-desktop-shell-list-refresh))

(defun lispm-desktop-eat-new ()
  "Start a new shell: in lispm-desktop-term once built, see
`lispm-desktop-shell-new', else in Eat."
  (interactive)
  (if (lispm-desktop-term-ready-p)
      (lispm-desktop-shell-new)
    (eat nil '(4))))

(defun lispm-desktop-shell-list--render ()
  "Render the running shells into the current buffer."
  (let ((inhibit-read-only t)
        (shells (seq-filter #'lispm-desktop-shell--buffer-p (buffer-list))))
    (erase-buffer)
    (lispm-desktop-sidebar-set-toolbar
     'shells #'lispm-desktop-shell-list-refresh
     '(("New Shell" lispm-desktop-shell-new "New shell at the main window's project root")))
    (setq lispm-desktop-platinum-info (lispm-desktop-sidebar-count (length shells) "shell"))
    (lispm-desktop-sidebar-insert-columns "Name" "Directory")
    (if (null shells)
        (lispm-desktop-sidebar-insert-empty "No running shells")
      (dolist (buffer shells)
        (let ((directory (buffer-local-value 'default-directory buffer)))
          (lispm-desktop-sidebar-insert-entry
           (lispm-desktop-platinum-icon "computer")
           (string-trim (buffer-name buffer) "\\*" "\\*")
           (file-name-nondirectory (directory-file-name directory))
           (apply-partially #'lispm-desktop-sidebar-show-in-main buffer)
           (format "Show this shell, in %s" (abbreviate-file-name directory))))))))

(defun lispm-desktop-shell-list-refresh (&rest _)
  "Re-render the shell sidebar when it exists."
  (interactive)
  (lispm-desktop-sidebar-refresh-list 'shells))

(defvar-keymap lispm-desktop-shell-list-mode-map
  :doc "Keymap for the shell sidebar."
  :parent special-mode-map
  "g" #'lispm-desktop-shell-list-refresh
  "+" #'lispm-desktop-shell-new
  "n" #'forward-button
  "p" #'backward-button
  "TAB" #'forward-button
  "<backtab>" #'backward-button)

(define-derived-mode lispm-desktop-shell-list-mode special-mode "Shells"
  "Sidebar listing running shells.
\\{lispm-desktop-shell-list-mode-map}"
  (lispm-desktop-sidebar-setup-buffer))

(lispm-desktop-sidebar-define-view
 'shells :title "Shells" :icon "computer" :command 'lispm-desktop-sidebar-shells
 :help "Shells"
 :buffer lispm-desktop-shell-list-buffer-name
 :mode #'lispm-desktop-shell-list-mode
 :render #'lispm-desktop-shell-list--render)

(defun lispm-desktop-sidebar-shells ()
  "Show the running shells in the left sidebar, or hide them when shown."
  (interactive)
  (lispm-desktop-sidebar-select 'shells))

(defun lispm-desktop-setup-shells ()
  "Bind \\`C-c t' to a new shell, and keep the shells sidebar current as
terminals exit."
  (keymap-global-set "C-c t" #'lispm-desktop-eat-new)
  (add-hook 'eat-exit-hook #'lispm-desktop-shell-list-refresh)
  (add-hook 'lispm-desktop-term-exit-functions #'lispm-desktop-shell-list-refresh))

(provide 'lispm-desktop-shells)
;;; lispm-desktop-shells.el ends here
