;;; lispm-desktop-interface.el --- Windows, workspaces, tabs and terminals  -*- lexical-binding: t; -*-

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

;; The frame's layout: a left sidebar, a main area with terminals on the
;; left and files on the right, a workspace per project, window tab
;; lines, fonts that keep terminal columns aligned, and Eat terminals.

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'thingatpt)
(require 'url-util)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-i18n)
(require 'lispm-desktop-tty-images)

(defvar lispm-desktop-platinum-mode-line-format)

(defun lispm-desktop-disable-display-line-numbers ()
  "Disable line numbers in the current buffer."
  (display-line-numbers-mode -1))

(defun lispm-desktop-dired-plain-lines ()
  "Show dired listings without line numbers or visual line wrapping."
  (display-line-numbers-mode -1)
  (visual-line-mode -1)
  (setq-local truncate-lines t))

;;;; Main area layout
;;
;; The main area is every window that is not a side window.  It keeps two
;; columns, whatever command shows a buffer:
;;
;;   sessions   terminals and agent conversations, on the left: a window
;;              already showing a session is reused, else the leftmost
;;              window, split first when it shows a file
;;   the rest   files, and any buffer switched to in place, on the right,
;;              never over a session nor in the sidebar: they go to the
;;              rightmost window that is not a session, else a session
;;              window is split for them
;;
;; Popups (help, completions, transient menus) open as they ask.  The
;; rules sit in `display-buffer-alist', and `switch-to-buffer' obeys them
;; too, so C-x b, C-x C-f, the sidebar, the title bar's buffer menu and
;; the agents' own display requests all keep the columns.

(defalias 'lispm-desktop-main-area--window-p #'lispm-desktop-platinum-main-window-p)
(defalias 'lispm-desktop-main-area--windows #'lispm-desktop-platinum-main-windows)

(defvar-local lispm-desktop-conversation-buffer nil
  "Non-nil in a buffer holding a conversation with an agent running in Emacs.
It is laid out as a terminal, though it may visit a file.")
(put 'lispm-desktop-conversation-buffer 'permanent-local t)

(defvar lispm-desktop-conversation-modes nil
  "Major modes of agent conversations, laid out as terminals.
Agents running inside Emacs add their chat modes, so a chat keeps to the
session column from the moment it is created.")

(defun lispm-desktop-terminal-buffer-p (buffer)
  "Return non-nil when BUFFER is laid out as a terminal: a session.
That is an Eat terminal, or an agent conversation, see
`lispm-desktop-conversation-buffer' and `lispm-desktop-conversation-modes'."
  (let ((mode (buffer-local-value 'major-mode buffer)))
    (or (lispm-desktop-terminal-p buffer)
        (buffer-local-value 'lispm-desktop-conversation-buffer buffer)
        (and lispm-desktop-conversation-modes
             (apply #'provided-mode-derived-p mode lispm-desktop-conversation-modes)))))

(defun lispm-desktop-main-area--terminal-p (window)
  "Return non-nil when WINDOW shows a terminal."
  (lispm-desktop-terminal-buffer-p (window-buffer window)))

(defun lispm-desktop-main-area--kind (window)
  "Return the kind of WINDOW of the main area: `session' for a terminal.
This is `lispm-desktop-platinum-window-kind-function': sessions trade
places only with sessions, keeping to the left."
  (and (lispm-desktop-main-area--terminal-p window) 'session))

(defun lispm-desktop--session-scroll-bar (buffer)
  "Say no scroll bar for BUFFER, a session, laid out as a terminal.
This is one of `lispm-desktop-platinum-scroll-bar-functions'."
  (and (lispm-desktop-terminal-buffer-p buffer) 'no))

(defun lispm-desktop-main-area--file-p (window)
  "Return non-nil when WINDOW shows a buffer visiting a file, not a terminal."
  (let ((buffer (window-buffer window)))
    (and (buffer-file-name buffer) (not (lispm-desktop-terminal-buffer-p buffer)))))

(defun lispm-desktop-main-area--by-use (windows)
  "Return WINDOWS, the most recently used first."
  (sort (copy-sequence windows)
        (lambda (a b) (> (window-use-time a) (window-use-time b)))))

(defun lispm-desktop-main-area--session-window (buffer)
  "Return the window session BUFFER belongs in, and whether it is new.
The value is (WINDOW . NEW).  A window showing BUFFER wins, then the
session window used last; else the leftmost window, split off to the
left of a file it shows."
  (let* ((windows (lispm-desktop-main-area--windows))
         (leftmost (or (car windows) (selected-window))))
    (or (when-let* ((window (or (get-buffer-window buffer)
                                (car (lispm-desktop-main-area--by-use
                                      (seq-filter #'lispm-desktop-main-area--terminal-p windows))))))
          (cons window nil))
        (if (lispm-desktop-main-area--file-p leftmost)
            (cons (split-window leftmost nil 'left) t)
          (cons leftmost nil)))))

(defun lispm-desktop-main-area--file-window ()
  "Return the window a file belongs in, and whether it is new.
The value is (WINDOW . NEW).  The rightmost window showing a file wins,
then the rightmost window showing no session; else the rightmost session
window is split, and the file takes its right half."
  (let ((windows (lispm-desktop-main-area--windows)))
    (if-let* ((window (or (car (last (seq-filter #'lispm-desktop-main-area--file-p windows)))
                          (car (last (seq-remove #'lispm-desktop-main-area--terminal-p windows))))))
        (cons window nil)
      (cons (split-window (or (car (last windows)) (selected-window)) nil 'right) t))))

(defun lispm-desktop-main-area-display-session (buffer alist)
  "Display session BUFFER in the session column, per ALIST.
This is a `display-buffer' action function."
  (pcase-let ((`(,window . ,new) (lispm-desktop-main-area--session-window buffer)))
    (window--display-buffer buffer window (if new 'window 'reuse) alist)))

(defun lispm-desktop-main-area-display-file (buffer alist)
  "Display file BUFFER in the file column, per ALIST.
This is a `display-buffer' action function."
  (pcase-let ((`(,window . ,new) (lispm-desktop-main-area--file-window)))
    (window--display-buffer buffer window (if new 'window 'reuse) alist)))

(defun lispm-desktop-main-area-show-terminal (buffer)
  "Show terminal BUFFER in the session column and select its window."
  (select-window (lispm-desktop-main-area-display-session buffer nil))
  buffer)

(defun lispm-desktop-main-area--session-p (buffer-or-name _action)
  "Return non-nil when BUFFER-OR-NAME is a session about to be shown.
This is the `display-buffer-alist' condition keeping sessions on the left."
  (when-let* ((buffer (get-buffer buffer-or-name)))
    (and (lispm-desktop-terminal-buffer-p buffer)
         ;; A session shown in place in a session window stays there, so
         ;; two sessions side by side remain possible.
         (not (and (lispm-desktop-main-area--terminal-p (selected-window))
                   (lispm-desktop-main-area--window-p (selected-window))
                   (not (get-buffer-window buffer)))))))

(defun lispm-desktop-main-area--astray-p (buffer-or-name action)
  "Return non-nil when BUFFER-OR-NAME is about to cover a session or the sidebar.
That is a buffer other than a session, shown from a session window or a
side window, such as the sidebar: a file, whatever ACTION asks, or any
buffer switched to in place.  This is the `display-buffer-alist'
condition keeping the session column for sessions."
  (when-let* ((buffer (get-buffer buffer-or-name)))
    (and (not (lispm-desktop-terminal-buffer-p buffer))
         (or (buffer-file-name buffer)
             (memq 'display-buffer-same-window (ensure-list (car-safe action))))
         (or (window-parameter (selected-window) 'window-side)
             (lispm-desktop-main-area--terminal-p (selected-window))))))

;;;; Project workspaces
;;
;; Under `tabspaces-mode' each tab is a workspace owning its buffers.  Entry
;; points that open a project's buffer from anywhere, such as the agent
;; sidebar, first enter that project's workspace, so workspaces follow
;; projects instead of one tab collecting every project.

(declare-function tabspaces--current-tab-name "tabspaces" ())
(declare-function tabspaces--list-tabspaces "tabspaces" ())
(declare-function tabspaces--remember-project-tab "tabspaces" (project-directory tab-name))
(declare-function tabspaces-switch-or-create-workspace "tabspaces" (&optional workspace))
(declare-function tabspaces-reset-buffer-list "tabspaces" ())

(defun lispm-desktop-workspace-enter (name &optional directory)
  "Select the workspace NAME, creating it when missing.
Return non-nil when it was created.  DIRECTORY, when given, is recorded as the
new workspace's project, so project commands run inside it resolve there.
Without `tabspaces-mode' nothing happens."
  (when (and (bound-and-true-p tabspaces-mode)
             (not (equal name (tabspaces--current-tab-name))))
    (let ((fresh (not (member name (tabspaces--list-tabspaces)))))
      (tabspaces-switch-or-create-workspace name)
      (when (and fresh directory)
        (tabspaces--remember-project-tab directory name))
      fresh)))

(defun lispm-desktop-workspace-show-alone (buffer)
  "Show BUFFER as the only buffer of the workspace just created.
A new tab starts with the previous workspace's buffer on display; replacing
it and resetting the tab's buffer list keeps that buffer out."
  (let ((window (lispm-desktop-platinum-main-window)))
    (set-window-buffer window buffer)
    (select-window window))
  (tabspaces-reset-buffer-list)
  buffer)

;;;; Tab bar

(defun lispm-desktop-tab-bar-tab-name ()
  "Name the current tab after the buffer of its main, non-side window.
Agent terminals are named by their session title."
  (let* ((window (or (seq-find (lambda (window)
                                 (not (window-parameter window 'window-side)))
                               (cons (selected-window) (window-list nil 'no-mini)))
                     (selected-window)))
         (buffer (window-buffer window)))
    (or (lispm-agents-buffer-label buffer) (buffer-name buffer))))

(defconst lispm-desktop-default-font-families '("Monaco" "Menlo" "DejaVu Sans Mono")
  "Font families for the default face, in order of preference.
Mac OS Emacs (NS) draws in Monaco when nothing is set, but X Emacs asks
fontconfig for \"monospace\", which on a Mac is Andale Mono; naming the
family keeps every build of Emacs alike.")

(defun lispm-desktop-pin-default-font ()
  "Draw the default face in the first installed of the default font families.
They are `lispm-desktop-default-font-families'.  A default face customized
with Customize is left as it is."
  (unless (or (get 'default 'saved-face) (get 'default 'customized-face))
    (when-let* ((family (seq-find (lambda (family) (member family (font-family-list)))
                                  lispm-desktop-default-font-families)))
      (set-face-attribute 'default nil :family family :height 120))))

(defun lispm-desktop-frame-menu-bar (&optional frame)
  "Keep FRAME's menu bar only where it is drawn outside the frame.
Mac OS Emacs (NS) puts it at the top of the screen; on X or a text
terminal it would be a second bar inside the frame, above the Platinum
menu bar."
  (let ((frame (or frame (selected-frame))))
    (when (and (not (eq (framep frame) 'ns))
               (or (display-graphic-p frame) (lispm-desktop-tty-p frame)))
      (set-frame-parameter frame 'menu-bar-lines 0))))

(defconst lispm-desktop-cjk-font-families (list lispm-desktop-terminal-font-family "PingFang SC")
  "Font families for CJK scripts, in order of preference.
Without a pinned family Emacs searches fallback fonts for each CJK glyph,
which is slow on macOS and makes text-heavy terminals stutter.  The
terminal family comes first: its ideographs are two cells wide, so CJK
text keeps the column grid; PingFang is proportional and does not.")

(defconst lispm-desktop-fallback-font-rescale '(("Arial Unicode MS" . 0.86))
  "Scale factors for proportional fallback fonts.
Symbols no monospace family covers (dingbat asterisks such as ✻ ✳ ✽)
fall back to Arial Unicode MS, whose glyphs are wider and taller than a
12pt cell; at this scale they fit one cell without growing the line.")

;; A zh_CN locale selects a Chinese language environment, which counts
;; East Asian Ambiguous characters (│ ─ ● … ⏺) as two columns.  Terminal
;; programs follow wcwidth and count them as one, so Eat placed every
;; character after them a column off and split-pane dividers broke apart.
(setopt cjk-ambiguous-chars-are-wide nil)

(defun lispm-desktop-pin-cjk-font ()
  "Draw CJK scripts with the first installed of `lispm-desktop-cjk-font-families'."
  (when-let* ((family (seq-find (lambda (family) (member family (font-family-list)))
                                lispm-desktop-cjk-font-families)))
    (dolist (script '(han cjk-misc kana bopomofo hangul))
      (set-fontset-font t script (font-spec :family family))))
  (dolist (rescale lispm-desktop-fallback-font-rescale)
    (setf (alist-get (car rescale) face-font-rescale-alist nil nil #'equal)
          (cdr rescale))))

(defun lispm-desktop-eat-mark-unmodified (&rest _)
  "Mark the current Eat buffer unmodified after terminal output.
A terminal has no file to save, so its modified flag carries no meaning; left
set, it makes tab bars show an unsaved marker in place of the close button.
`restore-buffer-modified-p' avoids forcing a mode line update per output."
  (restore-buffer-modified-p nil))

(defun lispm-desktop-ansi-apply-faces (&rest _)
  "Keep the two black ANSI colours legible on the active theme.
Dracula draws ANSI black in the editor background and bright black in
pure black, so terminal programs' grey text (Codex streams its status
and reasoning in bright black) vanished until selected.  Bright black
takes the theme's own dimmed text colour, and black a shade below the
background, as in Dracula's terminal palette.  Every terminal and
compilation buffer reads these faces.  A light theme is left alone: on
white, black is legible as it is.  Colours come only from faces this
function never sets, so re-running it is idempotent."
  (require 'ansi-color)
  (when-let* ((background (face-background 'default nil t))
              (dark (< (apply #'+ (or (lispm-desktop-platinum-rgb background) '(1 1 1))) 1.5))
              (dim (face-foreground 'shadow nil t))
              (black (lispm-desktop-platinum-blend background "#000000" 0.2)))
    (set-face-attribute 'ansi-color-bright-black nil :foreground dim :background dim)
    (set-face-attribute 'ansi-color-black nil :foreground black :background black)))

(defcustom lispm-desktop-tty-mouse t
  "Non-nil lets the mouse work in LispM on a text terminal.
Clicks then reach the menu bar, the title bars, the lists and the
desktop, and the wheel scrolls, as on a graphical display; the terminal
selects text itself only with Shift held (Option in macOS's Terminal),
since the clicks are Emacs's.  See `xterm-mouse-mode'."
  :type 'boolean
  :group 'lispm-desktop)

(defun lispm-desktop-setup-interface ()
  "Set up interface."
  (electric-pair-mode 1)
  ;; A terminal reports the mouse only when asked to; on a graphical
  ;; display this changes nothing.
  (when lispm-desktop-tty-mouse
    (xterm-mouse-mode 1))
  ;; A window moved on a text terminal leaves no copy where it was.
  (when (featurep 'tty-child-frames)
    (add-function :before pre-redisplay-function #'lispm-desktop--tty-redraw-moved-frames))
  ;; Icons as images where the terminal shows them.
  (lispm-desktop-tty-images-setup)
  (when (lispm-desktop-macos-p)
    (define-key global-map [ns-drag-file] 'ns-find-file))
  (when (lispm-desktop-gui-p)
    (lispm-desktop-pin-default-font)
    (lispm-desktop-pin-cjk-font))
  (lispm-desktop-ansi-apply-faces)
  (add-hook 'enable-theme-functions #'lispm-desktop-ansi-apply-faces)
  (setq lispm-desktop-platinum-window-kind-function #'lispm-desktop-main-area--kind)
  (add-hook 'lispm-desktop-platinum-scroll-bar-functions #'lispm-desktop--session-scroll-bar)
  ;; `switch-to-buffer' obeys these too, so no command bypasses the columns;
  ;; a dedicated window, such as the sidebar's, sends the buffer elsewhere.
  ;; A file of another project opens where it is asked for, as in Emacs:
  ;; only switching projects changes the workspace.
  (add-to-list 'display-buffer-alist
               '(lispm-desktop-main-area--astray-p lispm-desktop-main-area-display-file))
  (add-to-list 'display-buffer-alist
               '(lispm-desktop-main-area--session-p lispm-desktop-main-area-display-session))
  (add-to-list 'display-buffer-alist
               '(lispm-desktop-sidebar-view-buffer-p lispm-desktop-sidebar-display-view))
  (setq switch-to-buffer-obey-display-actions t
        switch-to-buffer-in-dedicated-window 'pop)
  ;; Emacs opens on the desktop, as Mac OS did: no splash screen.
  (setq inhibit-startup-screen t
        ring-bell-function 'ignore
        doc-view-continuous t)
  (set-face-attribute 'mode-line nil :height 120)
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode 1)
    (mapc #'lispm-desktop-frame-menu-bar (frame-list))
    (add-hook 'after-make-frame-functions #'lispm-desktop-frame-menu-bar))
  ;; On a text terminal, windows meet at a line and menus and alerts are
  ;; outlined in lines, not in | - and +.
  (when (fboundp 'standard-display-unicode-special-glyphs)
    (standard-display-unicode-special-glyphs))
  (show-paren-mode 1)
  (when (fboundp 'tooltip-mode)
    (tooltip-mode 1))
  (global-prettify-symbols-mode 1)
  (global-visual-line-mode 1)
  (global-display-line-numbers-mode 1)
  (global-hl-line-mode 1)
  (dolist (hook '(eshell-mode-hook term-mode-hook shell-mode-hook eat-mode-hook doc-view-mode-hook treemacs-mode-hook))
    (add-hook hook #'lispm-desktop-disable-display-line-numbers))

  (use-package avy
    :bind (("C-c SPC" . avy-goto-char-timer)
           ("M-g w" . avy-goto-word-1)
           ("M-g l" . avy-goto-line)))

  (use-package treemacs
    :bind (("C-c p t" . treemacs-add-and-display-current-project)
           ("C-c p T" . treemacs))
    :config
    ;; Two blanks on a text terminal.
    (setq treemacs-indentation-string (propertize "  " 'display '(when (display-graphic-p) space :width (12)))
          treemacs-indentation 1)
    ;; Every sidebar view has the same bottom edge as any window, so
    ;; switching views keeps it in place.
    (setq treemacs-width (lispm-desktop-sidebar-columns)
          treemacs-is-never-other-window t
          treemacs-user-mode-line-format lispm-desktop-platinum-mode-line-format)
    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

  (use-package dirvish
    :demand t
    :bind (("C-c d" . dirvish)
           ("C-c D" . dirvish-side)
           :map dirvish-mode-map
           ("a" . dirvish-quick-access)
           ("TAB" . dirvish-subtree-toggle)
           ("f" . dirvish-file-info-menu)
           ("s" . dirvish-quicksort)
           ("y" . dirvish-yank-menu)
           ("v" . dirvish-vc-menu)
           ("^" . dirvish-history-last)
           ("h" . dirvish-history-jump)
           ("M-l" . dirvish-ls-switches-menu)
           ("M-m" . dirvish-mark-menu)
           ("?" . dirvish-dispatch))
    :init
    (when (executable-find "gls")
      (setq insert-directory-program "gls"
            dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group"))
    (setq dired-dwim-target t
          dired-kill-when-opening-new-dired-buffer t
          dirvish-attributes '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg)
          dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index))
          dirvish-header-line-height 26
          dirvish-mode-line-height 26
          dirvish-side-width 32
          ;; Named in the language LispM speaks as it is set up.
          dirvish-quick-access-entries `(("h" "~/" ,(lispm-desktop-tr "Home"))
                                         ("d" "~/Downloads/" ,(lispm-desktop-tr "Downloads"))
                                         ("e" ,user-emacs-directory "Emacs")))
    :hook ((dired-mode . lispm-desktop-dired-plain-lines)
           (dirvish-directory-view-mode . lispm-desktop-dired-plain-lines))
    :config
    (dirvish-override-dired-mode 1)
    (dirvish-side-follow-mode 1))

  (use-package eat
    :commands (eat eat-project)
    :bind ("C-c T" . eat-project)
    :init
    (setq eat-enable-auto-line-mode nil
          eat-very-visible-cursor-type '(box nil nil)
          eat-very-visible-vertical-bar-cursor-type '(bar nil nil)
          eat-very-visible-horizontal-bar-cursor-type '(hbar nil nil)
          eat-term-name "xterm-256color"
          eat-term-terminfo-directory (cond
                                       ((file-directory-p "/opt/local/share/terminfo")
                                        "/opt/local/share/terminfo")
                                       ((file-directory-p "/usr/share/terminfo")
                                        "/usr/share/terminfo")))
    :config
    (advice-add 'eat-exec :filter-args #'lispm-desktop-eat-wrap-command-with-stty)
    (add-hook 'eat-update-hook #'lispm-desktop-eat-mark-unmodified)
    :hook ((eat-mode . lispm-desktop-terminal-display)
           (eat-mode . lispm-desktop-eat-terminal-keys)
           (eshell-load . eat-eshell-mode)
           (eshell-load . eat-eshell-visual-command-mode)))

  (use-package nerd-icons)

  (use-package tramp
    :ensure nil
    :config (setq tramp-default-method "sshx")))


(provide 'lispm-desktop-interface)
;;; lispm-desktop-interface.el ends here
