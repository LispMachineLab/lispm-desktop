;;; lispm-desktop-core.el --- Platform, clipboard and terminal keys  -*- lexical-binding: t; -*-

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

;; What the other modules build on: the platform tests, the clipboard
;; shared by Emacs, Evil and terminals, and the keys Eat terminals pass
;; through to the programs they run.

;;; Code:

(require 'lispm-desktop-i18n)

(defconst lispm-desktop-directory
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "The desktop's directory, a checkout of lispm-desktop: the parent of its
lisp/ directory.")

(defgroup lispm-desktop nil
  "LispM's Emacs distribution."
  :group 'convenience
  :prefix "lispm-desktop-")

(defcustom lispm-desktop-editing-style nil
  "How text is edited: `vim' (Evil) or `emacs'.
Under `vim', Evil edits buffers of program, text and configuration files,
with SPC as the leader key, and every other buffer (terminals, agent
chats, the sidebar, Magit, ...) keeps Emacs keys.  nil means the choice
has not been made yet: the first start asks for it and saves the answer."
  :type '(choice (const :tag "Vim (Evil)" vim)
                 (const :tag "Emacs" emacs)
                 (const :tag "Ask at the next start" nil)))

(defcustom lispm-desktop-theme nil
  "The colour theme: `dracula', `platinum' (Mac OS 9's Platinum greys),
`win3' (Windows 3.2's scheme), `os2' (OS/2 2.0's), or any other theme
installed; the chrome takes its colours from whichever.  nil
means the choice has not been made yet: the first start asks for it.  Change
it from the λ menu, with `lispm-desktop-set-theme', or by setting it in your
personal.el: once Emacs is set up, a new value is loaded at once."
  :type '(choice (const :tag "Dracula" dracula)
                 (const :tag "Platinum" platinum)
                 (const :tag "Windows 3.2" win3)
                 (const :tag "OS/2 2.0" os2)
                 (symbol :tag "Another installed theme")
                 (const :tag "Ask at the next start" nil)))

(defvar lispm-desktop--set-up nil
  "Non-nil once lispm-desktop is set up, its theme loaded.")

(defvar lispm-desktop--loading-theme nil
  "Non-nil while `lispm-desktop--load-theme-now' loads a theme.")

(defvar lispm-desktop--pending-theme nil
  "The theme the next redisplay loads before it draws, as (THEME), or nil.")

(defvar lispm-desktop--theme-error nil
  "The error loading the pending theme signalled, or nil.")

(defun lispm-desktop--theme-changed (_symbol theme operation _where)
  "Load THEME when `lispm-desktop-theme' is set to it once Emacs is set up.
The theme is loaded once the command setting it is done, as whatever
the variable is set to by then.  What a theme being loaded sets it to
is not followed: enabling a theme enables the `user' theme again, which
sets the variable back to the value saved with Customize for a moment,
and loading that theme too would show the old theme again before the
new one.  This is a variable watcher."
  (when (and lispm-desktop--set-up (eq operation 'set) theme
             (not lispm-desktop--loading-theme))
    (run-at-time 0 nil #'lispm-desktop--follow-theme)))

(defun lispm-desktop--follow-theme ()
  "Load `lispm-desktop-theme' unless it is already the only theme enabled."
  (when (and lispm-desktop-theme
             (not (equal custom-enabled-themes (list lispm-desktop-theme))))
    (lispm-desktop-load-theme lispm-desktop-theme)))

(defun lispm-desktop--load-theme-now (theme)
  "Make THEME, by default Dracula, the only colour theme enabled."
  (let ((lispm-desktop--loading-theme t)
        (chosen lispm-desktop-theme))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (or theme 'dracula) t)
    ;; The `user' theme enabled again set the variable back to the value
    ;; saved with Customize; it keeps the one it had.
    (unless (eq lispm-desktop-theme chosen)
      (set-default 'lispm-desktop-theme chosen))))

(defun lispm-desktop--load-pending-theme (&rest _)
  "Load the theme left for this redisplay, before it draws anything.
This is advice before `pre-redisplay-function', which removes itself."
  (remove-function pre-redisplay-function #'lispm-desktop--load-pending-theme)
  (when-let* ((pending lispm-desktop--pending-theme))
    (setq lispm-desktop--pending-theme nil)
    (condition-case err
        (lispm-desktop--load-theme-now (car pending))
      (error (setq lispm-desktop--theme-error err)))))

(defun lispm-desktop--theme-in-redisplay-p ()
  "Return non-nil when a theme is loaded by a redisplay run for it: on
macOS, once Emacs is set up.  See `lispm-desktop-load-theme'."
  (and lispm-desktop--set-up (eq (window-system) 'ns)))

(defun lispm-desktop-load-theme (theme)
  "Make THEME, by default Dracula, the only colour theme enabled.
On macOS, once Emacs is set up, the theme is loaded by a redisplay run
for it, as the redisplay starts and before it draws anything.  NS Emacs
clears a frame to its new background as soon as the background changes,
which a theme change does twice on every frame, and it puts what the
frames hold on the screen whenever it looks for input, which every
redisplay does first: a theme loaded before would show every window
blank for as long as the redisplay takes to draw them all again, a
third of a second on the desktop."
  (if (not (lispm-desktop--theme-in-redisplay-p))
      (lispm-desktop--load-theme-now theme)
    (setq lispm-desktop--pending-theme (list theme)
          lispm-desktop--theme-error nil)
    (add-function :before pre-redisplay-function #'lispm-desktop--load-pending-theme)
    (redisplay t)
    ;; Loaded now if no redisplay ran, as while a keyboard macro runs.
    (lispm-desktop--load-pending-theme)
    (when-let* ((err lispm-desktop--theme-error))
      (setq lispm-desktop--theme-error nil)
      (signal (car err) (cdr err)))))

(defun lispm-desktop-set-theme (theme)
  "Switch to colour THEME now and at every later start."
  (interactive (list (intern (completing-read (lispm-desktop-tr "Theme: ") '("dracula" "platinum" "win3" "os2" "phosphor" "neon") nil t))))
  ;; Saved first, so what the theme draws, as Control Panels, shows it chosen.
  (customize-save-variable 'lispm-desktop-theme theme)
  (lispm-desktop-load-theme theme))

(defcustom lispm-desktop-user-file (locate-user-emacs-file "personal.el")
  "The user's own configuration, loaded after lispm-desktop is set up."
  :type 'file)

(defconst lispm-desktop-sidebar-width 40
  "Total width in columns of every left sidebar view.
All views share it, so switching views keeps the main window's width: a
terminal in the main window is not resized, and a full-screen program in it,
such as a coding agent, does not redraw its whole history.")

(defun lispm-desktop-sidebar-columns (&optional frame)
  "Return the width in columns of the sidebar of FRAME, by default the
selected one: `lispm-desktop-sidebar-width', but on a text terminal no
more than a third of it, so a small terminal keeps room for the rest."
  (if (lispm-desktop-tty-p frame)
      (min lispm-desktop-sidebar-width (max 24 (/ (frame-width frame) 3)))
    lispm-desktop-sidebar-width))

(defun lispm-desktop-macos-p ()
  "Return non-nil on macOS."
  (eq system-type 'darwin))

(defun lispm-desktop-linux-p ()
  "Return non-nil on GNU/Linux."
  (eq system-type 'gnu/linux))

(defun lispm-desktop-gui-p ()
  "Return non-nil when Emacs is running with a graphical display."
  (display-graphic-p))

(defun lispm-desktop-tty-p (&optional frame)
  "Return non-nil when FRAME, by default the selected one, is on a text terminal.
Its chrome is then drawn in text, cells for pixels.  A batch Emacs has
no display to draw on and is not one: what it draws, as for tests, is
what a graphical display gets."
  (and (not noninteractive) (not (display-graphic-p frame))))

(defun lispm-desktop-px (pixels)
  "Return PIXELS, a length the chrome was drawn to in pixels, for the
selected frame.  A text terminal counts a cell as a pixel, so there it
is the cells as many pixels of an 8-pixel-wide font would take: 4 to
11 pixels a cell, a gap of fewer none."
  (if (lispm-desktop-tty-p)
      (/ (+ pixels 4) 8)
    pixels))

(defun lispm-desktop-align-right (&optional pixels)
  "Return where to align text PIXELS short of a window's right edge, as
`(space :align-to POSITION)' takes it; PIXELS are the frame's, cells on
a text terminal, by default none.  A text terminal keeps a window's last
column for the sign of a line too long for it, so there the edge is a
cell short of the window's; and space aligned up to that column runs
into it, so a terminal aligns a cell short of that again."
  (let ((pixels (+ (or pixels 0) (if (lispm-desktop-tty-p) 2 0))))
    (if (zerop pixels) 'right `(- right (,pixels)))))

(defun lispm-desktop-setup-meta-key-issue ()
  "Map macOS command key to Meta."
  (when (lispm-desktop-macos-p)
    (setq mac-option-key-is-meta nil
          mac-command-key-is-meta t
          mac-command-modifier 'meta
          mac-option-modifier nil)))

(defun lispm-desktop-pbcopy (text &optional _push)
  "Copy TEXT to the macOS clipboard."
  (let ((process-connection-type nil))
    (let ((process (start-process "pbcopy" nil "pbcopy")))
      (process-send-string process text)
      (process-send-eof process))))

(defun lispm-desktop-pbpaste ()
  "Return text from the macOS clipboard."
  (let ((text (shell-command-to-string "pbpaste")))
    (unless (string= text "")
      text)))

(defun lispm-desktop-setup-clipboard ()
  "Set up clipboard integration."
  (setq select-enable-clipboard t
        save-interprogram-paste-before-kill t)
  (when (and (lispm-desktop-macos-p)
             (not (lispm-desktop-gui-p))
             (executable-find "pbcopy")
             (executable-find "pbpaste"))
    (setq interprogram-cut-function #'lispm-desktop-pbcopy
          interprogram-paste-function #'lispm-desktop-pbpaste)))

(defun lispm-desktop-clipboard-text ()
  "Return text from the system clipboard or kill ring."
  (or (and (lispm-desktop-macos-p) (executable-find "pbpaste") (lispm-desktop-pbpaste))
      (current-kill 0 t)))

(defun lispm-desktop-selection-range ()
  "Return the active selection range."
  (cond
   ((and (bound-and-true-p evil-local-mode)
         (eq evil-state 'visual)
         evil-visual-beginning
         evil-visual-end)
    (cons evil-visual-beginning evil-visual-end))
   ((use-region-p)
    (cons (region-beginning) (region-end)))))

(defun lispm-desktop-clear-selection ()
  "Clear the active selection."
  (when (and (bound-and-true-p evil-local-mode)
             (eq evil-state 'visual))
    (evil-normal-state))
  (deactivate-mark))

(defvar-local lispm-desktop-copy-function nil
  "Command copying this buffer's selection, for buffers whose selection
is not an Emacs region, such as a web page; nil copies the region.")

(defvar-local lispm-desktop-paste-function nil
  "Function inserting the clipboard text, its one argument, for buffers
that cannot take it as buffer text, such as a web page; nil inserts it.")

(defun lispm-desktop-copy-region ()
  "Copy the active region, or this buffer's selection by its own function.
That is `lispm-desktop-copy-function', when the buffer sets it."
  (interactive)
  (let ((range (lispm-desktop-selection-range)))
    (cond
     (lispm-desktop-copy-function (call-interactively lispm-desktop-copy-function))
     (range
      (kill-new (filter-buffer-substring (car range) (cdr range)))
      (lispm-desktop-clear-selection)
      (message (lispm-desktop-tr "Copied")))
     (t (user-error (lispm-desktop-tr "No active region"))))))

(defconst lispm-desktop-clipboard-keys
  '(("M-c" . lispm-desktop-copy-region)
    ("M-v" . lispm-desktop-paste))
  "The unified copy and paste keys: Command-C and Command-V, Command being
Meta.  C-w, M-w and C-y stay Emacs's own kill and yank, which share the
system clipboard, except in terminals, see
`lispm-desktop-terminal-clipboard-keys'.")

(defconst lispm-desktop-terminal-clipboard-keys
  '(("M-w" . lispm-desktop-copy-region)
    ("C-y" . lispm-desktop-paste))
  "The keys that copy and paste in a terminal besides
`lispm-desktop-clipboard-keys': a terminal's text is the program's, not a
kill ring, so Emacs's own copy and yank keys take the clipboard there.")

(defun lispm-desktop-bind-clipboard-keys (map &optional terminal)
  "Bind the unified copy and paste keys in MAP, and \\`M-x' to
`execute-extended-command', which a map taking Meta keys would hide.
With TERMINAL, bind the terminal's copy and paste keys too, see
`lispm-desktop-terminal-clipboard-keys'."
  (keymap-set map "M-x" #'execute-extended-command)
  (pcase-dolist (`(,key . ,command) (append lispm-desktop-clipboard-keys
                                            (and terminal lispm-desktop-terminal-clipboard-keys)))
    (keymap-set map key command)))

;;;; Terminals

(defconst lispm-desktop-terminal-major-modes '(eat-mode lispm-desktop-term-mode)
  "Major modes of terminals: Eat's, and lispm-desktop-term's.")

(defun lispm-desktop-terminal-p (&optional buffer)
  "Return non-nil when BUFFER, by default the current one, is a terminal,
of Eat or of lispm-desktop-term, its program running or not.
`lispm-desktop-eat-buffer-p' and `lispm-desktop-term-buffer-p' tell the
two apart."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (memq (buffer-local-value 'major-mode buffer) lispm-desktop-terminal-major-modes)
         t)))

(defconst lispm-desktop-terminal-font-family "Maple Mono NF CN"
  "Monospace family whose glyphs all occupy whole terminal cells.
A CJK ideograph is exactly two Latin cells wide, and box drawing, block
elements and most terminal UI symbols are covered at one cell, so columns
that a terminal program lays out stay aligned.")

(defconst lispm-desktop-terminal-glyph-substitutions '((?⏺ . ?●) (?✢ . ?✶))
  "Display substitutions for symbols no monospace font draws at one cell.
Each pair maps a character a terminal program emits to the look-alike
shown in its place; the buffer text is unchanged.")

(defun lispm-desktop-terminal-display-table ()
  "Return a display table applying `lispm-desktop-terminal-glyph-substitutions'.
It draws the border of a text terminal's windows as the standard display
table does, which it stands in for."
  (let ((table (make-display-table)))
    (pcase-dolist (`(,from . ,to) lispm-desktop-terminal-glyph-substitutions)
      (aset table from (vector to)))
    (when standard-display-table
      (set-display-table-slot table 'vertical-border
                              (display-table-slot standard-display-table 'vertical-border)))
    table))

(defun lispm-desktop-terminal-display ()
  "Lay the current buffer out for a terminal's output, as every terminal is.
The terminal already wraps lines to its width, so Emacs-side visual line
wrapping only adds work; terminal text is left-to-right, so bidirectional
reordering is turned off; and a full-screen program redraws every line, so
highlighting the current line only adds an overlay update per redraw.
Terminal output is not an edit: `first-change-hook' is emptied locally, so
output never runs edit-tracking hooks such as tab bars recomputing their
modified markers.  Symbols no monospace font draws at one cell are shown
as look-alikes, see `lispm-desktop-terminal-glyph-substitutions'.  On a
graphical display, text is drawn in `lispm-desktop-terminal-font-family' so
every glyph fills whole cells and split-pane dividers drawn with box
characters stay straight."
  (visual-line-mode -1)
  (setq-local buffer-display-table (lispm-desktop-terminal-display-table)
              first-change-hook nil
              truncate-lines t
              bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t
              global-hl-line-mode nil)
  (when (and (display-graphic-p)
             (member lispm-desktop-terminal-font-family (font-family-list)))
    (face-remap-add-relative 'default :family lispm-desktop-terminal-font-family)))

(defun lispm-desktop-eat-buffer-p ()
  "Return non-nil in an Eat terminal buffer."
  (bound-and-true-p eat-terminal))

(defun lispm-desktop-eat-wrap-command-with-stty (args)
  "Wrap Eat command ARGS to initialize tty erase."
  (pcase-let ((`(,buffer ,name ,command ,startfile ,switches) args))
    (list buffer
          name
          "/bin/sh"
          startfile
          (list "-lc"
                (format "stty erase '^?'; exec %s"
                        (mapconcat #'shell-quote-argument
                                   (cons command switches)
                                   " "))))))

(defun lispm-desktop-eat-send-backspace ()
  "Send a backward delete character to Eat."
  (interactive)
  (eat-term-send-string eat-terminal "\C-?"))

(defconst lispm-desktop-eat-terminal-key-sequences
  '(("<escape>" . "\e")
    ("S-<return>" . "\e[13;2u")
    ("C-<return>" . "\e[13;5u")
    ("C-u" . "\C-u")
    ("C-w" . "\C-w")
    ("C-z" . "\C-z"))
  "Keys Emacs would keep from a terminal, with the bytes they send instead.
Escape alone is otherwise a Meta prefix waiting for its next key, so agents
never see it (both stop their work on it).  Return with Shift or Control
has no byte of its own; it is sent in the CSI u form Claude Code and Codex
read as a new line and, for Claude, sending a queued message now.  C-u
clears the input line rather than starting a prefix argument, C-w deletes
a word rather than cutting a region, and C-z reaches the program rather
than leaving Evil's Emacs state.")

(defun lispm-desktop-eat-send-terminal-key ()
  "Send the terminal the bytes of the key just pressed.
The bytes come from `lispm-desktop-eat-terminal-key-sequences'."
  (interactive)
  (when-let* ((sequence (cdr (assoc (key-description (this-command-keys-vector))
                                    lispm-desktop-eat-terminal-key-sequences))))
    (eat-term-send-string eat-terminal sequence)))

(defvar lispm-desktop-eat-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (lispm-desktop-bind-clipboard-keys map t)
    (define-key map (kbd "DEL") #'lispm-desktop-eat-send-backspace)
    (define-key map (kbd "<backspace>") #'lispm-desktop-eat-send-backspace)
    (define-key map (kbd "<delete>") #'lispm-desktop-eat-send-backspace)
    (define-key map (kbd "<deletechar>") #'lispm-desktop-eat-send-backspace)
    (dolist (key lispm-desktop-eat-terminal-key-sequences)
      (define-key map (kbd (car key)) #'lispm-desktop-eat-send-terminal-key))
    map)
  "Keymap of Eat terminals: the unified clipboard keys, and the keys of
`lispm-desktop-eat-terminal-key-sequences' passed through to the program.  Every
other key behaves as Eat binds it.")

(define-minor-mode lispm-desktop-eat-keys-mode
  "Use the unified clipboard keys and pass agent keys through in Eat.
Its keymap takes precedence over every mode keymap, see
`emulation-mode-map-alists'."
  :init-value nil
  :lighter nil
  :keymap lispm-desktop-eat-keys-mode-map)

(defvar lispm-desktop-eat-keys-mode-map-alist
  `((lispm-desktop-eat-keys-mode . ,lispm-desktop-eat-keys-mode-map)))

(add-to-list 'emulation-mode-map-alists 'lispm-desktop-eat-keys-mode-map-alist)

(defun lispm-desktop-eat-terminal-keys ()
  "Give the current Eat terminal its keys, in `lispm-desktop-eat-keys-mode'.
Keymaps earlier in `emulation-mode-map-alists' win, and Evil, loaded after
this file, puts its own first; its Emacs state binds C-w and C-z.  The
terminal keymap is moved to the front again, so in a terminal it outranks
Evil; elsewhere it is inactive and changes nothing."
  (setq emulation-mode-map-alists
        (cons 'lispm-desktop-eat-keys-mode-map-alist
              (delq 'lispm-desktop-eat-keys-mode-map-alist emulation-mode-map-alists)))
  (lispm-desktop-eat-keys-mode 1))

(defun lispm-desktop-paste ()
  "Paste from the system clipboard or kill ring.
A buffer's `lispm-desktop-paste-function', when set, receives the text instead."
  (interactive)
  (let ((text (lispm-desktop-clipboard-text)))
    (unless text
      (user-error (lispm-desktop-tr "Clipboard is empty")))
    (cond (lispm-desktop-paste-function (funcall lispm-desktop-paste-function text))
          ((lispm-desktop-eat-buffer-p)
           (eat-term-send-string-as-yank eat-terminal (list text)))
          (t (insert-for-yank text)))))

(defun lispm-desktop--paste-delete-selection ()
  "Return the `delete-selection' type of `lispm-desktop-paste': `yank'
where it inserts buffer text, so that, where `delete-selection-mode' is on,
pasted text replaces the selection as yanked text does.  Text sent to a
page or a terminal leaves the buffer's selection alone."
  (unless (or lispm-desktop-paste-function (lispm-desktop-eat-buffer-p))
    'yank))

(put 'lispm-desktop-paste 'delete-selection #'lispm-desktop--paste-delete-selection)

(defconst lispm-desktop-area-prefixes
  '(tab-bar tab-line header-line mode-line menu-bar tool-bar vertical-line
    vertical-scroll-bar horizontal-scroll-bar left-fringe right-fringe
    left-margin right-margin right-divider bottom-divider internal-border
    child-frame-border)
  "Symbols `read-event' returns before a mouse event over part of a frame
or window other than text: the event follows.")

(defun lispm-desktop-typed-event-p (event)
  "Return non-nil when EVENT was typed: a character or a function key, not
a mouse event, nor the area symbol before one, see
`lispm-desktop-area-prefixes'."
  (or (integerp event)
      (and (symbolp event) event (not (memq event lispm-desktop-area-prefixes)))))

(provide 'lispm-desktop-core)
;;; lispm-desktop-core.el ends here
