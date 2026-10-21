;;; lispm-desktop-app-telega.el --- Telega, Telegram on the desktop  -*- lexical-binding: t; -*-

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

;; Telega, the App Store's Telegram application, as it opens on the
;; desktop, beyond what lispm-desktop-app-ui.el finds out of it:
;;
;;   window     titled Telega, opening on Telega's own list of chats, its
;;              root buffer; a chat opens in the window, titled by the
;;              chat, Back going back to the list
;;   setting up Telega talks to Telegram through telega-server, a program
;;              it builds once from TDLib, Telegram's library, of version
;;              1.8.66 or later; until it is built the window says what is
;;              missing and how to get it: TDLib, with Homebrew's command
;;              to copy, the C compiler, then a button building
;;              telega-server; then it signs in, asking your phone number
;;              and the code Telegram sends
;;   buttons    Search…, New Chat… and Join by Link… over the list; Back,
;;              Attach… and Mark Read over a chat
;;   status     Telega's connection and the chats with messages unread, on
;;              the window's bottom
;;
;; A configuration of yours running telega-server in Docker
;; (`telega-use-docker') is followed, and nothing is built.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-app-ui)

;; Telega's own variables and functions, read once it runs.
(defvar telega-directory)
(defvar telega-server-command)
(defvar telega-server-libs-prefix)
(defvar telega-use-docker)
(defvar telega-root-buffer-name)
(defvar telega-tdlib-min-version)
(defvar telega--status)
(defvar telega--unread-chat-count)
(defvar telega-chatbuf--chat)
(declare-function telega "telega" (&optional arg))
(declare-function telega-server-live-p "telega-server" ())
(declare-function telega-server-build "telega-server" (&optional build-flags))
(declare-function telega-chat-title "telega-chat" (chat &optional fmt-type no-badges))

(defconst lispm-desktop-app-telega-setup-buffer-name "Telega"
  "Name of the buffer of Telega's window while telega-server is not built.")

(defconst lispm-desktop-app-telega-tdlib-prefixes '("/opt/homebrew" "/usr/local")
  "Where TDLib is looked for beside `telega-server-libs-prefix': Homebrew's
places, on Apple's chips and on Intel's.")

(defconst lispm-desktop-app-telega-tdlib-command "brew install tdlib --HEAD"
  "The command installing TDLib: Homebrew's release of it is too old for
Telega, its HEAD is built from TDLib's latest sources.")

(defconst lispm-desktop-app-telega-tdlib-upgrade-command "brew reinstall tdlib --HEAD"
  "The command installing TDLib's latest sources over an older TDLib.")

(defconst lispm-desktop-app-telega-compiler-command "xcode-select --install"
  "The command installing the C compiler and make, Apple's Command Line Tools.")

;;;; What it needs

(defun lispm-desktop-app-telega--server ()
  "Return the file of telega-server once built, or nil."
  (let ((exec-path (cons telega-directory exec-path)))
    (executable-find telega-server-command)))

(defun lispm-desktop-app-telega--tdlib-header (prefix)
  "Return non-nil when TDLib's headers are installed under PREFIX."
  (file-exists-p (expand-file-name "include/td/telegram/td_json_client.h" prefix)))

(defun lispm-desktop-app-telega--tdlib-prefix ()
  "Return where TDLib is installed: `telega-server-libs-prefix', or one of
`lispm-desktop-app-telega-tdlib-prefixes'; nil when nowhere."
  (seq-find #'lispm-desktop-app-telega--tdlib-header
            (cons telega-server-libs-prefix lispm-desktop-app-telega-tdlib-prefixes)))

(defun lispm-desktop-app-telega-tdlib-version (prefix)
  "Return the version of TDLib installed under PREFIX, as its CMake files
say it, or nil when they do not."
  (let ((file (expand-file-name "lib/cmake/Td/TdConfigVersion.cmake" prefix)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (when (re-search-forward "PACKAGE_VERSION \"\\([0-9.]+\\)\"" nil t)
          (match-string 1))))))

(defun lispm-desktop-app-telega-missing ()
  "Return what Telega lacks before it can start, the first of: `tdlib',
`old-tdlib', `compiler' and `server'; or nil when it can start."
  (unless (or telega-use-docker (lispm-desktop-app-telega--server))
    (let* ((prefix (lispm-desktop-app-telega--tdlib-prefix))
           (version (and prefix (lispm-desktop-app-telega-tdlib-version prefix))))
      (cond ((null prefix) 'tdlib)
            ((and version (version< version telega-tdlib-min-version)) 'old-tdlib)
            ((not (and (executable-find "cc") (or (executable-find "gmake") (executable-find "make"))))
             'compiler)
            (t 'server)))))

;;;; Opening it

(defun lispm-desktop-app-telega ()
  "Open Telega's list of chats, starting Telega; or, while telega-server is
not built, the page saying what it needs."
  (interactive)
  (require 'telega)
  (if (lispm-desktop-app-telega-missing)
      (progn
        (switch-to-buffer (get-buffer-create lispm-desktop-app-telega-setup-buffer-name))
        (unless (derived-mode-p 'lispm-desktop-app-telega-setup-mode)
          (lispm-desktop-app-telega-setup-mode)))
    (unless (telega-server-live-p)
      (telega t))
    (switch-to-buffer (get-buffer-create telega-root-buffer-name))))

(defun lispm-desktop-app-telega-list ()
  "Go back to Telega's list of chats."
  (interactive)
  (if (get-buffer telega-root-buffer-name)
      (switch-to-buffer telega-root-buffer-name)
    (lispm-desktop-app-telega)))

(defun lispm-desktop-app-telega-check-again ()
  "Look again for what Telega needs, and start it once it has it all."
  (interactive)
  (if (lispm-desktop-app-telega-missing)
      (message "Telega still needs %s" (lispm-desktop-app-telega--missing-name))
    (lispm-desktop-app-telega)))

(defun lispm-desktop-app-telega-build ()
  "Build telega-server from the TDLib installed, then start Telega."
  (interactive)
  (let ((telega-server-libs-prefix (or (lispm-desktop-app-telega--tdlib-prefix)
                                       (user-error "TDLib is not installed"))))
    (message "Building telega-server; it takes a minute…")
    (redisplay t)
    (telega-server-build ""))
  (lispm-desktop-app-telega))

(defun lispm-desktop-app-telega--copy (command)
  "Copy COMMAND, to paste in Terminal."
  (kill-new command)
  (message "Copied: %s" command))

(defun lispm-desktop-app-telega-copy-command ()
  "Copy the command installing what Telega lacks, to paste in Terminal."
  (interactive)
  (lispm-desktop-app-telega--copy
   (pcase (lispm-desktop-app-telega-missing)
     ('old-tdlib lispm-desktop-app-telega-tdlib-upgrade-command)
     ('compiler lispm-desktop-app-telega-compiler-command)
     (_ lispm-desktop-app-telega-tdlib-command))))

;;;; The page while it cannot start

(defun lispm-desktop-app-telega--missing-name ()
  "Return what Telega lacks, in words."
  (pcase (lispm-desktop-app-telega-missing)
    ('tdlib "TDLib")
    ('old-tdlib (format "TDLib %s or later" telega-tdlib-min-version))
    ('compiler "a C compiler")
    ('server "telega-server built")
    (_ "nothing")))

(defun lispm-desktop-app-telega--page-text ()
  "Return what Telega's page says it lacks, and how to get it."
  (pcase (lispm-desktop-app-telega-missing)
    ('tdlib
     (format (concat "Telega talks to Telegram through TDLib, Telegram's library,\n"
                     "of version %s or later.  Install it with Homebrew, in Terminal:\n\n%s\n\n"
                     "It is built from its sources, which takes a while.\nThen check again.")
             telega-tdlib-min-version lispm-desktop-app-telega-tdlib-command))
    ('old-tdlib
     (let ((prefix (lispm-desktop-app-telega--tdlib-prefix)))
       (format (concat "The TDLib installed, %s, is older than the %s Telega needs.\n"
                       "Install its latest sources over it, in Terminal:\n\n%s\n\nThen check again.")
               (lispm-desktop-app-telega-tdlib-version prefix) telega-tdlib-min-version
               lispm-desktop-app-telega-tdlib-upgrade-command)))
    ('compiler
     (format (concat "Telega builds telega-server, its program talking to Telegram,\n"
                     "with a C compiler, which Apple's Command Line Tools install:\n\n%s\n\n"
                     "Then check again.")
             lispm-desktop-app-telega-compiler-command))
    (_
     (concat "TDLib is installed.  Telega now builds telega-server,\n"
             "its program talking to Telegram, once: it takes a minute.\n"
             "Then it signs in with your phone number."))))

(defun lispm-desktop-app-telega--page-buttons ()
  "Return the buttons of Telega's page: copying the command it needs, and
looking again; or building telega-server."
  (if (eq (lispm-desktop-app-telega-missing) 'server)
      '(("Build telega-server" lispm-desktop-app-telega-build))
    '(("Copy Command" lispm-desktop-app-telega-copy-command)
      ("Check Again" lispm-desktop-app-telega-check-again))))

(define-derived-mode lispm-desktop-app-telega-setup-mode special-mode "Telega"
  "Telega's window while telega-server is not built.")

(lispm-desktop-app-ui-define 'lispm-desktop-app-telega-setup-mode
  :app 'telega
  :run #'lispm-desktop-app-telega
  :title "Telega"
  :cursor nil
  :buttons '(("Check Again" lispm-desktop-app-telega-check-again))
  :empty `(,(lambda () t) lispm-desktop-app-telega--page-text lispm-desktop-app-telega--page-buttons))

;;;; The list of chats and a chat

(defun lispm-desktop-app-telega--status ()
  "Return what Telega's bottom says: its connection, and the chats with
messages unread."
  (delq nil (list (and (boundp 'telega--status) (stringp telega--status)
                       (not (string-empty-p telega--status)) telega--status)
                  (when-let* ((count (plist-get (bound-and-true-p telega--unread-chat-count) :unread_count))
                              ((> count 0)))
                    (format "%d unread" count)))))

(defun lispm-desktop-app-telega--chat-label ()
  "Return what Telega shows over a chat, its header line, for the strip
of buttons that takes the header line's place."
  (let ((text (string-trim (format-mode-line header-line-format))))
    (unless (string-empty-p text) text)))

(defun lispm-desktop-app-telega--setup-chat ()
  "Title the window by the chat it shows."
  (when-let* ((chat (bound-and-true-p telega-chatbuf--chat)))
    (setq-local lispm-desktop-platinum-title (substring-no-properties (telega-chat-title chat)))))

(lispm-desktop-app-ui-define 'telega-root-mode
  :app 'telega
  :run #'lispm-desktop-app-telega
  :title "Telega"
  :buttons '(("Search…" telega-view-search) ("New Chat…" telega-chat-create)
             ("Join by Link…" telega-chat-join-by-link))
  :status #'lispm-desktop-app-telega--status
  :keys '(("Return" . "Open the chat") ("n  p" . "Next and previous chat") ("s" . "Search")
          ("/" . "Filter the chats") ("\\" . "Sort the chats") ("N" . "New chat")
          ("J" . "Join by a link") ("R" . "Mark the chats read") ("Q" . "Quit Telega")))

(lispm-desktop-app-ui-define 'telega-chat-mode
  :app 'telega
  :run #'lispm-desktop-app-telega
  :buttons '(("Back" lispm-desktop-app-telega-list) ("Attach…" telega-chatbuf-attach)
             ("Mark Read" telega-chatbuf-read-all))
  :label #'lispm-desktop-app-telega--chat-label
  :status #'lispm-desktop-app-telega--status
  :setup #'lispm-desktop-app-telega--setup-chat
  :keys '(("Return" . "Send what is typed") ("C-c C-a" . "Attach a file, a photo…")
          ("C-c C-v" . "Attach what is copied") ("Tab" . "Complete a name, or go to the next link")
          ("M-p  M-n" . "Edit your previous, next message") ("C-c C-k" . "Cancel the reply or edit")
          ("C-c /" . "Filter the messages")))

(provide 'lispm-desktop-app-telega)
;;; lispm-desktop-app-telega.el ends here
