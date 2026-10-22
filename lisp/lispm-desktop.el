;;; lispm-desktop.el --- LispM's Emacs distribution  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-desktop
;; Version: 0.9.1
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

;; lispm-desktop is LispM's Emacs distribution: Emacs set up as a workspace
;; for coding agents, ready from the first start.
;;
;;   menu bar       the λ menu, a title per sidebar view, and the workspaces
;;                  in the Application menu, laid out as Mac OS 9's
;;   windows        each with a Mac OS 9 title bar: close, zoom, collapse
;;   left sidebar   files, agents, shells, open buffers, as Finder lists
;;   main area      agent chats and terminals on the left, files on the right
;;   workspaces     one per project, owning that project's buffers
;;   coherence      windows out on the computer's screen, the desktop in
;;                  the Dock (C-c h)
;;   loading        Mac OS 9's watch on the pointer while LispM is busy,
;;                  or loads something for what is in front
;;   agents         LispM's, running inside Emacs, and Claude Code and
;;                  Codex, each when its CLI is on PATH (C-c a); others,
;;                  as Hermes, are extensions from the App Store
;;   App Store      applications and extensions to install, from catalogs
;;   applications   the App Store's, each in a window with the desktop's
;;                  interface: buttons, status, menus, keys, boards drawn
;;   Software Update  LispM kept up to date, checked daily and installed
;;                  as it quits, in Control Panels
;;   Startup        LispM opened at login, and startup items opened once
;;                  it has started: applications, documents, programs
;;                  run as services, commands; in Control Panels
;;   Sandbox        the commands and MCP servers agents start confined by
;;                  srt, at None, Low, Middle, High or rules of your own;
;;                  in Control Panels
;;   guide          a tour of all this, as Apple Guide gave one, the
;;                  first time LispM starts and from the Help menu (C-c ?)
;;
;; Its init.el runs `lispm-desktop-setup' and then the user's own file,
;; `lispm-desktop-user-file'.  An existing configuration can use it the same way:
;;
;;   (add-to-list 'load-path "~/src/lispm-desktop/lisp")
;;   (require 'lispm-desktop)
;;   (lispm-desktop-setup)
;;
;; The first start asks for the editing style (`lispm-desktop-editing-style') and
;; the appearance (`lispm-desktop-theme') on a page filling the frame, see
;; lispm-desktop-assistant.el, and installs the icon font; then the guide
;; shows LispM around, see lispm-desktop-guide.el.

;;; Code:

(require 'lispm-desktop-core)
(require 'lispm-desktop-i18n)
(require 'lispm-desktop-proxy)
(require 'lispm-desktop-deps)
;; The browser is lispm-browser, which the desktop's browser files require.
(lispm-desktop-deps-ensure 'lispm-browser)
;; The editor's agents are lispm-agent's, which lispm-desktop-ai.el requires.
(lispm-desktop-deps-ensure 'lispm)
(require 'lispm-desktop-keys)
(require 'lispm-desktop-packages)
(require 'lispm-desktop-interface)
(require 'lispm-desktop-frame)
(require 'lispm-desktop-term)
(require 'lispm-desktop-loading)
(require 'lispm-desktop-look)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-popup)
(require 'lispm-desktop-assistant)
(require 'lispm-desktop-sidebar)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-coherence)
(require 'lispm-desktop-browser)
(require 'lispm-desktop-browser-places)
(require 'lispm-desktop-surface)
(require 'lispm-desktop-win3)
(require 'lispm-desktop-os2)
(require 'lispm-desktop-glow)
(require 'lispm-desktop-glass)
(require 'lispm-desktop-editor)
(require 'lispm-desktop-buffer-manager)
(require 'lispm-desktop-workspaces)
(require 'lispm-desktop-finder)
(require 'lispm-desktop-settings)
(require 'lispm-desktop-update)
(require 'lispm-desktop-startup)
(require 'lispm-desktop-app-window)
(require 'lispm-desktop-needs)
(require 'lispm-desktop-store)
(require 'lispm-desktop-app-yeetube)
(require 'lispm-desktop-app-games)
(require 'lispm-desktop-app-skins)
(require 'lispm-desktop-app-emms)
(require 'lispm-desktop-app-erc)
(require 'lispm-desktop-app-slack)
(require 'lispm-desktop-app-telega)
(require 'lispm-desktop-strip)
(require 'lispm-desktop-guide)
(require 'lispm-desktop-exwm)
(require 'lispm-desktop-languages)
(require 'lispm-desktop-ai)
(require 'lispm-desktop-agent-list)
(require 'lispm-desktop-shells)
(require 'lispm-desktop-lispm)
(require 'lispm-agents-claude)
(require 'lispm-agents-codex)
(require 'lispm-desktop-kernel)

(defvar nerd-icons-font-family)
(declare-function nerd-icons-install-fonts "nerd-icons" (&optional pfx))

(defcustom lispm-desktop-startup-view 'agents
  "Sidebar view shown when Emacs starts, or nil for none.
A key of `lispm-desktop-sidebar-views'."
  :type '(choice (const :tag "None" nil) symbol)
  :group 'lispm-desktop)

(defun lispm-desktop--first-start ()
  "Settle what the first start asks; return non-nil when this is it.
The editing style and the appearance not chosen yet are asked, see
`lispm-desktop-assistant-ask', and the answers saved with Customize;
the language first, along with them, while it is not chosen either.  A
batch Emacs asks nothing and saves nothing: the computer's language,
Emacs keys, Dracula; so it goes too when the questions are put off,
until the next start asks again."
  (when-let* ((questions (seq-remove (lambda (question) (symbol-value (car question)))
                                     lispm-desktop-assistant-questions)))
    (let* ((questions (if lispm-desktop-language
                          questions
                        (cons lispm-desktop-assistant-language-question questions)))
           (answers (unless noninteractive (lispm-desktop-assistant-ask questions))))
      ;; Customize's word on the file it wrote would sit under the page.
      (let ((inhibit-message t))
        (dolist (question questions)
          (if-let* ((answer (assq (car question) answers)))
              (customize-save-variable (car question) (cdr answer))
            (set-default (car question) (lispm-desktop-assistant-default question))))))
    t))

(defun lispm-desktop--icon-font-installed-p ()
  "Return non-nil when the icon font of nerd-icons is installed."
  (find-font (font-spec :family nerd-icons-font-family)))

(defun lispm-desktop-install-fonts ()
  "Install the icon font the sidebar, tabs and mode line draw with."
  (interactive)
  (require 'nerd-icons)
  (nerd-icons-install-fonts t))

(defun lispm-desktop--install-missing-fonts ()
  "On a graphical display, install the icon font when it is missing.
Nothing is asked: without it the sidebar, tabs and mode line show empty
boxes.  A download that fails, as offline, is only reported."
  (when (and (display-graphic-p) (not noninteractive))
    (require 'nerd-icons)
    (unless (lispm-desktop--icon-font-installed-p)
      (lispm-desktop-assistant-report (lispm-desktop-tr "Installing the icon font…"))
      (condition-case err
          (lispm-desktop-install-fonts)
        (error (message (lispm-desktop-tr "The icon font could not be installed (%s); M-x lispm-desktop-install-fonts tries again")
                        (error-message-string err)))))))

(defun lispm-desktop--show-startup-view ()
  "Show `lispm-desktop-startup-view' in the sidebar of the first frame."
  (when (and lispm-desktop-startup-view (not noninteractive)
             (not (lispm-desktop-sidebar-current-view)))
    (lispm-desktop-sidebar-select lispm-desktop-startup-view)))

(defun lispm-desktop-setup ()
  "Set Emacs up as lispm-desktop."
  (let ((first (lispm-desktop--first-start)))
    ;; The first start's page stays up while LispM is set up, and goes
    ;; however that ends.
    (unwind-protect
        (progn
          ;; First, so that whatever is fetched next goes through it.
          (lispm-desktop-setup-proxy)
          (lispm-desktop-setup-common-packages)
          (lispm-desktop-setup-clipboard)
          (lispm-desktop-setup-keymapping)
          (lispm-desktop-setup-interface)
          (lispm-desktop-setup-term)
          (lispm-desktop-setup-loading)
          (lispm-desktop-setup-platinum)
          (lispm-desktop-setup-sidebar)
          (lispm-desktop-popup-enable)
          (lispm-desktop-setup-apps)
          (lispm-desktop-setup-coherence)
          (lispm-desktop-setup-desktop)
          (lispm-desktop-setup-editor)
          (lispm-desktop-setup-workspaces)
          (lispm-desktop-setup-finder)
          (lispm-desktop-setup-browser)
          (lispm-desktop-setup-browser-places)
          (lispm-desktop-setup-settings)
          (lispm-desktop-setup-update)
          (lispm-desktop-setup-startup)
          (lispm-desktop-setup-app-windows)
          (lispm-desktop-setup-app-ui)
          (lispm-desktop-setup-needs)
          (lispm-desktop-setup-app-games)
          (lispm-desktop-setup-store)
          (lispm-desktop-setup-strip)
          (lispm-desktop-setup-guide)
          (setq lispm-desktop--set-up t)
          (add-variable-watcher 'lispm-desktop-theme #'lispm-desktop--theme-changed)
          (lispm-desktop-setup-look)
          (lispm-desktop-setup-glass)
          (lispm-desktop-setup-i18n)
          (lispm-desktop-setup-langs)
          (lispm-desktop-setup-ai)
          (lispm-desktop-setup-agent-list)
          (lispm-desktop-setup-shells)
          ;; LispM is set up first, so its agent comes first; but the
          ;; others are set up even when it cannot be, as offline at a
          ;; first start that would install it.
          (condition-case err
              (lispm-desktop-setup-lispm)
            (error (message (lispm-desktop-tr "LispM's agents are not set up: %s") (error-message-string err))))
          (lispm-agents-setup-claude)
          (lispm-agents-setup-codex)
          ;; After the agents, whose sessions it reads; off unless asked for.
          (lispm-desktop-setup-kernel)
          ;; After init, so personal.el can still set `lispm-desktop-window-manager'.
          (add-hook 'window-setup-hook #'lispm-desktop-setup-exwm)
          (add-hook 'window-setup-hook #'lispm-desktop--show-startup-view)
          (when first
            (lispm-desktop--install-missing-fonts)))
      (lispm-desktop-assistant-close))))


(provide 'lispm-desktop)
;;; lispm-desktop.el ends here
