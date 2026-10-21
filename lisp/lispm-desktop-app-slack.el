;;; lispm-desktop-app-slack.el --- Slack on the desktop  -*- lexical-binding: t; -*-

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

;; emacs-slack, the App Store's Slack application, as it opens on the
;; desktop, beyond what lispm-desktop-app-ui.el finds out of it:
;;
;;   window      titled Slack, opening on the list of its conversations,
;;               see lispm-desktop-app-chat.el: each workspace over its
;;               channels and the people you talk with; a page saying how
;;               to sign in while it has no workspace
;;   workspaces  signed in to as it opens: Add Workspace… asks for its
;;               name, as in NAME.slack.com, your email there, and the
;;               token and cookie Slack's web app signs in with, kept with
;;               auth-source, as emacs-slack's manual keeps them, and the
;;               workspace with Customize; How to Sign In says where to
;;               find them
;;   rooms       opened in its window, Back going back to the list, and
;;               so whatever emacs-slack shows, rather than in another
;;               window
;;   buttons     Add Workspace…, Open and How to Sign In over the list;
;;               Back, Thread and React… over a room
;;   status      the workspace and how many conversations have messages
;;               unread, on the window's bottom

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'auth-source)
(require 'lispm-desktop-app-ui)
(require 'lispm-desktop-app-chat)

;; emacs-slack's own variables and functions, read once it runs.
(defvar slack-teams-by-token)
(defvar slack-current-buffer)
(defvar slack-buffer-function)
(declare-function slack-register-team "slack" (&rest plist))
(declare-function slack-start "slack" (&optional team))
(declare-function slack-team-find-by-token "slack-team" (token))
(declare-function slack-team-name "slack-team" (team))
(declare-function slack-team-connectedp "slack-team" (team))
(declare-function slack-team-channels "slack-team" (team))
(declare-function slack-team-groups "slack-team" (team))
(declare-function slack-team-ims "slack-team" (team))
(declare-function slack-team-disconnect "slack-websocket" (team))
(declare-function slack-team--delete "slack-team" (team))
(declare-function slack-room-name "slack-room" (room team))
(declare-function slack-room-hidden-p "slack-room" (room))
(declare-function slack-room-member-p "slack-room" (room))
(declare-function slack-room-muted-p "slack-room" (room team))
(declare-function slack-room-has-unread-p "slack-room" (room team))
(declare-function slack-room-mention-count "slack-room" (room team))
(declare-function slack-room-display "slack-message-buffer" (room team &optional success-callback))
(declare-function slack-mpim-p "slack-room" (room))
(declare-function slack-im-user-name "slack-im" (im team))
(declare-function slack-im-p "slack-im" (object))
(declare-function slack-buffer-room "slack-buffer" (buffer))
(declare-function slack-buffer-team "slack-buffer" (buffer))
(declare-function eieio-object-p "eieio-core" (obj))

(defcustom lispm-desktop-app-slack-workspaces nil
  "The Slack workspaces Slack's window signs in to as it opens.
A list of plists, one a workspace added from the window: (:name NAME
:user USER), NAME as in NAME.slack.com, USER your email there.  Their
tokens and cookies are kept with auth-source, as emacs-slack's manual
keeps them: the token as the password of USER at NAME.slack.com, the
cookie as that of USER^cookie."
  :type '(repeat (plist :key-type symbol :value-type string))
  :group 'lispm-desktop)

(defconst lispm-desktop-app-slack-list-buffer-name "Slack"
  "Name of the buffer of Slack's list of conversations.")

(defconst lispm-desktop-app-slack-help-buffer-name "How to Sign In to Slack"
  "Name of the buffer saying how to find a workspace's token and cookie.")

(defconst lispm-desktop-app-slack-customize-url "https://my.slack.com/customize"
  "The page of Slack's web app whose console shows its token.")

;;;; Signing in

(defun lispm-desktop-app-slack--host (workspace)
  "Return the host of WORKSPACE, as auth-source keeps its token for."
  (concat (plist-get workspace :name) ".slack.com"))

(defun lispm-desktop-app-slack--secret (host user)
  "Return the secret auth-source keeps for USER at HOST, or nil."
  (auth-source-pick-first-password :host host :user user))

(defun lispm-desktop-app-slack--keep-secret (host user secret)
  "Keep SECRET for USER at HOST with auth-source, which asks where.
A secret it keeps already is left as it is."
  (unless (lispm-desktop-app-slack--secret host user)
    (let* ((found (car (auth-source-search :host host :user user :secret secret :create t :max 1)))
           (save (plist-get found :save-function)))
      (when (functionp save)
        (funcall save))
      (auth-source-forget-all-cached))))

(defun lispm-desktop-app-slack--team (workspace)
  "Return emacs-slack's team of WORKSPACE, registered with the token and
cookie auth-source keeps for it, or nil when it keeps none."
  (let* ((host (lispm-desktop-app-slack--host workspace))
         (user (plist-get workspace :user))
         (token (lispm-desktop-app-slack--secret host user)))
    (when token
      (or (slack-team-find-by-token token)
          (progn
            (slack-register-team :name (plist-get workspace :name) :token token
                                 :cookie (lispm-desktop-app-slack--secret host (concat user "^cookie"))
                                 :full-and-display-names t)
            (slack-team-find-by-token token))))))

(defun lispm-desktop-app-slack--teams ()
  "Return emacs-slack's teams: those of the workspaces kept, and any your
own configuration registers, by name."
  (when (boundp 'slack-teams-by-token)
    (sort (hash-table-values slack-teams-by-token)
          (lambda (a b) (string-lessp (downcase (slack-team-name a)) (downcase (slack-team-name b)))))))

(defvar lispm-desktop-app-slack--started nil
  "The teams emacs-slack was started for since Emacs started.")

(defun lispm-desktop-app-slack--connected-p (team)
  "Return non-nil when TEAM is connected to Slack."
  (ignore-errors (slack-team-connectedp team)))

(defun lispm-desktop-app-slack--start (team &optional again)
  "Sign in to TEAM, once since Emacs started, or AGAIN."
  (when (or again (not (memq team lispm-desktop-app-slack--started)))
    (cl-pushnew team lispm-desktop-app-slack--started)
    (slack-start team)))

(defun lispm-desktop-app-slack--start-all ()
  "Sign in to the workspaces kept, and those your configuration registers."
  (dolist (workspace lispm-desktop-app-slack-workspaces)
    (unless (lispm-desktop-app-slack--team workspace)
      (message "Slack: auth-source keeps no token for %s at %s"
               (plist-get workspace :user) (lispm-desktop-app-slack--host workspace))))
  (mapc #'lispm-desktop-app-slack--start (lispm-desktop-app-slack--teams)))

(defun lispm-desktop-app-slack-display (buffer)
  "Show BUFFER, which emacs-slack opened, in Slack's window when it is open,
else in another window, as emacs-slack does.  emacs-slack shows a room
once its messages come: in whatever window is selected by then, but for
this.  `slack-buffer-function' is set to this."
  (if-let* ((window (lispm-desktop-app-ui-window 'slack)))
      (lispm-desktop-app-chat-show-in window buffer)
    (switch-to-buffer-other-window buffer)))

(defconst lispm-desktop-app-slack-options
  '((slack-buffer-function lispm-desktop-app-slack-display switch-to-buffer))
  "emacs-slack's options as Slack's window sets them while they are as
emacs-slack has them, see `lispm-desktop-app-chat-set-options': else it
shows its rooms in whatever window is selected once their messages come.
slack-util.el's `defvar' of `slack-buffer-function' comes before its
`defcustom'.")

;;;; What it is signed in to

(defun lispm-desktop-app-slack--room-name (room team)
  "Return the name of ROOM of TEAM, as the list shows it: a channel's after
#, a person's alone."
  (cond ((slack-im-p room) (slack-im-user-name room team))
        ((slack-mpim-p room) (slack-room-name room team))
        (t (concat "#" (slack-room-name room team)))))

(defun lispm-desktop-app-slack--room (room team)
  "Return the conversation of ROOM of TEAM for the list."
  (let ((muted (ignore-errors (slack-room-muted-p room team)))
        (name (lispm-desktop-app-slack--room-name room team)))
    (list :key (list (slack-team-name team) name)
          :name name
          :unread (if (and (not muted) (ignore-errors (slack-room-has-unread-p room team))) t 0)
          :mentions (or (ignore-errors (slack-room-mention-count room team)) 0)
          :state (and muted "muted")
          :open (lambda () (slack-room-display room team)))))

(defun lispm-desktop-app-slack--team-conversations (team)
  "Return TEAM's line, then its channels and the people you talk with."
  (let* ((visible (lambda (rooms)
                    (seq-filter (lambda (room)
                                  (ignore-errors
                                    (and (not (slack-room-hidden-p room))
                                         ;; A channel of the workspace's you have not joined.
                                         (or (slack-im-p room) (slack-room-member-p room)))))
                                rooms)))
         (by-name (lambda (rooms)
                    (sort (mapcar (lambda (room) (lispm-desktop-app-slack--room room team)) rooms)
                          (lambda (a b) (string-lessp (downcase (plist-get a :name)) (downcase (plist-get b :name))))))))
    (cons (list :key (slack-team-name team) :name (slack-team-name team) :heading t
                :state (unless (lispm-desktop-app-slack--connected-p team)
                         (if (memq team lispm-desktop-app-slack--started) "connecting…" "signed out")))
          (append (funcall by-name (funcall visible (append (slack-team-channels team) (slack-team-groups team))))
                  (funcall by-name (funcall visible (slack-team-ims team)))))))

(defun lispm-desktop-app-slack--conversations ()
  "Return the conversations of Slack's list: each workspace over its
channels, then the people you talk with, by name."
  (mapcan (lambda (team)
            (condition-case err
                (lispm-desktop-app-slack--team-conversations team)
              (error (list (list :key (ignore-errors (slack-team-name team))
                                 :name (or (ignore-errors (slack-team-name team)) "Slack") :heading t
                                 :state (error-message-string err))))))
          (lispm-desktop-app-slack--teams)))

(defun lispm-desktop-app-slack--team-here ()
  "Return the team of the line point is on, or of the room this window
shows, or the only one; nil when it cannot tell."
  (let ((teams (lispm-desktop-app-slack--teams)))
    (or (if (derived-mode-p 'lispm-desktop-app-chat-list-mode)
            (when-let* ((key (plist-get (lispm-desktop-app-chat-conversation) :key)))
              (seq-find (lambda (team) (equal (slack-team-name team) (if (consp key) (car key) key))) teams))
          (and (eieio-object-p (bound-and-true-p slack-current-buffer))
               (ignore-errors (slack-buffer-team slack-current-buffer))))
        (and (= (length teams) 1) (car teams)))))

;;;; Commands

(defun lispm-desktop-app-slack ()
  "Open Slack's list of conversations, signing in to its workspaces."
  (interactive)
  (require 'slack)
  (lispm-desktop-app-chat-set-options lispm-desktop-app-slack-options)
  (lispm-desktop-app-slack-list)
  (lispm-desktop-app-slack--start-all)
  (lispm-desktop-app-chat-refresh))

(defun lispm-desktop-app-slack-list ()
  "Go back to Slack's list of conversations."
  (interactive)
  (lispm-desktop-app-chat-show-list lispm-desktop-app-slack-list-buffer-name
                                    'lispm-desktop-app-slack-list-mode))

(defun lispm-desktop-app-slack-add-workspace (name user)
  "Sign in to the Slack workspace NAME, as in NAME.slack.com, as USER, your
email there: its token, and its cookie for a token of the web app's, are
asked for unless auth-source keeps them, and kept with it."
  (interactive
   (list (string-trim (string-remove-suffix ".slack.com" (string-trim (read-string "Workspace, as in NAME.slack.com: "))))
         (string-trim (read-string "Your email there: " user-mail-address))))
  (when (or (string-empty-p name) (string-empty-p user))
    (user-error "A workspace needs its name and your email there"))
  (require 'slack)
  (lispm-desktop-app-chat-set-options lispm-desktop-app-slack-options)
  (let* ((workspace (list :name name :user user))
         (host (lispm-desktop-app-slack--host workspace)))
    (unless (lispm-desktop-app-slack--secret host user)
      (let ((token (string-trim (read-passwd (format "Token of %s (xoxc-… or xoxp-…): " host)))))
        (when (string-empty-p token)
          (user-error "No token"))
        (when (string-prefix-p "xoxc" token)
          (let ((cookie (string-trim (read-passwd "Its cookie d (xoxd-…; d-s=…; lc=…): "))))
            (when (string-empty-p cookie)
              (user-error "A token of the web app's needs its cookie"))
            (lispm-desktop-app-slack--keep-secret host (concat user "^cookie") cookie)))
        (lispm-desktop-app-slack--keep-secret host user token)))
    (unless (member workspace lispm-desktop-app-slack-workspaces)
      (customize-save-variable 'lispm-desktop-app-slack-workspaces
                               (append lispm-desktop-app-slack-workspaces (list workspace))))
    (let ((team (or (lispm-desktop-app-slack--team workspace)
                    (user-error "auth-source did not keep the token of %s" host))))
      (lispm-desktop-app-slack--start team t)))
  (unless (derived-mode-p 'lispm-desktop-app-slack-list-mode)
    (lispm-desktop-app-slack-list))
  (lispm-desktop-app-chat-refresh))

(defun lispm-desktop-app-slack-open ()
  "Open the conversation of the line point is on."
  (interactive)
  (lispm-desktop-app-chat-open))

(defun lispm-desktop-app-slack-reconnect ()
  "Sign in again to the workspace of the line point is on, or of this room."
  (interactive)
  (let ((team (or (lispm-desktop-app-slack--team-here) (user-error "No workspace here"))))
    (lispm-desktop-app-slack--start team t)))

(defun lispm-desktop-app-slack-sign-out ()
  "Sign out of the workspace of the line point is on, and sign in to it no
more as Slack's window opens.  Its token stays where auth-source keeps it."
  (interactive)
  (let* ((team (or (lispm-desktop-app-slack--team-here) (user-error "No workspace here")))
         (name (slack-team-name team)))
    (when (y-or-n-p (format "Sign out of %s? " name))
      (ignore-errors (slack-team-disconnect team))
      (slack-team--delete team)
      (setq lispm-desktop-app-slack--started (delq team lispm-desktop-app-slack--started))
      (customize-save-variable 'lispm-desktop-app-slack-workspaces
                               (seq-remove (lambda (workspace) (equal (plist-get workspace :name) name))
                                           lispm-desktop-app-slack-workspaces))
      (lispm-desktop-app-chat-refresh))))

(defun lispm-desktop-app-slack-help ()
  "Say how to find a workspace's token and cookie, in this window."
  (interactive)
  (switch-to-buffer (get-buffer-create lispm-desktop-app-slack-help-buffer-name))
  (unless (derived-mode-p 'lispm-desktop-app-slack-help-mode)
    (lispm-desktop-app-slack-help-mode)))

(defun lispm-desktop-app-slack-open-customize ()
  "Open the page of Slack's web app whose console shows its token."
  (interactive)
  (browse-url lispm-desktop-app-slack-customize-url))

;;;; The list

(defvar-keymap lispm-desktop-app-slack-list-mode-map
  :doc "Keymap of Slack's list of conversations."
  :parent lispm-desktop-app-chat-list-mode-map
  "a" #'lispm-desktop-app-slack-add-workspace
  "o" #'lispm-desktop-app-slack-open
  "r" #'lispm-desktop-app-slack-reconnect
  "D" #'lispm-desktop-app-slack-sign-out
  "?" #'lispm-desktop-app-slack-help)

(define-derived-mode lispm-desktop-app-slack-list-mode lispm-desktop-app-chat-list-mode "Slack"
  "Slack's conversations, under the workspaces they are in."
  (setq-local lispm-desktop-app-chat-conversations-function #'lispm-desktop-app-slack--conversations))

(defun lispm-desktop-app-slack--list-status ()
  "Return what the bottom of Slack's list says: the workspaces signed in
to, and the conversations with messages unread."
  (let ((connected (seq-count #'lispm-desktop-app-slack--connected-p (lispm-desktop-app-slack--teams))))
    (delq nil (list (format "%d workspace%s" connected (if (= connected 1) "" "s"))
                    (lispm-desktop-app-chat-unread-status (lispm-desktop-app-slack--conversations))))))

(defun lispm-desktop-app-slack--list-menu ()
  "Return the items of Slack's menu over its list."
  (let ((team (lispm-desktop-app-slack--team-here)))
    `((:label "Add Workspace…" :action lispm-desktop-app-slack-add-workspace)
      (:label "Sign In Again" :action lispm-desktop-app-slack-reconnect :enabled ,(and team t))
      (:label "Sign Out…" :action lispm-desktop-app-slack-sign-out :enabled ,(and team t))
      separator
      (:label "Open" :action lispm-desktop-app-slack-open :enabled ,(and (lispm-desktop-app-chat-conversation) t))
      separator
      (:label "How to Sign In" :action lispm-desktop-app-slack-help))))

(lispm-desktop-app-ui-define 'lispm-desktop-app-slack-list-mode
  :app 'slack
  :run #'lispm-desktop-app-slack
  :title "Slack"
  :list t
  :status #'lispm-desktop-app-slack--list-status
  :buttons `(("Add Workspace…" lispm-desktop-app-slack-add-workspace)
             ("Open" lispm-desktop-app-slack-open
              ,(lambda () (plist-get (lispm-desktop-app-chat-conversation) :open)))
             ("How to Sign In" lispm-desktop-app-slack-help))
  :menu (cons "Slack" #'lispm-desktop-app-slack--list-menu)
  :empty `(,(lambda () (null (lispm-desktop-app-slack--teams)))
           ,(concat "No Slack workspace yet.\nSign in to one with the token and cookie\n"
                    "Slack's web app signs in with, which How to Sign In finds.")
           (("Add Workspace…" lispm-desktop-app-slack-add-workspace)
            ("How to Sign In" lispm-desktop-app-slack-help)))
  :keys '(("Return" . "Open the conversation") ("n  p" . "Next and previous conversation")
          ("a" . "Add a workspace") ("r" . "Sign in again") ("D" . "Sign out")
          ("?" . "How to sign in") ("g" . "Draw the list again")))

;;;; How to sign in

(defconst lispm-desktop-app-slack-help-text
  "Slack lets emacs-slack in with the token and the cookie its web app signs in with.  To find them, sign in to your workspace in Chrome or Firefox, then:

1.  Open my.slack.com/customize, with Open Slack's Page below.

2.  Open the browser's developer tools, Inspect in the page's context menu, and their Console.  There type

        window.prompt(\"Token\", TS.boot_data.api_token)

    and copy the token it shows, which starts with xoxc-.

3.  In the developer tools' Application tab (Storage in Firefox), under Cookies, find the cookie named d and copy its value as it is shown, not decoded.  Copy the cookies d-s and lc too, and put them together as

        xoxd-…; d-s=…; lc=…

4.  Press Add Workspace…, then give the workspace's name, as in NAME.slack.com, your email there, the token and the cookie.

They are kept with auth-source, in the first of your auth-sources (~/.authinfo, unless you said another); auth-source asks before it writes there.  Anyone who reads them can read and write in Slack as you: keep them in an encrypted ~/.authinfo.gpg, or in the macOS Keychain."
  "What Slack's window says of signing in.")

(define-derived-mode lispm-desktop-app-slack-help-mode special-mode "How to Sign In"
  "How to find a Slack workspace's token and cookie."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert lispm-desktop-app-slack-help-text "\n")
    (goto-char (point-min))))

(lispm-desktop-app-ui-define 'lispm-desktop-app-slack-help-mode
  :app 'slack
  :title "How to Sign In"
  :prose t
  :buttons '(("Back" lispm-desktop-app-slack-list) ("Open Slack's Page" lispm-desktop-app-slack-open-customize)
             ("Add Workspace…" lispm-desktop-app-slack-add-workspace)))

;;;; A room

(defconst lispm-desktop-app-slack-room-keys
  '(("Return" . "Send what is typed") ("C-c C-f" . "Attach a file")
    ("M-p  M-n" . "What was typed before, and after"))
  "The keys of a room's window.")

(defun lispm-desktop-app-slack--buffer-room ()
  "Return (ROOM . TEAM) of the current room buffer, or nil."
  (when (eieio-object-p (bound-and-true-p slack-current-buffer))
    (ignore-errors (cons (slack-buffer-room slack-current-buffer) (slack-buffer-team slack-current-buffer)))))

(defun lispm-desktop-app-slack--status ()
  "Return what the bottom of a room says: its workspace, and the
conversations with messages unread."
  (delq nil (list (when-let* ((team (cdr (lispm-desktop-app-slack--buffer-room))))
                    (slack-team-name team))
                  (lispm-desktop-app-chat-unread-status (lispm-desktop-app-slack--conversations)))))

(defun lispm-desktop-app-slack--setup-room ()
  "Title the window by the room it shows."
  (pcase (lispm-desktop-app-slack--buffer-room)
    (`(,room . ,team)
     (when room
       (setq-local lispm-desktop-platinum-title (lispm-desktop-app-slack--room-name room team))))))

(lispm-desktop-app-ui-define 'slack-mode
  :app 'slack
  :run #'lispm-desktop-app-slack
  :buttons '(("Back" lispm-desktop-app-slack-list) ("Thread" slack-thread-show-or-create)
             ("React…" slack-message-add-reaction) ("Attach…" slack-file-attach))
  :status #'lispm-desktop-app-slack--status
  :setup #'lispm-desktop-app-slack--setup-room
  :keys lispm-desktop-app-slack-room-keys)

;; A thread's, and the other buffers emacs-slack shows.
(lispm-desktop-app-ui-define 'slack-buffer-mode
  :app 'slack
  :run #'lispm-desktop-app-slack
  :buttons '(("Back" lispm-desktop-app-slack-list) ("React…" slack-message-add-reaction)
             ("Attach…" slack-file-attach))
  :status #'lispm-desktop-app-slack--status
  :keys lispm-desktop-app-slack-room-keys)

(provide 'lispm-desktop-app-slack)
;;; lispm-desktop-app-slack.el ends here
