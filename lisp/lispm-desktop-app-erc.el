;;; lispm-desktop-app-erc.el --- ERC, IRC on the desktop  -*- lexical-binding: t; -*-

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

;; ERC, Emacs's own IRC client, as the App Store's IRC application opens
;; on the desktop, beyond what lispm-desktop-app-ui.el finds out of it:
;;
;;   window    titled ERC, opening on the list of its conversations, see
;;             lispm-desktop-app-chat.el: each network it is on over its
;;             channels and the people talking with you; a page saying how
;;             to begin while it is on none
;;   networks  connected to again as it opens: Connect… asks for a server,
;;             Libera.Chat's by default, its port, TLS's but for 6667,
;;             and a nickname, and keeps them with Customize, and the
;;             channels joined there, as they are joined and left
;;   channels  opened in its window, Back going back to the list: those
;;             joined with Join…, and with /join and /query typed there;
;;             any other ERC opens is listed, its messages unread, rather
;;             than shown in another window
;;   buttons   Connect…, Join…, Open and Disconnect over the list; Back,
;;             Join… and Leave over a channel, its topic beside them
;;   status    the network and your nickname, how many are in the channel
;;             and how many conversations have messages unread, on the
;;             window's bottom
;;
;; Passwords are ERC's to ask for: it finds a server's, and NickServ's,
;; with auth-source.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-app-ui)
(require 'lispm-desktop-app-chat)
(require 'lispm-desktop-i18n)

;; ERC's own variables and functions, read once it runs.
(defvar erc-server-process)
(defvar erc-server-connected)
(defvar erc-session-server)
(defvar erc-session-port)
(defvar erc-network)
(defvar erc-channel-topic)
(defvar erc-channel-users)
(defvar erc-modified-channels-alist)
(defvar erc-autojoin-channels-alist)
(defvar erc-nick)
(defvar erc-join-buffer)
(declare-function erc "erc" (&rest arguments))
(declare-function erc-tls "erc" (&rest arguments))
(declare-function erc-buffer-list "erc" (&optional predicate proc))
(declare-function erc-server-buffer "erc" ())
(declare-function erc-server-process-alive "erc-backend" (&optional buffer))
(declare-function erc-current-nick "erc" ())
(declare-function erc-default-target "erc" ())
(declare-function erc-channel-p "erc" (target))
(declare-function erc-query-buffer-p "erc" (&optional buffer))
(declare-function erc-cmd-JOIN "erc" (channel &optional key))
(declare-function erc-cmd-QUIT "erc" (reason))
(declare-function erc-cmd-RECONNECT "erc" (&rest arguments))
(declare-function erc-controls-strip "erc-goodies" (str))

(defcustom lispm-desktop-app-erc-networks nil
  "The IRC networks ERC's window connects to as it opens.
A list of plists, one a network connected to from the window: (:server
HOST :port PORT :tls TLS :nick NICK :channels CHANNELS), TLS non-nil to
connect with TLS, CHANNELS the channels joined there, kept as they are
joined and left."
  :type '(repeat (plist :key-type symbol :value-type sexp))
  :group 'lispm-desktop)

(defconst lispm-desktop-app-erc-servers
  '("irc.libera.chat" "irc.oftc.net" "irc.rizon.net" "irc.ergo.chat" "irc.hackint.org")
  "IRC servers Connect… offers, Libera.Chat's, where Emacs's channels are, first.")

(defconst lispm-desktop-app-erc-list-buffer-name "ERC"
  "Name of the buffer of ERC's list of conversations.")

;;;; The networks kept

(defun lispm-desktop-app-erc--network (server)
  "Return the network kept whose server is SERVER, or nil."
  (seq-find (lambda (network) (string-equal-ignore-case (plist-get network :server) server))
            lispm-desktop-app-erc-networks))

(defun lispm-desktop-app-erc--keep (networks)
  "Keep NETWORKS as `lispm-desktop-app-erc-networks', with Customize."
  (unless (equal networks lispm-desktop-app-erc-networks)
    (customize-save-variable 'lispm-desktop-app-erc-networks networks)))

(defun lispm-desktop-app-erc--remember (network)
  "Keep NETWORK, a plist as `lispm-desktop-app-erc-networks' holds, in
place of the one of the same server, its channels kept."
  (let* ((old (lispm-desktop-app-erc--network (plist-get network :server)))
         (network (plist-put (copy-sequence network) :channels (plist-get old :channels))))
    (lispm-desktop-app-erc--keep
     (if old
         (mapcar (lambda (other) (if (eq other old) network other)) lispm-desktop-app-erc-networks)
       (append lispm-desktop-app-erc-networks (list network))))))

(defun lispm-desktop-app-erc--keep-channel (server channel joined)
  "Keep CHANNEL among the channels of the network of SERVER when JOINED,
else take it out, for the next connection there."
  (when-let* ((network (lispm-desktop-app-erc--network server)))
    (let* ((channels (plist-get network :channels))
           (kept (seq-find (lambda (other) (string-equal-ignore-case other channel)) channels))
           (channels (cond ((and joined (not kept)) (append channels (list channel)))
                           ((and kept (not joined)) (remove kept channels))
                           (t channels))))
      (unless (equal channels (plist-get network :channels))
        (lispm-desktop-app-erc--keep
         (mapcar (lambda (other)
                   (if (eq other network) (plist-put (copy-sequence network) :channels channels) other))
                 lispm-desktop-app-erc-networks))))))

(defun lispm-desktop-app-erc--joined ()
  "Keep the channel just joined, the current buffer's.
This is one of `erc-join-hook'."
  (when-let* ((channel (erc-default-target)))
    (lispm-desktop-app-erc--keep-channel erc-session-server channel t)
    (lispm-desktop-app-erc--opened-joined (current-buffer) channel)))

(defun lispm-desktop-app-erc--parted (buffer)
  "Take out the channel of BUFFER, just left.
This is one of `erc-part-hook'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* ((channel (erc-default-target)))
        (lispm-desktop-app-erc--keep-channel erc-session-server channel nil)))))

(defun lispm-desktop-app-erc--killed ()
  "Take out the channel of the current buffer, killed while connected, which
leaves it.  This is one of `erc-kill-channel-hook'."
  (when-let* (((erc-server-process-alive))
              (channel (erc-default-target)))
    (lispm-desktop-app-erc--keep-channel erc-session-server channel nil)))

(defun lispm-desktop-app-erc--connected (&rest _)
  "Join the channels kept for the network just connected to, in its buffer.
This is one of `erc-after-connect'."
  (when-let* ((network (lispm-desktop-app-erc--network erc-session-server)))
    (dolist (channel (plist-get network :channels))
      (erc-cmd-JOIN channel))))

;;;; Showing what ERC opens

(defvar lispm-desktop-app-erc--opening nil
  "The channel Join… asked for and the window it was asked in, while the
network has not answered: (CHANNEL . WINDOW).")

(defun lispm-desktop-app-erc--opened-joined (buffer channel)
  "Show BUFFER, of CHANNEL just joined, in the window Join… asked for it in."
  (pcase lispm-desktop-app-erc--opening
    (`(,asked . ,window)
     (when (string-equal-ignore-case asked channel)
       (setq lispm-desktop-app-erc--opening nil)
       (lispm-desktop-app-chat-show-in window buffer)))))

(defun lispm-desktop-app-erc-display (buffer action)
  "Show BUFFER, which ERC opened, as its window on the desktop does: in its
window when asked for, as by /join or /query typed there, or in the
selected window while ERC's is closed; else not, and its messages unread
are listed.  ACTION is ERC's, its alist saying what opened BUFFER.
ERC's options showing buffers are set to this."
  (when (alist-get 'erc-interactive-display (cdr action))
    (if-let* ((window (lispm-desktop-app-ui-window 'erc)))
        (lispm-desktop-app-chat-show-in window buffer)
      (switch-to-buffer buffer))))

(defconst lispm-desktop-app-erc-options
  '((erc-buffer-display lispm-desktop-app-erc-display)
    (erc-interactive-display lispm-desktop-app-erc-display)
    (erc-receive-query-display lispm-desktop-app-erc-display)
    (erc-auto-reconnect-display lispm-desktop-app-erc-display)
    (erc-track-exclude-types ("JOIN" "KICK" "MODE" "NICK" "PART" "QUIT"
                              "324" "329" "332" "333" "353" "366" "477")))
  "ERC's options as its window sets them while they are as ERC has them,
see `lispm-desktop-app-chat-set-options': the buffers it opens shown by
`lispm-desktop-app-erc-display', else ERC shows them in other windows,
splitting its window on the desktop; and messages unread counted without
those saying who came, left or set what, nor what a channel holds.")

(defun lispm-desktop-app-erc--set-up ()
  "Set ERC up for its window: its options, where you have not set them,
see `lispm-desktop-app-erc-options', and keeping the networks' channels."
  (require 'erc)
  (require 'erc-track)
  (lispm-desktop-app-chat-set-options lispm-desktop-app-erc-options)
  (add-hook 'erc-after-connect #'lispm-desktop-app-erc--connected)
  (add-hook 'erc-join-hook #'lispm-desktop-app-erc--joined)
  (add-hook 'erc-part-hook #'lispm-desktop-app-erc--parted)
  (add-hook 'erc-kill-channel-hook #'lispm-desktop-app-erc--killed)
  (add-hook 'erc-track-list-changed-hook #'lispm-desktop-app-chat-refresh-all))

;;;; What it is connected to

(defun lispm-desktop-app-erc--server-p ()
  "Return non-nil when the current buffer is an ERC network's own buffer."
  (and (bound-and-true-p erc-server-process)
       (eq (process-buffer erc-server-process) (current-buffer))))

(defun lispm-desktop-app-erc--servers ()
  "Return the buffers of the networks ERC is on, or was, by name."
  (when (featurep 'erc)
    (sort (erc-buffer-list #'lispm-desktop-app-erc--server-p)
          (lambda (a b) (string-lessp (downcase (buffer-name a)) (downcase (buffer-name b)))))))

(defun lispm-desktop-app-erc--live-servers ()
  "Return the buffers of the networks ERC is connected to."
  (seq-filter #'erc-server-process-alive (lispm-desktop-app-erc--servers)))

(defun lispm-desktop-app-erc-network-name (&optional buffer)
  "Return the name of the network of BUFFER, by default the current one's:
the network's own, once it said it, else its server's."
  (with-current-buffer (or buffer (current-buffer))
    (let ((server (or (erc-server-buffer) (current-buffer))))
      (with-current-buffer server
        (if (bound-and-true-p erc-network) (symbol-name erc-network) erc-session-server)))))

(defun lispm-desktop-app-erc--state (server)
  "Return what the line of the network of buffer SERVER says of it: nil
once connected."
  (cond ((not (process-live-p (buffer-local-value 'erc-server-process server))) (lispm-desktop-tr "disconnected"))
        ((not (buffer-local-value 'erc-server-connected server)) (lispm-desktop-tr "connecting…"))))

(defun lispm-desktop-app-erc--unread (buffer)
  "Return (UNREAD . MENTIONS) of BUFFER, as ERC tracks them: how many
messages came there while it was not shown, and t when one named you, as
every one does in a conversation with one person."
  (pcase (assq buffer (bound-and-true-p erc-modified-channels-alist))
    (`(,_ ,count . ,face)
     (cons count (and (or (memq 'erc-current-nick-face (ensure-list face)) (erc-query-buffer-p buffer))
                      t)))
    (_ '(0 . nil))))

(defun lispm-desktop-app-erc--conversation (buffer &optional heading state)
  "Return the conversation of BUFFER for the list, as a HEADING with STATE
for a network's, whose own messages, the server's, are not counted."
  (pcase-let ((`(,unread . ,mentions) (if heading '(0 . nil) (lispm-desktop-app-erc--unread buffer))))
    (list :key buffer
          :name (if heading (lispm-desktop-app-erc-network-name buffer) (buffer-name buffer))
          :heading heading :state state :unread unread :mentions mentions
          :open (lambda () (switch-to-buffer buffer)))))

(defun lispm-desktop-app-erc--conversations ()
  "Return the conversations of ERC's list: each network, then its channels
and the people talking with you, by name."
  (mapcan (lambda (server)
            (let ((process (buffer-local-value 'erc-server-process server)))
              (cons (lispm-desktop-app-erc--conversation server t (lispm-desktop-app-erc--state server))
                    (mapcar #'lispm-desktop-app-erc--conversation
                            (sort (erc-buffer-list (lambda () (not (eq (current-buffer) server))) process)
                                  (lambda (a b)
                                    (let ((a-channel (erc-channel-p a)) (b-channel (erc-channel-p b)))
                                      (if (eq (and a-channel t) (and b-channel t))
                                          (string-lessp (downcase (buffer-name a)) (downcase (buffer-name b)))
                                        a-channel))))))))
          (lispm-desktop-app-erc--servers)))

(defun lispm-desktop-app-erc--server-here ()
  "Return the buffer of the network of the line point is on, or of the
current ERC buffer, or the only one connected; nil when it cannot tell."
  (let ((buffer (if (derived-mode-p 'lispm-desktop-app-chat-list-mode)
                    (plist-get (lispm-desktop-app-chat-conversation) :key)
                  (current-buffer))))
    (or (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (and (derived-mode-p 'erc-mode) (erc-server-buffer))))
        (let ((live (lispm-desktop-app-erc--live-servers)))
          (and (= (length live) 1) (car live))))))

(defun lispm-desktop-app-erc--read-server (prompt)
  "Return the buffer of a network connected to, asking with PROMPT which,
where the line point is on does not say."
  (or (when-let* ((server (lispm-desktop-app-erc--server-here)))
        (and (erc-server-process-alive server) server))
      (let ((live (mapcar (lambda (server) (cons (lispm-desktop-app-erc-network-name server) server))
                          (lispm-desktop-app-erc--live-servers))))
        (unless live
          (user-error (lispm-desktop-tr "ERC is connected to no network")))
        (cdr (assoc (completing-read prompt live nil t) live)))))

;;;; Commands

(defun lispm-desktop-app-erc ()
  "Open ERC's list of conversations, connecting to the networks kept where
ERC is on none."
  (interactive)
  (lispm-desktop-app-erc--set-up)
  (lispm-desktop-app-erc-list)
  (unless (lispm-desktop-app-erc--servers)
    (mapc #'lispm-desktop-app-erc--open-network lispm-desktop-app-erc-networks)
    (lispm-desktop-app-chat-refresh)))

(defun lispm-desktop-app-erc-list ()
  "Go back to ERC's list of conversations."
  (interactive)
  (lispm-desktop-app-chat-show-list lispm-desktop-app-erc-list-buffer-name 'lispm-desktop-app-erc-list-mode))

(defun lispm-desktop-app-erc--open-network (network)
  "Connect to NETWORK, a plist as `lispm-desktop-app-erc-networks' holds,
its buffer opened but not shown, nor left current, as ERC leaves it."
  (save-current-buffer
    (let ((erc-join-buffer 'bury))
      (apply (if (plist-get network :tls) #'erc-tls #'erc)
             :server (plist-get network :server) :port (plist-get network :port)
             :nick (plist-get network :nick) nil))))

(defun lispm-desktop-app-erc-connect (server port nick)
  "Connect to the IRC SERVER at PORT, with TLS but on 6667, as NICK; and
keep them, to connect again as ERC's window opens."
  (interactive
   (let* ((server (string-trim (completing-read (format-prompt (lispm-desktop-tr "Connect to the IRC server")
                                                               (car lispm-desktop-app-erc-servers))
                                                lispm-desktop-app-erc-servers nil nil nil nil
                                                (car lispm-desktop-app-erc-servers))))
          (kept (lispm-desktop-app-erc--network server))
          (port (read-number (lispm-desktop-tr "Port: ") (or (plist-get kept :port) 6697)))
          (default (or (plist-get kept :nick) (bound-and-true-p erc-nick) (user-login-name)))
          (nick (string-trim (read-string (format-prompt (lispm-desktop-tr "Nickname") default) nil nil default))))
     (list server port nick)))
  (lispm-desktop-app-erc--set-up)
  (let ((network (list :server server :port port :tls (/= port 6667) :nick nick)))
    (lispm-desktop-app-erc--remember network)
    (lispm-desktop-app-erc--open-network network))
  (when (derived-mode-p 'lispm-desktop-app-chat-list-mode)
    (lispm-desktop-app-chat-refresh)))

(defun lispm-desktop-app-erc-channel-name (name)
  "Return NAME, typed, as a channel's name: # before it unless it starts
with a channel's prefix already, as #emacs, &local or ##lispm."
  (let ((name (string-trim name)))
    (when (string-empty-p name)
      (user-error (lispm-desktop-tr "No channel")))
    (if (string-match-p "\\`[#&+!]" name) name (concat "#" name))))

(defun lispm-desktop-app-erc-join (channel)
  "Join CHANNEL on the network of the line point is on, or of the current
channel, and show it in this window once joined."
  (interactive (list (lispm-desktop-app-erc-channel-name (read-string (lispm-desktop-tr "Join the channel: ")))))
  (let ((server (lispm-desktop-app-erc--read-server (lispm-desktop-tr "Join on the network: "))))
    (setq lispm-desktop-app-erc--opening (cons channel (selected-window)))
    (with-current-buffer server
      (erc-cmd-JOIN channel))))

(defun lispm-desktop-app-erc-leave ()
  "Leave the channel this window shows, or close the conversation, and go
back to the list."
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (and (derived-mode-p 'erc-mode) (not (lispm-desktop-app-erc--server-p)))
      (user-error (lispm-desktop-tr "This is no channel")))
    (lispm-desktop-app-erc-list)
    ;; Killing a channel's buffer leaves the channel.
    (kill-buffer buffer)
    (lispm-desktop-app-chat-refresh)))

(defun lispm-desktop-app-erc-open ()
  "Open the conversation of the line point is on."
  (interactive)
  (lispm-desktop-app-chat-open))

(defun lispm-desktop-app-erc-disconnect ()
  "Disconnect from the network of the line point is on, or of this window.
It stays kept, to connect to again as ERC's window opens."
  (interactive)
  (let ((server (lispm-desktop-app-erc--read-server (lispm-desktop-tr "Disconnect from the network: "))))
    (with-current-buffer server
      (erc-cmd-QUIT ""))))

(defun lispm-desktop-app-erc-reconnect ()
  "Connect again to the network of the line point is on, or of this window,
and to every network kept where it cannot tell."
  (interactive)
  (lispm-desktop-app-erc--set-up)
  (let ((server (let ((buffer (if (derived-mode-p 'lispm-desktop-app-chat-list-mode)
                                  (plist-get (lispm-desktop-app-chat-conversation) :key)
                                (current-buffer))))
                  (and (buffer-live-p buffer)
                       (with-current-buffer buffer (and (derived-mode-p 'erc-mode) (erc-server-buffer)))))))
    (cond ((and server (not (erc-server-process-alive server)))
           (with-current-buffer server (erc-cmd-RECONNECT)))
          (server (message (lispm-desktop-tr "Already connected to %s") (lispm-desktop-app-erc-network-name server)))
          (t (dolist (network lispm-desktop-app-erc-networks)
               (unless (seq-some (lambda (buffer)
                                   (and (string-equal-ignore-case (buffer-local-value 'erc-session-server buffer)
                                                                  (plist-get network :server))
                                        (erc-server-process-alive buffer)))
                                 (lispm-desktop-app-erc--servers))
                 (lispm-desktop-app-erc--open-network network))))))
  (when (derived-mode-p 'lispm-desktop-app-chat-list-mode)
    (lispm-desktop-app-chat-refresh)))

(defun lispm-desktop-app-erc-forget ()
  "Forget the network of the line point is on: disconnect from it, close
its channels, and connect to it no more as ERC's window opens."
  (interactive)
  (let* ((server (or (lispm-desktop-app-erc--server-here) (user-error (lispm-desktop-tr "No network here"))))
         (name (lispm-desktop-app-erc-network-name server))
         (host (buffer-local-value 'erc-session-server server)))
    (when (y-or-n-p (lispm-desktop-tr "Forget %s, closing its channels? " name))
      (lispm-desktop-app-erc--keep (seq-remove (lambda (network)
                                                 (string-equal-ignore-case (plist-get network :server) host))
                                               lispm-desktop-app-erc-networks))
      (with-current-buffer server
        ;; ERC's autojoin would join its channels at the next connection.
        (let ((network (bound-and-true-p erc-network)))
          (setq erc-autojoin-channels-alist
                (seq-remove (pcase-lambda (`(,key . ,_))
                              (if (stringp key) (string-match-p key host) (eq key network)))
                            (bound-and-true-p erc-autojoin-channels-alist))))
        (let ((buffers (erc-buffer-list nil erc-server-process)))
          (when (erc-server-process-alive)
            (erc-cmd-QUIT ""))
          (mapc #'kill-buffer buffers)))
      (when (derived-mode-p 'lispm-desktop-app-chat-list-mode)
        (lispm-desktop-app-chat-refresh)))))

;;;; The list

(defvar-keymap lispm-desktop-app-erc-list-mode-map
  :doc "Keymap of ERC's list of conversations."
  :parent lispm-desktop-app-chat-list-mode-map
  "c" #'lispm-desktop-app-erc-connect
  "j" #'lispm-desktop-app-erc-join
  "o" #'lispm-desktop-app-erc-open
  "d" #'lispm-desktop-app-erc-disconnect
  "r" #'lispm-desktop-app-erc-reconnect
  "D" #'lispm-desktop-app-erc-forget)

(define-derived-mode lispm-desktop-app-erc-list-mode lispm-desktop-app-chat-list-mode "ERC"
  "ERC's conversations, under the networks they are on."
  (setq-local lispm-desktop-app-chat-conversations-function #'lispm-desktop-app-erc--conversations))

(defun lispm-desktop-app-erc--on-network-p ()
  "Return non-nil when the line point is on is a network connected to, or
one of its conversations."
  (when-let* ((server (lispm-desktop-app-erc--server-here)))
    (erc-server-process-alive server)))

(defun lispm-desktop-app-erc--list-status ()
  "Return what the bottom of ERC's list says: the networks it is on, and the
conversations with messages unread."
  (let ((live (length (lispm-desktop-app-erc--live-servers))))
    (delq nil (list (lispm-desktop-tr-n "%d network" "%d networks" live)
                    (lispm-desktop-app-chat-unread-status (lispm-desktop-app-erc--conversations))))))

(defun lispm-desktop-app-erc--list-empty-text ()
  "Return what ERC's page says while it is on no network."
  (if lispm-desktop-app-erc-networks
      (lispm-desktop-tr "Not connected.\nConnect again to the networks you were on, or to another.")
    (lispm-desktop-tr "Not on IRC yet.\nConnect to a network, Libera.Chat or another,\nthen join its channels.")))

(defun lispm-desktop-app-erc--list-empty-buttons ()
  "Return the buttons of ERC's page while it is on no network."
  (if lispm-desktop-app-erc-networks
      `((,(lispm-desktop-tr "Connect") lispm-desktop-app-erc-reconnect)
        (,(lispm-desktop-tr "Connect to Another…") lispm-desktop-app-erc-connect))
    `((,(lispm-desktop-tr "Connect…") lispm-desktop-app-erc-connect))))

(defun lispm-desktop-app-erc--list-menu ()
  "Return the items of ERC's menu over its list."
  (let ((network (lispm-desktop-app-erc--on-network-p))
        (here (lispm-desktop-app-erc--server-here)))
    `((:label ,(lispm-desktop-tr "Connect…") :action lispm-desktop-app-erc-connect)
      (:label ,(lispm-desktop-tr "Connect Again") :action lispm-desktop-app-erc-reconnect
       :enabled ,(and lispm-desktop-app-erc-networks (not network) t))
      (:label ,(lispm-desktop-tr "Disconnect") :action lispm-desktop-app-erc-disconnect :enabled ,(and network t))
      (:label ,(lispm-desktop-tr "Forget Network…") :action lispm-desktop-app-erc-forget :enabled ,(and here t))
      separator
      (:label ,(lispm-desktop-tr "Join Channel…") :action lispm-desktop-app-erc-join
       :enabled ,(and (lispm-desktop-app-erc--live-servers) t))
      (:label ,(lispm-desktop-tr "Open") :action lispm-desktop-app-erc-open
       :enabled ,(and (lispm-desktop-app-chat-conversation) t)))))

(lispm-desktop-app-ui-define 'lispm-desktop-app-erc-list-mode
  :app 'erc
  :run #'lispm-desktop-app-erc
  :title "ERC"
  :list t
  :status #'lispm-desktop-app-erc--list-status
  :buttons `((,(lispm-desktop-tr-noop "Connect…") lispm-desktop-app-erc-connect)
             (,(lispm-desktop-tr-noop "Join…") lispm-desktop-app-erc-join
              ,(lambda () (lispm-desktop-app-erc--live-servers)))
             (,(lispm-desktop-tr-noop "Open") lispm-desktop-app-erc-open ,#'lispm-desktop-app-chat-conversation)
             (,(lispm-desktop-tr-noop "Disconnect") lispm-desktop-app-erc-disconnect
              ,#'lispm-desktop-app-erc--on-network-p))
  :menu (cons "IRC" #'lispm-desktop-app-erc--list-menu)
  :empty `(,(lambda () (null (lispm-desktop-app-erc--servers)))
           lispm-desktop-app-erc--list-empty-text
           lispm-desktop-app-erc--list-empty-buttons)
  :keys `(("Return" . ,(lispm-desktop-tr-noop "Open the conversation"))
          ("n  p" . ,(lispm-desktop-tr-noop "Next and previous conversation"))
          ("c" . ,(lispm-desktop-tr-noop "Connect to a network")) ("j" . ,(lispm-desktop-tr-noop "Join a channel"))
          ("d" . ,(lispm-desktop-tr-noop "Disconnect")) ("r" . ,(lispm-desktop-tr-noop "Connect again"))
          ("D" . ,(lispm-desktop-tr-noop "Forget the network"))
          ("g" . ,(lispm-desktop-tr-noop "Draw the list again"))))

;;;; A conversation

(defun lispm-desktop-app-erc--buttons ()
  "Return the buttons over an ERC buffer: Back, then Join… and what leaves."
  (let ((server (lispm-desktop-app-erc--server-p))
        (live (erc-server-process-alive)))
    `((,(lispm-desktop-tr "Back") lispm-desktop-app-erc-list)
      (,(lispm-desktop-tr "Join…") lispm-desktop-app-erc-join ,(lambda () live))
      ,(cond ((and server live) (list (lispm-desktop-tr "Disconnect") 'lispm-desktop-app-erc-disconnect))
             (server (list (lispm-desktop-tr "Connect Again") 'lispm-desktop-app-erc-reconnect))
             ((erc-query-buffer-p) (list (lispm-desktop-tr "Close") 'lispm-desktop-app-erc-leave))
             (t (list (lispm-desktop-tr "Leave") 'lispm-desktop-app-erc-leave))))))

(defun lispm-desktop-app-erc--topic ()
  "Return the current channel's topic, as the strip says it, or nil."
  (when-let* ((topic (bound-and-true-p erc-channel-topic))
              ((not (string-empty-p topic))))
    (string-trim (replace-regexp-in-string
                  "[\x00-\x1f]" "" (if (fboundp 'erc-controls-strip) (erc-controls-strip topic) topic)))))

(defun lispm-desktop-app-erc--status ()
  "Return what the bottom of an ERC buffer says: its network and your
nickname there, how many are in its channel, and the conversations with
messages unread."
  (delq nil (list (lispm-desktop-app-erc-network-name)
                  (and (erc-server-process-alive) (erc-current-nick))
                  (when (and (bound-and-true-p erc-channel-users) (erc-default-target)
                             (erc-channel-p (erc-default-target)))
                    (lispm-desktop-tr-n "%d person" "%d people" (hash-table-count erc-channel-users)))
                  (unless (erc-server-process-alive) (lispm-desktop-tr "Disconnected"))
                  (lispm-desktop-app-chat-unread-status (lispm-desktop-app-erc--conversations)))))

(defun lispm-desktop-app-erc--setup-conversation ()
  "Title the window by the conversation it shows: the channel, or the
network for a network's own buffer."
  (setq-local lispm-desktop-platinum-window-title
              (if (lispm-desktop-app-erc--server-p) (lispm-desktop-app-erc-network-name) (buffer-name))))

(lispm-desktop-app-ui-define 'erc-mode
  :app 'erc
  :run #'lispm-desktop-app-erc
  :buttons #'lispm-desktop-app-erc--buttons
  :label #'lispm-desktop-app-erc--topic
  :status #'lispm-desktop-app-erc--status
  :setup #'lispm-desktop-app-erc--setup-conversation
  :keys `(("Return" . ,(lispm-desktop-tr-noop "Send what is typed"))
          ("Tab" . ,(lispm-desktop-tr-noop "Complete a nickname or a command"))
          ("M-p  M-n" . ,(lispm-desktop-tr-noop "What was typed before, and after"))
          ("/join #channel" . ,(lispm-desktop-tr-noop "Join a channel"))
          ("/query nick" . ,(lispm-desktop-tr-noop "Talk with one person"))
          ("/me does" . ,(lispm-desktop-tr-noop "Say what you do"))
          ("/part" . ,(lispm-desktop-tr-noop "Leave the channel"))
          ("/quit" . ,(lispm-desktop-tr-noop "Disconnect"))))

(provide 'lispm-desktop-app-erc)
;;; lispm-desktop-app-erc.el ends here
