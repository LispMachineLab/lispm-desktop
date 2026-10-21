;;; lispm-desktop-app-chat.el --- The App Store's chat applications  -*- lexical-binding: t; -*-

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

;; What the App Store's applications to chat with share on the desktop:
;; ERC on IRC (lispm-desktop-app-erc.el), Slack (lispm-desktop-app-slack.el)
;; and Telega on Telegram (lispm-desktop-app-telega.el).  Each opens on the
;; list of its conversations, as chat applications do, and a conversation
;; opened takes its window, Back in its strip going back to the list:
;;
;;   list   the networks or workspaces it is on, each over its
;;          conversations, channels, groups and people, one a line in
;;          the list face, the line point is on highlighted; those with
;;          messages unread in bold, how many on their right, in red
;;          where they name you; Return or a double click opens one
;;   kept   up to date while it is shown, as messages come, point staying
;;          on its conversation
;;
;; Telega lists its chats itself, in its root buffer; ERC and Slack list
;; none, and this list is theirs: a mode derived from
;; `lispm-desktop-app-chat-list-mode' gives it its conversations, setting
;; `lispm-desktop-app-chat-conversations-function'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-app-ui)

(defface lispm-desktop-app-chat-heading '((t :weight bold))
  "Face of a network's or a workspace's line in a list of conversations."
  :group 'lispm-desktop)

(defface lispm-desktop-app-chat-unread '((t :weight bold))
  "Face of a conversation with messages unread."
  :group 'lispm-desktop)

(defface lispm-desktop-app-chat-state '((t :inherit shadow))
  "Face of what a line says after its name, as \"connecting\"."
  :group 'lispm-desktop)

(defface lispm-desktop-app-chat-mentions
  '((t :foreground "white" :background "#cc0000" :weight bold :box (:line-width (4 . 1) :color "#cc0000")))
  "Face of the count of the messages naming you, as a badge."
  :group 'lispm-desktop)

(defvar-local lispm-desktop-app-chat-conversations-function nil
  "Function returning the conversations the current list shows, in order.
Each is a plist:

:key       what tells it from the others, compared with `equal'
:name      its name, a string
:heading   non-nil for a network's or a workspace's line, over its
           conversations
:state     what its line says after its name, as \"connecting\", or nil
:unread    how many messages it has unread, 0 for none, t for some
:mentions  how many of those name you, or t when some do and how many
           is not known, or nil
:open      a function showing it in the selected window, or nil.")

(defvar-local lispm-desktop-app-chat--shown 'none
  "The conversations the list shows, as drawn last, their :open left out.")

(defun lispm-desktop-app-chat--drawn (conversations)
  "Return CONVERSATIONS as the list compares them: without their :open."
  (mapcar (lambda (conversation)
            (cl-loop for (key value) on conversation by #'cddr
                     unless (eq key :open) append (list key value)))
          conversations))

(defun lispm-desktop-app-chat-unread-p (conversation)
  "Return non-nil when CONVERSATION has messages unread."
  (let ((unread (plist-get conversation :unread)))
    (or (eq unread t) (and (numberp unread) (> unread 0)))))

(defun lispm-desktop-app-chat--count (conversation)
  "Return what CONVERSATION's line says on its right: how many messages are
unread, in red where some name you, or nil."
  (let* ((unread (plist-get conversation :unread))
         (unread (and (numberp unread) (> unread 0) unread))
         (mentions (plist-get conversation :mentions))
         (mentions (if (numberp mentions) (and (> mentions 0) mentions) (and mentions unread))))
    (cond (mentions
           (propertize (number-to-string mentions) 'face 'lispm-desktop-app-chat-mentions))
          (unread
           (propertize (number-to-string unread) 'face 'lispm-desktop-app-chat-unread)))))

(defun lispm-desktop-app-chat-line (conversation)
  "Return CONVERSATION's line of the list, without its newline."
  (let* ((heading (plist-get conversation :heading))
         (state (plist-get conversation :state))
         (count (lispm-desktop-app-chat--count conversation))
         (name (propertize (plist-get conversation :name)
                           'face (cond (heading 'lispm-desktop-app-chat-heading)
                                       ((lispm-desktop-app-chat-unread-p conversation)
                                        'lispm-desktop-app-chat-unread)))))
    (concat (if heading " " "    ") name
            (and state (concat "  " (propertize state 'face 'lispm-desktop-app-chat-state)))
            (and count (concat (propertize " " 'display `(space :align-to (- right (,(lispm-desktop-app-chat--right count)))))
                               count)))))

(defun lispm-desktop-app-chat--right (count)
  "Return how many pixels from the window's right edge COUNT, a line's
count of messages unread, starts: its width, measured in the list's
face, and a margin."
  (+ 16 (string-pixel-width (propertize count 'face (list (get-text-property 0 'face count)
                                                          'lispm-desktop-platinum-list)))))

(defun lispm-desktop-app-chat-conversation ()
  "Return the conversation of the line point is on, or nil."
  (get-text-property (line-beginning-position) 'lispm-desktop-app-chat-conversation))

(defun lispm-desktop-app-chat-conversations ()
  "Return the conversations of the current list, or nil."
  (and lispm-desktop-app-chat-conversations-function
       (condition-case err
           (funcall lispm-desktop-app-chat-conversations-function)
         (error (message "%s" (error-message-string err)) nil))))

(defun lispm-desktop-app-chat-refresh (&optional force)
  "Draw the current list again where its conversations changed, or with
FORCE, point staying on the conversation it was on.  A buffer that is no
list is left alone."
  (interactive (list t))
  (let ((conversations (and (derived-mode-p 'lispm-desktop-app-chat-list-mode)
                            (lispm-desktop-app-chat-conversations))))
    (when (and (derived-mode-p 'lispm-desktop-app-chat-list-mode)
               (or force (not (equal (lispm-desktop-app-chat--drawn conversations) lispm-desktop-app-chat--shown))))
      (let* ((windows (get-buffer-window-list nil nil t))
             (key (save-excursion
                    ;; A window's point, where the list is shown: the buffer's
                    ;; own is where it was left in another window.
                    (when windows (goto-char (window-point (car windows))))
                    (plist-get (lispm-desktop-app-chat-conversation) :key)))
             (starts (mapcar (lambda (window) (cons window (count-lines (point-min) (window-start window))))
                             windows))
             (inhibit-read-only t)
             (at nil))
        (erase-buffer)
        (dolist (conversation conversations)
          (when (and key (equal key (plist-get conversation :key)))
            (setq at (point)))
          (insert (propertize (lispm-desktop-app-chat-line conversation)
                              'lispm-desktop-app-chat-conversation conversation)
                  "\n"))
        (setq lispm-desktop-app-chat--shown (lispm-desktop-app-chat--drawn conversations))
        (goto-char (or at (point-min)))
        (pcase-dolist (`(,window . ,lines) starts)
          (set-window-start window (save-excursion (goto-char (point-min)) (forward-line lines) (point)) t)
          (set-window-point window (point)))))))

(defun lispm-desktop-app-chat-refresh-all ()
  "Draw again the lists of conversations shown, where they changed."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'lispm-desktop-app-chat-list-mode)
                 (get-buffer-window buffer t))
        (lispm-desktop-app-chat-refresh)))))

(defvar lispm-desktop-app-chat--timer nil
  "The timer drawing the lists shown again, as messages come.")

(defconst lispm-desktop-app-chat-refresh-interval 2
  "Seconds between looks at whether the lists shown changed: Slack says
nothing when a message comes, and ERC not of every change.")

(defun lispm-desktop-app-chat--tick ()
  "Draw again the lists of conversations shown, where they changed; stop
looking once no list is left."
  (if (seq-some (lambda (buffer)
                  (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                           'lispm-desktop-app-chat-list-mode))
                (buffer-list))
      (lispm-desktop-app-chat-refresh-all)
    (cancel-timer lispm-desktop-app-chat--timer)
    (setq lispm-desktop-app-chat--timer nil)))

(defun lispm-desktop-app-chat-open (&optional event)
  "Open the conversation of the line point is on, or of the one clicked,
EVENT, in the selected window."
  (interactive (list last-nonmenu-event))
  (when (and (consp event) (posn-point (event-end event)))
    (select-window (posn-window (event-end event)))
    (goto-char (posn-point (event-end event))))
  (let ((open (plist-get (lispm-desktop-app-chat-conversation) :open)))
    (if open (funcall open) (user-error "Nothing to open here"))))

(defun lispm-desktop-app-chat-next (&optional count)
  "Go to the next conversation, or COUNT conversations on."
  (interactive "p")
  (forward-line (or count 1))
  (when (eobp) (forward-line -1)))

(defun lispm-desktop-app-chat-previous (&optional count)
  "Go to the previous conversation, or COUNT conversations back."
  (interactive "p")
  (forward-line (- (or count 1))))

(defvar-keymap lispm-desktop-app-chat-list-mode-map
  :doc "Keymap of a list of conversations."
  :parent special-mode-map
  "RET" #'lispm-desktop-app-chat-open
  "<double-mouse-1>" #'lispm-desktop-app-chat-open
  "n" #'lispm-desktop-app-chat-next
  "p" #'lispm-desktop-app-chat-previous
  "g" #'lispm-desktop-app-chat-refresh)

(define-derived-mode lispm-desktop-app-chat-list-mode special-mode "Conversations"
  "The conversations of a chat application, under the networks they are on.
A mode derived from this one sets
`lispm-desktop-app-chat-conversations-function'."
  (setq-local truncate-lines t
              revert-buffer-function (lambda (&rest _) (lispm-desktop-app-chat-refresh t)))
  (unless lispm-desktop-app-chat--timer
    (setq lispm-desktop-app-chat--timer
          (run-with-timer lispm-desktop-app-chat-refresh-interval lispm-desktop-app-chat-refresh-interval
                          #'lispm-desktop-app-chat--tick))))

(defun lispm-desktop-app-chat-show-list (name mode)
  "Show the list of conversations of buffer NAME, in MODE, in the selected
window, up to date."
  (switch-to-buffer (get-buffer-create name))
  (unless (eq major-mode mode)
    (funcall mode))
  (lispm-desktop-app-chat-refresh))

(defun lispm-desktop-app-chat-unread-status (conversations)
  "Return what a window's bottom says of CONVERSATIONS' messages unread,
or nil when none are."
  (let ((unread (seq-count #'lispm-desktop-app-chat-unread-p
                           (seq-remove (lambda (conversation) (plist-get conversation :heading))
                                       conversations))))
    (when (> unread 0)
      (format "%d unread" unread))))

(defun lispm-desktop-app-chat-set-options (options)
  "Set OPTIONS of a chat application as its window on the desktop wants
them, each only while it is as its package has it: one you set is left as
you set it.  OPTIONS is a list of (OPTION VALUE SHIPPED...), SHIPPED the
values OPTION has as the package ships it beside its standard value, as
where a `defvar' of another of its files comes first."
  (pcase-dolist (`(,option ,value . ,shipped) options)
    (when (and (boundp option)
               (not (or (get option 'saved-value) (get option 'customized-value)))
               (member (default-value option)
                       (cons (eval (car (get option 'standard-value)) t) shipped)))
      (set-default option value))))

(defun lispm-desktop-app-chat-show-in (window buffer)
  "Show BUFFER in WINDOW, when WINDOW is still an application's window:
a conversation asked for there, which comes once the network answers."
  (when (and (window-live-p window) (buffer-live-p buffer) (lispm-desktop-app-ui-app window))
    (set-window-buffer window buffer)
    (when (eq (frame-selected-window (window-frame window)) window)
      (with-current-buffer buffer
        (goto-char (point-max))))))

(provide 'lispm-desktop-app-chat)
;;; lispm-desktop-app-chat.el ends here
