;;; lispm-desktop-app-ui.el --- The applications' interface  -*- lexical-binding: t; -*-

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

;; The App Store's applications are Emacs packages, written for Emacs's
;; own windows: a buffer named between stars, a mode line, keys learnt
;; from C-h m, questions in the echo area.  In their windows on the
;; desktop each of them gets the desktop's interface instead, the same
;; for all:
;;
;;   title      the application's name, as the App Store lists it, and
;;              its icon
;;   buttons    a strip under the title bar holding push buttons for its
;;              main commands: New Game, Pause, Update
;;   status     what it has to report, on placards along the window's
;;              bottom: the score, the date, what is playing
;;   menus      its own menus in the menu bar, or one listing its
;;              commands with their keys, and a Help menu opening the
;;              window of its keys
;;   questions  asked in alerts, as Mac OS 9's applications asked theirs
;;   window     no line numbers, fringes nor wrapped lines; a game's
;;              window takes the size of its board; the arrows it binds
;;              also on C-f, C-b, C-n and C-p, as the HHKB has none
;;   needs      the programs, libraries and applications it uses beyond
;;              Emacs, as its catalog declares them: one missing, an
;;              alert offers to install it as the application opens, and
;;              its Help menu does, see lispm-desktop-needs.el
;;
;; All of it is found out from the application itself, from its mode's
;; keymap, menus and variables, so an application nobody described gets
;; it all the same.  What cannot be found out is said with
;; `lispm-desktop-app-ui-define': the buttons and status an application
;; would rather have, its board drawn as an image
;; (lispm-desktop-app-board.el), a page shown while it has nothing to
;; show.  Emacs's games are described in lispm-desktop-app-games.el, the
;; App Store's other applications in lispm-desktop-app-skins.el; a
;; package of the App Store may describe itself the same way.
;;
;; What an application says of itself here, its title, buttons, status,
;; menus, keys and pages, is in English, translated as it is shown, see
;; lispm-desktop-i18n.el: words of its own that no catalog holds show as
;; they are.
;;
;; `lispm-desktop-app-ui-audit' opens every application installed in
;; turn and reports what of this it finds missing.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-popup)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-app-window)
(require 'lispm-desktop-app-board)
(require 'lispm-desktop-needs)
(require 'lispm-desktop-i18n)

(defvar evil-emacs-state-cursor)
(defvar evil-normal-state-cursor)

;;;; What is said of an application

(defvar lispm-desktop-app-ui-skins nil
  "What is said of applications beyond what is found out: (KEY . PROPERTIES).
See `lispm-desktop-app-ui-define'.")

(defun lispm-desktop-app-ui-define (key &rest properties)
  "Describe the application of KEY with PROPERTIES; return KEY.
KEY is a major mode: its buffers, and those of the modes derived from
it, take PROPERTIES in an application's window.  It is an application's
key instead for an application whose buffer has no mode of its own, as
Pong's.  Any property may be left out; what it says is then found out
from the application.  Its words, the title, the buttons' LABELs, the
strip's label, the menus' TITLEs and items, the keys' WHATs, the status
and the page's TEXT and buttons, are in English, translated as they are
shown; a function may return them translated already:

:app      the key of the application, or a list of them, the buffers
          belong to, for :run.
:run      a function opening the application in its window, selected,
          instead of its command: for a game that would hold Emacs
          until it ends, or that sizes its board from its window.
:title    the window's title, by default the application's name.
:status   a function returning what the window's bottom says: a string,
          a list of strings each on a placard, or nil; by default the
          score, the moves and the process state it keeps.
:buttons  the push buttons under the title bar: a list of (LABEL
          COMMAND ENABLED), LABEL a string or a function returning one,
          ENABLED a function, or left out; or a function returning such
          a list; by default its commands starting, pausing, undoing or
          updating, see `lispm-desktop-app-ui-button-commands'.
:label    a function returning what the strip of buttons says after
          them, or nil.
:menu     (TITLE . ITEMS) its menu when its mode has none in the menu
          bar, ITEMS a function returning items; by default one listing
          its commands, under its name, or Game for a game.
:menus    menus of its own beside those of its mode, ((TITLE . ITEMS)
          ...), ITEMS a function returning items, as a Library menu
          of things to add.
:keys     the keys its keys window lists, ((KEYS . WHAT) ...), as
          strings; by default its commands' keys.
:board    its buffer drawn as an image, see
          `lispm-desktop-app-board-enable'.
:fit      non-nil when the window takes the size of what it shows, as a
          game's of its board; t for games by default.
:prose    non-nil when its text is read as prose: in the proportional
          face, wrapped at words, with margins.
:list     non-nil when it lists things, one a line: in the list face,
          the line point is on highlighted.
:cursor   nil hides the cursor.
:bottom   nil leaves the window without its bottom, where another
          window of the application's has it.
:empty    (PREDICATE TEXT BUTTONS): while PREDICATE returns non-nil in
          its buffer, the window shows TEXT and BUTTONS, a list of
          (LABEL COMMAND), instead, as an application with nothing
          to show yet; TEXT and BUTTONS may be functions returning
          them.
:setup    a function called in its buffer once it has the interface."
  (setf (alist-get key lispm-desktop-app-ui-skins) properties)
  key)

(defvar-local lispm-desktop-app-ui--app nil
  "The application whose window shows this buffer, once it has its interface.")

(defvar lispm-desktop-app-ui--buffers nil
  "The buffer each application's window showed last: (APP . BUFFER).")

(defun lispm-desktop-app-ui-app (&optional window)
  "Return the key of the installed application whose window WINDOW, by
default the selected one, belongs to, or nil."
  (frame-parameter (window-frame window) 'lispm-desktop-app-window))

(defun lispm-desktop-app-ui--buffer-app (&optional buffer)
  "Return the application BUFFER, by default the current one, belongs to."
  (let ((buffer (or buffer (current-buffer))))
    (or (buffer-local-value 'lispm-desktop-app-ui--app buffer)
        (seq-some #'lispm-desktop-app-ui-app (get-buffer-window-list buffer 'never t))
        (and (eq (window-buffer) buffer) (lispm-desktop-app-ui-app)))))

(defun lispm-desktop-app-ui-skin (&optional buffer)
  "Return what is said of BUFFER's application, see `lispm-desktop-app-ui-define'."
  (with-current-buffer (or buffer (current-buffer))
    (or (cdr (seq-find (lambda (skin) (provided-mode-derived-p major-mode (car skin)))
                       lispm-desktop-app-ui-skins))
        (when-let* ((app (lispm-desktop-app-ui--buffer-app)))
          (alist-get app lispm-desktop-app-ui-skins)))))

(defun lispm-desktop-app-ui--safely (function &rest arguments)
  "Return FUNCTION's value for ARGUMENTS, or nil when it signals: an
application's status and buttons are read while it is drawn, before it
may have set itself up."
  (condition-case nil (apply function arguments) (error nil)))

(defun lispm-desktop-app-ui-get (property &optional default)
  "Return PROPERTY said of the current buffer's application, or DEFAULT."
  (let ((skin (lispm-desktop-app-ui-skin)))
    (if (plist-member skin property) (plist-get skin property) default)))

(defun lispm-desktop-app-ui--app-skin (app)
  "Return what is said of application APP itself: its own skin, or one
naming it with :app."
  (or (alist-get app lispm-desktop-app-ui-skins)
      (cdr (seq-find (lambda (skin) (memq app (ensure-list (plist-get (cdr skin) :app))))
                     lispm-desktop-app-ui-skins))))

(defun lispm-desktop-app-ui--app-property (app property)
  "Return PROPERTY of APP as the desktop knows it, see `lispm-desktop-apps'."
  (plist-get (alist-get app lispm-desktop-apps) property))

(defun lispm-desktop-app-ui-name (&optional app)
  "Return the name of APP, by default the current buffer's application, in
the language LispM speaks."
  (let ((app (or app (lispm-desktop-app-ui--buffer-app))))
    (if-let* ((name (lispm-desktop-app-ui--app-property app :name)))
        (lispm-desktop-tr name)
      (and app (capitalize (symbol-name app))))))

(defun lispm-desktop-app-ui-game-p (&optional app)
  "Return non-nil when APP, by default the current buffer's application,
is a game, as the App Store files it."
  (eq (lispm-desktop-app-ui--app-property (or app (lispm-desktop-app-ui--buffer-app)) :category)
      'games))

;;;; Commands found out

(defconst lispm-desktop-app-ui-unlisted-commands
  '(undefined ignore self-insert-command digit-argument negative-argument universal-argument
    describe-mode revert-buffer scroll-up-command scroll-down-command beginning-of-buffer
    end-of-buffer quit-window bury-buffer kill-current-buffer kill-this-buffer keyboard-quit
    execute-extended-command newline undo)
  "Emacs's own commands an application's keymap holds: its window's menus
and keys leave them out.")

(defconst lispm-desktop-app-ui--unlisted-events
  '(menu-bar tool-bar tab-bar header-line mode-line tab-line remap vertical-line
    vertical-scroll-bar horizontal-scroll-bar left-fringe right-fringe left-margin right-margin
    compose-last-chars)
  "Events of a keymap no key sends.")

(defun lispm-desktop-app-ui--key-event-p (event)
  "Return non-nil when EVENT is sent by a key, not the mouse nor the menu bar."
  (and (not (consp event))
       (let ((base (event-basic-type event)))
         (or (integerp base)
             (and (symbolp base)
                  (not (memq base lispm-desktop-app-ui--unlisted-events))
                  (not (string-match-p "mouse\\|wheel\\|drag\\|touch\\|pinch" (symbol-name base))))))))

(defun lispm-desktop-app-ui--prefixes (&optional app)
  "Return the prefixes naming the current buffer's application in its
commands' names, the longest first: its mode's, as tetris- in
`tetris-start-game', and APP's own, as pong- for Pong."
  (let* ((parts (split-string (string-remove-suffix "-mode" (symbol-name major-mode)) "-" t))
         (prefixes (cl-loop for i from (length parts) downto 1
                            collect (concat (string-join (seq-take parts i) "-") "-"))))
    (if-let* ((app (or app (lispm-desktop-app-ui--buffer-app))))
        (append prefixes (list (concat (symbol-name app) "-")))
      prefixes)))

(defvar lispm-desktop-app-ui--definitions (make-hash-table :test #'eq)
  "What the file of each major mode looked at defines: MODE to DEFINITIONS.")

(defun lispm-desktop-app-ui--mode-definitions ()
  "Return what the file defining the current major mode defines, as
`load-history' keeps it, or nil; looked up once a mode."
  (with-memoization (gethash major-mode lispm-desktop-app-ui--definitions)
    (when-let* ((file (symbol-file major-mode 'defun)))
      (cdr (assoc file load-history)))))

(defun lispm-desktop-app-ui--bare-name (command &optional prefixes definitions)
  "Return COMMAND's name without the prefix naming its application, or nil
when nothing names it as the current buffer's application's: no prefix of
PREFIXES, by default `lispm-desktop-app-ui--prefixes', and not defined
with the major mode, in DEFINITIONS, by default
`lispm-desktop-app-ui--mode-definitions', its first word then its prefix,
as bb- in Blackbox's `bb-romp'."
  (let ((name (symbol-name command)))
    (or (seq-some (lambda (prefix)
                    (and (string-prefix-p prefix name) (> (length name) (length prefix))
                         (substring name (length prefix))))
                  (or prefixes (lispm-desktop-app-ui--prefixes)))
        (and (member (cons 'defun command) (or definitions (lispm-desktop-app-ui--mode-definitions)))
             (string-match "\\`[^-]+-\\(.+\\)" name)
             (match-string 1 name)))))

(defvar lispm-desktop-app-ui--commands (make-hash-table :test #'eq :weakness 'key)
  "The commands found in keymaps: KEYMAP to (MODE APP . COMMANDS).
A keymap is looked through once: the strip of buttons asks at every
redisplay, and a keymap as Calendar's binds commands of many files.")

(defun lispm-desktop-app-ui-commands (&optional map)
  "Return the application's own commands MAP binds, by default the current
buffer's keymap, in its order: a list of (COMMAND KEYS...), each KEYS a key
sequence, those typed on the keyboard alone first.  Commands named for
another package are left out, as Emacs's own of `special-mode-map', and so
are those bound only to the mouse, the menu bar or a remapping; a binding
its parent keymap's shadows is not counted."
  (when-let* ((map (or map (current-local-map))))
    (let* ((app (lispm-desktop-app-ui--buffer-app))
           (cached (gethash map lispm-desktop-app-ui--commands)))
      (if (and cached (eq (car cached) major-mode) (eq (cadr cached) app))
          (cddr cached)
        (let ((commands (lispm-desktop-app-ui--find-commands map)))
          (puthash map (cons major-mode (cons app commands)) lispm-desktop-app-ui--commands)
          commands)))))

(defun lispm-desktop-app-ui--find-commands (map)
  "Look through MAP for its application's commands, see
`lispm-desktop-app-ui-commands'."
  (let ((seen (make-hash-table :test #'equal))
        (prefixes (lispm-desktop-app-ui--prefixes))
        (definitions (lispm-desktop-app-ui--mode-definitions))
        (found nil))
    (cl-labels ((walk (map prefix)
                  (map-keymap
                   (lambda (event binding)
                     (let ((keys (vconcat prefix (vector event))))
                       (when (and (not (gethash keys seen)) (lispm-desktop-app-ui--key-event-p event))
                         (puthash keys t seen)
                         (cond ((and (keymapp binding) (not (commandp binding)) (< (length prefix) 2))
                                (walk (if (symbolp binding) (indirect-function binding) binding) keys))
                               ((and (symbolp binding) (commandp binding)
                                     (not (memq binding lispm-desktop-app-ui-unlisted-commands))
                                     (lispm-desktop-app-ui--bare-name binding prefixes definitions))
                                (if-let* ((entry (assq binding found)))
                                    (setcdr entry (append (cdr entry) (list keys)))
                                  (push (list binding keys) found)))))))
                   map)))
      (walk map []))
    (mapcar (pcase-lambda (`(,command . ,keys))
              (cons command (append (seq-filter #'lispm-desktop-popup-keyboard-p keys)
                                    (seq-remove #'lispm-desktop-popup-keyboard-p keys))))
            (nreverse found))))

(defconst lispm-desktop-app-ui--small-words
  '("a" "an" "and" "as" "at" "by" "for" "from" "in" "of" "on" "or" "the" "to" "with")
  "Words a menu's label leaves in lower case after its first.")

(defconst lispm-desktop-app-ui--acronyms '("url" "opml" "rss" "html" "id" "mpv" "pdf")
  "Words a menu's label writes in capitals.")

(defun lispm-desktop-app-ui-label (words)
  "Return WORDS, a command's name or what is left of it, as a menu says
them: `start-game' as Start Game, `browse-url' as Browse URL."
  (string-join
   (seq-map-indexed (lambda (word index)
                      (cond ((member word lispm-desktop-app-ui--acronyms) (upcase word))
                            ((and (> index 0) (member word lispm-desktop-app-ui--small-words)) word)
                            (t (concat (upcase (substring word 0 1)) (substring word 1)))))
                    (split-string words "-+" t))
   " "))

(defun lispm-desktop-app-ui-command-label (command)
  "Return the words a menu says COMMAND in: its name without its
application's prefix, `tetris-start-game' as Start Game."
  (lispm-desktop-app-ui-label (or (lispm-desktop-app-ui--bare-name command) (symbol-name command))))

(defconst lispm-desktop-app-ui-button-commands
  `((,(lispm-desktop-tr-noop "New Game") . "\\`\\(?:start\\|new\\|restart\\)\\(?:-game\\)?\\'")
    (,(lispm-desktop-tr-noop "Pause") . "\\`\\(?:pause\\|toggle-pause\\)\\(?:-game\\)?\\'")
    (,(lispm-desktop-tr-noop "Undo") . "\\`\\(?:undo\\|undo-move\\|takes?-back\\|human-takes-back\\)\\'")
    (,(lispm-desktop-tr-noop "Hint") . "\\`\\(?:hint\\|suggest\\|solve-suggest\\)\\'")
    (,(lispm-desktop-tr-noop "Update") . "\\`\\(?:update\\|fetch\\|refresh\\)\\'"))
  "The push buttons an application's window holds when nothing says which:
\(LABEL . REGEXP), for the first of its commands whose name, its
application's prefix taken off, REGEXP matches.  LABEL is in English,
translated where shown.")

(defun lispm-desktop-app-ui--found-buttons ()
  "Return the push buttons found among the current buffer's commands."
  (let* ((prefixes (lispm-desktop-app-ui--prefixes))
         (definitions (lispm-desktop-app-ui--mode-definitions))
         (names (mapcar (pcase-lambda (`(,command . ,_))
                          (cons command (lispm-desktop-app-ui--bare-name command prefixes definitions)))
                        (lispm-desktop-app-ui-commands))))
    (delq nil (mapcar (pcase-lambda (`(,label . ,regexp))
                        (when-let* ((command (car (seq-find (lambda (name) (string-match-p regexp (cdr name)))
                                                            names))))
                          (list label command)))
                      lispm-desktop-app-ui-button-commands))))

(defun lispm-desktop-app-ui-buttons ()
  "Return the push buttons of the current buffer's window, as (LABEL
COMMAND ENABLED), each LABEL a string in the language LispM speaks and
ENABLED non-nil or nil."
  (let ((buttons (lispm-desktop-app-ui-get :buttons 'found)))
    (mapcar (pcase-lambda (`(,label ,command ,enabled))
              (list (if-let* ((label (if (functionp label) (lispm-desktop-app-ui--safely label) label)))
                        (lispm-desktop-tr label)
                      "…")
                    command
                    (or (null enabled) (and (lispm-desktop-app-ui--safely enabled) t))))
            (cond ((eq buttons 'found) (lispm-desktop-app-ui--found-buttons))
                  ((functionp buttons) (lispm-desktop-app-ui--safely buttons))
                  (t buttons)))))

(defun lispm-desktop-app-ui--new-command ()
  "Return the current buffer's command starting anew, or nil."
  (nth 1 (seq-find (lambda (button) (equal (car button) "New Game"))
                   (lispm-desktop-app-ui--found-buttons))))

;;;; Running commands in an application's window

(defun lispm-desktop-app-ui-call (window action)
  "Run ACTION, a command or a function, in WINDOW, selected, as typed there."
  (when (window-live-p window)
    (select-window window)
    (if (commandp action) (call-interactively action) (funcall action))))

(defun lispm-desktop-app-ui-window (app)
  "Return the window application APP shows its buffer in, or nil."
  (when-let* ((frame (lispm-desktop-app-window-frame app)))
    (frame-selected-window frame)))

;;;; Status

(defun lispm-desktop-app-ui--number (number)
  "Return NUMBER written with commas between thousands."
  (let ((text (number-to-string number)))
    (while (string-match "\\([0-9]\\)\\([0-9]\\{3\\}\\)\\(\\'\\|,\\)" text)
      (setq text (replace-match "\\1,\\2\\3" t nil text)))
    text))

(defconst lispm-desktop-app-ui-status-variables
  `((,(lispm-desktop-tr-noop "Score %s") . "score") (,(lispm-desktop-tr-noop "Moves %s") . "moves"))
  "What an application's status says when nothing says what: (FORMAT .
NAME), for each of its variables PREFIX-NAME holding a number, FORMAT
saying the number in English, translated where shown.")

(defun lispm-desktop-app-ui--found-status ()
  "Return the status found in the current buffer: the numbers its
application keeps, see `lispm-desktop-app-ui-status-variables', and its
process state."
  (let ((prefixes (lispm-desktop-app-ui--prefixes)))
    (delq nil
          `(,@(mapcar (pcase-lambda (`(,label . ,name))
                        (seq-some (lambda (prefix)
                                    (let ((variable (intern-soft (concat prefix name))))
                                      (and variable (boundp variable) (numberp (symbol-value variable))
                                           (lispm-desktop-tr label (lispm-desktop-app-ui--number
                                                                    (symbol-value variable))))))
                                  prefixes))
                      lispm-desktop-app-ui-status-variables)
            ,(let ((process (string-trim (format-mode-line mode-line-process))))
               (unless (string-empty-p process)
                 (string-trim process "[: ]+")))))))

(defun lispm-desktop-app-ui-status ()
  "Return what the bottom of the current buffer's window says: a list of
strings, see :status, in the language LispM speaks."
  (let ((status (lispm-desktop-app-ui-get :status 'found)))
    (if (eq status 'found)
        (seq-remove (lambda (text) (or (null text) (string-empty-p text)))
                    (lispm-desktop-app-ui--safely #'lispm-desktop-app-ui--found-status))
      (mapcar #'lispm-desktop-tr
              (seq-remove (lambda (text) (or (null text) (string-empty-p text)))
                          (ensure-list (if (functionp status) (lispm-desktop-app-ui--safely status) status)))))))

;;;; The strip of buttons

(defconst lispm-desktop-app-ui--target 'lispm-desktop-app-ui-button
  "What the strip's push buttons are, held down, with their place: see
`lispm-desktop-platinum-press'.")

(defun lispm-desktop-app-ui--press (event)
  "Hold the strip's button EVENT pressed, running its command if let go on it."
  (interactive "e")
  (let* ((start (event-start event))
         (string (posn-string start))
         (command (and string (get-text-property (cdr string) 'lispm-desktop-app-ui-command (car string))))
         (window (posn-window start)))
    (when command
      (lispm-desktop-platinum-press start (lispm-desktop-platinum-same-target-p start)
                                    (lambda () (lispm-desktop-app-ui-call window command))))))

(defvar-keymap lispm-desktop-app-ui--button-map
  :doc "Keymap of a push button in an application's strip."
  "<header-line> <down-mouse-1>" #'lispm-desktop-app-ui--press
  "<header-line> <mouse-1>" #'ignore)

(defun lispm-desktop-app-ui--button (label command enabled index)
  "Return the strip's INDEXth button, reading LABEL, running COMMAND,
dimmed unless ENABLED."
  (let ((target (list lispm-desktop-app-ui--target index)))
    (if (not enabled)
        (lispm-desktop-platinum-header-button label 'disabled)
      (propertize (lispm-desktop-platinum-header-button
                   label (and (lispm-desktop-platinum-pressed-p target) 'pressed))
                  'help-echo (let ((keys (where-is-internal command nil t)))
                               (if keys (lispm-desktop-tr "%s (%s)" label (key-description keys)) label))
                  'lispm-desktop-platinum-target target
                  'lispm-desktop-app-ui-command command
                  'keymap lispm-desktop-app-ui--button-map))))

(defun lispm-desktop-app-ui--label ()
  "Return what the strip says after its buttons, in the language LispM
speaks, or nil."
  (when-let* ((label (lispm-desktop-app-ui-get :label))
              (text (lispm-desktop-app-ui--safely label)))
    (lispm-desktop-tr text)))

(defun lispm-desktop-app-ui-strip ()
  "Return the strip under the window's title bar: its push buttons, then
what the application says there, as Finder windows' strips."
  (let ((left 0) (index 0))
    (concat
     (mapconcat (pcase-lambda (`(,label ,command ,enabled))
                  (setq left (+ left (lispm-desktop-platinum-header-button-width label)))
                  (lispm-desktop-app-ui--button label command enabled (cl-incf index)))
                (lispm-desktop-app-ui-buttons) "")
     (lispm-desktop-platinum-header-strip (lispm-desktop-app-ui--label) left))))

(defun lispm-desktop-app-ui--strip-p ()
  "Return non-nil when the current buffer's window has a strip of buttons."
  (or (lispm-desktop-app-ui-buttons) (lispm-desktop-app-ui-get :label)))

;;;; Menus

(defun lispm-desktop-app-ui--key (action window)
  "Return the key ACTION, a command, is typed with in WINDOW, or nil."
  (when (and (symbolp action) (window-live-p window))
    (with-current-buffer (window-buffer window)
      (when-let* ((keys (seq-find #'lispm-desktop-popup-keyboard-p
                                  (where-is-internal action (list (current-local-map))))))
        (key-description keys)))))

(defun lispm-desktop-app-ui--in-window (window items &optional translate)
  "Return menu ITEMS acting in WINDOW: their commands run there, with the
keys typing them there shown, and so do their submenus'.  TRANSLATE
non-nil translates their labels, in English, as a skin's menus are."
  (mapcar (lambda (item)
            (if (not (consp item))
                item
              (let ((action (plist-get item :action))
                    (submenu (plist-get item :submenu))
                    (label (plist-get item :label))
                    (item (copy-sequence item)))
                (when (and translate (stringp label))
                  (setq item (plist-put item :label (lispm-desktop-tr label))))
                (when action
                  (unless (plist-get item :key)
                    (setq item (plist-put item :key (lispm-desktop-app-ui--key action window))))
                  (setq item (plist-put item :action (lambda () (lispm-desktop-app-ui-call window action)))))
                (when (functionp submenu)
                  (setq item (plist-put item :submenu
                                        (lambda ()
                                          (lispm-desktop-app-ui--in-window
                                           window (with-selected-window window (funcall submenu))
                                           translate)))))
                item)))
          items))

(defun lispm-desktop-app-ui--own-menu (app window)
  "Return the menu of APP's window when its mode has none in the menu bar:
:menu, translated, or one listing its commands, the strip's first, under
its name, or Game for a game."
  (with-current-buffer (window-buffer window)
    (pcase-let* ((own (lispm-desktop-app-ui-get :menu))
                 (`(,title . ,items)
                  (if own
                      (cons (lispm-desktop-tr (car own)) (cdr own))
                    (let* ((commands (lispm-desktop-app-ui-commands))
                           (buttons (delq nil (mapcar (lambda (button) (assq (nth 1 button) commands))
                                                      (lispm-desktop-app-ui-buttons))))
                           (others (seq-take (seq-difference commands buttons) 24))
                           (item (pcase-lambda (`(,command ,keys . ,_))
                                   (list :label (lispm-desktop-app-ui-command-label command) :action command
                                         :key (key-description keys)))))
                      (cons (if (lispm-desktop-app-ui-game-p app)
                                (lispm-desktop-tr "Game")
                              (lispm-desktop-app-ui-name app))
                            (lambda ()
                              `(,@(mapcar item buttons) ,@(and buttons others '(separator))
                                ,@(mapcar item others))))))))
      (when items
        (list (list (intern (format "lispm-desktop-app-ui-%s-own" app)) title
                    (lambda () (lispm-desktop-app-ui--in-window
                                window (with-selected-window window (funcall items)) (and own t)))))))))

(defun lispm-desktop-app-ui--mode-menus (app window)
  "Return the menus of APP's window: its mode's own menus of the menu bar,
or else one of its commands, see `lispm-desktop-app-ui--own-menu'."
  (with-current-buffer (window-buffer window)
    (let ((bar (and (current-local-map) (keymap-lookup (current-local-map) "<menu-bar>"))))
      (or (and (keymapp bar)
               (delq nil (mapcar (lambda (item)
                                   (when-let* (((consp item))
                                               (submenu (plist-get item :submenu)))
                                     (list (intern (format "lispm-desktop-app-ui-%s-%s" app (plist-get item :label)))
                                           (plist-get item :label)
                                           (lambda ()
                                             (lispm-desktop-app-ui--in-window
                                              window (with-selected-window window (funcall submenu)))))))
                                 (with-selected-window window (lispm-desktop-popup-keymap-items bar)))))
          (lispm-desktop-app-ui--own-menu app window)))))

(defun lispm-desktop-app-ui--file-items (app window)
  "Return the items of APP's File menu: New Game, where it starts anew,
and Close Window."
  `(,@(when-let* (((window-live-p window))
                  (new (with-current-buffer (window-buffer window) (lispm-desktop-app-ui--new-command))))
        `((:label ,(if (lispm-desktop-app-ui-game-p app) (lispm-desktop-tr "New Game") (lispm-desktop-tr "New"))
           :action ,(lambda () (lispm-desktop-app-ui-call window new))
           :key ,(lispm-desktop-app-ui--key new window))
          separator))
    ,@(lispm-desktop-app-window-file-items app)))

(defun lispm-desktop-app-ui--help-items (app)
  "Return the items of APP's Help menu: its keys, what it is, and
installing what it uses beyond Emacs that is missing."
  (let ((name (lispm-desktop-app-ui-name app)))
    `((:label ,(lispm-desktop-tr "%s Keys" name) :action ,(lambda () (lispm-desktop-app-ui-show-keys app)))
      (:label ,(lispm-desktop-tr "About %s" name) :action ,(lambda () (lispm-desktop-app-ui-about app)))
      ,@(when-let* ((missing (lispm-desktop-needs-missing (lispm-desktop-app-ui-needs app) t)))
          (cons 'separator
                (mapcar (lambda (need)
                          (list :label (lispm-desktop-tr "Install %s…" (lispm-desktop-needs-name need))
                                :action (lambda () (lispm-desktop-needs-ask (list need) name))))
                        missing))))))

(defun lispm-desktop-app-ui--skin-menus (app window)
  "Return the menus APP's skin adds, see :menus, acting in WINDOW, their
titles and items translated."
  (with-current-buffer (window-buffer window)
    (mapcar (pcase-lambda (`(,title . ,items))
              (list (intern (format "lispm-desktop-app-ui-%s-%s" app title)) (lispm-desktop-tr title)
                    (lambda () (lispm-desktop-app-ui--in-window
                                window (with-selected-window window (funcall items)) t))))
            (lispm-desktop-app-ui-get :menus))))

(defun lispm-desktop-app-ui-menus (app)
  "Return the menus of installed application APP for the menu bar: File,
its own and Help, each acting in its window.
This is the :menus of an application the App Store installed."
  (let ((window (lispm-desktop-app-ui-window app)))
    `((,(intern (format "lispm-desktop-app-ui-%s-file" app)) ,(lispm-desktop-tr "File")
       ,(lambda () (lispm-desktop-app-ui--file-items app window)))
      ,@(when (window-live-p window)
          (append (lispm-desktop-app-ui--mode-menus app window)
                  (lispm-desktop-app-ui--skin-menus app window)))
      (,(intern (format "lispm-desktop-app-ui-%s-help" app)) ,(lispm-desktop-tr "Help")
       ,(lambda () (lispm-desktop-app-ui--help-items app))))))

(defun lispm-desktop-app-ui-about (app)
  "Say what application APP is, in an alert, as Mac OS 9's About boxes."
  (lispm-desktop-popup-alert 'note (lispm-desktop-app-ui-name app) `((,(lispm-desktop-tr "OK") ok default))
                             (when-let* ((description (lispm-desktop-app-ui--app-property app :description)))
                               (lispm-desktop-tr description))))

;;;; The window of keys

(defconst lispm-desktop-app-ui--key-names
  '(("<left>" . "←") ("<right>" . "→") ("<up>" . "↑") ("<down>" . "↓") ("SPC" . "Space")
    ("RET" . "Return") ("DEL" . "Delete") ("TAB" . "Tab") ("ESC" . "Esc")
    ("<return>" . "Return") ("<tab>" . "Tab") ("<backspace>" . "Delete"))
  "Keys as a key cap names them.")

(defun lispm-desktop-app-ui-key-name (keys)
  "Return KEYS, a key sequence or its description, as its key caps read."
  (mapconcat (lambda (key) (or (cdr (assoc key lispm-desktop-app-ui--key-names)) key))
             (split-string (if (stringp keys) keys (key-description keys)) " ")
             " "))

(defun lispm-desktop-app-ui-key-list ()
  "Return the keys of the current buffer's application: a list of (KEYS
. WHAT), KEYS as its key caps read, from :keys, both translated (KEYS
may say a word, as \"any key\"), or its commands."
  (or (mapcar (pcase-lambda (`(,keys . ,what)) (cons (lispm-desktop-tr keys) (lispm-desktop-tr what)))
              (lispm-desktop-app-ui-get :keys))
      (mapcar (pcase-lambda (`(,command . ,keys))
                (cons (mapconcat #'lispm-desktop-app-ui-key-name
                                 (seq-take (seq-uniq (cons (car keys)
                                                           (seq-remove #'lispm-desktop-popup-keyboard-p keys)))
                                           2)
                                 "  ")
                      (lispm-desktop-app-ui-command-label command)))
              (lispm-desktop-app-ui-commands))))

(define-derived-mode lispm-desktop-app-ui-keys-mode lispm-desktop-app-window-mode "Keys"
  "Major mode of an application's keys window: each key and what it does,
the whole list in sight, without a bottom saying where in it point is."
  (face-remap-add-relative 'default 'lispm-desktop-platinum-list)
  (setq-local line-spacing 4
              mode-line-format nil))

(defun lispm-desktop-app-ui--insert-keys (keys)
  "Insert KEYS, a value of `lispm-desktop-app-ui-key-list', as rows of key
caps and what they do, the caps right-aligned in a column of their own."
  (let* ((caps (mapcar (lambda (entry)
                         (mapconcat (lambda (key) (lispm-desktop-platinum-bevel-button
                                                   key nil 18 'lispm-desktop-platinum-small))
                                    (split-string (car entry) "  ") " "))
                       keys))
         (column (+ 12 (seq-reduce #'max (mapcar #'string-pixel-width caps) 0))))
    (insert (propertize " " 'display '(space :height (8))) "\n")
    (cl-mapc (lambda (cap entry)
               (insert (propertize " " 'display `(space :align-to (,(- column (string-pixel-width cap)))))
                       cap
                       (propertize " " 'display `(space :align-to (,(+ column 10))))
                       (cdr entry) "\n"))
             caps keys)))

(defun lispm-desktop-app-ui-show-keys (app)
  "Show the keys of application APP in a window of their own: each key cap
and what it does, as found out from its keymap."
  (interactive (list (lispm-desktop-apps-frontmost)))
  (let* ((window (or (lispm-desktop-app-ui-window app)
                     (user-error (lispm-desktop-tr "%s is not open") app)))
         (keys (with-current-buffer (window-buffer window) (lispm-desktop-app-ui-key-list)))
         (kind (intern (format "%s-keys" app)))
         (name (lispm-desktop-tr "%s Keys" (lispm-desktop-app-ui-name app)))
         (buffer (get-buffer-create (format " *%s Keys*" (or (lispm-desktop-app-ui--app-property app :name)
                                                               (capitalize (symbol-name app)))))))
    (with-current-buffer buffer
      (lispm-desktop-app-ui-keys-mode)
      (setq-local lispm-desktop-platinum-title
                  (lambda () (lispm-desktop-tr "%s Keys" (lispm-desktop-app-ui-name app))))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if keys
            (lispm-desktop-app-ui--insert-keys keys)
          (insert "\n  " (lispm-desktop-tr "%s is used with the mouse." (lispm-desktop-app-ui-name app)) "\n")))
      (goto-char (point-min)))
    (unless (assq kind lispm-desktop-app-window-kinds)
      (lispm-desktop-app-window-define kind :app app :name name :close #'lispm-desktop-app-window-close-box))
    (let ((frame (lispm-desktop-app-window-show kind :buffer buffer :size '(340 . 200))))
      (lispm-desktop-app-ui--fit-frame (frame-root-window frame) 16)
      frame)))

;;;; Questions

(defun lispm-desktop-app-ui--question (prompt)
  "Return PROMPT as an alert asks it, without its (y or n)."
  (string-trim (replace-regexp-in-string "(\\(?:y or n\\|yes or no\\)) *\\'" ""
                                         (string-trim (substring-no-properties prompt)))))

(defun lispm-desktop-app-ui-ask (prompt)
  "Ask PROMPT in an alert with Yes and No; return non-nil for Yes.
The alert is over the window asking, or over the screen while that
window is not shown yet, as when a game asks before its first board."
  (let ((frame (selected-frame)))
    (with-selected-frame (if (frame-visible-p frame) frame (lispm-desktop-apps-screen))
      (lispm-desktop-popup-alert 'caution (lispm-desktop-app-ui--question prompt)
                                 `((,(lispm-desktop-tr "Yes") t default ?y ?Y)
                                   (,(lispm-desktop-tr "No") nil cancel ?n ?N))))))

(defun lispm-desktop-app-ui--asking (ask prompt &rest arguments)
  "Ask PROMPT in an alert when an application's window asks it, else as ASK
does with ARGUMENTS.  This is the advice of `y-or-n-p' and `yes-or-no-p'."
  (if (and (lispm-desktop-app-ui-app) (stringp prompt) (not executing-kbd-macro)
           (lispm-desktop-popup-available-p))
      (lispm-desktop-app-ui-ask prompt)
    (apply ask prompt arguments)))

;;;; Fitting a window to what it shows

(defun lispm-desktop-app-ui-fit-p ()
  "Return non-nil when the current buffer's window takes the size of what
it shows: as :fit says, by default for a game, but for one read as prose."
  (lispm-desktop-app-ui-get :fit (and (lispm-desktop-app-ui-game-p) (not (lispm-desktop-app-ui-get :prose)))))

(defun lispm-desktop-app-ui--fit-frame (window &optional padding)
  "Size WINDOW's floating window to what WINDOW shows, with its strip and
bottom, PADDING pixels more each way.  A window holding several, or
rolled up, is left."
  (let ((frame (window-frame window))
        (padding (or padding 0)))
    (when (and (window-live-p window)
               (not (lispm-desktop-app-window-rolled-p frame))
               (null (cdr (window-list frame 'never))))
      (pcase-let ((`(,width . ,height) (window-text-pixel-size window nil nil 4000 4000 t))
                  (board (lispm-desktop-app-board-size (window-buffer window))))
        ;; A board is centred in its window, which the text's width counts.
        (let ((width (+ (if board (car board) width) padding
                        (- (window-pixel-width window) (window-body-width window t))))
              (height (+ height 2 padding)))
          (unless (and (= width (frame-text-width frame)) (= height (frame-text-height frame)))
            (if (lispm-desktop-tty-p frame)
                (set-frame-size frame (max 25 width) (max 6 height) t)
              (set-frame-size frame (max 200 width) (max 100 height) t))
            (lispm-desktop-platinum-frame-update frame)
            (lispm-desktop-apps-keep-on-desktop frame)))))))

(defun lispm-desktop-app-ui-fit (&optional window)
  "Size WINDOW's floating window, by default the selected one's, to what
WINDOW shows, with its strip and bottom, as Mac OS 9 sized a game's
window to its board; WINDOW is an application's."
  (let ((window (or window (selected-window))))
    (when (lispm-desktop-app-ui-app window)
      (lispm-desktop-app-ui--fit-frame window))))

(defun lispm-desktop-app-ui--fit-board (buffer)
  "Fit the windows showing BUFFER's board, drawn at another size, when
their application's windows fit what they show.
This is one of `lispm-desktop-app-board-resized-functions'."
  (dolist (window (get-buffer-window-list buffer 'never t))
    (when (and (lispm-desktop-app-ui-app window)
               (with-current-buffer buffer (lispm-desktop-app-ui-fit-p)))
      (run-at-time 0 nil #'lispm-desktop-app-ui-fit window))))

;;;; Pages while there is nothing to show

(defvar-local lispm-desktop-app-ui--page nil
  "The overlay showing the current buffer's page while it has nothing to show.")

(defun lispm-desktop-app-ui--page-button (label command)
  "Return a push button reading LABEL that runs COMMAND, for a page."
  (let ((map (make-sparse-keymap))
        (run (lambda (event)
               (interactive "e")
               (lispm-desktop-app-ui-call (posn-window (event-start event)) command))))
    (keymap-set map "<down-mouse-1>" #'ignore)
    (keymap-set map "<mouse-1>" run)
    (propertize (lispm-desktop-platinum-push-button label) 'keymap map 'mouse-face nil
                'help-echo (let ((keys (where-is-internal command nil t)))
                             (if keys (lispm-desktop-tr "%s (%s)" label (key-description keys)) label)))))

(defun lispm-desktop-app-ui--page (text buttons)
  "Return the page saying TEXT over BUTTONS, centred in the window; TEXT
and the buttons' labels are in English, translated here, TEXT's lines
broken where it says."
  (let ((centre (lambda (string)
                  (concat (propertize " " 'display
                                      `(space :align-to (- center (0.5 . (,(string-pixel-width string))))))
                          string "\n"))))
    (concat (propertize " " 'display '(space :height (60))) "\n"
            (mapconcat (lambda (line) (funcall centre (propertize line 'face 'lispm-desktop-platinum-list)))
                       (split-string (lispm-desktop-tr text) "\n") "")
            (propertize " " 'display '(space :height (16))) "\n"
            (funcall centre (mapconcat (pcase-lambda (`(,label ,command))
                                         (lispm-desktop-app-ui--page-button (lispm-desktop-tr label) command))
                                       buttons
                                       (propertize " " 'display '(space :width (12))))))))

(defun lispm-desktop-app-ui--refresh-page ()
  "Show the current buffer's page while its application has nothing to
show, see :empty, and its text again once it has."
  (when-let* ((empty (lispm-desktop-app-ui-get :empty)))
    (pcase-let ((`(,predicate ,text ,buttons) empty))
      (if (ignore-errors (funcall predicate))
          (let ((page (lispm-desktop-app-ui--page (if (functionp text) (funcall text) text)
                                                  (if (functionp buttons) (funcall buttons) buttons))))
            (unless (and lispm-desktop-app-ui--page (overlay-buffer lispm-desktop-app-ui--page))
              (setq lispm-desktop-app-ui--page (make-overlay (point-min) (point-max) nil nil t))
              (overlay-put lispm-desktop-app-ui--page 'invisible t))
            (unless (equal-including-properties page (overlay-get lispm-desktop-app-ui--page 'before-string))
              (overlay-put lispm-desktop-app-ui--page 'before-string page))
            (move-overlay lispm-desktop-app-ui--page (point-min) (point-max)))
        (when lispm-desktop-app-ui--page
          (delete-overlay lispm-desktop-app-ui--page)
          (setq lispm-desktop-app-ui--page nil))))))

(defun lispm-desktop-app-ui--page-changed (&rest _)
  "Show the page again or take it away once the buffer's change is done:
an application also writes its buffer from timers and processes, as
EMMS its library once read.  This is an `after-change-functions' function."
  (let ((buffer (current-buffer)))
    (run-at-time 0 nil (lambda ()
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (lispm-desktop-app-ui--refresh-page)))))))

;;;; The window's bottom

(defun lispm-desktop-app-ui--bottom-right ()
  "Return the right of an application's window's bottom, its grow box,
aligned to the right edge."
  (let ((right (lispm-desktop-platinum-mode-line-right)))
    (concat (propertize " " 'display `(space :align-to (- right (,(string-pixel-width right)))))
            right)))

(defconst lispm-desktop-app-ui-bottom
  '((:eval (lispm-desktop-platinum-mode-line-left))
    (:eval (lispm-desktop-app-ui--bottom-right)))
  "The bottom of an application's window: its placards on the left, its
grow box on the right.  Aligned without `mode-line-format-right-align',
which reads the buffer's own mode line, as Calendar's, a string.")

;;;; Giving buffers the interface

(defconst lispm-desktop-app-ui--letters
  '(("<left>" . "C-b") ("<right>" . "C-f") ("<up>" . "C-p") ("<down>" . "C-n"))
  "The arrows, and the letters doing their work on the HHKB.")

(defconst lispm-desktop-app-ui--emacs-motions
  '(forward-char backward-char next-line previous-line right-char left-char)
  "Emacs's own commands of the arrows and their letters.")

(defun lispm-desktop-app-ui--letters-map ()
  "Return a keymap giving C-b, C-f, C-p and C-n the commands the current
buffer's application gives the arrows, where it gives none to the letters."
  (let ((map (make-sparse-keymap))
        (local (current-local-map)))
    (when local
      (pcase-dolist (`(,arrow . ,letter) lispm-desktop-app-ui--letters)
        (let ((command (keymap-lookup local arrow))
              (own (keymap-lookup local letter)))
          (when (and (commandp command) (symbolp command)
                     (not (memq command lispm-desktop-app-ui--emacs-motions))
                     (or (null own) (memq own lispm-desktop-app-ui--emacs-motions)))
            (keymap-set map letter command)))))
    map))

(define-minor-mode lispm-desktop-app-ui-mode
  "The interface of an application's buffer in its window on the desktop.
See lispm-desktop-app-ui.el.  It is turned on in each buffer an
installed application's window shows."
  :lighter nil
  (when lispm-desktop-app-ui-mode
    (lispm-desktop-app-ui--set-up-buffer)))

(defun lispm-desktop-app-ui--set-up-buffer ()
  "Give the current buffer its application's interface."
  (when (bound-and-true-p display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (setq-local lispm-desktop-platinum-title (let ((title (lispm-desktop-app-ui-get :title)))
                                             (lambda ()
                                               (if title (lispm-desktop-tr title) (lispm-desktop-app-ui-name))))
              lispm-desktop-platinum-icon (when-let* ((icon (lispm-desktop-app-ui--app-property
                                                             lispm-desktop-app-ui--app :icon)))
                                            (lispm-desktop-platinum-icon icon))
              lispm-desktop-platinum-info #'lispm-desktop-app-ui-status
              show-trailing-whitespace nil
              indicate-empty-lines nil
              minor-mode-overriding-map-alist
              (cons (cons 'lispm-desktop-app-ui-mode (lispm-desktop-app-ui--letters-map))
                    (assq-delete-all 'lispm-desktop-app-ui-mode
                                     (copy-sequence minor-mode-overriding-map-alist))))
  (cond ((lispm-desktop-app-ui-get :prose)
         (visual-line-mode 1)
         (face-remap-add-relative 'default 'variable-pitch)
         ;; Lines of Chinese and Japanese break between characters.
         (setq-local line-spacing 0.3
                     word-wrap-by-category t))
        (t
         (visual-line-mode -1)
         (setq-local truncate-lines t)))
  (when (lispm-desktop-app-ui-get :list)
    (face-remap-add-relative 'default 'lispm-desktop-platinum-list)
    (setq-local line-spacing 4)
    (hl-line-mode 1))
  (unless (lispm-desktop-app-ui-get :cursor t)
    (setq-local cursor-type nil
                evil-emacs-state-cursor '(nil)
                evil-normal-state-cursor '(nil)))
  (when-let* ((board (lispm-desktop-app-ui-get :board)))
    (lispm-desktop-app-board-enable board))
  (when (lispm-desktop-app-ui-get :empty)
    (add-hook 'post-command-hook #'lispm-desktop-app-ui--refresh-page nil t)
    (add-hook 'after-change-functions #'lispm-desktop-app-ui--page-changed nil t)
    (lispm-desktop-app-ui--refresh-page))
  (when-let* ((setup (lispm-desktop-app-ui-get :setup)))
    (funcall setup)))

(defun lispm-desktop-app-ui-dress (window)
  "Give WINDOW, of an installed application's window, the application's
interface: its buffer, and the window its strip and bottom, kept even
where its mode sets its own mode line, as Calendar's does."
  (when-let* ((app (lispm-desktop-app-ui-app window))
              ((not (window-minibuffer-p window)))
              ((not (eq (window-buffer window) (lispm-desktop-app-window-blank)))))
    (with-current-buffer (window-buffer window)
      (unless (and lispm-desktop-app-ui-mode (eq lispm-desktop-app-ui--app app))
        (setq lispm-desktop-app-ui--app app)
        (lispm-desktop-app-ui-mode 1))
      (when (eq window (frame-selected-window (window-frame window)))
        (setf (alist-get app lispm-desktop-app-ui--buffers) (current-buffer)))
      ;; The global mode numbering lines turns it on again with every
      ;; major mode, after this buffer's set up.
      (when (bound-and-true-p display-line-numbers-mode)
        (display-line-numbers-mode -1))
      (set-window-fringes window 0 0)
      (set-window-parameter window 'mode-line-format
                            (if (lispm-desktop-app-ui-get :bottom t) lispm-desktop-app-ui-bottom 'none))
      (set-window-parameter window 'header-line-format
                            (and (lispm-desktop-app-ui--strip-p) '(:eval (lispm-desktop-app-ui-strip))))
      (when (lispm-desktop-app-ui-get :prose)
        (set-window-margins window 2 2))
      (when (and (lispm-desktop-app-ui-fit-p)
                 (not (frame-parameter (window-frame window) 'lispm-desktop-app-ui-fitted)))
        (set-frame-parameter (window-frame window) 'lispm-desktop-app-ui-fitted t)
        (run-at-time 0 nil #'lispm-desktop-app-ui-fit window)))))

(defun lispm-desktop-app-ui--dress-frame (frame)
  "Give the windows of FRAME, when an installed application's window, its
interface.  This is one of `window-buffer-change-functions'."
  (when (and (frame-live-p frame) (frame-parameter frame 'lispm-desktop-app-window))
    (dolist (window (window-list frame 'never))
      (lispm-desktop-app-ui-dress window))))

(defun lispm-desktop-app-ui--mode-changed ()
  "Give the current buffer its interface again in the applications' windows
showing it, its mode having changed and taken it away.
This is one of `after-change-major-mode-hook'."
  (dolist (window (get-buffer-window-list nil 'never t))
    (when (lispm-desktop-app-ui-app window)
      (lispm-desktop-app-ui-dress window))))

(defun lispm-desktop-app-ui--refresh-pages ()
  "Show the pages of the applications' buffers again, in the language now
spoken.  This is one of `lispm-desktop-language-changed-hook'."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when lispm-desktop-app-ui-mode
        (lispm-desktop-app-ui--refresh-page)))))

;;;; What an application needs
;;
;; An application may use programs, libraries and applications beyond
;; Emacs, as EMMS a player: its catalog declares them as its needs (see
;; lispm-desktop-needs.el), and the App Store passes them on as its
;; :needs.  One missing whose stand-in is installed, as macOS's afplay
;; for mpv, is offered from the Help menu; one missing with nothing
;; standing in is offered as the application opens, once a session.

(defun lispm-desktop-app-ui-needs (app)
  "Return what APP needs beyond Emacs.  APP's :needs may be a function
returning them, as the App Store's are, read from its catalog."
  (let ((needs (lispm-desktop-app-ui--app-property app :needs)))
    (if (functionp needs) (funcall needs) needs)))

(defvar lispm-desktop-app-ui--told nil
  "The applications offered this session what they need and lack.")

(defun lispm-desktop-app-ui--offer-missing (app)
  "Offer once a session to install what APP needs and lacks, with nothing
standing in for it."
  (unless (memq app lispm-desktop-app-ui--told)
    (push app lispm-desktop-app-ui--told)
    (lispm-desktop-needs-ensure (lispm-desktop-app-ui-needs app) (lispm-desktop-app-ui-name app))))

;;;; Opening an application

(defun lispm-desktop-app-ui-run (app command)
  "Open application APP in its window, selected: on the buffer it showed
when its window was closed, if still there, as a Mac OS 9 application
kept its documents while its windows were closed; else as its :run
says, or by COMMAND, as typed.  A game is so found as it was left,
rather than asked whether to abandon it.  What it needs and lacks is
offered first."
  (lispm-desktop-needs-refresh (lispm-desktop-app-ui-needs app))
  (unless noninteractive
    (lispm-desktop-app-ui--offer-missing app))
  (let ((buffer (alist-get app lispm-desktop-app-ui--buffers)))
    (cond ((buffer-live-p buffer) (switch-to-buffer buffer))
          ((plist-get (lispm-desktop-app-ui--app-skin app) :run)
           (funcall (plist-get (lispm-desktop-app-ui--app-skin app) :run)))
          (t (call-interactively command)))))

;;;; Auditing the applications

(defun lispm-desktop-app-ui--audit-window (window)
  "Return what WINDOW, an application's window, lacks of its interface, as
a list of strings."
  (with-current-buffer (window-buffer window)
    (let ((frame (window-frame window)))
      (delq nil
            (list (unless lispm-desktop-app-ui-mode "no interface")
                  (when (string-match-p "\\`[ *]" (lispm-desktop-platinum-buffer-title (current-buffer)))
                    "titled by its buffer")
                  (when (bound-and-true-p display-line-numbers-mode) "line numbers")
                  (when (and (lispm-desktop-app-ui-game-p) (null (lispm-desktop-app-ui-buttons)))
                    "no buttons")
                  (when (null (lispm-desktop-app-ui--mode-menus lispm-desktop-app-ui--app window))
                    "no menu")
                  (when (null (lispm-desktop-app-ui-key-list)) "no keys")
                  (pcase-let* ((`(,width . ,height) (window-text-pixel-size window nil nil 4000 4000))
                               ;; A board is centred, which the text's width counts.
                               (width (or (car (lispm-desktop-app-board-size)) width)))
                    (when (and (lispm-desktop-app-ui-fit-p)
                               (or (> width (+ 4 (window-body-width window t)))
                                   (> height (+ 4 (window-body-height window t)))))
                      (format "shows %dx%d in %dx%d" width height
                              (window-body-width window t) (window-body-height window t))))
                  (unless (frame-visible-p frame) "not shown"))))))

(defun lispm-desktop-app-ui-audit (&optional apps)
  "Open each application installed, or of APPS, check what of its interface
it lacks, close it again, and list what was found.  An application found
lacking needs a description, see `lispm-desktop-app-ui-define'."
  (interactive)
  (let ((apps (or apps (mapcar #'car (seq-filter (lambda (app) (lispm-desktop-app-ui--app-property
                                                                  (car app) :store))
                                                 lispm-desktop-apps))))
        (report nil))
    (dolist (app apps)
      (let ((opened (not (lispm-desktop-app-window-open-p app))))
        (condition-case err
            (progn
              (lispm-desktop-apps-activate app)
              ;; Boards are drawn, and windows fitted, by timers.
              (dotimes (_ 3) (redisplay t) (sit-for 0.5))
              (let ((window (lispm-desktop-app-ui-window app)))
                (push (cons app (if window (lispm-desktop-app-ui--audit-window window) '("no window")))
                      report)))
          (error (push (list app (error-message-string err)) report)))
        (when opened
          (lispm-desktop-app-window-close app))))
    (with-current-buffer (get-buffer-create "*Application Audit*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (pcase-dolist (`(,app . ,lacks) (nreverse report))
          (insert (format "%-12s %s\n" app (if lacks (string-join lacks ", ") "ok"))))
        (special-mode))
      (display-buffer (current-buffer)))
    report))

;;;; Scroll bars

(defun lispm-desktop-app-ui--scroll-bar (buffer)
  "Say that BUFFER's window has no scroll bar when it shows a board or a
game's grid, which its window holds whole.
This is one of `lispm-desktop-platinum-scroll-bar-functions'."
  (with-current-buffer buffer
    (when (and lispm-desktop-app-ui-mode
               (or lispm-desktop-app-board (lispm-desktop-app-ui-fit-p)))
      'no)))

;;;; Set up

(defun lispm-desktop-setup-app-ui ()
  "Give the installed applications' windows the desktop's interface."
  (add-hook 'window-buffer-change-functions #'lispm-desktop-app-ui--dress-frame)
  ;; After the globalized minor modes, as `global-display-line-numbers-mode',
  ;; have turned themselves on in the buffer.
  (add-hook 'after-change-major-mode-hook #'lispm-desktop-app-ui--mode-changed 90)
  (add-hook 'lispm-desktop-app-board-resized-functions #'lispm-desktop-app-ui--fit-board)
  (add-hook 'lispm-desktop-platinum-scroll-bar-functions #'lispm-desktop-app-ui--scroll-bar)
  (add-hook 'lispm-desktop-language-changed-hook #'lispm-desktop-app-ui--refresh-pages)
  (advice-add 'y-or-n-p :around #'lispm-desktop-app-ui--asking)
  (advice-add 'yes-or-no-p :around #'lispm-desktop-app-ui--asking))

(provide 'lispm-desktop-app-ui)
;;; lispm-desktop-app-ui.el ends here
