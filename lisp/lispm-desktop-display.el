;;; lispm-desktop-display.el --- LispM drawing the windows of applications of their own  -*- lexical-binding: t; -*-

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

;; An X server draws the windows of its clients, each a process of its
;; own; LispM draws the windows of its applications opened on their own
;; so, the roles turned round: each application is a process of the
;; computer's own, app.m of native/lispm-desktop-display, so it has its
;; own tile in the Dock, its own menu bar and its own place in the
;; application switcher, and shows in its window what LispM draws for it.
;;
;;   - LispM listens on a socket of its own,
;;     `lispm-desktop-display-socket'; an application's process connects,
;;     starting LispM out of sight first when it does not listen, and asks
;;     for its application;
;;   - LispM opens the application's window out on the computer's screen,
;;     the desktop out of sight, and exports it, see display.m beside
;;     app.m: the window and those over it, menus, alerts, are shown again
;;     in the process's window, while LispM's own stay where they are,
;;     transparent;
;;   - what is typed and clicked in the process's window comes to LispM as
;;     Emacs's own events, in the window under the mouse or with the
;;     keyboard; text composed with an input method comes once composed,
;;     the process showing it meanwhile at LispM's cursor;
;;   - the process's menu bar holds the application's menus, as the
;;     desktop's menu bar would; its window moves as the computer's do,
;;     dragged by LispM's title bar, and LispM's follows;
;;   - closing the application's window quits its process; quitting the
;;     process closes the window; Show LispM's Desktop in its menu brings
;;     the desktop back, and the window with it.
;;
;; They speak in lines of JSON, see app.m.  Both are built here from their
;; sources with the command line tools' clang, which LispM.app's installer
;; installs first, and kept in `lispm-desktop-display-directory'.
;;
;; Where it works: macOS, an Emacs built for its window system, and Core
;; Animation's contexts, which are not public API.  Elsewhere nothing is
;; built and applications open as before.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-app-window)
(require 'lispm-desktop-coherence)
(require 'lispm-desktop-i18n)

(declare-function lispm-desktop-display-module-export "lispm-desktop-display-module" (address))
(declare-function lispm-desktop-display-module-unexport "lispm-desktop-display-module" (number))
(declare-function lispm-desktop-display-module-focus "lispm-desktop-display-module" (address on))
(declare-function lispm-desktop-display-module-available-p "lispm-desktop-display-module" ())
(declare-function lispm-desktop-display-module-accessory "lispm-desktop-display-module" (on))
(declare-function lispm-desktop-standalone-apps "lispm-desktop-standalone" ())
(declare-function lispm-desktop-standalone--close "lispm-desktop-standalone" (frame))
(declare-function lispm-desktop-standalone-own-process-p "lispm-desktop-standalone" (app))
(declare-function lispm-desktop-platinum-drag-outline "lispm-desktop-platinum-frames" (frame kind))

(defgroup lispm-desktop-display nil
  "LispM drawing the windows of its applications opened on their own."
  :group 'lispm-desktop)

(defcustom lispm-desktop-display-directory (locate-user-emacs-file "lispm-desktop-display/")
  "Directory the display's module and the applications' executable are
built in, from native/lispm-desktop-display."
  :type 'directory
  :group 'lispm-desktop-display)

(defconst lispm-desktop-display-source-directory
  (expand-file-name "../native/lispm-desktop-display/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory of the display's sources: display.m, the module; app.m, the
applications' executable.")

;;;; Building

(defun lispm-desktop-display--module-header ()
  "Return the directory of this Emacs's emacs-module.h, or nil."
  (seq-find (lambda (directory) (file-readable-p (expand-file-name "emacs-module.h" directory)))
            (list (expand-file-name "../include/" data-directory)
                  (expand-file-name "../../include/" data-directory)
                  (expand-file-name "include/" installation-directory)
                  "/opt/homebrew/include/" "/usr/local/include/")))

(defun lispm-desktop-display--build-plan ()
  "Return how the display's module and the applications' executable are
built here, from their sources and for this Emacs: a plist of :directory,
named by what they are built from, :module, :executable and :commands,
the clang commands building them; or nil where they cannot be built."
  (when-let* (((eq system-type 'darwin))
              ((executable-find "xcrun"))
              (header (lispm-desktop-display--module-header)))
    (let* ((display (expand-file-name "display.m" lispm-desktop-display-source-directory))
           (app (expand-file-name "app.m" lispm-desktop-display-source-directory))
           (stamp (substring (secure-hash 'sha256 (with-temp-buffer
                                                    (insert emacs-version system-configuration)
                                                    (insert-file-contents-literally display)
                                                    (insert-file-contents-literally app)
                                                    (buffer-string)))
                             0 16))
           (directory (file-name-as-directory (expand-file-name stamp lispm-desktop-display-directory)))
           (module (expand-file-name "lispm-desktop-display.dylib" directory))
           (executable (expand-file-name "lispm-app" directory))
           (clang '("xcrun" "clang" "-fobjc-arc" "-O2" "-mmacosx-version-min=11.0"
                    "-framework" "AppKit" "-framework" "QuartzCore")))
      (list :directory directory :module module :executable executable
            :commands (list (append clang (list "-dynamiclib" "-undefined" "dynamic_lookup"
                                                "-I" header "-o" module display))
                            (append clang (list "-o" executable app)))))))

(defun lispm-desktop-display--built-p (plan)
  "Return non-nil when what PLAN builds is built."
  (and plan (file-exists-p (plist-get plan :module))
       (file-executable-p (plist-get plan :executable))))

(defvar lispm-desktop-display--building nil
  "The process building the display, or nil.")

(defvar lispm-desktop-display--when-built nil
  "Functions to call once the display is built.")

(defun lispm-desktop-display-build (&optional then)
  "Build the display's module and the applications' executable in the
background, unless they are built; then call THEN, if given, once they
are.  Builds of sources since changed are deleted.  Return nil where
they cannot be built."
  (when-let* ((plan (lispm-desktop-display--build-plan)))
    (if (lispm-desktop-display--built-p plan)
        (when then (funcall then))
      (when then (add-to-list 'lispm-desktop-display--when-built then t))
      (unless (process-live-p lispm-desktop-display--building)
        (let ((directory (plist-get plan :directory)))
          (make-directory directory t)
          (dolist (old (directory-files lispm-desktop-display-directory t "\\`[0-9a-f]\\{16\\}\\'"))
            (unless (equal (file-name-as-directory old) directory)
              (delete-directory old t)))
          (setq lispm-desktop-display--building
                (make-process
                 :name "lispm-display-build" :buffer (generate-new-buffer " *lispm-desktop-display-build*")
                 :noquery t
                 :command (list "/bin/sh" "-c"
                                (mapconcat (lambda (command) (mapconcat #'shell-quote-argument command " "))
                                           (plist-get plan :commands) " && "))
                 :sentinel
                 (lambda (process _event)
                   (unless (process-live-p process)
                     (let ((output (with-current-buffer (process-buffer process)
                                     (string-trim (buffer-string))))
                           (then lispm-desktop-display--when-built))
                       (kill-buffer (process-buffer process))
                       (setq lispm-desktop-display--when-built nil)
                       (if (and (zerop (process-exit-status process)) (lispm-desktop-display--built-p plan))
                           (mapc #'funcall then)
                         (ignore-errors (delete-directory directory t))
                         (message (lispm-desktop-tr "LispM could not build %s: %s")
                                  "native/lispm-desktop-display" output)))))))))
      t)))

(defun lispm-desktop-display-load ()
  "Load the display's module, once built; return non-nil when LispM can
draw the windows of applications of their own here, now."
  (or (and (featurep 'lispm-desktop-display-module)
           (lispm-desktop-display-module-available-p))
      (when-let* (((fboundp 'module-load))
                  ((eq (framep (selected-frame)) 'ns))
                  (plan (lispm-desktop-display--build-plan))
                  ((lispm-desktop-display--built-p plan)))
        (condition-case err
            (progn
              (module-load (plist-get plan :module))
              (lispm-desktop-display-module-available-p))
          (error (message (lispm-desktop-tr "LispM could not load its display: %s")
                          (error-message-string err))
                 nil)))))

(defun lispm-desktop-display-executable ()
  "Return the applications' executable, once built, or nil."
  (when-let* ((plan (lispm-desktop-display--build-plan))
              ((lispm-desktop-display--built-p plan)))
    (plist-get plan :executable)))

;;;; Frames

(defun lispm-desktop-display--address (frame)
  "Return the address of FRAME's record, as Emacs prints it, or nil."
  (when-let* ((printed (prin1-to-string frame))
              ((string-match " 0x\\([0-9a-f]+\\)>\\'" printed)))
    (string-to-number (match-string 1 printed) 16)))

(defvar lispm-desktop-display--clients nil
  "The applications' processes connected, as their connections.")

(defun lispm-desktop-display--client-of (frame)
  "Return the connection of the process whose window FRAME is, or is over."
  (let ((frame (and (frame-live-p frame) frame)))
    (while (and frame (not (seq-find (lambda (client) (eq (process-get client :outer) frame))
                                     lispm-desktop-display--clients)))
      (setq frame (frame-parent frame)))
    (and frame (seq-find (lambda (client) (eq (process-get client :outer) frame))
                         lispm-desktop-display--clients))))

(defun lispm-desktop-display-exported-p (frame)
  "Return non-nil when FRAME is drawn in an application's process."
  (and (lispm-desktop-display--client-of frame) t))

(defun lispm-desktop-display--group (outer)
  "Return OUTER and the frames shown over it, its children's children too:
the frames drawn in its process's window, the deepest last."
  (let ((group (list outer)) (todo (list outer)))
    (while todo
      (let ((parent (pop todo)))
        (dolist (frame (frame-list))
          (when (and (eq (frame-parent frame) parent) (frame-visible-p frame))
            (setq group (append group (list frame)))
            (push frame todo)))))
    group))

(defun lispm-desktop-display--edges (frame type)
  "Return FRAME's edges of TYPE, as `frame-edges' does, on the computer's
screen: a child frame's count from its parent's inside."
  (pcase-let ((`(,left ,top ,right ,bottom) (frame-edges frame type))
              (parent (frame-parent frame)))
    (if (not parent)
        (list left top right bottom)
      (pcase-let ((`(,x ,y . ,_) (lispm-desktop-display--edges parent 'native-edges)))
        (list (+ left x) (+ top y) (+ right x) (+ bottom y))))))

(defun lispm-desktop-display--geometry (outer)
  "Return where OUTER is on the computer's screen, as the process takes it."
  (pcase-let ((`(,left ,top ,right ,bottom) (lispm-desktop-display--edges outer 'outer-edges)))
    (list :x left :y top :w (- right left) :h (- bottom top))))

;;;; Speaking

(defun lispm-desktop-display--send (client &rest message)
  "Send MESSAGE, a plist, to CLIENT's process as a line of JSON."
  (when (process-live-p client)
    (process-send-string client (concat (json-serialize message) "\n"))))

(defun lispm-desktop-display--filter (client output)
  "Take OUTPUT from CLIENT's process, lines of JSON, each handled.
Whatever may ask or show something runs as an event of its own, from the
command loop: a modal alert run here would wait for the very input this
filter brings."
  (let ((pending (concat (or (process-get client :pending) "") output)))
    (while (string-match "\n" pending)
      (let ((line (substring pending 0 (match-beginning 0))))
        (setq pending (substring pending (match-end 0)))
        (condition-case err
            (lispm-desktop-display--handle
             client (json-parse-string line :object-type 'plist :array-type 'list
                                       :null-object nil :false-object nil))
          (error (message "LispM: an application said %S: %s" line (error-message-string err))))))
    (process-put client :pending pending)))

(defun lispm-desktop-display--enqueue (event)
  "Have EVENT read next, after those already waiting, see
`lispm-desktop-setup-display'."
  (setq unread-command-events (append unread-command-events (list event))))

(defun lispm-desktop-display--handle (client message)
  "Act on MESSAGE from CLIENT's process, see app.m."
  (pcase (plist-get message :op)
    ("open" (process-put client :app (intern (plist-get message :app)))
            (process-put client :pid (plist-get message :pid))
            (lispm-desktop-display--enqueue (list 'lispm-desktop-display-open client)))
    ("key" (lispm-desktop-display--keyboard client)
           (lispm-desktop-display--enqueue (lispm-desktop-display--key message)))
    ("text" (lispm-desktop-display--keyboard client)
            (setq unread-command-events (append unread-command-events
                                                (string-to-list (plist-get message :text)))))
    ("mouse" (lispm-desktop-display--mouse client message))
    ("wheel" (lispm-desktop-display--wheel client message))
    ("focus" (lispm-desktop-display--focus client (plist-get message :on)))
    ("moved" (lispm-desktop-display--moved client message))
    ("menu" (lispm-desktop-display--enqueue
             (list 'lispm-desktop-display-menu client (plist-get message :id))))
    ("quit" (lispm-desktop-display--enqueue (list 'lispm-desktop-display-quit client)))
    ("desktop" (lispm-desktop-display--enqueue (list 'lispm-desktop-display-desktop client)))))

;;;; Opening

(defun lispm-desktop-display--app-frame (app)
  "Return the window APP shows, or nil."
  (lispm-desktop-app-window-of-app app))

(defvar lispm-desktop-standalone--launched)

(defvar lispm-desktop-display--opening nil
  "The connection whose application is being opened, or nil.")

(defun lispm-desktop-display-open (event)
  "Open the application EVENT's process asked for, and draw its window
there, unless it opened over the desktop shown: then the desktop takes
it, and the process leaves."
  (interactive "e")
  (let* ((client (nth 1 event))
         (app (process-get client :app)))
    (cond ((not (memq app (lispm-desktop-standalone-apps)))
           (message (lispm-desktop-tr "LispM has no application %s to open") app)
           (lispm-desktop-display--send client :op "bye"))
          (t
           (let ((lispm-desktop-display--opening client))
             (lispm-desktop-apps-activate app))
           (let ((frame (lispm-desktop-display--app-frame app)))
             (if (and frame (lispm-desktop-apps-out-p frame))
                 (lispm-desktop-display--attach client frame)
               ;; Over the desktop, or a window LispM draws itself.
               (lispm-desktop-display--send client :op "desktop")))))))

(defun lispm-desktop-display--attach (client frame)
  "Draw FRAME, an application's window out, in CLIENT's process."
  (let* ((outer (lispm-desktop-platinum-window-outer frame))
         (context (lispm-desktop-display-module-export (lispm-desktop-display--address outer))))
    (if (not context)
        (lispm-desktop-display--send client :op "desktop")
      (process-put client :frame frame)
      (process-put client :outer outer)
      (process-put client :context context)
      (setq lispm-desktop-standalone--launched
            (assq-delete-all (process-get client :app) lispm-desktop-standalone--launched))
      (let ((geometry (lispm-desktop-display--geometry outer)))
        (process-put client :geometry geometry)
        (apply #'lispm-desktop-display--send client :op "window" :context context
               :title (or (frame-parameter frame 'name) "")
               :labels (lispm-desktop-display--labels (process-get client :app))
               geometry))
      (lispm-desktop-display--send-menus client)
      (run-hooks 'lispm-desktop-display-changed-hook))))

(defvar lispm-desktop-display-changed-hook nil
  "Hook run once a window is drawn in an application's process, or no
longer.")

(defun lispm-desktop-display--detach (client &optional message)
  "Stop drawing CLIENT's window in its process; send it MESSAGE, a value of
\"op\", if given.  LispM's own window shows again, if still there."
  (when-let* ((context (process-get client :context)))
    (process-put client :context nil)
    (lispm-desktop-display-module-unexport context))
  (process-put client :frame nil)
  (process-put client :outer nil)
  (when message
    (lispm-desktop-display--send client :op message))
  (setq lispm-desktop-display--clients (delq client lispm-desktop-display--clients))
  (run-hooks 'lispm-desktop-display-changed-hook))

;;;; The process's own menu

(defun lispm-desktop-display--labels (app)
  "Return the items of the application menu of APP's process, in the
language LispM speaks: About, Show Desktop, Hide and Quit."
  (let ((name (or (lispm-desktop-apps-name app) (symbol-name app))))
    (list :about (lispm-desktop-tr "About %s" name)
          :desktop (lispm-desktop-tr "Show Desktop")
          :hide (lispm-desktop-tr "Hide %s" name)
          :quit (lispm-desktop-tr "Quit %s" name))))

;;;; Input

(defun lispm-desktop-display--keyboard (client)
  "Have the keyboard type into CLIENT's window: select the frame the
application's window shows, unless a frame over it, or the minibuffer
read from, is selected."
  (when-let* ((frame (process-get client :frame))
              ((frame-live-p frame))
              ((not (active-minibuffer-window)))
              ((not (eq (lispm-desktop-display--client-of (selected-frame)) client))))
    (select-frame frame 'norecord)))

(defun lispm-desktop-display--key (message)
  "Return the event a key MESSAGE stands for."
  (let* ((key (plist-get message :key))
         (base (if (= (length key) 1) (string-to-char key) (intern key))))
    (event-convert-list (append (mapcar #'intern (plist-get message :mods)) (list base)))))

(defun lispm-desktop-display--frame-at (client x y)
  "Return the frame of CLIENT's window at X, Y of it, the one in front, and
the point in it: (FRAME X . Y)."
  (pcase-let* ((outer (process-get client :outer))
               (`(,ox ,oy . ,_) (lispm-desktop-display--edges outer 'outer-edges))
               (found nil))
    (dolist (frame (lispm-desktop-display--group outer))
      (pcase-let ((`(,left ,top ,right ,bottom) (lispm-desktop-display--edges frame 'inner-edges)))
        (let ((fx (- (+ ox x) left)) (fy (- (+ oy y) top)))
          (when (and (>= fx 0) (>= fy 0) (< fx (- right left)) (< fy (- bottom top)))
            (setq found (cons frame (cons fx fy)))))))
    (or found
        (pcase-let ((`(,left ,top . ,_) (lispm-desktop-display--edges outer 'inner-edges)))
          (cons outer (cons (- (+ ox x) left) (- (+ oy y) top)))))))

(defun lispm-desktop-display--posn (client x y)
  "Return the position X, Y of CLIENT's window, as a click's."
  (pcase-let ((`(,frame ,fx . ,fy) (lispm-desktop-display--frame-at client x y)))
    (posn-at-x-y (max 0 fx) (max 0 fy) frame)))

(defun lispm-desktop-display--mouse (client message)
  "Turn MESSAGE, the mouse pressed, moved or let go in CLIENT's window, into
Emacs's events: a press, a click or a drag, double and triple too, and
motion while a button is down."
  (let* ((x (plist-get message :x)) (y (plist-get message :y))
         (button (plist-get message :button))
         (mods (mapcar #'intern (plist-get message :mods)))
         (clicks (plist-get message :clicks))
         (repeat (pcase clicks (2 '(double)) (3 '(triple)) (_ nil)))
         (posn (lispm-desktop-display--posn client x y))
         (mouse (intern (format "mouse-%d" button))))
    (pcase (plist-get message :kind)
      ("down"
       (process-put client :down (list button x y posn))
       (lispm-desktop-display--enqueue
        (list (event-convert-list (append mods repeat (list 'down mouse))) posn)))
      ("move"
       (when (process-get client :down)
         (lispm-desktop-display--enqueue (list 'mouse-movement posn))))
      ("up"
       (pcase-let ((`(,_ ,x0 ,y0 ,start) (process-get client :down)))
         (process-put client :down nil)
         (if (and start (> (+ (abs (- x x0)) (abs (- y y0))) double-click-fuzz))
             (lispm-desktop-display--enqueue
              (list (event-convert-list (append mods (list 'drag mouse))) start posn))
           (lispm-desktop-display--enqueue
            (list (event-convert-list (append mods repeat (list mouse))) posn))))))))

(defun lispm-desktop-display--wheel (client message)
  "Turn MESSAGE, the wheel turned in CLIENT's window, into Emacs's events."
  (let* ((lines (plist-get message :lines))
         (posn (lispm-desktop-display--posn client (plist-get message :x) (plist-get message :y)))
         (event (event-convert-list (append (mapcar #'intern (plist-get message :mods))
                                            (list (if (> lines 0) 'wheel-up 'wheel-down))))))
    (dotimes (_ (min 10 (abs lines)))
      (lispm-desktop-display--enqueue (list event posn)))))

(defvar lispm-desktop-display--focused nil
  "The connection whose window has the keyboard, or nil.")

(defun lispm-desktop-display--focus (client on)
  "Give the keyboard to CLIENT's window, ON, or take it away: Emacs is told
so, and draws the cursor there so; the application comes in front."
  (when-let* ((frame (process-get client :frame))
              ((frame-live-p frame)))
    (lispm-desktop-display-module-focus (lispm-desktop-display--address frame) on)
    (if on
        (progn
          (setq lispm-desktop-display--focused client)
          (lispm-desktop-display--keyboard client)
          (when-let* ((app (process-get client :app)))
            (lispm-desktop-apps-set-frontmost app))
          (lispm-desktop-display--send-menus client)
          (lispm-desktop-display--send-cursor client))
      (when (eq lispm-desktop-display--focused client)
        (setq lispm-desktop-display--focused nil)))))

(defun lispm-desktop-display--moved (client message)
  "Move CLIENT's window in LispM where its process's window moved."
  (when-let* ((outer (process-get client :outer))
              ((frame-live-p outer)))
    (let ((geometry (lispm-desktop-display--geometry outer))
          (x (round (plist-get message :x))) (y (round (plist-get message :y))))
      (unless (and (= x (plist-get geometry :x)) (= y (plist-get geometry :y)))
        (process-put client :geometry (plist-put (plist-put geometry :x x) :y y))
        (set-frame-position outer x y)))))

;;;; Following LispM's windows

(defun lispm-desktop-display--drag-outline (drag frame kind)
  "Move FRAME as DRAG does it, KIND `move' or `grow', unless FRAME is drawn
in an application's process and moves: then that window moves as the
computer's windows do, LispM's following once it stops, see
`lispm-desktop-display--moved'.  This is around
`lispm-desktop-platinum-drag-outline'."
  (if-let* (((eq kind 'move))
            (client (lispm-desktop-display--client-of frame)))
      (progn
        ;; The computer takes the mouse from here: no release comes.
        (process-put client :down nil)
        (lispm-desktop-display--send client :op "drag")
        nil)
    (funcall drag frame kind)))

(defun lispm-desktop-display--frame-changed (frame)
  "Move or size the window of the process FRAME is drawn in as FRAME now is.
This is one of `move-frame-functions' and `window-size-change-functions'."
  (when-let* ((client (seq-find (lambda (client) (eq (process-get client :outer) frame))
                                lispm-desktop-display--clients))
              ((frame-live-p frame)))
    (let ((geometry (lispm-desktop-display--geometry frame)))
      (unless (equal geometry (process-get client :geometry))
        (process-put client :geometry geometry)
        (apply #'lispm-desktop-display--send client :op "geometry" geometry)))))

(defun lispm-desktop-display--frame-deleted (frame)
  "Have the process FRAME is drawn in leave, its window closed.
This is one of `delete-frame-functions'."
  (dolist (client lispm-desktop-display--clients)
    (when (or (eq (process-get client :outer) frame) (eq (process-get client :frame) frame))
      (lispm-desktop-display--detach client "bye"))))

(defun lispm-desktop-display--desktop-changed ()
  "Let the processes go once the desktop shows again: their windows go in
over it, LispM's own.  This is one of `lispm-desktop-coherence-changed-hook'."
  (unless (seq-find (lambda (frame) (and (lispm-desktop-apps-screen-p frame)
                                         (lispm-desktop-coherence-out-of-sight-p frame)))
                    (frame-list))
    (dolist (client lispm-desktop-display--clients)
      (lispm-desktop-display--detach client "desktop"))))

(defun lispm-desktop-display--x-focus-frame (focus frame &rest args)
  "Give FRAME the keyboard as FOCUS does, unless it is drawn in an
application's process, or is being opened for one, or will be, its
process starting: that process comes in front instead, LispM staying out
of sight and out of the Dock.  This is around `x-focus-frame'."
  (let* ((app (and (frame-live-p frame) (frame-parameter frame 'lispm-desktop-app)))
         (client (or (lispm-desktop-display--client-of frame)
                     lispm-desktop-display--opening
                     ;; Shown a moment after it opened.
                     (and app (seq-find (lambda (client) (eq (process-get client :app) app))
                                        lispm-desktop-display--clients)))))
    (cond (client
           (select-frame frame 'norecord)
           (lispm-desktop-display--send client :op "activate"))
          ((and app (lispm-desktop-standalone-own-process-p app))
           (select-frame frame 'norecord))
          (t (apply focus frame args)))))

;;;; Menus and the cursor

(defun lispm-desktop-display--menu-items (client items)
  "Return ITEMS, as `lispm-desktop-popup-menu' takes them, as JSON for
CLIENT's process, each action numbered and kept."
  (let ((items (if (functionp items) (funcall items) items)))
    (vconcat
     (seq-keep
      (lambda (item)
        (cond ((eq item 'separator) (list :separator t))
              ((not (plist-get item :label)) nil)
              (t (let ((id (1+ (or (process-get client :menu-count) 0)))
                       (submenu (plist-get item :submenu)))
                   ;; Not `cl-incf': on Emacs 30 it returns what `process-put' does.
                   (process-put client :menu-count id)
                   (process-put client :menu-actions
                                (cons (cons id (plist-get item :action))
                                      (process-get client :menu-actions)))
                   `(:id ,id :label ,(substring-no-properties (plist-get item :label))
                         :enabled ,(if (and (plist-member item :enabled) (not (plist-get item :enabled))) :false t)
                         :checked ,(if (plist-get item :checked) t :false)
                         ,@(when-let* ((key (plist-get item :key))) (list :key key))
                         ,@(when submenu (list :items (lispm-desktop-display--menu-items client submenu))))))))
      items))))

(defun lispm-desktop-display--send-menus (client)
  "Send CLIENT's process the menus of its application, unless it has them."
  (when-let* ((frame (process-get client :frame))
              ((frame-live-p frame))
              (menus (plist-get (alist-get (process-get client :app) lispm-desktop-apps) :menus)))
    (process-put client :menu-count 0)
    (process-put client :menu-actions nil)
    (let* ((json (condition-case nil
                     (with-selected-frame frame
                       (vconcat
                        (mapcar (pcase-lambda (`(,_ ,title ,items))
                                  (list :title (substring-no-properties title)
                                        :items (lispm-desktop-display--menu-items client items)))
                                (funcall menus))))
                   (error [])))
           (line (json-serialize (list :op "menus" :menus json))))
      (unless (equal line (process-get client :menus))
        (process-put client :menus line)
        (when (process-live-p client)
          (process-send-string client (concat line "\n")))))))

(defun lispm-desktop-display--send-cursor (client)
  "Tell CLIENT's process where LispM's cursor is in its window, for the
input method's text and candidates."
  (when-let* ((outer (process-get client :outer))
              ((frame-live-p outer))
              ((eq (lispm-desktop-display--client-of (selected-frame)) client))
              (position (window-absolute-pixel-position)))
    (pcase-let* ((`(,ox ,oy . ,_) (lispm-desktop-display--edges outer 'outer-edges))
                 (cursor (list (- (car position) ox) (- (cdr position) oy) (line-pixel-height))))
      (unless (equal cursor (process-get client :cursor))
        (process-put client :cursor cursor)
        (lispm-desktop-display--send client :op "cursor"
                                     :x (nth 0 cursor) :y (nth 1 cursor) :h (nth 2 cursor))))))

(defun lispm-desktop-display--after-command ()
  "Keep the menus and the cursor of the window with the keyboard as they are
now.  This is one of `post-command-hook'."
  (when-let* ((client lispm-desktop-display--focused)
              ((process-live-p client)))
    (lispm-desktop-display--send-menus client)
    (lispm-desktop-display--send-cursor client)))

(defun lispm-desktop-display-menu (event)
  "Run the menu item EVENT's process chose."
  (interactive "e")
  (let* ((client (nth 1 event))
         (action (alist-get (nth 2 event) (process-get client :menu-actions)))
         (frame (process-get client :frame)))
    (when (frame-live-p frame)
      (select-frame frame 'norecord))
    (cond ((commandp action) (call-interactively action))
          ((functionp action) (funcall action)))))

;;;; Quitting and the desktop

(defun lispm-desktop-display-quit (event)
  "Close the windows of the application EVENT's process quits; it leaves
once they are closed, and stays when one is kept open."
  (interactive "e")
  (let* ((client (nth 1 event))
         (app (process-get client :app)))
    (dolist (frame (frame-list))
      (when (and (frame-live-p frame) (frame-parameter frame 'lispm-desktop-window)
                 (eq (frame-parameter frame 'lispm-desktop-app) app))
        (lispm-desktop-standalone--close frame)))
    (when (memq client lispm-desktop-display--clients)
      (lispm-desktop-display--send client :op "stay"))))

(defun lispm-desktop-display-desktop (_event)
  "Show LispM's desktop, as an application's process asked: the windows of
the processes come over it, LispM's own, and the processes leave."
  (interactive "e")
  (when-let* ((screen (seq-find (lambda (frame) (and (lispm-desktop-apps-screen-p frame)
                                                     (lispm-desktop-apps-hidden-p frame)))
                                (frame-list))))
    (lispm-desktop-show-desktop screen)
    (select-frame-set-input-focus screen)))

;;;; The socket

(defun lispm-desktop-display-socket ()
  "Return the socket LispM listens on for its applications' processes: one
for each init directory, as LispM.app and an Emacs of one's own differ."
  (expand-file-name (format "lispm-display-%s"
                            (substring (md5 (expand-file-name user-emacs-directory)) 0 8))
                    temporary-file-directory))

(defvar lispm-desktop-display--server nil
  "The process listening for the applications' processes.")

(defun lispm-desktop-display--sentinel (client _event)
  "Forget CLIENT once its process is gone: its application quit, or failed;
the windows it showed close."
  (unless (process-live-p client)
    (when (memq client lispm-desktop-display--clients)
      (let ((app (process-get client :app)))
        (lispm-desktop-display--detach client)
        (run-at-time 0 nil (lambda ()
                             (dolist (frame (frame-list))
                               (when (and (frame-live-p frame) (frame-parameter frame 'lispm-desktop-window)
                                          (eq (frame-parameter frame 'lispm-desktop-app) app))
                                 (lispm-desktop-standalone--close frame)))))))))

(defun lispm-desktop-display--accept (_server client _message)
  "Take CLIENT, an application's process just connected."
  (set-process-query-on-exit-flag client nil)
  (push client lispm-desktop-display--clients))

(defun lispm-desktop-display-start ()
  "Listen for the applications' processes, unless LispM does, or cannot draw
their windows here; return non-nil while it listens.  Where the display
is still to be built, it is, in the background, and LispM listens after."
  (or (process-live-p lispm-desktop-display--server)
      (if (not (lispm-desktop-display-load))
          (progn (lispm-desktop-display-build (lambda ()
                                                (when (lispm-desktop-display-load)
                                                  (lispm-desktop-display-start))))
                 nil)
        (let ((socket (lispm-desktop-display-socket)))
          (when (file-exists-p socket)
            (delete-file socket))
          (setq lispm-desktop-display--server
                (make-network-process :name "lispm-display" :server t :family 'local
                                      :service socket :coding 'utf-8-unix :noquery t
                                      :filter #'lispm-desktop-display--filter
                                      :sentinel #'lispm-desktop-display--sentinel
                                      :log #'lispm-desktop-display--accept))
          t))))

(defun lispm-desktop-display--bind-events ()
  "Run what a process asks that may show something as a special event, as
soon as read, even while a key sequence or an alert waits."
  (keymap-set special-event-map "<lispm-desktop-display-open>" #'lispm-desktop-display-open)
  (keymap-set special-event-map "<lispm-desktop-display-menu>" #'lispm-desktop-display-menu)
  (keymap-set special-event-map "<lispm-desktop-display-quit>" #'lispm-desktop-display-quit)
  (keymap-set special-event-map "<lispm-desktop-display-desktop>" #'lispm-desktop-display-desktop))

(defun lispm-desktop-setup-display ()
  "Let LispM draw the windows of its applications opened on their own."
  (lispm-desktop-display--bind-events)
  (add-hook 'move-frame-functions #'lispm-desktop-display--frame-changed)
  (add-hook 'window-size-change-functions #'lispm-desktop-display--frame-changed)
  (add-hook 'delete-frame-functions #'lispm-desktop-display--frame-deleted)
  (add-hook 'lispm-desktop-coherence-changed-hook #'lispm-desktop-display--desktop-changed)
  (add-hook 'post-command-hook #'lispm-desktop-display--after-command)
  (advice-add 'x-focus-frame :around #'lispm-desktop-display--x-focus-frame)
  (advice-add 'lispm-desktop-platinum-drag-outline :around #'lispm-desktop-display--drag-outline))

(provide 'lispm-desktop-display)
;;; lispm-desktop-display.el ends here
