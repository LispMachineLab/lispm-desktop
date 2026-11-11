;;; lispm-desktop-editor.el --- Emacs, the editor, as an application  -*- lexical-binding: t; -*-

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

;; On the desktop, Emacs the editor is an application, as the browser is:
;; its icon opens it, the Application menu brings it forward, and while it
;; is in front the menu bar holds its menus, File, Edit, View, Project and
;; Window.  Its workspaces, one per project, live inside it.  A title
;; strip runs across its top: the editor's title bar, naming the
;; workspace, over a row of tabs choosing the sidebar's view (Files,
;; Agents, Shells).
;;
;; The editor is drawn in one of two ways, and its zoom box moves it from
;; one to the other:
;;
;;   full screen   in the frame's own windows, under the menu bar, the
;;                 desktop's workspace out of sight
;;   floating      in a child frame over the desktop, moved by its title
;;                 bar and resized from its grow box or its edges
;;
;; Moving carries every workspace along: each is a tab, and goes to the
;; other frame as its window state.  The title strip and the sidebar are
;; side windows, which a window state does not carry, so they come off
;; before the move and are put back after it.
;;
;; Documents opened while the desktop or the browser is in front, files,
;; folders, terminals and agents, open in the editor, which comes forward.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-app-window)
(require 'lispm-desktop-surface)
(require 'lispm-desktop-i18n)

(declare-function lispm-desktop-show-desktop "lispm-desktop-coherence" (&optional screen))
(declare-function lispm-desktop-coherence-bring-in "lispm-desktop-coherence" (frame))
(declare-function lispm-desktop-sidebar--call "lispm-desktop-sidebar" (view what))
(declare-function lispm-desktop-sidebar-current-view "lispm-desktop-sidebar" ())
(declare-function lispm-desktop-sidebar-select "lispm-desktop-sidebar" (view))
(declare-function lispm-desktop-terminal-buffer-p "lispm-desktop-interface" (buffer))
(declare-function lispm-desktop-main-area--windows "lispm-desktop-interface" ())
(declare-function lispm-desktop-buffer-manager "lispm-desktop-buffer-manager" ())
(declare-function lispm-ui-menu-keyboard-p "lispm-ui-menu" (keys))
(declare-function tabspaces--current-tab-name "tabspaces" ())
(declare-function tabspaces-local-buffer-list "tabspaces" (&optional frame tabnum))
(declare-function tabspaces-reset-buffer-list "tabspaces" ())
(declare-function tabspaces--set-buffer-predicate "tabspaces" (frame))
(defvar lispm-desktop-sidebar-views)
(defvar lispm-desktop-browser-workspace-name)
(defvar tabspaces-default-tab)

(defvar lispm-desktop-editor--frame nil
  "The floating editor's child frame, or nil while the editor is full screen.")

(defvar lispm-desktop-editor--last-workspace nil
  "Name of the workspace the full-screen editor showed when it was left.")

(defvar lispm-desktop-editor--moving nil
  "Non-nil while the editor moves between full screen and floating.")

(defvar lispm-desktop-editor--closed nil
  "Non-nil once the floating editor was closed, until it is opened again.
A floating editor hidden only while the browser was in front shows again
with the desktop; a closed one does not.")

(defun lispm-desktop-editor-floating-p ()
  "Return non-nil while the editor floats in a window of its own."
  (frame-live-p lispm-desktop-editor--frame))

(defun lispm-desktop-editor-frame ()
  "Return the frame the editor is drawn in."
  (if (lispm-desktop-editor-floating-p) lispm-desktop-editor--frame (lispm-desktop-apps-screen)))

;;;; Workspaces

(defun lispm-desktop-editor--workspace-tab-p (tab)
  "Return non-nil when TAB is one of the editor's workspaces.
The desktop's and the browser's workspaces are not."
  (not (member (alist-get 'name tab)
               (list lispm-desktop-tab-name (bound-and-true-p lispm-desktop-browser-workspace-name)))))

(defun lispm-desktop-editor--in-workspace-p (&optional frame)
  "Return non-nil when FRAME, by default the selected one, shows an editor
workspace."
  (lispm-desktop-editor--workspace-tab-p (tab-bar--current-tab nil frame)))

(defun lispm-desktop-editor-workspaces ()
  "Return the names of the editor's workspaces, in order."
  (mapcar (lambda (tab) (alist-get 'name tab))
          (seq-filter #'lispm-desktop-editor--workspace-tab-p
                      (funcall tab-bar-tabs-function (lispm-desktop-editor-frame)))))

(defun lispm-desktop-editor--frame-p (frame)
  "Return non-nil when FRAME draws the editor: its floating window, or the
screen while one of its workspaces is in front.  Popups, child frames that
are no application's window, go with the editor."
  (cond ((eq frame lispm-desktop-editor--frame) t)
        ;; Another application's window, over the desktop or out on the
        ;; computer's screen.
        ((frame-parameter frame 'lispm-desktop-window) nil)
        ;; The frame drawing a window's frame, or an outline.
        ((lispm-desktop-platinum-chrome-frame-p frame) nil)
        ((frame-parent frame) t)
        ((lispm-desktop-apps-screen-p frame) (lispm-desktop-editor--in-workspace-p frame))))

(defun lispm-desktop-editor--current-workspace ()
  "Return the name of the editor's workspace in front, or nil."
  (let ((frame (lispm-desktop-editor-frame)))
    (when (lispm-desktop-editor--in-workspace-p frame)
      (alist-get 'name (tab-bar--current-tab nil frame)))))

(defun lispm-desktop-editor-workspace-title (name)
  "Return the title workspace NAME shows under: its name, the first
workspace's, `tabspaces-default-tab', in the language LispM speaks."
  (if (equal name (bound-and-true-p tabspaces-default-tab))
      (lispm-desktop-tr name)
    name))

(defun lispm-desktop-editor--remember-workspace (from-tab _to-tab)
  "Remember FROM-TAB when the full screen editor's workspace is left.
This is a `tab-bar-tab-post-select-functions' function."
  (when (and (not lispm-desktop-editor--moving) (lispm-desktop-editor--workspace-tab-p from-tab))
    (setq lispm-desktop-editor--last-workspace (alist-get 'name from-tab))))

;;;; The title strip

(defconst lispm-desktop-editor-strip-buffer-name " *Emacs*"
  "Name of the buffer of the editor's title strip.")

(defun lispm-desktop-editor--active-p (_window)
  "Return non-nil while the editor is in front: its title bar is then active."
  (eq (lispm-desktop-apps-frontmost) 'editor))

(defun lispm-desktop-editor--title ()
  "Return the editor's title: Emacs, and the workspace in front."
  (if-let* ((workspace (lispm-desktop-editor--current-workspace)))
      (format "Emacs — %s" (lispm-desktop-editor-workspace-title workspace))
    "Emacs"))

(defvar-keymap lispm-desktop-editor-tab-map
  :doc "Keymap of a tab of the editor's title strip."
  "<down-mouse-1>" #'lispm-desktop-editor-tab-press
  "<mouse-1>" #'ignore
  "<drag-mouse-1>" #'ignore)

(defun lispm-desktop-editor--tab-at (position)
  "Return the sidebar view at mouse POSITION, or nil outside a tab."
  (let ((window (posn-window position))
        (point (posn-point position)))
    (when (and (window-live-p window) (integer-or-marker-p point))
      (get-text-property point 'lispm-desktop-editor-view (window-buffer window)))))

(defun lispm-desktop-editor-tab-press (event)
  "Switch sidebar tabs when EVENT is released over the same tab.
Moving within a label still chooses it; releasing outside it cancels."
  (interactive "e")
  (let* ((start (event-start event))
         (window (posn-window start))
         (view (lispm-desktop-editor--tab-at start))
         (release (when view (lispm-ui-modal-read-event))))
    (when (and (consp release)
               (eq (event-basic-type release) 'mouse-1)
               (window-live-p window)
               (eq (posn-window (event-end release)) window)
               (eq (lispm-desktop-editor--tab-at (event-end release)) view))
      (with-selected-frame (window-frame window)
        (lispm-desktop-sidebar-select view)))))

(defun lispm-desktop-editor-tab-click (event)
  "Show the sidebar view whose tab EVENT clicked, or hide it when shown."
  (interactive "e")
  (let* ((position (event-start event))
         (view (get-text-property (posn-point position) 'lispm-desktop-editor-view
                                  (window-buffer (posn-window position)))))
    (when view
      (with-selected-frame (window-frame (posn-window position))
        (lispm-desktop-sidebar-select view)))))

(defun lispm-desktop-editor--tab (view selected)
  "Return the strip's tab for sidebar VIEW, in front when SELECTED, as
`lispm-desktop-platinum-tabs' takes it, its title and help in the language
LispM speaks."
  (let ((spec (cdr view)))
    (list (concat (lispm-ui-icon (plist-get spec :icon)) " "
                  (lispm-desktop-tr (plist-get spec :title)))
          selected
          'lispm-desktop-editor-view (car view) 'keymap lispm-desktop-editor-tab-map
          'help-echo (when-let* ((help (plist-get spec :help))) (lispm-desktop-tr help))
          'pointer 'arrow)))

(defvar-local lispm-desktop-editor--strip-width nil
  "The width in pixels the title strip's tabs were last drawn for.")

(defun lispm-desktop-editor--render-strip ()
  "Draw the title strip: its title, and a tab per sidebar view, standing on
the edge of the windows below, which runs across the strip."
  (when-let* ((buffer (get-buffer lispm-desktop-editor-strip-buffer-name)))
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
             (frame (lispm-desktop-editor-frame))
             (shown (with-selected-frame frame (lispm-desktop-sidebar-current-view)))
             (window (get-buffer-window buffer frame))
             ;; A line as wide as its window would wrap.
             (width (1- (if window (window-body-width window t) (frame-inner-width frame)))))
        (setq lispm-desktop-editor--strip-width width)
        (erase-buffer)
        (insert (lispm-desktop-platinum-tabs
                 (mapcar (lambda (view) (lispm-desktop-editor--tab view (eq (car view) shown)))
                         lispm-desktop-sidebar-views)
                 width))
        ;; Point stays before the tabs and the strip unscrolled: a strip
        ;; narrower than its tabs for a moment, as while the window is
        ;; resized from its edges, would otherwise be scrolled to the end of
        ;; the tabs, and stay so, the tabs out of sight.
        (goto-char (point-min))
        (dolist (strip (get-buffer-window-list buffer nil t))
          (set-window-point strip (point-min))
          (set-window-hscroll strip 0))
        (setq lispm-desktop-platinum-window-title (lispm-desktop-editor--title)
              ;; Only a floating window rolls up into its title bar.
              lispm-desktop-platinum-box-functions
              `(:close lispm-desktop-editor--close-box
                ,@(when (lispm-desktop-editor-can-float-p frame) '(:zoom lispm-desktop-editor--zoom-box))
                ,@(when (lispm-desktop-editor-floating-p) '(:collapse lispm-desktop-editor--collapse-box))))
        (force-mode-line-update)
        ;; The tabs may have changed the strip's height.
        (when window
          (lispm-desktop-editor--refit-strip window))))))

(defun lispm-desktop-editor--close-box (_window)
  "Close the editor, from its title bar's close box."
  (lispm-desktop-editor-close))

(defun lispm-desktop-editor--zoom-box (_window)
  "Move the editor between full screen and floating, from its zoom box."
  (lispm-desktop-editor-toggle-floating))

(defun lispm-desktop-editor--collapse-box (_window)
  "Roll the floating editor up to its title strip, or down again."
  (lispm-desktop-editor-shade))

(defvar evil-emacs-state-cursor)
(defvar evil-normal-state-cursor)

(define-derived-mode lispm-desktop-editor-strip-mode special-mode "Emacs"
  "Major mode of the editor's title strip: its title bar over its tabs."
  (setq-local mode-line-format nil
              header-line-format nil
              cursor-type nil
              truncate-lines t
              auto-hscroll-mode nil
              left-fringe-width 0
              right-fringe-width 0
              evil-emacs-state-cursor '(nil)
              evil-normal-state-cursor '(nil)
              lispm-desktop-platinum-window-icon (lispm-ui-icon 'emacs)
              lispm-desktop-platinum-box-functions (list :close #'lispm-desktop-editor--close-box
                                                         :zoom #'lispm-desktop-editor--zoom-box)
              lispm-desktop-platinum-active-function #'lispm-desktop-editor--active-p)
  ;; Its height is its title bar's and tabs': a frame growing, as on a tall
  ;; screen, gives its height to the windows below, the strip fitted again.
  (add-hook 'window-size-change-functions #'lispm-desktop-editor--refit-strip nil t)
  (face-remap-add-relative 'default 'lispm-desktop-platinum-toolbar)
  ;; The tabs' row is one line, never wrapped, whatever visual line mode
  ;; is on globally.
  (visual-line-mode -1)
  (setq-local truncate-lines t)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1)))

(defun lispm-desktop-editor--strip-buffer ()
  "Return the title strip's buffer, made when missing."
  (or (get-buffer lispm-desktop-editor-strip-buffer-name)
      (with-current-buffer (get-buffer-create lispm-desktop-editor-strip-buffer-name)
        (lispm-desktop-editor-strip-mode)
        (current-buffer))))

(defun lispm-desktop-editor--fit-strip ()
  "Fit the title strip to its title bar and tabs, in pixels, and keep it so:
a frame growing, as on a tall screen, gives its height to the windows
below, never to the strip."
  (when-let* ((window (get-buffer-window lispm-desktop-editor-strip-buffer-name)))
    (lispm-desktop-editor--refit-strip window)))

(defun lispm-desktop-editor--refit-strip (window)
  "Fit the strip's WINDOW to its title bar and tabs again once its size
changed, unless it is alone in its frame, as in a rolled-up editor; its
tabs are drawn again when its width changed.  This is a buffer-local
`window-size-change-functions' function."
  (unless (eql (1- (window-body-width window t)) lispm-desktop-editor--strip-width)
    (lispm-desktop-editor--render-strip))
  (unless (frame-root-window-p window)
    (let ((window-resize-pixelwise t)
          (fit-window-to-buffer-horizontally nil)
          ;; The strip is its title bar over one line of tabs, shorter
          ;; than the four lines a window keeps at the least.
          (window-min-height 1))
      ;; The height kept from the last fit would hold the window at it.
      (window-preserve-size window nil nil)
      (fit-window-to-buffer window nil nil nil nil t))))

(defun lispm-desktop-editor--show-strip ()
  "Put the title strip across the top of the selected frame, once."
  (let ((buffer (lispm-desktop-editor--strip-buffer)))
    (lispm-desktop-editor--render-strip)
    (unless (get-buffer-window buffer)
      (display-buffer-in-side-window
       buffer '((side . top) (slot . 0) (dedicated . t)
                (window-parameters (no-delete-other-windows . t)
                                   (no-other-window . t))))
      (lispm-desktop-editor--fit-strip)
      ;; A new window's lines are measured right once it was drawn.
      (run-at-time 0 nil #'lispm-desktop-editor--fit-strip))))

(defun lispm-desktop-editor--delete-side-windows ()
  "Delete the selected frame's side windows: the title strip and the sidebar."
  (dolist (window (window-list nil 'no-mini))
    (when (and (window-live-p window) (window-parameter window 'window-side))
      (delete-window window))))

(defvar lispm-desktop-editor--views nil
  "The sidebar view of each workspace taken off for a move: (NAME . VIEW).
Each is put back when its workspace is next in front.")

(defun lispm-desktop-editor--restore-chrome ()
  "Put the title strip on the workspace in front, and the sidebar view it
showed before the editor last moved."
  (lispm-desktop-editor--show-strip)
  (when-let* ((entry (assoc (alist-get 'name (tab-bar--current-tab)) lispm-desktop-editor--views)))
    (setq lispm-desktop-editor--views (delq entry lispm-desktop-editor--views))
    (when (and (cdr entry) (not (eq (cdr entry) (lispm-desktop-sidebar-current-view))))
      (let ((lispm-desktop-editor--moving nil))
        (lispm-desktop-sidebar-select (cdr entry))))))

(defun lispm-desktop-editor--keep-chrome ()
  "Keep the title strip on the editor's workspaces, and off any other.
Run after every command, so a workspace made or switched to by any means
gets it; the check is cheap.  Popups, child frames other than the
floating editor, are left alone."
  (let ((frame (selected-frame)))
    ;; Another application's window, out on the computer's screen too,
    ;; is no screen, whose workspaces the strip goes on.
    (unless (or lispm-desktop-editor--moving (minibufferp)
                (lispm-desktop-app-window-rolled-p lispm-desktop-editor--frame)
                (and (not (lispm-desktop-apps-screen-p frame)) (not (eq frame lispm-desktop-editor--frame))))
      (let ((strip (get-buffer-window lispm-desktop-editor-strip-buffer-name)))
        (cond ((lispm-desktop-editor--in-workspace-p)
               (unless strip (lispm-desktop-editor--restore-chrome)))
              (strip (delete-window strip)))))))

(defun lispm-desktop-editor--keep-floating-chrome ()
  "Put the title strip on the floating editor, when it is missing."
  (when (and (lispm-desktop-editor-floating-p) (not lispm-desktop-editor--moving)
             (not (get-buffer-window lispm-desktop-editor-strip-buffer-name lispm-desktop-editor--frame)))
    (with-selected-frame lispm-desktop-editor--frame
      (lispm-desktop-editor--show-strip))))

;;;; Bringing the editor in front, and closing it

(defun lispm-desktop-editor-activate ()
  "Bring the editor in front: floating over the desktop, or full screen in
the workspace it showed last."
  (interactive)
  (lispm-desktop-apps-set-frontmost 'editor)
  (setq lispm-desktop-editor--closed nil)
  ;; With the desktop hidden, or in looks where applications float over the
  ;; root screen, the editor floats in its own window.
  (when (and (or (lispm-desktop-apps-hidden-p)
                 (lispm-desktop-apps-floating-look-p))
             (not (lispm-desktop-editor-floating-p)))
    (lispm-desktop-editor-float))
  (let ((screen (lispm-desktop-apps-screen)))
    (if (lispm-desktop-editor-floating-p)
        (progn
          (unless (lispm-desktop-apps-hidden-p screen)
            (with-selected-frame screen
              (lispm-desktop--select-tab)))
          (lispm-desktop-platinum-show-window lispm-desktop-editor--frame)
          (select-frame-set-input-focus lispm-desktop-editor--frame)
          (lispm-desktop-editor--keep-floating-chrome))
      (unless (eq (selected-frame) screen)
        (select-frame-set-input-focus screen))
      (unless (lispm-desktop-editor--in-workspace-p)
        (let ((workspace (or (car (member lispm-desktop-editor--last-workspace (lispm-desktop-editor-workspaces)))
                             (car (lispm-desktop-editor-workspaces))
                             (bound-and-true-p tabspaces-default-tab)
                             "Home")))
          (tab-bar-switch-to-tab workspace)))
      (lispm-desktop-editor--show-strip)))
  (lispm-desktop-editor--render-strip))

(defun lispm-desktop-editor-hide ()
  "Hide the floating editor, out of the way of the application in front."
  (when (and (lispm-desktop-editor-floating-p)
             ;; Out on the computer's screen, it stays where it is.
             (not (lispm-desktop-apps-out-p lispm-desktop-editor--frame)))
    (lispm-desktop-platinum-hide-window lispm-desktop-editor--frame)
    (select-frame-set-input-focus (lispm-desktop-platinum-window-parent lispm-desktop-editor--frame))))

(defun lispm-desktop-editor--show-with-desktop ()
  "Show the floating editor again over the desktop, unless it was closed."
  (when (and (lispm-desktop-editor-floating-p) (not lispm-desktop-editor--closed))
    (lispm-desktop-platinum-show-window lispm-desktop-editor--frame)))

(defun lispm-desktop-editor-close ()
  "Close the editor, leaving the desktop in front, or the window floating
behind the floating editor with the keyboard, see
`lispm-desktop-apps-focus-after'.  Nothing in it is lost: it opens again
as it was."
  (interactive)
  (setq lispm-desktop-editor--closed t)
  (let ((next nil))
    (if (lispm-desktop-editor-floating-p)
        (progn (setq next (lispm-desktop-apps-focus-after lispm-desktop-editor--frame))
               (lispm-desktop-platinum-hide-window lispm-desktop-editor--frame))
      (when-let* ((workspace (lispm-desktop-editor--current-workspace)))
        (setq lispm-desktop-editor--last-workspace workspace)))
    ;; With the desktop hidden, the windows left out keep the screen.
    (unless (or next (lispm-desktop-apps-hidden-p))
      (lispm-desktop-activate))))

(defun lispm-desktop-editor--focus ()
  "Bring the editor in front unless it is, and select its frame."
  (cond ((lispm-desktop-editor-floating-p)
         (unless (eq (selected-frame) lispm-desktop-editor--frame)
           (lispm-desktop-apps-set-frontmost 'editor)
           (lispm-desktop-platinum-show-window lispm-desktop-editor--frame)
           (select-frame-set-input-focus lispm-desktop-editor--frame)))
        ((not (lispm-desktop-editor--in-workspace-p))
         (lispm-desktop-editor-activate))))

;;;; Documents opened from elsewhere

(defun lispm-desktop-editor--document-p (buffer)
  "Return non-nil when BUFFER belongs in the editor: anything but the
desktop, the browser's pages and Emacs's own internal buffers."
  (let ((name (buffer-name buffer)))
    (not (or (string-prefix-p " " name)
             (minibufferp buffer)
             (member name '("*Completions*"))
             (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                      'xwidget-webkit-mode)))))

(defvar lispm-desktop-editor--displaying nil
  "Non-nil while `lispm-desktop-editor-display' shows a document in the editor.")

(defun lispm-desktop-editor--astray-p (buffer-or-name _action)
  "Return non-nil when a document is about to show outside the editor.
That is any of `lispm-desktop-editor--document-p' while another application is
in front: the desktop, the Finder window or the browser, outside the
minibuffer.  An installed application's window keeps what it shows, see
lispm-desktop-app-window.el.  This is the `display-buffer-alist' condition
sending documents to the editor."
  (when-let* ((buffer (get-buffer buffer-or-name)))
    (and (not lispm-desktop-editor--moving)
         (not lispm-desktop-editor--displaying)
         (not (active-minibuffer-window))
         (not (lispm-desktop-editor--frame-p (selected-frame)))
         (not (frame-parameter (selected-frame) 'lispm-desktop-app-window))
         (not (frame-parameter (selected-frame) 'lispm-desktop-own-frame))
         (not (bound-and-true-p lispm-terminal-mode))
         (lispm-desktop-editor--document-p buffer))))

(defun lispm-desktop-editor-display (buffer alist)
  "Display BUFFER in the editor, brought in front first, per ALIST.
This is a `display-buffer' action function."
  (lispm-desktop-editor-activate)
  (let ((lispm-desktop-editor--displaying t))
    (display-buffer buffer (cons nil alist))))

;;;; Full screen and floating

(declare-function treemacs-get-local-buffer "treemacs-scope" ())

(defun lispm-desktop-editor--take-off-chrome (frame)
  "Take the title strip and the sidebar off FRAME's editor workspaces.
The view each showed is kept in `lispm-desktop-editor--views', to be put back.
The project tree of FRAME goes too: treemacs keeps one per frame, and the
editor's next frame builds its own."
  (with-selected-frame frame
    (let ((current (tab-bar--current-tab-index))
          (views nil))
      (seq-do-indexed (lambda (tab index)
                        (when (lispm-desktop-editor--workspace-tab-p tab)
                          (unless (= index (tab-bar--current-tab-index))
                            (tab-bar-select-tab (1+ index)))
                          (push (cons (alist-get 'name tab) (lispm-desktop-sidebar-current-view)) views)
                          (lispm-desktop-editor--delete-side-windows)))
                      (funcall tab-bar-tabs-function))
      (setq lispm-desktop-editor--views views)
      (unless (= current (tab-bar--current-tab-index))
        (tab-bar-select-tab (1+ current)))
      (when-let* (((fboundp 'treemacs-get-local-buffer))
                  (tree (treemacs-get-local-buffer)))
        (kill-buffer tree)))))

(defun lispm-desktop-editor--move-workspaces (from to)
  "Move the editor's workspaces from frame FROM to the end of frame TO's tabs.
The one in front in FROM goes as its present window state; each goes
without the window configuration that ties it to FROM."
  (let* ((tabs (funcall tab-bar-tabs-function from))
         (moving (seq-filter #'lispm-desktop-editor--workspace-tab-p tabs))
         ;; Stored before FROM loses them: the one in front is turned into
         ;; a stored tab from FROM's windows while they are still its own.
         (stored (with-selected-frame from
                   (mapcar (lambda (tab)
                             (assq-delete-all
                              'wc (copy-sequence (if (eq (car tab) 'current-tab)
                                                     (tab-bar--tab from)
                                                   tab))))
                           moving))))
    (tab-bar-tabs-set (seq-remove (lambda (tab) (memq tab moving)) tabs) from)
    (tab-bar-tabs-set (append (funcall tab-bar-tabs-function to) stored) to)))

(defun lispm-desktop-editor--default-geometry (screen)
  "Return where the editor floats first: most of SCREEN, under its menu bar."
  (let* ((width (frame-inner-width screen))
         (height (frame-inner-height screen))
         (top (+ (lispm-desktop-app-window-menu-bar-height screen) 20))
         (left (round (* width 0.06))))
    (list left top (- width (* 2 left)) (- height top 40))))

(defun lispm-desktop-editor--set-buffer-predicate (frame)
  "Let FRAME, the floating editor, switch only among its workspaces' buffers."
  (when (fboundp 'tabspaces--set-buffer-predicate)
    (tabspaces--set-buffer-predicate frame)))

(defun lispm-desktop-editor--roll (_frame)
  "Leave the floating editor, selected, one window for its title strip, and
return the strip's height: the editor rolls up to it."
  ;; The strip's height is measured before its window goes: the one
  ;; window left fills the frame until the frame shrinks to it.
  (let* ((strip (get-buffer-window lispm-desktop-editor-strip-buffer-name))
         (height (if strip (window-pixel-height strip) 60))
         (window (car (lispm-desktop-main-area--windows)))
         (ignore-window-parameters t))
    (delete-other-windows window)
    (set-window-dedicated-p window nil)
    (set-window-buffer window (lispm-desktop-editor--strip-buffer))
    height))

;; The editor's own code shows and hides its window: the editor floats
;; only some of the time, and once closed it stays hidden.
(lispm-desktop-app-window-define 'editor
  :name "Emacs" :geometry #'lispm-desktop-editor--default-geometry
  :after-make #'lispm-desktop-editor--set-buffer-predicate
  :roll #'lispm-desktop-editor--roll :follow nil)

(defun lispm-desktop-editor-can-float-p (&optional frame)
  "Return non-nil when FRAME can hold the editor in a child frame.
Text terminals can do so when Emacs supports terminal child frames."
  (or (not (lispm-desktop-tty-p frame)) (featurep 'tty-child-frames)))

(defun lispm-desktop-editor-float ()
  "Float the editor in a window of its own over the desktop."
  (interactive)
  (unless (or (lispm-desktop-editor-floating-p)
              (not (lispm-desktop-editor-can-float-p (lispm-desktop-apps-screen))))
    (let* ((screen (lispm-desktop-apps-screen))
           (lispm-desktop-editor--moving t)
           (_ (lispm-desktop-editor--take-off-chrome screen))
           (workspace (or (with-selected-frame screen (lispm-desktop-editor--current-workspace))
                          lispm-desktop-editor--last-workspace))
           (frame (lispm-desktop-app-window-make-frame 'editor screen)))
      (with-selected-frame screen
        (lispm-desktop--select-tab))
      (lispm-desktop-editor--move-workspaces screen frame)
      (setq lispm-desktop-editor--frame frame)
      ;; The frame's own first tab, made with it, holds nothing: the
      ;; workspaces follow it, and once one is selected it is dropped.
      (with-selected-frame frame
        (let ((names (mapcar (lambda (tab) (alist-get 'name tab))
                             (cdr (funcall tab-bar-tabs-function frame)))))
          (if (not names)
              (progn
                (tab-bar-rename-tab (or (bound-and-true-p tabspaces-default-tab) "Home"))
                (when (fboundp 'tabspaces-reset-buffer-list)
                  (tabspaces-reset-buffer-list))
                (lispm-desktop-editor--show-splash (frame-first-window frame)))
            (tab-bar-select-tab (+ 2 (or (seq-position names workspace) 0)))
            (tab-bar-tabs-set (cdr (funcall tab-bar-tabs-function frame)) frame))))
      (lispm-desktop-apps-show-window frame)
      (select-frame-set-input-focus frame)
      (let ((lispm-desktop-editor--moving nil))
        (with-selected-frame frame
          (lispm-desktop-editor--restore-chrome)))
      (lispm-desktop-apps-set-frontmost 'editor))))

(defun lispm-desktop-editor-full-screen ()
  "Bring the floating editor back to full screen, under the menu bar."
  (interactive)
  ;; Full screen is the desktop's: it shows first.
  (when (and (lispm-desktop-apps-hidden-p) (fboundp 'lispm-desktop-show-desktop))
    (lispm-desktop-show-desktop))
  (when (lispm-desktop-editor-floating-p)
    (let* ((frame lispm-desktop-editor--frame)
           (_ (when (lispm-desktop-apps-out-p frame) (lispm-desktop-coherence-bring-in frame)))
           (screen (lispm-desktop-platinum-window-parent frame))
           (lispm-desktop-editor--moving t)
           (_ (lispm-desktop-editor--take-off-chrome frame))
           (workspace (with-selected-frame frame (lispm-desktop-editor--current-workspace))))
      (lispm-desktop-editor--unshade)
      (lispm-desktop-app-window-release frame)
      (lispm-desktop-editor--move-workspaces frame screen)
      (setq lispm-desktop-editor--frame nil)
      (select-frame-set-input-focus screen)
      (tab-bar-switch-to-tab workspace)
      (delete-frame frame)
      (let ((lispm-desktop-editor--moving nil))
        (lispm-desktop-editor--restore-chrome))
      (lispm-desktop-apps-set-frontmost 'editor))))

(defun lispm-desktop-editor-toggle-floating ()
  "Move the editor between full screen and a floating window, as its zoom
box does."
  (interactive)
  (if (lispm-desktop-editor-floating-p) (lispm-desktop-editor-full-screen) (lispm-desktop-editor-float)))

(defun lispm-desktop-editor--unshade ()
  "Roll the floating editor down again, when it is rolled up."
  (when (lispm-desktop-editor-floating-p)
    (lispm-desktop-app-window-roll-down lispm-desktop-editor--frame)))

(defun lispm-desktop-editor-shade ()
  "Roll the floating editor up to its title strip, or down again, as Mac OS 9
rolled a window up into its title bar."
  (interactive)
  (when (lispm-desktop-editor-floating-p)
    (lispm-desktop-app-window-toggle-roll lispm-desktop-editor--frame)))

;;;; Menus

(defun lispm-desktop-editor--key (command)
  "Return the keys of COMMAND typed on the keyboard, as a menu shows them."
  (when-let* ((keys (seq-find #'lispm-ui-menu-keyboard-p (where-is-internal command))))
    (key-description keys)))

(defun lispm-desktop-editor--item (label command &rest properties)
  "Return the menu item LABEL running COMMAND in the editor, and PROPERTIES.
The editor comes forward first: a menu bar click focuses the desktop
under it."
  `(:label ,label
    :action ,(lambda ()
               (lispm-desktop-editor--focus)
               (if (commandp command) (call-interactively command) (funcall command)))
    ,@(when (and (symbolp command) (lispm-desktop-editor--key command))
        (list :key (lispm-desktop-editor--key command)))
    ,@properties))

(defun lispm-desktop-editor--in-editor (function)
  "Call FUNCTION with the editor's frame and its selected window selected."
  (with-selected-frame (lispm-desktop-editor-frame)
    (funcall function)))

(defun lispm-desktop-editor--file-items ()
  "Return the items of the editor's File menu."
  (let ((buffer (lispm-desktop-editor--in-editor (lambda () (window-buffer (selected-window))))))
    (list (lispm-desktop-editor--item (lispm-desktop-tr "New File…") #'find-file)
          (lispm-desktop-editor--item (lispm-desktop-tr "Open Project…") #'project-switch-project)
          ;; What C-x C-r runs, so the menu shows that key.
          (lispm-desktop-editor--item (lispm-desktop-tr "Open Recent…")
                                      (cond ((fboundp 'consult-recent-file) #'consult-recent-file)
                                            ((fboundp 'recentf-open) #'recentf-open)
                                            (t #'find-file)))
          'separator
          (lispm-desktop-editor--item (lispm-desktop-tr "Save") #'save-buffer
                                      :enabled (and (buffer-file-name buffer) (buffer-modified-p buffer) t))
          (lispm-desktop-editor--item (lispm-desktop-tr "Save As…") #'write-file)
          (lispm-desktop-editor--item (lispm-desktop-tr "Revert") #'revert-buffer
                                      :enabled (and (buffer-file-name buffer) t))
          'separator
          (lispm-desktop-editor--item (lispm-desktop-tr "Close Window")
                                      (lambda () (lispm-desktop-platinum-close-window (selected-window))))
          (lispm-desktop-editor--item (lispm-desktop-tr "Close Emacs") #'lispm-desktop-editor-close))))

(defun lispm-desktop-editor--edit-items ()
  "Return the items of the editor's Edit menu."
  (list (lispm-desktop-editor--item (lispm-desktop-tr "Undo") #'undo)
        (lispm-desktop-editor--item (lispm-desktop-tr "Redo") #'undo-redo)
        'separator
        (lispm-desktop-editor--item (lispm-desktop-tr "Cut") #'kill-region)
        (lispm-desktop-editor--item (lispm-desktop-tr "Copy") #'lispm-desktop-copy-region)
        (lispm-desktop-editor--item (lispm-desktop-tr "Paste") #'lispm-desktop-paste)
        (lispm-desktop-editor--item (lispm-desktop-tr "Select All") #'mark-whole-buffer)
        'separator
        (lispm-desktop-editor--item (lispm-desktop-tr "Find…") #'isearch-forward)
        (lispm-desktop-editor--item (lispm-desktop-tr "Replace…") #'query-replace)))

(defun lispm-desktop-editor--view-items ()
  "Return the items of the editor's View menu: the sidebar's views, and
the editor full screen or floating."
  (let ((shown (lispm-desktop-editor--in-editor #'lispm-desktop-sidebar-current-view)))
    `(,@(mapcar (lambda (view)
                  (lispm-desktop-editor--item (lispm-desktop-tr (plist-get (cdr view) :title))
                                              (lambda () (lispm-desktop-sidebar-select (car view)))
                                              :checked (eq (car view) shown)))
                lispm-desktop-sidebar-views)
      separator
      ,(lispm-desktop-editor--item (lispm-desktop-tr "Full Screen") #'lispm-desktop-editor-full-screen
                                   :checked (not (lispm-desktop-editor-floating-p)))
      ,(lispm-desktop-editor--item (lispm-desktop-tr "Floating Window") #'lispm-desktop-editor-float
                                   :checked (lispm-desktop-editor-floating-p)
                                   :enabled (lispm-desktop-editor-can-float-p)))))

(defun lispm-desktop-editor--window-items ()
  "Return the items of the editor's Window menu: the buffer manager, then
the workspace's buffers, the one shown checked, as Mac OS 9's Window menus
listed an application's windows."
  (lispm-desktop-editor--in-editor
   (lambda ()
     (let ((shown (window-buffer (selected-window))))
       `(,(lispm-desktop-editor--item (lispm-desktop-tr "Buffer Manager") #'lispm-desktop-buffer-manager)
         separator
         ,@(mapcar (lambda (buffer)
                     (lispm-desktop-editor--item (truncate-string-to-width
                                                  (lispm-desktop-platinum-buffer-title buffer) 48 nil nil "…")
                                                 (lambda () (switch-to-buffer buffer))
                                                 :checked (eq buffer shown)))
                   (seq-take (lispm-desktop-platinum-window-buffers (selected-window)) 30)))))))

(defun lispm-desktop-editor-menus ()
  "Return the editor's menus, for the menu bar while it is in front."
  `((lispm-desktop-editor-file ,(lispm-desktop-tr "File") lispm-desktop-editor--file-items)
    (lispm-desktop-editor-edit ,(lispm-desktop-tr "Edit") lispm-desktop-editor--edit-items)
    (lispm-desktop-editor-view ,(lispm-desktop-tr "View") lispm-desktop-editor--view-items)
    ;; The workspaces show as thumbnails, see lispm-desktop-workspaces.el.
    (lispm-desktop-editor-project ,(lispm-desktop-tr "Project") lispm-desktop-workspaces-menu-panel)
    (lispm-desktop-editor-window ,(lispm-desktop-tr "Window") lispm-desktop-editor--window-items)))

;;;; Setup

(declare-function lispm-desktop-genera-welcome-svg "lispm-desktop-genera"
                  (width height &optional standalone focused))
(declare-function lispm-desktop-genera-welcome-text "lispm-desktop-genera"
                  (width height &optional focused))
(declare-function lispm-desktop-genera-welcome-actions "lispm-desktop-genera" ())
(declare-function lispm-desktop-genera--welcome-colors "lispm-desktop-genera" ())
(declare-function lispm-desktop-genera-command "lispm-desktop-genera" (input))
(declare-function lispm-desktop-genera-listener-self-insert "lispm-desktop-genera" ())
(declare-function lispm-desktop-genera-listener-backspace "lispm-desktop-genera" ())
(declare-function lispm-desktop-genera-listener-kill-input "lispm-desktop-genera" ())
(declare-function lispm-desktop-genera-listener-abort "lispm-desktop-genera" ())
(declare-function lispm-desktop-genera-listener-yank "lispm-desktop-genera" ())
(declare-function lispm-desktop-genera-listener-complete "lispm-desktop-genera" ())
(declare-function lispm-desktop-genera-listener-ret "lispm-desktop-genera" ())
(declare-function lispm-desktop-genera-listener-history-previous "lispm-desktop-genera" ())
(declare-function lispm-desktop-genera-listener-history-next "lispm-desktop-genera" ())
(declare-function lispm-desktop-genera-mouse-system-menu "lispm-desktop-genera" (event))
(declare-function lispm-desktop-agent-lispm-new "lispm-desktop-lispm" (&optional role))
(declare-function lispm-desktop-shell-toggle "lispm-desktop-shells" ())
(declare-function lispm-desktop-guide "lispm-desktop-guide" ())
(declare-function lispm-terminal-quit "lispm-terminal" ())
(defvar lispm-desktop-genera--welcome-hits)
(defvar lispm-desktop-genera--listener-input)
(defvar lispm-desktop-genera--listener-output)
(defvar evil-emacs-state-cursor)
(defvar evil-normal-state-cursor)

(defconst lispm-desktop-editor-splash-buffer-name "*GNU Emacs*"
  "Name of the buffer of the unified LispM × GNU Emacs welcome screen.")

(defvar-local lispm-desktop-editor--splash-image nil
  "Last SVG image drawn in the welcome screen buffer.")

(defvar-local lispm-desktop-editor--splash-focused nil
  "Key of the focused quick-action button on the welcome screen, or nil.")

(defvar-local lispm-desktop-editor--splash-background nil
  "Face remapping cookies coloring the welcome screen buffer.")

(defvar-local lispm-desktop-editor--splash-state nil
  "Last rendered state (WIDTH HEIGHT FOCUSED LOOK THEME GRAPHIC-P) of splash.")

(defvar lispm-ui-in-gui-action)
(defvar lispm-ui--command-from-mouse)

(defun lispm-desktop-editor--splash-run-action (key)
  "Run the welcome page quick-action command associated with KEY."
  (when-let* ((spec (assq key (lispm-desktop-genera-welcome-actions)))
              (command (nth 2 spec)))
    ;; Run `New File…' (`find-file') and `Open Project…' via the minibuffer
    ;; prompt so the user can type a new file name or path cleanly.
    (let ((lispm-ui-in-gui-action nil)
          (lispm-ui--command-from-mouse nil))
      (if (commandp command)
          (call-interactively command)
        (funcall command)))))

(defun lispm-desktop-editor-splash-click (event)
  "Activate the welcome screen quick-action button clicked by mouse EVENT."
  (interactive "e")
  (let* ((pos (event-start event))
         (win (posn-window pos))
         (buf (and (windowp win) (window-buffer win))))
    (when (windowp win)
      (select-window win))
    (let ((key (or (when-let* ((pt (posn-point pos))
                               ((bufferp buf)))
                     (get-text-property pt 'lispm-desktop-welcome-action buf))
                   (when-let* ((xy (posn-object-x-y pos))
                               ((bufferp buf)))
                     (lispm-ui-modal-hit
                      (buffer-local-value 'lispm-desktop-genera--welcome-hits buf)
                      (car xy) (cdr xy))))))
      (cond
       ((eq key 'command)
        (when (bufferp buf)
          (with-current-buffer buf
            (setq lispm-desktop-editor--splash-focused nil)
            (lispm-desktop-editor--render-splash win t))))
       (key
        (when (bufferp buf)
          (with-current-buffer buf
            (setq lispm-desktop-editor--splash-focused key)
            (lispm-desktop-editor--render-splash win t)))
        (run-at-time 0 nil #'lispm-desktop-editor--splash-run-action key))
       (t
        (when (bufferp buf)
          (with-current-buffer buf
            (when lispm-desktop-editor--splash-focused
              (setq lispm-desktop-editor--splash-focused nil)
              (lispm-desktop-editor--render-splash win t)))))))))

(defun lispm-desktop-editor--splash-step (delta)
  "Move welcome screen button focus by DELTA (1 or -1)."
  (let* ((keys (mapcar #'car (lispm-desktop-genera-welcome-actions)))
         (idx (seq-position keys lispm-desktop-editor--splash-focused)))
    (when keys
      (setq lispm-desktop-editor--splash-focused
            (nth (lispm-ui-modal-cycle idx delta (length keys)) keys))
      (lispm-desktop-editor--render-splash nil t))))

(defun lispm-desktop-editor-splash-next ()
  "Focus the next quick-action button on the welcome screen."
  (interactive)
  (lispm-desktop-editor--splash-step 1))

(defun lispm-desktop-editor-splash-previous ()
  "Focus the previous quick-action button on the welcome screen."
  (interactive)
  (lispm-desktop-editor--splash-step -1))

(defun lispm-desktop-editor-splash-tab ()
  "Complete Listener input when non-empty, else focus the next button."
  (interactive)
  (if (and (boundp 'lispm-desktop-genera--listener-input)
           (not (string-empty-p lispm-desktop-genera--listener-input))
           (fboundp 'lispm-desktop-genera-listener-complete))
      (call-interactively #'lispm-desktop-genera-listener-complete)
    (lispm-desktop-editor-splash-next)))

(defun lispm-desktop-editor-splash-self-insert ()
  "Type the pressed character into `Command: █' on the welcome screen."
  (interactive)
  (setq lispm-desktop-editor--splash-focused nil)
  (when (fboundp 'lispm-desktop-genera-listener-self-insert)
    (call-interactively #'lispm-desktop-genera-listener-self-insert)))

(defun lispm-desktop-editor-splash-spc ()
  "Insert a space into `Command: █', or activate the focused button."
  (interactive)
  (if (and lispm-desktop-editor--splash-focused
           (string-empty-p (or (bound-and-true-p lispm-desktop-genera--listener-input) "")))
      (lispm-desktop-editor--splash-run-action lispm-desktop-editor--splash-focused)
    (lispm-desktop-editor-splash-self-insert)))

(defun lispm-desktop-editor-splash-ret ()
  "Execute the live `Command: █' input, or activate the focused button."
  (interactive)
  (cond
   ((and (boundp 'lispm-desktop-genera--listener-input)
         (not (string-empty-p (string-trim lispm-desktop-genera--listener-input)))
         (fboundp 'lispm-desktop-genera-listener-ret))
    (call-interactively #'lispm-desktop-genera-listener-ret))
   (lispm-desktop-editor--splash-focused
    (lispm-desktop-editor--splash-run-action lispm-desktop-editor--splash-focused))
   ((fboundp 'lispm-desktop-genera-listener-ret)
    (call-interactively #'lispm-desktop-genera-listener-ret))
   (t
    (call-interactively #'lispm-desktop-genera-command))))

(defun lispm-desktop-editor-splash-abort ()
  "Clear button focus and abort any live input at `Command: █'."
  (interactive)
  (setq lispm-desktop-editor--splash-focused nil)
  (if (fboundp 'lispm-desktop-genera-listener-abort)
      (call-interactively #'lispm-desktop-genera-listener-abort)
    (lispm-desktop-editor--render-splash nil t)))

(defun lispm-desktop-editor-splash-focus-mode ()
  "Toggle Focus Mode from the unified welcome screen."
  (interactive)
  (if (bound-and-true-p lispm-terminal-mode)
      (when (fboundp 'lispm-terminal-quit)
        (call-interactively #'lispm-terminal-quit))
    (call-interactively #'lispm-desktop-apps-focus-mode)))

(defvar lispm-desktop-editor-splash-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (keymap-set map "<remap> <self-insert-command>" #'lispm-desktop-editor-splash-self-insert)
    (cl-loop for ch from 33 to 126 do
             (define-key map (vector ch) #'lispm-desktop-editor-splash-self-insert))
    (keymap-set map "SPC" #'lispm-desktop-editor-splash-spc)
    (keymap-set map "DEL" #'lispm-desktop-genera-listener-backspace)
    (keymap-set map "<backspace>" #'lispm-desktop-genera-listener-backspace)
    (keymap-set map "C-u" #'lispm-desktop-genera-listener-kill-input)
    (keymap-set map "C-k" #'lispm-desktop-genera-listener-kill-input)
    (keymap-set map "C-y" #'lispm-desktop-genera-listener-yank)
    (keymap-set map "s-v" #'lispm-desktop-genera-listener-yank)
    (keymap-set map "M-p" #'lispm-desktop-genera-listener-history-previous)
    (keymap-set map "M-n" #'lispm-desktop-genera-listener-history-next)
    (keymap-set map "C-g" #'lispm-desktop-editor-splash-abort)
    (keymap-set map "<escape>" #'lispm-desktop-editor-splash-abort)
    (keymap-set map "<down-mouse-1>" #'ignore)
    (keymap-set map "<mouse-1>" #'lispm-desktop-editor-splash-click)
    (keymap-set map "<down-mouse-3>" #'lispm-desktop-genera-mouse-system-menu)
    (keymap-set map "<mouse-3>" #'ignore)
    (keymap-set map "TAB" #'lispm-desktop-editor-splash-tab)
    (keymap-set map "<tab>" #'lispm-desktop-editor-splash-tab)
    (keymap-set map "<right>" #'lispm-desktop-editor-splash-next)
    (keymap-set map "<down>" #'lispm-desktop-editor-splash-next)
    (keymap-set map "C-f" #'lispm-desktop-editor-splash-next)
    (keymap-set map "C-n" #'lispm-desktop-editor-splash-next)
    (keymap-set map "<backtab>" #'lispm-desktop-editor-splash-previous)
    (keymap-set map "S-TAB" #'lispm-desktop-editor-splash-previous)
    (keymap-set map "<left>" #'lispm-desktop-editor-splash-previous)
    (keymap-set map "<up>" #'lispm-desktop-editor-splash-previous)
    (keymap-set map "C-b" #'lispm-desktop-editor-splash-previous)
    (keymap-set map "C-p" #'lispm-desktop-editor-splash-previous)
    (keymap-set map "RET" #'lispm-desktop-editor-splash-ret)
    (keymap-set map "<return>" #'lispm-desktop-editor-splash-ret)
    map)
  "Keymap of the unified LispM Desktop welcome screen.")

(defun lispm-desktop-editor--paint-splash (buffer)
  "Color BUFFER and its fringes with the welcome screen's background."
  (when (and (buffer-live-p buffer)
             (fboundp 'lispm-desktop-genera--welcome-colors))
    (with-current-buffer buffer
      (let ((bg (car (lispm-desktop-genera--welcome-colors))))
        (mapc #'face-remap-remove-relative lispm-desktop-editor--splash-background)
        (setq lispm-desktop-editor--splash-background
              (list (face-remap-add-relative 'default :background bg)
                    (face-remap-add-relative 'fringe :background bg)))))))

(defun lispm-desktop-editor--render-splash (&optional window force)
  "Render the unified LispM × GNU Emacs welcome page in `*GNU Emacs*'."
  (when-let* ((buffer (get-buffer lispm-desktop-editor-splash-buffer-name))
              ((fboundp 'lispm-desktop-genera-welcome-svg)))
    (let* ((win (or (and (window-live-p window)
                         (eq (window-buffer window) buffer)
                         window)
                    (get-buffer-window buffer t)
                    (selected-window)))
           (graphic (and (display-images-p (and (window-live-p win) (window-frame win)))
                         (image-type-available-p 'svg)))
           (width (if graphic
                      (max 320 (if (window-live-p win) (window-body-width win t) 800))
                    (max 40 (if (window-live-p win) (window-body-width win) 80))))
           (height (if graphic
                       (max 220 (if (window-live-p win) (1- (window-body-height win t)) 600))
                     (max 14 (if (window-live-p win) (window-body-height win) 24)))))
      (with-current-buffer buffer
        (let ((state (list width height lispm-desktop-editor--splash-focused
                           lispm-desktop-look lispm-desktop-theme graphic
                           (bound-and-true-p lispm-terminal-mode)
                           (bound-and-true-p lispm-desktop-genera--listener-input)
                           (bound-and-true-p lispm-desktop-genera--listener-output))))
          (when (or force (not (equal state lispm-desktop-editor--splash-state)))
            (setq lispm-desktop-editor--splash-state state)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (if graphic
                  (let* ((svg (lispm-desktop-genera-welcome-svg
                               width height t lispm-desktop-editor--splash-focused))
                         (image (create-image svg 'svg t :scale 1 :ascent 100)))
                    (insert (propertize " " 'display image 'pointer 'arrow))
                    (when (and lispm-desktop-editor--splash-image (window-live-p win))
                      (image-flush lispm-desktop-editor--splash-image (window-frame win)))
                    (setq lispm-desktop-editor--splash-image image))
                (insert (lispm-desktop-genera-welcome-text
                         width height lispm-desktop-editor--splash-focused)))
              (goto-char (point-min))
              (when (and (window-live-p win) (eq (window-buffer win) buffer))
                (set-window-start win (point-min) t)
                (set-window-point win (point-min))))))))))

(defun lispm-desktop-editor--refresh-splash (&rest _)
  "Re-render the welcome screen when the look, theme or language changes."
  (when-let* ((buffer (get-buffer lispm-desktop-editor-splash-buffer-name)))
    (lispm-desktop-editor--paint-splash buffer)
    (lispm-desktop-editor--render-splash nil t)))

(defun lispm-desktop-editor--splash-scroll-bar (buffer)
  "Disable scroll bars on the welcome screen BUFFER."
  (and (eq (buffer-local-value 'major-mode buffer)
           'lispm-desktop-editor-splash-mode)
       'no))

(define-derived-mode lispm-desktop-editor-splash-mode special-mode "LispM"
  "Major mode of the unified LispM × GNU Emacs welcome screen.
\\{lispm-desktop-editor-splash-mode-map}"
  (visual-line-mode -1)
  (setq-local header-line-format nil
              cursor-type nil
              truncate-lines t
              left-fringe-width 1
              right-fringe-width 1
              show-trailing-whitespace nil
              evil-emacs-state-cursor '(nil)
              evil-normal-state-cursor '(nil)
              revert-buffer-function (lambda (&rest _) (lispm-desktop-editor--refresh-splash)))
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (lispm-desktop-editor--paint-splash (current-buffer))
  (add-hook 'window-size-change-functions #'lispm-desktop-editor--render-splash nil t)
  (add-hook 'window-buffer-change-functions #'lispm-desktop-editor--render-splash nil t))

(defun lispm-desktop-editor-splash-buffer ()
  "Return the buffer of the unified LispM × GNU Emacs welcome screen,
made when missing without being shown."
  (let ((buffer (get-buffer-create lispm-desktop-editor-splash-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lispm-desktop-editor-splash-mode)
        (lispm-desktop-editor-splash-mode)
        (lispm-desktop-editor--render-splash)))
    buffer))

(defun lispm-desktop-editor--new-workspace-buffer ()
  "Show the unified welcome screen in a new workspace's window; return nil.
This is the editor's `tab-bar-new-tab-choice'; the screen is put in the
window itself, see `lispm-desktop-show-in-new-tab'."
  (lispm-desktop-show-in-new-tab (lispm-desktop-editor-splash-buffer)))

(defun lispm-desktop-editor--show-splash (window)
  "Show the unified welcome screen in WINDOW when it is in an editor workspace
holding nothing else, and return non-nil; else return nil.
This is the editor's `lispm-desktop-empty-function'."
  (when (lispm-desktop-editor--in-workspace-p (window-frame window))
    (let ((buffer (lispm-desktop-editor-splash-buffer)))
      (set-window-buffer window buffer)
      (lispm-desktop-editor--render-splash window)
      (set-window-start window 1)
      t)))

(defun lispm-desktop-editor-kill ()
  "Kill the floating editor and reset its workspace, sidebar and geometry state.
Unlike `lispm-desktop-editor-close', which buries the editor frame while
preserving its buffers, sidebar view, workspace and geometry, this destroys
the floating frame and leaves no residual editor state behind."
  (interactive)
  (let* ((frame (and (lispm-desktop-editor-floating-p) lispm-desktop-editor--frame))
         (screen (lispm-desktop-apps-screen))
         (splash lispm-desktop-editor-splash-buffer-name)
         (keep (list lispm-desktop-buffer-name
                     lispm-desktop-empty-buffer-name
                     splash
                     "*scratch*"
                     "*Messages*"))
         (buffers
          (seq-uniq
           (seq-filter
            (lambda (buf)
              (and (buffer-live-p buf)
                   (not (member (buffer-name buf) keep))
                   (lispm-desktop-editor--document-p buf)))
            (append
             (when (frame-live-p frame)
               (append (mapcar #'window-buffer (window-list frame 'no-mini))
                       (frame-parameter frame 'buffer-list)
                       (frame-parameter frame 'buried-buffer-list)))
             (when (and (frame-live-p screen)
                        (lispm-desktop-editor--in-workspace-p screen))
               (mapcar #'window-buffer (window-list screen 'no-mini))))))))
    (when (cl-every #'lispm-desktop-platinum-confirm-close buffers)
      (when (frame-live-p frame)
        (lispm-desktop-editor--unshade)
        (with-selected-frame frame
          (dolist (entry lispm-desktop-sidebar-views)
            (when (and (fboundp 'lispm-desktop-sidebar--call)
                       (lispm-desktop-sidebar--call (car entry) :visible))
              (lispm-desktop-sidebar--call (car entry) :hide)))
          (lispm-desktop-editor--delete-side-windows)
          (when (fboundp 'treemacs-get-local-buffer)
            (when-let* ((tree (treemacs-get-local-buffer)))
              (kill-buffer tree)))))
      (setq lispm-desktop-editor--closed t
            lispm-desktop-editor--views nil
            lispm-desktop-editor--last-workspace nil)
      (when (frame-live-p screen)
        (with-selected-frame screen
          (lispm-desktop--select-tab screen)
          (let ((tabs (funcall tab-bar-tabs-function screen)))
            (tab-bar-tabs-set
             (seq-remove #'lispm-desktop-editor--workspace-tab-p tabs)
             screen))))
      (let ((next (and (frame-live-p frame)
                       (lispm-desktop-apps-focus-after frame))))
        (when (frame-live-p frame)
          (lispm-desktop-app-window-release frame)
          (setq lispm-desktop-editor--frame nil)
          (delete-frame frame))
        (setq lispm-desktop-app-window--geometry
              (assq-delete-all 'editor lispm-desktop-app-window--geometry))
        (let ((kill-buffer-query-functions nil))
          (dolist (buf buffers)
            (when (buffer-live-p buf)
              (kill-buffer buf)))
          (dolist (entry lispm-desktop-sidebar-views)
            (when-let* ((name (plist-get (cdr entry) :buffer))
                        (sbuf (get-buffer name)))
              (kill-buffer sbuf)))
          (when-let* ((strip (get-buffer lispm-desktop-editor-strip-buffer-name)))
            (kill-buffer strip)))
        (when-let* ((sbuf (get-buffer splash)))
          (with-current-buffer sbuf
            (setq lispm-desktop-editor--splash-focused nil
                  lispm-desktop-editor--splash-state nil)))
        (unless (or next (lispm-desktop-apps-hidden-p))
          (lispm-desktop-activate))))))

(defun lispm-desktop-editor--prepare-view ()
  "Bring the editor in front before a sidebar view is shown outside it."
  (unless (lispm-desktop-editor--frame-p (selected-frame))
    (lispm-desktop-editor-activate)
    (select-frame (lispm-desktop-editor-frame))))

(defvar lispm-desktop-sidebar-prepare-function)
(defvar lispm-desktop-sidebar-change-hook)

(defun lispm-desktop-setup-editor ()
  "Make Emacs the editor an application on the desktop."
  (setq lispm-desktop-sidebar-prepare-function #'lispm-desktop-editor--prepare-view
        lispm-desktop-empty-function #'lispm-desktop-editor--show-splash
        ;; The editor opens on the unified LispM × GNU Emacs welcome screen:
        ;; Emacs starts in its first workspace, and a new workspace starts there too.
        initial-buffer-choice #'lispm-desktop-editor-splash-buffer
        tab-bar-new-tab-choice #'lispm-desktop-editor--new-workspace-buffer)
  (add-hook 'lispm-desktop-platinum-scroll-bar-functions #'lispm-desktop-editor--splash-scroll-bar)
  (add-hook 'lispm-desktop-sidebar-change-hook #'lispm-desktop-editor--render-strip)
  (add-hook 'lispm-desktop-look-changed-hook #'lispm-desktop-editor--render-strip)
  (add-hook 'lispm-desktop-language-changed-hook #'lispm-desktop-editor--render-strip)
  (add-hook 'lispm-desktop-look-changed-hook #'lispm-desktop-editor--refresh-splash)
  (add-hook 'lispm-desktop-language-changed-hook #'lispm-desktop-editor--refresh-splash)
  (add-hook 'enable-theme-functions #'lispm-desktop-editor--refresh-splash 90)
  (add-hook 'post-command-hook #'lispm-desktop-editor--keep-chrome)
  (add-hook 'tab-bar-tab-post-select-functions #'lispm-desktop-editor--remember-workspace)
  (add-hook 'tab-bar-tab-post-select-functions
            (lambda (&rest _) (lispm-desktop-editor--render-strip)) 90)
  (add-hook 'lispm-desktop-activate-hook #'lispm-desktop-editor--show-with-desktop)
  (add-to-list 'display-buffer-alist '(lispm-desktop-editor--astray-p lispm-desktop-editor-display)))

(provide 'lispm-desktop-editor)
;;; lispm-desktop-editor.el ends here
