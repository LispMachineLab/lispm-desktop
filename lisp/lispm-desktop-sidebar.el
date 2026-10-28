;;; lispm-desktop-sidebar.el --- The left sidebar  -*- lexical-binding: t; -*-

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

;; The left sidebar, its registry of views and the parts every list view
;; is built from: toolbar, column headings, rows and groups.  The files
;; view lives here; every other view lives in its own file, beside what it
;; lists, and adds itself with `lispm-desktop-sidebar-define-view'.

;;; Code:

(require 'cl-lib)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-i18n)

;;;; Left sidebar views
;;
;; The left sidebar shows at most one view at a time.  Each tab bar button
;; selects a view; selecting the view already shown hides the sidebar.  The
;; main window is never touched:
;;
;;   none --files--> files --agents--> agents --agents--> none

(defvar lispm-desktop-sidebar-views nil
  "Views the left sidebar can show, in the order of the editor's tabs.
Each entry is (VIEW . PROPS), see `lispm-desktop-sidebar-define-view'.")

(defun lispm-desktop-sidebar-define-view (view &rest props)
  "Make VIEW one of the left sidebar's views, or redefine it in place.
PROPS is a property list:
  :title    the name its title bar and tab show
  :icon     its Finder icon, see `lispm-desktop-platinum-icon'
  :help     what its tab says it shows
  :command  the command its menu bar title runs
:title and :help are in English, marked with `lispm-desktop-tr-noop', and
translated where shown.
A list view, which the sidebar shows, hides and draws anew itself, names
its list:
  :buffer   the name of the list's buffer
  :mode     the list buffer's major mode, which calls
            `lispm-desktop-sidebar-setup-buffer'
  :render   a function drawing the list into the current buffer
Any other view gives the functions testing, showing and hiding it in the
selected frame, each called with no arguments: :visible, :show and
:hide.  Views are listed in the order they were first defined."
  (if-let* ((entry (assq view lispm-desktop-sidebar-views)))
      (setcdr entry props)
    (setq lispm-desktop-sidebar-views
          (append lispm-desktop-sidebar-views (list (cons view props))))))

(defun lispm-desktop-sidebar--call (view what)
  "Call VIEW's WHAT function, :visible, :show or :hide, and return its value.
A list view uses the sidebar's own, see `lispm-desktop-sidebar-define-view'."
  (if (plist-get (alist-get view lispm-desktop-sidebar-views) :buffer)
      (funcall (pcase what
                 (:visible #'lispm-desktop-sidebar-list-visible-p)
                 (:show #'lispm-desktop-sidebar--show-list)
                 (:hide #'lispm-desktop-sidebar--hide-list))
               view)
    (funcall (plist-get (alist-get view lispm-desktop-sidebar-views) what))))

;;;; Sidebar parts
;;
;; Every sidebar view is a side window of `lispm-desktop-sidebar-width' laid out as a
;; Finder window in list view: the title bar every window wears (see
;; lispm-desktop-platinum.el) names the view, a header below it holds the view's
;; buttons, and the content is a list of one-line rows under column
;; headings.  The title bar and header stay put while the rows scroll.

(defun lispm-desktop-sidebar-one-line (text width)
  "Return TEXT collapsed to a single line and truncated to WIDTH columns."
  (truncate-string-to-width
   (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " text))
   width nil nil "…"))

(defun lispm-desktop-sidebar--trim (label)
  "Return LABEL without the plain spaces at its ends.
A space showing an icon, as an image or a stretch, stays."
  (let ((start 0) (end (length label)))
    (while (and (< start end) (eq (aref label start) ?\s)
                (not (get-text-property start 'display label)))
      (setq start (1+ start)))
    (while (and (> end start) (eq (aref label (1- end)) ?\s)
                (not (get-text-property (1- end) 'display label)))
      (setq end (1- end)))
    (substring label start end)))

(defun lispm-desktop-sidebar-toolbar-button (label command help row &optional state)
  "Return LABEL as a bevel button calling COMMAND, described by HELP.
It sits on toolbar ROW, the window line holding it: `tab-line',
`header-line' or `mode-line'.  LABEL is shown in the list's face, its
icons keeping their own faces.  STATE is nil, `pressed' for a toggle
that is on, or `disabled', when the button does nothing.  STATE may be
a face instead: LABEL is then text in that face, as the browser's
Netscape toolbars draw theirs.  The button is pressed rather than
clicked: drawn pushed in while held, and run on release over it, see
`lispm-desktop-platinum-press'.  Built while its line is drawn, as
toolbars are, it shows itself pushed in from the window's
`lispm-desktop-platinum-pressed'."
  (let* ((face (and (facep state) state))
         (label (if face label (lispm-desktop-sidebar--trim label)))
         ;; The command tells buttons apart: `equal' ignores text
         ;; properties, and icons drawn as images all read " ", so the new
         ;; session buttons of two agents with image icons had one label.
         (target (list row command label))
         (pressed (lispm-desktop-platinum-pressed-p target))
         (button (if face
                     (let ((styled (copy-sequence label)))
                       (add-face-text-property 0 (length styled)
                                               (if pressed 'lispm-desktop-platinum-button-pressed face)
                                               t styled)
                       styled)
                   (lispm-desktop-platinum-bevel-button label (if pressed 'pressed state)))))
    (if (eq state 'disabled)
        button
      (propertize button
                  'help-echo help
                  'lispm-desktop-platinum-target target
                  'keymap (let ((map (make-sparse-keymap)))
                            (keymap-set map (format "<%s> <down-mouse-1>" row)
                                        (lambda (event)
                                          (interactive "e")
                                          (let ((start (event-start event)))
                                            (lispm-desktop-platinum-press
                                             start (lispm-desktop-platinum-same-target-p start)
                                             (lambda ()
                                               (with-selected-window (posn-window start)
                                                 (call-interactively command)))))))
                            (keymap-set map (format "<%s> <mouse-1>" row) #'ignore)
                            map)))))

(defun lispm-desktop-sidebar--toolbar-buttons (actions row)
  "Return ACTIONS as bevel buttons on ROW.
Each action is (LABEL COMMAND HELP [PRESSED]); PRESSED draws its button
pushed in, as a toggle that is on.  LABEL and HELP are translated here,
as the buttons are drawn: in English, marked with
`lispm-desktop-tr-noop', they show in the language LispM speaks."
  (mapconcat (pcase-lambda (`(,label ,command ,help ,pressed))
               (lispm-desktop-sidebar-toolbar-button (lispm-desktop-tr label) command
                                                     (and help (lispm-desktop-tr help))
                                                     row (and pressed 'pressed)))
             actions (propertize " " 'display `(space :width (,(lispm-desktop-px 4))))))

(defvar-local lispm-desktop-sidebar--faces nil
  "Face remapping cookies drawing this buffer as a list view.")

(defvar-local lispm-desktop-sidebar--header nil
  "This list window's header: (ACTIONS REFRESH TITLE-ACTIONS).")

(defun lispm-desktop-sidebar--header-line ()
  "Return the header of the list window being drawn, its buttons pressed or not.
ACTIONS sit at its left end; TITLE-ACTIONS and the refresh button end a
few pixels short of its right, their width measured, as bevels and icons
are not whole columns."
  (pcase-let* ((`(,actions ,refresh ,title-actions) lispm-desktop-sidebar--header)
               (trailing (concat (when title-actions
                                   (concat (lispm-desktop-sidebar--toolbar-buttons title-actions
                                                                                   'header-line)
                                           " "))
                                 (lispm-desktop-sidebar-toolbar-button "⟳" refresh (lispm-desktop-tr "Refresh")
                                                                        'header-line))))
    (concat " " (lispm-desktop-sidebar--toolbar-buttons actions 'header-line)
            (propertize " " 'display
                        `(space :align-to ,(lispm-desktop-align-right (+ (string-pixel-width trailing)
                                                                         (lispm-desktop-px 6)))))
            trailing)))

(defun lispm-desktop-sidebar-set-toolbar (view refresh actions &optional title-actions)
  "Give the current buffer the title bar and header of sidebar VIEW.
VIEW, a key of `lispm-desktop-sidebar-views', names the title bar and gives its
icon.  The header shows ACTIONS at its left end and, at its right end,
TITLE-ACTIONS followed by a button calling REFRESH.  Both action lists
hold (LABEL COMMAND HELP [PRESSED]) entries, LABEL and HELP in English,
translated where shown, see `lispm-desktop-sidebar--toolbar-buttons'."
  (let ((spec (alist-get view lispm-desktop-sidebar-views)))
    (setq-local lispm-desktop-platinum-window-title
                (lambda () (lispm-desktop-tr (plist-get (alist-get view lispm-desktop-sidebar-views) :title)))
                lispm-desktop-platinum-icon (lispm-desktop-platinum-icon (plist-get spec :icon))))
  (kill-local-variable 'tab-line-format)
  (unless lispm-desktop-sidebar--faces
    (setq-local lispm-desktop-sidebar--faces
                (list (face-remap-add-relative 'default 'lispm-desktop-platinum-list)
                      (face-remap-add-relative 'header-line 'lispm-desktop-platinum-header))))
  (setq-local lispm-desktop-sidebar--header (list actions refresh title-actions)
              header-line-format '(:eval (lispm-desktop-sidebar--header-line))))

(defun lispm-desktop-sidebar-setup-buffer ()
  "Set up the current buffer as sidebar content.
Visual line mode is turned off explicitly, so `global-visual-line-mode'
leaves the buffer alone and an overlong title is truncated, not wrapped.
Rows are spaced and coloured as Finder's list view: lighter than the
platinum, the column sorted by in platinum, a lit line atop each.  The
window's bottom edge counts the rows, from `lispm-desktop-platinum-info',
as Finder's counted items.
A list shows no text cursor, whatever state Evil is in: the row at point
is highlighted in the selection colour instead, as Finder showed the
selected item."
  (visual-line-mode -1)
  (setq-local truncate-lines t
              cursor-type nil
              line-spacing 3)
  (face-remap-add-relative 'default 'lispm-desktop-platinum-list-body)
  (dolist (cursor '(evil-emacs-state-cursor evil-normal-state-cursor
                    evil-motion-state-cursor))
    (set (make-local-variable cursor) '(nil)))
  (face-remap-add-relative 'hl-line 'region)
  ;; A window's title menu offers documents, not the sidebar's lists.
  (setq-local lispm-desktop-platinum-listed nil)
  (display-line-numbers-mode -1))

(defun lispm-desktop-sidebar--keep-width (window)
  "Preserve the width sidebar WINDOW has now, whatever changed it.
Emacs preserves the width of a window's text, and forgets it as soon as
that changes, as when the scroll bar widens the right fringe after the
sidebar is shown, or a drag of its edge or \\[enlarge-window-horizontally]
resizes it; the frame narrowing would then squeeze the sidebar with the
main area.  This is a buffer-local `window-size-change-functions' function."
  (when (window-parameter window 'window-side)
    (window-preserve-size window t t)))

(defun lispm-desktop-sidebar--display (buffer)
  "Show BUFFER as the left sidebar of this frame and return its window.
The window is dedicated to BUFFER, so nothing else is ever shown in it:
switching buffers there goes to the main area instead.  It keeps its
width while the frame changes size, the main area taking the difference
as long as the frame is wide enough for both; a drag of its edge still
resizes it, see `lispm-desktop-sidebar--keep-width'."
  (with-current-buffer buffer
    (add-hook 'window-size-change-functions #'lispm-desktop-sidebar--keep-width nil t))
  (display-buffer-in-side-window
   buffer `((side . left)
            (slot . -1)
            (window-width . ,(lispm-desktop-sidebar-columns))
            (preserve-size . (t . nil))
            (dedicated . t)
            (window-parameters . ((no-delete-other-windows . t)
                                  (no-other-window . t))))))

(defun lispm-desktop-sidebar-buffer-view (buffer)
  "Return the sidebar view BUFFER is the list of, or nil.
See the :mode of `lispm-desktop-sidebar-views'."
  (car (seq-find (lambda (view)
                   (when-let* ((mode (plist-get (cdr view) :mode)))
                     (eq (buffer-local-value 'major-mode buffer) mode)))
                 lispm-desktop-sidebar-views)))

(defun lispm-desktop-sidebar-view-buffer-p (buffer-or-name _action)
  "Return non-nil when BUFFER-OR-NAME is a sidebar list about to be shown.
This is the `display-buffer-alist' condition keeping lists in the sidebar."
  (when-let* ((buffer (get-buffer buffer-or-name)))
    (lispm-desktop-sidebar-buffer-view buffer)))

(defun lispm-desktop-sidebar-display-view (buffer _alist)
  "Show the sidebar view whose list BUFFER is, and return its window.
This is a `display-buffer' action function: however a list is asked for,
it is shown in the sidebar, never in the main area."
  (let ((view (lispm-desktop-sidebar-buffer-view buffer)))
    (unless (eq view (lispm-desktop-sidebar-current-view))
      (lispm-desktop-sidebar-select view))
    (get-buffer-window buffer)))

(defun lispm-desktop-sidebar--list-buffer (view)
  "Return the name of the buffer of list VIEW."
  (plist-get (alist-get view lispm-desktop-sidebar-views) :buffer))

(defun lispm-desktop-sidebar-list-visible-p (view)
  "Return non-nil when list VIEW is shown in the selected frame."
  (and (get-buffer-window (lispm-desktop-sidebar--list-buffer view)) t))

(defun lispm-desktop-sidebar--show-list (view)
  "Show list VIEW, drawn anew, on the left of this frame and return its window."
  (let* ((props (alist-get view lispm-desktop-sidebar-views))
         (buffer (get-buffer-create (plist-get props :buffer))))
    (with-current-buffer buffer
      (unless (derived-mode-p (plist-get props :mode))
        (funcall (plist-get props :mode)))
      (funcall (plist-get props :render))
      (goto-char (point-min)))
    (lispm-desktop-sidebar--display buffer)))

(defun lispm-desktop-sidebar--hide-list (view)
  "Hide list VIEW in the selected frame."
  (when-let* ((window (get-buffer-window (lispm-desktop-sidebar--list-buffer view))))
    (delete-window window)))

(defun lispm-desktop-sidebar-refresh-list (view)
  "Draw list VIEW anew when it exists, point staying on the same line."
  (when-let* ((buffer (get-buffer (lispm-desktop-sidebar--list-buffer view))))
    (with-current-buffer buffer
      (let ((line (line-number-at-pos))
            (window (get-buffer-window buffer)))
        (funcall (plist-get (alist-get view lispm-desktop-sidebar-views) :render))
        (goto-char (point-min))
        (forward-line (1- line))
        (when window (set-window-point window (point)))))))

(defun lispm-desktop-sidebar--refresh-lists ()
  "Draw every list view anew where it exists, as in another language."
  (dolist (view lispm-desktop-sidebar-views)
    (when (plist-get (cdr view) :buffer)
      (lispm-desktop-sidebar-refresh-list (car view)))))

(defun lispm-desktop-sidebar-show-in-main (buffer)
  "Show BUFFER in the main area and select it.
Sessions go to the session column, see `lispm-desktop-main-area-show-terminal';
anything else to the file column, through `display-buffer', so a file of
another project still opens in that project's workspace."
  (if (lispm-desktop-terminal-buffer-p buffer)
      (lispm-desktop-main-area-show-terminal buffer)
    (select-window (display-buffer buffer '(lispm-desktop-main-area-display-file)))
    buffer))

(defconst lispm-desktop-sidebar-indent 18
  "Pixels each level of a list is indented by, the width of a triangle and gap.")

(defvar-local lispm-desktop-sidebar--columns-width nil
  "The width in pixels the list's column headings were drawn for.")

(defun lispm-desktop-sidebar-insert-columns (name meta)
  "Insert the column headings NAME and META, the list being sorted by META.
NAME stands over the rows' names, past their indentation and icon.  The
headings are drawn as wide as the list; the list is drawn again when its
window's width changes, as when its scroll bar comes."
  (setq lispm-desktop-sidebar--columns-width (lispm-desktop-sidebar--row-width))
  (add-hook 'window-size-change-functions #'lispm-desktop-sidebar--refit-columns nil t)
  (insert (lispm-desktop-platinum-columns name meta lispm-desktop-sidebar--columns-width
                                          (lispm-desktop-sidebar--column-pixels)
                                          (+ 8 lispm-desktop-sidebar-indent))))

(defun lispm-desktop-sidebar--refit-columns (window)
  "Draw WINDOW's list again when it is no longer as wide as its headings.
This is a buffer-local `window-size-change-functions' function."
  (with-current-buffer (window-buffer window)
    (when (and lispm-desktop-sidebar--columns-width lispm-desktop-sidebar--header
               (/= (lispm-desktop-sidebar--row-width) lispm-desktop-sidebar--columns-width))
      (funcall (nth 1 lispm-desktop-sidebar--header)))))

(defun lispm-desktop-sidebar--row-width ()
  "Return the pixel width rows have to fill: the sidebar's text area, on a
text terminal but its last two columns, see `lispm-desktop-align-right'."
  (- (if-let* ((window (get-buffer-window (current-buffer))))
         (window-body-width window t)
       (* (lispm-desktop-sidebar-columns) (frame-char-width)))
     (if (lispm-desktop-tty-p) 2 0)))

(defun lispm-desktop-sidebar--blank (spec)
  "Return a blank displayed as SPEC, in the list's face.
Every part of a row carries a face of its own: a part without one would
take the `button' face of the row, underline and all."
  (propertize " " 'display spec 'face 'lispm-desktop-platinum-list))

(defun lispm-desktop-sidebar--column-pixels ()
  "Return the width in pixels of a list's right-hand column."
  (* lispm-desktop-platinum-column-width (frame-char-width)))

(defun lispm-desktop-sidebar--sorted-column (meta)
  "Return the right-hand column of a row holding META, flush right.
The column is shaded down its whole length, as Finder shaded the column
a list was sorted by."
  (let* ((meta (lispm-desktop-platinum-fit (propertize (or meta "") 'face
                                                        (or (get-text-property 0 'face (or meta ""))
                                                            'lispm-desktop-platinum-list))
                                            (- (lispm-desktop-sidebar--column-pixels)
                                               (lispm-desktop-px 16))))
         (meta-width (string-pixel-width meta)))
    (add-face-text-property 0 (length meta) 'lispm-desktop-platinum-sorted t meta)
    (concat (lispm-desktop-sidebar--blank
             `(space :align-to ,(lispm-desktop-align-right (lispm-desktop-sidebar--column-pixels))))
            (propertize " " 'display `(space :align-to ,(lispm-desktop-align-right
                                                         (+ meta-width (lispm-desktop-px 10))))
                        'face 'lispm-desktop-platinum-sorted)
            meta
            (propertize " " 'display `(space :align-to ,(lispm-desktop-align-right))
                        'face 'lispm-desktop-platinum-sorted))))

(defun lispm-desktop-sidebar--row (lead name meta width)
  "Return a row of a list WIDTH pixels wide: LEAD, NAME, and META at its right.
LEAD (indentation, a triangle or an icon) keeps its size; META sits in
the right-hand column, see `lispm-desktop-sidebar--sorted-column'; NAME is
shortened to fit between them."
  (let ((room (- width (string-pixel-width (propertize lead 'face 'lispm-desktop-platinum-list))
                 (lispm-desktop-sidebar--column-pixels) (lispm-desktop-px 12))))
    (concat lead
            (lispm-desktop-platinum-fit (propertize name 'face (or (get-text-property 0 'face name)
                                                                    'lispm-desktop-platinum-list))
                                         (max 0 room))
            (lispm-desktop-sidebar--sorted-column meta))))

(defun lispm-desktop-sidebar-insert-entry (icon title meta action help &optional properties depth)
  "Insert a one-line row and make it one button calling ACTION.
The row shows ICON and TITLE, indented by DEPTH levels, and META, dimmed,
in the right-hand column; HELP describes the button, which spans the row
to the window's right edge.  PROPERTIES, a property list, is added to the
row's text."
  (let* ((beg (point))
         (meta (copy-sequence (or meta ""))))
    ;; The dimming is layered, so coloured marks such as "running" survive;
    ;; the button below sets no face, which would replace the icon's Nerd
    ;; Font family.
    (add-face-text-property 0 (length meta) 'shadow t meta)
    (insert (lispm-desktop-sidebar--row
             (concat (lispm-desktop-sidebar--blank `(space :width (,(lispm-desktop-px
                                                                       (+ 8 lispm-desktop-sidebar-indent
                                                                          (* lispm-desktop-sidebar-indent
                                                                             (or depth 0)))))))
                     icon (propertize " " 'face 'lispm-desktop-platinum-list))
             (lispm-desktop-sidebar-one-line title 200)
             meta
             (lispm-desktop-sidebar--row-width)))
    (make-text-button beg (point)
                      'action (lambda (_) (funcall action))
                      'follow-link t
                      'help-echo help
                      'mouse-face 'highlight)
    (lispm-desktop-sidebar--mark-row beg (point))
    (when properties
      (add-text-properties beg (point) properties))
    (insert "\n")))

(defun lispm-desktop-sidebar-insert-empty (text)
  "Insert the row of an empty list, saying TEXT."
  (insert (lispm-desktop-sidebar--blank `(space :width (,(lispm-desktop-px 8))))
          (propertize text 'face 'shadow)
          (lispm-desktop-sidebar--sorted-column nil) "\n"))

(defun lispm-desktop-sidebar--mark-row (beg end)
  "Draw the row from BEG to END as a list's row, a lit line atop it."
  (add-face-text-property beg end 'lispm-desktop-platinum-row t))

(defun lispm-desktop-sidebar-insert-group (open name meta action help &optional directory)
  "Insert a group's row: a disclosure triangle, OPEN or not, a folder and NAME.
The folder is badged when DIRECTORY is a git repository.  META sits in
the right-hand column; clicking the row calls ACTION, described by HELP."
  (let ((beg (point)))
    (insert (lispm-desktop-sidebar--row
             (concat (lispm-desktop-sidebar--blank `(space :width (,(lispm-desktop-px 8))))
                     (propertize (lispm-desktop-platinum-triangle open) 'face 'lispm-desktop-platinum-list)
                     (lispm-desktop-sidebar--blank `(space :width (,(lispm-desktop-px 6))))
                     (lispm-desktop-platinum-folder-icon directory)
                     (lispm-desktop-sidebar--blank `(space :width (,(lispm-desktop-px 6)))))
             (propertize (lispm-desktop-sidebar-one-line name 200)
                         'face '(:inherit lispm-desktop-platinum-list :weight bold))
             (propertize (or meta "") 'face 'shadow)
             (lispm-desktop-sidebar--row-width)))
    (make-text-button beg (point)
                      'action (lambda (_) (funcall action))
                      'follow-link t
                      'help-echo help
                      'mouse-face 'highlight)
    (lispm-desktop-sidebar--mark-row beg (point))
    (insert "\n")))

;;;; Files sidebar

(declare-function treemacs-current-workspace "treemacs-workspaces" ())
(declare-function treemacs-workspace->projects "treemacs-workspaces" (workspace))
(declare-function treemacs-project->path "treemacs-workspaces" (project))

(defun lispm-desktop-sidebar--files-info ()
  "Return what the project tree's bottom edge says: the project shown."
  (if-let* ((projects (treemacs-workspace->projects (treemacs-current-workspace))))
      (abbreviate-file-name (treemacs-project->path (car projects)))
    (lispm-desktop-tr "No project")))

(defun lispm-desktop-sidebar--files-toolbar ()
  "Give the project tree buffer its sidebar toolbar and bottom edge."
  (setq lispm-desktop-platinum-info #'lispm-desktop-sidebar--files-info)
  (lispm-desktop-sidebar-set-toolbar
   'files #'treemacs-refresh
   `((,(lispm-desktop-tr-noop "New File") treemacs-create-file
      ,(lispm-desktop-tr-noop "New file in the selected directory"))
     (,(lispm-desktop-tr-noop "New Folder") treemacs-create-dir
      ,(lispm-desktop-tr-noop "New folder in the selected directory"))
     (,(lispm-desktop-tr-noop "Collapse") treemacs-collapse-all-projects ,(lispm-desktop-tr-noop "Collapse all")))))

(defun lispm-desktop-sidebar--files-visible-p ()
  "Return non-nil when the project tree is shown in the selected frame."
  (and (featurep 'treemacs) (eq (treemacs-current-visibility) 'visible)))

(defun lispm-desktop-sidebar--files-show ()
  "Show the project tree of the main window's buffer, keeping the selection.
Outside any project treemacs falls back to the buffer's directory."
  (require 'treemacs)
  (save-selected-window
    (with-current-buffer (window-buffer (lispm-desktop-platinum-main-window))
      (treemacs-add-and-display-current-project-exclusively))))

(defun lispm-desktop-sidebar--files-hide ()
  "Hide the project tree in the selected frame."
  (when-let* ((window (and (featurep 'treemacs) (treemacs-get-local-window))))
    (delete-window window)))

(lispm-desktop-sidebar-define-view
 'files :title (lispm-desktop-tr-noop "Files") :icon "disk" :command 'lispm-desktop-sidebar-files
 :help (lispm-desktop-tr-noop "Project files")
 :visible #'lispm-desktop-sidebar--files-visible-p
 :show #'lispm-desktop-sidebar--files-show
 :hide #'lispm-desktop-sidebar--files-hide)

(defun lispm-desktop-sidebar-current-view ()
  "Return the view the left sidebar shows, or nil when it is hidden."
  (car (seq-find (lambda (view) (lispm-desktop-sidebar--call (car view) :visible))
                 lispm-desktop-sidebar-views)))

(defvar lispm-desktop-sidebar-prepare-function nil
  "Function called before a view is shown or hidden, or nil.
The views belong to the editor; it brings the editor in front when a
view is asked for elsewhere, as on the desktop.")

(defvar lispm-desktop-sidebar-change-hook nil
  "Hook run after the sidebar's view changed.")

(defun lispm-desktop-sidebar-select (view)
  "Show VIEW in the left sidebar, or hide the sidebar when VIEW is shown.
Every other view is hidden first, so at most one is ever visible; see
`lispm-desktop-sidebar-prepare-function' for views asked for outside the editor.
Only a view shown where it is asked for hides: asked for on the desktop,
VIEW is shown, whatever the editor showed out of sight."
  ;; Read before the editor comes forward, which brings back the view it
  ;; showed when it was left.
  (let ((current (lispm-desktop-sidebar-current-view)))
    (when lispm-desktop-sidebar-prepare-function
      (funcall lispm-desktop-sidebar-prepare-function))
    (dolist (entry lispm-desktop-sidebar-views)
      (when (lispm-desktop-sidebar--call (car entry) :visible)
        (lispm-desktop-sidebar--call (car entry) :hide)))
    (unless (eq view current)
      (lispm-desktop-sidebar--call view :show)))
  (run-hooks 'lispm-desktop-sidebar-change-hook)
  (force-mode-line-update t))

(defun lispm-desktop-sidebar-files ()
  "Show the project tree in the left sidebar, or hide it when shown."
  (interactive)
  (lispm-desktop-sidebar-select 'files))

(defvar lispm-desktop-sidebar-focus-view 'agents
  "The view a hidden sidebar shows when the keyboard moves into it, see
`lispm-desktop-sidebar-focus'.")

(defun lispm-desktop-sidebar--window ()
  "Return the window of the left sidebar in the selected frame, or nil."
  (window-with-parameter 'window-side 'left))

(defun lispm-desktop-sidebar-focus ()
  "Move the keyboard into the left sidebar, or out of it to the main area.
The sidebar is left out of `other-window', as Emacs leaves side windows
out, so its list keys are reached this way; a hidden sidebar shows
`lispm-desktop-sidebar-focus-view' first."
  (interactive)
  (let ((window (lispm-desktop-sidebar--window)))
    (cond ((and window (eq window (selected-window)))
           (select-window (lispm-desktop-platinum-main-window)))
          (window (select-window window))
          (t (lispm-desktop-sidebar-select lispm-desktop-sidebar-focus-view)
             (when-let* ((window (lispm-desktop-sidebar--window)))
               (select-window window))))))

(defun lispm-desktop-sidebar--close-side-window (_window)
  "Hide the sidebar's view, from a side window's close box.
This is `lispm-desktop-platinum-side-window-close-function'."
  (when-let* ((view (lispm-desktop-sidebar-current-view)))
    (lispm-desktop-sidebar-select view)))

(defun lispm-desktop-sidebar--scroll-bar (buffer)
  "Say a scroll bar for BUFFER when it is one of the sidebar's lists.
This is one of `lispm-desktop-platinum-scroll-bar-functions'."
  (and (lispm-desktop-sidebar-buffer-view buffer) 'yes))

(defun lispm-desktop-setup-sidebar ()
  "Let the sidebar's views close from their windows' close boxes, its lists
scroll as Finder's did and follow a change of language, and the project
tree wear a list's toolbar; C-c s goes into the sidebar and back."
  (setq lispm-desktop-platinum-side-window-close-function #'lispm-desktop-sidebar--close-side-window)
  (add-hook 'lispm-desktop-platinum-scroll-bar-functions #'lispm-desktop-sidebar--scroll-bar)
  (add-hook 'lispm-desktop-language-changed-hook #'lispm-desktop-sidebar--refresh-lists)
  (add-hook 'treemacs-mode-hook #'lispm-desktop-sidebar--files-toolbar)
  ;; Into the sidebar and back: C-x o leaves side windows out.
  (global-set-key (kbd "C-c s") #'lispm-desktop-sidebar-focus))

(provide 'lispm-desktop-sidebar)
;;; lispm-desktop-sidebar.el ends here
