;;; lispm-desktop-popup.el --- Mac OS 9's menus and alerts  -*- lexical-binding: t; -*-

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

;; LispM UI draws menus and alerts and reads their modal input.  The
;; desktop connects its menus to Emacs: keymaps and x-popup-menu's panes,
;; contextual menus from the mouse or keyboard, and the menu bar's walk.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-i18n)
(require 'lispm-ui-menu)
(require 'lispm-ui-ask)
(require 'lispm-ui-alert)

;;;; Emacs's menus

(defun lispm-desktop-popup--eval (form)
  "Return FORM's value, nil when it signals: a menu item's condition."
  (condition-case nil (eval form t) (error nil)))

(defun lispm-desktop-popup--keymap-item (binding &optional path)
  "Return the menu item of BINDING, an entry of a menu keymap, or nil.
PATH is the list of keys reaching BINDING, kept in the item's :path.  A
separator becomes `separator'; an item hidden by its :visible form, or an
entry that is not a menu item, nothing."
  (pcase binding
    ((pred stringp)
     (when (string-prefix-p "--" binding) 'separator))
    (`(menu-item ,name . ,rest)
     (let ((name (if (stringp name) name (lispm-desktop-popup--eval name)))
           (real (car rest))
           (props (cdr rest)))
       (cond
        ((and (plist-member props :visible)
              (not (lispm-desktop-popup--eval (plist-get props :visible))))
         nil)
        ((and (stringp name) (string-prefix-p "--" name)) 'separator)
        ((not (stringp name)) nil)
        (t
         (let* ((real (if-let* ((filter (plist-get props :filter)))
                          (funcall filter real)
                        real))
                (submenu (and real (keymapp real) (not (commandp real))
                              (lispm-desktop-popup--keymap-map real)))
                (button (plist-get props :button)))
           `(:label ,name
             :action ,(unless submenu real)
             :path ,path
             ,@(when submenu
                 `(:submenu ,(let ((map submenu))
                               (lambda () (lispm-desktop-popup-keymap-items map path)))))
             :enabled ,(and (or submenu real)
                            (or (not (plist-member props :enable))
                                (lispm-desktop-popup--eval (plist-get props :enable)))
                            t)
             ,@(when button `(:checked ,(and (lispm-desktop-popup--eval (cdr button)) t)))
             ;; Keys may be a function returning them, as Edit's Cut has.
             ,@(when-let* ((keys (plist-get props :keys))
                           (keys (if (functionp keys) (ignore-errors (funcall keys)) keys))
                           ((stringp keys)))
                 `(:key ,(substitute-command-keys keys)))))))))
    (`(,(and name (pred stringp)) . ,rest)
     (if (string-prefix-p "--" name)
         'separator
       (let ((real (if (stringp (car-safe rest)) (cdr rest) rest)))
         (lispm-desktop-popup--keymap-item `(menu-item ,name ,real) path))))))

(defun lispm-desktop-popup--keymap-map (binding)
  "Return BINDING as a keymap, following a symbol to its function."
  (let ((map (indirect-function binding)))
    (and (keymapp map) map)))

(defun lispm-desktop-popup-keymap-items (keymap &optional prefix)
  "Return the items of menu KEYMAP, as `lispm-ui-menu' takes them.
Items come in the keymap's order, as Emacs shows them, each with the
:path of keys reaching it after PREFIX; separators are not doubled nor
left at either end."
  (let ((items nil) (seen nil))
    (map-keymap (lambda (key binding)
                  (unless (memq key seen)
                    (push key seen)
                    (when-let* ((item (lispm-desktop-popup--keymap-item
                                       binding (append prefix (list key)))))
                      (push item items))))
                (lispm-desktop-popup--keymap-map keymap))
    (let ((cleaned nil))
      (dolist (item (nreverse items))
        (unless (and (eq item 'separator)
                     (or (null cleaned) (eq (car cleaned) 'separator)))
          (push item cleaned)))
      (when (eq (car cleaned) 'separator) (pop cleaned))
      (nreverse cleaned))))

(defun lispm-desktop-popup-frame-xy (position)
  "Return POSITION as (X . Y) in its frame: an event, a posn, or nil (the mouse)."
  (let ((posn (cond ((posnp position) position)
                    ;; A buffer position before an event: Emacs takes an
                    ;; integer for a character typed, too.
                    ((integerp position) (posn-at-point position))
                    ((eventp position) (event-start position))
                    (t nil))))
    (if-let* ((window (and posn (posn-window posn)))
              ((windowp window))
              (xy (posn-x-y posn)))
        (let ((edges (window-pixel-edges window))
              (body (window-body-pixel-edges window)))
          (cons (+ (car body) (car xy))
                (+ (if (memq (posn-area posn) '(mode-line header-line tab-line))
                       (nth 1 edges)
                     (nth 1 body))
                   (cdr xy))))
      (cdr (mouse-pixel-position)))))

(defun lispm-desktop-popup-keymap-menu (keymap &optional position)
  "Pop up menu KEYMAP at POSITION as a Mac OS 9 menu and run what is chosen.
POSITION is an event, a posn, a buffer position or nil for the mouse."
  (pcase-let ((`(,x . ,y) (lispm-desktop-popup-frame-xy position)))
    (lispm-ui-menu (lispm-desktop-popup-keymap-items keymap) (1+ x) (1+ y))))

(defun lispm-desktop-popup--local-right-click-p (position)
  "Return non-nil when a right click at POSITION has a binding of its own.
That is a [mouse-3] command of the buffer or text there, such as the
project tree's or the agents sidebar's menu, which then opens instead."
  (let ((binding (key-binding [mouse-3] t nil position)))
    (and binding (not (eq binding (keymap-lookup global-map "<mouse-3>"))))))

(defun lispm-desktop-popup-context-menu (event)
  "Pop up the context menu for mouse EVENT, as Mac OS 9's contextual menus.
The menu is Emacs's own, `context-menu-map', drawn as a Platinum menu.
Where the click has a right-click command of its own, that runs on the
release instead; where the context menu is empty, nothing opens."
  (interactive "e")
  (let* ((position (event-start event))
         (window (posn-window position)))
    (unless (lispm-desktop-popup--local-right-click-p position)
      (when (windowp window)
        (select-window window))
      (lispm-desktop-popup-keymap-menu (context-menu-map event) event))))

(defvar-local lispm-desktop-context-menu-function nil
  "Function popping up this buffer's right-click menu from the keyboard, or nil.
Windows whose items are drawn as images, the desktop's and the Finder's,
set it: their menu is of the item selected, which point does not mark.")

(defun lispm-desktop-popup-context-menu-at-point ()
  "Pop up the right-click menu of what point is on, as a right click there
would: the place's own menu where it has one, such as an agent's in the
sidebar, else Emacs's context menu.  Right clicks need the mouse, and no
key of the keyboard lispm-desktop is used from opens one."
  (interactive)
  (if lispm-desktop-context-menu-function
      (funcall lispm-desktop-context-menu-function)
    (let* ((position (or (posn-at-point) (user-error (lispm-desktop-tr "Point is not in sight"))))
           (local (and (lispm-desktop-popup--local-right-click-p position)
                       (key-binding [mouse-3] t nil position))))
      (if (commandp local)
          (call-interactively local nil (vector (list 'mouse-3 position)))
        (lispm-desktop-popup-keymap-menu (context-menu-map (list 'down-mouse-3 position)) position)))))

(defun lispm-desktop-popup--pane-items (menu)
  "Return the items of MENU, an `x-popup-menu' menu of panes.
MENU is (TITLE (PANE-TITLE ITEM...)...); an ITEM (STRING . VALUE)
returns VALUE when chosen, a lone STRING is a line that cannot be, and
one beginning with -- a separator.  Panes are parted by separators, and
titled when there are several."
  (let ((panes (cdr menu))
        (items nil))
    (dolist (pane panes)
      (when items (push 'separator items))
      (when (and (cdr panes) (stringp (car pane)) (not (string-empty-p (car pane))))
        (push (list :label (car pane) :enabled nil) items))
      (dolist (entry (cdr pane))
        (push (pcase entry
                ((pred stringp)
                 (if (or (string-empty-p entry) (string-prefix-p "--" entry))
                     'separator
                   (list :label entry :enabled nil)))
                (`(,(and label (pred stringp)) . ,value)
                 (list :label label :value value)))
              items)))
    (nreverse items)))

(defun lispm-desktop-popup--answer-paths (items)
  "Return ITEMS, from a menu keymap, each answering with its key path.
The items of their submenus answer the same way, when they open."
  (dolist (item items items)
    (when (consp item)
      (if-let* ((inner (copy-sequence item))
                ((plist-get item :submenu)))
          (plist-put item :submenu
                     (lambda () (lispm-desktop-popup--answer-paths (lispm-ui-menu--submenu inner))))
        (plist-put item :value (plist-get item :path))))))

(defun lispm-desktop-popup--x-menu-p (menu)
  "Return non-nil when MENU, as `x-popup-menu' takes it, can be drawn here:
a keymap, a list of keymaps, or a title and its panes."
  (or (keymapp menu)
      (and (consp menu) (keymapp (car menu)))
      (and (consp menu) (stringp (car menu)) (seq-every-p #'consp (cdr menu)))))

(defun lispm-desktop-popup--x-items (menu)
  "Return the items of MENU as `x-popup-menu' takes it.
Keymap items answer with their key path, pane items with their value."
  (cond ((keymapp menu)
         (lispm-desktop-popup--answer-paths (lispm-desktop-popup-keymap-items menu)))
        ((keymapp (car menu))
         (let ((items nil))
           (dolist (map menu)
             (let ((more (lispm-desktop-popup--x-items map)))
               (when (and items more) (push 'separator items))
               (setq items (append (reverse more) items))))
           (nreverse items)))
        (t (lispm-desktop-popup--pane-items menu))))

(defun lispm-desktop-popup--x-position (position)
  "Return POSITION, as `x-popup-menu' takes it, as (X . Y) in its frame.
Offsets from a window are taken from its text area, where `popup-menu'
measures a click or point."
  (pcase position
    ('t (cdr (mouse-pixel-position)))
    (`((,x ,y) ,(and window (pred windowp)))
     (let ((edges (window-body-pixel-edges window)))
       (cons (+ (car edges) x) (+ (nth 1 edges) y))))
    (`((,x ,y) ,(pred framep)) (cons x y))
    (_ (lispm-desktop-popup-frame-xy position))))

(defun lispm-desktop-popup--x-popup-menu (x-popup-menu position menu)
  "Draw MENU as a Mac OS 9 menu where `x-popup-menu' would draw a native one.
X-POPUP-MENU is the original, called without a POSITION (it then only
computes key equivalents), where no popup can be drawn, and for menus
it cannot read.  The value is what the original returns: the key path of
the item chosen from a keymap, the value of one chosen from panes, nil
when cancelled."
  (if (not (and position (lispm-ui-popup-available-p) (lispm-desktop-popup--x-menu-p menu)))
      (funcall x-popup-menu position menu)
    (pcase-let ((`(,x . ,y) (lispm-desktop-popup--x-position position)))
      (plist-get (lispm-ui-menu-choose (lispm-desktop-popup--x-items menu) (1+ x) (1+ y)) :value))))

(defun lispm-desktop-popup-enable ()
  "Draw right-click menus, and every menu `x-popup-menu' pops up, as Mac
OS 9's menus.  A right click opens Emacs's context menu, or the right-click
command of the place clicked."
  (keymap-global-set "<down-mouse-3>" #'lispm-desktop-popup-context-menu)
  (keymap-global-set "<mouse-3>" #'ignore)
  (keymap-global-set "C-c c" #'lispm-desktop-popup-context-menu-at-point)
  (advice-add 'x-popup-menu :around #'lispm-desktop-popup--x-popup-menu))

(provide 'lispm-desktop-popup)
;;; lispm-desktop-popup.el ends here
