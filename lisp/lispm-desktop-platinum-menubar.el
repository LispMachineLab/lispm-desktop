;;; lispm-desktop-platinum-menubar.el --- The menu bar  -*- lexical-binding: t; -*-

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

;; The menu bar, in the tab bar: the λ menu at its left end, the menus of
;; the application in front, and the Application menu at its right end;
;; walking it from the keyboard.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-platinum-titlebar)

(declare-function lispm-desktop-sidebar-current-view "lispm-desktop-sidebar" ())
(declare-function lispm-desktop-browser-workspace-p "lispm-desktop-browser" ())
(declare-function lispm-desktop-popup-menu "lispm-desktop-popup" (items &optional x y align))
(declare-function lispm-desktop-popup-alert "lispm-desktop-popup" (icon message buttons &optional detail))
(declare-function lispm-desktop-agent-available "lispm-desktop-ai" ())
(declare-function lispm-desktop-store-application-items "lispm-desktop-store" ())
(declare-function lispm-browser-available-p "lispm-browser" ())

(declare-function lispm-desktop-platinum-menu-status "lispm-desktop-platinum-status" ())
(defvar lispm-desktop-sidebar-views)


;;;; Menu bar

(defvar lispm-desktop-platinum--open-menu nil
  "The menu bar item whose menu is popped up, drawn highlighted meanwhile.")

(defvar lispm-desktop-platinum-menu-closed-hook nil
  "Hook run when a menu dropped from the menu bar closes, chosen or not.")

(defvar lispm-desktop-platinum--menu-bar-walk nil
  "Non-nil while the menu bar is walked from the keyboard, menu to menu:
the window that had the keyboard gets it back once the walk is over.")

(defmacro lispm-desktop-platinum--with-open-menu (key &rest body)
  "Run BODY, which pops up a menu, with menu bar item KEY highlighted."
  (declare (indent 1))
  `(let ((lispm-desktop-platinum--open-menu ,key))
     (force-mode-line-update t)
     (redisplay t)
     (unwind-protect (progn ,@body)
       (force-mode-line-update t)
       (unless lispm-desktop-platinum--menu-bar-walk
         (run-hooks 'lispm-desktop-platinum-menu-closed-hook)))))

(defconst lispm-desktop-platinum-menu-padding 10
  "Blank pixels on either side of a menu bar title.")

(defconst lispm-desktop-platinum--screen-corner
  '("#####5ADW" "###5AWWW" "##5DW" "#5DW" "#AW" "5W" "AW" "DW")
  "The screen's top left corner, rounded over the menu bar's left end, a
row of pixels a string: black (#) softened in greys (5, A, D) into the
bar's lit edges (W).  The right corner is its mirror.")

(cl-defun lispm-desktop-platinum-draw-menu-title (palette width &key label highlighted corner)
  "Return an SVG of a stretch of the menu bar WIDTH pixels wide, in PALETTE.
The bar is `lispm-desktop-platinum-menu-bar-height' tall: a lit top line,
platinum, a shaded line and a black line along its bottom.  LABEL, a
string as Emacs would show it in the menu titles' face, its icons
included, is centred; HIGHLIGHTED draws it as an open menu's title, in
the variation colour, light on dark.  CORNER, `left' or `right', rounds
the screen's corner over that end."
  (let* ((h lispm-desktop-platinum-menu-bar-height)
         (svg (svg-create width h))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level))))
    (lispm-desktop-platinum--fill svg 0 0 width h (funcall grey #xDD))
    (lispm-desktop-platinum--row svg 0 (1- width) 0 (funcall grey #xFF))
    (lispm-desktop-platinum--row svg 0 (1- width) (- h 2) (funcall grey #x99))
    (lispm-desktop-platinum--row svg 0 (1- width) (1- h) (plist-get palette :outline))
    (when highlighted
      (lispm-desktop-platinum--fill svg 0 1 width (- h 3) (plist-get palette :highlight))
      (lispm-desktop-platinum--row svg 0 (1- width) 0 (plist-get palette :thumb-dark))
      (lispm-desktop-platinum--row svg 0 (1- width) (- h 2) (plist-get palette :highlight-dark)))
    (when label
      (lispm-desktop-platinum--draw-content
       svg label (/ (- width (lispm-desktop-platinum--content-width label 'lispm-desktop-platinum-menu-title)) 2)
       1 (- h 3)
       (plist-get palette (if highlighted :highlight-text :text))
       'lispm-desktop-platinum-menu-title))
    (when corner
      (let ((colors `((?# . ,(plist-get palette :outline)) (?5 . ,(funcall grey #x55))
                      (?A . ,(funcall grey #xAA)) (?D . ,(funcall grey #xDD))
                      (?W . ,(funcall grey (if (eq corner 'left) #xFF #xDD))))))
        (if (eq corner 'left)
            (progn
              (lispm-desktop-platinum--column svg 0 8 (- h 3) (funcall grey #xFF))
              (lispm-desktop-platinum--draw-map svg 0 0 lispm-desktop-platinum--screen-corner colors))
          (lispm-desktop-platinum--draw-map
           svg (- width 9) 0
           (mapcar (lambda (row) (reverse (concat row (make-string (- 9 (length row)) ?\s))))
                   lispm-desktop-platinum--screen-corner)
           (cons '(?W . nil) colors)))))
    svg))

(defun lispm-desktop-platinum--menu-title (key label command help &optional active corner)
  "Return the menu bar item KEY showing LABEL, which runs COMMAND.
HELP describes it; ACTIVE draws it highlighted, as an open menu, the
highlight spanning the title's padding, as Mac OS 9 highlighted a whole
title.  The title is drawn as an image, so the bar's lines run under it;
CORNER rounds the screen's corner over it, see
`lispm-desktop-platinum-draw-menu-title'.  The title string keeps LABEL
in its `lispm-desktop-platinum-label' property."
  (let* ((width (+ (lispm-desktop-platinum--content-width label 'lispm-desktop-platinum-menu-title)
                   (* 2 lispm-desktop-platinum-menu-padding)))
         (title (propertize " " 'display
                            (lispm-desktop-platinum--image
                             (list 'menu-title (lispm-desktop-platinum--content-key label) width active corner)
                             (lambda ()
                               (lispm-desktop-platinum-draw-menu-title lispm-desktop-platinum--palette width
                                                                       :label label :highlighted active
                                                                       :corner corner)))
                            'lispm-desktop-platinum-label label
                            'lispm-desktop-platinum-highlighted active)))
    `(,key menu-item ,title ,command :help ,help)))

(defun lispm-desktop-platinum--menu-filler (width)
  "Return the menu bar's stretch WIDTH pixels wide between its two ends."
  (list 'lispm-desktop-menu-filler 'menu-item
        (propertize " " 'display
                    (lispm-desktop-platinum--image
                     (list 'menu-title nil width nil nil)
                     (lambda () (lispm-desktop-platinum-draw-menu-title lispm-desktop-platinum--palette
                                                                        width))))
        #'ignore))

(defun lispm-desktop-platinum-menu-bar-line ()
  "Return the whole menu bar, as `tab-bar-format' takes it: the λ menu and
the frontmost application's menus at its left end, the status and the
Application menu at its right, and the bar's own stretch between them,
so its lines run from edge to edge."
  (let* ((left (lispm-desktop-platinum-menu-bar))
         (right (append (lispm-desktop-platinum-menu-status) (lispm-desktop-platinum-application-menu)))
         (used (apply #'+ (mapcar (lambda (item) (string-pixel-width (nth 2 item))) (append left right)))))
    (append left (list (lispm-desktop-platinum--menu-filler (max 0 (- (frame-inner-width) used)))) right)))

(defun lispm-desktop-platinum--lambda ()
  "Return LispM's rainbow λ, the counterpart of Mac OS 9's rainbow Apple.
Where images cannot be shown, a plain λ stands in."
  (if (display-images-p) (lispm-desktop-platinum-icon 'lambda) "λ"))

(defvar lispm-desktop-popup-menu-bar-step)

(defvar lispm-desktop-theme)

(defun lispm-desktop-platinum--menu-y ()
  "Return where a menu dropped from the menu bar starts: on the bar's black
bottom line, which the menu's outline then draws, as on Mac OS 9."
  (1- (tab-bar-height nil t)))

(defun lispm-desktop-platinum-lambda-items ()
  "Return the items of the λ menu, as `lispm-desktop-popup-menu' takes them."
  `((:label "About LispM" :action lispm-desktop-platinum-about)
    separator
    ;; A new session of each agent that can run here, those built in first.
    ,@(mapcar (pcase-lambda (`(,_ . ,props))
                (list :label (format "New %s Session" (plist-get props :name))
                      :action (plist-get props :command)))
              (lispm-desktop-agent-available))
    (:label "Resume Session…" :action lispm-desktop-agent-sessions)
    (:label "Send Region to Agent" :action lispm-desktop-agent-send-region :enabled ,(use-region-p))
    separator
    (:label "New Shell" :action lispm-desktop-shell-new)
    ,@(when (lispm-desktop-platinum--browser-p) '((:label "Browser" :action lispm-desktop-browser-show)))
    (:label "Magit Status" :action magit-status)
    separator
    ,@(when (fboundp 'lispm-desktop-store-show)
        '((:label "App Store" :action lispm-desktop-store-show)))
    ,@(when (and (fboundp 'lispm-desktop-store-application-items)
                 (lispm-desktop-store-application-items))
        '((:label "Applications" :submenu lispm-desktop-store-application-items)))
    ,@(when (fboundp 'lispm-desktop-settings-panel-items)
        `((:label "Control Panels" :submenu lispm-desktop-settings-panel-items)))
    (:label "Customize lispm-desktop…" :action ,(lambda () (customize-group 'lispm-desktop)))))

(defun lispm-desktop-platinum-lambda-menu ()
  "Pop up the λ menu, LispM's counterpart of the Apple menu."
  (interactive)
  (lispm-desktop-platinum--with-open-menu 'lispm-desktop-lambda
    (lispm-desktop-popup-menu (lispm-desktop-platinum-lambda-items) 4 (lispm-desktop-platinum--menu-y))))

(defun lispm-desktop-platinum-about ()
  "Tell which LispM and lispm-desktop this is, as About This Computer did."
  (interactive)
  (lispm-desktop-popup-alert
   'note "LispM, a Lisp Machine for agents, inside Emacs."
   '(("OK" ok default))
   (format "lispm-desktop, Emacs %s.  github.com/LispMachineLab/lispm-desktop" emacs-version)))

(defun lispm-desktop-platinum--browser-p ()
  "Return non-nil when the embedded browser can run in this Emacs."
  (and (fboundp 'lispm-browser-available-p) (lispm-browser-available-p)))

(defvar lispm-desktop-platinum-menus-function nil
  "Function returning the frontmost application's menus, or nil.
The menus follow the λ menu in the menu bar, as Mac OS 9's menu bar held
the frontmost application's: a list of (KEY TITLE ITEMS), ITEMS a function
returning the items `lispm-desktop-popup-menu' takes.  Without it the menu bar
holds a title per sidebar view, and Web.")

(defvar lispm-desktop-platinum-application-function nil
  "Function returning the frontmost application as (ICON . NAME), or nil.
The Application menu names it; without it, the workspace is named.")

(defvar lispm-desktop-platinum-applications-function nil
  "Function returning the Application menu's items, or nil for the workspaces.")

(defun lispm-desktop-platinum--application-menu-command (key items left)
  "Return a command dropping menu KEY, whose ITEMS a function returns.
LEFT is the pixel position of its title's left edge, where the menu drops.
A function whose `lispm-desktop-platinum-panel' property is set drops a panel of
its own instead, called with KEY, LEFT and the menu bar's bottom."
  (lambda ()
    (interactive)
    (if (and (symbolp items) (get items 'lispm-desktop-platinum-panel))
        (funcall items key left (lispm-desktop-platinum--menu-y))
      (lispm-desktop-platinum--with-open-menu key
        (lispm-desktop-popup-menu (funcall items) left (lispm-desktop-platinum--menu-y))))))

(defun lispm-desktop-platinum--application-menus (menus left)
  "Return the menu bar titles of MENUS, the first starting LEFT pixels in.
MENUS is a value of `lispm-desktop-platinum-menus-function'."
  (mapcar (pcase-lambda (`(,key ,title ,items))
            (let ((item (lispm-desktop-platinum--menu-title
                         key title (lispm-desktop-platinum--application-menu-command key items left)
                         title (eq lispm-desktop-platinum--open-menu key))))
              (setq left (+ left (string-pixel-width (nth 2 item))))
              item))
          menus))

(defun lispm-desktop-platinum-menu-bar ()
  "Return the left end of the menu bar: the λ menu, then the frontmost
application's menus, see `lispm-desktop-platinum-menus-function'; without one, a
title per view, highlighted while the sidebar shows it, and Web, while the
browser is in front."
  (let ((lambda-title (lispm-desktop-platinum--menu-title
                       'lispm-desktop-lambda (lispm-desktop-platinum--lambda) #'lispm-desktop-platinum-lambda-menu
                       "LispM" (eq lispm-desktop-platinum--open-menu 'lispm-desktop-lambda) 'left)))
    (cons
     lambda-title
     (if lispm-desktop-platinum-menus-function
         (lispm-desktop-platinum--application-menus (funcall lispm-desktop-platinum-menus-function)
                                                    (string-pixel-width (nth 2 lambda-title)))
       (let ((shown (lispm-desktop-sidebar-current-view)))
         (append
          (mapcar (lambda (view)
                    (let ((spec (cdr view)))
                      (lispm-desktop-platinum--menu-title (intern (format "lispm-desktop-view-%s" (car view)))
                                                          (plist-get spec :title)
                                                          (plist-get spec :command)
                                                          (plist-get spec :help)
                                                          (eq (car view) shown))))
                  lispm-desktop-sidebar-views)
          (when (lispm-desktop-platinum--browser-p)
            (list (lispm-desktop-platinum--menu-title 'lispm-desktop-view-web "Web" 'lispm-desktop-browser "Browser"
                                                      (lispm-desktop-browser-workspace-p))))))))))

(defun lispm-desktop-platinum-workspace-menu ()
  "Pop up the Application menu: the applications, see
`lispm-desktop-platinum-applications-function', or else the workspaces, the
current one checked."
  (interactive)
  (let ((current (tab-bar--current-tab-index))
        (tabs (funcall tab-bar-tabs-function)))
    (lispm-desktop-platinum--with-open-menu 'lispm-desktop-application
      (lispm-desktop-popup-menu
       (if lispm-desktop-platinum-applications-function
           (funcall lispm-desktop-platinum-applications-function)
         `(,@(seq-map-indexed
              (lambda (tab index)
                (list :label (alist-get 'name tab)
                      :action (lambda () (tab-bar-select-tab (1+ index)))
                      :checked (= index current)))
              tabs)
           separator
           (:label "New Workspace" :action tab-bar-new-tab)
           (:label "Close Workspace" :action tab-bar-close-tab :enabled ,(and (cdr tabs) t))))
       (- (frame-pixel-width) 4) (lispm-desktop-platinum--menu-y) 'right))))

(defvar tabspaces-default-tab)

(defun lispm-desktop-platinum-application-menu ()
  "Return the right end of the menu bar: the Application menu.
It names the current workspace with its icon, as Mac OS 9's named the
frontmost application: the hard disk for the default workspace, a
project's folder for a project's.  Clicked, it lists every workspace."
  (let ((name (alist-get 'name (tab-bar--current-tab)))
        (application (and lispm-desktop-platinum-application-function
                          (funcall lispm-desktop-platinum-application-function))))
    (list (lispm-desktop-platinum--menu-title
           'lispm-desktop-application
           (concat (cond (application (car application))
                         ((equal name (bound-and-true-p tabspaces-default-tab))
                          (lispm-desktop-platinum-icon "disk"))
                         ((and (fboundp 'lispm-desktop-browser-workspace-p) (lispm-desktop-browser-workspace-p))
                          (lispm-desktop-platinum-icon "netscape"))
                         (t (lispm-desktop-platinum-folder-icon
                             (car (rassoc name (bound-and-true-p tabspaces-project-tab-map))))))
                   " " (or (cdr application) name))
           #'lispm-desktop-platinum-workspace-menu "Workspaces"
           (eq lispm-desktop-platinum--open-menu 'lispm-desktop-application) 'right))))

(defun lispm-desktop-platinum--menu-bar-menus ()
  "Return the menus of the menu bar, left to right, as (KEY . COMMAND):
the λ menu, the frontmost application's menus and the Application menu."
  (mapcar (lambda (item) (cons (car item) (nth 3 item)))
          (append (if lispm-desktop-platinum-menus-function
                      (lispm-desktop-platinum-menu-bar)
                    (list (car (lispm-desktop-platinum-menu-bar))))
                  (lispm-desktop-platinum-application-menu))))

(defun lispm-desktop-platinum-menu-bar-open ()
  "Open the menu bar from the keyboard: the λ menu drops with its first
item highlighted.  Up and Down move through a menu, typing a letter goes
to the item it starts, Left and Right go to the menu beside, Return
chooses and Escape closes, so every menu is reached without the mouse.
The menus drop from the screen's menu bar, as when it is clicked,
whichever window has the keyboard, and it is given back once they close;
from a window out, its screen hidden in the Dock, they drop from the top
of that window."
  (interactive)
  (let ((screen (selected-frame)))
    (while (frame-parent screen)
      (setq screen (frame-parent screen)))
    (when-let* ((out (frame-parameter screen 'lispm-desktop-out))
                ((frame-live-p out)))
      (setq screen (if (frame-parameter out 'lispm-desktop-hidden) (selected-frame) out)))
    (unless (eq (selected-frame) screen)
      (select-frame-set-input-focus screen)))
  (let ((menus (lispm-desktop-platinum--menu-bar-menus))
        (index 0)
        step)
    (unwind-protect
        (let ((lispm-desktop-platinum--menu-bar-walk t))
          (while (setq step (catch 'lispm-desktop-popup-menu-bar-step
                              (let ((lispm-desktop-popup-menu-bar-step t))
                                (call-interactively (cdr (nth index menus))))
                              nil))
            (setq index (mod (+ index step) (length menus)))))
      (run-hooks 'lispm-desktop-platinum-menu-closed-hook))))

(provide 'lispm-desktop-platinum-menubar)
;;; lispm-desktop-platinum-menubar.el ends here
