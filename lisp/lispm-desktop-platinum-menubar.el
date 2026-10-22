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
(require 'lispm-desktop-platinum-coach)
(require 'lispm-desktop-i18n)

(declare-function lispm-desktop-popup-menu "lispm-desktop-popup" (items &optional x y align))
(declare-function lispm-desktop-popup-alert "lispm-desktop-popup" (icon message buttons &optional detail))

(declare-function lispm-desktop-platinum-menu-status "lispm-desktop-platinum-status" ())

;;;; Menu bar

(defvar lispm-desktop-platinum--open-menu nil
  "The menu bar item whose menu is popped up, drawn highlighted meanwhile.")

(defvar lispm-desktop-platinum-menu-closed-hook nil
  "Hook run when a menu dropped from the menu bar closes, chosen or not.")

(defvar lispm-desktop-platinum--menu-bar-walk nil
  "Non-nil while the menu bar is walked from the keyboard, menu to menu:
the window that had the keyboard gets it back once the walk is over.")

(defmacro lispm-desktop-platinum-with-open-menu (key &rest body)
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

(cl-defgeneric lispm-desktop-platinum-draw-menu-title (palette width &key label highlighted corner marked)
  "Return an SVG of a stretch of the menu bar WIDTH pixels wide, in PALETTE.
The bar is `lispm-desktop-platinum-menu-bar-height' tall: a lit top line,
platinum, a shaded line and a black line along its bottom.  LABEL, a
string as Emacs would show it in the menu titles' face, its icons
included, is centred; HIGHLIGHTED draws it as an open menu's title, in
the variation colour, light on dark.  CORNER, `left' or `right', rounds
the screen's corner over that end.  MARKED circles the title with a
coach mark, see lispm-desktop-platinum-coach.el."
  (let* ((h lispm-desktop-platinum-menu-bar-height)
         (svg (svg-create width h))
         (grey (lambda (level) (lispm-desktop-platinum-grey palette level))))
    (lispm-desktop-platinum-svg-fill svg 0 0 width h (funcall grey #xDD))
    (lispm-desktop-platinum-svg-row svg 0 (1- width) 0 (funcall grey #xFF))
    (lispm-desktop-platinum-svg-row svg 0 (1- width) (- h 2) (funcall grey #x99))
    (lispm-desktop-platinum-svg-row svg 0 (1- width) (1- h) (plist-get palette :outline))
    (when highlighted
      (lispm-desktop-platinum-svg-fill svg 0 1 width (- h 3) (plist-get palette :highlight))
      (lispm-desktop-platinum-svg-row svg 0 (1- width) 0 (plist-get palette :thumb-dark))
      (lispm-desktop-platinum-svg-row svg 0 (1- width) (- h 2) (plist-get palette :highlight-dark)))
    (when label
      (lispm-desktop-platinum-svg-content
       svg label (/ (- width (lispm-desktop-platinum-content-width label 'lispm-desktop-platinum-menu-title)) 2)
       1 (- h 3)
       (plist-get palette (if highlighted :highlight-text :text))
       'lispm-desktop-platinum-menu-title))
    (when corner
      (let ((colors `((?# . ,(plist-get palette :outline)) (?5 . ,(funcall grey #x55))
                      (?A . ,(funcall grey #xAA)) (?D . ,(funcall grey #xDD))
                      (?W . ,(funcall grey (if (eq corner 'left) #xFF #xDD))))))
        (if (eq corner 'left)
            (progn
              (lispm-desktop-platinum-svg-column svg 0 8 (- h 3) (funcall grey #xFF))
              (lispm-desktop-platinum-svg-pixel-map svg 0 0 lispm-desktop-platinum--screen-corner colors))
          (lispm-desktop-platinum-svg-pixel-map
           svg (- width 9) 0
           (mapcar (lambda (row) (reverse (concat row (make-string (- 9 (length row)) ?\s))))
                   lispm-desktop-platinum--screen-corner)
           (cons '(?W . nil) colors)))))
    (when marked
      ;; A flat loop along the title, between the bar's lit top line and
      ;; its shaded bottom lines.
      (lispm-desktop-platinum-svg-coach-mark svg 2.5 2.5 (- width 5) (- h 7) 2 1))
    svg))

(defun lispm-desktop-platinum--menu-title (key label command help &optional active corner)
  "Return the menu bar item KEY showing LABEL, which runs COMMAND.
HELP describes it; ACTIVE draws it highlighted, as an open menu, the
highlight spanning the title's padding, as Mac OS 9 highlighted a whole
title.  The title is drawn as an image, so the bar's lines run under it;
CORNER rounds the screen's corner over it, see
`lispm-desktop-platinum-draw-menu-title'; a coach mark circles it while
it is what the mark circles, (menu . KEY).  The title string keeps LABEL
in its `lispm-desktop-platinum-label' property."
  (let* ((width (+ (lispm-desktop-platinum-content-width label 'lispm-desktop-platinum-menu-title)
                   (* 2 lispm-desktop-platinum-menu-padding)))
         (marked (lispm-desktop-platinum-coach-marked-p (cons 'menu key)))
         (title (if (lispm-desktop-tty-p)
                    (lispm-desktop-platinum--text-menu-title label active)
                  (propertize " " 'display
                              (lispm-desktop-platinum-image
                               (list 'menu-title (lispm-desktop-platinum--content-key label) width active corner
                                     marked)
                               (lambda ()
                                 (lispm-desktop-platinum-draw-menu-title lispm-desktop-platinum-current-palette width
                                                                         :label label :highlighted active
                                                                         :corner corner :marked marked)))
                              'lispm-desktop-platinum-label label
                              'lispm-desktop-platinum-highlighted active))))
    `(,key menu-item ,title ,command :help ,help)))

(defun lispm-desktop-platinum--text-menu-title (label active)
  "Return a menu bar title showing LABEL on a text terminal, a cell of the
bar either side of it; ACTIVE highlights it all, as an open menu's."
  (let ((title (concat " " label " ")))
    (add-face-text-property 0 (length title)
                            (if active 'lispm-desktop-platinum-menu-title-active
                              'lispm-desktop-platinum-menu-title)
                            t title)
    (propertize title
                'lispm-desktop-platinum-label label
                'lispm-desktop-platinum-highlighted active)))

(defun lispm-desktop-platinum--menu-filler (width)
  "Return the menu bar's stretch WIDTH pixels wide between its two ends,
or WIDTH columns on a text terminal."
  (list 'lispm-desktop-menu-filler 'menu-item
        (if (lispm-desktop-tty-p)
            (propertize (make-string width ?\s) 'face 'lispm-desktop-platinum-menu-title)
          (propertize " " 'display
                      (lispm-desktop-platinum-image
                       (list 'menu-title nil width nil nil nil)
                       (lambda () (lispm-desktop-platinum-draw-menu-title lispm-desktop-platinum-current-palette
                                                                          width)))))
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
bottom line, which the menu's outline then draws, as on Mac OS 9.  On a
text terminal the bar is a line of text, and the outline starts under it."
  (if (lispm-desktop-tty-p)
      (+ (or (frame-parameter nil 'menu-bar-lines) 0) (or (frame-parameter nil 'tab-bar-lines) 0))
    (1- (tab-bar-height nil t))))

(defvar lispm-desktop-platinum-lambda-items-function nil
  "Function returning the λ menu's items between About and Customize, as
`lispm-desktop-popup-menu' takes them: what the desktop runs, as Mac OS
9's Apple menu listed its applications and control panels.")

(defun lispm-desktop-platinum-lambda-items ()
  "Return the items of the λ menu, as `lispm-desktop-popup-menu' takes them.
See `lispm-desktop-platinum-lambda-items-function'."
  `((:label ,(lispm-desktop-tr "About LispM") :action lispm-desktop-platinum-about)
    separator
    ,@(and lispm-desktop-platinum-lambda-items-function
           (funcall lispm-desktop-platinum-lambda-items-function))
    (:label ,(lispm-desktop-tr "Customize lispm-desktop…")
            :action ,(lambda () (customize-group 'lispm-desktop)))))

(defun lispm-desktop-platinum-lambda-menu ()
  "Pop up the λ menu, LispM's counterpart of the Apple menu."
  (interactive)
  (lispm-desktop-platinum-with-open-menu 'lispm-desktop-lambda
    (lispm-desktop-popup-menu (lispm-desktop-platinum-lambda-items) (lispm-desktop-px 4)
                              (lispm-desktop-platinum--menu-y))))

(defun lispm-desktop-platinum-about ()
  "Tell which LispM and lispm-desktop this is, as About This Computer did."
  (interactive)
  (lispm-desktop-popup-alert
   'note (lispm-desktop-tr "LispM, a Lisp Machine for agents, inside Emacs.")
   `((,(lispm-desktop-tr "OK") ok default))
   (format "lispm-desktop, Emacs %s.  github.com/LispMachineLab/lispm-desktop" emacs-version)))

(defvar lispm-desktop-platinum-menus-function nil
  "Function returning the frontmost application's menus, or nil.
The menus follow the λ menu in the menu bar, as Mac OS 9's menu bar held
the frontmost application's: a list of (KEY TITLE ITEMS), ITEMS a function
returning the items `lispm-desktop-popup-menu' takes.  Without it the menu bar
holds the λ menu alone.")

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
      (lispm-desktop-platinum-with-open-menu key
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
application's menus, see `lispm-desktop-platinum-menus-function'."
  (let ((lambda-title (lispm-desktop-platinum--menu-title
                       'lispm-desktop-lambda (lispm-desktop-platinum--lambda) #'lispm-desktop-platinum-lambda-menu
                       "LispM" (eq lispm-desktop-platinum--open-menu 'lispm-desktop-lambda) 'left)))
    (cons
     lambda-title
     (when lispm-desktop-platinum-menus-function
       (lispm-desktop-platinum--application-menus (funcall lispm-desktop-platinum-menus-function)
                                                  (string-pixel-width (nth 2 lambda-title)))))))

(defun lispm-desktop-platinum-workspace-menu ()
  "Pop up the Application menu: the applications, see
`lispm-desktop-platinum-applications-function', or else the workspaces, the
current one checked."
  (interactive)
  (let ((current (tab-bar--current-tab-index))
        (tabs (funcall tab-bar-tabs-function)))
    (lispm-desktop-platinum-with-open-menu 'lispm-desktop-application
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
           (:label ,(lispm-desktop-tr "New Workspace") :action tab-bar-new-tab)
           (:label ,(lispm-desktop-tr "Close Workspace") :action tab-bar-close-tab :enabled ,(and (cdr tabs) t))))
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
                         (t (lispm-desktop-platinum-folder-icon
                             (car (rassoc name (bound-and-true-p tabspaces-project-tab-map))))))
                   " " (or (cdr application) name))
           #'lispm-desktop-platinum-workspace-menu (lispm-desktop-tr "Workspaces")
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
