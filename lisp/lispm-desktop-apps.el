;;; lispm-desktop-apps.el --- The applications on the desktop  -*- lexical-binding: t; -*-

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

;; Mac OS 9 ran applications over its desktop, one of them in front at a
;; time: its menus filled the menu bar after the Apple menu, and the
;; Application menu at the right end named it and listed the others.
;; Here the applications are the desktop itself, Emacs the editor and the
;; browser:
;;
;;   desktop   the desktop's workspace: the desktop alone
;;   editor    the project workspaces, full screen or in a floating window
;;   browser   the browser's workspace
;;
;; Which one is in front is kept here, as the menu bar is drawn from it;
;; clicking the menu bar, which focuses the frame under it, does not change
;; it.
;;
;; The applications' windows float over the desktop, child frames of the
;; screen, Emacs's frame the desktop is drawn in; one can also be out on
;; the screen of the computer, a frame of its own that still belongs to
;; the desktop, see lispm-desktop-coherence.el.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-i18n)

(defvar lispm-desktop-apps
  `((desktop :name ,(lispm-desktop-tr-noop "Desktop") :icon "disk" :menus lispm-desktop-menus
             :activate lispm-desktop-activate)
    (editor :name "Emacs" :icon emacs :menus lispm-desktop-editor-menus
            :activate lispm-desktop-editor-activate)
    (browser :name ,(lispm-desktop-tr-noop "Browser") :icon "netscape" :menus lispm-desktop-browser-menus
             :activate lispm-desktop-browser-show :visible lispm-browser-available-p
             :running lispm-browser-tabs)
    (settings :name ,(lispm-desktop-tr-noop "Control Panels") :icon "control-panels"
              :menus lispm-desktop-settings-menus
              :activate lispm-desktop-settings-show :visible lispm-desktop-settings-open-p))
  "The applications, in the Application menu's order: (KEY . PROPERTIES).
:name and :icon (see `lispm-desktop-platinum-icon-file') name an application,
:name in English, translated where shown, see `lispm-desktop-apps-name';
:menus is a function returning its menus as
`lispm-desktop-platinum-menus-function' does, :activate brings it in front,
:visible, when given, is a function deciding whether it can run here, and
:running one deciding whether it runs now; without it, an application runs
whenever it can.  :folder, a large icon as :icon is a small one, makes it
an application built in: the Applications folder holds it, and the λ
menu's Applications lists it, whether it runs or not.  The functions are
symbols or lambdas; an application whose function is not defined does not
run.  Applications installed later are added with `lispm-desktop-define-app'.")

(defvar lispm-desktop-apps--frontmost nil
  "The application in front, a key of `lispm-desktop-apps', or nil at first.")

(defun lispm-desktop-apps-frontmost ()
  "Return the application in front, a key of `lispm-desktop-apps'."
  (or lispm-desktop-apps--frontmost 'desktop))

(defvar lispm-desktop-apps-frontmost-functions nil
  "Functions called with the application in front whenever it is set,
changed or not, as windows follow it: the desktop's Finder window shows
with the desktop and hides from the full-screen editor.")

(defun lispm-desktop-apps-set-frontmost (app)
  "Put APP, a key of `lispm-desktop-apps', in front; redraw the menu bar."
  (unless (eq app lispm-desktop-apps--frontmost)
    (setq lispm-desktop-apps--frontmost app)
    (force-mode-line-update t))
  (run-hook-with-args 'lispm-desktop-apps-frontmost-functions app))

(declare-function lispm-desktop-editor-floating-p "lispm-desktop-editor" ())
(defvar lispm-desktop-browser-floating)

(defun lispm-desktop-apps-full-screen-p (app)
  "Return non-nil when APP, in front, fills the screen and covers the
floating windows: the editor or the browser, unless it floats itself.
The floating windows show while any other application is in front."
  (pcase app
    ('editor (not (and (fboundp 'lispm-desktop-editor-floating-p) (lispm-desktop-editor-floating-p))))
    ('browser (not (bound-and-true-p lispm-desktop-browser-floating)))))

(defun lispm-desktop-define-app (key &rest properties)
  "Make KEY an application with PROPERTIES, as `lispm-desktop-apps' holds one.
An application defined again keeps its place in the Application menu; a
new one comes after the others, as installed from the App Store.  The
list is copied, never changed in place.  Return KEY."
  (setq lispm-desktop-apps
        (if (assq key lispm-desktop-apps)
            (mapcar (lambda (app) (if (eq (car app) key) (cons key properties) app))
                    lispm-desktop-apps)
          (append lispm-desktop-apps (list (cons key properties)))))
  (force-mode-line-update t)
  key)

(defun lispm-desktop-remove-app (key)
  "Remove the application KEY; the desktop comes in front if it was there."
  (setq lispm-desktop-apps (seq-remove (lambda (app) (eq (car app) key)) lispm-desktop-apps))
  (when (eq lispm-desktop-apps--frontmost key)
    (lispm-desktop-apps-set-frontmost 'desktop))
  (force-mode-line-update t))

(defun lispm-desktop-apps-out-p (frame)
  "Return the screen FRAME belongs to when FRAME is out on the computer's
screen, a frame of its own rather than a child of the screen, or nil.
Windows out carry it as their frame parameter `lispm-desktop-out', and
so does the minibuffer they share."
  (let ((screen (frame-parameter frame 'lispm-desktop-out)))
    (and (frame-live-p screen) screen)))

(defun lispm-desktop-apps-screen-p (frame)
  "Return non-nil when FRAME is a screen, the frame the desktop is drawn
in: one of its own that is neither a floating window out on the
computer's screen nor the minibuffer they share."
  (not (or (frame-parent frame) (frame-parameter frame 'lispm-desktop-window)
           (lispm-desktop-apps-out-p frame) (lispm-desktop-platinum-chrome-frame-p frame))))

(defun lispm-desktop-apps-window-screen (frame)
  "Return the screen FRAME, a floating window, belongs to: the one it floats
over, or the screen it left, when it is out."
  (or (lispm-desktop-platinum-window-parent frame) (lispm-desktop-apps-out-p frame)))

(defun lispm-desktop-apps-screen ()
  "Return the frame the desktop and the applications are drawn in: the
selected frame's outermost parent, or the screen a window out belongs to."
  (let ((frame (selected-frame)))
    (while (frame-parent frame)
      (setq frame (frame-parent frame)))
    (or (lispm-desktop-apps-out-p frame) frame)))

(defun lispm-desktop-apps-hidden-p (&optional screen)
  "Return non-nil while SCREEN, by default the selected one, is hidden in the
Dock, its windows out on the computer's screen."
  (frame-parameter (or screen (lispm-desktop-apps-screen)) 'lispm-desktop-hidden))

(defun lispm-desktop-apps-windows (&optional screen)
  "Return the floating windows of SCREEN: child frames marked as windows,
and those of its windows that are out.  They carry the frame parameter
`lispm-desktop-window'."
  (let ((screen (or screen (lispm-desktop-apps-screen))))
    (seq-filter (lambda (frame)
                  (and (eq (lispm-desktop-apps-window-screen frame) screen)
                       (frame-parameter frame 'lispm-desktop-window)))
                (frame-list))))

(defun lispm-desktop-apps--screen-origin (screen)
  "Return where SCREEN's inside starts on the computer's screen: (X . Y).
A child frame's position counts from there."
  (pcase-let ((`(,left ,top . ,_) (frame-edges screen 'native-edges)))
    (cons left top)))

(defun lispm-desktop-apps--geometry (frame)
  "Return where FRAME is: (LEFT TOP TEXT-WIDTH TEXT-HEIGHT) in pixels, over
its screen, whether it floats there or is out."
  (pcase-let ((`(,left . ,top) (lispm-desktop-platinum-window-position frame)))
    (when-let* ((screen (and (not (lispm-desktop-platinum-window-parent frame))
                             (lispm-desktop-apps-out-p frame))))
      (pcase-let ((`(,x . ,y) (lispm-desktop-apps--screen-origin screen)))
        (setq left (- left x) top (- top y))))
    ;; A window rolled up is remembered at the height it rolls down to.
    (list left top (frame-text-width frame)
          (or (lispm-desktop-platinum-rolled-up-height frame) (frame-text-height frame)))))

(defun lispm-desktop-apps--workarea (frame)
  "Return the part of the computer's screen windows may use where FRAME is:
\(X Y WIDTH HEIGHT), the menu bar and the Dock left out."
  (or (frame-monitor-workarea frame) (list 0 0 (display-pixel-width) (display-pixel-height))))

(defun lispm-desktop-apps--keep-on-screen (frame)
  "Keep FRAME, a window out, where its title bar can still be reached: its
top under the menu bar, and some of it left on the screen's sides.  The
frame carrying it moves, see `lispm-desktop-platinum-window-outer'."
  (pcase-let* ((outer (lispm-desktop-platinum-window-outer frame))
               (`(,x ,y ,w ,h) (lispm-desktop-apps--workarea frame))
               (`(,left . ,top) (frame-position outer))
               (width (frame-pixel-width outer))
               (keep 80))
    (let ((new-left (max (- (+ x keep) width) (min left (- (+ x w) keep))))
          (new-top (max y (min top (- (+ y h) 30)))))
      (unless (and (= new-left left) (= new-top top))
        (set-frame-position outer new-left new-top)))))

(declare-function lispm-desktop-coherence-take-out "lispm-desktop-coherence" (frame))

(defun lispm-desktop-apps--in-order (windows frames)
  "Return those of WINDOWS among FRAMES, in the order of FRAMES, front
first; a frame drawing a window's frame stands for the window."
  (seq-uniq (seq-filter (lambda (frame) (memq frame windows))
                        (mapcar (lambda (frame) (or (lispm-desktop-platinum-window-frame-p frame) frame))
                                frames))))

(defun lispm-desktop-apps-windows-out-in-order (screen)
  "Return the windows of SCREEN out on the computer's screen, the one in
front first.  The window system orders the frames carrying them."
  (lispm-desktop-apps--in-order (lispm-desktop-apps-windows screen) (frame-list-z-order)))

(defun lispm-desktop-apps-windows-in-order (screen)
  "Return the windows floating over SCREEN, the one in front first.
The window system orders the frames carrying them."
  (lispm-desktop-apps--in-order
   (seq-filter (lambda (frame) (eq (lispm-desktop-platinum-window-parent frame) screen))
               (lispm-desktop-apps-windows screen))
   (frame-list-z-order screen)))

(defun lispm-desktop-apps-focus-after (frame)
  "Give the keyboard to what comes after FRAME, a floating window going
away: the window shown right behind it, as on Mac OS 9 the next window
came active as the one in front closed, its application in front with
it; with no window left, the screen.  While the screen is hidden, the
windows are those out on the computer's screen, and with none left the
keyboard stays where it is.  Return the window given the keyboard, or
nil."
  (let ((screen (lispm-desktop-apps-window-screen frame)))
    (when (frame-live-p screen)
      (let* ((hidden (lispm-desktop-apps-hidden-p screen))
             (next (seq-find (lambda (other)
                               (and (not (eq other frame)) (frame-visible-p other)))
                             (if hidden
                                 (lispm-desktop-apps-windows-out-in-order screen)
                               (lispm-desktop-apps-windows-in-order screen)))))
        (cond (next
               (select-frame-set-input-focus next)
               (when-let* ((app (frame-parameter next 'lispm-desktop-app)))
                 (lispm-desktop-apps-set-frontmost app))
               next)
              ((not hidden)
               (select-frame-set-input-focus screen)
               nil))))))

(defun lispm-desktop-apps-desktop-edges (screen)
  "Return where SCREEN's desktop lies, (LEFT TOP RIGHT BOTTOM) in pixels,
counted from the screen's inside as a child frame's position is: under
the menu bar, across the screen, down to the echo area."
  ;; The root window of a screen split in windows is not live, and only a
  ;; live window has a divider: the bottom windows' is the one.
  (pcase-let ((`(,left ,top ,right ,bottom) (window-pixel-edges (frame-root-window screen))))
    (list left top right
          (- bottom (window-bottom-divider-width (car (window-at-side-list screen 'bottom)))))))

(defun lispm-desktop-apps--put (frame place)
  "Put floating FRAME at PLACE, kept whole on its screen's desktop with the
frame drawn round it, see `lispm-desktop-platinum-window-margins': under
the menu bar, above the echo area, and no larger than the desktop.  PLACE
is a value of `lispm-desktop-apps--geometry'.
On macOS a child frame is not clipped to its parent, so a window let past
the desktop's edge would hang out of Emacs's frame over the screen."
  (when-let* (((frame-live-p frame))
              (screen (lispm-desktop-platinum-window-parent frame)))
    (pcase-let* ((`(,left ,top ,width ,height) place)
                 (`(,desktop-left ,desktop-top ,desktop-right ,desktop-bottom)
                  (lispm-desktop-apps-desktop-edges screen))
                 ;; Its outline, band and shadow, none while it is not framed.
                 (`(,frame-left ,frame-top ,frame-right ,frame-bottom)
                  (lispm-desktop-platinum-window-margins frame))
                 ;; What the window's own frame adds around its text: borders, mostly.
                 (extra-width (- (frame-pixel-width frame) (frame-text-width frame)))
                 (extra-height (- (frame-pixel-height frame) (frame-text-height frame)))
                 ;; At least 200 by 100 pixels, or 25 cells by 6 lines on
                 ;; a text terminal.
                 (tty (lispm-desktop-tty-p screen))
                 (fit-width (max (if tty 25 200)
                                 (min width (- desktop-right desktop-left
                                               frame-left extra-width frame-right))))
                 ;; A window rolled up keeps to its title bar.
                 (fit-height (if (lispm-desktop-platinum-rolled-up-height frame)
                                 (frame-text-height frame)
                               (max (if tty 6 100)
                                    (min height (- desktop-bottom desktop-top
                                                   frame-top extra-height frame-bottom))))))
      (unless (and (= fit-width (frame-text-width frame))
                   (= fit-height (frame-text-height frame)))
        (set-frame-size frame fit-width fit-height t)
        (lispm-desktop-platinum-frame-update frame))
      (let ((x (max (+ desktop-left frame-left)
                    (min left (- desktop-right frame-right extra-width fit-width))))
            (y (max (+ desktop-top frame-top)
                    (min top (- desktop-bottom frame-bottom extra-height fit-height)))))
        (unless (equal (cons x y) (lispm-desktop-platinum-window-position frame))
          (lispm-desktop-platinum-move-window frame x y))))))

(defun lispm-desktop-apps-keep-on-desktop (frame)
  "Keep floating FRAME on the desktop where it was just put or moved, and
remember that place: the window system moves child frames on its own when
their screen changes size, so they are put back there afterwards.  A
window out is kept on the computer's screen instead, and one shown while
its screen is hidden goes out there at once."
  (when (frame-live-p frame)
    (when (and (lispm-desktop-platinum-window-parent frame)
               (lispm-desktop-apps-hidden-p (lispm-desktop-platinum-window-parent frame))
               (fboundp 'lispm-desktop-coherence-take-out))
      (lispm-desktop-coherence-take-out frame))
    (if (lispm-desktop-apps-out-p frame)
        (lispm-desktop-apps--keep-on-screen frame)
      (lispm-desktop-apps--put frame (lispm-desktop-apps--geometry frame))
      (set-frame-parameter frame 'lispm-desktop-window-place (lispm-desktop-apps--geometry frame)))))

(defun lispm-desktop-apps--place-windows (screen &optional again)
  "Put the floating windows of SCREEN back in their places, each kept on the
desktop and given back its own size where there is room.  AGAIN forces
the window system to place them anew and shows them again: going full
screen or back, macOS moves child windows its own way and may leave them
out of sight while Emacs takes them for shown."
  (when (frame-live-p screen)
    (dolist (window (seq-filter #'lispm-desktop-platinum-window-parent
                                (lispm-desktop-apps-windows screen)))
      (let ((place (or (frame-parameter window 'lispm-desktop-window-place)
                       (lispm-desktop-apps--geometry window))))
        (when again
          ;; A move to where Emacs takes the window to be would be skipped.
          (lispm-desktop-platinum-move-window window (1+ (car place)) (cadr place)))
        (lispm-desktop-apps--put window place)
        (when (and again (frame-visible-p window))
          (lispm-desktop-platinum-hide-window window)
          (lispm-desktop-platinum-show-window window))))))

(defun lispm-desktop-apps--screen-resized (frame)
  "Put the floating windows of FRAME back in their places when it changed
size, and once more after it went full screen or back, when the window
system is done with them.  This is a `window-size-change-functions'
function."
  (unless (or (frame-parent frame) (lispm-desktop-platinum-chrome-frame-p frame))
    (lispm-desktop-apps--place-windows frame)
    (let ((fullscreen (frame-parameter frame 'fullscreen)))
      (unless (equal fullscreen (frame-parameter frame 'lispm-desktop-apps-fullscreen))
        (set-frame-parameter frame 'lispm-desktop-apps-fullscreen fullscreen)
        (run-at-time 1 nil #'lispm-desktop-apps--place-windows frame t)))))

(defun lispm-desktop-apps--available ()
  "Return the applications that can run here."
  (seq-filter (lambda (app)
                (let ((visible (plist-get (cdr app) :visible)))
                  (or (not visible) (and (functionp visible) (funcall visible)))))
              lispm-desktop-apps))

(defun lispm-desktop-apps-built-in ()
  "Return the applications built in, those with a :folder icon, as
`lispm-desktop-apps' says: (KEY . PROPERTIES)."
  (seq-filter (lambda (app) (plist-get (cdr app) :folder)) lispm-desktop-apps))

(defun lispm-desktop-apps-activate (app)
  "Bring APP, a key of `lispm-desktop-apps', in front."
  (funcall (plist-get (alist-get app lispm-desktop-apps) :activate)))

(defun lispm-desktop-apps--running ()
  "Return the applications running now, in the Application menu's order."
  (seq-filter (lambda (app)
                (let ((running (plist-get (cdr app) :running)))
                  (or (not running) (and (functionp running) (funcall running)))))
              (lispm-desktop-apps--available)))

(defun lispm-desktop-apps-next ()
  "Bring the next running application in front, as the Application menu
would, wrapping around: the desktop, the editor, the browser while it
has a page open, Control Panels while they are open."
  (interactive)
  (let* ((apps (mapcar #'car (lispm-desktop-apps--running)))
         (index (or (seq-position apps (lispm-desktop-apps-frontmost)) -1)))
    (when apps
      (lispm-desktop-apps-activate (nth (mod (1+ index) (length apps)) apps)))))

(defun lispm-desktop-apps-menus ()
  "Return the frontmost application's menus, for the menu bar."
  (let ((menus (plist-get (alist-get (lispm-desktop-apps-frontmost) lispm-desktop-apps) :menus)))
    (and (functionp menus) (funcall menus))))

(defun lispm-desktop-apps-name (app)
  "Return the name of APP, a key of `lispm-desktop-apps', in the language
LispM speaks, or nil."
  (when-let* ((name (plist-get (alist-get app lispm-desktop-apps) :name)))
    (lispm-desktop-tr name)))

(defun lispm-desktop-apps-application ()
  "Return the frontmost application as (ICON . NAME), for the Application menu."
  (let ((app (lispm-desktop-apps-frontmost)))
    (cons (lispm-desktop-platinum-icon (plist-get (alist-get app lispm-desktop-apps) :icon))
          (lispm-desktop-apps-name app))))

(declare-function lispm-desktop-coherence-available-p "lispm-desktop-coherence" (&optional screen))

(defun lispm-desktop-apps-application-items ()
  "Return the Application menu's items: Hide Desktop or Show Desktop, as
Mac OS 9's began with Hide and Show, then every application, the front
one checked, as Mac OS 9's listed the running applications."
  `(,@(when (and (fboundp 'lispm-desktop-coherence-available-p)
                 (lispm-desktop-coherence-available-p (lispm-desktop-apps-screen)))
        (if (lispm-desktop-apps-hidden-p)
            `((:label ,(lispm-desktop-tr "Show Desktop") :action lispm-desktop-show-desktop :key "C-c h")
              separator)
          `((:label ,(lispm-desktop-tr "Hide Desktop") :action lispm-desktop-hide-desktop :key "C-c h")
            separator)))
    ,@(mapcar (pcase-lambda (`(,key . ,_))
                (list :label (lispm-desktop-apps-name key)
                      :action (lambda () (lispm-desktop-apps-activate key))
                      :checked (eq key (lispm-desktop-apps-frontmost))))
              (lispm-desktop-apps--available))))

(defvar lispm-desktop-tab-name)
(defvar lispm-desktop-browser-workspace-name)
(defvar lispm-desktop-editor--moving)

(defun lispm-desktop-apps--follow-workspace (&rest _)
  "Put in front the application whose workspace was just selected.
The desktop's and the browser's workspaces are theirs; any other, and
any workspace of the floating editor, is the editor's.  This is a
`tab-bar-tab-post-select-functions' function."
  (unless (bound-and-true-p lispm-desktop-editor--moving)
    (let ((name (alist-get 'name (tab-bar--current-tab))))
      (lispm-desktop-apps-set-frontmost
       (cond ((frame-parent (selected-frame)) 'editor)
             ((equal name (bound-and-true-p lispm-desktop-tab-name)) 'desktop)
             ((equal name (bound-and-true-p lispm-desktop-browser-workspace-name)) 'browser)
             (t 'editor))))))

(defun lispm-desktop-apps--follow-window (&rest _)
  "Put in front the application whose floating window was just selected,
as the frame's `lispm-desktop-app' parameter names it, and raise the window over
the other floating windows.  A click in a window selects
its frame; the focus cannot tell, as macOS reports every child frame
focused along with its parent.  The screen names no application: a click
on the menu bar selects it, and the application in front stays so.
This is a `window-selection-change-functions' function."
  (let ((frame (selected-frame)))
    (when-let* ((app (frame-parameter frame 'lispm-desktop-app)))
      (unless (eq app lispm-desktop-apps--frontmost)
        (lispm-desktop-apps-set-frontmost app))
      ;; The window clicked comes over the other floating windows, as on
      ;; Mac OS 9; macOS leaves a child window where it was.
      (when (frame-parameter frame 'lispm-desktop-window)
        (lispm-desktop-platinum-raise-window frame)))))

(declare-function lispm-agents-available "lispm-agents" ())
(declare-function lispm-desktop-store-application-items "lispm-desktop-store" ())
(declare-function lispm-browser-available-p "lispm-browser" ())

(defun lispm-desktop-lambda-items ()
  "Return the λ menu's items for the desktop's applications, agents and
tools, as Mac OS 9's Apple menu listed applications and control panels.
This is `lispm-desktop-platinum-lambda-items-function'."
  `(;; A new session of each agent that can run here, those built in first.
    ,@(mapcar (pcase-lambda (`(,_ . ,props))
                (list :label (lispm-desktop-tr "New %s Session" (plist-get props :name))
                      :action (plist-get props :command)))
              (and (fboundp 'lispm-agents-available) (lispm-agents-available)))
    (:label ,(lispm-desktop-tr "Resume Session…") :action lispm-agents-sessions)
    (:label ,(lispm-desktop-tr "Send Region to Agent") :action lispm-agents-send-region
            :enabled ,(use-region-p))
    separator
    (:label ,(lispm-desktop-tr "New Shell") :action lispm-desktop-shell-new)
    ,@(when (and (fboundp 'lispm-browser-available-p) (lispm-browser-available-p))
        `((:label ,(lispm-desktop-tr "Browser") :action lispm-desktop-browser-show)))
    (:label ,(lispm-desktop-tr "Magit Status") :action magit-status)
    separator
    ,@(when (fboundp 'lispm-desktop-store-show)
        '((:label "App Store" :action lispm-desktop-store-show)))
    ,@(when (and (fboundp 'lispm-desktop-store-application-items)
                 (lispm-desktop-store-application-items))
        `((:label ,(lispm-desktop-tr "Applications") :submenu lispm-desktop-store-application-items)))
    ,@(when (fboundp 'lispm-desktop-settings-panel-items)
        `((:label ,(lispm-desktop-tr "Control Panels") :submenu lispm-desktop-settings-panel-items)))))

(defun lispm-desktop-apps-show-licence ()
  "Show GNU Emacs's licence in the editor, brought in front.
This is `lispm-desktop-platinum-licence-function'."
  (lispm-desktop-apps-activate 'editor)
  (describe-copying))

(defun lispm-desktop-setup-apps ()
  "Draw the menu bar and the Application menu from the application in front."
  (setq lispm-desktop-platinum-menus-function #'lispm-desktop-apps-menus
        lispm-desktop-platinum-lambda-items-function #'lispm-desktop-lambda-items
        lispm-desktop-platinum-application-function #'lispm-desktop-apps-application
        lispm-desktop-platinum-applications-function #'lispm-desktop-apps-application-items
        lispm-desktop-platinum-licence-function #'lispm-desktop-apps-show-licence)
  (add-hook 'tab-bar-tab-post-select-functions #'lispm-desktop-apps--follow-workspace)
  (add-hook 'window-selection-change-functions #'lispm-desktop-apps--follow-window)
  (add-hook 'window-size-change-functions #'lispm-desktop-apps--screen-resized)
  (add-hook 'lispm-desktop-platinum-frame-moved-functions #'lispm-desktop-apps-keep-on-desktop)
  ;; The next application from the keyboard, as Command-Tab is the
  ;; system's.
  (keymap-global-set "C-c o" #'lispm-desktop-apps-next))

(provide 'lispm-desktop-apps)
;;; lispm-desktop-apps.el ends here
