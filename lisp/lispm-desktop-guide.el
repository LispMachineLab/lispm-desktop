;;; lispm-desktop-guide.el --- Showing LispM around, as Apple Guide did  -*- lexical-binding: t; -*-

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

;; Mac OS showed newcomers around with Apple Guide: a small window,
;; floating over every other, told them a step at a time what they were
;; looking at and what to do, while a red loop drawn as if by hand, a
;; coach mark, circled the menu, the icon or the control the step spoke
;; of (see lispm-desktop-platinum-coach.el).  LispM's guide does the same,
;; by itself the first time LispM starts, and whenever asked from the
;; desktop's Help menu or with C-c ?:
;;
;;   Welcome           what LispM is, and how to go through the guide
;;   The λ menu        LispM's own menu, as the Apple menu was
;;   The application   the menus of the application in front, and the
;;                     Application menu naming the others
;;   The desktop       its icons: Home, in a Finder window
;;   Emacs             the projects, their files, terminals and agents
;;   The browser       the web, where Emacs can show it
;;   The App Store     applications and extensions to install
;;   Control Strip     settings at hand
;;   Windows           title bars, boxes and grow boxes, this one's
;;   Emacs, as ever    Emacs's own keys
;;
;; Each step names the keys that do what it shows, drawn as key caps, and
;; most have a Show Me button that does it.  N, Return, Space, C-f or C-n
;; go to the next step, P, DEL, C-b or C-p back, S shows, Q or Escape
;; puts the guide away, and Tab rings its buttons in turn.  The guide
;; floats over the other windows but takes the keyboard only when it is
;; clicked or called: what a step shows is tried in the windows
;; themselves, and C-c ? gives the guide the keyboard back.  It shows
;; over the desktop, hiding from the full-screen editor and browser as
;; the other windows do; C-c ? brings the desktop back in front with it,
;; and so does a step whose coach mark lies on the desktop.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-app-window)

(declare-function lispm-desktop-activate "lispm-desktop-surface" ())
(declare-function lispm-desktop-strip-edges "lispm-desktop-strip" ())
(declare-function lispm-desktop-coherence-available-p "lispm-desktop-coherence" (&optional screen))
(declare-function lispm-browser-available-p "lispm-browser" ())
(defvar lispm-desktop-help-items-function)

(defcustom lispm-desktop-guide-shown nil
  "Non-nil once the guide has shown itself, the first time LispM started:
it does not again by itself.  The desktop's Help menu and C-c ? show it
whenever asked."
  :type 'boolean
  :group 'lispm-desktop)

(defvar lispm-desktop-guide-steps
  '((welcome
     :title "Welcome to LispM"
     :icon "guide-32"
     :text "LispM is a desktop for coding agents, made of Emacs.  This guide shows it to you a part at a time, and circles each part in red as it speaks of it."
     :keys (("n, RET" "the next step")
            ("p, DEL" "the step before")
            ("q" "puts the guide away")
            ("C-c ?" "brings it back, from anywhere")))
    (lambda-menu
     :title "The λ menu"
     :icon lambda
     :mark (menu . lispm-desktop-lambda)
     :text "LispM's own menu, at the left end of the menu bar, as the Apple menu was: a new agent session, a shell, the App Store and the control panels."
     :keys (("C-c m" "opens the menu bar from the keyboard; C-f and C-b go to the menu beside"))
     :show lispm-desktop-platinum-lambda-menu)
    (application
     :title "The application in front"
     :icon "applications-32"
     :mark (menu . lispm-desktop-application)
     :text "The menus after the λ are those of the application in front, the desktop's now.  The Application menu, at the right end, names it and lists the others."
     :keys (("C-c o" "brings the next application forward"))
     :show lispm-desktop-platinum-workspace-menu)
    (desktop
     :title "The desktop's icons"
     :icon "hard-disk-32"
     :mark (icon . home)
     :text "They stand down the right edge.  A click selects one, a double click opens it, and one dragged stays where it is dropped.  Home is your home folder, in a Finder window."
     :keys (("C-n, C-p" "select the next icon or the one before, while the desktop has the keyboard")
            ("RET" "opens the icon selected"))
     :show lispm-desktop-finder-home)
    (editor
     :title "Emacs"
     :icon emacs
     :mark (icon . editor)
     :text "Where the work is done: a workspace for each project, with its files, its terminals and its agents, and a sidebar listing them.  It fills the screen, or floats over the desktop from its zoom box; C-c ? comes back here."
     :keys (("C-c a l" "starts a LispM agent in the project")
            ("C-c a a" "shows the agents")
            ("C-c t" "opens a terminal"))
     :show lispm-desktop-editor-activate)
    (browser
     :title "The browser"
     :icon "netscape"
     :mark (icon . browser)
     :when lispm-desktop-guide--browser-p
     :text "The web, dressed as Netscape Communicator: tabs, bookmarks and history, and pages followed from the keyboard."
     :keys (("M-l" "goes to the location field")
            ("M-t" "opens a new tab")
            ("f" "labels a page's links, to follow the one typed"))
     :show lispm-desktop-browser-show)
    (store
     :title "The App Store"
     :icon "app-store-32"
     :mark (icon . store)
     :when lispm-desktop-guide--store-p
     :text "Applications and extensions to install: games, news, music and more agents.  The applications installed are in the Applications folder."
     :show lispm-desktop-store-show)
    (strip
     :title "The Control Strip"
     :icon "control-panels-32"
     :mark (area . lispm-desktop-strip-edges)
     :when lispm-desktop-guide--strip-p
     :text "Settings at hand, in the corner: the appearance, the desktop's picture, the keyboard and the Internet.  The Control Panels icon holds every setting."
     :keys (("C-c b" "goes to the strip: C-f and C-b to a module, RET opens it")
            ("C-c B" "hides the strip, or shows it again"))
     :show lispm-desktop-strip-select)
    (windows
     :title "Windows"
     :icon "document-32"
     :mark (area . lispm-desktop-guide--edges)
     :text "This window is like every other: its title bar drags it, the box at the left closes it, the one at the right rolls it up into its title bar, and the corner at the bottom right sizes it.  A zoom box, where there is one, fills the screen with the window."
     :keys (("C-c h" "hides the desktop in the Dock, its windows out on your screen"
             lispm-desktop-guide--coherence-p))
     :show lispm-desktop-guide-roll)
    (keys
     :title "Emacs, as ever"
     :icon "keyboard-32"
     :text "Emacs's own keys work everywhere, as they always did.  This guide waits in the desktop's Help menu whenever you want it again."
     :keys (("C-x C-f" "opens a file")
            ("C-x b" "switches to a buffer")
            ("M-x" "runs any command")
            ("C-g" "cancels"))))
  "The guide's steps, in order: (KEY . PROPERTIES).
:title names a step, :text says what it shows, and :icon is its large
icon, as `lispm-desktop-platinum-icon-file' takes it.  :mark is what the
coach mark circles meanwhile, see lispm-desktop-platinum-coach.el: (menu
. KEY), a menu bar title; (icon . KEY), an icon of the desktop; or (area
. FUNCTION), the place on the screen FUNCTION returns.  A mark on the
desktop brings the desktop in front.  :keys are the keys doing what it
shows, (KEYS TEXT) each, KEYS key sequences as `kbd' reads them, comma
separated, TEXT what they do; a third element, a function, shows the
keys only while it returns non-nil.  :show is the command Show Me runs
to do it.  :when, a function, decides whether the step is shown at all.")

(defconst lispm-desktop-guide-buffer-name " *LispM Guide*"
  "Name of the buffer the guide's window shows.")

(defconst lispm-desktop-guide-size '(400 . 320)
  "Size in pixels of the guide's window the first time it opens; it is then
made as tall as its tallest step, see `lispm-desktop-guide--fit'.")

(defvar lispm-desktop-guide--index 0
  "Index of the step the guide is on, among those shown, see
`lispm-desktop-guide--steps'.")

;;;; Steps

(defun lispm-desktop-guide--browser-p ()
  "Return non-nil when the browser can run in this Emacs."
  (and (fboundp 'lispm-browser-available-p) (lispm-browser-available-p)))

(defun lispm-desktop-guide--store-p ()
  "Return non-nil when the App Store is here."
  (fboundp 'lispm-desktop-store-show))

(defun lispm-desktop-guide--strip-p ()
  "Return non-nil while the Control Strip is shown."
  (and (fboundp 'lispm-desktop-strip-edges) (lispm-desktop-strip-edges) t))

(defun lispm-desktop-guide--coherence-p ()
  "Return non-nil when the desktop's windows can go out on the screen."
  (and (fboundp 'lispm-desktop-coherence-available-p) (lispm-desktop-coherence-available-p)))

(defun lispm-desktop-guide--steps ()
  "Return the steps shown here, see `lispm-desktop-guide-steps'."
  (seq-filter (lambda (step)
                (let ((when (plist-get (cdr step) :when)))
                  (or (null when) (funcall when))))
              lispm-desktop-guide-steps))

(defun lispm-desktop-guide--step ()
  "Return the step the guide is on, (KEY . PROPERTIES)."
  (let ((steps (lispm-desktop-guide--steps)))
    (nth (max 0 (min lispm-desktop-guide--index (1- (length steps)))) steps)))

(defun lispm-desktop-guide--keys (step)
  "Return the keys STEP shows, (KEYS TEXT) each, those whose predicate
says no left out."
  (seq-filter (pcase-lambda (`(,_ ,_ ,predicate)) (or (null predicate) (funcall predicate)))
              (plist-get (cdr step) :keys)))

(defun lispm-desktop-guide--on-desktop-p (mark)
  "Return non-nil when coach mark MARK lies on the desktop."
  (memq (car-safe mark) '(icon area)))

;;;; Where things are

(defun lispm-desktop-guide--frame ()
  "Return the guide's window while it is shown."
  (when-let* ((frame (lispm-desktop-app-window-frame 'guide))
              ((frame-visible-p frame)))
    frame))

(defun lispm-desktop-guide--edges ()
  "Return where the guide's window is over its screen, its frame included,
\(X Y WIDTH HEIGHT); nil while it is not on the desktop."
  (when-let* ((frame (lispm-desktop-guide--frame))
              (outer (lispm-desktop-platinum-window-outer frame))
              ((frame-parent outer)))
    (pcase-let ((`(,x . ,y) (frame-position outer)))
      (list x y (frame-pixel-width outer) (frame-pixel-height outer)))))

(defun lispm-desktop-guide--geometry (screen)
  "Return where the guide's window opens first over SCREEN: at the left,
over the Control Strip, out of the way of the menu bar and the icons."
  (pcase-let ((`(,width . ,height) lispm-desktop-guide-size)
              (`(,_ ,top ,_ ,bottom) (lispm-desktop-apps-desktop-edges screen)))
    (list 48 (max (+ top 40) (- bottom height 96)) width height)))

;;;; Drawing

(defun lispm-desktop-guide--icon (name)
  "Return the large icon NAME as a string showing it, 32 pixels square."
  (let ((file (lispm-desktop-platinum-icon-file name)))
    (if (and (display-images-p) (file-readable-p file))
        (propertize " " 'display (create-image file nil nil :width 32 :height 32 :ascent 'center))
      "")))

(defun lispm-desktop-guide--key-caps (keys)
  "Return KEYS, key sequences separated by commas, drawn as key caps."
  (mapconcat (lambda (sequence)
               (mapconcat (lambda (key)
                            (lispm-desktop-platinum-bevel-button key nil 18 'lispm-desktop-platinum-small))
                          (split-string sequence " " t)
                          (propertize " " 'display '(space :width (3)))))
             (split-string keys ", *" t)
             (propertize " or " 'face '(shadow lispm-desktop-platinum-small))))

(defconst lispm-desktop-guide--margin 20
  "Pixels between the guide's text and its window's left side.")

(defun lispm-desktop-guide--gap (height)
  "Return a blank line HEIGHT pixels tall."
  (concat (propertize " " 'display `(space :height (,height))) "\n"))

(defun lispm-desktop-guide--insert-buttons (step first last)
  "Insert the guide's buttons for STEP: Show Me at the left, when STEP
shows something, and at the right Back, but on the FIRST step, and Next,
or Done on the LAST, which Return presses."
  (when (plist-get (cdr step) :show)
    (lispm-desktop-platinum-insert-push-button "Show Me" #'lispm-desktop-guide-show-me
                                               :help "Do what this step shows"))
  (let* ((forward (if last "Done" "Next"))
         ;; The buttons' images: room for a focus ring about Back, for
         ;; the default ring about Next, see `lispm-desktop-platinum-push-button'.
         (width (+ (lispm-desktop-platinum-push-button-width "Back") 4
                   6
                   (lispm-desktop-platinum-push-button-width forward) 10)))
    (insert (propertize " " 'display `(space :align-to (- right (,(+ width 14))))))
    (lispm-desktop-platinum-insert-push-button "Back" #'lispm-desktop-guide-previous
                                               :help "The step before" :disabled first)
    (insert (propertize " " 'display '(space :width (6))))
    (lispm-desktop-platinum-insert-push-button forward #'lispm-desktop-guide-next :default t
                                               :help (if last "Put the guide away" "The next step"))
    (insert "\n")))

(defun lispm-desktop-guide--insert-step (step first last &optional lift)
  "Insert STEP in the current buffer: its icon and title, what it says,
its keys, and the buttons, Back but on the FIRST step, Done on the LAST,
LIFT pixels lower than right under the keys."
  (let* ((properties (cdr step))
         (keys (lispm-desktop-guide--keys step))
         (indent `(space :width (,lispm-desktop-guide--margin)))
         (caps (mapcar (lambda (entry) (lispm-desktop-guide--key-caps (car entry))) keys))
         (column (+ lispm-desktop-guide--margin 12
                    (apply #'max 0 (mapcar #'string-pixel-width caps)))))
    (insert (lispm-desktop-guide--gap 14))
    (insert (propertize (concat (lispm-desktop-guide--icon (plist-get properties :icon))
                                (propertize " " 'display '(space :width (10)))
                                (propertize (plist-get properties :title)
                                            'face '(:inherit lispm-desktop-platinum-title :height 1.25)))
                        'line-prefix indent)
            "\n")
    (insert (lispm-desktop-guide--gap 8))
    (insert (propertize (plist-get properties :text) 'line-prefix indent 'wrap-prefix indent) "\n")
    (when keys
      (insert (lispm-desktop-guide--gap 10))
      (seq-mapn (lambda (entry cap)
                  (insert (propertize (concat cap
                                              (propertize " " 'display `(space :align-to (,column)))
                                              (propertize (nth 1 entry) 'face 'lispm-desktop-platinum-small))
                                      'line-prefix indent
                                      'wrap-prefix `(space :align-to (,column)))
                          "\n"))
                keys caps))
    (insert (lispm-desktop-guide--gap (+ 14 (or lift 0))))
    (insert (propertize " " 'display `(space :width (,(- lispm-desktop-guide--margin 4)))))
    (lispm-desktop-guide--insert-buttons step first last)
    (insert (lispm-desktop-guide--gap 6))))

(defun lispm-desktop-guide--render (&rest _)
  "Fill the guide's window with the step it is on, its buttons along the
window's bottom, as Apple Guide kept its own."
  (when-let* ((buffer (get-buffer lispm-desktop-guide-buffer-name)))
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
             (steps (lispm-desktop-guide--steps))
             (step (lispm-desktop-guide--step))
             (index (seq-position steps step))
             (first (= index 0))
             (last (= index (1- (length steps))))
             (window (get-buffer-window buffer t)))
        (erase-buffer)
        (lispm-desktop-guide--insert-step step first last)
        (when-let* ((window)
                    (spare (- (window-body-height window t)
                              (cdr (window-text-pixel-size window nil nil nil 8000))))
                    ((> spare 0)))
          (erase-buffer)
          (lispm-desktop-guide--insert-step step first last spare))
        (goto-char (point-min))
        (setq lispm-desktop-platinum-focus nil
              lispm-desktop-platinum-info (format "Step %d of %d" (1+ index) (length steps)))
        (lispm-desktop-platinum-show-focus)
        (force-mode-line-update)))))

(defun lispm-desktop-guide--fit (frame)
  "Make FRAME, the guide's window, as tall as its tallest step at its width,
so that no step moves its buttons out of sight."
  (let* ((window (frame-root-window frame))
         (steps (lispm-desktop-guide--steps))
         (height (with-current-buffer (window-buffer window)
                   (let ((inhibit-read-only t))
                     (prog1 (apply #'max
                                   (seq-map-indexed
                                    (lambda (step index)
                                      (erase-buffer)
                                      (lispm-desktop-guide--insert-step step (= index 0)
                                                                        (= index (1- (length steps))))
                                      (cdr (window-text-pixel-size window nil nil nil 8000 t)))
                                    steps))
                       (lispm-desktop-guide--render))))))
    (unless (or (lispm-desktop-app-window-rolled-p frame) (= height (frame-text-height frame)))
      (let ((frame-resize-pixelwise t))
        (set-frame-size frame (frame-text-width frame) height t))
      (lispm-desktop-platinum-frame-update frame)
      (lispm-desktop-apps-keep-on-desktop frame))))

;;;; Going from step to step

(defun lispm-desktop-guide--mark (step)
  "Circle what STEP speaks of, bringing the desktop in front when its mark
lies there, and the guide keeping the keyboard if it had it."
  (let ((mark (plist-get (cdr step) :mark))
        (guide (lispm-desktop-guide--frame)))
    (when (and (lispm-desktop-guide--on-desktop-p mark)
               (lispm-desktop-apps-full-screen-p (lispm-desktop-apps-frontmost))
               (fboundp 'lispm-desktop-activate))
      (let ((had (and guide (eq (selected-frame) guide))))
        (lispm-desktop-activate)
        (when had
          (select-frame-set-input-focus guide))))
    (lispm-desktop-platinum-set-coach-mark mark)))

(defun lispm-desktop-guide--go (index)
  "Go to step INDEX of the guide: draw it and circle what it speaks of."
  (setq lispm-desktop-guide--index (max 0 (min index (1- (length (lispm-desktop-guide--steps))))))
  (lispm-desktop-guide--render)
  (lispm-desktop-guide--mark (lispm-desktop-guide--step)))

(defun lispm-desktop-guide-next ()
  "Go to the guide's next step; after the last, put the guide away."
  (interactive)
  (if (< lispm-desktop-guide--index (1- (length (lispm-desktop-guide--steps))))
      (lispm-desktop-guide--go (1+ lispm-desktop-guide--index))
    (setq lispm-desktop-guide--index 0)
    (lispm-desktop-guide-close)))

(defun lispm-desktop-guide-previous ()
  "Go back to the guide's step before."
  (interactive)
  (when (> lispm-desktop-guide--index 0)
    (lispm-desktop-guide--go (1- lispm-desktop-guide--index))))

(defun lispm-desktop-guide-show-me ()
  "Do what the guide's step shows, as the user would, from the desktop.
The guide gets the keyboard back unless a window took it."
  (interactive)
  (when-let* ((command (plist-get (cdr (lispm-desktop-guide--step)) :show))
              ((commandp command)))
    (let ((guide (lispm-desktop-guide--frame))
          (screen (lispm-desktop-apps-screen)))
      (unless (eq (selected-frame) screen)
        (select-frame-set-input-focus screen))
      (unwind-protect
          (call-interactively command)
        (when (and guide (frame-visible-p guide) (lispm-desktop-apps-screen-p (selected-frame)))
          (select-frame-set-input-focus guide))))))

(defun lispm-desktop-guide-roll ()
  "Roll the guide's window up into its title bar and down again, as its
collapse box does."
  (interactive)
  (when-let* ((frame (lispm-desktop-guide--frame)))
    (when (lispm-desktop-app-window-roll-up frame)
      (redisplay t)
      (run-at-time 0.8 nil (lambda ()
                             (when (frame-live-p frame)
                               (lispm-desktop-app-window-roll-down frame)))))))

(defun lispm-desktop-guide-return ()
  "Press the button point is on, else go to the next step, as the default
button does."
  (interactive)
  (if (button-at (point))
      (lispm-desktop-platinum-push)
    (lispm-desktop-guide-next)))

;;;; The window

(defvar-keymap lispm-desktop-guide-mode-map
  :doc "Keymap of the guide's window."
  "n" #'lispm-desktop-guide-next
  "SPC" #'lispm-desktop-guide-next
  "RET" #'lispm-desktop-guide-return
  "C-f" #'lispm-desktop-guide-next
  "C-n" #'lispm-desktop-guide-next
  "p" #'lispm-desktop-guide-previous
  "DEL" #'lispm-desktop-guide-previous
  "C-b" #'lispm-desktop-guide-previous
  "C-p" #'lispm-desktop-guide-previous
  "s" #'lispm-desktop-guide-show-me
  "q" #'lispm-desktop-guide-close
  "<escape>" #'lispm-desktop-guide-close
  "TAB" #'forward-button
  "<backtab>" #'backward-button)

(define-derived-mode lispm-desktop-guide-mode lispm-desktop-app-window-mode "Guide"
  "Major mode of the guide's window: a step of the guide, its buttons under it.
\\{lispm-desktop-guide-mode-map}"
  (setq-local lispm-desktop-platinum-title "LispM Guide"
              lispm-desktop-platinum-icon (lispm-desktop-platinum-icon "guide")
              lispm-desktop-platinum-box-functions (list :close #'lispm-desktop-app-window-close-box
                                                         :collapse #'lispm-desktop-app-window-collapse-box)
              truncate-lines nil
              word-wrap t
              right-margin-width 2)
  (face-remap-add-relative 'default 'lispm-desktop-platinum-list-body)
  (add-hook 'window-size-change-functions #'lispm-desktop-guide--render nil t)
  (add-hook 'post-command-hook #'lispm-desktop-platinum-show-focus nil t))

(defun lispm-desktop-guide--buffer ()
  "Return the guide's buffer, made when missing."
  (or (get-buffer lispm-desktop-guide-buffer-name)
      (with-current-buffer (get-buffer-create lispm-desktop-guide-buffer-name)
        (lispm-desktop-guide-mode)
        (current-buffer))))

(defun lispm-desktop-guide--closed ()
  "Take the coach mark away once the guide's window is closed."
  (lispm-desktop-platinum-set-coach-mark nil))

(lispm-desktop-app-window-define 'guide
  ;; The guide belongs to no application: selecting it leaves the one in
  ;; front there.  It hides from the full-screen editor and browser, as
  ;; every floating window does.
  :app nil :name "LispM Guide" :size lispm-desktop-guide-size
  :geometry #'lispm-desktop-guide--geometry :buffer #'lispm-desktop-guide--buffer
  :roll 'chrome :on-close #'lispm-desktop-guide--closed)

(defun lispm-desktop-guide (&optional again)
  "Show the guide, or give it the keyboard back while it is shown.
It opens on the step it was put away on, or with AGAIN, the prefix
argument, on its first step; once gone through, it starts over.  The
guide shows over the desktop: a full-screen editor or browser in front
gives way to it."
  (interactive "P")
  (unless (display-graphic-p)
    (user-error "The guide is drawn on a graphical display only"))
  (when again
    (setq lispm-desktop-guide--index 0))
  (when (and (lispm-desktop-apps-full-screen-p (lispm-desktop-apps-frontmost))
             (fboundp 'lispm-desktop-activate))
    (lispm-desktop-activate))
  (let ((frame (or (lispm-desktop-app-window-frame 'guide)
                   (lispm-desktop-app-window-ensure 'guide))))
    (lispm-desktop-platinum-show-window frame)
    (lispm-desktop-guide--fit frame)
    (lispm-desktop-apps-keep-on-desktop frame)
    (lispm-desktop-platinum-raise-window frame)
    (select-frame-set-input-focus frame)
    (lispm-desktop-guide--go lispm-desktop-guide--index)
    frame))

(defun lispm-desktop-guide-close ()
  "Put the guide away; \\[lispm-desktop-guide] shows it again."
  (interactive)
  (lispm-desktop-app-window-close 'guide))

(defun lispm-desktop-guide--keep-above (&rest _)
  "Keep the guide over the floating window just selected or put in front,
as Apple Guide's window floated over every application's.
This is a `window-selection-change-functions' function and one of
`lispm-desktop-apps-frontmost-functions'."
  (when-let* ((guide (lispm-desktop-guide--frame))
              ((not (eq (selected-frame) guide)))
              ((frame-parameter (selected-frame) 'lispm-desktop-window)))
    (lispm-desktop-platinum-raise-window guide)))

(defun lispm-desktop-guide--moved (&rest _)
  "Draw the coach mark again where a window moved or was resized, when it
circles a place on the screen, as the guide's window or the Control
Strip.  This is one of `lispm-desktop-platinum-frame-moved-functions'."
  (when (eq (car-safe lispm-desktop-platinum-coach-mark) 'area)
    (lispm-desktop-platinum-set-coach-mark lispm-desktop-platinum-coach-mark)))

(defun lispm-desktop-guide--refresh (&rest _)
  "Draw the guide again in the colours of the theme just enabled."
  (when (lispm-desktop-guide--frame)
    (lispm-desktop-guide--render)))

;;;; The first time

(defun lispm-desktop-guide--welcome ()
  "Say how to start, where no guide can be shown."
  (message "%s"
           (substitute-command-keys
            (concat "LispM is ready.  \\`C-c a l' starts an agent in this project, "
                    "\\`C-c a a' shows the agents, \\`C-c a s' resumes a session."))))

(defun lispm-desktop-guide-first-time ()
  "Show the guide unless it has shown itself before, see
`lispm-desktop-guide-shown'; where it cannot be shown, as in a text
terminal, say how to start instead.  Once is kept for every later start.
A batch Emacs, or a daemon, which has no frame to show it on yet, does
neither."
  (unless (or lispm-desktop-guide-shown noninteractive (daemonp))
    (let ((inhibit-message t))
      (customize-save-variable 'lispm-desktop-guide-shown t))
    (if (and (display-graphic-p) (image-type-available-p 'svg))
        ;; Once the frame has its size and the desktop is drawn.
        (run-with-idle-timer 0.3 nil (lambda () (lispm-desktop-guide 'again)))
      (lispm-desktop-guide--welcome))))

(defun lispm-desktop-guide-help-items ()
  "Return the items of the desktop's Help menu: the guide."
  '((:label "LispM Guide" :action lispm-desktop-guide :key "C-c ?")))

(defun lispm-desktop-setup-guide ()
  "Offer the guide in the desktop's Help menu and with C-c ?, keep it over
the other windows and its coach mark on what moves, and show it the
first time LispM starts."
  (setq lispm-desktop-help-items-function #'lispm-desktop-guide-help-items)
  (keymap-global-set "C-c ?" #'lispm-desktop-guide)
  (add-hook 'window-selection-change-functions #'lispm-desktop-guide--keep-above 90)
  (add-hook 'lispm-desktop-apps-frontmost-functions #'lispm-desktop-guide--keep-above 90)
  (add-hook 'lispm-desktop-platinum-frame-moved-functions #'lispm-desktop-guide--moved 90)
  (add-hook 'enable-theme-functions #'lispm-desktop-guide--refresh 95)
  (add-hook 'window-setup-hook #'lispm-desktop-guide-first-time 96))

(provide 'lispm-desktop-guide)
;;; lispm-desktop-guide.el ends here
