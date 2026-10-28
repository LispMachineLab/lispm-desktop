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
;; most have a Show Me button that does it, as the user would: it opens
;; the menu, the window or the application, Emacs and the browser
;; floating over the desktop so that the guide stays in sight, and
;; leaves the keyboard with the guide.  What it opened is put back as the
;; guide goes on.  The first time, the guide plays by itself: each step
;; does its Show Me a moment after it comes, and a menu it opened closes
;; again a moment later, unless something is typed or clicked in it.
;; N, Return, Space, C-f or C-n go to the next step, P, DEL, C-b or C-p
;; back, S shows, Q or Escape puts the guide away, to come back on the
;; same step, Skip leaves it, to start over, and Tab rings its buttons in
;; turn.  The guide floats over the other windows but takes the keyboard
;; only when it is clicked or called, and C-c ? gives it the keyboard
;; back.  It shows over the desktop, hiding from the full-screen editor
;; and browser as the other windows do; C-c ? brings the desktop back in
;; front with it, and so does a step whose coach mark lies on the
;; desktop.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-app-window)
(require 'lispm-desktop-i18n)

(declare-function lispm-desktop-activate "lispm-desktop-surface" ())
(declare-function lispm-desktop-strip-edges "lispm-desktop-strip" ())
(declare-function lispm-desktop-strip-select "lispm-desktop-strip" ())
(declare-function lispm-desktop-finder-home "lispm-desktop-finder" ())
(declare-function lispm-desktop-finder-close "lispm-desktop-finder" ())
(declare-function lispm-desktop-finder-shown-p "lispm-desktop-finder" ())
(declare-function lispm-desktop-store-show "lispm-desktop-store" ())
(declare-function lispm-desktop-store-close "lispm-desktop-store" ())
(declare-function lispm-desktop-editor-floating-p "lispm-desktop-editor" ())
(declare-function lispm-desktop-editor-frame "lispm-desktop-editor" ())
(declare-function lispm-desktop-editor-float "lispm-desktop-editor" ())
(declare-function lispm-desktop-editor-activate "lispm-desktop-editor" ())
(declare-function lispm-desktop-editor-full-screen "lispm-desktop-editor" ())
(declare-function lispm-desktop-editor-close "lispm-desktop-editor" ())
(declare-function lispm-desktop-browser-floating-p "lispm-desktop-browser" ())
(declare-function lispm-desktop-browser-show "lispm-desktop-browser" ())
(declare-function lispm-desktop-browser-float "lispm-desktop-browser" ())
(declare-function lispm-desktop-browser-full-screen "lispm-desktop-browser" ())
(declare-function lispm-browser-tabs "lispm-browser" ())
(declare-function lispm-browser-quit "lispm-browser" ())
(defvar lispm-desktop-browser-floating)
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
  `((welcome
     :title ,(lispm-desktop-tr-noop "Welcome to LispM")
     :icon "guide-32"
     :text ,(lispm-desktop-tr-noop "LispM is a desktop for coding agents, made of Emacs.  This guide shows it to you a part at a time, and circles each part in red as it speaks of it.")
     :keys (("n, RET" ,(lispm-desktop-tr-noop "the next step"))
            ("p, DEL" ,(lispm-desktop-tr-noop "the step before"))
            ("q" ,(lispm-desktop-tr-noop "puts the guide away"))
            ("C-c ?" ,(lispm-desktop-tr-noop "brings it back, from anywhere"))))
    (lambda-menu
     :title ,(lispm-desktop-tr-noop "The λ menu")
     :icon lambda
     :mark (menu . lispm-desktop-lambda)
     :text ,(lispm-desktop-tr-noop "LispM's own menu, at the left end of the menu bar, as the Apple menu was: a new agent session, a shell, the App Store and the control panels.")
     :keys (("C-c m" ,(lispm-desktop-tr-noop "opens the menu bar from the keyboard; C-f and C-b go to the menu beside")))
     :show lispm-desktop-guide--show-lambda-menu)
    (application
     :title ,(lispm-desktop-tr-noop "The application in front")
     :icon "applications-32"
     :mark (menu . lispm-desktop-application)
     :text ,(lispm-desktop-tr-noop "The menus after the λ are those of the application in front, this guide's now: File and Go.  The Application menu, at the right end, names it and lists the others, to bring one forward.")
     :keys (("C-c o" ,(lispm-desktop-tr-noop "brings the next application forward")))
     :show lispm-desktop-guide--show-application-menu)
    (desktop
     :title ,(lispm-desktop-tr-noop "The desktop's icons")
     :icon "hard-disk-32"
     :mark (icon . home)
     :text ,(lispm-desktop-tr-noop "They stand down the right edge.  A click selects one, a double click opens it, and one dragged stays where it is dropped.  Home is your home folder, in a Finder window.")
     :keys (("C-n, C-p" ,(lispm-desktop-tr-noop "select the next icon or the one before, while the desktop has the keyboard"))
            ("RET" ,(lispm-desktop-tr-noop "opens the icon selected")))
     :show lispm-desktop-guide--show-home)
    (editor
     :title "Emacs"
     :icon emacs
     :mark (icon . editor)
     :text ,(lispm-desktop-tr-noop "Where the work is done: a workspace for each project, with its files, its terminals and its agents, and a sidebar listing them.  It fills the screen, or floats over the desktop as Show Me opens it; its zoom box switches.")
     :keys (("C-c a l" ,(lispm-desktop-tr-noop "starts a LispM agent in the project"))
            ("C-c a a" ,(lispm-desktop-tr-noop "shows the agents"))
            ("C-c t" ,(lispm-desktop-tr-noop "opens a terminal")))
     :show lispm-desktop-guide--show-editor)
    (browser
     :title ,(lispm-desktop-tr-noop "The browser")
     :icon "netscape"
     :mark (icon . browser)
     :when lispm-desktop-guide--browser-p
     :text ,(lispm-desktop-tr-noop "The web, dressed as Netscape Communicator: tabs, bookmarks and history, and pages followed from the keyboard.")
     :keys (("M-l" ,(lispm-desktop-tr-noop "goes to the location field"))
            ("M-t" ,(lispm-desktop-tr-noop "opens a new tab"))
            ("f" ,(lispm-desktop-tr-noop "labels a page's links, to follow the one typed")))
     :show lispm-desktop-guide--show-browser)
    (store
     :title ,(lispm-desktop-tr-noop "The App Store")
     :icon "app-store-32"
     :mark (icon . store)
     :when lispm-desktop-guide--store-p
     :text ,(lispm-desktop-tr-noop "Applications and extensions to install: games, news, music and more agents.  The applications installed are in the Applications folder.")
     :keys (("/" ,(lispm-desktop-tr-noop "searches the App Store"))
            ("RET" ,(lispm-desktop-tr-noop "opens the selected item's page, DEL goes back")))
     :show lispm-desktop-guide--show-store)
    (strip
     :title ,(lispm-desktop-tr-noop "The Control Strip")
     :icon "control-panels-32"
     :mark (area . lispm-desktop-strip-edges)
     :when lispm-desktop-guide--strip-p
     :text ,(lispm-desktop-tr-noop "Settings at hand, in the corner: the appearance, the desktop's picture, the keyboard and the Internet.  The Control Panels icon holds every setting.")
     :keys (("C-c b" ,(lispm-desktop-tr-noop "goes to the strip: C-f and C-b to a module, RET opens it"))
            ("C-c B" ,(lispm-desktop-tr-noop "hides the strip, or shows it again")))
     :show lispm-desktop-guide--show-strip)
    (windows
     :title ,(lispm-desktop-tr-noop "Windows")
     :icon "document-32"
     :mark (area . lispm-desktop-guide--edges)
     :text ,(lispm-desktop-tr-noop "This window is like every other: its title bar drags it, the box at the left closes it, the one at the right rolls it up into its title bar, and the corner at the bottom right sizes it.  A zoom box, where there is one, fills the screen with the window.")
     :keys (("C-c h" ,(lispm-desktop-tr-noop "hides the desktop in the Dock, its windows out on your screen")
             lispm-desktop-guide--coherence-p))
     :show lispm-desktop-guide-roll)
    (keys
     :title ,(lispm-desktop-tr-noop "Emacs, as ever")
     :icon "keyboard-32"
     :text ,(lispm-desktop-tr-noop "Emacs's own keys work everywhere, as they always did.  This guide is an application of its own: it waits in the Applications folder, the λ menu and the desktop's Help menu whenever you want it again.")
     :keys (("C-x C-f" ,(lispm-desktop-tr-noop "opens a file"))
            ("C-x b" ,(lispm-desktop-tr-noop "switches to a buffer"))
            ("M-x" ,(lispm-desktop-tr-noop "runs any command"))
            ("C-g" ,(lispm-desktop-tr-noop "cancels")))))
  "The guide's steps, in order: (KEY . PROPERTIES).
:title names a step, :text says what it shows, and :icon is its large
icon, as `lispm-desktop-platinum-icon-file' takes it.  :mark is what the
coach mark circles meanwhile, see lispm-desktop-platinum-coach.el: (menu
. KEY), a menu bar title; (icon . KEY), an icon of the desktop; or (area
. FUNCTION), the place on the screen FUNCTION returns.  A mark on the
desktop brings the desktop in front.  :keys are the keys doing what it
shows, (KEYS TEXT) each, KEYS key sequences as `kbd' reads them, comma
separated, TEXT what they do; a third element, a function, shows the
keys only while it returns non-nil.  :show is the function Show Me calls
to do it, as the user would; it returns a function putting back what it
opened, called as the guide leaves the step, or nil.  :when, a
function, decides whether the step is shown at all.
The words, :title, :text and each key's TEXT, are in English, translated
where shown; :text names a menu, an icon or a button in the words it
reads, so that their translations match.")

(defconst lispm-desktop-guide-buffer-name " *LispM Guide*"
  "Name of the buffer the guide's window shows.")

(defconst lispm-desktop-guide-size '(400 . 320)
  "Size in pixels of the guide's window the first time it opens; it is then
made as tall as its tallest step, see `lispm-desktop-guide--fit'.")

(defvar lispm-desktop-guide--index 0
  "Index of the step the guide is on, among those shown, see
`lispm-desktop-guide--steps'.")

(defconst lispm-desktop-guide-show-delay 0.8
  "Seconds a step is on before the guide playing by itself shows what it
speaks of: time for the coach mark to be seen first.")

(defconst lispm-desktop-guide-show-time 2.5
  "Seconds a menu shown by the guide playing by itself stays open, unless
something is typed or clicked in it meanwhile.")

(defvar lispm-desktop-guide--playing nil
  "Non-nil while the guide plays by itself, as the first time it shows:
each step does its Show Me on its own, a menu shown closing again.")

(defvar lispm-desktop-guide--timer nil
  "Timer of the Show Me the guide playing by itself does next.")

(defvar lispm-desktop-guide--undo nil
  "Function putting back what the last Show Me opened, or nil.")

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

(defun lispm-desktop-guide--centre (edges width height margins)
  "Return where a window WIDTH by HEIGHT pixels goes, (X . Y) as
`lispm-desktop-platinum-move-window' takes it, for its frame, MARGINS
\(LEFT TOP RIGHT BOTTOM) about it, to stand in the middle of a desktop
lying at EDGES, (LEFT TOP RIGHT BOTTOM)."
  (pcase-let ((`(,left ,top ,right ,bottom) edges)
              (`(,ml ,mt ,mr ,mb) margins))
    (cons (+ left ml (max 0 (/ (- right left width ml mr) 2)))
          (+ top mt (max 0 (/ (- bottom top height mt mb) 2))))))

(defun lispm-desktop-guide--geometry (screen)
  "Return where the guide's window opens over SCREEN: in the middle of
the desktop, as an alert stands."
  (pcase-let* ((`(,width . ,height) lispm-desktop-guide-size)
               (`(,x . ,y) (lispm-desktop-guide--centre (lispm-desktop-apps-desktop-edges screen)
                                                         width height (lispm-desktop-platinum-frame-margins))))
    (list x y width height)))

(defun lispm-desktop-guide--place (frame)
  "Put FRAME, the guide's window, in the middle of its screen's desktop."
  (when-let* ((screen (lispm-desktop-platinum-window-parent frame)))
    (pcase-let ((`(,x . ,y) (lispm-desktop-guide--centre (lispm-desktop-apps-desktop-edges screen)
                                                          (frame-pixel-width frame) (frame-pixel-height frame)
                                                          (lispm-desktop-platinum-window-margins frame))))
      (lispm-desktop-platinum-move-window frame x y))))

;;;; Drawing

(defun lispm-desktop-guide--icon (name)
  "Return the large icon NAME as a string showing it, 32 pixels square."
  (let ((file (lispm-desktop-platinum-icon-file name)))
    (if (and (display-images-p) (file-readable-p file))
        (propertize " " 'display (create-image file nil nil :width 32 :height 32 :ascent 'center))
      "")))

(defun lispm-desktop-guide--key-caps (keys)
  "Return KEYS, key sequences separated by commas, drawn as key caps, the
words between the sequences dimmed."
  (let* ((caps (mapcar (lambda (sequence)
                         (mapconcat (lambda (key)
                                      (lispm-desktop-platinum-bevel-button key nil 18 'lispm-ui-small))
                                    (split-string sequence " " t)
                                    (propertize " " 'display '(space :width (3)))))
                       (split-string keys ", *" t)))
         (text (seq-reduce (lambda (text cap) (lispm-desktop-tr "%s or %s" text cap))
                           (cdr caps) (or (car caps) ""))))
    ;; The words are what shows no key cap.
    (dotimes (i (length text))
      (unless (get-text-property i 'display text)
        (put-text-property i (1+ i) 'face '(shadow lispm-ui-small) text)))
    text))

(defconst lispm-desktop-guide--margin 20
  "Pixels between the guide's text and its window's left side.")

(defun lispm-desktop-guide--gap (height)
  "Return a blank line HEIGHT pixels tall."
  (concat (propertize " " 'display `(space :height (,height))) "\n"))

(defun lispm-desktop-guide--insert-buttons (step first last)
  "Insert the guide's buttons for STEP: at the left Skip, but on the LAST
step, and Show Me, when STEP shows something; at the right Back, but on
the FIRST step, and Next, or Done on the LAST, which Return presses."
  (unless last
    (lispm-desktop-platinum-insert-push-button (lispm-desktop-tr "Skip") #'lispm-desktop-guide-skip
                                               :help (lispm-desktop-tr "Leave the guide; C-c ? starts it over"))
    (insert (propertize " " 'display '(space :width (6)))))
  (when (plist-get (cdr step) :show)
    (lispm-desktop-platinum-insert-push-button (lispm-desktop-tr "Show Me") #'lispm-desktop-guide-show-me
                                               :help (lispm-desktop-tr "Do what this step shows")))
  (let* ((back (lispm-desktop-tr "Back"))
         (forward (if last (lispm-desktop-tr "Done") (lispm-desktop-tr "Next")))
         ;; The buttons' images: room for a focus ring about Back, for
         ;; the default ring about Next, see `lispm-desktop-platinum-push-button'.
         (width (+ (lispm-desktop-platinum-push-button-width back) 4
                   6
                   (lispm-desktop-platinum-push-button-width forward) 10)))
    (insert (propertize " " 'display `(space :align-to (- right (,(+ width 14))))))
    (lispm-desktop-platinum-insert-push-button back #'lispm-desktop-guide-previous
                                               :help (lispm-desktop-tr "The step before") :disabled first)
    (insert (propertize " " 'display '(space :width (6))))
    (lispm-desktop-platinum-insert-push-button forward #'lispm-desktop-guide-next :default t
                                               :help (if last
                                                         (lispm-desktop-tr "Put the guide away")
                                                       (lispm-desktop-tr "The next step")))
    (insert "\n")))

(defun lispm-desktop-guide--insert-step (step first last &optional lift)
  "Insert STEP in the current buffer, in the language LispM speaks: its
icon and title, what it says, its keys, and the buttons, Back but on the
FIRST step, Done on the LAST, LIFT pixels lower than right under the keys."
  (let* ((properties (cdr step))
         (keys (lispm-desktop-guide--keys step))
         (indent `(space :width (,lispm-desktop-guide--margin)))
         (caps (mapcar (lambda (entry) (lispm-desktop-guide--key-caps (car entry))) keys))
         (column (+ lispm-desktop-guide--margin 12
                    (apply #'max 0 (mapcar #'string-pixel-width caps)))))
    (insert (lispm-desktop-guide--gap 14))
    (insert (propertize (concat (lispm-desktop-guide--icon (plist-get properties :icon))
                                (propertize " " 'display '(space :width (10)))
                                (propertize (lispm-desktop-tr (plist-get properties :title))
                                            'face '(:inherit lispm-ui-title :height 1.25)))
                        'line-prefix indent)
            "\n")
    (insert (lispm-desktop-guide--gap 8))
    (insert (propertize (lispm-desktop-tr (plist-get properties :text)) 'line-prefix indent 'wrap-prefix indent)
            "\n")
    (when keys
      (insert (lispm-desktop-guide--gap 10))
      (seq-mapn (lambda (entry cap)
                  (insert (propertize (concat cap
                                              (propertize " " 'display `(space :align-to (,column)))
                                              (propertize (lispm-desktop-tr (nth 1 entry))
                                                          'face 'lispm-ui-small))
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
              lispm-desktop-platinum-info (lispm-desktop-tr "Step %d of %d" (1+ index) (length steps)))
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

(defun lispm-desktop-guide--tidy ()
  "Put back what the last Show Me opened, and forget the Show Me to come."
  (when (timerp lispm-desktop-guide--timer)
    (cancel-timer lispm-desktop-guide--timer))
  (setq lispm-desktop-guide--timer nil)
  (when-let* ((undo lispm-desktop-guide--undo))
    (setq lispm-desktop-guide--undo nil)
    (funcall undo)))

(defun lispm-desktop-guide--go (index)
  "Go to step INDEX of the guide: put back what the step before showed,
draw the step and circle what it speaks of.  Playing by itself, the
guide shows it a moment later, see `lispm-desktop-guide--playing'."
  (lispm-desktop-guide--tidy)
  (setq lispm-desktop-guide--index (max 0 (min index (1- (length (lispm-desktop-guide--steps))))))
  (lispm-desktop-guide--render)
  (let ((step (lispm-desktop-guide--step)))
    (lispm-desktop-guide--mark step)
    (when (and lispm-desktop-guide--playing (plist-get (cdr step) :show))
      (setq lispm-desktop-guide--timer
            (run-at-time lispm-desktop-guide-show-delay nil #'lispm-desktop-guide--play step)))))

(defun lispm-desktop-guide--play (step)
  "Show what STEP speaks of, the guide playing by itself, if the guide is
still on it."
  (setq lispm-desktop-guide--timer nil)
  (when (and (lispm-desktop-guide--frame) (equal step (lispm-desktop-guide--step)))
    (lispm-desktop-guide-show-me)))

(defun lispm-desktop-guide-next ()
  "Go to the guide's next step; after the last, put the guide away."
  (interactive)
  (if (< lispm-desktop-guide--index (1- (length (lispm-desktop-guide--steps))))
      (lispm-desktop-guide--go (1+ lispm-desktop-guide--index))
    (lispm-desktop-guide-skip)))

(defun lispm-desktop-guide-previous ()
  "Go back to the guide's step before."
  (interactive)
  (when (> lispm-desktop-guide--index 0)
    (lispm-desktop-guide--go (1- lispm-desktop-guide--index))))

(defun lispm-desktop-guide-show-me ()
  "Do what the guide's step shows, as the user would, from the desktop.
What it opens is put back as the guide leaves the step, and the guide
keeps the keyboard, so that the steps go on as they did."
  (interactive)
  (when-let* ((show (plist-get (cdr (lispm-desktop-guide--step)) :show)))
    (lispm-desktop-guide--tidy)
    (let ((guide (lispm-desktop-guide--frame))
          (screen (lispm-desktop-apps-screen)))
      (unless (eq (selected-frame) screen)
        (select-frame-set-input-focus screen))
      (unwind-protect
          (setq lispm-desktop-guide--undo (funcall show))
        (when (and guide (frame-visible-p guide))
          (lispm-desktop-platinum-raise-window guide)
          (select-frame-set-input-focus guide))))))

(defun lispm-desktop-guide-return ()
  "Press the button point is on, else go to the next step, as the default
button does."
  (interactive)
  (if (button-at (point))
      (lispm-desktop-platinum-push)
    (lispm-desktop-guide-next)))

;;;; Showing

(defun lispm-desktop-guide--modal (command &optional keys)
  "Call COMMAND, which reads events until it is done, as a menu does, the
events KEYS typed into it first.  While the guide plays by itself, it is
closed after `lispm-desktop-guide-show-time' seconds, Escape typed into
it until it returns, unless something was typed or clicked meanwhile;
an Escape left unread once it returned is taken back.  Return nil:
nothing is left to put back."
  ;; Events typed into Emacs from here count as read, as the user's do:
  ;; KEYS, and each Escape.
  (let* ((input (+ num-nonmacro-input-events (length keys)))
         (timer (and lispm-desktop-guide--playing
                     (run-at-time lispm-desktop-guide-show-time 0.3
                                  (lambda ()
                                    (when (and (<= num-nonmacro-input-events input)
                                               (null unread-command-events))
                                      (setq unread-command-events (list 'escape)
                                            input (1+ input))))))))
    (setq unread-command-events (append unread-command-events keys))
    (unwind-protect
        (call-interactively command)
      (when timer
        (cancel-timer timer)
        (setq unread-command-events (delq 'escape unread-command-events))))
    nil))

(defun lispm-desktop-guide--show-lambda-menu ()
  "Open the λ menu, as a click on it does."
  (lispm-desktop-guide--modal #'lispm-desktop-platinum-lambda-menu))

(defun lispm-desktop-guide--show-application-menu ()
  "Open the Application menu, as a click on it does."
  (lispm-desktop-guide--modal #'lispm-desktop-platinum-workspace-menu))

(defun lispm-desktop-guide--show-strip ()
  "Go to the Control Strip from the keyboard and open its first module's
menu, as \\`C-c b' and Return do."
  (lispm-desktop-guide--modal #'lispm-desktop-strip-select '(return)))

(defun lispm-desktop-guide--show-home ()
  "Open Home in a Finder window; return what closes it again, unless it
was open already."
  (let ((open (lispm-desktop-finder-shown-p)))
    (lispm-desktop-finder-home)
    (unless open #'lispm-desktop-finder-close)))

(defun lispm-desktop-guide--show-store ()
  "Open the App Store; return what closes it again, unless it was open
already."
  (let ((open (lispm-desktop-app-window-open-p 'store)))
    (lispm-desktop-store-show)
    (unless open #'lispm-desktop-store-close)))

(defun lispm-desktop-guide--show-editor ()
  "Open Emacs floating over the desktop, not filling the screen, which
would cover the guide; return what puts it back as it was: full screen
behind the desktop, or closed."
  (let ((floating (lispm-desktop-editor-floating-p))
        (shown (and (lispm-desktop-editor-floating-p)
                    (frame-visible-p (lispm-desktop-editor-frame)))))
    (unless floating
      (lispm-desktop-editor-float))
    (lispm-desktop-editor-activate)
    (cond ((not floating)
           (lambda ()
             (when (lispm-desktop-editor-floating-p)
               (lispm-desktop-editor-full-screen)
               (lispm-desktop-activate))))
          ((not shown) #'lispm-desktop-editor-close))))

(defun lispm-desktop-guide--show-browser ()
  "Open the browser floating over the desktop, not filling the screen,
which would cover the guide; return what puts it back as it was: quit,
when it was not running, else full screen behind the desktop."
  (let ((running (lispm-browser-tabs))
        (floating lispm-desktop-browser-floating))
    (if (lispm-desktop-browser-floating-p)
        (lispm-desktop-browser-show)
      (lispm-desktop-browser-float))
    (cond ((not running)
           (lambda ()
             (lispm-browser-quit)
             (setq lispm-desktop-browser-floating floating)))
          ((not floating)
           (lambda ()
             (lispm-desktop-browser-full-screen)
             (lispm-desktop-activate))))))

(defun lispm-desktop-guide-roll ()
  "Roll the guide's window up into its title bar and down again, as its
collapse box does."
  (interactive)
  (when-let* ((frame (lispm-desktop-guide--frame)))
    (when (lispm-desktop-app-window-roll-up frame)
      (redisplay t)
      (run-at-time 0.8 nil (lambda ()
                             (when (frame-live-p frame)
                               (lispm-desktop-app-window-roll-down frame))))))
  nil)

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
  (setq-local lispm-desktop-platinum-window-title (lambda () (lispm-desktop-tr "LispM Guide"))
              lispm-desktop-platinum-icon (lispm-desktop-platinum-icon "guide")
              lispm-desktop-platinum-box-functions (list :close #'lispm-desktop-app-window-close-box
                                                         :collapse #'lispm-desktop-app-window-collapse-box)
              truncate-lines nil
              word-wrap t
              ;; Chinese and Japanese, which have no spaces, wrap too.
              word-wrap-by-category t
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
  "Once the guide's window is closed, put back what its Show Me opened, stop
playing and take the coach mark away."
  (setq lispm-desktop-guide--playing nil)
  (lispm-desktop-guide--tidy)
  (lispm-desktop-platinum-set-coach-mark nil))

(lispm-desktop-app-window-define 'guide
  ;; An application's window: selected, it brings the guide in front, its
  ;; menus in the menu bar.  It hides from the full-screen editor and
  ;; browser, as every floating window does.
  :name (lispm-desktop-tr-noop "LispM Guide") :size lispm-desktop-guide-size
  :geometry #'lispm-desktop-guide--geometry :buffer #'lispm-desktop-guide--buffer
  :roll 'chrome :on-close #'lispm-desktop-guide--closed)

(defun lispm-desktop-guide-open-p ()
  "Return non-nil while the guide's window is open."
  (lispm-desktop-app-window-open-p 'guide))

(defun lispm-desktop-guide (&optional again play)
  "Show the guide, or give it the keyboard back while it is shown.
It opens on the step it was put away on, or with AGAIN, the prefix
argument, on its first step; once gone through or skipped, it starts
over.  PLAY has the guide play by itself, each step doing its Show Me on
its own, as the first time it shows.  The guide shows over the desktop:
a full-screen editor or browser in front gives way to it."
  (interactive "P")
  (unless (display-graphic-p)
    (user-error (lispm-desktop-tr "The guide is drawn on a graphical display only")))
  (when again
    (setq lispm-desktop-guide--index 0))
  (when play
    (setq lispm-desktop-guide--playing t))
  (when (and (lispm-desktop-apps-full-screen-p (lispm-desktop-apps-frontmost))
             (fboundp 'lispm-desktop-activate))
    (lispm-desktop-activate))
  (let* ((open (lispm-desktop-guide-open-p))
         (frame (lispm-desktop-app-window-ensure 'guide)))
    (lispm-desktop-platinum-show-window frame)
    ;; Opening, it stands in the middle of the desktop, wherever it was.
    (unless open
      (lispm-desktop-guide--fit frame)
      (lispm-desktop-guide--place frame))
    (lispm-desktop-app-window-present 'guide)
    (lispm-desktop-guide--go lispm-desktop-guide--index)
    frame))

(defun lispm-desktop-guide-close ()
  "Put the guide away; \\[lispm-desktop-guide] shows it again on the same step."
  (interactive)
  (lispm-desktop-app-window-close 'guide))

(defun lispm-desktop-guide-skip ()
  "Leave the guide: put it away, to start over the next time it shows."
  (interactive)
  (setq lispm-desktop-guide--index 0)
  (lispm-desktop-guide-close))

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

(defun lispm-desktop-guide--language-changed ()
  "Make the guide's window as tall as its steps are in the language now
spoken, and draw it again.  This is one of
`lispm-desktop-language-changed-hook'."
  (when-let* ((frame (lispm-desktop-guide--frame)))
    (lispm-desktop-guide--fit frame)))

;;;; The application

(defun lispm-desktop-guide--file-items ()
  "Return the items of the guide's File menu."
  `((:label ,(lispm-desktop-tr "Close Guide") :action lispm-desktop-guide-close :key "q")
    (:label ,(lispm-desktop-tr "Skip Guide") :action lispm-desktop-guide-skip)))

(defun lispm-desktop-guide--go-items ()
  "Return the items of the guide's Go menu: back, on and Show Me, then
every step, the one the guide is on checked, as Apple Guide listed its
topics."
  (let* ((steps (lispm-desktop-guide--steps))
         (index (seq-position steps (lispm-desktop-guide--step))))
    `((:label ,(lispm-desktop-tr "Previous Step") :action lispm-desktop-guide-previous :key "p"
              :enabled ,(> index 0))
      (:label ,(lispm-desktop-tr "Next Step") :action lispm-desktop-guide-next :key "n"
              :enabled ,(< index (1- (length steps))))
      (:label ,(lispm-desktop-tr "Show Me") :action lispm-desktop-guide-show-me :key "s"
              :enabled ,(and (plist-get (cdr (lispm-desktop-guide--step)) :show) t))
      separator
      ,@(seq-map-indexed (lambda (step i)
                           (list :label (lispm-desktop-tr (plist-get (cdr step) :title))
                                 :action (lambda () (lispm-desktop-guide--go i))
                                 :checked (= i index)))
                         steps))))

(defun lispm-desktop-guide-menus ()
  "Return the guide's menus, for the menu bar while it is in front."
  `((lispm-desktop-guide-file ,(lispm-desktop-tr "File") lispm-desktop-guide--file-items)
    (lispm-desktop-guide-go ,(lispm-desktop-tr "Go") lispm-desktop-guide--go-items)))

(defun lispm-desktop-guide--define-app ()
  "Make the guide an application built in: in the Applications folder, the
λ menu's Applications and, while it is open, the Application menu."
  (lispm-desktop-define-app 'guide :name (lispm-desktop-tr-noop "LispM Guide") :icon "guide" :folder "guide-32"
                            :menus #'lispm-desktop-guide-menus
                            :activate #'lispm-desktop-guide
                            :visible #'lispm-desktop-guide-open-p))

;;;; The first time

(defun lispm-desktop-guide--welcome ()
  "Say how to start, where no guide can be shown."
  (message "%s"
           (substitute-command-keys
            (lispm-desktop-tr "LispM is ready.  \\`C-c a l' starts an agent in this project, \\`C-c a a' shows the agents, \\`C-c a s' resumes a session."))))

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
        (run-with-idle-timer 0.3 nil (lambda () (lispm-desktop-guide 'again 'play)))
      (lispm-desktop-guide--welcome))))

(defun lispm-desktop-guide-help-items ()
  "Return the items of the desktop's Help menu: the guide."
  `((:label ,(lispm-desktop-tr "LispM Guide") :action lispm-desktop-guide :key "C-c ?")))

(defun lispm-desktop-setup-guide ()
  "Make the guide an application built in, offer it in the desktop's Help
menu and with C-c ?, keep it over the other windows and its coach mark
on what moves, draw it again in a new language, and show it the first
time LispM starts."
  (lispm-desktop-guide--define-app)
  (setq lispm-desktop-help-items-function #'lispm-desktop-guide-help-items)
  (keymap-global-set "C-c ?" #'lispm-desktop-guide)
  (add-hook 'window-selection-change-functions #'lispm-desktop-guide--keep-above 90)
  (add-hook 'lispm-desktop-apps-frontmost-functions #'lispm-desktop-guide--keep-above 90)
  (add-hook 'lispm-desktop-platinum-frame-moved-functions #'lispm-desktop-guide--moved 90)
  (add-hook 'enable-theme-functions #'lispm-desktop-guide--refresh 95)
  (add-hook 'lispm-desktop-language-changed-hook #'lispm-desktop-guide--language-changed)
  (add-hook 'window-setup-hook #'lispm-desktop-guide-first-time 96))

(provide 'lispm-desktop-guide)
;;; lispm-desktop-guide.el ends here
