;;; lispm-desktop-platinum-titlebar.el --- Title bars and their commands  -*- lexical-binding: t; -*-

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

;; Every window wears a title bar: a close box, its title between
;; pinstripes, a zoom box and a collapse box, drawn in its tab line; and
;; what the boxes and a click or a menu on the title do.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-platinum-frames)
(require 'lispm-desktop-i18n)

(declare-function tabspaces--buffer-list "tabspaces" (&optional frame tabnum))
(declare-function lispm-desktop-popup-menu "lispm-desktop-popup" (items &optional x y align))
(declare-function lispm-desktop-popup-alert "lispm-desktop-popup" (icon message buttons &optional detail))
(declare-function lispm-desktop-popup-available-p "lispm-desktop-popup" ())

;;;; What the desktop tells title bars

(defvar lispm-desktop-platinum-buffer-title-functions nil
  "Functions naming a buffer's window, tried in turn when the buffer names
no title of its own: each is called with the buffer and returns its
title, or nil.  An agent's session is named by its title this way.")

(defvar lispm-desktop-platinum-buffer-status-functions nil
  "Functions saying what a buffer's program is doing, shown in its title
bar after its title: each is called with the buffer and returns a short
label, or nil.  A terminal says what its program last said it does.")

(defvar lispm-desktop-platinum-running-buffers-functions nil
  "Functions returning buffers whose programs quitting Emacs would stop,
though they are no process of Emacs's: a terminal's runs in a thread of
its own.")

(defvar lispm-desktop-platinum-window-kind-function #'ignore
  "Function of a window of the main area returning its kind: a window
dropped on another by its title bar trades places with it only when
both are of one kind.  Sessions keep to the left of the main area this way.")

(defvar lispm-desktop-platinum-side-window-close-function #'delete-window
  "Function closing a side window from its close box, called with the
window.  The sidebar hides its view.")

(defvar lispm-desktop-platinum-last-window-function #'ignore
  "Function called with the main area's last window once its buffer was
closed, the window staying: the desktop shows in it.")

(defvar-local lispm-desktop-platinum-listed t
  "Non-nil when this buffer is among those a window's title menu offers.
The sidebar's lists are not.")

(defun lispm-desktop-platinum-main-window-p (window)
  "Return non-nil when WINDOW holds content of the main area.
Side windows and chrome windows, such as the browser's address window
\(marked by the `lispm-desktop-chrome' window parameter), do not."
  (not (or (window-parameter window 'window-side)
           (window-parameter window 'lispm-desktop-chrome))))

(defun lispm-desktop-platinum-main-windows ()
  "Return the live windows of the main area, left to right."
  (sort (seq-filter #'lispm-desktop-platinum-main-window-p (window-list nil 'no-mini))
        (lambda (a b) (< (car (window-edges a)) (car (window-edges b))))))

(defun lispm-desktop-platinum-main-window ()
  "Return the most recently used window of the main area of this frame."
  (or (car (sort (lispm-desktop-platinum-main-windows)
                 (lambda (a b) (> (window-use-time a) (window-use-time b)))))
      (selected-window)))

;;;; Title bars

(defvar-local lispm-desktop-platinum-box-functions nil
  "What the title bar boxes of this buffer's window do instead of their own.
A plist of :close, :zoom and :collapse functions, each called with the
window, for chrome whose boxes act on what it frames, such as the
browser's address window or the editor's title strip.  Where the plist is
set, a box without a function is drawn disabled.")

(defvar-local lispm-desktop-platinum-active-function nil
  "Function deciding whether this buffer's title bar is drawn active, or nil.
Called with the window, it lets chrome look active along with what it
frames, as the editor's title strip does while the editor is in front.")

(defun lispm-desktop-platinum--box-function (window box)
  "Return the function WINDOW's buffer gives title bar BOX, or nil.
BOX is :close, :zoom or :collapse; see `lispm-desktop-platinum-box-functions'."
  (plist-get (buffer-local-value 'lispm-desktop-platinum-box-functions (window-buffer window)) box))

(defun lispm-desktop-platinum-redraw-title-bars (&optional frame)
  "Draw the title bars and bottoms of FRAME's windows again at the next
redisplay, FRAME being the selected one by default.  The other frames'
windows and the menu bar are left as they are, which
`(force-mode-line-update t)' would redraw too."
  (dolist (window (window-list frame 'no-mini))
    (with-current-buffer (window-buffer window)
      (force-mode-line-update))))

(defun lispm-desktop-platinum-no-title-bar ()
  "Give the current buffer's windows no title bar, as for transient popups."
  (setq-local tab-line-format nil))

(defun lispm-desktop-platinum-buffer-title (buffer)
  "Return the title of BUFFER's window."
  (or (lispm-desktop-platinum-title-text buffer)
      (run-hook-with-args-until-success 'lispm-desktop-platinum-buffer-title-functions buffer)
      (buffer-name buffer)))

(defun lispm-desktop-platinum--buffer-icon (buffer)
  "Return the proxy icon of BUFFER's window, as Finder's windows showed one."
  (or (buffer-local-value 'lispm-desktop-platinum-icon buffer)
      (lispm-desktop-platinum-buffer-icon buffer)))

(defun lispm-desktop-platinum--window-command (command)
  "Return a mouse command calling COMMAND with the window clicked."
  (lambda (event)
    (interactive "e")
    (funcall command (posn-window (event-start event)))))

(defun lispm-desktop-platinum-press (position pressed-p command)
  "Hold a button down from the press at POSITION until the mouse button rises.
The button is drawn pushed in, through the window parameter
`lispm-desktop-platinum-pressed' that the drawing reads, until the release;
COMMAND then runs, with no arguments, only when PRESSED-P accepts the
release's position: as on Mac OS 9, moving off a button before letting go
cancels."
  (let ((window (posn-window position))
        (target (lispm-desktop-platinum--press-target position)))
    (set-window-parameter window 'lispm-desktop-platinum-pressed target)
    (with-current-buffer (window-buffer window) (force-mode-line-update))
    (redisplay t)
    (let ((release (let ((echo-keystrokes 0)) (lispm-ui-modal-read-event))))
      (set-window-parameter window 'lispm-desktop-platinum-pressed nil)
      (with-current-buffer (window-buffer window) (force-mode-line-update))
      (when (and (consp release)
                 (eq (event-basic-type release) 'mouse-1)
                 (funcall pressed-p (event-end release)))
        (funcall command)))))

(defun lispm-desktop-platinum--press-target (position)
  "Return what the press at POSITION is on: the target named there.
That is the text property `lispm-desktop-platinum-target'."
  (when-let* ((string (posn-string position)))
    (get-text-property (cdr string) 'lispm-desktop-platinum-target (car string))))

(defun lispm-desktop-platinum-same-target-p (position)
  "Return a predicate true of positions on the same target as POSITION."
  (let ((window (posn-window position))
        (area (posn-area position))
        (target (lispm-desktop-platinum--press-target position)))
    (lambda (release)
      (and (eq (posn-window release) window)
           (eq (posn-area release) area)
           (equal (lispm-desktop-platinum--press-target release) target)))))

(defun lispm-desktop-platinum-pressed-p (target)
  "Return non-nil while TARGET is held down in the window being drawn."
  (equal (window-parameter (selected-window) 'lispm-desktop-platinum-pressed) target))

(defun lispm-desktop-platinum-box-button (string kind command help)
  "Return STRING as title bar box KIND, which runs COMMAND with its window.
HELP describes it.  It is pressed rather than clicked, see
`lispm-desktop-platinum-press'."
  (propertize string
              'help-echo help
              'lispm-desktop-platinum-target kind
              'keymap (let ((map (make-sparse-keymap)))
                        (keymap-set map "<tab-line> <down-mouse-1>"
                                    (lambda (event)
                                      (interactive "e")
                                      (let ((start (event-start event)))
                                        (lispm-desktop-platinum-press
                                         start (lispm-desktop-platinum-same-target-p start)
                                         (lambda () (funcall command (posn-window start)))))))
                        (keymap-set map "<tab-line> <mouse-1>" #'ignore)
                        map)))

(defun lispm-desktop-platinum--title-press (event)
  "Move the child frame whose top title bar EVENT pressed, as a window.
A window at the top of a child frame, such as the floating editor's
title strip, moves the frame by its outline, as Mac OS 9 moved a window
by its title, see `lispm-desktop-platinum-drag-outline'; elsewhere the
press does nothing and the release selects the window."
  (interactive "e")
  (let* ((window (posn-window (event-start event)))
         (frame (and (windowp window) (window-frame window))))
    (when (and frame (lispm-desktop-platinum--movable-p frame) (window-at-side-p window 'top))
      (lispm-desktop-platinum-drag-outline frame 'move))))

(defvar lispm-desktop-platinum-title-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<tab-line> <down-mouse-1>" #'lispm-desktop-platinum--title-press)
    (keymap-set map "<tab-line> <mouse-1>"
                (lispm-desktop-platinum--window-command #'select-window))
    (keymap-set map "<tab-line> <double-down-mouse-1>" #'ignore)
    (keymap-set map "<tab-line> <double-mouse-1>"
                (lispm-desktop-platinum--window-command #'lispm-desktop-platinum-shade-or-zoom))
    (keymap-set map "<tab-line> <drag-mouse-1>" #'lispm-desktop-platinum-drag-window)
    (keymap-set map "<tab-line> M-<down-mouse-1>" #'ignore)
    (keymap-set map "<tab-line> M-<mouse-1>"
                (lispm-desktop-platinum--window-command #'lispm-desktop-platinum-title-menu))
    map)
  "Keymap of a title bar outside its boxes, as Mac OS 9's title bars
behaved: a click brings the window forward, a double click rolls it up,
a drag moves the window, and a Command click (Meta) pops up its buffers.")

(defun lispm-desktop-platinum-title-help ()
  "Return the help shown over a title bar, in the language LispM speaks."
  (lispm-desktop-tr "Drag onto another window to swap them; double-click to roll up; %s-click, or \
C-c u, for the window's buffers"
                    (if (eq system-type 'darwin) "⌘" "M")))

(defun lispm-desktop-platinum-drag-window (event)
  "Move the window whose title bar EVENT dragged to where it was dropped.
Tiled windows cannot float as Mac OS 9's did, so a window dropped on
another trades places with it.  The main area's columns hold: a session
trades places only with a session, anything else only with another
window that is not a session."
  (interactive "e")
  (let ((from (posn-window (event-start event)))
        (to (posn-window (event-end event))))
    (when (and (windowp to) (window-live-p to) (not (eq from to))
               (lispm-desktop-platinum-main-window-p from) (lispm-desktop-platinum-main-window-p to))
      (if (equal (funcall lispm-desktop-platinum-window-kind-function from)
                 (funcall lispm-desktop-platinum-window-kind-function to))
          (progn (window-swap-states from to)
                 (select-window to))
        (message (lispm-desktop-tr "Sessions keep to the left, everything else to the right"))))))

(defun lispm-desktop-platinum-fit (string width)
  "Return STRING shortened with an ellipsis to fit WIDTH pixels.
The ellipsis takes the face of the text it follows."
  (if (<= (string-pixel-width string) width)
      string
    (let ((n (length string))
          (ellipsis (propertize "…" 'face (get-text-property (max 0 (1- (length string)))
                                                              'face string))))
      (while (and (> n 1)
                  (> (string-pixel-width (concat (substring string 0 n) ellipsis)) width))
        (setq n (max 1 (min (1- n)
                            (floor (* n (/ (float width)
                                           (max 1 (string-pixel-width
                                                   (substring string 0 n))))))))))
      (concat (substring string 0 n) ellipsis))))

(defconst lispm-desktop-platinum-box-margin 3
  "Pixels from a title bar's end to the box nearest it, Mac OS 9's.")

(defconst lispm-desktop-platinum-resize-corners
  '((left top . 12) (right top . 12) (left bottom . 12) (right bottom . 15))
  "Where macOS takes a press on an undecorated frame as resizing it from a
corner: (SIDE END . SQUARE), a square SQUARE pixels on a side, in the
frame, at its SIDE and END.  Between the corners it takes three pixels
along each edge, and nothing outside the frame.  So AppKit reports a
borderless resizable window on macOS 27, as an undecorated frame is; see
\"Window frames\" in lispm-desktop-platinum-frames.el.")

(defun lispm-desktop-platinum--resized-from-corners-p (frame)
  "Return non-nil when the window system resizes FRAME from its corners, see
`lispm-desktop-platinum-resize-corners': FRAME is undecorated, on macOS,
and not full screen, which Emacs shows in a window that does not resize."
  (and (eq (framep frame) 'ns)
       (frame-parameter frame 'undecorated)
       (not (memq (frame-parameter frame 'fullscreen) '(fullboth fullscreen)))))

(defun lispm-desktop-platinum--box-margins (frame-width frame-height left right top)
  "Return how far in from its ends a title bar stands the boxes nearest
them, in a frame FRAME-WIDTH by FRAME-HEIGHT pixels the window system
resizes from its corners: (LEFT . RIGHT) pixels.  The bar runs from
column LEFT to RIGHT of the frame, its top at row TOP.  Mac OS 9 stood
the boxes `lispm-desktop-platinum-box-margin' in; a box that would so lie
in one of `lispm-desktop-platinum-resize-corners' stands past it, as a
press there resizes the frame and never reaches the box:

  margin(side) = max (box-margin,
                      max { square - inset(side) | a corner at side
                            whose rows the boxes' rows cross })

where inset(left) = LEFT and inset(right) = FRAME-WIDTH - RIGHT.  A bar
standing a corner's square in from the frame's side keeps Mac OS 9's."
  (let* ((from (+ top lispm-desktop-platinum-box-top))
         (to (+ from lispm-desktop-platinum-box-size))
         (insets `((left . ,left) (right . ,(- frame-width right)))))
    (cl-flet ((margin (side)
                (seq-reduce (pcase-lambda (margin `(,corner ,end . ,square))
                              (if (and (eq corner side)
                                       (if (eq end 'top)
                                           (< from square)
                                         (> to (- frame-height square))))
                                  (max margin (- square (alist-get side insets)))
                                margin))
                            lispm-desktop-platinum-resize-corners
                            lispm-desktop-platinum-box-margin)))
      (cons (margin 'left) (margin 'right)))))

(defun lispm-desktop-platinum-window-box-margins (window)
  "Return how far in from the ends of WINDOW's title bar its boxes stand,
see `lispm-desktop-platinum--box-margins': Mac OS 9's unless the window
system resizes WINDOW's frame from its corners.  A floating window's
frame has two of them at its title bar's ends, and, rolled up, all four."
  (let ((frame (window-frame window))
        (margin lispm-desktop-platinum-box-margin))
    (if (lispm-desktop-platinum--resized-from-corners-p frame)
        (pcase-let ((`(,left ,top ,right ,_) (window-pixel-edges window)))
          (lispm-desktop-platinum--box-margins
           (frame-native-width frame) (frame-native-height frame)
           left (- right (window-right-divider-width window) (window-scroll-bar-width window))
           top))
      (cons margin margin))))

(cl-defgeneric lispm-desktop-platinum-title-layout (width title-width &optional controls margins)
  "Return how a title bar WIDTH pixels wide lays out a title TITLE-WIDTH wide.
CONTROLS, as `lispm-desktop-platinum-window-controls' returns them,
says which boxes the bar holds, by default all three.  MARGINS,
\(LEFT . RIGHT) pixels, stands the boxes nearest the bar's ends that far
in, by default Mac OS 9's `lispm-desktop-platinum-box-margin'; an end
without boxes keeps Mac OS 9's.  A window's bar stands its boxes clear
of the corners its frame is resized from, see
`lispm-desktop-platinum-window-box-margins'.  The value is a plist: the
title stretch, between the boxes, runs
from :start to :end; the title, as wide as :title, starts at :label-x;
:ridges lists the \(FROM . TO) columns of the ridges either side;
:margins, (LEFT . RIGHT), the bar's ground before the close box and after
the last box.  Spacing is Mac OS 9's: the close box three pixels in from
the edge, four pixels to the ridges, four from the ridges to the zoom
box, three to the collapse box and three to the edge.  The title is
centred in the bar unless that would push it into the boxes; a title too
wide is narrowed so that a short run of ridges stays on either side."
  (pcase-let* ((`(,close ,zoom ,collapse) (or controls '(on on on)))
               (s lispm-desktop-platinum-box-size)
               (margin lispm-desktop-platinum-box-margin)
               (`(,lead . ,trail) (or margins (cons margin margin)))
               (lead (if close lead 0))
               (trail (if (or zoom collapse) trail margin))
               (start (if close (+ lead s) 0))
               (end (- width trail (if zoom s 0) (if (and zoom collapse) 3 0) (if collapse s 0)))
               (from (+ start (if close 4 7)))
               (to (- end (if (or zoom collapse) 5 8)))
               (pad 6)
               (least 12)
               (title (max 0 (min title-width (- to from (* 2 (+ pad least))))))
               (label-x (max (+ from least pad)
                             (min (/ (- width title) 2) (- to least pad title)))))
    (list :start start :end end :title title :label-x label-x :margins (cons lead trail)
          :ridges (list (cons from (- label-x pad)) (cons (+ label-x title pad) to)))))

(defun lispm-desktop-platinum--fixed-p (window)
  "Return non-nil when WINDOW keeps its place and size: a side window, such
as the sidebar, or chrome, such as the browser's address window."
  (or (window-parameter window 'window-side)
      (window-parameter window 'lispm-desktop-chrome)))

(defun lispm-desktop-platinum-window-controls (window)
  "Return the title bar boxes of WINDOW as (CLOSE ZOOM COLLAPSE).
Each is `on' or nil, a box the window lacks being left out, as Mac OS 9
left it out.  A side window or chrome closes but neither zooms nor
collapses; a window collapses only when another shares its column."
  (let ((fixed (lispm-desktop-platinum--fixed-p window))
        (functions (buffer-local-value 'lispm-desktop-platinum-box-functions (window-buffer window))))
    (if functions
        (mapcar (lambda (box) (and (plist-get functions box) 'on))
                '(:close :zoom :collapse))
      (list 'on
            (and (not fixed) 'on)
            (and (not fixed) (window-combined-p window) 'on)))))

(defun lispm-desktop-platinum--title-bar (window)
  "Return the title bar of WINDOW, drawn for its current state."
  (let* ((buffer (window-buffer window))
         ;; Chrome wearing another window's title bar, as the browser's
         ;; address window wears the page's, is active with that window.
         (active (or (mode-line-window-selected-p)
                     (eq (window-parameter window 'lispm-desktop-platinum-title-for)
                         (old-selected-window))
                     (when-let* ((active (buffer-local-value 'lispm-desktop-platinum-active-function
                                                             buffer)))
                       (funcall active window))))
         (pressed (window-parameter window 'lispm-desktop-platinum-pressed))
         (width (- (window-pixel-width window)
                   (window-right-divider-width window)
                   (window-scroll-bar-width window)))
         (icon (lispm-desktop-platinum--buffer-icon buffer))
         (title (lispm-desktop-platinum-buffer-title buffer))
         (modified (and (buffer-file-name buffer) (buffer-modified-p buffer)))
         (controls (lispm-desktop-platinum-window-controls window))
         ;; What its program says it is doing, as a terminal's does.
         (status (run-hook-with-args-until-success 'lispm-desktop-platinum-buffer-status-functions
                                                   buffer))
         ;; A window's frame drawn about its floating frame lights and
         ;; shades the bar's ends already.
         (framed (lispm-desktop-platinum--framed-p (window-frame window)))
         (edges (cons (not (and framed (window-at-side-p window 'left)))
                      (not (and framed (window-at-side-p window 'right)))))
         (margins (lispm-desktop-platinum-window-box-margins window))
         (tty (lispm-desktop-tty-p (window-frame window)))
         (key (list lispm-desktop-platinum-generation width active icon title modified controls
                    pressed status edges margins tty))
         (cached (window-parameter window 'lispm-desktop-platinum-title-bar)))
    (if (equal (car cached) key)
        (cdr cached)
      (let ((bar (if tty
                     (lispm-desktop-platinum-text-title-bar
                      (- width (if (window-at-side-p window 'right) 0 1))
                      active icon title modified controls pressed status)
                   (lispm-desktop-platinum-draw-title-bar width active icon title modified controls
                                                          pressed status edges margins))))
        (set-window-parameter window 'lispm-desktop-platinum-title-bar (cons key bar))
        bar))))

(cl-defgeneric lispm-desktop-platinum-draw-title-bar (width active icon title modified controls
                                                     &optional pressed status edges margins)
  "Return a title bar WIDTH pixels wide for ICON and TITLE.
ACTIVE draws the ridges and boxes; an inactive bar lies flat, its title
greyed.  MODIFIED marks the close box; CONTROLS is a value of
`lispm-desktop-platinum-window-controls'; PRESSED names the box held
down, if any; STATUS, a coloured label, follows the title.  EDGES,
\(LEAD . TRAIL), says whether the bar lights its left end and shades its
right, by default both.  MARGINS stands the boxes in from the bar's
ends, see `lispm-desktop-platinum-title-layout'.  The bar is drawn as
images, a stretch each between its boxes, so its lines run under the
title too."
  (pcase-let* ((label (concat (when icon (concat icon " ")) title
                              (when status (concat "  " status))))
               (face (if active 'lispm-desktop-platinum-title 'lispm-desktop-platinum-title-inactive))
               (`(,close ,zoom ,collapse) controls)
               (layout (lispm-desktop-platinum-title-layout
                        width (lispm-desktop-platinum-content-width label face) controls margins))
               (`(,lead-margin . ,trail-margin) (plist-get layout :margins))
               (label (let ((label (copy-sequence label)))
                        (add-face-text-property 0 (length label) face t label)
                        (lispm-desktop-platinum-fit label (plist-get layout :title))))
               (start (plist-get layout :start))
               (end (plist-get layout :end))
               (lead (if edges (car edges) t))
               (trail (if edges (cdr edges) t))
               (title-help (lispm-desktop-platinum-title-help))
               (title-part
                (lambda (from to &rest options)
                  (propertize (apply #'lispm-desktop-platinum--title-part (- to from) :active active
                                     :label label :label-x (- (plist-get layout :label-x) from)
                                     options)
                              'keymap lispm-desktop-platinum-title-map
                              'help-echo title-help))))
    (if (not active)
        (funcall title-part 0 width)
      (cl-flet ((box (kind command help)
                  (lispm-desktop-platinum-box-button
                   (propertize " " 'display (lispm-desktop-platinum--box kind (and (eq kind 'close) modified)
                                                                        (eq pressed kind)))
                   kind command help))
                (plain (width &rest options)
                  (apply #'lispm-desktop-platinum--title-part width :active t options)))
        (concat
         (when close
           (concat (plain lead-margin :lead lead)
                   (box 'close #'lispm-desktop-platinum-close-window (lispm-desktop-tr "Close this window"))))
         (funcall title-part start end :lead (and lead (not close))
                  :ridges (mapcar (pcase-lambda (`(,from . ,to)) (cons (- from start) (- to start)))
                                  (plist-get layout :ridges)))
         (when zoom
           (box 'zoom #'lispm-desktop-platinum-zoom-window (lispm-desktop-tr "Zoom this window")))
         (when (and zoom collapse)
           (plain 3))
         (when collapse
           (box 'collapse #'lispm-desktop-platinum-collapse-window
                (lispm-desktop-tr "Collapse this window to its title bar")))
         (plain trail-margin :trail trail))))))

(defun lispm-desktop-platinum-text-title-bar (width active icon title modified controls
                                                    &optional pressed status)
  "Return a title bar WIDTH columns wide on a text terminal, as text.
Turbo Vision drew windows in text much as Mac OS 9 drew them in pixels,
and the bar follows it: the close box at its left, the title centred
between double rules and the zoom and collapse boxes at its right, all
drawn only while the window is active; an inactive bar is a single rule,
its title greyed.  The arguments are those of
`lispm-desktop-platinum-draw-title-bar'; MODIFIED marks the close box
with a dot, and PRESSED shows its box held down."
  (pcase-let* ((`(,close ,zoom ,collapse) controls)
               (palette lispm-desktop-platinum-current-palette)
               (rule `(:foreground ,(plist-get palette (if active :text :muted))))
               (face (if active 'lispm-desktop-platinum-title 'lispm-desktop-platinum-title-inactive))
               (title-help (lispm-desktop-platinum-title-help))
               (rules (lambda (n)
                        (propertize (make-string (max 0 n) (if active ?═ ?─))
                                    'face rule 'keymap lispm-desktop-platinum-title-map
                                    'help-echo title-help)))
               (box (lambda (kind glyph command help)
                      (lispm-desktop-platinum-box-button
                       (concat (propertize "[" 'face rule)
                               (propertize glyph 'face (if (eq pressed kind)
                                                           `(:foreground ,(plist-get palette :highlight-text)
                                                             :background ,(plist-get palette :highlight))
                                                         rule))
                               (propertize "]" 'face rule))
                       kind command help)))
               (lead (if (and active close)
                         (concat (funcall rules 1)
                                 (funcall box 'close (if modified "•" "■")
                                          #'lispm-desktop-platinum-close-window
                                          (lispm-desktop-tr "Close this window")))
                       ""))
               (trail (if active
                          (concat (when zoom
                                    (funcall box 'zoom "↕" #'lispm-desktop-platinum-zoom-window
                                             (lispm-desktop-tr "Zoom this window")))
                                  (when collapse
                                    (funcall box 'collapse "─" #'lispm-desktop-platinum-collapse-window
                                             (lispm-desktop-tr "Collapse this window to its title bar")))
                                  (when (or zoom collapse) (funcall rules 1)))
                        ""))
               ;; At least two rules either side of the title.
               (room (- width (string-width lead) (string-width trail) 6))
               (label (concat (when icon (concat icon " ")) title (when status (concat "  " status))))
               (label (let ((label (truncate-string-to-width label (max 0 room) nil nil "…")))
                        (add-face-text-property 0 (length label) face t label)
                        (propertize (concat " " label " ")
                                    'keymap lispm-desktop-platinum-title-map 'help-echo title-help)))
               (free (- width (string-width lead) (string-width trail) (string-width label)))
               ;; Centred in the bar unless that would push it into the boxes.
               (before (max 2 (min (- free 2) (- (/ (- width (string-width label)) 2)
                                                 (string-width lead))))))
    (concat lead (funcall rules before) label (funcall rules (- free before)) trail)))

(defun lispm-desktop-platinum-title-bar ()
  "The tab line of every window: its title bar.
See `lispm-desktop-platinum--title-bar'."
  (lispm-desktop-platinum--title-bar (selected-window)))

;;;; Title bar commands

(defun lispm-desktop-platinum-close-window (window)
  "Close WINDOW as Mac OS closed a window: put its document away.
A sidebar is hidden; a window whose frame names a function closing it,
its frame parameter `lispm-desktop-close-function', as an application's
window does, is closed by that function.  Otherwise WINDOW's buffer is
killed, once an alert has asked about unsaved changes or a running
process, and WINDOW goes too when other windows remain in the main area;
the last one shows the desktop."
  (cond
   ((lispm-desktop-platinum--box-function window :close)
    (funcall (lispm-desktop-platinum--box-function window :close) window))
   ((frame-parameter (window-frame window) 'lispm-desktop-close-function)
    (funcall (frame-parameter (window-frame window) 'lispm-desktop-close-function) window))
   ((window-parameter window 'window-side)
    (funcall lispm-desktop-platinum-side-window-close-function window))
   (t
    (let ((buffer (window-buffer window)))
      (when (lispm-desktop-platinum-confirm-close buffer)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))
        ;; The main area's last window uncovers the desktop, as closing
        ;; Mac OS 9's last window did; any other window goes.
        (when (window-live-p window)
          (if (cdr (lispm-desktop-platinum-main-windows))
              (delete-window window)
            (funcall lispm-desktop-platinum-last-window-function window))))))))

(defun lispm-desktop-platinum-confirm-close (buffer)
  "Ask, as Mac OS 9 asked, whether BUFFER may close; return non-nil if so.
A modified file asks to be saved first, and is saved when told to; a
buffer whose process still runs asks before the process is stopped."
  (let ((name (lispm-desktop-platinum-buffer-title buffer))
        (process (get-buffer-process buffer)))
    (cond
     ((and (buffer-file-name buffer) (buffer-modified-p buffer))
      (pcase (lispm-desktop-popup-alert
              'caution (lispm-desktop-tr "Save changes to the document “%s” before closing?" name)
              `((,(lispm-desktop-tr "Don’t Save") discard nil ?\M-d)
                (,(lispm-desktop-tr "Cancel") cancel cancel)
                (,(lispm-desktop-tr "Save") save default))
              (lispm-desktop-tr "If you don’t save, your changes will be lost."))
        ('save (with-current-buffer buffer (save-buffer)) t)
        ('discard (with-current-buffer buffer (set-buffer-modified-p nil)) t)))
     ((and process (process-live-p process))
      (eq 'close (lispm-desktop-popup-alert
                  'caution (lispm-desktop-tr "Close “%s”?" name)
                  `((,(lispm-desktop-tr "Cancel") cancel cancel) (,(lispm-desktop-tr "Close") close default))
                  (lispm-desktop-tr "What runs in it will be stopped."))))
     (t t))))

(defun lispm-desktop-platinum--yes-or-no (prompt &rest _)
  "Ask PROMPT, a question answered yes or no, in an alert; non-nil for Yes."
  (eq 'yes (lispm-desktop-popup-alert
            'caution
            (replace-regexp-in-string "\\s-*([^()]*)\\s-*\\'" "" (string-trim prompt))
            `((,(lispm-desktop-tr "No") no cancel ?n) (,(lispm-desktop-tr "Yes") yes default ?y)))))

(defun lispm-desktop-platinum-quoted-names (names)
  "Return NAMES as a phrase: the first three quoted, then a count of the
rest, in the language LispM speaks."
  (let ((quoted (mapcar (lambda (name) (lispm-desktop-tr "“%s”" (string-trim name "\\*" "\\*")))
                        (seq-take names 3)))
        (comma (lispm-desktop-tr-in "list" ", "))
        (rest (- (length names) 3)))
    (cond ((> rest 0)
           (lispm-desktop-tr-n "%s and %d other" "%s and %d others" rest (string-join quoted comma) rest))
          ((cdr quoted)
           (lispm-desktop-tr "%s and %s" (string-join (butlast quoted) comma) (car (last quoted))))
          (t (car quoted)))))

(defun lispm-desktop-platinum--running-programs ()
  "Return the titles of the buffers whose programs quitting Emacs stops:
those of Emacs's processes that ask before they are killed, and those
`lispm-desktop-platinum-running-buffers-functions' return, as terminals
whose programs are not Emacs's processes."
  (delete-dups
   (append
    (delq nil (mapcar (lambda (process)
                        (when (and (memq (process-status process) '(run stop open listen))
                                   (process-query-on-exit-flag process))
                          (if (buffer-live-p (process-buffer process))
                              (lispm-desktop-platinum-buffer-title (process-buffer process))
                            (process-name process))))
                      (process-list)))
    (mapcar #'lispm-desktop-platinum-buffer-title
            (apply #'append (mapcar #'funcall lispm-desktop-platinum-running-buffers-functions))))))

(defun lispm-desktop-platinum--unsaved-documents ()
  "Return the buffers `save-buffers-kill-emacs' offers to save."
  (if (fboundp 'files--buffers-needing-to-be-saved)
      (files--buffers-needing-to-be-saved t)
    (seq-filter (lambda (buffer) (and (buffer-file-name buffer) (buffer-modified-p buffer)))
                (buffer-list))))

(defun lispm-desktop-platinum--quit-documents (documents restart)
  "Ask of each of DOCUMENTS whether to save it before quitting, or
RESTART'ing, as Mac OS 9 asked; return nil if one was cancelled."
  (catch 'cancel
    (dolist (buffer documents)
      (pcase (lispm-desktop-popup-alert
              'caution (if restart
                           (lispm-desktop-tr "Save changes to the document “%s” before restarting?"
                                             (lispm-desktop-platinum-buffer-title buffer))
                         (lispm-desktop-tr "Save changes to the document “%s” before quitting?"
                                           (lispm-desktop-platinum-buffer-title buffer)))
              ;; y and n answer as `save-some-buffers' takes them.
              `((,(lispm-desktop-tr "Don’t Save") discard nil ?\M-d ?n)
                (,(lispm-desktop-tr "Cancel") cancel cancel)
                (,(lispm-desktop-tr "Save") save default ?y))
              (lispm-desktop-tr "If you don’t save, your changes will be lost."))
        ('save (with-current-buffer buffer (save-buffer)))
        ('cancel (throw 'cancel nil))))
    t))

(defun lispm-desktop-platinum--quit (quit &optional arg restart)
  "Quit Emacs as QUIT, `save-buffers-kill-emacs', does, asking in alerts
instead of the minibuffer: whether to save each modified document, and
whether to stop the programs still running, agents in terminals among
them.  ARG saves every document unasked; RESTART starts
Emacs again.  Where alerts cannot be drawn, QUIT asks."
  (if (not (lispm-desktop-popup-available-p))
      (funcall quit arg restart)
    (cl-letf (((symbol-function 'yes-or-no-p) #'lispm-desktop-platinum--yes-or-no)
              ((symbol-function 'y-or-n-p) #'lispm-desktop-platinum--yes-or-no))
      (let* ((verb (if restart (lispm-desktop-tr "Restart") (lispm-desktop-tr "Quit")))
             (documents (lispm-desktop-platinum--unsaved-documents))
             (programs (and confirm-kill-processes (lispm-desktop-platinum--running-programs))))
        (when (and (if arg
                       (progn (save-some-buffers t t) t)
                     (lispm-desktop-platinum--quit-documents documents restart))
                   ;; Abbrevs and the like, which ask yes or no.
                   (progn (dolist (function save-some-buffers-functions)
                            (funcall function nil arg))
                          t)
                   (or (null programs)
                       (eq 'quit (lispm-desktop-popup-alert
                                  'caution (if restart
                                               (lispm-desktop-tr "Restart Emacs while programs are running?")
                                             (lispm-desktop-tr "Quit Emacs while programs are running?"))
                                  `((,(lispm-desktop-tr "Cancel") cancel cancel) (,verb quit default))
                                  (if restart
                                      (lispm-desktop-tr "Restarting stops %s."
                                                        (lispm-desktop-platinum-quoted-names programs))
                                    (lispm-desktop-tr "Quitting stops %s."
                                                      (lispm-desktop-platinum-quoted-names programs))))))
                   (run-hook-with-args-until-failure 'kill-emacs-query-functions)
                   ;; As `save-buffers-kill-emacs', confirm only when nothing
                   ;; was asked already.
                   (or (null confirm-kill-emacs) documents programs
                       (funcall confirm-kill-emacs
                                (if restart
                                    (lispm-desktop-tr "Really restart Emacs? ")
                                  (lispm-desktop-tr "Really quit Emacs? ")))))
          (kill-emacs nil restart))))))

(defun lispm-desktop-platinum-zoom-window (window)
  "Zoom WINDOW to the whole main area, or back to where it was.
The sidebar stays, as `delete-other-windows' leaves side windows be."
  (let ((saved (frame-parameter nil 'lispm-desktop-platinum-unzoom)))
    (cond
     ((lispm-desktop-platinum--box-function window :zoom)
      (funcall (lispm-desktop-platinum--box-function window :zoom) window))
     ((and saved (eq (car saved) window) (not (cdr (lispm-desktop-platinum-main-windows))))
      (set-frame-parameter nil 'lispm-desktop-platinum-unzoom nil)
      (set-window-configuration (cdr saved)))
     (t
      (set-frame-parameter nil 'lispm-desktop-platinum-unzoom
                           (cons window (current-window-configuration)))
      (delete-other-windows window)))))

(defun lispm-desktop-platinum-collapse-window (window)
  "Roll WINDOW up to its title bar, or down again, as a window shade.
Only a window sharing its column with another can roll up."
  (cond
   ((lispm-desktop-platinum--box-function window :collapse)
    (funcall (lispm-desktop-platinum--box-function window :collapse) window))
   ((window-parameter window 'lispm-desktop-platinum-shade)
    (let ((height (window-parameter window 'lispm-desktop-platinum-shade)))
      (set-window-parameter window 'lispm-desktop-platinum-shade nil)
      (window-resize window (- height (window-total-height window)) nil t)))
   ((window-combined-p window)
    (set-window-parameter window 'lispm-desktop-platinum-shade (window-total-height window))
    (window-resize window (- (window-min-size window nil t)
                             (window-total-height window))
                   nil t))))

(defun lispm-desktop-platinum-window-buffers (window)
  "Return the buffers WINDOW could show: its workspace's.
Hidden buffers and the sidebar's lists are left out."
  (seq-remove (lambda (buffer)
                (or (string-prefix-p " " (buffer-name buffer))
                    (not (buffer-local-value 'lispm-desktop-platinum-listed buffer))))
              (if (bound-and-true-p tabspaces-mode)
                  (tabspaces--buffer-list (window-frame window))
                (buffer-list (window-frame window)))))

(defun lispm-desktop-platinum-shade-or-zoom (window)
  "Roll WINDOW up or down, or zoom it when it cannot roll up.
A double click on a title bar does this, as on Mac OS 9; a window alone
in its column cannot roll up, so it zooms instead.  A side window or
chrome does neither."
  (cond ((lispm-desktop-platinum--box-function window :collapse)
         (funcall (lispm-desktop-platinum--box-function window :collapse) window))
        ((lispm-desktop-platinum--fixed-p window))
        ((or (window-parameter window 'lispm-desktop-platinum-shade) (window-combined-p window))
         (lispm-desktop-platinum-collapse-window window))
        (t (lispm-desktop-platinum-zoom-window window))))

(defvar-local lispm-desktop-platinum-title-menu-function nil
  "Function returning the menu a Command click on this buffer's title pops
up, called with the window, or nil for the workspace's buffers.  The
Finder window gives its folder's path, as Mac OS 9's did.")

(defun lispm-desktop-platinum--title-xy (window)
  "Return where a menu drops from WINDOW's title: (X . Y) in its frame,
the title's left under the title bar."
  (let* ((buffer (window-buffer window))
         (icon (lispm-desktop-platinum--buffer-icon buffer))
         (label (concat (when icon (concat icon " ")) (lispm-desktop-platinum-buffer-title buffer)))
         (layout (lispm-desktop-platinum-title-layout
                  (- (window-pixel-width window) (window-right-divider-width window)
                     (window-scroll-bar-width window))
                  (lispm-desktop-platinum-content-width label 'lispm-desktop-platinum-title)
                  (lispm-desktop-platinum-window-controls window)
                  (lispm-desktop-platinum-window-box-margins window)))
         (edges (window-pixel-edges window)))
    (cons (+ (nth 0 edges) (plist-get layout :label-x))
          (+ (nth 1 edges) (window-tab-line-height window)))))

(defun lispm-desktop-platinum-title-menu (window &optional under-title)
  "Pop up the buffers WINDOW could show, and show the one chosen.
As Mac OS 9 popped up a window's path from its title, the title pops up
the workspace's buffers, the one WINDOW shows checked, or what its
buffer's `lispm-desktop-platinum-title-menu-function' gives.  A buffer
chosen goes to WINDOW unless the main area's columns send it elsewhere:
a session to the left, a file to the right.  The menu pops up under the
mouse, or UNDER-TITLE, from WINDOW's title, as it does interactively:
from the keyboard, for the selected window."
  (interactive (list (selected-window) t))
  (pcase-let ((`(,x . ,y) (and under-title (lispm-desktop-platinum--title-xy window))))
    (lispm-desktop-popup-menu
     (if-let* ((function (buffer-local-value 'lispm-desktop-platinum-title-menu-function
                                             (window-buffer window))))
         (funcall function window)
       (mapcar (lambda (buffer)
                 (list :label (lispm-desktop-platinum-buffer-title buffer)
                       :action (lambda ()
                                 (select-window window)
                                 (switch-to-buffer buffer))
                       :checked (eq buffer (window-buffer window))))
               (seq-take (lispm-desktop-platinum-window-buffers window) 30)))
     x y)))

(provide 'lispm-desktop-platinum-titlebar)
;;; lispm-desktop-platinum-titlebar.el ends here
