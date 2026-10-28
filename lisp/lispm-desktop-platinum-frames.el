;;; lispm-desktop-platinum-frames.el --- Floating windows: their frames, bottoms, moves and growth  -*- lexical-binding: t; -*-

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

;; What makes a child frame a Mac OS 9 window: the frame drawn round it,
;; its bottom edge with placards and the grow box, rolling it up into its
;; title bar, and moving and growing it by an outline.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-platinum-controls)
(require 'lispm-desktop-platinum-icons)
(require 'lispm-ui-modal)


;;;; A window's title, its moves and rolling it up


(defvar-local lispm-desktop-platinum-title nil
  "Title the window of this buffer shows instead of the buffer's own name:
a string, or a function returning one when the window is drawn, as a
title in words of the language LispM speaks is, see
lispm-desktop-i18n.el.")

(defun lispm-desktop-platinum-title-text (&optional buffer)
  "Return the title `lispm-desktop-platinum-title' gives BUFFER's window,
by default the current buffer's, or nil."
  (let ((title (buffer-local-value 'lispm-desktop-platinum-title (or buffer (current-buffer)))))
    (if (functionp title)
        (with-current-buffer (or buffer (current-buffer)) (funcall title))
      title)))

(defvar-local lispm-desktop-platinum-icon nil
  "Icon the window of this buffer shows before its title, a string.")

(defvar lispm-desktop-platinum-frame-moved-functions nil
  "Functions called with a floating window once it was moved or grown by
its outline, from its title bar, its frame or its grow box, so it can be
kept in place.")

(defun lispm-desktop-platinum--movable-p (frame)
  "Return non-nil when FRAME moves by its title bar: a child frame, or a
floating window out on the computer's screen."
  (or (frame-parent frame) (frame-parameter frame 'lispm-desktop-window)))

(defun lispm-desktop-platinum-space (width)
  "Return a blank WIDTH pixels wide."
  (propertize " " 'display `(space :width (,(max 0 width)))))

(defconst lispm-desktop-platinum--rolled-up-locals
  '((window-min-height . 1) (window-safe-min-height . 0)
    (window-resize-pixelwise . t) (window-scroll-functions . nil))
  "The values the buffer of a rolled-up window holds as its own.
A window keeps `window-min-height' lines and, under its tab line, a line
of `window-safe-min-height', rounded up to whole lines unless resized
pixelwise; with these, it may be as short as its tab line.  With no line
of its text in sight, Emacs scrolls it at every redisplay, so no
`window-scroll-functions' run on it: the scroll bars' would have Emacs
redisplay again at once, and again.")

(defun lispm-desktop-platinum-roll-up (frame height)
  "Roll floating window FRAME up to HEIGHT pixels, as Mac OS 9's
WindowShade rolled a window up into its title bar.  FRAME's one window
may be as short as its title bar, its tab line: its buffer holds the
values of `lispm-desktop-platinum--rolled-up-locals' until
`lispm-desktop-platinum-roll-down'.  FRAME stays a pixel taller than a
line of its font, as Emacs drops the tab line of a window no taller."
  (let ((buffer (window-buffer (frame-root-window frame))))
    (with-current-buffer buffer
      (pcase-dolist (`(,variable . ,value) lispm-desktop-platinum--rolled-up-locals)
        (set (make-local-variable variable) value)))
    (set-frame-parameter frame 'lispm-desktop-platinum-rolled-up
                         (cons buffer (frame-text-height frame)))
    (set-frame-height frame (max height (1+ (frame-char-height frame))) nil t)))

(defun lispm-desktop-platinum-rolled-up-height (frame)
  "Return the height in pixels of FRAME's text before it was rolled up, or
nil when FRAME is not rolled up."
  (cdr (frame-parameter frame 'lispm-desktop-platinum-rolled-up)))

(defun lispm-desktop-platinum-roll-down (frame height)
  "Roll floating window FRAME, rolled up by `lispm-desktop-platinum-roll-up',
down again to HEIGHT pixels."
  (when-let* ((buffer (car (frame-parameter frame 'lispm-desktop-platinum-rolled-up)))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (dolist (local lispm-desktop-platinum--rolled-up-locals)
        (kill-local-variable (car local)))))
  (set-frame-parameter frame 'lispm-desktop-platinum-rolled-up nil)
  (set-frame-height frame height nil t))

;;;; Window bottoms

(defvar-local lispm-desktop-platinum-info nil
  "What the bottom of this buffer's window says instead of the buffer's state.
List windows set it, as Finder's windows counted their items: a string,
or a function returning one when the window is drawn.  A function may
return a list of strings instead, each on a placard of its own, or nil
for none: an application's window says only what it has to say.")

(defconst lispm-desktop-platinum--grow-ridges
  '("wwwwwwwwwwwwwww" "w" "w" "w       ww" "w      w h" "w     w h ww" "w    w h w h"
    "w   w h w h ww" "w  w h w h w h" "w  ih w h w h" "w    w h w h" "w    ih w h"
    "w      w h" "w      ih" "w")
  "The inside of Mac OS 9's grow box, a row of pixels a string: lit along
its top and left (w), crossed by three diagonal ridges, each a lit line
beside a shaded one (h), softened at their lower ends (i).")

(cl-defgeneric lispm-desktop-platinum-draw-grow-box (palette height)
  "Return an SVG of the grow box, the ridged square in a window's corner.
It is sixteen pixels square, outlined along its top and left, in
PALETTE's greys, centred in HEIGHT pixels."
  (let* ((s 16)
         (svg (svg-create s height))
         (y (/ (- height s) 2)))
    (lispm-desktop-platinum-svg-fill svg 0 0 s height (plist-get palette :chrome))
    (lispm-desktop-platinum-svg-fill svg 0 y s s (plist-get palette :outline))
    (lispm-desktop-platinum-svg-fill svg 1 (1+ y) (1- s) (1- s) (lispm-desktop-platinum-grey palette #xCC))
    (lispm-desktop-platinum-svg-pixel-map svg 1 (1+ y) lispm-desktop-platinum--grow-ridges
                                      `((?w . ,(lispm-desktop-platinum-grey palette #xFF))
                                        (?h . ,(lispm-desktop-platinum-grey palette #x77))
                                        (?i . ,(lispm-desktop-platinum-grey palette #xAA))))
    svg))

(defun lispm-desktop-platinum--grow-press (event)
  "Resize the child frame whose grow box EVENT pressed, from its corner, by
its outline, as Mac OS 9's GrowWindow did, see
`lispm-desktop-platinum-drag-outline'.  Floating windows resize so;
elsewhere windows tile, and the grow box only marks the corner."
  (interactive "e")
  (let* ((window (posn-window (event-start event)))
         (frame (and (windowp window) (window-frame window))))
    (when (and frame (lispm-desktop-platinum--movable-p frame))
      (lispm-desktop-platinum-drag-outline frame 'grow))))

(defvar lispm-desktop-platinum--grow-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<mode-line> <down-mouse-1>" #'lispm-desktop-platinum--grow-press)
    (keymap-set map "<mode-line> <mouse-1>" #'ignore)
    map)
  "Keymap of the grow box.")

(defun lispm-desktop-platinum--grow-box ()
  "Return the grow box as a string, a corner of text on a text terminal."
  (propertize (if (lispm-desktop-tty-p) "◢" " ")
              'display (unless (lispm-desktop-tty-p)
                         (lispm-desktop-platinum-image
                            (list 'grow)
                            (lambda () (lispm-desktop-platinum-draw-grow-box lispm-desktop-platinum-current-palette
                                                                             lispm-desktop-platinum-title-height))))
              'keymap lispm-desktop-platinum--grow-map))

(defun lispm-desktop-platinum--placard (text active)
  "Return TEXT as a placard, the outlined plate Mac OS 9 showed status in.
ACTIVE outlines it in black; an inactive window's placards are outlined
in grey.  The placard's face is layered under TEXT's own, so coloured
counts survive."
  (let* ((pad (propertize " " 'display `(space :width (,(lispm-desktop-px 6)))))
         (placard (concat pad text pad)))
    (add-face-text-property 0 (length placard)
                            (if active 'lispm-desktop-platinum-placard 'lispm-desktop-platinum-placard-inactive)
                            t placard)
    placard))

(defun lispm-desktop-platinum--mode-line-text (construct)
  "Return mode line CONSTRUCT formatted, trimmed, and safe to show again.
It is formatted for the current buffer, the one whose window is drawn."
  (string-replace "%" "%%" (string-trim (format-mode-line construct nil nil
                                                          (current-buffer)))))

(defvar evil-mode-line-tag)
(defvar flymake-mode-line-counters)

(defun lispm-desktop-platinum-mode-line-left ()
  "The left of a window's bottom edge: its state, buffer and position.
A list window shows its `lispm-desktop-platinum-info' instead."
  (let ((active (mode-line-window-selected-p)))
    (concat
     (propertize " " 'display (lispm-desktop-platinum--strut 8))
     (if lispm-desktop-platinum-info
         (mapconcat (lambda (text) (lispm-desktop-platinum--placard (string-replace "%" "%%" text) active))
                    (ensure-list (if (functionp lispm-desktop-platinum-info)
                                     (funcall lispm-desktop-platinum-info)
                                   lispm-desktop-platinum-info))
                    " ")
       (mapconcat
        (lambda (text) (lispm-desktop-platinum--placard text active))
        (delq nil
              (list (when (and (bound-and-true-p evil-local-mode)
                               (stringp evil-mode-line-tag))
                      (string-trim (substring-no-properties evil-mode-line-tag) "[ <]+" "[> ]+"))
                    (concat (string-replace "%" "%%" (or (lispm-desktop-platinum-title-text) (buffer-name)))
                            (when (and buffer-file-name (buffer-modified-p)) " •"))
                    (lispm-desktop-platinum--mode-line-text mode-line-position)))
        " ")))))

(defun lispm-desktop-platinum-mode-line-right ()
  "The right of a window's bottom edge: diagnostics, branch, mode, grow box.
Only the active window shows the grow box, and only where the window can
be resized; a side window cannot."
  (let* ((active (mode-line-window-selected-p))
         (parts (unless lispm-desktop-platinum-info
                  (delq nil
                        (list (when (bound-and-true-p flymake-mode)
                                (let ((counts (lispm-desktop-platinum--mode-line-text
                                               flymake-mode-line-counters)))
                                  (unless (string-empty-p counts) counts)))
                              (when (stringp vc-mode)
                                (replace-regexp-in-string
                                 "\\`[A-Za-z]+[-:@!?]" ""
                                 (string-trim (substring-no-properties vc-mode))))
                              (lispm-desktop-platinum--mode-line-text mode-name))))))
    (concat (mapconcat (lambda (text) (lispm-desktop-platinum--placard text active)) parts " ")
            (propertize " " 'display `(space :width (,(lispm-desktop-px 6))))
            (if (and active (not (window-parameter (selected-window) 'window-side)))
                (lispm-desktop-platinum--grow-box)
              (lispm-desktop-platinum-space (if (lispm-desktop-tty-p) 1 16)))
            (lispm-desktop-platinum-space (lispm-desktop-px 3)))))

(defconst lispm-desktop-platinum-mode-line-format
  '((:eval (lispm-desktop-platinum-mode-line-left))
    mode-line-format-right-align
    (:eval (lispm-desktop-platinum-mode-line-right)))
  "The mode line of every window: its bottom edge, laid out as Mac OS 9's.")

;;;; Window frames
;;
;; A floating window (a child frame with the parameter
;; `lispm-desktop-window', as the Finder window, Control Panels, the
;; floating editor and browser) is framed as Mac OS 9 framed its windows,
;; and is one window, as Mac OS 9's was.  Its Window Manager kept a window
;; as one object: the structure its definition drew, about the content
;; the application drew.  Here the frame drawing a window's outline, lit
;; band, content line and drop shadow, see
;; `lispm-desktop-platinum-draw-window-frame', is the parent of the
;; window's own frame, which stands in it at
;; `lispm-desktop-platinum-frame-inset':
;;
;;   screen ── the window's frame ── the window: title bar, content, bottom
;;
;; The window system moves, orders, shows and hides a child frame with
;; its parent, so the two go together of themselves.  Kept here is only
;; what it leaves to Emacs: the frame is its window's size and the insets,
;; and drawn active while its window is.  Whatever moves, raises, shows or
;; hides a floating window does it to the frame carrying it, through
;; `lispm-desktop-platinum-move-window', `lispm-desktop-platinum-raise-window',
;; `lispm-desktop-platinum-show-window' and `lispm-desktop-platinum-hide-window'.
;; Pressed, the frame moves its window, as Mac OS 9's did; see "Moving and
;; growing windows" below.
;;
;; On macOS the window system also resizes an undecorated frame, as both
;; are, dragged by its edges, behind Emacs's back, and parts the two:
;;
;;   the window's left or top edge   the window moves in its frame, which
;;                                   follows it there
;;   the frame's edge                the frame alone changes, and the
;;                                   window takes the size it leaves it
;;
;; and the window is then kept in place as after its grow box.  A theme
;; changing the frame's font changes its size too, and that is only undone.

(defcustom lispm-desktop-platinum-window-frames t
  "Non-nil frames floating windows as Mac OS 9 framed its windows."
  :type 'boolean
  :group 'lispm-desktop-platinum)

(defun lispm-desktop-platinum-window-border ()
  "Return the border width a floating window's own frame takes: none
where its frame is drawn for it, a pixel of outline where it is not."
  (if lispm-desktop-platinum-window-frames 0 1))

(defun lispm-desktop-platinum--frameable-p (frame)
  "Return non-nil when FRAME is a floating window to be framed.
A window with the frame parameter `lispm-desktop-platinum-own-frame'
draws its own, as a Mac OS 9 window whose definition function drew a
structure of its own: Stickies' notes, an edge of their colour."
  (and lispm-desktop-platinum-window-frames lispm-desktop-platinum-current-palette
       (frame-live-p frame) (display-graphic-p frame)
       (frame-parameter frame 'lispm-desktop-window)
       (not (frame-parameter frame 'lispm-desktop-platinum-own-frame))))

(defun lispm-desktop-platinum-window-frame-p (frame)
  "Return the floating window FRAME draws the frame of, or nil when FRAME
draws no window's frame."
  (let ((window (frame-parameter frame 'lispm-desktop-platinum-frames)))
    (and (frame-live-p window) window)))

(defun lispm-desktop-platinum-chrome-frame-p (frame)
  "Return non-nil when FRAME draws only a part of a window: its frame, or a
line of the outline a window moves or grows by."
  (or (frame-parameter frame 'lispm-desktop-platinum-frames)
      (frame-parameter frame 'lispm-desktop-platinum-outline)))

(defun lispm-desktop-platinum-window-outer (frame)
  "Return the frame carrying floating window FRAME: the frame drawing its
frame, its parent, or FRAME itself while it is not framed."
  (let ((parent (frame-parent frame)))
    (if (and parent (eq (frame-parameter parent 'lispm-desktop-platinum-frames) frame))
        parent
      frame)))

(defun lispm-desktop-platinum--framed-p (frame)
  "Return non-nil when floating window FRAME stands in a frame drawn for it."
  (not (eq (lispm-desktop-platinum-window-outer frame) frame)))

(defun lispm-desktop-platinum-window-parent (frame)
  "Return the frame floating window FRAME floats over, its screen, or nil
while it is out on the computer's screen, a window of its own."
  (frame-parent (lispm-desktop-platinum-window-outer frame)))

(defun lispm-desktop-platinum--inset (frame)
  "Return where floating window FRAME stands in the frame carrying it:
\(LEFT . TOP) pixels."
  (if (lispm-desktop-platinum--framed-p frame)
      lispm-desktop-platinum-frame-inset
    '(0 . 0)))

(defun lispm-desktop-platinum-frame-margins ()
  "Return what a window's frame takes around the window standing in it:
\(LEFT TOP RIGHT BOTTOM) pixels.  Past `lispm-desktop-platinum-frame-inset',
it is as wide on the right and at the bottom as on the left, and a pixel
of shadow more."
  (pcase-let ((`(,side . ,top) lispm-desktop-platinum-frame-inset))
    (list side top (1+ side) (1+ side))))

(defun lispm-desktop-platinum-window-margins (frame)
  "Return what the frame carrying floating window FRAME takes around it:
\(LEFT TOP RIGHT BOTTOM) pixels, see `lispm-desktop-platinum-frame-margins',
or none while FRAME is not framed."
  (if (lispm-desktop-platinum--framed-p frame)
      (lispm-desktop-platinum-frame-margins)
    (list 0 0 0 0)))

(defun lispm-desktop-platinum-window-position (frame)
  "Return where floating window FRAME stands, (X . Y) in pixels: over its
screen, counted from the screen's inside as a child frame's position is,
or on the computer's screen while it is out."
  (pcase-let ((`(,x . ,y) (frame-position (lispm-desktop-platinum-window-outer frame)))
              (`(,left . ,top) (lispm-desktop-platinum--inset frame)))
    (cons (+ x left) (+ y top))))

(defun lispm-desktop-platinum-move-window (frame x y)
  "Move floating window FRAME to X, Y, as `lispm-desktop-platinum-window-position'
counts: the frame carrying it moves, and it with the frame."
  (pcase-let ((`(,left . ,top) (lispm-desktop-platinum--inset frame)))
    (set-frame-position (lispm-desktop-platinum-window-outer frame) (- x left) (- y top))))

(defun lispm-desktop-platinum-raise-window (frame)
  "Raise floating window FRAME over the other windows, its frame with it.
Raised alone, a framed window would come up only within its frame."
  (let ((outer (lispm-desktop-platinum-window-outer frame)))
    (unless (eq outer frame)
      (raise-frame outer))
    (raise-frame frame)))

(defun lispm-desktop-platinum-show-window (frame)
  "Show floating window FRAME in its frame, framing it first when it is to
be framed, see `lispm-desktop-platinum-frame-window'."
  (lispm-desktop-platinum-frame-window frame)
  (let ((outer (lispm-desktop-platinum-window-outer frame)))
    (unless (eq outer frame)
      (lispm-desktop-platinum-frame-update frame)
      (make-frame-visible outer))
    (make-frame-visible frame)))

(defun lispm-desktop-platinum-hide-window (frame)
  "Hide floating window FRAME, and its frame with it."
  (let ((outer (lispm-desktop-platinum-window-outer frame)))
    (make-frame-invisible frame t)
    (unless (eq outer frame)
      (make-frame-invisible outer t))))

(defvar lispm-desktop-platinum--frame-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<down-mouse-1>" #'lispm-desktop-platinum--frame-press)
    (keymap-set map "<mouse-1>" #'ignore)
    (keymap-set map "<drag-mouse-1>" #'ignore)
    map)
  "Keymap of a window's frame: pressed, it moves its window.")

(defun lispm-desktop-platinum--make-window-frame (frame)
  "Return a new frame to draw the frame of floating window FRAME in, over
what FRAME floats over, not yet shown.  It takes no focus, and hooks run
for new frames do not see it."
  (let* ((buffer (generate-new-buffer " *lispm-desktop-window-frame*"))
         (outer (let ((after-make-frame-functions nil)
                      (before-make-frame-hook nil))
                  (make-frame
                   `((parent-frame . ,(frame-parent frame))
                     (minibuffer . nil) (no-accept-focus . t) (no-focus-on-map . t)
                     (undecorated . t) (unsplittable . t) (no-other-frame . t)
                     (skip-taskbar . t) (desktop-dont-save . t)
                     (internal-border-width . 0) (child-frame-border-width . 0)
                     (left-fringe . 0) (right-fringe . 0)
                     (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
                     (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
                     (line-spacing . 0) (cursor-type . nil) (no-special-glyphs . t)
                     (visibility . nil)
                     (background-color . ,(lispm-desktop-platinum-color :chrome))
                     (lispm-desktop-platinum-frames . ,frame)
                     ;; Out on the computer's screen, it is the window's.
                     (lispm-desktop-out . ,(frame-parameter frame 'lispm-desktop-out))
                     (width . 2) (height . 1))))))
    (with-current-buffer buffer
      (setq-local mode-line-format nil header-line-format nil tab-line-format nil
                  cursor-type nil cursor-in-non-selected-windows nil
                  truncate-lines t auto-hscroll-mode nil
                  ;; About a window rolled up, the frame is shorter than
                  ;; the four lines Emacs otherwise keeps it to.
                  window-min-height 1)
      (use-local-map lispm-desktop-platinum--frame-map))
    (let ((root (frame-root-window outer)))
      (set-window-buffer root buffer)
      (set-window-dedicated-p root t)
      (set-window-fringes root 0 0))
    outer))

(defun lispm-desktop-platinum-frame-window (frame)
  "Frame floating window FRAME when it is to be framed and is not yet: the
frame drawing its frame takes its place, and FRAME stands in it.
Return FRAME."
  (when (and (lispm-desktop-platinum--frameable-p frame)
             (not (lispm-desktop-platinum--framed-p frame)))
    (pcase-let* ((`(,x . ,y) (frame-position frame))
                 (`(,left . ,top) lispm-desktop-platinum-frame-inset)
                 (outer (lispm-desktop-platinum--make-window-frame frame)))
      (set-frame-parameter frame 'lispm-desktop-platinum-frame outer)
      (lispm-desktop-platinum-frame-update frame)
      (set-frame-position outer (- x left) (- y top))
      (when (frame-visible-p frame)
        (make-frame-visible outer))
      (modify-frame-parameters frame `((parent-frame . ,outer)))
      ;; Emacs takes a frame just given a parent to be where it was asked
      ;; to be before.
      (lispm-desktop-platinum--stand-in-frame frame)))
  frame)

(defun lispm-desktop-platinum--stand-in-frame (frame)
  "Put floating window FRAME at the insets in the frame carrying it.  Emacs
may take FRAME to be there already while the window system has it
elsewhere; moved by a pixel first, it is placed anew."
  (pcase-let ((`(,left . ,top) lispm-desktop-platinum-frame-inset))
    (set-frame-position frame (1+ left) top)
    (set-frame-position frame left top)))

(defun lispm-desktop-platinum--frame-size (frame)
  "Return the size of the frame of floating window FRAME: (WIDTH . HEIGHT)
in pixels, FRAME's and its margins, see `lispm-desktop-platinum-frame-margins'."
  (pcase-let ((`(,left ,top ,right ,bottom) (lispm-desktop-platinum-frame-margins)))
    (cons (+ (frame-pixel-width frame) left right)
          (+ (frame-pixel-height frame) top bottom))))

(defvar lispm-desktop-platinum-window-min-size)

(defun lispm-desktop-platinum--frame-follow (frame outer)
  "Bring floating window FRAME and OUTER, the frame drawing its frame,
together again after the window system resized either from its edges, as
\"Window frames\" above tells.  Return non-nil when it had."
  (when (eq (frame-parent frame) outer)
    (pcase-let ((`(,left . ,top) lispm-desktop-platinum-frame-inset)
                (`(,x . ,y) (frame-position frame))
                (size (frame-parameter outer 'lispm-desktop-platinum-size)))
      (cond ((not (and (= x left) (= y top)))
             ;; The window's left or top edge: it moved in its frame, and
             ;; the frame follows it there.
             (pcase-let ((`(,outer-x . ,outer-y) (frame-position outer)))
               (set-frame-position outer (+ outer-x (- x left)) (+ outer-y (- y top))))
             (set-frame-position frame left top)
             t)
            ((and size
                  ;; None of the sizes it was given lately: the window
                  ;; system tells of each a while after it was set.
                  (not (member (cons (frame-pixel-width outer) (frame-pixel-height outer))
                               (frame-parameter outer 'lispm-desktop-platinum-sizes)))
                  ;; Neither the window's size changed nor the frame's font.
                  (equal (lispm-desktop-platinum--frame-size frame) size)
                  (equal (frame-parameter outer 'font)
                         (frame-parameter outer 'lispm-desktop-platinum-font)))
             ;; The frame's edge: the window takes the size it leaves it, a
             ;; rolled-up window only its width, and stands in it again,
             ;; where the frame's left or top edge went.
             (let ((least lispm-desktop-platinum-window-min-size)
                   (width (+ (frame-text-width frame) (- (frame-pixel-width outer) (car size))))
                   (height (+ (frame-text-height frame) (- (frame-pixel-height outer) (cdr size)))))
               (set-frame-size frame (max (car least) width)
                               (if (lispm-desktop-platinum-rolled-up-height frame)
                                   (frame-text-height frame)
                                 (max (cdr least) height))
                               t))
             (lispm-desktop-platinum--stand-in-frame frame)
             t)))))

(defvar lispm-desktop-platinum--resize-timers nil
  "Timers keeping windows resized from their edges in place: (FRAME . TIMER).")

(defun lispm-desktop-platinum--resized-from-edges (frame)
  "Keep floating window FRAME in place, as after its grow box, see
`lispm-desktop-platinum-frame-moved-functions', once the window system is
done resizing it from its edges, a step at a time."
  (when-let* ((timer (alist-get frame lispm-desktop-platinum--resize-timers)))
    (cancel-timer timer))
  (setf (alist-get frame lispm-desktop-platinum--resize-timers)
        (run-at-time 0.2 nil
                     (lambda ()
                       (setf (alist-get frame lispm-desktop-platinum--resize-timers nil t) nil)
                       (when (frame-live-p frame)
                         (run-hook-with-args 'lispm-desktop-platinum-frame-moved-functions frame))))))

(defun lispm-desktop-platinum-set-background (frame color)
  "Give FRAME the background COLOR, unless it has it already.
NS Emacs clears a frame whenever its background is set, to the colour it
has too, and a frame cleared just before the screen is updated shows
blank until it is drawn again."
  (unless (equal (frame-parameter frame 'background-color) color)
    (set-frame-parameter frame 'background-color color)))

(defun lispm-desktop-platinum--frame-strips (width height)
  "Return the strips a window's frame WIDTH by HEIGHT pixels is drawn in,
round the window standing in it, see `lispm-desktop-platinum-frame-margins':
the top, left, right and bottom ones, each (X Y WIDTH HEIGHT) in pixels.
The left and right strips run between the top and bottom ones."
  (pcase-let ((`(,left ,top ,right ,bottom) (lispm-desktop-platinum-frame-margins)))
    (let ((middle (max 0 (- height top bottom))))
      (list (list 0 0 width top)
            (list 0 top left middle)
            (list (- width right) top right middle)
            (list 0 (- height bottom) width bottom)))))

(defun lispm-desktop-platinum--svg-part (svg x y width height)
  "Return the part of SVG WIDTH by HEIGHT pixels from X, Y: an SVG that
size, showing what SVG draws there.  Rectangles wholly outside it are
left out, which spares writing and reading them again."
  (apply #'dom-node 'svg
         `((width . ,width) (height . ,height)
           (viewBox . ,(format "%d %d %d %d" x y width height))
           ,@(seq-remove (lambda (attribute) (memq (car attribute) '(width height viewBox)))
                         (dom-attributes svg)))
         (seq-remove
          (lambda (node)
            (and (consp node) (eq (dom-tag node) 'rect)
                 (let ((left (dom-attr node 'x)) (top (dom-attr node 'y))
                       (across (dom-attr node 'width)) (down (dom-attr node 'height)))
                   (and (numberp left) (numberp top) (numberp across) (numberp down)
                        (or (>= left (+ x width)) (<= (+ left across) x)
                            (>= top (+ y height)) (<= (+ top down) y))))))
          (dom-children svg))))

(defun lispm-desktop-platinum--frame-text (svg width height)
  "Return the text drawing SVG, a window's frame WIDTH by HEIGHT pixels:
each of its strips, see `lispm-desktop-platinum--frame-strips', an image
of its own, the top and bottom ones on lines of their own, the left and
right ones on a line with the window's place between them, left as space
under the window.  Emacs converts every pixel of an image it shows, so
one image of the whole frame took as long as the frame was large, a
tenth of a second for a large window; its strips take a few milliseconds,
however large the window."
  (pcase-let ((`(,top ,left ,right ,bottom) (lispm-desktop-platinum--frame-strips width height))
              ;; The line as tall as its images, the font's height aside.
              (newline (propertize "\n" 'line-height t)))
    (cl-flet ((strip (rectangle)
                (propertize " " 'display
                            (svg-image (apply #'lispm-desktop-platinum--svg-part svg rectangle)
                                       :ascent 100 :scale 1)))
              (shown-p (rectangle) (and (> (nth 2 rectangle) 0) (> (nth 3 rectangle) 0))))
      (concat (when (shown-p top)
                (concat (strip top) newline))
              (when (shown-p left)
                (concat (strip left)
                        (propertize " " 'display
                                    `(space :width (,(- width (nth 2 left) (nth 2 right)))
                                            :height (,(nth 3 left)) :ascent 100))
                        (strip right) newline))
              (when (shown-p bottom)
                (concat (strip bottom) newline))))))

(defun lispm-desktop-platinum-frame-update (frame)
  "Bring the frame of floating window FRAME up to date: as large as FRAME
and its margins, see `lispm-desktop-platinum-frame-margins', around FRAME
again when the window system resized either from its edges, and drawn
active while FRAME is selected."
  (when-let* ((outer (frame-parameter frame 'lispm-desktop-platinum-frame))
              ((frame-live-p outer))
              ((frame-live-p frame)))
    (when (lispm-desktop-platinum--frame-follow frame outer)
      (lispm-desktop-platinum--resized-from-edges frame))
    (let* ((size (lispm-desktop-platinum--frame-size frame))
           (width (car size))
           (height (cdr size))
           (active (eq frame (selected-frame)))
           (key (list lispm-desktop-platinum-generation width height active)))
      (unless (and (equal size (frame-parameter outer 'lispm-desktop-platinum-size))
                   ;; Emacs sizes a frame anew to whole lines as its fonts
                   ;; change, as with a theme.
                   (= (frame-pixel-width outer) width)
                   (= (frame-pixel-height outer) height))
        (set-frame-parameter outer 'lispm-desktop-platinum-size size)
        (set-frame-parameter outer 'lispm-desktop-platinum-sizes
                             (seq-take (cons size (remove size (frame-parameter outer 'lispm-desktop-platinum-sizes)))
                                       4))
        (let ((window-min-width 1) (window-safe-min-width 1)
              (window-min-height 1) (window-safe-min-height 1)
              (frame-resize-pixelwise t))
          (set-frame-size outer width height t)))
      ;; The font the frame was last sized in: changed, it changed the
      ;; frame's size, which no edge did.
      (unless (equal (frame-parameter outer 'font) (frame-parameter outer 'lispm-desktop-platinum-font))
        (set-frame-parameter outer 'lispm-desktop-platinum-font (frame-parameter outer 'font)))
      (unless (equal key (frame-parameter outer 'lispm-desktop-platinum-key))
        (set-frame-parameter outer 'lispm-desktop-platinum-key key)
        (lispm-desktop-platinum-set-background outer (lispm-desktop-platinum-color :chrome))
        (let ((root (frame-root-window outer)))
          (with-current-buffer (window-buffer root)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (lispm-desktop-platinum--frame-text
                       (lispm-desktop-platinum-draw-window-frame
                        lispm-desktop-platinum-current-palette width height :active active)
                       width height))))
          (set-window-start root 1))))))

(defun lispm-desktop-platinum-frames-sync (&optional frame &rest _)
  "Bring floating windows' frames up to date, and take away the frames of
windows gone.  A window's frame is its parent and goes with it, so
only its size and whether it is drawn active are left to follow, and the
two put together again after the window system resized either from its
edges: this runs as windows change size, as the selection changes and as
a theme is enabled.  Given a live FRAME, as the window hooks pass the
frame whose windows changed, only that window, or the window FRAME
frames, is brought up to date: the selection hooks pass both the frame
selected before and the one selected now.  Otherwise, as a theme is
enabled or a frame was deleted, every floating window is."
  (if (and (framep frame) (frame-live-p frame))
      (when-let* ((window (if (frame-parameter frame 'lispm-desktop-window)
                              frame
                            (lispm-desktop-platinum-window-frame-p frame))))
        (lispm-desktop-platinum-frame-update window))
    (dolist (frame (frame-list))
      (cond ((frame-parameter frame 'lispm-desktop-platinum-frames)
             (unless (frame-live-p (frame-parameter frame 'lispm-desktop-platinum-frames))
               (let ((buffer (window-buffer (frame-root-window frame))))
                 (delete-frame frame t)
                 (kill-buffer buffer))))
            ((frame-parameter frame 'lispm-desktop-window)
             (lispm-desktop-platinum-frame-update frame))))))

(defun lispm-desktop-platinum--frame-moved (frame)
  "Bring the frame of a floating window up to date when FRAME, the window or
its frame, moved: the window system moves either resizing it from its
left or top edge.  This is a `move-frame-functions' function."
  (when-let* ((window (if (frame-parameter frame 'lispm-desktop-window)
                          frame
                        (lispm-desktop-platinum-window-frame-p frame))))
    (lispm-desktop-platinum-frame-update window)))

(defun lispm-desktop-platinum--frame-press (event)
  "Move the window whose frame EVENT pressed by its outline, as Mac OS 9's
frame did, see `lispm-desktop-platinum-drag-outline'."
  (interactive "e")
  (when-let* ((frame (lispm-desktop-platinum-window-frame-p
                      (window-frame (posn-window (event-start event))))))
    (lispm-desktop-platinum-drag-outline frame 'move)))

(defun lispm-desktop-platinum-frames-enable ()
  "Frame floating windows as Mac OS 9 framed its windows, each frame as
large as its window and drawn active with it."
  (add-hook 'window-size-change-functions #'lispm-desktop-platinum-frames-sync)
  (add-hook 'window-selection-change-functions #'lispm-desktop-platinum-frames-sync)
  (add-hook 'enable-theme-functions #'lispm-desktop-platinum-frames-sync 90)
  (add-hook 'after-delete-frame-functions #'lispm-desktop-platinum-frames-sync)
  (add-hook 'move-frame-functions #'lispm-desktop-platinum--frame-moved))

;;;; Moving and growing windows
;;
;; Mac OS 9 moved a window by its outline: pressed in its title bar or its
;; frame, DragWindow had DragGrayRgn drag a dotted outline of the window
;; after the mouse, the window itself staying where it was, and only once
;; the button rose did MoveWindow put it there.  GrowWindow dragged an
;; outline at the window's new size from its grow box, the lines of its
;; title bar, its content and its grow box within, and the window was
;; drawn again once, at the end.  Both were drawn in the 50% grey pattern
;; inverted over the screen: every other pixel.
;;
;;   press --> outline follows the pointer --> release --> window moves once
;;                                         \-> Escape or C-g: nothing moves
;;
;; The outline is drawn in frames a line each, over what the window floats
;; over; on macOS a child frame is not clipped by its parent, so the
;; outline passes the desktop's edge on its way out.  The window system
;; inverts nothing an Emacs frame shows, so its dots are the theme's
;; outline over the desktop's colour.
;;
;; A text terminal draws nothing thinner than a cell, and tells where the
;; mouse is only in cells of the frame it is over.  There the window
;; itself follows the mouse, moving or growing as it goes, as windows of
;; text were dragged, and Escape or C-g puts it back.

(defconst lispm-desktop-platinum-grow-box-size 15
  "Pixels square of the grow box the outline of a window growing marks.")

(defcustom lispm-desktop-platinum-drag-interval 0.015
  "Seconds between two looks at the mouse while a window or an icon is
dragged.  On macOS Emacs reports a move of the mouse only once it leaves
the glyph it was over, and an image is one glyph, however large, so the
pointer is looked at itself."
  :type 'number
  :group 'lispm-desktop-platinum)

(defun lispm-desktop-platinum-outline-lines (kind x y width height &optional title)
  "Return the lines of the outline Mac OS 9 drew to move or grow a window.
KIND is `move' or `grow'; X, Y, WIDTH and HEIGHT are the window's outer
rectangle, its frame and shadow included.  Each line is
\(X Y LENGTH THICKNESS VERTICAL).
Moving, the outline is a band two pixels thick round the rectangle.
Growing, it is a line a pixel thick round it; given TITLE, the height
of the title bar of a window framed, the lines of its content follow
within: under the title bar, round the content inside the frame, and
round the grow box in the content's corner."
  (if (eq kind 'move)
      (list (list x y width 2 nil)
            (list x (+ y height -2) width 2 nil)
            (list x (+ y 2) (- height 4) 2 t)
            (list (+ x width -2) (+ y 2) (- height 4) 2 t))
    (append
     (list (list x y width 1 nil)
           (list x (+ y height -1) width 1 nil)
           (list x (1+ y) (- height 2) 1 t)
           (list (+ x width -1) (1+ y) (- height 2) 1 t))
     (when title
       ;; The content's black line, drawn by the frame: past the outline
       ;; and the band, and before the shadow, see
       ;; `lispm-desktop-platinum-draw-window-frame'.
       (let* ((left (+ x (1- (car lispm-desktop-platinum-frame-inset))))
              (right (+ x width -2 (- (1- (car lispm-desktop-platinum-frame-inset)))))
              (top (+ y title))
              (bottom (+ y height -2 (- (1- (car lispm-desktop-platinum-frame-inset)))))
              (across (1+ (- right left)))
              (down (1- (- bottom top)))
              (box lispm-desktop-platinum-grow-box-size))
         (list (list left top across 1 nil)
               (list left (1+ top) down 1 t)
               (list right (1+ top) down 1 t)
               (list left bottom across 1 nil)
               (list (- right box) (- bottom box) box 1 nil)
               (list (- right box) (1+ (- bottom box)) (1- box) 1 t)))))))

(defun lispm-desktop-platinum--strip-tab-ends (rows)
  "Return the column of each of ROWS' last outline pixel, its last #."
  (mapcar (lambda (row)
            (let ((i (length row)))
              (while (and (> i 0) (/= (aref row (1- i)) ?#))
                (setq i (1- i)))
              (1- i)))
          rows))

(defun lispm-desktop-platinum-strip-outline-lines (x y width side color)
  "Return the lines of the outline Mac OS 9 drew to move the Control Strip
by its tab, as `lispm-desktop-platinum-outline-lines' returns lines, each
with COLOR last, the colour it is drawn in solid: Mac OS 9 inverted what
was under it.  The outline is a band two pixels thick round the strip,
the strip's own outline and the pixel outside it, widening to four where
it steps round the tab's slanted end, as sampled from Mac OS 9.0.4.  The
strip is at X, Y, WIDTH pixels wide and `lispm-desktop-platinum-strip-height'
tall, its tab its last pixels, or its first when SIDE is `right', on the
screen's right edge."
  (let* ((h lispm-desktop-platinum-strip-height)
         (tab (- width (lispm-desktop-platinum-strip-segment-width 'tab)))
         (ends (vconcat (lispm-desktop-platinum--strip-tab-ends lispm-desktop-platinum--strip-tab)))
         (runs nil))
    ;; Each row's runs (ROW FROM TO) as on the left edge, the strip's top
    ;; row 0 and its outside -1 to its left.
    (push (list -1 -1 (+ tab (aref ends 0))) runs)
    (push (list 0 -1 (+ tab (aref ends 0) 1)) runs)
    (dotimes (i (- h 2))
      (let* ((row (1+ i))
             (end (aref ends row))
             (slanted (or (/= end (aref ends (1- row))) (/= end (aref ends (1+ row))))))
        (push (list row -1 0) runs)
        (push (list row (+ tab (if slanted (- end 2) end)) (+ tab end 1)) runs)))
    (push (list (1- h) -1 (+ tab (aref ends (1- h)) 1)) runs)
    (push (list h -1 (+ tab (aref ends (1- h)))) runs)
    (when (eq side 'right)
      (setq runs (mapcar (pcase-lambda (`(,row ,from ,to)) (list row (- width 1 to) (- width 1 from)))
                         runs)))
    ;; Runs alike in rows one under the other make one line.
    (let ((lines nil) (last nil))
      (dolist (run (sort runs (lambda (p q) (or (< (nth 1 p) (nth 1 q))
                                                (and (= (nth 1 p) (nth 1 q))
                                                     (or (< (nth 2 p) (nth 2 q))
                                                         (and (= (nth 2 p) (nth 2 q)) (< (car p) (car q)))))))))
        (if (and last (= (nth 1 run) (nth 1 last)) (= (nth 2 run) (nth 2 last))
                 (= (car run) (1+ (nth 3 last))))
            (setcar (nthcdr 3 last) (car run))
          (setq last (list (car run) (nth 1 run) (nth 2 run) (car run)))
          (push last lines)))
      (mapcar (pcase-lambda (`(,top ,from ,to ,bottom))
                (if (= top bottom)
                    (list (+ x from) (+ y top) (1+ (- to from)) 1 nil color)
                  (list (+ x from) (+ y top) (1+ (- bottom top)) (1+ (- to from)) t color)))
              (nreverse lines)))))

(defun lispm-desktop-platinum-inverse (color)
  "Return COLOR inverted, as Mac OS 9 drew over it with XOR.  A colour
written #RRGGBB is read as written; Emacs without a display reads it
only roughly."
  (let ((values (if (string-match (concat "\\`#\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{2\\}\\)"
                                          "\\([[:xdigit:]]\\{2\\}\\)\\'")
                                  color)
                    (mapcar (lambda (group) (string-to-number (match-string group color) 16)) '(1 2 3))
                  (mapcar (lambda (value) (/ value 257)) (color-values color)))))
    (apply #'format "#%02x%02x%02x" (mapcar (lambda (value) (- 255 value)) values))))

(defun lispm-desktop-platinum-dots (width height parity)
  "Return the 50% grey pattern WIDTH by HEIGHT pixels as XBM data: a vector
of rows, each a bool-vector, a pixel set where its column and row, PARITY
added, sum to an even number.  PARITY is that of the pattern's top left
corner on the screen, so the dots line up with the screen's pixels as
Mac OS 9's patterns did, wherever the line is."
  (let ((rows (make-vector height nil)))
    (dotimes (row height)
      (let ((bits (make-bool-vector width nil)))
        (dotimes (column width)
          (when (cl-evenp (+ parity row column))
            (aset bits column t)))
        (aset rows row bits)))
    rows))

(defvar lispm-desktop-platinum--dots (make-hash-table :test #'equal)
  "Images of the outline's lines, by (WIDTH HEIGHT PARITY GENERATION).")

(defun lispm-desktop-platinum--solid-image (width height color)
  "Return an image WIDTH by HEIGHT all in COLOR, for a solid line."
  (let ((key (list 'solid width height color)))
    (or (gethash key lispm-desktop-platinum--dots)
        (let ((rows (make-vector height nil)))
          (dotimes (row height)
            (aset rows row (make-bool-vector width t)))
          (when (> (hash-table-count lispm-desktop-platinum--dots) 400)
            (clrhash lispm-desktop-platinum--dots))
          (puthash key
                   (create-image rows 'xbm t :data-width width :data-height height :ascent 100
                                 :foreground color :background color)
                   lispm-desktop-platinum--dots)))))

(defun lispm-desktop-platinum--dots-image (width height parity)
  "Return an image of the outline's dots WIDTH by HEIGHT, see
`lispm-desktop-platinum-dots': the theme's outline over the desktop's colour."
  (let ((key (list width height parity lispm-desktop-platinum-generation)))
    (or (gethash key lispm-desktop-platinum--dots)
        (progn
          (when (> (hash-table-count lispm-desktop-platinum--dots) 400)
            (clrhash lispm-desktop-platinum--dots))
          (puthash key
                   (create-image (lispm-desktop-platinum-dots width height parity) 'xbm t
                                 :data-width width :data-height height :ascent 100
                                 :foreground (lispm-desktop-platinum-color :outline)
                                 :background (lispm-desktop-platinum-desktop-color))
                   lispm-desktop-platinum--dots)))))

(defvar lispm-desktop-platinum-desktop-color-function #'ignore
  "Function returning the colour the desktop is drawn in, or nil for the
theme's.")

(defun lispm-desktop-platinum-desktop-color ()
  "Return the desktop's colour: the one it is drawn in, or the theme's."
  (or (funcall lispm-desktop-platinum-desktop-color-function)
      (lispm-desktop-platinum-color :desktop)))

(defvar lispm-desktop-platinum--outline-frames nil
  "The frames drawing outlines' lines, by what they are drawn over:
\(PARENT . FRAMES), PARENT nil for the computer's screen.")

(defun lispm-desktop-platinum--keep-thin (frame)
  "Give FRAME, a frame drawing a line of an outline, a font a pixel tall
unless it has it, so the frame can be a pixel thin.  Enabling a theme,
even the one enabled, or changing the default face gives every frame
the default face's font and the theme's background again: a frame kept
for the next outline would then be a line of text tall, a bar in the
theme's background across the window dragged."
  (unless (equal (frame-parameter frame 'font)
                 (frame-parameter frame 'lispm-desktop-platinum-thin-font))
    (ignore-errors (set-frame-font (font-spec :family (face-attribute 'default :family nil t) :size 1)
                                   nil (list frame)))
    ;; Kept as it came out, so a font that cannot be a pixel tall is not
    ;; set again at every line drawn.
    (set-frame-parameter frame 'lispm-desktop-platinum-thin-font (frame-parameter frame 'font))
    (lispm-desktop-platinum-set-background frame (lispm-desktop-platinum-desktop-color))
    ;; Sized under the other font, it is sized again.
    (set-frame-parameter frame 'lispm-desktop-platinum-line nil)))

(defun lispm-desktop-platinum--make-line-frame (parent)
  "Return a new frame to draw a line of an outline in, over PARENT, or on
the computer's screen, above its windows, when PARENT is nil.  Its font
is a pixel tall, so the frame can be a pixel thin."
  (let* ((buffer (generate-new-buffer " *lispm-desktop-outline*"))
         (frame (let ((after-make-frame-functions nil)
                      (before-make-frame-hook nil))
                  (make-frame
                   `((parent-frame . ,parent)
                     (minibuffer . nil) (no-accept-focus . t) (no-focus-on-map . t)
                     (undecorated . t) (unsplittable . t) (no-other-frame . t)
                     (skip-taskbar . t) (desktop-dont-save . t)
                     (internal-border-width . 0) (child-frame-border-width . 0)
                     (left-fringe . 0) (right-fringe . 0)
                     (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
                     (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
                     (line-spacing . 0) (cursor-type . nil) (no-special-glyphs . t)
                     (visibility . nil) (lispm-desktop-platinum-outline . t)
                     (background-color . ,(lispm-desktop-platinum-desktop-color))
                     ,@(unless parent '((z-group . above)))
                     (width . 2) (height . 1))))))
    (lispm-desktop-platinum--keep-thin frame)
    (with-current-buffer buffer
      (setq-local mode-line-format nil header-line-format nil tab-line-format nil
                  cursor-type nil cursor-in-non-selected-windows nil
                  truncate-lines t auto-hscroll-mode nil line-spacing 0))
    (let ((root (frame-root-window frame)))
      (set-window-buffer root buffer)
      (set-window-dedicated-p root t)
      (set-window-fringes root 0 0)
      (set-window-margins root 0 0))
    frame))

(defun lispm-desktop-platinum--line-frames (parent count)
  "Return COUNT frames to draw an outline's lines over PARENT, made when
missing and kept for the next outline."
  (setq lispm-desktop-platinum--outline-frames
        (seq-filter (lambda (entry) (or (null (car entry)) (frame-live-p (car entry))))
                    lispm-desktop-platinum--outline-frames))
  (let ((frames (seq-filter #'frame-live-p (alist-get parent lispm-desktop-platinum--outline-frames))))
    (while (< (length frames) count)
      (setq frames (append frames (list (lispm-desktop-platinum--make-line-frame parent)))))
    (setf (alist-get parent lispm-desktop-platinum--outline-frames) frames)
    frames))

(defun lispm-desktop-platinum--draw-line (frame line origin)
  "Draw LINE, a value of `lispm-desktop-platinum-outline-lines', in FRAME,
placed in the coordinates of what FRAME is drawn over.  ORIGIN, (X . Y),
is where those start on the computer's screen, which lines the dots up.
A line with a colour after its five values is drawn solid in it."
  (pcase-let* ((`(,x ,y ,length ,thickness ,vertical ,color) line)
               (width (max 1 (if vertical thickness length)))
               (height (max 1 (if vertical length thickness)))
               (parity (mod (+ x y (car origin) (cdr origin)) 2))
               (size (list width height parity color lispm-desktop-platinum-generation)))
    (lispm-desktop-platinum--keep-thin frame)
    (unless (equal size (frame-parameter frame 'lispm-desktop-platinum-line))
      (set-frame-parameter frame 'lispm-desktop-platinum-line size)
      (let ((window-min-width 1) (window-safe-min-width 1)
            (window-min-height 1) (window-safe-min-height 1)
            (frame-resize-pixelwise t))
        (set-frame-size frame width height t))
      (with-current-buffer (window-buffer (frame-root-window frame))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize " " 'display
                              (if color
                                  (lispm-desktop-platinum--solid-image width height color)
                                (lispm-desktop-platinum--dots-image width height parity))))))
      (set-window-start (frame-root-window frame) 1))
    (set-frame-position frame x y)))

(defun lispm-desktop-platinum-keep-in-front (frames parent)
  "Raise FRAMES, children of PARENT, unless one of them is in front of
PARENT's children already.  Over the screen, the window clicked comes in
front once Emacs notices it was selected, and others may, while what a
drag draws is to stay over them all, as Mac OS 9 drew it over the
screen; a new child frame need not come in front either."
  (when (and parent frames (not (memq (car (frame-list-z-order parent)) frames)))
    (mapc #'raise-frame frames)))

(defun lispm-desktop-platinum-show-outline (parent lines origin)
  "Draw LINES over PARENT, or on the computer's screen when PARENT is nil,
ORIGIN as `lispm-desktop-platinum--draw-line' takes it; frames left over
from a longer outline are hidden."
  (let ((frames (lispm-desktop-platinum--line-frames parent (length lines)))
        (shown nil))
    (dolist (frame frames)
      (if-let* ((line (pop lines)))
          (progn
            (lispm-desktop-platinum--draw-line frame line origin)
            (unless (frame-visible-p frame)
              (make-frame-visible frame))
            (push frame shown))
        (when (frame-visible-p frame)
          (make-frame-invisible frame t))))
    ;; On the computer's screen the outline floats above all by itself.
    (lispm-desktop-platinum-keep-in-front shown parent)))

(defun lispm-desktop-platinum-hide-outline ()
  "Take every outline away."
  (dolist (entry lispm-desktop-platinum--outline-frames)
    (dolist (frame (cdr entry))
      (when (and (frame-live-p frame) (frame-visible-p frame))
        (make-frame-invisible frame t)))))

(defun lispm-desktop-platinum-origin (parent)
  "Return where PARENT's inside starts on the computer's screen, (X . Y),
or (0 . 0) when PARENT is nil, the computer's screen itself."
  (if (null parent)
      '(0 . 0)
    (pcase-let ((`(,left ,top . ,_) (frame-edges parent 'native-edges)))
      (cons left top))))

(defun lispm-desktop-platinum-tty-pointer ()
  "Return where the mouse is on a text terminal, (COLUMN . LINE) counted
from the top left of its root frame, or nil while that is not known.
The terminal reports the mouse within the frame it is over, see
`mouse-position'; a child frame's place within its parent carries that
out to the root, which holds still while a window over it moves."
  (pcase-let ((`(,frame ,x . ,y) (mouse-position)))
    (when (and (frame-live-p frame) (numberp x) (numberp y))
      (while (frame-parent frame)
        (pcase-let ((`(,left . ,top) (frame-position frame)))
          (setq x (+ x left)
                y (+ y top)
                frame (frame-parent frame))))
      (cons x y))))

(defun lispm-desktop-platinum-track (step &optional pointer start)
  "Follow the mouse while its button is held, calling STEP with how far it
has moved from START, (DX . DY) in pixels, whenever that changes.
POINTER, a function of no arguments, tells where the mouse is, (X . Y),
or nil when it cannot; by default, on the computer's screen, and on a
text terminal in cells of its root frame, see
`lispm-desktop-platinum-tty-pointer'.  START is by default where the
mouse is when the drag begins.  The mouse is looked at every
`lispm-desktop-platinum-drag-interval' seconds; on a text terminal,
which reports every move from cell to cell, as each move or click comes,
and only then.
Return (EVENT . DELTA): EVENT the button's release, or nil when Escape
or C-g gave the drag up; DELTA the last move, or nil when it never moved.
Mac OS 9's mouse had one button, so any of the three let go ends the
drag: none leaves it following the mouse once every button is up.
Other keys and the window system's own events change nothing."
  (let* ((tty (lispm-desktop-tty-p))
         (pointer (or pointer
                      (if tty
                          #'lispm-desktop-platinum-tty-pointer
                        #'mouse-absolute-pixel-position)))
         (start (or start (funcall pointer) '(0 . 0)))
         (delta nil)
        (event nil)
        (done nil))
    (condition-case nil
        (track-mouse
          (setq track-mouse 'dragging)
          (while (not done)
            ;; A terminal's keys and mouse reports are sequences, which a
            ;; read given up halfway would lose: there it waits for them.
            (setq event (lispm-ui-modal-read-event (unless tty lispm-desktop-platinum-drag-interval)))
            (cond ((memq event '(escape ?\e ?\C-g))
                   (setq done t event nil))
                  ((and event (not (mouse-movement-p event))
                        (memq (event-basic-type event) '(mouse-1 mouse-2 mouse-3))
                        (seq-intersection (event-modifiers event) '(click drag)))
                   (setq done t)))
            (when-let* (((if tty
                             ;; A terminal tells where the mouse is within
                             ;; the frame under it, as that frame stood when
                             ;; the event was read: a window moved since
                             ;; would put it askew, and the release of a drag
                             ;; comes after the move that brought it there.
                             (and event
                                  (or (mouse-movement-p event)
                                      (memq 'click (event-modifiers event))))
                           (or (not done) event)))
                        (place (funcall pointer))
                        (now (cons (- (car place) (car start)) (- (cdr place) (cdr start))))
                        ((not (or (equal now delta) (and (null delta) (equal now '(0 . 0)))))))
              (setq delta now)
              (funcall step delta))))
      (quit (setq event nil)))
    (cons event delta)))

(defun lispm-desktop-platinum--moved-p (delta &optional cells)
  "Return non-nil when DELTA, as `lispm-desktop-platinum-track' returns it,
is a move rather than a hand's jitter in a click; CELLS non-nil when it
counts a text terminal's cells."
  (lispm-ui-modal-moved-p delta cells))

(defconst lispm-desktop-platinum-window-min-size '(200 . 100)
  "The least a window grows to: (WIDTH . HEIGHT) of its text, in pixels.")

(defun lispm-desktop-platinum--window-min-size (frame)
  "Return the least floating window FRAME grows to, (WIDTH . HEIGHT) of its
text: `lispm-desktop-platinum-window-min-size', and on a text terminal a
cell for 8 of its pixels across and 16 down, as a window opens there,
see `lispm-desktop-app-window--tty-geometry'."
  (pcase-let ((`(,width . ,height) lispm-desktop-platinum-window-min-size))
    (if (lispm-desktop-tty-p frame)
        (cons (/ width 8) (/ height 16))
      (cons width height))))

(defun lispm-desktop-platinum-drag-outline (frame kind)
  "Move floating window FRAME, KIND `move', or grow it, KIND `grow', by an
outline the mouse drags, as Mac OS 9's DragWindow and GrowWindow did.
The outline follows the mouse from where the button was pressed, see
`lispm-desktop-platinum-outline-lines'; the window moves or grows once,
when the button rises, comes in front and has the keyboard, and
`lispm-desktop-platinum-frame-moved-functions' run.  Escape or C-g gives
up and leaves it where it was; a press let go where it was is a click,
handed on.  On a text terminal, which draws no outline, the window
itself comes in front with the keyboard as the button goes down and
follows the mouse, and goes back where it was when the drag is given
up.  Return non-nil when the window moved or grew."
  (let* ((outer (lispm-desktop-platinum-window-outer frame))
         (parent (frame-parent outer))
         (origin (lispm-desktop-platinum-origin parent))
         (live (lispm-desktop-tty-p frame))
         (position (frame-position outer))
         (width (frame-pixel-width outer))
         (height (frame-pixel-height outer))
         (text (cons (frame-text-width frame) (frame-text-height frame)))
         (title (and (eq kind 'grow) (not (eq outer frame)) lispm-desktop-platinum-title-bar-height))
         (least (let ((smallest (lispm-desktop-platinum--window-min-size frame)))
                  (cons (- (car smallest) (car text)) (- (cdr smallest) (cdr text)))))
         (rectangle
          (lambda (delta)
            (if (eq kind 'move)
                (list (+ (car position) (car delta)) (+ (cdr position) (cdr delta)) width height)
              (list (car position) (cdr position)
                    (+ width (max (min 0 (car least)) (car delta)))
                    (+ height (max (min 0 (cdr least)) (cdr delta)))))))
         (place
          (lambda (delta)
            (pcase-let ((`(,x ,y ,new-width ,new-height) (funcall rectangle delta)))
              (if (eq kind 'move)
                  (set-frame-position outer x y)
                (set-frame-size frame (+ (car text) (- new-width width))
                                (+ (cdr text) (- new-height height)) t)
                (lispm-desktop-platinum-frame-update frame)))))
         (result
          (progn
            (when live
              (select-frame-set-input-focus frame)
              (lispm-desktop-platinum-raise-window frame))
            (unwind-protect
                (lispm-desktop-platinum-track
                 (if live
                     place
                   (lambda (delta)
                     (lispm-desktop-platinum-show-outline
                      parent (apply #'lispm-desktop-platinum-outline-lines kind
                                    (append (funcall rectangle delta) (list title)))
                      origin))))
              (lispm-desktop-platinum-hide-outline))))
         (event (car result))
         (delta (cdr result)))
    (cond ((and event (lispm-desktop-platinum--moved-p delta live))
           (unless live
             (funcall place delta))
           (select-frame-set-input-focus frame)
           (lispm-desktop-platinum-raise-window frame)
           (run-hook-with-args 'lispm-desktop-platinum-frame-moved-functions frame)
           t)
          (t
           ;; Given up, or a click: a window that followed the mouse goes
           ;; back where it was.
           (when (and live delta)
             (funcall place '(0 . 0)))
           (when event
             ;; A click: what a click there does, it does.
             (if (memq 'click (event-modifiers event))
                 (push event unread-command-events)
               (select-frame-set-input-focus frame)))
           nil))))

(provide 'lispm-desktop-platinum-frames)
;;; lispm-desktop-platinum-frames.el ends here
