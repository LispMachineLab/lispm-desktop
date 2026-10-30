;;; lispm-desktop-app-stickies.el --- Stickies' notes on the desktop  -*- lexical-binding: t; -*-

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

;; Stickies (lispm-stickies, which the App Store installs) keeps notes in
;; any Emacs; once it is loaded, the desktop shows them as Mac OS 9.0.4's
;; Stickies did, through the variables lispm-stickies gives a frontend:
;; each note a small window of its own over the desktop, in one of
;; Stickies' seven colours.  The note in front wears a title bar of
;; pinstripes with a close box and a zoom box, and a grow box in its
;; corner; the others are plain paper.  A double click on a title bar
;; rolls the note up into it, its first line for a title; the zoom box
;; sizes the note to its text, and back.  Stickies becomes one of the
;; desktop's applications under its catalog item's ID, `stickies', with
;; the menus of Mac OS 9's Stickies: File, Edit, Note and Color.
;;
;; A Stickies of before 2026-10-25, which made itself the desktop's,
;; shows its notes itself.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'face-remap)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-app-window)
(require 'lispm-desktop-popup)

;; Stickies' own, read once it is loaded.
(defvar lispm-stickies-icon)
(defvar lispm-stickies-colors)
(defvar lispm-stickies-face)
(defvar lispm-stickies-note-mode-map)
(defvar lispm-stickies-show-note-function)
(defvar lispm-stickies-hide-note-function)
(defvar lispm-stickies-shown-function)
(defvar lispm-stickies-front-note-function)
(defvar lispm-stickies-confirm-function)
(defvar lispm-stickies-place-function)
(defvar lispm-stickies-changed-hook)
(declare-function lispm-stickies "ext:lispm-stickies" ())
(declare-function lispm-stickies-color-value "ext:lispm-stickies" (key part))
(declare-function lispm-stickies-note-p "ext:lispm-stickies" (buffer))
(declare-function lispm-stickies-note-buffers "ext:lispm-stickies" (&optional read))
(declare-function lispm-stickies-note-id "ext:lispm-stickies" (buffer))
(declare-function lispm-stickies-note-color "ext:lispm-stickies" (buffer))
(declare-function lispm-stickies-note-title "ext:lispm-stickies" (buffer))
(declare-function lispm-stickies-note-place "ext:lispm-stickies" (buffer))
(declare-function lispm-stickies-set-note-place "ext:lispm-stickies" (buffer geometry rolled))
(declare-function lispm-stickies-save-soon "ext:lispm-stickies" ())
(declare-function lispm-stickies-new-note "ext:lispm-stickies" (&optional text color))
(declare-function lispm-stickies-close-note "ext:lispm-stickies" (&optional buffer))
(declare-function lispm-stickies-set-color "ext:lispm-stickies" (color &optional buffer))
(declare-function lispm-stickies-next-note "ext:lispm-stickies" ())
(declare-function lispm-stickies-previous-note "ext:lispm-stickies" ())
(declare-function lispm-stickies-note-info "ext:lispm-stickies" (&optional buffer))
(declare-function lispm-stickies-import-text "ext:lispm-stickies" (file))
(declare-function lispm-stickies-export-text "ext:lispm-stickies" (file &optional buffer))
(declare-function lispm-stickies-quit "ext:lispm-stickies" ())

(defconst lispm-desktop-app-stickies-size '(240 . 160)
  "Size in pixels of a new note: (WIDTH . HEIGHT).")

(defconst lispm-desktop-app-stickies-title-height 10
  "Height in pixels of a note's title bar, its top edge included.")

(defconst lispm-desktop-app-stickies-bottom-height 8
  "Height in pixels of a note's bottom, its edge and the grow box above it.")

(defconst lispm-desktop-app-stickies-padding 4
  "Pixels between a note's left edge and its text.")

(defconst lispm-desktop-app-stickies-box-inset 5
  "Pixels from a note's side edges to its title bar boxes, Stickies'.")

(defvar-local lispm-desktop-app-stickies--unzoomed nil
  "This note's size before its zoom box sized it to its text, or nil.")

(defvar-local lispm-desktop-app-stickies--remaps nil
  "The face remappings of this note's title bar and bottom, and the
colour they are of: (COLOR . REMAPS), or nil before they are made.")

(defun lispm-desktop-app-stickies--colors (key)
  "Return the colours of the note colour KEY: (PAPER EDGE INK)."
  (mapcar (lambda (part) (lispm-stickies-color-value key part)) '(paper edge ink)))

;;;; The windows

(defun lispm-desktop-app-stickies--kind (buffer)
  "Return the kind of window of the note BUFFER, see
`lispm-desktop-app-window-define'."
  (intern (concat "lispm-stickies-" (lispm-stickies-note-id buffer))))

(defun lispm-desktop-app-stickies--frame (buffer)
  "Return the window of the note BUFFER while it is open."
  (lispm-desktop-app-window-frame (lispm-desktop-app-stickies--kind buffer)))

(defun lispm-desktop-app-stickies--note (frame)
  "Return the note FRAME shows, or nil when FRAME is no note's window."
  (let ((buffer (frame-parameter frame 'lispm-stickies-note)))
    (and (buffer-live-p buffer) buffer)))

(defun lispm-desktop-app-stickies--frames ()
  "Return the notes' open windows, the oldest note's first."
  (delq nil (mapcar #'lispm-desktop-app-stickies--frame (lispm-stickies-note-buffers))))

(defun lispm-desktop-app-stickies--front-frame ()
  "Return the window of the note in front, or nil when none is open."
  (let ((frames (lispm-desktop-app-stickies--frames)))
    (or (seq-find (lambda (frame) (memq frame frames))
                  (lispm-desktop-apps-windows-in-order (lispm-desktop-apps-screen)))
        (car (last frames)))))

(defun lispm-desktop-app-stickies--front-note ()
  "Return the note in front, or nil when none is open.
This is `lispm-stickies-front-note-function' on the desktop."
  (when-let* ((frame (lispm-desktop-app-stickies--front-frame)))
    (lispm-desktop-app-stickies--note frame)))

(defun lispm-desktop-app-stickies--running-p ()
  "Return non-nil while a note is open."
  (and (featurep 'lispm-stickies) (lispm-desktop-app-stickies--frames) t))

(defun lispm-desktop-app-stickies--new-geometry (front)
  "Return where a new note opens: a little right of and below FRONT, the
window of the note in front, or near the desktop's top left corner."
  (if (frame-live-p front)
      (pcase-let ((`(,left ,top . ,_) (lispm-desktop-app-stickies--frame-geometry front)))
        (list (+ left 20) (+ top 20)
              (car lispm-desktop-app-stickies-size) (cdr lispm-desktop-app-stickies-size)))
    (list 60 (+ 40 (lispm-desktop-app-window-menu-bar-height (lispm-desktop-apps-screen)))
          (car lispm-desktop-app-stickies-size) (cdr lispm-desktop-app-stickies-size))))

(defun lispm-desktop-app-stickies--frame-geometry (frame)
  "Return where FRAME, a note's window, is: (LEFT TOP WIDTH HEIGHT), as
`lispm-stickies-note-place' holds it.  A window out on the computer's
screen counts from its desktop's corner; one rolled up keeps the height
it rolls down to."
  (pcase-let ((`(,left . ,top) (lispm-desktop-platinum-window-position frame)))
    (when-let* ((screen (and (not (lispm-desktop-platinum-window-parent frame))
                             (lispm-desktop-apps-out-p frame))))
      (pcase-let ((`(,x ,y . ,_) (frame-edges screen 'native-edges)))
        (setq left (- left x) top (- top y))))
    (list left top (frame-text-width frame)
          (or (lispm-desktop-platinum-rolled-up-height frame) (frame-text-height frame)))))

(defun lispm-desktop-app-stickies--place (buffer)
  "Return where the note BUFFER's window is, (GEOMETRY . ROLLED), or nil
when it has none open.
This is `lispm-stickies-place-function' on the desktop."
  (when-let* ((frame (lispm-desktop-app-stickies--frame buffer)))
    (cons (lispm-desktop-app-stickies--frame-geometry frame)
          (and (lispm-desktop-app-window-rolled-p frame) t))))

(defun lispm-desktop-app-stickies--remember (frame)
  "Remember where FRAME, a note's window, is and whether it is rolled up."
  (when-let* ((buffer (lispm-desktop-app-stickies--note frame)))
    (lispm-stickies-set-note-place buffer (lispm-desktop-app-stickies--frame-geometry frame)
                                   (lispm-desktop-app-window-rolled-p frame))))

(defun lispm-desktop-app-stickies--define-kind (buffer)
  "Make the window of the note BUFFER a kind of floating window, which
draws its own frame, see `lispm-desktop-app-window-define'."
  (lispm-desktop-app-window-define
   (lispm-desktop-app-stickies--kind buffer)
   :app 'stickies :name "Stickies"
   :geometry (lambda (_screen) (car (lispm-stickies-note-place buffer)))
   :buffer (lambda () buffer)
   :params `((lispm-desktop-platinum-own-frame . t) (lispm-stickies-note . ,buffer)
             (child-frame-border-width . 0) (left-fringe . 1) (right-fringe . 1)
             (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
             (right-divider-width . 0) (bottom-divider-width . 0)
             (cursor-color . "#000000"))
   :roll #'lispm-desktop-app-stickies--roll
   :refocus-p #'ignore
   :on-close #'lispm-stickies-save-soon))

(defun lispm-desktop-app-stickies--forget (buffer)
  "Forget the kind of window of the note BUFFER."
  (setf (alist-get (lispm-desktop-app-stickies--kind buffer) lispm-desktop-app-window-kinds nil t) nil))

(defun lispm-desktop-app-stickies--open (buffer)
  "Open the note BUFFER in its window over the desktop, in front, rolled up
when it was; return the window."
  (let ((open (lispm-desktop-app-stickies--frame buffer)))
    (unless open
      (lispm-desktop-app-stickies--dress buffer)
      (unless (car (lispm-stickies-note-place buffer))
        (lispm-stickies-set-note-place
         buffer (lispm-desktop-app-stickies--new-geometry (lispm-desktop-app-stickies--front-frame))
         (cdr (lispm-stickies-note-place buffer)))))
    (lispm-desktop-app-stickies--define-kind buffer)
    (let ((frame (lispm-desktop-app-window-show (lispm-desktop-app-stickies--kind buffer))))
      (when (and (not open) (cdr (lispm-stickies-note-place buffer)))
        (lispm-desktop-app-window-roll-up frame))
      frame)))

(defun lispm-desktop-app-stickies--show (buffer)
  "Bring the note BUFFER in front over the desktop, Stickies' other notes
opened too when it was not running; return its window.
This is `lispm-stickies-show-note-function' on the desktop."
  (unless (lispm-desktop-app-stickies--frames)
    (dolist (other (lispm-stickies-note-buffers))
      (unless (eq other buffer)
        (lispm-desktop-app-stickies--open other))))
  (lispm-desktop-app-stickies--open buffer))

(defun lispm-desktop-app-stickies--hide (buffer)
  "Close the window of the note BUFFER, which is put away or thrown away.
This is `lispm-stickies-hide-note-function' on the desktop."
  (let ((frame (lispm-desktop-app-stickies--frame buffer)))
    (lispm-desktop-app-window-close (lispm-desktop-app-stickies--kind buffer))
    (when (frame-live-p frame)
      (delete-frame frame))
    (lispm-desktop-app-stickies--forget buffer)))

(defun lispm-desktop-app-stickies-open ()
  "Open Stickies on the desktop: its notes come back where they were, the
note in front with the keyboard; a first note opens when there is none.
This is Stickies' :activate."
  (interactive)
  (if-let* ((notes (lispm-stickies-note-buffers t)))
      (let ((front (lispm-desktop-app-stickies--front-frame)))
        (dolist (buffer notes)
          (let ((frame (lispm-desktop-app-stickies--frame buffer)))
            (if frame
                (lispm-desktop-platinum-raise-window frame)
              (lispm-desktop-app-stickies--open buffer))))
        (let ((front (or front (lispm-desktop-app-stickies--front-frame))))
          (lispm-desktop-platinum-raise-window front)
          (select-frame-set-input-focus front))
        (lispm-desktop-apps-set-frontmost 'stickies))
    (lispm-stickies-new-note)))

(defun lispm-desktop-app-stickies--rolled-height (frame)
  "Return the height of FRAME's title bar while it is rolled up: a pixel
more than a line, as Emacs keeps the tab line of no window shorter."
  (1+ (frame-char-height frame)))

(defun lispm-desktop-app-stickies--roll (frame)
  "Leave FRAME's note its title bar alone, for rolling it up; return the
title bar's height.  This is the :roll of a note's kind of window."
  (set-window-parameter (frame-root-window frame) 'mode-line-format 'none)
  (lispm-desktop-app-stickies--rolled-height frame))

(defun lispm-desktop-app-stickies--resized (window)
  "Take the note in WINDOW as rolled down when the window system made it
taller while it was rolled up, as macOS does when its edge is dragged:
its title bar would be drawn rolled up over its text.  A size that is
the note's own, rolled up or down, is not taken so, as macOS reports a
size Emacs set a moment late.
This is one of `window-size-change-functions' of a note."
  (let ((frame (window-frame window)))
    (when (and (lispm-desktop-app-window-rolled-p frame)
               (not (memql (frame-text-height frame)
                           (list (lispm-desktop-platinum-rolled-up-height frame)
                                 (lispm-desktop-app-stickies--rolled-height frame)))))
      (lispm-desktop-app-window-roll-down frame)
      (lispm-desktop-app-stickies--remember frame))))

(defun lispm-desktop-app-stickies--moved (frame)
  "Remember where FRAME is when it is a note's window, moved or grown.
This is one of `lispm-desktop-platinum-frame-moved-functions'."
  (when (and (featurep 'lispm-stickies) (lispm-desktop-app-stickies--note frame))
    (lispm-desktop-app-stickies--remember frame)))

(defun lispm-desktop-app-stickies--refocus ()
  "Give the note in front the keyboard back after a menu bar menu closes,
while Stickies is in front.
This is one of `lispm-desktop-platinum-menu-closed-hook'."
  (when-let* (((featurep 'lispm-stickies))
              ((eq (lispm-desktop-apps-frontmost) 'stickies))
              (frame (lispm-desktop-app-stickies--front-frame))
              ((frame-visible-p frame))
              ((not (eq frame (selected-frame)))))
    (select-frame-set-input-focus frame)))

(defun lispm-desktop-app-stickies--scroll-bar (buffer)
  "Give a note no scroll bar, as Stickies gave none.
This is one of `lispm-desktop-platinum-scroll-bar-functions'."
  (and (featurep 'lispm-stickies) (lispm-stickies-note-p buffer) 'no))

;;;; Drawing

(defconst lispm-desktop-app-stickies--close-box
  '("......x" ".xxxx.x" ".x....x" ".x....x" ".x....x" "......x" "xxxxxxx")
  "The close box, a row of pixels a string: x is ink, the rest paper.")

(defconst lispm-desktop-app-stickies--zoom-box
  '("......x" "....x.x" "...x..x" "..x...x" ".x....x" "......x" "xxxxxxx")
  "The zoom box, a row of pixels a string: x is ink, the rest paper.")

(defconst lispm-desktop-app-stickies--grow-box
  '("....xxxx" "....x..x" "....x..x" "....x..x" "xxxxx..x" "x......x" "x......x" "xxxxxxxx")
  "The grow box over a note's bottom right corner, edges included.")

(defun lispm-desktop-app-stickies--draw-box (svg rows x y paper ink pressed)
  "Draw the box of ROWS on SVG at X, Y in PAPER and INK; PRESSED fills it
with the paper's edge colour instead."
  (lispm-ui-svg-fill svg x y (length (car rows)) (length rows) (or pressed paper))
  (lispm-ui-svg-pixel-map svg x y rows `((?x . ,ink))))

(defun lispm-desktop-app-stickies-draw-title-bar (colors width height active
                                                         &optional pressed label insets)
  "Return an SVG of a note's title bar, WIDTH by HEIGHT pixels, in COLORS,
\(PAPER EDGE INK).  Its top row is the note's top edge, and its ends lie
over the note's side edges.  ACTIVE draws it as the note in front:
pinstripes of every other pixel in the edge colour, as Stickies drew
them, the close box on the left and the zoom box on the right, PRESSED
naming the box held down, `close' or `zoom'.  INSETS, (LEFT . RIGHT)
pixels, part the boxes from the side edges, by default
`lispm-desktop-app-stickies-box-inset' each.  An inactive bar is plain
paper, lit along its top.  LABEL, the note's first line, is drawn in the
middle of a bar taller than `lispm-desktop-app-stickies-title-height', as
a rolled-up note's."
  (pcase-let* ((svg (svg-create width height))
               (`(,paper ,edge ,ink) colors)
               (rolled (> height lispm-desktop-app-stickies-title-height))
               (left (or (car insets) lispm-desktop-app-stickies-box-inset))
               (right (or (cdr insets) lispm-desktop-app-stickies-box-inset))
               (box-top (lispm-desktop-app-stickies--box-top height)))
    (lispm-ui-svg-fill svg 0 0 width height paper)
    (if (not active)
        (progn (lispm-ui-svg-fill svg 0 0 (1- width) 1 "#FFFFFF")
               (lispm-ui-svg-fill svg (1- width) 0 1 height edge))
      (lispm-ui-svg-fill svg 0 0 width 1 edge)
      (let ((bottom (if rolled (- height 2) (1- height))))
        (cl-loop for y from 1 to bottom
                 do (cl-loop for x from (if (cl-oddp y) 1 2) below (1- width) by 2
                             do (lispm-ui-svg-fill svg x y 1 1 edge))))
      (lispm-ui-svg-fill svg (1- width) 0 1 height edge)
      (lispm-desktop-app-stickies--draw-box svg lispm-desktop-app-stickies--close-box left box-top
                                            paper ink (and (eq pressed 'close) edge))
      (lispm-desktop-app-stickies--draw-box svg lispm-desktop-app-stickies--zoom-box
                                            (- width right 7) box-top paper ink
                                            (and (eq pressed 'zoom) edge)))
    ;; The left edge goes on down the fringe, in the edge colour.
    (lispm-ui-svg-fill svg 0 (if active 0 1) 1 height edge)
    (when rolled
      (lispm-ui-svg-fill svg 0 (1- height) width 1 edge)
      (when label
        (let* ((room (- width 2 (* 2 (+ (max left right) 7 6))))
               (label (lispm-desktop-platinum-fit label room))
               (label-width (lispm-ui-text-width label 'lispm-ui-title))
               (x (/ (- width label-width) 2)))
          (when (> room 0)
            (when active
              (lispm-ui-svg-fill svg (- x 4) 1 (+ label-width 8) (- height 2) paper))
            (lispm-ui-svg-content svg label x 0 height (if active "#000000" "#555555")
                                  'lispm-ui-title)))))
    svg))

(defun lispm-desktop-app-stickies-draw-bottom (colors width active)
  "Return an SVG of a note's bottom, WIDTH pixels wide, in COLORS, (PAPER
EDGE INK): paper over its bottom edge, which runs under the side edges'
ends.  ACTIVE draws the grow box in the corner, over the edges, as
Stickies drew it."
  (pcase-let* ((height lispm-desktop-app-stickies-bottom-height)
               (svg (svg-create width height))
               (`(,paper ,edge ,ink) colors))
    (lispm-ui-svg-fill svg 0 0 width height paper)
    (lispm-ui-svg-fill svg 0 0 1 height edge)
    (lispm-ui-svg-fill svg (1- width) 0 1 height edge)
    (lispm-ui-svg-fill svg 0 (1- height) width 1 edge)
    (when active
      (lispm-ui-svg-pixel-map svg (- width 8) 0 lispm-desktop-app-stickies--grow-box
                              `((?x . ,ink))))
    svg))

(defun lispm-desktop-app-stickies--window-width (window)
  "Return the width in pixels a note's title bar and bottom take in WINDOW."
  (- (window-pixel-width window) (window-right-divider-width window)
     (window-scroll-bar-width window)))

(defun lispm-desktop-app-stickies--slice (image x width &rest properties)
  "Return a string showing the columns of IMAGE from X, WIDTH pixels wide,
with PROPERTIES."
  (apply #'propertize " " 'display (list (list 'slice x 0 width 1.0) image) properties))

(defvar lispm-desktop-app-stickies--title-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<tab-line> <down-mouse-1>" #'lispm-desktop-app-stickies--title-press)
    (keymap-set map "<tab-line> <mouse-1>" #'ignore)
    (keymap-set map "<tab-line> <drag-mouse-1>" #'ignore)
    (keymap-set map "<tab-line> <double-down-mouse-1>" #'ignore)
    (keymap-set map "<tab-line> <double-mouse-1>" #'lispm-desktop-app-stickies--title-double-click)
    map)
  "Keymap of a note's title bar outside its boxes: a press moves the note
by its outline, a double click rolls it up or down.")

(defun lispm-desktop-app-stickies--title-press (event)
  "Move the note whose title bar EVENT pressed by its outline, as Mac OS 9
moved a window by its title bar."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (when (windowp window)
      (lispm-desktop-platinum-drag-outline (window-frame window) 'move))))

(defun lispm-desktop-app-stickies--title-double-click (event)
  "Roll the note whose title bar EVENT double-clicked up, or down."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (when (windowp window)
      (lispm-desktop-app-stickies-roll (window-buffer window)))))

(defun lispm-desktop-app-stickies--box-button (string target command help)
  "Return STRING as the title bar box TARGET, which runs COMMAND with the
note's buffer once released over it, as Mac OS 9's boxes did; HELP
describes it."
  (propertize string
              'help-echo help
              'lispm-desktop-platinum-target target
              'keymap (let ((map (make-sparse-keymap)))
                        (keymap-set map "<tab-line> <down-mouse-1>"
                                    (lambda (event)
                                      (interactive "e")
                                      (let ((start (event-start event)))
                                        (lispm-desktop-platinum-press
                                         start (lispm-desktop-platinum-same-target-p start)
                                         (lambda () (funcall command (window-buffer (posn-window start))))))))
                        (keymap-set map "<tab-line> <mouse-1>" #'ignore)
                        map)))

(defun lispm-desktop-app-stickies--box-top (height)
  "Return the row a title bar HEIGHT pixels tall stands its boxes on:
Stickies' second, or the middle of a rolled-up note's taller bar."
  (if (> height lispm-desktop-app-stickies-title-height) (/ (- height 7) 2) 2))

(defun lispm-desktop-app-stickies--box-insets (frame height)
  "Return how far in from the side edges of FRAME, a note's window, the
boxes of its title bar HEIGHT pixels tall stand: (LEFT . RIGHT) pixels.
They are `lispm-desktop-app-stickies-box-inset', or past each corner
whose rows they cross where macOS takes a press as resizing an
undecorated frame, see `lispm-desktop-platinum-resize-corners': the top
ones always, the bottom ones too on a note rolled up."
  (let ((top (lispm-desktop-app-stickies--box-top height))
        (frame-height (frame-pixel-height frame)))
    (cl-flet ((inset (side)
                (if (and (eq (framep frame) 'ns) (frame-parameter frame 'undecorated))
                    (seq-reduce (pcase-lambda (inset `(,corner ,end . ,square))
                                  (if (and (eq corner side)
                                           (if (eq end 'top)
                                               (< top square)
                                             (> (+ top 7) (- frame-height square))))
                                      (max inset square)
                                    inset))
                                lispm-desktop-platinum-resize-corners
                                lispm-desktop-app-stickies-box-inset)
                  lispm-desktop-app-stickies-box-inset)))
      (cons (inset 'left) (inset 'right)))))

(defun lispm-desktop-app-stickies--title-bar ()
  "The tab line of a note's window: its title bar, see
`lispm-desktop-app-stickies-draw-title-bar'."
  (let* ((window (selected-window))
         (frame (window-frame window))
         (active (mode-line-window-selected-p))
         (width (lispm-desktop-app-stickies--window-width window))
         (rolled (lispm-desktop-app-window-rolled-p frame))
         (height (if rolled
                     (lispm-desktop-app-stickies--rolled-height frame)
                   lispm-desktop-app-stickies-title-height))
         (label (and rolled (lispm-stickies-note-title (current-buffer))))
         (pressed (and active (window-parameter window 'lispm-ui-pressed)))
         (colors (lispm-desktop-app-stickies--colors (lispm-stickies-note-color (current-buffer))))
         (insets (lispm-desktop-app-stickies--box-insets frame height))
         (left (car insets))
         (right (cdr insets))
         (image (lispm-ui-image
                 (list 'lispm-desktop-app-stickies-title colors width height active pressed label insets)
                 (lambda () (lispm-desktop-app-stickies-draw-title-bar colors width height active
                                                                       pressed label insets)))))
    (if (or (not active) (< width (* 2 (+ left right 14))))
        (lispm-desktop-app-stickies--slice image 0 width 'keymap lispm-desktop-app-stickies--title-map)
      (let ((plain (lambda (x w)
                     (lispm-desktop-app-stickies--slice image x w 'keymap lispm-desktop-app-stickies--title-map))))
        (concat (funcall plain 0 left)
                (lispm-desktop-app-stickies--box-button
                 (lispm-desktop-app-stickies--slice image left 7) 'close
                 #'lispm-stickies-close-note "Throw this note away")
                (funcall plain (+ left 7) (- width left right 14))
                (lispm-desktop-app-stickies--box-button
                 (lispm-desktop-app-stickies--slice image (- width right 7) 7) 'zoom
                 #'lispm-desktop-app-stickies-zoom "Size this note to its text, or back")
                (funcall plain (- width right) right))))))

(defvar lispm-desktop-app-stickies--grow-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<mode-line> <down-mouse-1>" #'lispm-desktop-app-stickies--grow-press)
    (keymap-set map "<mode-line> <mouse-1>" #'ignore)
    map)
  "Keymap of a note's grow box.")

(defun lispm-desktop-app-stickies--grow-press (event)
  "Resize the note whose grow box EVENT pressed by its outline, as Mac OS 9's
GrowWindow did."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (when (windowp window)
      (lispm-desktop-platinum-drag-outline (window-frame window) 'grow))))

(defun lispm-desktop-app-stickies--bottom ()
  "The mode line of a note's window: its bottom, see
`lispm-desktop-app-stickies-draw-bottom'."
  (let* ((window (selected-window))
         (active (mode-line-window-selected-p))
         (width (lispm-desktop-app-stickies--window-width window))
         (colors (lispm-desktop-app-stickies--colors (lispm-stickies-note-color (current-buffer))))
         (image (lispm-ui-image
                 (list 'lispm-desktop-app-stickies-bottom colors width active)
                 (lambda () (lispm-desktop-app-stickies-draw-bottom colors width active)))))
    (if (or (not active) (< width 16))
        (lispm-desktop-app-stickies--slice image 0 width)
      (concat (lispm-desktop-app-stickies--slice image 0 (- width 8))
              (lispm-desktop-app-stickies--slice image (- width 8) 8
                                                 'keymap lispm-desktop-app-stickies--grow-map
                                                 'help-echo "Drag to resize this note")))))

;;;; A note's buffer, dressed

(defun lispm-desktop-app-stickies--paint-lines ()
  "Make the current note's title bar and bottom lines its paper, no taller
than what they show, unless they are already of its colour."
  (let ((color (lispm-stickies-note-color (current-buffer))))
    (unless (eq color (car lispm-desktop-app-stickies--remaps))
      (mapc #'face-remap-remove-relative (cdr lispm-desktop-app-stickies--remaps))
      (let ((line (list :background (lispm-stickies-color-value color 'paper)
                        :height 10 :box nil :overline nil :underline nil)))
        (setq lispm-desktop-app-stickies--remaps
              (cons color (mapcar (lambda (face) (face-remap-add-relative face line))
                                  '(tab-line mode-line mode-line-active mode-line-inactive)))))
      (force-mode-line-update))))

(defun lispm-desktop-app-stickies--dress (buffer)
  "Make the note BUFFER the inside of its window: a title bar and a bottom
drawn for it, its side edges its fringes, no scroll bar, kept out of the
windows' buffer menus.  A note dressed already stays as it is."
  (with-current-buffer buffer
    (unless lispm-desktop-app-stickies--remaps
      (let ((padding (propertize " " 'display `(space :width (,lispm-desktop-app-stickies-padding)))))
        (setq-local tab-line-format '(:eval (lispm-desktop-app-stickies--title-bar))
                    mode-line-format '(:eval (lispm-desktop-app-stickies--bottom))
                    header-line-format nil
                    lispm-desktop-platinum-listed nil
                    cursor-type 'bar
                    cursor-in-non-selected-windows nil
                    left-fringe-width 1
                    right-fringe-width 1
                    right-margin-width 1
                    fringes-outside-margins t
                    indicate-buffer-boundaries nil
                    fringe-indicator-alist (mapcar (lambda (entry) (list (car entry))) fringe-indicator-alist)
                    line-prefix padding
                    wrap-prefix padding))
      (when (bound-and-true-p display-line-numbers-mode)
        (display-line-numbers-mode -1))
      (add-hook 'window-size-change-functions #'lispm-desktop-app-stickies--resized nil t))
    (lispm-desktop-app-stickies--paint-lines)))

(defun lispm-desktop-app-stickies--changed ()
  "Draw the current note anew once its text or colour changed: its lines
of its colour, its title when rolled up.
This is one of `lispm-stickies-changed-hook'."
  (when lispm-desktop-app-stickies--remaps
    (lispm-desktop-app-stickies--paint-lines)
    (when-let* ((frame (lispm-desktop-app-stickies--frame (current-buffer)))
                ((lispm-desktop-app-window-rolled-p frame)))
      (force-mode-line-update t))))

;;;; Commands

(defun lispm-desktop-app-stickies--current ()
  "Return the note to act on: the current buffer when it is a note, else
the note in front."
  (cond ((lispm-stickies-note-p (current-buffer)) (current-buffer))
        ((lispm-desktop-app-stickies--front-note))
        (t (user-error "No note is open"))))

(defun lispm-desktop-app-stickies-roll (&optional buffer)
  "Roll the note BUFFER, by default this one, up into its title bar, or
down again."
  (interactive)
  (let ((buffer (or buffer (lispm-desktop-app-stickies--current))))
    (when-let* ((frame (lispm-desktop-app-stickies--frame buffer)))
      (lispm-desktop-app-window-toggle-roll frame)
      (lispm-desktop-app-stickies--remember frame))))

(defun lispm-desktop-app-stickies--fit-size (frame)
  "Return the size FRAME's note takes to show its text unwrapped, as far as
the screen allows: (WIDTH . HEIGHT) of its text area."
  (let* ((window (frame-root-window frame))
         (screen (lispm-desktop-apps-window-screen frame))
         (buffer (window-buffer window))
         (lines (split-string (with-current-buffer buffer (buffer-string)) "\n"))
         (text (seq-max (cons 0 (mapcar (lambda (line)
                                          (string-pixel-width (propertize line 'face lispm-stickies-face)))
                                        lines))))
         (chrome (- (frame-text-width frame) (window-body-width window t)))
         (width (min (max (+ text chrome lispm-desktop-app-stickies-padding (frame-char-width frame))
                          (car lispm-desktop-platinum-window-min-size))
                     (/ (frame-inner-width screen) 2))))
    (cons width
          (let ((frame-resize-pixelwise t))
            (set-frame-width frame width nil t)
            (redisplay t)
            (min (max (cdr (window-text-pixel-size window nil nil nil nil t))
                      (cdr lispm-desktop-platinum-window-min-size))
                 (/ (* 2 (frame-inner-height screen)) 3))))))

(defun lispm-desktop-app-stickies-zoom (&optional buffer)
  "Size the note BUFFER, by default this one, to its text, as far as the
screen allows, or back to the size it had, as Stickies' zoom box did."
  (interactive)
  (let ((buffer (or buffer (lispm-desktop-app-stickies--current))))
    (when-let* ((frame (lispm-desktop-app-stickies--frame buffer)))
      (lispm-desktop-app-window-roll-down frame)
      (let ((frame-resize-pixelwise t))
        (with-current-buffer buffer
          (if-let* ((size lispm-desktop-app-stickies--unzoomed))
              (progn (setq lispm-desktop-app-stickies--unzoomed nil)
                     (set-frame-size frame (car size) (cdr size) t))
            (setq lispm-desktop-app-stickies--unzoomed (cons (frame-text-width frame)
                                                             (frame-text-height frame)))
            (pcase-let ((`(,width . ,height) (lispm-desktop-app-stickies--fit-size frame)))
              (set-frame-size frame width height t)))))
      (lispm-desktop-apps-keep-on-desktop frame)
      (lispm-desktop-app-stickies--remember frame))))

;;;; Menus

(defun lispm-desktop-app-stickies--key (command)
  "Return the key of COMMAND in a note, or of Emacs, as a menu shows it."
  (when-let* ((keys (where-is-internal command (list lispm-stickies-note-mode-map global-map) t)))
    (key-description keys)))

(defun lispm-desktop-app-stickies--item (label command &rest properties)
  "Return the menu item LABEL running COMMAND in the note in front, and
PROPERTIES.  The note comes forward first: a menu bar click focuses the
desktop under it."
  `(:label ,label
    :action ,(lambda ()
               (when-let* ((frame (lispm-desktop-app-stickies--front-frame)))
                 (select-frame-set-input-focus frame))
               (call-interactively command))
    ,@(when-let* ((key (lispm-desktop-app-stickies--key command)))
        (list :key key))
    ,@properties))

(defun lispm-desktop-app-stickies--file-items ()
  "Return the items of Stickies' File menu."
  (let ((open (and (lispm-desktop-app-stickies--front-frame) t)))
    (list (lispm-desktop-app-stickies--item "New Note" #'lispm-stickies-new-note)
          (lispm-desktop-app-stickies--item "Close Note" #'lispm-stickies-close-note :enabled open)
          'separator
          (lispm-desktop-app-stickies--item "Import Text…" #'lispm-stickies-import-text)
          (lispm-desktop-app-stickies--item "Export Text…" #'lispm-stickies-export-text :enabled open)
          'separator
          (lispm-desktop-app-stickies--item "Quit Stickies" #'lispm-stickies-quit))))

(defun lispm-desktop-app-stickies--edit-items ()
  "Return the items of Stickies' Edit menu."
  (let ((open (and (lispm-desktop-app-stickies--front-frame) t)))
    (list (lispm-desktop-app-stickies--item "Undo" #'undo :enabled open)
          (lispm-desktop-app-stickies--item "Redo" #'undo-redo :enabled open)
          'separator
          (lispm-desktop-app-stickies--item "Cut" #'kill-region :enabled open)
          (lispm-desktop-app-stickies--item "Copy" #'kill-ring-save :enabled open)
          (lispm-desktop-app-stickies--item "Paste" #'yank :enabled open)
          (lispm-desktop-app-stickies--item "Select All" #'mark-whole-buffer :enabled open))))

(defun lispm-desktop-app-stickies--note-items ()
  "Return the items of Stickies' Note menu."
  (let* ((frame (lispm-desktop-app-stickies--front-frame))
         (open (and frame t)))
    (list (lispm-desktop-app-stickies--item (if (and frame (lispm-desktop-app-window-rolled-p frame))
                                                "Roll Down" "Roll Up")
                                            #'lispm-desktop-app-stickies-roll :enabled open)
          (lispm-desktop-app-stickies--item "Zoom" #'lispm-desktop-app-stickies-zoom :enabled open)
          'separator
          (lispm-desktop-app-stickies--item "Next Note" #'lispm-stickies-next-note :enabled open)
          (lispm-desktop-app-stickies--item "Previous Note" #'lispm-stickies-previous-note :enabled open)
          'separator
          (lispm-desktop-app-stickies--item "Note Info" #'lispm-stickies-note-info :enabled open))))

(defun lispm-desktop-app-stickies--color-items ()
  "Return the items of Stickies' Color menu: the colours, the note in
front's checked."
  (let* ((frame (lispm-desktop-app-stickies--front-frame))
         (current (and frame (lispm-stickies-note-color (lispm-desktop-app-stickies--note frame)))))
    (mapcar (pcase-lambda (`(,key ,name . ,_))
              (lispm-desktop-app-stickies--item name (lambda () (interactive) (lispm-stickies-set-color key))
                                                :checked (eq key current) :enabled (and frame t)))
            lispm-stickies-colors)))

(defun lispm-desktop-app-stickies--menus ()
  "Return Stickies' menus, for the menu bar while it is in front."
  '((lispm-stickies-file "File" lispm-desktop-app-stickies--file-items)
    (lispm-stickies-edit "Edit" lispm-desktop-app-stickies--edit-items)
    (lispm-stickies-note "Note" lispm-desktop-app-stickies--note-items)
    (lispm-stickies-color "Color" lispm-desktop-app-stickies--color-items)))

;;;; Taking part

(defun lispm-desktop-app-stickies--confirm-legacy (question detail)
  "Ask QUESTION with DETAIL in LispM UI for Stickies before 0.2.0."
  (lispm-ui-confirm question detail "Throw Away"))

(defun lispm-desktop-app-stickies-setup ()
  "Show Stickies' notes on the desktop, and make it one of the desktop's
applications, once lispm-stickies is loaded.  A Stickies that makes
itself the desktop's, of before it had a frontend's variables, is left
as it is."
  (when (boundp 'lispm-stickies-show-note-function)
    (setq lispm-stickies-show-note-function #'lispm-desktop-app-stickies--show
          lispm-stickies-hide-note-function #'lispm-desktop-app-stickies--hide
          lispm-stickies-shown-function #'lispm-desktop-app-stickies--frame
          lispm-stickies-front-note-function #'lispm-desktop-app-stickies--front-note
          lispm-stickies-place-function #'lispm-desktop-app-stickies--place)
    ;; An installed Stickies need not update with the desktop.  Only its
    ;; old default needs adapting; newer defaults and user hooks stay put.
    (when (eq lispm-stickies-confirm-function 'lispm-stickies-yes-or-no-p)
      (setq lispm-stickies-confirm-function #'lispm-desktop-app-stickies--confirm-legacy))
    (add-hook 'lispm-stickies-changed-hook #'lispm-desktop-app-stickies--changed)
    (keymap-set lispm-stickies-note-mode-map "C-c C-r" #'lispm-desktop-app-stickies-roll)
    (keymap-set lispm-stickies-note-mode-map "C-c C-z" #'lispm-desktop-app-stickies-zoom)
    (lispm-desktop-define-app 'stickies :name "Stickies" :icon lispm-stickies-icon
                              :menus #'lispm-desktop-app-stickies--menus
                              :activate #'lispm-desktop-app-stickies-open
                              :running #'lispm-desktop-app-stickies--running-p)
    (add-hook 'lispm-desktop-platinum-frame-moved-functions #'lispm-desktop-app-stickies--moved)
    (add-hook 'lispm-desktop-platinum-menu-closed-hook #'lispm-desktop-app-stickies--refocus)
    (add-hook 'lispm-desktop-platinum-scroll-bar-functions #'lispm-desktop-app-stickies--scroll-bar)))

(with-eval-after-load 'lispm-stickies
  (lispm-desktop-app-stickies-setup))

(provide 'lispm-desktop-app-stickies)
;;; lispm-desktop-app-stickies.el ends here
