;;; lispm-desktop-look.el --- The systems the desktop's chrome can look like  -*- lexical-binding: t; -*-

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

;; The chrome keeps Mac OS 9's layout: a menu bar along the top, a title
;; bar on every window, floating windows in frames, alerts, a desktop of
;; icons.  What it is drawn in is a look, the forms of one system's
;; interface:
;;
;;   platinum   Mac OS 9's Platinum appearance, lispm-desktop-platinum-*.el
;;   win3       Windows 3.2's, lispm-desktop-win3.el: navy captions, the
;;              control-menu box, minimize and maximize arrows, sizing
;;              borders, white menus
;;   os2        OS/2 2.0's Workplace Shell, lispm-desktop-os2.el: teal
;;              captions over pale yellow borders, the window's own icon
;;              as its system menu, pale grey menus
;;   phosphor   green phosphor on black glass, lispm-desktop-glow.el:
;;              scanned title bars, Lisp falling down the desktop
;;   neon       neon over a city at night, lispm-desktop-glow.el: magenta
;;              to violet title bars, cyan lines
;;
;; A look is defined with `lispm-desktop-look-define' and chosen with
;; `lispm-desktop-look', from Control Panels or `lispm-desktop-set-look'.
;;
;; The chrome's drawing functions are generic functions whose default
;; methods draw Platinum; a look draws its own forms in methods specialized
;; on it:
;;
;;   (cl-defmethod lispm-desktop-platinum-draw-grow-box
;;     (palette height &context (lispm-desktop-look (eql win3)))
;;     ...)
;;
;; Every drawing still takes its colours from the palette, so any colour
;; theme draws any look.  Each look names the theme in its system's own
;; colours, which choosing it switches to while the theme in use is the
;; previous look's own.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-core)
(require 'lispm-desktop-i18n)

(defvar lispm-desktop--set-up)

;;;; Looks

(defcustom lispm-desktop-look 'platinum
  "The look the chrome is drawn in: `platinum' (Mac OS 9), `win3'
\(Windows 3.2), `os2' (OS/2 2.0's Workplace Shell), `phosphor' or `neon'.  See
lispm-desktop-look.el.  Change it from Control Panels, with
`lispm-desktop-set-look', or by setting it in your personal.el: once
Emacs is set up, a new value is drawn at once."
  :type '(choice (const :tag "Mac OS 9" platinum)
                 (const :tag "Windows 3.2" win3)
                 (const :tag "OS/2 2.0" os2)
                 (const :tag "Phosphor" phosphor)
                 (const :tag "Neon" neon)
                 (symbol :tag "Another look"))
  :group 'lispm-desktop)

(defvar lispm-desktop-looks nil
  "The looks defined, as (NAME . PROPERTIES), in the order defined.
See `lispm-desktop-look-define'.")

(defun lispm-desktop-look-define (name &rest properties)
  "Define the look NAME, the forms of one system's interface.
PROPERTIES is a plist:

  :title            its name as Control Panels shows it, a proper name
  :description      a line on what it is, in English, translated where
                    shown
  :theme            the colour theme in that system's own colours
  :icons            directories of icons/ to look for icons in before
                    icons/os9, as their names
  :system-families  font families for title bars and menus, the first
                    installed one winning
  :list-families    font families for lists and labels
  :glass            the gain of the look's glass, when its windows let the
                    desktop show through, see lispm-desktop-glass.el
  :picture          the look's own desktop picture, a file under the
                    package's directory, which `lispm-desktop-picture'
                    replaces

A look may hold properties of its own too, as the glowing looks'
:title-style, see lispm-desktop-glow.el.

Return NAME."
  (if-let* ((look (assq name lispm-desktop-looks)))
      (setcdr look properties)
    (setq lispm-desktop-looks (append lispm-desktop-looks (list (cons name properties)))))
  name)

(defun lispm-desktop-look-get (property &optional look)
  "Return PROPERTY of LOOK, by default of `lispm-desktop-look'."
  (plist-get (alist-get (or look lispm-desktop-look) lispm-desktop-looks) property))

(defun lispm-desktop-look-title (&optional look)
  "Return the name of LOOK, by default of `lispm-desktop-look'."
  (or (lispm-desktop-look-get :title look)
      (capitalize (symbol-name (or look lispm-desktop-look)))))

(defun lispm-desktop-look-families (property default)
  "Return the font families PROPERTY of the look, or DEFAULT where the
look names none."
  (or (lispm-desktop-look-get property) default))

(defvar lispm-desktop-look-changed-hook nil
  "Hook run once the chrome is drawn in another look.")

(defun lispm-desktop-look-theme-to-follow (from to)
  "Return the colour theme a switch from look FROM to look TO switches to,
or nil to keep the theme in use: TO's own theme, while the theme in use
is FROM's own, or none was chosen yet."
  (let ((old (lispm-desktop-look-get :theme from))
        (new (lispm-desktop-look-get :theme to)))
    (and new (not (eq new lispm-desktop-theme))
         (or (null lispm-desktop-theme) (eq lispm-desktop-theme old))
         new)))

(defun lispm-desktop-look-redraw ()
  "Draw the whole chrome again, in the look now chosen and the language
now spoken: this is also one of `lispm-desktop-language-changed-hook'.
Every part of it redraws when a theme is enabled, so the hooks run then
run now, for the theme in use."
  (run-hook-with-args 'enable-theme-functions (car custom-enabled-themes))
  (force-mode-line-update t)
  (run-hooks 'lispm-desktop-look-changed-hook))

(defun lispm-desktop-look-apply (from)
  "Draw the chrome in `lispm-desktop-look', which was look FROM.
The colour theme follows, see `lispm-desktop-look-theme-to-follow'; a
theme loaded draws everything again, else it is drawn again in the
theme in use."
  (if-let* ((theme (lispm-desktop-look-theme-to-follow from lispm-desktop-look)))
      (progn (lispm-desktop-set-theme theme)
             (run-hooks 'lispm-desktop-look-changed-hook))
    (lispm-desktop-look-redraw)))

(defun lispm-desktop-look--changed (_symbol look operation _where)
  "Draw the chrome in LOOK when `lispm-desktop-look' is set to it once
Emacs is set up, as the command setting it ends.  This is a variable
watcher."
  (when (and lispm-desktop--set-up (eq operation 'set) look
             (not (eq look lispm-desktop-look)))
    (let ((from lispm-desktop-look))
      (run-at-time 0 nil #'lispm-desktop-look-apply from))))

(defun lispm-desktop-set-look (look)
  "Draw the chrome in LOOK now and at every later start.
LOOK is a name of `lispm-desktop-looks'; interactively, it is asked for."
  (interactive (list (intern (completing-read
                              (lispm-desktop-tr "Look: ")
                              (mapcar (lambda (look) (symbol-name (car look))) lispm-desktop-looks)
                              nil t))))
  (unless (assq look lispm-desktop-looks)
    (user-error (lispm-desktop-tr "No look named %s") look))
  (let ((from lispm-desktop-look))
    ;; Drawn here, not by the watcher a second time.
    (let ((lispm-desktop--set-up nil))
      (customize-save-variable 'lispm-desktop-look look))
    (unless (eq from look)
      (lispm-desktop-look-apply from))))

(defun lispm-desktop-setup-look ()
  "Follow `lispm-desktop-look' as it is set, once Emacs is set up, and
draw the chrome again in a new language as it is chosen."
  (add-variable-watcher 'lispm-desktop-look #'lispm-desktop-look--changed)
  (add-hook 'lispm-desktop-language-changed-hook #'lispm-desktop-look-redraw))

;;;; Parts shared by looks

(defvar lispm-desktop-platinum-box-margin)
(declare-function lispm-desktop-platinum-content-width "lispm-desktop-platinum-draw" (content face))

(defun lispm-desktop-look-box-margin (margin)
  "Return how far in from its end a title bar stands its buttons, given
MARGIN, Mac OS 9's or more, see `lispm-desktop-platinum--box-margins':
Windows and OS/2 put them at the very ends, unless the window system
takes presses there."
  (if (> margin lispm-desktop-platinum-box-margin) margin 0))

(defconst lispm-desktop-look-alert-width 364
  "Width in pixels of a Windows or OS/2 message box inside its outline.")

(defconst lispm-desktop-look-alert-text-x 64
  "Pixels from a Windows or OS/2 message box's left to its message, past
the icon.")

(defvar lispm-desktop-look-alert-title "LispM"
  "The title of Windows and OS/2 message boxes, which named the
application asking.")

(defun lispm-desktop-look-button-order (buttons)
  "Return an alert's BUTTONS in the order Windows and OS/2 put them: the
default first, Cancel last.  Each is (LABEL VALUE ROLE ...)."
  (append (seq-filter (lambda (button) (eq (nth 2 button) 'default)) buttons)
          (seq-filter (lambda (button) (null (nth 2 button))) buttons)
          (seq-filter (lambda (button) (eq (nth 2 button) 'cancel)) buttons)))

(defun lispm-desktop-look-push-button-width (label)
  "Return the width of a Windows or OS/2 push button reading LABEL: room
for it in the system font, at least 72 pixels."
  (max 72 (+ 24 (lispm-desktop-platinum-content-width label 'lispm-desktop-platinum-title))))

;;;; Drawing shared by looks
;;
;; The PC looks build their controls of the same few forms: a rectangle
;; framed in lines lit along its top and left and shaded along its bottom
;; and right, and small arrows of pixels.

(defun lispm-desktop-look-svg-fill (svg x y width height color)
  "Fill the WIDTH by HEIGHT rectangle at X, Y of SVG with COLOR.
Nothing is drawn for a rectangle without area or a nil COLOR."
  (when (and color (> width 0) (> height 0))
    (svg-rectangle svg x y width height :fill color :stroke "none")))

(defun lispm-desktop-look-svg-edges (svg x y width height top-left bottom-right)
  "Frame the WIDTH by HEIGHT rectangle at X, Y of SVG in lines inside it.
TOP-LEFT lists the colours of the lines along its top and left,
outermost first, BOTTOM-RIGHT those along its bottom and right; a nil
colour leaves its line out.  A line along the bottom and right takes the
corners it shares with one along the top and left, as Windows and OS/2
drew their buttons."
  (dotimes (i (max (length top-left) (length bottom-right)))
    (let ((lit (nth i top-left))
          (shade (nth i bottom-right))
          (left (+ x i))
          (top (+ y i))
          (right (- (+ x width) 1 i))
          (bottom (- (+ y height) 1 i)))
      (when (and (<= left right) (<= top bottom))
        (when shade
          (lispm-desktop-look-svg-fill svg left bottom (1+ (- right left)) 1 shade)
          (lispm-desktop-look-svg-fill svg right top 1 (1+ (- bottom top)) shade))
        (when lit
          (lispm-desktop-look-svg-fill svg left top (- right left) 1 lit)
          (lispm-desktop-look-svg-fill svg left top 1 (- bottom top) lit))))))

(defun lispm-desktop-look-svg-outline (svg x y width height color)
  "Outline the WIDTH by HEIGHT rectangle at X, Y of SVG in COLOR, inside it."
  (lispm-desktop-look-svg-edges svg x y width height (list color) (list color)))

(defun lispm-desktop-look-svg-arrow (svg x y size direction color)
  "Draw on SVG a solid arrowhead SIZE rows deep pointing in DIRECTION.
DIRECTION is `up', `down', `left' or `right'; the arrowhead's widest row
is 2 SIZE - 1 pixels long, and its bounding box's top left is at X, Y."
  (dotimes (i size)
    (let ((from i) (length (- (* 2 size) 1 (* 2 i))))
      (pcase direction
        ('down (lispm-desktop-look-svg-fill svg (+ x from) (+ y i) length 1 color))
        ('up (lispm-desktop-look-svg-fill svg (+ x from) (+ y (- size 1 i)) length 1 color))
        ('right (lispm-desktop-look-svg-fill svg (+ x i) (+ y from) 1 length color))
        ('left (lispm-desktop-look-svg-fill svg (+ x (- size 1 i)) (+ y from) 1 length color))))))

(defun lispm-desktop-look-svg-dotted (svg x y width height color)
  "Draw on SVG a dotted rectangle about the WIDTH by HEIGHT box at X, Y,
every other pixel in COLOR, as Windows and OS/2 marked the control the
keyboard was on."
  (cl-flet ((dot (dx dy) (lispm-desktop-look-svg-fill svg (+ x dx) (+ y dy) 1 1 color)))
    (cl-loop for dx from 0 below width by 2
             do (dot dx 0) (dot dx (1- height)))
    (cl-loop for dy from 0 below height by 2
             do (dot 0 dy) (dot (1- width) dy))))

(defun lispm-desktop-look-svg-pixels (svg x y rows colors)
  "Draw ROWS, strings of pixels, on SVG from X, Y.
COLORS maps each character to its colour; a character it leaves out is
left clear.  A run of one colour along a row is drawn as one rectangle."
  (seq-do-indexed
   (lambda (row dy)
     (let ((start 0))
       (while (< start (length row))
         (let* ((char (aref row start))
                (end start))
           (while (and (< end (length row)) (eq (aref row end) char))
             (setq end (1+ end)))
           (when-let* ((color (alist-get char colors)))
             (lispm-desktop-look-svg-fill svg (+ x start) (+ y dy) (- end start) 1 color))
           (setq start end)))))
   rows))

(provide 'lispm-desktop-look)
;;; lispm-desktop-look.el ends here
