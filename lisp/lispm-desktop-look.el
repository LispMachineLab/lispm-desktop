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
;; A look is defined with `lispm-ui-look-define', in LispM UI's registry
;; of looks (lispm-ui-look.el), and chosen with `lispm-desktop-look', from
;; Control Panels or `lispm-desktop-set-look'.  LispM UI draws in the look
;; chosen too: `lispm-ui-look' is an alias of `lispm-desktop-look'.  Beside
;; the properties LispM UI reads, the desktop reads a look's
;;
;;   :theme     the colour theme in that system's own colours
;;   :icons     directories of icons/ to look for icons in before
;;              icons/os9, as their names
;;   :glass     the gain of the look's glass, when its windows let the
;;              desktop show through, see lispm-desktop-glass.el
;;   :picture   the look's own desktop picture, a file under the
;;              package's directory, which `lispm-desktop-picture'
;;              replaces
;;
;; and the PC looks' words, see lispm-desktop-pc.el.
;;
;; The chrome's drawing functions are generic functions whose default
;; methods draw Platinum; a look draws its own forms in methods specialized
;; on it, by `lispm-desktop-look' for the desktop's generic functions and
;; by `lispm-ui-look' for LispM UI's:
;;
;;   (cl-defmethod lispm-desktop-platinum-draw-grow-box
;;     (palette height &context (lispm-desktop-look (eql win3)))
;;     ...)
;;
;; Looks drawn alike make a family, which draws them in methods of its
;; own, specialized on `lispm-ui-look-family':
;;
;;   pc         win3 and os2, lispm-desktop-pc.el
;;   glow       phosphor and neon, lispm-desktop-glow.el
;;
;;   (cl-defmethod lispm-desktop-platinum-draw-placard
;;     (palette width height &context (lispm-desktop-look (lispm-ui-look-family pc))
;;      &key label font state)
;;     ...)
;;
;; A family's methods draw each of its looks by the look's metrics, see
;; `lispm-ui-look-metric'.  A look's own method, `(eql win3)', comes
;; before its family's, whichever was defined first, and reaches it with
;; `cl-call-next-method'.
;;
;; Every drawing still takes its colours from the palette, so any colour
;; theme draws any look.  Each look names the theme in its system's own
;; colours, which choosing it switches to while the theme in use is the
;; previous look's own.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-i18n)
(require 'lispm-ui-look)
(require 'lispm-ui-text)

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

;; LispM UI draws in the look chosen here: its variable is this one.  It
;; becomes an alias only once this one has its value, the one saved or
;; `platinum': made one before, it would give this one LispM UI's
;; `classic', which the option would then keep, though the compiler
;; would have aliases made first.  It takes this one's value first, as
;; Emacs warns of an alias made of a variable of another value.
(set-default 'lispm-ui-look lispm-desktop-look)
(with-no-warnings
  (defvaralias 'lispm-ui-look 'lispm-desktop-look))

(defvar lispm-desktop-look-changed-hook nil
  "Hook run once the chrome is drawn in another look.")

(defun lispm-desktop-look-theme-to-follow (from to)
  "Return the colour theme a switch from look FROM to look TO switches to,
or nil to keep the theme in use: TO's own theme, while the theme in use
is FROM's own, or none was chosen yet."
  (let ((old (lispm-ui-look-get :theme from))
        (new (lispm-ui-look-get :theme to)))
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
LOOK is a name of `lispm-ui-looks'; interactively, it is asked for."
  (interactive (list (intern (completing-read
                              (lispm-desktop-tr "Look: ")
                              (mapcar (lambda (look) (symbol-name (car look))) lispm-ui-looks)
                              nil t))))
  (unless (assq look lispm-ui-looks)
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
  (max 72 (+ 24 (lispm-ui-text-width label 'lispm-ui-title))))

(provide 'lispm-desktop-look)
;;; lispm-desktop-look.el ends here
