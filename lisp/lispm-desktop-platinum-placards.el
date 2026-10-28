;;; lispm-desktop-platinum-placards.el --- What a window's bottom edge tells  -*- lexical-binding: t; -*-

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

;; The placards on a window's bottom edge: Mac OS 9's outlined plates,
;; lit along their top and left, each drawn by the look in use (see
;; `lispm-desktop-platinum-draw-placard') with the text Emacs would show
;; on it drawn in, its faces, colours and icons included.  What they tell
;; of a document, left to right:
;;
;;   locked     a padlock while its file is read-only; pressed, it may
;;              be edited
;;   position   the line and column of point; pressed, Go to Line
;;   coding     the coding system and line ends, only when they are not
;;              UTF-8 and LF, as most are
;;   problems   Flymake's errors, warnings and notes, each kind counted
;;              by its icon, a kind there are none of left out; pressed,
;;              the list of them
;;   branch     the version control branch; pressed, the VC menu
;;   mode       the major mode, by its name; pressed, its menu
;;
;; A placard opening a menu carries Mac OS 9's pop-up arrow.  Every
;; placard only shortcuts a command Emacs has on a key, which its help
;; names.  On a text terminal, or in an Emacs that cannot draw SVG, a
;; placard is its text in an outlined face, its icons letters.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-look)
(require 'lispm-desktop-platinum-controls)
(require 'lispm-desktop-i18n)

(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-show-buffer-diagnostics "flymake" ())
(declare-function mouse-menu-major-mode-map "mouse" ())
(declare-function vc-menu-map-filter "vc-hooks" (orig-binding))
(defvar vc-menu-map)
(defvar vc-prefix-map)

;;;; Placards

(defface lispm-desktop-platinum-placard-text
  '((t :inherit lispm-ui-list :height 0.95))
  "Face of the text drawn on a placard of a window's bottom edge."
  :group 'lispm-desktop-platinum)

(defconst lispm-desktop-platinum-placard-height 16
  "Height in pixels of a placard on a window's bottom edge.")

(defconst lispm-desktop-platinum-placard-padding 6
  "Pixels between a placard's sides and its text.")

(defconst lispm-desktop-platinum--popup-arrow '("#######" " ##### " "  ###  " "   #   ")
  "Mac OS 9's pop-up arrow, a row of pixels a string.")

(defvar lispm-desktop-platinum--placard-images (make-hash-table :test #'equal)
  "The images of placards, by look, palette and what they show.
They are kept apart from the chrome's, see `lispm-ui-image':
the position's changes with every move, and would crowd the title bars
out.")

(defun lispm-desktop-platinum-drawn-placards-p ()
  "Return non-nil where placards are drawn, not text in an outlined face."
  (and lispm-ui-current-palette
       (not (lispm-desktop-tty-p))
       (image-type-available-p 'svg)))

(defun lispm-desktop-platinum--placard-image (key draw)
  "Return the placard image of KEY, calling DRAW to make its SVG the first
time.  The image keeps the SVG's width as :lispm-desktop-platinum-width."
  (let ((key (cons (cons lispm-ui-palette-generation lispm-desktop-look) key)))
    (or (gethash key lispm-desktop-platinum--placard-images)
        (progn
          (when (> (hash-table-count lispm-desktop-platinum--placard-images) 256)
            (clrhash lispm-desktop-platinum--placard-images))
          (let ((svg (funcall draw)))
            (puthash key (append (lispm-ui-svg-image svg)
                                 (list :lispm-desktop-platinum-width (dom-attr svg 'width)))
                     lispm-desktop-platinum--placard-images))))))

(defun lispm-desktop-platinum-placard-width (placard)
  "Return the width PLACARD, a string `lispm-desktop-platinum-placard'
returned, takes on the mode line: in pixels, in columns on a text
terminal, about so for a placard of text on a graphical display."
  (let ((image (get-text-property 0 'display placard)))
    (or (and (eq (car-safe image) 'image)
             (plist-get (cdr image) :lispm-desktop-platinum-width))
        (* (string-width (substring-no-properties placard)) (frame-char-width)))))

(cl-defun lispm-desktop-platinum-draw-window-placard (palette content &key active menu)
  "Return an SVG of a placard of a window's bottom edge showing CONTENT, in
PALETTE: the look's placard, see `lispm-desktop-platinum-draw-placard',
CONTENT drawn in as Emacs would show it, see
`lispm-ui-svg-content', in the text colour.  Unless ACTIVE
the placard lies flat and CONTENT is muted, as on an inactive window.
MENU adds the pop-up arrow after CONTENT."
  (let* ((face 'lispm-desktop-platinum-placard-text)
         (pad lispm-desktop-platinum-placard-padding)
         (arrow (if menu (+ 6 (length (car lispm-desktop-platinum--popup-arrow))) 0))
         (width (+ pad (lispm-ui-text-width content face) arrow pad))
         (height lispm-desktop-platinum-placard-height)
         (ink (plist-get palette (if active :text :muted)))
         (svg (lispm-desktop-platinum-draw-placard palette width height
                                                   :state (unless active 'inactive)))
         (content (copy-sequence content)))
    ;; The face goes on the text, its height relative to the list's,
    ;; which `lispm-ui-svg-content' sizes text from.
    (add-face-text-property 0 (length content) face t content)
    (lispm-ui-svg-content svg content pad 0 height ink 'lispm-ui-list)
    (when menu
      (lispm-desktop-platinum-svg-pixel-map svg (- width pad (- arrow 6)) (/ (- height 4) 2)
                                            lispm-desktop-platinum--popup-arrow `((?# . ,ink))))
    svg))

(defun lispm-desktop-platinum--text-placard (text active menu)
  "Return TEXT as a placard of text, as on a text terminal: in an outlined
face, black on the active window, grey on an inactive one.  The face is
layered under TEXT's own, so coloured counts survive.  MENU adds an arrow."
  (let* ((pad (propertize " " 'display `(space :width (,(lispm-desktop-px 6)))))
         (placard (concat pad text (if menu " ▾" "") pad)))
    (add-face-text-property 0 (length placard)
                            (if active 'lispm-desktop-platinum-placard 'lispm-desktop-platinum-placard-inactive)
                            t placard)
    placard))

(cl-defun lispm-desktop-platinum-placard (content &key active menu keymap help kind)
  "Return CONTENT on a placard of a window's bottom edge, as a string to put
on a mode line.  CONTENT is a string as Emacs would show it, icons
included.  ACTIVE draws it as on the active window; MENU adds the pop-up
arrow of a placard opening a menu.  KEYMAP and HELP, a string or a
function as `help-echo' takes it, are the placard's own; KIND, kept in
its `lispm-desktop-platinum-kind' property, says which placard of a
document's it is, see `lispm-desktop-platinum-fit-placards'.  Where placards
are not drawn, see `lispm-desktop-platinum-drawn-placards-p', it is
CONTENT in an outlined face; a drawn one keeps CONTENT's text in its
`lispm-desktop-platinum-label' property."
  (let ((placard
         (if (lispm-desktop-platinum-drawn-placards-p)
             (propertize " " 'display
                         (lispm-desktop-platinum--placard-image
                          (list (lispm-ui-content-key content) active menu)
                          (lambda ()
                            (lispm-desktop-platinum-draw-window-placard
                             lispm-ui-current-palette content :active active :menu menu)))
                         'lispm-desktop-platinum-label (substring-no-properties content))
           ;; A mode line reads % as a construct.
           (lispm-desktop-platinum--text-placard (string-replace "%" "%%" content) active menu))))
    (when keymap
      (put-text-property 0 (length placard) 'keymap keymap placard))
    (when help
      (put-text-property 0 (length placard) 'help-echo help placard))
    (when kind
      (put-text-property 0 (length placard) 'lispm-desktop-platinum-kind kind placard))
    placard))

(defun lispm-desktop-platinum-placard-label (placard)
  "Return the text PLACARD, a string `lispm-desktop-platinum-placard'
returned, shows."
  (or (get-text-property 0 'lispm-desktop-platinum-label placard)
      (string-trim (substring-no-properties placard))))

;;;; Icons

(cl-defgeneric lispm-desktop-platinum-draw-placard-icon (palette kind)
  "Return an SVG of the small icon KIND a placard shows, in PALETTE: a
padlock (`locked'), Mac OS 9's stop sign, caution sign and note balloon
\(`error', `warning', `note') or a branch (`branch'), drawn pixel by
pixel, at most twelve square."
  (pcase-let* ((ink (plist-get palette :text))
               (`(,rows . ,colors)
                (pcase kind
                  ('locked
                   `(("...####..." "..#....#.." "..#....#.." "..#....#.."
                      "##########" "#wwwwwwww#" "#www##www#" "#www##www#"
                      "#wwwwwwww#" "##########")
                     (?# . ,ink) (?w . "#FFFFFF")))
                  ('error
                   '(("...oooooo..." "..orrrrrro.." ".orrrwwrrro." "orrrrwwrrrro"
                      "orrrrwwrrrro" "orrrrwwrrrro" "orrrrwwrrrro" "orrrrrrrrrro"
                      "orrrrwwrrrro" ".orrrwwrrro." "..orrrrrro.." "...oooooo...")
                     (?o . "#631818") (?r . "#CE3131") (?w . "#FFFFFF")))
                  ('warning
                   '((".....oo....." "....oyyo...." "....oyyo...." "...oykkyo..."
                      "...oykkyo..." "..oyykkyyo.." "..oyykkyyo.." ".oyyyyyyyyo."
                      ".oyyykkyyyo." "oyyyykkyyyyo" "oooooooooooo")
                     (?o . "#000000") (?y . "#FFCE00") (?k . "#000000")))
                  ('note
                   '(("..oooooooo.." ".owwwwwwwwo." "owwwwwwwwwwo" "owwkkkkkkwwo"
                      "owwwwwwwwwwo" "owwkkkkwwwwo" ".owwwwwwwwo." "..oowooooo.."
                      "...owo......" "...oo.......")
                     (?o . "#000000") (?w . "#FFFFFF") (?k . "#777777")))
                  ('branch
                   `(("###...###" "#.#...#.#" "###...###" ".#.....#." ".#.....#."
                      ".#....#.." ".#...#..." ".#..#...." ".#.#....." "###......"
                      "#.#......" "###......")
                     (?# . ,ink)))))
               (svg (svg-create (length (car rows)) (length rows))))
    (lispm-desktop-platinum-svg-pixel-map svg 0 0 rows colors)
    svg))

(defun lispm-desktop-platinum-placard-icon (kind)
  "Return the small icon KIND, see `lispm-desktop-platinum-draw-placard-icon',
as a string to put in a placard: its image where placards are drawn, else
letters in the faces of compilation's errors, warnings and notes."
  (if (lispm-desktop-platinum-drawn-placards-p)
      (propertize "*" 'display
                  (lispm-ui-image
                   (list 'placard-icon kind)
                   (lambda ()
                     (lispm-desktop-platinum-draw-placard-icon lispm-ui-current-palette kind))))
    (pcase kind
      ('locked "RO")
      ('error (propertize "E" 'face 'compilation-error))
      ('warning (propertize "W" 'face 'compilation-warning))
      ('note (propertize "N" 'face 'compilation-info))
      ('branch "⎇"))))

(defun lispm-desktop-platinum--placard-gap (pixels)
  "Return a gap of PIXELS within a placard, a space on a text terminal."
  (if (lispm-desktop-tty-p) " " (propertize " " 'display `(space :width (,pixels)))))

;;;; What a document's placards tell

(defcustom lispm-desktop-platinum-mode-names
  '((emacs-lisp-mode . "Emacs Lisp") (lisp-data-mode . "Lisp Data")
    (c-mode . "C") (c++-mode . "C++") (objc-mode . "Objective-C") (java-mode . "Java"))
  "Names a window's bottom edge calls major modes by.
A mode left out is called by its `mode-name', which some modes write in
Emacs's shorthand, their state added: ELisp/l, C/*l."
  :type '(alist :key-type (symbol :tag "Major mode") :value-type (string :tag "Name"))
  :group 'lispm-desktop-platinum)

(defconst lispm-desktop-platinum--plain-codings
  '(utf-8 utf-8-emacs prefer-utf-8 undecided us-ascii)
  "Coding systems, by their bases, a window's bottom edge leaves unsaid.")

(defun lispm-desktop-platinum-position-text ()
  "Return where point is as the position placard tells it: its line and
column, the column counted from zero, as Emacs counts it, or from one
where `mode-line-position-column-format' says so.  A line too long to
count, see `line-number-display-limit', is ??."
  (lispm-desktop-tr "Line %s, Col %s" (format-mode-line "%l")
                    (format-mode-line (if (string-search "%C" (format "%s" mode-line-position-column-format))
                                          "%C" "%c"))))

(defun lispm-desktop-platinum-coding-text ()
  "Return the coding system and line ends of the buffer's file as the
coding placard tells them, or nil when they are UTF-8 and LF, as most are,
or the buffer visits no file."
  (when buffer-file-name
    (let* ((coding buffer-file-coding-system)
           (base (coding-system-base coding))
           (name (unless (memq base lispm-desktop-platinum--plain-codings)
                   (upcase (symbol-name (or (coding-system-get coding :mime-charset) base)))))
           (ends (pcase (coding-system-eol-type coding) (1 "CRLF") (2 "CR"))))
      (when (or name ends)
        (string-join (delq nil (list name ends)) " · ")))))

(defun lispm-desktop-platinum-problems ()
  "Return Flymake's diagnostics of the buffer counted, as (ERRORS WARNINGS
NOTES), or nil where Flymake is off.  A diagnostic of a type of its own
counts by the category it names, see `flymake-diagnostic-types-alist'."
  (when (bound-and-true-p flymake-mode)
    (let ((counts (list 0 0 0)))
      (dolist (diagnostic (flymake-diagnostics))
        (cl-incf (nth (pcase (get (flymake-diagnostic-type diagnostic) 'flymake-category)
                        ('flymake-error 0)
                        ('flymake-warning 1)
                        (_ 2))
                      counts)))
      counts)))

(defun lispm-desktop-platinum-problems-text (counts)
  "Return COUNTS, as `lispm-desktop-platinum-problems' returns them, as the
problems placard tells them: each kind's icon and count, a kind there are
none of left out, or nil when there are none."
  (let ((kinds (cl-loop for count in counts
                        for kind in '(error warning note)
                        unless (zerop count)
                        collect (concat (lispm-desktop-platinum-placard-icon kind)
                                        (lispm-desktop-platinum--placard-gap 3)
                                        (number-to-string count)))))
    (and kinds (mapconcat #'identity kinds (lispm-desktop-platinum--placard-gap 8)))))

(defconst lispm-desktop-platinum-branch-width 20
  "Columns the branch placard shows of a branch's name at most.")

(defun lispm-desktop-platinum-branch ()
  "Return the version control branch of the buffer's file, or nil.
A long name is cut short, see `lispm-desktop-platinum-branch-width'."
  (when (stringp vc-mode)
    (let ((branch (replace-regexp-in-string "\\`[A-Za-z]+[-:@!?]" ""
                                            (string-trim (substring-no-properties vc-mode)))))
      (unless (string-empty-p branch)
        (truncate-string-to-width branch lispm-desktop-platinum-branch-width nil nil "…")))))

(defun lispm-desktop-platinum-mode-text ()
  "Return the name the mode placard calls the buffer's major mode by, see
`lispm-desktop-platinum-mode-names'."
  (or (alist-get major-mode lispm-desktop-platinum-mode-names)
      (string-trim (substring-no-properties (format-mode-line mode-name)))))

;;;; Pressing placards
;;
;; Each command runs in the window whose bottom edge was pressed, which it
;; selects first, as pressing an inactive window's bottom brings it front.

(defun lispm-desktop-platinum-placard-goto-line (_event)
  "Go to a line of the document whose position placard was pressed."
  (interactive "@e")
  (call-interactively #'goto-line))

(defun lispm-desktop-platinum-placard-unlock (_event)
  "Let the document whose padlock was pressed be edited."
  (interactive "@e")
  (read-only-mode -1))

(defun lispm-desktop-platinum-placard-coding (_event)
  "Change the coding system of the document whose coding placard was pressed."
  (interactive "@e")
  (call-interactively #'set-buffer-file-coding-system))

(defun lispm-desktop-platinum-placard-problems (_event)
  "List the problems Flymake found in the document whose problems placard
was pressed."
  (interactive "@e")
  (flymake-show-buffer-diagnostics))

(defun lispm-desktop-platinum-placard-vc-menu (event)
  "Pop up the version control menu of the document whose branch placard
EVENT pressed."
  (interactive "@e")
  (popup-menu (vc-menu-map-filter vc-menu-map) event))

(defun lispm-desktop-platinum-placard-mode-menu (event)
  "Pop up the menu of the major mode whose placard EVENT pressed."
  (interactive "@e")
  (popup-menu (mouse-menu-major-mode-map) event))

(defvar-keymap lispm-desktop-platinum-position-placard-map
  :doc "Keymap of the position placard."
  "<mode-line> <mouse-1>" #'lispm-desktop-platinum-placard-goto-line)

(defvar-keymap lispm-desktop-platinum-locked-placard-map
  :doc "Keymap of the padlock placard."
  "<mode-line> <mouse-1>" #'lispm-desktop-platinum-placard-unlock)

(defvar-keymap lispm-desktop-platinum-coding-placard-map
  :doc "Keymap of the coding placard."
  "<mode-line> <mouse-1>" #'lispm-desktop-platinum-placard-coding)

(defvar-keymap lispm-desktop-platinum-problems-placard-map
  :doc "Keymap of the problems placard."
  "<mode-line> <mouse-1>" #'lispm-desktop-platinum-placard-problems)

(defvar-keymap lispm-desktop-platinum-branch-placard-map
  :doc "Keymap of the branch placard, which pops its menu up as it is pressed."
  "<mode-line> <down-mouse-1>" #'lispm-desktop-platinum-placard-vc-menu
  "<mode-line> <mouse-1>" #'ignore)

(defvar-keymap lispm-desktop-platinum-mode-placard-map
  :doc "Keymap of the mode placard, which pops its menu up as it is pressed."
  "<mode-line> <down-mouse-1>" #'lispm-desktop-platinum-placard-mode-menu
  "<mode-line> <mouse-1>" #'ignore
  "<mode-line> <mouse-2>" #'describe-mode)

(defun lispm-desktop-platinum--placard-help (help command)
  "Return a `help-echo' function saying HELP, a function of no arguments
returning a string with a %s for the keys of COMMAND, a command or a
prefix keymap, which says M-x vc-dir where nothing leads to it.  It is
called only as the mouse comes over the placard, in the window's buffer,
as finding keys takes long."
  (lambda (window _object _position)
    (with-current-buffer (window-buffer window)
      (let ((keys (if (keymapp command)
                      (if-let* ((keys (where-is-internal command nil t)))
                          (key-description keys)
                        (substitute-command-keys "\\[vc-dir]"))
                    (substitute-command-keys (format "\\[%s]" command)))))
        (format (funcall help) keys)))))

(defun lispm-desktop-platinum-document-placards (active)
  "Return the placards the left of a window's bottom edge shows of its
document, ACTIVE drawn as on the active window: its padlock, position and
coding, see the commentary of this file."
  (delq nil
        (list
         (when (and buffer-read-only buffer-file-name)
           (lispm-desktop-platinum-placard
            (lispm-desktop-platinum-placard-icon 'locked)
            :active active :keymap lispm-desktop-platinum-locked-placard-map
            :help (lispm-desktop-platinum--placard-help
                   (lambda () (lispm-desktop-tr "Read only.  Click to allow editing (%s)."))
                   'read-only-mode)))
         (lispm-desktop-platinum-placard
          (lispm-desktop-platinum-position-text)
          :active active :keymap lispm-desktop-platinum-position-placard-map
          :help (lispm-desktop-platinum--placard-help
                 (lambda () (lispm-desktop-tr "Click to go to a line (%s)."))
                 'goto-line))
         (when-let* ((coding (lispm-desktop-platinum-coding-text)))
           (lispm-desktop-platinum-placard
            coding
            :active active :keymap lispm-desktop-platinum-coding-placard-map
            :help (let ((system buffer-file-coding-system))
                    (lispm-desktop-platinum--placard-help
                     (lambda () (lispm-desktop-tr "Coding system %s.  Click to change it (%%s)." system))
                     'set-buffer-file-coding-system)))))))

(defun lispm-desktop-platinum-state-placards (active)
  "Return the placards the right of a window's bottom edge shows of its
buffer's state, ACTIVE drawn as on the active window: its problems,
branch and mode, see the commentary of this file."
  (delq nil
        (list
         (when-let* ((counts (lispm-desktop-platinum-problems))
                     (text (lispm-desktop-platinum-problems-text counts)))
           (lispm-desktop-platinum-placard
            text
            :active active :kind 'problems :keymap lispm-desktop-platinum-problems-placard-map
            :help (lispm-desktop-platinum--placard-help
                   (lambda ()
                     (pcase-let ((`(,errors ,warnings ,notes) counts))
                       (lispm-desktop-tr "Errors: %d, warnings: %d, notes: %d.  Click to list them (%%s)."
                                         errors warnings notes)))
                   'flymake-show-buffer-diagnostics)))
         (when-let* ((branch (lispm-desktop-platinum-branch)))
           (lispm-desktop-platinum-placard
            (concat (lispm-desktop-platinum-placard-icon 'branch)
                    (lispm-desktop-platinum--placard-gap 4) branch)
            :active active :menu t :kind 'branch :keymap lispm-desktop-platinum-branch-placard-map
            :help (lispm-desktop-platinum--placard-help
                   (lambda () (lispm-desktop-tr "Click for version control (%s)."))
                   vc-prefix-map)))
         (lispm-desktop-platinum-placard
          (lispm-desktop-platinum-mode-text)
          :active active :menu t :kind 'mode :keymap lispm-desktop-platinum-mode-placard-map
          :help (lambda (&rest _) (lispm-desktop-tr "Click for the mode's menu."))))))

(defconst lispm-desktop-platinum-placard-drop-order '(branch problems mode)
  "The placards of a buffer's state a window too narrow for them all leaves
out, first to last.")

(defun lispm-desktop-platinum-fit-placards (placards room gap)
  "Return PLACARDS, set GAP apart, fitted in ROOM: those of the kinds of
`lispm-desktop-platinum-placard-drop-order' left out, in its order, until
the rest fit.  Widths are as `lispm-desktop-platinum-placard-width' says."
  (let ((drop lispm-desktop-platinum-placard-drop-order))
    (cl-flet ((width (placards)
                (+ (apply #'+ (mapcar #'lispm-desktop-platinum-placard-width placards))
                   (* gap (max 0 (1- (length placards)))))))
      (while (and drop (> (width placards) room))
        (setq placards (seq-remove (lambda (placard)
                                     (eq (get-text-property 0 'lispm-desktop-platinum-kind placard)
                                         (car drop)))
                                   placards)
              drop (cdr drop)))
      placards)))

(provide 'lispm-desktop-platinum-placards)
;;; lispm-desktop-platinum-placards.el ends here
