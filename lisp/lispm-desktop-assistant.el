;;; lispm-desktop-assistant.el --- The first start's questions, filling the frame  -*- lexical-binding: t; -*-

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

;; The first start asks which language LispM speaks, how text is edited
;; and which appearance LispM wears.  Someone who has never seen Emacs would hardly notice a question
;; at the bottom of an empty frame, so the questions fill the frame
;; instead, as Mac OS Setup Assistant asked its own: a window on the
;; desktop, a question to a page, each choice a large button saying what it
;; does, and each appearance showing a small picture of LispM in its
;; colours.  The page is drawn in Platinum, before any theme is chosen.
;;
;; The language is asked first, and the next pages speak the one chosen.
;; Its eight choices, the computer's language and LispM's seven, each
;; named in itself, stand in two columns of smaller buttons.
;;
;; Every choice has a letter or a digit, shown on its button, that
;; chooses it.  Tab,
;; C-f and C-n ring the next choice, S-Tab, C-b and C-p the one before;
;; Return or Space chooses the one ringed, DEL goes back a question, and
;; Escape or C-g puts the questions off: LispM starts with the first
;; choices and asks again at the next start.  The mouse clicks them too.
;; Where no SVG can be drawn, as in a terminal, the questions are asked in
;; the minibuffer.

;;; Code:

(require 'cl-lib)
(require 'face-remap)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-core)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-popup)
(require 'lispm-ui-modal)
(require 'lispm-desktop-i18n)

(defconst lispm-desktop-assistant-questions
  `((lispm-desktop-editing-style
     :title ,(lispm-desktop-tr-noop "How do you edit text?")
     :detail ,(lispm-desktop-tr-noop "Choose the keys your fingers know.  Control Panels, on the desktop, changes this later.")
     :icon "keyboard-32"
     :choices ((emacs :key ?e :label ,(lispm-desktop-tr-noop "Emacs keys")
                      :text ,(lispm-desktop-tr-noop "C-x C-s saves and C-g cancels, everywhere.  The one to choose if you know neither."))
               (vim :key ?v :label ,(lispm-desktop-tr-noop "Vim keys")
                    :text ,(lispm-desktop-tr-noop "Evil in files, SPC as the leader key; Emacs keys in terminals, agents and lists."))))
    (lispm-desktop-theme
     :title ,(lispm-desktop-tr-noop "Which system do you know?")
     :detail ,(lispm-desktop-tr-noop "Control Panels, on the desktop, changes this later too.")
     :icon "appearance-32"
     :choices ((genera :key ?g :label "Open Genera" :text ,(lispm-desktop-tr-noop "Monochrome Lisp Machine, Zmacs and Listener.")
                       :preview genera)
               (platinum :key ?m :label "Mac OS 9" :text ,(lispm-desktop-tr-noop "Mac OS 9's greys, black on white.")
                         :preview platinum)
               (win3 :key ?w :label "Windows 3.1" :text ,(lispm-desktop-tr-noop "Navy captions and VGA grey.")
                     :preview win3))))
  "The first start's questions, in the order asked: (VARIABLE . PROPERTIES).
:title is the question, :detail a line or two under it, :icon the name
of its large icon in icons/os9.  :choices are (VALUE . PROPERTIES), the
first of them taken by a start that asks nothing, or a function
returning them: :key is the letter choosing it, :label and :text what
its button says, :preview a key of `lispm-desktop-assistant--previews',
the appearance its button pictures.  The words are in English,
translated when shown; a function's choices come translated.  A
question that is :live takes its choice at once, so the next pages show
it.")

(defconst lispm-desktop-assistant-language-question
  `(lispm-desktop-language
    :title ,(lispm-desktop-tr-noop "Which language?")
    :detail ,(lispm-desktop-tr-noop "Choose the language LispM speaks.  Control Panels, on the desktop, changes this later.")
    :icon "language-32"
    :choices lispm-desktop-assistant--language-choices
    :live t)
  "The first start's question of the language LispM speaks, asked before
`lispm-desktop-assistant-questions' when they are asked and the
language is not chosen yet.")

(defun lispm-desktop-assistant--language-choices ()
  "Return the choices of `lispm-desktop-assistant-language-question': the
computer's language, then each of `lispm-desktop-languages' named in
itself, chosen by the digits 1 to 8."
  (cons (list nil :key ?1 :label (lispm-desktop-tr "The computer's language")
              :text (lispm-desktop-language-name (lispm-desktop-system-language)))
        (seq-map-indexed (pcase-lambda (`(,language ,english) index)
                           (let ((name (lispm-desktop-language-name language)))
                             (list language :key (+ ?2 index) :label name
                                   :text (let ((translated (lispm-desktop-tr english)))
                                           (unless (equal translated name) translated)))))
                         lispm-desktop-languages)))

(defconst lispm-desktop-assistant--previews
  '((dracula :colors ("#282A36" "#F8F8F2" "#6272A4" "#BD93F9")
             :code ("#FF79C6" "#50FA7B" "#F1FA8C" "#6272A4" "#F8F8F2"))
    (platinum :code ("#3163FF" "#427B7B" "#316331" "#CE3131" "#000000"))
    (win3 :colors ("#FFFFFF" "#000000" "#808080" "#000080")
          :palette (:win3 t :desktop "#008080" :chrome "#C0C0C0" :title "#000080")
          :code ("#000080" "#008080" "#008000" "#800000" "#000000"))
    (genera :colors ("#FFFFFF" "#000000" "#555555" "#000000")
            :palette (:genera t :desktop "#FFFFFF" :chrome "#FFFFFF" :title "#FFFFFF")
            :code ("#000000" "#000000" "#555555" "#555555" "#000000")))
  "What the appearances' pictures are drawn in, by appearance.
:colors are the theme's background, text, dimmed text and accent, which
the chrome's palette is derived from, see
`lispm-ui-derive-palette': written out, since at the first
start Dracula's package may not be installed yet.  Without them the
picture takes the page's own palette, Platinum's.  :code colours the
lines of the picture's document: keywords, names, strings, comments,
text.")

(defconst lispm-desktop-assistant-buffer-name " *LispM Setup Assistant*"
  "Name of the buffer the assistant's page is shown in.")

(defconst lispm-desktop-assistant-width 560
  "Width in pixels of the assistant's window, its frame and shadow included.")

(defconst lispm-desktop-assistant-height 344
  "Height in pixels of the assistant's window, its frame and shadow included.")

(defconst lispm-desktop-assistant-card-height 150
  "Height in pixels of a choice's button.")

(defvar lispm-desktop-assistant--window nil
  "The window the assistant's page fills, while it is shown.")

(defvar lispm-desktop-assistant--shown nil
  "What the page shows now: its size, page, focus and pressed button.")

(defvar lispm-desktop-assistant--last-page nil
  "The page shown last, which `lispm-desktop-assistant-report' adds to.")

(defun lispm-desktop-assistant--choices (question)
  "Return QUESTION's choices, (VALUE . PROPERTIES), as the page shows them:
their words in the language LispM speaks."
  (let ((choices (plist-get (cdr question) :choices)))
    (if (functionp choices)
        (funcall choices)
      (mapcar (pcase-lambda (`(,value . ,properties))
                (let ((properties (copy-sequence properties)))
                  (dolist (property '(:label :text))
                    (when-let* ((text (plist-get properties property)))
                      (setq properties (plist-put properties property (lispm-desktop-tr text)))))
                  (cons value properties)))
              choices))))

(defun lispm-desktop-assistant-default (question)
  "Return the value QUESTION takes when nothing is asked: its first choice's."
  (car (car (lispm-desktop-assistant--choices question))))

;;;; Pages

(defun lispm-desktop-assistant--page (question index count)
  "Return the page asking QUESTION, the INDEXth of COUNT questions.
A page is a plist: QUESTION's properties, its words in the language
LispM speaks, and :step, saying how far along it is, and :back, non-nil
when there is a question to go back to."
  (let ((properties (cdr question)))
    (append (list :step (lispm-desktop-tr "Step %d of %d" (1+ index) count) :back (> index 0)
                  :title (lispm-desktop-tr (plist-get properties :title))
                  :detail (lispm-desktop-tr (plist-get properties :detail))
                  :choices (lispm-desktop-assistant--choices question))
            properties)))

(defun lispm-desktop-assistant--choice-label (question value)
  "Return the label of QUESTION's choice of VALUE."
  (plist-get (alist-get value (lispm-desktop-assistant--choices question)) :label))

(defun lispm-desktop-assistant--done-page (questions answers)
  "Return the page shown while LispM is set up after QUESTIONS were asked.
ANSWERS are the choices made, or nil when the questions were put off."
  (let ((chosen (mapconcat (lambda (question)
                             (lispm-desktop-assistant--choice-label
                              question (alist-get (car question) answers
                                                  (lispm-desktop-assistant-default question))))
                           questions ", ")))
    (list :title (lispm-desktop-tr "Setting LispM up…")
          :detail (if answers
                      (lispm-desktop-tr "%s.  This takes a moment." chosen)
                    (lispm-desktop-tr "%s for now; LispM asks again at the next start." chosen))
          :picture t)))

;;;; Layout

(defun lispm-desktop-assistant--layout (width height page)
  "Return where PAGE's parts go on a page WIDTH by HEIGHT pixels.
The value is a plist of rectangles, each (X Y WIDTH HEIGHT): the
:window, its frame and shadow included, in the middle; :content, the
platinum inside its title bar and frame; :cards, a rectangle for each of
PAGE's choices, side by side under the question, or, for more than
three, in two columns of smaller buttons, row after row; and :back, the
Go Back button at the bottom right, when PAGE goes back."
  (pcase-let* ((w lispm-desktop-assistant-width)
               (h lispm-desktop-assistant-height)
               (x (max 0 (/ (- width w) 2)))
               (y (max 0 (/ (- height h) 2)))
               ;; Inside the frame's band and black line, under the title
               ;; bar, as `lispm-desktop-platinum-draw-window-frame' draws them.
               (`(,left ,top ,right ,bottom) (lispm-desktop-platinum-frame-margins))
               (cx (+ x left))
               (cy (+ y top lispm-ui-title-bar-height))
               (cw (- w left right))
               (ch (- h top bottom lispm-ui-title-bar-height))
               (margin 20)
               (count (length (plist-get page :choices)))
               (columns (if (> count 3) 2 count))
               (rows (if (> columns 0) (/ (+ count columns -1) columns) 0))
               (raw-width (if (> count 0) (/ (- cw (* 2 margin) (* 16 (1- columns))) columns) 0))
               (card-width (if (and (> columns 1)
                                    (/= (% (- cw (* 2 margin) (* columns raw-width)) (1- columns)) 0))
                               (1+ raw-width)
                             raw-width))
               (card-height (if (> rows 1)
                                (/ (- lispm-desktop-assistant-card-height (* 6 (1- rows))) rows)
                              lispm-desktop-assistant-card-height))
               ;; The pixels left over widen the gaps, so both margins match.
               (gap (if (> columns 1) (/ (- cw (* 2 margin) (* columns card-width)) (1- columns)) 0))
               (back-width (lispm-ui-push-button-width (lispm-desktop-tr "Go Back"))))
    (list :window (list x y w h)
          :content (list cx cy cw ch)
          :cards (mapcar (lambda (index)
                           (list (+ cx margin (* (% index columns) (+ card-width gap)))
                                 (+ cy 92 (* (/ index columns) (+ card-height 6)))
                                 card-width card-height))
                         (number-sequence 0 (1- count)))
          :back (and (plist-get page :back)
                     (list (- (+ cx cw) margin back-width)
                           (- (+ cy ch) 14 lispm-ui-button-height)
                           back-width lispm-ui-button-height)))))

(defun lispm-desktop-assistant--target-at (layout x y)
  "Return what the page of LAYOUT has at X, Y: a choice's index, `back' or nil."
  (lispm-ui-modal-hit (append (seq-map-indexed (lambda (card index) (cons index card))
                                                    (plist-get layout :cards))
                                   (list (cons 'back (plist-get layout :back))))
                           x y))

;;;; Drawing

(defun lispm-desktop-assistant--draw-preview (svg x y width height palette code)
  "Draw on SVG at X, Y a picture WIDTH by HEIGHT pixels of LispM in PALETTE:
its desktop under the menu bar, and a window over it showing a document
whose lines are coloured CODE, a list of colours."
  (let* ((outline (plist-get palette :outline))
         (win3 (plist-get palette :win3))
         (genera (plist-get palette :genera))
         (no-menubar (or win3 genera))
         (grey (lambda (level) (lispm-ui-grey palette level))))
    (cl-flet ((fill (left top w h color) (lispm-ui-svg-fill svg left top w h color)))
      ;; The screen, outlined.
      (fill (1- x) (1- y) (+ width 2) (+ height 2) outline)
      (fill x y width height (plist-get palette :desktop))
      ;; The menu bar on Mac OS 9; Windows 3.1 and Genera have none at the top.
      (unless no-menubar
        (fill x y width 6 (plist-get palette :chrome))
        (fill x (+ y 6) width 1 outline))
      ;; Open Genera has no desktop: the full screen is Dynamic Lisp Listener 1
      ;; with a double-rule frame, left 1-bit scroll bar, inverse-video Mouse
      ;; Documentation Line, and white Who-Line.
      (if genera
          (let ((pane-h (- height 8)))
            (fill x y width pane-h outline)
            (fill (+ x 2) (+ y 2) (- width 4) (- pane-h 4) (plist-get palette :content))
            ;; Left 1-bit vertical scroll bar.
            (fill (+ x 5) (+ y 5) 3 (- pane-h 12) outline)
            (fill (+ x 6) (+ y 6) 1 (- pane-h 14) (plist-get palette :content))
            ;; Centered Symbolics Open Genera 2.0 herald and listener lines.
            (fill (+ x (/ (- width 44) 2)) (+ y 7) 44 3 outline)
            (fill (+ x (/ (- width 64) 2)) (+ y 13) 64 2 (nth 2 code))
            (fill (+ x 12) (+ y 21) (- width 24) 1 outline)
            (seq-do-indexed
             (pcase-lambda (`(,indent ,length ,color) index)
               (when-let* ((ink (nth color code)))
                 (fill (+ x 12 indent) (+ y 25 (* 5 index)) (min length (- width 20 indent)) 2 ink)))
             '((0 36 0) (0 52 2) (0 44 1) (0 18 0)))
            ;; Bottom 2-line inverse-video black Mouse Documentation Line + white Who-Line.
            (fill x (- (+ y height) 7) width 4 outline)
            (fill x (- (+ y height) 3) width 3 (plist-get palette :content)))
        ;; A window over the desktop: on Windows 3.1, a navy caption with
        ;; control-menu and sizing boxes; on Mac OS 9, a striped title bar
        ;; casting its shadow.
        (let ((left (+ x 12)) (top (+ y (if no-menubar 6 13))) (w (- width 30)) (h (- height (if win3 15 19))))
          (unless no-menubar
            (fill (1+ left) (1+ top) w h outline))
          (fill left top w h outline)
          (cond
           (win3
            (fill (1+ left) (1+ top) (- w 2) 7 (plist-get palette :title))
            (fill (1+ left) (1+ top) 7 7 (plist-get palette :chrome))
            (fill (+ left 3) (+ top 4) 3 1 outline)
            (fill (- (+ left w) 15) (1+ top) 14 7 (plist-get palette :chrome))
            (fill (- (+ left w) 8) (1+ top) 1 7 outline))
           (t
            (fill (1+ left) (1+ top) (- w 2) 7 (funcall grey #xCC))
            (dolist (row '(2 4 6))
              (fill (+ left 4) (+ top row) (- w 8) 1 (funcall grey #xFF)))))
          (fill (1+ left) (+ top 8) (- w 2) 1 outline)
          (fill (1+ left) (+ top 9) (- w 2) (- h 10) (plist-get palette :content))
          ;; The document: a definition and its body, a comment, more code.
          (seq-do-indexed
           (pcase-lambda (`(,indent ,length ,color) index)
             (when-let* ((ink (nth color code)))
               (fill (+ left 5 indent) (+ top 13 (* 5 index)) (min length (- w 10 indent)) 2 ink)))
           '((0 14 0) (0 28 1) (6 40 2) (6 22 4) (0 46 3) (6 30 0))))))))

(defun lispm-desktop-assistant--draw-row-card (svg ink rect properties cap)
  "Draw on SVG, in INK, the words of a small button filling RECT, of
PROPERTIES as a choice has them: its label at the left, its text dimmed
before the key cap, CAP pixels wide, at the right."
  (pcase-let* ((`(,x ,y ,w ,h) rect)
               (label (plist-get properties :label))
               (text (plist-get properties :text))
               (room (- w 12 cap 16 (lispm-ui-text-width label 'lispm-ui-title))))
    (lispm-ui-svg-content svg label (+ x 12) (+ y (/ (- h 20) 2)) 20 ink
                                          'lispm-ui-title)
    (when (and text (> room 40))
      (let ((text (lispm-desktop-platinum-fit (propertize text 'face 'lispm-ui-list) (- room 8))))
        (lispm-ui-svg-content
         svg text (- (+ x w) cap 16 (lispm-ui-text-width text 'lispm-ui-list))
         (+ y (/ (- h 16) 2)) 16 ink 'lispm-ui-list)))))

(defun lispm-desktop-assistant--draw-card (svg palette rect choice state focused)
  "Draw on SVG the button of CHOICE, (VALUE . PROPERTIES), filling RECT.
It is a bevel button in PALETTE, in STATE as
`lispm-ui-draw-bevel-button' takes it, ringed when
FOCUSED.  It holds the picture of CHOICE's appearance, if any, its label
and its text, and its key as a key cap in its top right corner; a button
too short for that, one of a column, holds its label and text on one
line, the key cap at its right."
  (pcase-let* ((`(,x ,y ,w ,h) rect)
               (properties (cdr choice))
               (ink (plist-get (lispm-ui-bevel-button-colors palette state) :label))
               (row (< h 60))
               (cap 20))
    (when focused
      (lispm-ui-svg-focus-ring svg x y w h 1 palette))
    (lispm-ui-svg-embed
     svg (lispm-ui-draw-bevel-button palette w h :state state) x y w h)
    (lispm-ui-svg-embed
     svg (lispm-ui-draw-bevel-button
          palette cap cap :label (upcase (char-to-string (plist-get properties :key)))
          :face 'lispm-ui-title)
     (- (+ x w) cap 8) (if row (+ y (/ (- h cap) 2)) (+ y 8)) cap cap)
    (if row
        (lispm-desktop-assistant--draw-row-card svg ink rect properties cap)
      (lispm-desktop-assistant--draw-block-card svg palette ink rect properties))))

(defun lispm-desktop-assistant--draw-block-card (svg palette ink rect properties)
  "Draw on SVG, in INK, the inside of a choice's button filling RECT, of
PROPERTIES as a choice has them, as one block in the middle: the
picture of its appearance, if any, drawn in PALETTE when the appearance
names no colours, its label and the lines of its text, centred."
  (pcase-let* ((`(,x ,y ,w ,h) rect)
               (preview (alist-get (plist-get properties :preview) lispm-desktop-assistant--previews))
               (lines (lispm-ui-alert--wrap (or (plist-get properties :text) "") (- w 32)
                                            'lispm-ui-list))
               (label (propertize (plist-get properties :label) 'face '(:height 1.25)))
               (top (+ y (/ (- h (+ (if preview 74 0) 24 (* 16 (length lines)))) 2))))
    (when preview
      (let* ((base (if-let* ((colors (plist-get preview :colors)))
                       (apply #'lispm-ui-derive-palette colors)
                     palette))
             (picture (append (plist-get preview :palette) base)))
        (lispm-desktop-assistant--draw-preview svg (+ x (/ (- w 120) 2)) top 120 64 picture
                                               (plist-get preview :code)))
      (setq top (+ top 74)))
    (lispm-ui-svg-content
     svg label (+ x (/ (- w (lispm-ui-text-width label 'lispm-ui-title)) 2))
     top 20 ink 'lispm-ui-title)
    (setq top (+ top 24))
    (dolist (line lines)
      (lispm-ui-svg-content
       svg line (+ x (/ (- w (lispm-ui-text-width line 'lispm-ui-list)) 2))
       top 16 ink 'lispm-ui-list)
      (setq top (+ top 16)))))

(defun lispm-desktop-assistant--hint (page)
  "Return the line telling how PAGE's choices are made."
  (let ((keys (mapcar (lambda (choice) (upcase (char-to-string (plist-get (cdr choice) :key))))
                      (plist-get page :choices))))
    (concat (if (> (length keys) 3)
                (lispm-desktop-tr "Press %s to %s, or click one." (car keys) (car (last keys)))
              (lispm-desktop-tr "Press %s, or click one." (string-join keys (lispm-desktop-tr " or "))))
            "  "
            (if (plist-get page :back)
                (lispm-desktop-tr "Tab moves, Return chooses, DEL goes back, Esc decides later.")
              (lispm-desktop-tr "Tab moves, Return chooses, Esc decides later.")))))

(defun lispm-desktop-assistant-draw (palette width height page focused pressed)
  "Return an SVG WIDTH by HEIGHT pixels of the assistant showing PAGE.
It is drawn in PALETTE as a Mac OS 9 window in the middle of the desktop,
titled Welcome to LispM: PAGE's icon and question at the top left, its
choices as buttons under them, the one whose index is FOCUSED ringed
and the one PRESSED, an index or `back', held down; under the buttons
how to choose, then how far along the questions are and Go Back.  A
page with a :picture shows LispM's λ instead of choices, and its
:status under it."
  (let* ((layout (lispm-desktop-assistant--layout width height page))
         (svg (svg-create width height))
         (text (plist-get palette :text))
         (title (propertize (lispm-desktop-tr "Welcome to LispM") 'face 'lispm-ui-title)))
    (lispm-ui-svg-fill svg 0 0 width height (plist-get palette :desktop))
    (pcase-let ((`(,x ,y ,w ,h) (plist-get layout :window))
                (`(,cx ,cy ,cw ,ch) (plist-get layout :content)))
      (lispm-ui-svg-embed
       svg (lispm-desktop-platinum-draw-window-frame palette w h :active t) x y w h)
      (let ((bar (lispm-ui-title-layout
                  cw (lispm-ui-text-width title 'lispm-ui-title)
                  '(nil nil nil))))
        (lispm-ui-svg-embed
         svg (lispm-ui-draw-title-part
              palette cw :active t :ridges (plist-get bar :ridges)
              :label (lispm-desktop-platinum-fit title (plist-get bar :title))
              :label-x (plist-get bar :label-x))
         cx (1+ y) cw lispm-ui-title-bar-height))
      (lispm-ui-svg-fill svg cx cy cw ch (lispm-ui-grey palette #xDD))
      ;; The icon, the question and what it is about.
      (if (plist-get page :picture)
          (svg-embed svg (lispm-ui-icon-file 'lambda)
                     "image/png" nil :x (+ cx 20) :y (+ cy 18) :width 32 :height 32)
        (lispm-ui-svg-icon
         svg (lispm-ui-icon-file (plist-get page :icon))
         (+ cx 20) (+ cy 18) 32))
      (let ((heading (propertize (plist-get page :title) 'face '(:height 1.4)))
            (top (+ cy 42)))
        (lispm-ui-svg-content svg heading (+ cx 68) (+ cy 14) 26 text
                                              'lispm-ui-title)
        (dolist (line (lispm-ui-alert--wrap (plist-get page :detail) (- cw 88)
                                            'lispm-ui-list))
          (lispm-ui-svg-content svg line (+ cx 68) top 16 text 'lispm-ui-list)
          (setq top (+ top 16))))
      (if (plist-get page :picture)
          ;; Leave room below the logo for the setup progress and messages.
          (let* ((size 96)
                 (top (+ cy 90))
                 (bar-x (+ cx 32))
                 (bar-y (- (+ cy ch) 94))
                 (bar-width (- cw 64))
                 (done (or (plist-get page :progress) 0))
                 (sub (or (plist-get page :subprogress) 0.0))
                 (total (max 1 (or (plist-get page :total) 1)))
                 (effective (min (float total) (+ (float done) (if (< done total) sub 0.0)))))
            (svg-embed svg (lispm-ui-icon-file 'lambda)
                       "image/png" nil :x (+ cx (/ (- cw size) 2)) :y top :width size :height size)
            (svg-rectangle svg bar-x bar-y bar-width 18 :fill "#ffffff" :stroke "#222222")
            (when (> effective 0)
              (svg-rectangle svg (+ bar-x 2) (+ bar-y 2)
                             (max 1 (round (/ (* (- bar-width 4) effective) (float total))))
                             14 :fill "#306ba4"))
            (lispm-ui-svg-content svg (format "%d / %d" done total)
                                 bar-x (+ bar-y 22) 16 text 'lispm-ui-small)
            (when-let* ((status (plist-get page :status)))
              (lispm-ui-svg-content svg (lispm-desktop-platinum-fit (propertize status 'face 'lispm-ui-list) (- bar-width 10))
                                   bar-x (+ bar-y 40) 16 text 'lispm-ui-list))
            (when-let* ((message (plist-get page :message)))
              (lispm-ui-svg-content svg (lispm-desktop-platinum-fit (propertize message 'face 'lispm-ui-small) (- bar-width 10))
                                   bar-x (+ bar-y 59) 16 text 'lispm-ui-small)))
        (seq-mapn (lambda (choice rect index)
                    (lispm-desktop-assistant--draw-card svg palette rect choice
                                                        (and (eql pressed index) 'pressed)
                                                        (eql focused index)))
                  (plist-get page :choices) (plist-get layout :cards)
                  (number-sequence 0 (1- (length (plist-get page :choices)))))
        (let ((hint (lispm-desktop-assistant--hint page)))
          (lispm-ui-svg-content
           svg hint (+ cx (/ (- cw (lispm-ui-text-width hint 'lispm-ui-small)) 2))
           (+ cy 92 lispm-desktop-assistant-card-height 10) 16
           (lispm-ui-grey palette #x55) 'lispm-ui-small))
        (lispm-ui-svg-content
         svg (plist-get page :step) (+ cx 20) (- (+ cy ch) 14 lispm-ui-button-height)
         lispm-ui-button-height text 'lispm-ui-list)
        (pcase (plist-get layout :back)
          (`(,bx ,by ,bw ,_)
           (lispm-ui-svg-push-button svg bx by palette (lispm-desktop-tr "Go Back") bw
                                                     :state (and (eq pressed 'back) 'pressed))))))
    svg))

;;;; The page

(defun lispm-desktop-assistant--scroll-bar (buffer)
  "Say no scroll bar for BUFFER when it is the setup assistant or alert.
This is one of `lispm-desktop-platinum-scroll-bar-functions'."
  (and (member (buffer-name buffer)
               (list lispm-desktop-assistant-buffer-name
                     " *lispm-tty-alert*"
                     " *lispm-confirm-backdrop*"
                     " *lispm-focus-backdrop*"))
       'no))

(add-hook 'lispm-desktop-platinum-scroll-bar-functions #'lispm-desktop-assistant--scroll-bar)

(defvar lispm-desktop-assistant--saved-frame-colors nil
  "Cons (FRAME BG . FRINGE-BG) saved before colouring the frame as desktop.")

(defvar lispm-desktop-assistant--echo-remap-cookies nil
  "Alist of (BUFFER . COOKIES) remapping the echo area to the desktop colour.")

(defun lispm-desktop-assistant--color-echo-area (color)
  "Colour the idle minibuffer and echo area buffers with COLOR."
  (when (stringp color)
    (dolist (name '(" *Minibuf-0*" " *Echo Area 0*" " *Echo Area 1*"))
      (when-let* ((buf (get-buffer name)))
        (with-current-buffer buf
          (mapc #'face-remap-remove-relative
                (alist-get buf lispm-desktop-assistant--echo-remap-cookies))
          (setf (alist-get buf lispm-desktop-assistant--echo-remap-cookies)
                (list (face-remap-add-relative 'default :background color)
                      (face-remap-add-relative 'fringe :background color))))))))

(defun lispm-desktop-assistant--restore-echo-area ()
  "Remove face remappings added by `lispm-desktop-assistant--color-echo-area'."
  (pcase-dolist (`(,buf . ,cookies) lispm-desktop-assistant--echo-remap-cookies)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (mapc #'face-remap-remove-relative cookies))))
  (setq lispm-desktop-assistant--echo-remap-cookies nil))

(defun lispm-desktop-assistant--save-frame-colors (frame)
  "Remember FRAME's background and fringe colours before changing them."
  (when (and (frame-live-p frame)
             (not (eq (car-safe lispm-desktop-assistant--saved-frame-colors) frame)))
    (setq lispm-desktop-assistant--saved-frame-colors
          (cons frame
                (cons (frame-parameter frame 'background-color)
                      (face-background 'fringe frame nil))))))

(defun lispm-desktop-assistant--restore-frame-colors ()
  "Restore the frame colours saved by `lispm-desktop-assistant--save-frame-colors'."
  (lispm-desktop-assistant--restore-echo-area)
  (when-let* ((saved lispm-desktop-assistant--saved-frame-colors)
              (frame (car saved))
              ((frame-live-p frame)))
    (let ((bg (or (cadr saved)
                  (and (bound-and-true-p lispm-ui-current-palette)
                       (plist-get lispm-ui-current-palette :content))
                  (face-attribute 'default :background nil 'default)))
          (fringe-bg (cddr saved)))
      (when (stringp bg)
        (set-frame-parameter frame 'background-color bg))
      (set-face-background 'fringe (or fringe-bg bg 'unspecified) frame)))
  (setq lispm-desktop-assistant--saved-frame-colors nil))

(defun lispm-desktop-assistant-available-p ()
  "Return non-nil when the page can be drawn: an interactive Emacs showing SVG."
  (and (not noninteractive) (display-graphic-p) (image-type-available-p 'svg)))

(defun lispm-desktop-assistant--draw-tty (palette width height page)
  "Return a text rendering of PAGE in PALETTE for a terminal WIDTH by HEIGHT."
  (let* ((desktop (or (plist-get palette :desktop) "#666699"))
         (chrome (or (plist-get palette :chrome) "#DDDDDD"))
         (outline (or (plist-get palette :outline) "#000000"))
         (text (or (plist-get palette :text) "#000000"))
         (muted (or (plist-get palette :muted) "#555555"))
         (accent "#306ba4")
         (box-w (min (max 44 (- width 6)) 64))
         (inner-w (- box-w 2))
         (content-w (- inner-w 6))
         (title (concat " " (lispm-desktop-tr "Welcome to LispM") " "))
         (title-w (string-width title))
         (left-eq (max 2 (/ (- inner-w title-w) 2)))
         (right-eq (max 2 (- inner-w title-w left-eq)))
         (border-face `(:foreground ,outline :background ,chrome))
         (body-face `(:foreground ,text :background ,chrome))
         (muted-face `(:foreground ,muted :background ,chrome))
         (heading-face `(:foreground ,text :background ,chrome :weight bold))
         (shadow-face `(:foreground ,outline :background ,desktop))
         (top-bar (concat (propertize (concat "┌" (make-string left-eq ?═)) 'face border-face)
                          (propertize title 'face heading-face)
                          (propertize (concat (make-string right-eq ?═) "┐") 'face border-face)))
         (sep-bar (propertize (concat "├" (make-string inner-w ?─) "┤") 'face border-face))
         (bot-bar (concat (propertize (concat "└" (make-string inner-w ?─) "┘") 'face border-face)
                          (propertize "█" 'face shadow-face)))
         (shadow-bar (concat " " (propertize (make-string box-w ?▀) 'face shadow-face)))
         (heading (truncate-string-to-width
                   (or (plist-get page :title) (lispm-desktop-tr "Setting LispM up…"))
                   content-w))
         (details (lispm-ui-alert--wrap (or (plist-get page :detail) "") content-w 'lispm-ui-list))
         (logo (or (ignore-errors (lispm-ui-icon-picture "lambda" 16 nil chrome))
                   (list (propertize "  λ  " 'face `(:foreground ,accent :background ,chrome :weight bold)))))
         (done (or (plist-get page :progress) 0))
         (sub (or (plist-get page :subprogress) 0.0))
         (total (max 1 (or (plist-get page :total) 1)))
         (effective (min (float total) (+ (float done) (if (< done total) sub 0.0))))
         (bar-w (max 12 content-w))
         (filled (min bar-w (max 0 (round (/ (* bar-w effective) (float total))))))
         (bar (concat (propertize (make-string filled ?\s)
                                  'face `(:background ,accent :foreground "#ffffff"))
                      (propertize (make-string (- bar-w filled) ?\s)
                                  'face '(:background "#ffffff" :foreground "#222222"))))
         (count-str (format "%d / %d" done total))
         (status-str (truncate-string-to-width (or (plist-get page :status) "") content-w))
         (msg-str (truncate-string-to-width (or (plist-get page :message) "") content-w))
         (rows nil))
    (cl-flet ((make-row (content &optional face centered)
                (let* ((f (or face body-face))
                       (w (string-width content))
                       (pad-l (if centered (max 0 (/ (- inner-w w) 2)) 3))
                       (pad-r (max 0 (- inner-w pad-l w)))
                       (styled (copy-sequence content)))
                  (add-face-text-property 0 (length styled) f t styled)
                  (concat (propertize "│" 'face border-face)
                          (propertize (make-string pad-l ?\s) 'face body-face)
                          styled
                          (propertize (make-string pad-r ?\s) 'face body-face)
                          (propertize "│" 'face border-face)
                          (propertize "█" 'face shadow-face)))))
      (push top-bar rows)
      (push (concat sep-bar (propertize "█" 'face shadow-face)) rows)
      (push (make-row "") rows)
      (push (make-row heading heading-face) rows)
      (dolist (d details)
        (push (make-row d body-face) rows))
      (push (make-row "") rows)
      (dolist (l logo)
        (push (make-row l body-face t) rows))
      (push (make-row "") rows)
      (push (make-row bar) rows)
      (push (make-row count-str muted-face) rows)
      (push (make-row status-str body-face) rows)
      (push (make-row msg-str muted-face) rows)
      (push bot-bar rows)
      (push shadow-bar rows))
    (setq rows (nreverse rows))
    (let* ((box-h (length rows))
           (pad-top (max 0 (/ (- height box-h) 2)))
           (pad-left (make-string (max 0 (/ (- width (1+ box-w)) 2)) ?\s)))
      (concat (make-string pad-top ?\n)
              (mapconcat (lambda (r) (concat pad-left r)) rows "\n")))))

(defun lispm-desktop-assistant--root-frame (&optional frame)
  "Return the top-level parent frame of FRAME, by default the selected frame."
  (let ((frame (or frame (selected-frame))))
    (while (and (frame-live-p frame) (frame-parent frame))
      (setq frame (frame-parent frame)))
    frame))

(defun lispm-desktop-assistant--open ()
  "Fill the selected frame with the assistant's page, drawn in Platinum.
The Platinum theme is enabled for it when no theme is active yet; setting
LispM up afterwards loads the theme chosen."
  (let ((root (lispm-desktop-assistant--root-frame)))
    (when (and (frame-live-p root) (not (eq root (selected-frame))))
      (select-frame root)))
  (unless custom-enabled-themes
    (add-to-list 'custom-theme-load-path (expand-file-name "themes/" lispm-desktop-directory))
    (with-demoted-errors "LispM's Setup Assistant could not load Platinum: %S"
      (load-theme 'platinum t)))
  (lispm-ui-palette-update)
  (let ((buffer (get-buffer-create lispm-desktop-assistant-buffer-name))
        (desktop (plist-get lispm-ui-current-palette :desktop)))
    ;; What the page leaves bare, the frame's border, the column kept for
    ;; the sign of a truncated line and the echo area's fringes, in the
    ;; desktop's colour too.  The theme loaded once LispM is set up
    ;; colours the frame anew.
    (lispm-desktop-assistant--save-frame-colors (selected-frame))
    (set-frame-parameter nil 'background-color desktop)
    (set-face-background 'fringe desktop (selected-frame))
    (lispm-desktop-assistant--color-echo-area desktop)
    (with-current-buffer buffer
      (face-remap-add-relative 'default :background desktop)
      (face-remap-add-relative 'fringe :background desktop)
      ;; One image fills the window, edge to edge: nothing about it.  The
      ;; window is never scrolled sideways to show point after it.
      (let ((table (or buffer-display-table
                       (setq buffer-display-table (make-display-table)))))
        (set-display-table-slot table 'truncation (vector))
        (set-display-table-slot table 'wrap (vector)))
      (setq-local auto-hscroll-mode nil
                  hscroll-margin 0)
      (setq-local mode-line-format nil
                  header-line-format nil
                  tab-line-format nil
                  cursor-type nil
                  cursor-in-non-selected-windows nil
                  evil-emacs-state-cursor '(nil)
                  evil-normal-state-cursor '(nil)
                  evil-insert-state-cursor '(nil)
                  evil-motion-state-cursor '(nil)
                  truncate-lines t
                  line-spacing nil
                  left-fringe-width 0
                  right-fringe-width 0
                  left-margin-width 0
                  right-margin-width 0
                  vertical-scroll-bar nil
                  horizontal-scroll-bar nil
                  indicate-empty-lines nil
                  show-trailing-whitespace nil
                  scroll-margin 0))
    (let ((message-log-max nil))
      (message nil))
    (let ((window (selected-window)))
      (set-window-dedicated-p window nil)
      (ignore-errors (delete-other-windows window))
      (setq window (selected-window))
      (set-window-dedicated-p window nil)
      (set-window-buffer window buffer)
      (when (and (window-parameter window 'lispm-desktop-platinum-scroll-frame)
                 (fboundp 'lispm-desktop-platinum-scroll-sync))
        (lispm-desktop-platinum-scroll-sync))
      (set-window-scroll-bars window 0 nil 0 nil)
      (set-window-fringes window 0 0)
      (set-window-margins window 0 0)
      (internal-show-cursor window nil)
      (setq lispm-desktop-assistant--window window
            lispm-desktop-assistant--shown nil))))

(defun lispm-desktop-assistant--show (page focused pressed)
  "Show PAGE filling the assistant's window, FOCUSED ringed, PRESSED held.
See `lispm-desktop-assistant-draw'.  Return PAGE's layout.  Nothing is
drawn again while the page and the window's size stay as they are."
  (let* ((window lispm-desktop-assistant--window)
         (graphic (display-graphic-p (window-frame window)))
         (width (window-body-width window graphic))
         (height (window-body-height window graphic))
         (shown (list graphic width height page focused pressed lispm-ui-palette-generation)))
    (setq lispm-desktop-assistant--last-page page)
    (unless (equal shown lispm-desktop-assistant--shown)
      (setq lispm-desktop-assistant--shown shown)
      (when (and (window-parameter window 'lispm-desktop-platinum-scroll-frame)
                 (fboundp 'lispm-desktop-platinum-scroll-sync))
        (lispm-desktop-platinum-scroll-sync))
      (set-window-scroll-bars window 0 nil 0 nil)
      (set-window-fringes window 0 0)
      (set-window-margins window 0 0)
      (internal-show-cursor window nil)
      (when-let* ((frame (window-frame window))
                  (desktop (plist-get lispm-ui-current-palette :desktop)))
        (unless (equal (frame-parameter frame 'background-color) desktop)
          (set-frame-parameter frame 'background-color desktop))
        (set-face-background 'fringe desktop frame)
        (lispm-desktop-assistant--color-echo-area desktop))
      (with-current-buffer (window-buffer window)
        (setq-local cursor-type nil
                    cursor-in-non-selected-windows nil
                    evil-emacs-state-cursor '(nil)
                    evil-normal-state-cursor '(nil)
                    evil-insert-state-cursor '(nil)
                    evil-motion-state-cursor '(nil))
        (erase-buffer)
        (if graphic
            (insert (propertize " " 'display
                                (svg-image (lispm-desktop-assistant-draw lispm-ui-current-palette
                                                                         width height page focused pressed)
                                           :ascent 100 :scale 1)))
          (insert (lispm-desktop-assistant--draw-tty lispm-ui-current-palette
                                                     width height page)))
        (goto-char (point-min)))
      (set-window-point window (point-min))
      (set-window-start window (point-min) t)
      (redisplay t))
    (lispm-desktop-assistant--layout width height page)))

(defun lispm-desktop-assistant--read ()
  "Read the next event, a quit included, without quitting; nil after a while
without one, so that a window resized meanwhile is drawn again."
  (lispm-ui-modal-read 0.5))

(defun lispm-desktop-assistant--quit-p (event)
  "Return non-nil when EVENT quits Emacs: Command-Q, or the frame closed."
  (or (eq event 'ns-power-off)
      (eq (car-safe event) 'delete-frame)
      (and (integerp event) (memq 'super (event-modifiers event))
           (eq (event-basic-type event) ?q))))

(defun lispm-desktop-assistant--action (event page layout)
  "Return what EVENT does on PAGE, laid out as LAYOUT, or nil for nothing.
The value is (choose . INDEX), `confirm' (choose the choice ringed),
\(focus . STEP), `back', `later' (put the questions off) or `quit'; or,
for the mouse, (press . TARGET) as a button is pressed and
\(click . TARGET) as it is released, TARGET as
`lispm-desktop-assistant--target-at' returns it."
  (let ((choices (plist-get page :choices)))
    (cl-flet ((target (position)
                (when-let* (((posn-image position))
                            (xy (posn-object-x-y position)))
                  (lispm-desktop-assistant--target-at layout (car xy) (cdr xy)))))
      (cond
       ((and (characterp event)
             (cl-position (downcase event) choices :key (lambda (choice) (plist-get (cdr choice) :key))))
        (cons 'choose (cl-position (downcase event) choices
                                   :key (lambda (choice) (plist-get (cdr choice) :key)))))
       ((memq (lispm-ui-modal-role event) '(confirm press)) 'confirm)
       ((lispm-ui-modal-step event) (cons 'focus (lispm-ui-modal-step event)))
       ((eq (lispm-ui-modal-role event) 'back) (and (plist-get page :back) 'back))
       ((lispm-ui-modal-cancel-p event) 'later)
       ((lispm-desktop-assistant--quit-p event) 'quit)
       ((eq (car-safe event) 'down-mouse-1) (cons 'press (target (event-start event))))
       ((memq (car-safe event) '(mouse-1 drag-mouse-1)) (cons 'click (target (event-end event))))))))

(defun lispm-desktop-assistant--ask-on-page (questions)
  "Ask QUESTIONS on the assistant's page, one to a page.
Return the answers as an alist of (VARIABLE . VALUE) in QUESTIONS'
order, or nil when they were put off."
  (let ((index 0) (focused 0) (pressed nil) (held nil) (answers nil) (result nil))
    (while (not result)
      (let* ((question (nth index questions))
             (count (length (lispm-desktop-assistant--choices question)))
             (page (lispm-desktop-assistant--page question index (length questions)))
             (layout (lispm-desktop-assistant--show page focused pressed))
             (event (lispm-desktop-assistant--read))
             (action (and event (lispm-desktop-assistant--action event page layout))))
        (cl-labels ((answered (variable)
                    ;; The choice made before, when going back or forth.
                    (let ((question (assq variable questions)))
                      (or (cl-position (alist-get variable answers)
                                       (lispm-desktop-assistant--choices question) :key #'car)
                          0)))
                  (choose (choice)
                    (let ((value (car (nth choice (lispm-desktop-assistant--choices question)))))
                      (setf (alist-get (car question) answers) value)
                      ;; The next pages show it.
                      (when (plist-get (cdr question) :live)
                        (set-default (car question) value)))
                    (setq index (1+ index) held nil pressed nil)
                    (if (= index (length questions))
                        (setq result 'answered)
                      (setq focused (answered (car (nth index questions))))))
                  (blink (choice)
                    ;; A button chosen from the keyboard blinks as if pressed.
                    (lispm-ui-modal-flash 'button
                                               (lambda () (lispm-desktop-assistant--show page focused choice)))))
          (pcase action
            (`(choose . ,choice) (blink choice) (choose choice))
            ('confirm (blink focused) (choose focused))
            (`(focus . ,step) (setq focused (lispm-ui-modal-cycle focused step count)))
            ('back (setq index (1- index) held nil pressed nil
                         focused (answered (car (nth index questions)))))
            ('later (setq result 'later)
                    ;; Put off, the choices shown take their first again.
                    (dolist (question questions)
                      (when (plist-get (cdr question) :live)
                        (set-default (car question) (lispm-desktop-assistant-default question)))))
            ('quit (kill-emacs))
            (`(press . ,target) (setq held target pressed target))
            (`(click . ,target)
             (let ((was held))
               (setq held nil pressed nil)
               (cond ((null was))
                     ((not (equal target was)))
                     ((eq target 'back)
                      (setq index (1- index) focused (answered (car (nth index questions)))))
                     (t (setq focused target) (choose target)))))))))
    (when (eq result 'answered)
      (mapcar (lambda (question) (assq (car question) answers)) questions))))

;;;; In the minibuffer

(defun lispm-desktop-assistant--ask-in-minibuffer (questions)
  "Ask QUESTIONS in the minibuffer, one after another.
Return the answers as `lispm-desktop-assistant--ask-on-page' does; C-g
puts the questions off."
  (condition-case nil
      (seq-map-indexed
       (lambda (question index)
         (let* ((choices (lispm-desktop-assistant--choices question))
                (key (car (read-multiple-choice
                           (concat (if (= index 0) (concat (lispm-desktop-tr "Welcome to LispM.") "  ") "")
                                   (lispm-desktop-tr (plist-get (cdr question) :title)))
                           (mapcar (lambda (choice)
                                     (list (plist-get (cdr choice) :key)
                                           (downcase (plist-get (cdr choice) :label))
                                           (plist-get (cdr choice) :text)))
                                   choices))))
                (value (car (seq-find (lambda (choice) (eq (plist-get (cdr choice) :key) key)) choices))))
           (when (plist-get (cdr question) :live)
             (set-default (car question) value))
           (cons (car question) value)))
       questions)
    (quit nil)))

;;;; Asking

(defun lispm-desktop-assistant-ask (questions)
  "Ask QUESTIONS, entries of `lispm-desktop-assistant-questions'.
Return the answers as an alist of (VARIABLE . VALUE) in QUESTIONS'
order, or nil when they were put off.  They are asked on a page filling
the selected frame where it can be drawn, else in the minibuffer.  The
page then stays, saying LispM is being set up, until
`lispm-desktop-assistant-close'."
  (if (not (lispm-desktop-assistant-available-p))
      (lispm-desktop-assistant--ask-in-minibuffer questions)
    (condition-case err
        (progn
          (lispm-desktop-assistant--open)
          (let ((answers (lispm-desktop-assistant--ask-on-page questions)))
            (lispm-desktop-assistant--show (lispm-desktop-assistant--done-page questions answers) nil nil)
            answers))
      (error
       (lispm-desktop-assistant-close)
       (display-warning 'lispm-desktop (format (lispm-desktop-tr "LispM's Setup Assistant failed: %S") err))
       (lispm-desktop-assistant--ask-in-minibuffer questions)))))

(defun lispm-desktop-assistant-report (status)
  "Say STATUS, what setting LispM up is doing, on the page while it is shown."
  (when (lispm-desktop-assistant--visible-p)
    (lispm-desktop-assistant--show (plist-put (copy-sequence lispm-desktop-assistant--last-page)
                                              :status status)
                                   nil nil)))

(defun lispm-desktop-assistant--visible-p ()
  "Return non-nil when the assistant still occupies its window."
  (and (window-live-p lispm-desktop-assistant--window)
       (eq (window-buffer lispm-desktop-assistant--window)
           (get-buffer lispm-desktop-assistant-buffer-name))
       lispm-desktop-assistant--last-page))

(defvar lispm-desktop-assistant--setup-stage "Preparing setup")
(defvar lispm-desktop-assistant--last-done 0)
(defvar lispm-desktop-assistant--last-total 4)
(defvar lispm-desktop-assistant--package-count nil
  "Cons (INSTALLED . TOTAL) while installing archive packages during setup.")

(defconst lispm-desktop-assistant--stage-translations
  '((zh-Hans
     ("Preparing setup" . "正在准备设置…")
     ("Preparing common packages" . "正在准备常用软件包…")
     ("Setting up the desktop" . "正在设置桌面环境…")
     ("Setting up appearance and language" . "正在设置外观与语言…")
     ("Setting up terminal and language" . "正在设置终端与语言…")
     ("Finishing setup" . "正在完成设置…")
     ("Preparing password protection" . "正在准备密码保护…")
     ("Synchronous setup finished" . "基础设置已完成")
     ("Installing LispM Auth service…" . "正在安装 LispM 认证服务…")
     ("Installing the icon font…" . "正在安装图标字体…")
     ("Installing updates…" . "正在安装更新…")
     ("Installing component: %s…" . "正在安装组件：%s…")
     ("Updating component: %s…" . "正在更新组件：%s…")
     ("Update LispM Auth service to %s? " . "是否将 LispM 认证服务更新至 %s？")
     ("Update LispM Auth client to %s? " . "是否将 LispM 认证客户端更新至 %s？")
     ("Update component %s to its latest release? " . "是否将组件 %s 更新至最新版本？")
     ("Install LispM updates (%s) now? " . "是否现在安装 LispM 更新（%s）？")
     ("Installing package: %s (%d / %d)…" . "正在安装软件包：%s（%d / %d）…")
     ("Installing package: %s…" . "正在安装软件包：%s…")
     ("Compiling packages (%d / %d)…" . "正在编译软件包（%d / %d）…"))
    (zh-Hant
     ("Preparing setup" . "正在準備設定…")
     ("Preparing common packages" . "正在準備常用套件…")
     ("Setting up the desktop" . "正在設定桌面環境…")
     ("Setting up appearance and language" . "正在設定外觀與語言…")
     ("Setting up terminal and language" . "正在設定終端機與語言…")
     ("Finishing setup" . "正在完成設定…")
     ("Preparing password protection" . "正在準備密碼保護…")
     ("Synchronous setup finished" . "基礎設定已完成")
     ("Installing LispM Auth service…" . "正在安裝 LispM 認證服務…")
     ("Installing the icon font…" . "正在安裝圖示字型…")
     ("Installing updates…" . "正在安裝更新…")
     ("Installing component: %s…" . "正在安裝組件：%s…")
     ("Updating component: %s…" . "正在更新組件：%s…")
     ("Update LispM Auth service to %s? " . "是否將 LispM 認證服務更新至 %s？")
     ("Update LispM Auth client to %s? " . "是否將 LispM 認證用戶端更新至 %s？")
     ("Update component %s to its latest release? " . "是否將組件 %s 更新至最新版本？")
     ("Install LispM updates (%s) now? " . "是否現在安裝 LispM 更新（%s）？")
     ("Installing package: %s (%d / %d)…" . "正在安裝套件：%s（%d / %d）…")
     ("Installing package: %s…" . "正在安裝套件：%s…")
     ("Compiling packages (%d / %d)…" . "正在編譯套件（%d / %d）…"))
    (ja
     ("Preparing setup" . "セットアップを準備中…")
     ("Preparing common packages" . "基本パッケージを準備中…")
     ("Setting up the desktop" . "デスクトップを設定中…")
     ("Setting up appearance and language" . "外観と言語を設定中…")
     ("Setting up terminal and language" . "ターミナルと言語を設定中…")
     ("Finishing setup" . "セットアップを完了中…")
     ("Preparing password protection" . "パスワード保護を準備中…")
     ("Synchronous setup finished" . "基本セットアップ完了")
     ("Installing LispM Auth service…" . "LispM 認証サービスをインストール中…")
     ("Installing the icon font…" . "アイコンフォントをインストール中…")
     ("Installing updates…" . "アップデートをインストール中…")
     ("Installing component: %s…" . "コンポーネントをインストール中: %s…")
     ("Updating component: %s…" . "コンポーネントを更新中: %s…")
     ("Update LispM Auth service to %s? " . "LispM 認証サービスを %s に更新しますか？ ")
     ("Update LispM Auth client to %s? " . "LispM 認証クライアントを %s に更新しますか？ ")
     ("Update component %s to its latest release? " . "コンポーネント %s を最新リリースに更新しますか？ ")
     ("Install LispM updates (%s) now? " . "LispM のアップデート (%s) を今すぐインストールしますか？ ")
     ("Installing package: %s (%d / %d)…" . "パッケージをインストール中: %s (%d / %d)…")
     ("Installing package: %s…" . "パッケージをインストール中: %s…")
     ("Compiling packages (%d / %d)…" . "パッケージをコンパイル中 (%d / %d)…"))
    (fr
     ("Preparing setup" . "Préparation de la configuration…")
     ("Preparing common packages" . "Préparation des paquets courants…")
     ("Setting up the desktop" . "Configuration du bureau…")
     ("Setting up appearance and language" . "Configuration de l'apparence et de la langue…")
     ("Setting up terminal and language" . "Configuration du terminal et de la langue…")
     ("Finishing setup" . "Finalisation de la configuration…")
     ("Preparing password protection" . "Préparation de la protection par mot de passe…")
     ("Synchronous setup finished" . "Configuration terminée")
     ("Installing LispM Auth service…" . "Installation du service d'authentification LispM…")
     ("Installing the icon font…" . "Installation de la police d'icônes…")
     ("Installing updates…" . "Installation des mises à jour…")
     ("Installing component: %s…" . "Installation du composant : %s…")
     ("Updating component: %s…" . "Mise à jour du composant : %s…")
     ("Update LispM Auth service to %s? " . "Mettre à jour le service d'authentification LispM vers %s ? ")
     ("Update LispM Auth client to %s? " . "Mettre à jour le client d'authentification LispM vers %s ? ")
     ("Update component %s to its latest release? " . "Mettre à jour le composant %s vers sa dernière version ? ")
     ("Install LispM updates (%s) now? " . "Installer les mises à jour de LispM (%s) maintenant ? ")
     ("Installing package: %s (%d / %d)…" . "Installation du paquet : %s (%d / %d)…")
     ("Installing package: %s…" . "Installation du paquet : %s…")
     ("Compiling packages (%d / %d)…" . "Compilation des paquets (%d / %d)…"))
    (de
     ("Preparing setup" . "Einrichtung wird vorbereitet …")
     ("Preparing common packages" . "Standardpakete werden vorbereitet …")
     ("Setting up the desktop" . "Schreibtisch wird eingerichtet …")
     ("Setting up appearance and language" . "Erscheinungsbild und Sprache werden eingerichtet …")
     ("Setting up terminal and language" . "Terminal und Sprache werden eingerichtet …")
     ("Finishing setup" . "Einrichtung wird abgeschlossen …")
     ("Preparing password protection" . "Passwortschutz wird vorbereitet …")
     ("Synchronous setup finished" . "Einrichtung abgeschlossen")
     ("Installing LispM Auth service…" . "LispM-Authentifizierungsdienst wird installiert …")
     ("Installing the icon font…" . "Symbolschriftart wird installiert …")
     ("Installing updates…" . "Updates werden installiert …")
     ("Installing component: %s…" . "Komponente wird installiert: %s …")
     ("Updating component: %s…" . "Komponente wird aktualisiert: %s …")
     ("Update LispM Auth service to %s? " . "LispM-Authentifizierungsdienst auf %s aktualisieren? ")
     ("Update LispM Auth client to %s? " . "LispM-Authentifizierungs-Client auf %s aktualisieren? ")
     ("Update component %s to its latest release? " . "Komponente %s auf die neueste Version aktualisieren? ")
     ("Install LispM updates (%s) now? " . "LispM-Updates (%s) jetzt installieren? ")
     ("Installing package: %s (%d / %d)…" . "Paket wird installiert: %s (%d / %d) …")
     ("Installing package: %s…" . "Paket wird installiert: %s …")
     ("Compiling packages (%d / %d)…" . "Pakete werden kompiliert (%d / %d) …"))))

(defun lispm-desktop-assistant--tr-status (status &rest args)
  "Translate setup STATUS with ARGS in the desktop's current language."
  (let* ((lang (and (fboundp 'lispm-desktop-current-language)
                    (ignore-errors (lispm-desktop-current-language))))
         (lang-sym (if (symbolp lang) lang (and (stringp lang) (intern lang))))
         (table (alist-get lang-sym lispm-desktop-assistant--stage-translations))
         (template (or (cdr (assoc status table)) status)))
    (cond
     (args (apply #'format template args))
     ((and (stringp status)
           (string-match "\\`\\(Installing component\\|Updating component\\): \\(.*\\)…\\'" status))
      (lispm-desktop-assistant--tr-status
       (concat (match-string 1 status) ": %s…")
       (match-string 2 status)))
     (t template))))

(defun lispm-desktop-assistant--update-page (&optional status)
  "Return the progress page shown when setup installs or updates components."
  (let* ((answers (mapcar (lambda (question)
                            (cons (car question)
                                  (if (boundp (car question))
                                      (symbol-value (car question))
                                    (lispm-desktop-assistant-default question))))
                          lispm-desktop-assistant-questions))
         (page (lispm-desktop-assistant--done-page
                lispm-desktop-assistant-questions answers)))
    (setq page (plist-put page :progress lispm-desktop-assistant--last-done))
    (setq page (plist-put page :total (max 1 lispm-desktop-assistant--last-total)))
    (setq page (plist-put page :status
                          (and status (lispm-desktop-assistant--tr-status status))))
    page))

(defun lispm-desktop-assistant--activate-frame (frame)
  "Make FRAME visible and active on the desktop, including when started hidden."
  (when (frame-live-p frame)
    (unless (frame-visible-p frame)
      (make-frame-visible frame))
    (raise-frame frame)
    (select-frame-set-input-focus frame)))

(defun lispm-desktop-assistant-ensure-progress (&optional status)
  "Ensure the assistant's Platinum progress page is visible during setup."
  (when (and (not noninteractive)
             (not (daemonp))
             (not (equal (terminal-name) "initial_terminal"))
             (not (lispm-desktop-assistant--focus-mode-p))
             (or (lispm-desktop-assistant-available-p)
                 (not (display-graphic-p))))
    (let ((root (lispm-desktop-assistant--root-frame)))
      (when (and (frame-live-p root) (not (eq root (selected-frame))))
        (select-frame root))
      (unless (frame-visible-p (selected-frame))
        (lispm-desktop-assistant--activate-frame (selected-frame))))
    (unless (lispm-desktop-assistant--visible-p)
      (lispm-desktop-assistant--open)
      (setq lispm-desktop-assistant--last-page
            (lispm-desktop-assistant--update-page
             (or status lispm-desktop-assistant--setup-stage))))
    (let ((page (copy-sequence lispm-desktop-assistant--last-page)))
      (when status
        (setq page (plist-put page :status
                              (lispm-desktop-assistant--tr-status status))))
      (lispm-desktop-assistant--show page nil nil))))

(defun lispm-desktop-assistant--focus-mode-p ()
  "Return non-nil when LispM runs in or is starting as Focus Mode."
  (if (bound-and-true-p lispm-desktop-started-as)
      (eq lispm-desktop-started-as 'terminal)
    (eq (or (and (fboundp 'lispm-desktop-start-as--argument)
                 (lispm-desktop-start-as--argument (cdr command-line-args)))
            (bound-and-true-p lispm-desktop-start-as))
        'terminal)))

(defun lispm-desktop-assistant--tty-alert (icon message buttons &optional detail)
  "Render a visual text alert in the selected window when child frames lack."
  (let* ((default (nth 1 (seq-find (lambda (b) (eq (nth 2 b) 'default)) buttons)))
         (cancel (nth 1 (seq-find (lambda (b) (eq (nth 2 b) 'cancel)) buttons)))
         (values (mapcar #'cadr buttons))
         (desktop (or (plist-get lispm-ui-current-palette :desktop) "#666699"))
         (buf (generate-new-buffer " *lispm-tty-alert*"))
         (win (selected-window))
         (old-buf (window-buffer win))
         (old-ded (window-dedicated-p win))
         (pressed nil)
         (focused default)
         (choice cancel))
    (with-current-buffer buf
      (face-remap-add-relative 'default :background desktop)
      (face-remap-add-relative 'fringe :background desktop)
      (setq-local mode-line-format nil
                  header-line-format nil
                  tab-line-format nil
                  cursor-type nil
                  truncate-lines t))
    (unwind-protect
        (cl-flet ((redraw ()
                    (with-current-buffer buf
                      (let ((inhibit-read-only t)
                            (box (lispm-ui-alert--text icon message detail buttons
                                                       pressed focused))
                            (win-w (window-body-width win))
                            (win-h (window-body-height win)))
                        (erase-buffer)
                        (let* ((lines (split-string box "\n"))
                               (box-h (length lines))
                               (box-w (if lines (string-width (car lines)) 0))
                               (pad-top (max 0 (/ (- win-h box-h) 2)))
                               (pad-left (make-string (max 0 (/ (- win-w box-w) 2)) ?\s)))
                          (insert (make-string pad-top ?\n))
                          (dolist (line lines)
                            (insert pad-left line "\n")))
                        (goto-char (point-min))))
                    (set-window-dedicated-p win nil)
                    (set-window-buffer win buf)
                    (redisplay t)))
          (redraw)
          (catch 'done
            (while t
              (let* ((event (lispm-ui-modal-read))
                     (role (lispm-ui-modal-role event))
                     (step (lispm-ui-modal-step event)))
                (cond
                 ((seq-find (lambda (b) (memql event (nthcdr 3 b))) buttons)
                  (setq choice (nth 1 (seq-find (lambda (b) (memql event (nthcdr 3 b))) buttons)))
                  (throw 'done nil))
                 ((and (eq role 'cancel) (or cancel default))
                  (setq choice (or cancel default))
                  (throw 'done nil))
                 ((and (eq role 'confirm) default)
                  (setq choice default)
                  (throw 'done nil))
                 (step
                  (setq focused (nth (lispm-ui-modal-cycle (seq-position values focused)
                                                           step (length values))
                                     values))
                  (redraw))
                 ((and focused (eq role 'press))
                  (setq choice focused)
                  (throw 'done nil)))))))
      (when (window-live-p win)
        (set-window-buffer win old-buf)
        (set-window-dedicated-p win old-ded))
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    choice))

(defun lispm-desktop-assistant--prepare-focus-backdrop ()
  "Show a clean modeline-free, cursor-free backdrop in Focus Mode during setup."
  (when (and (not noninteractive)
             (not (equal (terminal-name) "initial_terminal"))
             (or (bound-and-true-p lispm-desktop-setup--in-progress)
                 (not (bound-and-true-p lispm-desktop--set-up))))
    (let ((root (lispm-desktop-assistant--root-frame)))
      (when (and (frame-live-p root) (not (eq root (selected-frame))))
        (select-frame root)))
    (let ((window (selected-window))
          (backdrop (get-buffer-create " *lispm-focus-backdrop*")))
      (with-current-buffer backdrop
        (setq-local mode-line-format nil
                    header-line-format nil
                    tab-line-format nil
                    cursor-type nil
                    cursor-in-non-selected-windows nil
                    evil-emacs-state-cursor '(nil)
                    evil-normal-state-cursor '(nil)
                    evil-insert-state-cursor '(nil)
                    evil-motion-state-cursor '(nil)
                    truncate-lines t
                    left-fringe-width 0
                    right-fringe-width 0
                    left-margin-width 0
                    right-margin-width 0
                    vertical-scroll-bar nil
                    horizontal-scroll-bar nil))
      (when (window-live-p window)
        (set-window-dedicated-p window nil)
        (ignore-errors (delete-other-windows window))
        (setq window (selected-window))
        (set-window-dedicated-p window nil)
        (set-window-buffer window backdrop)
        (set-window-scroll-bars window 0 nil 0 nil)
        (set-window-fringes window 0 0)
        (set-window-margins window 0 0)
        (internal-show-cursor window nil)
        (redisplay t)))))

(defun lispm-desktop-assistant--prepare-confirm-backdrop ()
  "Ensure a clean desktop backdrop in the selected window while confirming.
Return a cleanup function restoring the window's prior state."
  (let ((was-visible (lispm-desktop-assistant--visible-p))
        (in-setup (or (bound-and-true-p lispm-desktop-setup--in-progress)
                      (not (bound-and-true-p lispm-desktop--set-up)))))
    (if (or (equal (terminal-name) "initial_terminal")
            (and (not was-visible) (not in-setup)))
        #'ignore
      (let ((root (lispm-desktop-assistant--root-frame)))
        (when (and (frame-live-p root) (not (eq root (selected-frame))))
          (select-frame root)))
      (unless custom-enabled-themes
        (add-to-list 'custom-theme-load-path (expand-file-name "themes/" lispm-desktop-directory))
        (with-demoted-errors "LispM's Setup Assistant could not load Platinum: %S"
          (load-theme 'platinum t)))
      (unless lispm-ui-current-palette
        (lispm-ui-palette-update))
      (let* ((palette (or (bound-and-true-p lispm-ui-current-palette)
                          (bound-and-true-p lispm-ui-platinum-palette)))
             (desktop (or (plist-get palette :desktop) "#6E6E6E"))
             (frame (selected-frame))
             (window (selected-window))
             (backdrop (get-buffer-create " *lispm-confirm-backdrop*")))
        (lispm-desktop-assistant--save-frame-colors frame)
        (set-frame-parameter frame 'background-color desktop)
        (set-face-background 'fringe desktop frame)
        (lispm-desktop-assistant--color-echo-area desktop)
        (with-current-buffer backdrop
          (setq-local face-remapping-alist
                      `((default (:background ,desktop) default)
                        (fringe (:background ,desktop) fringe))
                      mode-line-format nil
                      header-line-format nil
                      tab-line-format nil
                      cursor-type nil
                      cursor-in-non-selected-windows nil
                      evil-emacs-state-cursor '(nil)
                      evil-normal-state-cursor '(nil)
                      evil-insert-state-cursor '(nil)
                      evil-motion-state-cursor '(nil)
                      truncate-lines t
                      left-fringe-width 0
                      right-fringe-width 0
                      left-margin-width 0
                      right-margin-width 0
                      vertical-scroll-bar nil
                      horizontal-scroll-bar nil))
        (let ((message-log-max nil))
          (message nil))
        (cond
         (was-visible
          ;; Temporarily clear the setup card so the alert dialog does not
          ;; collide with the "Setting LispM up…" window underneath.
          (with-current-buffer (window-buffer lispm-desktop-assistant--window)
            (let ((inhibit-read-only t))
              (erase-buffer)))
          (setq lispm-desktop-assistant--shown nil)
          (when (and (window-parameter lispm-desktop-assistant--window
                                       'lispm-desktop-platinum-scroll-frame)
                     (fboundp 'lispm-desktop-platinum-scroll-sync))
            (lispm-desktop-platinum-scroll-sync))
          (set-window-scroll-bars lispm-desktop-assistant--window 0 nil 0 nil)
          (internal-show-cursor lispm-desktop-assistant--window nil)
          (redisplay t)
          (lambda ()
            (when (buffer-live-p backdrop)
              (kill-buffer backdrop))
            (when (and (lispm-desktop-assistant--visible-p)
                       lispm-desktop-assistant--last-page)
              (lispm-desktop-assistant--show lispm-desktop-assistant--last-page nil nil))))
         (t
          (set-window-dedicated-p window nil)
          (ignore-errors (delete-other-windows window))
          (setq window (selected-window))
          (set-window-dedicated-p window nil)
          (set-window-buffer window backdrop)
          (when (and (window-parameter window 'lispm-desktop-platinum-scroll-frame)
                     (fboundp 'lispm-desktop-platinum-scroll-sync))
            (lispm-desktop-platinum-scroll-sync))
          (set-window-scroll-bars window 0 nil 0 nil)
          (set-window-fringes window 0 0)
          (set-window-margins window 0 0)
          (internal-show-cursor window nil)
          (redisplay t)
          (lambda ()
            (when (window-live-p window)
              (internal-show-cursor window t))
            (unless (lispm-desktop-assistant--visible-p)
              (lispm-desktop-assistant--restore-frame-colors)))))))))

(defun lispm-desktop-assistant-confirm-update (template &rest args)
  "Confirm an update with TEMPLATE and ARGS.
In GUI and TUI Desktop Mode, display a visual Mac OS 9 alert dialog;
in Focus Mode, prompt with `y-or-n-p'."
  (if noninteractive
      t
    (let* ((question (apply #'lispm-desktop-assistant--tr-status template args))
           (trimmed (string-trim question)))
      (cond
       ((or (lispm-desktop-assistant--focus-mode-p)
            (and (not (subrp (symbol-function 'y-or-n-p)))
                 (not (byte-code-function-p (symbol-function 'y-or-n-p)))
                 (or (equal (terminal-name) "initial_terminal")
                     (not (lispm-ui-popup-available-p)))))
        (let ((win (selected-window)))
          (when (lispm-desktop-assistant--focus-mode-p)
            (lispm-desktop-assistant--prepare-focus-backdrop))
          (unwind-protect
              (let ((use-dialog-box nil))
                (y-or-n-p question))
            (when (window-live-p win)
              (internal-show-cursor win t)))))
       (t
        (let ((restore (lispm-desktop-assistant--prepare-confirm-backdrop)))
          (unwind-protect
              (progn
                (unless (frame-visible-p (selected-frame))
                  (lispm-desktop-assistant--activate-frame (selected-frame))
                  (redisplay t))
                (let* ((lispm-ui-current-palette
                        (or (bound-and-true-p lispm-ui-current-palette)
                            (bound-and-true-p lispm-ui-platinum-palette)))
                       (buttons `((,(lispm-desktop-tr "Later") later cancel ?n ?N)
                                  (,(lispm-desktop-tr "Update") update default ?y ?Y))))
                  (eq 'update
                      (if (lispm-ui-popup-available-p)
                          (lispm-ui-alert 'note trimmed buttons)
                        (lispm-desktop-assistant--tty-alert 'note trimmed buttons)))))
            (funcall restore))))))))

(defun lispm-desktop-assistant-progress (done total status)
  "Show DONE of TOTAL synchronous setup stages and STATUS on the page."
  (setq lispm-desktop-assistant--setup-stage status
        lispm-desktop-assistant--last-done done
        lispm-desktop-assistant--last-total total
        lispm-desktop-assistant--package-count nil)
  (when (lispm-desktop-assistant--visible-p)
    (let ((page (copy-sequence lispm-desktop-assistant--last-page)))
      (setq page (plist-put page :progress done)
            page (plist-put page :subprogress 0.0)
            page (plist-put page :total total)
            page (plist-put page :message nil)
            page (plist-put page :status (lispm-desktop-assistant--tr-status status)))
      (lispm-desktop-assistant--show page nil nil))))

(defun lispm-desktop-assistant-compile-progress (done total)
  "Update setup page with parallel compilation progress DONE of TOTAL files."
  (when (and (lispm-desktop-assistant--visible-p)
             lispm-desktop-assistant--last-page)
    (let ((page (copy-sequence lispm-desktop-assistant--last-page)))
      (setq page (plist-put page :subprogress (* 0.95 (/ (float done) (float (max 1 total)))))
            page (plist-put page :status
                            (lispm-desktop-assistant--tr-status
                             "Compiling packages (%d / %d)…" done total)))
      (lispm-desktop-assistant--show page nil nil))))

(defvar lispm-desktop-assistant--capturing-message nil)

(defun lispm-desktop-assistant--package-message (original format-string &rest args)
  "Display package ORIGINAL's FORMAT-STRING and ARGS on the setup page."
  (if (or inhibit-message lispm-desktop-assistant--capturing-message
          (not (stringp format-string)))
      (apply original format-string args)
    (let ((inhibit-message t)
          (lispm-desktop-assistant--capturing-message t))
      (prog1 (apply original format-string args)
        (when-let* ((message (condition-case nil
                                 (apply #'format-message format-string args)
                               (error nil)))
                    ((not (string-empty-p message)))
                    ((lispm-desktop-assistant--visible-p))
                    (page lispm-desktop-assistant--last-page))
          (lispm-desktop-assistant--show
           (plist-put (copy-sequence page)
                      :message (lispm-desktop-assistant--tr-status message))
           nil nil))))))

(defvar lispm-desktop-assistant--package-depth 0)

(declare-function lispm-desktop-deps--required "lispm-desktop-deps" ())

(defun lispm-desktop-assistant--package-operation (original &rest args)
  "Capture messages and update progress during package operation ORIGINAL."
  (when (and (bound-and-true-p lispm-desktop-setup--in-progress)
             (not (lispm-desktop-assistant--visible-p)))
    (lispm-desktop-assistant-ensure-progress lispm-desktop-assistant--setup-stage))
  (when (and (lispm-desktop-assistant--visible-p)
             (package-desc-p (car args))
             lispm-desktop-assistant--last-page)
    (let* ((desc (car args))
           (name (package-desc-name desc))
           (counts (or lispm-desktop-assistant--package-count
                       (setq lispm-desktop-assistant--package-count
                             (cons 0 (max 1 (length (ignore-errors
                                                      (when (fboundp 'lispm-desktop-deps--required)
                                                        (package-compute-transaction
                                                         nil (lispm-desktop-deps--required))))))))))
           (installed (1+ (car counts)))
           (total (max installed (cdr counts)))
           (page (copy-sequence lispm-desktop-assistant--last-page)))
      (setcar counts installed)
      (setcdr counts total)
      (setq page (plist-put page :subprogress (* 0.9 (/ (float installed) (float total))))
            page (plist-put page :status
                            (if (> total 1)
                                (lispm-desktop-assistant--tr-status
                                 "Installing package: %s (%d / %d)…" name installed total)
                              (lispm-desktop-assistant--tr-status
                               "Installing package: %s…" name))))
      (lispm-desktop-assistant--show page nil nil)))
  (if (or (not (lispm-desktop-assistant--visible-p))
          (> lispm-desktop-assistant--package-depth 0))
      (apply original args)
    (let ((lispm-desktop-assistant--package-depth (1+ lispm-desktop-assistant--package-depth)))
      (advice-add 'message :around #'lispm-desktop-assistant--package-message)
      (unwind-protect (apply original args)
        (advice-remove 'message #'lispm-desktop-assistant--package-message)))))

(defun lispm-desktop-assistant-close ()
  "Put the assistant's page away, if it is shown."
  (when (window-live-p lispm-desktop-assistant--window)
    (internal-show-cursor lispm-desktop-assistant--window t))
  (when (window-live-p (selected-window))
    (internal-show-cursor (selected-window) t))
  (when-let* ((focus-buf (get-buffer " *lispm-focus-backdrop*")))
    (kill-buffer focus-buf))
  (when-let* ((confirm-buf (get-buffer " *lispm-confirm-backdrop*")))
    (kill-buffer confirm-buf))
  (when-let* ((buffer (get-buffer lispm-desktop-assistant-buffer-name)))
    (when (and (window-live-p lispm-desktop-assistant--window)
               (eq (window-buffer lispm-desktop-assistant--window) buffer)
               (fboundp 'lispm-desktop-empty-buffer))
      (ignore-errors
        (set-window-buffer lispm-desktop-assistant--window
                           (lispm-desktop-empty-buffer))))
    (kill-buffer buffer))
  (let ((message-log-max nil))
    (message nil))
  (lispm-desktop-assistant--restore-frame-colors)
  (when (fboundp 'lispm-ui-palette-update)
    (lispm-ui-palette-update))
  (setq lispm-desktop-assistant--window nil
        lispm-desktop-assistant--shown nil
        lispm-desktop-assistant--last-page nil))


(provide 'lispm-desktop-assistant)
;;; lispm-desktop-assistant.el ends here
