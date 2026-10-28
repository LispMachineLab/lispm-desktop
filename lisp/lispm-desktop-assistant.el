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
     :title ,(lispm-desktop-tr-noop "Which appearance?")
     :detail ,(lispm-desktop-tr-noop "Control Panels, on the desktop, changes this later too.")
     :icon "appearance-32"
     :choices ((dracula :key ?d :label "Dracula" :text ,(lispm-desktop-tr-noop "Light text on a dark background.")
                        :preview dracula)
               (platinum :key ?p :label "Platinum" :text ,(lispm-desktop-tr-noop "Mac OS 9's greys, black on white.")
                         :preview platinum))))
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
    (platinum :code ("#3163FF" "#427B7B" "#316331" "#CE3131" "#000000")))
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
               (cy (+ y top lispm-desktop-platinum-title-bar-height))
               (cw (- w left right))
               (ch (- h top bottom lispm-desktop-platinum-title-bar-height))
               (margin 20)
               (count (length (plist-get page :choices)))
               (columns (if (> count 3) 2 count))
               (rows (if (> columns 0) (/ (+ count columns -1) columns) 0))
               (card-width (if (> count 0) (/ (- cw (* 2 margin) (* 16 (1- columns))) columns) 0))
               (card-height (if (> rows 1)
                                (/ (- lispm-desktop-assistant-card-height (* 6 (1- rows))) rows)
                              lispm-desktop-assistant-card-height))
               ;; The pixels left over widen the gaps, so both margins match.
               (gap (if (> columns 1) (/ (- cw (* 2 margin) (* columns card-width)) (1- columns)) 0))
               (back-width (lispm-desktop-platinum-push-button-width (lispm-desktop-tr "Go Back"))))
    (list :window (list x y w h)
          :content (list cx cy cw ch)
          :cards (mapcar (lambda (index)
                           (list (+ cx margin (* (% index columns) (+ card-width gap)))
                                 (+ cy 92 (* (/ index columns) (+ card-height 6)))
                                 card-width card-height))
                         (number-sequence 0 (1- count)))
          :back (and (plist-get page :back)
                     (list (- (+ cx cw) margin back-width)
                           (- (+ cy ch) 14 lispm-desktop-platinum-button-height)
                           back-width lispm-desktop-platinum-button-height)))))

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
  (let ((outline (plist-get palette :outline))
        (grey (lambda (level) (lispm-ui-grey palette level))))
    (cl-flet ((fill (left top w h color) (lispm-ui-svg-fill svg left top w h color)))
      ;; The screen, outlined.
      (fill (1- x) (1- y) (+ width 2) (+ height 2) outline)
      (fill x y width height (plist-get palette :desktop))
      ;; The menu bar.
      (fill x y width 6 (plist-get palette :chrome))
      (fill x (+ y 6) width 1 outline)
      ;; A window, casting its shadow: a striped title bar over the document.
      (let ((left (+ x 12)) (top (+ y 13)) (w (- width 30)) (h (- height 19)))
        (fill (1+ left) (1+ top) w h outline)
        (fill left top w h outline)
        (fill (1+ left) (1+ top) (- w 2) 7 (funcall grey #xCC))
        (dolist (row '(2 4 6))
          (fill (+ left 4) (+ top row) (- w 8) 1 (funcall grey #xFF)))
        (fill (1+ left) (+ top 8) (- w 2) 1 outline)
        (fill (1+ left) (+ top 9) (- w 2) (- h 10) (plist-get palette :content))
        ;; The document: a definition and its body, a comment, more code.
        (seq-do-indexed
         (pcase-lambda (`(,indent ,length ,color) index)
           (when-let* ((ink (nth color code)))
             (fill (+ left 5 indent) (+ top 13 (* 5 index)) (min length (- w 10 indent)) 2 ink)))
         '((0 14 0) (0 28 1) (6 40 2) (6 22 4) (0 46 3) (6 30 0)))))))

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
`lispm-desktop-platinum-draw-bevel-button' takes it, ringed when
FOCUSED.  It holds the picture of CHOICE's appearance, if any, its label
and its text, and its key as a key cap in its top right corner; a button
too short for that, one of a column, holds its label and text on one
line, the key cap at its right."
  (pcase-let* ((`(,x ,y ,w ,h) rect)
               (properties (cdr choice))
               (ink (plist-get (lispm-desktop-platinum-bevel-button-colors palette state) :label))
               (row (< h 60))
               (cap 20))
    (when focused
      (lispm-desktop-platinum-svg-focus-ring svg x y w h 1 palette))
    (lispm-ui-svg-embed
     svg (lispm-desktop-platinum-draw-bevel-button palette w h :state state) x y w h)
    (lispm-ui-svg-embed
     svg (lispm-desktop-platinum-draw-bevel-button
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
               (lines (lispm-desktop-popup--wrap (or (plist-get properties :text) "") (- w 32)
                                                 'lispm-ui-list))
               (label (propertize (plist-get properties :label) 'face '(:height 1.25)))
               (top (+ y (/ (- h (+ (if preview 74 0) 24 (* 16 (length lines)))) 2))))
    (when preview
      (let ((picture (if-let* ((colors (plist-get preview :colors)))
                         (apply #'lispm-ui-derive-palette colors)
                       palette)))
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
      (let ((bar (lispm-desktop-platinum-title-layout
                  cw (lispm-ui-text-width title 'lispm-ui-title)
                  '(nil nil nil))))
        (lispm-ui-svg-embed
         svg (lispm-desktop-platinum-draw-title-part
              palette cw :active t :ridges (plist-get bar :ridges)
              :label (lispm-desktop-platinum-fit title (plist-get bar :title))
              :label-x (plist-get bar :label-x))
         cx (1+ y) cw lispm-desktop-platinum-title-bar-height))
      (lispm-ui-svg-fill svg cx cy cw ch (lispm-ui-grey palette #xDD))
      ;; The icon, the question and what it is about.
      (if (plist-get page :picture)
          (svg-embed svg (lispm-desktop-platinum-icon-file 'lambda)
                     "image/png" nil :x (+ cx 20) :y (+ cy 18) :width 32 :height 32)
        (lispm-ui-svg-icon
         svg (lispm-desktop-platinum-icon-file (plist-get page :icon))
         (+ cx 20) (+ cy 18) 32))
      (let ((heading (propertize (plist-get page :title) 'face '(:height 1.4)))
            (top (+ cy 42)))
        (lispm-ui-svg-content svg heading (+ cx 68) (+ cy 14) 26 text
                                              'lispm-ui-title)
        (dolist (line (lispm-desktop-popup--wrap (plist-get page :detail) (- cw 88)
                                                 'lispm-ui-list))
          (lispm-ui-svg-content svg line (+ cx 68) top 16 text 'lispm-ui-list)
          (setq top (+ top 16))))
      (if (plist-get page :picture)
          ;; The λ and the status under it, in the middle of what is left.
          (let* ((size 96)
                 (top (+ cy 70 (/ (- ch 70 size 12 16) 2))))
            (svg-embed svg (lispm-desktop-platinum-icon-file 'lambda)
                       "image/png" nil :x (+ cx (/ (- cw size) 2)) :y top :width size :height size)
            (when-let* ((status (plist-get page :status)))
              (lispm-ui-svg-content
               svg status (+ cx (/ (- cw (lispm-ui-text-width status 'lispm-ui-list)) 2))
               (+ top size 12) 16 text 'lispm-ui-list)))
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
         svg (plist-get page :step) (+ cx 20) (- (+ cy ch) 14 lispm-desktop-platinum-button-height)
         lispm-desktop-platinum-button-height text 'lispm-ui-list)
        (pcase (plist-get layout :back)
          (`(,bx ,by ,bw ,_)
           (lispm-desktop-platinum-svg-push-button svg bx by palette (lispm-desktop-tr "Go Back") bw
                                                     :state (and (eq pressed 'back) 'pressed))))))
    svg))

;;;; The page

(defun lispm-desktop-assistant-available-p ()
  "Return non-nil when the page can be drawn: an interactive Emacs showing SVG."
  (and (not noninteractive) (display-graphic-p) (image-type-available-p 'svg)))

(defun lispm-desktop-assistant--open ()
  "Fill the selected frame with the assistant's page, drawn in Platinum.
The Platinum theme is enabled for it; setting LispM up afterwards loads
the theme chosen."
  (add-to-list 'custom-theme-load-path (expand-file-name "themes/" lispm-desktop-directory))
  (with-demoted-errors "LispM's Setup Assistant could not load Platinum: %S"
    (load-theme 'platinum t))
  (lispm-desktop-platinum-apply-faces)
  (let ((buffer (get-buffer-create lispm-desktop-assistant-buffer-name)))
    ;; What the page leaves bare, the frame's border, the column kept for
    ;; the sign of a truncated line and the echo area's fringes, in the
    ;; desktop's colour too.  The theme loaded once LispM is set up
    ;; colours the frame anew.
    (let ((desktop (plist-get lispm-ui-current-palette :desktop)))
      (set-frame-parameter nil 'background-color desktop)
      (set-face-background 'fringe desktop (selected-frame)))
    (with-current-buffer buffer
      ;; One image fills the window, edge to edge: nothing about it.  The
      ;; window is never scrolled sideways to show point after it.
      (setq-local auto-hscroll-mode nil
                  hscroll-margin 0)
      (setq-local mode-line-format nil
                  header-line-format nil
                  tab-line-format nil
                  cursor-type nil
                  cursor-in-non-selected-windows nil
                  truncate-lines t
                  line-spacing nil
                  left-fringe-width 0
                  right-fringe-width 0
                  left-margin-width 0
                  right-margin-width 0
                  vertical-scroll-bar nil
                  indicate-empty-lines nil
                  show-trailing-whitespace nil
                  scroll-margin 0))
    (delete-other-windows)
    (set-window-buffer (selected-window) buffer)
    (setq lispm-desktop-assistant--window (selected-window)
          lispm-desktop-assistant--shown nil)))

(defun lispm-desktop-assistant--show (page focused pressed)
  "Show PAGE filling the assistant's window, FOCUSED ringed, PRESSED held.
See `lispm-desktop-assistant-draw'.  Return PAGE's layout.  Nothing is
drawn again while the page and the window's size stay as they are."
  (let* ((window lispm-desktop-assistant--window)
         ;; Without a right fringe, a line's last column is kept for the
         ;; sign of a truncated line: the image leaves it.
         (width (- (window-body-width window t) (frame-char-width (window-frame window))))
         (height (window-body-height window t))
         (shown (list width height page focused pressed lispm-ui-palette-generation)))
    (setq lispm-desktop-assistant--last-page page)
    (unless (equal shown lispm-desktop-assistant--shown)
      (setq lispm-desktop-assistant--shown shown)
      (with-current-buffer (window-buffer window)
        (erase-buffer)
        (insert (propertize " " 'display
                            (svg-image (lispm-desktop-assistant-draw lispm-ui-current-palette
                                                                     width height page focused pressed)
                                       :ascent 100 :scale 1)))
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
  (when (and (window-live-p lispm-desktop-assistant--window) lispm-desktop-assistant--last-page
             (get-buffer lispm-desktop-assistant-buffer-name))
    (lispm-desktop-assistant--show (plist-put (copy-sequence lispm-desktop-assistant--last-page)
                                              :status status)
                                   nil nil)))

(defun lispm-desktop-assistant-close ()
  "Put the assistant's page away, if it is shown."
  (when-let* ((buffer (get-buffer lispm-desktop-assistant-buffer-name)))
    (kill-buffer buffer))
  (setq lispm-desktop-assistant--window nil
        lispm-desktop-assistant--shown nil
        lispm-desktop-assistant--last-page nil))


(provide 'lispm-desktop-assistant)
;;; lispm-desktop-assistant.el ends here
