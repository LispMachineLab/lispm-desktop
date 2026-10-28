;;; lispm-desktop-app-board.el --- Boards drawn over applications' text  -*- lexical-binding: t; -*-

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

;; Emacs's games draw their boards in characters: dots and hashes, an o
;; for a peg, X and O for stones.  A board here is drawn after that text
;; as one image, in Platinum's forms, the text shrunk out of sight above
;; it, while the game goes on as it was written: its keys move its point
;; and play, it writes its buffer and counts its columns, and after every
;; command the board is read again and drawn anew when it changed.  A
;; click on the image is the game's too, at the cell clicked.
;;
;;   the game's buffer --:cells--> rows of values --:tiles--> one SVG
;;   a click on the SVG --cell--> :click, the game's own commands
;;
;; A board is a grid of cells, each value drawn as a tile:
;;
;;   well     an empty cell, sunken into the board
;;   block    a raised square in a colour: a filled cell, a piece
;;   peg      a round peg; hole, the hole a peg stands in
;;   point    a point where grid lines cross; stone, a stone on one
;;   ball     a ball in a colour
;;   letter   a plate bearing a letter or a digit
;;   key      a key to press, bearing its label
;;   blank    nothing: the board's ground
;;
;; What is no grid, as the calendar's months, a board draws by a function
;; of its own (:draw), and says what a click on it does.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-i18n)

(defvar-local lispm-desktop-app-board nil
  "The board drawn over this buffer, a plist; see `lispm-desktop-app-board-enable'.")

(defvar-local lispm-desktop-app-board--overlay nil
  "The overlay showing the board over this buffer's text.")

(defvar-local lispm-desktop-app-board--drawn nil
  "What the board was last drawn from, so it is drawn again only when it changed.")

(defvar-local lispm-desktop-app-board--hit nil
  "Function of a click's X and Y on the board returning what to call there.")

(defvar-local lispm-desktop-app-board--size nil
  "Width and height of the board's image, (WIDTH . HEIGHT) in pixels.")

(defvar-local lispm-desktop-app-board--pending nil
  "Timer drawing the board again after its buffer changed outside a command.")

(defvar lispm-desktop-app-board-resized-functions nil
  "Functions called with a buffer whose board was drawn at another size:
the window showing it may fit it again.")

(defconst lispm-desktop-app-board-margin 14
  "Pixels about a board, between it and its window's edges.")

(defconst lispm-desktop-app-board-cell 32
  "Side in pixels of a board's cell, unless the board says otherwise.")

;;;; Colours

(defun lispm-desktop-app-board--lighter (color ratio)
  "Return COLOR RATIO of the way to white."
  (lispm-ui-blend color "#ffffff" ratio))

(defun lispm-desktop-app-board--darker (color ratio)
  "Return COLOR RATIO of the way to black."
  (lispm-ui-blend color "#000000" ratio))

(defun lispm-desktop-app-board--grey (palette level)
  "Return PALETTE's grey standing where Platinum's LEVEL stands."
  (lispm-ui-grey palette level))

(defun lispm-desktop-app-board--gradient (svg from to)
  "Return the name of a gradient on SVG from FROM at the top left to TO at
the bottom right, as Platinum shades its spheres; defined once per SVG."
  (let ((id (format "g%s%s" (string-remove-prefix "#" from) (string-remove-prefix "#" to))))
    (unless (dom-by-id svg (concat "\\`" id "\\'"))
      (let ((defs (or (car (dom-by-tag svg 'defs))
                      (let ((defs (dom-node 'defs)))
                        (dom-add-child-before svg defs)
                        defs))))
        (dom-append-child defs (dom-node 'linearGradient
                                         `((id . ,id) (x1 . 0) (y1 . 0) (x2 . 1) (y2 . 1))
                                         (dom-node 'stop `((offset . "0%") (stop-color . ,from)))
                                         (dom-node 'stop `((offset . "100%") (stop-color . ,to)))))))
    (format "url(#%s)" id)))

;;;; Tiles

(defun lispm-desktop-app-board--fill (svg x y width height color)
  "Fill the rectangle of WIDTH by HEIGHT at X, Y of SVG with COLOR."
  (lispm-ui-svg-fill svg x y width height color))

(defun lispm-desktop-app-board-draw-bevel (svg x y width height color &optional sunken)
  "Draw on SVG a square WIDTH by HEIGHT at X, Y in COLOR, raised as a
Platinum bevel button is, outlined, lit along its top and left and shaded
along its bottom and right; SUNKEN lights and shades it the other way."
  (let ((lit (lispm-desktop-app-board--lighter color 0.55))
        (shade (lispm-desktop-app-board--darker color 0.3))
        (edge (lispm-desktop-app-board--darker color 0.6)))
    (lispm-desktop-app-board--fill svg x y width height edge)
    (lispm-desktop-app-board--fill svg (1+ x) (1+ y) (- width 2) (- height 2) color)
    (let ((top (if sunken shade lit))
          (bottom (if sunken lit shade)))
      (lispm-desktop-app-board--fill svg (1+ x) (1+ y) (- width 3) 2 top)
      (lispm-desktop-app-board--fill svg (1+ x) (1+ y) 2 (- height 3) top)
      (lispm-desktop-app-board--fill svg (1+ x) (- (+ y height) 3) (- width 2) 2 bottom)
      (lispm-desktop-app-board--fill svg (- (+ x width) 3) (1+ y) 2 (- height 2) bottom))))

(defun lispm-desktop-app-board-draw-well (svg x y width height ground)
  "Draw on SVG a well WIDTH by HEIGHT at X, Y sunk into GROUND: shaded
along its top and left, lit along its bottom and right."
  (let ((inside (lispm-desktop-app-board--darker ground 0.06)))
    (lispm-desktop-app-board--fill svg (1+ x) (1+ y) (- width 2) (- height 2) inside)
    (lispm-desktop-app-board--fill svg (1+ x) (1+ y) (- width 2) 1 (lispm-desktop-app-board--darker ground 0.35))
    (lispm-desktop-app-board--fill svg (1+ x) (1+ y) 1 (- height 2) (lispm-desktop-app-board--darker ground 0.35))
    (lispm-desktop-app-board--fill svg (1+ x) (- (+ y height) 2) (- width 2) 1 (lispm-desktop-app-board--lighter ground 0.7))
    (lispm-desktop-app-board--fill svg (- (+ x width) 2) (1+ y) 1 (- height 2) (lispm-desktop-app-board--lighter ground 0.7))))

(defun lispm-desktop-app-board-draw-ball (svg cx cy radius color)
  "Draw on SVG a ball of RADIUS centred at CX, CY in COLOR, shaded as a
Platinum sphere from light at its top left to dark at its bottom right."
  (svg-circle svg cx cy radius
              :fill (lispm-desktop-app-board--gradient svg (lispm-desktop-app-board--lighter color 0.6)
                                                       (lispm-desktop-app-board--darker color 0.35))
              :stroke (lispm-desktop-app-board--darker color 0.65) :stroke-width 1))

(defun lispm-desktop-app-board-draw-text (svg text x y size color &optional bold)
  "Write TEXT on SVG centred at X, its middle at Y, SIZE pixels high in COLOR.
BOLD sets it bold."
  (let ((font (lispm-ui-svg-font 'lispm-ui-list)))
    (svg-text svg text :x x :y (+ y (* 0.36 size)) :text-anchor "middle"
              :font-family (plist-get font :family) :font-size size
              :font-weight (if bold "bold" "normal") :fill color)))

(defun lispm-desktop-app-board--ring (svg x y size palette)
  "Ring the cell SIZE pixels square at X, Y in PALETTE's focus colour, as
Mac OS 9 ringed what the keyboard was on."
  (svg-rectangle svg (+ x 1) (+ y 1) (- size 2) (- size 2) :fill "none"
                 :stroke (plist-get palette :focus) :stroke-width 3 :rx 3))

(defun lispm-desktop-app-board-draw-tile (svg tile x y size palette ground &optional where)
  "Draw TILE on SVG in the cell SIZE pixels square at X, Y.
TILE is a plist: :shape, see the Commentary, :color, :text, and :mark,
`dot' for a small dot over it, `cross' for a cross, `ring' for a ring
round it.  PALETTE gives the greys, GROUND is the board's colour.
WHERE, (ROW COLUMN ROWS COLUMNS), places a point among the grid lines."
  (let* ((shape (plist-get tile :shape))
         (color (or (plist-get tile :color) (plist-get palette :accent)))
         (text (plist-get tile :text))
         (middle (/ size 2.0))
         (cx (+ x middle))
         (cy (+ y middle)))
    (pcase shape
      ('well (lispm-desktop-app-board-draw-well svg x y size size ground))
      ('block (lispm-desktop-app-board-draw-bevel svg (+ x 1) (+ y 1) (- size 2) (- size 2) color))
      ('hole
       (svg-circle svg cx cy (* size 0.16) :fill (lispm-desktop-app-board--darker ground 0.45)
                   :stroke (lispm-desktop-app-board--lighter ground 0.6) :stroke-width 1))
      ('peg (lispm-desktop-app-board-draw-ball svg cx cy (* size 0.34) color))
      ('ball (lispm-desktop-app-board-draw-ball svg cx cy (- middle 3) color))
      ((or 'point 'stone)
       (pcase-let ((`(,row ,column ,rows ,columns) (or where '(1 1 3 3)))
                   (line (lispm-desktop-app-board--darker ground 0.55)))
         (lispm-desktop-app-board--fill svg (if (= column 0) cx x) cy
                                        (if (memq column (list 0 (1- columns))) middle size) 1 line)
         (lispm-desktop-app-board--fill svg cx (if (= row 0) cy y)
                                        1 (if (memq row (list 0 (1- rows))) middle size) line))
       (when (eq shape 'stone)
         (lispm-desktop-app-board-draw-ball svg cx cy (- middle 2) color)))
      ('letter
       (lispm-desktop-app-board-draw-bevel svg (+ x 2) (+ y 2) (- size 4) (- size 4)
                                           (lispm-desktop-app-board--grey palette #xEE))
       (lispm-desktop-app-board-draw-text svg text cx cy (* size 0.5)
                                          (or (plist-get tile :color) (plist-get palette :text)) t))
      ('key
       (lispm-desktop-app-board-draw-bevel svg (+ x 2) (+ y 2) (- size 4) (- size 4)
                                           (or (plist-get tile :color) (lispm-desktop-app-board--grey palette #xDD)))
       (lispm-desktop-app-board-draw-text svg text cx cy (* size (if (> (length text) 2) 0.28 0.42))
                                          (plist-get palette :text)))
      ('text (lispm-desktop-app-board-draw-text svg text cx cy (* size 0.5)
                                                (or (plist-get tile :color) (plist-get palette :text)) t))
      (_ nil))
    (pcase (plist-get tile :mark)
      ('dot (svg-circle svg cx cy (* size 0.12) :fill (lispm-desktop-app-board--darker color 0.7)))
      ('cross
       (let ((d (* size 0.28)))
         (svg-line svg (- cx d) (- cy d) (+ cx d) (+ cy d) :stroke "#cc0000" :stroke-width 3)
         (svg-line svg (- cx d) (+ cy d) (+ cx d) (- cy d) :stroke "#cc0000" :stroke-width 3)))
      ('ring (svg-circle svg cx cy (- middle 1.5) :fill "none" :stroke "#cc0000" :stroke-width 2)))))

;;;; Grids

(defun lispm-desktop-app-board--tile (board value)
  "Return the tile BOARD draws VALUE as."
  (let ((tiles (plist-get board :tiles)))
    (or (if (functionp tiles) (funcall tiles value) (cdr (assoc value tiles)))
        '(:shape blank))))

(defun lispm-desktop-app-board-draw-grid (board palette cells cursor)
  "Return (SVG . HIT) for grid BOARD showing CELLS, in PALETTE.
CELLS is a list of rows, each a list of values; CURSOR, (ROW . COLUMN),
is ringed.  The cells lie in a sunken field on a raised Platinum plate,
`lispm-desktop-app-board-margin' in from the image's edges; rules are
drawn under the rows BOARD's :rules names.  HIT is a function of a
click's X and Y returning a function playing the cell there, or nil."
  (let* ((size (or (plist-get board :size) lispm-desktop-app-board-cell))
         (rows (length cells))
         (columns (seq-reduce #'max (mapcar #'length cells) 0))
         (ground (or (plist-get board :ground) (lispm-desktop-app-board--grey palette #xDD)))
         (margin lispm-desktop-app-board-margin)
         (plate 6)
         (left (+ margin plate))
         (top (+ margin plate))
         (width (+ (* 2 left) (* columns size)))
         (height (+ (* 2 top) (* rows size)))
         (svg (svg-create width height)))
    (lispm-desktop-app-board--fill svg 0 0 width height (plist-get palette :content))
    (lispm-desktop-app-board-draw-bevel svg margin margin (- width (* 2 margin)) (- height (* 2 margin))
                                        (lispm-desktop-app-board--grey palette #xDD))
    (lispm-desktop-app-board-draw-well svg (1- left) (1- top) (+ 2 (* columns size)) (+ 2 (* rows size))
                                       (lispm-desktop-app-board--grey palette #xDD))
    (lispm-desktop-app-board--fill svg left top (* columns size) (* rows size) ground)
    (seq-do-indexed
     (lambda (row r)
       (seq-do-indexed
        (lambda (value c)
          (lispm-desktop-app-board-draw-tile svg (lispm-desktop-app-board--tile board value)
                                             (+ left (* c size)) (+ top (* r size)) size palette ground
                                             (list r c rows columns)))
        row))
     cells)
    (dolist (rule (plist-get board :rules))
      (lispm-desktop-app-board--fill svg (+ left (/ size 4)) (+ top (* (1+ rule) size) -1)
                                     (- (* columns size) (/ size 2)) 2 (plist-get palette :text)))
    (when cursor
      (lispm-desktop-app-board--ring svg (+ left (* (cdr cursor) size)) (+ top (* (car cursor) size))
                                     size palette))
    (cons svg
          (lambda (x y)
            (let ((column (floor (- x left) size))
                  (row (floor (- y top) size)))
              (when (and (< -1 row rows) (< -1 column columns))
                (when-let* ((click (plist-get board :click)))
                  (lambda () (funcall click row column)))))))))

;;;; Showing the board

(defvar-keymap lispm-desktop-app-board-map
  :doc "Keymap of a board: a click plays the cell clicked."
  "<down-mouse-1>" #'ignore
  "<mouse-1>" #'lispm-desktop-app-board-click
  "<double-mouse-1>" #'lispm-desktop-app-board-click
  "<triple-mouse-1>" #'lispm-desktop-app-board-click
  "<drag-mouse-1>" #'ignore)

(defun lispm-desktop-app-board--model ()
  "Return what the board would be drawn from now, or nil while the game
cannot say yet, as before it made its board."
  (let ((board lispm-desktop-app-board))
    (condition-case nil
        (list lispm-ui-palette-generation
              (if-let* ((model (plist-get board :model))) (funcall model) t)
              (when-let* ((cells (plist-get board :cells))) (funcall cells))
              (when-let* ((cursor (plist-get board :cursor))) (funcall cursor)))
      (error nil))))

(defun lispm-desktop-app-board--render (model)
  "Return (SVG . HIT) of this buffer's board drawn from MODEL."
  (let ((palette lispm-ui-current-palette))
    (if-let* ((draw (plist-get lispm-desktop-app-board :draw)))
        (funcall draw palette)
      (lispm-desktop-app-board-draw-grid lispm-desktop-app-board palette (nth 2 model) (nth 3 model)))))

(defun lispm-desktop-app-board--region ()
  "Return the text the board is drawn over, (BEG . END)."
  (if-let* ((region (plist-get lispm-desktop-app-board :region)))
      (funcall region)
    (cons (point-min) (point-max))))

(defun lispm-desktop-app-board--image (svg)
  "Return the image of SVG, never scaled."
  (if (image-type-available-p 'svg)
      (svg-image svg :ascent 'center :scale 1)
    (list 'image :type 'svg :data (with-temp-buffer (svg-print svg) (buffer-string)) :scale 1)))

(defface lispm-desktop-app-board-text '((t :height 10))
  "Face of a game's text over its board: one point high, too small to be
seen; a height relative to the text's would be smaller than any font has,
and the text would stay as large as it was.
The text stays as the game wrote it, whose commands count its columns
and lines; an image displayed instead of it would change its columns."
  :group 'lispm-desktop-platinum)

(defun lispm-desktop-app-board--place (image)
  "Show IMAGE after the board's text, centred in its window, and the text
shrunk out of sight over it; the overlay is made again where the game
removed it, and moved over its text when it was written anew."
  (pcase-let ((`(,beg . ,end) (lispm-desktop-app-board--region)))
    (unless (and lispm-desktop-app-board--overlay (overlay-buffer lispm-desktop-app-board--overlay))
      (setq lispm-desktop-app-board--overlay (make-overlay beg end nil nil t))
      (overlay-put lispm-desktop-app-board--overlay 'lispm-desktop-app-board t)
      (overlay-put lispm-desktop-app-board--overlay 'keymap lispm-desktop-app-board-map)
      (overlay-put lispm-desktop-app-board--overlay 'priority 100))
    (let ((overlay lispm-desktop-app-board--overlay))
      (unless (and (= (overlay-start overlay) beg) (= (overlay-end overlay) end))
        (move-overlay overlay beg end))
      (overlay-put overlay 'face `(lispm-desktop-app-board-text
                                   (:foreground ,(face-background 'default nil t))))
      ;; The board comes after the text, so the line point is on is
      ;; always in sight above it: were it below a board taller than the
      ;; window, Emacs would scroll at every redisplay.
      (when image
        (overlay-put overlay 'after-string
                     (concat (propertize " " 'display `(space :align-to (- center (0.5 . ,image))))
                             (propertize " " 'display image 'keymap lispm-desktop-app-board-map)))))))

(defun lispm-desktop-app-board-refresh (&optional force)
  "Draw this buffer's board again when what it shows changed, or FORCE."
  (when lispm-desktop-app-board
    (setq lispm-desktop-app-board--pending nil)
    (let ((model (lispm-desktop-app-board--model)))
      (cond
       ((null model))
       ((and (not force) (equal model lispm-desktop-app-board--drawn))
        (lispm-desktop-app-board--place nil))
       ;; A board that cannot be drawn leaves the game's text in sight.
       ((let ((drawn (condition-case err
                         (lispm-desktop-app-board--render model)
                       (error (message (lispm-desktop-tr "The board cannot be drawn: %s")
                                       (error-message-string err))
                              nil))))
          (when drawn
            (setq lispm-desktop-app-board--drawn model)
            (pcase-let* ((`(,svg . ,hit) drawn)
                         (image (lispm-desktop-app-board--image svg))
                         (size (cons (dom-attr svg 'width) (dom-attr svg 'height))))
              (setq lispm-desktop-app-board--hit hit)
              (lispm-desktop-app-board--place image)
              (unless (equal size lispm-desktop-app-board--size)
                (setq lispm-desktop-app-board--size size)
                (run-hook-with-args 'lispm-desktop-app-board-resized-functions (current-buffer)))))))))))

(defun lispm-desktop-app-board--changed (&rest _)
  "Draw the board again soon, its buffer having changed, and once more a
moment later: a game also writes its buffer from timers and as it
starts, outside any command, and may keep what it wrote in variables of
its own only after writing it, as Gomoku its stones.
This is an `after-change-functions' function."
  (unless lispm-desktop-app-board--pending
    (let ((buffer (current-buffer)))
      (cl-flet ((refresh ()
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (lispm-desktop-app-board-refresh)))))
        (setq lispm-desktop-app-board--pending
              (run-at-time 0 nil (lambda ()
                                   (refresh)
                                   (run-at-time 0.3 nil #'refresh))))))))

(defun lispm-desktop-app-board-click (event)
  "Play the cell of the board EVENT clicked, as the game says a click does."
  (interactive "e")
  (let* ((position (event-start event))
         (window (posn-window position))
         (xy (posn-object-x-y position)))
    (when (and (windowp window) xy)
      (select-window window)
      (with-current-buffer (window-buffer window)
        (when-let* ((hit lispm-desktop-app-board--hit)
                    (play (funcall hit (car xy) (cdr xy))))
          (funcall play))
        (lispm-desktop-app-board-refresh)))))

(defun lispm-desktop-app-board-size (&optional buffer)
  "Return the size of BUFFER's board as last drawn, (WIDTH . HEIGHT) in
pixels, or nil without one."
  (buffer-local-value 'lispm-desktop-app-board--size (or buffer (current-buffer))))

(defun lispm-desktop-app-board-enable (board)
  "Draw BOARD over the current buffer's text, and again after each command.
BOARD is a plist:

:cells   a function returning the board's rows, each a list of values,
         called in the buffer
:tiles   an alist of (VALUE . TILE), or a function of a value returning
         its tile, see `lispm-desktop-app-board-draw-tile'
:cursor  a function returning the cell the game is on, (ROW . COLUMN),
         ringed as the keyboard's place, or nil
:click   a function of a clicked cell's ROW and COLUMN, called in the
         buffer, its window selected
:size    a cell's side in pixels, `lispm-desktop-app-board-cell' by default
:ground  the colour of the field the cells lie in
:rules   the rows under which a rule is drawn, as under a sum
:draw    instead of :cells, a function of the palette returning (SVG
         . HIT), HIT a function of a click's X and Y returning what to
         call for it, or nil
:model   with :draw, a function returning what the drawing shows: it
         is drawn again when that changes
:region  a function returning (BEG . END), the text the board is drawn
         over; the whole buffer by default.

The cursor is hidden; point stays where the game puts it, in its text
shrunk out of sight over the board."
  (setq-local lispm-desktop-app-board board
              lispm-desktop-app-board--drawn nil
              line-spacing 0
              cursor-type nil
              cursor-in-non-selected-windows nil
              truncate-lines t)
  (add-hook 'post-command-hook #'lispm-desktop-app-board-refresh nil t)
  (add-hook 'after-change-functions #'lispm-desktop-app-board--changed nil t)
  (lispm-desktop-app-board-refresh t))

(defun lispm-desktop-app-board-disable ()
  "Show the current buffer's text again instead of its board."
  (remove-hook 'post-command-hook #'lispm-desktop-app-board-refresh t)
  (remove-hook 'after-change-functions #'lispm-desktop-app-board--changed t)
  (when lispm-desktop-app-board--overlay
    (delete-overlay lispm-desktop-app-board--overlay))
  (kill-local-variable 'lispm-desktop-app-board)
  (kill-local-variable 'line-spacing)
  (kill-local-variable 'cursor-type))

;;;; Reading boards from text

(defun lispm-desktop-app-board-position (line column)
  "Return the position of COLUMN on LINE, both counted from 0 at the
buffer's start, or nil past the buffer's end or the line's."
  (save-excursion
    (goto-char (point-min))
    (when (zerop (forward-line line))
      (let ((target (+ (point) column)))
        (move-to-column column)
        (and (= (current-column) column) (<= target (line-end-position)) (point))))))

(defun lispm-desktop-app-board-char (line column)
  "Return the character at COLUMN on LINE, counted from 0, or nil."
  (when-let* ((position (lispm-desktop-app-board-position line column)))
    (char-after position)))

(defun lispm-desktop-app-board-line-column (&optional position)
  "Return the line and column of POSITION, by default point's, both
counted from 0: (LINE . COLUMN)."
  (save-excursion
    (when position (goto-char position))
    (cons (1- (line-number-at-pos)) (current-column))))

(provide 'lispm-desktop-app-board)
;;; lispm-desktop-app-board.el ends here
