;;; lispm-desktop-app-games.el --- Emacs's games on the desktop  -*- lexical-binding: t; -*-

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

;; What is said of Emacs's games, as the App Store installs them, beyond
;; what lispm-desktop-app-ui.el finds out of them by itself:
;;
;;   Tetris, Snake, Pong  gamegrid's games: their cells drawn as Platinum
;;                        tiles, their scores on the window's bottom, a
;;                        game over said in the strip, not in a score
;;                        file thrown into the window
;;   5x5, Gomoku, Solitaire, Blackbox, Mpuz, Bubbles, Life
;;                        boards drawn over their characters
;;                        (lispm-desktop-app-board.el), played with the
;;                        mouse as with their keys
;;   Hanoi, Life          animated without holding Emacs, or once their
;;                        windows are up
;;   Dunnet, Doctor       read as prose, the adventure's common words on
;;                        buttons
;;
;; Every game keeps its own logic and keys; only how it looks and how it
;; is started change.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-app-board)
(require 'lispm-desktop-app-ui)
(require 'lispm-desktop-i18n)

;; The games' own variables and functions, read once a game runs.
(defvar gamegrid-display-table)
(defvar gamegrid-display-mode)
(defvar tetris-buffer-height)
(defvar tetris-score)
(defvar tetris-n-rows)
(defvar tetris-n-shapes)
(defvar tetris-paused)
(defvar tetris-mode-map)
(defvar snake-buffer-height)
(defvar snake-score)
(defvar snake-length)
(defvar snake-paused)
(defvar snake-mode-map)
(defvar pong-height)
(defvar pong-score-player1)
(defvar pong-score-player2)
(defvar 5x5-grid)
(defvar 5x5-grid-size)
(defvar 5x5-x-pos)
(defvar 5x5-y-pos)
(defvar 5x5-moves)
(defvar 5x5-solver-output)
(defvar gomoku-board)
(defvar gomoku-board-width)
(defvar gomoku-board-height)
(defvar gomoku-game-history)
(defvar gomoku-game-in-progress)
(defvar gomoku-emacs-is-computing)
(defvar gomoku-number-of-human-wins)
(defvar gomoku-number-of-emacs-wins)
(defvar gomoku-number-of-draws)
(defvar solitaire-start-x)
(defvar solitaire-start-y)
(defvar solitaire-center)
(defvar solitaire-stones)
(defvar bb-board)
(defvar bb-balls-placed)
(defvar bb-x)
(defvar bb-y)
(defvar bb-score)
(defvar mpuz-board)
(defvar mpuz-digit-to-letter)
(defvar mpuz-found-digits)
(defvar mpuz-trivial-digits)
(defvar mpuz-nb-errors)
(defvar mpuz-nb-completed-games)
(defvar bubbles--score)
(defvar bubbles--neighborhood-score)
(defvar life--max-width)
(defvar life--max-height)
(defvar life-generation-string)
(defvar hanoi-use-faces)
(defvar hanoi-move-period)
(declare-function gamegrid-get-cell "gamegrid" (x y))
(declare-function gamegrid-set-cell "gamegrid" (x y c))
(declare-function 5x5-grid-value "5x5" (grid))
(declare-function 5x5-flip-current "5x5" ())
(declare-function gomoku "gomoku" (&optional n m))
(declare-function gomoku-xy-to-index "gomoku" (x y))
(declare-function gomoku-index-to-x "gomoku" (index))
(declare-function gomoku-index-to-y "gomoku" (index))
(declare-function gomoku-point-x "gomoku" ())
(declare-function gomoku-point-y "gomoku" ())
(declare-function gomoku-goto-xy "gomoku" (x y))
(declare-function gomoku-human-plays "gomoku" ())
(declare-function solitaire-possible-move "solitaire" (movesymbol))
(declare-function bb-goto "blackbox" (pos))
(declare-function bb-romp "blackbox" ())
(declare-function mpuz-try-proposal "mpuz" (letter-char digit-char))
(declare-function bubbles--grid-width "bubbles" ())
(declare-function bubbles--grid-height "bubbles" ())
(declare-function bubbles--goto "bubbles" (row col))
(declare-function bubbles--mark-neighborhood "bubbles" (&optional pos))
(declare-function bubbles-plop "bubbles" ())
(declare-function life-setup "life" ())
(declare-function life--tick "life" ())
(declare-function hanoi "hanoi" (nrings))
(declare-function dun-parse "dunnet" (arg))

(defconst lispm-desktop-app-games-colors
  '(blue "#3d6fd6" purple "#8e4fc6" yellow "#efc11b" magenta "#d24b9a" cyan "#26b0c6"
    green "#36a852" red "#d8402f" orange "#ef8a1a" grey "#a8a8a8" black "#262626" white "#f4f4f4")
  "The colours of the games' pieces, Mac OS 9's bright ones rather than the
pure ones the games name.")

(defun lispm-desktop-app-games-color (name)
  "Return the colour NAME of `lispm-desktop-app-games-colors'."
  (plist-get lispm-desktop-app-games-colors name))

(defun lispm-desktop-app-games--toggle-label (paused)
  "Return the label of a button pausing a game, or resuming it when PAUSED."
  (if paused (lispm-desktop-tr "Resume") (lispm-desktop-tr "Pause")))

;;;; gamegrid: Tetris, Snake and Pong
;;
;; gamegrid draws each cell as one character, shown as an image of its
;; value made once, when the game's buffer is set up, and kept in
;; `gamegrid-display-table'.  The game's :gamegrid says which values are
;; drawn as which tiles, and how many rows its buffer has, so that the
;; cells are as large as the desktop lets them be; its text, the score
;; the game writes beside its board, is hidden, the window's bottom
;; saying it instead.

(defvar-local lispm-desktop-app-games--best nil
  "The best score of the game in this buffer, read from its scores once
it ended, or nil while it is played.")

(defun lispm-desktop-app-games--cell-size (rows columns)
  "Return the side of a gamegrid cell for a board of ROWS rows and COLUMNS
columns: as large as the screen's desktop holds, from 10 to 28 pixels."
  (let* ((screen (lispm-desktop-apps-screen))
         (height (- (frame-inner-height screen) 170))
         (width (- (frame-inner-width screen) 160)))
    (max 10 (min 28 (/ height (max 1 rows)) (/ width (max 1 columns))))))

(defun lispm-desktop-app-games--tile-image (tile size)
  "Return the image of TILE, a cell SIZE pixels square."
  (let* ((palette lispm-desktop-platinum-current-palette)
         (svg (svg-create size size))
         (ground (lispm-desktop-platinum-grey palette #xE6)))
    (lispm-desktop-platinum-svg-fill svg 0 0 size size ground)
    (lispm-desktop-app-board-draw-tile svg tile 0 0 size palette ground)
    (lispm-desktop-app-board--image svg)))

(defun lispm-desktop-app-games--retile ()
  "Draw the current gamegrid buffer's cells as its game's :gamegrid says.
This runs after `gamegrid-initialize-display', in an application's
window only."
  (when-let* (((lispm-desktop-app-ui--buffer-app))
              (spec (lispm-desktop-app-ui-get :gamegrid))
              ((eq gamegrid-display-mode 'glyph)))
    (let* ((rows (plist-get spec :rows))
           (size (lispm-desktop-app-games--cell-size (if (functionp rows) (funcall rows) rows)
                                                     (plist-get spec :columns))))
      (pcase-dolist (`(,value . ,tile) (plist-get spec :tiles))
        (aset gamegrid-display-table value (lispm-desktop-app-games--tile-image tile size)))
      ;; The game's words beside its board: the window's bottom says them.
      (cl-loop for char from ?! to ?~ do (aset gamegrid-display-table char ""))
      (setq-local line-prefix (propertize " " 'display '(space :width (14)))))))

(defun lispm-desktop-app-games--quiet-score (add-score file score &rest arguments)
  "Keep SCORE in FILE as ADD-SCORE does with ARGUMENTS, but out of sight in
an application's window, its best score remembered for the strip to say:
gamegrid shows its scores file in a window of its own, and selects it."
  (if (not (lispm-desktop-app-ui--buffer-app))
      (apply add-score file score arguments)
    (let ((game (current-buffer)))
      (save-window-excursion
        (apply add-score file score arguments))
      (when-let* ((scores (seq-find (lambda (buffer)
                                      (equal (file-name-nondirectory (or (buffer-file-name buffer) ""))
                                             file))
                                    (buffer-list))))
        (let ((best (with-current-buffer scores
                      (goto-char (point-min))
                      (and (looking-at "\\([0-9]+\\)") (string-to-number (match-string 1))))))
          (kill-buffer scores)
          (with-current-buffer game
            (setq lispm-desktop-app-games--best (or best score))))))))

(defun lispm-desktop-app-games--over-label (over)
  "Return what the strip says once a game is OVER: Game Over, and the best
score, or nil while it is played."
  (when over
    (if lispm-desktop-app-games--best
        (lispm-desktop-tr "Game Over — best %s" (lispm-desktop-app-ui--number lispm-desktop-app-games--best))
      (lispm-desktop-tr "Game Over"))))

(defun lispm-desktop-app-games--gamegrid-setup ()
  "Leave room above a gamegrid board, and forget the best score of the game
before, as a new game starts."
  (setq lispm-desktop-app-games--best nil))

(defconst lispm-desktop-app-games--wall '(:shape block :color "#b4b4b4")
  "A wall round a gamegrid board.")

(lispm-desktop-app-ui-define 'tetris-mode
  :gamegrid `(:rows ,(lambda () (if (boundp 'tetris-buffer-height) tetris-buffer-height 22))
              :columns 20
              :tiles ((0 :shape block :color ,(lispm-desktop-app-games-color 'blue))
                      (1 :shape block :color ,(lispm-desktop-app-games-color 'purple))
                      (2 :shape block :color ,(lispm-desktop-app-games-color 'yellow))
                      (3 :shape block :color ,(lispm-desktop-app-games-color 'magenta))
                      (4 :shape block :color ,(lispm-desktop-app-games-color 'cyan))
                      (5 :shape block :color ,(lispm-desktop-app-games-color 'green))
                      (6 :shape block :color ,(lispm-desktop-app-games-color 'red))
                      (7 :shape well)
                      (8 ,@lispm-desktop-app-games--wall)))
  :status (lambda () (list (lispm-desktop-tr "Score %s" (lispm-desktop-app-ui--number tetris-score))
                           (lispm-desktop-tr "Rows %d" tetris-n-rows)
                           (lispm-desktop-tr "Shapes %d" tetris-n-shapes)))
  :buttons `((,(lispm-desktop-tr-noop "New Game") tetris-start-game)
             (,(lambda () (lispm-desktop-app-games--toggle-label tetris-paused)) tetris-pause-game
              ,(lambda () (eq (current-local-map) tetris-mode-map))))
  :label (lambda () (lispm-desktop-app-games--over-label (not (eq (current-local-map) tetris-mode-map))))
  :keys `(("←  →" . ,(lispm-desktop-tr-noop "Move")) ("↑" . ,(lispm-desktop-tr-noop "Rotate"))
          ("↓" . ,(lispm-desktop-tr-noop "Move down")) ("Space" . ,(lispm-desktop-tr-noop "Drop"))
          ("p" . ,(lispm-desktop-tr-noop "Pause or resume")) ("n" . ,(lispm-desktop-tr-noop "New game"))
          ("q" . ,(lispm-desktop-tr-noop "End the game"))))

(lispm-desktop-app-ui-define 'snake-mode
  :gamegrid `(:rows ,(lambda () (if (boundp 'snake-buffer-height) snake-buffer-height 22))
              :columns 30
              :tiles ((0 :shape well)
                      (1 :shape block :color ,(lispm-desktop-app-games-color 'green))
                      (2 :shape ball :color ,(lispm-desktop-app-games-color 'red))
                      (3 ,@lispm-desktop-app-games--wall)))
  :status (lambda () (list (lispm-desktop-tr "Score %s" (lispm-desktop-app-ui--number snake-score))
                           (lispm-desktop-tr "Length %d" snake-length)))
  :buttons `((,(lispm-desktop-tr-noop "New Game") snake-start-game)
             (,(lambda () (lispm-desktop-app-games--toggle-label snake-paused)) snake-pause-game
              ,(lambda () (eq (current-local-map) snake-mode-map))))
  :label (lambda () (lispm-desktop-app-games--over-label (not (eq (current-local-map) snake-mode-map))))
  :keys `(("C-f  →" . ,(lispm-desktop-tr-noop "Right")) ("C-b  ←" . ,(lispm-desktop-tr-noop "Left"))
          ("C-p  ↑" . ,(lispm-desktop-tr-noop "Up")) ("C-n  ↓" . ,(lispm-desktop-tr-noop "Down"))
          ("p" . ,(lispm-desktop-tr-noop "Pause or resume")) ("n" . ,(lispm-desktop-tr-noop "New game"))
          ("q" . ,(lispm-desktop-tr-noop "End the game"))))

(defun lispm-desktop-app-games--pong-paused-p ()
  "Return non-nil while Pong is paused: its pause key then resumes."
  (and (current-local-map) (where-is-internal 'pong-resume (current-local-map) t)))

(lispm-desktop-app-ui-define 'pong
  :gamegrid `(:rows ,(lambda () (+ 2 (if (boundp 'pong-height) pong-height 30)))
              :columns 50
              :tiles ((0 :shape well)
                      (1 :shape block :color ,(lispm-desktop-app-games-color 'blue))
                      (2 :shape ball :color ,(lispm-desktop-app-games-color 'orange))
                      (3 ,@lispm-desktop-app-games--wall)))
  :status (lambda () (list (lispm-desktop-tr "Left %d" pong-score-player1)
                           (lispm-desktop-tr "Right %d" pong-score-player2)))
  :buttons `((,(lambda () (lispm-desktop-app-games--toggle-label (lispm-desktop-app-games--pong-paused-p)))
              ,(lambda () (interactive)
                 (call-interactively (if (lispm-desktop-app-games--pong-paused-p) 'pong-resume 'pong-pause)))))
  :keys `(("C-p  ↑" . ,(lispm-desktop-tr-noop "Left bat up"))
          ("C-n  ↓" . ,(lispm-desktop-tr-noop "Left bat down"))
          ("8" . ,(lispm-desktop-tr-noop "Right bat up")) ("2" . ,(lispm-desktop-tr-noop "Right bat down"))
          ("p" . ,(lispm-desktop-tr-noop "Pause or resume")) ("q" . ,(lispm-desktop-tr-noop "Quit"))))

;;;; 5x5

(defun lispm-desktop-app-games--5x5-hints ()
  "Return the cells to press for the solution 5x5 suggested, a grid, or nil
when it no longer holds."
  (when (and 5x5-solver-output (= (car 5x5-solver-output) 5x5-moves))
    (cdadr 5x5-solver-output)))

(defun lispm-desktop-app-games--5x5-cells ()
  "Return 5x5's board: each cell `on' or `off', `-hint' where the solution
found presses it."
  (let ((hints (lispm-desktop-app-games--5x5-hints)))
    (cl-loop for y below 5x5-grid-size
             collect (cl-loop for x below 5x5-grid-size
                              collect (intern (concat (if (aref (aref 5x5-grid y) x) "on" "off")
                                                      (if (and hints (aref (aref hints y) x)) "-hint" "")))))))

(defun lispm-desktop-app-games--5x5-click (row column)
  "Flip 5x5's cell at ROW and COLUMN, with those beside it."
  (setq 5x5-x-pos column 5x5-y-pos row)
  (5x5-flip-current))

(defun lispm-desktop-app-games--5x5-solved-p ()
  "Return non-nil when every cell of 5x5's board is filled."
  (= (5x5-grid-value 5x5-grid) (* 5x5-grid-size 5x5-grid-size)))

(lispm-desktop-app-ui-define '5x5-mode
  :board `(:cells lispm-desktop-app-games--5x5-cells
           :tiles ((on :shape block :color ,(lispm-desktop-app-games-color 'blue))
                   (off :shape well)
                   (on-hint :shape block :color ,(lispm-desktop-app-games-color 'blue) :mark dot)
                   (off-hint :shape well :mark dot))
           :cursor ,(lambda () (cons 5x5-y-pos 5x5-x-pos))
           :click lispm-desktop-app-games--5x5-click
           :size 56)
  :status (lambda () (list (lispm-desktop-tr "Filled %d of %d" (5x5-grid-value 5x5-grid)
                                             (* 5x5-grid-size 5x5-grid-size))
                           (lispm-desktop-tr "Moves %d" 5x5-moves)))
  :buttons `((,(lispm-desktop-tr-noop "New Game") 5x5-new-game) (,(lispm-desktop-tr-noop "Shuffle") 5x5-randomize)
             (,(lispm-desktop-tr-noop "Hint") 5x5-solve-suggest))
  :label (lambda () (when (lispm-desktop-app-games--5x5-solved-p) (lispm-desktop-tr "Solved!")))
  :keys `(("Space  Return" . ,(lispm-desktop-tr-noop "Flip the square and those beside it"))
          ("C-f C-b C-n C-p" . ,(lispm-desktop-tr-noop "Move")) ("n" . ,(lispm-desktop-tr-noop "New game"))
          ("r" . ,(lispm-desktop-tr-noop "Shuffle")) ("s" . ,(lispm-desktop-tr-noop "Show a solution"))))

;;;; Gomoku

(defun lispm-desktop-app-games-gomoku ()
  "Play Gomoku on a board of 15 by 15, however large its window is: Gomoku
would take the window's size, and a new window has none yet."
  (interactive)
  (require 'gomoku)
  (cl-letf (((symbol-function 'gomoku-max-width) (lambda () 15))
            ((symbol-function 'gomoku-max-height) (lambda () 15)))
    (gomoku 15 15)))

(defun lispm-desktop-app-games--gomoku-cells ()
  "Return Gomoku's board: each point `empty', `x' (yours), `o' (Emacs's),
`x-last' or `o-last' for the last move."
  (let ((last (caar gomoku-game-history)))
    (cl-loop for y from 1 to gomoku-board-height
             collect (cl-loop for x from 1 to gomoku-board-width
                              collect (let* ((index (gomoku-xy-to-index x y))
                                             (stone (pcase (aref gomoku-board index) (1 "x") (6 "o"))))
                                        (if stone
                                            (intern (concat stone (if (eql index last) "-last" "")))
                                          'empty))))))

(defun lispm-desktop-app-games--gomoku-cursor ()
  "Return the point of Gomoku's board point is on."
  (ignore-errors (cons (1- (gomoku-point-y)) (1- (gomoku-point-x)))))

(defun lispm-desktop-app-games--gomoku-click (row column)
  "Play on Gomoku's point at ROW and COLUMN when it is free; else go there."
  (gomoku-goto-xy (1+ column) (1+ row))
  (when (zerop (aref gomoku-board (gomoku-xy-to-index (1+ column) (1+ row))))
    (gomoku-human-plays)))

(lispm-desktop-app-ui-define 'gomoku-mode
  :app 'gomoku
  :run #'lispm-desktop-app-games-gomoku
  :board `(:cells lispm-desktop-app-games--gomoku-cells
           :tiles ((empty :shape point)
                   (x :shape stone :color ,(lispm-desktop-app-games-color 'black))
                   (o :shape stone :color ,(lispm-desktop-app-games-color 'white))
                   (x-last :shape stone :color ,(lispm-desktop-app-games-color 'black) :mark ring)
                   (o-last :shape stone :color ,(lispm-desktop-app-games-color 'white) :mark ring))
           :cursor lispm-desktop-app-games--gomoku-cursor
           :click lispm-desktop-app-games--gomoku-click
           :ground "#dcb468"
           :size 30)
  :status (lambda () (list (cond (gomoku-emacs-is-computing (lispm-desktop-tr "Thinking…"))
                                 (gomoku-game-in-progress (lispm-desktop-tr "Your move"))
                                 (t (lispm-desktop-tr "Game over")))
                           (lispm-desktop-tr "Won %d · Lost %d · Drawn %d" gomoku-number-of-human-wins
                                             gomoku-number-of-emacs-wins gomoku-number-of-draws)))
  :buttons `((,(lispm-desktop-tr-noop "New Game") lispm-desktop-app-games-gomoku)
             (,(lispm-desktop-tr-noop "Take Back") gomoku-human-takes-back)
             (,(lispm-desktop-tr-noop "Resign") gomoku-human-resigns))
  :keys `(("Space  Return" . ,(lispm-desktop-tr-noop "Put a stone here"))
          ("C-f C-b C-n C-p" . ,(lispm-desktop-tr-noop "Move"))
          ("y u b n" . ,(lispm-desktop-tr-noop "Move diagonally"))
          ("C-c C-b" . ,(lispm-desktop-tr-noop "Take back your move"))
          ("C-c C-r" . ,(lispm-desktop-tr-noop "Resign"))))

;;;; Solitaire

(defun lispm-desktop-app-games--solitaire-geometry ()
  "Return where Solitaire's holes are: (LINE COLUMN LINE-STEP COLUMN-STEP),
the first hole's line and column, counted from 0, and the steps to the
next hole down and right, as its window's size laid them out."
  (pcase-let ((`(,line . ,column) (lispm-desktop-app-board-line-column solitaire-center)))
    (list solitaire-start-y solitaire-start-x
          (/ (- line solitaire-start-y) 3) (/ (- column solitaire-start-x) 3))))

(defun lispm-desktop-app-games--solitaire-position (row column)
  "Return the position of Solitaire's hole at ROW and COLUMN, or nil off
the cross."
  (when (or (<= 2 row 4) (<= 2 column 4))
    (pcase-let ((`(,line ,first ,down ,right) (lispm-desktop-app-games--solitaire-geometry)))
      (lispm-desktop-app-board-position (+ line (* row down)) (+ first (* column right))))))

(defun lispm-desktop-app-games--solitaire-cell (position)
  "Return the row and column of Solitaire's hole at POSITION, or nil."
  (pcase-let ((`(,line ,first ,down ,right) (lispm-desktop-app-games--solitaire-geometry))
              (`(,at . ,column) (lispm-desktop-app-board-line-column position)))
    (when (and (> down 0) (> right 0)
               (zerop (% (- at line) down)) (zerop (% (- column first) right)))
      (cons (/ (- at line) down) (/ (- column first) right)))))

(defconst lispm-desktop-app-games--solitaire-moves
  '((solitaire-right . solitaire-move-right) (solitaire-left . solitaire-move-left)
    (solitaire-up . solitaire-move-up) (solitaire-down . solitaire-move-down))
  "Solitaire's directions, and its commands jumping that way.")

(defun lispm-desktop-app-games--solitaire-targets ()
  "Return the holes the peg at point can jump to, as positions."
  (delq nil (mapcar (pcase-lambda (`(,direction . ,_))
                      (let ((move (solitaire-possible-move direction)))
                        (and (consp move) (nth 2 move))))
                    lispm-desktop-app-games--solitaire-moves)))

(defun lispm-desktop-app-games--solitaire-cells ()
  "Return Solitaire's board: `peg', `hole', `target' where the peg at point
can jump, or nil off the cross."
  (let ((targets (and (eq (char-after) ?o) (lispm-desktop-app-games--solitaire-targets))))
    (cl-loop for row below 7
             collect (cl-loop for column below 7
                              collect (when-let* ((position (lispm-desktop-app-games--solitaire-position
                                                             row column)))
                                        (pcase (char-after position)
                                          (?o 'peg)
                                          (_ (if (memql position targets) 'target 'hole))))))))

(defun lispm-desktop-app-games--solitaire-click (row column)
  "Jump the peg at point to the hole at ROW and COLUMN when it can; else go
to the hole clicked."
  (when-let* ((position (lispm-desktop-app-games--solitaire-position row column)))
    (if-let* (((eq (char-after position) ?.))
              (move (seq-find (pcase-lambda (`(,direction . ,_))
                                (let ((move (solitaire-possible-move direction)))
                                  (and (consp move) (eql (nth 2 move) position))))
                              lispm-desktop-app-games--solitaire-moves)))
        (progn (undo-boundary) (funcall (cdr move)))
      (goto-char position))))

(lispm-desktop-app-ui-define 'solitaire-mode
  :board `(:cells lispm-desktop-app-games--solitaire-cells
           :tiles ((peg :shape peg :color ,(lispm-desktop-app-games-color 'blue))
                   (hole :shape hole)
                   (target :shape hole :mark dot))
           :cursor ,(lambda () (lispm-desktop-app-games--solitaire-cell (point)))
           :click lispm-desktop-app-games--solitaire-click
           :size 48)
  :status (lambda () (list (lispm-desktop-tr-n "%d peg left" "%d pegs left" solitaire-stones)))
  :buttons `((,(lispm-desktop-tr-noop "New Game") solitaire) (,(lispm-desktop-tr-noop "Undo") solitaire-undo))
  :label (lambda () (when (= solitaire-stones 1) (lispm-desktop-tr "You won!")))
  :keys `(("C-f C-b C-n C-p" . ,(lispm-desktop-tr-noop "Go to another hole"))
          ("S-→  S-←  S-↑  S-↓" . ,(lispm-desktop-tr-noop "Jump the peg that way"))
          ("Return" . ,(lispm-desktop-tr-noop "Jump, then the way"))
          ("Space" . ,(lispm-desktop-tr-noop "Count the moves left")) ("C-/" . ,(lispm-desktop-tr-noop "Undo"))))

;;;; Blackbox

(defun lispm-desktop-app-games--blackbox-char (x y)
  "Return Blackbox's character at X and Y, from -1 to 8: the box's inside
from 0 to 7, the edges it is shot into at -1 and 8."
  (char-after (+ 26 (* 2 x) (* 22 y))))

(defun lispm-desktop-app-games--blackbox-edge (x y)
  "Return what Blackbox's edge at X and Y says: nil before a shot, else H,
R or the detour's number."
  (let ((char (lispm-desktop-app-games--blackbox-char x y))
        (before (char-after (+ 25 (* 2 x) (* 22 y)))))
    (cond ((memq char '(nil ?\s)) nil)
          ((and before (<= ?0 before ?9) (<= ?0 char ?9)) (string before char))
          (t (string char)))))

(defun lispm-desktop-app-games--blackbox-cells ()
  "Return Blackbox's board, the box inside its edges: `empty', `ball' where
a ball is guessed, `wrong' and `missed' once it is done; the edges each
nil before a shot, or what came out."
  (cl-loop for row below 10
           collect (cl-loop for column below 10
                            collect (let ((x (1- column)) (y (1- row)))
                                      (cond ((and (memq x '(-1 8)) (memq y '(-1 8))) 'corner)
                                            ((or (memq x '(-1 8)) (memq y '(-1 8)))
                                             (cons 'edge (lispm-desktop-app-games--blackbox-edge x y)))
                                            (t (pcase (lispm-desktop-app-games--blackbox-char x y)
                                                 (?O 'ball) (?x 'wrong) (?o 'missed) (_ 'empty))))))))

(defun lispm-desktop-app-games--blackbox-tile (value)
  "Return the tile of Blackbox's VALUE."
  (pcase value
    ('empty '(:shape well))
    ('ball `(:shape ball :color ,(lispm-desktop-app-games-color 'blue)))
    ('wrong `(:shape ball :color ,(lispm-desktop-app-games-color 'blue) :mark cross))
    ('missed `(:shape ball :color ,(lispm-desktop-app-games-color 'grey)))
    (`(edge . ,said)
     `(:shape key :text ,(or said "")
       :color ,(pcase said
                 ('nil nil)
                 ("H" "#f3b9b0")
                 ("R" "#f4e3a0")
                 (_ "#c6dcf5"))))
    (_ '(:shape blank))))

(defun lispm-desktop-app-games--blackbox-click (row column)
  "Shoot into Blackbox's edge at ROW and COLUMN, or guess a ball inside."
  (let ((x (1- column)) (y (1- row)))
    (unless (and (memq x '(-1 8)) (memq y '(-1 8)))
      (setq bb-x x bb-y y)
      (bb-goto (cons x y))
      (bb-romp))))

(lispm-desktop-app-ui-define 'blackbox-mode
  :board '(:cells lispm-desktop-app-games--blackbox-cells
           :tiles lispm-desktop-app-games--blackbox-tile
           :cursor (lambda () (cons (1+ bb-y) (1+ bb-x)))
           :click lispm-desktop-app-games--blackbox-click
           :size 40)
  :status (lambda () (list (lispm-desktop-tr-n "%d ball hidden" "%d balls hidden" (length bb-board))
                           (lispm-desktop-tr-n "%d guessed" "%d guessed" (length bb-balls-placed))
                           (lispm-desktop-tr "Score %d" bb-score)))
  :buttons `((,(lispm-desktop-tr-noop "New Game") blackbox)
             (,(lispm-desktop-tr-noop "Done") bb-done ,(lambda () (= (length bb-balls-placed) (length bb-board)))))
  :keys `(("Space" . ,(lispm-desktop-tr-noop "Shoot a ray from the edge, or guess a ball inside"))
          ("C-f C-b C-n C-p" . ,(lispm-desktop-tr-noop "Move"))
          ("Return" . ,(lispm-desktop-tr-noop "Done: show the balls"))))

;;;; Mpuz

(defconst lispm-desktop-app-games--mpuz-rows '(2 4 6 8 10)
  "Mpuz's lines holding digits, top to bottom.")

(defconst lispm-desktop-app-games--mpuz-columns '(1 3 5 7 9)
  "Mpuz's columns holding digits, left to right.")

(defun lispm-desktop-app-games--mpuz-cells ()
  "Return Mpuz's multiplication: each square (TEXT STATE), the letter or,
once found, the digit, and `solved', `trivial' or `unsolved'; nil where
no digit is, but for the times sign."
  (let ((cells (make-list 5 nil)))
    (dotimes (row 5) (setf (nth row cells) (make-list 5 nil)))
    (setf (nth 2 (nth 1 cells)) '("×" sign))
    (dotimes (digit 10)
      (dolist (square (aref mpuz-board digit))
        (when-let* ((row (seq-position lispm-desktop-app-games--mpuz-rows (car square)))
                    (column (seq-position lispm-desktop-app-games--mpuz-columns (cdr square))))
          (setf (nth column (nth row cells))
                (cond ((aref mpuz-found-digits digit) (list (number-to-string digit) 'solved))
                      ((aref mpuz-trivial-digits digit) (list (number-to-string digit) 'trivial))
                      (t (list (string (+ ?A (aref mpuz-digit-to-letter digit))) 'unsolved)))))))
    cells))

(defun lispm-desktop-app-games--mpuz-tile (value)
  "Return the tile of Mpuz's square VALUE."
  (pcase value
    ('nil '(:shape blank))
    (`(,text sign) `(:shape text :text ,text))
    (`(,text ,state) `(:shape letter :text ,text
                       :color ,(pcase state ('solved "#1e7a34") ('trivial "#2457a6") (_ "#b8281e"))))))

(defun lispm-desktop-app-games--mpuz-click (row column)
  "Ask which digit the letter at ROW and COLUMN stands for, in a menu, and
propose it."
  (pcase (nth column (nth row (lispm-desktop-app-games--mpuz-cells)))
    (`(,text unsolved)
     (let ((letter (string-to-char text)))
       (lispm-desktop-popup-menu
        (mapcar (lambda (digit)
                  (list :label (lispm-desktop-tr "%s is %d" text digit)
                        :action (lambda () (mpuz-try-proposal letter (+ ?0 digit)))))
                (number-sequence 0 9)))))))

(lispm-desktop-app-ui-define 'mpuz-mode
  :board '(:cells lispm-desktop-app-games--mpuz-cells
           :tiles lispm-desktop-app-games--mpuz-tile
           :click lispm-desktop-app-games--mpuz-click
           :rules (1 3)
           :size 44)
  :status (lambda () (list (lispm-desktop-tr "Errors %d" mpuz-nb-errors)
                           (lispm-desktop-tr "Games won %d" mpuz-nb-completed-games)))
  :buttons `((,(lispm-desktop-tr-noop "New Game") mpuz-start-new-game)
             (,(lispm-desktop-tr-noop "Show Solution") mpuz-show-solution))
  :label (lambda () (lispm-desktop-tr "Click a letter to say its digit"))
  :keys `((,(lispm-desktop-tr-noop "A … J, then 0 … 9")
           . ,(lispm-desktop-tr-noop "Say which digit a letter stands for"))
          ("C-g" . ,(lispm-desktop-tr-noop "Give up the game"))))

;;;; Bubbles

(defconst lispm-desktop-app-games--bubble-colors
  '("#d8402f" "#36a852" "#3d6fd6" "#ef8a1a" "#8e4fc6")
  "The colours of Bubbles' bubbles, by its colour index.")

(defun lispm-desktop-app-games--bubbles-cells ()
  "Return Bubbles' board: each cell its colour's index, or -1 when empty,
with `active' when in the group the pointer is on."
  (save-excursion
    (cl-loop for row below (bubbles--grid-height)
             collect (cl-loop for column below (bubbles--grid-width)
                              collect (progn
                                        (bubbles--goto row column)
                                        (cons (or (get-text-property (point) 'index) -1)
                                              (and (get-text-property (point) 'active) t)))))))

(defun lispm-desktop-app-games--bubbles-tile (value)
  "Return the tile of Bubbles' VALUE, (INDEX . ACTIVE)."
  (pcase-let ((`(,index . ,active) value))
    (if (< index 0)
        '(:shape well)
      (let ((color (nth (mod index (length lispm-desktop-app-games--bubble-colors))
                        lispm-desktop-app-games--bubble-colors)))
        `(:shape ball :color ,(if active (lispm-desktop-platinum-blend color "#ffffff" 0.35) color)
          ,@(when active '(:mark dot)))))))

(defun lispm-desktop-app-games--bubbles-click (row column)
  "Pop the group of bubbles at ROW and COLUMN."
  (bubbles--goto row column)
  (bubbles--mark-neighborhood)
  (bubbles-plop))

(defun lispm-desktop-app-games--bubbles-images (show &rest arguments)
  "Show Bubbles' images as SHOW does with ARGUMENTS, unless a board draws
its bubbles: they would stand in its text, which the board keeps out of
sight, and SHOW removes every overlay of the buffer, the board's too."
  (unless lispm-desktop-app-board
    (apply show arguments)))

(defvar bubbles--col-offset)
(defvar bubbles--row-offset)

(defun lispm-desktop-app-games--bubbles-offsets ()
  "Leave Bubbles' text where it starts in an application's window: Bubbles
centres it in its window with blank space, which would push its board
down; the board centres itself.
This runs after `bubbles--compute-offsets'."
  (when (lispm-desktop-app-ui-app)
    (setq bubbles--col-offset 0 bubbles--row-offset 0)))

(lispm-desktop-app-ui-define 'bubbles-mode
  :board '(:cells lispm-desktop-app-games--bubbles-cells
           :tiles lispm-desktop-app-games--bubbles-tile
           :click lispm-desktop-app-games--bubbles-click
           :size 32)
  :status (lambda () (list (lispm-desktop-tr "Score %s" (lispm-desktop-app-ui--number bubbles--score))
                           (lispm-desktop-tr "This group %d" bubbles--neighborhood-score)))
  :buttons `((,(lispm-desktop-tr-noop "New Game") bubbles) (,(lispm-desktop-tr-noop "Undo") bubbles-undo))
  :keys `(("Space  Return" . ,(lispm-desktop-tr-noop "Pop the group the cursor is on"))
          ("C-f C-b n p" . ,(lispm-desktop-tr-noop "Move")) ("u" . ,(lispm-desktop-tr-noop "Undo"))))

;;;; Life

(defvar-local lispm-desktop-app-games--life-timer nil
  "The timer giving Life its next generation, while it runs.")

(defvar-local lispm-desktop-app-games--life-end nil
  "Why Life stopped by itself, in English, translated where shown, or nil.")

(defconst lispm-desktop-app-games-life-period 0.25
  "Seconds between Life's generations.")

(defun lispm-desktop-app-games--life-step (&optional buffer)
  "Give Life in BUFFER, by default the current one, its next generation.
Life ends its own loop when all life has perished; that ends the timer,
as does Life's window closing."
  (with-current-buffer (or buffer (current-buffer))
    (unless (get-buffer-window nil t)
      (lispm-desktop-app-games-life-stop))
    (let ((start (window-start (get-buffer-window nil t))))
      (save-excursion
        (condition-case nil
            (catch 'life-exit (life--tick))
          (quit (lispm-desktop-app-games-life-stop)
                (setq lispm-desktop-app-games--life-end (lispm-desktop-tr-noop "All life has perished")))))
      (when-let* ((window (get-buffer-window nil t)))
        (set-window-start window start))
      (lispm-desktop-app-board-refresh)
      (force-mode-line-update t))))

(defun lispm-desktop-app-games-life-go ()
  "Let Life run, a generation every `lispm-desktop-app-games-life-period'."
  (interactive)
  (unless lispm-desktop-app-games--life-timer
    (setq lispm-desktop-app-games--life-end nil
          lispm-desktop-app-games--life-timer
          (run-at-time lispm-desktop-app-games-life-period lispm-desktop-app-games-life-period
                       #'lispm-desktop-app-games--life-step (current-buffer)))))

(defun lispm-desktop-app-games-life-stop ()
  "Stop Life where it is."
  (interactive)
  (when lispm-desktop-app-games--life-timer
    (cancel-timer lispm-desktop-app-games--life-timer)
    (setq lispm-desktop-app-games--life-timer nil)))

(defun lispm-desktop-app-games-life-toggle ()
  "Stop Life, or let it run again."
  (interactive)
  (if lispm-desktop-app-games--life-timer
      (lispm-desktop-app-games-life-stop)
    (lispm-desktop-app-games-life-go)))

(defun lispm-desktop-app-games-life-next ()
  "Show Life's next generation, stopped."
  (interactive)
  (lispm-desktop-app-games-life-stop)
  (lispm-desktop-app-games--life-step))

(defun lispm-desktop-app-games-life ()
  "Start Life on a new random pattern, a field the desktop holds, without
holding Emacs as `life' does until a key is pressed."
  (interactive)
  (require 'life)
  (when-let* ((old (get-buffer "*Life*")))
    (with-current-buffer old (lispm-desktop-app-games-life-stop)))
  (let ((life--max-width 64) (life--max-height 40))
    (life-setup))
  (add-hook 'kill-buffer-hook #'lispm-desktop-app-games-life-stop nil t)
  (lispm-desktop-app-games-life-go))

(defun lispm-desktop-app-games--life-cells ()
  "Return Life's field: each cell t while alive."
  (mapcar (lambda (line) (mapcar (lambda (char) (eq char ?@)) line))
          (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")))

(lispm-desktop-app-ui-define 'life-mode
  :app 'life
  :run #'lispm-desktop-app-games-life
  :board `(:cells lispm-desktop-app-games--life-cells
           :tiles ((t :shape block :color ,(lispm-desktop-app-games-color 'green)))
           :size 10)
  :status (lambda () (let ((alive (how-many "@" (point-min) (point-max))))
                       (list (lispm-desktop-tr "Generation %s" life-generation-string)
                             (lispm-desktop-tr-n "%d alive" "%d alive" alive))))
  :buttons `((,(lambda () (if lispm-desktop-app-games--life-timer
                              (lispm-desktop-tr "Stop")
                            (lispm-desktop-tr-in "run the game" "Go")))
              lispm-desktop-app-games-life-toggle)
             (,(lispm-desktop-tr-noop "Next") lispm-desktop-app-games-life-next)
             (,(lispm-desktop-tr-noop "New Pattern") lispm-desktop-app-games-life))
  :label (lambda () (when lispm-desktop-app-games--life-end
                      (lispm-desktop-tr lispm-desktop-app-games--life-end)))
  :keys `(("Space" . ,(lispm-desktop-tr-noop "Stop, or go on")) ("n" . ,(lispm-desktop-tr-noop "Next generation"))
          ("r" . ,(lispm-desktop-tr-noop "A new pattern"))))

(defvar-keymap lispm-desktop-app-games-life-map
  :doc "Keys of Life in its window."
  "SPC" #'lispm-desktop-app-games-life-toggle
  "n" #'lispm-desktop-app-games-life-next
  "r" #'lispm-desktop-app-games-life)

(defun lispm-desktop-app-games--life-keys ()
  "Give Life's buffer the keys of `lispm-desktop-app-games-life-map'."
  (when (derived-mode-p 'life-mode)
    (use-local-map (make-composed-keymap lispm-desktop-app-games-life-map (current-local-map)))))

(add-hook 'life-mode-hook #'lispm-desktop-app-games--life-keys)

;;;; Towers of Hanoi

(defconst lispm-desktop-app-games-hanoi-rings 5
  "Rings the Towers of Hanoi are played with.")

(defun lispm-desktop-app-games--hanoi-faces ()
  "Draw Hanoi's rings as raised bars of colour, its poles and base grey."
  (dolist (face '(hanoi-even-ring-face hanoi-odd-ring-face hanoi-pole-face hanoi-base-face))
    (let ((color (pcase face
                   ('hanoi-even-ring-face (lispm-desktop-app-games-color 'blue))
                   ('hanoi-odd-ring-face (lispm-desktop-app-games-color 'orange))
                   ('hanoi-pole-face "#9a9a9a")
                   (_ "#6a6a6a"))))
      (face-remap-add-relative face `(:background ,color :foreground ,color
                                      :box (:line-width (1 . 1) :style released-button))))))

(defun lispm-desktop-app-games-hanoi ()
  "Solve the Towers of Hanoi in the window once it is up, its rings drawn
as raised bars.  Hanoi holds Emacs until it is done, or a key is pressed."
  (interactive)
  (require 'hanoi)
  (let ((window (selected-window)))
    (switch-to-buffer (get-buffer-create "*Hanoi*"))
    (lispm-desktop-app-games--hanoi-faces)
    (run-at-time 0.3 nil
                 (lambda ()
                   (when (window-live-p window)
                     (with-selected-window window
                       (let ((hanoi-use-faces t)
                             (hanoi-move-period 0.25))
                         (condition-case nil
                             (hanoi lispm-desktop-app-games-hanoi-rings)
                           (quit nil)))))))))

(lispm-desktop-app-ui-define 'hanoi
  :run #'lispm-desktop-app-games-hanoi
  :buttons `((,(lispm-desktop-tr-noop "Start Again") lispm-desktop-app-games-hanoi))
  :label (lambda () (lispm-desktop-tr "Any key stops it"))
  :fit nil
  :cursor nil
  :keys `((,(lispm-desktop-tr-noop "any key") . ,(lispm-desktop-tr-noop "Stop"))))

;;;; Dunnet and Doctor

(defun lispm-desktop-app-games--dunnet-say (words)
  "Return a command saying WORDS to Dunnet, as typed at its prompt."
  (lambda ()
    (interactive)
    (goto-char (point-max))
    (insert words)
    (dun-parse 1)))

(lispm-desktop-app-ui-define 'dun-mode
  :prose t
  ;; The labels are translated; the words said to Dunnet are its own.
  :buttons `((,(lispm-desktop-tr-noop "Look" "game command") ,(lispm-desktop-app-games--dunnet-say "look"))
             (,(lispm-desktop-tr-noop "Inventory") ,(lispm-desktop-app-games--dunnet-say "inventory"))
             (,(lispm-desktop-tr-noop "Score") ,(lispm-desktop-app-games--dunnet-say "score"))
             (,(lispm-desktop-tr-noop "Help") ,(lispm-desktop-app-games--dunnet-say "help")))
  :label (lambda () (lispm-desktop-tr "Type what to do, then Return"))
  :keys `(("Return" . ,(lispm-desktop-tr-noop "Do what is typed: look, take the shovel, go east"))))

(lispm-desktop-app-ui-define 'doctor-mode
  :prose t
  :buttons `((,(lispm-desktop-tr-noop "New Session") doctor))
  :label (lambda () (lispm-desktop-tr "Return twice to say it"))
  :keys `(("Return Return" . ,(lispm-desktop-tr-noop "Say what is typed"))
          ("C-j" . ,(lispm-desktop-tr-noop "Say it at once"))))

;;;; Set up

(defun lispm-desktop-setup-app-games ()
  "Draw gamegrid's cells as Platinum tiles in the applications' windows, and
keep their scores out of sight."
  (advice-add 'gamegrid-initialize-display :after #'lispm-desktop-app-games--retile)
  (advice-add 'gamegrid-add-score :around #'lispm-desktop-app-games--quiet-score)
  (advice-add 'bubbles--show-images :around #'lispm-desktop-app-games--bubbles-images)
  (advice-add 'bubbles--compute-offsets :after #'lispm-desktop-app-games--bubbles-offsets)
  (add-hook 'tetris-mode-hook #'lispm-desktop-app-games--gamegrid-setup)
  (add-hook 'snake-mode-hook #'lispm-desktop-app-games--gamegrid-setup))

(provide 'lispm-desktop-app-games)
;;; lispm-desktop-app-games.el ends here
