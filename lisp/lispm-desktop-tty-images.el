;;; lispm-desktop-tty-images.el --- Icons as images on a text terminal  -*- lexical-binding: t; -*-

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

;; A text terminal that speaks the Kitty graphics protocol, as kitty,
;; Ghostty, WezTerm and iTerm2 do, shows the desktop's icons as images
;; instead of block characters.  Emacs draws no images on a terminal, so
;; they are put there behind its back: kitty-graphics.el sends each image
;; once, then places it at a cell, replacing the placement as the icon
;; moves.  The terminal is asked at the start whether it can, and how
;; large its cells are; where it can, kitty-graphics.el is installed the
;; first time, see `lispm-desktop-tty-images-package'.  Elsewhere, and
;; inside tmux, the icons stay block characters.
;;
;; An image lies over the cells' backgrounds and under their text, and
;; is cut where a window or a menu in front of its own covers it, so that
;; only the rectangles of it left show: placed anew before Emacs draws
;; the screen, whenever one moves, comes or goes, a menu's own loop
;; included, see `lispm-desktop-tty-images--before-redisplay'.  An image
;; is clear round its icon, and a pixel icon's pixels are squares of
;; whole device pixels, so that they stay crisp, see
;; `lispm-ui-raster-png'.
;;
;; A cleared screen loses the placements, as Emacs clears it after a
;; resize or a full redraw: they are placed again after each command and
;; each change of the screen's size.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-ui-raster)
(require 'lispm-desktop-platinum-icons)
(require 'lispm-desktop-i18n)

(declare-function kitty-graphics--kitty-prepare "kitty-graphics" (file image-id))
(declare-function kitty-graphics--transmitted-p "kitty-graphics" (id))
(declare-function kitty-graphics--terminal-send "kitty-graphics" (str))
(declare-function kitty-graphics--delete-placement "kitty-graphics" (id pid))
(declare-function kitty-graphics--alloc-id "kitty-graphics" ())
(declare-function kitty-graphics--alloc-placement-id "kitty-graphics" ())

(defcustom lispm-desktop-tty-images t
  "Non-nil shows the desktop's icons as images on a text terminal that
can show them: one speaking the Kitty graphics protocol.  Elsewhere,
and with this nil, they are block characters, see
`lispm-ui-icon-picture'."
  :type 'boolean
  :group 'lispm-desktop)

(defconst lispm-desktop-tty-images-package
  '(kitty-graphics :url "https://github.com/cashmeredev/kitty-graphics.el"
                   :rev "13666d4eb2ef4eeed24697c0326368eff3667dce")
  "The package sending the images, (NAME :url URL :rev COMMIT): installed
with package-vc at the commit it was read at, on the first terminal that
shows images.")

(defvar lispm-desktop-tty-images-ready-hook nil
  "Hook run once the terminal is found to show images.")

(defun lispm-desktop-tty-images-directory ()
  "Return the directory the icons' images are kept in."
  (file-name-as-directory (locate-user-emacs-file "lispm-desktop/tty-images")))

;;;; Asking the terminal

(defconst lispm-desktop-tty-images--answers
  '(("\e_G[^\e]*\e\\\\" . nil)
    ;; The cell's size: CSI 16 t, as kitty, Ghostty and WezTerm answer it.
    ("\e\\[6;\\([0-9.]+\\);\\([0-9.]+\\)t" . cell)
    ;; The cell's size and the screen's scale, as iTerm2 answers it.
    ("\e]1337;ReportCellSize=\\([0-9.]+\\);\\([0-9.]+\\);?\\([0-9.]*\\)\\(?:\a\\|\e\\\\\\)" . iterm)
    ;; The text area's size, CSI 14 t.
    ("\e\\[4;\\([0-9]+\\);\\([0-9]+\\)t" . area)
    ("\e\\[\\?[0-9;]*c" . nil))
  "The terminal's answers to `lispm-desktop-tty-images--probe': (REGEXP .
WHAT), WHAT saying what the first two numbers are a height and width
of: `cell', a cell; `iterm', a cell, and the third the screen's scale;
`area', the text area.")

(defun lispm-desktop-tty-images--probe ()
  "Ask the selected frame's terminal whether it shows Kitty images, and
how.  Return nil where it does not, else a plist: :width and :height of
a cell in the terminal's units, :scale, device pixels a unit, and
:shift, device pixels an image is put right by, where the terminal
draws them that far left of their cell.  The replies are read here, up
to the answer to the device attributes every terminal gives, a second
at most; anything typed meanwhile is put back."
  (let ((reply "") (events nil) (deadline (+ (float-time) 1.0)))
    (send-string-to-terminal
     (concat "\e_Gi=31,s=1,v=1,a=q,t=d,f=24;AAAA\e\\" "\e[16t" "\e]1337;ReportCellSize\a" "\e[14t" "\e[c"))
    (while (and (< (float-time) deadline)
                (not (string-match-p "\e\\[\\?[0-9;]*c" reply)))
      (let ((event (read-event nil nil (max 0.01 (- deadline (float-time))))))
        (cond ((characterp event) (setq reply (concat reply (string event))))
              (event (push event events)))))
    (let ((rest reply) (found nil))
      (pcase-dolist (`(,answer . ,what) lispm-desktop-tty-images--answers)
        (when (and what (string-match answer reply))
          (push (list what
                      (string-to-number (match-string 2 reply))
                      (string-to-number (match-string 1 reply))
                      (and (match-beginning 3) (string-to-number (match-string 3 reply))))
                found))
        (setq rest (replace-regexp-in-string answer "" rest)))
      (setq unread-command-events (append (nreverse events) (string-to-list rest) unread-command-events))
      (when (string-match-p "\e_Gi=31;OK" reply)
        (pcase-let* ((`(,_ ,area-width ,area-height ,_) (assq 'area found))
                     (`(,_ ,width ,height ,_) (or (assq 'cell found) (assq 'iterm found)
                                                  (and area-width
                                                       (list 'area (/ (float area-width) (display-pixel-width))
                                                             (/ (float area-height) (display-pixel-height))))
                                                  ;; Keeping its size to itself: as most are.
                                                  (list 'cell 8.0 16.0)))
                     (iterm (assq 'iterm found))
                     (scale (or (and iterm (nth 3 iterm) (> (nth 3 iterm) 0) (nth 3 iterm)) 2))
                     ;; iTerm2 3.6 draws an image without the margin it
                     ;; leaves at the text's left: the margin is what the
                     ;; text area has more than its cells, shared by the
                     ;; two sides.
                     (margin (if (and iterm area-width)
                                 (max 0 (/ (- area-width (* width (display-pixel-width))) 2.0))
                               0)))
          (list :width width :height height :scale scale :shift (round (* margin scale))))))))

(defun lispm-desktop-tty-images--package ()
  "Load kitty-graphics.el, installing it first where it is missing, see
`lispm-desktop-tty-images-package'.  Return non-nil where it loaded."
  (or (require 'kitty-graphics nil t)
      (pcase-let ((`(,name :url ,url :rev ,rev) lispm-desktop-tty-images-package))
        (require 'package-vc)
        (message "%s" (lispm-desktop-tr "Getting what draws images in a terminal…"))
        (ignore-errors (package-vc-install (list name :url url) rev))
        (require 'kitty-graphics nil t))))

(defun lispm-desktop-tty-images--check ()
  "Find out whether the selected frame's terminal shows images, once, and
run `lispm-desktop-tty-images-ready-hook' where it does.  The answer is
the terminal's parameter `lispm-desktop-tty-images': what
`lispm-desktop-tty-images--probe' found, or `none'."
  (when (and lispm-desktop-tty-images
             (lispm-desktop-tty-p)
             (null (terminal-parameter nil 'lispm-desktop-tty-images)))
    (let ((terminal (let ((terminal (and (not (getenv "TMUX")) (lispm-desktop-tty-images--probe))))
                      (and terminal (lispm-desktop-tty-images--package) terminal))))
      (set-terminal-parameter nil 'lispm-desktop-tty-images (or terminal 'none))
      (when terminal
        (run-hooks 'lispm-desktop-tty-images-ready-hook)))))

(defun lispm-desktop-tty-images-p ()
  "Return non-nil when the selected frame's terminal shows the icons as
images, see `lispm-desktop-tty-images'."
  (and lispm-desktop-tty-images
       (lispm-desktop-tty-p)
       (consp (terminal-parameter nil 'lispm-desktop-tty-images))))

;;;; Images

(defun lispm-desktop-tty-images--file (name columns rows selected)
  "Return a PNG of icon NAME for COLUMNS by ROWS cells, darkened when
SELECTED, clear round it, as (FILE WIDTH HEIGHT); made once.  It is as
many device pixels as the cells are, the icon nine tenths of it: a
pixel icon at a whole multiple of its own size, any other drawn at half
and doubled.  NAME is as `lispm-ui-icon-file' takes it."
  (let* ((terminal (terminal-parameter nil 'lispm-desktop-tty-images))
         (scale (plist-get terminal :scale))
         (width (round (* scale columns (plist-get terminal :width))))
         (height (round (* scale rows (plist-get terminal :height))))
         (source (lispm-ui-icon-file name))
         (fit (floor (* 0.9 (min width height))))
         (own (lispm-ui-raster-pixel-size source))
         (size (or own (max 1 (/ fit 2))))
         (factor (if own (max 1 (/ fit own)) 2))
         (key (md5 (prin1-to-string
                    (list source (file-attribute-modification-time (file-attributes source))
                          size factor width height (and selected t)))))
         (file (expand-file-name (concat key ".png") (lispm-desktop-tty-images-directory))))
    (unless (file-exists-p file)
      (when-let* ((pixels (lispm-ui-raster source size)))
        (when selected
          (setq pixels (vconcat (mapcar (lambda (row)
                                          (vconcat (mapcar (lambda (pixel)
                                                             (and pixel (lispm-ui-raster-darker pixel)))
                                                           row)))
                                        pixels))))
        (make-directory (file-name-directory file) t)
        (let ((coding-system-for-write 'binary))
          (write-region (lispm-ui-raster-png pixels factor width height)
                        nil file nil 'silent))))
    (and (file-exists-p file) (list file width height))))

;;;; Placements

(defvar lispm-desktop-tty-images--wanted nil
  "The icons shown as images: a list of (KEY FILE WIDTH HEIGHT WINDOW
BUFFER LINE COLUMN COLUMNS ROWS), the image FILE, WIDTH by HEIGHT
pixels, over COLUMNS by ROWS cells from LINE, COLUMN of BUFFER's text,
where WINDOW shows it.  KEY is (WINDOW . the icon's key).")

(defvar lispm-desktop-tty-images--placed nil
  "The placements on the terminal: an alist of (KEY . PIECE) to (ID
PLACEMENT . SENT), SENT the escapes placing it.")

(defvar lispm-desktop-tty-images--ids (make-hash-table :test #'equal)
  "The image id of each file sent.")

(defvar lispm-desktop-tty-images--placements (make-hash-table :test #'equal)
  "The placement id of each piece of an icon, by (KEY . PIECE).")

(defvar lispm-desktop-tty-images--timer nil
  "The timer placing the images anew after the screen was drawn.")

(defun lispm-desktop-tty-images-show (window icons)
  "Show ICONS in WINDOW as images, and no other images of WINDOW.
ICONS is a list of (KEY NAME SELECTED LINE COLUMN COLUMNS ROWS), each
icon NAME, darkened when SELECTED, COLUMNS by ROWS cells from LINE,
COLUMN of the text WINDOW shows, from 0.  Each shows where WINDOW shows
it but for what windows and menus in front of it cover."
  (let ((buffer (window-buffer window)))
    (setq lispm-desktop-tty-images--wanted
          (append (seq-remove (lambda (wanted) (eq (nth 4 wanted) window)) lispm-desktop-tty-images--wanted)
                  (delq nil
                        (mapcar (pcase-lambda (`(,key ,name ,selected ,line ,column ,columns ,rows))
                                  (when-let* ((image (lispm-desktop-tty-images--file
                                                      name columns rows selected)))
                                    `((,window . ,key) ,@image ,window ,buffer ,line ,column ,columns ,rows)))
                                icons)))))
  (lispm-desktop-tty-images--schedule))

(defun lispm-desktop-tty-images-blank (key name selected columns rows)
  "Return ROWS blank lines COLUMNS wide for icon NAME, darkened when
SELECTED, to show as an image, or nil where the terminal shows none or
NAME is no drawing it can be made of.  The first line carries the icon,
KEY its key in its window, as its `lispm-desktop-tty-image' property,
for `lispm-desktop-tty-images-show-marked' to find where it went."
  (when (and (lispm-desktop-tty-images-p)
             (lispm-desktop-tty-images--file name columns rows selected))
    (cons (propertize (make-string columns ?\s) 'lispm-desktop-tty-image
                      (list key name selected columns rows))
          (make-list (1- rows) (make-string columns ?\s)))))

(defun lispm-desktop-tty-images-show-marked (window)
  "Show as images the icons WINDOW's text marks, as
`lispm-desktop-tty-images-blank' makes them, and no other images of
WINDOW; where none, take WINDOW's away."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (save-excursion
        (let ((icons nil)
              (at (point-min)))
          (while (setq at (text-property-not-all at (point-max) 'lispm-desktop-tty-image nil))
            (pcase-let ((`(,key ,name ,selected ,columns ,rows) (get-text-property at 'lispm-desktop-tty-image)))
              (goto-char at)
              (push (list key name selected (1- (line-number-at-pos)) (current-column) columns rows) icons))
            (setq at (or (next-single-property-change at 'lispm-desktop-tty-image) (point-max))))
          (lispm-desktop-tty-images-show window icons))))))

(defun lispm-desktop-tty-images--schedule ()
  "Place every image anew once Emacs has drawn the screen, as one it
cleared has lost them."
  (unless (timerp lispm-desktop-tty-images--timer)
    (setq lispm-desktop-tty-images--timer
          (run-at-time 0 nil (lambda ()
                               (setq lispm-desktop-tty-images--timer nil)
                               (lispm-desktop-tty-images--place t))))))

(defun lispm-desktop-tty-images--origin (frame)
  "Return where FRAME's top left is on the terminal, (COLUMN . ROW) from 0."
  (let ((column 0) (row 0))
    (while (frame-parent frame)
      (pcase-let ((`(,x . ,y) (frame-position frame)))
        (setq column (+ column x) row (+ row y)))
      (setq frame (frame-parent frame)))
    (cons column row)))

(defun lispm-desktop-tty-images--frame-cells (frame)
  "Return the cells FRAME covers on the terminal, (TOP LEFT BOTTOM RIGHT),
the ends excluded, its outline a cell outside it where it has one."
  (pcase-let ((`(,x . ,y) (lispm-desktop-tty-images--origin frame))
              (outline (if (frame-parameter frame 'undecorated) 0 1)))
    (list (- y outline) (- x outline)
          (+ y (frame-pixel-height frame) outline) (+ x (frame-pixel-width frame) outline))))

(defun lispm-desktop-tty-images--subtract (cells cut)
  "Return the parts of CELLS outside CUT, both (TOP LEFT BOTTOM RIGHT), as
a list of such rectangles."
  (pcase-let ((`(,top ,left ,bottom ,right) cells)
              (`(,cut-top ,cut-left ,cut-bottom ,cut-right) cut))
    (if (or (>= cut-top bottom) (<= cut-bottom top) (>= cut-left right) (<= cut-right left))
        (list cells)
      (let ((cut-top (max cut-top top))
            (cut-bottom (min cut-bottom bottom))
            (pieces nil))
        (when (> cut-top top) (push (list top left cut-top right) pieces))
        (when (> cut-left left) (push (list cut-top left cut-bottom cut-left) pieces))
        (when (< cut-right right) (push (list cut-top cut-right cut-bottom right) pieces))
        (when (< cut-bottom bottom) (push (list cut-bottom left bottom right) pieces))
        (nreverse pieces)))))

(defun lispm-desktop-tty-images--pieces (wanted frames)
  "Return where the image WANTED is on the terminal, as (CELLS . PIECES):
the cells it covers, and those of them its window shows and no frame of
FRAMES, the frames from the front, in front of the window's covers, as
rectangles.  Nil where its window does not show it."
  (pcase-let ((`(,_ ,_ ,_ ,_ ,window ,buffer ,line ,column ,columns ,rows) wanted))
    (when (and (window-live-p window)
               (eq (window-buffer window) buffer)
               (frame-visible-p (window-frame window))
               (eq (frame-root-frame (window-frame window)) (frame-root-frame (selected-frame))))
      (pcase-let* ((frame (window-frame window))
                   (root (frame-root-frame frame))
                   (`(,x . ,y) (lispm-desktop-tty-images--origin frame))
                   (`(,body-left ,body-top ,body-right ,body-bottom) (window-body-edges window))
                   (start (with-current-buffer buffer
                            (save-excursion (goto-char (window-start window)) (1- (line-number-at-pos)))))
                   (top (+ y body-top (- line start)))
                   (left (+ x body-left (- column (window-hscroll window))))
                   (cells (list top left (+ top rows) (+ left columns)))
                   (shown (list (max top (+ y body-top)) (max left (+ x body-left))
                                (min (+ top rows) (+ y body-bottom)) (min (+ left columns) (+ x body-right))))
                   (pieces (and (< (nth 0 shown) (nth 2 shown)) (< (nth 1 shown) (nth 3 shown))
                                (list shown))))
        (dolist (front (seq-take-while (lambda (other) (not (eq other frame))) frames))
          (when (and pieces (frame-parent front) (frame-visible-p front)
                     (eq (frame-root-frame front) root))
            (let ((cut (lispm-desktop-tty-images--frame-cells front)))
              (setq pieces (mapcan (lambda (piece) (lispm-desktop-tty-images--subtract piece cut)) pieces)))))
        (cons cells pieces)))))

(defun lispm-desktop-tty-images--place (&optional again)
  "Place the images wanted where their windows show them, but for what
frames in front of them cover, and take away the rest, see
`lispm-desktop-tty-images--wanted'.  A placement as it was is sent
again only when AGAIN."
  (when (and (lispm-desktop-tty-p) (featurep 'kitty-graphics))
    (setq lispm-desktop-tty-images--wanted
          (seq-filter (lambda (wanted) (window-live-p (nth 4 wanted))) lispm-desktop-tty-images--wanted))
    (let* ((shown (lispm-desktop-tty-images-p))
           (terminal (terminal-parameter nil 'lispm-desktop-tty-images))
           (cell (and shown (max 1 (round (* (plist-get terminal :scale) (plist-get terminal :width))))))
           (shift (if shown (plist-get terminal :shift) 0))
           (frames (and shown (frame-list-z-order)))
           (placed nil)
           (out nil))
      (when shown
        (dolist (wanted lispm-desktop-tty-images--wanted)
          (pcase-let* ((`(,key ,file ,width ,height ,_ ,_ ,_ ,_ ,columns ,rows) wanted)
                       (`(,cells . ,pieces) (lispm-desktop-tty-images--pieces wanted frames))
                       (id (with-memoization (gethash file lispm-desktop-tty-images--ids)
                             (kitty-graphics--alloc-id))))
            (seq-do-indexed
             (pcase-lambda (`(,top ,left ,bottom ,right) index)
               (let* ((piece (cons key index))
                      (placement (with-memoization (gethash piece lispm-desktop-tty-images--placements)
                                   (kitty-graphics--alloc-placement-id)))
                      (whole (equal (list top left bottom right) cells))
                      (sent (concat
                             (format "\e[%d;%dH" (1+ top) (+ 1 left (/ shift cell)))
                             (format "\e_Gq=2,a=p,i=%d,p=%d,c=%d,r=%d,z=-1,X=%d,C=1"
                                     id placement (- right left) (- bottom top) (% shift cell))
                             (unless whole
                               (format ",x=%d,y=%d,w=%d,h=%d"
                                       (round (* (- left (nth 1 cells)) width) columns)
                                       (round (* (- top (nth 0 cells)) height) rows)
                                       (round (* (- right left) width) columns)
                                       (round (* (- bottom top) height) rows)))
                             "\e\\"))
                      (before (cdr (assoc piece lispm-desktop-tty-images--placed))))
                 (unless (kitty-graphics--transmitted-p id)
                   (kitty-graphics--kitty-prepare file id))
                 ;; A placement belongs to its image: the piece's image
                 ;; before, as before the icon was selected, goes first.
                 (when (and before (/= (car before) id))
                   (push (format "\e_Ga=d,d=i,i=%d,p=%d,q=2\e\\" (car before) (cadr before)) out))
                 (when (or again (not (equal before (cons id (cons placement sent)))))
                   (push sent out))
                 (push (cons piece (cons id (cons placement sent))) placed)))
             pieces))))
      (pcase-dolist (`(,piece ,id ,placement . ,_) lispm-desktop-tty-images--placed)
        (unless (assoc piece placed)
          (push (format "\e_Ga=d,d=i,i=%d,p=%d,q=2\e\\" id placement) out)))
      (when out
        (dolist (escape (append '("\e[?2026h\e7") (nreverse out) '("\e8\e[?2026l")))
          (kitty-graphics--terminal-send escape)))
      (setq lispm-desktop-tty-images--placed placed))))

(defun lispm-desktop-tty-images--before-redisplay (&rest _)
  "Place the images anew where a window or a menu moved, came or went,
before Emacs draws it, a menu's own loop included.  This is advice
after `pre-redisplay-function'."
  (when (or lispm-desktop-tty-images--wanted lispm-desktop-tty-images--placed)
    (with-demoted-errors "LispM images: %S"
      (lispm-desktop-tty-images--place))))

(defun lispm-desktop-tty-images--take-away ()
  "Take every image placed off the terminal, as Emacs leaves it."
  (when (and lispm-desktop-tty-images--placed (featurep 'kitty-graphics))
    (pcase-dolist (`(,_ ,id ,placement . ,_) lispm-desktop-tty-images--placed)
      (kitty-graphics--delete-placement id placement))
    (setq lispm-desktop-tty-images--placed nil)))

(defun lispm-desktop-tty-images--refresh (&rest _)
  "Place the images anew: the screen may have been cleared.  This is a
`post-command-hook' and `window-size-change-functions' function."
  (when (or lispm-desktop-tty-images--wanted lispm-desktop-tty-images--placed)
    (lispm-desktop-tty-images--schedule)))

(defun lispm-desktop-tty-images-setup ()
  "Show icons as images on a text terminal that can, see
`lispm-desktop-tty-images'.  The terminal is asked once Emacs is idle."
  (when (lispm-desktop-tty-p)
    (run-with-idle-timer 0.2 nil #'lispm-desktop-tty-images--check)
    (add-function :after pre-redisplay-function #'lispm-desktop-tty-images--before-redisplay)
    (add-hook 'post-command-hook #'lispm-desktop-tty-images--refresh)
    (add-hook 'window-size-change-functions #'lispm-desktop-tty-images--refresh)
    (add-hook 'suspend-hook #'lispm-desktop-tty-images--take-away)
    (add-hook 'suspend-resume-hook #'lispm-desktop-tty-images--refresh)
    (add-hook 'kill-emacs-hook #'lispm-desktop-tty-images--take-away)))

;; LispM UI's alerts and file dialog use the desktop's terminal renderer.
(setq lispm-ui-tty-image-function #'lispm-desktop-tty-images-blank
      lispm-ui-tty-image-show-function #'lispm-desktop-tty-images-show-marked)

(provide 'lispm-desktop-tty-images)
;;; lispm-desktop-tty-images.el ends here
