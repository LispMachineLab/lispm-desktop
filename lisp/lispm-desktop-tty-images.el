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
;; An image lies under the cells' backgrounds.  The desktop is drawn in
;; the terminal's default background, so it shows through there, and
;; whatever Emacs draws over it with a background of its own, a window, a
;; menu or an alert, hides it, with no reckoning here.  While images are
;; shown, that default background is the desktop's colour, so an image
;; is clear round its icon.  A pixel icon's pixels are squares of whole device pixels, so
;; that they stay crisp, see `lispm-desktop-platinum-raster-png'.
;;
;; A cleared screen loses the placements, as Emacs clears it after a
;; resize or a full redraw: they are placed again after each command and
;; each change of the screen's size.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-platinum-raster)
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
`lispm-desktop-platinum-icon-picture'."
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

(defconst lispm-desktop-tty-images--z -1073741825
  "The images' z-index: under every cell with a background of its own.")

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
  "Return a PNG file of icon NAME for COLUMNS by ROWS cells, darkened when
SELECTED, clear round it; made once.  It is as many device pixels as
the cells are, the icon nine tenths of it: a pixel icon at a whole
multiple of its own size, any other drawn at half and doubled.  NAME is
as `lispm-desktop-platinum-icon-file' takes it."
  (let* ((terminal (terminal-parameter nil 'lispm-desktop-tty-images))
         (scale (plist-get terminal :scale))
         (width (round (* scale columns (plist-get terminal :width))))
         (height (round (* scale rows (plist-get terminal :height))))
         (source (lispm-desktop-platinum-icon-file name))
         (fit (floor (* 0.9 (min width height))))
         (own (lispm-desktop-platinum-raster-pixel-size source))
         (size (or own (max 1 (/ fit 2))))
         (factor (if own (max 1 (/ fit own)) 2))
         (key (md5 (prin1-to-string
                    (list source (file-attribute-modification-time (file-attributes source))
                          size factor width height (and selected t)))))
         (file (expand-file-name (concat key ".png") (lispm-desktop-tty-images-directory))))
    (unless (file-exists-p file)
      (when-let* ((pixels (lispm-desktop-platinum-raster source size)))
        (when selected
          (setq pixels (vconcat (mapcar (lambda (row)
                                          (vconcat (mapcar (lambda (pixel)
                                                             (and pixel (lispm-desktop-platinum--darker pixel)))
                                                           row)))
                                        pixels))))
        (make-directory (file-name-directory file) t)
        (let ((coding-system-for-write 'binary))
          (write-region (lispm-desktop-platinum-raster-png pixels factor width height)
                        nil file nil 'silent))))
    (and (file-exists-p file) file)))

;;;; Placements

(defvar lispm-desktop-tty-images--wanted nil
  "The icons shown as images: a list of (KEY FILE WINDOW LINE COLUMN
COLUMNS ROWS), each FILE COLUMNS by ROWS cells from LINE, COLUMN of
WINDOW's text, kept where WINDOW shows the buffer it showed.")

(defvar lispm-desktop-tty-images--placed nil
  "The images placed on the terminal: an alist of KEY to (ID . PLACEMENT).")

(defvar lispm-desktop-tty-images--ids (make-hash-table :test #'equal)
  "The image id of each file sent.")

(defvar lispm-desktop-tty-images--placements (make-hash-table :test #'equal)
  "The placement id of each icon, by its key.")

(defvar lispm-desktop-tty-images--timer nil
  "The timer placing the images after the screen was drawn.")

(defvar lispm-desktop-tty-images--background nil
  "The desktop's colour, which the terminal's default background is
while icons are images.")

(defvar lispm-desktop-tty-images--ground nil
  "The colour the terminal's default background was set to, or nil.")

(defun lispm-desktop-tty-images-show (window icons background)
  "Show ICONS in WINDOW as images, and no other images of WINDOW.
ICONS is a list of (KEY NAME SELECTED LINE COLUMN COLUMNS ROWS), each
icon NAME, darkened when SELECTED, COLUMNS by ROWS cells from LINE,
COLUMN of WINDOW's text, on BACKGROUND, \"#RRGGBB\".  They are placed
once Emacs has drawn the screen."
  (setq lispm-desktop-tty-images--background background)
  (setq lispm-desktop-tty-images--wanted
        (append (seq-remove (lambda (wanted) (eq (nth 2 wanted) window)) lispm-desktop-tty-images--wanted)
                (delq nil
                      (mapcar (pcase-lambda (`(,key ,name ,selected ,line ,column ,columns ,rows))
                                (when-let* ((file (lispm-desktop-tty-images--file
                                                   name columns rows selected)))
                                  (list key file window line column columns rows)))
                              icons))))
  (lispm-desktop-tty-images--schedule))

(defun lispm-desktop-tty-images--schedule ()
  "Place the images once Emacs has drawn the screen."
  (unless (timerp lispm-desktop-tty-images--timer)
    (setq lispm-desktop-tty-images--timer
          (run-at-time 0 nil (lambda ()
                               (setq lispm-desktop-tty-images--timer nil)
                               (lispm-desktop-tty-images--place))))))

(defun lispm-desktop-tty-images--where (window line column columns rows)
  "Return where COLUMNS by ROWS cells from LINE, COLUMN of WINDOW's text
are on the terminal, (ROW . COLUMN) from 1, or nil where WINDOW does
not show them all: a window of the frame on the terminal, the text
from its start."
  (when (and (window-live-p window)
             (eq (window-frame window) (frame-root-frame (selected-frame)))
             (frame-visible-p (window-frame window))
             (= (window-start window) (with-current-buffer (window-buffer window) (point-min)))
             (<= (+ line rows) (window-body-height window))
             (<= (+ column columns) (window-body-width window)))
    (pcase-let ((`(,left ,top ,_ ,_) (window-body-edges window)))
      (cons (+ top line 1) (+ left column 1)))))

(defun lispm-desktop-tty-images--place ()
  "Place the images wanted where their windows show them, and take away
the rest, see `lispm-desktop-tty-images--wanted'."
  (when (and (lispm-desktop-tty-p) (featurep 'kitty-graphics))
    (setq lispm-desktop-tty-images--wanted
          (seq-filter (lambda (wanted) (window-live-p (nth 2 wanted))) lispm-desktop-tty-images--wanted))
    (let* ((placed nil)
           (shown (lispm-desktop-tty-images-p))
           (terminal (terminal-parameter nil 'lispm-desktop-tty-images))
           (cell (and shown (round (* (plist-get terminal :scale) (plist-get terminal :width)))))
           (shift (and shown (plist-get terminal :shift))))
      (kitty-graphics--terminal-send "\e[?2026h")
      (pcase-dolist (`(,key ,file ,window ,line ,column ,columns ,rows) lispm-desktop-tty-images--wanted)
        (when-let* ((shown)
                    (where (lispm-desktop-tty-images--where window line column columns rows)))
          (let ((id (with-memoization (gethash file lispm-desktop-tty-images--ids)
                      (kitty-graphics--alloc-id)))
                (placement (with-memoization (gethash key lispm-desktop-tty-images--placements)
                             (kitty-graphics--alloc-placement-id))))
            (unless (kitty-graphics--transmitted-p id)
              (kitty-graphics--kitty-prepare file id))
            ;; A placement belongs to its image: the icon's other image,
            ;; as before it was selected, goes first.
            (when-let* ((before (alist-get key lispm-desktop-tty-images--placed nil nil #'equal))
                        ((/= (car before) id)))
              (kitty-graphics--delete-placement (car before) (cdr before)))
            (kitty-graphics--terminal-send
             (format "\e7\e[%d;%dH" (car where) (+ (cdr where) (/ shift (max 1 cell)))))
            (kitty-graphics--terminal-send
             (format "\e_Gq=2,a=p,i=%d,p=%d,c=%d,r=%d,z=%d,X=%d,C=1\e\\"
                     id placement columns rows lispm-desktop-tty-images--z (% shift (max 1 cell))))
            (kitty-graphics--terminal-send "\e8")
            (push (cons key (cons id placement)) placed))))
      (pcase-dolist (`(,key . (,id . ,placement)) lispm-desktop-tty-images--placed)
        (unless (assoc key placed)
          (kitty-graphics--delete-placement id placement)))
      (lispm-desktop-tty-images--set-ground (and shown lispm-desktop-tty-images--background))
      (kitty-graphics--terminal-send "\e[?2026l")
      (setq lispm-desktop-tty-images--placed placed))))

(defun lispm-desktop-tty-images--set-ground (color)
  "Make the terminal's default background COLOR, \"#RRGGBB\", or its own
again where COLOR is nil."
  (unless (equal color lispm-desktop-tty-images--ground)
    (send-string-to-terminal
     (if color
         (format "\e]11;rgb:%s/%s/%s\e\\" (substring color 1 3) (substring color 3 5) (substring color 5 7))
       "\e]111\e\\"))
    (setq lispm-desktop-tty-images--ground color)))

(defun lispm-desktop-tty-images--take-away ()
  "Take every image placed off the terminal, and give it back its own
background, as Emacs leaves it."
  (when (and lispm-desktop-tty-images--placed (featurep 'kitty-graphics))
    (pcase-dolist (`(,_ . (,id . ,placement)) lispm-desktop-tty-images--placed)
      (kitty-graphics--delete-placement id placement))
    (setq lispm-desktop-tty-images--placed nil))
  (when (lispm-desktop-tty-p)
    (lispm-desktop-tty-images--set-ground nil)))

(defun lispm-desktop-tty-images--refresh (&rest _)
  "Place the images again: the screen may have been cleared.  This is a
`post-command-hook' and `window-size-change-functions' function."
  (when (or lispm-desktop-tty-images--wanted lispm-desktop-tty-images--placed
            lispm-desktop-tty-images--ground)
    (lispm-desktop-tty-images--schedule)))

(defun lispm-desktop-tty-images-setup ()
  "Show icons as images on a text terminal that can, see
`lispm-desktop-tty-images'.  The terminal is asked once Emacs is idle."
  (when (lispm-desktop-tty-p)
    (run-with-idle-timer 0.2 nil #'lispm-desktop-tty-images--check)
    (add-hook 'post-command-hook #'lispm-desktop-tty-images--refresh)
    (add-hook 'window-size-change-functions #'lispm-desktop-tty-images--refresh)
    (add-hook 'suspend-hook #'lispm-desktop-tty-images--take-away)
    (add-hook 'suspend-resume-hook #'lispm-desktop-tty-images--refresh)
    (add-hook 'kill-emacs-hook #'lispm-desktop-tty-images--take-away)))

(provide 'lispm-desktop-tty-images)
;;; lispm-desktop-tty-images.el ends here
