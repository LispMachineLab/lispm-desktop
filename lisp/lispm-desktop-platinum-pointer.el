;;; lispm-desktop-platinum-pointer.el --- Mac OS 9's watch pointer  -*- lexical-binding: t; -*-

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

;; The pointer Mac OS 9 showed while it worked: a wristwatch, ten pixels
;; by sixteen, its minute hand going round in eight steps while the hour
;; hand stays at nine.  It keeps its black and white whatever the
;; appearance, as Mac OS 9's pointers did, and has no outline: it sits
;; on what is under it as it was.  The frames are rows of pixels, "#"
;; black, "." white and " " clear, for whatever draws the pointer.

;;; Code:

(require 'subr-x)

(defconst lispm-desktop-platinum-watch-case
  '("  ######  "
    "  ######  "
    "  ######  "
    "  ######  "
    " #......# "
    "#........#"
    "#........#"
    "#........#"
    "#..###...#"
    "#........#"
    "#........#"
    " #......# "
    "  ######  "
    "  ######  "
    "  ######  "
    "  ######  ")
  "The watch pointer without its minute hand: the strap above and below,
the case, and the hour hand at nine, which meets the minute hand at the
sixth pixel of the ninth row.")

(defconst lispm-desktop-platinum-watch-hands
  '(((5 . 5) (5 . 6) (5 . 7))           ; twelve
    ((6 . 7) (7 . 6))                   ; half past one
    ((6 . 8) (7 . 8))                   ; three
    ((6 . 9))                           ; half past four
    ((5 . 9) (5 . 10))                  ; six
    ((4 . 9))                           ; half past seven
    ()                                  ; nine, under the hour hand
    ((4 . 7) (3 . 6)))                  ; half past ten
  "The minute hand of each frame of the watch, its black pixels as (X . Y)
from the top left corner, round from twelve.  Twelve to six are Mac OS
9.0.4's own, read off its screen; the rest mirror them.")

(defconst lispm-desktop-platinum-watch-hot-spot '(8 . 8)
  "Where the watch points, (X . Y) from its top left corner: Mac OS 9's
spot, eight pixels in from the case's left edge and down from the top of
the strap.")

(defun lispm-desktop-platinum-watch-frames ()
  "Return the frames of the watch pointer, round from twelve, each a string
of rows: \"#\" black, \".\" white, \" \" clear."
  (mapcar (lambda (hand)
            (let ((rows (mapcar #'copy-sequence lispm-desktop-platinum-watch-case)))
              (pcase-dolist (`(,x . ,y) hand)
                (aset (nth y rows) x ?#))
              (string-join rows "\n")))
          lispm-desktop-platinum-watch-hands))

(provide 'lispm-desktop-platinum-pointer)
;;; lispm-desktop-platinum-pointer.el ends here
