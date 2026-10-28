;;; lispm-desktop-choose.el --- Choosing a file, in the desktop's own dialog  -*- lexical-binding: t; -*-

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

;; Where LispM asks for a file, as a form asks for a character's picture,
;; the desktop shows a dialog of its own, as Mac OS 9's Navigation
;; Services showed one: a movable modal window over the window asking,
;; titled with what is asked.  At its top, a pop-up menu of the folder
;; shown and the folders holding it, and buttons popping up the usual
;; places and the files chosen lately; under them the folder's folders
;; and the files that may be chosen, in a list sorted by name or by date,
;; a scroll bar beside it; beside the list, a preview of the picture
;; selected; at the bottom, Cancel, and the default button, which opens
;; the folder selected or chooses the file.
;;
;;   folder --items, sorted--> list --selected--> preview --Choose--> file
;;
;; It is drawn as one image in a child frame and reads events in a loop
;; of its own until a file is chosen or it is cancelled, as the desktop's
;; alerts do (lispm-ui-modal.el): it waits for the answer as
;; `read-file-name' does, and never hands the choice to the system's own
;; panel, which an application shown on its own would hide.
;;
;; Keys, all on the HHKB: C-n and C-p, or the arrows, select; C-v and
;; M-v go a list's height down and up, M-< and M-> to the ends; Return
;; opens the folder selected or chooses the file, C-f opens a folder too;
;; C-b or Delete goes up to the folder holding this one, ~ home; / pops
;; up the usual places; typing a name's first letters selects it; Escape
;; or C-g cancels.  The mouse selects, opens with a double click, scrolls
;; with the wheel and the scroll bar, and sorts by the column whose
;; heading it presses.  Where no dialog can be drawn, as on a text
;; terminal, the file is read in the minibuffer.

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

(defconst lispm-desktop-choose-width 450
  "Width in pixels of the dialog choosing a file, its outline included.")

(defconst lispm-desktop-choose-rows 7
  "How many rows of the dialog's list show at once.")

(defconst lispm-desktop-choose-row-height 20
  "Height in pixels of a row of the dialog's list, the white line under it
included, as Mac OS 9's lists drew them.")

(defconst lispm-desktop-choose-preview-limit (* 16 1024 1024)
  "Size in bytes of the largest picture the dialog previews.")

(defvar lispm-desktop-choose-recent nil
  "The files chosen in the dialog choosing a file, the last first.")

(defconst lispm-desktop-choose-recent-count 10
  "How many files the dialog's Recent menu keeps.")

(with-eval-after-load 'savehist
  (defvar savehist-additional-variables)
  (add-to-list 'savehist-additional-variables 'lispm-desktop-choose-recent))

(cl-defstruct (lispm-desktop-choose--dialog
               (:constructor lispm-desktop-choose--make-dialog)
               (:copier nil))
  "A dialog choosing a file, as it stands.
ITEMS are the folder's, each (NAME FILE FOLDER TIME), in the order
shown; SELECTED is the index of the one selected, TOP of the first
row shown; SORT is `name' or `date', DESCENDING turns its order over;
PRESSED names the control held down; TYPED and TYPED-AT are the
letters typed to select a name and when the last was; PICTURES keeps
the pictures previewed."
  title predicate preview directory items selected (top 0) (sort 'name) descending
  pressed (typed "") (typed-at 0) (pictures (make-hash-table :test #'equal)))

;;;; The folder's items

(defun lispm-desktop-choose--read (directory predicate)
  "Return the items of DIRECTORY the dialog lists, as (NAME FILE FOLDER TIME):
its folders and the files PREDICATE, a function of a file's name, takes,
or all of them for nil; those whose names start with a dot are hidden,
as Finder hid them."
  (let ((items nil))
    (dolist (entry (ignore-errors (directory-files-and-attributes directory nil "\\`[^.]" t)))
      (let* ((file (expand-file-name (car entry) directory))
             (folder (file-directory-p file)))
        (when (or folder (null predicate) (funcall predicate file))
          (push (list (car entry) file folder (file-attribute-modification-time (cdr entry)))
                items))))
    items))

(defun lispm-desktop-choose--item (dialog)
  "Return DIALOG's item selected, or nil."
  (when-let* ((index (lispm-desktop-choose--dialog-selected dialog)))
    (nth index (lispm-desktop-choose--dialog-items dialog))))

(defun lispm-desktop-choose--select (dialog index)
  "Select DIALOG's item INDEX, kept among its items, or none for nil,
scrolled into view."
  (let ((count (length (lispm-desktop-choose--dialog-items dialog))))
    (setf (lispm-desktop-choose--dialog-selected dialog)
          (and index (> count 0) (max 0 (min index (1- count)))))
    (when-let* ((selected (lispm-desktop-choose--dialog-selected dialog)))
      (let ((top (lispm-desktop-choose--dialog-top dialog)))
        (cond ((< selected top) (setq top selected))
              ((>= selected (+ top lispm-desktop-choose-rows))
               (setq top (- selected lispm-desktop-choose-rows -1))))
        (lispm-desktop-choose--scroll-to dialog top)))))

(defun lispm-desktop-choose--scroll-to (dialog top)
  "Show DIALOG's list from row TOP, kept within its items."
  (setf (lispm-desktop-choose--dialog-top dialog)
        (max 0 (min top (- (length (lispm-desktop-choose--dialog-items dialog))
                           lispm-desktop-choose-rows)))))

(defun lispm-desktop-choose--sort (dialog)
  "Sort DIALOG's items as it says, the item selected staying selected:
by name, A first whatever its case, the numbers in names counted, or
by date, the latest first; DESCENDING the other way."
  (let* ((selected (lispm-desktop-choose--item dialog))
         (items (sort (copy-sequence (lispm-desktop-choose--dialog-items dialog))
                      (if (eq (lispm-desktop-choose--dialog-sort dialog) 'date)
                          (lambda (a b) (time-less-p (nth 3 b) (nth 3 a)))
                        (lambda (a b) (string-version-lessp (downcase (car a)) (downcase (car b))))))))
    (setf (lispm-desktop-choose--dialog-items dialog)
          (if (lispm-desktop-choose--dialog-descending dialog) (nreverse items) items))
    (lispm-desktop-choose--select dialog (and selected (seq-position (lispm-desktop-choose--dialog-items dialog)
                                                                    selected #'eq)))))

(defun lispm-desktop-choose--go (dialog directory &optional name)
  "Show DIRECTORY in DIALOG, the item NAME selected, else the first."
  (let ((directory (file-name-as-directory (expand-file-name directory))))
    (setf (lispm-desktop-choose--dialog-directory dialog) directory
          (lispm-desktop-choose--dialog-items dialog)
          (lispm-desktop-choose--read directory (lispm-desktop-choose--dialog-predicate dialog))
          (lispm-desktop-choose--dialog-selected dialog) nil
          (lispm-desktop-choose--dialog-top dialog) 0)
    (lispm-desktop-choose--sort dialog)
    (lispm-desktop-choose--select
     dialog (or (and name (seq-position (lispm-desktop-choose--dialog-items dialog) name
                                        (lambda (item name) (equal (car item) name))))
                0))))

(defun lispm-desktop-choose--parent (directory)
  "Return the folder holding DIRECTORY, or nil at the top."
  (let ((parent (file-name-directory (directory-file-name directory))))
    (unless (or (null parent) (equal parent directory)) parent)))

(defun lispm-desktop-choose--up (dialog)
  "Go up to the folder holding the one DIALOG shows, this one selected."
  (let ((directory (lispm-desktop-choose--dialog-directory dialog)))
    (when-let* ((parent (lispm-desktop-choose--parent directory)))
      (lispm-desktop-choose--go dialog parent (file-name-nondirectory (directory-file-name directory))))))

(defun lispm-desktop-choose--open (dialog)
  "Open DIALOG's item selected: go into a folder; return a file, chosen."
  (when-let* ((item (lispm-desktop-choose--item dialog)))
    (if (nth 2 item)
        (progn (lispm-desktop-choose--go dialog (nth 1 item)) nil)
      (nth 1 item))))

(defun lispm-desktop-choose--type (dialog char)
  "Select DIALOG's first item whose name starts with the letters typed, CHAR
the last: letters typed within a second of each other make one name."
  (let ((now (float-time)))
    (setf (lispm-desktop-choose--dialog-typed dialog)
          (concat (if (< (- now (lispm-desktop-choose--dialog-typed-at dialog)) 1.0)
                      (lispm-desktop-choose--dialog-typed dialog)
                    "")
                  (downcase (char-to-string char)))
          (lispm-desktop-choose--dialog-typed-at dialog) now)
    (let ((typed (lispm-desktop-choose--dialog-typed dialog)))
      (when-let* ((index (seq-position (lispm-desktop-choose--dialog-items dialog) typed
                                       (lambda (item typed) (string-prefix-p typed (downcase (car item)))))))
        (lispm-desktop-choose--select dialog index)))))

(defun lispm-desktop-choose--places ()
  "Return the usual places, which the Shortcuts button pops up: (NAME . FOLDER)
for each that is there."
  (seq-filter (lambda (place) (file-directory-p (cdr place)))
              (list (cons (lispm-desktop-tr "Desktop") "~/Desktop/")
                    (cons (lispm-desktop-tr "Home") "~/")
                    (cons (lispm-desktop-tr "Documents") "~/Documents/")
                    (cons (lispm-desktop-tr "Downloads") "~/Downloads/")
                    (cons (lispm-desktop-tr "Pictures") "~/Pictures/"))))

(defun lispm-desktop-choose--recent (dialog)
  "Return the files chosen lately that DIALOG may choose, the last first."
  (let ((predicate (lispm-desktop-choose--dialog-predicate dialog)))
    (seq-filter (lambda (file) (and (file-regular-p file) (or (null predicate) (funcall predicate file))))
                lispm-desktop-choose-recent)))

(defun lispm-desktop-choose--remember (file)
  "Put FILE first among the files chosen lately."
  (setq lispm-desktop-choose-recent
        (seq-take (cons file (delete file lispm-desktop-choose-recent)) lispm-desktop-choose-recent-count)))

(defun lispm-desktop-choose--date (time)
  "Return TIME as the list's Date Modified column shows it."
  (let ((days (- (time-to-days nil) (time-to-days time))))
    (cond ((= days 0) (lispm-desktop-tr "Today"))
          ((= days 1) (lispm-desktop-tr "Yesterday"))
          (t (lispm-desktop-format-time "%-m/%-d/%y" time)))))

;;;; Keys

(defun lispm-desktop-choose--key (dialog event)
  "Do what EVENT, a key, does in DIALOG.
Return the file chosen, `cancel', or nil while the dialog stays."
  (let ((rows lispm-desktop-choose-rows)
        (selected (or (lispm-desktop-choose--dialog-selected dialog) -1)))
    (pcase (lispm-ui-modal-role event)
      ('cancel 'cancel)
      ('confirm (lispm-desktop-choose--open dialog))
      ('down (lispm-desktop-choose--select dialog (1+ selected)) nil)
      ('up (lispm-desktop-choose--select dialog (max 0 (1- selected))) nil)
      ('right (when (nth 2 (lispm-desktop-choose--item dialog))
                (lispm-desktop-choose--open dialog)))
      ((or 'left 'back) (lispm-desktop-choose--up dialog) nil)
      (_
       (pcase event
         ((or ?\C-v 'next) (lispm-desktop-choose--select dialog (+ (max selected 0) rows)) nil)
         ((or ?\M-v 'prior) (lispm-desktop-choose--select dialog (max 0 (- selected rows))) nil)
         ((or ?\M-< 'home) (lispm-desktop-choose--select dialog 0) nil)
         ((or ?\M-> 'end)
          (lispm-desktop-choose--select dialog (1- (length (lispm-desktop-choose--dialog-items dialog))))
          nil)
         (?~ (lispm-desktop-choose--go dialog "~/") nil)
         ((pred characterp)
          (when (and (>= event ?\s) (< event 127) (not (eq event ?/)))
            (lispm-desktop-choose--type dialog event))
          nil))))))

;;;; Layout

(defun lispm-desktop-choose--button-labels (dialog)
  "Return the labels of DIALOG's buttons, (CANCEL . DEFAULT): the default
button opens a folder selected and chooses a file."
  (cons (lispm-desktop-tr "Cancel")
        (if (nth 2 (lispm-desktop-choose--item dialog))
            (lispm-desktop-tr "Open")
          (lispm-desktop-tr "Choose"))))

(defun lispm-desktop-choose--layout ()
  "Return where the dialog draws its parts, a plist of (X Y WIDTH HEIGHT)
in pixels from its outline's top left, and its :width and :height.
They stand as Navigation Services stood them: the pop-up menu of folders
at the top left, the Shortcuts and Recent buttons at the top right, the
list under them, its :header, :rows and :scroll bar, the :preview
beside it, and the buttons at the bottom right, the default
\(:choose) last, its ring outside its place."
  (let* ((w lispm-desktop-choose-width)
         (body (1+ lispm-desktop-platinum-title-bar-height))
         (list-y (+ body 41))
         (list-w 260)
         (rows-h (* lispm-desktop-choose-rows lispm-desktop-choose-row-height))
         (list-h (+ 1 lispm-desktop-platinum-list-header-height rows-h 1))
         (rows-y (+ list-y 1 lispm-desktop-platinum-list-header-height))
         (pane-x (+ 15 list-w 5))
         (right (- w 15))
         (buttons-y (+ list-y list-h 16))
         (labels (list (lispm-desktop-tr "Cancel") (lispm-desktop-tr "Open") (lispm-desktop-tr "Choose")))
         (choose-w (max (lispm-desktop-platinum-push-button-width (nth 1 labels))
                        (lispm-desktop-platinum-push-button-width (nth 2 labels))))
         (cancel-w (lispm-desktop-platinum-push-button-width (nth 0 labels)))
         (choose-x (- right 3 choose-w)))
    (list :width w :height (+ buttons-y lispm-desktop-platinum-button-height 15)
          :location (list 17 (+ body 13) 193 lispm-desktop-platinum-button-height)
          :shortcuts (list (- right 31 4 31) (+ body 10) 31 27)
          :recent (list (- right 31) (+ body 10) 31 27)
          :list (list 15 list-y list-w list-h)
          :header (list 16 (1+ list-y) (- list-w 2) lispm-desktop-platinum-list-header-height)
          :rows (list 16 rows-y (- list-w 2 lispm-desktop-platinum-scroll-width) rows-h)
          :date 96
          :scroll (list (- (+ 15 list-w) lispm-desktop-platinum-scroll-width) rows-y
                        lispm-desktop-platinum-scroll-width (1+ rows-h))
          :preview (list pane-x list-y (- right pane-x) list-h)
          :cancel (list (- choose-x 3 lispm-desktop-popup-button-gap cancel-w) buttons-y cancel-w
                        lispm-desktop-platinum-button-height)
          :choose (list choose-x buttons-y choose-w lispm-desktop-platinum-button-height))))

(defun lispm-desktop-choose--hit (dialog layout x y)
  "Return what of DIALOG, laid out as LAYOUT says, is at X, Y: `location',
`shortcuts', `recent', `cancel' or `choose'; (header . COLUMN), COLUMN
`name', `date' or `direction'; (row . INDEX) of an item; (scroll .
PART), PART `up', `down', `page-up', `page-down' or `thumb'; or nil."
  (cl-flet ((in (part) (lispm-ui-modal-inside-p (plist-get layout part) x y)))
    (cond
     ((in :location) 'location)
     ((in :shortcuts) 'shortcuts)
     ((in :recent) 'recent)
     ((in :cancel) 'cancel)
     ((in :choose) 'choose)
     ((in :header)
      (pcase-let ((`(,left ,_ ,width ,_) (plist-get layout :header)))
        (cons 'header (cond ((>= x (- (+ left width) 16)) 'direction)
                            ((>= x (- (+ left width) 16 (plist-get layout :date))) 'date)
                            (t 'name)))))
     ((in :rows)
      (let ((index (+ (lispm-desktop-choose--dialog-top dialog)
                      (/ (- y (nth 1 (plist-get layout :rows))) lispm-desktop-choose-row-height))))
        (when (< index (length (lispm-desktop-choose--dialog-items dialog)))
          (cons 'row index))))
     ((in :scroll)
      (pcase-let* ((`(,_ ,top ,_ ,height) (plist-get layout :scroll))
                   (bar (lispm-desktop-platinum-scroll-layout height))
                   (at (- y top))
                   (thumb (lispm-desktop-choose--thumb dialog height)))
        (cons 'scroll
              (cond ((>= at (plist-get bar :down)) 'down)
                    ((>= at (plist-get bar :up)) 'up)
                    ((null thumb) nil)
                    ((< at (car thumb)) 'page-up)
                    ((>= at (+ (car thumb) (cdr thumb))) 'page-down)
                    (t 'thumb))))))))

(defun lispm-desktop-choose--thumb (dialog height)
  "Return the thumb of DIALOG's scroll bar HEIGHT tall, see
`lispm-desktop-platinum-scroll-thumb', or nil when every item shows."
  (let ((top (lispm-desktop-choose--dialog-top dialog)))
    (lispm-desktop-platinum-scroll-thumb height top (+ top lispm-desktop-choose-rows)
                                         0 (length (lispm-desktop-choose--dialog-items dialog)))))

;;;; Drawing

(defun lispm-desktop-choose--picture (dialog file)
  "Return the picture in FILE as DIALOG previews it, (DATA TYPE WIDTH
HEIGHT), DATA its bytes, or nil where it cannot be shown.  Each is read
once while the dialog is open."
  (let ((pictures (lispm-desktop-choose--dialog-pictures dialog)))
    (with-memoization (gethash file pictures)
      (ignore-errors
        (when (and (display-images-p)
                   (< (or (file-attribute-size (file-attributes file)) 0) lispm-desktop-choose-preview-limit))
          (let* ((type (image-supported-file-p file))
                 (size (and type (image-size (create-image file type) t))))
            (when size
              (list (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (insert-file-contents-literally file)
                      (buffer-string))
                    type (car size) (cdr size)))))))))

(defun lispm-desktop-choose--draw-picture (svg dialog rect palette)
  "Draw on SVG the preview of DIALOG's item selected in RECT, the pane's
inside, and its size under it, if it is a picture."
  (when-let* (((lispm-desktop-choose--dialog-preview dialog))
              (item (lispm-desktop-choose--item dialog))
              ((not (nth 2 item)))
              (picture (lispm-desktop-choose--picture dialog (nth 1 item))))
    (pcase-let* ((`(,data ,type ,pw ,ph) picture)
                 (`(,x ,y ,w ,h) rect)
                 (room-h (- h 22))
                 (scale (min 1.0 (/ (float w) pw) (/ (float room-h) ph)))
                 (iw (max 1 (round (* pw scale))))
                 (ih (max 1 (round (* ph scale))))
                 (ix (+ x (/ (- w iw) 2)))
                 (iy (+ y (/ (- room-h ih) 2))))
      (svg-embed svg data (format "image/%s" (pcase type ('svg "svg+xml") (_ type))) t
                 :x ix :y iy :width iw :height ih)
      (lispm-ui-svg-content svg (format "%d × %d" pw ph)
                            (+ x (/ (- w (lispm-ui-text-width
                                          (format "%d × %d" pw ph)
                                          'lispm-ui-small))
                                    2))
                            (+ y room-h 4) 16 (plist-get palette :muted)
                            'lispm-ui-small))))

(defun lispm-desktop-choose--draw-title (svg width title palette)
  "Draw on SVG a movable modal window's title bar WIDTH wide reading TITLE,
inside the outline: ridges either side of the title, and no boxes."
  (let* ((label (let ((label (copy-sequence title)))
                  (add-face-text-property 0 (length label) 'lispm-ui-title t label)
                  label))
         (layout (lispm-desktop-platinum-title-layout
                  width (lispm-ui-text-width label 'lispm-ui-title)
                  '(nil nil nil)))
         (label (lispm-desktop-platinum-fit label (plist-get layout :title))))
    (lispm-ui-svg-embed
     svg (lispm-desktop-platinum-draw-title-part palette width :active t :lead t :trail t
                                                 :ridges (plist-get layout :ridges)
                                                 :label label :label-x (plist-get layout :label-x))
     1 1 width lispm-desktop-platinum-title-bar-height)))

(defun lispm-desktop-choose--draw-location (svg dialog rect palette)
  "Draw on SVG in RECT the pop-up menu of the folder DIALOG shows: the
folder's icon and name, and the menu's arrows."
  (pcase-let* ((`(,x ,y ,w ,_) rect)
               (state (and (eq (lispm-desktop-choose--dialog-pressed dialog) 'location) 'pressed))
               (directory (lispm-desktop-choose--dialog-directory dialog))
               (name (or (file-name-nondirectory (directory-file-name directory)) ""))
               (label (concat (lispm-desktop-platinum-icon "folder") " "
                              (lispm-desktop-platinum-fit
                               (propertize (if (string-empty-p name) "/" name) 'face 'lispm-ui-title)
                               (- w 52)))))
    (lispm-ui-svg-embed svg (lispm-desktop-platinum-draw-popup-button palette "" w :state state)
                        x y w lispm-desktop-platinum-button-height)
    (lispm-ui-svg-content svg label (+ x 8) y lispm-desktop-platinum-button-height
                          (plist-get (lispm-desktop-platinum--push-button-colors palette state) :label)
                          'lispm-ui-title)))

(defun lispm-desktop-choose--draw-place-button (svg rect palette state kind)
  "Draw on SVG in RECT the bevel button of KIND, `shortcuts' or `recent', in
STATE: a disk for the usual places, a clock for the files chosen lately,
and the small arrow of a button popping up a menu."
  (pcase-let* ((`(,x ,y ,w ,h) rect)
               (ink (if (eq state 'disabled)
                        (lispm-ui-grey palette #x99)
                      (plist-get palette :outline))))
    (lispm-ui-svg-embed svg (lispm-desktop-platinum-draw-bevel-button palette w h :state state)
                        x y w h)
    (if (eq kind 'shortcuts)
        (lispm-ui-svg-icon svg (lispm-desktop-platinum-icon-file "disk") (+ x 5) (+ y 5) 16)
      (let ((cx (+ x 13)) (cy (+ y 13)))
        (svg-circle svg cx cy 6.5 :fill (if (eq state 'disabled)
                                             (lispm-ui-grey palette #xDD)
                                           (lispm-ui-grey palette #xFF))
                    :stroke ink)
        (svg-line svg cx cy cx (- cy 4) :stroke ink)
        (svg-line svg cx cy (+ cx 3) cy :stroke ink)))
    (svg-polygon svg (list (cons (+ x w -9) (+ y h -8)) (cons (+ x w -4) (+ y h -8))
                           (cons (+ x w -6.5) (+ y h -5.5)))
                 :fill ink)))

(defun lispm-desktop-choose--draw-list (svg dialog layout palette)
  "Draw on SVG DIALOG's list as LAYOUT places it: its headings, its rows,
the sorted column darker, the item selected in reverse, and its scroll
bar."
  (pcase-let* ((grey (lambda (level) (lispm-ui-grey palette level)))
               (`(,lx ,ly ,lw ,lh) (plist-get layout :list))
               (`(,hx ,hy ,hw ,hh) (plist-get layout :header))
               (`(,rx ,ry ,rw ,_) (plist-get layout :rows))
               (`(,sx ,sy ,sw ,sh) (plist-get layout :scroll))
               (date-w (plist-get layout :date))
               (name-w (- rw date-w))
               (by-date (eq (lispm-desktop-choose--dialog-sort dialog) 'date))
               (items (lispm-desktop-choose--dialog-items dialog))
               (top (lispm-desktop-choose--dialog-top dialog))
               (text (plist-get palette :text)))
    (lispm-ui-svg-fill svg lx ly lw lh (plist-get palette :outline))
    (cl-flet ((heading (label width)
                ;; A heading longer than its column is cut short, as a
                ;; longer language's may be.
                (lispm-desktop-platinum-fit (propertize label 'face 'lispm-ui-list) width)))
      (lispm-ui-svg-embed
       svg (lispm-desktop-platinum-draw-list-header
            palette (list (list (heading (lispm-desktop-tr "Name") (- name-w 34)) name-w (not by-date) 28)
                          (list (heading (lispm-desktop-tr "Date Modified") (- date-w 10)) date-w by-date 6)))
       hx hy hw hh))
    (dotimes (row lispm-desktop-choose-rows)
      (let ((y (+ ry (* row lispm-desktop-choose-row-height)))
            (index (+ top row)))
        (lispm-ui-svg-fill svg rx y name-w (1- lispm-desktop-choose-row-height)
                           (funcall grey (if by-date #xEE #xDD)))
        (lispm-ui-svg-fill svg (+ rx name-w) y date-w (1- lispm-desktop-choose-row-height)
                           (funcall grey (if by-date #xDD #xEE)))
        (lispm-ui-svg-row svg rx (+ rx rw -1) (+ y lispm-desktop-choose-row-height -1)
                          (funcall grey #xFF))
        (when-let* ((item (nth index items)))
          (pcase-let* ((`(,name ,file ,folder ,time) item)
                       (selected (eql index (lispm-desktop-choose--dialog-selected dialog)))
                       (label (lispm-desktop-platinum-fit (propertize name 'face 'lispm-ui-list)
                                                          (- name-w 34)))
                       (label-w (lispm-ui-text-width label 'lispm-ui-list)))
            (lispm-ui-svg-icon
             svg (lispm-desktop-platinum-icon-file (if folder "folder" (lispm-desktop-platinum-file-icon-name file)))
             (+ rx 8) (+ y 2) 16)
            (when selected
              (lispm-ui-svg-fill svg (+ rx 26) (+ y 2) (+ label-w 4) 15 (plist-get palette :outline)))
            (lispm-ui-svg-content svg label (+ rx 28) y (1- lispm-desktop-choose-row-height)
                                  (if selected (funcall grey #xFF) text)
                                  'lispm-ui-list)
            (lispm-ui-svg-content svg (lispm-desktop-choose--date time) (+ rx name-w 6) y
                                  (1- lispm-desktop-choose-row-height) text
                                  'lispm-ui-list)))))
    (lispm-ui-svg-embed
     svg (lispm-desktop-platinum-draw-scroll-bar palette sh (lispm-desktop-choose--thumb dialog sh) t)
     sx sy sw sh)))

(defun lispm-desktop-choose-draw (dialog palette)
  "Return an SVG of DIALOG in PALETTE, laid out by `lispm-desktop-choose--layout'.
It is drawn as Mac OS 9 drew a movable modal window: outlined, its title
bar ridged either side of its title and holding no boxes, its body
platinum, lit along its top and left inside and shaded along its bottom
and right; the controls as Navigation Services drew them."
  (let* ((layout (lispm-desktop-choose--layout))
         (w (plist-get layout :width))
         (h (plist-get layout :height))
         (svg (svg-create w h))
         (grey (lambda (level) (lispm-ui-grey palette level)))
         (pressed (lispm-desktop-choose--dialog-pressed dialog))
         (labels (lispm-desktop-choose--button-labels dialog))
         (body (1+ lispm-desktop-platinum-title-bar-height)))
    (lispm-ui-svg-fill svg 0 0 w h (plist-get palette :outline))
    (lispm-ui-svg-fill svg 1 1 (- w 2) (- h 2) (funcall grey #xDD))
    (lispm-desktop-choose--draw-title svg (- w 2) (lispm-desktop-choose--dialog-title dialog) palette)
    (lispm-ui-svg-row svg 2 (- w 4) (1+ body) (funcall grey #xFF))
    (lispm-ui-svg-column svg 2 (1+ body) (- h 4) (funcall grey #xFF))
    (lispm-ui-svg-row svg 3 (- w 3) (- h 3) (funcall grey #x99))
    (lispm-ui-svg-column svg (- w 3) (+ body 2) (- h 3) (funcall grey #x99))
    (lispm-desktop-choose--draw-location svg dialog (plist-get layout :location) palette)
    (lispm-desktop-choose--draw-place-button svg (plist-get layout :shortcuts) palette
                                             (and (eq pressed 'shortcuts) 'pressed) 'shortcuts)
    (lispm-desktop-choose--draw-place-button svg (plist-get layout :recent) palette
                                             (cond ((eq pressed 'recent) 'pressed)
                                                   ((null (lispm-desktop-choose--recent dialog)) 'disabled))
                                             'recent)
    (lispm-desktop-choose--draw-list svg dialog layout palette)
    ;; The preview pane, sunk into the body.
    (pcase-let ((`(,x ,y ,pw ,ph) (plist-get layout :preview)))
      (lispm-ui-svg-row svg x (+ x pw -2) y (funcall grey #x88))
      (lispm-ui-svg-column svg x y (+ y ph -2) (funcall grey #x88))
      (lispm-ui-svg-row svg (1+ x) (+ x pw -1) (+ y ph -1) (funcall grey #xFF))
      (lispm-ui-svg-column svg (+ x pw -1) (1+ y) (+ y ph -1) (funcall grey #xFF))
      (lispm-desktop-choose--draw-picture svg dialog (list (+ x 8) (+ y 8) (- pw 16) (- ph 16)) palette))
    (pcase-dolist (`(,part ,label ,default) `((:cancel ,(car labels) nil) (:choose ,(cdr labels) t)))
      (pcase-let ((`(,x ,y ,bw ,_) (plist-get layout part)))
        (lispm-desktop-platinum-svg-push-button
         svg x y palette label bw
         :default default
         :state (cond ((eq pressed (intern (substring (symbol-name part) 1))) 'pressed)
                      ((and default (null (lispm-desktop-choose--item dialog))) 'disabled)))))
    svg))

(defun lispm-desktop-choose--show (frame dialog)
  "Show DIALOG in its popup FRAME, drawn anew."
  (lispm-desktop-popup-fill
   frame (propertize " " 'display (svg-image (lispm-desktop-choose-draw dialog lispm-ui-current-palette)
                                             :ascent 100 :scale 1)))
  (redisplay t))

;;;; The mouse

(defun lispm-desktop-choose--at (event frame dialog)
  "Return what of DIALOG, shown in FRAME, mouse EVENT is over, see
`lispm-desktop-choose--hit'; nil when it is not over FRAME."
  (when-let* ((position (lispm-ui-modal-over event frame))
              (xy (posn-x-y position)))
    (lispm-desktop-choose--hit dialog (lispm-desktop-choose--layout) (car xy) (cdr xy))))

(defun lispm-desktop-choose--hold (frame dialog part)
  "Hold PART of DIALOG, shown in FRAME, pressed while the mouse stays down
over it; return non-nil when it rose over it."
  (lispm-ui-modal-hold
   (lambda (event) (equal (lispm-desktop-choose--at event frame dialog) part))
   (lambda (down)
     (setf (lispm-desktop-choose--dialog-pressed dialog) (and down part))
     (lispm-desktop-choose--show frame dialog))))

(defun lispm-desktop-choose--menu (frame dialog part items)
  "Pop up ITEMS under PART of DIALOG, shown in FRAME, held pressed while the
menu is open; return the item chosen, or nil."
  (pcase-let ((`(,x ,y ,_ ,h) (plist-get (lispm-desktop-choose--layout) part))
              (`(,fx . ,fy) (frame-position frame)))
    (setf (lispm-desktop-choose--dialog-pressed dialog) (intern (substring (symbol-name part) 1)))
    (lispm-desktop-choose--show frame dialog)
    (unwind-protect
        (lispm-desktop-popup-choose items (+ fx x) (+ fy y h))
      (setf (lispm-desktop-choose--dialog-pressed dialog) nil))))

(defun lispm-desktop-choose--folder-icon ()
  "Return the image of a folder's small icon, for a menu item."
  (create-image (lispm-desktop-platinum-icon-file "folder") nil nil :ascent 'center))

(defun lispm-desktop-choose--path-items (dialog)
  "Return the menu of the folders holding the one DIALOG shows, it first."
  (let ((folders nil)
        (directory (lispm-desktop-choose--dialog-directory dialog)))
    (while directory
      (push directory folders)
      (setq directory (lispm-desktop-choose--parent directory)))
    (mapcar (lambda (folder)
              (let ((name (file-name-nondirectory (directory-file-name folder))))
                (list :label (if (string-empty-p name) "/" name) :value folder
                      :icon (lispm-desktop-choose--folder-icon))))
            (nreverse folders))))

(defun lispm-desktop-choose--shortcut (frame dialog &optional keyboard)
  "Pop up the usual places under DIALOG's Shortcuts button, in FRAME, and go
to the one chosen.  KEYBOARD says the keyboard asked: nothing is held."
  (when-let* ((item (if keyboard
                        (pcase-let ((`(,x ,y ,_ ,h) (plist-get (lispm-desktop-choose--layout) :shortcuts))
                                    (`(,fx . ,fy) (frame-position frame)))
                          (lispm-desktop-popup-choose (lispm-desktop-choose--place-items) (+ fx x) (+ fy y h)))
                      (lispm-desktop-choose--menu frame dialog :shortcuts
                                                  (lispm-desktop-choose--place-items)))))
    (lispm-desktop-choose--go dialog (plist-get item :value))))

(defun lispm-desktop-choose--place-items ()
  "Return the menu of the usual places."
  (mapcar (pcase-lambda (`(,name . ,folder))
            (list :label name :value folder :icon (lispm-desktop-choose--folder-icon)))
          (lispm-desktop-choose--places)))

(defun lispm-desktop-choose--drag-thumb (frame dialog grab)
  "Drag DIALOG's scroll bar thumb, in FRAME, held GRAB pixels below its top,
the list following it, until the mouse rises."
  (pcase-let* ((`(,_ ,top ,_ ,height) (plist-get (lispm-desktop-choose--layout) :scroll))
               (track (plist-get (lispm-desktop-platinum-scroll-layout height) :track-length))
               (count (length (lispm-desktop-choose--dialog-items dialog)))
               (room (- count lispm-desktop-choose-rows)))
    (catch 'done
      (while t
        (let ((event (lispm-ui-modal-read)))
          (cond ((or (lispm-ui-modal-mouse-up-p event) (lispm-ui-modal-cancel-p event))
                 (throw 'done nil))
                ((mouse-movement-p event)
                 (when-let* ((position (lispm-ui-modal-over event frame))
                             (y (cdr (posn-x-y position)))
                             (thumb (lispm-desktop-choose--thumb dialog height)))
                   (let ((free (max 1 (- track (cdr thumb)))))
                     (lispm-desktop-choose--scroll-to
                      dialog (round (* room (/ (float (- y top grab)) free))))
                     (lispm-desktop-choose--show frame dialog))))))))))

(defun lispm-desktop-choose--click (frame dialog event)
  "Do what mouse EVENT, a press, does in DIALOG, shown in FRAME.
Return the file chosen, `cancel', or nil while the dialog stays."
  (let ((double (memq 'double (event-modifiers event))))
    (pcase (lispm-desktop-choose--at event frame dialog)
      ('cancel (and (lispm-desktop-choose--hold frame dialog 'cancel) 'cancel))
      ('choose (and (lispm-desktop-choose--item dialog)
                    (lispm-desktop-choose--hold frame dialog 'choose)
                    (lispm-desktop-choose--open dialog)))
      ('location
       (when-let* ((item (lispm-desktop-choose--menu frame dialog :location
                                                     (lispm-desktop-choose--path-items dialog))))
         (lispm-desktop-choose--go dialog (plist-get item :value)))
       nil)
      ('shortcuts (lispm-desktop-choose--shortcut frame dialog) nil)
      ('recent
       (when-let* ((files (lispm-desktop-choose--recent dialog))
                   (item (lispm-desktop-choose--menu
                          frame dialog :recent
                          (mapcar (lambda (file)
                                    (list :label (file-name-nondirectory file) :value file
                                          :icon (create-image (lispm-desktop-platinum-icon-file
                                                               (lispm-desktop-platinum-file-icon-name file))
                                                              nil nil :ascent 'center)))
                                  files))))
         ;; The file is shown selected, its preview beside it, to be chosen.
         (let ((file (plist-get item :value)))
           (lispm-desktop-choose--go dialog (file-name-directory file) (file-name-nondirectory file))))
       nil)
      (`(header . ,column)
       (if (eq column 'direction)
           (setf (lispm-desktop-choose--dialog-descending dialog)
                 (not (lispm-desktop-choose--dialog-descending dialog)))
         (setf (lispm-desktop-choose--dialog-sort dialog) column))
       (lispm-desktop-choose--sort dialog)
       nil)
      (`(row . ,index)
       (lispm-desktop-choose--select dialog index)
       (when double (lispm-desktop-choose--open dialog)))
      (`(scroll . ,part)
       (let ((top (lispm-desktop-choose--dialog-top dialog)))
         (pcase part
           ('up (lispm-desktop-choose--scroll-to dialog (1- top)))
           ('down (lispm-desktop-choose--scroll-to dialog (1+ top)))
           ('page-up (lispm-desktop-choose--scroll-to dialog (- top lispm-desktop-choose-rows)))
           ('page-down (lispm-desktop-choose--scroll-to dialog (+ top lispm-desktop-choose-rows)))
           ('thumb
            (pcase-let* ((`(,_ ,bar-top ,_ ,height) (plist-get (lispm-desktop-choose--layout) :scroll))
                         (y (cdr (posn-x-y (event-start event)))))
              (lispm-desktop-choose--drag-thumb
               frame dialog (- y bar-top (car (lispm-desktop-choose--thumb dialog height))))))))
       nil))))

;;;; The dialog

(defun lispm-desktop-choose--loop (frame dialog)
  "Read events for DIALOG, shown in FRAME, until a file is chosen or it is
cancelled; return the file, or nil."
  (track-mouse
    (catch 'done
      (while t
        (let* ((event (lispm-ui-modal-read))
               (result
                (cond
                 ((lispm-ui-modal-mouse-down-p event)
                  (lispm-desktop-choose--click frame dialog event))
                 ((memq (event-basic-type event) '(wheel-up wheel-down mouse-4 mouse-5))
                  (when (lispm-ui-modal-over event frame)
                    (lispm-desktop-choose--scroll-to
                     dialog (+ (lispm-desktop-choose--dialog-top dialog)
                               (if (memq (event-basic-type event) '(wheel-down mouse-5)) 3 -3))))
                  nil)
                 ((or (mouse-event-p event) (mouse-movement-p event)) 'ignore)
                 ((eq event ?/) (lispm-desktop-choose--shortcut frame dialog t) nil)
                 ((and (lispm-ui-modal-confirm-p event) (lispm-desktop-choose--item dialog))
                  ;; Return presses the default button, which blinks.
                  (lispm-ui-modal-flash 'button
                                             (lambda ()
                                               (setf (lispm-desktop-choose--dialog-pressed dialog) 'choose)
                                               (lispm-desktop-choose--show frame dialog)))
                  (setf (lispm-desktop-choose--dialog-pressed dialog) nil)
                  (lispm-desktop-choose--key dialog event))
                 (t (lispm-desktop-choose--key dialog event)))))
          (pcase result
            ('cancel (throw 'done nil))
            ((pred stringp) (throw 'done result))
            ('ignore nil)
            (_ (lispm-desktop-choose--show frame dialog))))))))

(defun lispm-desktop-choose--fallback (title file directory predicate)
  "Read a file in the minibuffer, as the dialog of TITLE would choose one:
from FILE's folder or DIRECTORY, among those PREDICATE takes; never in
the system's own panel."
  (condition-case nil
      (let* ((use-dialog-box nil)
             (chosen (read-file-name (format-prompt title nil)
                                     (if file (file-name-directory file) directory)
                                     nil t (and file (file-name-nondirectory file))
                                     (lambda (name)
                                       (or (file-directory-p name) (null predicate) (funcall predicate name))))))
        (and (file-regular-p chosen) (expand-file-name chosen)))
    (quit nil)))

(cl-defun lispm-desktop-choose-file (&key title file directory predicate preview)
  "Show the desktop's dialog choosing a file; return the file chosen, or nil.
TITLE titles it, by default \"Choose a File\".  It opens on FILE's
folder, FILE selected, else on DIRECTORY, by default `default-directory'.
It lists the folders, and the files PREDICATE, a function of a file's
name, takes, by default all; PREVIEW shows the picture selected beside
the list.  It sits over the upper middle of the selected frame, as Mac
OS 9's dialogs sat on the screen.  Where no dialog can be drawn, the
file is read in the minibuffer."
  (let ((title (or title (lispm-desktop-tr "Choose a File")))
        (directory (or (and file (file-name-directory file)) directory default-directory)))
    (if (not (lispm-desktop-popup-graphic-p))
        (lispm-desktop-choose--fallback title file directory predicate)
      (let* ((dialog (lispm-desktop-choose--make-dialog :title title :predicate predicate :preview preview))
             (parent (selected-frame))
             (frame (progn (lispm-desktop-choose--go dialog directory (and file (file-name-nondirectory file)))
                           (lispm-desktop-popup-make-frame parent)))
             (layout (lispm-desktop-choose--layout)))
        (unwind-protect
            (progn
              (lispm-desktop-choose--show frame dialog)
              (lispm-desktop-popup-set-size frame (plist-get layout :width) (plist-get layout :height))
              (lispm-desktop-popup-show frame
                                        (/ (- (frame-pixel-width parent) (plist-get layout :width)) 2)
                                        (max 0 (/ (- (frame-pixel-height parent) (plist-get layout :height)) 3))
                                        'alert)
              (when-let* ((chosen (lispm-desktop-choose--loop frame dialog)))
                (lispm-desktop-choose--remember chosen)
                chosen))
          (lispm-desktop-popup-dispose frame))))))

(provide 'lispm-desktop-choose)
;;; lispm-desktop-choose.el ends here
