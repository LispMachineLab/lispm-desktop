;;; lispm-desktop-app-yeetube-page.el --- Yeetube's window drawn as YouTube's pages  -*- lexical-binding: t; -*-

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

;; Yeetube's window shows YouTube's pages, as youtube.com draws them,
;; rather than Yeetube's table:
;;
;;   masthead  over every page: YouTube's mark beside Yeetube's name,
;;             and the search box in the
;;             middle, the button with the magnifier at its end; a back
;;             arrow before the logo while a video plays
;;   home      before anything is searched: a prompt to search
;;   results   one video a row, as youtube.com/results lists them: the
;;             thumbnail, rounded, its length in a badge, then the
;;             title, the views and age, and the channel; chips over
;;             them sorting by relevance, date, views or rating, and a
;;             button under them showing more
;;   watch     the video playing in the window, YouTube's own player
;;             through WebKit, in a page of its own served from this
;;             machine, as YouTube plays only in a page saying where it
;;             is; then its title, its channel, Download,
;;             Save and Share, its views and age, and Up next: the other
;;             results, smaller, as youtube.com lays a watch page out in
;;             a window this wide
;;
;; Each row of a page is one SVG on a line of its own, its text the
;; title, so that isearch finds videos.  A row of a video carries
;; Yeetube's `tabulated-list-id': Yeetube's own commands, downloading or
;; saving, work on the video point is on.  The row point is on is shaded,
;; as youtube.com shades the one under the mouse.  The colours are
;; YouTube's light theme's, or its dark theme's under a dark desktop
;; theme; the font is YouTube's, Roboto, else Arial, as youtube.com falls
;; back to.  Thumbnails are YouTube's larger ones, fetched as rows need
;; them.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'url)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-i18n)

(defvar yeetube-items)
(defvar yeetube--continuation)
(declare-function make-xwidget "xwidget.c" (type title width height arguments &optional buffer related))
(declare-function xwidget-live-p "xwidget.c" (object))
(declare-function xwidget-resize "xwidget.c" (xwidget new-width new-height))
(declare-function xwidget-webkit-goto-uri "xwidget.c" (xwidget uri))
(declare-function kill-xwidget "xwidget" (xwidget))

;;;; Colours, font, words

(defconst lispm-desktop-app-yeetube-page-light
  '(:background "#ffffff" :text "#0f0f0f" :secondary "#606060" :hover "#f2f2f2"
    :skeleton "#e5e5e5" :border "#c6c6c6" :button "#f8f8f8" :button-border "#d3d3d3"
    :chip "#f2f2f2" :chip-text "#0f0f0f" :chip-selected "#0f0f0f" :chip-selected-text "#ffffff"
    :focus "#1c62b9" :placeholder "#888888" :primary "#065fd4" :primary-text "#ffffff"
    :logo "#ff0000" :logo-mark "#ffffff")
  "YouTube's colours in its light theme.")

(defconst lispm-desktop-app-yeetube-page-dark
  '(:background "#0f0f0f" :text "#f1f1f1" :secondary "#aaaaaa" :hover "#272727"
    :skeleton "#272727" :border "#303030" :button "#222222" :button-border "#303030"
    :chip "#272727" :chip-text "#f1f1f1" :chip-selected "#f1f1f1" :chip-selected-text "#0f0f0f"
    :focus "#1c62b9" :placeholder "#888888" :primary "#3ea6ff" :primary-text "#0f0f0f"
    :logo "#ff0000" :logo-mark "#ffffff")
  "YouTube's colours in its dark theme.")

(defun lispm-desktop-app-yeetube-page-colors ()
  "Return YouTube's colours for the desktop's theme: its dark theme's
under a dark one, else its light theme's."
  (let ((content (lispm-ui-color :content)))
    (if (ignore-errors (color-dark-p (color-name-to-rgb content)))
        lispm-desktop-app-yeetube-page-dark
      lispm-desktop-app-yeetube-page-light)))

(defconst lispm-desktop-app-yeetube-page-font "Roboto, Arial, sans-serif"
  "YouTube's font, and what youtube.com falls back to without it.")

(defvar lispm-desktop-app-yeetube-page--family nil
  "The family of YouTube's font installed, as rows are measured in it.")

(defun lispm-desktop-app-yeetube-page--family ()
  "Return the family of YouTube's font installed: Roboto, else Arial."
  (with-memoization lispm-desktop-app-yeetube-page--family
    (or (seq-find (lambda (family) (member family (font-family-list))) '("Roboto" "Arial" "Helvetica"))
        "Arial")))

(defun lispm-desktop-app-yeetube-page-measure (size &optional weight)
  "Return a function of a string returning its width in pixels, written at
SIZE pixels in YouTube's font, of WEIGHT, a symbol as faces take."
  (let ((face (list :family (lispm-desktop-app-yeetube-page--family) :height (* 10 size)
                    :weight (or weight 'normal))))
    (lambda (text) (string-pixel-width (propertize text 'face face)))))

(defun lispm-desktop-app-yeetube-page--fit (text width measure suffix)
  "Return how many characters of TEXT, SUFFIX after them, are at most WIDTH
pixels wide as MEASURE measures them."
  (let ((low 0) (high (length text)))
    (while (< low high)
      (let ((middle (/ (+ low high 1) 2)))
        (if (<= (funcall measure (concat (substring text 0 middle) suffix)) width)
            (setq low middle)
          (setq high (1- middle)))))
    low))

(defun lispm-desktop-app-yeetube-page-wrap (text width lines measure)
  "Return TEXT broken into at most LINES lines, each at most WIDTH pixels
wide as MEASURE, a function of a string, measures it: at spaces where it
has them, else between characters, as a run of Chinese; the last line
ends in an ellipsis when TEXT runs over, as YouTube cuts its titles."
  (let ((text (string-trim (replace-regexp-in-string "[ \t\n]+" " " (or text ""))))
        (result nil))
    (while (and (not (string-empty-p text)) (< (length result) lines))
      (cond ((<= (funcall measure text) width)
             (push text result)
             (setq text ""))
            ((= (length result) (1- lines))
             (push (concat (string-trim-right
                            (substring text 0 (lispm-desktop-app-yeetube-page--fit text width measure "…")))
                           "…")
                   result)
             (setq text ""))
            (t
             (let* ((fits (max 1 (lispm-desktop-app-yeetube-page--fit text width measure "")))
                    (space (cl-position ?\s text :end (min (length text) (1+ fits)) :from-end t))
                    (cut (if (and space (> space 0)) space fits)))
               (push (string-trim-right (substring text 0 cut)) result)
               (setq text (string-trim-left (substring text cut)))))))
    (nreverse result)))

(defun lispm-desktop-app-yeetube-page--compact (number)
  "Return NUMBER as YouTube writes a count: 950, 1.2K, 96K, 3.4M, 1.2B."
  (cl-flet ((in (unit suffix)
              (let ((value (/ (float number) unit)))
                (if (< value 10)
                    (concat (string-remove-suffix ".0" (format "%.1f" (/ (ffloor (* value 10)) 10))) suffix)
                  (format "%d%s" (floor value) suffix)))))
    (cond ((< number 1000) (number-to-string number))
          ((< number 1000000) (in 1000 "K"))
          ((< number 1000000000) (in 1000000 "M"))
          (t (in 1000000000 "B")))))

(defun lispm-desktop-app-yeetube-page-views (views)
  "Return VIEWS, as Yeetube read them, \"96,521 views\", as YouTube's
pages write them, \"96K views\"."
  (if (and views (string-match "\\`\\([0-9][0-9,]*\\)\\(.*\\)\\'" views))
      (concat (lispm-desktop-app-yeetube-page--compact
               (string-to-number (string-replace "," "" (match-string 1 views))))
              (match-string 2 views))
    (or views "")))

(defun lispm-desktop-app-yeetube-page-meta (item)
  "Return the line of ITEM's views and age, as youtube.com writes it:
views, a bullet, age, as YouTube gave them; Playlist for a playlist."
  (if (eq (plist-get item :type) 'playlist)
      (lispm-desktop-tr "Playlist")
    (string-join (seq-remove #'string-empty-p
                             (list (lispm-desktop-app-yeetube-page-views (plist-get item :views))
                                   (or (plist-get item :date) "")))
                 " • ")))

;;;; Thumbnails

(defvar lispm-desktop-app-yeetube-page--thumbnails (make-hash-table :test #'equal)
  "The thumbnails fetched: ID to JPEG bytes, `fetching' or `failed'.")

(defvar lispm-desktop-app-yeetube-page-thumbnail-functions nil
  "Functions called with an id once its thumbnail is fetched, or failed.")

(defun lispm-desktop-app-yeetube-page--thumbnail-urls (item)
  "Return the addresses ITEM's thumbnail is fetched from, best first:
YouTube's larger one, 480 by 360, and Yeetube's own."
  (delq nil (list (when (eq (plist-get item :type) 'video)
                    (format "https://i.ytimg.com/vi/%s/hqdefault.jpg" (plist-get item :id)))
                  (let ((url (plist-get item :thumbnail-url)))
                    (and (stringp url) (not (string-empty-p url)) url)))))

(defun lispm-desktop-app-yeetube-page--fetch (id urls)
  "Fetch the thumbnail of ID from the first of URLS answering with one."
  (if (null urls)
      (progn (puthash id 'failed lispm-desktop-app-yeetube-page--thumbnails)
             (run-hook-with-args 'lispm-desktop-app-yeetube-page-thumbnail-functions id))
    ;; Not queued: the queue drops a fetch slower than five seconds.
    (url-retrieve
     (car urls)
     (lambda (status)
       (let ((data (unwind-protect
                       (and (not (plist-get status :error))
                            (eql (bound-and-true-p url-http-response-status) 200)
                            (bound-and-true-p url-http-end-of-headers)
                            (buffer-substring-no-properties (1+ url-http-end-of-headers) (point-max)))
                     (kill-buffer (current-buffer)))))
         (if (and data (> (length data) 256))
             (progn (puthash id data lispm-desktop-app-yeetube-page--thumbnails)
                    (run-hook-with-args 'lispm-desktop-app-yeetube-page-thumbnail-functions id))
           (lispm-desktop-app-yeetube-page--fetch id (cdr urls)))))
     nil t t)))

(defun lispm-desktop-app-yeetube-page-thumbnail (item)
  "Return ITEM's thumbnail as JPEG bytes once fetched, else nil; the first
time it is asked for, it is fetched."
  (let* ((id (plist-get item :id))
         (got (gethash id lispm-desktop-app-yeetube-page--thumbnails)))
    (cond ((stringp got) got)
          (got nil)
          ((null id) nil)
          (t (puthash id 'fetching lispm-desktop-app-yeetube-page--thumbnails)
             (lispm-desktop-app-yeetube-page--fetch id (lispm-desktop-app-yeetube-page--thumbnail-urls item))
             nil))))

;;;; Drawing

(defconst lispm-desktop-app-yeetube-page-avatar-colors
  '("#e53935" "#d81b60" "#8e24aa" "#5e35b1" "#3949ab" "#1e88e5" "#039be5"
    "#00acc1" "#00897b" "#43a047" "#7cb342" "#f4511e" "#6d4c41" "#546e7a")
  "Colours of the avatars standing in for channels' pictures, as YouTube
colours those of channels without one.")

(defun lispm-desktop-app-yeetube-page--text (svg text x y size color &rest attributes)
  "Write TEXT on SVG at X, Y, its baseline, SIZE pixels tall in YouTube's
font, in COLOR, with ATTRIBUTES more."
  (apply #'svg-text svg text :x x :y y :font-family lispm-desktop-app-yeetube-page-font
         :font-size size :fill color attributes))

(defun lispm-desktop-app-yeetube-page--avatar (svg channel x y size)
  "Draw on SVG the avatar of CHANNEL, a name, SIZE pixels wide at X, Y:
its first letter on a colour of its own."
  (let ((name (string-trim (or channel ""))))
    (svg-circle svg (+ x (/ size 2.0)) (+ y (/ size 2.0)) (/ size 2.0)
                :fill (nth (mod (sxhash-equal name) (length lispm-desktop-app-yeetube-page-avatar-colors))
                           lispm-desktop-app-yeetube-page-avatar-colors))
    (unless (string-empty-p name)
      (lispm-desktop-app-yeetube-page--text svg (upcase (substring name 0 1)) (+ x (/ size 2.0))
                                            (+ y (* size 0.68)) (round (* size 0.5)) "#ffffff"
                                            :text-anchor "middle" :font-weight "500"))))

(defun lispm-desktop-app-yeetube-page--thumbnail-at (svg item x y width height radius colors)
  "Draw on SVG ITEM's thumbnail at X, Y, WIDTH by HEIGHT pixels, its corners
rounded by RADIUS, its length in a badge at its bottom right; a grey
stand-in while it is fetched."
  (let ((data (lispm-desktop-app-yeetube-page-thumbnail item))
        (clip (format "thumb-%d-%d" x y)))
    (svg-rectangle svg x y width height :rx radius :fill (plist-get colors :skeleton))
    (when data
      (let ((path (svg-clip-path svg :id clip)))
        (svg-rectangle path x y width height :rx radius))
      (svg-embed svg data "image/jpeg" t :x x :y y :width width :height height
                 :preserveAspectRatio "xMidYMid slice" :clip-path (format "url(#%s)" clip)))
    (let ((badge (plist-get item :duration)))
      (when (and (stringp badge) (not (string-empty-p badge)))
        (let* ((size (if (< width 200) 11 12))
               (wide (+ 8 (funcall (lispm-desktop-app-yeetube-page-measure size 'medium) badge)))
               (bx (- (+ x width) 4 wide))
               (by (- (+ y height) 4 18)))
          (svg-rectangle svg bx by wide 18 :rx 4 :fill "#000000" :fill-opacity 0.8)
          (lispm-desktop-app-yeetube-page--text svg badge (+ bx (/ wide 2.0)) (+ by 13) size "#ffffff"
                                                :text-anchor "middle" :font-weight "500"))))))

(defun lispm-desktop-app-yeetube-page-result (item width selected)
  "Return the SVG of ITEM's row of the results, WIDTH pixels wide: the
thumbnail, then its title, views and age, and channel, as youtube.com's
results; shaded when SELECTED, point on it."
  (let* ((colors (lispm-desktop-app-yeetube-page-colors))
         (pad 8)
         (thumb-width (min 360 (max 168 (round (* 0.4 (- width 32))))))
         (thumb-height (round (* thumb-width 0.5625)))
         (height (+ thumb-height (* 2 pad)))
         (svg (svg-create width height))
         (left (+ 16 pad thumb-width 16))
         (room (- width left pad 16))
         (title (lispm-desktop-app-yeetube-page-wrap (plist-get item :title) room 2
                                                     (lispm-desktop-app-yeetube-page-measure 18)))
         (y (+ pad 20)))
    (svg-rectangle svg 0 0 width height :fill (plist-get colors :background))
    (when selected
      (svg-rectangle svg 8 0 (- width 16) height :rx 12 :fill (plist-get colors :hover)))
    (lispm-desktop-app-yeetube-page--thumbnail-at svg item (+ 16 pad) pad thumb-width thumb-height 12 colors)
    (dolist (line title)
      (lispm-desktop-app-yeetube-page--text svg line left y 18 (plist-get colors :text))
      (setq y (+ y 26)))
    (lispm-desktop-app-yeetube-page--text svg (lispm-desktop-app-yeetube-page-meta item) left (- y 4) 12
                                          (plist-get colors :secondary))
    (let ((channel (plist-get item :channel)))
      (when (and channel (not (string-empty-p channel)))
        (lispm-desktop-app-yeetube-page--avatar svg channel left (+ y 8) 24)
        (lispm-desktop-app-yeetube-page--text svg channel (+ left 32) (+ y 24) 12 (plist-get colors :secondary))))
    svg))

(defun lispm-desktop-app-yeetube-page-compact (item width selected)
  "Return the SVG of ITEM's row of Up next, WIDTH pixels wide: a smaller
thumbnail, then its title, channel, views and age; shaded when SELECTED."
  (let* ((colors (lispm-desktop-app-yeetube-page-colors))
         (pad 4)
         (thumb-width 168)
         (thumb-height 94)
         (height (+ thumb-height (* 2 pad)))
         (svg (svg-create width height))
         (left (+ 16 pad thumb-width 8))
         (title (lispm-desktop-app-yeetube-page-wrap (plist-get item :title) (- width left 24) 2
                                                     (lispm-desktop-app-yeetube-page-measure 14 'medium)))
         (y (+ pad 16)))
    (svg-rectangle svg 0 0 width height :fill (plist-get colors :background))
    (when selected
      (svg-rectangle svg 8 0 (- width 16) height :rx 8 :fill (plist-get colors :hover)))
    (lispm-desktop-app-yeetube-page--thumbnail-at svg item (+ 16 pad) pad thumb-width thumb-height 8 colors)
    (dolist (line title)
      (lispm-desktop-app-yeetube-page--text svg line left y 14 (plist-get colors :text) :font-weight "500")
      (setq y (+ y 20)))
    (lispm-desktop-app-yeetube-page--text svg (or (plist-get item :channel) "") left (+ y 2) 12
                                          (plist-get colors :secondary))
    (lispm-desktop-app-yeetube-page--text svg (lispm-desktop-app-yeetube-page-meta item) left (+ y 20) 12
                                          (plist-get colors :secondary))
    svg))

(defun lispm-desktop-app-yeetube-page-skeleton (width)
  "Return the SVG of a row of results still coming, WIDTH pixels wide: grey
shapes where the thumbnail and the lines will be, as youtube.com shows
them while it loads."
  (let* ((colors (lispm-desktop-app-yeetube-page-colors))
         (thumb-width (min 360 (max 168 (round (* 0.4 (- width 32))))))
         (thumb-height (round (* thumb-width 0.5625)))
         (left (+ 24 thumb-width 16))
         (svg (svg-create width (+ thumb-height 16)))
         (gray (plist-get colors :skeleton)))
    (svg-rectangle svg 0 0 width (+ thumb-height 16) :fill (plist-get colors :background))
    (svg-rectangle svg 24 8 thumb-width thumb-height :rx 12 :fill gray)
    (svg-rectangle svg left 12 (round (* 0.9 (- width left 24))) 20 :rx 4 :fill gray)
    (svg-rectangle svg left 40 (round (* 0.5 (- width left 24))) 20 :rx 4 :fill gray)
    (svg-rectangle svg left 76 (round (* 0.3 (- width left 24))) 14 :rx 4 :fill gray)
    svg))

(defun lispm-desktop-app-yeetube-page-message (width title text &optional button)
  "Return (SVG . HITS) of a page saying TITLE over TEXT, WIDTH pixels wide,
a magnifier over them and BUTTON, a label, under them: HITS has the
button's rectangle, as (search X Y WIDTH HEIGHT)."
  (let* ((colors (lispm-desktop-app-yeetube-page-colors))
         (height (if button 300 240))
         (svg (svg-create width height))
         (centre (/ width 2.0))
         (hits nil))
    (svg-rectangle svg 0 0 width height :fill (plist-get colors :background))
    (svg-circle svg centre 88 52 :fill (plist-get colors :hover))
    (svg-circle svg (- centre 6) 82 18 :fill "none" :stroke (plist-get colors :text) :stroke-width 4)
    (svg-line svg (+ centre 7) 95 (+ centre 20) 108 :stroke (plist-get colors :text) :stroke-width 5
              :stroke-linecap "round")
    (lispm-desktop-app-yeetube-page--text svg title centre 180 24 (plist-get colors :text) :text-anchor "middle")
    (lispm-desktop-app-yeetube-page--text svg text centre 210 14 (plist-get colors :secondary) :text-anchor "middle")
    (when button
      (let* ((wide (+ 32 (funcall (lispm-desktop-app-yeetube-page-measure 14 'medium) button)))
             (x (round (- centre (/ wide 2.0)))))
        (svg-rectangle svg x 240 wide 36 :rx 18 :fill (plist-get colors :primary))
        (lispm-desktop-app-yeetube-page--text svg button centre 263 14 (plist-get colors :primary-text)
                                              :text-anchor "middle" :font-weight "500")
        (push (list 'search x 240 wide 36) hits)))
    (cons svg hits)))

(defconst lispm-desktop-app-yeetube-page-sorts
  `((,(lispm-desktop-tr-noop "Relevance") "Relevance" ,(lispm-desktop-tr-noop "Sort by Relevance"))
    (,(lispm-desktop-tr-noop "Upload date") "Date" ,(lispm-desktop-tr-noop "Sort by Upload date"))
    (,(lispm-desktop-tr-noop "View count") "Views" ,(lispm-desktop-tr-noop "Sort by View count"))
    (,(lispm-desktop-tr-noop "Rating") "Rating" ,(lispm-desktop-tr-noop "Sort by Rating")))
  "The chips sorting the results: (LABEL FILTER ITEM), FILTER a value of
`yeetube-filter', LABEL the chip's and ITEM the Video menu's item sorting
so, both in English, translated where shown.")

(defun lispm-desktop-app-yeetube-page-chips (width selected)
  "Return (SVG . HITS) of the chips sorting the results, WIDTH pixels wide,
SELECTED's chip, a value of `yeetube-filter', dark as YouTube marks the
chip chosen: HITS has (FILTER X Y WIDTH HEIGHT) for each chip."
  (let* ((colors (lispm-desktop-app-yeetube-page-colors))
         (svg (svg-create width 48))
         (measure (lispm-desktop-app-yeetube-page-measure 14 'medium))
         (x 16)
         (hits nil))
    (svg-rectangle svg 0 0 width 48 :fill (plist-get colors :background))
    (pcase-dolist (`(,english ,filter . ,_) lispm-desktop-app-yeetube-page-sorts)
      (let* ((label (lispm-desktop-tr english))
             (wide (+ 24 (funcall measure label)))
             (on (equal filter selected)))
        (svg-rectangle svg x 8 wide 32 :rx 8 :fill (plist-get colors (if on :chip-selected :chip)))
        (lispm-desktop-app-yeetube-page--text svg label (+ x 12) 29 14
                                              (plist-get colors (if on :chip-selected-text :chip-text))
                                              :font-weight "500")
        (push (list filter x 8 wide 32) hits)
        (setq x (+ x wide 12))))
    (cons svg hits)))

(defun lispm-desktop-app-yeetube-page-more (width loading)
  "Return (SVG . HITS) of the button under the results showing more, WIDTH
pixels wide, saying so while LOADING: HITS has (more X Y WIDTH HEIGHT)."
  (let* ((colors (lispm-desktop-app-yeetube-page-colors))
         (label (if loading (lispm-desktop-tr "Loading…") (lispm-desktop-tr "Show more results")))
         (wide (+ 32 (funcall (lispm-desktop-app-yeetube-page-measure 14 'medium) label)))
         (x (round (- (/ width 2.0) (/ wide 2.0))))
         (svg (svg-create width 64)))
    (svg-rectangle svg 0 0 width 64 :fill (plist-get colors :background))
    (svg-rectangle svg x 14 wide 36 :rx 18 :fill (plist-get colors :hover))
    (lispm-desktop-app-yeetube-page--text svg label (/ width 2.0) 37 14 (plist-get colors :text)
                                          :text-anchor "middle" :font-weight "500")
    (cons svg (list (list 'more x 14 wide 36)))))

(defun lispm-desktop-app-yeetube-page-info (item width)
  "Return (SVG . HITS) of what the watch page says under the player, WIDTH
pixels wide: ITEM's title, its channel, the Download, Save and Share
buttons, and a box of its views and age.  HITS has (ACTION X Y WIDTH
HEIGHT) for the channel and each button, ACTION `channel', `download',
`save' or `share'."
  (let* ((colors (lispm-desktop-app-yeetube-page-colors))
         (title (lispm-desktop-app-yeetube-page-wrap (plist-get item :title) (- width 32) 2
                                                     (lispm-desktop-app-yeetube-page-measure 20 'bold)))
         (y (+ 12 22))
         (row (+ 12 (* 28 (length title)) 12))
         (height (+ row 40 12 64 12))
         (svg (svg-create width height))
         (channel (or (plist-get item :channel) ""))
         (measure (lispm-desktop-app-yeetube-page-measure 14 'medium))
         (hits nil))
    (svg-rectangle svg 0 0 width height :fill (plist-get colors :background))
    (dolist (line title)
      (lispm-desktop-app-yeetube-page--text svg line 16 y 20 (plist-get colors :text) :font-weight "bold")
      (setq y (+ y 28)))
    (unless (string-empty-p channel)
      (lispm-desktop-app-yeetube-page--avatar svg channel 16 row 40)
      (lispm-desktop-app-yeetube-page--text svg channel 68 (+ row 26) 16 (plist-get colors :text)
                                            :font-weight "500")
      (push (list 'channel 16 row (+ 52 (funcall (lispm-desktop-app-yeetube-page-measure 16 'medium) channel)) 40)
            hits))
    ;; The buttons, from the right, as youtube.com sets them.
    (let ((x (- width 16)))
      (dolist (button `((share . ,(lispm-desktop-tr "Share")) (save . ,(lispm-desktop-tr "Save"))
                        (download . ,(lispm-desktop-tr "Download"))))
        (let* ((wide (+ 32 (funcall measure (cdr button))))
               (bx (- x wide)))
          (svg-rectangle svg bx row wide 36 :rx 18 :fill (plist-get colors :hover))
          (lispm-desktop-app-yeetube-page--text svg (cdr button) (+ bx (/ wide 2.0)) (+ row 23) 14
                                                (plist-get colors :text) :text-anchor "middle" :font-weight "500")
          (push (list (car button) bx row wide 36) hits)
          (setq x (- bx 8)))))
    (let ((box (+ row 40 12)))
      (svg-rectangle svg 16 box (- width 32) 64 :rx 12 :fill (plist-get colors :hover))
      (lispm-desktop-app-yeetube-page--text svg (lispm-desktop-app-yeetube-page-meta item) 28 (+ box 26) 14
                                            (plist-get colors :text) :font-weight "500")
      (lispm-desktop-app-yeetube-page--text svg (if (string-empty-p channel) "" (lispm-desktop-tr "From %s" channel))
                                            28 (+ box 48) 14 (plist-get colors :secondary)))
    (cons svg hits)))

(defun lispm-desktop-app-yeetube-page-heading (width text)
  "Return the SVG of a heading over a page's list, WIDTH pixels wide."
  (let* ((colors (lispm-desktop-app-yeetube-page-colors))
         (svg (svg-create width 40)))
    (svg-rectangle svg 0 0 width 40 :fill (plist-get colors :background))
    (lispm-desktop-app-yeetube-page--text svg text 16 28 16 (plist-get colors :text) :font-weight "500")
    svg))

;;;; The masthead

(defconst lispm-desktop-app-yeetube-page-masthead-height 56
  "Height in pixels of the masthead over the pages, as youtube.com's.")

(defun lispm-desktop-app-yeetube-page-masthead-layout (width back)
  "Return the places of the masthead's parts, WIDTH pixels wide, with the
back arrow when BACK: an alist of (PART X Y WIDTH HEIGHT), PART `back',
`logo', `input' or `button'."
  (let* ((logo (if back 56 16))
         (start (+ logo 90 32))
         (pill (max 160 (min 640 (- width start 16 40))))
         (x (max start (round (- (/ width 2.0) (/ pill 2.0)))))
         (input (- pill 64)))
    `(,@(when back '((back 8 8 40 40)))
      (logo ,logo 16 90 24)
      (input ,x 8 ,input 40)
      (button ,(+ x input) 8 64 40))))

(defun lispm-desktop-app-yeetube-page-masthead (width query focused back)
  "Return the SVG of the masthead, WIDTH pixels wide: the back arrow when
BACK, the logo, the search box saying QUERY, or Search when it is nil,
ringed in blue while FOCUSED, the field over it, and its button."
  (let* ((colors (lispm-desktop-app-yeetube-page-colors))
         (height lispm-desktop-app-yeetube-page-masthead-height)
         (svg (svg-create width height))
         (layout (lispm-desktop-app-yeetube-page-masthead-layout width back))
         (text (plist-get colors :text)))
    (svg-rectangle svg 0 0 width height :fill (plist-get colors :background))
    (pcase-let ((`(,x ,y ,_ ,_) (alist-get 'back layout)))
      (when x
        (svg-line svg (+ x 12) (+ y 20) (+ x 28) (+ y 20) :stroke text :stroke-width 2)
        (svg-line svg (+ x 12) (+ y 20) (+ x 19) (+ y 13) :stroke text :stroke-width 2 :stroke-linecap "round")
        (svg-line svg (+ x 12) (+ y 20) (+ x 19) (+ y 27) :stroke text :stroke-width 2 :stroke-linecap "round")))
    (pcase-let ((`(,x ,y ,_ ,_) (alist-get 'logo layout)))
      (svg-rectangle svg x (+ y 2) 29 20 :rx 6 :fill (plist-get colors :logo))
      (svg-polygon svg (list (cons (+ x 11.5) (+ y 7)) (cons (+ x 11.5) (+ y 17)) (cons (+ x 20) (+ y 12)))
                   :fill (plist-get colors :logo-mark))
      ;; YouTube's mark, the application's own name beside it.
      (svg-text svg "Yeetube" :x (+ x 32) :y (+ y 19) :font-family "Arial, sans-serif" :font-size 20
                :font-weight "bold" :letter-spacing "-1" :fill text))
    (pcase-let ((`(,x ,y ,w ,h) (alist-get 'input layout))
                (`(,bx ,_ ,bw ,_) (alist-get 'button layout)))
      ;; The field rounded on the left, the button on the right, a line
      ;; between them, as youtube.com's search box.
      (svg-rectangle svg x y (+ w 20) h :rx 20 :fill (plist-get colors :background)
                     :stroke (plist-get colors (if focused :focus :border)))
      (svg-rectangle svg bx y bw h :rx 20 :fill (plist-get colors :button) :stroke (plist-get colors :button-border))
      (svg-rectangle svg bx (+ y 0.5) 20 (- h 1) :fill (plist-get colors :button))
      (svg-line svg bx y (+ bx 20) y :stroke (plist-get colors :button-border))
      (svg-line svg bx (+ y h) (+ bx 20) (+ y h) :stroke (plist-get colors :button-border))
      (svg-line svg bx y bx (+ y h) :stroke (plist-get colors (if focused :focus :border)))
      (unless focused
        (lispm-desktop-app-yeetube-page--text
         svg (if (and query (not (string-empty-p query)))
                 (car (lispm-desktop-app-yeetube-page-wrap query (- w 32) 1 (lispm-desktop-app-yeetube-page-measure 16)))
               (lispm-desktop-tr "Search"))
         (+ x 16) (+ y 26) 16 (plist-get colors (if (and query (not (string-empty-p query))) :text :placeholder))))
      (let ((cx (+ bx 29)) (cy (+ y 18)))
        (svg-circle svg cx cy 7 :fill "none" :stroke text :stroke-width 1.6)
        (svg-line svg (+ cx 5) (+ cy 5) (+ cx 11) (+ cy 11) :stroke text :stroke-width 1.8
                  :stroke-linecap "round")))
    svg))

;;;; The player

(defun lispm-desktop-app-yeetube-page-embed-url (item)
  "Return the address YouTube's player plays ITEM from, inside a page of
its own: the video, or a playlist's videos one after another."
  (if (eq (plist-get item :type) 'playlist)
      (format "https://www.youtube.com/embed/videoseries?list=%s&autoplay=1&rel=0" (plist-get item :id))
    (format "https://www.youtube.com/embed/%s?autoplay=1&rel=0&playsinline=1" (plist-get item :id))))

(defun lispm-desktop-app-yeetube-page-player-html (item)
  "Return the page holding YouTube's player of ITEM, the whole page its
own: YouTube plays only inside a page saying where it is, by its Referer,
which its player's address loaded by itself does not."
  (format (concat "<!doctype html><html><head><meta charset=\"utf-8\">"
                  "<meta name=\"referrer\" content=\"strict-origin-when-cross-origin\">"
                  "<style>html,body{margin:0;height:100%%;background:#000;overflow:hidden}"
                  "iframe{border:0;width:100%%;height:100%%}</style></head><body>"
                  "<iframe src=\"%s\" allow=\"autoplay; encrypted-media; picture-in-picture; fullscreen\""
                  " allowfullscreen referrerpolicy=\"strict-origin-when-cross-origin\"></iframe>"
                  "</body></html>")
          (lispm-desktop-app-yeetube-page-embed-url item)))

(defun lispm-desktop-app-yeetube-page-player-size (width height)
  "Return the size of the player, (WIDTH . HEIGHT), in a window WIDTH by
HEIGHT pixels: as wide as the page, 16 by 9, but leaving a third of the
window's height to what it says under it."
  (let* ((w (- width 32))
         (h (round (* w 0.5625)))
         (most (round (* height 0.66))))
    (if (> h most)
        (cons (round (/ most 0.5625)) most)
      (cons w h))))

(provide 'lispm-desktop-app-yeetube-page)
;;; lispm-desktop-app-yeetube-page.el ends here
