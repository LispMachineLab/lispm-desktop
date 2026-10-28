;;; lispm-desktop-platinum-draw.el --- Drawing Mac OS 9's forms as SVG  -*- lexical-binding: t; -*-

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

;; The drawing primitives the chrome is made of, all pure: palette and
;; geometry in, an SVG out.  Rows, columns and dots of pixels, the grounds
;; and ridges of title bars, triangles, and text in the chrome's faces; and
;; the cache of drawn images.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-platinum-palette)
(require 'lispm-desktop-i18n)


(defconst lispm-desktop-platinum-title-height 22
  "Height in pixels of a title bar, and of a window's bottom edge.")

(defconst lispm-desktop-platinum-menu-bar-height 20
  "Height in pixels of the menu bar, Mac OS 9's.")

(defconst lispm-desktop-platinum-box-size 13
  "Side in pixels of a title bar box.")

(defconst lispm-desktop-platinum-box-top 3
  "Pixels from a title bar's top to its boxes'.")

;;;; Drawing (pure)

(defvar lispm-desktop-platinum--images (make-hash-table :test #'equal)
  "Drawn images by key: what was drawn, its size and the palette generation.")

(defun lispm-ui-image (key draw)
  "Return the image of KEY, calling DRAW to make its SVG the first time,
an SVG of svg.el's or SVG text."
  (let ((key (cons lispm-ui-palette-generation key)))
    (or (gethash key lispm-desktop-platinum--images)
        (progn
          (when (> (hash-table-count lispm-desktop-platinum--images) 512)
            (clrhash lispm-desktop-platinum--images))
          (puthash key (lispm-ui-svg-image (funcall draw))
                   lispm-desktop-platinum--images)))))

(defun lispm-ui-svg-image (svg)
  "Return the image of SVG, svg.el's or SVG text, centred on its line and
never scaled.  An Emacs built without SVG gets the image all the same,
which it cannot show: the text under it shows instead, where
`svg-image' would signal."
  (cond ((not (image-type-available-p 'svg))
         (list 'image :type 'svg :data (if (stringp svg) svg (with-temp-buffer (svg-print svg) (buffer-string)))
               :ascent 'center :scale 1))
        ((stringp svg) (create-image svg 'svg t :ascent 'center :scale 1))
        (t (svg-image svg :ascent 'center :scale 1))))

(defun lispm-ui-svg-fill (svg x y width height color)
  "Fill the rectangle of WIDTH by HEIGHT at X, Y of SVG with COLOR.
Lines are drawn as one-pixel rectangles, so they fall on whole pixels."
  (svg-rectangle svg x y width height :fill color :stroke "none"))

(defun lispm-ui-svg-dot (svg x y color)
  "Colour the pixel at X, Y of SVG with COLOR."
  (lispm-ui-svg-fill svg x y 1 1 color))

(defun lispm-ui-svg-row (svg from to y color)
  "Draw a line along row Y of SVG in COLOR, from column FROM to TO, both in."
  (when (<= from to)
    (lispm-ui-svg-fill svg from y (1+ (- to from)) 1 color)))

(defun lispm-ui-svg-column (svg x from to color)
  "Draw a line down column X of SVG in COLOR, from row FROM to TO, both in."
  (when (<= from to)
    (lispm-ui-svg-fill svg x from 1 (1+ (- to from)) color)))

(defconst lispm-desktop-platinum-title-bar-height 21
  "Height in pixels of a title bar: its lit top line to the content's
black line, both included.  The outline above it is drawn by what is
above: the menu bar's black line, a window divider, a window's frame.")

(defun lispm-desktop-platinum--draw-title-ground (svg x width palette active)
  "Draw a title bar's ground on SVG from X, WIDTH pixels wide.
An ACTIVE bar is platinum, lit along its top, shaded along its bottom
over the content's black line; an inactive one lies flat, over a grey
line."
  (let ((grey (lambda (level) (lispm-ui-grey palette level)))
        (h lispm-desktop-platinum-title-bar-height))
    (lispm-ui-svg-fill svg x 0 width h (funcall grey (if active #xCC #xDD)))
    (if (not active)
        (lispm-ui-svg-fill svg x (1- h) width 1 (funcall grey #x55))
      (lispm-ui-svg-fill svg x 0 width 1 (funcall grey #xFF))
      (lispm-ui-svg-fill svg x (- h 2) width 1 (funcall grey #x99))
      (lispm-ui-svg-fill svg x (1- h) width 1 (plist-get palette :outline)))))

(defun lispm-desktop-platinum--draw-ridges (svg from to palette)
  "Draw a title bar's six ridges on SVG from column FROM to TO, both in.
Each ridge is a lit line over a shaded one, the lit line starting a
pixel further left and the shaded one ending a pixel further right, so
their ends look engraved."
  (when (< from to)
    (dotimes (i 6)
      (let ((y (+ 3 (* 2 i))))
        (lispm-ui-svg-row svg from (1- to) y (lispm-ui-grey palette #xFF))
        (lispm-ui-svg-row svg (1+ from) to (1+ y) (lispm-ui-grey palette #x77))))))

(defun lispm-desktop-platinum-draw-box (palette kind &optional modified pressed)
  "Return an SVG of a title bar box of KIND, `close', `zoom' or `collapse'.
The box is `lispm-desktop-platinum-box-size' square on an active title
bar's ground, as Mac OS 9 drew it: a recessed well round a dark frame,
inside it a bevel round a square shaded diagonally from grey to white.
The zoom box's glyph is a square sharing the frame's top left corner,
the collapse box's two lines across it.  MODIFIED puts a dot in a close
box; PRESSED draws the box held down, its square darkened and its bevel
turned over."
  (let* ((s lispm-desktop-platinum-box-size)
         (svg (svg-create s lispm-desktop-platinum-title-bar-height))
         (grey (lambda (level) (lispm-ui-grey palette level)))
         (y lispm-desktop-platinum-box-top)
         (ink (funcall grey #x22))
         (ramp (if pressed '(#x55 #x66 #x77 #x88 #x99 #xAA #xBB)
                 '(#x99 #xAA #xBB #xCC #xDD #xEE #xFF))))
    (cl-flet ((dot (x dy color) (lispm-ui-svg-dot svg x (+ y dy) color))
              (row (from to dy color) (lispm-ui-svg-row svg from to (+ y dy) color))
              (column (x from to color) (lispm-ui-svg-column svg x (+ y from) (+ y to) color)))
      (lispm-desktop-platinum--draw-title-ground svg 0 s palette t)
      ;; The well.
      (row 0 11 0 (funcall grey #x88))
      (column 0 0 11 (funcall grey #x88))
      (row 1 12 12 (funcall grey #xFF))
      (column 12 1 12 (funcall grey #xFF))
      ;; The frame.
      (row 1 11 1 ink)
      (row 1 11 11 ink)
      (column 1 1 11 ink)
      (column 11 1 11 ink)
      ;; The bevel, lit from the top left, or from below when pressed.
      (row 2 10 2 (funcall grey (if pressed #x88 #xCC)))
      (column 2 3 10 (funcall grey (if pressed #x88 #xCC)))
      (dot 2 2 (funcall grey (if pressed #x77 #xFF)))
      (column 10 3 10 (funcall grey (if pressed #xCC #x88)))
      (row 3 10 10 (funcall grey (if pressed #xCC #x88)))
      ;; The square, shaded along its diagonal.
      (dotimes (j 7)
        (dotimes (i 7)
          (dot (+ 3 i) (+ 3 j) (funcall grey (nth (/ (+ i j) 2) ramp)))))
      (pcase kind
        ('zoom
         (column 7 1 7 ink)
         (row 1 7 7 ink))
        ('collapse
         (row 1 11 5 ink)
         (row 1 11 7 ink)))
      (when (and modified (eq kind 'close))
        (svg-circle svg (/ s 2.0) (+ y (/ s 2.0)) 2 :fill (plist-get palette :outline))))
    svg))

(cl-defun lispm-desktop-platinum-draw-title-part (palette width &key active ridges label label-x
                                                          lead trail)
  "Return an SVG of a stretch of title bar WIDTH pixels wide, in PALETTE.
It shows the ground of an ACTIVE or inactive bar, the RIDGES, a list of
\(FROM . TO) columns, and LABEL, a string as Emacs would show it in the
titles' face, its icon included, from column LABEL-X.  LEAD draws the
lit line down the bar's left end, TRAIL the shaded one down its right."
  (let ((svg (svg-create width lispm-desktop-platinum-title-bar-height))
        (h lispm-desktop-platinum-title-bar-height))
    (lispm-desktop-platinum--draw-title-ground svg 0 width palette active)
    (when active
      (when lead
        (lispm-ui-svg-column svg 0 0 (- h 3) (lispm-ui-grey palette #xFF)))
      (when trail
        (lispm-ui-svg-column svg (1- width) 1 (- h 3) (lispm-ui-grey palette #x99)))
      (pcase-dolist (`(,from . ,to) ridges)
        (lispm-desktop-platinum--draw-ridges svg from to palette)))
    (when label
      (lispm-ui-svg-content
       svg label label-x 0 (- h 2)
       (if active (plist-get palette :text) (lispm-ui-grey palette #x77))
       (if active 'lispm-ui-title 'lispm-desktop-platinum-title-inactive)))
    svg))

(defconst lispm-desktop-platinum--triangle
  '("O" "OO" "OLO" "OLFO" "OLFFO" "OLFFDO" "OLFDOSs" "OLDOSs" "OFOSs" "OOSs" "OSs" " s")
  "Mac OS 9's disclosure triangle pointing right, a row of pixels a string.
O is the outline, L, F and D the light, face and dark of the variation
colour, S and s the shadow falling to the lower right.")

(cl-defgeneric lispm-desktop-platinum-draw-triangle (palette open)
  "Return an SVG of a list's disclosure triangle in PALETTE.
OPEN points it down, at a disclosed group; otherwise it points right.
It is drawn as Mac OS 9 drew it: outlined, in the variation colour lit
from the left, casting a shadow; pointing down, it is the same triangle
turned over its diagonal."
  (let ((svg (svg-create 12 12))
        (colors `((?O . ,(plist-get palette :outline)) (?L . ,(plist-get palette :thumb-light))
                  (?F . ,(plist-get palette :thumb)) (?D . ,(plist-get palette :thumb-dark))
                  (?S . ,(lispm-ui-grey palette #x88))
                  (?s . ,(lispm-ui-grey palette #xBB)))))
    (seq-do-indexed
     (lambda (row y)
       (seq-do-indexed
        (lambda (pixel x)
          (when-let* ((color (alist-get pixel colors)))
            (if open
                (lispm-ui-svg-dot svg y (+ x 3) color)
              (lispm-ui-svg-dot svg (+ x 3) y color))))
        row))
     lispm-desktop-platinum--triangle)
    svg))

(defun lispm-desktop-platinum--strut (width)
  "Return a blank image WIDTH pixels wide, as tall as a title bar.
Every title bar opens with one, so active and inactive bars are equally
tall and the content below never shifts when the focus moves."
  (lispm-ui-image (list 'strut width)
                                 (lambda ()
                                   (let ((svg (svg-create width lispm-desktop-platinum-title-height)))
                                     (lispm-ui-svg-fill svg 0 0 width lispm-desktop-platinum-title-height
                                                                   (plist-get lispm-ui-current-palette :chrome))
                                     svg))))

(defun lispm-desktop-platinum--box (kind &optional modified pressed)
  "Return the image of a title bar box of KIND.
See `lispm-desktop-platinum-draw-box'."
  (lispm-ui-image (list 'box kind modified pressed)
                                 (lambda () (lispm-desktop-platinum-draw-box lispm-ui-current-palette kind
                                                                             modified pressed))))

(defun lispm-desktop-platinum--title-part (width &rest options)
  "Return a stretch of title bar WIDTH pixels wide as a string displaying it.
OPTIONS are those of `lispm-desktop-platinum-draw-title-part'."
  (propertize " " 'display
              (lispm-ui-image
               (list 'title-part width
                     (plist-get options :active) (plist-get options :ridges)
                     (and (plist-get options :label)
                          (lispm-ui-content-key (plist-get options :label)))
                     (plist-get options :label-x) (plist-get options :lead) (plist-get options :trail))
               (lambda () (apply #'lispm-desktop-platinum-draw-title-part lispm-ui-current-palette
                                 width options)))))

(defun lispm-desktop-platinum-triangle (open)
  "Return a disclosure triangle as a string, pointing down when OPEN.
On a text terminal it is a character, a cell wide."
  (if (lispm-desktop-tty-p)
      (if open "▾" "▸")
    (propertize " " 'display (lispm-ui-image
                              (list 'triangle open)
                              (lambda () (lispm-desktop-platinum-draw-triangle lispm-ui-current-palette open))))))

(defvar lispm-desktop-platinum--svg-fonts (make-hash-table :test #'equal)
  "Fonts of SVG labels by palette generation and face, see
`lispm-ui-svg-font'; faces are set again with every palette.")

(defun lispm-desktop-platinum--synthetic-bold-p (face)
  "Return non-nil when Emacs draws bold FACE by thickening its regular glyphs.
A family without a bold face of its own, as Geneva, is drawn so; its
bold text then runs exactly as wide as its regular, which a real bold
face hardly ever does."
  (with-memoization (gethash (list lispm-ui-palette-generation 'synthetic face)
                             lispm-desktop-platinum--svg-fonts)
    (let ((sample "Don’t Save Cancel OK")
          (spec (if (symbolp face) (list :inherit face) face)))
      (= (string-pixel-width (propertize sample 'face `(:weight bold ,@spec)))
         (string-pixel-width (propertize sample 'face `(:weight normal ,@spec)))))))

(defun lispm-desktop-platinum--svg-families (family)
  "Return the font families SVG text set in FAMILY, a face's, is drawn
in: FAMILY, by default Geneva, then the one for Chinese and Japanese in
the language spoken, then any sans-serif."
  (string-join (delq nil (list (if (stringp family) family "Geneva")
                               (lispm-ui-text-cjk-family)
                               "sans-serif"))
               ", "))

(defun lispm-ui-svg-font (&optional face)
  "Return the font of SVG labels matching FACE, by default the titles' face.
The value is a plist: :family and :size, in pixels, are FACE's; :weight
is bold when FACE is.  A family without a bold face Emacs makes bold
itself, drawing every glyph a second time half a pixel to the right;
:synthetic is then non-nil and labels are drawn the same way, so a label
drawn as SVG is as heavy as the titles and menus Emacs draws beside it."
  (let ((face (or face 'lispm-ui-title)))
    (with-memoization (gethash (cons lispm-ui-palette-generation face)
                               lispm-desktop-platinum--svg-fonts)
      (let* ((family (face-attribute face :family nil t))
             (height (face-attribute face :height nil t))
             (bold (memq (face-attribute face :weight nil t)
                         '(semi-bold bold extra-bold ultra-bold heavy black)))
             (synthetic (and bold (display-graphic-p)
                             (lispm-desktop-platinum--synthetic-bold-p face))))
        (list :family (lispm-desktop-platinum--svg-families family)
              :size (if (numberp height) (/ height 10.0) 12)
              :weight (if (and bold (not synthetic)) "bold" "normal")
              :synthetic synthetic)))))

(defun lispm-ui-svg-label (svg text x y color font &optional anchor)
  "Write TEXT on SVG at X, its baseline at Y, in COLOR and FONT.
FONT is a value of `lispm-ui-svg-font', with its :style,
as \"italic\", and its :decoration, as \"underline\", where given;
ANCHOR is SVG's text anchor, by default the middle."
  (dolist (dx (if (plist-get font :synthetic) '(0 0.5) '(0)))
    ;; Spaces are kept, as Emacs shows them, at a run's ends too.
    (apply #'svg-text svg (substring-no-properties text) :x (+ x dx) :y y :text-anchor (or anchor "middle")
           :xml:space "preserve"
           :font-family (plist-get font :family) :font-size (plist-get font :size)
           :font-weight (plist-get font :weight) :fill color
           (append (and (plist-get font :style) (list :font-style (plist-get font :style)))
                   (and (plist-get font :decoration) (list :text-decoration (plist-get font :decoration)))))))

;;;;; Labels
;;
;; A control's label may be any text Emacs shows: words in a face, glyphs
;; of an icon font, images.  Drawn into the control's SVG run by run, it
;; keeps the widths Emacs measures for it, so a control is as wide as its
;; label would be on the line.

(defvar lispm-desktop-platinum--icon-doms (make-hash-table :test #'equal)
  "SVG documents read from files or data, by file or data.")

(defun lispm-desktop-platinum--escape-xml (string)
  "Return STRING with the characters XML reserves written as references."
  (replace-regexp-in-string "[&<>\"]"
                            (lambda (char)
                              (pcase char ("&" "&amp;") ("<" "&lt;") (">" "&gt;") (_ "&quot;")))
                            string t t))

(defun lispm-desktop-platinum--clean-dom (node)
  "Return NODE, parsed XML, ready to be printed again by `svg-print':
comments dropped, text and attribute values escaped, which `svg-print'
writes as they are."
  (cond ((stringp node) (lispm-desktop-platinum--escape-xml node))
        ((memq (car-safe node) '(comment top)) nil)
        ((consp node)
         `(,(car node)
           ,(mapcar (lambda (attribute)
                      (cons (car attribute)
                            (if (stringp (cdr attribute))
                                (lispm-desktop-platinum--escape-xml (cdr attribute))
                              (cdr attribute))))
                    (nth 1 node))
           ,@(delq nil (mapcar #'lispm-desktop-platinum--clean-dom (nthcdr 2 node)))))))

(defun lispm-ui-svg-dom (file &optional data)
  "Return the SVG document of FILE, or of the string DATA, or nil.
Nil too where Emacs cannot parse XML.  The document is cleaned to be
printed again, see `lispm-desktop-platinum--clean-dom'."
  (when (fboundp 'libxml-parse-xml-region)
    (with-memoization (gethash (or data file) lispm-desktop-platinum--icon-doms)
      (when (or data (file-readable-p file))
        (with-temp-buffer
          (if data (insert data) (insert-file-contents file))
          (let ((dom (libxml-parse-xml-region (point-min) (point-max))))
            ;; A document with a declaration or comments around its root
            ;; comes back as a `top' node holding them.
            (when (eq (car-safe dom) 'top)
              (setq dom (seq-find (lambda (node) (eq (car-safe node) 'svg)) (nthcdr 2 dom))))
            (and dom (lispm-desktop-platinum--clean-dom dom))))))))

(defun lispm-ui-svg-embed (svg dom x y width height)
  "Draw the SVG document DOM on SVG at X, Y, WIDTH by HEIGHT pixels."
  (let ((copy (copy-tree dom)))
    (dom-set-attribute copy 'x x)
    (dom-set-attribute copy 'y y)
    (dom-set-attribute copy 'width width)
    (dom-set-attribute copy 'height height)
    (dom-append-child svg copy)))

(defun lispm-ui-svg-icon (svg file x y size)
  "Draw the SVG icon FILE on SVG at X, Y, SIZE pixels square."
  (when-let* ((icon (lispm-ui-svg-dom file)))
    (lispm-ui-svg-embed svg icon x y size size)))

(defun lispm-desktop-platinum--image-size (spec)
  "Return the size in pixels of image SPEC, as (WIDTH . HEIGHT).
Where no image can be shown its :width and :height are taken, or 16."
  (or (ignore-errors (image-size spec t))
      (cons (or (image-property spec :width) 16) (or (image-property spec :height) 16))))

(defun lispm-ui-svg-emacs-image (svg spec x y width height)
  "Draw the Emacs image SPEC on SVG at X, Y, WIDTH by HEIGHT pixels.
SVG images are drawn as SVG; others are embedded as they are."
  (let* ((file (image-property spec :file))
         (data (image-property spec :data))
         (type (or (image-property spec :type)
                   (and file (image-supported-file-p file))
                   (and data (image-type-from-data data)))))
    (if (eq type 'svg)
        (when-let* ((dom (lispm-ui-svg-dom file data)))
          (lispm-ui-svg-embed svg dom x y width height))
      (when (or data (and file (file-readable-p file)))
        (svg-embed svg (or data file) (format "image/%s" (if (eq type 'jpeg) "jpeg" (or type 'png)))
                   (and data t) :x x :y y :width width :height height)))))

(defun lispm-desktop-platinum--face-attribute (faces attribute)
  "Return ATTRIBUTE of FACES, a face property's value, or nil.
The first face naming it wins, as in Emacs's own merging."
  (catch 'found
    (dolist (face (if (or (symbolp faces) (keywordp (car-safe faces))) (list faces) faces))
      (let ((value (cond ((and (symbolp face) (facep face)) (face-attribute face attribute nil t))
                         ((consp face) (or (plist-get face attribute)
                                           (when-let* ((parent (plist-get face :inherit)))
                                             (lispm-desktop-platinum--face-attribute parent attribute)))))))
        (unless (or (null value) (eq value 'unspecified))
          (throw 'found value))))
    nil))

(defun lispm-ui-svg-content (svg content x top height color face)
  "Draw CONTENT, a string as Emacs would show it, on SVG from X.
Its runs are centred in the HEIGHT rows from TOP: words and icon glyphs
in their faces layered over FACE, images as they are, stretches of space
as wide as they say.  Text without a colour of its own is drawn in COLOR.
Return the x where CONTENT ends."
  (let ((content (copy-sequence content))
        (start 0))
    (add-face-text-property 0 (length content) face t content)
    (while (< start (length content))
      (let* ((end (or (next-property-change start content) (length content)))
             (run (substring content start end))
             (width (string-pixel-width run))
             (display (get-text-property 0 'display run))
             (image (and (eq (car-safe display) 'image) display)))
        (cond
         (image
          ;; As wide as Emacs lays it out, which `image-size' can exceed
          ;; on a display that scales images up.
          (pcase-let* ((`(,iw . ,ih) (lispm-desktop-platinum--image-size image))
                       (w (if (> width 0) width iw))
                       (h (round (* ih (/ (float w) (max 1 iw))))))
            (lispm-ui-svg-emacs-image svg image x (+ top (/ (- height h) 2)) w h)))
         ;; A stretch of space, or text shown as something else.
         ((or (eq (car-safe display) 'space) (stringp display)))
         ((not (string-blank-p run))
          (let* ((faces (get-text-property 0 'face run))
                 (family (lispm-desktop-platinum--face-attribute faces :family))
                 (base (face-attribute face :height nil t))
                 (scale (seq-reduce (lambda (size face)
                                      (let ((h (cond ((consp face) (plist-get face :height))
                                                     ((and (symbolp face) (facep face))
                                                      (face-attribute face :height nil t)))))
                                        (if (floatp h) (* size h) size)))
                                    (ensure-list faces) 1.0))
                 (size (* scale (/ (if (numberp base) base 120) 10.0)))
                 (bold (memq (lispm-desktop-platinum--face-attribute faces :weight)
                             '(semi-bold bold extra-bold ultra-bold heavy black)))
                 (synthetic (and bold (display-graphic-p)
                                 (lispm-desktop-platinum--synthetic-bold-p `(:family ,family))))
                 (ink (or (lispm-desktop-platinum--face-attribute faces :foreground) color))
                 (lines (delq nil (list (and (lispm-desktop-platinum--face-attribute faces :underline) "underline")
                                        (and (lispm-desktop-platinum--face-attribute faces :strike-through)
                                             "line-through")))))
            (lispm-ui-svg-label
             svg run x (+ top (round (/ (+ height (* 0.7 size)) 2.0))) ink
             (list :family (lispm-desktop-platinum--svg-families family)
                   :size size :weight (if (and bold (not synthetic)) "bold" "normal")
                   :synthetic synthetic
                   :style (and (memq (lispm-desktop-platinum--face-attribute faces :slant) '(italic oblique))
                               "italic")
                   :decoration (and lines (string-join lines " ")))
             "start"))))
        (setq x (+ x width)
              start end)))
    x))

(defun lispm-ui-content-key (content)
  "Return what tells CONTENT apart from other contents, for memoizing:
its text and its properties, which `equal' alone ignores."
  (cons (substring-no-properties content) (object-intervals content)))

(defun lispm-ui-text-width (content face)
  "Return the width in pixels of CONTENT shown in FACE."
  (let ((content (copy-sequence content)))
    (add-face-text-property 0 (length content) face t content)
    (string-pixel-width content)))

;;
;; The controls above as strings to put on a line, of a buffer or of a
;; mode, header or tab line: a character displaying the control's image,
;; memoized, which the caller gives its keymap, help and target.

(defconst lispm-desktop-platinum-bevel-padding 8
  "Pixels between a bevel button's sides and its label.")

(provide 'lispm-desktop-platinum-draw)
;;; lispm-desktop-platinum-draw.el ends here
