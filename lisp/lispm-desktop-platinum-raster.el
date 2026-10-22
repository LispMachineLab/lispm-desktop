;;; lispm-desktop-platinum-raster.el --- Icons drawn in text  -*- lexical-binding: t; -*-

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

;; A text terminal shows no images, but a cell shows a block character
;; in two colours: ▀, ▌, ▚ and the like split it into the pixels of a 2
;; by 4 grid.  Icons are drawn so on a terminal, a 16-pixel icon 8 cells
;; by 4 lines, their SVG rasterized here, in Lisp:
;;
;;   SVG --shapes--> polygons --samples, 4 by 4 a pixel--> pixels
;;       --2 by 4 pixels a cell--> lines of block characters
;;
;; Mac OS 9's icons are pixels drawn as rectangles, so at their own size
;; they come out exactly, and at half of it each pixel keeps a colour of
;; those it stands for, see `lispm-desktop-platinum-raster--pixel'; other
;; drawings are averaged.  Paths, polygons, circles and
;; ellipses are filled and stroked, their curves followed in short lines,
;; and gradients are coloured along their axis; filters and text are left
;; out.  A pixel less than half covered is clear: the background shows.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'dom)
(require 'seq)
(require 'subr-x)

(defconst lispm-desktop-platinum-raster-samples 4
  "Samples a pixel is measured with each way: 4 by 4, 16 a pixel.")

;;;; Colours and attributes

(defun lispm-desktop-platinum-raster--color (spec)
  "Return colour SPEC as (R G B), each 0 to 1, or nil for none.
SPEC is as SVG writes it: #RGB, #RRGGBB, rgb(...) or a colour's name."
  (when (and (stringp spec) (not (member (downcase (string-trim spec)) '("none" "transparent" ""))))
    (let ((spec (string-trim spec)))
      (cond
       ((string-match "\\`#\\([[:xdigit:]]\\)\\([[:xdigit:]]\\)\\([[:xdigit:]]\\)\\'" spec)
        (mapcar (lambda (n) (/ (string-to-number (match-string n spec) 16) 15.0)) '(1 2 3)))
       ((string-match "\\`#\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{2\\}\\)" spec)
        (mapcar (lambda (n) (/ (string-to-number (match-string n spec) 16) 255.0)) '(1 2 3)))
       ((string-match "\\`rgb(\\([^,]+\\),\\([^,]+\\),\\([^)]+\\))" spec)
        (mapcar (lambda (n)
                  (let ((value (string-trim (match-string n spec))))
                    (if (string-suffix-p "%" value)
                        (/ (string-to-number value) 100.0)
                      (/ (string-to-number value) 255.0))))
                '(1 2 3)))
       ((when-let* ((entry (assoc-string spec color-name-rgb-alist t)))
          (mapcar (lambda (value) (/ value 65535.0)) (cdr entry))))))))

(defun lispm-desktop-platinum-raster--hex (red green blue)
  "Return the colour of RED, GREEN and BLUE, each 0 to 1, as \"#rrggbb\",
rounded to the nearest: `color-rgb-to-hex' cuts each down."
  (apply #'format "#%02x%02x%02x"
         (mapcar (lambda (value) (max 0 (min 255 (round (* value 255))))) (list red green blue))))

(defun lispm-desktop-platinum-raster--style (node)
  "Return the properties NODE's style attribute sets, an alist of strings."
  (when-let* ((style (dom-attr node 'style)))
    (delq nil (mapcar (lambda (declaration)
                        (when (string-match "\\`\\s-*\\([-a-z]+\\)\\s-*:\\s-*\\(.*?\\)\\s-*\\'" declaration)
                          (cons (intern (match-string 1 declaration)) (match-string 2 declaration))))
                      (split-string style ";" t)))))

(defun lispm-desktop-platinum-raster--property (node name)
  "Return the presentation property NAME of NODE, a string, or nil: its
style's, else its attribute's."
  (or (alist-get name (lispm-desktop-platinum-raster--style node))
      (dom-attr node name)))

(defun lispm-desktop-platinum-raster--number (string &optional default)
  "Return STRING read as a number, DEFAULT when it is none."
  (if (and (stringp string) (string-match "[-+]?[0-9.]" string))
      (string-to-number string)
    default))

;;;; Transforms

;; A transform is (A B C D E F), mapping X, Y to A X + C Y + E, B X + D Y + F.

(defconst lispm-desktop-platinum-raster--identity '(1 0 0 1 0 0)
  "The transform moving nothing.")

(defun lispm-desktop-platinum-raster--compose (outer inner)
  "Return the transform doing INNER, then OUTER."
  (pcase-let ((`(,a1 ,b1 ,c1 ,d1 ,e1 ,f1) outer)
              (`(,a2 ,b2 ,c2 ,d2 ,e2 ,f2) inner))
    (list (+ (* a1 a2) (* c1 b2)) (+ (* b1 a2) (* d1 b2))
          (+ (* a1 c2) (* c1 d2)) (+ (* b1 c2) (* d1 d2))
          (+ (* a1 e2) (* c1 f2) e1) (+ (* b1 e2) (* d1 f2) f1))))

(defun lispm-desktop-platinum-raster--apply (transform x y)
  "Return X, Y moved by TRANSFORM, as (X . Y)."
  (pcase-let ((`(,a ,b ,c ,d ,e ,f) transform))
    (cons (+ (* a x) (* c y) e) (+ (* b x) (* d y) f))))

(defun lispm-desktop-platinum-raster--numbers (string)
  "Return the numbers STRING holds, in order."
  (let ((numbers nil) (start 0))
    (while (string-match "[-+]?\\(?:[0-9]*\\.[0-9]+\\|[0-9]+\\.?\\)\\(?:[eE][-+]?[0-9]+\\)?" string start)
      (push (string-to-number (match-string 0 string)) numbers)
      (setq start (match-end 0)))
    (nreverse numbers)))

(defun lispm-desktop-platinum-raster--transform (string)
  "Return the transform the attribute STRING writes, the identity for none."
  (let ((transform lispm-desktop-platinum-raster--identity)
        (start 0))
    (while (and (stringp string) (string-match "\\([a-zA-Z]+\\)\\s-*(\\([^)]*\\))" string start))
      (let ((name (match-string 1 string))
            (args (lispm-desktop-platinum-raster--numbers (match-string 2 string))))
        (setq start (match-end 0)
              transform
              (lispm-desktop-platinum-raster--compose
               transform
               (pcase name
                 ("matrix" (if (= (length args) 6) args lispm-desktop-platinum-raster--identity))
                 ("translate" (list 1 0 0 1 (or (nth 0 args) 0) (or (nth 1 args) 0)))
                 ("scale" (let ((sx (or (nth 0 args) 1))) (list sx 0 0 (or (nth 1 args) sx) 0 0)))
                 ("rotate"
                  (let* ((angle (degrees-to-radians (or (nth 0 args) 0)))
                         (cx (or (nth 1 args) 0)) (cy (or (nth 2 args) 0))
                         (cos (cos angle)) (sin (sin angle)))
                    (list cos sin (- sin) cos
                          (+ cx (- (* cos cx)) (* sin cy)) (+ cy (- (* sin cx)) (- (* cos cy))))))
                 (_ lispm-desktop-platinum-raster--identity))))))
    transform))

;;;; Shapes

(defconst lispm-desktop-platinum-raster--curve-steps 8
  "Straight lines a curve is followed in.")

(defun lispm-desktop-platinum-raster--path (data)
  "Return the outlines path DATA draws, as lists of points (X . Y), its
curves followed in straight lines and its arcs drawn straight."
  (let ((tokens nil) (start 0))
    (while (string-match "[MmLlHhVvCcSsQqTtAaZz]\\|[-+]?\\(?:[0-9]*\\.[0-9]+\\|[0-9]+\\.?\\)\\(?:[eE][-+]?[0-9]+\\)?"
                         data start)
      (let ((token (match-string 0 data)))
        ;; Numbers as floats: an integer would pass for a command's character.
        (push (if (string-match-p "\\`[A-Za-z]\\'" token) (aref token 0) (float (string-to-number token)))
              tokens))
      (setq start (match-end 0)))
    (setq tokens (nreverse tokens))
    (let ((outlines nil) (outline nil) (command ?M)
          (x 0.0) (y 0.0) (start-x 0.0) (start-y 0.0) (control nil))
      (cl-labels ((next () (if (numberp (car tokens)) (pop tokens) 0))
                  (vertex (px py) (push (cons px py) outline) (setq x px y py))
                  (close () (when outline (push (nreverse outline) outlines)) (setq outline nil))
                  (curve (points)
                    ;; A Bezier curve of POINTS, the first where it starts.
                    (dotimes (i lispm-desktop-platinum-raster--curve-steps)
                      (let ((tt (/ (1+ i) (float lispm-desktop-platinum-raster--curve-steps)))
                            (ps points))
                        (while (cdr ps)
                          (setq ps (cl-loop for (a b) on ps while b
                                            collect (cons (+ (car a) (* tt (- (car b) (car a))))
                                                          (+ (cdr a) (* tt (- (cdr b) (cdr a))))))))
                        (push (car ps) outline)))
                    (setq x (car (car (last points))) y (cdr (car (last points))))))
        (while tokens
          (when (characterp (car tokens))
            (setq command (pop tokens)))
          (let ((relative (and (>= command ?a) (<= command ?z)))
                (upper (upcase command)))
            (cl-flet ((ax (v) (if relative (+ x v) v))
                      (ay (v) (if relative (+ y v) v)))
              (pcase upper
                (?M (close)
                    (let* ((px (ax (next))) (py (ay (next))))
                      (vertex px py)
                      (setq start-x px start-y py control nil
                            ;; Coordinates after a move draw lines.
                            command (if relative ?l ?L))))
                (?L (let* ((px (ax (next))) (py (ay (next)))) (vertex px py) (setq control nil)))
                (?H (vertex (ax (next)) y) (setq control nil))
                (?V (vertex x (ay (next))) (setq control nil))
                (?C (let* ((x1 (ax (next))) (y1 (ay (next))) (x2 (ax (next))) (y2 (ay (next)))
                           (px (ax (next))) (py (ay (next))))
                      (curve (list (cons x y) (cons x1 y1) (cons x2 y2) (cons px py)))
                      (setq control (cons x2 y2))))
                (?S (let* ((x1 (if control (- (* 2 x) (car control)) x))
                           (y1 (if control (- (* 2 y) (cdr control)) y))
                           (x2 (ax (next))) (y2 (ay (next))) (px (ax (next))) (py (ay (next))))
                      (curve (list (cons x y) (cons x1 y1) (cons x2 y2) (cons px py)))
                      (setq control (cons x2 y2))))
                (?Q (let* ((x1 (ax (next))) (y1 (ay (next))) (px (ax (next))) (py (ay (next))))
                      (curve (list (cons x y) (cons x1 y1) (cons px py)))
                      (setq control (cons x1 y1))))
                (?T (let* ((x1 (if control (- (* 2 x) (car control)) x))
                           (y1 (if control (- (* 2 y) (cdr control)) y))
                           (px (ax (next))) (py (ay (next))))
                      (curve (list (cons x y) (cons x1 y1) (cons px py)))
                      (setq control (cons x1 y1))))
                (?A (let* ((rx (abs (next))) (ry (abs (next))) (rotation (next))
                           (large (/= 0 (next))) (sweep (/= 0 (next)))
                           (px (ax (next))) (py (ay (next))))
                      (dolist (arc-point (lispm-desktop-platinum-raster--arc x y rx ry rotation large sweep px py))
                        (vertex (car arc-point) (cdr arc-point)))
                      (setq control nil)))
                (?Z (when outline (vertex start-x start-y))
                    (close)
                    (setq x start-x y start-y control nil)
                    ;; Numbers after a close are a new move's.
                    (when (numberp (car tokens)) (setq command ?L)))
                (_ (pop tokens))))))
        (close))
      (nreverse outlines))))

(defun lispm-desktop-platinum-raster--arc (x1 y1 rx ry rotation large sweep x2 y2)
  "Return the points of the elliptical arc from X1, Y1 to X2, Y2, as an SVG
path's A command draws it: radii RX and RY, turned ROTATION degrees, the
LARGE arc or the small one, clockwise when SWEEP; the start left out.
The arc is turned into its ellipse's centre and angles as the SVG
specification says (F.6.5), and followed in short lines."
  (if (or (zerop rx) (zerop ry) (and (= x1 x2) (= y1 y2)))
      (list (cons x2 y2))
    (let* ((phi (degrees-to-radians rotation))
           (cos (cos phi)) (sin (sin phi))
           (dx (/ (- x1 x2) 2.0)) (dy (/ (- y1 y2) 2.0))
           (x1p (+ (* cos dx) (* sin dy)))
           (y1p (+ (- (* sin dx)) (* cos dy)))
           (lambda (+ (/ (* x1p x1p) (* rx rx)) (/ (* y1p y1p) (* ry ry))))
           (rx (if (> lambda 1) (* rx (sqrt lambda)) (float rx)))
           (ry (if (> lambda 1) (* ry (sqrt lambda)) (float ry)))
           (numerator (- (* rx rx ry ry) (* rx rx y1p y1p) (* ry ry x1p x1p)))
           (denominator (+ (* rx rx y1p y1p) (* ry ry x1p x1p)))
           (coef (* (if (eq large sweep) -1 1)
                    (sqrt (max 0.0 (/ numerator (max denominator 1e-12))))))
           (cxp (/ (* coef rx y1p) ry))
           (cyp (- (/ (* coef ry x1p) rx)))
           (cx (+ (- (* cos cxp) (* sin cyp)) (/ (+ x1 x2) 2.0)))
           (cy (+ (* sin cxp) (* cos cyp) (/ (+ y1 y2) 2.0)))
           (angle (lambda (ux uy vx vy) (atan (- (* ux vy) (* uy vx)) (+ (* ux vx) (* uy vy)))))
           (ux (/ (- x1p cxp) rx)) (uy (/ (- y1p cyp) ry))
           (vx (/ (- (- x1p) cxp) rx)) (vy (/ (- (- y1p) cyp) ry))
           (start (funcall angle 1 0 ux uy))
           (delta (funcall angle ux uy vx vy))
           (delta (cond ((and (not sweep) (> delta 0)) (- delta (* 2 float-pi)))
                        ((and sweep (< delta 0)) (+ delta (* 2 float-pi)))
                        (t delta)))
           (steps (max 2 (ceiling (* 16 (/ (abs delta) (* 2 float-pi)))))))
      (mapcar (lambda (i)
                (let ((theta (+ start (* delta (/ i (float steps))))))
                  (cons (+ cx (- (* cos rx (cos theta)) (* sin ry (sin theta))))
                        (+ cy (* sin rx (cos theta)) (* cos ry (sin theta))))))
              (number-sequence 1 steps)))))

(defun lispm-desktop-platinum-raster--ellipse (cx cy rx ry)
  "Return the outline of the ellipse at CX, CY of radii RX and RY."
  (list (mapcar (lambda (i)
                  (let ((angle (* 2 float-pi (/ i 32.0))))
                    (cons (+ cx (* rx (cos angle))) (+ cy (* ry (sin angle))))))
                (number-sequence 0 32))))

(defun lispm-desktop-platinum-raster--outlines (node)
  "Return the outlines NODE, a shape, draws in its own units, or nil."
  (cl-flet ((num (name &optional default)
              (lispm-desktop-platinum-raster--number (dom-attr node name) (or default 0))))
    (pcase (dom-tag node)
      ('rect (let ((x (num 'x)) (y (num 'y)) (w (num 'width)) (h (num 'height)))
               (when (and (> w 0) (> h 0))
                 (list (list (cons x y) (cons (+ x w) y) (cons (+ x w) (+ y h)) (cons x (+ y h)))))))
      ('circle (lispm-desktop-platinum-raster--ellipse (num 'cx) (num 'cy) (num 'r) (num 'r)))
      ('ellipse (lispm-desktop-platinum-raster--ellipse (num 'cx) (num 'cy) (num 'rx) (num 'ry)))
      ((or 'polygon 'polyline)
       (let ((numbers (lispm-desktop-platinum-raster--numbers (or (dom-attr node 'points) ""))))
         (list (cl-loop for (x y) on numbers by #'cddr when y collect (cons x y)))))
      ('line (list (list (cons (num 'x1) (num 'y1)) (cons (num 'x2) (num 'y2)))))
      ('path (when-let* ((data (dom-attr node 'd)))
               (lispm-desktop-platinum-raster--path data))))))

;;;; Gradients

(defun lispm-desktop-platinum-raster--gradients (dom)
  "Return the gradients DOM defines, by id: plists of :kind, :stops, a list
of (OFFSET COLOR OPACITY), :units and their geometry.  A gradient taking
another's stops by its href has them."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (node (append (dom-by-tag dom 'linearGradient) (dom-by-tag dom 'radialGradient)))
      (when-let* ((id (dom-attr node 'id)))
        (puthash id
                 (list :kind (dom-tag node)
                       :href (string-remove-prefix "#" (or (dom-attr node 'xlink:href) (dom-attr node 'href) ""))
                       :units (dom-attr node 'gradientUnits)
                       :transform (lispm-desktop-platinum-raster--transform (dom-attr node 'gradientTransform))
                       :geometry (mapcar (lambda (name) (cons name (dom-attr node name)))
                                         '(x1 y1 x2 y2 cx cy r))
                       :stops (mapcar (lambda (stop)
                                        (let ((offset (or (lispm-desktop-platinum-raster--property stop 'offset) "0")))
                                          (list (if (string-suffix-p "%" offset)
                                                    (/ (string-to-number offset) 100.0)
                                                  (string-to-number offset))
                                                (lispm-desktop-platinum-raster--color
                                                 (lispm-desktop-platinum-raster--property stop 'stop-color))
                                                (lispm-desktop-platinum-raster--number
                                                 (lispm-desktop-platinum-raster--property stop 'stop-opacity) 1))))
                                      (dom-by-tag node 'stop)))
                 table)))
    ;; Stops and geometry a gradient leaves out are its href's.
    (maphash (lambda (_ gradient)
               (let ((seen nil) (from gradient))
                 (while (and (not (plist-get gradient :stops)) (plist-get from :href)
                             (not (member (plist-get from :href) seen)))
                   (push (plist-get from :href) seen)
                   (setq from (gethash (plist-get from :href) table))
                   (when from
                     (plist-put gradient :stops (plist-get from :stops))))))
             table)
    table))

(defun lispm-desktop-platinum-raster--stop-color (stops offset)
  "Return the colour and opacity of STOPS at OFFSET, 0 to 1: (COLOR . OPACITY)."
  (let* ((stops (seq-filter #'cadr stops))
         (offset (max 0.0 (min 1.0 offset)))
         (before (car stops)) (after (car (last stops))))
    (dolist (stop stops)
      (when (<= (car stop) offset) (setq before stop))
      (when (and (>= (car stop) offset) (> (car after) (car stop))) (setq after stop)))
    (when before
      (let* ((span (- (car after) (car before)))
             (tt (if (> span 0) (/ (- offset (car before)) span) 0.0)))
        (cons (cl-mapcar (lambda (a b) (+ a (* tt (- b a)))) (nth 1 before) (nth 1 after))
              (+ (nth 2 before) (* tt (- (nth 2 after) (nth 2 before)))))))))

(defun lispm-desktop-platinum-raster--paint (spec gradients bounds to-grid)
  "Return how SPEC, a fill or stroke, paints a sample: a function of the
sample's X and Y returning (COLOR . OPACITY), or nil for none.  GRADIENTS
are the document's; BOUNDS, (X0 Y0 X1 Y1) in the grid, the shape's, for
a gradient across it; TO-GRID maps the shape's user units to the grid."
  (cond
   ((and (stringp spec) (string-match "\\`url(#\\([^)]+\\))" spec))
    (when-let* ((gradient (gethash (match-string 1 spec) gradients))
                (stops (plist-get gradient :stops)))
      (pcase-let* ((`(,x0 ,y0 ,x1 ,y1) bounds)
                   (geometry (plist-get gradient :geometry))
                   (user (equal (plist-get gradient :units) "userSpaceOnUse"))
                   (value (lambda (name default)
                            (let ((string (alist-get name geometry)))
                              (if (and string (string-suffix-p "%" string))
                                  (/ (string-to-number string) 100.0)
                                (lispm-desktop-platinum-raster--number string default)))))
                   ;; Where a point of the gradient is in the grid.
                   (place (lambda (gx gy)
                            (if user
                                (lispm-desktop-platinum-raster--apply
                                 (lispm-desktop-platinum-raster--compose to-grid (plist-get gradient :transform)) gx gy)
                              (cons (+ x0 (* gx (- x1 x0))) (+ y0 (* gy (- y1 y0))))))))
        (if (eq (plist-get gradient :kind) 'radialGradient)
            (let* ((center (funcall place (funcall value 'cx 0.5) (funcall value 'cy 0.5)))
                   (edge (funcall place (+ (funcall value 'cx 0.5) (funcall value 'r 0.5))
                                  (funcall value 'cy 0.5)))
                   (radius (max 1e-6 (abs (- (car edge) (car center))))))
              (lambda (x y)
                (lispm-desktop-platinum-raster--stop-color
                 stops (/ (sqrt (+ (expt (- x (car center)) 2) (expt (- y (cdr center)) 2))) radius))))
          (let* ((from (funcall place (funcall value 'x1 0) (funcall value 'y1 0)))
                 (to (funcall place (funcall value 'x2 1) (funcall value 'y2 0)))
                 (dx (- (car to) (car from))) (dy (- (cdr to) (cdr from)))
                 (length (max 1e-6 (+ (* dx dx) (* dy dy)))))
            (lambda (x y)
              (lispm-desktop-platinum-raster--stop-color
               stops (/ (+ (* (- x (car from)) dx) (* (- y (cdr from)) dy)) length))))))))
   ((lispm-desktop-platinum-raster--color spec)
    (let ((paint (cons (lispm-desktop-platinum-raster--color spec) 1.0)))
      (lambda (_ _) paint)))))

;;;; Rasterizing

(defun lispm-desktop-platinum-raster--fill (canvas size outlines rule paint opacity)
  "Fill OUTLINES, in the grid's samples, on CANVAS, SIZE samples a side, by
fill RULE, `nonzero' or `evenodd', with PAINT at OPACITY."
  (let ((edges nil) (top size) (bottom 0))
    (dolist (outline outlines)
      (cl-loop for (a b) on (append outline (list (car outline))) while b
               do (unless (= (cdr a) (cdr b))
                    (push (list (car a) (cdr a) (car b) (cdr b)) edges))
               (setq top (min top (cdr a)) bottom (max bottom (cdr a)))))
    (cl-loop for row from (max 0 (floor top)) below (min size (ceiling bottom))
             for y = (+ row 0.5)
             do (let ((crossings nil))
                  (dolist (edge edges)
                    (pcase-let ((`(,x1 ,y1 ,x2 ,y2) edge))
                      (when (or (and (<= y1 y) (< y y2)) (and (<= y2 y) (< y y1)))
                        (push (cons (+ x1 (/ (* (- y y1) (- x2 x1)) (- y2 y1))) (if (< y1 y2) 1 -1))
                              crossings))))
                  (setq crossings (sort crossings (lambda (a b) (< (car a) (car b)))))
                  (let ((winding 0))
                    (cl-loop for (here next) on crossings while next
                             do (setq winding (if (eq rule 'evenodd) (logxor winding 1)
                                                (+ winding (cdr here))))
                             (unless (zerop winding)
                               (cl-loop for column from (max 0 (ceiling (- (car here) 0.5)))
                                        to (min (1- size) (floor (- (car next) 0.5)))
                                        do (lispm-desktop-platinum-raster--put
                                            canvas size column row
                                            (funcall paint (+ column 0.5) y) opacity)))))))))

(defun lispm-desktop-platinum-raster--stroke (canvas size outlines closed width paint opacity)
  "Stroke OUTLINES, closed when CLOSED, WIDTH samples wide, on CANVAS, SIZE
samples a side, with PAINT at OPACITY: every sample within half WIDTH of
a line."
  (let ((half (/ width 2.0)))
    (dolist (outline outlines)
      (cl-loop for (a b) on (if closed (append outline (list (car outline))) outline) while b
               do (pcase-let ((`(,x1 . ,y1) a) (`(,x2 . ,y2) b))
                    (let* ((dx (- x2 x1)) (dy (- y2 y1)) (length (+ (* dx dx) (* dy dy))))
                      (cl-loop for row from (max 0 (floor (- (min y1 y2) half)))
                               below (min size (ceiling (+ (max y1 y2) half)))
                               do (cl-loop for column from (max 0 (floor (- (min x1 x2) half)))
                                           below (min size (ceiling (+ (max x1 x2) half)))
                                           do (let* ((px (+ column 0.5)) (py (+ row 0.5))
                                                     (tt (if (> length 0)
                                                             (max 0.0 (min 1.0 (/ (+ (* (- px x1) dx) (* (- py y1) dy))
                                                                                  length)))
                                                           0.0))
                                                     (ex (- px (+ x1 (* tt dx)))) (ey (- py (+ y1 (* tt dy)))))
                                                (when (<= (+ (* ex ex) (* ey ey)) (* half half))
                                                  (lispm-desktop-platinum-raster--put
                                                   canvas size column row (funcall paint px py) opacity)))))))))))

(defun lispm-desktop-platinum-raster--put (canvas size column row paint opacity)
  "Paint the sample at COLUMN, ROW of CANVAS, SIZE samples a side, with
PAINT, (COLOR . OPACITY), at OPACITY more, over what is there."
  (when paint
    (let* ((alpha (* opacity (cdr paint)))
           (index (+ column (* row size)))
           (under (aref canvas index)))
      (when (> alpha 0)
        (aset canvas index
              (if (or (null under) (>= alpha 1))
                  (cons (car paint) (max alpha (if under (cdr under) 0)))
                (cons (cl-mapcar (lambda (top bottom) (+ (* alpha top) (* (- 1 alpha) bottom)))
                                 (car paint) (car under))
                      (+ alpha (* (- 1 alpha) (cdr under))))))))))

(defun lispm-desktop-platinum-raster--draw (node canvas size to-grid gradients inherited)
  "Draw NODE and what it holds on CANVAS, SIZE samples a side.  TO-GRID maps
NODE's parent's units to the grid; GRADIENTS are the document's;
INHERITED, an alist, the presentation properties NODE's parents set."
  (when (consp node)
    (let* ((to-grid (lispm-desktop-platinum-raster--compose
                     to-grid (lispm-desktop-platinum-raster--transform (dom-attr node 'transform))))
           (properties (let ((properties (copy-alist inherited)))
                         (dolist (name '(fill stroke stroke-width fill-rule fill-opacity stroke-opacity))
                           (when-let* ((value (lispm-desktop-platinum-raster--property node name)))
                             (setf (alist-get name properties) value)))
                         ;; Opacity is not inherited, but a group's dims what it holds.
                         (setf (alist-get 'opacity properties)
                               (* (or (alist-get 'opacity inherited) 1.0)
                                  (lispm-desktop-platinum-raster--number
                                   (lispm-desktop-platinum-raster--property node 'opacity) 1.0)))
                         properties)))
      (pcase (dom-tag node)
        ((guard (equal (lispm-desktop-platinum-raster--property node 'display) "none")))
        ((or 'defs 'linearGradient 'radialGradient 'filter 'metadata 'title 'desc 'text 'clipPath 'mask 'style))
        ((or 'g 'svg 'a 'switch)
         (dolist (child (dom-children node))
           (lispm-desktop-platinum-raster--draw child canvas size to-grid gradients properties)))
        (_
         (when-let* ((outlines (lispm-desktop-platinum-raster--outlines node))
                     (grid (mapcar (lambda (outline)
                                     (mapcar (lambda (point)
                                               (lispm-desktop-platinum-raster--apply to-grid (car point) (cdr point)))
                                             outline))
                                   outlines))
                     (points (apply #'append grid)))
           (let* ((bounds (list (apply #'min (mapcar #'car points)) (apply #'min (mapcar #'cdr points))
                                (apply #'max (mapcar #'car points)) (apply #'max (mapcar #'cdr points))))
                  (opacity (alist-get 'opacity properties))
                  (fill (if (memq (dom-tag node) '(line polyline)) "none" (or (alist-get 'fill properties) "black")))
                  (stroke (alist-get 'stroke properties))
                  (scale (sqrt (abs (- (* (nth 0 to-grid) (nth 3 to-grid)) (* (nth 1 to-grid) (nth 2 to-grid)))))))
             (when-let* ((paint (lispm-desktop-platinum-raster--paint fill gradients bounds to-grid)))
               (lispm-desktop-platinum-raster--fill
                canvas size grid (if (equal (alist-get 'fill-rule properties) "evenodd") 'evenodd 'nonzero)
                paint (* opacity (lispm-desktop-platinum-raster--number (alist-get 'fill-opacity properties) 1.0))))
             (when-let* ((width (lispm-desktop-platinum-raster--number (alist-get 'stroke-width properties) 1.0))
                         ((> width 0))
                         (paint (lispm-desktop-platinum-raster--paint stroke gradients bounds to-grid)))
               ;; An outline stays a pixel wide, however small the icon.
               (lispm-desktop-platinum-raster--stroke
                canvas size grid (not (memq (dom-tag node) '(line polyline)))
                (max lispm-desktop-platinum-raster-samples (* scale width))
                paint (* opacity (lispm-desktop-platinum-raster--number (alist-get 'stroke-opacity properties) 1.0)))))))))))

(defun lispm-desktop-platinum-raster--pixel (canvas grid column row samples crisp)
  "Return the colour of pixel COLUMN, ROW of CANVAS, GRID samples a side,
SAMPLES by SAMPLES of them a pixel, or nil when it is less than half
covered.  A CRISP drawing, pixels drawn as rectangles, keeps its colours:
the pixel takes the colour covering most of it.  Of two covering as
much, as an outline a pixel wide and what it outlines do in an icon drawn
at half its size, it takes the darker, so that the outline survives;
but the lighter where the darker covers most of the pixel's surroundings,
half a pixel around it, so that what close outlines outline survives too.
Otherwise the colours are averaged."
  (let ((red 0.0) (green 0.0) (blue 0.0) (alpha 0.0) (shares nil))
    (dotimes (sy samples)
      (dotimes (sx samples)
        (when-let* ((sample (aref canvas (+ (* column samples) sx (* grid (+ (* row samples) sy))))))
          (let ((a (cdr sample)) (rgb (car sample)))
            (setq red (+ red (* a (nth 0 rgb)))
                  green (+ green (* a (nth 1 rgb)))
                  blue (+ blue (* a (nth 2 rgb)))
                  alpha (+ alpha a))
            (when crisp
              (let ((share (assoc rgb shares)))
                (if share (setcdr share (+ (cdr share) a)) (push (cons rgb a) shares))))))))
    (when (>= alpha (/ (* samples samples) 2.0))
      (if crisp
          (let* ((shares (sort shares :key #'cdr :reverse t))
                 (most (cdr (car shares)))
                 (tied (seq-take-while (lambda (share) (= (cdr share) most)) shares)))
            (apply #'lispm-desktop-platinum-raster--hex
                   (car (if (cdr tied)
                            (let* ((tied (sort tied :key (lambda (share) (apply #'+ (car share)))))
                                   (around (lispm-desktop-platinum-raster--around
                                            canvas grid column row samples)))
                              (if (> (* 2 (or (cdr (assoc (car (car tied)) around)) 0))
                                     (apply #'+ (mapcar #'cdr around)))
                                  (car (last tied))
                                (car tied)))
                          (car shares)))))
        (lispm-desktop-platinum-raster--hex (/ red alpha) (/ green alpha) (/ blue alpha))))))

(defun lispm-desktop-platinum-raster--around (canvas grid column row samples)
  "Return how much of each colour lies within half a pixel around pixel
COLUMN, ROW of CANVAS, GRID samples a side, SAMPLES by SAMPLES of them
a pixel, outside it: an alist of (RGB . COVERAGE)."
  (let* ((ring (max 1 (/ samples 2)))
         (left (* column samples)) (top (* row samples))
         (shares nil))
    (cl-loop for y from (max 0 (- top ring)) below (min grid (+ top samples ring))
             do (cl-loop for x from (max 0 (- left ring)) below (min grid (+ left samples ring))
                         unless (and (<= left x) (< x (+ left samples)) (<= top y) (< y (+ top samples)))
                         do (when-let* ((sample (aref canvas (+ x (* grid y)))))
                              (let ((share (assoc (car sample) shares)))
                                (if share (setcdr share (+ (cdr share) (cdr sample)))
                                  (push (cons (car sample) (cdr sample)) shares))))))
    shares))

(defun lispm-desktop-platinum-raster-dom (dom size)
  "Return SVG document DOM drawn SIZE pixels a side: a vector of SIZE rows,
each a vector of SIZE colours, \"#RRGGBB\", or nil where clear.  A
document asking for crisp edges, as Mac OS 9's pixel icons do, keeps its
colours, see `lispm-desktop-platinum-raster--pixel'."
  (let* ((samples lispm-desktop-platinum-raster-samples)
         (crisp (equal (dom-attr dom 'shape-rendering) "crispEdges"))
         (grid (* size samples))
         (canvas (make-vector (* grid grid) nil))
         (box (lispm-desktop-platinum-raster--numbers (or (dom-attr dom 'viewBox) "")))
         (box (if (= (length box) 4) box
                (list 0 0 (lispm-desktop-platinum-raster--number (dom-attr dom 'width) size)
                      (lispm-desktop-platinum-raster--number (dom-attr dom 'height) size))))
         (scale (/ (float grid) (max (nth 2 box) (nth 3 box))))
         (to-grid (list scale 0 0 scale (- (* scale (nth 0 box))) (- (* scale (nth 1 box))))))
    (lispm-desktop-platinum-raster--draw dom canvas grid to-grid
                                         (lispm-desktop-platinum-raster--gradients dom) nil)
    (let ((pixels (make-vector size nil)))
      (dotimes (row size)
        (let ((line (make-vector size nil)))
          (dotimes (column size)
            (aset line column (lispm-desktop-platinum-raster--pixel canvas grid column row samples crisp)))
          (aset pixels row line)))
      pixels)))

(defvar lispm-desktop-platinum--rasters (make-hash-table :test #'equal)
  "Icons drawn as pixels, by (FILE SIZE), see `lispm-desktop-platinum-raster'.")

(defun lispm-desktop-platinum-raster (file size)
  "Return the SVG FILE drawn SIZE pixels a side, as
`lispm-desktop-platinum-raster-dom' returns it, or nil where it cannot
be read: kept, as the same icon is drawn again and again."
  (with-memoization (gethash (list file size) lispm-desktop-platinum--rasters)
    (when-let* (((fboundp 'libxml-parse-xml-region))
                ((file-readable-p file))
                ((string-suffix-p ".svg" file t))
                (dom (with-temp-buffer
                       (insert-file-contents file)
                       (let ((dom (libxml-parse-xml-region (point-min) (point-max))))
                         (if (eq (car-safe dom) 'top)
                             (seq-find (lambda (node) (eq (car-safe node) 'svg)) (nthcdr 2 dom))
                           dom)))))
      (ignore-errors (lispm-desktop-platinum-raster-dom dom size)))))

;;;; Pixels as text

(defun lispm-desktop-platinum--rgb (color)
  "Return COLOR, \"#RRGGBB\", as (R G B), each 0 to 1, exactly: a text
terminal's own colours, which `color-name-to-rgb' would round it to, do
not come into it."
  (lispm-desktop-platinum-raster--color color))

(defun lispm-desktop-platinum--darker (color)
  "Return COLOR, \"#RRGGBB\", at half its brightness, as Mac OS 9 darkened
a selected icon."
  (apply #'lispm-desktop-platinum-raster--hex
         (mapcar (lambda (value) (/ value 2)) (lispm-desktop-platinum--rgb color))))

(defconst lispm-desktop-platinum-text-cell '(2 . 4)
  "Pixels a cell of text shows of a picture, (ACROSS . DOWN): a cell is
about twice as tall as it is wide, so they are square.")

(defun lispm-desktop-platinum-text-size (size)
  "Return how large a picture SIZE pixels a side is drawn in text, see
`lispm-desktop-platinum-text-picture': (COLUMNS . LINES)."
  (cons (ceiling size (car lispm-desktop-platinum-text-cell))
        (ceiling size (cdr lispm-desktop-platinum-text-cell))))

(defconst lispm-desktop-platinum--blocks
  '(("▀" "▄" . #b00001111) ("▌" "▐" . #b01010101)
    ("▂" nil . #b11000000) ("▆" nil . #b11111100)
    ("▘" "▟" . #b00000101) ("▝" "▙" . #b00001010)
    ("▖" "▜" . #b01010000) ("▗" "▛" . #b10100000)
    ("▚" "▞" . #b10100101))
  "The block characters a cell of a picture is drawn with: (CHARACTER
OTHER . PIXELS).  PIXELS has a bit for each of the cell's 2 by 4
pixels, the top row's left one lowest, set where CHARACTER is drawn in
its foreground; OTHER, where there is one, is drawn just where it is
not.  The simplest come first.")

(defun lispm-desktop-platinum--cell (pixels distance)
  "Return the block that draws PIXELS, the eight of a cell, closest, as
\(CHARACTER IN OUT): of `lispm-desktop-platinum--blocks', the
character, the colour of the pixels it covers and that of the rest,
each one of theirs and nil for clear; CHARACTER nil where they are all
one colour.  DISTANCE says how far apart two pixels' colours are."
  (let* ((colors (seq-uniq pixels))
         (count (length colors)))
    (if (= count 1)
        (list nil (car colors) (car colors))
      ;; The pixels as indexes into COLORS, and the distances between
      ;; those, reckoned once.
      (let* ((colors (vconcat colors))
             (indexes (vconcat (mapcar (lambda (pixel) (seq-position colors pixel)) pixels)))
             (distances (make-vector (* count count) 0))
             (best nil) (best-error nil))
        (dotimes (i count)
          (dotimes (j i)
            (let ((d (funcall distance (aref colors i) (aref colors j))))
              (aset distances (+ (* i count) j) d)
              (aset distances (+ (* j count) i) d))))
        (cl-flet ((closest (bits side)
                    ;; The colour of the pixels on SIDE of BITS, t for
                    ;; those set, nearest them all, and how far they are
                    ;; from it in all: (ERROR . INDEX).
                    (let ((found nil))
                      (dotimes (candidate 8)
                        (when (eq side (/= 0 (logand bits (ash 1 candidate))))
                          (let ((color (aref indexes candidate)) (sum 0))
                            (dotimes (pixel 8)
                              (when (eq side (/= 0 (logand bits (ash 1 pixel))))
                                (setq sum (+ sum (aref distances (+ (* color count) (aref indexes pixel)))))))
                            (when (or (null found) (< sum (car found)))
                              (setq found (cons sum color))))))
                      found)))
          (pcase-dolist (`(,character ,_ . ,bits) lispm-desktop-platinum--blocks)
            (let* ((in (closest bits t))
                   (out (closest bits nil))
                   (error (+ (car in) (car out))))
              (when (or (null best-error) (< error best-error))
                (setq best-error error
                      best (list character (aref colors (cdr in)) (aref colors (cdr out))))))))
        best))))

(defun lispm-desktop-platinum-text-picture (pixels &optional darkened background)
  "Return PIXELS, as `lispm-desktop-platinum-raster-dom' returns them, as
lines of text, a cell for each 2 by 4 of them, see
`lispm-desktop-platinum-text-cell': the block character and two of its
pixels' colours that draw them closest, see
`lispm-desktop-platinum--blocks'.  What is clear is left without a
colour where it can be, so that what is under the picture shows there;
BACKGROUND, the colour under it, by default the frame's, stands in for
it in the reckoning, and where a character cannot leave it so.
DARKENED halves every colour."
  (pcase-let* ((`(,across . ,down) lispm-desktop-platinum-text-cell)
               (rows (length pixels))
               (columns (if (> rows 0) (length (aref pixels 0)) 0))
               (background (or (and (stringp background) (color-defined-p background) background)
                               (let ((color (face-background 'default nil t)))
                                 (and (stringp color) (color-defined-p color) color))
                               "#000000"))
               (rgbs (make-hash-table :test #'equal))
               (rgb (lambda (color)
                      (with-memoization (gethash color rgbs)
                        (let ((color (or color background)))
                          (if (string-match-p "\\`#[[:xdigit:]]\\{6\\}\\'" color)
                              (lispm-desktop-platinum--rgb color)
                            (color-name-to-rgb color))))))
               (distance (lambda (a b)
                           (if (equal a b) 0
                             (apply #'+ (cl-mapcar (lambda (x y) (* (- x y) (- x y)))
                                                   (funcall rgb a) (funcall rgb b)))))))
    (cl-loop
     for top from 0 below rows by down
     collect
     (mapconcat
      (lambda (left)
        (let ((cell (cl-loop for row from top below (+ top down)
                             append (cl-loop for column from left below (+ left across)
                                             collect (let ((pixel (and (< row rows) (< column columns)
                                                                       (aref (aref pixels row) column))))
                                                       (if (and pixel darkened)
                                                           (lispm-desktop-platinum--darker pixel)
                                                         pixel))))))
          (pcase-let ((`(,character ,in ,out) (lispm-desktop-platinum--cell cell distance)))
            (cond ((and (null in) (null out)) " ")
                  ((null character) (propertize " " 'face `(:background ,in)))
                  ((null out) (propertize character 'face `(:foreground ,in)))
                  ((null in)
                   (if-let* ((other (cadr (assoc character lispm-desktop-platinum--blocks))))
                       (propertize other 'face `(:foreground ,out))
                     (propertize character 'face `(:foreground ,background :background ,out))))
                  (t (propertize character 'face `(:foreground ,in :background ,out)))))))
      (number-sequence 0 (1- columns) across)
      ""))))

(defun lispm-desktop-platinum-raster-color (pixels)
  "Return the colour PIXELS are drawn in most, black, white and greys but
where nothing else is, or nil where they are all clear: what stands for
the picture where only a character's room is left."
  (let ((counts (make-hash-table :test #'equal)))
    (seq-doseq (row pixels)
      (seq-doseq (pixel row)
        (when pixel (cl-incf (gethash pixel counts 0)))))
    (let ((best nil) (best-grey nil))
      (maphash (lambda (color count)
                 (pcase-let ((`(,_ ,saturation ,_) (apply #'color-rgb-to-hsl (lispm-desktop-platinum--rgb color))))
                   (if (> saturation 0.15)
                       (when (or (null best) (> count (cdr best))) (setq best (cons color count)))
                     (when (or (null best-grey) (> count (cdr best-grey))) (setq best-grey (cons color count))))))
               counts)
      (car (or best best-grey)))))

(defun lispm-desktop-platinum-text-compose (width height pieces)
  "Return HEIGHT lines of text WIDTH cells wide holding PIECES, a list of
\(ROW COLUMN STRING): each STRING at COLUMN of line ROW, the lines
blank elsewhere.  Pieces of a line stand left to right and do not
overlap; one past the right edge is cut."
  (let ((lines (make-vector (max 0 height) nil)))
    (pcase-dolist (`(,row ,column ,string) pieces)
      (when (< -1 row height)
        (push (cons column string) (aref lines row))))
    (mapcar (lambda (parts)
              (let ((line "") (at 0))
                (pcase-dolist (`(,column . ,string) (sort parts (lambda (a b) (< (car a) (car b)))))
                  (when (< (max column at) width)
                    (let ((string (truncate-string-to-width string (- width (max column at)))))
                      (setq line (concat line (make-string (max 0 (- column at)) ?\s) string)
                            at (+ (max column at) (string-width string))))))
                (concat line (make-string (max 0 (- width at)) ?\s))))
            lines)))

;;;; Pixels as a PNG

(defun lispm-desktop-platinum-raster-pixel-size (file)
  "Return the size in pixels SVG FILE is drawn for, where it is pixels
drawn as rectangles, asking for crisp edges, as Mac OS 9's icons are;
else nil: a drawing of any size."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file nil 0 512)
      (goto-char (point-min))
      (and (re-search-forward "shape-rendering=\"crispEdges\"" nil t)
           (progn (goto-char (point-min))
                  (re-search-forward "<svg[^>]*\\bwidth=\"\\([0-9]+\\)\"" nil t))
           (string-to-number (match-string 1))))))

(defconst lispm-desktop-platinum--crc-table
  (let ((table (make-vector 256 0)))
    (dotimes (n 256)
      (let ((c n))
        (dotimes (_ 8)
          (setq c (if (= 1 (logand c 1)) (logxor #xEDB88320 (ash c -1)) (ash c -1))))
        (aset table n c)))
    table)
  "The CRC-32 of each byte, see `lispm-desktop-platinum--crc'.")

(defun lispm-desktop-platinum--crc (string)
  "Return the CRC-32 of unibyte STRING, as PNG reckons it."
  (let ((c #xFFFFFFFF))
    (dotimes (i (length string))
      (setq c (logxor (aref lispm-desktop-platinum--crc-table (logand (logxor c (aref string i)) #xFF))
                      (ash c -8))))
    (logxor c #xFFFFFFFF)))

(defun lispm-desktop-platinum--uint32 (n)
  "Return N as four bytes, the most significant first."
  (unibyte-string (logand (ash n -24) 255) (logand (ash n -16) 255) (logand (ash n -8) 255) (logand n 255)))

(defun lispm-desktop-platinum--zlib (data)
  "Return unibyte DATA as a zlib stream of blocks stored as they are: a
PNG of an icon is small, and Emacs has no deflate of its own."
  (let ((a 1) (b 0) (blocks nil) (offset 0) (length (length data)))
    (dotimes (i length)
      (setq a (% (+ a (aref data i)) 65521)
            b (% (+ b a) 65521)))
    (while (let* ((end (min length (+ offset 65535)))
                  (size (- end offset)))
             (push (concat (unibyte-string (if (= end length) 1 0)
                                           (logand size 255) (logand (ash size -8) 255)
                                           (logand (lognot size) 255) (logand (ash (lognot size) -8) 255))
                           (substring data offset end))
                   blocks)
             (setq offset end)
             (< offset length)))
    (concat (unibyte-string #x78 #x01) (apply #'concat (nreverse blocks))
            (lispm-desktop-platinum--uint32 (logior (ash b 16) a)))))

(defun lispm-desktop-platinum--png-chunk (type data)
  "Return the PNG chunk of TYPE, four letters, holding unibyte DATA."
  (concat (lispm-desktop-platinum--uint32 (length data)) type data
          (lispm-desktop-platinum--uint32 (lispm-desktop-platinum--crc (concat type data)))))

(defun lispm-desktop-platinum-raster-png (pixels scale width height &optional background)
  "Return a PNG image, as a unibyte string, WIDTH by HEIGHT pixels with
PIXELS, as `lispm-desktop-platinum-raster-dom' returns them, in its
middle, each a SCALE by SCALE square, so that they stay crisp.  The rest
is BACKGROUND, \"#RRGGBB\", clear pixels too, so that the image is opaque;
or, without BACKGROUND, clear."
  (let* ((size (length pixels))
         (drawn (* size scale))
         (left (/ (- width drawn) 2))
         (top (/ (- height drawn) 2))
         (channels (if background 3 4))
         (stride (1+ (* channels width)))
         (raw (make-string (* height stride) 0))
         (bytes (make-hash-table :test #'equal))
         (rgb (lambda (color)
                (with-memoization (gethash color bytes)
                  (append (mapcar (lambda (value) (round (* 255 value))) (lispm-desktop-platinum--rgb color))
                          (and (not background) (list 255))))))
         (ground (if background (funcall rgb background) (list 0 0 0 0))))
    (dotimes (y height)
      (let ((row (and (<= top y) (< y (+ top drawn)) (aref pixels (/ (- y top) scale)))))
        (dotimes (x width)
          (let ((color (and row (<= left x) (< x (+ left drawn)) (aref row (/ (- x left) scale))))
                (at (+ (* y stride) 1 (* channels x))))
            (seq-do-indexed (lambda (value index) (aset raw (+ at index) value))
                            (if color (funcall rgb color) ground))))))
    (concat "\x89PNG\r\n\x1a\n"
            (lispm-desktop-platinum--png-chunk
             "IHDR" (concat (lispm-desktop-platinum--uint32 width) (lispm-desktop-platinum--uint32 height)
                            ;; 8 bits a channel, RGB or RGBA, deflate, no
                            ;; filter, not interlaced.
                            (unibyte-string 8 (if background 2 6) 0 0 0)))
            (lispm-desktop-platinum--png-chunk "IDAT" (lispm-desktop-platinum--zlib raw))
            (lispm-desktop-platinum--png-chunk "IEND" ""))))

(provide 'lispm-desktop-platinum-raster)
;;; lispm-desktop-platinum-raster.el ends here
