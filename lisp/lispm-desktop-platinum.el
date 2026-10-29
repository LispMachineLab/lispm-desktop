;;; lispm-desktop-platinum.el --- Mac OS 9's layout and forms, in the theme's colours  -*- lexical-binding: t; -*-

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

;; The frame is laid out as Mac OS 9 laid out its desktop, and drawn in the
;; forms of its Platinum appearance; every colour still comes from the
;; theme.
;;
;;   menu bar     the tab bar: the λ menu at its left end, then a title per
;;                sidebar view; at its right end the Application menu,
;;                which lists the workspaces
;;   windows      every window wears a title bar: a close box, its title
;;                between pinstripes, a zoom box and a collapse box.  Only
;;                the active window shows stripes and boxes, so it is the
;;                one that stands out
;;   lists        the sidebar is a Finder window in list view: a header
;;                holding its buttons, column headings, disclosure
;;                triangles and one row per item
;;   bottoms      every window's mode line is its bottom edge: placards
;;                for the buffer, its position and its mode, and the grow
;;                box in the corner of the active window
;;   bevels       controls stand one pixel proud: lit along the top and
;;                left edges, shaded along the bottom and right
;;   type         title bars and menus in Charcoal, or the nearest
;;                installed face; lists in Geneva
;;
;; Colours come from the theme.  Bevels, stripes and frames are shades of
;; its background and text, LispM UI's palette, derived again whenever a
;; theme is enabled (`lispm-ui-palette-follow-theme'), the chrome's faces
;; then set in it (`lispm-desktop-platinum-apply-faces'); a theme can name
;; them outright in `lispm-desktop-platinum-colors', as the platinum theme
;; gives Platinum's own greys.
;;
;; Boxes, stripes and triangles are drawn as SVG.  The drawing functions are
;; pure: palette and geometry in, an SVG out; drawn images are memoized.
;;
;; The parts are files of their own, which this one loads:
;;
;;   lispm-desktop-platinum-palette
;;   lispm-desktop-platinum-draw
;;   lispm-desktop-platinum-controls
;;   lispm-desktop-platinum-icons (and lispm-ui-raster)
;;   lispm-desktop-platinum-frames
;;   lispm-desktop-platinum-titlebar
;;   lispm-desktop-platinum-coach
;;   lispm-desktop-platinum-menubar
;;   lispm-desktop-platinum-scroll
;;   lispm-desktop-platinum-status
;;   lispm-desktop-platinum-pointer

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'color)
(require 'lispm-desktop-core)
(require 'lispm-desktop-look)
(require 'lispm-desktop-platinum-palette)
(require 'lispm-desktop-platinum-draw)
(require 'lispm-desktop-platinum-controls)
(require 'lispm-desktop-platinum-icons)
(require 'lispm-desktop-platinum-frames)
(require 'lispm-desktop-platinum-titlebar)
(require 'lispm-desktop-platinum-coach)
(require 'lispm-desktop-platinum-menubar)
(require 'lispm-desktop-platinum-scroll)
(require 'lispm-desktop-platinum-status)
(require 'lispm-desktop-platinum-pointer)
(require 'lispm-desktop-i18n)

(declare-function treemacs-load-theme "treemacs-themes" (name))


(lispm-ui-look-define
 'platinum
 :title "Mac OS 9"
 :description (lispm-desktop-tr-noop "Mac OS 9's Platinum appearance: bevelled greys, pinstriped title bars")
 :theme 'platinum)

(defcustom lispm-desktop-platinum-system-families
  '("Charcoal" "Charcoal CY" "ChicagoFLF" "Chicago" "Geneva" "Lucida Grande")
  "Font families for title bars and menus, the first installed one winning.
Mac OS 9 drew them in Charcoal."
  :type '(repeat string)
  :group 'lispm-desktop-platinum)

(defcustom lispm-desktop-platinum-list-families '("Geneva" "Lucida Grande")
  "Font families for list views, the first installed one winning.
Mac OS 9's Finder listed files in Geneva."
  :type '(repeat string)
  :group 'lispm-desktop-platinum)

;;;; Lists

(defconst lispm-desktop-platinum-column-width 11
  "Width in columns of a list's right-hand column, the one it is sorted by.")

(defconst lispm-ui-list-header-height 21
  "Height in pixels of a list's row of column headings.")

(defconst lispm-ui-list--sort-glyph
  '("      #" "     ##" "    ###" "   ####" "  #####" " ######" "#######")
  "The glyph of a list's sort-direction button, a row of pixels a string.")

(cl-defgeneric lispm-ui-draw-list-header (palette columns &optional label-x)
  "Return an SVG of a list's row of column headings, in PALETTE.
COLUMNS lists (LABEL WIDTH SORTED [X]) from left to right; a
sixteen-pixel sort-direction button ends the row.  Each heading is a
bevelled cell, its label in the list's font X pixels in, by default
LABEL-X or eight; the heading of the SORTED column is drawn pushed in
and darker, as Mac OS 9 drew the column a list was sorted by."
  (let* ((h lispm-ui-list-header-height)
         (width (+ 16 (apply #'+ (mapcar #'cadr columns))))
         (svg (svg-create width h))
         (grey (lambda (level) (lispm-ui-grey palette level)))
         (x 0))
    (pcase-dolist (`(,label ,w ,sorted ,x-in) (append columns '((nil 16 nil))))
      (cl-flet ((row (y level) (lispm-ui-svg-row svg x (+ x w -1) y (funcall grey level)))
                (column (cx level) (lispm-ui-svg-column svg (+ x cx) 0 (1- h) (funcall grey level))))
        (if sorted
            (progn
              (lispm-ui-svg-fill svg x 0 w h (funcall grey #x88))
              (row (- h 2) #xAA) (column (- w 2) #xAA)
              (row (1- h) #x44) (column (1- w) #x44)
              (row 1 #x55) (column 1 #x55)
              (row 0 #x11) (column 0 #x11))
          (lispm-ui-svg-fill svg x 0 w h (funcall grey #xCC))
          (row (- h 2) #x88) (row (1- h) #x33)
          (row 1 #xFF) (column 1 #xFF)
          (row 0 #x66) (column 0 #x66)))
      (when label
        (lispm-ui-svg-content svg label (+ x (or x-in label-x 8)) 1 (- h 3)
                                              (plist-get palette :text) 'lispm-ui-list))
      (setq x (+ x w)))
    (lispm-ui-svg-pixel-map svg (- width 12) 7 lispm-ui-list--sort-glyph
                                      `((?# . ,(plist-get palette :text))))
    svg))

(defun lispm-desktop-platinum-columns (name meta width meta-width &optional label-x)
  "Return a list's column headings NAME and META as a line WIDTH pixels wide.
META is the right-hand column, META-WIDTH pixels wide with the
sort-direction button, the column the list is sorted by; NAME is
LABEL-X pixels in.  See `lispm-ui-draw-list-header'.  On a
text terminal the headings are text, the widths cells."
  (let ((name-width (- width meta-width)))
    (if (lispm-desktop-tty-p)
        (let* ((lead (make-string (lispm-desktop-px (or label-x 8)) ?\s))
               (name (truncate-string-to-width (concat lead name) (max 0 (1- name-width)) nil ?\s))
               (meta (truncate-string-to-width (concat " " meta) (max 0 (- meta-width 2)) nil ?\s)))
          (concat (propertize (concat name " ") 'face 'lispm-desktop-platinum-column)
                  (propertize (concat meta "▴ ") 'face 'lispm-desktop-platinum-column-sorted)
                  "\n"))
     (concat (propertize " " 'display
                        (lispm-ui-image
                         (list 'list-header name meta width meta-width label-x)
                         (lambda ()
                           (lispm-ui-draw-list-header
                            lispm-ui-current-palette
                            (list (list name name-width nil) (list meta (- meta-width 16) t 4))
                            label-x))))
             "\n"))))

;;;; Treemacs

(defun lispm-desktop-platinum-treemacs-theme ()
  "Draw treemacs's tree with Finder's icons: disks, folders and documents.
A directory shows a disclosure triangle before its folder, as Finder's
list view did; files are indented by a triangle's width, so their icons
line up with the folders'.  It is made again when a theme is enabled,
as the triangles take the theme's colour."
  (when (featurep 'treemacs)
    ;; Treemacs puts its background face in front of the default face; left
    ;; inheriting the default, it would bring back the editor's font over
    ;; the list's.  The project's name is not underlined, as Finder's
    ;; disks were not.
    (set-face-attribute 'treemacs-window-background-face nil :inherit 'unspecified)
    (set-face-attribute 'treemacs-root-face nil :underline nil :weight 'bold :height 1.0)
    (let* ((gap (propertize " " 'display '(space :width (5))))
           (no-triangle (propertize " " 'display '(space :width (12))))
           (row (lambda (lead icon) (concat lead gap (lispm-ui-icon icon) gap)))
           ;; What a text terminal shows, treemacs's fallback.
           (text-row (lambda (lead icon) (concat lead " " (lispm-ui-text-icon icon) " "))))
      (eval
       `(treemacs-create-theme "lispm-desktop-platinum"
          :config
          (progn
            (treemacs-create-icon :icon ,(funcall row (lispm-ui-triangle t) "disk")
                                  :fallback ,(funcall text-row "▾" "disk")
                                  :extensions (root-open))
            (treemacs-create-icon :icon ,(funcall row (lispm-ui-triangle nil) "disk")
                                  :fallback ,(funcall text-row "▸" "disk")
                                  :extensions (root-closed))
            (treemacs-create-icon :icon ,(funcall row (lispm-ui-triangle t) "folder")
                                  :fallback ,(funcall text-row "▾" "folder")
                                  :extensions (dir-open))
            (treemacs-create-icon :icon ,(funcall row (lispm-ui-triangle nil) "folder")
                                  :fallback ,(funcall text-row "▸" "folder")
                                  :extensions (dir-closed))
            (treemacs-create-icon :icon ,(funcall row (lispm-ui-triangle t) "document-blank")
                                  :fallback ,(funcall text-row "▾" "document-blank")
                                  :extensions (tag-open))
            (treemacs-create-icon :icon ,(funcall row (lispm-ui-triangle nil) "document-blank")
                                  :fallback ,(funcall text-row "▸" "document-blank")
                                  :extensions (tag-closed))
            (treemacs-create-icon :icon ,(funcall row no-triangle "document-blank")
                                  :fallback ,(funcall text-row " " "document-blank")
                                  :extensions (tag-leaf))
            (treemacs-create-icon :icon ,(funcall row no-triangle "document-image")
                                  :fallback ,(funcall text-row " " "document-image")
                                  :extensions ("png" "jpg" "jpeg" "gif" "svg" "webp" "bmp"
                                               "tif" "tiff" "ico" "heic"))
            (treemacs-create-icon :icon ,(funcall row no-triangle "document-lisp")
                                  :fallback ,(funcall text-row " " "document-lisp")
                                  :extensions ,lispm-ui-lisp-extensions)
            (treemacs-create-icon :icon ,(funcall row no-triangle "document-markdown")
                                  :fallback ,(funcall text-row " " "document-markdown")
                                  :extensions ("md" "markdown" "mkd"))
            (treemacs-create-icon :icon ,(funcall row no-triangle "document-org")
                                  :fallback ,(funcall text-row " " "document-org")
                                  :extensions ("org"))
            (treemacs-create-icon :icon ,(funcall row no-triangle "document-code")
                                  :fallback ,(funcall text-row " " "document-code")
                                  :extensions ,lispm-ui-code-extensions)
            (treemacs-create-icon :icon ,(funcall row no-triangle "document")
                                  :fallback ,(funcall text-row " " "document")
                                  :extensions (fallback))))
       t)
      (treemacs-load-theme "lispm-desktop-platinum"))))

;;;; Faces

(defun lispm-desktop-platinum--family (families)
  "Return the first of FAMILIES installed, or nil."
  (let ((installed (font-family-list)))
    (seq-find (lambda (family) (member family installed)) families)))

(defun lispm-desktop-platinum--cjk-fontset (family)
  "Return a fontset drawing FAMILY, its Chinese and Japanese in the family
for them in the language spoken (see `lispm-ui-text-cjk-family'),
made or brought up to date; or nil without either."
  (when-let* (((stringp family))
              (cjk (lispm-ui-text-cjk-family))
              ;; The name's last field, past "fontset-", takes no hyphen.
              (name (concat "fontset-lispm" (downcase (replace-regexp-in-string "[^[:alnum:]]" "" family)))))
    (unless (query-fontset name)
      (create-fontset-from-fontset-spec (format "-*-%s-*-*-*-*-*-*-*-*-*-*-%s" family name)))
    (dolist (script '(han cjk-misc kana bopomofo))
      (set-fontset-font name script (font-spec :family cjk)))
    name))

(defun lispm-desktop-platinum--fontset-attribute (family)
  "Return the face attributes giving FAMILY its fontset, see
`lispm-desktop-platinum--cjk-fontset'; none on a text terminal, which
draws with the terminal's font and has no fontsets.
Without a fontset, as where no family of the look is installed, the
face takes the default face's: `reset' asks for it, as `unspecified'
cannot.  Emacs keeps `unspecified' for new frames as `:ignore-defface',
then looks that up as a fontset's name and fails."
  (when (display-graphic-p)
    (list :fontset (or (lispm-desktop-platinum--cjk-fontset family) 'reset))))

(defun lispm-desktop-platinum--color-distance (a b)
  "Return the perceptual CIE76 distance between colours A and B, or nil.
Unlike raw RGB distance, this accounts for the chroma of Platinum's
lavender selection as well as its lightness against the list body."
  (when-let* ((rgb-a (lispm-ui-rgb a))
              (rgb-b (lispm-ui-rgb b)))
    (sqrt (apply #'+
                 (cl-mapcar (lambda (x y) (expt (- x y) 2))
                            (apply #'color-srgb-to-lab rgb-a)
                            (apply #'color-srgb-to-lab rgb-b))))))

(defun lispm-desktop-platinum--linear-component (component)
  "Return the linear-light value of an sRGB COMPONENT."
  (if (<= component 0.04045) (/ component 12.92)
    (expt (/ (+ component 0.055) 1.055) 2.4)))

(defun lispm-desktop-platinum--relative-luminance (color)
  "Return COLOR's WCAG relative luminance, or nil if unresolved."
  (when-let* ((rgb (lispm-ui-rgb color)))
    (apply #'+ (cl-mapcar #'* '(0.2126 0.7152 0.0722)
                          (mapcar #'lispm-desktop-platinum--linear-component rgb)))))

(defun lispm-desktop-platinum--text-contrast (a b)
  "Return the WCAG contrast ratio of colours A and B, or nil."
  (when-let* ((x (lispm-desktop-platinum--relative-luminance a))
              (y (lispm-desktop-platinum--relative-luminance b)))
    (/ (+ (max x y) 0.05) (+ (min x y) 0.05))))

(defun lispm-desktop-platinum--row-foregrounds ()
  "Return foregrounds used by sidebar text, statuses and agent icons."
  (let ((faces (append '(default shadow success warning error)
                       (and (boundp 'lispm-agents-label-faces) lispm-agents-label-faces)
                       (mapcar (lambda (agent) (nth 2 (plist-get (cdr agent) :icon)))
                               (and (boundp 'lispm-agents-defined) lispm-agents-defined))
                       (seq-filter (lambda (face)
                                     (string-match-p "\\`lispm-agents-.*-icon\\'" (symbol-name face)))
                                   (face-list)))))
    (delete-dups (delq nil (mapcar (lambda (face)
                                    (when (and face (facep face))
                                      (face-foreground face nil t))) faces)))))

(defun lispm-desktop-platinum--selection-luminances (body foregrounds)
  "Return closed luminance intervals preserving FOREGROUNDS on BODY.
For each foreground, the contrast inequality permits a dark interval
and a light interval.  Intersect those intervals instead of searching
a coarse colour grid, which cannot establish whether a solution exists."
  (when-let* ((ground (lispm-desktop-platinum--relative-luminance body)))
    (let ((intervals (list (cons 0.0 1.0))))
      (dolist (foreground foregrounds intervals)
        (when-let* ((ink (lispm-desktop-platinum--relative-luminance foreground)))
          (let* ((ratio (/ (+ (max ink ground) 0.05) (+ (min ink ground) 0.05)))
                 (dark (- (/ (+ ink 0.05) ratio) 0.05))
                 (light (- (* ratio (+ ink 0.05)) 0.05))
                 (allowed (append (and (>= dark -1e-12)
                                       (list (cons 0.0 (max 0.0 dark))))
                                  (and (<= light (+ 1.0 1e-12))
                                       (list (cons (min 1.0 light) 1.0)))))
                 next)
            (dolist (interval intervals)
              (dolist (range allowed)
                (let ((low (max (car interval) (car range)))
                      (high (min (cdr interval) (cdr range))))
                  (when (<= low (+ high 1e-12))
                    (push (cons low (max low high)) next)))))
            (setq intervals (delete-dups (nreverse next)))))))))

(defun lispm-desktop-platinum--color-at-luminance (color luminance)
  "Tint or shade COLOR to LUMINANCE in linear light, preserving its hue.
Return a normal 24-bit colour; callers check contrast after rounding."
  (when-let* ((rgb (lispm-ui-rgb color))
              (light (lispm-desktop-platinum--relative-luminance color)))
    (let* ((end (if (< light luminance) 1.0 0.0))
           (fraction (if (= light end) 0.0 (/ (- luminance light) (- end light)))))
      (apply #'format "#%02x%02x%02x"
             (mapcar
              (lambda (component)
                (let* ((linear (lispm-desktop-platinum--linear-component component))
                       (mixed (+ linear (* fraction (- end linear))))
                       (srgb (if (<= mixed 0.0031308) (* mixed 12.92)
                               (- (* 1.055 (expt mixed (/ 1.0 2.4))) 0.055))))
                  (round (* 255 (max 0.0 (min 1.0 srgb)))))) rgb)))))

(defun lispm-desktop-platinum--selection-background (body accent region foregrounds current)
  "Choose a selection on BODY preserving every FOREGROUNDS contrast.
Prefer REGION, then an ACCENT tint or shade in an allowed luminance
interval.  An underline distinguishes selection from CURRENT even
where preserving dim text permits only a subtle background change.
Return nil only when the allowed luminance is solely BODY's pure black
or white: no other colour can satisfy it.  If 24-bit rounding prevents
an accent tint from fitting a narrow interval, retain BODY and its
text colours; the underline still identifies the selected row."
  (let* ((tint (lispm-ui-blend body accent 0.60))
         (ground (lispm-desktop-platinum--relative-luminance body))
         (intervals (lispm-desktop-platinum--selection-luminances body foregrounds)))
    (cond
     ;; Unresolved terminal colours retain the theme's background.
     ((not (and tint ground (lispm-ui-rgb current))) region)
     ((and (or (= ground 0.0) (= ground 1.0))
           (seq-every-p (lambda (range)
                          (and (< (abs (- (car range) ground)) 1e-12)
                               (< (abs (- (cdr range) ground)) 1e-12))) intervals))
      nil)
     (t
      (cl-labels ((suitable (candidate)
                    (when-let* ((light (lispm-desktop-platinum--relative-luminance candidate)))
                      (seq-some (lambda (range)
                                  (<= (- (car range) 1e-12) light (+ (cdr range) 1e-12)))
                                intervals))))
        (or (and (suitable region)
                 (> (or (lispm-desktop-platinum--color-distance body region) 0) 3)
                 region)
            (and (suitable tint) tint)
            (catch 'found
              (dolist (range intervals)
                ;; Interior samples avoid 24-bit rounding across a bound;
                ;; these are projections of the accent, not gamut searches.
                (dolist (fraction '(0.5 0.25 0.75))
                  (let ((candidate
                         (lispm-desktop-platinum--color-at-luminance
                          accent (+ (car range) (* fraction (- (cdr range) (car range)))))))
                    (when (suitable candidate) (throw 'found candidate))))))
            body))))))

(defun lispm-desktop-platinum-apply-faces ()
  "Set every chrome face in the palette just derived from the active theme,
and the look's fonts, LispM UI's faces' too, whose colours LispM UI has
set itself.  This is one of `lispm-ui-palette-changed-hook', which runs
whenever a theme is enabled.  Colours come only from the palette and
faces this function never sets, so re-running it is idempotent."
  (let ((text (face-foreground 'default nil t))
        (dim (face-foreground 'shadow nil t))
        (chrome (lispm-ui-color :chrome))
        (header (lispm-ui-color :header))
        (system (and (display-graphic-p)
                     (lispm-desktop-platinum--family
                      (lispm-ui-look-families :system-families
                                              lispm-desktop-platinum-system-families))))
        (list-family (and (display-graphic-p)
                          (lispm-desktop-platinum--family
                           (lispm-ui-look-families :list-families
                                                   lispm-desktop-platinum-list-families)))))
    (set-face-attribute 'tab-line nil
                        :inherit 'unspecified :background chrome :foreground text
                        :box nil :height 'unspecified)
    (set-face-attribute 'tab-bar nil
                        :inherit 'unspecified :background chrome :foreground text
                        :box nil)
    (set-face-attribute 'mode-line nil
                        :inherit 'unspecified :background chrome :foreground text
                        :box nil :overline 'unspecified :underline 'unspecified
                        :height 'unspecified)
    (set-face-attribute 'mode-line-inactive nil
                        :inherit 'unspecified :background chrome
                        :foreground (lispm-ui-color :muted)
                        :box nil :overline 'unspecified :underline 'unspecified
                        :height 'unspecified)
    ;; Lists as Finder's list view drew them: rows lighter than the
    ;; platinum, the column sorted by in platinum, a lit line atop
    ;; each row.
    (set-face-attribute 'lispm-desktop-platinum-sorted nil
                        :background (lispm-ui-grey lispm-ui-current-palette #xDD)
                        :extend t)
    (set-face-attribute 'lispm-desktop-platinum-list-body nil
                        :inherit 'lispm-ui-list
                        :background (lispm-ui-grey lispm-ui-current-palette #xEE))
    (set-face-attribute 'lispm-desktop-platinum-row nil
                        :overline (lispm-ui-grey lispm-ui-current-palette #xFF))
    ;; Balloon Help: a note in the list's type, on the header's grey.
    ;; It carries no outline: macOS sizes a tooltip without its box,
    ;; which would cut the right edge off.
    (set-face-attribute 'tooltip nil
                        :inherit 'lispm-ui-list :background header
                        :foreground text :height 0.9 :box nil)
    ;; Placards are Mac OS 9's outlined plates; an inactive window's
    ;; lie flat, outlined in grey.  Their text stays text, so what
    ;; other packages show there, images included, still shows.
    (set-face-attribute 'lispm-desktop-platinum-placard nil
                        :inherit 'lispm-ui-list :background chrome :foreground text
                        :height 0.95
                        :box `(:line-width (1 . 1) :color ,(lispm-ui-color :outline)))
    (set-face-attribute 'lispm-desktop-platinum-placard-inactive nil
                        :inherit 'lispm-ui-list :background chrome
                        :foreground (lispm-ui-color :muted) :height 0.95
                        :box `(:line-width (1 . 1)
                               :color ,(lispm-ui-grey lispm-ui-current-palette #x99)))
    (dolist (face '(lispm-ui-title lispm-ui-title-inactive
                    lispm-desktop-platinum-menu-title lispm-desktop-platinum-menu-title-active))
      (apply #'set-face-attribute face nil :family (or system 'unspecified) :height 120
             :weight 'bold (lispm-desktop-platinum--fontset-attribute system)))
    (set-face-attribute 'lispm-ui-title-inactive nil
                        :foreground (lispm-ui-color :muted) :inherit 'unspecified)
    ;; Menu bar titles are drawn as images in these faces; menus'
    ;; items inherit them.
    (set-face-attribute 'lispm-desktop-platinum-menu-title nil
                        :foreground text :background chrome :inverse-video nil :box nil)
    (set-face-attribute 'lispm-desktop-platinum-menu-title-active nil
                        :foreground (lispm-ui-color :highlight-text)
                        :background (lispm-ui-color :highlight)
                        :inverse-video nil :box nil)
    ;; Chinese and Japanese in a proportional family, as the words
    ;; around them are: Emacs's fallback is the terminals' monospace.
    (apply #'set-face-attribute 'lispm-ui-list nil :family (or list-family 'unspecified)
           (lispm-desktop-platinum--fontset-attribute list-family))
    (set-face-attribute 'lispm-desktop-platinum-header nil
                        :inherit 'lispm-ui-list :background header :foreground dim
                        :box `(:line-width (1 . 4) :color ,header) :underline nil)
    (set-face-attribute 'lispm-desktop-platinum-toolbar nil
                        :inherit 'lispm-ui-list :background header :foreground text)
    (set-face-attribute 'lispm-desktop-platinum-toolbar-label nil
                        :inherit 'lispm-desktop-platinum-toolbar :foreground dim)
    (set-face-attribute 'lispm-desktop-platinum-column nil
                        :inherit 'lispm-ui-list :background header :foreground text
                        :height 0.9
                        :box '(:line-width (1 . 1) :style released-button))
    (set-face-attribute 'lispm-desktop-platinum-column-sorted nil
                        :inherit 'lispm-ui-list :background chrome :foreground text
                        :height 0.9
                        :box '(:line-width (1 . 1) :style released-button))
    ;; Menus, alerts and windows are outlined in black, as Mac OS 9's were.
    (set-face-attribute 'child-frame-border nil :background (lispm-ui-color :outline))
    ;; Neighbouring windows meet across a divider drawn as a window's
    ;; frame: lit where it leaves the window before, platinum, then
    ;; the outline of the window after.
    (set-face-attribute 'window-divider nil
                        :foreground (lispm-ui-grey lispm-ui-current-palette #xCC))
    (set-face-attribute 'window-divider-first-pixel nil
                        :foreground (lispm-ui-grey lispm-ui-current-palette #xFF))
    (set-face-attribute 'window-divider-last-pixel nil
                        :foreground (lispm-ui-color :outline))
    (lispm-desktop-platinum-apply-look-faces lispm-ui-current-palette)
    ;; The row of what is being worked on elsewhere: the list, as the
    ;; look drew it, tinted with the accent, as the desktop behind the
    ;; windows is.  The selection's own colour will not do: a dark
    ;; theme's, as Dracula's, is barely lighter than its list.  Where
    ;; colours cannot be blended, as on a text terminal, the secondary
    ;; selection stands in.
    (let* ((body (face-background 'lispm-desktop-platinum-list-body nil t))
           (accent (lispm-ui-color :accent))
           (tinted (lispm-ui-blend body accent 0.25))
           (region (face-background 'region nil t))
           (preserved (lispm-desktop-platinum--selection-background
                       body accent region (lispm-desktop-platinum--row-foregrounds) tinted))
           ;; Some light palettes leave no room for a distinct background
           ;; while preserving both default and dim text.  Only then use
           ;; a conventional selection with its own contrasting text.
           (selected (or preserved
                         (if (and (>= (or (lispm-desktop-platinum--text-contrast
                                            region "#ffffff") 0) 4.5)
                                  (> (or (lispm-desktop-platinum--color-distance body region) 0) 3))
                             region
                           (lispm-desktop-platinum--color-at-luminance accent 0.10))))
           (foreground (and (not preserved) selected "#ffffff"))
           ;; A focus edge remains clear when light themes allow only a
           ;; near-white fill.  It does not cover icon or status colours,
           ;; or change row height as a box around the row would.
           (edge (and tinted selected
                      (if (>= (or (lispm-desktop-platinum--text-contrast accent selected) 0) 3)
                          accent
                        (or foreground text)))))
      (set-face-attribute 'lispm-desktop-platinum-row-current nil
                          :inherit (if tinted 'unspecified 'secondary-selection)
                          :background (or tinted 'unspecified))
      ;; Never inherit region's foreground.  Reset an exceptional text
      ;; colour on theme/look changes so the normal path keeps row colours.
      (set-face-attribute 'lispm-desktop-platinum-row-selected nil
                          :inherit 'unspecified
                          :background (or selected 'unspecified)
                          :foreground (or foreground 'unspecified)
                          :underline (if edge `(:color ,edge :style line) 'unspecified)
                          :extend t))))

;; Before LispM UI draws its forms again, at 90: they are measured in the
;; fonts set here.
(add-hook 'lispm-ui-palette-changed-hook #'lispm-desktop-platinum-apply-faces 50)

(cl-defgeneric lispm-desktop-platinum-apply-look-faces (palette)
  "Set the chrome's faces as the look draws them, once Platinum's are set,
in PALETTE.  Platinum's are its own; another look sets those it draws
otherwise, as Windows' flat placards and sunken fields."
  (ignore palette))

;;;; Setup

(defun lispm-desktop-platinum--treemacs-scroll-bar (buffer)
  "Say a scroll bar for BUFFER when it is a project's tree, as a list.
This is one of `lispm-desktop-platinum-scroll-bar-functions'."
  (and (eq (buffer-local-value 'major-mode buffer) 'treemacs-mode) 'yes))

(defun lispm-desktop-setup-platinum ()
  "Lay the frame out as Mac OS 9 did, in the active theme's colours."
  (add-hook 'lispm-desktop-platinum-scroll-bar-functions #'lispm-desktop-platinum--treemacs-scroll-bar)
  (setq-default tab-line-format '(:eval (lispm-desktop-platinum-title-bar))
                mode-line-format lispm-desktop-platinum-mode-line-format)
  (setq mode-line-right-align-edge 'window)
  (setq tab-bar-format '(lispm-desktop-platinum-menu-bar-line))
  (lispm-desktop-platinum--start-status)
  ;; Frames take sizes in pixels: a scroll bar is exactly as tall as its
  ;; window, and a maximized frame fills the screen.
  (setq frame-resize-pixelwise t)
  ;; The menu bar runs from edge to edge and the desktop starts under its
  ;; black line: the screen has no border of its own.
  (add-to-list 'default-frame-alist '(internal-border-width . 0))
  (dolist (frame (frame-list))
    (when (and (display-graphic-p frame) (not (frame-parent frame)))
      (set-frame-parameter frame 'internal-border-width 0)))
  (lispm-desktop-platinum-scroll-enable)
  (lispm-desktop-platinum-frames-enable)
  ;; Window frames: a dark outline on each window's edge, platinum between.
  (setq window-divider-default-places t
        window-divider-default-right-width 5
        window-divider-default-bottom-width 5)
  (window-divider-mode 1)
  ;; The insertion point is a thin bar, as Mac OS 9's was; Evil's normal
  ;; state keeps its block.
  (setq-default cursor-type '(bar . 2))
  ;; Help over a control shows as Balloon Help, in Emacs's own frames.
  (setq use-system-tooltips nil
        tooltip-delay 0.6
        tooltip-frame-parameters '((name . "tooltip")
                                   (internal-border-width . 6)
                                   (no-special-glyphs . t)))
  (lispm-ui-palette-follow-theme)
  (with-eval-after-load 'treemacs
    (lispm-desktop-platinum-treemacs-theme)
    (add-hook 'enable-theme-functions (lambda (&rest _) (lispm-desktop-platinum-treemacs-theme)) 90))
  (add-hook 'which-key-init-buffer-hook #'lispm-desktop-platinum-no-title-bar)
  (add-hook 'transient-setup-buffer-hook #'lispm-desktop-platinum-no-title-bar)
  ;; Quitting asks in alerts, as Mac OS 9's Quit and Shut Down did.
  (advice-add 'save-buffers-kill-emacs :around #'lispm-desktop-platinum--quit)
  ;; The menu bar and the title's menu from the keyboard; C-c keys, which
  ;; terminals leave to Emacs too.  U is Info's key for up, as the
  ;; Finder's title menu goes up the folder's path.
  (keymap-global-set "C-c m" #'lispm-desktop-platinum-menu-bar-open)
  (keymap-global-set "C-c u" #'lispm-desktop-platinum-title-menu))

(provide 'lispm-desktop-platinum)
;;; lispm-desktop-platinum.el ends here
