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
;; its background and text, derived again whenever a theme is enabled
;; (`lispm-desktop-platinum-apply-faces'); a theme can name them outright in
;; `lispm-desktop-platinum-colors', as the platinum theme gives Platinum's own greys.
;;
;; Boxes, stripes and triangles are drawn as SVG.  The drawing functions are
;; pure: palette and geometry in, an SVG out; drawn images are memoized.
;;
;; The parts are files of their own, which this one loads:
;;
;;   lispm-desktop-platinum-palette
;;   lispm-desktop-platinum-draw
;;   lispm-desktop-platinum-controls
;;   lispm-desktop-platinum-icons (and lispm-desktop-platinum-raster)
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

(defconst lispm-desktop-platinum-list-header-height 21
  "Height in pixels of a list's row of column headings.")

(defconst lispm-desktop-platinum--sort-glyph
  '("      #" "     ##" "    ###" "   ####" "  #####" " ######" "#######")
  "The glyph of a list's sort-direction button, a row of pixels a string.")

(cl-defgeneric lispm-desktop-platinum-draw-list-header (palette columns &optional label-x)
  "Return an SVG of a list's row of column headings, in PALETTE.
COLUMNS lists (LABEL WIDTH SORTED [X]) from left to right; a
sixteen-pixel sort-direction button ends the row.  Each heading is a
bevelled cell, its label in the list's font X pixels in, by default
LABEL-X or eight; the heading of the SORTED column is drawn pushed in
and darker, as Mac OS 9 drew the column a list was sorted by."
  (let* ((h lispm-desktop-platinum-list-header-height)
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
    (lispm-desktop-platinum-svg-pixel-map svg (- width 12) 7 lispm-desktop-platinum--sort-glyph
                                      `((?# . ,(plist-get palette :text))))
    svg))

(defun lispm-desktop-platinum-columns (name meta width meta-width &optional label-x)
  "Return a list's column headings NAME and META as a line WIDTH pixels wide.
META is the right-hand column, META-WIDTH pixels wide with the
sort-direction button, the column the list is sorted by; NAME is
LABEL-X pixels in.  See `lispm-desktop-platinum-draw-list-header'.  On a
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
                           (lispm-desktop-platinum-draw-list-header
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
           (row (lambda (lead icon) (concat lead gap (lispm-desktop-platinum-icon icon) gap)))
           ;; What a text terminal shows, treemacs's fallback.
           (text-row (lambda (lead icon) (concat lead " " (lispm-desktop-platinum--text-icon icon) " "))))
      (eval
       `(treemacs-create-theme "lispm-desktop-platinum"
          :config
          (progn
            (treemacs-create-icon :icon ,(funcall row (lispm-desktop-platinum-triangle t) "disk")
                                  :fallback ,(funcall text-row "▾" "disk")
                                  :extensions (root-open))
            (treemacs-create-icon :icon ,(funcall row (lispm-desktop-platinum-triangle nil) "disk")
                                  :fallback ,(funcall text-row "▸" "disk")
                                  :extensions (root-closed))
            (treemacs-create-icon :icon ,(funcall row (lispm-desktop-platinum-triangle t) "folder")
                                  :fallback ,(funcall text-row "▾" "folder")
                                  :extensions (dir-open))
            (treemacs-create-icon :icon ,(funcall row (lispm-desktop-platinum-triangle nil) "folder")
                                  :fallback ,(funcall text-row "▸" "folder")
                                  :extensions (dir-closed))
            (treemacs-create-icon :icon ,(funcall row (lispm-desktop-platinum-triangle t) "document-blank")
                                  :fallback ,(funcall text-row "▾" "document-blank")
                                  :extensions (tag-open))
            (treemacs-create-icon :icon ,(funcall row (lispm-desktop-platinum-triangle nil) "document-blank")
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
                                  :extensions ,lispm-desktop-platinum-lisp-extensions)
            (treemacs-create-icon :icon ,(funcall row no-triangle "document-markdown")
                                  :fallback ,(funcall text-row " " "document-markdown")
                                  :extensions ("md" "markdown" "mkd"))
            (treemacs-create-icon :icon ,(funcall row no-triangle "document-org")
                                  :fallback ,(funcall text-row " " "document-org")
                                  :extensions ("org"))
            (treemacs-create-icon :icon ,(funcall row no-triangle "document-code")
                                  :fallback ,(funcall text-row " " "document-code")
                                  :extensions ,lispm-desktop-platinum-code-extensions)
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

(defun lispm-desktop-platinum-apply-faces (&rest _)
  "Derive the palette and every chrome face from the active theme.
Colours come only from faces this function never sets, so re-running
it, as on every theme change, is idempotent."
  (let ((content (face-background 'default nil t))
        (text (face-foreground 'default nil t))
        (dim (face-foreground 'shadow nil t))
        (accent (face-foreground 'font-lock-constant-face nil t)))
    (when (and content text dim accent (color-name-to-rgb content))
      (setq lispm-ui-current-palette (lispm-ui-merge-palette
                                             (lispm-ui-derive-palette content text dim accent)
                                             lispm-desktop-platinum-colors)
            lispm-ui-palette-generation (1+ lispm-ui-palette-generation))
      (let ((chrome (lispm-ui-color :chrome))
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
        (dolist (face '(lispm-ui-title lispm-desktop-platinum-title-inactive
                        lispm-desktop-platinum-menu-title lispm-desktop-platinum-menu-title-active))
          (apply #'set-face-attribute face nil :family (or system 'unspecified) :height 120
                 :weight 'bold (lispm-desktop-platinum--fontset-attribute system)))
        (set-face-attribute 'lispm-ui-title nil :foreground text)
        (set-face-attribute 'lispm-desktop-platinum-title-inactive nil
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
        (set-face-attribute 'lispm-ui-small nil
                            :inherit 'lispm-ui-list :height 90 :weight 'normal)
        (set-face-attribute 'lispm-desktop-platinum-header nil
                            :inherit 'lispm-ui-list :background header :foreground dim
                            :box `(:line-width (1 . 4) :color ,header) :underline nil)
        (set-face-attribute 'lispm-desktop-platinum-toolbar nil
                            :inherit 'lispm-ui-list :background header :foreground text)
        (set-face-attribute 'lispm-desktop-platinum-toolbar-label nil
                            :inherit 'lispm-desktop-platinum-toolbar :foreground dim)
        (set-face-attribute 'lispm-ui-field nil
                            :inherit 'lispm-ui-list :background (lispm-ui-color :content)
                            :foreground text
                            :box `(:line-width (1 . 1) :color ,(lispm-ui-color :outline)))
        ;; Emacs draws one box about text, so the ring stands for the frame
        ;; within it while the field has the keyboard.
        (set-face-attribute 'lispm-ui-field-focused nil
                            :inherit 'lispm-ui-field
                            :box `(:line-width (2 . 2) :color ,(lispm-ui-color :focus)))
        (set-face-attribute 'lispm-ui-button nil
                            :inherit 'lispm-ui-list :background chrome :foreground text
                            :box '(:line-width (1 . 1) :style released-button))
        (set-face-attribute 'lispm-ui-button-pressed nil
                            :inherit 'lispm-ui-list :background (lispm-ui-color :shade)
                            :foreground (lispm-ui-color :accent)
                            :box '(:line-width (1 . 1) :style pressed-button))
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
        (lispm-desktop-platinum-apply-look-faces lispm-ui-current-palette)))))

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
  (lispm-desktop-platinum-apply-faces)
  (add-hook 'enable-theme-functions #'lispm-desktop-platinum-apply-faces)
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
