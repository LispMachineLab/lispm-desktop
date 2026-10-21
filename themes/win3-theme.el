;;; win3-theme.el --- Windows 3.2's default colours  -*- lexical-binding: t; -*-

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

;; The "Windows Default" colour scheme of Windows 3.2 (and 3.1): black on
;; white windows, navy active captions, white inactive ones, buttons and
;; borders in the VGA's light grey, selections in navy.  Code takes its
;; colours from the same sixteen VGA colours, as the editors of the time
;; coloured it.
;;
;; lispm-desktop's chrome takes the scheme's colours from
;; `lispm-desktop-platinum-colors', which this theme sets; the win3 look
;; (lispm-desktop-win3.el) draws Windows' forms in them.

;;; Code:

(deftheme win3
  "Windows 3.2's Windows Default scheme: navy captions, grey buttons, black on white.")

(let* ((white "#FFFFFF")
       (black "#000000")
       ;; The VGA's sixteen colours.
       (grey "#C0C0C0")
       (dark-grey "#808080")
       (navy "#000080")
       (blue "#0000FF")
       (green "#008000")
       (teal "#008080")
       (maroon "#800000")
       (red "#FF0000")
       (purple "#800080")
       (olive "#808000")
       (yellow "#FFFF00")
       (lime "#00FF00")
       (cyan "#00FFFF")
       (magenta "#FF00FF"))
  (custom-theme-set-faces
   'win3
   ;; The document.
   `(default ((t :background ,white :foreground ,black)))
   `(cursor ((t :background ,black)))
   `(fringe ((t :background ,white)))
   `(region ((t :background ,navy :foreground ,white :extend t)))
   `(secondary-selection ((t :background ,cyan :extend t)))
   `(highlight ((t :background ,grey)))
   `(hl-line ((t :background "#F0F0F0" :extend t)))
   `(shadow ((t :foreground ,dark-grey)))
   `(line-number ((t :foreground ,dark-grey :background ,white)))
   `(line-number-current-line ((t :foreground ,black :background ,white)))
   `(minibuffer-prompt ((t :foreground ,navy :weight bold)))
   `(link ((t :foreground ,blue :underline t)))
   `(link-visited ((t :foreground ,purple :underline t)))
   `(button ((t :foreground ,blue :underline t)))
   `(error ((t :foreground ,red :weight bold)))
   `(warning ((t :foreground ,maroon :weight bold)))
   `(success ((t :foreground ,green :weight bold)))
   `(trailing-whitespace ((t :background ,yellow)))
   `(escape-glyph ((t :foreground ,teal)))
   `(homoglyph ((t :foreground ,teal)))
   ;; Searching and matching.
   `(isearch ((t :background ,navy :foreground ,white)))
   `(isearch-fail ((t :background ,red :foreground ,white)))
   `(lazy-highlight ((t :background ,cyan)))
   `(match ((t :background ,yellow)))
   `(show-paren-match ((t :background ,cyan :weight bold)))
   `(show-paren-mismatch ((t :background ,red :foreground ,white)))
   ;; Code.
   `(font-lock-keyword-face ((t :foreground ,navy :weight bold)))
   `(font-lock-comment-face ((t :foreground ,green)))
   `(font-lock-comment-delimiter-face ((t :foreground ,green)))
   `(font-lock-doc-face ((t :foreground ,olive)))
   `(font-lock-string-face ((t :foreground ,maroon)))
   `(font-lock-constant-face ((t :foreground ,purple)))
   `(font-lock-builtin-face ((t :foreground ,blue)))
   `(font-lock-function-name-face ((t :foreground ,teal :weight bold)))
   `(font-lock-variable-name-face ((t :foreground ,navy)))
   `(font-lock-type-face ((t :foreground ,teal)))
   `(font-lock-preprocessor-face ((t :foreground ,olive)))
   `(font-lock-negation-char-face ((t :foreground ,red)))
   `(font-lock-warning-face ((t :foreground ,red :weight bold)))
   ;; Chrome: lispm-desktop derives the rest from `lispm-desktop-platinum-colors'.
   `(mode-line ((t :background ,grey :foreground ,black)))
   `(mode-line-inactive ((t :background ,grey :foreground ,dark-grey)))
   `(header-line ((t :background ,grey :foreground ,black)))
   `(tab-bar ((t :background ,white :foreground ,black)))
   `(tab-line ((t :background ,grey :foreground ,black)))
   `(vertical-border ((t :foreground ,black)))
   `(window-divider ((t :foreground ,grey)))
   `(window-divider-first-pixel ((t :foreground ,black)))
   `(window-divider-last-pixel ((t :foreground ,black)))
   ;; Completion.
   `(completions-common-part ((t :foreground ,navy :weight bold)))
   `(completions-first-difference ((t :weight bold)))
   `(vertico-current ((t :background ,navy :foreground ,white :extend t)))
   `(orderless-match-face-0 ((t :foreground ,navy :weight bold)))
   `(orderless-match-face-1 ((t :foreground ,teal :weight bold)))
   `(orderless-match-face-2 ((t :foreground ,purple :weight bold)))
   `(orderless-match-face-3 ((t :foreground ,maroon :weight bold)))
   `(marginalia-documentation ((t :foreground ,dark-grey)))
   `(corfu-default ((t :background ,white :foreground ,black)))
   `(corfu-current ((t :background ,navy :foreground ,white)))
   `(corfu-border ((t :background ,black)))
   ;; Diffs and version control.
   `(diff-added ((t :background "#E0FFE0" :foreground ,green :extend t)))
   `(diff-removed ((t :background "#FFE0E0" :foreground ,maroon :extend t)))
   `(diff-refine-added ((t :background ,lime)))
   `(diff-refine-removed ((t :background "#FFC0C0")))
   `(diff-header ((t :background ,grey)))
   `(diff-file-header ((t :background ,grey :weight bold)))
   `(magit-section-highlight ((t :background "#F0F0F0" :extend t)))
   `(magit-branch-local ((t :foreground ,navy)))
   `(magit-branch-remote ((t :foreground ,green)))
   `(magit-hash ((t :foreground ,dark-grey)))
   ;; Diagnostics.
   `(flymake-error ((t :underline (:style wave :color ,red))))
   `(flymake-warning ((t :underline (:style wave :color ,maroon))))
   `(flymake-note ((t :underline (:style wave :color ,green))))
   ;; Outlines.
   `(org-level-1 ((t :foreground ,navy :weight bold :height 1.2)))
   `(org-level-2 ((t :foreground ,teal :weight bold :height 1.1)))
   `(org-level-3 ((t :foreground ,purple :weight bold)))
   `(org-level-4 ((t :foreground ,maroon :weight bold)))
   `(org-block ((t :background "#F0F0F0" :extend t)))
   `(org-code ((t :foreground ,purple)))
   `(org-todo ((t :foreground ,red :weight bold)))
   `(org-done ((t :foreground ,green :weight bold)))
   `(markdown-header-face ((t :foreground ,navy :weight bold)))
   `(markdown-code-face ((t :background "#F0F0F0" :extend t)))
   ;; Terminals: the VGA's colours, as MS-DOS Prompt showed them.
   `(ansi-color-black ((t :foreground ,black :background ,black)))
   `(ansi-color-red ((t :foreground ,maroon :background ,maroon)))
   `(ansi-color-green ((t :foreground ,green :background ,green)))
   `(ansi-color-yellow ((t :foreground ,olive :background ,olive)))
   `(ansi-color-blue ((t :foreground ,navy :background ,navy)))
   `(ansi-color-magenta ((t :foreground ,purple :background ,purple)))
   `(ansi-color-cyan ((t :foreground ,teal :background ,teal)))
   `(ansi-color-white ((t :foreground ,grey :background ,grey)))
   `(ansi-color-bright-black ((t :foreground ,dark-grey :background ,dark-grey)))
   `(ansi-color-bright-red ((t :foreground ,red :background ,red)))
   `(ansi-color-bright-green ((t :foreground ,green :background ,green)))
   `(ansi-color-bright-yellow ((t :foreground ,olive :background ,olive)))
   `(ansi-color-bright-blue ((t :foreground ,blue :background ,blue)))
   `(ansi-color-bright-magenta ((t :foreground ,magenta :background ,magenta)))
   `(ansi-color-bright-cyan ((t :foreground ,teal :background ,teal)))
   `(ansi-color-bright-white ((t :foreground ,grey :background ,grey))))

  (custom-theme-set-variables
   'win3
   `(lispm-desktop-platinum-colors '(:chrome ,grey :header ,grey :light ,white
                                     :shade ,dark-grey :frame ,dark-grey :muted ,dark-grey
                                     :accent ,navy :outline ,black
                                     :highlight ,navy :highlight-dark ,navy :highlight-text ,white
                                     :thumb ,grey :thumb-light ,white :thumb-dark ,dark-grey
                                     :focus ,black
                                     :alert ,grey :alert-dark ,dark-grey
                                     :strip ,grey :strip-light ,white
                                     :strip-shade ,dark-grey :strip-dark ,dark-grey
                                     ;; Captions, borders and menus.
                                     :title ,navy :title-text ,white
                                     :title-inactive ,white :title-inactive-text ,black
                                     :border ,grey :border-inactive ,grey
                                     :menu ,white :menu-text ,black
                                     ;; The desktop, Windows Default's grey.
                                     :desktop ,grey))))

(provide-theme 'win3)

;;; win3-theme.el ends here
