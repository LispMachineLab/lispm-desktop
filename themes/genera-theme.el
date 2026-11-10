;;; genera-theme.el --- Symbolics Genera's paper-white monochrome colours  -*- lexical-binding: t; -*-

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

;; The high-contrast paper-white 1-bit monochrome scheme of Symbolics Genera
;; (Dynamic Windows / OpenGenera): pitch-black ink on crisp paper-white
;; panes, inverse-video black label bars and Who-Line plates, 1-pixel solid
;; black rules without 3D bevels, and Zmacs typographic code faces (bold
;; forms, italic comments, underlined presentations).

;;; Code:

(deftheme genera
  "Symbolics Genera's monochrome scheme: inverse-video labels, black ink on paper white.")

(let* ((white "#FFFFFF")
       (ivory "#F5F5F5")
       (screen "#FFFFFF")
       (black "#000000")
       (ink "#1A1A1A")
       (muted "#555555")
       (rule "#333333")
       (wash "#EEEEEE")
       (red "#8B0000")
       (amber "#6B4C00")
       (green "#005500"))
  (custom-theme-set-faces
   'genera
   ;; The document pane.
   `(default ((t :background ,white :foreground ,black)))
   `(cursor ((t :background ,black)))
   `(fringe ((t :background ,white :foreground ,black)))
   `(region ((t :background ,black :foreground ,white :extend t)))
   `(secondary-selection ((t :background ,wash :extend t)))
   `(highlight ((t :background ,black :foreground ,white)))
   `(hl-line ((t :background ,ivory :extend t)))
   `(shadow ((t :foreground ,muted)))
   `(line-number ((t :foreground ,muted :background ,white)))
   `(line-number-current-line ((t :foreground ,black :background ,ivory :weight bold)))
   `(minibuffer-prompt ((t :foreground ,black :weight bold)))
   `(link ((t :foreground ,black :underline t :weight bold)))
   `(link-visited ((t :foreground ,muted :underline t)))
   `(button ((t :foreground ,black :box (:line-width (1 . 1) :color ,black))))
   `(error ((t :foreground ,red :weight bold)))
   `(warning ((t :foreground ,amber :weight bold)))
   `(success ((t :foreground ,green :weight bold)))
   `(trailing-whitespace ((t :background ,wash)))
   `(escape-glyph ((t :foreground ,black :weight bold)))
   `(homoglyph ((t :foreground ,black :weight bold)))
   ;; Searching and matching.
   `(isearch ((t :background ,black :foreground ,white :weight bold)))
   `(isearch-fail ((t :background ,red :foreground ,white)))
   `(lazy-highlight ((t :background ,wash :foreground ,black)))
   `(match ((t :background ,wash :weight bold)))
   `(show-paren-match ((t :background ,black :foreground ,white :weight bold)))
   `(show-paren-mismatch ((t :background ,red :foreground ,white)))
   ;; Zmacs typography: bold keywords and definitions, italic comments and strings.
   `(font-lock-keyword-face ((t :foreground ,black :weight bold)))
   `(font-lock-comment-face ((t :foreground ,muted :slant italic)))
   `(font-lock-comment-delimiter-face ((t :foreground ,muted :slant italic)))
   `(font-lock-doc-face ((t :foreground ,muted :slant italic)))
   `(font-lock-string-face ((t :foreground ,ink :slant italic)))
   `(font-lock-constant-face ((t :foreground ,black :weight bold)))
   `(font-lock-builtin-face ((t :foreground ,black :weight bold)))
   `(font-lock-function-name-face ((t :foreground ,black :weight bold :underline t)))
   `(font-lock-variable-name-face ((t :foreground ,ink)))
   `(font-lock-type-face ((t :foreground ,black :weight bold)))
   `(font-lock-preprocessor-face ((t :foreground ,black :weight bold)))
   `(font-lock-negation-char-face ((t :foreground ,black :weight bold)))
   `(font-lock-warning-face ((t :foreground ,red :weight bold)))
   ;; Chrome and window dividers: crisp 1px black rules.
   `(mode-line ((t :background ,black :foreground ,white)))
   `(mode-line-inactive ((t :background ,ivory :foreground ,black)))
   `(header-line ((t :background ,black :foreground ,white)))
   `(tab-bar ((t :background ,white :foreground ,black)))
   `(tab-line ((t :background ,ivory :foreground ,black)))
   `(vertical-border ((t :foreground ,black)))
   `(window-divider ((t :foreground ,black)))
   `(window-divider-first-pixel ((t :foreground ,black)))
   `(window-divider-last-pixel ((t :foreground ,black)))
   ;; Completion.
   `(completions-common-part ((t :foreground ,black :weight bold :underline t)))
   `(completions-first-difference ((t :weight bold)))
   `(vertico-current ((t :background ,black :foreground ,white :extend t)))
   `(orderless-match-face-0 ((t :foreground ,black :weight bold :underline t)))
   `(orderless-match-face-1 ((t :foreground ,black :weight bold)))
   `(orderless-match-face-2 ((t :foreground ,ink :slant italic)))
   `(orderless-match-face-3 ((t :foreground ,muted :underline t)))
   `(marginalia-documentation ((t :foreground ,muted :slant italic)))
   `(corfu-default ((t :background ,white :foreground ,black)))
   `(corfu-current ((t :background ,black :foreground ,white)))
   `(corfu-border ((t :background ,black)))
   ;; Diffs and version control.
   `(diff-added ((t :background "#E6F2E6" :foreground ,green :extend t)))
   `(diff-removed ((t :background "#F9E6E6" :foreground ,red :extend t)))
   `(diff-refine-added ((t :background "#C8E6C8" :weight bold)))
   `(diff-refine-removed ((t :background "#F2C8C8" :weight bold)))
   `(diff-header ((t :background ,ivory)))
   `(diff-file-header ((t :background ,ivory :weight bold)))
   `(magit-section-highlight ((t :background ,ivory :extend t)))
   `(magit-branch-local ((t :foreground ,black :weight bold)))
   `(magit-branch-remote ((t :foreground ,muted :weight bold)))
   `(magit-hash ((t :foreground ,muted)))
   ;; Diagnostics.
   `(flymake-error ((t :underline (:style line :color ,red))))
   `(flymake-warning ((t :underline (:style line :color ,amber))))
   `(flymake-note ((t :underline (:style line :color ,muted))))
   ;; Outlines.
   `(org-level-1 ((t :foreground ,black :weight bold :height 1.15 :underline t)))
   `(org-level-2 ((t :foreground ,black :weight bold :height 1.05)))
   `(org-level-3 ((t :foreground ,black :weight bold)))
   `(org-level-4 ((t :foreground ,ink :slant italic)))
   `(org-block ((t :background ,ivory :extend t)))
   `(org-code ((t :foreground ,black :weight bold)))
   `(org-todo ((t :background ,black :foreground ,white :weight bold)))
   `(org-done ((t :foreground ,muted :weight bold)))
   `(markdown-header-face ((t :foreground ,black :weight bold :underline t)))
   `(markdown-code-face ((t :background ,ivory :extend t)))
   ;; Terminals.
   `(ansi-color-black ((t :foreground ,black :background ,black)))
   `(ansi-color-red ((t :foreground ,red :background ,red)))
   `(ansi-color-green ((t :foreground ,green :background ,green)))
   `(ansi-color-yellow ((t :foreground ,amber :background ,amber)))
   `(ansi-color-blue ((t :foreground ,black :background ,black)))
   `(ansi-color-magenta ((t :foreground ,ink :background ,ink)))
   `(ansi-color-cyan ((t :foreground ,rule :background ,rule)))
   `(ansi-color-white ((t :foreground ,ivory :background ,ivory)))
   `(ansi-color-bright-black ((t :foreground ,muted :background ,muted)))
   `(ansi-color-bright-red ((t :foreground ,red :background ,red)))
   `(ansi-color-bright-green ((t :foreground ,green :background ,green)))
   `(ansi-color-bright-yellow ((t :foreground ,amber :background ,amber)))
   `(ansi-color-bright-blue ((t :foreground ,black :background ,black)))
   `(ansi-color-bright-magenta ((t :foreground ,ink :background ,ink)))
   `(ansi-color-bright-cyan ((t :foreground ,rule :background ,rule)))
   `(ansi-color-bright-white ((t :foreground ,white :background ,white))))

  (custom-theme-set-variables
   'genera
   `(lispm-desktop-platinum-colors '(:content ,white :text ,black :dim ,muted
                                     :chrome ,white :header ,white :light ,white
                                     :shade ,rule :frame ,black :muted ,muted
                                     :accent ,black :outline ,black
                                     :highlight ,black :highlight-dark ,black :highlight-text ,white
                                     :thumb ,black :thumb-light ,black :thumb-dark ,black
                                     :focus ,black
                                     :alert ,white :alert-dark ,black
                                     :strip ,white :strip-light ,white
                                     :strip-shade ,rule :strip-dark ,black
                                     ;; Open Genera 2.0: pure paper-white pane headers with bold-italic black ink,
                                     ;; thick+thin double black outer frame, and inverse-video black selections.
                                     :title ,white :title-text ,black
                                     :title-inactive ,white :title-inactive-text ,muted
                                     :border ,white :border-inactive ,white
                                     :menu ,white :menu-text ,black
                                     :desktop ,screen))))

(provide-theme 'genera)

;;; genera-theme.el ends here
