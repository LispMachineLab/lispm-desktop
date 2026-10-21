;;; neon-theme.el --- Cyan, magenta and violet neon on night purple  -*- lexical-binding: t; -*-

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

;; Neon over a city at night: cyan lines, magenta accents, violet, on a
;; night purple, text a pale lavender.  No yellow anywhere.
;;
;; The neon look (lispm-desktop-glow.el) is made of glass of gain 1.9: a
;; pixel is as opaque as 1.9 times its red, green and blue, so the window
;; body, #1A0B3A, lets the city through by almost a third, and black is
;; clear.  The chrome takes its colours from
;; `lispm-desktop-platinum-colors', which this theme sets.

;;; Code:

(deftheme neon
  "Cyan, magenta and violet neon on night purple; no yellow.")

(let* ((void "#000000")
       (night "#120424")
       (bar "#0B0220")
       (track "#0B0620")
       (panel "#1A0B3A")
       (menu "#1E0E45")
       (header "#241050")
       (line "#3A2F5C")
       (dim "#8A7FB8")
       (text "#ECEAFF")
       (white "#FFFFFF")
       (cyan "#00F0FF")
       (pale-cyan "#9FF8FF")
       (deep-cyan "#00B8C4")
       (magenta "#FF2A6D")
       (deep-magenta "#C81E55")
       (violet "#9D4DFF")
       (lilac "#C3A0FF")
       (red "#FF003C"))
  (custom-theme-set-faces
   'neon
   ;; The document.
   `(default ((t :background ,panel :foreground ,text)))
   `(cursor ((t :background ,cyan)))
   `(fringe ((t :background ,panel)))
   `(region ((t :background ,line :foreground ,white :extend t)))
   `(secondary-selection ((t :background ,header :extend t)))
   `(highlight ((t :background ,header)))
   `(hl-line ((t :background ,header :extend t)))
   `(shadow ((t :foreground ,dim)))
   `(line-number ((t :foreground ,line :background ,panel)))
   `(line-number-current-line ((t :foreground ,cyan :background ,panel)))
   `(minibuffer-prompt ((t :foreground ,magenta :weight bold)))
   `(link ((t :foreground ,cyan :underline t)))
   `(link-visited ((t :foreground ,lilac :underline t)))
   `(button ((t :foreground ,cyan :underline t)))
   `(error ((t :foreground ,red :weight bold)))
   `(warning ((t :foreground ,magenta :weight bold)))
   `(success ((t :foreground ,cyan :weight bold)))
   `(trailing-whitespace ((t :background ,deep-magenta)))
   `(escape-glyph ((t :foreground ,violet)))
   `(homoglyph ((t :foreground ,violet)))
   ;; Searching and matching.
   `(isearch ((t :background ,magenta :foreground ,white)))
   `(isearch-fail ((t :background ,red :foreground ,white)))
   `(lazy-highlight ((t :background ,line :foreground ,pale-cyan)))
   `(match ((t :background ,line :foreground ,cyan)))
   `(show-paren-match ((t :background ,line :foreground ,cyan :weight bold)))
   `(show-paren-mismatch ((t :background ,red :foreground ,white)))
   ;; Code.
   `(font-lock-keyword-face ((t :foreground ,magenta :weight bold)))
   `(font-lock-comment-face ((t :foreground ,dim :slant italic)))
   `(font-lock-comment-delimiter-face ((t :foreground ,dim :slant italic)))
   `(font-lock-doc-face ((t :foreground ,lilac)))
   `(font-lock-string-face ((t :foreground ,cyan)))
   `(font-lock-constant-face ((t :foreground ,lilac)))
   `(font-lock-builtin-face ((t :foreground ,pale-cyan)))
   `(font-lock-function-name-face ((t :foreground ,cyan :weight bold)))
   `(font-lock-variable-name-face ((t :foreground ,text)))
   `(font-lock-type-face ((t :foreground ,lilac)))
   `(font-lock-preprocessor-face ((t :foreground ,violet)))
   `(font-lock-negation-char-face ((t :foreground ,magenta)))
   `(font-lock-warning-face ((t :foreground ,red :weight bold)))
   ;; Chrome: lispm-desktop derives the rest from `lispm-desktop-platinum-colors'.
   `(mode-line ((t :background ,header :foreground ,text)))
   `(mode-line-inactive ((t :background ,panel :foreground ,dim)))
   `(header-line ((t :background ,header :foreground ,text)))
   `(tab-bar ((t :background ,bar :foreground ,cyan)))
   `(tab-line ((t :background ,header :foreground ,text)))
   `(vertical-border ((t :foreground ,line)))
   `(window-divider ((t :foreground ,line)))
   `(window-divider-first-pixel ((t :foreground ,line)))
   `(window-divider-last-pixel ((t :foreground ,line)))
   ;; Completion.
   `(completions-common-part ((t :foreground ,cyan :weight bold)))
   `(completions-first-difference ((t :weight bold)))
   `(vertico-current ((t :background ,line :foreground ,white :extend t)))
   `(orderless-match-face-0 ((t :foreground ,cyan :weight bold)))
   `(orderless-match-face-1 ((t :foreground ,magenta :weight bold)))
   `(orderless-match-face-2 ((t :foreground ,lilac :weight bold)))
   `(orderless-match-face-3 ((t :foreground ,pale-cyan :weight bold)))
   `(marginalia-documentation ((t :foreground ,dim)))
   `(corfu-default ((t :background ,menu :foreground ,text)))
   `(corfu-current ((t :background ,line :foreground ,white)))
   `(corfu-border ((t :background ,cyan)))
   ;; Diffs and version control.
   `(diff-added ((t :background "#0E2A40" :foreground ,pale-cyan :extend t)))
   `(diff-removed ((t :background "#3A0B2C" :foreground "#FF8FB1" :extend t)))
   `(diff-refine-added ((t :background ,deep-cyan :foreground ,white)))
   `(diff-refine-removed ((t :background ,deep-magenta :foreground ,white)))
   `(diff-header ((t :background ,header)))
   `(diff-file-header ((t :background ,header :weight bold)))
   `(magit-section-highlight ((t :background ,header :extend t)))
   `(magit-branch-local ((t :foreground ,cyan)))
   `(magit-branch-remote ((t :foreground ,magenta)))
   `(magit-hash ((t :foreground ,dim)))
   ;; Diagnostics.
   `(flymake-error ((t :underline (:style wave :color ,red))))
   `(flymake-warning ((t :underline (:style wave :color ,magenta))))
   `(flymake-note ((t :underline (:style wave :color ,cyan))))
   ;; Outlines.
   `(org-level-1 ((t :foreground ,magenta :weight bold :height 1.2)))
   `(org-level-2 ((t :foreground ,cyan :weight bold :height 1.1)))
   `(org-level-3 ((t :foreground ,lilac :weight bold)))
   `(org-level-4 ((t :foreground ,pale-cyan :weight bold)))
   `(org-block ((t :background ,track :extend t)))
   `(org-code ((t :foreground ,cyan)))
   `(org-todo ((t :foreground ,magenta :weight bold)))
   `(org-done ((t :foreground ,dim :weight bold)))
   `(markdown-header-face ((t :foreground ,magenta :weight bold)))
   `(markdown-code-face ((t :background ,track :extend t)))
   ;; Terminals.
   `(ansi-color-black ((t :foreground ,night :background ,night)))
   `(ansi-color-red ((t :foreground ,magenta :background ,magenta)))
   `(ansi-color-green ((t :foreground ,deep-cyan :background ,deep-cyan)))
   `(ansi-color-yellow ((t :foreground ,lilac :background ,lilac)))
   `(ansi-color-blue ((t :foreground ,violet :background ,violet)))
   `(ansi-color-magenta ((t :foreground ,deep-magenta :background ,deep-magenta)))
   `(ansi-color-cyan ((t :foreground ,cyan :background ,cyan)))
   `(ansi-color-white ((t :foreground ,text :background ,text)))
   `(ansi-color-bright-black ((t :foreground ,line :background ,line)))
   `(ansi-color-bright-red ((t :foreground ,red :background ,red)))
   `(ansi-color-bright-green ((t :foreground ,pale-cyan :background ,pale-cyan)))
   `(ansi-color-bright-yellow ((t :foreground ,white :background ,white)))
   `(ansi-color-bright-blue ((t :foreground ,lilac :background ,lilac)))
   `(ansi-color-bright-magenta ((t :foreground ,magenta :background ,magenta)))
   `(ansi-color-bright-cyan ((t :foreground ,pale-cyan :background ,pale-cyan)))
   `(ansi-color-bright-white ((t :foreground ,white :background ,white))))

  (custom-theme-set-variables
   'neon
   `(lispm-desktop-platinum-colors '(:chrome ,panel :header ,header :light ,dim
                                     :shade ,line :frame ,line :muted ,dim
                                     :accent ,cyan :outline ,cyan
                                     :highlight ,magenta :highlight-dark ,deep-magenta :highlight-text ,white
                                     :thumb ,cyan :thumb-light ,pale-cyan :thumb-dark ,deep-cyan
                                     :focus ,cyan
                                     :alert ,magenta :alert-dark ,violet
                                     :strip ,panel :strip-light ,dim
                                     :strip-shade ,line :strip-dark ,header
                                     ;; Title bars, frames and menus.
                                     :title ,magenta :title-text ,white
                                     :title-inactive ,panel :title-inactive-text ,dim
                                     :border ,cyan :border-inactive ,line
                                     :menu ,menu :menu-text ,text
                                     :desktop ,night
                                     ;; What the glowing looks add.
                                     :glow ,cyan :hot ,magenta :title-end ,violet
                                     :bar ,bar :bar-text ,cyan :bar-line ,magenta
                                     :track ,track :pressed ,magenta :pressed-text ,white
                                     :danger ,red))))

(provide-theme 'neon)

;;; neon-theme.el ends here
