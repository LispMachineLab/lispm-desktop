;;; phosphor-theme.el --- Green phosphor on black glass  -*- lexical-binding: t; -*-

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

;; A green phosphor screen: one green in eight steps, from the black
;; glass to the white-hot head of a falling glyph.  Code is told apart by
;; how bright it burns, as monochrome terminals told it.
;;
;; The phosphor look (lispm-desktop-glow.el) is made of glass of gain 4:
;; a pixel is as opaque as four times its red, green and blue, so the
;; window body, #041E0F, lets a quarter of the desktop through, and black
;; is clear.  The chrome takes its colours from
;; `lispm-desktop-platinum-colors', which this theme sets.

;;; Code:

(deftheme phosphor
  "Green phosphor on black glass: one green in eight steps.")

(let* ((void "#000000")
       (track "#020A05")
       (panel "#041E0F")
       (header "#062A12")
       (title "#07301A")
       (edge "#0B4A1D")
       (dim "#0F7A2E")
       (mid "#00C834")
       (green "#00FF41")
       (head "#D4FFE0"))
  (custom-theme-set-faces
   'phosphor
   ;; The document.
   `(default ((t :background ,panel :foreground ,green)))
   `(cursor ((t :background ,green)))
   `(fringe ((t :background ,panel)))
   `(region ((t :background ,edge :foreground ,head :extend t)))
   `(secondary-selection ((t :background ,header :extend t)))
   `(highlight ((t :background ,edge)))
   `(hl-line ((t :background ,header :extend t)))
   `(shadow ((t :foreground ,dim)))
   `(line-number ((t :foreground ,edge :background ,panel)))
   `(line-number-current-line ((t :foreground ,mid :background ,panel)))
   `(minibuffer-prompt ((t :foreground ,head :weight bold)))
   `(link ((t :foreground ,head :underline t)))
   `(link-visited ((t :foreground ,mid :underline t)))
   `(button ((t :foreground ,head :underline t)))
   `(error ((t :foreground ,head :weight bold)))
   `(warning ((t :foreground ,green :weight bold)))
   `(success ((t :foreground ,mid :weight bold)))
   `(trailing-whitespace ((t :background ,edge)))
   `(escape-glyph ((t :foreground ,mid)))
   `(homoglyph ((t :foreground ,mid)))
   ;; Searching and matching.
   `(isearch ((t :background ,green :foreground ,void)))
   `(isearch-fail ((t :background ,head :foreground ,void)))
   `(lazy-highlight ((t :background ,edge :foreground ,head)))
   `(match ((t :background ,edge :foreground ,head)))
   `(show-paren-match ((t :background ,edge :foreground ,head :weight bold)))
   `(show-paren-mismatch ((t :background ,head :foreground ,void)))
   ;; Code, by brightness.
   `(font-lock-keyword-face ((t :foreground ,head :weight bold)))
   `(font-lock-comment-face ((t :foreground ,dim :slant italic)))
   `(font-lock-comment-delimiter-face ((t :foreground ,dim :slant italic)))
   `(font-lock-doc-face ((t :foreground ,dim)))
   `(font-lock-string-face ((t :foreground ,mid)))
   `(font-lock-constant-face ((t :foreground ,head)))
   `(font-lock-builtin-face ((t :foreground ,mid :weight bold)))
   `(font-lock-function-name-face ((t :foreground ,green :weight bold)))
   `(font-lock-variable-name-face ((t :foreground ,green)))
   `(font-lock-type-face ((t :foreground ,head)))
   `(font-lock-preprocessor-face ((t :foreground ,mid)))
   `(font-lock-negation-char-face ((t :foreground ,head)))
   `(font-lock-warning-face ((t :foreground ,head :weight bold)))
   ;; Chrome: lispm-desktop derives the rest from `lispm-desktop-platinum-colors'.
   `(mode-line ((t :background ,title :foreground ,green)))
   `(mode-line-inactive ((t :background ,panel :foreground ,dim)))
   `(header-line ((t :background ,header :foreground ,green)))
   `(tab-bar ((t :background ,void :foreground ,green)))
   `(tab-line ((t :background ,header :foreground ,green)))
   `(vertical-border ((t :foreground ,edge)))
   `(window-divider ((t :foreground ,edge)))
   `(window-divider-first-pixel ((t :foreground ,edge)))
   `(window-divider-last-pixel ((t :foreground ,edge)))
   ;; Completion.
   `(completions-common-part ((t :foreground ,head :weight bold)))
   `(completions-first-difference ((t :weight bold)))
   `(vertico-current ((t :background ,edge :foreground ,head :extend t)))
   `(orderless-match-face-0 ((t :foreground ,head :weight bold)))
   `(orderless-match-face-1 ((t :foreground ,mid :weight bold)))
   `(orderless-match-face-2 ((t :foreground ,green :weight bold :underline t)))
   `(orderless-match-face-3 ((t :foreground ,mid :underline t)))
   `(marginalia-documentation ((t :foreground ,dim)))
   `(corfu-default ((t :background ,title :foreground ,green)))
   `(corfu-current ((t :background ,edge :foreground ,head)))
   `(corfu-border ((t :background ,green)))
   ;; Diffs and version control.
   `(diff-added ((t :background ,header :foreground ,head :extend t)))
   `(diff-removed ((t :background ,panel :foreground ,dim :strike-through t :extend t)))
   `(diff-refine-added ((t :background ,edge :foreground ,head)))
   `(diff-refine-removed ((t :background ,track :foreground ,dim)))
   `(diff-header ((t :background ,header)))
   `(diff-file-header ((t :background ,header :weight bold)))
   `(magit-section-highlight ((t :background ,header :extend t)))
   `(magit-branch-local ((t :foreground ,head)))
   `(magit-branch-remote ((t :foreground ,mid)))
   `(magit-hash ((t :foreground ,dim)))
   ;; Diagnostics.
   `(flymake-error ((t :underline (:style wave :color ,head))))
   `(flymake-warning ((t :underline (:style wave :color ,green))))
   `(flymake-note ((t :underline (:style wave :color ,dim))))
   ;; Outlines.
   `(org-level-1 ((t :foreground ,head :weight bold :height 1.2)))
   `(org-level-2 ((t :foreground ,green :weight bold :height 1.1)))
   `(org-level-3 ((t :foreground ,mid :weight bold)))
   `(org-level-4 ((t :foreground ,dim :weight bold)))
   `(org-block ((t :background ,track :extend t)))
   `(org-code ((t :foreground ,head)))
   `(org-todo ((t :foreground ,head :weight bold)))
   `(org-done ((t :foreground ,dim :weight bold)))
   `(markdown-header-face ((t :foreground ,head :weight bold)))
   `(markdown-code-face ((t :background ,track :extend t)))
   ;; Terminals: every colour a step of the green.
   `(ansi-color-black ((t :foreground ,void :background ,void)))
   `(ansi-color-red ((t :foreground ,head :background ,head)))
   `(ansi-color-green ((t :foreground ,green :background ,green)))
   `(ansi-color-yellow ((t :foreground ,mid :background ,mid)))
   `(ansi-color-blue ((t :foreground ,dim :background ,dim)))
   `(ansi-color-magenta ((t :foreground ,mid :background ,mid)))
   `(ansi-color-cyan ((t :foreground ,green :background ,green)))
   `(ansi-color-white ((t :foreground ,green :background ,green)))
   `(ansi-color-bright-black ((t :foreground ,dim :background ,dim)))
   `(ansi-color-bright-red ((t :foreground ,head :background ,head)))
   `(ansi-color-bright-green ((t :foreground ,head :background ,head)))
   `(ansi-color-bright-yellow ((t :foreground ,head :background ,head)))
   `(ansi-color-bright-blue ((t :foreground ,mid :background ,mid)))
   `(ansi-color-bright-magenta ((t :foreground ,head :background ,head)))
   `(ansi-color-bright-cyan ((t :foreground ,head :background ,head)))
   `(ansi-color-bright-white ((t :foreground ,head :background ,head))))

  (custom-theme-set-variables
   'phosphor
   `(lispm-desktop-platinum-colors '(:chrome ,panel :header ,header :light ,mid
                                     :shade ,edge :frame ,edge :muted ,dim
                                     :accent ,green :outline ,green
                                     :highlight ,green :highlight-dark ,mid :highlight-text ,void
                                     :thumb ,green :thumb-light ,head :thumb-dark ,mid
                                     :focus ,head
                                     :alert ,green :alert-dark ,dim
                                     :strip ,panel :strip-light ,mid
                                     :strip-shade ,edge :strip-dark ,dim
                                     ;; Title bars, frames and menus.
                                     :title ,title :title-text ,green
                                     :title-inactive ,panel :title-inactive-text ,dim
                                     :border ,green :border-inactive ,edge
                                     :menu ,title :menu-text ,green
                                     :desktop ,void
                                     ;; What the glowing looks add.
                                     :glow ,green :hot ,head :title-end ,green
                                     :bar ,void :bar-text ,green :bar-line ,green
                                     :track ,track :pressed ,mid :pressed-text ,void
                                     :danger ,green))))

(provide-theme 'phosphor)

;;; phosphor-theme.el ends here
