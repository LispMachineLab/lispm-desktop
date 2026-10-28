;;; lispm-ui-renames.eld --- The desktop's names that moved to LispM UI  -*- lisp-data -*-
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; See lispm-ui-rename.el.  Each entry is ("OLD" . "NEW"), a symbol
;; renamed, or (:prefix "OLD" . "NEW"), every symbol starting with OLD;
;; in the order of the moves, a move's renames together.

;; LispM UI 0.1.0: LispM's forms, which moved there from LispM Agent.
((:prefix "lispm-form-" . "lispm-ui-form-")
 ("lispm-form" . "lispm-ui-form")
 ;; LispM UI 0.1.0: what the modal panels share, from lispm-desktop-modal.el.
 (:prefix "lispm-desktop-modal-" . "lispm-ui-modal-")
 ("lispm-desktop-modal" . "lispm-ui-modal")
 ;; LispM UI 0.2.0: the palette, from lispm-desktop-platinum-palette.el.
 ("lispm-desktop-platinum-rgb" . "lispm-ui-rgb")
 ("lispm-desktop-platinum-blend" . "lispm-ui-blend")
 ("lispm-desktop-platinum--luminance" . "lispm-ui-luminance")
 ("lispm-desktop-platinum-legible" . "lispm-ui-legible")
 ("lispm-desktop-platinum-current-palette" . "lispm-ui-current-palette")
 ("lispm-desktop-platinum-generation" . "lispm-ui-palette-generation")
 ("lispm-desktop-platinum-derive-palette" . "lispm-ui-derive-palette")
 ("lispm-desktop-platinum-grey" . "lispm-ui-grey")
 ("lispm-desktop-platinum--merge-palette" . "lispm-ui-merge-palette")
 ("lispm-desktop-platinum-color" . "lispm-ui-color")
 ("lispm-desktop-platinum-title" . "lispm-ui-title")
 ("lispm-desktop-platinum-list" . "lispm-ui-list")
 ("lispm-desktop-platinum-small" . "lispm-ui-small")
 ("lispm-desktop-platinum-field" . "lispm-ui-field")
 ("lispm-desktop-platinum-field-focused" . "lispm-ui-field-focused")
 ("lispm-desktop-platinum-button" . "lispm-ui-button")
 ("lispm-desktop-platinum-button-pressed" . "lispm-ui-button-pressed")
 ;; LispM UI 0.2.0: the looks' registry, from lispm-desktop-look.el.
 ("lispm-desktop-looks" . "lispm-ui-looks")
 ("lispm-desktop-look-define" . "lispm-ui-look-define")
 ("lispm-desktop-look-get" . "lispm-ui-look-get")
 ("lispm-desktop-look-title" . "lispm-ui-look-title")
 ("lispm-desktop-look-family" . "lispm-ui-look-family")
 ("lispm-desktop-look-metric" . "lispm-ui-look-metric")
 ("lispm-desktop-look-families" . "lispm-ui-look-families")
 ;; LispM UI 0.2.0: measuring and breaking text, from
 ;; lispm-desktop-platinum-draw.el and lispm-desktop-i18n.el.
 ("lispm-desktop-platinum-content-width" . "lispm-ui-text-width")
 ("lispm-desktop-i18n-cjk-families" . "lispm-ui-text-cjk-families")
 ("lispm-desktop-i18n-cjk-family" . "lispm-ui-text-cjk-family")
 ("lispm-desktop-i18n-words" . "lispm-ui-text-words")
 ("lispm-desktop-i18n-wrap" . "lispm-ui-text-wrap")
 ;; LispM UI 0.2.0: drawing, from lispm-desktop-platinum-draw.el and
 ;; lispm-desktop-look.el.
 ("lispm-desktop-platinum-image" . "lispm-ui-image")
 ("lispm-desktop-platinum--svg-image" . "lispm-ui-svg-image")
 ("lispm-desktop-look-image" . "lispm-ui-look-image")
 ("lispm-desktop-platinum-svg-fill" . "lispm-ui-svg-fill")
 ("lispm-desktop-platinum--dot" . "lispm-ui-svg-dot")
 ("lispm-desktop-platinum-svg-row" . "lispm-ui-svg-row")
 ("lispm-desktop-platinum-svg-column" . "lispm-ui-svg-column")
 ("lispm-desktop-look-svg-fill" . "lispm-ui-svg-paint")
 ("lispm-desktop-look-svg-edges" . "lispm-ui-svg-edges")
 ("lispm-desktop-look-svg-outline" . "lispm-ui-svg-outline")
 ("lispm-desktop-look-svg-arrow" . "lispm-ui-svg-arrow")
 ("lispm-desktop-look-svg-dotted" . "lispm-ui-svg-dotted")
 ("lispm-desktop-look-svg-pixels" . "lispm-ui-svg-pixels")
 ("lispm-desktop-platinum-svg-font" . "lispm-ui-svg-font")
 ("lispm-desktop-platinum--label" . "lispm-ui-svg-label")
 ("lispm-desktop-platinum-svg-dom" . "lispm-ui-svg-dom")
 ("lispm-desktop-platinum-svg-embed" . "lispm-ui-svg-embed")
 ("lispm-desktop-platinum-svg-icon" . "lispm-ui-svg-icon")
 ("lispm-desktop-platinum-svg-emacs-image" . "lispm-ui-svg-emacs-image")
 ("lispm-desktop-platinum-svg-content" . "lispm-ui-svg-content")
 ("lispm-desktop-platinum--content-key" . "lispm-ui-content-key")
 ;; LispM UI 0.2.0: dressing forms, from lispm-desktop-form.el.  Its
 ;; `lispm-desktop-form--faces' laid a form out for the desktop's windows
 ;; too, which the desktop still does.
 ("lispm-desktop-form--control-at" . "lispm-ui-form--control-at")
 ("lispm-desktop-form--field-overlay" . "lispm-ui-form--field-overlay")
 ("lispm-desktop-form--focus" . "lispm-ui-form--focus")
 ("lispm-desktop-form--follow" . "lispm-ui-form--follow")
 ("lispm-desktop-form--image" . "lispm-ui-form--image")
 ("lispm-desktop-form--remaps" . "lispm-ui-form--remaps")
 ("lispm-desktop-form--show" . "lispm-ui-form--show")
 ("lispm-desktop-form--text-areas" . "lispm-ui-form--text-areas")
 ("lispm-desktop-form--well" . "lispm-ui-form--well")
 ("lispm-desktop-form-control-properties" . "lispm-ui-form-control-properties")
 ("lispm-desktop-form-dress" . "lispm-ui-form-dress")
 ("lispm-desktop-form-glyph" . "lispm-ui-form-glyph")
 ("lispm-desktop-form-group" . "lispm-ui-form-group")
 ("lispm-desktop-form-redraw-all" . "lispm-ui-form-redraw-all")
 ("lispm-desktop-form-text-area" . "lispm-ui-form-text-area")
 ("lispm-desktop-form-well-size" . "lispm-ui-form-well-size"))
