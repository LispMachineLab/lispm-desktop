;;; lispm-ui-renames.eld --- The desktop's names that moved to LispM UI  -*- lisp-data -*-
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; See lispm-ui-rename.el.  Each entry is ("OLD" . "NEW"), a symbol
;; renamed, or (:prefix "OLD" . "NEW"), every symbol starting with OLD;
;; in the order of the moves, a move's renames together.

;; LispM UI 0.1.0: LispM's forms, which moved there from LispM Agent.
((:prefix "lispm-form-" . "lispm-ui-form-")
 ("lispm-form" . "lispm-ui-form"))
