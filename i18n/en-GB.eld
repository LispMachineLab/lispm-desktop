;;; en-GB.eld --- LispM Desktop's words in British English  -*- lisp-data -*-
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; See lispm-desktop-i18n.el.  Each entry is (MESSAGE . TRANSLATION), a
;; message said in a context (CONTEXT . MESSAGE), a plural's translation
;; the list of its forms; in the order the code writes them.

(:name "English (UK)"
 :messages
 (("Move the %s session “%s” to the Trash?" . "Move the %s session “%s” to the Bin?")
  ("Move to Trash" . "Move to Bin")
  ("Its %d file goes to the Trash." "Its %d file goes to the Bin." "Its %d files go to the Bin.")
  ("“%s” was moved to the Trash" . "“%s” was moved to the Bin")
  ("%-I:%M %p" . "%H:%M")
  ("Center" . "Centre")
  ("Trash" . "Bin")
  ("The Trash cannot be read: %s" . "The Bin cannot be read: %s")))
