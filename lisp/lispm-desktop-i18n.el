;;; lispm-desktop-i18n.el --- The language LispM speaks  -*- lexical-binding: t; -*-

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

;; The desktop's words are written in English in the code, and shown in
;; the language chosen in Control Panels' Language panel, or the
;; computer's own (`lispm-desktop-language').  Every word shown passes
;; through one of:
;;
;;   (lispm-desktop-tr MESSAGE ARGS...)      MESSAGE translated, then
;;                                           formatted with ARGS, if any
;;   (lispm-desktop-tr-n SINGULAR PLURAL COUNT ARGS...)
;;                                           the form for COUNT, formatted
;;                                           with ARGS, or with COUNT
;;   (lispm-desktop-tr-in CONTEXT MESSAGE ARGS...)
;;                                           MESSAGE as meant in CONTEXT,
;;                                           where the same English says two
;;                                           things
;;
;; Words kept in a list made as a file loads, as menus and panels are,
;; stay English there, each marked with `lispm-desktop-tr-noop', and are
;; translated where shown, so that a change of language shows at once;
;; `(lispm-desktop-tr-noop MESSAGE CONTEXT)' keeps its context with it.
;; A translation may take the arguments in another order with `%1$s'.
;;
;; The engine is LispM Kit's, lispm-kit-i18n.el, which every LispM
;; program shares: the desktop keeps only its words, as the domain
;; `lispm-desktop', and their translations, the catalogs of i18n/, one
;; per language, named by its tag, as zh-Hans.eld (see lispm-kit-i18n.el
;; for what a catalog holds).  The English catalogs hold only what their
;; spelling changes: the code's English spells as Britain does, its
;; words as Mac OS 9 had them.  Every file of the desktop requires this
;; one first, so a first start finds LispM Kit here, or installs it (see
;; lispm-desktop-deps.el), before anything shows a word.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'cl-lib)

(declare-function lispm-desktop-deps-ensure "lispm-desktop-deps" (feature))

(unless (locate-library "lispm-kit-i18n")
  (require 'lispm-desktop-deps)
  (lispm-desktop-deps-ensure 'lispm-kit))
(require 'lispm-kit-i18n)

(defalias 'lispm-desktop-tr-noop #'lispm-kit-tr-noop
  "Return MESSAGE as it is, marking it as words to translate where shown.
For words kept as a file loads, see lispm-desktop-i18n.el.  MESSAGE as
meant in CONTEXT carries it, and `lispm-desktop-tr' translates it so.

\(fn MESSAGE &optional CONTEXT)")

(defconst lispm-desktop-languages lispm-kit-languages
  "The languages LispM speaks, in the Language panel's order:
\(TAG ENGLISH-NAME).  TAG names the catalog of i18n/.")

(defcustom lispm-desktop-language nil
  "The language the desktop speaks: a tag of `lispm-desktop-languages',
or nil for the computer's own, see `lispm-desktop-system-language'.
Change it in Control Panels' Language panel, with
`lispm-desktop-set-language', or by setting it in your personal.el: once
Emacs is set up, a new value shows at once."
  :type `(choice (const :tag "The computer's language" nil)
                 ,@(mapcar (pcase-lambda (`(,tag ,name)) `(const :tag ,name ,tag))
                           lispm-desktop-languages))
  :group 'lispm-desktop)

(defconst lispm-desktop-i18n-directory
  (expand-file-name "../i18n" (file-name-directory (or load-file-name buffer-file-name)))
  "The directory of the catalogs, one per language: i18n/ of the
desktop's checkout.  This file requires nothing of the desktop's but its
way to LispM Kit, so that every other can require it,
lispm-desktop-core.el too.")

(defvar lispm-desktop-language-changed-hook nil
  "Hook run once the desktop speaks another language, to draw again what
shows words.")

;; Followed only once Emacs is set up, see `lispm-desktop-setup-i18n'.
(lispm-kit-i18n-define 'lispm-desktop
  :directory lispm-desktop-i18n-directory
  :language 'lispm-desktop-language
  :changed-hook 'lispm-desktop-language-changed-hook
  :watch nil)

(defalias 'lispm-desktop-system-language #'lispm-kit-system-language)

(defalias 'lispm-desktop-i18n-language-of #'lispm-kit-i18n-language-of)

(defalias 'lispm-desktop-i18n-forget #'lispm-kit-i18n-forget)

(defun lispm-desktop-current-language ()
  "Return the language the desktop speaks now, a tag."
  (lispm-kit-current-language 'lispm-desktop))

;;;; Catalogs

(defun lispm-desktop-i18n-catalog-file (language)
  "Return the file of LANGUAGE's catalog."
  (lispm-kit-i18n-catalog-file 'lispm-desktop language))

(defun lispm-desktop-i18n-read-catalog (language)
  "Read LANGUAGE's catalog from its file; see `lispm-kit-i18n-read-catalog'."
  (lispm-kit-i18n-read-catalog 'lispm-desktop language))

(defun lispm-desktop-i18n-catalog (&optional language)
  "Return LANGUAGE's catalog, by default the current language's, read
once."
  (lispm-kit-i18n-catalog 'lispm-desktop language))

(defun lispm-desktop-language-name (language)
  "Return the name of LANGUAGE in itself, as its catalog gives it."
  (lispm-kit-language-name 'lispm-desktop language))

;;;; Translating

(defun lispm-desktop-tr-in (context message &rest args)
  "Return MESSAGE, as meant in CONTEXT, a string or nil, in the current
language, formatted with ARGS when there are any."
  (apply #'lispm-kit-tr-in 'lispm-desktop context message args))

(defun lispm-desktop-tr (message &rest args)
  "Return MESSAGE in the current language, formatted with ARGS when there
are any.  MESSAGE is the English written in the code; see
lispm-desktop-i18n.el.  A MESSAGE marked with a context by
`lispm-desktop-tr-noop' is translated as meant in it."
  (apply #'lispm-kit-tr 'lispm-desktop message args))

(defun lispm-desktop-tr-n (singular plural count &rest args)
  "Return the form of SINGULAR or PLURAL for COUNT in the current language,
formatted with ARGS, or with COUNT when there are none."
  (apply #'lispm-kit-tr-n 'lispm-desktop singular plural count args))

(defun lispm-desktop-format-time (format &optional time)
  "Return TIME, by default now, as FORMAT, translated, says it, the names
of months and days in the current language; see `format-time-string'."
  (lispm-kit-format-time 'lispm-desktop format time))

;;;; Chinese and Japanese type

(defconst lispm-ui-text-cjk-families
  '((ja "Hiragino Sans" "Hiragino Kaku Gothic ProN" "Noto Sans CJK JP")
    (zh-Hant "PingFang TC" "Noto Sans CJK TC")
    (zh-Hans "PingFang SC" "Noto Sans CJK SC"))
  "The proportional families the chrome draws Chinese and Japanese in,
by language: (TAG FAMILY...), the first installed taken.  Another
language draws them as Simplified Chinese does.  The fonts the chrome's
text is set in have no ideographs, and Emacs's own fallback for them is
the terminals' monospace family, its ideographs a cell apart.")

(defun lispm-ui-text-cjk-family ()
  "Return the family the chrome draws Chinese and Japanese in, for the
language spoken, or nil when none is installed."
  (let ((installed (font-family-list)))
    (seq-find (lambda (family) (member family installed))
              (or (alist-get (lispm-desktop-current-language) lispm-ui-text-cjk-families)
                  (alist-get 'zh-Hans lispm-ui-text-cjk-families)))))

;;;; Breaking lines

(defun lispm-ui-text-words (text)
  "Return TEXT in the pieces a line may break between.
A piece is a word, after the blank before it if any, or a character of a
script written without blanks, as Chinese and Japanese; punctuation that
may not start a line stays on the piece before it, and punctuation that
may not end one on the piece after it.  Concatenated, the pieces are TEXT
with its blanks made single spaces; a line starting at a piece drops its
blank, see `lispm-ui-text-wrap'.  The pieces keep TEXT's
properties, a blank those of the last blank it stands for."
  (require 'kinsoku)
  (let ((pieces nil) (start nil) (end nil) (lead nil)
        (blank nil) (joined nil) (breakable nil))
    (cl-flet ((piece ()
                (concat (if lead (apply #'propertize " " (text-properties-at (1- start) text)) "")
                        (substring text start end))))
      (dotimes (index (length text))
        (let ((char (aref text index)))
          (if (memq char '(?\s ?\t ?\n ?\r))
              (setq blank t)
            (let* ((categories (char-category-set char))
                   (here (aref categories ?|)))
              (unless (and start (not blank)
                           (or joined (aref categories ?>) (not (or here breakable))))
                (when start (push (piece) pieces))
                (setq lead (and blank start t)
                      start index))
              (setq end (1+ index)
                    joined (aref categories ?<) breakable here blank nil)))))
      (nreverse (if start (cons (piece) pieces) pieces)))))

(defun lispm-ui-text-wrap (text fits)
  "Return TEXT broken into lines, each as long as FITS allows.
FITS is called with a line and returns non-nil when it is short enough;
lines break between the pieces of `lispm-ui-text-words', and a
piece too long for a line stands alone."
  (let ((lines nil) (line nil))
    (dolist (piece (lispm-ui-text-words text))
      (let ((longer (if line (concat line piece) (string-trim-left piece))))
        (if (or (null line) (funcall fits longer))
            (setq line longer)
          (push line lines)
          (setq line (string-trim-left piece)))))
    (nreverse (if line (cons line lines) lines))))

;;;; Changing the language

(defun lispm-desktop-set-language (language)
  "Speak LANGUAGE now and at every later start: a tag of
`lispm-desktop-languages', or nil for the computer's language.
Interactively, it is asked for."
  (interactive
   (list (let* ((choices (cons (cons (lispm-desktop-tr "The computer's language") nil)
                               (mapcar (pcase-lambda (`(,tag . ,_))
                                         (cons (lispm-desktop-language-name tag) tag))
                                       lispm-desktop-languages)))
                (choice (completing-read (lispm-desktop-tr "Language: ") choices nil t)))
           (cdr (assoc choice choices)))))
  (unless (or (null language) (assq language lispm-desktop-languages))
    (user-error (lispm-desktop-tr "LispM does not speak %s" language)))
  (lispm-kit-i18n-set 'lispm-desktop language t))

(defun lispm-desktop-setup-i18n ()
  "Follow `lispm-desktop-language' as it is set, once Emacs is set up."
  (lispm-kit-i18n-watch 'lispm-desktop))

(provide 'lispm-desktop-i18n)
;;; lispm-desktop-i18n.el ends here
