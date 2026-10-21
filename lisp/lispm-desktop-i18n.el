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
;; The translations are the catalogs of i18n/, one per language, named by
;; its tag, as zh-Hans.eld:
;;
;;   (:name "NAME" :messages ((MESSAGE . TRANSLATION) ...))
;;
;; NAME is the language's name in itself.  A MESSAGE said in a context is
;; (CONTEXT . MESSAGE); a plural's TRANSLATION is a list of its forms, as
;; many as the language has, see `lispm-desktop-i18n-plural-forms'.  A
;; message missing from the catalog shows as written.  The English
;; catalogs hold only what their spelling changes: the code's English
;; spells as Britain does, its words as Mac OS 9 had them.

;;; Code:

(require 'seq)
(require 'subr-x)

(defun lispm-desktop-tr-noop (message &optional context)
  "Return MESSAGE as it is, marking it as words to translate where shown.
For words kept as a file loads, see lispm-desktop-i18n.el.  MESSAGE as
meant in CONTEXT carries it, and `lispm-desktop-tr' translates it so."
  (if context (propertize message 'lispm-desktop-i18n-context context) message))

(defconst lispm-desktop-languages
  `((en-US ,(lispm-desktop-tr-noop "English (US)")) (en-GB ,(lispm-desktop-tr-noop "English (UK)"))
    (de ,(lispm-desktop-tr-noop "German")) (fr ,(lispm-desktop-tr-noop "French"))
    (ja ,(lispm-desktop-tr-noop "Japanese"))
    (zh-Hans ,(lispm-desktop-tr-noop "Chinese, Simplified"))
    (zh-Hant ,(lispm-desktop-tr-noop "Chinese, Traditional")))
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
desktop's checkout.  This file requires nothing of the desktop's, so that
every other can require it, lispm-desktop-core.el too.")

(defconst lispm-desktop-i18n-plural-forms
  '((de . lispm-desktop-i18n--one-else-other)
    (en-GB . lispm-desktop-i18n--one-else-other)
    (en-US . lispm-desktop-i18n--one-else-other)
    (fr . lispm-desktop-i18n--up-to-one-else-other))
  "How each language picks the plural form of a count: (TAG . FUNCTION),
FUNCTION returning the index of the form for a count.  A language not
listed has one form for every count, as Japanese and Chinese.")

(defun lispm-desktop-i18n--one-else-other (count)
  "Return the plural form for COUNT where one is singular: 0 or 1."
  (if (= count 1) 0 1))

(defun lispm-desktop-i18n--up-to-one-else-other (count)
  "Return the plural form for COUNT where 0 and 1 are singular, as in
French: 0 or 1."
  (if (< (abs count) 2) 0 1))

;;;; The computer's language

(defvar lispm-desktop-i18n--system-languages 'unread
  "The computer's preferred languages, most preferred first, once read.")

(defvar lispm-desktop-i18n--system-language '(unread)
  "The computer's language once found: (LANGUAGES . LANGUAGE), LANGUAGE
the one LispM speaks of the preferred LANGUAGES it was found among.")

(defun lispm-desktop-i18n--read-system-languages ()
  "Return the computer's preferred languages, most preferred first, as
language tags or locale names: macOS's AppleLanguages, else the
LANGUAGE, LC_ALL, LC_MESSAGES and LANG variables."
  (or (when (eq system-type 'darwin)
        (with-temp-buffer
          (when (eq 0 (ignore-errors (call-process "defaults" nil t nil
                                                   "read" "-g" "AppleLanguages")))
            (goto-char (point-min))
            (let ((tags nil))
              (while (re-search-forward "\"?\\([A-Za-z]+\\(?:[-_][A-Za-z0-9]+\\)*\\)\"?[,\n]" nil t)
                (push (match-string 1) tags))
              (nreverse tags)))))
      (seq-mapcat (lambda (variable)
                    (when-let* ((value (getenv variable)))
                      (split-string value ":" t)))
                  '("LANGUAGE" "LC_ALL" "LC_MESSAGES" "LANG"))))

(defun lispm-desktop-i18n-language-of (tag)
  "Return the language of `lispm-desktop-languages' TAG asks for, or nil.
TAG is a language tag, as zh-Hant-HK, or a locale, as zh_TW.UTF-8.
Chinese without a script is Simplified, except as written in Hong Kong,
Macao and Taiwan; English is American in the United States and without
a region, else British."
  (save-match-data
    (lispm-desktop-i18n--language-of tag)))

(defun lispm-desktop-i18n--language-of (tag)
  "Return the language of `lispm-desktop-languages' TAG asks for, or nil,
the match data changed; see `lispm-desktop-i18n-language-of'."
  (let* ((parts (split-string (car (split-string tag "[.@]")) "[-_]" t))
         (language (downcase (or (car parts) "")))
         (rest (mapcar (lambda (part) (if (= (length part) 4) (capitalize part) (upcase part)))
                       (cdr parts)))
         (script (seq-find (lambda (part) (= (length part) 4)) rest))
         (region (seq-find (lambda (part) (/= (length part) 4)) rest)))
    (pcase language
      ("zh" (cond ((equal script "Hant") 'zh-Hant)
                  ((equal script "Hans") 'zh-Hans)
                  ((member region '("HK" "MO" "TW")) 'zh-Hant)
                  (t 'zh-Hans)))
      ("en" (if (member region '(nil "US")) 'en-US 'en-GB))
      ("ja" 'ja)
      ("fr" 'fr)
      ("de" 'de))))

(defun lispm-desktop-system-language ()
  "Return the computer's language, the first of its preferred languages
that LispM speaks, else American English.  It is found once, as every
word shown asks for it while no language is chosen; the caller's match
data is kept."
  (when (eq lispm-desktop-i18n--system-languages 'unread)
    (setq lispm-desktop-i18n--system-languages
          (save-match-data (lispm-desktop-i18n--read-system-languages))))
  (let ((languages lispm-desktop-i18n--system-languages))
    (unless (eq (car lispm-desktop-i18n--system-language) languages)
      (setq lispm-desktop-i18n--system-language
            (cons languages (or (seq-some #'lispm-desktop-i18n-language-of languages) 'en-US)))))
  (cdr lispm-desktop-i18n--system-language))

(defun lispm-desktop-current-language ()
  "Return the language the desktop speaks now, a tag."
  (or lispm-desktop-language (lispm-desktop-system-language)))

;;;; Catalogs

(defvar lispm-desktop-i18n--catalogs nil
  "The catalogs read so far: (TAG . CATALOG), CATALOG the file's plist
with its :messages in a hash table.")

(defun lispm-desktop-i18n-catalog-file (language)
  "Return the file of LANGUAGE's catalog."
  (expand-file-name (format "%s.eld" language) lispm-desktop-i18n-directory))

(defun lispm-desktop-i18n--key (message context)
  "Return the key MESSAGE in CONTEXT has in a catalog's table."
  (if context (concat context "\4" message) message))

(defun lispm-desktop-i18n-read-catalog (language)
  "Read LANGUAGE's catalog from its file: its plist, with its :messages,
\(MESSAGE . TRANSLATION), in a hash table keyed by
`lispm-desktop-i18n--key'.  A language without a file has an empty one."
  (let ((file (lispm-desktop-i18n-catalog-file language))
        (table (make-hash-table :test #'equal)))
    (let ((catalog (when (file-readable-p file)
                     (with-temp-buffer
                       (insert-file-contents file)
                       (read (current-buffer))))))
      (dolist (entry (plist-get catalog :messages))
        (let ((message (car entry)))
          (puthash (if (consp message)
                       (lispm-desktop-i18n--key (cdr message) (car message))
                     message)
                   (cdr entry) table)))
      (plist-put (copy-sequence catalog) :messages table))))

(defun lispm-desktop-i18n-catalog (&optional language)
  "Return LANGUAGE's catalog, by default the current language's, read
once."
  (let ((language (or language (lispm-desktop-current-language))))
    (or (alist-get language lispm-desktop-i18n--catalogs)
        (setf (alist-get language lispm-desktop-i18n--catalogs)
              (lispm-desktop-i18n-read-catalog language)))))

(defun lispm-desktop-language-name (language)
  "Return the name of LANGUAGE in itself, as its catalog gives it."
  (or (plist-get (lispm-desktop-i18n-catalog language) :name)
      (cadr (assq language lispm-desktop-languages))
      (symbol-name language)))

(defun lispm-desktop-i18n--translation (message context)
  "Return the translation of MESSAGE in CONTEXT, or nil."
  (gethash (lispm-desktop-i18n--key message context)
           (plist-get (lispm-desktop-i18n-catalog) :messages)))

;;;; Translating

(defun lispm-desktop-tr-in (context message &rest args)
  "Return MESSAGE, as meant in CONTEXT, a string or nil, in the current
language, formatted with ARGS when there are any."
  (let ((translation (lispm-desktop-i18n--translation message context)))
    (let ((text (if (stringp translation) translation message)))
      (if args (apply #'format text args) text))))

(defun lispm-desktop-tr (message &rest args)
  "Return MESSAGE in the current language, formatted with ARGS when there
are any.  MESSAGE is the English written in the code; see
lispm-desktop-i18n.el.  A MESSAGE marked with a context by
`lispm-desktop-tr-noop' is translated as meant in it."
  (apply #'lispm-desktop-tr-in
         (and (> (length message) 0) (get-text-property 0 'lispm-desktop-i18n-context message))
         message args))

(defun lispm-desktop-tr-n (singular plural count &rest args)
  "Return the form of SINGULAR or PLURAL for COUNT in the current language,
formatted with ARGS, or with COUNT when there are none."
  (let* ((translation (lispm-desktop-i18n--translation singular nil))
         (rule (alist-get (lispm-desktop-current-language) lispm-desktop-i18n-plural-forms))
         (text (cond ((stringp translation) translation)
                     (translation (or (nth (if rule (funcall rule count) 0) translation)
                                      (car (last translation))))
                     ((= count 1) singular)
                     (t plural))))
    (apply #'format text (or args (list count)))))

(defconst lispm-desktop-i18n-locales
  '((en-US . "en_US.UTF-8") (en-GB . "en_GB.UTF-8") (de . "de_DE.UTF-8") (fr . "fr_FR.UTF-8")
    (ja . "ja_JP.UTF-8") (zh-Hans . "zh_CN.UTF-8") (zh-Hant . "zh_TW.UTF-8"))
  "The locale each language names months and days in: (TAG . LOCALE).")

(defun lispm-desktop-format-time (format &optional time)
  "Return TIME, by default now, as FORMAT, translated, says it, the names
of months and days in the current language; see `format-time-string'."
  (let ((system-time-locale (alist-get (lispm-desktop-current-language) lispm-desktop-i18n-locales)))
    (format-time-string (lispm-desktop-tr format) time)))

;;;; Chinese and Japanese type

(defconst lispm-desktop-i18n-cjk-families
  '((ja "Hiragino Sans" "Hiragino Kaku Gothic ProN" "Noto Sans CJK JP")
    (zh-Hant "PingFang TC" "Noto Sans CJK TC")
    (zh-Hans "PingFang SC" "Noto Sans CJK SC"))
  "The proportional families the chrome draws Chinese and Japanese in,
by language: (TAG FAMILY...), the first installed taken.  Another
language draws them as Simplified Chinese does.  The fonts the chrome's
text is set in have no ideographs, and Emacs's own fallback for them is
the terminals' monospace family, its ideographs a cell apart.")

(defun lispm-desktop-i18n-cjk-family ()
  "Return the family the chrome draws Chinese and Japanese in, for the
language spoken, or nil when none is installed."
  (let ((installed (font-family-list)))
    (seq-find (lambda (family) (member family installed))
              (or (alist-get (lispm-desktop-current-language) lispm-desktop-i18n-cjk-families)
                  (alist-get 'zh-Hans lispm-desktop-i18n-cjk-families)))))

;;;; Breaking lines

(defun lispm-desktop-i18n-words (text)
  "Return TEXT in the pieces a line may break between.
A piece is a word, after the blank before it if any, or a character of a
script written without blanks, as Chinese and Japanese; punctuation that
may not start a line stays on the piece before it, and punctuation that
may not end one on the piece after it.  Concatenated, the pieces are TEXT
with its blanks made single spaces; a line starting at a piece drops its
blank, see `lispm-desktop-i18n-wrap'."
  (require 'kinsoku)
  (let ((pieces nil) (piece nil) (blank nil) (joined nil) (breakable nil))
    (dolist (char (string-to-list text))
      (if (memq char '(?\s ?\t ?\n ?\r))
          (setq blank t)
        (let* ((categories (char-category-set char))
               (here (aref categories ?|)))
          (if (and piece (not blank)
                   (or joined (aref categories ?>) (not (or here breakable))))
              (setq piece (concat piece (string char)))
            (when piece (push piece pieces))
            (setq piece (concat (if (and blank piece) " " "") (string char))))
          (setq joined (aref categories ?<) breakable here blank nil))))
    (nreverse (if piece (cons piece pieces) pieces))))

(defun lispm-desktop-i18n-wrap (text fits)
  "Return TEXT broken into lines, each as long as FITS allows.
FITS is called with a line and returns non-nil when it is short enough;
lines break between the pieces of `lispm-desktop-i18n-words', and a
piece too long for a line stands alone."
  (let ((lines nil) (line nil))
    (dolist (piece (lispm-desktop-i18n-words text))
      (let ((longer (if line (concat line piece) (string-trim-left piece))))
        (if (or (null line) (funcall fits longer))
            (setq line longer)
          (push line lines)
          (setq line (string-trim-left piece)))))
    (nreverse (if line (cons line lines) lines))))

;;;; Changing the language

(defvar lispm-desktop--set-up)

(defvar lispm-desktop-language-changed-hook nil
  "Hook run once the desktop speaks another language, to draw again what
shows words.")

(defun lispm-desktop-i18n-forget ()
  "Forget the catalogs read and the computer's languages, so they are read
again when next needed."
  (setq lispm-desktop-i18n--catalogs nil
        lispm-desktop-i18n--system-languages 'unread
        lispm-desktop-i18n--system-language '(unread)))

(defun lispm-desktop-i18n--changed (_symbol language operation _where)
  "Speak LANGUAGE when `lispm-desktop-language' is set to it once Emacs is
set up, as the command setting it ends.  This is a variable watcher."
  (when (and lispm-desktop--set-up (eq operation 'set)
             (not (eq language lispm-desktop-language)))
    (run-at-time 0 nil #'run-hooks 'lispm-desktop-language-changed-hook)))

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
  (let ((from (lispm-desktop-current-language)))
    ;; Shown here, not by the watcher a second time.
    (let ((lispm-desktop--set-up nil))
      (customize-save-variable 'lispm-desktop-language language))
    (unless (eq from (lispm-desktop-current-language))
      (run-hooks 'lispm-desktop-language-changed-hook))))

(defun lispm-desktop-setup-i18n ()
  "Follow `lispm-desktop-language' as it is set, once Emacs is set up."
  (add-variable-watcher 'lispm-desktop-language #'lispm-desktop-i18n--changed))

(provide 'lispm-desktop-i18n)
;;; lispm-desktop-i18n.el ends here
