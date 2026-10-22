;;; lispm-desktop-markdown.el --- Markdown, read for the desktop to show  -*- lexical-binding: t; -*-

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

;; Markdown as the desktop shows it, as the App Store shows an item's
;; introduction: read into blocks, their words into strings in faces,
;; for whoever shows them to lay out, in images or in text.  It needs
;; nothing of the desktop's and runs in `emacs -Q --batch', as
;; lispm-desktop-catalog.el checks a catalog's introductions with it.
;;
;; It reads CommonMark's core, with GitHub's strike-through:
;;
;;   # to ######   headings, and lines underlined with === or ---
;;   paragraphs    lines joined; two spaces or \ at a line's end break it
;;   - + * 1.      lists, nested by indenting, numbered from the first
;;   >             quotes, holding any of these
;;   ``` ~~~       code, fenced, its language after the fence; and lines
;;                 indented four spaces
;;   --- *** ___   rules
;;   ![alt](file)  an image, alone in its paragraph
;;
;; and in a line: **strong** and __strong__, *emphasis* and _emphasis_,
;; `code`, [links](https://example.org "title"), <https://autolinks>,
;; ~~struck~~, backslash escapes, and the entities &amp; &lt; &gt; &quot;
;; &#39; &nbsp; and &#N;.  HTML is shown as written; tables and links by
;; reference are not read, their lines shown as a paragraph.
;;
;; `lispm-desktop-markdown-parse' returns a list of blocks:
;;
;;   (heading LEVEL TEXT)   (paragraph TEXT)    (code LANGUAGE TEXT)
;;   (blockquote BLOCKS)    (list START ITEMS)  (rule)
;;   (image ALT SOURCE)
;;
;; TEXT is a string in the faces below, a link's words carrying its URL
;; as the text property `lispm-desktop-markdown-link', a line broken
;; within a paragraph by a newline; LANGUAGE is a string or nil; START
;; is the number a numbered list starts at, nil for a list of bullets;
;; ITEMS are the items' BLOCKS.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defgroup lispm-desktop-markdown nil
  "Markdown, as the desktop shows it."
  :group 'lispm-desktop
  :prefix "lispm-desktop-markdown-")

(defface lispm-desktop-markdown-heading-1 '((t :weight bold :height 1.5))
  "Face of a heading of Markdown's first level, # or ===."
  :group 'lispm-desktop-markdown)

(defface lispm-desktop-markdown-heading-2 '((t :weight bold :height 1.25))
  "Face of a heading of Markdown's second level, ## or ---."
  :group 'lispm-desktop-markdown)

(defface lispm-desktop-markdown-heading-3 '((t :weight bold))
  "Face of a heading of Markdown's third level and those under it."
  :group 'lispm-desktop-markdown)

(defface lispm-desktop-markdown-strong '((t :weight bold))
  "Face of Markdown's **strong** words."
  :group 'lispm-desktop-markdown)

(defface lispm-desktop-markdown-emphasis '((t :slant italic))
  "Face of Markdown's *emphasised* words."
  :group 'lispm-desktop-markdown)

(defface lispm-desktop-markdown-code '((t :inherit fixed-pitch))
  "Face of Markdown's `code`, in a line or a block."
  :group 'lispm-desktop-markdown)

(defface lispm-desktop-markdown-link '((t :inherit link))
  "Face of a link's words."
  :group 'lispm-desktop-markdown)

(defface lispm-desktop-markdown-strike '((t :strike-through t))
  "Face of Markdown's ~~struck~~ words."
  :group 'lispm-desktop-markdown)

(defun lispm-desktop-markdown-heading-face (level)
  "Return the face of a heading of LEVEL."
  (pcase level
    (1 'lispm-desktop-markdown-heading-1)
    (2 'lispm-desktop-markdown-heading-2)
    (_ 'lispm-desktop-markdown-heading-3)))

;;;; Lines

(defconst lispm-desktop-markdown--blank "\\`[ \t]*\\'"
  "A blank line.")

(defconst lispm-desktop-markdown--fence "\\`\\( \\{0,3\\}\\)\\(`\\{3,\\}\\|~\\{3,\\}\\)[ \t]*\\([^`]*\\)\\'"
  "A line opening fenced code: its indent, its fence, its information.")

(defconst lispm-desktop-markdown--heading
  "\\` \\{0,3\\}\\(#\\{1,6\\}\\)\\(?:[ \t]+\\(.*?\\)\\)?\\(?:[ \t]+#+\\)?[ \t]*\\'"
  "A heading's line, # to ######: its hashes, its words.")

(defconst lispm-desktop-markdown--rule
  "\\` \\{0,3\\}\\(?:\\(?:\\*[ \t]*\\)\\{3,\\}\\|\\(?:-[ \t]*\\)\\{3,\\}\\|\\(?:_[ \t]*\\)\\{3,\\}\\)\\'"
  "A rule's line.")

(defconst lispm-desktop-markdown--underline "\\` \\{0,3\\}\\(=+\\|-+\\)[ \t]*\\'"
  "A line underlining a heading: === for the first level, --- the second.")

(defconst lispm-desktop-markdown--quote "\\` \\{0,3\\}> ?\\(.*\\)\\'"
  "A line of a quote: what it quotes.")

(defconst lispm-desktop-markdown--item
  "\\`\\( \\{0,3\\}\\)\\([-+*]\\|\\([0-9]\\{1,9\\}\\)[.)]\\)\\(?:\\([ \t]+\\)\\(.*\\)\\)?\\'"
  "The first line of a list's item: its indent, its marker, the number a
numbered marker has, the blanks after it and its words.")

(defconst lispm-desktop-markdown--indented "\\`    "
  "A line of code indented four spaces.")

(defun lispm-desktop-markdown--detab (line)
  "Return LINE with its tabs as the spaces to the next stop of four."
  (if (not (string-search "\t" line))
      line
    (let ((column 0) (chars nil))
      (dolist (char (string-to-list line))
        (if (eq char ?\t)
            (let ((spaces (- 4 (% column 4))))
              (dotimes (_ spaces) (push ?\s chars))
              (setq column (+ column spaces)))
          (push char chars)
          (setq column (1+ column))))
      (concat (nreverse chars)))))

(defun lispm-desktop-markdown--blank-p (line)
  "Return non-nil when LINE is blank."
  (string-match-p lispm-desktop-markdown--blank line))

(defun lispm-desktop-markdown--indentation (line)
  "Return how many spaces LINE starts with."
  (- (length line) (length (string-trim-left line " +"))))

(defun lispm-desktop-markdown--unindent (line count)
  "Return LINE without as many as COUNT spaces it starts with."
  (substring line (min count (lispm-desktop-markdown--indentation line))))

(defun lispm-desktop-markdown--item-p (line)
  "Return non-nil when LINE starts an item of a list, a rule aside."
  (and (not (string-match-p lispm-desktop-markdown--rule line))
       (string-match-p lispm-desktop-markdown--item line)))

(defun lispm-desktop-markdown--marker (line)
  "Return what kind of list LINE's item belongs to: its bullet, or the
character after its number, or nil when LINE starts no item."
  (when (and (lispm-desktop-markdown--item-p line)
             (string-match lispm-desktop-markdown--item line))
    (let ((marker (match-string 2 line)))
      (if (match-string 3 line) (substring marker -1) marker))))

(defun lispm-desktop-markdown--interrupts-p (line)
  "Return non-nil when LINE starts a block that ends a paragraph: a fence,
a heading, a rule, a quote, or an item of a list, a numbered one only
from 1, as CommonMark has it."
  (or (string-match-p lispm-desktop-markdown--fence line)
      (string-match-p lispm-desktop-markdown--heading line)
      (string-match-p lispm-desktop-markdown--rule line)
      (string-match-p lispm-desktop-markdown--quote line)
      (and (lispm-desktop-markdown--item-p line)
           (string-match lispm-desktop-markdown--item line)
           (match-string 5 line)
           (not (string-blank-p (match-string 5 line)))
           (member (or (match-string 3 line) "1") '("1")))))

(defun lispm-desktop-markdown--closes-p (line fence)
  "Return non-nil when LINE closes the code FENCE opened."
  (and (string-match "\\` \\{0,3\\}\\([`~]+\\)[ \t]*\\'" line)
       (eq (aref (match-string 1 line) 0) (aref fence 0))
       (>= (length (match-string 1 line)) (length fence))))

;;;; Blocks

(defun lispm-desktop-markdown-parse (text)
  "Return the blocks of TEXT, Markdown, as lispm-desktop-markdown.el
describes them."
  (lispm-desktop-markdown--blocks
   (mapcar #'lispm-desktop-markdown--detab
           (split-string (string-replace "\r" "" text) "\n"))))

(defun lispm-desktop-markdown--paragraph-text (lines)
  "Return the words of the paragraph of LINES, joined: a line ending in
two spaces or a backslash breaks it with a newline."
  (let ((parts nil))
    (while lines
      (let ((line (string-trim-left (pop lines))))
        (cond ((null lines) (push (string-trim-right line) parts))
              ((string-match "\\(?:\\\\\\| \\{2,\\}\\)\\'" line)
               (push (concat (string-trim-right (substring line 0 (match-beginning 0))) "\n") parts))
              (t (push (concat (string-trim-right line) " ") parts)))))
    (apply #'concat (nreverse parts))))

(defun lispm-desktop-markdown--paragraph (lines)
  "Return the block of the paragraph of LINES: an image, alone in it, or
its words."
  (let ((text (lispm-desktop-markdown--paragraph-text lines)))
    (if (string-match "\\`!\\[\\([^]]*\\)\\](<?\\([^ <>()]+\\)>?\\(?:[ \t]+\"[^\"]*\"\\)?)\\'" text)
        (list 'image (match-string 1 text) (match-string 2 text))
      (list 'paragraph (lispm-desktop-markdown-inline text)))))

(defun lispm-desktop-markdown--blocks (lines)
  "Return the blocks of LINES, Markdown's lines, its tabs spaces already."
  (let ((lines (vconcat lines))
        (i 0)
        (blocks nil))
    (cl-flet ((line () (aref lines i))
              (more-p () (< i (length lines))))
      (while (more-p)
        (let ((line (line)))
          (cond
           ((lispm-desktop-markdown--blank-p line)
            (cl-incf i))
           ((string-match lispm-desktop-markdown--fence line)
            (let ((indent (length (match-string 1 line)))
                  (fence (match-string 2 line))
                  (info (string-trim (match-string 3 line)))
                  (code nil))
              (cl-incf i)
              (while (and (more-p) (not (lispm-desktop-markdown--closes-p (line) fence)))
                (push (lispm-desktop-markdown--unindent (line) indent) code)
                (cl-incf i))
              (cl-incf i)
              (push (list 'code (car (split-string info)) (string-join (nreverse code) "\n")) blocks)))
           ((string-match lispm-desktop-markdown--heading line)
            (push (list 'heading (length (match-string 1 line))
                        (lispm-desktop-markdown-inline (string-trim (or (match-string 2 line) ""))))
                  blocks)
            (cl-incf i))
           ((string-match-p lispm-desktop-markdown--rule line)
            (push (list 'rule) blocks)
            (cl-incf i))
           ((string-match-p lispm-desktop-markdown--quote line)
            (let ((quoted nil))
              ;; A paragraph's line goes on the quote lazily.
              (while (and (more-p)
                          (or (string-match-p lispm-desktop-markdown--quote (line))
                              (and quoted (not (lispm-desktop-markdown--blank-p (car quoted)))
                                   (not (lispm-desktop-markdown--blank-p (line)))
                                   (not (lispm-desktop-markdown--interrupts-p (line))))))
                (push (if (string-match lispm-desktop-markdown--quote (line))
                          (match-string 1 (line))
                        (line))
                      quoted)
                (cl-incf i))
              (push (list 'blockquote (lispm-desktop-markdown--blocks (nreverse quoted))) blocks)))
           ((lispm-desktop-markdown--item-p line)
            (let ((kind (lispm-desktop-markdown--marker line))
                  (start (and (string-match lispm-desktop-markdown--item line)
                              (match-string 3 line)
                              (string-to-number (match-string 3 line))))
                  (items nil))
              (while (and (more-p) (equal (lispm-desktop-markdown--marker (line)) kind)
                          (< (lispm-desktop-markdown--indentation (line)) 4))
                (string-match lispm-desktop-markdown--item (line))
                (let* ((indent (length (match-string 1 (line))))
                       (marker (match-string 2 (line)))
                       (blanks (match-string 4 (line)))
                       (words (or (match-string 5 (line)) ""))
                       ;; Past four blanks, the item's words are code.
                       (wide (and blanks (> (length blanks) 4)))
                       (width (+ indent (length marker) (if (or wide (null blanks)) 1 (length blanks))))
                       (own (list (if wide (concat (make-string (1- (length blanks)) ?\s) words) words))))
                  (cl-incf i)
                  (catch 'done
                    (while (more-p)
                      (let ((next (line)))
                        (cond
                         ((lispm-desktop-markdown--blank-p next)
                          ;; A blank line: the item goes on if what follows is indented.
                          (let ((j (1+ i)))
                            (while (and (< j (length lines)) (lispm-desktop-markdown--blank-p (aref lines j)))
                              (cl-incf j))
                            (if (and (< j (length lines))
                                     (>= (lispm-desktop-markdown--indentation (aref lines j)) width))
                                (progn (push "" own) (cl-incf i))
                              (throw 'done nil))))
                         ((>= (lispm-desktop-markdown--indentation next) width)
                          (push (substring next width) own)
                          (cl-incf i))
                         ;; A paragraph's line goes on the item lazily.
                         ((and (not (lispm-desktop-markdown--blank-p (car own)))
                               (not (lispm-desktop-markdown--interrupts-p next))
                               (not (lispm-desktop-markdown--item-p next)))
                          (push (string-trim-left next) own)
                          (cl-incf i))
                         (t (throw 'done nil))))))
                  (push (lispm-desktop-markdown--blocks (nreverse own)) items))
                ;; Blank lines between two items of the list.
                (let ((j i))
                  (while (and (< j (length lines)) (lispm-desktop-markdown--blank-p (aref lines j)))
                    (cl-incf j))
                  (when (and (< j (length lines)) (equal (lispm-desktop-markdown--marker (aref lines j)) kind))
                    (setq i j))))
              (push (list 'list start (nreverse items)) blocks)))
           ((string-match-p lispm-desktop-markdown--indented line)
            (let ((code nil))
              (while (and (more-p) (or (string-match-p lispm-desktop-markdown--indented (line))
                                       (lispm-desktop-markdown--blank-p (line))))
                (push (lispm-desktop-markdown--unindent (line) 4) code)
                (cl-incf i))
              (while (and code (string-blank-p (car code)))
                (pop code))
              (push (list 'code nil (string-join (nreverse code) "\n")) blocks)))
           (t
            (let ((own (list line))
                  (level nil))
              (cl-incf i)
              (catch 'done
                (while (more-p)
                  (let ((next (line)))
                    (cond ((lispm-desktop-markdown--blank-p next) (throw 'done nil))
                          ((string-match lispm-desktop-markdown--underline next)
                           (setq level (if (eq (aref (match-string 1 next) 0) ?=) 1 2))
                           (cl-incf i)
                           (throw 'done nil))
                          ((lispm-desktop-markdown--interrupts-p next) (throw 'done nil))
                          (t (push next own) (cl-incf i))))))
              (push (if level
                        (list 'heading level (lispm-desktop-markdown-inline
                                              (string-trim (lispm-desktop-markdown--paragraph-text (nreverse own)))))
                      (lispm-desktop-markdown--paragraph (nreverse own)))
                    blocks)))))))
    (nreverse blocks)))

;;;; Words in a line

(defconst lispm-desktop-markdown--punctuation (string-to-list "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")
  "The characters a backslash escapes.")

(defconst lispm-desktop-markdown--entities
  '(("amp" . "&") ("lt" . "<") ("gt" . ">") ("quot" . "\"") ("apos" . "'") ("nbsp" . " "))
  "The named entities read, and what they stand for.")

(defun lispm-desktop-markdown--entities (text)
  "Return TEXT with its entities as what they stand for."
  (replace-regexp-in-string
   "&\\(?:\\([a-z]+\\)\\|#\\([0-9]\\{1,7\\}\\)\\|#[xX]\\([0-9a-fA-F]\\{1,6\\}\\)\\);"
   (lambda (entity)
     (let ((code (cond ((match-string 2 entity) (string-to-number (match-string 2 entity)))
                       ((match-string 3 entity) (string-to-number (match-string 3 entity) 16)))))
       (cond ((match-string 1 entity)
              (or (cdr (assoc (match-string 1 entity) lispm-desktop-markdown--entities)) entity))
             ((and code (< 0 code #x110000) (not (<= #xD800 code #xDFFF))) (string code))
             (t entity))))
   text t t))

(defun lispm-desktop-markdown--with-face (text face)
  "Return TEXT with FACE under the faces it has."
  (let ((text (copy-sequence text)))
    (add-face-text-property 0 (length text) face t text)
    text))

(defun lispm-desktop-markdown--run (text at char)
  "Return how many CHARs TEXT has in a row from AT."
  (let ((end at))
    (while (and (< end (length text)) (eq (aref text end) char))
      (cl-incf end))
    (- end at)))

(defun lispm-desktop-markdown--code-end (text at length)
  "Return where the run of exactly LENGTH backquotes closing a code span
starts in TEXT, searching from AT, or nil."
  (let ((found nil))
    (while (and (not found) (< at (length text)))
      (if (eq (aref text at) ?`)
          (let ((run (lispm-desktop-markdown--run text at ?`)))
            (if (= run length) (setq found at) (setq at (+ at run))))
        (cl-incf at)))
    found))

(defun lispm-desktop-markdown--bracket-end (text at)
  "Return where the ] closing the [ at AT of TEXT is, or nil: brackets
nest, a backslash escapes one, and code spans hold none."
  (let ((depth 0) (i at) (found nil))
    (while (and (not found) (< i (length text)))
      (let ((char (aref text i)))
        (cond ((eq char ?\\) (cl-incf i))
              ((eq char ?`)
               (let* ((run (lispm-desktop-markdown--run text i ?`))
                      (end (lispm-desktop-markdown--code-end text (+ i run) run)))
                 (setq i (if end (+ end run -1) (+ i run -1)))))
              ((eq char ?\[) (cl-incf depth))
              ((eq char ?\]) (cl-decf depth) (when (= depth 0) (setq found i)))))
      (cl-incf i))
    found))

(defun lispm-desktop-markdown--destination (text at)
  "Return the destination of a link written from AT of TEXT, as
\(URL . END), END where it ends, or nil when none is written there:
\(URL), (<URL>), each with a title in quotes or brackets after it."
  (when (and (< at (length text)) (eq (aref text at) ?\())
    (let ((i (1+ at)) url)
      (cl-flet ((blanks () (while (and (< i (length text)) (memq (aref text i) '(?\s ?\n))) (cl-incf i))))
        (blanks)
        (cond ((and (< i (length text)) (eq (aref text i) ?<))
               (when-let* ((end (string-search ">" text i)))
                 (setq url (substring text (1+ i) end) i (1+ end))))
              (t (let ((start i) (depth 0))
                   (while (and (< i (length text))
                               (not (memq (aref text i) '(?\s ?\n)))
                               (not (and (eq (aref text i) ?\)) (= depth 0))))
                     (pcase (aref text i)
                       (?\( (cl-incf depth))
                       (?\) (cl-decf depth))
                       (?\\ (cl-incf i)))
                     (cl-incf i))
                   (setq url (substring text start (min i (length text)))))))
        (when url
          (blanks)
          (when (and (< i (length text)) (memq (aref text i) '(?\" ?' ?\()))
            (let ((close (if (eq (aref text i) ?\() ?\) (aref text i))))
              (when-let* ((end (string-search (string close) text (1+ i))))
                (setq i (1+ end))
                (blanks))))
          (when (and (< i (length text)) (eq (aref text i) ?\)))
            (cons (replace-regexp-in-string "\\\\\\([[:punct:]]\\)" "\\1" url) (1+ i))))))))

(defun lispm-desktop-markdown--flanking-p (text at length)
  "Return non-nil when the run of LENGTH delimiters at AT of TEXT may open
an emphasis: something not blank follows it, and an underscore does not
stand within a word."
  (let ((after (and (< (+ at length) (length text)) (aref text (+ at length))))
        (before (and (> at 0) (aref text (1- at)))))
    (and after (not (memq after '(?\s ?\n ?\t)))
         (or (not (eq (aref text at) ?_))
             (not (and before (string-match-p "[[:alnum:]]" (string before))))))))

(defun lispm-desktop-markdown--closer (text from char length)
  "Return where in TEXT, from FROM, the LENGTH CHARs closing an emphasis
opened by as many start, or nil.  A run closes when what is before it is
not blank, and an underscore's when no word goes on after it; of a run
of three or more, its last LENGTH close.  Code spans are passed over."
  (let ((i from) (found nil))
    (while (and (not found) (< i (length text)))
      (let ((at (aref text i)))
        (cond
         ((eq at ?\\) (setq i (+ i 2)))
         ((eq at ?`)
          (let* ((run (lispm-desktop-markdown--run text i ?`))
                 (end (lispm-desktop-markdown--code-end text (+ i run) run)))
            (setq i (if end (+ end run) (+ i run)))))
         ((eq at char)
          (let* ((run (lispm-desktop-markdown--run text i char))
                 (before (aref text (1- i)))
                 (after (and (< (+ i run) (length text)) (aref text (+ i run)))))
            ;; A run of another length belongs to an emphasis of its own.
            (if (and (or (= run length) (>= run 3)) (> i from)
                     (not (memq before '(?\s ?\n ?\t)))
                     (or (not (eq char ?_)) (not (and after (string-match-p "[[:alnum:]]" (string after))))))
                (setq found (+ i (- run length)))
              (setq i (+ i run)))))
         (t (cl-incf i)))))
    found))

(defun lispm-desktop-markdown--link (text url)
  "Return TEXT as a link's words, to URL."
  (let ((text (lispm-desktop-markdown--with-face text 'lispm-desktop-markdown-link)))
    (add-text-properties 0 (length text) (list 'lispm-desktop-markdown-link url 'help-echo url) text)
    text))

(defun lispm-desktop-markdown-inline (text)
  "Return TEXT, the words of a paragraph or a heading in Markdown, as a
string in faces: strong, emphasised, struck, code and links, a link's
words carrying its URL as `lispm-desktop-markdown-link'.  An image
written in a line shows its words, emphasised."
  (let ((out nil) (i 0) (start 0) (n (length text)))
    (cl-flet ((flush (to)
                (when (< start to)
                  (push (lispm-desktop-markdown--entities (substring text start to)) out)))
              (emit (string) (push string out)))
      (while (< i n)
        (let ((char (aref text i)))
          (cond
           ((and (eq char ?\\) (< (1+ i) n) (memq (aref text (1+ i)) lispm-desktop-markdown--punctuation))
            (flush i)
            (emit (string (aref text (1+ i))))
            (setq i (+ i 2) start i))
           ((eq char ?`)
            (let* ((run (lispm-desktop-markdown--run text i ?`))
                   (end (lispm-desktop-markdown--code-end text (+ i run) run)))
              (if (not end)
                  (setq i (+ i run))
                (let ((code (string-replace "\n" " " (substring text (+ i run) end))))
                  (when (and (string-match-p "\\` .*[^ ].* \\'" code))
                    (setq code (substring code 1 -1)))
                  (flush i)
                  (emit (propertize code 'face 'lispm-desktop-markdown-code))
                  (setq i (+ end run) start i)))))
           ((and (eq char ?<)
                 (eq (string-match "<\\([a-zA-Z][a-zA-Z0-9+.-]\\{1,31\\}:[^ <>\n]*\\|[^ <>@\n]+@[^ <>@\n]+\\.[a-zA-Z]+\\)>"
                                   text i)
                     i))
            (let ((address (match-string 1 text))
                  (end (match-end 0)))
              (flush i)
              (emit (lispm-desktop-markdown--link
                     (copy-sequence address)
                     (if (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" address) address (concat "mailto:" address))))
              (setq i end start i)))
           ((or (eq char ?\[) (and (eq char ?!) (< (1+ i) n) (eq (aref text (1+ i)) ?\[)))
            (let* ((image (eq char ?!))
                   (open (if image (1+ i) i))
                   (close (lispm-desktop-markdown--bracket-end text open))
                   (destination (and close (lispm-desktop-markdown--destination text (1+ close)))))
              (if (not destination)
                  (setq i (1+ open))
                (let ((words (lispm-desktop-markdown-inline (substring text (1+ open) close))))
                  (flush i)
                  (emit (if image
                            (lispm-desktop-markdown--with-face words 'lispm-desktop-markdown-emphasis)
                          (lispm-desktop-markdown--link words (car destination))))
                  (setq i (cdr destination) start i)))))
           ((memq char '(?* ?_ ?~))
            (let* ((run (lispm-desktop-markdown--run text i char))
                   (lengths (if (eq char ?~) (and (= run 2) '(2)) (number-sequence (min run 3) 1 -1)))
                   (found (and (lispm-desktop-markdown--flanking-p text i run)
                               (seq-some (lambda (length)
                                           (when-let* ((end (lispm-desktop-markdown--closer
                                                             text (+ i run) char length)))
                                             (cons length end)))
                                         lengths))))
              (if (not found)
                  (setq i (+ i run))
                (pcase-let* ((`(,length . ,end) found)
                             (from (+ i (- run length)))
                             (inner (lispm-desktop-markdown-inline (substring text (+ from length) end))))
                  ;; Delimiters the emphasis does not take stay as written.
                  (flush from)
                  (dolist (face (pcase (cons char length)
                                  (`(?~ . ,_) '(lispm-desktop-markdown-strike))
                                  (`(,_ . 1) '(lispm-desktop-markdown-emphasis))
                                  (`(,_ . 2) '(lispm-desktop-markdown-strong))
                                  (_ '(lispm-desktop-markdown-strong lispm-desktop-markdown-emphasis))))
                    (setq inner (lispm-desktop-markdown--with-face inner face)))
                  (emit inner)
                  (setq i (+ end length) start i)))))
           (t (cl-incf i)))))
      (flush n)
      (apply #'concat (nreverse out)))))

;;;; What blocks hold

(defun lispm-desktop-markdown-texts (blocks)
  "Return the texts BLOCKS hold, those of the blocks they hold too, in order."
  (mapcan (lambda (block)
            (pcase block
              (`(heading ,_ ,text) (list text))
              (`(paragraph ,text) (list text))
              (`(blockquote ,inner) (lispm-desktop-markdown-texts inner))
              (`(list ,_ ,items) (mapcan #'lispm-desktop-markdown-texts items))))
          blocks))

(defun lispm-desktop-markdown-links (blocks)
  "Return the links of BLOCKS, in order, as (WORDS . URL)."
  (mapcan (lambda (text)
            (let ((links nil) (at 0))
              (while (setq at (text-property-not-all at (length text) 'lispm-desktop-markdown-link nil text))
                (let ((end (next-single-property-change at 'lispm-desktop-markdown-link text (length text))))
                  (push (cons (substring-no-properties text at end)
                              (get-text-property at 'lispm-desktop-markdown-link text))
                        links)
                  (setq at end)))
              (nreverse links)))
          (lispm-desktop-markdown-texts blocks)))

(defun lispm-desktop-markdown-images (blocks)
  "Return the images of BLOCKS, in order, as (ALT SOURCE)."
  (mapcan (lambda (block)
            (pcase block
              (`(image ,alt ,source) (list (list alt source)))
              (`(blockquote ,inner) (lispm-desktop-markdown-images inner))
              (`(list ,_ ,items) (mapcan #'lispm-desktop-markdown-images items))))
          blocks))

(defun lispm-desktop-markdown-headings (blocks)
  "Return the levels of the headings BLOCKS hold at their top, in order."
  (mapcan (lambda (block) (and (eq (car block) 'heading) (list (nth 1 block)))) blocks))

(provide 'lispm-desktop-markdown)
;;; lispm-desktop-markdown.el ends here
