;;; lispm-desktop-app-skins.el --- The App Store's applications on the desktop  -*- lexical-binding: t; -*-

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

;; What is said of the App Store's applications that are no games, beyond
;; what lispm-desktop-app-ui.el finds out of them by itself:
;;
;;   Calculator  Calc's stack over a keypad, as Mac OS 9's Calculator
;;               had one: digits typed into its display, operations
;;               done on the stack, Return pushing the number
;;   Calendar    three months drawn as Platinum's calendars: today
;;               marked, the day point is on ringed, holidays in red;
;;               a click goes to a day, the arrows to other months
;;   Elfeed      its entries in the list face, the line point is on
;;               highlighted; a page saying how to begin while it has
;;               no feeds
;;   Rotom       the daemon, the accounts and LispM's model at the
;;               bottom; a page while Rotom's program is missing, which
;;               installs it
;;   1Password   the vault and kind shown after its buttons, the items
;;               counted at the bottom; a page while op is missing or
;;               1Password is locked
;;   Security    the findings counted at the bottom
;;   MYCIN       what to type, after its buttons
;;   SHRDLU      what to type, after its buttons; the drawing of the
;;               blocks world without buttons under it
;;   Mail        a larger window, its panes titled by what they show,
;;               its folders and its table of messages in the list face,
;;               its headers' box in a list header's platinum, its
;;               buttons over the table alone; its panes laid out
;;               again as it opens again; its settings and accounts'
;;               forms drawn as the desktop's dialogs; Check for Mail of
;;               the menu bar asking its servers
;;
;; These six declare their commands with lispm-kit, so their buttons
;; and keys are their own, see `lispm-desktop-app-ui-declared'.  Each
;; says what it shows with functions of its own, as Rotom's
;; `lispm-rotom-running-p'; none of them knows the desktop.
;;
;; EMMS has a file of its own, lispm-desktop-app-emms.el.
;;
;; What they say is in English, marked as words to translate; the
;; interface translates it where it shows it, see lispm-desktop-app-ui.el.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-app-board)
(require 'lispm-desktop-app-ui)
(require 'lispm-desktop-form)
(require 'lispm-desktop-i18n)

;; The applications' own variables and functions, read once they run.
(defvar calc-angle-mode)
(defvar calc-internal-prec)
(defvar calc-stack)
(defvar calc-show-banner)
(defvar calc-display-trail)
(defvar calc-inhibit-startup-message)
(defvar calc-main-buffer)
(defvar displayed-month)
(defvar displayed-year)
(defvar calendar-week-start-day)
(defvar calendar-day-header-array)
(defvar elfeed-search-filter)
(defvar elfeed-search-entries)
(defvar elfeed-feeds)
(declare-function calc "calc" (&optional arg full-display interactive))
(declare-function calc-refresh "calc" (&optional align))
(declare-function calc-eval "calc" (str &optional separator &rest args))
(declare-function calc-stack-size "calc" ())
(declare-function calc-pop "calc" (n))
(declare-function calendar-cursor-to-date "calendar" (&optional error event))
(declare-function calendar-current-date "calendar" (&optional offset))
(declare-function calendar-goto-date "cal-move" (date &optional noecho))
(declare-function calendar-date-string "calendar" (date &optional abbreviate nodayname))
(declare-function calendar-month-name "calendar" (month &optional abbrev))
(declare-function calendar-last-day-of-month "calendar" (month year))
(declare-function calendar-day-of-week "calendar" (date))
(declare-function calendar-holiday-list "holidays" ())
(declare-function calendar-scroll-left "cal-move" (&optional arg event))
(declare-function calendar-scroll-right "cal-move" (&optional arg event))
(declare-function calendar-check-holidays "holidays" (date))
(defvar lispm-rotom-state)
(defvar lispm-rotom-accounts)
(defvar lispm-rotom-daemon)
(declare-function lispm-rotom-running-p "lispm-rotom" ())
(declare-function lispm-rotom-lispm "lispm-rotom" ())
(declare-function lispm-rotom-read "lispm-rotom" (&optional callback))
(declare-function lispm-1password-filter-label "lispm-1password-ui" ())
(declare-function lispm-1password-status "lispm-1password-ui" ())
(declare-function lispm-1password-item-status "lispm-1password-ui" ())
(declare-function lispm-security-status "lispm-security-ui" ())
(declare-function lispm-mycin-status "lispm-mycin" ())
(declare-function lispm-shrdlu-status "lispm-shrdlu" ())
(declare-function elfeed-db-last-update "elfeed-db" ())
(declare-function elfeed-tagged-p "elfeed-db" (tag entry))

;;;; Calculator

(defvar-local lispm-desktop-app-skins--calc-entry ""
  "The number being typed on the Calculator's keypad.")

(defvar-local lispm-desktop-app-skins--calc-window nil
  "The window of Calc's stack the keypad works on.")

(defconst lispm-desktop-app-skins-calc-keys
  '(("C" clear) ("⌫" delete) ("±" sign) ("÷" calc-divide)
    ("7" "7") ("8" "8") ("9" "9") ("×" calc-times)
    ("4" "4") ("5" "5") ("6" "6") ("−" calc-minus)
    ("1" "1") ("2" "2") ("3" "3") ("+" calc-plus)
    ("0" "0") ("." ".") ("√" calc-sqrt) ("Enter" enter))
  "The Calculator's keypad, four keys a row: (LABEL WHAT), WHAT a string
typed into the display, a Calc command done on the stack, or one of
`clear', `delete', `sign' and `enter'.")

(defconst lispm-desktop-app-skins-calc-key 56
  "Side of a key of the Calculator's keypad, in pixels.")

(defconst lispm-desktop-app-skins-calc-display 44
  "Height of the Calculator's display over its keys, in pixels.")

(defun lispm-desktop-app-skins--calc-size ()
  "Return the size of the keypad's drawing, (WIDTH . HEIGHT) in pixels."
  (let ((edge (* 2 (+ lispm-desktop-app-board-margin 6))))
    (cons (+ edge (* 4 lispm-desktop-app-skins-calc-key))
          (+ edge lispm-desktop-app-skins-calc-display
             (* (/ (length lispm-desktop-app-skins-calc-keys) 4) lispm-desktop-app-skins-calc-key)))))

(defun lispm-desktop-app-skins--calc-draw (palette)
  "Return (SVG . HIT) of the Calculator's keypad in PALETTE: its display,
showing the number being typed, over its keys."
  (pcase-let* ((`(,width . ,height) (lispm-desktop-app-skins--calc-size))
               (key lispm-desktop-app-skins-calc-key)
               (left (+ lispm-desktop-app-board-margin 6))
               (top (+ left lispm-desktop-app-skins-calc-display))
               (svg (svg-create width height))
               (grey (lambda (level) (lispm-desktop-platinum-grey palette level))))
    (lispm-desktop-platinum-svg-fill svg 0 0 width height (plist-get palette :content))
    (lispm-desktop-app-board-draw-bevel svg lispm-desktop-app-board-margin lispm-desktop-app-board-margin
                                        (- width (* 2 lispm-desktop-app-board-margin))
                                        (- height (* 2 lispm-desktop-app-board-margin)) (funcall grey #xDD))
    ;; The display: a placard, the number right-aligned in it.
    (lispm-desktop-app-board-draw-well svg (+ left 2) (+ left 2) (- (* 4 key) 4)
                                       (- lispm-desktop-app-skins-calc-display 10) (funcall grey #xF4))
    (let ((font (lispm-desktop-platinum-svg-font 'lispm-desktop-platinum-list))
          (entry lispm-desktop-app-skins--calc-entry))
      (svg-text svg (if (string-empty-p entry) "0" entry)
                :x (- (+ left (* 4 key)) 12) :y (+ left 27) :text-anchor "end"
                :font-family (plist-get font :family) :font-size 20
                :fill (if (string-empty-p entry) (funcall grey #x99) (plist-get palette :text))))
    (seq-do-indexed
     (pcase-lambda (`(,label ,what) index)
       (lispm-desktop-app-board-draw-tile
        svg `(:shape key :text ,label
              :color ,(cond ((eq what 'enter) (lispm-desktop-platinum-blend (plist-get palette :accent) "#ffffff" 0.55))
                            ((symbolp what) (funcall grey #xCC))))
        (+ left (* (% index 4) key)) (+ top (* (/ index 4) key)) key palette (funcall grey #xDD)))
     lispm-desktop-app-skins-calc-keys)
    (cons svg
          (lambda (x y)
            (let ((column (floor (- x left) key))
                  (row (floor (- y top) key)))
              (when (and (< -1 column 4) (<= 0 row) (< (+ column (* 4 row)) (length lispm-desktop-app-skins-calc-keys)))
                (let ((what (nth 1 (nth (+ column (* 4 row)) lispm-desktop-app-skins-calc-keys))))
                  (lambda () (lispm-desktop-app-skins-calc-press what)))))))))

(defun lispm-desktop-app-skins--calc-do (command)
  "Do Calc's COMMAND on its stack, as typed in its window."
  (let ((window lispm-desktop-app-skins--calc-window))
    (if (window-live-p window)
        (with-selected-window window (call-interactively command))
      (user-error (lispm-desktop-tr "The Calculator's stack is closed")))))

(defun lispm-desktop-app-skins--calc-push ()
  "Push the number typed on the keypad onto Calc's stack, if any; return
non-nil when one was."
  (let ((entry lispm-desktop-app-skins--calc-entry))
    (unless (member entry '("" "-" "." "-."))
      (setq lispm-desktop-app-skins--calc-entry "")
      (with-current-buffer (window-buffer lispm-desktop-app-skins--calc-window)
        (calc-eval entry 'push)
        (calc-refresh))
      t)))

(defun lispm-desktop-app-skins-calc-press (what)
  "Press the Calculator's key doing WHAT, see `lispm-desktop-app-skins-calc-keys'."
  (let ((entry lispm-desktop-app-skins--calc-entry))
    (pcase what
      ((pred stringp)
       (unless (and (equal what ".") (string-search "." entry))
         (setq lispm-desktop-app-skins--calc-entry (concat entry what))))
      ('clear (if (string-empty-p entry)
                  (when (> (calc-stack-size) 0)
                    (lispm-desktop-app-skins--calc-do (lambda () (interactive) (calc-pop (calc-stack-size)))))
                (setq lispm-desktop-app-skins--calc-entry "")))
      ('delete (if (string-empty-p entry)
                   (lispm-desktop-app-skins--calc-do 'calc-pop)
                 (setq lispm-desktop-app-skins--calc-entry (substring entry 0 -1))))
      ('sign (if (string-empty-p entry)
                 (lispm-desktop-app-skins--calc-do 'calc-change-sign)
               (setq lispm-desktop-app-skins--calc-entry
                     (if (string-prefix-p "-" entry) (substring entry 1) (concat "-" entry)))))
      ('enter (unless (lispm-desktop-app-skins--calc-push)
                (lispm-desktop-app-skins--calc-do 'calc-enter)))
      (_ (lispm-desktop-app-skins--calc-push)
         (lispm-desktop-app-skins--calc-do what)))
    (lispm-desktop-app-board-refresh)))

(defun lispm-desktop-app-skins--calc-typed ()
  "Press the keypad's key of the character just typed."
  (interactive)
  (let ((char (string last-command-event)))
    (lispm-desktop-app-skins-calc-press
     (pcase char
       ((pred (string-match-p "\\`[0-9.]\\'")) char)
       ("+" 'calc-plus) ("-" 'calc-minus) ("*" 'calc-times) ("/" 'calc-divide)
       ("\r" 'enter) ("\177" 'delete) ("n" 'sign) ("Q" 'calc-sqrt)))))

(defvar-keymap lispm-desktop-app-skins-calc-keypad-mode-map
  :doc "Keys of the Calculator's keypad: those of its keys, as Calc's."
  "0" #'lispm-desktop-app-skins--calc-typed "1" #'lispm-desktop-app-skins--calc-typed
  "2" #'lispm-desktop-app-skins--calc-typed "3" #'lispm-desktop-app-skins--calc-typed
  "4" #'lispm-desktop-app-skins--calc-typed "5" #'lispm-desktop-app-skins--calc-typed
  "6" #'lispm-desktop-app-skins--calc-typed "7" #'lispm-desktop-app-skins--calc-typed
  "8" #'lispm-desktop-app-skins--calc-typed "9" #'lispm-desktop-app-skins--calc-typed
  "." #'lispm-desktop-app-skins--calc-typed "+" #'lispm-desktop-app-skins--calc-typed
  "-" #'lispm-desktop-app-skins--calc-typed "*" #'lispm-desktop-app-skins--calc-typed
  "/" #'lispm-desktop-app-skins--calc-typed "RET" #'lispm-desktop-app-skins--calc-typed
  "DEL" #'lispm-desktop-app-skins--calc-typed "n" #'lispm-desktop-app-skins--calc-typed
  "Q" #'lispm-desktop-app-skins--calc-typed)

(define-derived-mode lispm-desktop-app-skins-calc-keypad-mode lispm-desktop-app-window-mode "Keypad"
  "Major mode of the Calculator's keypad, under Calc's stack.
\\{lispm-desktop-app-skins-calc-keypad-mode-map}")

(defun lispm-desktop-app-skins--calc-status ()
  "Return what the Calculator's bottom says: its angles and precision."
  (when-let* ((window lispm-desktop-app-skins--calc-window)
              ((window-live-p window)))
    (with-current-buffer (window-buffer window)
      (list (if (eq calc-angle-mode 'rad) (lispm-desktop-tr "Radians") (lispm-desktop-tr "Degrees"))
            (lispm-desktop-tr-n "%d digit" "%d digits" calc-internal-prec)))))

(defun lispm-desktop-app-skins-calculator ()
  "Open Calc in the selected window, its stack over a keypad, without the
trail: the stack and the display are where the numbers are."
  (interactive)
  (require 'calc)
  (let ((calc-display-trail nil)
        (calc-inhibit-startup-message t))
    (calc nil t))
  (setq-local calc-show-banner nil)
  (calc-refresh)
  (face-remap-add-relative 'default :height 1.3)
  (let* ((stack (selected-window))
         (frame (window-frame stack))
         (keypad (get-buffer-create " *Calculator Keypad*")))
    (delete-other-windows stack)
    (with-current-buffer keypad
      (lispm-desktop-app-skins-calc-keypad-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert " "))
      (setq lispm-desktop-app-skins--calc-window stack))
    (pcase-let* ((`(,width . ,height) (lispm-desktop-app-skins--calc-size))
                 (stack-height (+ (window-tab-line-height stack) lispm-desktop-platinum-header-strip-height
                                  (* 5 (default-line-height)) 12)))
      (set-frame-size frame width (+ stack-height height lispm-desktop-platinum-title-height 4) t)
      (let ((below (split-window stack (- (+ height lispm-desktop-platinum-title-height 4)) 'below t)))
        (set-window-buffer below keypad)
        (set-window-dedicated-p below t)))
    (select-window stack)))

(lispm-desktop-app-ui-define 'calc-mode
  :app 'calculator
  :run #'lispm-desktop-app-skins-calculator
  :bottom nil
  :buttons `((,(lispm-desktop-tr-noop "Undo") calc-undo) (,(lispm-desktop-tr-noop "Redo") calc-redo)
             (,(lambda ()
                 (if (eq calc-angle-mode 'rad) (lispm-desktop-tr "Radians") (lispm-desktop-tr "Degrees")))
              ,(lambda () (interactive)
                 (call-interactively (if (eq calc-angle-mode 'rad) 'calc-degrees-mode 'calc-radians-mode)))))
  :keys `(("0 … 9  ." . ,(lispm-desktop-tr-noop "Type a number"))
          ("Return" . ,(lispm-desktop-tr-noop "Push it onto the stack"))
          ("+ − * /" . ,(lispm-desktop-tr-noop "Add, subtract, multiply, divide the two last"))
          ("Delete" . ,(lispm-desktop-tr-noop "Drop the last number"))
          ("n" . ,(lispm-desktop-tr-noop "Change the sign")) ("Q" . ,(lispm-desktop-tr-noop "Square root"))
          ("U" . ,(lispm-desktop-tr-noop "Undo")) ("D" . ,(lispm-desktop-tr-noop "Redo"))))

(lispm-desktop-app-ui-define 'lispm-desktop-app-skins-calc-keypad-mode
  :board `(:draw lispm-desktop-app-skins--calc-draw
           :model ,(lambda () lispm-desktop-app-skins--calc-entry))
  :buttons nil
  :fit nil
  :status #'lispm-desktop-app-skins--calc-status)

;;;; Calendar

(defconst lispm-desktop-app-skins-calendar-cell '(34 . 26)
  "Width and height of a day of the Calendar's months, in pixels.")

(defvar-local lispm-desktop-app-skins--holidays nil
  "The holidays of the months shown, as (MONTH YEAR . HOLIDAYS).")

(defun lispm-desktop-app-skins--holidays ()
  "Return the holidays of the Calendar's months shown, ((DATE NAME) ...)."
  (unless (equal (cons displayed-month displayed-year) (take 2 lispm-desktop-app-skins--holidays))
    (setq lispm-desktop-app-skins--holidays
          (cons displayed-month (cons displayed-year (ignore-errors (require 'holidays) (calendar-holiday-list))))))
  (cddr lispm-desktop-app-skins--holidays))

(defun lispm-desktop-app-skins--calendar-months ()
  "Return the Calendar's months shown: three (MONTH . YEAR), the middle one
`displayed-month'."
  (mapcar (lambda (offset)
            (let ((index (+ (* displayed-year 12) (1- displayed-month) offset)))
              (cons (1+ (mod index 12)) (floor index 12))))
          '(-1 0 1)))

(defun lispm-desktop-app-skins--calendar-model ()
  "Return what the Calendar's drawing shows."
  (list displayed-month displayed-year (ignore-errors (calendar-cursor-to-date))
        (calendar-current-date) (lispm-desktop-app-skins--holidays)))

(defun lispm-desktop-app-skins--calendar-draw (palette)
  "Return (SVG . HIT) of the Calendar's three months in PALETTE, each a
title, its days' names and its weeks; HIT goes to the day clicked, or
to the months before or after from the arrows."
  (pcase-let* ((`(,cell-width . ,cell-height) lispm-desktop-app-skins-calendar-cell)
               (margin lispm-desktop-app-board-margin)
               (gap 18)
               (month-width (* 7 cell-width))
               (title 30)
               (top (+ margin 8))
               (width (+ (* 2 margin) 16 (* 3 month-width) (* 2 gap)))
               (height (+ top title cell-height (* 6 cell-height) margin 8))
               (svg (svg-create width height))
               (font (lispm-desktop-platinum-svg-font 'lispm-desktop-platinum-list))
               (text (plist-get palette :text))
               (muted (lispm-desktop-platinum-grey palette #x88))
               (red "#c0392b")
               (today (calendar-current-date))
               (cursor (ignore-errors (calendar-cursor-to-date)))
               (holidays (mapcar #'car (lispm-desktop-app-skins--holidays)))
               (spots nil))
    (cl-flet ((write (string x y &rest attributes)
                (let ((all (list :x x :y y :text-anchor "middle" :font-family (plist-get font :family)
                                 :font-size 13 :fill text)))
                  (cl-loop for (key value) on attributes by #'cddr
                           do (setq all (plist-put all key value)))
                  (apply #'svg-text svg string all))))
      (lispm-desktop-platinum-svg-fill svg 0 0 width height (plist-get palette :content))
      (lispm-desktop-app-board-draw-bevel svg margin margin (- width (* 2 margin)) (- height (* 2 margin))
                                          (lispm-desktop-platinum-grey palette #xEE))
      (seq-do-indexed
       (pcase-lambda (`(,month . ,year) index)
         (let* ((left (+ margin 8 (* index (+ month-width gap))))
                (first (mod (- (calendar-day-of-week (list month 1 year)) calendar-week-start-day) 7))
                (days (calendar-last-day-of-month month year)))
           (write (format "%s %d" (calendar-month-name month) year) (+ left (/ month-width 2)) (+ top 18)
                  :font-weight "bold")
           (dotimes (column 7)
             (let ((day (mod (+ column calendar-week-start-day) 7)))
               (write (aref calendar-day-header-array day) (+ left (* column cell-width) (/ cell-width 2))
                      (+ top title 13) :fill (if (memq day '(0 6)) red muted) :font-size 11)))
           (lispm-desktop-platinum-svg-fill svg left (+ top title cell-height -4) month-width 1 muted)
           (dotimes (day days)
             (let* ((slot (+ first day))
                    (x (+ left (* (% slot 7) cell-width)))
                    (y (+ top title cell-height (* (/ slot 7) cell-height)))
                    (date (list month (1+ day) year))
                    (weekday (mod (+ (% slot 7) calendar-week-start-day) 7)))
               (push (list x y date) spots)
               (when (equal date today)
                 (svg-rectangle svg (+ x 3) (+ y 2) (- cell-width 6) (- cell-height 4) :rx 4
                                :fill (plist-get palette :highlight)))
               (when (equal date cursor)
                 (svg-rectangle svg (+ x 2) (+ y 1) (- cell-width 4) (- cell-height 2) :rx 5 :fill "none"
                                :stroke (plist-get palette :focus) :stroke-width 2))
               (write (number-to-string (1+ day)) (+ x (/ cell-width 2)) (+ y 17)
                      :fill (cond ((equal date today) (plist-get palette :highlight-text))
                                  ((member date holidays) red)
                                  ((memq weekday '(0 6)) muted)
                                  (t text))
                      :font-weight (if (member date holidays) "bold" "normal"))
               (when (member date holidays)
                 (svg-circle svg (+ x (/ cell-width 2)) (+ y cell-height -3) 1.5 :fill red))))))
       (lispm-desktop-app-skins--calendar-months))
      ;; The arrows to the months before and after, by the outer titles.
      (let ((y (+ top 18)))
        (write "◀" (+ margin 20) y :fill muted)
        (write "▶" (- width margin 20) y :fill muted)))
    (cons svg
          (lambda (x y)
            (cond ((and (< y (+ top title)) (< x (+ margin 40))) #'calendar-scroll-right)
                  ((and (< y (+ top title)) (> x (- width margin 40))) #'calendar-scroll-left)
                  (t (when-let* ((spot (seq-find (pcase-lambda (`(,left ,top . ,_))
                                                   (and (<= left x (+ left cell-width -1))
                                                        (<= top y (+ top cell-height -1))))
                                                 spots)))
                       (lambda () (calendar-goto-date (nth 2 spot))))))))))

(defun lispm-desktop-app-skins--calendar-status ()
  "Return what the Calendar's bottom says: the day point is on, and its
holidays."
  (when-let* ((date (ignore-errors (calendar-cursor-to-date))))
    (cons (calendar-date-string date)
          (mapcar #'cadr (seq-filter (lambda (holiday) (equal (car holiday) date))
                                     (lispm-desktop-app-skins--holidays))))))

(lispm-desktop-app-ui-define 'calendar-mode
  :board `(:draw lispm-desktop-app-skins--calendar-draw :model lispm-desktop-app-skins--calendar-model)
  :fit t
  :status #'lispm-desktop-app-skins--calendar-status
  :buttons `((,(lispm-desktop-tr-noop "Today") calendar-goto-today)
             ("◀" calendar-scroll-right) ("▶" calendar-scroll-left))
  :keys `(("C-f  C-b" . ,(lispm-desktop-tr-noop "Next and previous day"))
          ("C-n  C-p" . ,(lispm-desktop-tr-noop "Next and previous week"))
          ("{  }" . ,(lispm-desktop-tr-noop "Previous and next month"))
          ("[  ]" . ,(lispm-desktop-tr-noop "Previous and next year"))
          ("<  >" . ,(lispm-desktop-tr-noop "Show earlier and later months"))
          ("." . ,(lispm-desktop-tr-noop "Today")) ("g d" . ,(lispm-desktop-tr-noop "Go to a date"))
          ("h" . ,(lispm-desktop-tr-noop "Holidays of the day"))
          ("a" . ,(lispm-desktop-tr-noop "Holidays of the months shown"))))

;;;; Elfeed

(defun lispm-desktop-app-skins--elfeed-status ()
  "Return what Elfeed's bottom says: its entries, those unread, its update."
  (when (boundp 'elfeed-search-entries)
    (list (lispm-desktop-tr-n "%d entry" "%d entries" (length elfeed-search-entries))
          (lispm-desktop-tr-n "%d unread" "%d unread"
                              (seq-count (lambda (entry) (elfeed-tagged-p 'unread entry)) elfeed-search-entries))
          (let ((time (elfeed-db-last-update)))
            (if (zerop time)
                (lispm-desktop-tr "Never updated")
              (lispm-desktop-format-time "Updated %b %e, %H:%M" time))))))

(defun lispm-desktop-app-skins--elfeed-empty-text ()
  "Return what Elfeed's page says while it has no entries, its lines
broken where they are to break."
  (if (bound-and-true-p elfeed-feeds)
      (lispm-desktop-tr "Your feeds have not been read yet.\nUpdate them to fetch their entries.")
    (lispm-desktop-tr
     "No feeds yet.\nAdd the address of a blog's or a site's feed,\nor import a list of feeds from another reader.")))

(lispm-desktop-app-ui-define 'elfeed-search-mode
  :app 'elfeed
  :list t
  :status #'lispm-desktop-app-skins--elfeed-status
  :label (lambda () (and (boundp 'elfeed-search-filter) elfeed-search-filter))
  :buttons `((,(lispm-desktop-tr-noop "Update") elfeed-update)
             (,(lispm-desktop-tr-noop "Add Feed…") elfeed-add-feed)
             (,(lispm-desktop-tr-noop "Filter…") elfeed-search-set-filter)
             (,(lispm-desktop-tr-noop "Open") elfeed-search-show-entry))
  :empty `(,(lambda () (zerop (elfeed-db-last-update)))
           lispm-desktop-app-skins--elfeed-empty-text
           ,(lambda () (if (bound-and-true-p elfeed-feeds)
                           `((,(lispm-desktop-tr "Update") elfeed-update))
                         `((,(lispm-desktop-tr "Add Feed…") elfeed-add-feed)
                           (,(lispm-desktop-tr "Import OPML…") elfeed-load-opml))))))

(lispm-desktop-app-ui-define 'elfeed-show-mode
  :app 'elfeed
  :prose t
  :buttons `((,(lispm-desktop-tr-noop "Back") elfeed-kill-buffer)
             (,(lispm-desktop-tr-noop "Previous") elfeed-show-prev)
             (,(lispm-desktop-tr-noop "Next") elfeed-show-next)
             (,(lispm-desktop-tr-noop "Open in Browser") elfeed-show-visit)))

;;;; Rotom

(defun lispm-desktop-app-skins--rotom-status ()
  "Return what Rotom's window says at its bottom: its daemon, the accounts
signed in, and the model LispM's agents use through it."
  (when (memq lispm-rotom-state '(ready no-account))
    (delq nil
          (list (if (lispm-rotom-running-p)
                    (lispm-desktop-tr "Running at %s" (plist-get lispm-rotom-daemon :url))
                  (lispm-desktop-tr "Daemon stopped"))
                (lispm-desktop-tr-n "%d account" "%d accounts" (length lispm-rotom-accounts))
                (when-let* ((lispm (lispm-rotom-lispm))
                            ((plist-get lispm :rotom)))
                  (lispm-desktop-tr "LispM uses %s" (plist-get lispm :model)))))))

(defun lispm-desktop-app-skins-rotom-install ()
  "Install Rotom's program, which its window lacks."
  (interactive)
  (lispm-desktop-app-ui-install-needs 'rotom))

(lispm-desktop-app-ui-define 'lispm-rotom-mode
  :app 'rotom
  :list t
  :status #'lispm-desktop-app-skins--rotom-status
  :empty `(,(lambda () (eq lispm-rotom-state 'missing))
           ,(lispm-desktop-tr-noop "Rotom is not installed yet.\nCargo, Rust's package manager, builds it\nin some minutes, in the Installer.")
           ((,(lispm-desktop-tr-noop "Install Rotom") lispm-desktop-app-skins-rotom-install)))
  :installed (lambda () (lispm-rotom-read)))

(defconst lispm-desktop-app-skins-rotom-words
  (list (lispm-desktop-tr-noop "Start") (lispm-desktop-tr-noop "Stop") (lispm-desktop-tr-noop "Restart")
        (lispm-desktop-tr-noop "Sign In…") (lispm-desktop-tr-noop "Refresh Tokens")
        (lispm-desktop-tr-noop "Use for LispM…")
        (lispm-desktop-tr-noop "Sign in, use the model, or start the daemon")
        (lispm-desktop-tr-noop "Start or stop the daemon") (lispm-desktop-tr-noop "Restart the daemon")
        (lispm-desktop-tr-noop "Sign in to an account")
        (lispm-desktop-tr-noop "Paste the address the browser shows")
        (lispm-desktop-tr-noop "Stop signing in") (lispm-desktop-tr-noop "Refresh the tokens")
        (lispm-desktop-tr-noop "Have LispM's agents use the model")
        (lispm-desktop-tr-noop "Have LispM's agents stop using Rotom")
        (lispm-desktop-tr-noop "Copy the model or the address")
        (lispm-desktop-tr-noop "View the daemon's log") (lispm-desktop-tr-noop "Update Rotom")
        (lispm-desktop-tr-noop "Read Rotom again") (lispm-desktop-tr-noop "Next line")
        (lispm-desktop-tr-noop "Previous line"))
  "The words of Rotom's buttons and keys, which Rotom declares with
lispm-kit, in English: the desktop's catalogs translate them, where its
buttons and keys window show them.")

;;;; 1Password

(lispm-desktop-app-ui-define 'lispm-1password-mode
  :title "1Password"
  :list t
  :label #'lispm-1password-filter-label
  :status #'lispm-1password-status
  :empty '(lispm-1password-page-p lispm-1password-page-text lispm-1password-page-buttons))

(lispm-desktop-app-ui-define 'lispm-1password-item-mode
  :title "1Password"
  :prose t
  :status #'lispm-1password-item-status)

;;;; Security

(lispm-desktop-app-ui-define 'lispm-security-mode
  :title "Security"
  :list t
  :status #'lispm-security-status)

(lispm-desktop-app-ui-define 'lispm-security-finding-mode
  :title "Security"
  :prose t
  :bottom nil)

;;;; MYCIN

(lispm-desktop-app-ui-define 'lispm-mycin-mode
  :title "MYCIN"
  :prose t
  :label (lambda () "Type an answer, then Return")
  :status #'lispm-mycin-status)

;;;; SHRDLU

(lispm-desktop-app-ui-define 'lispm-shrdlu-mode
  :app 'shrdlu
  :title "SHRDLU"
  :prose t
  :label (lambda () "Type a sentence, then Return")
  :bottom nil)

(lispm-desktop-app-ui-define 'lispm-shrdlu-world-mode
  :app 'shrdlu
  :title "SHRDLU"
  :buttons nil
  :cursor nil
  :status #'lispm-shrdlu-status)

;;;; Mail

(defvar lispm-mail-settings-glyph-function)
(defvar lispm-mail-settings-choose-function)
(defvar lispm-mail-settings-drawn-hook)
(defvar lispm-desktop-platinum-check-mail-functions)
(declare-function lispm-mail "lispm-mail" ())
(declare-function lispm-mail-check "lispm-mail-data" (&optional id))
(declare-function lispm-mail-settings-refresh "lispm-mail-settings" ())
(declare-function lispm-mail-list-title "lispm-mail" ())
(declare-function lispm-mail-message-title "lispm-mail" ())
(declare-function lispm-mail-list-status "lispm-mail" ())
(declare-function lispm-mail-search-label "lispm-mail" ())
(declare-function lispm-mail-compose-title "lispm-mail-compose" ())

(defun lispm-desktop-app-skins--mail-choose (title choices value)
  "Open the desktop's menu of CHOICES of a pop-up menu TITLE of Mail's
settings, VALUE checked, as `lispm-mail-settings-choose-function' asks."
  (lispm-desktop-form-choose title choices value nil))

(defun lispm-desktop-app-skins--mail-redraw (&rest _)
  "Draw Mail's settings again, in the look now in use."
  (when (fboundp 'lispm-mail-settings-refresh)
    (lispm-mail-settings-refresh)))

(defun lispm-desktop-app-skins--mail-message-setup ()
  "Give the box of a message's headers the platinum of a list's header."
  (face-remap-add-relative 'lispm-mail-header-box
                           `(:background ,(face-background 'lispm-desktop-platinum-header nil t) :extend t)))

(lispm-desktop-app-ui-define 'lispm-mail-mailboxes-mode
  :title (lispm-desktop-tr-noop "Folders")
  :list t
  :buttons nil
  :bottom nil
  :cursor nil)

(lispm-desktop-app-ui-define 'lispm-mail-message-mode
  :title (lambda () (lispm-mail-message-title))
  :buttons nil
  :bottom nil
  :setup #'lispm-desktop-app-skins--mail-message-setup)

(lispm-desktop-app-ui-define 'lispm-mail-compose-mode
  :title (lambda () (lispm-mail-compose-title))
  :bottom nil)

(lispm-desktop-app-ui-define 'lispm-mail-settings-mode
  :buttons nil
  :bottom nil)

(lispm-desktop-app-ui-define 'lispm-mail-account-mode
  :buttons nil
  :bottom nil)

(lispm-desktop-app-ui-define 'lispm-mail-mode
  :app 'mail
  :list t
  :title (lambda () (lispm-mail-list-title))
  :status (lambda () (lispm-mail-list-status))
  :label (lambda () (lispm-mail-search-label))
  :size '(1040 . 680)
  :restore (lambda () (lispm-mail))
  :cursor nil)

(with-eval-after-load 'lispm-mail-settings
  (setq lispm-mail-settings-glyph-function #'lispm-desktop-form-glyph
        lispm-mail-settings-choose-function #'lispm-desktop-app-skins--mail-choose)
  (add-hook 'lispm-mail-settings-drawn-hook #'lispm-desktop-form-dress)
  (add-hook 'enable-theme-functions #'lispm-desktop-app-skins--mail-redraw)
  (add-hook 'lispm-desktop-language-changed-hook #'lispm-desktop-app-skins--mail-redraw))

(with-eval-after-load 'lispm-mail-data
  (add-hook 'lispm-desktop-platinum-check-mail-functions #'lispm-mail-check))

(defconst lispm-desktop-app-skins-mail-words
  (list (lispm-desktop-tr-noop "Get Mail") (lispm-desktop-tr-noop "Reply")
        (lispm-desktop-tr-noop "Reply All") (lispm-desktop-tr-noop "Delete")
        (lispm-desktop-tr-noop "Send") (lispm-desktop-tr-noop "Attach")
        (lispm-desktop-tr-noop "Save Draft") (lispm-desktop-tr-noop "Discard")
        (lispm-desktop-tr-noop "Test") (lispm-desktop-tr-noop "Add Account")
        (lispm-desktop-tr-noop "Mailbox") (lispm-desktop-tr-noop "Message")
        (lispm-desktop-tr-noop "Account") (lispm-desktop-tr-noop "Mail"))
  "The words of Mail's buttons and menus, which Mail declares with
lispm-kit, in English: the desktop's catalogs translate them, where its
buttons and menus show them.")

(provide 'lispm-desktop-app-skins)
;;; lispm-desktop-app-skins.el ends here
