;;; lispm-desktop-platinum-status.el --- The time, load, mail and battery  -*- lexical-binding: t; -*-

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

;; What the menu bar and the Control Strip show of Emacs's own time, load,
;; mail and battery, read from its time and battery packages.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-desktop-platinum-menubar)
(require 'lispm-desktop-i18n)
(require 'lispm-exec)

(declare-function lispm-desktop-popup-menu "lispm-desktop-popup" (items &optional x y align))
(declare-function battery-format "battery" (format alist))
(declare-function battery-update "battery" ())
(declare-function display-time-update "time" ())


;;;; Status: Emacs's own time, load, mail and battery

;; Emacs keeps the time, the load average and the mail flag up to date
;; (`display-time-mode') and the battery (`display-battery-mode').  As
;; Mac OS 9's menu bar held its clock alone at its right end, the time is
;; a title of its own there, its menu giving the date, the calendar and
;; the world clock; the load, the mail and the battery are modules of
;; the Control Strip (lispm-desktop-strip.el), drawn and filled here:
;;
;;   load      a meter; the menu picks the 1, 5 or 15 minute average and
;;             opens the process list
;;   mail      an envelope, flagged when mail has come; the menu reads,
;;             composes and checks mail
;;   battery   a PowerBook's battery filled to its charge; the menu tells
;;             the charge, the power source and the time left
;;
;; Emacs computes everything; `display-time-hook' and
;; `battery-update-functions' only note what it found.  On macOS, where
;; Emacs asks pmset of the battery every minute and waits for it, pmset is
;; asked in the background instead, in lispm-kernel when it runs
;; (`lispm-exec'), and Emacs reads what it answered, see
;; `lispm-desktop-platinum--battery-pmset'.  Whatever else is
;; in `global-mode-string', such as LispM's agent count, stays on the menu
;; bar, before the time.

(defvar battery-status-function)
(defvar display-time-string)
(defvar battery-mode-line-string)
(defvar display-time-load-average)
(defvar display-time-format)
;; Bound by `display-time-update' while `display-time-hook' runs; the names
;; are time.el's.
(with-suppressed-warnings ((lexical load mail))
  (defvar load)
  (defvar mail))

(defvar lispm-desktop-platinum--load ""
  "The load average `display-time-mode' last showed, \"\" below its threshold.")

(defvar lispm-desktop-platinum--mail nil
  "Non-nil when `display-time-mode' last found new mail.")

(defvar lispm-desktop-platinum--battery nil
  "The battery status `display-battery-mode' last read, an alist.")

(defun lispm-desktop-platinum-battery ()
  "Return what the battery reported last, as `battery-status-function' does,
or nil before it reports."
  lispm-desktop-platinum--battery)

(defun lispm-desktop-platinum-load ()
  "Return the load average `display-time-mode' last showed, \"\" below its
threshold."
  lispm-desktop-platinum--load)

(defun lispm-desktop-platinum-new-mail-p ()
  "Return non-nil when `display-time-mode' last found new mail."
  lispm-desktop-platinum--mail)

(defun lispm-desktop-platinum--note-time ()
  "Note the load average and the mail flag Emacs has just computed.
Run from `display-time-hook', inside `display-time-update'."
  (setq lispm-desktop-platinum--load (string-trim (substring-no-properties load))
        lispm-desktop-platinum--mail (and mail t)))

(defun lispm-desktop-platinum--note-battery (data)
  "Note the battery status DATA Emacs has just read."
  (setq lispm-desktop-platinum--battery data))

(defvar lispm-desktop-platinum--pmset nil
  "What pmset -g ps answered last, asked in the background, or nil.")

(defvar lispm-desktop-platinum--pmset-asked nil
  "Non-nil while pmset is asked of the battery in the background.")

(defvar lispm-desktop-platinum--pmset-answered nil
  "Non-nil while Emacs's battery is updated with what pmset just answered.")

(defun lispm-desktop-platinum--pmset-status (pmset answer)
  "Return the battery status PMSET, `battery-pmset', reads from ANSWER, what
pmset -g ps printed, as it would read it from pmset itself."
  (cl-letf (((symbol-function 'call-process) (lambda (&rest _) (insert answer) 0)))
    (funcall pmset)))

(defun lispm-desktop-platinum--ask-pmset ()
  "Ask pmset of the battery in the background, then update Emacs's battery
with what it answered."
  (unless lispm-desktop-platinum--pmset-asked
    (setq lispm-desktop-platinum--pmset-asked t)
    (lispm-exec "pmset" '("-g" "ps")
                (lambda (exit output _errors)
                  (setq lispm-desktop-platinum--pmset-asked nil)
                  (when (eql exit 0)
                    (setq lispm-desktop-platinum--pmset output)
                    (when (bound-and-true-p display-battery-mode)
                      (let ((lispm-desktop-platinum--pmset-answered t))
                        (battery-update))))))))

(defun lispm-desktop-platinum--battery-pmset (pmset)
  "Return the battery status as PMSET, `battery-pmset', reads it, without
waiting for pmset: from what pmset answered last, asked again in the
background, Emacs's battery updated once it answers.  The first time,
nothing answered yet, PMSET asks pmset itself and waits.
This is around `battery-pmset'."
  (unless lispm-desktop-platinum--pmset-answered
    (lispm-desktop-platinum--ask-pmset))
  (if lispm-desktop-platinum--pmset
      (lispm-desktop-platinum--pmset-status pmset lispm-desktop-platinum--pmset)
    (funcall pmset)))

(defun lispm-desktop-platinum-draw-battery (palette percent charging)
  "Return an SVG of a battery sixteen pixels square, filled to PERCENT in
PALETTE, as the Control Strip's battery module shows it.  CHARGING draws
a bolt across it; a nearly empty one is filled in red."
  (let* ((svg (svg-create 16 16))
         (ink (plist-get palette :text))
         (level (round (* 10 (/ (max 0 (min 100 percent)) 100.0)))))
    (lispm-ui-svg-fill svg 0 4 14 8 ink)
    (lispm-ui-svg-fill svg 1 5 12 6 (plist-get palette :strip))
    (lispm-ui-svg-fill svg 14 6 2 4 ink)
    (lispm-ui-svg-fill svg 2 6 level 4 (if (< percent 10) "#CE3131" ink))
    (when charging
      (svg-polygon svg '((8 . 3) (4 . 8) (6.5 . 8) (5 . 13) (10 . 7) (7.5 . 7) (9 . 3))
                   :fill (plist-get palette :accent) :stroke (plist-get palette :strip)
                   :stroke-width 0.75))
    svg))

(defun lispm-desktop-platinum-draw-meter (palette fraction)
  "Return an SVG of a load meter sixteen pixels square in PALETTE: three
bars rising left to right, filled to FRACTION."
  (let ((svg (svg-create 16 16)))
    (dotimes (i 3)
      (let ((height (* 4 (1+ i))))
        (lispm-ui-svg-fill svg (1+ (* i 5)) (- 14 height) 4 height
                                      (if (> fraction (/ i 3.0))
                                          (plist-get palette :text)
                                        (plist-get palette :strip-dark)))))
    svg))

(defun lispm-desktop-platinum-load-fraction ()
  "Return the load average `display-time-mode' shows, over the processors."
  (/ (string-to-number lispm-desktop-platinum--load) (float (max 1 (num-processors)))))

(defun lispm-desktop-platinum-load-items ()
  "Return the load menu: which average to show, and the processes."
  (let ((averages (ignore-errors (load-average t))))
    `((:label ,(lispm-desktop-tr "Load Average") :enabled nil)
      ,@(seq-map-indexed
         (lambda (minutes index)
           (list :label (lispm-desktop-tr-n "%d minute: %.2f" "%d minutes: %.2f" minutes
                                            minutes (or (nth index averages) 0))
                 :checked (eql display-time-load-average index)
                 :action (lambda ()
                           (setq display-time-load-average index)
                           (display-time-update))))
         '(1 5 15))
      separator
      (:label ,(lispm-desktop-tr "Processes") :action proced))))

(defvar lispm-desktop-platinum-check-mail-functions nil
  "Functions called, with no argument, as Check for Mail is chosen: an
application reading mail asks its servers then.")

(defvar lispm-desktop-platinum-open-mail-functions nil
  "Functions called, with no argument, to read mail in an application's
window, until one returns non-nil: it opened it.  None doing so, Emacs's
`read-mail-command' reads it.")

(defun lispm-desktop-platinum-read-mail ()
  "Read mail: in the window of the application reading it, when it is one."
  (interactive)
  (unless (run-hook-with-args-until-success 'lispm-desktop-platinum-open-mail-functions)
    (call-interactively read-mail-command)))

(defun lispm-desktop-platinum-compose-mail ()
  "Write a message, in the window of the application reading mail when it
is one, as that application writes it."
  (interactive)
  (run-hook-with-args-until-success 'lispm-desktop-platinum-open-mail-functions)
  (call-interactively #'compose-mail))

(defun lispm-desktop-platinum-check-mail ()
  "Check for mail: ask the application reading it, then show what is found."
  (interactive)
  (run-hooks 'lispm-desktop-platinum-check-mail-functions)
  (display-time-update))

(defun lispm-desktop-platinum-mail-items ()
  "Return the Mail menu: read, compose and check mail."
  `((:label ,(if lispm-desktop-platinum--mail
                  (lispm-desktop-tr "You have new mail")
                (lispm-desktop-tr "No new mail"))
            :enabled nil)
    separator
    (:label ,(lispm-desktop-tr "Read Mail") :action lispm-desktop-platinum-read-mail)
    (:label ,(lispm-desktop-tr "Compose Mail…") :action lispm-desktop-platinum-compose-mail)
    separator
    (:label ,(lispm-desktop-tr "Check for Mail") :action lispm-desktop-platinum-check-mail)))

(defun lispm-desktop-platinum-battery-percent ()
  "Return the battery's charge in percent, or nil without a battery shown."
  (and (bound-and-true-p display-battery-mode) lispm-desktop-platinum--battery
       (ignore-errors (string-to-number (alist-get ?p lispm-desktop-platinum--battery)))))

(defun lispm-desktop-platinum-battery-charging-p ()
  "Return non-nil when the battery is on the power adapter."
  (member (alist-get ?L lispm-desktop-platinum--battery) '("AC" "on-line")))

(defun lispm-desktop-platinum-battery-items ()
  "Return the battery menu: its charge, its power source and the time left."
  (let* ((data lispm-desktop-platinum--battery)
         (line (lambda (format) (battery-format format data))))
    `((:label ,(funcall line (lispm-desktop-tr "Charge: %p%%")) :enabled nil)
      (:label ,(lispm-desktop-tr "Power: %s"
                                 (pcase (alist-get ?L data)
                                   ((or "AC" "on-line") (lispm-desktop-tr "Power Adapter"))
                                   ((or "Battery" "Battery Power" "off-line") (lispm-desktop-tr "Battery"))
                                   (source (or source (lispm-desktop-tr "unknown")))))
              :enabled nil)
      ,@(unless (member (alist-get ?t data) '(nil "N/A" "0:00"))
          `((:label ,(funcall line (lispm-desktop-tr "Time left: %t")) :enabled nil)))
      separator
      (:label ,(lispm-desktop-tr "Battery Status") :action battery))))

(defun lispm-desktop-platinum--status-title (key image label command help)
  "Return the menu bar status item KEY: IMAGE (an SVG or nil) and LABEL.
It runs COMMAND, described by HELP, and is highlighted while its menu is open."
  (lispm-desktop-platinum--menu-title
   key
   (concat (when image
             ;; An icon name gives a string already displayed as the icon.
             (if (stringp image) (lispm-desktop-platinum-icon image) (propertize " " 'display image)))
           (when (and image label) " ")
           label)
   command help (eq lispm-desktop-platinum--open-menu key)))

(defun lispm-desktop-platinum--status-menu (key items)
  "Pop up ITEMS under the status title KEY that was just clicked."
  (let ((x (car (posn-x-y (event-start last-input-event)))))
    (lispm-desktop-platinum-with-open-menu key
      (lispm-desktop-popup-menu items (max 0 (- x 12)) (lispm-desktop-platinum--menu-y)))))

(defun lispm-desktop-platinum-time-menu ()
  "Pop up the clock's menu: the date, the calendar, the world clock."
  (interactive)
  (lispm-desktop-platinum--status-menu
   'lispm-desktop-time
   `((:label ,(lispm-desktop-format-time "%A, %B %-e, %Y") :enabled nil)
     separator
     (:label ,(lispm-desktop-tr "Calendar") :action calendar)
     (:label ,(lispm-desktop-tr "World Clock") :action world-clock)
     separator
     (:label ,(lispm-desktop-tr "Time Format…")
             :action ,(lambda () (customize-variable 'display-time-format))))))

(defvar lispm-desktop-platinum-status-functions nil
  "Functions returning titles of the menu bar's right end, after the clock,
where Mac OS X put Spotlight's: each returns a list of items, as
`lispm-desktop-platinum-status-title' makes them, or nil.  Their order is
the hook's, see `add-hook''s DEPTH.")

(defun lispm-desktop-platinum-status-title (key image label command help)
  "Return the menu bar status item KEY: IMAGE (an SVG, an icon's name or
nil) and LABEL, either of them nil.  It runs COMMAND, described by HELP,
and is highlighted while its menu is open, see
`lispm-desktop-platinum-with-open-menu'."
  (lispm-desktop-platinum--status-title key image label command help))

(defun lispm-desktop-platinum-menu-status ()
  "Return the status titles at the menu bar's right end: whatever
`global-mode-string' holds besides the time and the battery, then the
time, as `display-time-format' says it, translated, see
`lispm-desktop-format-time', then the titles of
`lispm-desktop-platinum-status-functions'.  The load, the mail and the
battery are in the Control Strip."
  (let ((others (string-trim
                 (format-mode-line
                  (seq-remove (lambda (part)
                                (memq part '(display-time-string battery-mode-line-string)))
                              (ensure-list global-mode-string))))))
    (append
     (delq nil
           (list
            (unless (string-empty-p others)
              (lispm-desktop-platinum--menu-title 'lispm-desktop-status others #'ignore nil))
            (when (bound-and-true-p display-time-mode)
              (lispm-desktop-platinum--status-title
               'lispm-desktop-time nil
               ;; The format is translated, so that the clock reads as the
               ;; language has it, as 14:05 in German.
               (lispm-desktop-format-time (or display-time-format (lispm-desktop-tr-noop "%-I:%M %p")))
               #'lispm-desktop-platinum-time-menu (lispm-desktop-format-time "%A, %B %-e, %Y")))))
     (mapcan (lambda (function) (copy-sequence (funcall function)))
             (remq t lispm-desktop-platinum-status-functions)))))

(defun lispm-desktop-platinum--start-status ()
  "Turn on Emacs's own time, load and mail, and the battery if there is one.
The time reads as Mac OS 9's clock did, unless `display-time-format' was
set already."
  (require 'battery)
  (require 'time)
  (add-hook 'display-time-hook #'lispm-desktop-platinum--note-time)
  (add-hook 'battery-update-functions #'lispm-desktop-platinum--note-battery)
  (advice-add 'battery-pmset :around #'lispm-desktop-platinum--battery-pmset)
  (unless display-time-format
    (setq display-time-format (lispm-desktop-tr-noop "%-I:%M %p")))
  (display-time-mode 1)
  (when (and battery-status-function
             (not (equal (alist-get ?p (ignore-errors (funcall battery-status-function)))
                         "N/A")))
    (display-battery-mode 1)))

(provide 'lispm-desktop-platinum-status)
;;; lispm-desktop-platinum-status.el ends here
