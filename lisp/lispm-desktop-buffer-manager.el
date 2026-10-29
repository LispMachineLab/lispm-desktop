;;; lispm-desktop-buffer-manager.el --- What every buffer costs  -*- lexical-binding: t; -*-

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

;; The buffer manager lists the open buffers with what each costs, as Mac
;; OS 9's About This Computer barred each application's memory and as a
;; task manager lists processes: its kind, the size of its text, and for a
;; buffer running a process (a terminal, an agent's CLI) the CPU and the
;; memory of that process and every process it started, so an agent's
;; whole tree of tools is counted.  The list refreshes itself while shown,
;; ps listing the processes in the background, in lispm-kernel when it
;; runs (`lispm-exec'), as listing a thousand processes takes it a while.
;;
;;   ps --> pid: parent, CPU, memory --> each buffer's process tree --> row

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'tabulated-list)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-i18n)
(require 'lispm-exec)

(declare-function lispm-desktop-terminal-buffer-p "lispm-desktop-interface" (buffer))
(declare-function lispm-agents-buffer-p "lispm-agents" (buffer))
(declare-function lispm-desktop-sidebar-show-in-main "lispm-desktop-sidebar" (buffer))

(defconst lispm-desktop-buffer-manager-buffer-name "*Buffer Manager*"
  "Name of the buffer manager's buffer.")

(defconst lispm-desktop-buffer-manager-interval 2
  "Seconds between refreshes of the buffer manager while it is shown.")

;;;; Processes

(defconst lispm-desktop-buffer-manager--ps-arguments '("-axo" "pid=,ppid=,pcpu=,rss=")
  "What ps is asked for: each process's pid, parent, CPU and memory.")

(defvar lispm-desktop-buffer-manager--processes nil
  "The table of `lispm-desktop-buffer-manager--processes' as ps last
listed them, or nil before it did.")

(defvar lispm-desktop-buffer-manager--listing nil
  "Non-nil while ps lists the processes in the background.")

(defun lispm-desktop-buffer-manager--table (lines)
  "Return the table of pid to (PARENT CPU MEMORY) ps's LINES tell of."
  (let ((table (make-hash-table)))
    (dolist (line lines)
      (pcase (split-string line)
        (`(,pid ,parent ,cpu ,memory)
         (puthash (string-to-number pid)
                  (list (string-to-number parent) (string-to-number cpu)
                        (string-to-number memory))
                  table))))
    table))

(defun lispm-desktop-buffer-manager--processes ()
  "Return the system's processes: a table of pid to (PARENT CPU MEMORY).
CPU is a percentage of one core, MEMORY the resident size in kilobytes,
read from ps in one go: as it listed them last, in the background, see
`lispm-desktop-buffer-manager--list-processes', and the first time,
waiting for it.  Where ps is missing the table is empty."
  (or lispm-desktop-buffer-manager--processes
      (setq lispm-desktop-buffer-manager--processes
            (lispm-desktop-buffer-manager--table
             (ignore-errors (apply #'process-lines "ps" lispm-desktop-buffer-manager--ps-arguments))))))

(defun lispm-desktop-buffer-manager--list-processes (then)
  "Have ps list the system's processes in the background, then call THEN.
Listing them takes ps a while with a thousand processes, which Emacs does
not wait for; lispm-kernel runs ps when it runs, see `lispm-exec'.  While
a listing is under way, another is not started, nor THEN called."
  (unless lispm-desktop-buffer-manager--listing
    (setq lispm-desktop-buffer-manager--listing t)
    (lispm-exec "ps" lispm-desktop-buffer-manager--ps-arguments
                (lambda (exit output _errors)
                  (setq lispm-desktop-buffer-manager--listing nil)
                  (when (eql exit 0)
                    (setq lispm-desktop-buffer-manager--processes
                          (lispm-desktop-buffer-manager--table (split-string output "\n" t))))
                  (funcall then)))))

(defun lispm-desktop-buffer-manager--children (processes)
  "Return a table of pid to the pids of its children, from PROCESSES."
  (let ((children (make-hash-table)))
    (maphash (lambda (pid info) (push pid (gethash (car info) children))) processes)
    children))

(defun lispm-desktop-buffer-manager--tree-cost (pid processes children)
  "Return (CPU . MEMORY) of process PID and every process under it.
PROCESSES and CHILDREN are the tables of
`lispm-desktop-buffer-manager--processes' and
`lispm-desktop-buffer-manager--children'."
  (let ((cpu 0) (memory 0) (seen nil) (queue (list pid)))
    (while queue
      (let ((next (pop queue)))
        (unless (memq next seen)
          (push next seen)
          (when-let* ((info (gethash next processes)))
            (setq cpu (+ cpu (nth 1 info))
                  memory (+ memory (nth 2 info))))
          (setq queue (append (gethash next children) queue)))))
    (cons cpu memory)))

;;;; Rows

(defun lispm-desktop-buffer-manager--kind (buffer)
  "Return what kind of buffer BUFFER is, in words of the language LispM
speaks, or else its mode's name."
  (with-current-buffer buffer
    (cond ((and (fboundp 'lispm-agents-buffer-p) (lispm-agents-buffer-p buffer))
           (lispm-desktop-tr "Agent"))
          ((and (fboundp 'lispm-desktop-terminal-buffer-p) (lispm-desktop-terminal-buffer-p buffer))
           (lispm-desktop-tr "Terminal"))
          ((derived-mode-p 'xwidget-webkit-mode) (lispm-desktop-tr "Web Page"))
          ((derived-mode-p 'dired-mode) (lispm-desktop-tr "Folder"))
          (buffer-file-name (lispm-desktop-tr "Document"))
          ((derived-mode-p 'help-mode) (lispm-desktop-tr "Help"))
          (t (let ((name (format-mode-line mode-name nil nil buffer)))
               (if (string-empty-p name) (symbol-name major-mode) name))))))

(defun lispm-desktop-buffer-manager--buffers ()
  "Return the buffers to list: every one but Emacs's internal buffers."
  (seq-remove (lambda (buffer)
                (or (string-prefix-p " " (buffer-name buffer))
                    (minibufferp buffer)))
              (buffer-list)))

(defun lispm-desktop-buffer-manager--number (string value)
  "Return STRING carrying VALUE, the number its column sorts by."
  (propertize string 'lispm-desktop-value value))

(defun lispm-desktop-buffer-manager--size (bytes)
  "Return BYTES as a short size, as Finder wrote them."
  (if (< bytes 1024)
      (format "%d B" bytes)
    (file-size-human-readable bytes 'iec " ")))

(defun lispm-desktop-buffer-manager--bar (fraction)
  "Return a memory bar FRACTION full, as About This Computer drew one."
  (let ((filled (round (* (min 1 (max 0 fraction)) 58))))
    (propertize " " 'display
                (lispm-ui-image
                 (list 'memory-bar filled)
                 (lambda ()
                   (let ((svg (svg-create 60 10)))
                     (svg-rectangle svg 0 0 60 10 :fill (lispm-ui-color :frame))
                     (svg-rectangle svg 1 1 58 8 :fill (lispm-ui-color :content))
                     (when (> filled 0)
                       (svg-rectangle svg 1 1 filled 8 :fill (lispm-ui-color :accent)))
                     svg))))))

(defun lispm-desktop-buffer-manager--entries ()
  "Return the buffer manager's rows, as `tabulated-list-entries' takes them."
  (let* ((processes (lispm-desktop-buffer-manager--processes))
         (children (lispm-desktop-buffer-manager--children processes))
         (rows
          (mapcar
           (lambda (buffer)
             (let* ((process (get-buffer-process buffer))
                    ;; A lispm-desktop-term's program is the module's, not Emacs's.
                    (pid (or (and process (process-id process))
                             (and (fboundp 'lispm-desktop-term-pid) (lispm-desktop-term-pid buffer))))
                    (cost (and pid (lispm-desktop-buffer-manager--tree-cost pid processes children)))
                    (bytes (with-current-buffer buffer
                             (max 0 (1- (position-bytes (point-max)))))))
               (list buffer process pid cost bytes)))
           (lispm-desktop-buffer-manager--buffers)))
         (most (max 1 (apply #'max 1 (mapcar (lambda (row) (or (cdr (nth 3 row)) 0)) rows)))))
    (mapcar
     (pcase-lambda (`(,buffer ,process ,pid ,cost ,bytes))
       (let ((memory (or (cdr cost) 0)))
         (list buffer
               (vector
                ;; The icon is one character drawn two wide: the name is cut
                ;; short of the column, so it never runs into the next.
                (concat (lispm-desktop-platinum-buffer-icon buffer) " "
                        (truncate-string-to-width (lispm-desktop-platinum-buffer-title buffer)
                                                  29 nil nil "…"))
                (lispm-desktop-buffer-manager--kind buffer)
                (lispm-desktop-buffer-manager--number (lispm-desktop-buffer-manager--size bytes) bytes)
                (cond ((and pid process) (format "%s (%d)" (process-name process) pid))
                      (pid (lispm-desktop-tr "terminal (%d)" pid))
                      (process (lispm-desktop-tr "%s (network)" (process-name process)))
                      (t ""))
                (lispm-desktop-buffer-manager--number (if cost (format "%.1f%%" (car cost)) "")
                                                      (or (car cost) -1))
                (lispm-desktop-buffer-manager--number
                 (if cost
                     (concat (lispm-desktop-buffer-manager--size (* 1024 memory)) " "
                             (lispm-desktop-buffer-manager--bar (/ (float memory) most)))
                   "")
                 (if cost memory -1))))))
     rows)))

(defun lispm-desktop-buffer-manager--summary ()
  "Return the buffer manager's bottom line: the buffers, and Emacs's own cost."
  (let* ((attributes (ignore-errors (process-attributes (emacs-pid))))
         (cpu (alist-get 'pcpu attributes))
         (memory (alist-get 'rss attributes)))
    (let ((count (length (lispm-desktop-buffer-manager--buffers))))
      (concat (lispm-desktop-tr-n "%d buffer" "%d buffers" count)
              (when memory
                (let ((size (lispm-desktop-buffer-manager--size (* 1024 memory))))
                  (concat " · " (if cpu
                                    (lispm-desktop-tr "Emacs %s, %.1f%% CPU" size cpu)
                                  (lispm-desktop-tr "Emacs %s" size)))))))))

(defun lispm-desktop-buffer-manager--by (column)
  "Return a predicate sorting rows by the number in COLUMN."
  (lambda (a b)
    (< (or (get-text-property 0 'lispm-desktop-value (aref (cadr a) column)) -1)
       (or (get-text-property 0 'lispm-desktop-value (aref (cadr b) column)) -1))))

;;;; The list

(defvar lispm-desktop-buffer-manager--timer nil
  "Repeating timer refreshing the buffer manager while it is shown.")

(defun lispm-desktop-buffer-manager--shown-p ()
  "Return non-nil while the buffer manager is shown; else stop its timer."
  (or (when-let* ((buffer (get-buffer lispm-desktop-buffer-manager-buffer-name)))
        (get-buffer-window buffer t))
      (progn (when lispm-desktop-buffer-manager--timer
               (cancel-timer lispm-desktop-buffer-manager--timer)
               (setq lispm-desktop-buffer-manager--timer nil))
             nil)))

(defun lispm-desktop-buffer-manager--seen-p ()
  "Return non-nil while the buffer manager is in sight, not only shown on
a frame out of sight, as with the desktop hidden."
  (when-let* ((buffer (get-buffer lispm-desktop-buffer-manager-buffer-name)))
    (get-buffer-window buffer 'visible)))

(defun lispm-desktop-buffer-manager-refresh (&rest _)
  "Refresh the buffer manager's rows and bottom line, keeping the row at point.
The processes are as ps last listed them; asked for, it is listed anew,
the rows then refreshed again.  The refresh timer stops once the list is
no longer shown, and passes while it is out of sight, as with the
desktop hidden."
  (interactive)
  (when (and (lispm-desktop-buffer-manager--shown-p) (lispm-desktop-buffer-manager--seen-p))
    (with-current-buffer lispm-desktop-buffer-manager-buffer-name
      (setq tabulated-list-entries (lispm-desktop-buffer-manager--entries)
            lispm-desktop-platinum-info (lispm-desktop-buffer-manager--summary))
      (tabulated-list-print t t)
      (force-mode-line-update))
    (when (called-interactively-p 'any)
      (lispm-desktop-buffer-manager--tick))))

(defun lispm-desktop-buffer-manager--tick ()
  "Have ps list the processes anew, then refresh the buffer manager, while
it is in sight; while it is out of sight, ps is not asked."
  (when (and (lispm-desktop-buffer-manager--shown-p) (lispm-desktop-buffer-manager--seen-p))
    (lispm-desktop-buffer-manager--list-processes #'lispm-desktop-buffer-manager-refresh)))

(defun lispm-desktop-buffer-manager-show-buffer ()
  "Show the buffer of the row at point in the main area."
  (interactive)
  (when-let* ((buffer (tabulated-list-get-id))
              ((buffer-live-p buffer)))
    (lispm-desktop-sidebar-show-in-main buffer)))

(defun lispm-desktop-buffer-manager-close-buffer ()
  "Close the buffer of the row at point, asking first as its close box would."
  (interactive)
  (when-let* ((buffer (tabulated-list-get-id))
              ((buffer-live-p buffer))
              ((lispm-desktop-platinum-confirm-close buffer)))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))
    (lispm-desktop-buffer-manager-refresh)))

(defvar-keymap lispm-desktop-buffer-manager-mode-map
  :doc "Keymap of the buffer manager."
  :parent tabulated-list-mode-map
  "RET" #'lispm-desktop-buffer-manager-show-buffer
  "<double-mouse-1>" #'lispm-desktop-buffer-manager-show-buffer
  "k" #'lispm-desktop-buffer-manager-close-buffer
  "d" #'lispm-desktop-buffer-manager-close-buffer
  "<delete>" #'lispm-desktop-buffer-manager-close-buffer
  "g" #'lispm-desktop-buffer-manager-refresh)

(defun lispm-desktop-buffer-manager--columns (width)
  "Return the manager's column headings as (NAME PIXELS SORTED), WIDTH wide,
NAME the column's English name.  They line up with the columns
`tabulated-list-format' sets, the first taking the padding, the last
running on to the sort-direction button."
  (let* ((char (frame-char-width))
         (columns (append tabulated-list-format nil))
         (sorted (car tabulated-list-sort-key))
         (used 0)
         (cells (seq-map-indexed
                 (pcase-lambda (`(,name ,size . ,_) index)
                   (let ((pixels (* char (+ size 1 (if (= index 0) tabulated-list-padding 0)))))
                     (setq used (+ used pixels))
                     (list name pixels (equal name sorted))))
                 columns)))
    (setcar (cdr (car (last cells))) (+ (cadr (car (last cells))) (max 0 (- width used 16))))
    cells))

(defvar-keymap lispm-desktop-buffer-manager-header-map
  :doc "Keymap of the manager's column headings."
  "<header-line> <mouse-1>" #'lispm-desktop-buffer-manager-header-click)

(defun lispm-desktop-buffer-manager--header ()
  "Return the manager's header line: Mac OS 9's column headings, the column
sorted by pushed in.  Clicking a heading sorts by its column, again to
turn the order over.  The headings are drawn in the language LispM
speaks; the columns keep their English names, which sorting goes by."
  (let* ((width (window-body-width nil t))
         (columns (lispm-desktop-buffer-manager--columns width))
         (shown (mapcar (pcase-lambda (`(,name . ,rest)) (cons (lispm-desktop-tr name) rest)) columns)))
    (propertize " " 'display
                (lispm-ui-image
                 (list 'manager-header shown)
                 (lambda () (lispm-desktop-platinum-draw-list-header lispm-ui-current-palette
                                                                     shown
                                                                     (* (frame-char-width)
                                                                        (1+ tabulated-list-padding)))))
                'lispm-desktop-buffer-manager-columns columns
                'keymap lispm-desktop-buffer-manager-header-map
                'pointer 'hand 'help-echo (lispm-desktop-tr "Sort by this column"))))

(defun lispm-desktop-buffer-manager-header-click (event)
  "Sort the manager by the column whose heading EVENT clicked."
  (interactive "e")
  (let* ((position (event-start event))
         (x (car (posn-object-x-y position)))
         (columns (get-text-property (cdr (posn-string position)) 'lispm-desktop-buffer-manager-columns
                                     (car (posn-string position))))
         (left 0))
    (with-current-buffer (window-buffer (posn-window position))
      (when-let* ((column (seq-find (pcase-lambda (`(,_ ,pixels ,_))
                                      (prog1 (< x (+ left pixels)) (setq left (+ left pixels))))
                                    columns))
                  (name (car column))
                  ((nth 2 (assoc name (append tabulated-list-format nil)))))
        (setq tabulated-list-sort-key (cons name (and (equal name (car tabulated-list-sort-key))
                                                      (not (cdr tabulated-list-sort-key)))))
        (tabulated-list-print t)))))

(defun lispm-desktop-buffer-manager--keep-header (&rest _)
  "Give the manager's header back its drawn headings after
`tabulated-list-init-header' set its own, as sorting from the keyboard does."
  (when (derived-mode-p 'lispm-desktop-buffer-manager-mode)
    (setq header-line-format '(:eval (lispm-desktop-buffer-manager--header)))))

(define-derived-mode lispm-desktop-buffer-manager-mode tabulated-list-mode "Buffer Manager"
  "List the open buffers with what each costs: text, CPU and memory.
\\{lispm-desktop-buffer-manager-mode-map}"
  ;; The columns' names are English, translated where the header draws
  ;; them, see `lispm-desktop-buffer-manager--header'.
  (setq tabulated-list-format
        (vector (list (lispm-desktop-tr-noop "Name") 34 t)
                (list (lispm-desktop-tr-noop "Kind") 11 t)
                (list (lispm-desktop-tr-noop "Text") 9 (lispm-desktop-buffer-manager--by 2) :right-align t)
                (list (lispm-desktop-tr-noop "Process") 20 t)
                (list (lispm-desktop-tr-noop "CPU") 7 (lispm-desktop-buffer-manager--by 4) :right-align t)
                (list (lispm-desktop-tr-noop "Memory") 18 (lispm-desktop-buffer-manager--by 5)))
        tabulated-list-padding 1
        tabulated-list-sort-key '("Memory" . t))
  (setq-local lispm-desktop-platinum-window-title (lambda () (lispm-desktop-tr "Buffer Manager"))
              lispm-ui-icon (lispm-ui-icon "computer"))
  ;; One line per buffer, cut at the window's edge rather than wrapped;
  ;; turning visual line mode off restores the lines it found, so it goes
  ;; first.
  (visual-line-mode -1)
  (setq-local truncate-lines t)
  (face-remap-add-relative 'default 'lispm-ui-list)
  (face-remap-add-relative 'header-line '(:box nil :underline nil :overline nil))
  (advice-add 'tabulated-list-init-header :after #'lispm-desktop-buffer-manager--keep-header)
  (tabulated-list-init-header))

(defun lispm-desktop-buffer-manager ()
  "Show the buffer manager: every open buffer, and what each costs."
  (interactive)
  (let ((buffer (get-buffer-create lispm-desktop-buffer-manager-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lispm-desktop-buffer-manager-mode)
        (lispm-desktop-buffer-manager-mode)))
    (lispm-desktop-sidebar-show-in-main buffer)
    (let ((listed lispm-desktop-buffer-manager--processes))
      (lispm-desktop-buffer-manager-refresh)
      ;; Shown at once as ps listed them last, which may be long ago.
      (when listed
        (lispm-desktop-buffer-manager--tick)))
    (unless lispm-desktop-buffer-manager--timer
      (setq lispm-desktop-buffer-manager--timer
            (run-with-timer lispm-desktop-buffer-manager-interval lispm-desktop-buffer-manager-interval
                            #'lispm-desktop-buffer-manager--tick)))
    buffer))

(provide 'lispm-desktop-buffer-manager)
;;; lispm-desktop-buffer-manager.el ends here
