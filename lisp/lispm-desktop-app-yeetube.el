;;; lispm-desktop-app-yeetube.el --- Yeetube on the desktop  -*- lexical-binding: t; -*-

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

;; Yeetube, the App Store's YouTube application, as it opens on the
;; desktop, beyond what lispm-desktop-app-ui.el finds out of it:
;;
;;   window   titled Yeetube, opening on a page saying how to begin
;;            rather than asking at once what to search, wide enough for
;;            its columns; its videos in the list face under their
;;            columns' names, the line point is on highlighted, point on
;;            the first video once they come, a row too long cut clean; a
;;            video double-clicked plays, as Return plays it
;;   buttons  Search, and Play Saved once videos are saved, until
;;            something is searched; then Search, Play, Download, Save
;;            and More Results, each dimmed while it can do nothing
;;   asking   in a sheet dropping from under the strip, over the list,
;;            rather than in the minibuffer along the screen's bottom;
;;            Search clicked again while it asks gives it the keyboard
;;   status   what was searched, how many videos and playlists came, and
;;            what mpv is playing, on the window's bottom
;;   menus    a Video menu of everything Yeetube does, with its keys
;;
;; Yeetube fetches its results in the background, then shows them in
;; the selected window: they are shown in its own window instead, where
;; the search was made, rather than in whatever window is selected once
;; they come.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'url-parse)
(require 'url-util)
(require 'lispm-desktop-app-ui)

;; Yeetube's own variables and functions, read once it runs.
(defvar yeetube--buffer-name)
(defvar yeetube--current-url)
(defvar yeetube--continuation)
(defvar yeetube-items)
(defvar yeetube-saved-videos)
(defvar yeetube-history)
(defvar yeetube-settings-map)
(defvar yeetube-mpv--process-name)
(defvar yeetube-display-thumbnails-p)
(defvar yeetube-thumbnail-size)
(declare-function yeetube-mode "yeetube" ())
(declare-function yeetube-search "yeetube" (query))
(declare-function yeetube-play "yeetube" ())
(declare-function keymap-popup "keymap-popup" (keymap))

;;;; Opening it

(defconst lispm-desktop-app-yeetube-width 860
  "Least width in pixels of Yeetube's window as it opens: Yeetube sizes its
columns from the window's width, and in a narrower window they run past
its edge beside their thumbnails.")

(defun lispm-desktop-app-yeetube ()
  "Open Yeetube's list of videos, on the page saying how to begin while
nothing was searched, rather than asking at once what to search; its
window wide enough for its columns, where the desktop has room."
  (interactive)
  (require 'yeetube)
  (switch-to-buffer (get-buffer-create yeetube--buffer-name))
  (unless (derived-mode-p 'yeetube-mode)
    (yeetube-mode))
  (when (lispm-desktop-app-ui-app)
    (let* ((frame (selected-frame))
           (desktop (frame-parent frame))
           (width (if desktop
                      (min lispm-desktop-app-yeetube-width (- (frame-inner-width desktop) 80))
                    lispm-desktop-app-yeetube-width)))
      (when (< (frame-text-width frame) width)
        (set-frame-size frame width (frame-text-height frame) t)))))

(defun lispm-desktop-app-yeetube--setup ()
  "Give Yeetube's list its window's keys, see
`lispm-desktop-app-yeetube-window-mode', and name its columns over them,
in the buffer: the strip of buttons takes the window's header line,
where Yeetube names them.  A row longer
than the window is cut at its edge, without the dollar sign a window
without fringes shows there."
  (lispm-desktop-app-yeetube-window-mode 1)
  (setq-local tabulated-list-use-header-line nil)
  (let ((table (copy-sequence (or buffer-display-table standard-display-table (make-display-table)))))
    (set-display-table-slot table 'truncation ?\s)
    (setq-local buffer-display-table table))
  (when tabulated-list-entries
    (tabulated-list-init-header)
    (tabulated-list-print t)))

;;;; What it shows

(defun lispm-desktop-app-yeetube--video ()
  "Return the id of the video or playlist point is on, or nil."
  (and (derived-mode-p 'yeetube-mode) (tabulated-list-get-id)))

(defun lispm-desktop-app-yeetube--listed-p ()
  "Return non-nil when the list shows videos, not Yeetube's Loading."
  (or (get-text-property (point-min) 'tabulated-list-id)
      (next-single-property-change (point-min) 'tabulated-list-id)))

(defun lispm-desktop-app-yeetube--searched-p ()
  "Return non-nil once the list was asked for videos: searched, or a
channel's listed."
  (bound-and-true-p yeetube--current-url))

(defun lispm-desktop-app-yeetube--more-p ()
  "Return non-nil when YouTube has more results than those listed."
  (and (bound-and-true-p yeetube--continuation) (lispm-desktop-app-yeetube--listed-p)))

(defun lispm-desktop-app-yeetube--playing ()
  "Return the title mpv is playing, or t when it plays one without, or nil."
  (when-let* (((boundp 'yeetube-mpv--process-name))
              (process (get-process yeetube-mpv--process-name)))
    (or (process-get process :now-playing) t)))

(defvar lispm-desktop-app-yeetube--saved nil
  "Whether videos were saved, as last read: (FILE TIME . SAVED).")

(defun lispm-desktop-app-yeetube--saved-p ()
  "Return non-nil when videos were saved to watch later, in the file where
Yeetube keeps them; read again only once the file changed, as the strip
asks at every redisplay."
  (let* ((file (locate-user-emacs-file "yeetube"))
         (time (file-attribute-modification-time (file-attributes file))))
    (unless (and (equal (car lispm-desktop-app-yeetube--saved) file)
                 (equal (cadr lispm-desktop-app-yeetube--saved) time))
      (setq lispm-desktop-app-yeetube--saved
            (cons file (cons time (and time
                                       (ignore-errors
                                         (with-temp-buffer
                                           (insert-file-contents file)
                                           (read (current-buffer))))
                                       t)))))
    (cddr lispm-desktop-app-yeetube--saved)))

(defun lispm-desktop-app-yeetube-query (address)
  "Return what the list fetched from ADDRESS shows, as its status says it:
the words searched, quoted, those searched in a channel, or a channel's
videos or streams; nil for an address of no such list.  Its %XX are
unhexed into bytes, read as UTF-8."
  (when address
    (pcase-let* ((`(,path . ,query) (url-path-and-query (url-generic-parse-url address)))
                 (fields (and query (url-parse-query-string
                                     (encode-coding-string (string-replace "+" "%20" query) 'utf-8))))
                 (words (when-let* ((field (or (assoc "q" fields) (assoc "query" fields))))
                          (decode-coding-string (cadr field) 'utf-8)))
                 (channel (when (and path (string-match "\\`/\\(channel/[^/]+\\|[^/]+\\)/\\([a-z]+\\)" path))
                            (let ((name (match-string 1 path))
                                  (list (match-string 2 path)))
                              (cons (decode-coding-string (url-unhex-string (encode-coding-string name 'utf-8))
                                                          'utf-8)
                                    list)))))
      (cond ((and words channel) (format "“%s” in %s" words (car channel)))
            (words (format "“%s”" words))
            (channel (format "%s, %s" (car channel) (capitalize (cdr channel))))))))

(defun lispm-desktop-app-yeetube-count (items)
  "Return how many videos and playlists ITEMS, Yeetube's, are, in words."
  (let ((videos (seq-count (lambda (item) (eq (plist-get item :type) 'video)) items))
        (playlists (seq-count (lambda (item) (eq (plist-get item :type) 'playlist)) items))
        (words (lambda (count one) (format "%d %s%s" count one (if (= count 1) "" "s")))))
    (cond ((and (zerop videos) (zerop playlists)) "No videos")
          ((zerop playlists) (funcall words videos "video"))
          ((zerop videos) (funcall words playlists "playlist"))
          (t (concat (funcall words videos "video") ", " (funcall words playlists "playlist"))))))

(defun lispm-desktop-app-yeetube--status ()
  "Return what Yeetube's bottom says: what was searched, how many came,
and what is playing."
  (let ((playing (lispm-desktop-app-yeetube--playing)))
    (append (when-let* ((query (lispm-desktop-app-yeetube-query
                                (lispm-desktop-app-yeetube--searched-p))))
              (list query (if (lispm-desktop-app-yeetube--listed-p)
                              (lispm-desktop-app-yeetube-count (bound-and-true-p yeetube-items))
                            "Searching…")))
            (when playing
              (list (if (stringp playing) (concat "Playing: " playing) "Playing"))))))

;;;; Showing the results where they were asked for

(defun lispm-desktop-app-yeetube--render-in-place (render &rest arguments)
  "Show Yeetube's results, RENDER called with ARGUMENTS, in the window
already showing its list, rather than in the window selected once they
come; and put point on the first of them, under the columns' names.
This is the advice of `yeetube--render-items'."
  (let* ((buffer (get-buffer yeetube--buffer-name))
         (window (and buffer (if (eq (window-buffer) buffer)
                                 (selected-window)
                               (get-buffer-window buffer t)))))
    (if (window-live-p window)
        (with-selected-window window (apply render arguments))
      (apply render arguments))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (unless (tabulated-list-get-id)
          (when-let* ((first (next-single-property-change (point-min) 'tabulated-list-id)))
            (goto-char first)
            (when (window-live-p window)
              (set-window-point window first))))))))

(defconst lispm-desktop-app-yeetube--stand-in-width (length "[[01234567890.jpg]]")
  "Width in columns of the stand-in Yeetube writes for a thumbnail until it
comes, as [[ID.jpg]], an id of YouTube's 11 characters.")

(defun lispm-desktop-app-yeetube--whole-stand-ins (format)
  "Return FORMAT, of Yeetube's list, its column of thumbnails as wide as their
stand-ins: one cut to the column is never found again once its thumbnail
comes, and its cut name stays in the list.
This is the advice of `yeetube-ui--tabulated-list-format'."
  (when (and (> (length format) 0) (equal (car (aref format 0)) "Thumbnail"))
    (setf (nth 1 (aref format 0)) (max (nth 1 (aref format 0)) lispm-desktop-app-yeetube--stand-in-width)))
  format)

(defun lispm-desktop-app-yeetube--blank-stand-in (row)
  "Return ROW, of Yeetube's list, its thumbnail's stand-in shown as blank
room the thumbnail's size until the thumbnail comes, rather than as the
name of its file.  This is the advice of `yeetube-ui--entry-to-row'."
  (let ((cells (cadr row)))
    (when (and (bound-and-true-p yeetube-display-thumbnails-p) (vectorp cells) (> (length cells) 0)
               (stringp (aref cells 0)) (string-prefix-p "[[" (aref cells 0))
               (not (get-text-property 0 'display (aref cells 0))))
      (pcase-let ((`(,width . ,height) (bound-and-true-p yeetube-thumbnail-size)))
        (aset cells 0 (propertize (aref cells 0) 'display
                                  `(space :width (,(or width 120)) :height (,(or height 90))))))))
  row)

(with-eval-after-load 'yeetube
  (advice-add 'yeetube--render-items :around #'lispm-desktop-app-yeetube--render-in-place)
  (advice-add 'yeetube-ui--tabulated-list-format :filter-return #'lispm-desktop-app-yeetube--whole-stand-ins)
  (advice-add 'yeetube-ui--entry-to-row :filter-return #'lispm-desktop-app-yeetube--blank-stand-in))

;;;; Asking in a sheet over the window
;;
;; Yeetube asks in the minibuffer, which is the desktop's, along the
;; screen's bottom, far from its window.  Asked from its window, its
;; questions drop instead in a sheet from under its strip, as Mac OS X's
;; sheets dropped from their windows' title bars: a field, the question
;; over it with Cancel and the button answering it.  The field is the
;; minibuffer of a frame of its own, as lispm-desktop-coherence.el gives
;; the windows out theirs, so typing, input methods, history and
;; completion work as anywhere; it takes no focus, keys typed in
;; Yeetube's window going to it while it asks.

(defconst lispm-desktop-app-yeetube-questions
  '((yeetube-read-query "Search YouTube for:" "Search")
    (yeetube--read-channel-id "Show the videos of the channel:" "Show")
    (yeetube-save-video "Save the video to watch later as:" "Save")
    (yeetube-play-saved-video "Play the saved video:" "Play")
    (yeetube-replay "Play again:" "Play")
    (yeetube-remove-saved-video "Remove the saved video:" "Remove")
    (yeetube-set-results-limit "List this many results:" "OK")
    (yeetube--read-video-quality "Play videos at this height:" "OK")
    (yeetube-set-download-directory "Download into the folder:" "OK")
    (yeetube--read-audio-format "Download the sound alone, as:" "OK"))
  "Yeetube's functions asking questions, as (FUNCTION QUESTION BUTTON):
from Yeetube's window each asks in its sheet, QUESTION over the field,
BUTTON answering it.")

(defvar lispm-desktop-app-yeetube--sheet nil
  "The frame of Yeetube's sheet, its field its minibuffer.")

(defvar lispm-desktop-app-yeetube--asking nil
  "What Yeetube's sheet asks while it asks: (QUESTION BUTTON SHEET WINDOW),
WINDOW Yeetube's, which it drops over.")

(defconst lispm-desktop-app-yeetube--sheet-buffer-name " *Yeetube Sheet*"
  "Name of the buffer of the line over the field of Yeetube's sheet.")

(defconst lispm-desktop-app-yeetube-sheet-width 520
  "Width in pixels of Yeetube's sheet, where its window has room.")

(defun lispm-desktop-app-yeetube-sheet-answer ()
  "Answer the question of Yeetube's sheet with what its field holds, as
Return does there."
  (interactive)
  (when-let* ((field (active-minibuffer-window)))
    (with-selected-window field
      (call-interactively (key-binding (kbd "RET"))))))

(defun lispm-desktop-app-yeetube-sheet-cancel ()
  "Take back the question of Yeetube's sheet, as C-g does."
  (interactive)
  (when-let* ((field (active-minibuffer-window)))
    (with-selected-window field
      (if (fboundp 'abort-minibuffers) (abort-minibuffers) (abort-recursive-edit)))))

(defvar-keymap lispm-desktop-app-yeetube-sheet-map
  :doc "Keys of the field of Yeetube's sheet, beside the minibuffer's:
Escape cancels, as in Mac OS's dialogs."
  "<escape>" #'lispm-desktop-app-yeetube-sheet-cancel)

(defun lispm-desktop-app-yeetube--sheet-button (label command default)
  "Return a push button of the sheet reading LABEL, running COMMAND when
clicked, ringed as the one Return presses when DEFAULT."
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<down-mouse-1>" #'ignore)
    (keymap-set map "<mouse-1>" command)
    (propertize (lispm-desktop-platinum-push-button label :default default)
                'keymap map 'mouse-face nil 'help-echo label)))

(defun lispm-desktop-app-yeetube-sheet-line (question button)
  "Return the line of Yeetube's sheet over its field: QUESTION, then Cancel
and BUTTON, the one Return presses, at its right.  Their width is that of
their drawings, rings included, and a column more: a window without
fringes keeps its last column for the sign of a line cut."
  (let ((width (+ (lispm-desktop-platinum-push-button-width "Cancel") (* 2 2) 8
                  (lispm-desktop-platinum-push-button-width button) (* 2 5)
                  (frame-char-width))))
    (concat (propertize " " 'display '(space :width (10)))
            (propertize question 'face 'lispm-desktop-platinum-title)
            (propertize " " 'display `(space :align-to (- right (,width))))
            (lispm-desktop-app-yeetube--sheet-button "Cancel" #'lispm-desktop-app-yeetube-sheet-cancel nil)
            (propertize " " 'display '(space :width (8)))
            (lispm-desktop-app-yeetube--sheet-button button #'lispm-desktop-app-yeetube-sheet-answer t))))

(defun lispm-desktop-app-yeetube--sheet-buffer ()
  "Return the buffer of the line over the field of Yeetube's sheet."
  (or (get-buffer lispm-desktop-app-yeetube--sheet-buffer-name)
      (with-current-buffer (get-buffer-create lispm-desktop-app-yeetube--sheet-buffer-name)
        (setq-local mode-line-format nil header-line-format nil tab-line-format nil
                    cursor-type nil cursor-in-non-selected-windows nil truncate-lines t
                    buffer-read-only t line-spacing 6
                    ;; Its lines are all it needs.
                    window-min-height 1)
        (current-buffer))))

(defun lispm-desktop-app-yeetube--paint-sheet (sheet)
  "Give SHEET the colours of the palette in use: the field in the list's
colour, about it the chrome's, outlined in the text's."
  (let ((chrome (lispm-desktop-platinum-color :chrome))
        (field (lispm-desktop-platinum-color :content))
        (text (lispm-desktop-platinum-color :text)))
    (when chrome
      (set-face-attribute 'internal-border sheet :background chrome)
      (with-current-buffer (lispm-desktop-app-yeetube--sheet-buffer)
        (setq-local face-remapping-alist `((default :background ,chrome)))))
    (when text
      (set-face-attribute 'child-frame-border sheet :background text))
    (when field
      (set-face-attribute 'default sheet :background field)
      (set-face-attribute 'fringe sheet :background field))))

(defun lispm-desktop-app-yeetube--sheet (frame)
  "Return Yeetube's sheet over FRAME, its window, made when missing."
  (let ((sheet lispm-desktop-app-yeetube--sheet))
    (unless (and (frame-live-p sheet) (eq (frame-parent sheet) frame))
      (when (frame-live-p sheet)
        (delete-frame sheet t))
      (setq sheet (let ((after-make-frame-functions nil)
                        (before-make-frame-hook nil))
                    (make-frame
                     `((minibuffer . t) (parent-frame . ,frame) (name . "Yeetube Sheet")
                       (undecorated . t) (visibility . nil) (unsplittable . t)
                       ;; Keys typed in Yeetube's window come to the field;
                       ;; shown, the sheet takes no focus from that window.
                       (no-accept-focus . t) (no-focus-on-map . t)
                       (no-other-frame . t) (skip-taskbar . t) (desktop-dont-save . t)
                       (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
                       (internal-border-width . 10) (child-frame-border-width . 1)
                       (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
                       (left-fringe . 0) (right-fringe . 0)
                       (width . 60) (height . 3))))
            lispm-desktop-app-yeetube--sheet sheet)
      (let ((window (frame-root-window sheet)))
        (set-window-buffer window (lispm-desktop-app-yeetube--sheet-buffer))
        (set-window-dedicated-p window t)
        (set-window-fringes window 0 0)))
    (lispm-desktop-app-yeetube--paint-sheet sheet)
    sheet))

(defun lispm-desktop-app-yeetube--fit-sheet (&rest _)
  "Make Yeetube's sheet as tall as its line and its field: Emacs grows the
field to what it shows, the candidates of a completion under it
included, taking the room from the line, which this gives back.
This is a `post-command-hook' and `window-size-change-functions'
function of the field while it asks."
  (when-let* ((sheet (nth 2 lispm-desktop-app-yeetube--asking))
              ((frame-live-p sheet)))
    (let ((height (+ (cdr (window-text-pixel-size (frame-root-window sheet) nil nil 4000 4000))
                     (window-pixel-height (minibuffer-window sheet)))))
      (unless (= (frame-text-height sheet) height)
        (set-frame-height sheet height nil t)))))

(defun lispm-desktop-app-yeetube--place-sheet (sheet window)
  "Put SHEET over WINDOW, Yeetube's, centred under its strip, as wide as
`lispm-desktop-app-yeetube-sheet-width' where the window has room."
  (let* ((frame (window-frame window))
         (border (* 2 (+ (frame-parameter sheet 'internal-border-width)
                         (frame-parameter sheet 'child-frame-border-width))))
         (width (min lispm-desktop-app-yeetube-sheet-width (- (frame-inner-width frame) border 32))))
    (set-frame-width sheet width nil t)
    (set-frame-position sheet (max 0 (/ (- (frame-inner-width frame) width border) 2))
                        (+ (window-pixel-top window) (window-tab-line-height window)
                           (window-header-line-height window)))))

(defun lispm-desktop-app-yeetube--sheet-setup ()
  "Drop Yeetube's sheet, its question over its field, when the field is read
from.  This is a `minibuffer-setup-hook' function."
  (pcase-let ((`(,question ,button ,sheet ,window) lispm-desktop-app-yeetube--asking))
    (when (and (frame-live-p sheet) (eq (selected-window) (minibuffer-window sheet)))
      (with-current-buffer (lispm-desktop-app-yeetube--sheet-buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          ;; Room over the line and under it, the sheet's own border a
          ;; pixel of outline.
          (insert (propertize " " 'display '(space :height (6))) "\n"
                  (lispm-desktop-app-yeetube-sheet-line question button))))
      (use-local-map (make-composed-keymap lispm-desktop-app-yeetube-sheet-map (current-local-map)))
      ;; The question over the field says what Yeetube's prompt says.
      (let ((prompt (make-overlay (point-min) (minibuffer-prompt-end))))
        (overlay-put prompt 'invisible t)
        (overlay-put prompt 'evaporate t))
      (setq-local line-prefix (propertize " " 'display '(space :width (6))))
      (add-hook 'post-command-hook #'lispm-desktop-app-yeetube--fit-sheet 90 t)
      (add-hook 'window-size-change-functions #'lispm-desktop-app-yeetube--fit-sheet nil t)
      (lispm-desktop-app-yeetube--place-sheet sheet window)
      (lispm-desktop-app-yeetube--fit-sheet)
      (make-frame-visible sheet)
      ;; Once a completion has drawn its candidates.
      (run-at-time 0 nil #'lispm-desktop-app-yeetube--fit-sheet))))

(defun lispm-desktop-app-yeetube--keep-field ()
  "Give the field of Yeetube's sheet the keyboard back after a command left
it in Yeetube's window, as a click in the list does: a sheet holds its
window's keys while it asks, rather than letting them run Yeetube's
commands.  This is a `post-command-hook' function while the sheet asks."
  (pcase-let ((`(,_ ,_ ,sheet ,window) lispm-desktop-app-yeetube--asking))
    (when-let* (((frame-live-p sheet))
                (field (active-minibuffer-window))
                ((eq field (minibuffer-window sheet)))
                ((memq (selected-window) (list window (frame-root-window sheet)))))
      (select-window field))))

(defun lispm-desktop-app-yeetube-ask (question button function)
  "Return what FUNCTION returns, the question it reads from the minibuffer
asked in Yeetube's sheet, QUESTION over the field and BUTTON answering,
when it asks from Yeetube's window; elsewhere it asks as it would.
Asked while another question is open, that one gets the keyboard."
  (let* ((window (selected-window))
         (frame (window-frame window)))
    (cond ((not (and (eq (lispm-desktop-app-ui-app window) 'yeetube)
                     (display-graphic-p frame)
                     ;; A frame of its own minibuffer keeps it.
                     (not (eq (window-frame (minibuffer-window frame)) frame))))
           (funcall function))
          ((active-minibuffer-window)
           (select-window (active-minibuffer-window))
           (user-error "Answer the question asked first"))
          (t
           (let ((sheet (lispm-desktop-app-yeetube--sheet frame))
                 (minibuffer (minibuffer-window frame)))
             (setq lispm-desktop-app-yeetube--asking (list question button sheet window))
             (unwind-protect
                 (progn
                   (set-frame-parameter frame 'minibuffer (minibuffer-window sheet))
                   (add-hook 'post-command-hook #'lispm-desktop-app-yeetube--keep-field)
                   (funcall function))
               (remove-hook 'post-command-hook #'lispm-desktop-app-yeetube--keep-field)
               (setq lispm-desktop-app-yeetube--asking nil)
               (when (frame-live-p frame)
                 (set-frame-parameter frame 'minibuffer minibuffer))
               (when (frame-live-p sheet)
                 (make-frame-invisible sheet t))))))))

(with-eval-after-load 'yeetube
  (pcase-dolist (`(,function ,question ,button) lispm-desktop-app-yeetube-questions)
    (advice-add function :around
                (lambda (ask &rest arguments)
                  (lispm-desktop-app-yeetube-ask question button (lambda () (apply ask arguments))))
                '((name . lispm-desktop-app-yeetube-ask))))
  (add-hook 'minibuffer-setup-hook #'lispm-desktop-app-yeetube--sheet-setup))

;;;; Searching, and playing with the mouse

(defun lispm-desktop-app-yeetube-search ()
  "Search YouTube, asking what for in Yeetube's sheet; while a question is
open, give its field the keyboard instead, as clicking a window's
button again would."
  (interactive)
  (if-let* ((field (active-minibuffer-window)))
      (select-window field)
    (call-interactively #'yeetube-search)))

(defun lispm-desktop-app-yeetube-play-clicked (event)
  "Play the video double-clicked, as Finder opened the item double-clicked."
  (interactive "e")
  (mouse-set-point event)
  (when (lispm-desktop-app-yeetube--video)
    (call-interactively #'yeetube-play)))

(defvar-keymap lispm-desktop-app-yeetube-window-mode-map
  :doc "Keys of Yeetube's list in its window: Search asks in the window's
sheet, or gives an open one the keyboard; a video double-clicked plays."
  "<remap> <yeetube-search>" #'lispm-desktop-app-yeetube-search
  "<double-down-mouse-1>" #'ignore
  "<double-mouse-1>" #'lispm-desktop-app-yeetube-play-clicked)

(define-minor-mode lispm-desktop-app-yeetube-window-mode
  "Yeetube's list as its window on the desktop has it.
\\{lispm-desktop-app-yeetube-window-mode-map}"
  :lighter nil)

;;;; Its window

(defun lispm-desktop-app-yeetube-settings ()
  "Show Yeetube's settings: quality, results, where downloads go."
  (interactive)
  (keymap-popup yeetube-settings-map))

(defun lispm-desktop-app-yeetube--menu-items ()
  "Return the items of Yeetube's Video menu."
  (let ((video (lispm-desktop-app-yeetube--video))
        (playing (lispm-desktop-app-yeetube--playing))
        (saved (lispm-desktop-app-yeetube--saved-p)))
    `((:label "Play" :action yeetube-play :enabled ,(and video t))
      (:label "Play Again…" :action yeetube-replay :enabled ,(and (bound-and-true-p yeetube-history) t))
      (:label "Play Saved…" :action yeetube-play-saved-video :enabled ,(and saved t))
      separator
      (:label "Pause or Resume" :action yeetube-mpv-toggle-pause :enabled ,(and playing t))
      (:label "Show or Hide Video" :action yeetube-mpv-toggle-video :enabled ,(and playing t))
      (:label "Stop" :action yeetube-mpv-quit :enabled ,(and playing t))
      separator
      (:label "Search…" :action lispm-desktop-app-yeetube-search :key "s")
      (:label "More Results" :action yeetube-next-page
       :enabled ,(and (lispm-desktop-app-yeetube--more-p) t))
      (:label "Channel’s Videos" :action yeetube-channel-videos :enabled ,(and video t))
      (:label "Channel’s Streams" :action yeetube-channel-streams :enabled ,(and video t))
      separator
      (:label "Download" :action yeetube-download-video :enabled ,(and video t))
      (:label "Save to Watch Later…" :action yeetube-save-video :enabled ,(and video t))
      (:label "Remove Saved…" :action yeetube-remove-saved-video :enabled ,(and saved t))
      (:label "Copy URL" :action yeetube-copy-url :enabled ,(and video t))
      (:label "Copy RSS Feed URL" :action yeetube-copy-rss-feed-url :enabled ,(and video t))
      (:label "Open in Browser" :action yeetube-browse-url :enabled ,(and video t))
      separator
      (:label "Settings…" :action lispm-desktop-app-yeetube-settings :key "o"))))

(defun lispm-desktop-app-yeetube--empty-buttons ()
  "Return the buttons of Yeetube's page while nothing was searched: Search,
and Play Saved once a video is saved."
  `(("Search…" lispm-desktop-app-yeetube-search)
    ,@(when (lispm-desktop-app-yeetube--saved-p)
        '(("Play Saved…" yeetube-play-saved-video)))))

(defun lispm-desktop-app-yeetube--buttons ()
  "Return the push buttons of Yeetube's strip: those of its page while
nothing was searched, rather than buttons that could do nothing yet;
then those of its list, each dimmed while it can do nothing."
  (if (lispm-desktop-app-yeetube--searched-p)
      '(("Search…" lispm-desktop-app-yeetube-search)
        ("Play" yeetube-play lispm-desktop-app-yeetube--video)
        ("Download" yeetube-download-video lispm-desktop-app-yeetube--video)
        ("Save…" yeetube-save-video lispm-desktop-app-yeetube--video)
        ("More Results" yeetube-next-page lispm-desktop-app-yeetube--more-p))
    (lispm-desktop-app-yeetube--empty-buttons)))

(lispm-desktop-app-ui-define 'yeetube-mode
  :app 'yeetube
  :run #'lispm-desktop-app-yeetube
  :title "Yeetube"
  :list t
  :setup #'lispm-desktop-app-yeetube--setup
  :status #'lispm-desktop-app-yeetube--status
  :buttons #'lispm-desktop-app-yeetube--buttons
  :menu '("Video" . lispm-desktop-app-yeetube--menu-items)
  :empty `(,(lambda () (not (lispm-desktop-app-yeetube--searched-p)))
           ,(concat "Search YouTube for videos.\nThey play in mpv, download with yt-dlp,\n"
                    "and can be saved to watch later.")
           lispm-desktop-app-yeetube--empty-buttons)
  :keys '(("s" . "Search YouTube") ("C-n  C-p" . "Next and previous video")
          ("Return" . "Play the video, as a double click does") ("d" . "Download it") ("S" . "Save it to watch later")
          ("P" . "Play a saved video") ("r" . "Play one played before")
          ("M-n" . "More results") ("c  L" . "The channel’s videos, streams")
          ("p  v" . "Pause, show or hide the video") ("C" . "Copy its URL")
          ("b" . "Open it in the browser") ("o" . "Settings") ("h" . "All of Yeetube’s commands")
          ("Esc" . "Take back a question asked over the list")))

(provide 'lispm-desktop-app-yeetube)
;;; lispm-desktop-app-yeetube.el ends here
