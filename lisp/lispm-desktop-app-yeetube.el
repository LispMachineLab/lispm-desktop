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
;; desktop: its window works as youtube.com does, the pages drawn by
;; lispm-desktop-app-yeetube-page.el, rather than as Yeetube's table.
;;
;;   window   titled Yeetube, opening on its home page rather than
;;            asking at once what to search, wide enough for its pages
;;   search   in the masthead's search box: a click on it, s or /, and
;;            the field opens in it; Return searches, Escape gives up
;;   results  a click on a video, or Return, plays it in the window, on
;;            its watch page; the chips sort the results, the button
;;            under them shows more
;;   watch    the video in YouTube's own player; Download, Save and
;;            Share under it, the channel's name showing its videos;
;;            Up next below; q, Delete or the back arrow go back
;;   asking   Yeetube's other questions, a name to save a video as, a
;;            saved video to play, its settings, in a dialog dropping
;;            from under the masthead, as YouTube's own dialogs
;;   menus    a Video menu of everything Yeetube does, with its keys
;;
;; Yeetube fetches its results in the background, then shows them in
;; the selected window: they are shown in its own window instead, where
;; the search was made.  Every way Yeetube plays a video, Return, a
;; saved video, one played before, plays it on the watch page.

;;; Code:

(require 'face-remap)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'url-parse)
(require 'url-util)
(require 'lispm-desktop-local-http)
(require 'lispm-desktop-app-ui)
(require 'lispm-desktop-app-yeetube-page)

;; Yeetube's own variables and functions, read once it runs.
(defvar yeetube--buffer-name)
(defvar yeetube--current-url)
(defvar yeetube--continuation)
(defvar yeetube-items)
(defvar yeetube-filter)
(defvar yeetube-filter-code-alist)
(defvar yeetube-history)
(defvar yeetube-play-function)
(defvar yeetube-settings-map)
(defvar yeetube-display-thumbnails-p)
(defvar yeetube-thumbnail-size)
(defvar global-hl-line-overlay)
(declare-function yeetube-mode "yeetube" ())
(declare-function yeetube-search "yeetube" (query))
(declare-function yeetube--find-item "yeetube" (id))
(declare-function yeetube-mpv-play "yeetube-mpv" (input &optional info))
(declare-function yeetube-next-page "yeetube" ())
(declare-function yeetube-channel-videos "yeetube" (&optional channel-id))
(declare-function yeetube-download-video "yeetube" (&optional url))
(declare-function yeetube-save-video "yeetube" (arg))
(declare-function yeetube-copy-url "yeetube" ())
(declare-function keymap-popup "keymap-popup" (keymap))
(declare-function kill-xwidget "xwidget" (xwidget))
(declare-function make-xwidget "xwidget.c" (type title width height arguments &optional buffer related))
(declare-function xwidget-live-p "xwidget.c" (object))
(declare-function xwidget-resize "xwidget.c" (xwidget new-width new-height))
(declare-function xwidget-webkit-goto-uri "xwidget.c" (xwidget uri))
(declare-function xwidget-put "xwidget.c" (xwidget propname value))
(declare-function set-xwidget-query-on-exit-flag "xwidget.c" (xwidget flag))
(declare-function xwidget-get "xwidget.c" (xwidget propname))

;; Defined below.
(defvar lispm-desktop-app-yeetube-window-mode)
(defvar lispm-desktop-app-yeetube-row-map)
(defvar lispm-desktop-app-yeetube--asking)

;;;; The state of its window

(defvar-local lispm-desktop-app-yeetube--view nil
  "The page Yeetube's window shows: `home', `loading', `results' or `watch'.")

(defvar-local lispm-desktop-app-yeetube--watching nil
  "The video, or playlist, the watch page plays: an item of Yeetube's.")

(defvar-local lispm-desktop-app-yeetube--player nil
  "The watch page's player: its xwidget, (mpv . ID) while mpv plays ID in
its stead, or nil.")

(defvar-local lispm-desktop-app-yeetube--selected nil
  "The marker of the row drawn shaded, point on it.")

(defvar-local lispm-desktop-app-yeetube--width nil
  "The width in pixels the page was drawn for.")

(defvar-local lispm-desktop-app-yeetube--loading-more nil
  "Non-nil while more results are fetched.")

(defvar-local lispm-desktop-app-yeetube--background nil
  "The cookie of the remapping giving the window YouTube's colours.")

;;;; Opening it

(defconst lispm-desktop-app-yeetube-width 860
  "Least width in pixels of Yeetube's window as it opens, room for a page of
results beside their thumbnails.")

(defun lispm-desktop-app-yeetube ()
  "Open Yeetube's window on its home page, rather than asking at once what
to search; wide enough for its pages, where the desktop has room."
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
  "Make Yeetube's buffer its window's pages: their keys, see
`lispm-desktop-app-yeetube-window-mode', the masthead over them, videos
played on the watch page, and the page it is on drawn."
  (lispm-desktop-app-yeetube-window-mode 1)
  ;; Yeetube's table, drawn under the page, needs no thumbnails nor the
  ;; header line, which the masthead takes.
  (setq-local tabulated-list-use-header-line nil
              yeetube-display-thumbnails-p nil
              yeetube-play-function #'lispm-desktop-app-yeetube-watch-url
              header-line-format '(:eval (lispm-desktop-app-yeetube--masthead))
              revert-buffer-function (lambda (&rest _) (lispm-desktop-app-yeetube-render))
              line-spacing 0
              truncate-lines t)
  ;; The page shades the row point is on itself.
  (setq-local global-hl-line-mode nil)
  (when (overlayp (bound-and-true-p global-hl-line-overlay))
    (delete-overlay global-hl-line-overlay))
  (when (bound-and-true-p hl-line-mode)
    (hl-line-mode -1))
  (add-hook 'post-command-hook #'lispm-desktop-app-yeetube--follow nil t)
  (add-hook 'window-size-change-functions #'lispm-desktop-app-yeetube--resized nil t)
  (unless lispm-desktop-app-yeetube--view
    (setq lispm-desktop-app-yeetube--view (if (bound-and-true-p yeetube-items) 'results 'home)))
  (lispm-desktop-app-yeetube-render))

;;;; What it shows

(defun lispm-desktop-app-yeetube--video ()
  "Return the id of the video or playlist point is on, or nil."
  (and (derived-mode-p 'yeetube-mode) (tabulated-list-get-id)))

(defun lispm-desktop-app-yeetube--searched-p ()
  "Return non-nil once the list was asked for videos: searched, or a
channel's listed."
  (bound-and-true-p yeetube--current-url))

(defun lispm-desktop-app-yeetube--more-p ()
  "Return non-nil when YouTube has more results than those listed."
  (and (bound-and-true-p yeetube--continuation) (eq lispm-desktop-app-yeetube--view 'results)))

(defvar lispm-desktop-app-yeetube--saved nil
  "Whether videos were saved, as last read: (FILE TIME . SAVED).")

(defun lispm-desktop-app-yeetube--saved-p ()
  "Return non-nil when videos were saved to watch later, in the file where
Yeetube keeps them; read again only once the file changed, as menus ask
each time they drop."
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

(defun lispm-desktop-app-yeetube--fields (address)
  "Return the path of ADDRESS and the fields of its query, (PATH . FIELDS),
the fields' %XX unhexed into bytes read as UTF-8."
  (pcase-let ((`(,path . ,query) (url-path-and-query (url-generic-parse-url address))))
    (cons path
          (mapcar (lambda (field) (list (car field) (decode-coding-string (cadr field) 'utf-8)))
                  (and query (url-parse-query-string
                              (encode-coding-string (string-replace "+" "%20" query) 'utf-8)))))))

(defun lispm-desktop-app-yeetube-words (address)
  "Return the words searched for by ADDRESS, Yeetube's, or nil for an
address of no search."
  (when address
    (pcase-let ((`(,_ . ,fields) (lispm-desktop-app-yeetube--fields address)))
      (cadr (or (assoc "q" fields) (assoc "query" fields))))))

(defun lispm-desktop-app-yeetube-filter (address)
  "Return the value of `yeetube-filter' ADDRESS, Yeetube's, sorts by, or
nil."
  (when address
    (pcase-let ((`(,_ . ,fields) (lispm-desktop-app-yeetube--fields address)))
      (when-let* ((code (cadr (assoc "sp" fields))))
        (car (rassoc (url-hexify-string code) (bound-and-true-p yeetube-filter-code-alist)))))))

;;;; Showing the results where they were asked for

(defun lispm-desktop-app-yeetube--render-in-place (render &rest arguments)
  "Show Yeetube's results, RENDER called with ARGUMENTS, in the window
already showing its list, rather than in the window selected once they
come; on the results page in Yeetube's window.
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
        (if lispm-desktop-app-yeetube-window-mode
            (progn
              (lispm-desktop-app-yeetube--stop)
              (setq lispm-desktop-app-yeetube--view 'results)
              (lispm-desktop-app-yeetube-render))
          (unless (tabulated-list-get-id)
            (when-let* ((first (next-single-property-change (point-min) 'tabulated-list-id)))
              (goto-char first)
              (when (window-live-p window)
                (set-window-point window first)))))))))

(defun lispm-desktop-app-yeetube--loading (&rest _)
  "Show the results page as YouTube does while it searches.
This is the advice of `yeetube--display-loading'."
  (with-current-buffer yeetube--buffer-name
    (when lispm-desktop-app-yeetube-window-mode
      (lispm-desktop-app-yeetube--stop)
      (setq lispm-desktop-app-yeetube--view 'loading)
      (lispm-desktop-app-yeetube-render))))

(defun lispm-desktop-app-yeetube--appended (&rest _)
  "Draw the results page again once more results came, point where it was.
This is the advice of `yeetube-ui-append'."
  (when (and lispm-desktop-app-yeetube-window-mode (eq lispm-desktop-app-yeetube--view 'results))
    (setq lispm-desktop-app-yeetube--loading-more nil)
    (lispm-desktop-app-yeetube-render t)))

(defconst lispm-desktop-app-yeetube--stand-in-width (length "[[01234567890.jpg]]")
  "Width in columns of the stand-in Yeetube writes for a thumbnail until it
comes, as [[ID.jpg]], an id of YouTube's 11 characters.")

(defun lispm-desktop-app-yeetube--whole-stand-ins (format)
  "Return FORMAT, of Yeetube's table, its column of thumbnails as wide as
their stand-ins: one cut to the column is never found again once its
thumbnail comes, and its cut name stays in the table, as Yeetube shows it
outside its window.  This is the advice of
`yeetube-ui--tabulated-list-format'."
  (when (and (> (length format) 0) (equal (car (aref format 0)) "Thumbnail"))
    (setf (nth 1 (aref format 0)) (max (nth 1 (aref format 0)) lispm-desktop-app-yeetube--stand-in-width)))
  format)

(defun lispm-desktop-app-yeetube--blank-stand-in (row)
  "Return ROW, of Yeetube's table, its thumbnail's stand-in shown as blank
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
  (advice-add 'yeetube--display-loading :after #'lispm-desktop-app-yeetube--loading)
  (advice-add 'yeetube-ui-append :after #'lispm-desktop-app-yeetube--appended)
  (advice-add 'yeetube-ui--tabulated-list-format :filter-return #'lispm-desktop-app-yeetube--whole-stand-ins)
  (advice-add 'yeetube-ui--entry-to-row :filter-return #'lispm-desktop-app-yeetube--blank-stand-in))

;;;; The pages in the buffer
;;
;; Each row of a page is a line whose text is its title, displayed as
;; the row's drawing; the line carries the row, (:kind KIND :item ITEM
;; ...), and a video's `tabulated-list-id'.

(defvar lispm-desktop-app-yeetube--images (make-hash-table :test #'equal)
  "The rows drawn: (KIND ID WIDTH SELECTED THUMBNAIL DARK) to their image.")

(defun lispm-desktop-app-yeetube--window ()
  "Return the window showing Yeetube's buffer, the selected one first."
  (if (eq (window-buffer) (current-buffer))
      (selected-window)
    (get-buffer-window (current-buffer) t)))

(defun lispm-desktop-app-yeetube--page-width ()
  "Return the width in pixels pages are drawn for, the window's."
  (if-let* ((window (lispm-desktop-app-yeetube--window)))
      (window-body-width window t)
    (or lispm-desktop-app-yeetube--width 820)))

(defun lispm-desktop-app-yeetube--draw (row width selected)
  "Return (IMAGE . HITS) of ROW drawn WIDTH pixels wide, shaded when
SELECTED, HITS where its buttons are, see the page's drawings."
  (let* ((item (plist-get row :item))
         (kind (plist-get row :kind))
         (key (list kind (plist-get item :id) width selected
                    (stringp (gethash (plist-get item :id) lispm-desktop-app-yeetube-page--thumbnails))
                    (eq (lispm-desktop-app-yeetube-page-colors) lispm-desktop-app-yeetube-page-dark)
                    (plist-get row :state))))
    (or (and (memq kind '(result compact)) (gethash key lispm-desktop-app-yeetube--images))
        (let* ((drawn (pcase kind
                        ('result (list (lispm-desktop-app-yeetube-page-result item width selected)))
                        ('compact (list (lispm-desktop-app-yeetube-page-compact item width selected)))
                        ('skeleton (list (lispm-desktop-app-yeetube-page-skeleton width)))
                        ('heading (list (lispm-desktop-app-yeetube-page-heading width (plist-get row :text))))
                        ('chips (lispm-desktop-app-yeetube-page-chips width (plist-get row :state)))
                        ('more (lispm-desktop-app-yeetube-page-more width (plist-get row :state)))
                        ('info (lispm-desktop-app-yeetube-page-info item width))
                        ('message (lispm-desktop-app-yeetube-page-message
                                   width (plist-get row :title) (plist-get row :text) (plist-get row :button)))))
               (image (cons (svg-image (car drawn) :ascent 'center) (cdr drawn))))
          (when (memq kind '(result compact))
            (puthash key image lispm-desktop-app-yeetube--images))
          image))))

(defun lispm-desktop-app-yeetube--insert-row (row text width)
  "Insert ROW as a line of the page, its text TEXT, drawn WIDTH pixels wide."
  (let* ((row (copy-sequence row))
         (start (point))
         (drawn (lispm-desktop-app-yeetube--draw row width nil))
        (id (and (memq (plist-get row :kind) '(result compact info))
                 (plist-get (plist-get row :item) :id))))
    (insert (if (string-empty-p (or text "")) " " (replace-regexp-in-string "\n" " " text)))
    (put-text-property start (point) 'display (car drawn))
    (insert "\n")
    (add-text-properties start (point)
                         `(lispm-desktop-app-yeetube-row ,(plist-put row :hits (cdr drawn))
                           keymap ,lispm-desktop-app-yeetube-row-map
                           ,@(when id (list 'tabulated-list-id id))
                           help-echo ,(and (plist-get row :item) (plist-get (plist-get row :item) :title))))))


(defun lispm-desktop-app-yeetube--position (id)
  "Return the start of the first row of the video of ID, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found) (not (eobp)))
        (if (equal (get-text-property (point) 'tabulated-list-id) id)
            (setq found (point))
          (forward-line 1)))
      found)))

(defun lispm-desktop-app-yeetube--row (&optional position)
  "Return the row at POSITION, by default point's, or nil."
  (get-text-property (or position (point)) 'lispm-desktop-app-yeetube-row))

(defun lispm-desktop-app-yeetube--redraw (start selected)
  "Draw the row of the line at START again, shaded when SELECTED."
  (when-let* ((row (lispm-desktop-app-yeetube--row start)))
    (save-excursion
      (goto-char start)
      (let ((drawn (lispm-desktop-app-yeetube--draw row lispm-desktop-app-yeetube--width selected)))
        (with-silent-modifications
          (put-text-property (line-beginning-position) (line-end-position) 'display (car drawn))
          (put-text-property (line-beginning-position) (min (point-max) (1+ (line-end-position)))
                             'lispm-desktop-app-yeetube-row (plist-put row :hits (cdr drawn))))))))

(defun lispm-desktop-app-yeetube--selectable-p (row)
  "Return non-nil when ROW is one point shades, a video's."
  (memq (plist-get row :kind) '(result compact)))

(defun lispm-desktop-app-yeetube--follow ()
  "Shade the row point is on, and no other, as youtube.com shades the video
under the mouse.  This is a `post-command-hook' function of the pages."
  (let* ((row (lispm-desktop-app-yeetube--row))
         (start (and (lispm-desktop-app-yeetube--selectable-p row) (line-beginning-position)))
         (old (and lispm-desktop-app-yeetube--selected (marker-position lispm-desktop-app-yeetube--selected))))
    (unless (eql start old)
      (when old
        (lispm-desktop-app-yeetube--redraw old nil))
      (if start
          (progn
            (lispm-desktop-app-yeetube--redraw start t)
            (setq lispm-desktop-app-yeetube--selected (copy-marker start)))
        (setq lispm-desktop-app-yeetube--selected nil)))))

(defun lispm-desktop-app-yeetube--insert-player (item width)
  "Insert the watch page's player of ITEM, playing it, for a page WIDTH
pixels wide: YouTube's own, through WebKit; without WebKit, ITEM plays in
mpv, its thumbnail in the player's place."
  (pcase-let* ((window (lispm-desktop-app-yeetube--window))
               (`(,w . ,h) (lispm-desktop-app-yeetube-page-player-size
                            width (if window (window-body-height window t) 600)))
               (start (point)))
    (insert (propertize " " 'display `(space :align-to (,(round (- (/ width 2.0) (/ w 2.0)))))))
    (if (not (featurep 'xwidget-internal))
        (progn
          (insert (propertize " " 'display
                              (svg-image (let ((svg (svg-create w h)))
                                           (lispm-desktop-app-yeetube-page--thumbnail-at
                                            svg item 0 0 w h 12 (lispm-desktop-app-yeetube-page-colors))
                                           svg))))
          (unless (equal lispm-desktop-app-yeetube--player (cons 'mpv (plist-get item :id)))
            (yeetube-mpv-play (lispm-desktop-app-yeetube--url item) (plist-get item :title))
            (setq lispm-desktop-app-yeetube--player (cons 'mpv (plist-get item :id)))))
      (require 'xwidget)
      (let ((url (lispm-desktop-app-yeetube--player-url item))
            (player lispm-desktop-app-yeetube--player))
        (if (and player (xwidget-live-p player))
            (progn
              (xwidget-resize player w h)
              (unless (equal (xwidget-get player 'lispm-desktop-app-yeetube-url) url)
                (xwidget-webkit-goto-uri player url)))
          (setq player (make-xwidget 'webkit "Yeetube" w h nil (current-buffer)))
          ;; Killing the buffer, or Emacs, need not ask about it.
          (set-xwidget-query-on-exit-flag player nil)
          ;; Its page loaded before the player shows in the window shows
          ;; nothing of YouTube's: it is loaded once the window shows it.
          (let ((new player))
            (run-at-time 0.3 nil (lambda ()
                                   (when (xwidget-live-p new)
                                     (xwidget-webkit-goto-uri new url))))))
        (xwidget-put player 'lispm-desktop-app-yeetube-url url)
        (setq lispm-desktop-app-yeetube--player player)
        (insert (propertize " " 'display (list 'xwidget :xwidget player)))))
    (insert "\n")
    (add-text-properties start (point) `(lispm-desktop-app-yeetube-row (:kind player :item ,item)
                                         ,@(when (yeetube--find-item (plist-get item :id))
                                             (list 'tabulated-list-id (plist-get item :id)))))))

(defun lispm-desktop-app-yeetube-render (&optional keep)
  "Draw the page Yeetube's window shows anew, see
`lispm-desktop-app-yeetube--view'; point on the video it was on when
KEEP, else on the page's first."
  (let* ((width (lispm-desktop-app-yeetube--page-width))
         (keep (and keep (lispm-desktop-app-yeetube--video)))
         (items (bound-and-true-p yeetube-items))
         (inhibit-read-only t))
    (setq lispm-desktop-app-yeetube--width width
          lispm-desktop-app-yeetube--selected nil)
    ;; Round the player, as between rows, the window in the page's colours.
    (let ((colors (lispm-desktop-app-yeetube-page-colors)))
      (when lispm-desktop-app-yeetube--background
        (face-remap-remove-relative lispm-desktop-app-yeetube--background))
      (setq lispm-desktop-app-yeetube--background
            (face-remap-add-relative 'default :background (plist-get colors :background)
                                     :foreground (plist-get colors :text))))
    (with-silent-modifications
      (erase-buffer)
      (pcase lispm-desktop-app-yeetube--view
        ('loading
         (dotimes (_ 4)
           (lispm-desktop-app-yeetube--insert-row '(:kind skeleton) "" width)))
        ('results
         (lispm-desktop-app-yeetube--insert-row
          (list :kind 'chips :state (or (lispm-desktop-app-yeetube-filter (lispm-desktop-app-yeetube--searched-p))
                                        (bound-and-true-p yeetube-filter)))
          "Sort" width)
         (if items
             (dolist (item items)
               (lispm-desktop-app-yeetube--insert-row (list :kind 'result :item item) (plist-get item :title) width))
           (lispm-desktop-app-yeetube--insert-row
            '(:kind message :title "No results found" :text "Try different words, or fewer of them.")
            "No results found" width))
         (when (bound-and-true-p yeetube--continuation)
           (lispm-desktop-app-yeetube--insert-row (list :kind 'more :state lispm-desktop-app-yeetube--loading-more)
                                                  "Show more results" width)))
        ('watch
         (let ((item lispm-desktop-app-yeetube--watching))
           (lispm-desktop-app-yeetube--insert-player item width)
           (lispm-desktop-app-yeetube--insert-row (list :kind 'info :item item) (plist-get item :title) width)
           (let ((others (seq-remove (lambda (other) (equal (plist-get other :id) (plist-get item :id))) items)))
             (when others
               (lispm-desktop-app-yeetube--insert-row '(:kind heading :text "Up next") "Up next" width)
               (dolist (other others)
                 (lispm-desktop-app-yeetube--insert-row (list :kind 'compact :item other)
                                                        (plist-get other :title) width))))))
        (_
         (lispm-desktop-app-yeetube--insert-row
          '(:kind message :title "Search to get started"
                  :text "Find videos on YouTube; they play right here, in this window."
                  :button "Search")
          "Search to get started" width))))
    (goto-char (point-min))
    (if-let* ((at (and keep (lispm-desktop-app-yeetube--position keep))))
        (goto-char at)
      (when-let* ((first (seq-find (lambda (position)
                                     (lispm-desktop-app-yeetube--selectable-p
                                      (lispm-desktop-app-yeetube--row position)))
                                   (let (starts)
                                     (save-excursion
                                       (goto-char (point-min))
                                       (while (not (eobp))
                                         (push (point) starts)
                                         (forward-line 1)))
                                     (nreverse starts)))))
        (unless (eq lispm-desktop-app-yeetube--view 'watch)
          (goto-char first))))
    (when-let* ((window (lispm-desktop-app-yeetube--window)))
      (set-window-point window (point))
      (unless keep
        (set-window-start window (point-min))))
    (lispm-desktop-app-yeetube--follow)
    (force-mode-line-update)))

(defun lispm-desktop-app-yeetube--resized (window)
  "Draw the page again for WINDOW's new width.
This is a `window-size-change-functions' function of the pages."
  (with-current-buffer (window-buffer window)
    (unless (eql (window-body-width window t) lispm-desktop-app-yeetube--width)
      (lispm-desktop-app-yeetube-render t))))

(defun lispm-desktop-app-yeetube--thumbnail-came (id)
  "Draw again the rows of Yeetube's window showing the video of ID, its
thumbnail come.  This is one of
`lispm-desktop-app-yeetube-page-thumbnail-functions'."
  (when-let* ((buffer (and (boundp 'yeetube--buffer-name) (get-buffer yeetube--buffer-name))))
    (with-current-buffer buffer
      (when lispm-desktop-app-yeetube-window-mode
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((row (lispm-desktop-app-yeetube--row)))
              (when (equal (plist-get (plist-get row :item) :id) id)
                (lispm-desktop-app-yeetube--redraw
                 (point) (and lispm-desktop-app-yeetube--selected
                              (= (point) lispm-desktop-app-yeetube--selected)))))
            (forward-line 1)))))))

(add-hook 'lispm-desktop-app-yeetube-page-thumbnail-functions #'lispm-desktop-app-yeetube--thumbnail-came)

;;;; Watching

(defvar lispm-desktop-app-yeetube--server nil
  "The server of the player's page, on this machine.")

(defun lispm-desktop-app-yeetube-serve (method path _headers _body)
  "Answer the request METHOD PATH of the player's page: /watch/ID or
/playlist/ID, the page of the player of that video or playlist; nothing
else.  This is the handler of `lispm-desktop-app-yeetube--server'."
  (if (and (equal method "GET")
           (string-match "\\`/\\(watch\\|playlist\\)/\\([A-Za-z0-9_-]+\\)\\'" path))
      (list "200 OK"
            (encode-coding-string
             (lispm-desktop-app-yeetube-page-player-html
              (list :id (match-string 2 path) :type (if (equal (match-string 1 path) "playlist") 'playlist 'video)))
             'utf-8)
            "text/html; charset=utf-8")
    (list "404 Not Found")))

(defun lispm-desktop-app-yeetube--player-url (item)
  "Return the address of the page of ITEM's player, served on this machine,
its server started the first time."
  (unless (process-live-p lispm-desktop-app-yeetube--server)
    (setq lispm-desktop-app-yeetube--server
          (lispm-desktop-local-http-serve "lispm-desktop-yeetube-player" #'always
                                          #'lispm-desktop-app-yeetube-serve)))
  (format "http://127.0.0.1:%d/%s/%s" (process-contact lispm-desktop-app-yeetube--server :service)
          (if (eq (plist-get item :type) 'playlist) "playlist" "watch")
          (url-hexify-string (plist-get item :id))))

(defun lispm-desktop-app-yeetube--url (item)
  "Return the address of ITEM on YouTube."
  (format (if (eq (plist-get item :type) 'playlist)
              "https://youtube.com/playlist?list=%s"
            "https://youtube.com/watch?v=%s")
          (plist-get item :id)))

(defun lispm-desktop-app-yeetube-watch (item)
  "Play ITEM, an item of Yeetube's, on the watch page in Yeetube's window."
  (setq lispm-desktop-app-yeetube--watching item
        lispm-desktop-app-yeetube--view 'watch)
  (lispm-desktop-app-yeetube-render))

(defun lispm-desktop-app-yeetube-watch-url (url &optional title)
  "Play the video or playlist at URL, of TITLE, on the watch page; one of
no address YouTube's player plays, in mpv.  This is Yeetube's
`yeetube-play-function' in its window, so that whichever way it plays a
video, the video plays there."
  (let* ((fields (cdr (lispm-desktop-app-yeetube--fields url)))
         (video (cadr (assoc "v" fields)))
         (list (cadr (assoc "list" fields)))
         (id (or video list)))
    (cond ((or (null id) (equal id "nil"))
           (yeetube-mpv-play url title))
          (t
           (with-current-buffer yeetube--buffer-name
             (lispm-desktop-app-yeetube-watch
              (or (yeetube--find-item id)
                  (list :id id :title (or title "") :type (if video 'video 'playlist)))))))
    nil))

(defun lispm-desktop-app-yeetube--stop ()
  "Stop the watch page's player: its xwidget is killed, or mpv quits."
  (let ((player lispm-desktop-app-yeetube--player))
    (cond ((consp player)
           (when-let* ((process (get-process "yeetube-mpv")))
             (delete-process process)))
          (player
           (ignore-errors (kill-xwidget player)))))
  (setq lispm-desktop-app-yeetube--player nil))

(defun lispm-desktop-app-yeetube--watching-p ()
  "Return non-nil while Yeetube's window plays a video."
  (eq lispm-desktop-app-yeetube--view 'watch))

(defun lispm-desktop-app-yeetube-back ()
  "Leave the watch page for the results, the video stopped, point on it."
  (interactive)
  (when (lispm-desktop-app-yeetube--watching-p)
    (let ((id (plist-get lispm-desktop-app-yeetube--watching :id)))
      (lispm-desktop-app-yeetube--stop)
      (setq lispm-desktop-app-yeetube--view (if (bound-and-true-p yeetube-items) 'results 'home))
      (lispm-desktop-app-yeetube-render)
      (when-let* ((at (lispm-desktop-app-yeetube--position id)))
        (goto-char at)
        (when-let* ((window (lispm-desktop-app-yeetube--window)))
          (set-window-point window at))
        (lispm-desktop-app-yeetube--follow)))))

(defun lispm-desktop-app-yeetube-home ()
  "Go to Yeetube's home page, as YouTube's logo does, the video stopped."
  (interactive)
  (lispm-desktop-app-yeetube--stop)
  (setq lispm-desktop-app-yeetube--view 'home)
  (lispm-desktop-app-yeetube-render))

(defun lispm-desktop-app-yeetube--hidden (&optional closing)
  "Stop the watch page's video once no window shows Yeetube's buffer but
those of CLOSING, a frame being deleted, as its window closing: it would
play on unseen.  This is one of `window-buffer-change-functions' and
`delete-frame-functions'."
  (when-let* ((buffer (and (boundp 'yeetube--buffer-name) (get-buffer yeetube--buffer-name))))
    (with-current-buffer buffer
      (when (and lispm-desktop-app-yeetube--player
                 (not (seq-find (lambda (window)
                                  (not (and (frame-live-p closing) (eq (window-frame window) closing))))
                                (get-buffer-window-list buffer nil t))))
        (lispm-desktop-app-yeetube--stop)
        (setq lispm-desktop-app-yeetube--view (if (bound-and-true-p yeetube-items) 'results 'home))
        (lispm-desktop-app-yeetube-render)))))

(add-hook 'window-buffer-change-functions (lambda (_) (lispm-desktop-app-yeetube--hidden)))
(add-hook 'delete-frame-functions #'lispm-desktop-app-yeetube--hidden)

;;;; Acting on the pages

(defun lispm-desktop-app-yeetube-sort (filter)
  "Search again for what was searched, sorted as FILTER says, a value of
`yeetube-filter'."
  (if-let* ((words (lispm-desktop-app-yeetube-words (lispm-desktop-app-yeetube--searched-p))))
      (let ((yeetube-filter filter))
        (yeetube-search words))
    (user-error "Search first")))

(defun lispm-desktop-app-yeetube-more ()
  "Show more results under those listed."
  (interactive)
  (when (bound-and-true-p yeetube--continuation)
    (setq lispm-desktop-app-yeetube--loading-more t)
    (save-excursion
      (goto-char (point-max))
      (forward-line -1)
      (when (eq (plist-get (lispm-desktop-app-yeetube--row) :kind) 'more)
        (plist-put (lispm-desktop-app-yeetube--row) :state t)
        (lispm-desktop-app-yeetube--redraw (point) nil)))
    (call-interactively #'yeetube-next-page)))

(defun lispm-desktop-app-yeetube--act (row action)
  "Do ACTION of ROW, at point: a button of it, or what the row is for."
  (pcase action
    ('search (lispm-desktop-app-yeetube-search))
    ('more (lispm-desktop-app-yeetube-more))
    ('channel (call-interactively #'yeetube-channel-videos))
    ('download (call-interactively #'yeetube-download-video))
    ('save (call-interactively #'yeetube-save-video))
    ('share (call-interactively #'yeetube-copy-url))
    ((pred stringp) (lispm-desktop-app-yeetube-sort action))
    (_ (pcase (plist-get row :kind)
         ((or 'result 'compact) (lispm-desktop-app-yeetube-watch (plist-get row :item)))
         ('more (lispm-desktop-app-yeetube-more))
         ('message (when (plist-get row :button) (lispm-desktop-app-yeetube-search)))))))

(defun lispm-desktop-app-yeetube--hit (hits x y)
  "Return the action of HITS, ((ACTION X Y WIDTH HEIGHT) ...), at X, Y."
  (car (seq-find (pcase-lambda (`(,_ ,left ,top ,width ,height))
                   (and (<= left x (+ left width)) (<= top y (+ top height))))
                 hits)))

(defun lispm-desktop-app-yeetube-click (event)
  "Do what is clicked on a page: a button of a row, or what the row is
for, as playing a video.  While a question is open, its field keeps the
keyboard instead."
  (interactive "e")
  (if-let* ((field (active-minibuffer-window)))
      (select-window field)
    (mouse-set-point event)
    (when-let* ((row (lispm-desktop-app-yeetube--row)))
      (pcase-let ((`(,x . ,y) (or (posn-object-x-y (event-start event)) '(0 . 0))))
        (lispm-desktop-app-yeetube--act row (lispm-desktop-app-yeetube--hit (plist-get row :hits) x y))))))

(defun lispm-desktop-app-yeetube-activate ()
  "Do what the row point is on is for: play its video, show more results,
or search."
  (interactive)
  (when-let* ((row (lispm-desktop-app-yeetube--row)))
    (lispm-desktop-app-yeetube--act row nil)))

(defvar-keymap lispm-desktop-app-yeetube-row-map
  :doc "Keys of a row of Yeetube's pages: a click does what it is for."
  "<down-mouse-1>" #'ignore
  "<mouse-1>" #'lispm-desktop-app-yeetube-click
  "<double-down-mouse-1>" #'ignore
  "<double-mouse-1>" #'ignore)

;;;; The masthead

(defun lispm-desktop-app-yeetube--searching-p ()
  "Return non-nil while the search box's field is open."
  (eq (nth 4 lispm-desktop-app-yeetube--asking) 'search))

(defun lispm-desktop-app-yeetube-masthead-click (event)
  "Do what is clicked in the masthead: go back, home, or search."
  (interactive "e")
  (pcase-let* ((posn (event-start event))
               (`(,x . ,y) (or (posn-object-x-y posn) '(0 . 0)))
               (window (posn-window posn)))
    (with-selected-window window
      (pcase (lispm-desktop-app-yeetube--hit
              (mapcar (pcase-lambda (`(,part . ,rect)) (cons part rect))
                      (lispm-desktop-app-yeetube-page-masthead-layout
                       (window-pixel-width window) (lispm-desktop-app-yeetube--watching-p)))
              x y)
        ('back (lispm-desktop-app-yeetube-back))
        ('logo (lispm-desktop-app-yeetube-home))
        ('input (lispm-desktop-app-yeetube-search))
        ('button (if (lispm-desktop-app-yeetube--searching-p)
                     (lispm-desktop-app-yeetube-sheet-answer)
                   (lispm-desktop-app-yeetube-search)))))))

(defvar-keymap lispm-desktop-app-yeetube-masthead-map
  :doc "Keys of the masthead of Yeetube's pages."
  "<header-line> <down-mouse-1>" #'ignore
  "<header-line> <mouse-1>" #'lispm-desktop-app-yeetube-masthead-click)

(defvar lispm-desktop-app-yeetube--masthead nil
  "The masthead last drawn: (WHAT . STRING), WHAT what it shows.")

(defun lispm-desktop-app-yeetube--masthead ()
  "Return the masthead over Yeetube's pages, as its window's header line;
drawn again only once what it shows changed, as the header line is read
at every redisplay."
  (let ((what (list (window-pixel-width)
                    (lispm-desktop-app-yeetube-words (lispm-desktop-app-yeetube--searched-p))
                    (lispm-desktop-app-yeetube--searching-p)
                    (lispm-desktop-app-yeetube--watching-p)
                    (lispm-desktop-app-yeetube-page-colors))))
    (unless (equal what (car lispm-desktop-app-yeetube--masthead))
      (setq lispm-desktop-app-yeetube--masthead
            (cons what (propertize " " 'display (svg-image (apply #'lispm-desktop-app-yeetube-page-masthead
                                                                  (take 4 what))
                                                           :ascent 'center)
                                   'keymap lispm-desktop-app-yeetube-masthead-map))))
    (cdr lispm-desktop-app-yeetube--masthead)))

;;;; Asking over the window
;;
;; Yeetube asks in the minibuffer, which is the desktop's, along the
;; screen's bottom, far from its window.  Asked from its window, what to
;; search is asked in the masthead's search box, its field opening in
;; it; its other questions in a dialog dropping from under the masthead,
;; the question, Cancel and the button answering it over a field, as
;; YouTube's dialogs.  Each field is the minibuffer of a frame of its
;; own, as lispm-desktop-coherence.el gives the windows out theirs, so
;; typing, input methods, history and completion work as anywhere; it
;; takes no focus, keys typed in Yeetube's window going to it while it
;; asks.

(defconst lispm-desktop-app-yeetube-questions
  '((yeetube-read-query "Search YouTube for:" "Search" search)
    (yeetube--read-channel-id "Show the videos of the channel:" "Show")
    (yeetube-save-video "Save the video to watch later as:" "Save")
    (yeetube-play-saved-video "Play the saved video:" "Play")
    (yeetube-replay "Play again:" "Play")
    (yeetube-remove-saved-video "Remove the saved video:" "Remove")
    (yeetube-set-results-limit "List this many results:" "OK")
    (yeetube--read-video-quality "Play videos at this height:" "OK")
    (yeetube-set-download-directory "Download into the folder:" "OK")
    (yeetube--read-audio-format "Download the sound alone, as:" "OK"))
  "Yeetube's functions asking questions, as (FUNCTION QUESTION BUTTON
STYLE): from Yeetube's window each asks in its dialog, QUESTION over the
field, BUTTON answering it; or, STYLE `search', in the search box.")

(defvar lispm-desktop-app-yeetube--sheet nil
  "The frame of Yeetube's dialog, its field its minibuffer.")

(defvar lispm-desktop-app-yeetube--search-field nil
  "The frame of the field of the masthead's search box, a minibuffer.")

(defvar lispm-desktop-app-yeetube--asking nil
  "What Yeetube asks while it asks: (QUESTION BUTTON FRAME WINDOW STYLE),
FRAME the dialog's or the search box's, WINDOW Yeetube's, over which it
asks.")

(defconst lispm-desktop-app-yeetube--sheet-buffer-name " *Yeetube Sheet*"
  "Name of the buffer of the line over the field of Yeetube's dialog.")

(defconst lispm-desktop-app-yeetube-sheet-width 520
  "Width in pixels of Yeetube's dialog, where its window has room.")

(defun lispm-desktop-app-yeetube-sheet-answer ()
  "Answer the question asked with what its field holds, as Return does there."
  (interactive)
  (when-let* ((field (active-minibuffer-window)))
    (with-selected-window field
      (call-interactively (key-binding (kbd "RET"))))))

(defun lispm-desktop-app-yeetube-sheet-cancel ()
  "Take back the question asked, as C-g does."
  (interactive)
  (when-let* ((field (active-minibuffer-window)))
    (with-selected-window field
      (if (fboundp 'abort-minibuffers) (abort-minibuffers) (abort-recursive-edit)))))

(defvar-keymap lispm-desktop-app-yeetube-sheet-map
  :doc "Keys of the field of a question of Yeetube's, beside the
minibuffer's: Escape cancels, as in YouTube's dialogs."
  "<escape>" #'lispm-desktop-app-yeetube-sheet-cancel)

(defun lispm-desktop-app-yeetube--dialog-button (label command primary)
  "Return a button of the dialog reading LABEL, running COMMAND when
clicked: YouTube's blue one when PRIMARY, else its grey one."
  (let* ((colors (lispm-desktop-app-yeetube-page-colors))
         (width (+ 32 (funcall (lispm-desktop-app-yeetube-page-measure 14 'medium) label)))
         (svg (svg-create width 36))
         (map (make-sparse-keymap)))
    (svg-rectangle svg 0 0 width 36 :rx 18 :fill (plist-get colors (if primary :primary :hover)))
    (lispm-desktop-app-yeetube-page--text svg label (/ width 2.0) 23 14
                                          (plist-get colors (if primary :primary-text :text))
                                          :text-anchor "middle" :font-weight "500")
    (keymap-set map "<down-mouse-1>" #'ignore)
    (keymap-set map "<mouse-1>" command)
    (propertize " " 'display (svg-image svg :ascent 'center) 'keymap map 'mouse-face nil 'help-echo label)))

(defun lispm-desktop-app-yeetube-sheet-line (question button)
  "Return the line of Yeetube's dialog over its field: QUESTION, then Cancel
and BUTTON, the one Return presses, at its right.  Their width is that of
their drawings and a column more: a window without fringes keeps its last
column for the sign of a line cut."
  (let* ((measure (lispm-desktop-app-yeetube-page-measure 14 'medium))
         (width (+ 32 (funcall measure "Cancel") 8 32 (funcall measure button) (frame-char-width))))
    (concat (propertize " " 'display '(space :width (16)))
            (propertize question 'face (list :family (lispm-desktop-app-yeetube-page--family) :height 160
                                             :foreground (plist-get (lispm-desktop-app-yeetube-page-colors) :text)))
            (propertize " " 'display `(space :align-to (- right (,width))))
            (lispm-desktop-app-yeetube--dialog-button "Cancel" #'lispm-desktop-app-yeetube-sheet-cancel nil)
            (propertize " " 'display '(space :width (8)))
            (lispm-desktop-app-yeetube--dialog-button button #'lispm-desktop-app-yeetube-sheet-answer t))))

(defun lispm-desktop-app-yeetube--sheet-buffer ()
  "Return the buffer of the line over the field of Yeetube's dialog."
  (or (get-buffer lispm-desktop-app-yeetube--sheet-buffer-name)
      (with-current-buffer (get-buffer-create lispm-desktop-app-yeetube--sheet-buffer-name)
        (setq-local mode-line-format nil header-line-format nil tab-line-format nil
                    cursor-type nil cursor-in-non-selected-windows nil truncate-lines t
                    buffer-read-only t line-spacing 8
                    ;; Its lines are all it needs.
                    window-min-height 1)
        (current-buffer))))

(defun lispm-desktop-app-yeetube--paint (frame)
  "Give FRAME, the dialog's or the search box's, YouTube's colours and font."
  (let ((colors (lispm-desktop-app-yeetube-page-colors)))
    (set-face-attribute 'default frame :family (lispm-desktop-app-yeetube-page--family) :height 160
                        :background (plist-get colors :background) :foreground (plist-get colors :text))
    (set-face-attribute 'internal-border frame :background (plist-get colors :background))
    (set-face-attribute 'child-frame-border frame :background (plist-get colors :border))
    (set-face-attribute 'fringe frame :background (plist-get colors :background))))

(defun lispm-desktop-app-yeetube--make-frame (parent minibuffer name border)
  "Return a frame over PARENT, Yeetube's window, invisible until asked:
of its own minibuffer, its only window when MINIBUFFER is `only'; named
NAME, its border BORDER pixels wide."
  (let ((after-make-frame-functions nil)
        (before-make-frame-hook nil))
    (make-frame
     `((minibuffer . ,minibuffer) (parent-frame . ,parent) (name . ,name)
       (undecorated . t) (visibility . nil) (unsplittable . t)
       ;; Keys typed in Yeetube's window come to the field; shown, the
       ;; frame takes no focus from that window.
       (no-accept-focus . t) (no-focus-on-map . t)
       (no-other-frame . t) (skip-taskbar . t) (desktop-dont-save . t)
       (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
       (child-frame-border-width . ,border)
       (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
       (left-fringe . 0) (right-fringe . 0)
       (width . 60) (height . ,(if (eq minibuffer 'only) 1 3))))))

(defun lispm-desktop-app-yeetube--sheet (frame)
  "Return Yeetube's dialog over FRAME, its window, made when missing."
  (let ((sheet lispm-desktop-app-yeetube--sheet))
    (unless (and (frame-live-p sheet) (eq (frame-parent sheet) frame))
      (when (frame-live-p sheet)
        (delete-frame sheet t))
      (setq sheet (lispm-desktop-app-yeetube--make-frame frame t "Yeetube Dialog" 1)
            lispm-desktop-app-yeetube--sheet sheet)
      (let ((window (frame-root-window sheet)))
        (set-window-buffer window (lispm-desktop-app-yeetube--sheet-buffer))
        (set-window-dedicated-p window t)
        (set-window-fringes window 0 0)))
    (lispm-desktop-app-yeetube--paint sheet)
    sheet))

(defun lispm-desktop-app-yeetube--search-field (frame)
  "Return the field of the masthead's search box over FRAME, Yeetube's
window, made when missing."
  (let ((field lispm-desktop-app-yeetube--search-field))
    (unless (and (frame-live-p field) (eq (frame-parent field) frame))
      (when (frame-live-p field)
        (delete-frame field t))
      (setq field (lispm-desktop-app-yeetube--make-frame frame 'only "Yeetube Search" 0)
            lispm-desktop-app-yeetube--search-field field))
    (lispm-desktop-app-yeetube--paint field)
    field))

(defun lispm-desktop-app-yeetube--fit-sheet (&rest _)
  "Make Yeetube's dialog as tall as its line and its field: Emacs grows the
field to what it shows, the candidates of a completion under it
included, taking the room from the line, which this gives back.
This is a `post-command-hook' and `window-size-change-functions'
function of the field while it asks."
  (pcase-let ((`(,_ ,_ ,frame ,_ ,style) lispm-desktop-app-yeetube--asking))
    (when (and (frame-live-p frame) (not (eq style 'search)))
      (let ((height (+ (cdr (window-text-pixel-size (frame-root-window frame) nil nil 4000 4000))
                       (window-pixel-height (minibuffer-window frame)))))
        (unless (= (frame-text-height frame) height)
          (set-frame-height frame height nil t))))))

(defun lispm-desktop-app-yeetube--place (frame window style)
  "Put FRAME over WINDOW, Yeetube's: the search box's field, STYLE
`search', in the box; else the dialog, centred under the masthead, as
wide as `lispm-desktop-app-yeetube-sheet-width' where it has room."
  (let ((top (+ (window-pixel-top window) (window-tab-line-height window))))
    (if (eq style 'search)
        (pcase-let ((`(,x ,y ,w ,h) (alist-get 'input (lispm-desktop-app-yeetube-page-masthead-layout
                                                       (window-pixel-width window)
                                                       (with-current-buffer (window-buffer window)
                                                         (lispm-desktop-app-yeetube--watching-p)))))
                    (line (frame-char-height frame)))
          (set-frame-size frame (- w 32) line t)
          (set-frame-position frame (+ (window-pixel-left window) x 16) (+ top y (/ (- h line) 2))))
      (let* ((outer (frame-inner-width (window-frame window)))
             (width (min lispm-desktop-app-yeetube-sheet-width (- outer 34))))
        (set-frame-width frame width nil t)
        (set-frame-position frame (max 0 (/ (- outer width 2) 2))
                            (+ top (window-header-line-height window) 8))))))

(defun lispm-desktop-app-yeetube--sheet-setup ()
  "Show the field asked in, its question over it in the dialog, when it is
read from.  This is a `minibuffer-setup-hook' function."
  (pcase-let ((`(,question ,button ,frame ,window ,style) lispm-desktop-app-yeetube--asking))
    (when (and (frame-live-p frame) (eq (selected-window) (minibuffer-window frame)))
      (unless (eq style 'search)
        (with-current-buffer (lispm-desktop-app-yeetube--sheet-buffer)
          (let ((inhibit-read-only t))
            (setq-local face-remapping-alist
                        `((default :background ,(plist-get (lispm-desktop-app-yeetube-page-colors) :background))))
            (erase-buffer)
            ;; Room over the line and under it.
            (insert (propertize " " 'display '(space :height (8))) "\n"
                    (lispm-desktop-app-yeetube-sheet-line question button)))))
      (use-local-map (make-composed-keymap lispm-desktop-app-yeetube-sheet-map (current-local-map)))
      ;; The question says what Yeetube's prompt says, the search box
      ;; that it is one.
      (let ((prompt (make-overlay (point-min) (minibuffer-prompt-end))))
        (overlay-put prompt 'invisible t)
        (overlay-put prompt 'evaporate t))
      (unless (eq style 'search)
        (setq-local line-prefix (propertize " " 'display '(space :width (16))))
        (add-hook 'post-command-hook #'lispm-desktop-app-yeetube--fit-sheet 90 t)
        (add-hook 'window-size-change-functions #'lispm-desktop-app-yeetube--fit-sheet nil t))
      (lispm-desktop-app-yeetube--place frame window style)
      (lispm-desktop-app-yeetube--fit-sheet)
      (make-frame-visible frame)
      (force-mode-line-update t)
      ;; Once a completion has drawn its candidates.
      (run-at-time 0 nil #'lispm-desktop-app-yeetube--fit-sheet))))

(defun lispm-desktop-app-yeetube--keep-field ()
  "Give the field asked in the keyboard back after a command left it in
Yeetube's window, as a click there does: the question holds the window's
keys while it asks, rather than letting them run Yeetube's commands.
This is a `post-command-hook' function while a question is open."
  (pcase-let ((`(,_ ,_ ,frame ,window ,_) lispm-desktop-app-yeetube--asking))
    (when-let* (((frame-live-p frame))
                (field (active-minibuffer-window))
                ((eq field (minibuffer-window frame)))
                ((memq (selected-window) (list window (frame-root-window frame)))))
      (select-window field))))

(defun lispm-desktop-app-yeetube-ask (question button function &optional style)
  "Return what FUNCTION returns, the question it reads from the minibuffer
asked over Yeetube's window when it asks from there: in the search box
when STYLE is `search', else in its dialog, QUESTION over the field and
BUTTON answering.  Elsewhere it asks as it would.  Asked while another
question is open, that one gets the keyboard."
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
           (let ((asking (if (eq style 'search)
                             (lispm-desktop-app-yeetube--search-field frame)
                           (lispm-desktop-app-yeetube--sheet frame)))
                 (minibuffer (minibuffer-window frame)))
             (setq lispm-desktop-app-yeetube--asking (list question button asking window style))
             (unwind-protect
                 (progn
                   (set-frame-parameter frame 'minibuffer (minibuffer-window asking))
                   (add-hook 'post-command-hook #'lispm-desktop-app-yeetube--keep-field)
                   (funcall function))
               (remove-hook 'post-command-hook #'lispm-desktop-app-yeetube--keep-field)
               (setq lispm-desktop-app-yeetube--asking nil)
               (when (frame-live-p frame)
                 (set-frame-parameter frame 'minibuffer minibuffer))
               (when (frame-live-p asking)
                 (make-frame-invisible asking t))
               (force-mode-line-update t)))))))

(with-eval-after-load 'yeetube
  (pcase-dolist (`(,function ,question ,button ,style) lispm-desktop-app-yeetube-questions)
    (advice-add function :around
                (lambda (ask &rest arguments)
                  (lispm-desktop-app-yeetube-ask question button (lambda () (apply ask arguments)) style))
                '((name . lispm-desktop-app-yeetube-ask))))
  (add-hook 'minibuffer-setup-hook #'lispm-desktop-app-yeetube--sheet-setup))

;;;; Its window's keys

(defun lispm-desktop-app-yeetube-search ()
  "Search YouTube, asking what for in the masthead's search box; while a
question is open, give its field the keyboard instead, as clicking the
box again would."
  (interactive)
  (if-let* ((field (active-minibuffer-window)))
      (select-window field)
    (call-interactively #'yeetube-search)))

(defvar-keymap lispm-desktop-app-yeetube-window-mode-map
  :doc "Keys of Yeetube's pages in its window, beside Yeetube's own: s and
/ search, as on youtube.com; Return does what the row point is on is for,
as playing its video; q and Delete leave the watch page."
  "<remap> <yeetube-search>" #'lispm-desktop-app-yeetube-search
  "/" #'lispm-desktop-app-yeetube-search
  "RET" #'lispm-desktop-app-yeetube-activate
  "q" `(menu-item "" lispm-desktop-app-yeetube-back :filter ,(lambda (command)
                                                              (and (lispm-desktop-app-yeetube--watching-p) command)))
  "DEL" `(menu-item "" lispm-desktop-app-yeetube-back :filter ,(lambda (command)
                                                                (and (lispm-desktop-app-yeetube--watching-p) command))))

(define-minor-mode lispm-desktop-app-yeetube-window-mode
  "Yeetube's buffer as its window's pages.
\\{lispm-desktop-app-yeetube-window-mode-map}"
  :lighter nil)

;;;; Its window

(defun lispm-desktop-app-yeetube-settings ()
  "Show Yeetube's settings: quality, results, where downloads go."
  (interactive)
  (keymap-popup yeetube-settings-map))

(defun lispm-desktop-app-yeetube-play-in-mpv ()
  "Play the video point is on in mpv's window, as Yeetube plays it."
  (interactive)
  (when-let* ((item (yeetube--find-item (lispm-desktop-app-yeetube--video))))
    (yeetube-mpv-play (lispm-desktop-app-yeetube--url item) (plist-get item :title))))

(defun lispm-desktop-app-yeetube--menu-items ()
  "Return the items of Yeetube's Video menu."
  (let ((video (lispm-desktop-app-yeetube--video))
        (saved (lispm-desktop-app-yeetube--saved-p))
        (searched (lispm-desktop-app-yeetube-words (lispm-desktop-app-yeetube--searched-p)))
        (sorted (or (lispm-desktop-app-yeetube-filter (lispm-desktop-app-yeetube--searched-p))
                    (bound-and-true-p yeetube-filter))))
    `((:label "Play" :action lispm-desktop-app-yeetube-activate :key "RET" :enabled ,(and video t))
      (:label "Play in mpv" :action lispm-desktop-app-yeetube-play-in-mpv :enabled ,(and video t))
      (:label "Back to Results" :action lispm-desktop-app-yeetube-back :key "q"
       :enabled ,(lispm-desktop-app-yeetube--watching-p))
      (:label "Play Again…" :action yeetube-replay :enabled ,(and (bound-and-true-p yeetube-history) t))
      (:label "Play Saved…" :action yeetube-play-saved-video :enabled ,(and saved t))
      separator
      (:label "Search…" :action lispm-desktop-app-yeetube-search :key "/")
      (:label "More Results" :action lispm-desktop-app-yeetube-more :key "M-n"
       :enabled ,(and (lispm-desktop-app-yeetube--more-p) t))
      ,@(mapcar (pcase-lambda (`(,label . ,filter))
                  (list :label (concat "Sort by " label) :action (lambda () (lispm-desktop-app-yeetube-sort filter))
                        :enabled (and searched t) :checked (equal filter sorted)))
                lispm-desktop-app-yeetube-page-sorts)
      separator
      (:label "Download" :action yeetube-download-video :enabled ,(and video t))
      (:label "Save to Watch Later…" :action yeetube-save-video :enabled ,(and video t))
      (:label "Share: Copy URL" :action yeetube-copy-url :enabled ,(and video t))
      (:label "Open in Browser" :action yeetube-browse-url :enabled ,(and video t))
      (:label "Channel’s Videos" :action yeetube-channel-videos :enabled ,(and video t))
      (:label "Remove Saved…" :action yeetube-remove-saved-video :enabled ,(and saved t))
      separator
      (:label "Settings…" :action lispm-desktop-app-yeetube-settings :key "o"))))

(lispm-desktop-app-ui-define 'yeetube-mode
  :app 'yeetube
  :run #'lispm-desktop-app-yeetube
  :title "Yeetube"
  :buttons nil
  :status #'ignore
  :cursor nil
  :setup #'lispm-desktop-app-yeetube--setup
  :menu '("Video" . lispm-desktop-app-yeetube--menu-items)
  :keys '(("s  /" . "Search YouTube") ("C-n  C-p" . "Next and previous video")
          ("Return" . "Play the video here, as a click does") ("q  Delete" . "Back to the results")
          ("d" . "Download it") ("S" . "Save it to watch later") ("C" . "Copy its URL, to share it")
          ("P" . "Play a saved video") ("r" . "Play one played before")
          ("M-n" . "More results") ("c" . "The channel’s videos")
          ("b" . "Open it in the browser") ("o" . "Settings") ("h" . "All of Yeetube’s commands")
          ("Esc" . "Take back a question asked")))

(provide 'lispm-desktop-app-yeetube)
;;; lispm-desktop-app-yeetube.el ends here
