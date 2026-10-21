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
;;            the first video once they come, a row too long cut clean
;;   buttons  Search, Play, Download, Save and More Results, each dimmed
;;            while it can do nothing
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
(declare-function yeetube-mode "yeetube" ())
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
  "Name the list's columns over them, in the buffer: the strip of buttons
takes the window's header line, where Yeetube names them.  A row longer
than the window is cut at its edge, without the dollar sign a window
without fringes shows there."
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

(defun lispm-desktop-app-yeetube--saved-p ()
  "Return non-nil when videos were saved to watch later, in the file where
Yeetube keeps them."
  (or (bound-and-true-p yeetube-saved-videos)
      (let ((file (locate-user-emacs-file "yeetube")))
        (and (file-readable-p file)
             (ignore-errors
               (with-temp-buffer
                 (insert-file-contents file)
                 (read (current-buffer))))))))

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

(with-eval-after-load 'yeetube
  (advice-add 'yeetube--render-items :around #'lispm-desktop-app-yeetube--render-in-place))

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
      (:label "Search…" :action yeetube-search)
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
  "Return the buttons of Yeetube's page while nothing was searched."
  `(("Search…" yeetube-search)
    ,@(when (lispm-desktop-app-yeetube--saved-p)
        '(("Play Saved…" yeetube-play-saved-video)))))

(lispm-desktop-app-ui-define 'yeetube-mode
  :app 'yeetube
  :run #'lispm-desktop-app-yeetube
  :title "Yeetube"
  :list t
  :setup #'lispm-desktop-app-yeetube--setup
  :status #'lispm-desktop-app-yeetube--status
  :buttons `(("Search…" yeetube-search)
             ("Play" yeetube-play lispm-desktop-app-yeetube--video)
             ("Download" yeetube-download-video lispm-desktop-app-yeetube--video)
             ("Save…" yeetube-save-video lispm-desktop-app-yeetube--video)
             ("More Results" yeetube-next-page lispm-desktop-app-yeetube--more-p))
  :menu '("Video" . lispm-desktop-app-yeetube--menu-items)
  :empty `(,(lambda () (not (lispm-desktop-app-yeetube--searched-p)))
           ,(concat "Search YouTube for videos.\nThey play in mpv, download with yt-dlp,\n"
                    "and can be saved to watch later.")
           lispm-desktop-app-yeetube--empty-buttons)
  :keys '(("s" . "Search YouTube") ("C-n  C-p" . "Next and previous video")
          ("Return" . "Play the video") ("d" . "Download it") ("S" . "Save it to watch later")
          ("P" . "Play a saved video") ("r" . "Play one played before")
          ("M-n" . "More results") ("c  L" . "The channel’s videos, streams")
          ("p  v" . "Pause, show or hide the video") ("C" . "Copy its URL")
          ("b" . "Open it in the browser") ("o" . "Settings") ("h" . "All of Yeetube’s commands")))

(provide 'lispm-desktop-app-yeetube)
;;; lispm-desktop-app-yeetube.el ends here
