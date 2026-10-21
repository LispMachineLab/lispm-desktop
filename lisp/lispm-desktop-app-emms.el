;;; lispm-desktop-app-emms.el --- EMMS on the desktop  -*- lexical-binding: t; -*-

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

;; EMMS, the App Store's music player, as it opens on the desktop:
;;
;;   players  mpv, VLC or MPlayer where one is installed; else macOS's own
;;            afplay, which plays MP3, AAC, Apple Lossless, WAV, AIFF and
;;            FLAC, and pauses, but cannot seek
;;   tags     EMMS's own reader for MP3, FLAC, Ogg and Opus; for the rest,
;;            as the AAC files of a Music library, exiftool where it is
;;            installed, else what Spotlight read of them, through mdls
;;   Music    the library of macOS's Music app, read, and only read,
;;            through Apple's iTunesLibrary: every song it keeps a file
;;            of, its tags as Music has them, and its playlists, which
;;            play from the Library menu; what EMMS cannot play, Apple
;;            Music's songs and those bought protected, plays in Music
;;            only
;;   window   its library in the list face, the player's buttons, what is
;;            playing on the window's bottom, and a page while it knows
;;            no music
;;
;; Players and tag readers a user set up are kept: EMMS's own defaults
;; alone are replaced.
;;
;; The library is read by LispM Music Library.app, built the first time
;; from native/lispm-music-library with Xcode's command line tools, and
;; again when its sources change: an application of its own, opened by
;; LaunchServices, so that macOS asks once, in its name, to let it read
;; the media library (Privacy & Security, Media & Apple Music).  It
;; controls nothing: Music need not run, and nothing in it changes.  The catalog declares mpv and exiftool as the
;; programs EMMS uses, afplay and mdls standing in for them, so the App
;; Store and the window say how to install them, see
;; lispm-desktop-app-ui.el.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-core)
(require 'lispm-desktop-popup)
(require 'lispm-desktop-app-ui)

;; EMMS's own variables and functions, read once it runs.
(defvar emms-player-list)
(defvar emms-setup-default-player-list)
(defvar emms-info-functions)
(defvar emms-cache-db)
(defvar emms-cache-dirty)
(defvar emms-playlist-buffer)
(defvar emms-track-initialize-functions)
(declare-function emms-all "emms-setup" ())
(declare-function emms-browser "emms-browser" ())
(declare-function emms-browse-by-artist "emms-browser" ())
(declare-function emms-add-directory-tree "emms-source-file" (dir))
(declare-function emms-add-file "emms-source-file" (file))
(declare-function emms-playlist-current-selected-track "emms" ())
(declare-function emms-playlist-current-clear "emms" ())
(declare-function emms-playlist-current-select-first "emms" ())
(declare-function emms-playlist-insert-track "emms" (track))
(declare-function emms-playlist-buffer-p "emms" (&optional buffer))
(declare-function emms-start "emms" ())
(declare-function emms-stop "emms" ())
(declare-function emms-track "emms" (type name))
(declare-function emms-track-set "emms" (track name value))
(declare-function emms-track-get "emms" (track name &optional default))
(declare-function emms-track-type "emms" (track))
(declare-function emms-track-name "emms" (track))
(declare-function emms-track-description "emms" (track))
(declare-function emms-cache-set "emms-cache" (type path track))
(declare-function emms-cache-save "emms-cache" ())
(declare-function emms-player-simple-regexp "emms-player-simple" (&rest extensions))

;;;; Players and tags

(defconst lispm-desktop-app-emms-afplay-formats
  '("mp3" "m4a" "m4b" "aac" "mp4" "wav" "aif" "aiff" "aifc" "caf" "flac")
  "What afplay plays: the formats of macOS's Core Audio.")

(defun lispm-desktop-app-emms--define-afplay ()
  "Define EMMS's player of afplay, macOS's own, once."
  (unless (fboundp 'emms-player-afplay-playable-p)
    (require 'emms-player-simple)
    (eval `(define-emms-simple-player afplay '(file)
             (emms-player-simple-regexp ,@lispm-desktop-app-emms-afplay-formats)
             "afplay")
          t)))

(defun lispm-desktop-app-emms-players ()
  "Return the players EMMS plays with here: EMMS's defaults whose program
is installed, then afplay where macOS has it.  EMMS's mpv takes every
track whether mpv is installed or not, so it is left out without it."
  (append (seq-remove (lambda (player)
                        (and (eq player 'emms-player-mpv) (not (executable-find "mpv"))))
                      emms-setup-default-player-list)
          (when (executable-find "afplay")
            (lispm-desktop-app-emms--define-afplay)
            '(emms-player-afplay))))

(defconst lispm-desktop-app-emms--mdls-keys
  '((kMDItemTitle . info-title) (kMDItemAuthors . info-artist) (kMDItemAlbum . info-album)
    (kMDItemAudioTrackNumber . info-tracknumber) (kMDItemMusicalGenre . info-genre)
    (kMDItemRecordingYear . info-year) (kMDItemComposer . info-composer)
    (kMDItemDurationSeconds . info-playing-time))
  "Spotlight's attributes of an audio file, and EMMS's fields for them.")

(defun lispm-desktop-app-emms--mdls-value (key)
  "Return the value mdls printed in the current buffer for KEY, a string,
or nil: the first of a list, a string read, a number as written."
  (goto-char (point-min))
  (when (re-search-forward (concat "^" (regexp-quote key) " *= *\\(.*\\)$") nil t)
    (let ((value (string-trim (match-string 1))))
      (cond ((equal value "(null)") nil)
            ((equal value "(")
             (forward-line 1)
             (when (looking-at " *\\(\".*\"\\),?$")
               (read (match-string 1))))
            ((string-prefix-p "\"" value) (read value))
            (t value)))))

(defun lispm-desktop-app-emms-info-mdls (track)
  "Set TRACK's tags as Spotlight read them, through mdls, unless a reader
before set its title.  This is one of `emms-info-functions'."
  (when (and (eq (emms-track-type track) 'file)
             (not (emms-track-get track 'info-title)))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
        (when (eql 0 (apply #'call-process "mdls" nil t nil
                            (append (mapcan (lambda (key) (list "-name" (symbol-name (car key))))
                                            lispm-desktop-app-emms--mdls-keys)
                                    (list (expand-file-name (emms-track-name track))))))
          (pcase-dolist (`(,key . ,field) lispm-desktop-app-emms--mdls-keys)
            (when-let* ((value (lispm-desktop-app-emms--mdls-value (symbol-name key))))
              (emms-track-set track field (if (eq field 'info-playing-time)
                                              (round (string-to-number value))
                                            value)))))))))

(defun lispm-desktop-app-emms-info-functions ()
  "Return the functions reading tags here, after EMMS's own: exiftool's
where it is installed, else Spotlight's where mdls is."
  `(emms-info-native
    ,@(cond ((executable-find "exiftool") '(emms-info-exiftool))
            ((executable-find "mdls") '(lispm-desktop-app-emms-info-mdls)))
    emms-info-cueinfo))

(defvar lispm-desktop-app-emms--set-up nil
  "Non-nil once EMMS was set up here.")

(defun lispm-desktop-app-emms-setup ()
  "Set EMMS up once: as its manual says, with all it has, unless the user
set it up; its players and tag readers those found here, unless the user
chose others than EMMS's own."
  (require 'emms-setup)
  (unless lispm-desktop-app-emms--set-up
    (setq lispm-desktop-app-emms--set-up t)
    (let ((players (bound-and-true-p emms-player-list)))
      (unless players
        (emms-all))
      (when (or (null players) (equal players emms-setup-default-player-list))
        (setq emms-player-list (lispm-desktop-app-emms-players)))
      ;; Those `emms-all' sets.
      (when (equal (bound-and-true-p emms-info-functions) '(emms-info-native emms-info-cueinfo))
        (setq emms-info-functions (lispm-desktop-app-emms-info-functions))))))

(defun lispm-desktop-app-emms ()
  "Open EMMS's browser of the music it knows, EMMS set up the first time."
  (interactive)
  (lispm-desktop-app-emms-setup)
  (emms-browser))

;;;; The Music library

(defcustom lispm-desktop-app-emms-music-playlists-file
  (locate-user-emacs-file "lispm-music-playlists.eld")
  "File keeping the playlists read from the Music library."
  :type 'file
  :group 'lispm-desktop)

(defvar lispm-desktop-app-emms--music-playlists 'unread
  "The Music library's playlists: ((NAME FILE ...) ...), or `unread'.")

(defun lispm-desktop-app-emms-music-playlists ()
  "Return the playlists read from the Music library, ((NAME FILE ...) ...)."
  (when (eq lispm-desktop-app-emms--music-playlists 'unread)
    (setq lispm-desktop-app-emms--music-playlists
          (ignore-errors
            (with-temp-buffer
              (insert-file-contents lispm-desktop-app-emms-music-playlists-file)
              (read (current-buffer))))))
  lispm-desktop-app-emms--music-playlists)

(defun lispm-desktop-app-emms--protected-p (song)
  "Return non-nil when SONG, as Music describes it, is protected: bought
with its old copy protection, or Apple Music's."
  (or (eq (plist-get song :protected) t)
      (string-match-p "Protected\\|Apple Music" (or (plist-get song :kind) ""))))

(defun lispm-desktop-app-emms--song-track (song)
  "Return EMMS's track of SONG, as Music describes it, its tags Music's."
  (let ((track (let ((emms-track-initialize-functions nil))
                 ;; Music has read its tags: EMMS need not read them again.
                 (emms-track 'file (plist-get song :path))))
        (number (lambda (value) (and (numberp value) (> value 0) (number-to-string value)))))
    (pcase-dolist (`(,field . ,value)
                   `((info-title . ,(plist-get song :name))
                     (info-artist . ,(plist-get song :artist))
                     (info-albumartist . ,(plist-get song :albumArtist))
                     (info-album . ,(plist-get song :album))
                     (info-tracknumber . ,(funcall number (plist-get song :trackNumber)))
                     (info-discnumber . ,(funcall number (plist-get song :discNumber)))
                     (info-year . ,(funcall number (plist-get song :year)))
                     (info-genre . ,(plist-get song :genre))
                     (info-composer . ,(plist-get song :composer))
                     (info-playing-time . ,(and (numberp (plist-get song :duration))
                                                (round (plist-get song :duration))))))
      (when (and value (not (equal value "")))
        (emms-track-set track field value)))
    ;; Read now, so EMMS reads it no more until the file changes.
    (emms-track-set track 'info-mtime (current-time))
    track))

(defun lispm-desktop-app-emms--import (data)
  "Keep the songs and playlists of the Music library DATA, as the script
read it, in EMMS's cache and the playlists file; return (SONGS PLAYLISTS
LEFT), LEFT the songs EMMS cannot play."
  (let ((files (make-hash-table :test #'equal))
        (songs 0)
        (left (- (or (plist-get data :total) 0) (length (plist-get data :tracks)))))
    (dolist (song (plist-get data :tracks))
      (if (or (not (stringp (plist-get song :path))) (lispm-desktop-app-emms--protected-p song))
          (cl-incf left)
        (emms-cache-set 'file (plist-get song :path) (lispm-desktop-app-emms--song-track song))
        (puthash (plist-get song :id) (plist-get song :path) files)
        (cl-incf songs)))
    (let ((playlists (delq nil (mapcar (lambda (playlist)
                                         (when-let* ((paths (delq nil (mapcar (lambda (id) (gethash id files))
                                                                              (plist-get playlist :ids)))))
                                           (cons (plist-get playlist :name) paths)))
                                       (plist-get data :playlists)))))
      (setq lispm-desktop-app-emms--music-playlists playlists)
      (with-temp-file lispm-desktop-app-emms-music-playlists-file
        (let ((print-length nil) (print-level nil))
          (insert ";; -*- lisp-data -*-\n;; The Music library's playlists, as LispM read them.\n")
          (prin1 playlists (current-buffer))
          (insert "\n")))
      (setq emms-cache-dirty t)
      (emms-cache-save)
      (list songs (length playlists) (max 0 left)))))

(defun lispm-desktop-app-emms--imported (window result)
  "Say how the import went, RESULT of `lispm-desktop-app-emms--import',
and browse the library in WINDOW."
  (pcase-let ((`(,songs ,playlists ,left) result))
    (when (window-live-p window)
      (with-selected-window window (emms-browse-by-artist)))
    (lispm-desktop-popup-alert
     'note (format "%s songs and %d playlists came from Music."
                   (lispm-desktop-app-ui--number songs) playlists)
     '(("OK" ok default))
     (when (> left 0)
       (format "%s songs play in Music only: Apple Music's, and those bought protected."
               (lispm-desktop-app-ui--number left))))))

(defconst lispm-desktop-app-emms--reader-sources
  (expand-file-name "native/lispm-music-library/" lispm-desktop-directory)
  "The sources of LispM Music Library.app, the reader of the Music library.")

(defcustom lispm-desktop-app-emms-reader-directory (locate-user-emacs-file "lispm-music-library/")
  "Directory LispM Music Library.app, the reader of the Music library, is
built in."
  :type 'directory
  :group 'lispm-desktop)

(defun lispm-desktop-app-emms--reader ()
  "Return where LispM Music Library.app is built."
  (expand-file-name "LispM Music Library.app" lispm-desktop-app-emms-reader-directory))

(defun lispm-desktop-app-emms--reader-sources-hash ()
  "Return a hash of the reader's sources, which its build keeps."
  (secure-hash 'sha1 (mapconcat (lambda (file)
                                  (with-temp-buffer
                                    (insert-file-contents-literally
                                     (expand-file-name file lispm-desktop-app-emms--reader-sources))
                                    (buffer-string)))
                                '("main.swift" "Info.plist") "")))

(defun lispm-desktop-app-emms--reader-stamp ()
  "Return the file keeping the hash of the sources the reader was built from."
  (expand-file-name "Contents/lispm-sources" (lispm-desktop-app-emms--reader)))

(defun lispm-desktop-app-emms--reader-built-p ()
  "Return non-nil when the reader is built from its sources as they are."
  (equal (ignore-errors
           (with-temp-buffer
             (insert-file-contents (lispm-desktop-app-emms--reader-stamp))
             (buffer-string)))
         (lispm-desktop-app-emms--reader-sources-hash)))

(defun lispm-desktop-app-emms--build-reader (then)
  "Build LispM Music Library.app from its sources, signed for this computer
alone, then call THEN; say why when it cannot be built."
  (let* ((app (lispm-desktop-app-emms--reader))
         (log (generate-new-buffer " *lispm-music-library build*"))
         (hash (lispm-desktop-app-emms--reader-sources-hash)))
    (when (file-directory-p app)
      (delete-directory app t))
    (make-directory (expand-file-name "Contents/MacOS" app) t)
    (copy-file (expand-file-name "Info.plist" lispm-desktop-app-emms--reader-sources)
               (expand-file-name "Contents/Info.plist" app) t)
    (message "Building the reader of the Music library…")
    (make-process
     :name "lispm-music-library-build" :buffer log :noquery t
     :command (list "sh" "-c" "swiftc -O -framework iTunesLibrary -o \"$1\" \"$2\" && codesign --force --sign - \"$3\""
                    "sh" (expand-file-name "Contents/MacOS/lispm-music-library" app)
                    (expand-file-name "main.swift" lispm-desktop-app-emms--reader-sources) app)
     :sentinel
     (lambda (process _event)
       (unless (process-live-p process)
         (message nil)
         (if (= (process-exit-status process) 0)
             (progn (with-temp-file (lispm-desktop-app-emms--reader-stamp) (insert hash))
                    (kill-buffer log)
                    (funcall then))
           (let ((why (with-current-buffer log (string-trim (buffer-string)))))
             (kill-buffer log)
             (when (eq (lispm-desktop-popup-alert
                        'stop "The reader of the Music library could not be built."
                        '(("Copy Command" copy) ("OK" ok default))
                        (concat "It needs Xcode's command line tools; install them with: "
                                "xcode-select --install"
                                (unless (string-empty-p why) (concat "\n" (truncate-string-to-width why 300)))))
                       'copy)
               (kill-new "xcode-select --install")))))))))

(defun lispm-desktop-app-emms--read-library (then)
  "Read the Music library with LispM Music Library.app, opened by
LaunchServices, out of sight; call THEN with what it read, as JSON
parsed into plists."
  (let ((output (make-temp-file "lispm-music-library" nil ".json")))
    (make-process
     :name "lispm-music-library" :noquery t
     :command (list "open" "-W" "-g" "-n" (lispm-desktop-app-emms--reader) "--args" output)
     :sentinel
     (lambda (process _event)
       (unless (process-live-p process)
         (let ((data (ignore-errors
                       (with-temp-buffer
                         (insert-file-contents output)
                         (json-parse-buffer :object-type 'plist :array-type 'list
                                            :null-object nil :false-object nil)))))
           (delete-file output)
           (funcall then data)))))))

(defun lispm-desktop-app-emms-import-music ()
  "Read the Music app's library into EMMS: every song Music keeps a file
of, its tags as Music has them, and its playlists.  It is read, and only
read, through Apple's iTunesLibrary, by LispM Music Library.app, built
the first time; macOS asks once whether it may read the media library."
  (interactive)
  (unless (eq system-type 'darwin)
    (user-error "The Music app is macOS's"))
  (lispm-desktop-app-emms-setup)
  (let ((window (selected-window)))
    (cl-flet ((read-library ()
                (message "Reading the Music library…")
                (lispm-desktop-app-emms--read-library
                 (lambda (data)
                   (message nil)
                   (if (or (null data) (plist-get data :error))
                       (lispm-desktop-popup-alert
                        'stop "The Music library could not be read." '(("OK" ok default))
                        (concat (or (plist-get data :error) "LispM Music Library said nothing.")
                                "  If it was not let read the media library, let it in System Settings,"
                                " Privacy & Security, Media & Apple Music."))
                     (lispm-desktop-app-emms--imported window (lispm-desktop-app-emms--import data)))))))
      (if (lispm-desktop-app-emms--reader-built-p)
          (read-library)
        (lispm-desktop-app-emms--build-reader #'read-library)))))

(defun lispm-desktop-app-emms-play-music-playlist (name)
  "Play the Music library's playlist NAME, as read by
`lispm-desktop-app-emms-import-music'."
  (interactive (list (completing-read "Play Music playlist: "
                                      (mapcar #'car (lispm-desktop-app-emms-music-playlists)) nil t)))
  (lispm-desktop-app-emms-setup)
  (let ((files (cdr (assoc name (lispm-desktop-app-emms-music-playlists)))))
    (unless files
      (user-error "No playlist %s" name))
    (emms-stop)
    (emms-playlist-current-clear)
    (with-current-buffer emms-playlist-buffer
      (let ((inhibit-read-only t))
        (dolist (file files)
          (emms-playlist-insert-track (emms-track 'file file)))))
    (emms-playlist-current-select-first)
    (emms-start)))

;;;; Adding music

(defun lispm-desktop-app-emms--browse-later ()
  "Browse the music EMMS knows once it has read what was just added."
  (let ((window (selected-window)))
    (run-at-time 3 nil (lambda ()
                         (when (window-live-p window)
                           (with-selected-window window (emms-browse-by-artist)))))))

(defun lispm-desktop-app-emms-add-folder ()
  "Add a folder of music, and everything in it, to what EMMS knows."
  (interactive)
  (call-interactively #'emms-add-directory-tree)
  (lispm-desktop-app-emms--browse-later))

(defun lispm-desktop-app-emms-add-file ()
  "Add a file of music to what EMMS knows."
  (interactive)
  (call-interactively #'emms-add-file)
  (lispm-desktop-app-emms--browse-later))

(defun lispm-desktop-app-emms--library-items ()
  "Return the items of EMMS's Library menu: what adds music, then the
Music library's playlists, each playing."
  (let ((playlists (lispm-desktop-app-emms-music-playlists)))
    `(,@(when (eq system-type 'darwin)
          '((:label "Import Music Library" :action lispm-desktop-app-emms-import-music)))
      (:label "Add Folder…" :action lispm-desktop-app-emms-add-folder)
      (:label "Add File…" :action lispm-desktop-app-emms-add-file)
      separator
      ,@(if playlists
            (mapcar (pcase-lambda (`(,name . ,_))
                      (list :label name :action (lambda () (lispm-desktop-app-emms-play-music-playlist name))))
                    playlists)
          '((:label "No Music Playlists" :enabled nil))))))

;;;; The window

(defun lispm-desktop-app-emms--library-empty-p ()
  "Return non-nil while EMMS knows no music."
  (or (not (boundp 'emms-cache-db)) (not (hash-table-p emms-cache-db))
      (zerop (hash-table-count emms-cache-db))))

(defun lispm-desktop-app-emms--empty-buttons ()
  "Return the buttons of EMMS's page while it knows no music."
  `(,@(when (eq system-type 'darwin)
        '(("Import Music Library" lispm-desktop-app-emms-import-music)))
    ("Add Folder…" lispm-desktop-app-emms-add-folder)
    ("Add File…" lispm-desktop-app-emms-add-file)))

(defun lispm-desktop-app-emms--status ()
  "Return what EMMS's bottom says: what is playing, if anything."
  (when-let* (((bound-and-true-p emms-player-playing-p))
              (track (emms-playlist-current-selected-track)))
    (list (concat (if (bound-and-true-p emms-player-paused-p) "Paused: " "Playing: ")
                  (emms-track-description track)))))

(lispm-desktop-app-ui-define 'emms-browser-mode
  :app 'emms
  :run #'lispm-desktop-app-emms
  :list t
  :title "EMMS"
  :status #'lispm-desktop-app-emms--status
  :buttons `((,(lambda () (if (bound-and-true-p emms-player-paused-p) "Play" "Pause")) emms-pause
              ,(lambda () (bound-and-true-p emms-player-playing-p)))
             ("Stop" emms-stop ,(lambda () (bound-and-true-p emms-player-playing-p)))
             ("Previous" emms-previous) ("Next" emms-next))
  :menus '(("Library" . lispm-desktop-app-emms--library-items))
  :empty `(lispm-desktop-app-emms--library-empty-p
           ,(concat "No music yet.\nImport your Music library, or add a folder of music,\n"
                    "and browse it here by artist, album, genre or year.")
           lispm-desktop-app-emms--empty-buttons)
  :keys '(("Return" . "Add to the playlist") ("C-j" . "Add and play") ("SPC" . "Show or hide what is under")
          ("n  p" . "Next and previous line") ("b 1 … b 7" . "Browse by artist, album, genre, year…")
          ("s a  s t" . "Search artists, titles") ("+  −" . "Louder, softer")))

(provide 'lispm-desktop-app-emms)
;;; lispm-desktop-app-emms.el ends here
