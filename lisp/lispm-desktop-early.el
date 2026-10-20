;;; lispm-desktop-early.el --- Settings made before the first frame  -*- lexical-binding: t; -*-

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

;; Loaded from early-init.el: it runs before the first frame is drawn and
;; before packages are initialized.

;;; Code:

;; Terminal emulation (Eat) and language servers allocate heavily while
;; streaming output; at the default 800KB threshold a full-screen agent
;; redraw triggers a collection every fraction of a second.  Collect rarely
;; while busy and catch up when idle instead.
(setq gc-cons-threshold (* 64 1024 1024)
      read-process-output-max (* 1024 1024))
(run-with-idle-timer 5 t #'garbage-collect)

(setq frame-title-format nil
      icon-title-format nil
      package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(when (eq system-type 'darwin)
  (setq ns-use-proxy-icon nil
        ns-use-native-fullscreen nil))

;; Log asynchronous native-compilation warnings from third-party packages to
;; *Warnings* without popping up its window, and cap background compilation
;; at two concurrent jobs so a full package rebuild cannot saturate the machine.
(setq native-comp-async-report-warnings-errors 'silent
      native-comp-async-jobs-number 2)

;; Two things native compilation takes for granted on macOS go wrong in
;; some Emacs builds, LispM.app's (jimeh/emacs-builds) among them, and then
;; nothing compiles:
;;
;; - GCC's driver works out which macOS to build for from Darwin's version,
;;   and since Apple numbered macOS after the year (26) it can come to 18.0,
;;   which clang refuses.  Every macOS Emacs runs on is 11 or later.
;; - The build puts the command line tools' SDK on LIBRARY_PATH, where ld
;;   finds libSystem, while ld is the one of the developer directory
;;   xcode-select chose: Xcode 26's ld cannot read the macOS 27 SDK's.  The
;;   chosen directory's own SDK always suits its ld.
(defvar native-comp-driver-options)
(defconst lispm-desktop-early--tools-sdk-lib
  "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"
  "Where the command line tools keep the SDK's libraries.")

(defun lispm-desktop-early--sdk-lib ()
  "Return the libraries of the SDK xcode-select's choice links with, or nil."
  (when-let* ((sdk (ignore-errors (car (process-lines "xcrun" "--show-sdk-path"))))
              (lib (expand-file-name "usr/lib" sdk))
              ((file-directory-p lib)))
    lib))

(defun lispm-desktop-early--library-path (paths lib)
  "Return PATHS, a LIBRARY_PATH, with LIB for the command line tools' SDK."
  (mapconcat (lambda (path)
               (if (equal (directory-file-name path) lispm-desktop-early--tools-sdk-lib)
                   lib
                 path))
             (split-string paths path-separator t)
             path-separator))

(when (and (eq system-type 'darwin)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (with-eval-after-load 'comp
    (unless (string-search "-mmacosx-version-min=" (mapconcat #'identity native-comp-driver-options " "))
      (setq native-comp-driver-options
            (append native-comp-driver-options '("-mmacosx-version-min=11.0"))))
    (when-let* ((paths (getenv "LIBRARY_PATH"))
                ((string-search lispm-desktop-early--tools-sdk-lib paths))
                (lib (lispm-desktop-early--sdk-lib)))
      (setenv "LIBRARY_PATH" (lispm-desktop-early--library-path paths lib)))))

;; Frames stay fully opaque: on macOS any transparency makes every redraw
;; composite the whole window, which made scrolling a terminal stutter.
(dolist (frame-setting '((height . 40)
                         (width . 160)
                         (tool-bar-lines . 0)
                         (vertical-scroll-bars . nil)))
  (add-to-list 'default-frame-alist frame-setting)
  (add-to-list 'initial-frame-alist frame-setting))


(provide 'lispm-desktop-early)
;;; lispm-desktop-early.el ends here
