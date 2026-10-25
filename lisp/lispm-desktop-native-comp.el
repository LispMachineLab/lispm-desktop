;;; lispm-desktop-native-comp.el --- Native compilation that works in LispM.app  -*- lexical-binding: t; -*-

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
;;   chosen directory's own SDK always suits its ld.  xcrun says which
;;   SDK that is, asked as Emacs starts and heard once comp.el loads.
;;
;; lispm-desktop-early.el mends both before anything is compiled, and the
;; App Store's installer does in the Emacs of its own it runs with -Q,
;; where advice on primitives compiles trampolines at once.

;;; Code:

(defvar native-comp-driver-options)

(defconst lispm-desktop-native-comp--tools-sdk-lib
  "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"
  "Where the command line tools keep the SDK's libraries.")

(defvar lispm-desktop-native-comp--xcrun nil
  "The xcrun started as Emacs starts to say where the SDK of xcode-select's
choice is, a process, or nil.")

(defun lispm-desktop-native-comp--ask-xcrun ()
  "Start xcrun saying where the SDK of xcode-select's choice is, in the
background: it takes most of a tenth of a second, which comp.el's loading
then need not wait for.  Emacs starts it, as nothing else runs this early."
  (setq lispm-desktop-native-comp--xcrun
        (ignore-errors
          (make-process :name "lispm-xcrun" :buffer (generate-new-buffer " *lispm-xcrun*" t)
                        :command '("/bin/sh" "-c" "exec xcrun --show-sdk-path 2>/dev/null")
                        :connection-type 'pipe :noquery t :sentinel #'ignore))))

(defun lispm-desktop-native-comp--xcrun-said ()
  "Return the SDK the xcrun started as Emacs started said, waiting for the
rest of it, or nil when it failed."
  (let ((process lispm-desktop-native-comp--xcrun))
    (setq lispm-desktop-native-comp--xcrun nil)
    (while (process-live-p process)
      (accept-process-output process 0.05 nil t))
    (while (accept-process-output process 0 nil t))
    (prog1 (and (eql (process-exit-status process) 0)
                (car (split-string (with-current-buffer (process-buffer process) (buffer-string)) "\n" t)))
      (kill-buffer (process-buffer process)))))

(defun lispm-desktop-native-comp--sdk-lib ()
  "Return the libraries of the SDK xcode-select's choice links with, or nil.
xcrun says where it is: the one started as Emacs started, else one waited for."
  (when-let* ((sdk (if lispm-desktop-native-comp--xcrun
                       (lispm-desktop-native-comp--xcrun-said)
                     (ignore-errors (car (process-lines "xcrun" "--show-sdk-path")))))
              (lib (expand-file-name "usr/lib" sdk))
              ((file-directory-p lib)))
    lib))

(defun lispm-desktop-native-comp--library-path (paths lib)
  "Return PATHS, a LIBRARY_PATH, with LIB for the command line tools' SDK."
  (mapconcat (lambda (path)
               (if (equal (directory-file-name path) lispm-desktop-native-comp--tools-sdk-lib)
                   lib
                 path))
             (split-string paths path-separator t)
             path-separator))

(defconst lispm-desktop-native-comp--darwin-options '("-Wl,-w")
  "The driver options comp.el gives macOS, which it would have once loaded.")

(defun lispm-desktop-native-comp--driver-options (options)
  "Return OPTIONS, native compilation's driver options, building for macOS
11 or later unless they say for which already."
  (if (string-search "-mmacosx-version-min=" (mapconcat #'identity options " "))
      options
    (append options '("-mmacosx-version-min=11.0"))))

(defun lispm-desktop-native-comp-mend ()
  "Make native compilation work on macOS: build for macOS 11 or later, and
link with the chosen developer directory's SDK.
The driver options are set at once, not only once comp.el is loaded: the
Emacsen compiling packages in the background take the ones this Emacs has
when it starts them, which comp-run.el does before comp.el is loaded."
  (when (and (eq system-type 'darwin)
             (fboundp 'native-comp-available-p)
             (native-comp-available-p))
    ;; comp.el's defcustom keeps a value set before it is loaded.
    (setq native-comp-driver-options
          (lispm-desktop-native-comp--driver-options
           (if (boundp 'native-comp-driver-options)
               native-comp-driver-options
             lispm-desktop-native-comp--darwin-options)))
    (when-let* ((paths (getenv "LIBRARY_PATH"))
                ((string-search lispm-desktop-native-comp--tools-sdk-lib paths))
                ((not (featurep 'comp))))
      (lispm-desktop-native-comp--ask-xcrun))
    (with-eval-after-load 'comp
      (setq native-comp-driver-options
            (lispm-desktop-native-comp--driver-options native-comp-driver-options))
      (when-let* ((paths (getenv "LIBRARY_PATH"))
                  ((string-search lispm-desktop-native-comp--tools-sdk-lib paths))
                  (lib (lispm-desktop-native-comp--sdk-lib)))
        (setenv "LIBRARY_PATH" (lispm-desktop-native-comp--library-path paths lib))))))

(provide 'lispm-desktop-native-comp)
;;; lispm-desktop-native-comp.el ends here
