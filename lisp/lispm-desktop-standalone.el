;;; lispm-desktop-standalone.el --- Applications opened on their own, the desktop out of sight  -*- lexical-binding: t; -*-

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

;; An X application opens a window of its own on the screen, while the X
;; server it draws through runs where nobody looks; Mac OS X opened Mac
;; OS 9's applications so, Classic running out of sight.  LispM's
;; applications open so too, without its desktop:
;;
;;   - each application installed from the App Store, and the App Store,
;;     Control Panels and the browser, has an application of the
;;     computer's own in ~/Applications/LispM Apps, see
;;     `lispm-desktop-standalone-directory', its icon theirs, made and
;;     removed as they are installed and removed;
;;   - opened from the Finder, Launchpad, Spotlight or the Dock, it is a
;;     process of its own, with its own tile in the Dock, its own menu bar
;;     and its own place in the application switcher; it asks LispM,
;;     started for it out of sight when not running, for its application,
;;     and LispM opens the application's window out on the computer's
;;     screen, the desktop out of sight, see
;;     `lispm-desktop-coherence-put-out-of-sight', and draws it in the
;;     process's window, see lispm-desktop-display.el;
;;   - LispM itself leaves the Dock and the menu bar while its desktop is
;;     out of sight, running where nobody looks, as an X server does; a
;;     window it shows itself there, not drawn in an application's
;;     process, brings its tile back, wearing that application's icon;
;;   - quitting the application closes its windows, not LispM; Show
;;     LispM's Desktop, in its application menu, brings the desktop back,
;;     and LispM is as it always was.
;;
;; With the desktop shown, an application opened so opens over it, as
;; from the Applications folder.
;;
;; Where it works:
;;
;;   macOS            all of it
;;   X, a terminal    nothing: no application is made, and a desktop
;;                    started out of sight shows, as windows go out on
;;                    the computer's screen only on macOS
;;
;; LispM.app's applications start LispM.app.  An Emacs that loads LispM
;; from an init of its own makes applications that start that Emacs, its
;; init directory named.  iconutil and codesign finish them in the
;; background, in lispm-kernel when it runs (`lispm-exec').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-desktop-apps)
(require 'lispm-desktop-app-window)
(require 'lispm-desktop-coherence)
(require 'lispm-desktop-i18n)
(require 'lispm-desktop-display)
(require 'lispm-exec)

(declare-function lispm-browser-available-p "lispm-browser" ())
(defvar lispm-desktop-first-start)

(defgroup lispm-desktop-standalone nil
  "LispM's applications opened on their own, the desktop out of sight."
  :group 'lispm-desktop)

(defcustom lispm-desktop-standalone-directory "~/Applications/LispM Apps/"
  "Directory of the computer's applications that open LispM's on their own,
one for each, see lispm-desktop-standalone.el."
  :type 'directory
  :group 'lispm-desktop-standalone)

(defcustom lispm-desktop-standalone-launchers t
  "Non-nil gives each of LispM's applications one of the computer's own, in
`lispm-desktop-standalone-directory', that opens it on its own; nil makes
none, and removes those made."
  :type 'boolean
  :group 'lispm-desktop-standalone)

(defvar lispm-desktop-standalone-built-in
  '((store) (settings) (browser . lispm-browser-available-p))
  "The applications built in that open on their own, besides those installed
from the App Store: (KEY . PREDICATE), KEY one of `lispm-desktop-apps' and
PREDICATE, when given, a function saying whether it runs here.")

(defconst lispm-desktop-standalone-switch "--lispm-hidden"
  "The option LispM is started with to keep its desktop out of sight.
lispm-desktop-early.el looks for it before the first frame is made.")

(defun lispm-desktop-standalone-unsupported ()
  "Return why LispM's applications cannot open on their own here, as a
string in the language LispM speaks, or nil when they can."
  (cond (noninteractive (lispm-desktop-tr "Emacs runs without a display"))
        ((not (eq system-type 'darwin))
         (lispm-desktop-tr "applications open on their own only on macOS for now"))
        ((not (fboundp 'ns-do-applescript))
         (lispm-desktop-tr "this Emacs is not built for macOS's window system"))))

;;;; AppKit
;;
;; AppleScript run by Emacs runs in Emacs's own process, and AppleScript
;; can call Objective-C: so LispM tells AppKit what Emacs does not.

(defun lispm-desktop-standalone--string (string)
  "Return STRING as an AppleScript string literal."
  (concat "\"" (replace-regexp-in-string "[\"\\]" "\\\\\\&" string) "\""))

(defun lispm-desktop-standalone--appkit (script)
  "Run SCRIPT, AppleScript using AppKit as `current application's', in
Emacs's own process; return its result, or nil when it fails."
  (when (fboundp 'ns-do-applescript)
    (condition-case err
        (ns-do-applescript (concat "use framework \"AppKit\"\nset ca to current application\n" script))
      (error (message "LispM: AppKit refused: %s" (error-message-string err))
             nil))))

;;;; Started out of sight

(defun lispm-desktop-standalone-started-hidden-p ()
  "Return non-nil when LispM was started with its desktop out of sight, as
for an application opened on its own."
  (and (member lispm-desktop-standalone-switch command-line-args) t))

(defun lispm-desktop-standalone--screen ()
  "Return the screen hidden out of sight, or nil."
  (seq-find (lambda (frame)
              (and (lispm-desktop-apps-screen-p frame)
                   (lispm-desktop-coherence-out-of-sight-p frame)))
            (frame-list)))

(defun lispm-desktop-standalone--hide-at-start ()
  "Put the desktop out of sight as LispM starts so, and listen for the
application that started it at once; but show the desktop where windows
cannot go out, and where the first start asked its questions on it.
This is a `window-setup-hook' function."
  (when (lispm-desktop-standalone-started-hidden-p)
    (let ((screen (lispm-desktop-apps-screen)))
      (if (or (bound-and-true-p lispm-desktop-first-start)
              (lispm-desktop-coherence-unsupported screen))
          (make-frame-visible screen)
        (lispm-desktop-coherence-put-out-of-sight screen)
        (lispm-desktop-display-start)
        ;; The application that started LispM has its own tile already.
        (lispm-desktop-standalone--dock-show 'none)))))

;;;; Opening an application

(defun lispm-desktop-standalone-apps ()
  "Return the applications that open on their own: keys of
`lispm-desktop-apps', those installed from the App Store and those of
`lispm-desktop-standalone-built-in' that run here."
  (seq-keep (pcase-lambda (`(,key . ,props))
              (and (functionp (plist-get props :activate))
                   (if-let* ((built-in (assq key lispm-desktop-standalone-built-in)))
                       (or (null (cdr built-in))
                           (and (fboundp (cdr built-in)) (funcall (cdr built-in))))
                     (plist-get props :store))
                   key))
            lispm-desktop-apps))

;;;; The Dock

(defvar lispm-desktop-standalone--dock nil
  "What LispM's tile in the Dock shows: nil, LispM's own; the key of the
application whose icon it wears; or `none', out of the Dock.")

(defun lispm-desktop-standalone--dock-wanted ()
  "Return what LispM's tile in the Dock should show now, as
`lispm-desktop-standalone--dock' says it: with the desktop out of sight,
the icon of the application in front while LispM shows a window itself,
not drawn in an application's process, else nothing; with the desktop in
sight or in the Dock, LispM's own."
  (when-let* ((screen (lispm-desktop-standalone--screen)))
    (if-let* ((shown (seq-filter (lambda (frame)
                                   (and (frame-visible-p frame)
                                        (not (lispm-desktop-display-exported-p frame))
                                        (not (lispm-desktop-standalone--launching-p
                                              (frame-parameter frame 'lispm-desktop-app)))))
                                 (lispm-desktop-apps-windows screen))))
        (let ((front (lispm-desktop-apps-frontmost)))
          (if (seq-some (lambda (frame) (eq (frame-parameter frame 'lispm-desktop-app) front)) shown)
              front
            (or (frame-parameter (car shown) 'lispm-desktop-app) 'desktop)))
      'none)))

(defvar lispm-desktop-standalone--launched nil
  "The applications whose process LispM started, and when: (APP . TIME).")

(defun lispm-desktop-standalone-own-process-p (app)
  "Return non-nil when APP's windows shown out of sight are drawn in its own
process: its application of the computer's own is made."
  (and (lispm-desktop-standalone--screen)
       (memq app (lispm-desktop-standalone-apps))
       (file-directory-p (lispm-desktop-standalone-bundle app))))

(defun lispm-desktop-standalone--launching-p (app)
  "Return non-nil while the process of APP, started by LispM, is starting."
  (when-let* ((time (alist-get app lispm-desktop-standalone--launched)))
    (< (float-time (time-since time)) 30)))

(defun lispm-desktop-standalone--launch-processes ()
  "Start the process of each application whose window LispM shows out of
sight itself, as one opened from another's window, from the App Store:
each application has its own, its window drawn there."
  (when-let* ((screen (lispm-desktop-standalone--screen)))
    (dolist (frame (lispm-desktop-apps-windows screen))
      (when-let* (((frame-visible-p frame))
                  ((not (lispm-desktop-display-exported-p frame)))
                  (app (frame-parameter frame 'lispm-desktop-app))
                  ((lispm-desktop-standalone-own-process-p app))
                  ((not (lispm-desktop-standalone--launching-p app)))
                  (bundle (lispm-desktop-standalone-bundle app))
                  ((lispm-desktop-display-start)))
        (setf (alist-get app lispm-desktop-standalone--launched) (current-time))
        (call-process "open" nil 0 nil bundle)))))

(defconst lispm-desktop-standalone-dock-delay 2
  "Seconds LispM waits, its last window closed out of sight, before it leaves
the Dock: an application opened meanwhile keeps its tile there.")

(defvar lispm-desktop-standalone--dock-timer nil
  "Timer taking LispM out of the Dock once no window is left.")

(defun lispm-desktop-standalone--dock-update (&rest _)
  "Make LispM's tile in the Dock show what it should, see
`lispm-desktop-standalone--dock-wanted', at once, but for leaving the Dock,
which waits `lispm-desktop-standalone-dock-delay' seconds.  Out of the
Dock, LispM has no menu bar either; back in, it takes the menu bar again.
This is run whenever the application in front or the windows shown may
change; a window that should be drawn in its application's process gets
one first, see `lispm-desktop-standalone--launch-processes'."
  (lispm-desktop-standalone--launch-processes)
  (let ((wanted (lispm-desktop-standalone--dock-wanted)))
    (when (timerp lispm-desktop-standalone--dock-timer)
      (cancel-timer lispm-desktop-standalone--dock-timer)
      (setq lispm-desktop-standalone--dock-timer nil))
    (if (and (eq wanted 'none) (not (eq lispm-desktop-standalone--dock 'none)))
        (setq lispm-desktop-standalone--dock-timer
              (run-with-timer lispm-desktop-standalone-dock-delay nil
                              #'lispm-desktop-standalone--dock-show 'none))
      (lispm-desktop-standalone--dock-show wanted))))

(defun lispm-desktop-standalone--dock-show (wanted)
  "Make LispM's tile in the Dock show WANTED, as
`lispm-desktop-standalone--dock' says it, unless it does, or no longer
should."
  (setq lispm-desktop-standalone--dock-timer nil)
  (when (eq wanted (lispm-desktop-standalone--dock-wanted))
    (unless (eq wanted lispm-desktop-standalone--dock)
      (let ((icon (and wanted (not (eq wanted 'none)) (lispm-desktop-standalone--icon-file wanted)))
            (out (eq lispm-desktop-standalone--dock 'none)))
        (if (eq wanted 'none)
            ;; Accessory: no tile in the Dock, no menu bar.
            (lispm-desktop-standalone--accessory t)
          (lispm-desktop-standalone--accessory nil)
          (lispm-desktop-standalone--appkit
           (concat
            (if icon
                (format "ca's NSApp's setApplicationIconImage:(ca's NSImage's alloc()'s initWithContentsOfFile:%s)\n"
                        (lispm-desktop-standalone--string icon))
              "ca's NSApp's setApplicationIconImage:(missing value)\n")
            ;; Back from accessory, AppKit gives the menu bar only to an
            ;; application activated again.
            (if out "ca's NSApp's activateIgnoringOtherApps:true" "")))))
      (setq lispm-desktop-standalone--dock wanted))))

(defun lispm-desktop-standalone--accessory (on)
  "Take LispM out of the Dock and the menu bar, ON, or put it back.  The
display's module keeps it out while Emacs makes frames, which would put
it back each time."
  (if (fboundp 'lispm-desktop-display-module-accessory)
      (lispm-desktop-display-module-accessory on)
    (lispm-desktop-standalone--appkit (format "ca's NSApp's setActivationPolicy:%d" (if on 1 0)))))

(defun lispm-desktop-standalone--dock-update-soon ()
  "Update LispM's tile in the Dock once what is changing has changed: a
window its process let go of is deleted just after.
This is one of `lispm-desktop-display-changed-hook'."
  (run-at-time 0 nil #'lispm-desktop-standalone--dock-update))

(defun lispm-desktop-standalone--frame-deleted (_frame)
  "Update LispM's tile in the Dock once a frame is gone.
This is one of `delete-frame-functions', run while the frame still lives."
  (when (lispm-desktop-standalone--screen)
    (run-at-time 0 nil #'lispm-desktop-standalone--dock-update)))

;;;; Quitting an application

(defun lispm-desktop-standalone--close (frame)
  "Close FRAME, a floating window, whole, as its close box closes it."
  (if (eq (frame-parameter frame 'lispm-desktop-close-function) #'lispm-desktop-app-window-close-box)
      (lispm-desktop-app-window-close (lispm-desktop-app-window-id frame))
    (lispm-desktop-platinum-close-window (frame-selected-window frame))))

(defun lispm-desktop-standalone-quit ()
  "Quit the application in front.  With the desktop out of sight, its
windows close and LispM runs on out of sight, for the other applications;
else LispM quits, as \\[save-buffers-kill-emacs] does."
  (interactive)
  (if-let* ((screen (lispm-desktop-standalone--screen))
            (app (frame-parameter (window-frame (selected-window)) 'lispm-desktop-app)))
      (dolist (frame (lispm-desktop-apps-windows screen))
        (when (and (frame-live-p frame) (eq (frame-parameter frame 'lispm-desktop-app) app))
          (lispm-desktop-standalone--close frame)))
    (call-interactively #'save-buffers-kill-emacs)))

;;;; The computer's applications
;;
;; Each is a bundle of a few files: Info.plist, saying which application
;; it is, where LispM listens and how to start it out of sight; the
;; executable app.m builds to, see lispm-desktop-display.el; and the
;; icon, the application's own over the tile of macOS's grid, without
;; which macOS 26 frames it grey.

(defun lispm-desktop-standalone--file-name (app)
  "Return the file name, without .app, of the computer's application for APP."
  (let ((name (or (plist-get (alist-get app lispm-desktop-apps) :name) (symbol-name app))))
    (string-trim (replace-regexp-in-string "[/:]" "-" name))))

(defun lispm-desktop-standalone-bundle (app)
  "Return the directory of the computer's application for APP."
  (expand-file-name (concat (lispm-desktop-standalone--file-name app) ".app")
                    lispm-desktop-standalone-directory))

(defun lispm-desktop-standalone--icon-file (app)
  "Return the icon file of the computer's application for APP, if made."
  (let ((file (expand-file-name "Contents/Resources/AppIcon.icns" (lispm-desktop-standalone-bundle app))))
    (and (file-readable-p file) file)))

(defun lispm-desktop-standalone--source-icon (app)
  "Return the file of APP's icon, its large one where it has one."
  (when-let* ((name (or (lispm-desktop-apps-large-icon app)
                        (plist-get (alist-get app lispm-desktop-apps) :icon))))
    (let ((file (lispm-desktop-platinum-icon-file name)))
      (and (file-readable-p file) file))))

(defun lispm-desktop-standalone--emacs ()
  "Return how to start the Emacs LispM runs in: (APPLICATION ARGUMENTS...).
APPLICATION is its bundle, as LispM.app; its arguments keep the desktop out
of sight and, for an Emacs that is not LispM.app, name this init directory
unless it is Emacs's own default.  Nil outside an application bundle."
  (let ((directory (directory-file-name (file-truename invocation-directory))))
    (when (string-match "\\`\\(.*\\.app\\)/Contents/MacOS\\'" directory)
      (let ((application (match-string 1 directory)))
        `(,application
          ,lispm-desktop-standalone-switch
          ;; LispM.app's own executable names the init directory.
          ,@(unless (or (file-executable-p (expand-file-name "LispM" invocation-directory))
                        (member (file-name-as-directory (expand-file-name user-emacs-directory))
                                (mapcar #'expand-file-name '("~/.emacs.d/" "~/.config/emacs/"))))
              (list (concat "--init-directory=" (expand-file-name user-emacs-directory)))))))))

(defun lispm-desktop-standalone--identifier (app)
  "Return the bundle identifier of the computer's application for APP."
  (concat "org.lispmachinelab.lispm.app."
          (replace-regexp-in-string "[^A-Za-z0-9.-]" "-" (symbol-name app))))

(defun lispm-desktop-standalone--xml (string)
  "Return STRING escaped for XML."
  (replace-regexp-in-string
   "[&<>]" (lambda (char) (pcase char ("&" "&amp;") ("<" "&lt;") (">" "&gt;"))) string t t))

(defun lispm-desktop-standalone--stamp (app emacs executable)
  "Return what the computer's application for APP is made from, hashed: when
it changes, the application is made anew.  EMACS is as
`lispm-desktop-standalone--emacs' returns it, EXECUTABLE the one built."
  (let ((icon (lispm-desktop-standalone--source-icon app)))
    (secure-hash 'sha1 (prin1-to-string
                        (list 2 app (lispm-desktop-standalone--file-name app) emacs executable
                              (lispm-desktop-display-socket) icon
                              (and icon (file-attribute-modification-time (file-attributes icon))))))))

(defun lispm-desktop-standalone--info-plist (app emacs stamp)
  "Return the Info.plist of the computer's application for APP, starting
EMACS, as `lispm-desktop-standalone--emacs' returns it; STAMP its stamp."
  (let ((name (lispm-desktop-standalone--xml (lispm-desktop-standalone--file-name app))))
    (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
<plist version=\"1.0\">
<dict>
	<key>CFBundleDevelopmentRegion</key>
	<string>en</string>
	<key>CFBundleDisplayName</key>
	<string>" name "</string>
	<key>CFBundleExecutable</key>
	<string>" name "</string>
	<key>CFBundleIconFile</key>
	<string>AppIcon</string>
	<key>CFBundleIdentifier</key>
	<string>" (lispm-desktop-standalone--identifier app) "</string>
	<key>CFBundleInfoDictionaryVersion</key>
	<string>6.0</string>
	<key>CFBundleName</key>
	<string>" name "</string>
	<key>CFBundlePackageType</key>
	<string>APPL</string>
	<key>CFBundleShortVersionString</key>
	<string>1.0</string>
	<key>CFBundleVersion</key>
	<string>1</string>
	<key>LSMinimumSystemVersion</key>
	<string>11.0</string>
	<key>NSHighResolutionCapable</key>
	<true/>
	<key>NSPrincipalClass</key>
	<string>NSApplication</string>
	<key>LispMApp</key>
	<string>" (lispm-desktop-standalone--xml (symbol-name app)) "</string>
	<key>LispMSocket</key>
	<string>" (lispm-desktop-standalone--xml (lispm-desktop-display-socket)) "</string>
	<key>LispMApplication</key>
	<string>" (lispm-desktop-standalone--xml (car emacs)) "</string>
	<key>LispMArguments</key>
	<array>
" (mapconcat (lambda (argument) (concat "\t\t<string>" (lispm-desktop-standalone--xml argument) "</string>\n"))
             (cdr emacs) "") "\t</array>
	<key>LispMStamp</key>
	<string>" stamp "</string>
</dict>
</plist>
")))

(defun lispm-desktop-standalone--draw-icon (source iconset)
  "Draw SOURCE, an icon file, over the tile of macOS's grid into ICONSET, a
directory of PNG files at every size `iconutil' takes.  The tile is
platinum, 824 points of 1024 with corners of 185, and no shadow."
  (lispm-desktop-standalone--appkit
   (format "set icon to ca's NSImage's alloc()'s initWithContentsOfFile:%s
if icon is missing value then error \"no icon\"
repeat with entry in {{16, \"16x16\"}, {32, \"16x16@2x\"}, {32, \"32x32\"}, {64, \"32x32@2x\"}, {128, \"128x128\"}, {256, \"128x128@2x\"}, {256, \"256x256\"}, {512, \"256x256@2x\"}, {512, \"512x512\"}, {1024, \"512x512@2x\"}}
  set px to item 1 of entry
  set s to px / 1024
  set rep to ca's NSBitmapImageRep's alloc()'s initWithBitmapDataPlanes:(missing value) pixelsWide:px pixelsHigh:px bitsPerSample:8 samplesPerPixel:4 hasAlpha:true isPlanar:false colorSpaceName:(ca's NSDeviceRGBColorSpace) bytesPerRow:0 bitsPerPixel:0
  ca's NSGraphicsContext's saveGraphicsState()
  ca's NSGraphicsContext's setCurrentContext:(ca's NSGraphicsContext's graphicsContextWithBitmapImageRep:rep)
  set tile to ca's NSBezierPath's bezierPathWithRoundedRect:{{100 * s, 100 * s}, {824 * s, 824 * s}} xRadius:(185 * s) yRadius:(185 * s)
  set shade to ca's NSGradient's alloc()'s initWithStartingColor:(ca's NSColor's colorWithCalibratedWhite:0.94 alpha:1) endingColor:(ca's NSColor's colorWithCalibratedWhite:0.76 alpha:1)
  shade's drawInBezierPath:tile angle:-90
  (ca's NSGraphicsContext's currentContext())'s setImageInterpolation:(ca's NSImageInterpolationNone)
  icon's drawInRect:{{224 * s, 224 * s}, {576 * s, 576 * s}} fromRect:(ca's NSZeroRect) operation:(ca's NSCompositingOperationSourceOver) fraction:1.0
  ca's NSGraphicsContext's restoreGraphicsState()
  set png to rep's representationUsingType:(ca's NSBitmapImageFileTypePNG) |properties|:(ca's NSDictionary's dictionary())
  png's writeToFile:(%s & \"/icon_\" & (item 2 of entry) & \".png\") atomically:true
end repeat
return \"ok\""
           (lispm-desktop-standalone--string source)
           (lispm-desktop-standalone--string (expand-file-name iconset)))))

(defun lispm-desktop-standalone--write (app emacs executable stamp bundle then)
  "Make the computer's application for APP at BUNDLE, a directory not there
yet: EXECUTABLE its own, starting EMACS; stamped STAMP.  Then call THEN,
iconutil having made its icon and codesign sealed it, in the background."
  (let* ((contents (expand-file-name "Contents" bundle))
         (iconset (expand-file-name "AppIcon.iconset" contents)))
    (make-directory (expand-file-name "MacOS" contents) t)
    (make-directory (expand-file-name "Resources" contents) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (lispm-desktop-standalone--info-plist app emacs stamp) nil
                    (expand-file-name "Info.plist" contents) nil 'silent))
    (copy-file executable (expand-file-name (concat "MacOS/" (lispm-desktop-standalone--file-name app)) contents))
    (cl-flet ((seal (&rest _)
                (when (file-directory-p iconset)
                  (delete-directory iconset t))
                ;; Sealed as a whole, its executable's own signature with it.
                (lispm-exec "codesign" (list "--force" "--sign" "-" bundle)
                            (lambda (&rest _) (funcall then)))))
      (if-let* ((source (lispm-desktop-standalone--source-icon app))
                ((progn (make-directory iconset t)
                        (equal (lispm-desktop-standalone--draw-icon source iconset) "ok"))))
          (lispm-exec "iconutil" (list "-c" "icns" iconset
                                       "-o" (expand-file-name "Resources/AppIcon.icns" contents))
                      #'seal)
        (seal)))))

(defun lispm-desktop-standalone--made-stamp (bundle)
  "Return the stamp of BUNDLE, a computer's application made for one of
LispM's, or nil when it is none."
  (let ((plist (expand-file-name "Contents/Info.plist" bundle)))
    (when (file-readable-p plist)
      (with-temp-buffer
        (insert-file-contents plist)
        (and (re-search-forward "<key>LispMStamp</key>[ \t\n]*<string>\\([^<]*\\)</string>" nil t)
             (match-string 1))))))

(defvar lispm-desktop-standalone--register-program
  "/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister"
  "Launch Services' program, which tells the Finder and the Dock of an
application made or changed at once.")

(defun lispm-desktop-standalone--ensure (app emacs executable then)
  "Make the computer's application for APP, EXECUTABLE its own, starting
EMACS, unless it is made as it should be; then call THEN with its
directory, or nil when it could not be made, saying why."
  (let* ((bundle (lispm-desktop-standalone-bundle app))
         (stamp (lispm-desktop-standalone--stamp app emacs executable)))
    (if (equal (lispm-desktop-standalone--made-stamp bundle) stamp)
        (funcall then bundle)
      (let ((new (concat (directory-file-name bundle) ".new")))
        (when (file-directory-p new)
          (delete-directory new t))
        (lispm-desktop-standalone--write
         app emacs executable stamp new
         (lambda ()
           (funcall then
                    (condition-case err
                        (progn
                          (when (file-directory-p bundle)
                            (delete-directory bundle t))
                          (rename-file new bundle)
                          (when (file-executable-p lispm-desktop-standalone--register-program)
                            (start-process "lispm-lsregister" nil lispm-desktop-standalone--register-program
                                           "-f" bundle))
                          bundle)
                      (error (lispm-desktop-standalone--not-made app err)
                             nil)))))))))

(defun lispm-desktop-standalone--not-made (app error)
  "Say that the computer's application for APP could not be made, ERROR why."
  (message (lispm-desktop-tr "LispM could not make an application for %s: %s")
           app (error-message-string error)))

(defvar lispm-desktop-standalone--syncing nil
  "Nil, or while the computer's applications are being made, t, or
`again' when they are to be made again once done.")

(defun lispm-desktop-standalone-sync ()
  "Make the computer's applications for LispM's as they should be: one for
each of `lispm-desktop-standalone-apps', made anew where what it is made
from changed, and none for another, see `lispm-desktop-standalone-launchers'."
  (interactive)
  (if-let* ((why (lispm-desktop-standalone-unsupported)))
      (when (called-interactively-p 'interactive)
        (user-error (lispm-desktop-tr "Applications cannot open on their own: %s") why))
    (let ((emacs (lispm-desktop-standalone--emacs))
          (executable (and lispm-desktop-standalone-launchers
                           (or (lispm-desktop-display-executable)
                               ;; Made once it is built.
                               (progn (lispm-desktop-display-build #'lispm-desktop-standalone--schedule-sync)
                                      nil))))
          (directory (file-name-as-directory (expand-file-name lispm-desktop-standalone-directory))))
      (if lispm-desktop-standalone--syncing
          (setq lispm-desktop-standalone--syncing 'again)
        (setq lispm-desktop-standalone--syncing t)
        (let* ((apps (and emacs executable (lispm-desktop-standalone-apps)))
               (left (length apps))
               (made nil))
          (cl-flet ((finish ()
                      (unwind-protect
                          (progn
                            ;; What cannot be built now leaves those made before alone.
                            (when (and (file-directory-p directory)
                                       (or executable (not lispm-desktop-standalone-launchers)))
                              (dolist (bundle (directory-files directory t "\\.app\\'"))
                                (when (and (lispm-desktop-standalone--made-stamp bundle)
                                           (not (member (file-name-as-directory bundle) made)))
                                  (delete-directory bundle t))))
                            ;; The tile wears an icon made just now.
                            (when (lispm-desktop-standalone--screen)
                              (setq lispm-desktop-standalone--dock 'stale)
                              (lispm-desktop-standalone--dock-update)))
                        (let ((again (eq lispm-desktop-standalone--syncing 'again)))
                          (setq lispm-desktop-standalone--syncing nil)
                          (when again
                            (run-at-time 0 nil #'lispm-desktop-standalone-sync))))))
            (if (zerop left)
                (finish)
              (make-directory directory t)
              (dolist (app apps)
                (cl-flet ((made (bundle)
                            (when bundle
                              (push (file-name-as-directory bundle) made))
                            (when (zerop (setq left (1- left)))
                              (finish))))
                  (condition-case err
                      (lispm-desktop-standalone--ensure app emacs executable #'made)
                    (error (lispm-desktop-standalone--not-made app err)
                           (made nil))))))))))))

(defvar lispm-desktop-standalone--sync-timer nil
  "Timer making the computer's applications once Emacs is idle.")

(defun lispm-desktop-standalone--schedule-sync ()
  "Make the computer's applications as they should be once Emacs is idle a
moment, as applications come and go.
This is one of `lispm-desktop-apps-changed-hook'."
  (unless (or (lispm-desktop-standalone-unsupported) (timerp lispm-desktop-standalone--sync-timer))
    (setq lispm-desktop-standalone--sync-timer
          (run-with-idle-timer 3 nil (lambda ()
                                       (setq lispm-desktop-standalone--sync-timer nil)
                                       (lispm-desktop-standalone-sync)
                                       ;; An application may open on its own any time.
                                       (when lispm-desktop-standalone-launchers
                                         (lispm-desktop-display-start)))))))

;;;; Setup

(defun lispm-desktop-setup-standalone ()
  "Let LispM's applications open on their own, the desktop out of sight:
started so, keep it out of sight; draw the windows of the applications'
processes and listen for them; dress the Dock as windows come and go;
⌘Q quits only the application in front; and keep the computer's
applications made."
  ;; Taken, not visited as a file.
  (add-to-list 'command-switch-alist (cons lispm-desktop-standalone-switch #'ignore))
  (lispm-desktop-setup-display)
  ;; Last, once the desktop is drawn and put in front, its Control Strip shown.
  (add-hook 'window-setup-hook #'lispm-desktop-standalone--hide-at-start 100)
  (add-hook 'lispm-desktop-apps-frontmost-functions #'lispm-desktop-standalone--dock-update)
  (add-hook 'lispm-desktop-coherence-changed-hook #'lispm-desktop-standalone--dock-update)
  ;; Once a window let go is gone, or shown by LispM itself.
  (add-hook 'lispm-desktop-display-changed-hook #'lispm-desktop-standalone--dock-update-soon)
  (add-hook 'delete-frame-functions #'lispm-desktop-standalone--frame-deleted)
  (add-hook 'lispm-desktop-apps-changed-hook #'lispm-desktop-standalone--schedule-sync)
  (when (eq system-type 'darwin)
    (keymap-global-set "s-q" #'lispm-desktop-standalone-quit))
  (lispm-desktop-standalone--schedule-sync))

(provide 'lispm-desktop-standalone)
;;; lispm-desktop-standalone.el ends here
