;;; lispm-desktop-auth.el --- Opening LispM's secrets at the start  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

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

;; LispM Auth keeps LispM's keys encrypted, in a store opened with a
;; password.  The desktop opens it once, at the start, and only when an
;; application about to start declares that it uses secrets: the desktop
;; itself by default, and `lispm' whenever LispM starts.  An ordinary
;; start, of applications that declare nothing, asks for no password and
;; touches none of the encrypted data.
;;
;; An application opened on its own starts LispM out of sight, see
;; lispm-desktop-standalone.el.  Its bundle names it with
;; --lispm-app=KEY, and only that application is judged: an application
;; that declares nothing, as a game does, never shows the password page,
;; nor opens the store.  Without KEY, a start out of sight judges
;; nothing, which is safer than asking for every application.
;;
;; What the desktop declares is a declaration of LispM Auth's
;; (`lispm-auth-set-app-requires-password'), kept with the store, not
;; here: Control Panels changes it, and the desktop reads it at the next
;; start.  LispM itself always declares, and Control Panels cannot revoke
;; that; LispM's own start and Focus Mode call
;; `lispm-desktop-auth-start-lispm' for it.
;;
;; LispM Auth is a system component, installed by the desktop's dependency
;; bootstrap.  Its service is installed from the client's pinned release.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-ui)
(require 'lispm-desktop-platinum)
(require 'lispm-desktop-i18n)

(declare-function lispm-auth-install-service "lispm-auth" (&optional archive))
(declare-function lispm-auth--service-program "lispm-auth" ())
(defvar lispm-auth-service-program)
(declare-function lispm-auth-app-requires-password-p "lispm-auth" (app))
(declare-function lispm-auth-start-app "lispm-auth" (app))
(declare-function lispm-auth-start-apps "lispm-auth" (apps))
(declare-function lispm-auth-store-exists-p "lispm-auth" ())
(declare-function lispm-auth-locked-p "lispm-auth" ())
(declare-function lispm-auth-unlock "lispm-auth" ())
(declare-function lispm-auth-lock "lispm-auth" ())
(declare-function lispm-auth-change-password "lispm-auth" (&optional new-password))
(declare-function lispm-auth-set-app-requires-password "lispm-auth" (app required))
(declare-function lispm-auth--page-logo-file "lispm-auth" ())
(declare-function lispm-auth--read-password "lispm-auth" (prompt &optional confirm))
(declare-function lispm-terminal-p "lispm-terminal" (&optional frame))
(declare-function lispm-desktop-started-as-terminal-p "lispm-desktop-start-as" ())
(declare-function lispm-desktop-apps-screen "lispm-desktop-apps" (&optional frame))
(declare-function lispm-desktop-apps-windows "lispm-desktop-apps" (&optional screen))
(declare-function lispm-desktop-platinum-hide-window "lispm-desktop-platinum" (frame))
(declare-function lispm-desktop-platinum-show-window "lispm-desktop-platinum" (frame))
(declare-function lispm-desktop-platinum-scroll-sync "lispm-desktop-platinum-scroll" ())
(declare-function lispm-desktop-assistant-ensure-progress "lispm-desktop-assistant" (&optional status))
(declare-function lispm-desktop-assistant-confirm-update "lispm-desktop-assistant" (template &rest args))
(declare-function lispm-desktop-assistant--visible-p "lispm-desktop-assistant" ())
(declare-function lispm-desktop-assistant--package-message "lispm-desktop-assistant" (original format-string &rest args))
(declare-function lispm-desktop-assistant--tr-status "lispm-desktop-assistant" (status &rest args))
(defvar lispm-auth-service-directory)
(defvar lispm-auth-service-version)
(defvar lispm-desktop-console--quiet)
(defvar lispm-auth-page-logo)
(defvar lispm-auth-page-window-draw-function)
(defvar lispm-auth-page-window-event-function)
(defvar lispm-auth-page-windowed)
(defvar lispm-auth-password-function)
(defvar lispm-auth-translate-function)
(defconst lispm-desktop-auth--directory
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "The desktop's directory, a checkout of lispm-desktop.")

(defconst lispm-desktop-auth--messages
  (list (lispm-desktop-tr-noop "Current password")
        (lispm-desktop-tr-noop "New password")
        (lispm-desktop-tr-noop "Enter it again")
        (lispm-desktop-tr-noop "LispM password")
        (lispm-desktop-tr-noop "Enter the new password again:")
        (lispm-desktop-tr-noop "LispM password:")
        (lispm-desktop-tr-noop "New LispM password:")
        (lispm-desktop-tr-noop "Choose a password for %s's secrets.  It is asked for at each start.")
        (lispm-desktop-tr-noop "Return verifies the current password.")
        (lispm-desktop-tr-noop "Return confirms")
        (lispm-desktop-tr-noop "Esc cancels")
        (lispm-desktop-tr-noop "Type the password.")
        (lispm-desktop-tr-noop "The passwords do not match.  Try again.")
        (lispm-desktop-tr-noop "Wrong password.  Try again (%d of %d)."))
  "Messages shown by LispM Auth's password page.")

(defun lispm-desktop-auth--tr (message &rest args)
  "Translate MESSAGE with ARGS in the desktop's catalog."
  (cond
   ((and (equal message "Return opens the store.") (null args))
    (lispm-desktop-tr "Return unlocks LispM."))
   ((and (stringp message) (null args)
         (string-match "\\`Wrong password\\.  Try again (\\([0-9]+\\) of \\([0-9]+\\))\\.\\'"
                       message))
    (lispm-desktop-tr "Wrong password.  Try again (%d of %d)."
                      (string-to-number (match-string 1 message))
                      (string-to-number (match-string 2 message))))
   (t
    (apply #'lispm-desktop-tr message args))))

(defun lispm-desktop-auth--clear-echo ()
  "Clear echoed keys and any minibuffer message while reading a password."
  (clear-this-command-keys)
  (when (and (not noninteractive) (current-message))
    (let ((message-log-max nil))
      (message nil))))

(defun lispm-desktop-auth--patch-legacy-auth ()
  "Equip an older `lispm-auth' client with translation and echo suppression."
  (unless (fboundp 'lispm-auth--tr)
    (defalias 'lispm-auth--tr #'lispm-desktop-auth--tr)
    (when (fboundp 'lispm-auth--page-read)
      (advice-add
       'lispm-auth--page-read :around
       (lambda (orig prompt confirm message)
         (let ((echo-keystrokes 0))
           (lispm-desktop-auth--clear-echo)
           (unwind-protect
               (funcall orig prompt confirm
                        (and message (lispm-desktop-auth--tr message)))
             (lispm-desktop-auth--clear-echo))))))
    (when (fboundp 'lispm-auth--page-handle)
      (advice-add
       'lispm-auth--page-handle :before
       (lambda (&rest _) (lispm-desktop-auth--clear-echo))))
    (dolist (fn '(lispm-auth--page-title
                  lispm-auth--page-placeholder
                  lispm-auth--page-prompt))
      (when (fboundp fn)
        (advice-add fn :filter-return #'lispm-desktop-auth--tr)))
    (when (fboundp 'lispm-auth--page-answer)
      (advice-add
       'lispm-auth--page-answer :filter-return
       (lambda (str)
         (if (stringp str)
             (let ((face (get-text-property 0 'face str)))
               (propertize (lispm-desktop-auth--tr (substring-no-properties str))
                           'face face))
           str))))
    (when (fboundp 'lispm-auth--page-footer)
      (advice-add
       'lispm-auth--page-footer :override
       (lambda (page)
         (concat (lispm-desktop-tr "Return confirms")
                 "        "
                 (lispm-desktop-tr "Esc cancels")
                 (when (plist-get page :change)
                   (concat "        "
                           (lispm-desktop-tr "C-p changes the password")))))))
    (when (fboundp 'lispm-auth--page-lines)
      (advice-add
       'lispm-auth--page-lines :filter-return
       (lambda (lines)
         (mapcar (lambda (line)
                   (if (and (stringp line)
                            (equal (substring-no-properties line)
                                   "Encrypted Secrets"))
                       (propertize (lispm-desktop-tr "Encrypted Secrets")
                                   'face (get-text-property 0 'face line))
                     line))
                 lines))))))

(defconst lispm-desktop-auth-hidden-switch "--lispm-hidden"
  "The option LispM is started with to keep its desktop out of sight.
It is `lispm-desktop-standalone-switch'; kept here too, so that this
file does not need the standalone's to load.")

(defconst lispm-desktop-auth-app-switch "--lispm-app"
  "The option an application opened on its own starts LispM with.
Its value, the application's key as `lispm-desktop-apps' names it,
tells LispM Auth which application this start is for.")

(defcustom lispm-desktop-auth-apps '(desktop)
  "Applications the desktop starts that are told it uses secrets.
LispM Auth asks for the store's password at a start when one of these
declares that it uses secrets.  The desktop itself declares by default,
until Control Panels revokes it; LispM always declares, and is opened
when it starts whether or not it is named here, see
`lispm-desktop-auth-start-lispm'."
  :type '(repeat symbol)
  :group 'lispm-desktop)

(defcustom lispm-desktop-auth-lispm-apps '(lispm lispm-agent agent)
  "Keys that name LispM itself when it is opened on its own.
An application opened on its own is judged by its own key, but LispM
Agent always declares that it uses secrets, as LispM does; a bundle
made for it opens LispM's store whichever of these keys it was made
with."
  :type '(repeat symbol)
  :group 'lispm-desktop)

(defun lispm-desktop-auth-ensure ()
  "Load LispM Auth, here or in a checkout beside the desktop's.
Once it is loaded, LispM Auth draws its password pages the way the
desktop does: the pages that set a password, and every page of an
application opened on its own, as a window on LispM's frame; the page
LispM's own start asks the password on fills the window.  Return
non-nil when LispM Auth is loaded."
  (when (or (require 'lispm-auth nil t)
            (let ((directory (expand-file-name "../lispm-auth"
                                               lispm-desktop-auth--directory)))
              (when (file-exists-p (expand-file-name "lispm-auth.el" directory))
                (add-to-list 'load-path directory)
                (require 'lispm-auth nil t))))
    (setq lispm-auth-translate-function #'lispm-desktop-auth--tr)
    (lispm-desktop-auth--patch-legacy-auth)
    (when (and (boundp 'lispm-auth-page-logo)
               (or (null lispm-auth-page-logo)
                   (not (and (stringp lispm-auth-page-logo)
                             (file-readable-p lispm-auth-page-logo))))
               (not (and (fboundp 'lispm-auth--page-logo-file)
                         (lispm-auth--page-logo-file))))
      (let ((fallback (or (and (fboundp 'lispm-ui-icon-file)
                               (ignore-errors (lispm-ui-icon-file 'lambda)))
                          (expand-file-name "icons/lispm-lambda.png"
                                            lispm-desktop-auth--directory))))
        (when (and (stringp fallback) (file-readable-p fallback))
          (setq lispm-auth-page-logo fallback))))
    (when (boundp 'lispm-auth-page-window-event-function)
      (setq lispm-auth-page-window-event-function
            #'lispm-desktop-auth--setup-page-event))
    (when (fboundp 'lispm-auth--page-show)
      (advice-add 'lispm-auth--page-show :around
                  #'lispm-desktop-auth--around-page-show))
    (when (fboundp 'lispm-auth--ensure-process)
      (advice-add 'lispm-auth--ensure-process :before
                  #'lispm-desktop-auth--before-ensure-process))
    (when (fboundp 'lispm-auth-store-exists-p)
      (setq lispm-auth-page-window-draw-function
            #'lispm-desktop-auth--setup-page-draw
            ;; An application opened on its own asks for the password in a
            ;; window; LispM's own start asks for it across the window.
            lispm-auth-page-windowed
            (and (lispm-desktop-auth-started-hidden-p) t)))
    t))

(defun lispm-desktop-auth-started-hidden-p ()
  "Return non-nil when LispM was started with its desktop out of sight,
as for an application opened on its own."
  (and (member lispm-desktop-auth-hidden-switch command-line-args) t))

(defun lispm-desktop-auth-command-line-app ()
  "Return the application key this start was given with --lispm-app, or nil."
  (let ((arguments command-line-args)
        (app nil))
    (while (and arguments (not app))
      (let ((argument (pop arguments)))
        (cond ((equal argument "--") (setq arguments nil))
              ((string-match "\\`--?lispm-app=\\(.*\\)\\'" argument)
               (when-let* ((value (match-string 1 argument)))
                 (unless (string-empty-p value)
                   (setq app (intern value)))))
              ((equal argument lispm-desktop-auth-app-switch)
               (when-let* ((value (pop arguments)))
                 (unless (string-empty-p value)
                   (setq app (intern value))))))))
    app))

(defun lispm-desktop-auth--switch (switch)
  "Take the application's key off the command line, which
`lispm-desktop-auth-command-line-app' has read already.  This is a
function of `command-switch-alist'."
  ;; The key is given as --lispm-app=KEY; otherwise the next word is it.
  (unless (string-match-p "\\`--?lispm-app=" (or switch ""))
    (pop command-line-args-left)))

(add-to-list 'command-switch-alist
             (cons "-lispm-app" #'lispm-desktop-auth--switch))

(defun lispm-desktop-auth--apps (&optional given)
  "Return the applications this start asks LispM Auth about.
An application opened on its own is the one named with --lispm-app; a
start out of sight that names none asks about none.  Otherwise GIVEN,
defaulting to `lispm-desktop-auth-apps'."
  (if (lispm-desktop-auth-started-hidden-p)
      (when-let* ((app (lispm-desktop-auth-command-line-app)))
        (list app))
    (or given lispm-desktop-auth-apps)))

;;;; The page that sets the password

;; On the first start LispM asks which language it speaks, how text is
;; edited and which appearance it wears on a page of the setup
;; assistant's: a window in the middle of the desktop, in Platinum.  The
;; password it asks for next is asked for the same way: a window in
;; LispM's own frame, where it is chosen and entered again, and said there
;; that it cannot be recovered.  An application opened on its own asks for
;; the password in a window too.  LispM's own start, at the password a
;; store already has, fills the window with the page instead: the lock
;; screen it is.

(defconst lispm-desktop-auth-setup-page-width 520
  "Width in pixels of the window that asks for a password.")

(defconst lispm-desktop-auth-setup-page-height 240
  "Height in pixels of the window that asks for a password.")

(defconst lispm-desktop-auth--setup-page-margin 20
  "Pixels left clear between the password window's sides and its
contents.")

(defun lispm-desktop-auth--setup-page-palette ()
  "Return the palette the password window is drawn in."
  (or (bound-and-true-p lispm-ui-current-palette)
      (ignore-errors (lispm-ui-palette-update))
      '(:desktop "#6E6E6E" :content "#FFFFFF" :chrome "#DDDDDD"
        :light "#FFFFFF" :text "#000000" :muted "#777777"
        :outline "#000000" :alert-dark "#CE3131")))

(defun lispm-desktop-auth--setup-page-title (page)
  "Return the title of the password window showing PAGE."
  (if (eq (plist-get page :operation) 'change)
      (lispm-desktop-tr "Change Password")
    (pcase (plist-get page :stage)
      ((or 'new 'again) (lispm-desktop-tr "Set a Password"))
      (_ "LispM"))))

(defun lispm-desktop-auth--setup-page-label (page)
  "Return the label of the field PAGE asks its password in."
  (pcase (plist-get page :stage)
    ('current (lispm-desktop-tr "Current password:"))
    ('new (lispm-desktop-tr "New password:"))
    ('again (lispm-desktop-tr "Repeat password:"))
    (_ (lispm-desktop-tr "Password:"))))

(defun lispm-desktop-auth--setup-page-detail (page)
  "Return what PAGE says under its heading, before its field."
  (pcase (plist-get page :stage)
    ('new (lispm-desktop-tr "Choose a password for LispM's secrets."))
    ('again (lispm-desktop-tr "Enter the new password again."))
    (_ (lispm-desktop-tr "Enter the password for LispM's secrets."))))

(defun lispm-desktop-auth--setup-page-answer (page)
  "Return what PAGE says under its field: the message, or what Return does."
  (if-let* ((message (plist-get page :message)))
      (lispm-desktop-auth--tr message)
    (pcase (plist-get page :stage)
      ('new (lispm-desktop-tr "It is asked for at each start, and cannot be recovered."))
      ('again (lispm-desktop-tr "It is asked for at each start."))
      ('current (lispm-desktop-tr "Return verifies the current password."))
      (_ (lispm-desktop-tr "Return unlocks LispM.")))))

(defun lispm-desktop-auth--setup-page-lock (svg palette x y size)
  "Draw a padlock SIZE pixels square on SVG at X, Y, in PALETTE.
It is drawn as Mac OS 9 drew its own: black outline, a lit metal body,
and a keyhole."
  (let* ((unit (/ size 32.0))
         (ink (or (plist-get palette :text) "#000000"))
         (metal (lispm-ui-grey palette #xC8))
         (lit (lispm-ui-grey palette #xFF)))
    ;; The shackle: a ring whose lower half the body covers.
    (svg-circle svg (+ x (* unit 16)) (+ y (* unit 12)) (* unit 7)
                :fill "none" :stroke ink :stroke-width (* unit 2.5))
    (svg-rectangle svg (+ x (* unit 5)) (+ y (* unit 12))
                   (* unit 22) (* unit 16) :rx (* unit 2)
                   :fill metal :stroke ink :stroke-width (* unit 1.5))
    ;; The body is lit along its top and left, as a Platinum icon is.
    (lispm-ui-svg-fill svg (+ x (* unit 7)) (+ y (* unit 14))
                       (* unit 18) 1 lit)
    ;; The keyhole.
    (svg-circle svg (+ x (* unit 16)) (+ y (* unit 18)) (* unit 2.2)
                :fill ink :stroke "none")
    (svg-rectangle svg (+ x (* unit 15.1)) (+ y (* unit 18))
                   (* unit 1.8) (* unit 5) :fill ink :stroke "none")))

(defun lispm-desktop-auth--setup-page-field (svg palette page x y width height)
  "Draw PAGE's masked password field on SVG at X, Y, WIDTH by HEIGHT pixels."
  (svg-rectangle svg x y width height
                 :fill (or (plist-get palette :content) "#FFFFFF")
                 :stroke (or (plist-get palette :outline) "#000000")
                 :stroke-width 1)
  (let* ((password (or (plist-get page :password) ""))
         (padding 7)
         (step 9)
         (left (+ x padding))
         (centre (+ y (/ height 2)))
         (dots (min (length password) (max 1 (/ (- width (* 2 padding)) step)))))
    (dotimes (index dots)
      (svg-circle svg (+ left 4 (* index step)) centre 3
                  :fill (or (plist-get palette :text) "#000000")
                  :stroke "none"))
    ;; The caret after the last character, as a field has one.
    (lispm-ui-svg-fill svg (+ left (* dots step)) (- centre 6) 1 12
                       (or (plist-get palette :outline) "#000000"))))

(defun lispm-desktop-auth--setup-page-buttons (cx cw cy ch)
  "Return the Cancel and Confirm button bounds inside CX, CW, CY, CH."
  (let* ((cancel-label (lispm-desktop-tr "Cancel"))
         (confirm-label (lispm-desktop-tr "Confirm"))
         (cancel-w (max 80 (if (fboundp 'lispm-ui-push-button-width)
                               (lispm-ui-push-button-width cancel-label)
                             80)))
         (confirm-w (max 80 (if (fboundp 'lispm-ui-push-button-width)
                                (lispm-ui-push-button-width confirm-label)
                              80)))
         (right (- (+ cx cw) lispm-desktop-auth--setup-page-margin))
         (confirm-x (- right confirm-w 10))
         (cancel-x (- confirm-x cancel-w 10))
         (button-y (- (+ cy ch) 30)))
    (list :y button-y
          :cancel (list cancel-x cancel-w cancel-label)
          :confirm (list confirm-x confirm-w confirm-label))))

(defun lispm-desktop-auth--setup-page-svg (width height page palette)
  "Return an SVG WIDTH by HEIGHT pixels of PAGE as a window on the desktop.
PAGE is a page asking for LispM's password; PALETTE colours it."
  (let* ((w lispm-desktop-auth-setup-page-width)
         (h lispm-desktop-auth-setup-page-height)
         (x (max 0 (/ (- width w) 2)))
         (y (max 0 (/ (- height h) 2)))
         (svg (svg-create width height))
         (text (or (plist-get palette :text) "#000000"))
         (muted (or (plist-get palette :muted) "#777777"))
         (margin lispm-desktop-auth--setup-page-margin)
         (title (propertize (lispm-desktop-auth--setup-page-title page)
                            'face 'lispm-ui-title)))
    (lispm-ui-svg-fill svg 0 0 width height
                       (if (or (> width w) (> height h))
                           (or (plist-get palette :desktop) "#6E6E6E")
                         (lispm-ui-grey palette #xDD)))
    (pcase-let* ((`(,left ,top ,right ,bottom) (lispm-desktop-platinum-frame-margins))
                 (cx (+ x left))
                 (cy (+ y top lispm-ui-title-bar-height))
                 (cw (- w left right))
                 (ch (- h top bottom lispm-ui-title-bar-height))
                 (field-width (- cw (* 2 margin))))
      (lispm-ui-svg-embed
       svg (lispm-desktop-platinum-draw-window-frame palette w h :active t)
       x y w h)
      (let ((bar (lispm-ui-title-layout
                  cw (lispm-ui-text-width title 'lispm-ui-title)
                  '(nil nil nil))))
        (lispm-ui-svg-embed
         svg (lispm-ui-draw-title-part
              palette cw :active t :ridges (plist-get bar :ridges)
              :label (lispm-desktop-platinum-fit title (plist-get bar :title))
              :label-x (plist-get bar :label-x))
         cx (1+ y) cw lispm-ui-title-bar-height))
      (lispm-ui-svg-fill svg cx cy cw ch (lispm-ui-grey palette #xDD))
      ;; The lock, the heading, and what the page asks for.
      (lispm-desktop-auth--setup-page-lock
       svg palette (+ cx margin) (+ cy 16) 32)
      (lispm-ui-svg-content
       svg (propertize (if (eq (plist-get page :operation) 'change)
                           (lispm-desktop-tr "Change Password")
                         (lispm-desktop-tr "Encrypted Secrets"))
                       'face '(:height 1.4))
       (+ cx 68) (+ cy 12) 26 text 'lispm-ui-title)
      (let ((top (+ cy 42)))
        (dolist (line (lispm-ui-alert--wrap
                       (lispm-desktop-auth--setup-page-detail page)
                       (- cw 88) 'lispm-ui-list))
          (lispm-ui-svg-content svg line (+ cx 68) top 16 text 'lispm-ui-list)
          (setq top (+ top 16)))
        ;; The field, named by its label.
        (setq top (+ top 18))
        (lispm-ui-svg-content svg (lispm-desktop-auth--setup-page-label page)
                              (+ cx margin) top 16 text 'lispm-ui-list)
        (setq top (+ top 20))
        (lispm-desktop-auth--setup-page-field
         svg palette page (+ cx margin) top field-width 22)
        ;; What Return does, or why the password is asked for again.
        (setq top (+ top 32))
        (let ((color (if (plist-get page :message)
                         (or (plist-get palette :alert-dark) "#CE3131")
                       muted)))
          (dolist (line (lispm-ui-alert--wrap
                         (lispm-desktop-auth--setup-page-answer page)
                         field-width 'lispm-ui-list))
            (lispm-ui-svg-content svg line (+ cx margin) top 16 color
                                  'lispm-ui-list)
            (setq top (+ top 16)))))
      ;; A rule above the keys at the foot, lit under.
      (lispm-ui-svg-fill svg (+ cx margin) (- (+ cy ch) 40) field-width 1
                         (lispm-ui-grey palette #xB4))
      (lispm-ui-svg-fill svg (+ cx margin) (- (+ cy ch) 39) field-width 1
                         (lispm-ui-grey palette #xFF))
      (pcase-let* ((buttons (lispm-desktop-auth--setup-page-buttons cx cw cy ch))
                   (button-y (plist-get buttons :y))
                   (`(,cancel-x ,cancel-w ,cancel-label) (plist-get buttons :cancel))
                   (`(,confirm-x ,confirm-w ,confirm-label) (plist-get buttons :confirm))
                   (change (and (plist-get page :change)
                                (lispm-desktop-tr "C-p changes the password"))))
        (lispm-ui-svg-push-button svg cancel-x button-y palette cancel-label cancel-w)
        (lispm-ui-svg-push-button svg confirm-x button-y palette confirm-label confirm-w)
        (when change
          (lispm-ui-svg-content
           svg change
           (+ cx margin) button-y 16 muted 'lispm-ui-small)))
      svg)))

(defun lispm-desktop-auth--setup-page-event (_page event)
  "Map a mouse EVENT on PAGE's buttons to an Auth action."
  (let* ((posn (or (event-end event) (event-start event)))
         (window (posn-window posn))
         (xy (posn-object-x-y posn)))
    (when (and (windowp window) (consp xy))
      (let* ((width (window-body-width window t))
             (height (window-body-height window t))
             (w lispm-desktop-auth-setup-page-width)
             (h lispm-desktop-auth-setup-page-height)
             (x (max 0 (/ (- width w) 2)))
             (y (max 0 (/ (- height h) 2))))
        (pcase-let* ((`(,left ,top ,right ,bottom) (lispm-desktop-platinum-frame-margins))
                     (cx (+ x left))
                     (cy (+ y top lispm-ui-title-bar-height))
                     (cw (- w left right))
                     (ch (- h top bottom lispm-ui-title-bar-height))
                     (buttons (lispm-desktop-auth--setup-page-buttons cx cw cy ch))
                     (button-y (plist-get buttons :y))
                     (`(,cancel-x ,cancel-w ,_) (plist-get buttons :cancel))
                     (`(,confirm-x ,confirm-w ,_) (plist-get buttons :confirm)))
          (when (and (<= button-y (cdr xy))
                     (< (cdr xy) (+ button-y lispm-ui-button-height)))
            (cond ((and (<= cancel-x (car xy)) (< (car xy) (+ cancel-x cancel-w))) 'cancel)
                  ((and (<= confirm-x (car xy)) (< (car xy) (+ confirm-x confirm-w))) 'confirm))))))))

(defvar evil-emacs-state-cursor)
(defvar evil-normal-state-cursor)
(defvar evil-insert-state-cursor)
(defvar evil-motion-state-cursor)

(defun lispm-desktop-auth--hide-page-cursor (&optional page)
  "Hide the cursor in PAGE's buffer and window (including Evil cursor states)."
  (let ((buf (or (and page (plist-get page :buffer))
                 (get-buffer " *LispM Password*"))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq-local cursor-type nil
                    cursor-in-non-selected-windows nil
                    evil-emacs-state-cursor '(nil)
                    evil-normal-state-cursor '(nil)
                    evil-insert-state-cursor '(nil)
                    evil-motion-state-cursor '(nil)))
      (when-let* ((win (get-buffer-window buf t))
                  ((window-live-p win)))
        (internal-show-cursor win nil)
        (when (and (window-parameter win 'lispm-desktop-platinum-scroll-frame)
                   (fboundp 'lispm-desktop-platinum-scroll-sync))
          (lispm-desktop-platinum-scroll-sync))
        (set-window-scroll-bars win 0 nil 0 nil)))))

(defun lispm-desktop-auth--around-page-show (orig page)
  "Ensure the window and frame are prepared before showing PAGE."
  (when (not noninteractive)
    (add-hook 'lispm-auth-page-mode-hook #'lispm-desktop-auth--hide-page-cursor)
    (when (fboundp 'lispm-auth--page-redraw)
      (advice-add 'lispm-auth--page-redraw :before #'lispm-desktop-auth--hide-page-cursor))
    (when (and (window-live-p (selected-window))
               (or (window-minibuffer-p (selected-window))
                   (window-dedicated-p (selected-window))
                   (window-parameter (selected-window) 'window-side)))
      (when-let* ((main (or (window-main-window (selected-frame))
                            (frame-first-window (selected-frame)))))
        (when (window-live-p main)
          (select-window main))))
    (let ((window (selected-window)))
      (when (and (window-live-p window)
                 (window-parameter window 'lispm-desktop-platinum-scroll-frame)
                 (fboundp 'lispm-desktop-platinum-scroll-sync))
        (lispm-desktop-platinum-scroll-sync)))
    (when (and (display-graphic-p (selected-frame))
               (not (and (fboundp 'lispm-auth--page-windowed-p)
                         (lispm-auth--page-windowed-p page))))
      (let* ((palette (lispm-desktop-auth--setup-page-palette))
             (content (or (plist-get palette :content)
                          (face-attribute 'default :background nil 'default)
                          "#FFFFFF"))
             (text (or (plist-get palette :text)
                       (face-attribute 'default :foreground nil 'default)
                       "#000000")))
        (set-frame-parameter (selected-frame) 'background-color content)
        (set-frame-parameter (selected-frame) 'foreground-color text)
        (set-face-background 'fringe content (selected-frame)))))
  (let ((ignore-window-parameters t)
        (real-delete-other-windows (symbol-function 'delete-other-windows))
        (real-redisplay (symbol-function 'redisplay)))
    (cl-letf (((symbol-function 'delete-other-windows)
               (lambda (&optional window interactive)
                 (funcall real-delete-other-windows window interactive)
                 (let ((w (or window (selected-window))))
                   (when (window-live-p w)
                     (set-window-dedicated-p w nil)
                     (dolist (p '(window-side window-slot no-other-window no-delete-other-windows))
                       (set-window-parameter w p nil))))))
              ((symbol-function 'redisplay)
               (lambda (&optional force)
                 (when (and (plist-get page :buffer)
                            (buffer-live-p (plist-get page :buffer))
                            (zerop (buffer-size (plist-get page :buffer)))
                            (fboundp 'lispm-auth--page-windowed-p)
                            (lispm-auth--page-windowed-p page))
                   (with-current-buffer (plist-get page :buffer)
                     (let ((inhibit-read-only t))
                       (lispm-desktop-auth--setup-page-draw page))))
                 (funcall real-redisplay force))))
      (prog1 (funcall orig page)
        (lispm-desktop-auth--hide-page-cursor page)))))

(defun lispm-desktop-auth--scroll-bar (buffer)
  "Say no scroll bar for BUFFER when it is a password page.
This is one of `lispm-desktop-platinum-scroll-bar-functions'."
  (and (or (equal (buffer-name buffer) " *LispM Password*")
           (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                    'lispm-auth-page-mode))
       'no))

(add-hook 'lispm-desktop-platinum-scroll-bar-functions #'lispm-desktop-auth--scroll-bar)

(defun lispm-desktop-auth--setup-page-draw (page)
  "Draw PAGE, a page asking for LispM's password, as a window.
PAGE's buffer is current and empty, and its frame is over the desktop,
whose colour shows around the window as the setup assistant's page does.
This is `lispm-auth-page-window-draw-function'."
  (setq-local left-fringe-width 0
              right-fringe-width 0
              left-margin-width 0
              right-margin-width 0
              vertical-scroll-bar nil
              horizontal-scroll-bar nil
              auto-hscroll-mode nil
              hscroll-margin 0
              scroll-margin 0
              fringe-indicator-alist nil)
  (let ((table (or buffer-display-table
                   (setq buffer-display-table (make-display-table)))))
    (set-display-table-slot table 'truncation (vector))
    (set-display-table-slot table 'wrap (vector)))
  (let* ((frame (plist-get page :frame))
         (window (get-buffer-window (current-buffer) t))
         (palette (lispm-desktop-auth--setup-page-palette))
         (child-p (and (framep frame) (frame-parent frame)))
         (bg (if child-p
                 (lispm-ui-grey palette #xDD)
               (or (plist-get palette :desktop) "#6E6E6E"))))
    (when (and (windowp window) (window-live-p window))
      (when (and (window-parameter window 'lispm-desktop-platinum-scroll-frame)
                 (fboundp 'lispm-desktop-platinum-scroll-sync))
        (lispm-desktop-platinum-scroll-sync))
      (set-window-scroll-bars window 0 nil 0 nil)
      (set-window-fringes window 0 0)
      (set-window-margins window 0 0))
    (let ((width (if window (window-body-width window t)
                   (frame-pixel-width frame)))
          (height (if window (window-body-height window t)
                    (frame-pixel-height frame))))
      ;; What the image leaves bare shows in the frame's own colours.
      (when (frame-live-p frame)
        (dolist (parameter `((background-color . ,bg)
                             (foreground-color . ,(or (plist-get palette :text) "#000000"))))
          (unless (equal (frame-parameter frame (car parameter)) (cdr parameter))
            (set-frame-parameter frame (car parameter) (cdr parameter)))))
      (insert (propertize " " 'display
                          (svg-image (lispm-desktop-auth--setup-page-svg
                                      width height page palette)
                                     :ascent 100 :scale 1)))
      (goto-char (point-min))
      (when (and (windowp window) (window-live-p window))
        (set-window-point window (point-min))
        (set-window-start window (point-min) t)
        (set-window-hscroll window 0)))))

(defun lispm-desktop-auth--before-ensure-process ()
  "Ensure the Auth service binary is installed before starting its process."
  (unless noninteractive
    (lispm-desktop-auth-ensure-service)))

(defun lispm-desktop-auth--existing-service-program ()
  "Return the newest installed `lispm-auth-service' executable, or nil."
  (when (and (boundp 'lispm-auth-service-directory)
             (stringp lispm-auth-service-directory)
             (file-directory-p lispm-auth-service-directory)
             (fboundp 'lispm-auth--service-platform))
    (let* ((platform (lispm-auth--service-platform))
           (versions (and platform
                          (sort (directory-files lispm-auth-service-directory nil
                                                 "\\`v?[0-9]" t)
                                (lambda (a b)
                                  (version-list-<
                                   (or (ignore-errors
                                         (version-to-list (string-remove-prefix "v" b)))
                                       '(0))
                                   (or (ignore-errors
                                         (version-to-list (string-remove-prefix "v" a)))
                                       '(0))))))))
      (seq-some (lambda (ver)
                  (let ((prog (expand-file-name
                               (concat ver "/" platform "/lispm-auth-service")
                               lispm-auth-service-directory)))
                    (and (file-regular-p prog)
                         (file-executable-p prog)
                         prog)))
                versions))))

(defun lispm-desktop-auth-ensure-service ()
  "Install the Auth service when absent, without replacing a configured one."
  (condition-case err
      (lispm-auth--service-program)
    (lispm-auth-unavailable
     (when lispm-auth-service-program
       (signal (car err) (cdr err)))
     (let ((existing (lispm-desktop-auth--existing-service-program))
           (ver (or (bound-and-true-p lispm-auth-service-version) "latest")))
       (if (and existing
                (not noninteractive)
                (not (if (fboundp 'lispm-desktop-assistant-confirm-update)
                         (lispm-desktop-assistant-confirm-update
                          "Update LispM Auth service to %s? " ver)
                       (y-or-n-p (format "Update LispM Auth service to %s? " ver)))))
           (setq lispm-auth-service-program existing)
         (let ((opened-progress
                (and (not noninteractive)
                     (fboundp 'lispm-desktop-assistant-ensure-progress)
                     (not (and (fboundp 'lispm-desktop-assistant--visible-p)
                               (lispm-desktop-assistant--visible-p)))
                     (or (bound-and-true-p lispm-desktop-setup--in-progress)
                         (not (bound-and-true-p lispm-desktop--set-up))))))
           (when opened-progress
             (lispm-desktop-assistant-ensure-progress "Installing LispM Auth service…"))
           (when (not (and (fboundp 'lispm-desktop-assistant--visible-p)
                           (lispm-desktop-assistant--visible-p)))
             (message "Installing LispM Auth service…")
             (unless noninteractive
               (redisplay t)))
           (unwind-protect
               (condition-case install-error
                   (if (and (fboundp 'lispm-desktop-assistant--visible-p)
                            (lispm-desktop-assistant--visible-p)
                            (fboundp 'lispm-desktop-assistant--package-message))
                       (progn
                         (advice-add 'message :around #'lispm-desktop-assistant--package-message)
                         (unwind-protect
                             (lispm-auth-install-service)
                           (advice-remove 'message #'lispm-desktop-assistant--package-message)))
                     (lispm-auth-install-service))
                 (error
                  (error "LispM Auth service installation failed: %s; check the service installation or configure lispm-auth-service-program"
                         (error-message-string install-error))))
             (when (and opened-progress
                        (not (bound-and-true-p lispm-desktop-first-start))
                        (fboundp 'lispm-desktop-assistant-close))
               (lispm-desktop-assistant-close)))
           (lispm-auth--service-program)))))))

(defun lispm-desktop-auth-prepare (&optional apps)
  "Load Auth and prepare its service when APPS require a password."
  (let* ((startup-apps (lispm-desktop-auth--apps apps))
         (target-apps (mapcar (lambda (app)
                                (if (and (lispm-desktop-auth-started-hidden-p)
                                         (memq app lispm-desktop-auth-lispm-apps))
                                    'lispm
                                  app))
                              startup-apps)))
    (if (null target-apps)
        nil
      (condition-case err
          (if (not (lispm-desktop-auth-ensure))
              (if (seq-some (lambda (app)
                              (or (eq app 'desktop)
                                  (memq app lispm-desktop-auth-lispm-apps)))
                            target-apps)
                  (error "LispM Auth is not installed")
                nil)
            (when (or (not (fboundp 'lispm-auth-app-requires-password-p))
                      (seq-some #'lispm-auth-app-requires-password-p target-apps))
              (lispm-desktop-auth-ensure-service))
            t)
        (error
         (display-warning 'lispm-desktop
                          (format "LispM Auth is unavailable: %s. Password protection is unavailable."
                                  (error-message-string err)) :warning)
         nil)))))

(defun lispm-desktop-auth-setup (&optional apps)
  "Open LispM's secrets at the start, asking for their password once.
APPS, applications, default to `lispm-desktop-auth-apps'.  The store is
opened only when one of them declares that it uses secrets, so a start
of applications that declare nothing asks nothing.  Started out of sight
for an application opened on its own, only that application is judged.
A required store that remains locked stops interactive startup; batch
installation does not ask for a password."
  (let ((apps (lispm-desktop-auth--apps apps))
        (echo-keystrokes 0)
        (lispm-desktop-console--quiet t))
    (cond ((null apps))
          ((and (lispm-desktop-auth-started-hidden-p)
                (memq (car apps) lispm-desktop-auth-lispm-apps))
           (lispm-desktop-auth-start-lispm))
          (t
           (when (lispm-desktop-auth-ensure)
             (let ((opened (lispm-auth-start-apps apps)))
               (when (and (not noninteractive) (not opened)
                          (or (not (fboundp 'lispm-auth-store-exists-p))
                              (lispm-auth-store-exists-p))
                          (seq-some #'lispm-auth-app-requires-password-p apps))
                 (user-error "LispM Auth store was not opened; startup cancelled"))
               opened))))))

(defun lispm-desktop-auth-start-lispm ()
  "Open LispM's secrets for LispM itself, which always declares.
This is Focus Mode's and LispM's terminal's start, and the start out of
sight for LispM Agent: it does not depend on the desktop's declaration,
which Control Panels may revoke."
  (when (lispm-desktop-auth-prepare)
    (let* ((echo-keystrokes 0)
           (lispm-desktop-console--quiet t)
           (opened (lispm-auth-start-app 'lispm)))
      (when (and (not noninteractive) (not opened)
                 (or (not (fboundp 'lispm-auth-store-exists-p))
                     (lispm-auth-store-exists-p)))
        (user-error "LispM Auth store was not opened; startup cancelled"))
      opened)))

(defun lispm-desktop-auth-available-p ()
  "Return non-nil when LispM Auth is here.
Its functions already being here counts: loading its file then would
define them again, over any a caller has put in their place."
  (or (fboundp 'lispm-auth-store-exists-p)
      (and (lispm-desktop-auth-ensure) t)))

(defun lispm-desktop-auth-status ()
  "Return how LispM's password stands, as a sentence the user sees."
  (unless (lispm-desktop-auth-available-p)
    (user-error "%s" (lispm-desktop-tr "LispM Auth is not available.")))
  (cond ((not (lispm-auth-store-exists-p))
         (lispm-desktop-tr "No password is set yet. LispM makes one the first time it starts."))
        ((lispm-auth-locked-p)
         (lispm-desktop-tr "The password is set, and the store is locked."))
        (t
         (lispm-desktop-tr "The password is set, and the store is open for this session."))))

(defvar lispm-desktop-auth--minibuffer-password nil
  "Non-nil to ask for passwords in the native minibuffer.")

(defun lispm-desktop-auth--call-in-dialog (function)
  "Call FUNCTION in a temporary child frame over the desktop when possible."
  (let ((echo-keystrokes 0)
        (lispm-desktop-console--quiet t)
        (lispm-auth-page-windowed t))
    (lispm-desktop-auth--call-in-dialog-1 function)))

(defun lispm-desktop-auth--call-in-dialog-1 (function)
  "Call FUNCTION in a child frame when Desktop can display one."
  (if (or noninteractive (not (display-graphic-p))
          lispm-desktop-auth--minibuffer-password
          (eq lispm-auth-password-function #'read-passwd)
          (not (fboundp 'lispm-desktop-apps-screen)))
      (funcall function)
    (let* ((previous (selected-frame))
           (screen (lispm-desktop-apps-screen))
           (width (min lispm-desktop-auth-setup-page-width
                       (frame-pixel-width screen)))
           (height (min lispm-desktop-auth-setup-page-height
                        (frame-pixel-height screen)))
           (palette (lispm-desktop-auth--setup-page-palette))
           (after-make-frame-functions nil)
           (before-make-frame-hook nil)
           (frame (make-frame `((parent-frame . ,screen)
                                (minibuffer . nil)
                                (undecorated . t)
                                (visibility . nil)
                                (no-accept-focus . nil)
                                (left . ,(max 0 (/ (- (frame-pixel-width screen) width) 2)))
                                (top . ,(max 0 (/ (- (frame-pixel-height screen) height) 2)))
                                (width . (text-pixels . ,width))
                                (height . (text-pixels . ,height))
                                (background-color . ,(lispm-ui-grey palette #xDD))
                                (foreground-color . ,(or (plist-get palette :text) "#000000"))
                                (internal-border-width . 0)
                                (child-frame-border-width . 0)
                                (left-fringe . 0)
                                (right-fringe . 0)
                                (vertical-scroll-bars . nil)
                                (horizontal-scroll-bars . nil)
                                (menu-bar-lines . 0)
                                (tool-bar-lines . 0)
                                (tab-bar-lines . 0)
                                (unsplittable . t)
                                (no-other-frame . t)
                                (no-special-glyphs . t)
                                (line-spacing . 0)
                                (cursor-type . nil)
                                (desktop-dont-save . t)))))
      (unwind-protect
          (progn
            (let ((window (frame-root-window frame)))
              (when (window-live-p window)
                (set-window-fringes window 0 0)
                (set-window-margins window 0 0)))
            (when (fboundp 'lispm-ui-popup-set-size)
              (lispm-ui-popup-set-size frame width height))
            (make-frame-visible frame)
            (select-frame-set-input-focus frame)
            (with-selected-frame frame (funcall function)))
        (when (frame-live-p frame) (delete-frame frame t))
        (when (frame-live-p previous) (select-frame-set-input-focus previous))))))

(defun lispm-desktop-auth-change-password ()
  "Change LispM's password using Auth's password reader.
In Focus Mode, or when requested by the security menu, use the native
minibuffer.  Otherwise use Auth's password page.  Auth verifies the old
password when a store exists, then asks for the new one twice.  Return
`changed', `unchanged' or `failed'; the user sees a message saying which."
  (interactive)
  (unless (lispm-desktop-auth-available-p)
    (user-error "%s" (lispm-desktop-tr "LispM Auth is not available.")))
  (let ((lispm-auth-password-function
         (if (or lispm-desktop-auth--minibuffer-password
                 (and (fboundp 'lispm-desktop-started-as-terminal-p)
                      (lispm-desktop-started-as-terminal-p))
                 (and (fboundp 'lispm-terminal-p) (lispm-terminal-p)))
             #'read-passwd
           (and (boundp 'lispm-auth-password-function)
                lispm-auth-password-function))))
    (condition-case err
      (if (lispm-desktop-auth--call-in-dialog #'lispm-auth-change-password)
          (progn
            (message "%s" (lispm-desktop-tr "The password has changed."))
            'changed)
        (message "%s" (lispm-desktop-tr "The password is unchanged."))
        'unchanged)
    (quit
     (message "%s" (lispm-desktop-tr "The password is unchanged."))
     'unchanged)
    (user-error
     (let ((msg (error-message-string err)))
       (if (string-match-p "unchanged\\'" msg)
           (message "%s" (lispm-desktop-tr "The password is unchanged."))
         (message "%s" msg)))
     'unchanged)
    (error
     (message "%s" (error-message-string err))
     'failed))))

(defun lispm-desktop-auth--lock-screen ()
  "Show the lock screen until the store is opened again.
Hide the desktop's visible application windows during authentication."
  (unless noninteractive
    (message "%s" (lispm-desktop-tr "LispM is locked."))
    (let* ((echo-keystrokes 0)
           (lispm-desktop-console--quiet t)
           (previous (selected-frame))
           (screen (if (fboundp 'lispm-desktop-apps-screen)
                       (lispm-desktop-apps-screen) previous))
           (windows (when (and (display-graphic-p)
                               (fboundp 'lispm-desktop-apps-windows))
                      (seq-filter #'frame-visible-p
                                  (lispm-desktop-apps-windows screen))))
           (lispm-auth-page-windowed nil)
           (lispm-auth-password-function
            (if (or (and (fboundp 'lispm-desktop-started-as-terminal-p)
                         (lispm-desktop-started-as-terminal-p))
                    (and (fboundp 'lispm-terminal-p) (lispm-terminal-p)))
                #'read-passwd #'lispm-auth--read-password)))
      (dolist (frame windows)
        (lispm-desktop-platinum-hide-window frame))
      (unwind-protect
          (with-selected-frame screen
            (select-frame-set-input-focus screen)
            (while (lispm-auth-locked-p)
              (condition-case nil
                  (lispm-auth-unlock)
                (quit nil)
                (user-error nil))))
        ;; Never reveal the application windows after a failed unlock.
        (unless (lispm-auth-locked-p)
          (dolist (frame windows)
            (when (frame-live-p frame)
              (lispm-desktop-platinum-show-window frame)))
          (when (frame-live-p previous)
            (select-frame-set-input-focus previous)))))))

(defun lispm-desktop-auth-lock ()
  "Lock LispM's store now, forgetting its keys until the password is given.
Return `locked' or `no-store'."
  (interactive)
  (unless (lispm-desktop-auth-available-p)
    (user-error "%s" (lispm-desktop-tr "LispM Auth is not available.")))
  (cond ((not (lispm-auth-store-exists-p))
         (message "%s" (lispm-desktop-tr "No password is set yet."))
         'no-store)
        ((lispm-auth-locked-p)
         (lispm-desktop-auth--lock-screen)
         'locked)
        (t
         (lispm-auth-lock)
         (lispm-desktop-auth--lock-screen)
         'locked)))

(defun lispm-desktop-security ()
  "LispM's security settings, chosen in the minibuffer.
Change or set the password, lock the store now, and say whether the
Desktop asks for the password when it starts.  Choices and passwords use
the native minibuffer; C-g or Done leaves."
  (interactive)
  (unless (lispm-desktop-auth-available-p)
    (user-error "%s" (lispm-desktop-tr "LispM Auth is not available.")))
  (when noninteractive
    (user-error "%s" (lispm-desktop-tr "Security settings need an interactive Emacs.")))
  (let ((enable-recursive-minibuffers t)
        (done nil))
    (while (not done)
      (let* ((store (lispm-auth-store-exists-p))
             (locked (lispm-auth-locked-p))
             (actions
              (append
               (list (cons (lispm-desktop-tr (if store "Change Password…" "Set Password…"))
                           'change))
               (when (and store (not locked))
                 (list (cons (lispm-desktop-tr "Lock Now") 'lock)))
               (list (cons (format "%s %s"
                                   (if (lispm-auth-app-requires-password-p 'desktop) "[x]" "[ ]")
                                   (lispm-desktop-tr "Ask for the password when the Desktop starts"))
                           'desktop))
               (list (cons (lispm-desktop-tr "Done") 'done))))
             (choice (condition-case nil
                         (completing-read
                          (format "%s  " (lispm-desktop-auth-status))
                          (mapcar #'car actions) nil t)
                       (quit nil))))
        (if (null choice)
            (setq done t)
          (pcase (cdr (assoc choice actions))
            ('change
             (let ((lispm-desktop-auth--minibuffer-password t))
               (lispm-desktop-auth-change-password)))
            ('lock (lispm-desktop-auth-lock))
            ('desktop
             (condition-case err
                 (lispm-auth-set-app-requires-password
                  'desktop (not (lispm-auth-app-requires-password-p 'desktop)))
               (error (message "%s" (error-message-string err)))))
            (_ (setq done t))))))))

(provide 'lispm-desktop-auth)
;;; lispm-desktop-auth.el ends here
