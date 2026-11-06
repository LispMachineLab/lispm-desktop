;;; lispm-desktop-auth-test.el --- Tests of the desktop's opening LispM's secrets  -*- lexical-binding: t; -*-

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

;; The desktop asks LispM Auth to open the store with the applications it
;; starts, and asks nothing when it starts none that declare secrets.
;; Opening the store itself is LispM Auth's tests': here only what the
;; desktop hands it is checked.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lispm-desktop-deps)
(require 'lispm-desktop-auth)
(defvar lispm-auth-password-function)

(defun lispm-desktop-auth-test--skip-unless-lispm-auth ()
  "Skip unless LispM Auth is on `load-path', without loading it.
Loading it here would change what the panels of other tests show."
  (unless (locate-library "lispm-auth")
    (ert-skip "lispm-auth is not here")))

(ert-deftest lispm-desktop-auth-test-ensure-loads-lispm-auth ()
  "LispM Auth is loaded, from `load-path' or a checkout beside the desktop's."
  (lispm-desktop-auth-test--skip-unless-lispm-auth)
  (let (required)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _) (push feature required) t)))
      (should (lispm-desktop-auth-ensure))
      (should (equal required '(lispm-auth))))))

(ert-deftest lispm-desktop-auth-test-setup-hands-it-the-apps ()
  "The desktop starts with the applications it starts, the desktop first."
  (lispm-desktop-auth-test--skip-unless-lispm-auth)
  (let (started)
    (cl-letf (((symbol-function 'lispm-desktop-auth-ensure) (lambda () t))
              ((symbol-function 'lispm-auth-start-apps)
               (lambda (apps) (setq started apps) t)))
      (lispm-desktop-auth-setup)
      (should (equal started '(desktop)))
      (setq started nil)
      (lispm-desktop-auth-setup '(terminal))
      (should (equal started '(terminal))))))

(ert-deftest lispm-desktop-auth-test-setup-asks-nothing-with-no-apps ()
  "With no application to start, LispM Auth is asked to open nothing."
  (lispm-desktop-auth-test--skip-unless-lispm-auth)
  (let ((lispm-desktop-auth-apps nil)
        (started 'never))
    (cl-letf (((symbol-function 'lispm-auth-start-apps)
               (lambda (apps) (setq started apps) t)))
      (lispm-desktop-auth-setup)
      (should (eq started 'never)))))

;;;; Started out of sight, for an application opened on its own

(defconst lispm-desktop-auth-test--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "The test's directory, for reading the desktop's sources.")

(defvar lispm-desktop-auth-test--started nil
  "What LispM Auth was asked to start, oldest first.")

(defun lispm-desktop-auth-test--hidden (arguments body)
  "Run BODY with ARGUMENTS the command line, stubbing LispM Auth's starts.
The start functions record (start-apps APPS) and (start-app APP) in
`lispm-desktop-auth-test--started'."
  (let ((command-line-args arguments)
        (lispm-desktop-auth-test--started nil))
    (cl-letf (((symbol-function 'lispm-auth-start-apps)
               (lambda (apps)
                 (push (list 'start-apps apps)
                       lispm-desktop-auth-test--started)
                 t))
              ((symbol-function 'lispm-auth-start-app)
               (lambda (app)
                 (push (list 'start-app app)
                       lispm-desktop-auth-test--started)
                 t))
              ((symbol-function 'lispm-auth-app-requires-password-p) #'ignore)
              ((symbol-function 'lispm-desktop-auth-ensure-service) #'ignore)
              ((symbol-function 'lispm-desktop-auth-ensure) (lambda () t)))
      (funcall body)
      (nreverse lispm-desktop-auth-test--started))))

(ert-deftest lispm-desktop-auth-test-hidden-gomoku-judges-only-gomoku ()
  "Started out of sight for Gomoku, only Gomoku is judged: it declares
nothing, so no page is shown and no store is touched."
  (lispm-desktop-auth-test--skip-unless-lispm-auth)
  (should (equal (lispm-desktop-auth-test--hidden
                  '("Emacs" "--lispm-hidden" "--lispm-app=gomoku")
                  (lambda () (lispm-desktop-auth-setup)))
                 '((start-apps (gomoku))))))

(ert-deftest lispm-desktop-auth-test-hidden-lispm-opens-lispm ()
  "Started out of sight for LispM Agent, LispM is started, and so its
declaration is judged whichever the desktop's own is."
  (lispm-desktop-auth-test--skip-unless-lispm-auth)
  (should (equal (lispm-desktop-auth-test--hidden
                  '("Emacs" "--lispm-hidden" "--lispm-app=lispm")
                  (lambda () (lispm-desktop-auth-setup)))
                 '((start-app lispm))))
  (should (equal (lispm-desktop-auth-test--hidden
                  '("Emacs" "--lispm-hidden" "--lispm-app=lispm-agent")
                  (lambda () (lispm-desktop-auth-setup)))
                 '((start-app lispm)))))

(ert-deftest lispm-desktop-auth-test-hidden-without-an-app-asks-nothing ()
  "Started out of sight, naming no application, nothing is judged."
  (lispm-desktop-auth-test--skip-unless-lispm-auth)
  (should-not (lispm-desktop-auth-test--hidden
               '("Emacs" "--lispm-hidden")
               (lambda () (lispm-desktop-auth-setup)))))

(ert-deftest lispm-desktop-auth-test-password-pages-are-windows-only-on-their-own ()
  "LispM's own start asks for the password across the window, so the
page is not windowed there; an application opened on its own asks in a
window, and LispM Auth draws that one with the desktop's drawing."
  (let ((original (symbol-function 'fboundp)))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (symbol)
                 (or (eq symbol 'lispm-auth-store-exists-p)
                     (funcall original symbol))))
              ((symbol-function 'require) (lambda (&rest _) t)))
      (unwind-protect
          (progn
            (let ((command-line-args '("Emacs")))
              (should (lispm-desktop-auth-ensure))
              (should (eq lispm-auth-page-window-draw-function
                          #'lispm-desktop-auth--setup-page-draw))
              (should-not lispm-auth-page-windowed))
            (let ((command-line-args '("Emacs" "--lispm-hidden"
                                       "--lispm-app=gomoku")))
              (should (lispm-desktop-auth-ensure))
              (should (eq lispm-auth-page-windowed t))))
        (setq lispm-auth-page-windowed nil
              lispm-auth-page-window-draw-function nil)))))

(ert-deftest lispm-desktop-auth-test-app-key-is-read ()
  "The application key is read in either form the command line allows."
  (should (eq (let ((command-line-args '("Emacs" "--lispm-app=gomoku")))
                (lispm-desktop-auth-command-line-app))
              'gomoku))
  (should (eq (let ((command-line-args '("Emacs" "--lispm-app" "tetris")))
                (lispm-desktop-auth-command-line-app))
              'tetris))
  (should-not (let ((command-line-args '("Emacs")))
                (lispm-desktop-auth-command-line-app))))

(ert-deftest lispm-desktop-auth-test-start-lispm-always-starts-lispm ()
  "Focus Mode and LispM's terminal start LispM itself, which always
declares, rather than the desktop, which Control Panels may revoke."
  (lispm-desktop-auth-test--skip-unless-lispm-auth)
  (let (started)
    (cl-letf (((symbol-function 'lispm-desktop-auth-ensure) (lambda () t))
              ((symbol-function 'lispm-desktop-auth-ensure-service) #'ignore)
              ((symbol-function 'lispm-auth-start-app)
               (lambda (app) (setq started app) t))
              ((symbol-function 'lispm-auth-start-apps)
               (lambda (_apps) (ert-fail "the desktop's apps are not judged"))))
      (lispm-desktop-auth-start-lispm)
      (should (eq started 'lispm)))))

(ert-deftest lispm-desktop-auth-test-terminal-path-starts-lispm-before-terminal ()
  "`lispm-desktop--setup-terminal' opens LispM's secrets, for LispM, before
it puts the terminal on the frame."
  (let* ((file (expand-file-name "../lisp/lispm-desktop.el"
                                 lispm-desktop-auth-test--directory))
         (source (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string)))
         (start (string-match "(defun lispm-desktop--setup-terminal" source)))
    (should start)
    (let* ((end (or (string-match "\n(defun " source (match-end 0))
                    (length source)))
           (body (substring source start end))
           (auth (string-match "(lispm-desktop-auth-start-lispm)" body))
           (terminal (string-match
                      "^  (add-hook 'window-setup-hook #'lispm-desktop-start-as--open-terminal)"
                      body)))
      (should auth)
      (should terminal)
      (should (< auth terminal)))))

(ert-deftest lispm-desktop-auth-test-unavailable-service-warns-and-continues ()
  (let (warning)
    (cl-letf (((symbol-function 'lispm-desktop-auth-ensure) (lambda () t))
              ((symbol-function 'lispm-desktop-auth-ensure-service)
               (lambda () (error "No service for this platform")))
              ((symbol-function 'display-warning)
               (lambda (_type text &rest _) (setq warning text))))
      (should-not (lispm-desktop-auth-prepare))
      (should (string-match-p "Password protection is unavailable" warning)))))

(ert-deftest lispm-desktop-auth-test-client-rejects-bad-digest ()
  (let* ((user-emacs-directory (make-temp-file "auth-client-test-" t))
         (load-path (copy-sequence load-path)))
    (unwind-protect
        (cl-letf (((symbol-function 'url-copy-file)
                   (lambda (_url file &rest _)
                     (with-temp-file file (insert "incorrect archive")))))
          (require 'url)
          (should-error (lispm-desktop-deps--auth-install))
          (should-not (file-exists-p (expand-file-name (concat "lispm-desktop-components/lispm-auth-" lispm-desktop-deps-auth-version) user-emacs-directory))))
      (delete-directory user-emacs-directory t))))

(ert-deftest lispm-desktop-auth-test-client-preserves-user-checkout ()
  (let ((load-path (copy-sequence load-path)) (installed nil))
    (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) "/user/checkout/lispm-auth.el"))
              ((symbol-function 'lispm-desktop-deps--version) (lambda (_) '(0 2 5)))
              ((symbol-function 'lispm-desktop-deps--checkout) (lambda (_) "/user/checkout"))
              ((symbol-function 'lispm-desktop-deps--auth-install) (lambda () (setq installed t))))
      (lispm-desktop-deps-ensure-auth)
      (should-not installed))))

(ert-deftest lispm-desktop-auth-test-upgrades-unchanged-legacy-package ()
  (let* ((package-user-dir (make-temp-file "auth-elpa-" t))
         (main (expand-file-name "lispm-auth/lispm-auth.el" package-user-dir))
         installed)
    (unwind-protect
        (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) main))
                  ((symbol-function 'lispm-desktop-deps--checkout) #'ignore)
                  ((symbol-function 'lispm-desktop-deps--auth-install) (lambda () (setq installed t)))
                  ((symbol-function 'lispm-desktop-deps--git)
                   (lambda (_dir &rest args)
                     (pcase (car args)
                       ("rev-parse" "7f56c31de638c084aaba409a1f3630324180cd9b")
                       ("remote" "https://github.com/LispMachineLab/lispm-auth")
                       ("status" "")))))
          (lispm-desktop-deps-ensure-auth)
          (should installed))
      (delete-directory package-user-dir t))))

(ert-deftest lispm-desktop-auth-test-pinned-client-release ()
  (require 'lispm-desktop-deps)
  (should (equal lispm-desktop-deps-auth-version "0.2.6"))
  (should (equal lispm-desktop-deps-auth-sha256
                 "b39295ff84ffd1619b4a2793e3df47a535063bb3a7d6ed8d920c8a2f00a22eff")))

(ert-deftest lispm-desktop-auth-test-service-is-installed-automatically ()
  (require 'lispm-auth)
  (let ((lispm-auth-service-program nil) (installed nil))
    (cl-letf (((symbol-function 'lispm-auth--service-program)
               (lambda () (if installed "/service" (signal 'lispm-auth-unavailable nil))))
              ((symbol-function 'lispm-auth-install-service)
               (lambda () (setq installed t)))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) (ert-fail "Unexpected confirmation"))))
      (should (equal (lispm-desktop-auth-ensure-service) "/service"))
      (should installed))))

(ert-deftest lispm-desktop-auth-test-explicit-service-is-not-replaced ()
  (require 'lispm-auth)
  (let ((lispm-auth-service-program "/missing/service"))
    (cl-letf (((symbol-function 'lispm-auth--service-program)
               (lambda () (signal 'lispm-auth-unavailable nil)))
              ((symbol-function 'lispm-auth-install-service)
               (lambda () (ert-fail "Unexpected installation"))))
      (should-error (lispm-desktop-auth-ensure-service) :type 'lispm-auth-unavailable))))

(ert-deftest lispm-desktop-auth-test-cancelled-required-store-stops-startup ()
  (let ((noninteractive nil))
    (cl-letf (((symbol-function 'lispm-desktop-auth-ensure) (lambda () t))
              ((symbol-function 'lispm-auth-store-exists-p) (lambda () t))
              ((symbol-function 'lispm-auth-start-apps) #'ignore)
              ((symbol-function 'lispm-auth-app-requires-password-p) (lambda (_) t)))
      (should-error (lispm-desktop-auth-setup) :type 'user-error))))

(ert-deftest lispm-desktop-auth-test-cancelled-first-time-setup-continues ()
  (let ((noninteractive nil))
    (cl-letf (((symbol-function 'lispm-desktop-auth-ensure) (lambda () t))
              ((symbol-function 'lispm-auth-store-exists-p) (lambda () nil))
              ((symbol-function 'lispm-auth-start-apps) #'ignore)
              ((symbol-function 'lispm-auth-app-requires-password-p) (lambda (_) t)))
      (should-not (lispm-desktop-auth-setup)))))

(ert-deftest lispm-desktop-auth-test-opt-out-does-not-stop-startup ()
  (let ((noninteractive nil))
    (cl-letf (((symbol-function 'lispm-desktop-auth-ensure) (lambda () t))
              ((symbol-function 'lispm-auth-start-apps) #'ignore)
              ((symbol-function 'lispm-auth-app-requires-password-p) #'ignore))
      (should-not (lispm-desktop-auth-setup)))))

(ert-deftest lispm-desktop-auth-test-dialog-title-follows-operation ()
  (should (equal "Change Password"
                 (lispm-desktop-auth--setup-page-title '(:operation change :stage current))))
  (should (equal "Change Password"
                 (lispm-desktop-auth--setup-page-title '(:operation change :stage new))))
  (should (equal "Set a Password"
                 (lispm-desktop-auth--setup-page-title '(:operation setup :stage new)))))

(ert-deftest lispm-desktop-auth-test-lock-now-opens-lock-screen ()
  (let ((locked nil) (screen-shown nil))
    (cl-letf (((symbol-function 'lispm-desktop-auth-available-p) (lambda () t))
              ((symbol-function 'lispm-auth-store-exists-p) (lambda () t))
              ((symbol-function 'lispm-auth-locked-p) (lambda () locked))
              ((symbol-function 'lispm-auth-lock) (lambda () (setq locked t)))
              ((symbol-function 'lispm-desktop-auth--lock-screen)
               (lambda () (setq screen-shown locked))))
      (should (eq 'locked (lispm-desktop-auth-lock)))
      (should screen-shown))))

(ert-deftest lispm-desktop-auth-test-dialog-uses-native-password-reader ()
  (require 'lispm-auth)
  (let ((noninteractive nil)
        (lispm-auth-password-function #'read-passwd))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'make-frame)
               (lambda (&rest _) (ert-fail "Native input must not open a frame"))))
      (should (eq 'done (lispm-desktop-auth--call-in-dialog (lambda () 'done)))))))

(ert-deftest lispm-desktop-auth-test-dialog-windows-current-password-page ()
  (cl-letf (((symbol-function 'lispm-desktop-auth--call-in-dialog-1)
             (lambda (function) (funcall function))))
    (should (lispm-desktop-auth--call-in-dialog
             (lambda () lispm-auth-page-windowed)))))

(ert-deftest lispm-desktop-auth-test-lock-screen-retries-cancellation ()
  (let ((noninteractive nil)
        (locked t)
        (attempts 0))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil))
              ((symbol-function 'lispm-auth-locked-p) (lambda () locked))
              ((symbol-function 'lispm-auth-unlock)
               (lambda ()
                 (setq attempts (1+ attempts))
                 (if (= attempts 1) (signal 'quit nil) (setq locked nil))))
              ((symbol-function 'select-frame-set-input-focus) #'ignore))
      (lispm-desktop-auth--lock-screen)
      (should (= attempts 2))
      (should-not locked))))

(ert-deftest lispm-desktop-auth-test-password-button-hit-testing ()
  (let* ((window (selected-window))
         (width 560) (height 280)
         (margins (lispm-desktop-platinum-frame-margins))
         (left (nth 0 margins)) (top (nth 1 margins))
         (right (nth 2 margins)) (bottom (nth 3 margins))
         (x (/ (- width lispm-desktop-auth-setup-page-width) 2))
         (y (/ (- height lispm-desktop-auth-setup-page-height) 2))
         (cx (+ x left))
         (cy (+ y top lispm-ui-title-bar-height))
         (cw (- lispm-desktop-auth-setup-page-width left right))
         (ch (- lispm-desktop-auth-setup-page-height top bottom lispm-ui-title-bar-height))
         (bx (- (+ cx cw) lispm-desktop-auth--setup-page-margin))
         (by (- (+ cy ch) 30))
         point)
    (cl-letf (((symbol-function 'event-start) (lambda (_) 'position))
              ((symbol-function 'posn-window) (lambda (_) window))
              ((symbol-function 'posn-object-x-y) (lambda (_) point))
              ((symbol-function 'window-body-width) (lambda (&rest _) width))
              ((symbol-function 'window-body-height) (lambda (&rest _) height)))
      (setq point (cons (- bx 140) (+ by 5)))
      (should (eq 'cancel (lispm-desktop-auth--setup-page-event nil 'event)))
      (setq point (cons (- bx 50) (+ by 5)))
      (should (eq 'confirm (lispm-desktop-auth--setup-page-event nil 'event)))
      (setq point (cons (- bx 90) (- by 1)))
      (should-not (lispm-desktop-auth--setup-page-event nil 'event)))))

(ert-deftest lispm-desktop-auth-test-redraw-keeps-frame-colours ()
  (let ((colours '((background-color . "#aaaaaa") (foreground-color . "#111111")))
        (changes nil))
    (with-temp-buffer
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                ((symbol-function 'frame-live-p) (lambda (_) t))
                ((symbol-function 'frame-pixel-width) (lambda (_) 560))
                ((symbol-function 'frame-pixel-height) (lambda (_) 280))
                ((symbol-function 'frame-parameter) (lambda (_ key) (alist-get key colours)))
                ((symbol-function 'set-frame-parameter)
                 (lambda (_ key value) (push (cons key value) changes)
                   (setf (alist-get key colours) value)))
                ((symbol-function 'lispm-desktop-auth--setup-page-palette)
                 (lambda () '(:desktop "#aaaaaa" :text "#111111")))
                ((symbol-function 'lispm-desktop-auth--setup-page-svg) #'ignore)
                ((symbol-function 'svg-image) (lambda (&rest _) 'image)))
        (lispm-desktop-auth--setup-page-draw '(:frame frame))
        (should-not changes)
        (setf (alist-get 'background-color colours) "#ffffff")
        (lispm-desktop-auth--setup-page-draw '(:frame frame))
        (should (equal changes '((background-color . "#aaaaaa"))))))))

(ert-deftest lispm-desktop-auth-test-lock-screen-selects-reader ()
  (require 'lispm-auth)
  (dolist (focus '(nil t))
    (let ((noninteractive nil) (locked t) reader)
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil))
                ((symbol-function 'lispm-desktop-started-as-terminal-p) (lambda () focus))
                ((symbol-function 'lispm-terminal-p) (lambda (&rest _) nil))
                ((symbol-function 'lispm-auth-locked-p) (lambda () locked))
                ((symbol-function 'lispm-auth-unlock)
                 (lambda () (setq reader lispm-auth-password-function locked nil)))
                ((symbol-function 'select-frame-set-input-focus) #'ignore))
        (lispm-desktop-auth--lock-screen)
        (should (eq reader (if focus #'read-passwd #'lispm-auth--read-password)))))))

(ert-deftest lispm-desktop-auth-test-field-caret-is-snug ()
  (let ((fills nil))
    (cl-letf (((symbol-function 'svg-rectangle) #'ignore)
              ((symbol-function 'svg-circle) #'ignore)
              ((symbol-function 'lispm-ui-svg-fill)
               (lambda (_svg x y w h _color)
                 (push (list x y w h) fills))))
      (lispm-desktop-auth--setup-page-field nil nil '(:password "") 40 100 440 22)
      (should (equal (car fills) '(47 105 1 12)))
      (setq fills nil)
      (lispm-desktop-auth--setup-page-field nil nil '(:password "123456") 40 100 440 22)
      (should (equal (car fills) '(101 105 1 12))))))

(ert-deftest lispm-desktop-auth-test-dialog-frame-omits-desktop-and-chrome ()
  (let ((noninteractive nil)
        (lispm-desktop-auth--minibuffer-password nil)
        (lispm-auth-password-function nil)
        (after-make-frame-functions '(unexpected-hook))
        (before-make-frame-hook '(unexpected-before))
        captured-params captured-after captured-before)
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'lispm-desktop-apps-screen) (lambda () 'screen))
              ((symbol-function 'frame-pixel-width) (lambda (_) 1280))
              ((symbol-function 'frame-pixel-height) (lambda (_) 800))
              ((symbol-function 'make-frame)
               (lambda (params)
                 (setq captured-params params
                       captured-after after-make-frame-functions
                       captured-before before-make-frame-hook)
                 'dialog-frame))
              ((symbol-function 'frame-root-window) (lambda (_) nil))
              ((symbol-function 'lispm-ui-popup-set-size) #'ignore)
              ((symbol-function 'make-frame-visible) #'ignore)
              ((symbol-function 'select-frame-set-input-focus) #'ignore)
              ((symbol-function 'select-frame) #'ignore)
              ((symbol-function 'frame-live-p) (lambda (_) nil)))
      (lispm-desktop-auth--call-in-dialog-1 #'ignore)
      (should-not captured-after)
      (should-not captured-before)
      (should (equal (alist-get 'width captured-params)
                     (cons 'text-pixels lispm-desktop-auth-setup-page-width)))
      (should (equal (alist-get 'height captured-params)
                     (cons 'text-pixels lispm-desktop-auth-setup-page-height)))
      (should (eql 0 (alist-get 'tab-bar-lines captured-params)))
      (should (eql 0 (alist-get 'menu-bar-lines captured-params)))
      (should (eql 0 (alist-get 'left-fringe captured-params)))
      (should (eql 0 (alist-get 'right-fringe captured-params))))))

(ert-deftest lispm-desktop-auth-test-i18n-translates-dialog-and-lock-page ()
  (require 'lispm-auth)
  (lispm-desktop-auth-ensure)
  (let ((lispm-desktop-language "zh-Hans"))
    (should (equal "设置密码"
                   (lispm-desktop-auth--setup-page-title '(:operation setup :stage new))))
    (should (equal "更改密码"
                   (lispm-desktop-auth--setup-page-title '(:operation change :stage current))))
    (should (equal "新密码："
                   (lispm-desktop-auth--setup-page-label '(:stage new))))
    (should (equal "当前密码："
                   (lispm-desktop-auth--setup-page-label '(:stage current))))
    (should (equal "每次启动时都会询问该密码，且无法找回。"
                   (lispm-desktop-auth--setup-page-answer '(:stage new))))
    (should (equal "密码错误。请重试（第 1 次，共 3 次）。"
                   (lispm-desktop-auth--setup-page-answer
                    '(:stage single :message "Wrong password.  Try again (1 of 3)."))))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
      (let ((lines (lispm-auth--page-lines '(:stage single :change t))))
        (should (cl-some (lambda (l) (string-search "加密密钥" l)) lines))
        (should (cl-some (lambda (l) (string-search "按回车解锁 LispM。" l)) lines))
        (should (cl-some (lambda (l) (string-search "回车确认" l)) lines))
        (should (cl-some (lambda (l) (string-search "C-p 更改密码" l)) lines))))))

(ert-deftest lispm-desktop-auth-test-non-encrypted-app-skips-auth-service ()
  "An application that does not require a password neither installs the
Auth service nor warns when `lispm-auth' is absent."
  (let ((command-line-args '("Emacs" "--lispm-hidden" "--lispm-app=gomoku"))
        (service-checked nil)
        (warned nil))
    (cl-letf (((symbol-function 'lispm-desktop-auth-ensure) (lambda () t))
              ((symbol-function 'lispm-auth-app-requires-password-p) (lambda (_) nil))
              ((symbol-function 'lispm-desktop-auth-ensure-service)
               (lambda () (setq service-checked t)))
              ((symbol-function 'lispm-auth-start-apps) (lambda (apps) (equal apps '(gomoku)))))
      (should (lispm-desktop-auth-prepare))
      (should (lispm-desktop-auth-setup))
      (should-not service-checked))
    (cl-letf (((symbol-function 'lispm-desktop-auth-ensure) (lambda () nil))
              ((symbol-function 'display-warning)
               (lambda (&rest _) (setq warned t))))
      (should-not (lispm-desktop-auth-prepare))
      (should-not warned))))

(ert-deftest lispm-desktop-auth-test-page-show-clears-side-and-dedicated-windows ()
  "Showing the password page dismisses dedicated side windows and restores
them on hide, even when invoked while a dedicated side window is selected."
  (require 'lispm-auth)
  (lispm-desktop-auth-ensure)
  (save-window-excursion
    (set-frame-size (selected-frame) 80 24)
    (delete-other-windows)
    (let* ((main-buf (get-buffer-create " *test-main*"))
           (side-buf (get-buffer-create " *test-side*"))
           (page-buf (get-buffer-create " *LispM Password*")))
      (unwind-protect
          (progn
            (switch-to-buffer main-buf)
            (let ((side-win (display-buffer-in-side-window
                             side-buf '((side . top)
                                        (window-height . 4)
                                        (dedicated . t)
                                        (window-parameters
                                         (no-delete-other-windows . t)
                                         (no-other-window . t))))))
              (select-window side-win)
              (should (= (length (window-list)) 2))
              (should (window-dedicated-p (selected-window)))
              (let ((noninteractive nil)
                    (page (list :buffer page-buf :frame nil
                                :window-configuration nil
                                :frame-parameters nil
                                :password "" :first nil :stage 'single)))
                (cl-letf (((symbol-function 'redisplay) #'ignore))
                  (lispm-auth--page-show page))
                (should (= (length (window-list)) 1))
                (should (eq (window-buffer (selected-window)) page-buf))
                (should-not (window-dedicated-p (selected-window)))
                (should-not (window-parameter (selected-window) 'window-side))
                (lispm-auth--page-hide page)
                (should (= (length (window-list)) 2)))))
        (when (buffer-live-p page-buf) (kill-buffer page-buf))
        (when (buffer-live-p main-buf) (kill-buffer main-buf))
        (when (buffer-live-p side-buf) (kill-buffer side-buf))))))

(ert-deftest lispm-desktop-auth-test-service-install-opens-progress-ui ()
  "Installing the Auth service during setup opens the progress UI even on
subsequent starts."
  (require 'lispm-auth)
  (require 'lispm-desktop-assistant)
  (let ((noninteractive nil)
        (lispm-desktop-setup--in-progress t)
        (lispm-auth-service-program nil)
        (installed nil)
        (progress-shown nil))
    (cl-letf (((symbol-function 'lispm-auth--service-program)
               (lambda ()
                 (if installed "/service" (signal 'lispm-auth-unavailable nil))))
              ((symbol-function 'lispm-desktop-assistant-ensure-progress)
               (lambda (&optional status) (setq progress-shown status)))
              ((symbol-function 'lispm-auth-install-service)
               (lambda () (setq installed t)))
              ((symbol-function 'redisplay) #'ignore))
      (should (equal (lispm-desktop-auth-ensure-service) "/service"))
      (should installed)
      (should (equal progress-shown "Installing LispM Auth service…")))))

(ert-deftest lispm-desktop-auth-test-service-update-prompts-and-falls-back ()
  "When an older Auth service exists, interactive startup asks Y/N and
keeps the existing binary when declined."
  (require 'lispm-auth)
  (require 'lispm-desktop-assistant)
  (let ((noninteractive nil)
        (lispm-auth-service-program nil)
        (prompted nil))
    (cl-letf (((symbol-function 'lispm-auth--service-program)
               (lambda ()
                 (or lispm-auth-service-program
                     (signal 'lispm-auth-unavailable nil))))
              ((symbol-function 'lispm-desktop-auth--existing-service-program)
               (lambda () "/existing/v0.2.0/lispm-auth-service"))
              ((symbol-function 'y-or-n-p)
               (lambda (prompt) (setq prompted prompt) nil))
              ((symbol-function 'lispm-auth-install-service)
               (lambda () (ert-fail "Should not force install when declined"))))
      (should (equal (lispm-desktop-auth-ensure-service)
                     "/existing/v0.2.0/lispm-auth-service"))
      (should (equal lispm-auth-service-program
                     "/existing/v0.2.0/lispm-auth-service"))
      (should prompted))))

(ert-deftest lispm-desktop-auth-test-confirm-update-modes ()
  "Desktop Mode (GUI/TUI) uses visual alert popups; Focus Mode uses y-or-n-p."
  (require 'lispm-desktop-assistant)
  (require 'lispm-desktop-start-as)
  ;; 1. Desktop Mode with popup available -> calls lispm-ui-alert
  (let ((noninteractive nil)
        (lispm-desktop-started-as 'desktop)
        (alert-called nil))
    (cl-letf (((symbol-function 'lispm-ui-popup-available-p) #'always)
              ((symbol-function 'frame-visible-p) #'always)
              ((symbol-function 'lispm-ui-alert)
               (lambda (icon msg buttons &optional _detail)
                 (setq alert-called (list icon msg buttons))
                 'update)))
      (should (lispm-desktop-assistant-confirm-update
               "Update LispM Auth service to %s? " "v0.2.6"))
      (should (equal (car alert-called) 'note))
      (should (= (length (nth 2 alert-called)) 2))))
  ;; 2. Focus Mode -> calls y-or-n-p
  (let ((noninteractive nil)
        (lispm-desktop-started-as 'terminal)
        (yn-called nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (prompt) (setq yn-called prompt) nil)))
      (should-not (lispm-desktop-assistant-confirm-update
                   "Update LispM Auth service to %s? " "v0.2.6"))
      (should yn-called))))

(provide 'lispm-desktop-auth-test)
;;; lispm-desktop-auth-test.el ends here
