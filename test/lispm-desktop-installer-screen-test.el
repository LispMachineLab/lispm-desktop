;;; lispm-desktop-installer-screen-test.el --- Early installer tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:
;; Run standalone with emacs -Q --batch -l test/lispm-desktop-installer-screen-test.el -f ert-run-tests-batch-and-exit.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path
             (expand-file-name "../lisp" (file-name-directory (or load-file-name buffer-file-name))))
(require 'lispm-desktop-installer-screen)

(defmacro lispm-desktop-installer-screen-test--isolated (&rest body)
  "Run BODY with isolated presenter state and remove its advice afterwards."
  (declare (indent 0) (debug t))
  `(let ((lispm-desktop-installer-screen--active nil)
         (lispm-desktop-installer-screen--completed 0)
         (lispm-desktop-installer-screen--stage-packages 0)
         (lispm-desktop-installer-screen-language 'en)
         (lispm-desktop-installer-screen--window nil)
         (lispm-desktop-installer-screen--previous nil))
     (unwind-protect (progn ,@body)
       (lispm-desktop-installer-screen-stop))))

(ert-deftest lispm-desktop-installer-screen-test-batch-does-not-advise ()
  (lispm-desktop-installer-screen-test--isolated
    (let ((noninteractive t))
      (lispm-desktop-installer-screen-start)
      (should-not lispm-desktop-installer-screen--active)
      (should-not (advice-member-p #'lispm-desktop-installer-screen--archive
                                   'package-install-from-archive))
      (should-not (advice-member-p #'lispm-desktop-installer-screen--refresh
                                   'package-refresh-contents)))))

(ert-deftest lispm-desktop-installer-screen-test-archive-success ()
  (lispm-desktop-installer-screen-test--isolated
    (let ((desc (package-desc-create :name 'example :version '(1 0)))
          (reports nil) (calls nil))
      (cl-letf (((symbol-function 'lispm-desktop-installer-screen-report)
                 (lambda (status) (push status reports))))
        (should (eq 'installed
                    (lispm-desktop-installer-screen--archive
                     (lambda (&rest args) (push args calls) 'installed)
                     desc 'extra)))
        (should (equal calls (list (list desc 'extra))))
        (should (= 1 lispm-desktop-installer-screen--completed))
        (should (string-match-p "example" (car reports)))
        (lispm-desktop-installer-screen--archive (lambda (&rest _) nil) desc)
        (should (= 2 lispm-desktop-installer-screen--completed))))))

(ert-deftest lispm-desktop-installer-screen-test-archive-failure-and-quit ()
  (lispm-desktop-installer-screen-test--isolated
    (let ((desc (package-desc-create :name 'example :version '(1 0)))
          (lispm-desktop-installer-screen--completed 3))
      (cl-letf (((symbol-function 'lispm-desktop-installer-screen-report) #'ignore))
        (should-error (lispm-desktop-installer-screen--archive
                       (lambda (&rest _) (error "installation failed")) desc))
        (should (= 3 lispm-desktop-installer-screen--completed))
        (should (eq 'quit
                    (condition-case nil
                        (progn (lispm-desktop-installer-screen--archive
                                (lambda (&rest _) (signal 'quit nil)) desc)
                               'returned)
                      (quit 'quit))))
        (should (= 3 lispm-desktop-installer-screen--completed))))))

(ert-deftest lispm-desktop-installer-screen-test-refresh-forwards-arguments ()
  (lispm-desktop-installer-screen-test--isolated
    (let (reports calls)
      (cl-letf (((symbol-function 'lispm-desktop-installer-screen-report)
                 (lambda (status) (push status reports))))
        (should (eq 'refreshed
                    (lispm-desktop-installer-screen--refresh
                     (lambda (&rest args) (setq calls args) 'refreshed) 'one 'two)))
        (should (equal calls '(one two)))
        (should (= 1 (length reports)))
        (should (string-match-p "[Rr]efresh" (car reports)))))))

(ert-deftest lispm-desktop-installer-screen-test-stop-removes-advice ()
  (lispm-desktop-installer-screen-test--isolated
    (let ((noninteractive nil))
      (cl-letf (((symbol-function 'daemonp) (lambda (&rest _) nil)))
        (lispm-desktop-installer-screen-start)
        (should lispm-desktop-installer-screen--active)
        (should (advice-member-p #'lispm-desktop-installer-screen--archive
                                 'package-install-from-archive))
        (should (advice-member-p #'lispm-desktop-installer-screen--refresh
                                 'package-refresh-contents))
        (lispm-desktop-installer-screen-stop)
        (should-not lispm-desktop-installer-screen--active)
        (should-not (advice-member-p #'lispm-desktop-installer-screen--archive
                                     'package-install-from-archive))
        (should-not (advice-member-p #'lispm-desktop-installer-screen--refresh
                                     'package-refresh-contents))))))

(ert-deftest lispm-desktop-installer-screen-test-line-width ()
  (dolist (text '("" "Short" "A long installation message that needs truncation" "宽字符"))
    (should (= 30 (string-width (lispm-desktop-installer-screen--line text 30))))))

(ert-deftest lispm-desktop-installer-screen-test-svg-draw-status-and-logo ()
  (let ((status "Installing package: example")
        embedded)
    (cl-letf (((symbol-function 'svg-embed)
               (lambda (svg image image-type datap &rest args)
                 (setq embedded (list svg image image-type datap args)))))
      (let ((svg (lispm-desktop-installer-screen--draw 800 600 status)))
        (should (eq (car svg) 'svg))
        (should (string-match-p (regexp-quote status) (prin1-to-string svg)))
        (should embedded)
        (should (eq (car embedded) svg))
        (should (equal (nth 2 embedded) "image/png"))
        (if (nth 3 embedded)
            (should (equal (nth 1 embedded)
                           (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally
                              lispm-desktop-installer-screen--logo)
                             (buffer-string))))
          (should (equal (expand-file-name (nth 1 embedded))
                         (expand-file-name lispm-desktop-installer-screen--logo))))))))

(ert-deftest lispm-desktop-installer-screen-test-svg-unavailable ()
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil))
            ((symbol-function 'image-type-available-p) (lambda (&rest _) t)))
    (should-not (lispm-desktop-installer-screen--svg-available-p)))
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
            ((symbol-function 'image-type-available-p) (lambda (&rest _) nil)))
    (should-not (lispm-desktop-installer-screen--svg-available-p)))
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
            ((symbol-function 'image-type-available-p)
             (lambda (type) (eq type 'svg))))
    (should (lispm-desktop-installer-screen--svg-available-p))))

(ert-deftest lispm-desktop-installer-screen-test-stage-success-and-failure ()
  (lispm-desktop-installer-screen-test--isolated
    (let ((lispm-desktop-installer-screen--active t)
          (lispm-desktop-installer-screen--finished nil))
      (cl-letf (((symbol-function 'lispm-desktop-installer-screen-report) #'ignore))
        (should (eq 'ok (lispm-desktop-installer-screen--ensure (lambda (&rest _) 'ok) 'lispm-kit)))
        (lispm-desktop-installer-screen--ensure (lambda (&rest _) nil) 'lispm-kit)
        (should (= 1 (lispm-desktop-installer-screen--progress)))
        (should-error (lispm-desktop-installer-screen--ensure (lambda (&rest _) (error "failed")) 'lispm-ui))
        (should (= 1 (lispm-desktop-installer-screen--progress)))))))

(ert-deftest lispm-desktop-installer-screen-test-message-capture ()
  (lispm-desktop-installer-screen-test--isolated
    (let ((lispm-desktop-installer-screen--active t)
          (lispm-desktop-installer-screen--status "Installing")
          (lispm-desktop-installer-screen--message nil)
          reported)
      (cl-letf (((symbol-function 'lispm-desktop-installer-screen-report)
                 (lambda (status) (setq reported status))))
        (lispm-desktop-installer-screen--capture-message
         (lambda (&rest _) 'result) "Downloading %s" "example")
        (should (equal lispm-desktop-installer-screen--message "Downloading example"))
        (should (equal reported "Installing"))))))

(ert-deftest lispm-desktop-installer-screen-test-suppressed-message ()
  (lispm-desktop-installer-screen-test--isolated
    (let ((lispm-desktop-installer-screen--active t)
          (lispm-desktop-installer-screen--message nil)
          (inhibit-message t))
      (cl-letf (((symbol-function 'lispm-desktop-installer-screen-report)
                 (lambda (&rest _) (error "Unexpected display"))))
        (should (eq 'result (lispm-desktop-installer-screen--capture-message
                             (lambda (&rest _) 'result) "hidden")))
        (should-not lispm-desktop-installer-screen--message)))))

(ert-deftest lispm-desktop-installer-screen-test-failure-page-is-persistent ()
  (lispm-desktop-installer-screen-test--isolated
    (let ((lispm-desktop-installer-screen--failure nil)
          (lispm-desktop-installer-screen--stage nil))
      (lispm-desktop-installer-screen-fail "Preparing: lispm-kit")
      (with-current-buffer (get-buffer lispm-desktop-installer-screen--buffer)
        (should (string-match-p "INSTALLATION STOPPED" (buffer-string)))
        (should (string-match-p "lispm-kit" (buffer-string)))
        (should (eq (lookup-key (current-local-map) (kbd "q"))
                    #'lispm-desktop-installer-screen-stop)))
      (should lispm-desktop-installer-screen--failure))))

(ert-deftest lispm-desktop-installer-screen-test-i18n-and-substage-progress ()
  (lispm-desktop-installer-screen-test--isolated
    (let ((lispm-desktop-installer-screen--active t)
          (lispm-desktop-installer-screen--finished nil)
          (lispm-desktop-installer-screen--stage-packages 0)
          (lispm-desktop-installer-screen-language 'zh-Hans)
          (desc (package-desc-create :name 'example :version '(1 0))))
      (should (equal (lispm-desktop-installer-screen--tr "LispM Installation") "LispM 安装"))
      (should (equal (lispm-desktop-installer-screen--localize-status "Installing package: example")
                     "正在安装软件包：example"))
      (should (equal (lispm-desktop-installer-screen--localize-status "Downloading (65%)…")
                     "正在下载（65%）…"))
      (cl-letf (((symbol-function 'lispm-desktop-installer-screen-report) #'ignore))
        (should (= 0.0 (lispm-desktop-installer-screen--progress-fraction)))
        (lispm-desktop-installer-screen--archive (lambda (&rest _) 'ok) desc)
        (should (> (lispm-desktop-installer-screen--progress-fraction) 0.0))
        (should (< (lispm-desktop-installer-screen--progress-fraction) 1.0))
        (lispm-desktop-installer-screen-complete 'lispm-kit)
        (should (= 0 lispm-desktop-installer-screen--stage-packages))
        (should (= 1.0 (lispm-desktop-installer-screen--progress-fraction)))))))

(provide 'lispm-desktop-installer-screen-test)
;;; lispm-desktop-installer-screen-test.el ends here
