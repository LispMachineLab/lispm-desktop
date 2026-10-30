;;; lispm-desktop-store-defaults-test.el --- Removable distribution defaults -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Loaded after lispm-desktop-test.el.  The real Store registration,
;; Customize persistence and extension unload paths run against a fixture
;; in a temporary profile; no network or installed package is touched.

;;; Code:

(defvar lispm-desktop-test-default-active nil)
(defvar lispm-desktop-test-default-loads 0)
(defvar lispm-desktop-test-default-fail nil)

(defmacro lispm-desktop-test--with-default-store (&rest body)
  "Run BODY with a removable extension in a fresh, persistent profile."
  (declare (indent 0))
  `(let ((save-variable (symbol-function 'customize-save-variable)))
     (lispm-desktop-test--with-store
       (let* ((custom-file (expand-file-name "custom.el" lispm-desktop-store-directory))
              (user-init-file (expand-file-name "init.el" lispm-desktop-store-directory))
              (lispm-desktop-store-default-items '(office))
              (lispm-desktop-store-defaults-installed nil)
              (lispm-desktop-store--defaults-attempted nil)
              (lispm-desktop-test-default-active nil)
              (lispm-desktop-test-default-loads 0)
              (lispm-desktop-test-default-fail nil)
              (custom-properties
               (mapcar (lambda (symbol) (cons symbol (copy-tree (symbol-plist symbol))))
                       '(lispm-desktop-store-installed lispm-desktop-store-defaults-installed user)))
              (extension (expand-file-name "lispm-test-default-extension.el" lispm-desktop-store-directory))
              (load-path (cons lispm-desktop-store-directory load-path)))
         (with-temp-file extension
           (insert ";;; -*- lexical-binding: t; -*-\n"
                   "(when lispm-desktop-test-default-fail (error \"Fixture load failed\"))\n"
                   "(setq lispm-desktop-test-default-active t)\n"
                   "(cl-incf lispm-desktop-test-default-loads)\n"
                   "(defun lispm-test-default-extension-unload-function ()\n"
                   "  (setq lispm-desktop-test-default-active nil) nil)\n"
                   "(provide 'lispm-test-default-extension)\n"))
         (cl-letf (((symbol-function 'customize-save-variable) save-variable))
           (unwind-protect
               (progn
                 (lispm-desktop-test--store-catalog
                  "https://example.org/defaults"
                  '((office :kind extension :name "Office fixture"
                            :package (lispm-test-default-extension
                                      :url "https://example.org/office"
                                      :rev "0123456789abcdef0123456789abcdef01234567")
                            :feature lispm-test-default-extension)))
                 ,@body)
             (when (featurep 'lispm-test-default-extension)
               (unload-feature 'lispm-test-default-extension t))
             (dolist (entry custom-properties)
               (setplist (car entry) (cdr entry)))))))))

(ert-deftest lispm-desktop-test-store-default-remove-restart-reinstall ()
  "Removing the last default survives saved-state reload and installer reruns."
  (lispm-desktop-test--with-default-store
    (lispm-desktop-store--start-defaults)
    (should lispm-desktop-test-default-active)
    (should (= lispm-desktop-test-default-loads 1))
    (should (equal lispm-desktop-store-defaults-installed '(office)))
    (should-not (plist-get (lispm-desktop-store--installed 'office) :owned))
    (lispm-desktop-store-uninstall 'office t)
    (should-not lispm-desktop-test-default-active)
    (should-not (featurep 'lispm-test-default-extension))
    (should-not lispm-desktop-store-installed)
    (should (string-match-p "lispm-desktop-store-defaults-installed.*office"
                            (with-temp-buffer (insert-file-contents custom-file) (buffer-string))))
    ;; Reload the real persisted profile, including the explicit empty
    ;; installed list.  Neither the startup nor installer may reinstall.
    (let ((state
           (with-temp-buffer
             (should
              (zerop
               (call-process
                (expand-file-name invocation-name invocation-directory) nil '(t nil) nil
                "-Q" "--batch" "--eval"
                (prin1-to-string
                 `(progn
                    (defvar lispm-desktop-store-installed nil)
                    (defvar lispm-desktop-store-defaults-installed nil)
                    (load ,custom-file nil t)
                    (prin1 (list lispm-desktop-store-installed
                                 lispm-desktop-store-defaults-installed)))))))
             (goto-char (point-min))
             (read (current-buffer)))))
      (setq lispm-desktop-store-installed (car state)
            lispm-desktop-store-defaults-installed (cadr state)
            lispm-desktop-store--defaults-attempted nil))
    (should-not lispm-desktop-store-installed)
    (should (equal lispm-desktop-store-defaults-installed '(office)))
    (lispm-desktop-store--register-installed)
    (lispm-desktop-store--start-defaults)
    (lispm-desktop-store-install-defaults-batch)
    (should-not lispm-desktop-test-default-active)
    (should (= lispm-desktop-test-default-loads 1))
    (lispm-desktop-store-read-catalogs)
    (should (assq 'office (lispm-desktop-store-items)))
    (lispm-desktop-store-install 'office t)
    (should lispm-desktop-test-default-active)
    (should (= lispm-desktop-test-default-loads 2))
    (should (file-exists-p extension))))

(ert-deftest lispm-desktop-test-store-default-failure-retries-next-start ()
  "Failed activation is not saved as completed and is retried next start."
  (lispm-desktop-test--with-default-store
    (setq lispm-desktop-test-default-fail t)
    (lispm-desktop-store--start-defaults)
    (should-not lispm-desktop-store-installed)
    (should-not lispm-desktop-store-defaults-installed)
    (should (eq (lispm-desktop-store-job-state (lispm-desktop-store--job 'office)) 'failed))
    (setq lispm-desktop-test-default-fail nil)
    (lispm-desktop-store--start-defaults)
    (should-not lispm-desktop-test-default-active)
    (setq lispm-desktop-store--defaults-attempted nil)
    (lispm-desktop-store--start-defaults)
    (should lispm-desktop-test-default-active)
    (should (equal lispm-desktop-store-defaults-installed '(office)))))

(ert-deftest lispm-desktop-test-store-default-adopts-older-install ()
  "Existing Store installations need no fresh install to record completion."
  (lispm-desktop-test--with-default-store
    (lispm-desktop-store-install 'office t)
    (setq lispm-desktop-store-defaults-installed nil
          lispm-desktop-store--defaults-attempted nil)
    (cl-letf (((symbol-function 'lispm-desktop-store-install)
               (lambda (&rest _) (ert-fail "An existing default was installed again"))))
      (lispm-desktop-store--start-defaults))
    (should (= lispm-desktop-test-default-loads 1))
    (should (equal lispm-desktop-store-defaults-installed '(office)))))

(ert-deftest lispm-desktop-test-store-default-legacy-remove-before-idle ()
  "Removing an older installation before idle bootstrap also records optout."
  (lispm-desktop-test--with-default-store
    (lispm-desktop-store-install 'office t)
    (setq lispm-desktop-store-defaults-installed nil
          lispm-desktop-store--defaults-attempted nil)
    (lispm-desktop-store-uninstall 'office t)
    (lispm-desktop-store--start-defaults)
    (should-not lispm-desktop-test-default-active)
    (should (equal lispm-desktop-store-defaults-installed '(office)))))

(ert-deftest lispm-desktop-test-store-default-waits-for-catalog ()
  "A missing catalog is fetched asynchronously; completion queues the item."
  (lispm-desktop-test--with-default-store
    (let ((catalogs lispm-desktop-store--catalogs)
          (fetches 0)
          (lispm-desktop-store-update-automatically nil))
      (setq lispm-desktop-store--catalogs nil)
      (cl-letf (((symbol-function 'lispm-desktop-store-read-catalogs) #'ignore)
                ((symbol-function 'lispm-desktop-store-check-for-updates)
                 (lambda (&optional _) (cl-incf fetches)
                   (setq lispm-desktop-store--checking t))))
        (lispm-desktop-store--start-defaults)
        (lispm-desktop-store--start-defaults)
        (should (= fetches 1))
        (should-not lispm-desktop-test-default-active)
        (should-not lispm-desktop-store-defaults-installed))
      (setq lispm-desktop-store--catalogs catalogs
            lispm-desktop-store--checking nil)
      (lispm-desktop-store--checked-updates)
      (should lispm-desktop-test-default-active))))

(ert-deftest lispm-desktop-test-store-default-batch-reports-failure ()
  "The distribution installer fails when its default could not load."
  (lispm-desktop-test--with-default-store
    (setq lispm-desktop-test-default-fail t)
    (should-error (lispm-desktop-store-install-defaults-batch))
    (should-not lispm-desktop-store-defaults-installed)))

(ert-deftest lispm-desktop-test-store-default-catalog-start-failure ()
  "A catalog process that cannot start leaves defaults available for retry."
  (lispm-desktop-test--with-default-store
    (let ((fetches 0))
      (setq lispm-desktop-store--catalogs nil)
      (cl-letf (((symbol-function 'lispm-desktop-store-read-catalogs) #'ignore)
                ((symbol-function 'lispm-desktop-store-check-for-updates)
                 (lambda (&optional _) (cl-incf fetches) (error "Cannot start git"))))
        (lispm-desktop-store--start-defaults)
        (should-not lispm-desktop-store-defaults-installed)
        (should-not lispm-desktop-store--defaults-attempted)
        (lispm-desktop-store--start-defaults)
        (should (= fetches 2))))))

(ert-deftest lispm-desktop-test-store-default-no-defaults-no-fetch ()
  "An explicit empty default list does no catalog or installation work."
  (lispm-desktop-test--with-default-store
    (let ((lispm-desktop-store-default-items nil))
      (cl-letf (((symbol-function 'lispm-desktop-store-read-catalogs)
                 (lambda () (ert-fail "No defaults should require no catalog"))))
        (lispm-desktop-store--start-defaults)
        (lispm-desktop-store-install-defaults-batch)))
    (should-not lispm-desktop-test-default-active)))

(ert-deftest lispm-desktop-test-store-default-agent-package-dependency ()
  "A source checkout cannot satisfy package.el's Agent dependency by itself."
  (let ((package-alist nil)
        installed)
    (cl-letf (((symbol-function 'package-vc-install)
               (lambda (package rev) (setq installed (list package rev))))
              ((symbol-function 'package-installed-p)
               (lambda (name version)
                 (and installed (eq name 'lispm) (equal version '(0 15 0)))))
              ((symbol-function 'lispm-desktop-store--release-tag)
               (lambda (_url) "v0.15.0")))
      (should-not (lispm-desktop-store-installer--libraries
                   #'identity '((lispm (0 15 0)))))
      (should (equal installed
                     '((lispm :url "https://github.com/LispMachineLab/lispm-agent"
                              :lisp-dir "lisp") "v0.15.0"))))))

(ert-deftest lispm-desktop-test-store-default-terminal-startup ()
  "Terminal startup loads extensions, leaves apps out and schedules work once."
  (lispm-desktop-test--with-default-store
    (lispm-desktop-store-install 'office t)
    (unload-feature 'lispm-test-default-extension t)
    (push '(calendar :kind app :name "Calendar" :command calendar)
          lispm-desktop-store-installed)
    (let ((noninteractive nil)
          (lispm-desktop-store--startup-scheduled nil)
          timers)
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (_delay _repeat function &rest _) (push function timers)))
                ((symbol-function 'lispm-desktop-store--register-app)
                 (lambda (&rest _) (ert-fail "Terminal startup registered an application"))))
        (lispm-desktop-store-startup t)
        (should lispm-desktop-test-default-active)
        (should (equal (sort (copy-sequence timers) #'string-lessp)
                       '(lispm-desktop-store--start-defaults lispm-desktop-store--sync-skills-at-start)))
        (lispm-desktop-store-uninstall 'office t)
        (lispm-desktop-store-startup t)
        (lispm-desktop-store--start-defaults)
        (should-not lispm-desktop-test-default-active)
        (should (= (length timers) 2))))))

(provide 'lispm-desktop-store-defaults-test)
;;; lispm-desktop-store-defaults-test.el ends here
