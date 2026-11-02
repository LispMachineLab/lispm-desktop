;;; lispm-desktop-update-safety-test.el --- Host update readiness regressions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:
;; Loaded after lispm-desktop-test.el and the Store dependency fixtures.
;; Every checkout and update log belongs to a temporary directory.

;;; Code:

(defun lispm-desktop-test--safety-desktop (directory minimum)
  "Commit a Desktop dependency declaration in DIRECTORY requiring MINIMUM."
  (make-directory (expand-file-name "lisp" directory) t)
  (unless (file-directory-p (expand-file-name ".git" directory))
    (lispm-desktop-test--git directory "init" "--quiet"))
  (with-temp-file (expand-file-name "lisp/lispm-desktop-deps.el" directory)
    (insert ";;; -*- lexical-binding: t; -*-\n"
            ;; A preflight must not run arbitrary forms from this file.
            "(error \"Candidate code was evaluated\")\n"
            (format "(defconst lispm-desktop-deps '((proof-ui :checkout \"sibling-ui\" :version %S)))\n"
                    minimum)))
  (lispm-desktop-test--git directory "add" "lisp/lispm-desktop-deps.el")
  (lispm-desktop-test--git directory "commit" "--quiet" "-m" minimum)
  (lispm-desktop-test--git directory "rev-parse" "HEAD"))

(defmacro lispm-desktop-test--with-safety (&rest body)
  "Run BODY with old Desktop/UI checkouts and fetched candidate commits.
Bind ROOT, DESKTOP, UI, HOST-OLD, HOST-NEW, UI-OLD, UI-NEW and UPDATES.
The proposed list deliberately puts Desktop first to exercise ordering."
  (declare (indent 0))
  `(lispm-desktop-test--with-update
     (let* ((root (make-temp-file "lispm-update-safety-" t))
            (desktop (file-name-as-directory (expand-file-name "desktop" root)))
            (ui (file-name-as-directory (expand-file-name "active-ui" root)))
            (ui-old (lispm-desktop-test--dependency-repository ui 'proof-ui "0.6.0"))
            (ui-new (lispm-desktop-test--dependency-repository ui 'proof-ui "0.7.0"))
            (host-old (lispm-desktop-test--safety-desktop desktop "0.6.0"))
            (host-new (lispm-desktop-test--safety-desktop desktop "0.7.0"))
            (lispm-desktop-directory desktop)
            (load-path (cons ui load-path))
            (updates (list (list :name "Desktop" :directory desktop :from host-old :to host-new)
                           (list :name "UI" :directory ui :from ui-old :to ui-new))))
       (unwind-protect
           (progn
             (lispm-desktop-test--git ui "checkout" "--quiet" "--detach" ui-old)
             (lispm-desktop-test--git desktop "checkout" "--quiet" "--detach" host-old)
             ,@body)
         (delete-directory root t)))))

(ert-deftest lispm-desktop-test-update-safety-omitted-dirty-dependency ()
  "A dirty dependency omitted by discovery cannot strand the new Desktop."
  (lispm-desktop-test--with-safety
    (let ((source (expand-file-name "proof-ui.el" ui)))
      (with-temp-buffer
        (insert-file-contents source)
        (goto-char (point-max)) (insert ";; local preview\n")
        (write-region (point-min) (point-max) source nil 'silent))
      (let ((before (with-temp-buffer (insert-file-contents source) (buffer-string)))
            (lispm-desktop-update--results
             (list (car updates) (list :name "UI" :directory ui :state 'kept))))
        (should-not (lispm-desktop-update-install (list (car updates))))
        (should (equal (lispm-desktop-store--git desktop "rev-parse" "HEAD") host-old))
        (should (equal (lispm-desktop-store--git ui "rev-parse" "HEAD") ui-old))
        (should (equal (with-temp-buffer (insert-file-contents source) (buffer-string)) before))))))

(ert-deftest lispm-desktop-test-update-safety-active-package-shadows-sibling ()
  "A new sibling checkout cannot hide the old package startup actually uses."
  (lispm-desktop-test--with-safety
    (lispm-desktop-test--dependency-repository
     (expand-file-name "sibling-ui" root) 'proof-ui "0.7.0")
    (should-error (lispm-desktop-update-check-dependencies desktop host-new) :type 'user-error)
    (should-not (lispm-desktop-update-install (list (car updates))))
    (should (equal (lispm-desktop-store--git desktop "rev-parse" "HEAD") host-old))))

(ert-deftest lispm-desktop-test-update-safety-dependency-before-host ()
  "A source dependency advances first, even when Desktop was listed first."
  (lispm-desktop-test--with-safety
    (let ((install (symbol-function 'lispm-desktop-update--install-checkout))
          order)
      (cl-letf (((symbol-function 'lispm-desktop-update--install-checkout)
                 (lambda (update)
                   (when (equal (plist-get update :directory) desktop)
                     (should (equal (lispm-desktop-store--git ui "rev-parse" "HEAD") ui-new)))
                   (push (plist-get update :name) order)
                   (funcall install update))))
        (should (equal (lispm-desktop-update-install updates) (reverse updates))))
      (should (equal (nreverse order) '("UI" "Desktop"))))
    (should (equal (lispm-desktop-store--git desktop "rev-parse" "HEAD") host-new))
    (should (lispm-desktop-update-check-dependencies desktop host-new))))

(ert-deftest lispm-desktop-test-update-safety-failed-build-keeps-host ()
  "A dependency build failure leaves Desktop at its previously usable commit."
  (lispm-desktop-test--with-safety
    (setf (plist-get (cadr updates) :package) 'proof-ui)
    (cl-letf (((symbol-function 'lispm-desktop-store-installer-call)
               (lambda (_) (error "Dependency rebuild failed"))))
      (should-not (lispm-desktop-update-install updates)))
    (should (equal (lispm-desktop-store--git desktop "rev-parse" "HEAD") host-old))
    (should (equal (lispm-desktop-store--git ui "rev-parse" "HEAD") ui-old))))

(ert-deftest lispm-desktop-test-update-safety-symlinked-load-path ()
  "A dependency reached through a symlink still matches its candidate tree."
  (lispm-desktop-test--with-safety
    (let* ((alias (expand-file-name "ui-alias" root))
           (load-path (cons alias (remove ui load-path))))
      (make-symbolic-link (directory-file-name ui) alias)
      (should (lispm-desktop-update-check-dependencies desktop host-new updates))
      (should (equal (lispm-desktop-update-install updates) (reverse updates))))))

(ert-deftest lispm-desktop-test-update-safety-rechecks-after-preflight ()
  "A local edit made after group validation still blocks the switch."
  (lispm-desktop-test--with-safety
    (let ((install (symbol-function 'lispm-desktop-update--install-checkout)))
      (cl-letf (((symbol-function 'lispm-desktop-update--install-checkout)
                 (lambda (update)
                   (with-temp-buffer
                     (insert ";; edited while update was preparing\n")
                     (write-region (point-min) (point-max) (expand-file-name "proof-ui.el" ui) t 'silent))
                   (funcall install update))))
        (should-not (lispm-desktop-update-install updates))))
    (should (equal (lispm-desktop-store--git desktop "rev-parse" "HEAD") host-old))
    (should (equal (lispm-desktop-store--git ui "rev-parse" "HEAD") ui-old))))

(ert-deftest lispm-desktop-test-update-safety-checks-disk-after-plan ()
  "An installer reporting success without advancing a dependency cannot pass."
  (lispm-desktop-test--with-safety
    (setf (plist-get (cadr updates) :package) 'proof-ui)
    (cl-letf (((symbol-function 'lispm-desktop-store-installer-call) #'ignore))
      (should (equal (lispm-desktop-update-install updates) (list (cadr updates)))))
    (should (equal (lispm-desktop-store--git desktop "rev-parse" "HEAD") host-old))))

(ert-deftest lispm-desktop-test-update-safety-keeps-compatible-local-dependency ()
  "A locally edited dependency already satisfying the new host can stay put."
  (lispm-desktop-test--with-safety
    (lispm-desktop-test--git ui "checkout" "--quiet" "--detach" ui-new)
    (with-temp-buffer
      (insert ";; local customization\n")
      (write-region (point-min) (point-max) (expand-file-name "proof-ui.el" ui) t 'silent))
    (should (equal (lispm-desktop-update-install (list (car updates))) (list (car updates))))))

(ert-deftest lispm-desktop-test-update-safety-blocked-host-does-not-run-installer ()
  "A rejected Desktop update cannot bypass the gate through install.sh on quit."
  (cl-letf (((symbol-function 'lispm-desktop-update-installs-on-quit-p) #'always)
            ((symbol-function 'lispm-desktop-update-needs-installer-p) #'always)
            ((symbol-function 'lispm-desktop-update-install) #'ignore)
            ((symbol-function 'lispm-desktop-update--run-installer)
             (lambda (&rest _) (ert-fail "Installer ran after rejected update"))))
    (lispm-desktop-update--install-on-quit)))

;;; lispm-desktop-update-safety-test.el ends here
