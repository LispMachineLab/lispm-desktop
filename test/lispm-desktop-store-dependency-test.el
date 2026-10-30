;;; lispm-desktop-store-dependency-test.el --- Updating Store dependencies -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Loaded after lispm-desktop-test.el.  Package installation and rebuilding
;; use local git repositories and temporary profiles, never real packages.

;;; Code:

(defun lispm-desktop-test--dependency-repository (directory name version &optional requires lisp-dir code)
  "Commit NAME at VERSION in DIRECTORY with REQUIRES, LISP-DIR and CODE."
  (make-directory directory t)
  (unless (file-directory-p (expand-file-name ".git" directory))
    (lispm-desktop-test--git directory "init" "--quiet"))
  (let ((file (concat (when lisp-dir (concat lisp-dir "/")) (format "%s.el" name))))
    (make-directory (file-name-directory (expand-file-name file directory)) t)
    (with-temp-file (expand-file-name file directory)
      (insert (format ";;; %s --- Dependency fixture -*- lexical-binding: t; -*-\n" file)
              (format ";; Version: %s\n;; Package-Requires: %S\n" version
                      (cons '(emacs "30.1") requires))
              ";;; Code:\n"
              (format "(defun %s-test-version () \"Return the fixture version.\" %S)\n" name version)
              (or code "")
              (format "(provide '%s)\n;;; %s ends here\n" name file)))
    (lispm-desktop-test--git directory "add" file)
    (lispm-desktop-test--git directory "commit" "--quiet" "-m" version)
    (lispm-desktop-test--git directory "tag" (concat "v" version))
    (lispm-desktop-test--git directory "rev-parse" "HEAD")))

(defun lispm-desktop-test--dependency-child (root key form)
  "Run FORM with a real installer in ROOT's isolated profile named KEY."
  (with-temp-buffer
    (let ((status
           (call-process
            (expand-file-name invocation-name invocation-directory) nil '(t t) nil
            "-Q" "--batch" "-L" lispm-desktop-store-installer--directory "--eval"
            (prin1-to-string
             `(progn
                (setq user-emacs-directory ,(file-name-as-directory (expand-file-name (concat key "-profile") root))
                      package-user-dir ,(expand-file-name key root)
                      package-alist nil package-directory-list nil package-archives nil
                      package-check-signature nil package-native-compile nil
                      native-comp-enable-subr-trampolines nil
                      package-vc-register-as-project nil)
                (require 'ert)
                (require 'package-vc)
                (require 'lispm-desktop-store-installer)
                (lispm-desktop-store-installer--prepare)
                ,form)))))
      (unless (eql status 0)
        (ert-fail (buffer-string))))))

(ert-deftest lispm-desktop-test-store-dependency-real-vc-upgrade ()
  "Old Agent/UI checkouts update without prompts; absent ones install normally."
  (skip-unless (executable-find "git"))
  (let* ((root (make-temp-file "lispm-desktop-test-dependencies" t))
         (ui (expand-file-name "ui" root))
         (agent (expand-file-name "agent" root)))
    (unwind-protect
        (progn
          (lispm-desktop-test--dependency-repository ui 'lispm-ui "0.5.0")
          (lispm-desktop-test--dependency-repository ui 'lispm-ui "0.6.0")
          (lispm-desktop-test--dependency-repository agent 'lispm "0.14.0")
          (lispm-desktop-test--dependency-repository agent 'lispm "0.15.0")
          (dolist (existing '(lispm-ui lispm))
            (lispm-desktop-test--dependency-child
             root (format "elpa-%s" existing)
             `(let ((lispm-desktop-store-installer-libraries
                     '((lispm-ui :url ,(concat "file://" ui))
                       (lispm :url ,(concat "file://" agent)))))
                (package-vc-install
                 (assq ',existing lispm-desktop-store-installer-libraries)
                 ,(if (eq existing 'lispm-ui) "v0.5.0" "v0.14.0"))
                (require ',existing)
                (should-not (package-vc-install-dependencies '((lispm-ui (0 6 0)) (lispm (0 15 0)))))
                (should (package-installed-p 'lispm-ui '(0 6 0)))
                (should (package-installed-p 'lispm '(0 15 0)))
                (should (equal (funcall ',(intern (format "%s-test-version" existing)))
                               ,(if (eq existing 'lispm-ui) "0.6.0" "0.15.0")))
                ;; A rebuild is not proof that the minimum was satisfied.
                (should (equal (package-vc-install-dependencies '((lispm-ui (0 7 0))))
                               '((lispm-ui (0 7 0)))))))))
      (delete-directory root t))))

(ert-deftest lispm-desktop-test-store-dependency-no-release-keeps-checkout ()
  "Without a known release, an outdated checkout remains unmet and untouched."
  (let* ((desc (package-desc-create :name 'lispm-ui :version '(0 5 0) :kind 'vc :dir "/unused/"))
         (package-alist (list (list 'lispm-ui desc)))
         (package-activated-list nil))
    (cl-letf (((symbol-function 'lispm-desktop-store--release-tag) (lambda (_) nil))
              ((symbol-function 'package-vc-install) (lambda (&rest _) (ert-fail "Reinstalled checkout")))
              ((symbol-function 'lispm-desktop-store-installer--update)
               (lambda (&rest _) (ert-fail "Updated without a release"))))
      (should (equal (lispm-desktop-store-installer--libraries #'identity '((lispm-ui (0 6 0))))
                     '((lispm-ui (0 6 0))))))
    (should (equal (package-desc-version desc) '(0 5 0)))))

(ert-deftest lispm-desktop-test-store-dependency-update-rolls-back ()
  "Early and late rebuild failures restore source, descriptors and loaded code."
  (skip-unless (executable-find "git"))
  (let* ((root (make-temp-file "lispm-desktop-test-dependency-rollback" t))
         (source (expand-file-name "ui" root)))
    (unwind-protect
        (let ((old (lispm-desktop-test--dependency-repository source 'lispm-ui "0.5.0")))
          (lispm-desktop-test--dependency-repository source 'lispm-ui "0.6.0")
          (with-temp-file (expand-file-name "lispm-ui-new.el" source)
            (insert ";;; lispm-ui-new.el --- New fixture -*- lexical-binding: t; -*-\n"
                    "(provide 'lispm-ui-new)\n"))
          (lispm-desktop-test--git source "add" "lispm-ui-new.el")
          (lispm-desktop-test--git source "commit" "--quiet" "-m" "Add a library")
          (lispm-desktop-test--git source "tag" "--force" "v0.6.0")
          (dolist (late '(nil t))
            (lispm-desktop-test--dependency-child
             root (format "elpa-%s" late)
             `(let ((lispm-desktop-store-installer-libraries '((lispm-ui :url ,(concat "file://" source))))
                    (rebuild (symbol-function 'package-vc-rebuild)))
                (package-vc-install (car lispm-desktop-store-installer-libraries) "v0.5.0")
                (require 'lispm-ui)
                (cl-letf (((symbol-function 'package-vc-rebuild)
                           (lambda (desc)
                             (if (equal (lispm-desktop-store--git (package-desc-dir desc) "rev-parse" "HEAD") ,old)
                                 (funcall rebuild desc)
                               ,@(when late '((funcall rebuild desc)))
                               (error "Fixture rebuild failed")))))
                  (should-error (package-vc-install-dependencies '((lispm-ui (0 6 0))))))
                (should (equal (lispm-desktop-store--git
                                (package-desc-dir (cadr (assq 'lispm-ui package-alist))) "rev-parse" "HEAD")
                               ,old))
                (should (equal (package-desc-version (cadr (assq 'lispm-ui package-alist))) '(0 5 0)))
                (should (equal (lispm-ui-test-version) "0.5.0"))
                (should-not (locate-library "lispm-ui-new"))
                ;; The restored on-disk descriptor must also survive a restart.
                (setq package-alist nil package-activated-list nil)
                (package-initialize)
                (should-not (package-installed-p 'lispm-ui '(0 6 0)))
                (should-not (package-vc-install-dependencies '((lispm-ui (0 6 0)))))
                (should (equal (lispm-ui-test-version) "0.6.0"))))))
      (delete-directory root t))))

(ert-deftest lispm-desktop-test-store-dependency-adopts-rebuilt-library ()
  "The parent refreshes a rebuilt dependency's descriptor and loaded code."
  (skip-unless (executable-find "git"))
  (lispm-desktop-test--with-store
    (lispm-desktop-test--with-packages
      (let* ((root (make-temp-file "lispm-desktop-test-dependency-adopt" t))
             (source (expand-file-name "dependency" root))
             (consumer (expand-file-name "consumer" root))
             (library 'lispm-desktop-test-dependency)
             (url (concat "file://" source))
             (program (symbol-function 'lispm-desktop-store-installer-program)))
        (unwind-protect
            (progn
              (lispm-desktop-test--dependency-repository source library "0.5.0")
              (lispm-desktop-test--dependency-repository source library "0.6.0")
              (lispm-desktop-test--dependency-repository
               consumer 'lispm-desktop-test-pkg "1.0.0" '((lispm-desktop-test-dependency "0.6.0")))
              (package-vc-install `(,library :url ,url) "v0.5.0")
              (require library)
              (should (equal (funcall (intern (format "%s-test-version" library))) "0.5.0"))
              (lispm-desktop-test--pkg-catalog (concat "file://" consumer) "v1.0.0")
              (cl-letf (((symbol-function 'lispm-desktop-store-installer-program)
                         (lambda (task)
                           (let ((body (cdr (funcall program task))))
                             `(progn ,@(butlast body)
                                     (setq lispm-desktop-store-installer-libraries '((,library :url ,url)))
                                     ,(car (last body)))))))
                (lispm-desktop-store-install 'pkg t)
                (lispm-desktop-test--store-wait))
              (should (lispm-desktop-store-installed-p 'pkg))
              (should (package-installed-p library '(0 6 0)))
              (should (= (length (cdr (assq library package-alist))) 1))
              (should (equal (funcall (intern (format "%s-test-version" library))) "0.6.0")))
          (when (featurep library) (unload-feature library t))
          (delete-directory root t))))))

(ert-deftest lispm-desktop-test-store-dependency-adopts-source-checkouts ()
  "New packages replace loaded source checkouts, including lisp/ layouts."
  (skip-unless (executable-find "git"))
  (dolist (lisp-dir '(nil "lisp"))
    (lispm-desktop-test--with-store
      (lispm-desktop-test--with-packages
        (let* ((root (make-temp-file "lispm-desktop-test-source-adopt" t))
               (user-emacs-directory (file-name-as-directory (expand-file-name "profile" root)))
               (source (expand-file-name "dependency" root))
               (consumer (expand-file-name "consumer" root))
               (unrelated (expand-file-name "unrelated" root))
               (old (expand-file-name "old" root))
               (library 'lispm-desktop-test-source-dependency)
               (spec `(,library :url ,(concat "file://" source) ,@(when lisp-dir `(:lisp-dir ,lisp-dir))))
               (lispm-desktop-store-installer-libraries (list spec))
               (load-history load-history)
               (program (symbol-function 'lispm-desktop-store-installer-program)))
          (unwind-protect
              (progn
                (make-directory old t)
                (lispm-desktop-test--dependency-repository source library "0.5.0" nil lisp-dir)
                (copy-file (expand-file-name (format "%s.el" library) (if lisp-dir (expand-file-name lisp-dir source) source))
                           (expand-file-name (format "%s.el" library) old))
                (lispm-desktop-test--dependency-repository consumer 'lispm-desktop-test-pkg "0.1.0" nil lisp-dir)
                (copy-file (expand-file-name "lispm-desktop-test-pkg.el" (if lisp-dir (expand-file-name lisp-dir consumer) consumer))
                           (expand-file-name "lispm-desktop-test-pkg.el" old))
                (add-to-list 'load-path old)
                (require library)
                (require 'lispm-desktop-test-pkg)
                (should-not package-alist)
                (should (equal (funcall (intern (format "%s-test-version" library))) "0.5.0"))
                (should (equal (funcall (intern "lispm-desktop-test-pkg-test-version")) "0.1.0"))
                (lispm-desktop-test--dependency-repository
                 source library "0.6.0" nil lisp-dir
                 "(defun lispm-desktop-test-source-dependency-new-api () \"A newly added API.\" t)\n")
                (lispm-desktop-test--dependency-repository
                 consumer 'lispm-desktop-test-pkg "1.0.0" `((,library "0.6.0")) lisp-dir
                 "(require 'lispm-desktop-test-source-dependency)\n(unless (lispm-desktop-test-source-dependency-new-api) (error \"Dependency did not reload first\"))\n(defun lispm-desktop-test-pkg-dependency-ready () \"The dependency loaded first.\" t)\n")
                (lispm-desktop-test--dependency-repository unrelated 'lispm-desktop-test-unrelated "1.0.0")
                (lispm-desktop-test--store-catalog
                 "https://example.org/store"
                 `((pkg :kind app :name "Pkg" :command lispm-desktop-test-pkg-test-version
                        :package (lispm-desktop-test-pkg :url ,(concat "file://" consumer) :rev "v1.0.0"
                                                       ,@(when lisp-dir `(:lisp-dir ,lisp-dir))))))
                (cl-letf (((symbol-function 'lispm-desktop-store-installer-program)
                           (lambda (task)
                             (let ((body (cdr (funcall program task))))
                               `(progn ,@(butlast body)
                                       (setq lispm-desktop-store-installer-libraries ',(list spec))
                                       ;; Another package appearing in the same directory
                                       ;; must not be activated by this item's adoption.
                                       (setq package-alist nil package-vc-register-as-project nil
                                             package-native-compile nil native-comp-enable-subr-trampolines nil)
                                       (package-vc-install '(lispm-desktop-test-unrelated :url ,(concat "file://" unrelated)) "v1.0.0")
                                       ,(car (last body)))))))
                  (lispm-desktop-store-install 'pkg t)
                  (lispm-desktop-test--store-wait))
                (unless (lispm-desktop-store-installed-p 'pkg)
                  (ert-fail (mapcar (lambda (job) (with-current-buffer (lispm-desktop-store-job-log job) (buffer-string)))
                                    lispm-desktop-store--jobs)))
                (should (package-installed-p library '(0 6 0)))
                (should (equal (funcall (intern (format "%s-test-version" library))) "0.6.0"))
                (should (funcall (intern "lispm-desktop-test-source-dependency-new-api")))
                (should (equal (funcall (intern "lispm-desktop-test-pkg-test-version")) "1.0.0"))
                (should (funcall (intern "lispm-desktop-test-pkg-dependency-ready")))
                (should (assq 'lispm-desktop-test-unrelated package-alist))
                (should-not (memq 'lispm-desktop-test-unrelated package-activated-list))
                (should-not (featurep 'lispm-desktop-test-unrelated)))
            (dolist (feature (list 'lispm-desktop-test-pkg library))
              (when (featurep feature) (unload-feature feature t)))
            (delete-directory root t)))))))

(defvar lispm-desktop-test-entry-fail nil
  "Non-nil makes the temporary extension fail while it is activated.")

(defmacro lispm-desktop-test--with-old-entry-package (&rest body)
  "Run BODY with an old VC package and a catalog naming its new entry.
Bind SOURCE, DIRECTORY, OLD and NEW to the temporary repository, installed
package directory, original commit and catalog commit respectively."
  (declare (indent 0))
  `(lispm-desktop-test--with-store
     (lispm-desktop-test--with-packages
       (let* ((source (make-temp-file "lispm-desktop-test-entry-source" t))
              (directory (expand-file-name "lispm-desktop-test-pkg" package-user-dir))
              (user-emacs-directory (file-name-as-directory (expand-file-name "profile" source)))
              (old (lispm-desktop-test--dependency-repository
                    source 'lispm-desktop-test-pkg "0.1.0"))
              (new nil)
              (lispm-desktop-test-entry-fail nil))
         (unwind-protect
             (progn
               (package-vc-install
                (list 'lispm-desktop-test-pkg :url (concat "file://" source)) old)
               (require 'lispm-desktop-test-pkg)
               (should-not (locate-library "lispm-desktop-test-entry"))
               (lispm-desktop-test--dependency-repository
                source 'lispm-desktop-test-pkg "0.2.0")
               (with-temp-file (expand-file-name "lispm-desktop-test-entry.el" source)
                 (insert ";;; lispm-desktop-test-entry.el --- Fixture entry -*- lexical-binding: t; -*-\n"
                         "(when (bound-and-true-p lispm-desktop-test-entry-fail) (error \"Fixture activation failed\"))\n"
                         "(require 'lispm-desktop-test-pkg)\n"
                         "(provide 'lispm-desktop-test-entry)\n"))
               (lispm-desktop-test--git source "add" "lispm-desktop-test-entry.el")
               (lispm-desktop-test--git source "commit" "--quiet" "-m" "Add extension entry")
               (setq new (lispm-desktop-test--git source "rev-parse" "HEAD"))
               (lispm-desktop-test--store-catalog
                "https://example.org/store"
                (list (list 'pkg :kind 'extension :name "Pkg"
                            :feature 'lispm-desktop-test-entry
                            :package (list 'lispm-desktop-test-pkg
                                           :url (concat "file://" source) :rev new))))
               ,@body)
           (when (featurep 'lispm-desktop-test-entry)
             (unload-feature 'lispm-desktop-test-entry t))
           (delete-directory source t))))))

(ert-deftest lispm-desktop-test-store-adopts-package-with-new-entry ()
  "A missing entry is upgraded; later removal retains the pre-existing package."
  (skip-unless (executable-find "git"))
  (dolist (background '(nil t))
    (lispm-desktop-test--with-old-entry-package
      (let ((lispm-desktop-store-install-in-background background))
        (lispm-desktop-store-install 'pkg t)
        (lispm-desktop-test--store-wait)
        (should (lispm-desktop-store-installed-p 'pkg))
        (should (featurep 'lispm-desktop-test-entry))
        (should (equal (lispm-desktop-store--git directory "rev-parse" "HEAD") new))
        (should (equal (lispm-desktop-test-pkg-test-version) "0.2.0"))
        (should-not (plist-get (lispm-desktop-store--installed 'pkg) :owned))
        (lispm-desktop-store-uninstall 'pkg t)
        (should-not (featurep 'lispm-desktop-test-entry))
        (should (package-installed-p 'lispm-desktop-test-pkg '(0 2 0)))
        (should (file-directory-p directory))))))

(ert-deftest lispm-desktop-test-store-entry-failure-restores-old-package ()
  "Failed entry activation restores source, descriptor, bytecode and live code."
  (skip-unless (executable-find "git"))
  (dolist (background '(nil t))
    (lispm-desktop-test--with-old-entry-package
      (let ((lispm-desktop-store-install-in-background background)
            (lispm-desktop-test-entry-fail t))
        (lispm-desktop-store-install 'pkg t)
        (lispm-desktop-test--store-wait)
        (should-not (lispm-desktop-store-installed-p 'pkg))
        (should (eq (lispm-desktop-store-job-state (lispm-desktop-store--job 'pkg)) 'failed))
        (should (equal (lispm-desktop-store--git directory "rev-parse" "HEAD") old))
        (should (equal (lispm-desktop-test-pkg-test-version) "0.1.0"))
        (should (equal (package-desc-version (cadr (assq 'lispm-desktop-test-pkg package-alist))) '(0 1 0)))
        (should-not (featurep 'lispm-desktop-test-entry))
        (should-not (locate-library "lispm-desktop-test-entry")))
      ;; The failed job must not make the next Get adopt a leftover entry.
      (lispm-desktop-store-install 'pkg t)
      (lispm-desktop-test--store-wait)
      (should (lispm-desktop-store-installed-p 'pkg))
      (should (equal (lispm-desktop-test-pkg-test-version) "0.2.0")))))

(ert-deftest lispm-desktop-test-store-entry-repair-keeps-local-changes ()
  "An existing package with tracked edits is left untouched and unregistered."
  (skip-unless (executable-find "git"))
  (lispm-desktop-test--with-old-entry-package
    (let ((file (expand-file-name "lispm-desktop-test-pkg.el" directory)))
      (write-region "\n;; Local customization\n" nil file t 'silent)
      (let ((edited (with-temp-buffer (insert-file-contents file) (buffer-string))))
        (lispm-desktop-store-install 'pkg t)
        (lispm-desktop-test--store-wait)
        (should-not (lispm-desktop-store-installed-p 'pkg))
        (should (eq (lispm-desktop-store-job-state (lispm-desktop-store--job 'pkg)) 'failed))
        (should (equal (lispm-desktop-store--git directory "rev-parse" "HEAD") old))
        (should (equal edited (with-temp-buffer (insert-file-contents file) (buffer-string))))))))

(ert-deftest lispm-desktop-test-store-entry-rollback-failure-ends-job ()
  "Rollback errors leave a visible failed job and let unrelated work continue."
  (dolist (cause '(adoption child cancellation))
    (lispm-desktop-test--with-store
      (let* ((directory (make-temp-file "lispm-desktop-test-retained-package" t))
             (package-user-dir directory)
             (marker (expand-file-name "retained.el" directory))
             (job (lispm-desktop-store--make-job
                   :id 'pkg :action 'install :state 'running :repair "old"
                   :group (list 'repair) :process 'fixture
                   :item '(pkg :kind extension :name "Pkg"
                               :feature lispm-desktop-test-entry
                               :package (lispm-desktop-test-pkg :url "https://example.org/pkg" :rev "new"))
                   :installed (and (eq cause 'adoption) '("new"))
                   :stopped (eq cause 'cancellation)
                   :error (and (eq cause 'child) "Fixture child failed"))))
        (unwind-protect
            (progn
              (write-region ";; Existing package\n" nil marker nil 'silent)
              (setq lispm-desktop-store--jobs (list job))
              (lispm-desktop-store--queue
               '((next :kind extension :name "Next" :feature cl-lib)) 'install)
              (cl-letf (((symbol-function 'process-exit-status) (lambda (_) 0))
                        ((symbol-function 'lispm-desktop-store--adopt)
                         (lambda (_) (error "Fixture adoption failed")))
                        ((symbol-function 'lispm-desktop-store--undo)
                         (lambda (_) (error "Fixture rollback failed"))))
                (lispm-desktop-store--ended job))
              (should (eq (lispm-desktop-store-job-state job) 'failed))
              (should-not (lispm-desktop-store--busy-p 'pkg))
              (should (string-match-p "Fixture rollback failed"
                                      (lispm-desktop-store-job-error job)))
              (unless (eq cause 'cancellation)
                (should (string-match-p
                         (if (eq cause 'child) "Fixture child failed" "Fixture adoption failed")
                         (lispm-desktop-store-job-error job))))
              (should (lispm-desktop-store-installed-p 'next))
              (should (file-exists-p marker)))
          (delete-directory directory t))))))

(provide 'lispm-desktop-store-dependency-test)
;;; lispm-desktop-store-dependency-test.el ends here
