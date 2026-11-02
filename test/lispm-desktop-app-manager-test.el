;;; lispm-desktop-app-manager-test.el --- Apps manager tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: AGPL-3.0-or-later

(require 'ert)
(require 'cl-lib)
(require 'lispm-desktop-settings)
(setq lispm-desktop-language 'en)

(defmacro lispm-desktop-app-manager-test--inventory (&rest body)
  "Run BODY with a small installed inventory and no real packages."
  (declare (indent 0))
  `(let ((lispm-desktop-apps
          '((editor :name "Emacs" :activate ignore)
            (calc :name "Old Calc" :large-icon "applications-32" :activate ignore :listed-while-open t)
            (builtin :name "Built-in test" :folder "applications-32" :activate ignore)
            (transient :name "A temporary window")))
         (lispm-desktop-store-installed
          '((calc :name "Calculator" :kind app :installed "1.0" :package nil)
            (missing :name "Missing App" :kind app :installed "abcdef123456" :package (missing :url "https://example.com/app" :rev "abcdef123456"))
            (extension :name "Harness" :kind extension :installed "2.0" :package nil)
            (skill :name "Companion manual" :kind skill)))
         (lispm-desktop-store--catalogs nil)
         (lispm-desktop-store--jobs nil))
     ,@body))

(defun lispm-desktop-app-manager-test--fill ()
  "Draw the Apps panel here as a settings fill does, with its field dressed."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (erase-buffer)
    (lispm-desktop-app-manager-insert))
  (lispm-desktop-app-manager--after-fill))

(ert-deftest lispm-desktop-app-manager-test-inventory ()
  (lispm-desktop-app-manager-test--inventory
    (let ((items (lispm-desktop-app-manager-items)))
      (should (equal (mapcar #'car items) '(calc missing extension editor builtin)))
      (should (equal (plist-get (cdr (assq 'calc items)) :name) "Calculator"))
      (should (plist-get (cdr (assq 'missing items)) :managed))
      (should-not (plist-get (cdr (assq 'builtin items)) :managed))
      (should-not (assq 'skill items))
      (should-not (assq 'transient items)))))

(ert-deftest lispm-desktop-app-manager-test-search-and-filter ()
  (lispm-desktop-app-manager-test--inventory
    (with-temp-buffer
      (setq lispm-desktop-app-manager--search "MISSING app")
      (should (equal (mapcar #'car (lispm-desktop-app-manager--matches)) '(missing)))
      (setq lispm-desktop-app-manager--search "" lispm-desktop-app-manager--filter 'extension)
      (should (equal (mapcar #'car (lispm-desktop-app-manager--matches)) '(extension)))
      (setq lispm-desktop-app-manager--filter 'builtin)
      (should (equal (mapcar #'car (lispm-desktop-app-manager--matches)) '(editor builtin))))))

(ert-deftest lispm-desktop-app-manager-test-installed-info ()
  (lispm-desktop-app-manager-test--inventory
    (let ((info (lispm-desktop-store-installed-info 'missing)))
      (should (equal (plist-get info :version) "abcdef123456"))
      (should (equal (plist-get info :origin) "https://example.com/app"))
      (should-not (plist-get info :busy))
      (should-not (plist-get info :update-available)))
    (should-not (lispm-desktop-store-installed-info 'absent))
    (should (equal (mapcar #'car (lispm-desktop-store-installed-items)) '(calc missing extension)))))

(ert-deftest lispm-desktop-app-manager-test-independent-category ()
  (should (eq (lispm-desktop-settings-category 'apps) 'settings-apps))
  (should (equal (mapcar #'car (lispm-desktop-settings--panels 'settings-apps)) '(apps)))
  (should (assq 'settings-apps (lispm-desktop-settings--categories)))
  (let (opened)
    (cl-letf (((symbol-function 'lispm-desktop-settings-show) (lambda (panel) (setq opened panel))))
      (lispm-desktop-app-manager)
      (should (eq opened 'apps))))
  (let ((lispm-desktop-apps nil))
    (lispm-desktop-settings--define-apps)
    (should (plist-get (alist-get 'settings-apps lispm-desktop-apps) :listed-while-open))
    (should (plist-get (alist-get 'settings-apps lispm-desktop-apps) :activate))))

(ert-deftest lispm-desktop-app-manager-test-actions ()
  (lispm-desktop-app-manager-test--inventory
    (with-temp-buffer
      (let (calls)
        (cl-letf (((symbol-function 'lispm-desktop-app-manager-refresh) #'ignore)
                  ((symbol-function 'lispm-desktop-store-uninstall)
                   (lambda (id &optional noconfirm) (push (list 'uninstall id noconfirm) calls)))
                  ((symbol-function 'lispm-desktop-store-update)
                   (lambda (id &optional quietly) (push (list 'update id quietly) calls)))
                  ((symbol-function 'lispm-desktop-store-update-available-p) #'always)
                  ((symbol-function 'lispm-desktop-apps-activate) (lambda (id) (push (list 'open id) calls))))
          (setq lispm-desktop-app-manager--selected 'calc)
          (lispm-desktop-app-manager-action 'open)
          (lispm-desktop-app-manager-action 'update)
          (lispm-desktop-app-manager-action 'uninstall)
          (should (equal (reverse calls) '((open calc) (update calc nil) (uninstall calc nil))))
          (setq lispm-desktop-app-manager--selected 'builtin)
          (should-error (lispm-desktop-app-manager-action 'uninstall) :type 'user-error)
          (should-error (lispm-desktop-app-manager-action 'update) :type 'user-error)
          (setq lispm-desktop-app-manager--selected 'extension)
          (should-error (lispm-desktop-app-manager-action 'open) :type 'user-error)
          (cl-letf (((symbol-function 'lispm-desktop-store--busy-p) #'always))
            (should-error (lispm-desktop-app-manager-action 'uninstall) :type 'user-error)
            (should-error (lispm-desktop-app-manager-action 'update) :type 'user-error))
          (setq lispm-desktop-app-manager--selected 'absent)
          (should-error (lispm-desktop-app-manager-action 'uninstall) :type 'user-error))))))

(ert-deftest lispm-desktop-app-manager-test-page-size-follows-window ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'window-body-height) (lambda (_) 26)))
      (should (= (lispm-desktop-app-manager--page-size) 13))
      ;; The field is part of the panel's first rows, not a row of its own,
      ;; so a search never takes room from the list.
      (setq lispm-desktop-app-manager--search "calculator")
      (should (= (lispm-desktop-app-manager--page-size) 13))
      (setq lispm-desktop-app-manager--search ""))
    ;; A shorter window keeps at least the minimum, a tall one no more.
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'window-body-height) (lambda (_) 8)))
      (should (= (lispm-desktop-app-manager--page-size)
                 lispm-desktop-app-manager--page-minimum)))
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'window))
              ((symbol-function 'window-body-height) (lambda (_) 200)))
      (should (= (lispm-desktop-app-manager--page-size)
                 lispm-desktop-app-manager--page-maximum)))
    ;; Without a window, the inventory still pages by a full panel's worth.
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil)))
      (should (= (lispm-desktop-app-manager--page-size) 11)))))

(ert-deftest lispm-desktop-app-manager-test-render-and-selection ()
  (lispm-desktop-app-manager-test--inventory
    (let ((name (lispm-desktop-settings--buffer-name 'settings-apps)))
      (unwind-protect
          (progn
            (lispm-desktop-settings-select 'apps)
            (with-current-buffer name
              (should (equal (lispm-desktop-platinum-title-text) "Apps"))
              (setq lispm-desktop-app-manager--selected 'missing)
              (lispm-desktop-app-manager-refresh)
              (should (eq lispm-desktop-app-manager--selected 'missing))
              (should (string-search "abcdef123456" (buffer-string)))
              (should (string-search "https://example.com/app" (buffer-string)))
              (lispm-desktop-app-manager-search "no-such-item")
              (should-not lispm-desktop-app-manager--selected)
              (should (string-search "No matching applications" (buffer-string)))
              (lispm-desktop-app-manager-search "")
              (should (eq lispm-desktop-app-manager--selected 'calc))
              (setq lispm-desktop-app-manager--selected 'missing
                    lispm-desktop-app-manager--page 99)
              (lispm-desktop-app-manager-refresh)
              (should (= lispm-desktop-app-manager--page 0))))
        (when (get-buffer name) (kill-buffer name))))))

(ert-deftest lispm-desktop-app-manager-test-search-field-and-pagination ()
  (lispm-desktop-app-manager-test--inventory
    (with-temp-buffer
      (lispm-desktop-settings-mode)
      (setq lispm-desktop-settings--category 'settings-apps)
      (cl-letf (((symbol-function 'lispm-desktop-app-manager--page-size) (lambda () 2)))
        (let ((inhibit-read-only t)
              choices choose-action refreshed)
          (cl-letf (((symbol-function 'lispm-desktop-app-manager-refresh)
                     (lambda (&rest _) (setq refreshed t)))
                    ((symbol-function 'run-with-timer)
                     (lambda (_delay _repeat function) (funcall function)))
                    ((symbol-function 'lispm-ui-insert-popup-button)
                     (lambda (_label options action &rest _)
                       (setq choices options choose-action action))))
            (lispm-desktop-app-manager-test--fill)
            ;; The field is always there, empty, with the filters beside it:
            ;; no Search button opens a minibuffer to take the search.
            (should (equal (buffer-substring-no-properties
                            lispm-desktop-app-manager--field-start
                            lispm-desktop-app-manager--field-end)
                           ""))
            (should (equal (mapcar #'car choices) '(all app extension builtin)))
            (should-not (string-search "Search…" (buffer-string)))
            ;; What is typed into the field is the search, at once.
            (goto-char lispm-desktop-app-manager--field-end)
            (insert "Harness")
            (should refreshed)
            (should (equal lispm-desktop-app-manager--search "Harness"))
            (should (equal (buffer-substring-no-properties
                            lispm-desktop-app-manager--field-start
                            lispm-desktop-app-manager--field-end)
                           "Harness"))
            (setq refreshed nil)
            (funcall choose-action 'extension)
            (should refreshed)
            (should (eq lispm-desktop-app-manager--filter 'extension))
            (should (equal (mapcar #'car (lispm-desktop-app-manager--matches))
                           '(extension)))
            (setq lispm-desktop-app-manager--search "" lispm-desktop-app-manager--filter 'all)
            (lispm-desktop-app-manager--turn 1)
            (lispm-desktop-app-manager-test--fill)
            (should (eq lispm-desktop-app-manager--selected 'extension))))
        ;; Removing the selected item keeps the list usable on its page.
        (setq lispm-desktop-store-installed (assq-delete-all 'extension lispm-desktop-store-installed))
        (lispm-desktop-app-manager-test--fill)
        (should (eq lispm-desktop-app-manager--selected 'editor))))))

(ert-deftest lispm-desktop-app-manager-test-refresh-does-not-reopen ()
  (let ((name (lispm-desktop-settings--buffer-name 'settings-apps)))
    (when (get-buffer name) (kill-buffer name))
    (lispm-desktop-app-manager-refresh)
    (run-hooks 'lispm-desktop-store-changed-hook)
    (should-not (get-buffer name))))

(ert-deftest lispm-desktop-app-manager-test-store-notifications ()
  (let ((count 0)
        (lispm-desktop-store-changed-hook nil))
    (add-hook 'lispm-desktop-store-changed-hook (lambda () (cl-incf count)))
    (cl-letf (((symbol-function 'lispm-desktop-finder-folder-changed) #'ignore))
      (lispm-desktop-store--applications-changed)
      (let ((lispm-desktop-store-buffer-name " *nonexistent-store-test*"))
        (lispm-desktop-store--render)))
    (should (= count 2))))

(provide 'lispm-desktop-app-manager-test)
;;; lispm-desktop-app-manager-test.el ends here
