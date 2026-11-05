;;; init.el --- lispm-desktop  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs runs this file when a checkout of lispm-desktop is its init
;; directory: linked from ~/.emacs.d, or named with emacs --init-directory.
;; Your own settings go in personal.el beside it (`lispm-desktop-user-file'),
;; which git ignores.

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" (file-name-directory load-file-name)))

(setq custom-file (locate-user-emacs-file "custom.el")
      use-package-always-ensure t)
(when (file-exists-p custom-file)
  (load custom-file nil t))

(require 'lispm-desktop-installer-screen)
(let ((started nil)
      (failure nil))
  (unwind-protect
      (condition-case err
          (progn
            (lispm-desktop-installer-screen-start)
            (require 'lispm-desktop-deps)
            (lispm-desktop-deps-enable-fast-install)
            (lispm-desktop-installer-screen-watch-deps)
            (lispm-desktop-deps-prefetch-projects)
            (when (locate-library "lispm-kit-i18n")
              (lispm-desktop-installer-screen-complete 'lispm-kit))
            (require 'lispm-desktop)
            (lispm-desktop-installer-screen-complete 'desktop)
            (setq started t))
        (error
         (when (fboundp 'lispm-desktop-deps-disable-fast-install)
           (lispm-desktop-deps-disable-fast-install))
         (if (or noninteractive (daemonp))
             (signal (car err) (cdr err))
           (setq failure lispm-desktop-installer-screen--stage))))
    (lispm-desktop-installer-screen-stop))
  (if failure
      (lispm-desktop-installer-screen-fail failure)
    (when (and started
               (unwind-protect
                   (lispm-desktop-setup)
                 (when (fboundp 'lispm-desktop-deps-disable-fast-install)
                   (lispm-desktop-deps-disable-fast-install))))
      (when (file-exists-p lispm-desktop-user-file)
        (load lispm-desktop-user-file nil t))
      (require 'server)
      (unless (server-running-p)
        (server-start)))))

;;; init.el ends here
