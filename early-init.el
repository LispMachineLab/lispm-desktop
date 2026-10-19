;;; early-init.el --- lispm-desktop, before the first frame  -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs runs this file when a checkout of lispm-desktop is its init
;; directory: linked from ~/.emacs.d, or named with emacs --init-directory.

;;; Code:

(load (expand-file-name "lisp/lispm-desktop-early" (file-name-directory load-file-name)) nil t)

;;; early-init.el ends here
