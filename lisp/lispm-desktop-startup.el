;;; lispm-desktop-startup.el --- LispM opened at login, and its startup items  -*- lexical-binding: t; -*-

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

;; What starts with LispM, chosen in the Startup control panel, see
;; lispm-desktop-settings.el:
;;
;;   at login        the computer opens LispM when you log in, through a
;;                   launch agent on macOS (~/Library/LaunchAgents) or an
;;                   autostart entry on GNU/Linux (~/.config/autostart).
;;                   The file is the setting: LispM writes it when asked,
;;                   deletes it when asked, and otherwise only looks.
;;   startup items   what LispM opens once it has started, as Mac OS 9
;;                   opened the items of its Startup Items folder:
;;                   applications, documents and folders, programs and
;;                   Emacs commands, see `lispm-desktop-startup-items'.  A
;;                   program runs in the background as a service while
;;                   LispM runs, its output in a buffer of its own, and
;;                   stops as LispM quits.
;;
;; personal.el can set both:
;;
;;   (lispm-desktop-open-at-login t)
;;   (setq lispm-desktop-startup-items
;;         '((:program "ollama serve")
;;           (:app browser)
;;           (:file "~/notes/todo.org")
;;           (:command lispm-desktop-shell-new)))

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'xml)
(require 'lispm-desktop-i18n)

(defvar lispm-desktop-apps)
(declare-function lispm-desktop-apps-activate "lispm-desktop-apps" (app))
(declare-function lispm-desktop-editor-activate "lispm-desktop-editor" ())
(declare-function lispm-desktop-finder-open-directory "lispm-desktop-finder" (directory))

(defgroup lispm-desktop-startup nil
  "What starts with LispM: LispM itself at login, and its startup items."
  :group 'lispm-desktop)

(defcustom lispm-desktop-startup-items nil
  "What LispM opens once it has started, in this order, as Mac OS 9 opened
the items of its Startup Items folder.  Each item is a plist naming one
thing to open:

  (:app KEY)          an application, a key of `lispm-desktop-apps', as
                      `browser', or one the App Store installed
  (:file FILE)        a document, opened in the editor, or a folder,
                      opened in the Finder window
  (:program COMMAND)  a program: COMMAND run by the shell in the
                      background while LispM runs, as a service, in the
                      directory :directory, by default the home directory.
                      Its output goes to its own buffer, and it stops as
                      LispM quits.
  (:command COMMAND)  an Emacs command, called as if typed

:name names an item, by default after what it opens; :disabled, non-nil,
keeps an item in the list without opening it."
  :type '(repeat (plist :options ((:app symbol)
                                  (:file file)
                                  (:program string)
                                  (:command function)
                                  (:name string)
                                  (:directory directory)
                                  (:disabled boolean))))
  :group 'lispm-desktop-startup)

(defvar lispm-desktop-startup-changed-hook nil
  "Hook run when a startup item's program starts or stops, so that what
shows it, as the Startup control panel, can follow.")

;;;; At login

(defconst lispm-desktop-startup-login-label "org.lispmachinelab.lispm.login"
  "The label of the launch agent opening LispM at login on macOS.")

(defun lispm-desktop-startup-login-file ()
  "Return the file that has the computer open LispM at login: a launch
agent on macOS, an autostart entry on GNU/Linux and the BSDs, which the
desktops of X and Wayland read; nil on any other system."
  (pcase system-type
    ('darwin (expand-file-name (concat "Library/LaunchAgents/" lispm-desktop-startup-login-label ".plist")
                               "~"))
    ((or 'gnu/linux 'gnu/kfreebsd 'berkeley-unix)
     (expand-file-name "autostart/lispm.desktop"
                       (let ((config (getenv "XDG_CONFIG_HOME")))
                         (if (and config (file-name-absolute-p config)) config "~/.config"))))))

(defun lispm-desktop-startup-login-p ()
  "Return non-nil when the computer opens LispM at login."
  (when-let* ((file (lispm-desktop-startup-login-file)))
    (file-exists-p file)))

(defun lispm-desktop-startup--bundle ()
  "Return the application this Emacs runs in, as \"/Applications/LispM.app\",
or nil when it runs in none."
  (when (and (eq system-type 'darwin) invocation-directory)
    ;; The program is APP/Contents/MacOS/Emacs.
    (let ((contents (directory-file-name (file-name-directory (directory-file-name invocation-directory)))))
      (when (equal (file-name-nondirectory contents) "Contents")
        (let ((app (directory-file-name (file-name-directory contents))))
          (and (string-suffix-p ".app" app) app))))))

(defun lispm-desktop-startup--bundle-identifier (app)
  "Return the bundle identifier APP's Info.plist gives, or nil."
  (let ((plist (expand-file-name "Contents/Info.plist" app)))
    (when (file-readable-p plist)
      (with-temp-buffer
        (insert-file-contents plist)
        (when (re-search-forward "<key>CFBundleIdentifier</key>[ \t\n]*<string>\\([^<]+\\)</string>" nil t)
          (match-string 1))))))

(defun lispm-desktop-startup-login-command ()
  "Return the command that opens this LispM, as a list of strings: its
application through open(1), where it runs in one, else its Emacs; with
its init directory either way, so that the same configuration opens.
LispM.app's own program passes one too, the same, and the last counts."
  (let ((init (concat "--init-directory=" (expand-file-name user-emacs-directory)))
        (app (lispm-desktop-startup--bundle)))
    (if app
        (list "/usr/bin/open" "-a" app "--args" init)
      (list (expand-file-name invocation-name invocation-directory) init))))

(defun lispm-desktop-startup--launch-agent (command &optional identifier)
  "Return a launch agent running COMMAND, a list of strings, at login.
IDENTIFIER, the bundle identifier of the application it opens, has
macOS name that application in its Login Items."
  (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
          "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\""
          " \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n"
          "<plist version=\"1.0\">\n<dict>\n"
          "  <key>Label</key>\n"
          "  <string>" lispm-desktop-startup-login-label "</string>\n"
          "  <key>ProgramArguments</key>\n  <array>\n"
          (mapconcat (lambda (argument) (concat "    <string>" (xml-escape-string argument) "</string>\n"))
                     command)
          "  </array>\n"
          "  <key>RunAtLoad</key>\n  <true/>\n"
          "  <key>ProcessType</key>\n  <string>Interactive</string>\n"
          (if identifier
              (concat "  <key>AssociatedBundleIdentifiers</key>\n"
                      "  <string>" (xml-escape-string identifier) "</string>\n")
            "")
          "</dict>\n</plist>\n"))

(defun lispm-desktop-startup--exec-argument (argument)
  "Return ARGUMENT as an autostart entry's Exec key holds one: quoted
where it holds a character the desktop entry specification reserves, its
percent signs doubled, its backslashes escaped as the key's string."
  (let ((argument (string-replace "%" "%%" argument)))
    (string-replace
     "\\" "\\\\"
     (if (string-match-p "[] \t\n\"'\\\\><~|&;$*?#()`[]" argument)
         (concat "\"" (replace-regexp-in-string "[\"`$\\\\]" "\\\\\\&" argument) "\"")
       argument))))

(defun lispm-desktop-startup--autostart-entry (command)
  "Return an autostart entry running COMMAND, a list of strings, at login."
  (concat "[Desktop Entry]\n"
          "Type=Application\n"
          "Name=LispM\n"
          "Comment=Opens LispM at login, as its Startup control panel asks\n"
          "Exec=" (mapconcat #'lispm-desktop-startup--exec-argument command " ") "\n"
          "Terminal=false\n"
          "X-GNOME-Autostart-enabled=true\n"))

(defun lispm-desktop-startup--login-text ()
  "Return what the login file holds to open this LispM at login."
  (let ((command (lispm-desktop-startup-login-command)))
    (if (eq system-type 'darwin)
        (lispm-desktop-startup--launch-agent
         command (when-let* ((app (lispm-desktop-startup--bundle)))
                   (lispm-desktop-startup--bundle-identifier app)))
      (lispm-desktop-startup--autostart-entry command))))

(defun lispm-desktop-startup-login-description ()
  "Return what opens at login, as the Startup control panel tells it, in
the language LispM speaks: the application, or the Emacs, and its init
directory."
  (lispm-desktop-tr "%s with %s"
                    (abbreviate-file-name (or (lispm-desktop-startup--bundle)
                                              (expand-file-name invocation-name invocation-directory)))
                    (abbreviate-file-name (expand-file-name user-emacs-directory))))

(defun lispm-desktop-open-at-login (open)
  "Have the computer open LispM when you log in, when OPEN, else not.
Interactively, turn it on when it is off and off when it is on.  On
macOS it is a launch agent, which System Settings lists in its Login
Items; on GNU/Linux an autostart entry."
  (interactive (list (not (lispm-desktop-startup-login-p))))
  (let ((file (or (lispm-desktop-startup-login-file)
                  (user-error (lispm-desktop-tr "LispM cannot open at login on this system")))))
    (if open
        (let ((text (lispm-desktop-startup--login-text)))
          ;; Written only when it changed: macOS tells of every new one.
          (unless (and (file-readable-p file)
                       (equal text (with-temp-buffer
                                     (insert-file-contents file)
                                     (buffer-string))))
            (make-directory (file-name-directory file) t)
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region text nil file nil 'silent))))
      (when (file-exists-p file)
        (delete-file file)))
    (when (called-interactively-p 'interactive)
      (message (if open
                   (lispm-desktop-tr "LispM opens when you log in")
                 (lispm-desktop-tr "LispM no longer opens when you log in"))))
    open))

;;;; Startup items

(defun lispm-desktop-startup-item-kind (item)
  "Return what ITEM opens: `app', `file', `program' or `command', or nil
when it names none of them."
  (seq-find (lambda (kind) (plist-get item (intern (format ":%s" kind))))
            '(app file program command)))

(defun lispm-desktop-startup-program-name (command)
  "Return the name of the program the shell COMMAND runs, as \"ollama\"
for \"ollama serve\"."
  (let ((words (or (ignore-errors (split-string-and-unquote command))
                   (split-string command))))
    ;; Environment assignments before the program are passed over.
    (while (and (cdr words) (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*=" (car words)))
      (setq words (cdr words)))
    (if words (file-name-nondirectory (car words)) command)))

(defun lispm-desktop-startup-item-with (item key value)
  "Return a copy of ITEM, a startup item, with KEY set to VALUE, or left
out when VALUE is nil.  ITEM itself is left as it is."
  (let ((rest item) (copy nil))
    (while rest
      (unless (eq (car rest) key)
        (setq copy (append copy (list (car rest) (cadr rest)))))
      (setq rest (cddr rest)))
    (if value (append copy (list key value)) copy)))

(defun lispm-desktop-startup-item-name (item)
  "Return ITEM's name: its :name, else that of what it opens, an
application's in the language LispM speaks."
  (or (plist-get item :name)
      (pcase (lispm-desktop-startup-item-kind item)
        ('app (let ((app (plist-get item :app)))
                (if-let* ((name (plist-get (alist-get app (bound-and-true-p lispm-desktop-apps)) :name)))
                    (lispm-desktop-tr name)
                  (capitalize (symbol-name app)))))
        ('file (let* ((file (directory-file-name (plist-get item :file)))
                      (name (file-name-nondirectory file)))
                 (if (string-empty-p name) file name)))
        ('program (lispm-desktop-startup-program-name (plist-get item :program)))
        ('command (symbol-name (plist-get item :command))))
      (lispm-desktop-tr "Unknown")))

(defvar-local lispm-desktop-startup--exit-code nil
  "The exit code of the program whose output the buffer holds, once it
has stopped.")

(defun lispm-desktop-startup-program-buffer-name (item)
  "Return the name of the buffer holding the output of ITEM's program."
  (format "*Startup: %s*" (lispm-desktop-startup-item-name item)))

(defun lispm-desktop-startup-program-process (item)
  "Return the process of ITEM's program while it runs, else nil."
  (when-let* ((buffer (get-buffer (lispm-desktop-startup-program-buffer-name item)))
              (process (get-buffer-process buffer)))
    (and (process-live-p process) process)))

(defun lispm-desktop-startup-program-status (item)
  "Return how ITEM's program stands: `running', (exited . CODE), or nil
when it has not run."
  (if (lispm-desktop-startup-program-process item)
      'running
    (when-let* ((buffer (get-buffer (lispm-desktop-startup-program-buffer-name item))))
      (cons 'exited (buffer-local-value 'lispm-desktop-startup--exit-code buffer)))))

(defun lispm-desktop-startup--sentinel (process _event)
  "Note in PROCESS's buffer that its program stopped, and how."
  (unless (process-live-p process)
    (when-let* ((buffer (process-buffer process))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (setq lispm-desktop-startup--exit-code (process-exit-status process))
        (save-excursion
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert (propertize (format "%s stopped, exit code %d.\n"
                                      (format-time-string "%F %T")
                                      (process-exit-status process))
                              'face 'shadow)))))
    (run-hooks 'lispm-desktop-startup-changed-hook)))

(defun lispm-desktop-startup--start-program (item)
  "Run ITEM's program in the background, its output in its buffer; return
its process.  It runs by itself, quitting LispM stops it without asking,
and it reaches the network as LispM's own programs do."
  (let* ((command (plist-get item :program))
         (directory (file-name-as-directory (expand-file-name (or (plist-get item :directory) "~"))))
         (buffer (get-buffer-create (lispm-desktop-startup-program-buffer-name item))))
    (unless (file-directory-p directory)
      (error (lispm-desktop-tr "No directory %s") (abbreviate-file-name directory)))
    (with-current-buffer buffer
      (setq default-directory directory
            lispm-desktop-startup--exit-code nil)
      (goto-char (point-max))
      (unless (bobp) (insert "\n"))
      (insert (propertize (format "%s %s\n" (format-time-string "%F %T") command) 'face 'shadow)))
    (let* ((default-directory directory)
           (process (make-process :name (lispm-desktop-startup-item-name item)
                                  :buffer buffer
                                  :command (list shell-file-name shell-command-switch command)
                                  :connection-type 'pipe
                                  :noquery t
                                  :sentinel #'lispm-desktop-startup--sentinel)))
      (run-hooks 'lispm-desktop-startup-changed-hook)
      process)))

(defun lispm-desktop-startup--show-program (item)
  "Show the output of ITEM's program in the editor."
  (lispm-desktop-editor-activate)
  (pop-to-buffer-same-window (lispm-desktop-startup-program-buffer-name item)))

(defun lispm-desktop-startup-open-item (item &optional show)
  "Open ITEM, a startup item, see `lispm-desktop-startup-items'.
A program already running is left running; with SHOW, its output shows,
running or just started.  Signal an error when ITEM cannot be opened."
  (pcase (lispm-desktop-startup-item-kind item)
    ('app
     (let ((app (plist-get item :app)))
       (unless (assq app (bound-and-true-p lispm-desktop-apps))
         (error (lispm-desktop-tr "No application %s is installed") app))
       (lispm-desktop-apps-activate app)))
    ('file
     (let ((file (expand-file-name (plist-get item :file))))
       (cond ((file-directory-p file) (lispm-desktop-finder-open-directory file))
             ((file-exists-p file) (lispm-desktop-editor-activate) (find-file file))
             (t (error (lispm-desktop-tr "No file %s") (abbreviate-file-name file))))))
    ('program
     (unless (lispm-desktop-startup-program-process item)
       (lispm-desktop-startup--start-program item))
     (when show
       (lispm-desktop-startup--show-program item)))
    ('command
     (let ((command (plist-get item :command)))
       (unless (commandp command)
         (error (lispm-desktop-tr "No command %s") command))
       (call-interactively command)))
    (_ (error (lispm-desktop-tr "It names nothing to open: %S") item))))

(defun lispm-desktop-startup-open-items ()
  "Open the startup items not turned off, in their order, as at a start.
An item that cannot be opened is passed over, and told of."
  (interactive)
  (let ((failed nil))
    (dolist (item lispm-desktop-startup-items)
      (unless (plist-get item :disabled)
        (condition-case err
            (lispm-desktop-startup-open-item item)
          (error (push (lispm-desktop-tr "%s (%s)" (lispm-desktop-startup-item-name item)
                                         (error-message-string err))
                       failed)))))
    (when failed
      (message (lispm-desktop-tr "Startup items not opened: %s") (string-join (nreverse failed) "; ")))))

(defun lispm-desktop-startup--after-start ()
  "Open the startup items once Emacs is up and waits for input, so that
the desktop shows first.  This is a `window-setup-hook' function; a batch
Emacs opens none."
  (unless noninteractive
    (run-at-time 0 nil #'lispm-desktop-startup-open-items)))

(defun lispm-desktop-setup-startup ()
  "Open the startup items once LispM has started."
  (add-hook 'window-setup-hook #'lispm-desktop-startup--after-start 95))

(provide 'lispm-desktop-startup)
;;; lispm-desktop-startup.el ends here
