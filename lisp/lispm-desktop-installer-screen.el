;;; lispm-desktop-installer-screen.el --- Early installation display  -*- lexical-binding: t; -*-

;;; Commentary:

;; The desktop and its drawing libraries are not available yet.  This screen
;; uses only Emacs, including on a terminal.  Its bar is indeterminate: the
;; complete dependency graph is not known before packages are installed.

;;; Code:

(require 'cl-lib)
(require 'face-remap)
(require 'package)
(require 'subr-x)
(require 'svg)

(defconst lispm-desktop-installer-screen--logo
  (expand-file-name "../icons/lispm-lambda.png"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The bundled LispM logo, available before packages are installed.")

(defvar lispm-desktop-installer-screen--active nil)
(defvar lispm-desktop-installer-screen--window nil)
(defvar lispm-desktop-installer-screen--previous nil)
(defvar lispm-desktop-installer-screen--completed 0)
(defvar lispm-desktop-installer-screen--stage-packages 0)
(defvar lispm-desktop-installer-screen--last nil)
(defconst lispm-desktop-installer-screen--buffer " *LispM installation*")
(defvar lispm-desktop-installer-screen--large-logo nil)
(defconst lispm-desktop-installer-screen--stages '(lispm-kit lispm-ui lispm-browser lispm lispm-auth desktop))
(defvar lispm-desktop-installer-screen--finished nil)
(defvar lispm-desktop-installer-screen--status "Preparing installation...")
(defvar lispm-desktop-installer-screen--message nil)
(defvar lispm-desktop-installer-screen--reporting nil)
(defvar lispm-desktop-installer-screen--failure nil)
(defvar lispm-desktop-installer-screen--stage "Preparing installation")
(defvar lispm-desktop-installer-screen-language nil
  "Language tag for the early installer screen, or nil to detect in GUI/TTY.")
(defvar lispm-desktop-installer-screen--detected-language '(unread))

(defconst lispm-desktop-installer-screen--translations
  '((zh-Hans
     ("LispM Installation" . "LispM 安装")
     ("LISPM  /  SYSTEM INSTALLATION" . "LISPM  /  系统安装")
     ("Preparing your LispM system" . "正在准备您的 LispM 系统")
     ("Installing the components needed to get started." . "正在安装启动所需的组件。")
     ("Startup stages: %d / %d" . "启动阶段：%d / %d")
     ("Working — this may take a few minutes." . "正在处理 — 首次启动可能需要几分钟。")
     ("This may take a few minutes." . "首次启动可能需要几分钟。")
     ("Packages installed: %d" . "已安装软件包：%d")
     ("Last: %s" . "最近完成：%s")
     ("Preparing installation..." . "正在准备安装…")
     ("Refreshing software sources..." . "正在刷新软件源…")
     ("Preparing: %s" . "正在准备：%s")
     ("Completed: %s" . "已完成：%s")
     ("Installing package: %s" . "正在安装软件包：%s")
     ("Installing project: %s" . "正在安装组件：%s")
     ("Updating project: %s" . "正在更新组件：%s")
     ("Downloading (%d%%)…" . "正在下载（%d%%）…")
     ("Compiling (%d%%)…" . "正在编译（%d%%）…")
     ("INSTALLATION STOPPED" . "INSTALLATION STOPPED / 安装已中止")
     ("Stage: %s" . "阶段：%s")
     ("The installation could not finish." . "安装未能完成。")
     ("Check network access and available disk space." . "请检查网络连接与可用磁盘空间。")
     ("Correct the problem, then restart Emacs." . "解决问题后请重新启动 Emacs。")
     ("No automatic retry or cleanup was performed." . "未执行自动重试或清理。")
     ("Press q to dismiss this page." . "按 q 关闭此页面。"))
    (zh-Hant
     ("LispM Installation" . "LispM 安裝")
     ("LISPM  /  SYSTEM INSTALLATION" . "LISPM  /  系統安裝")
     ("Preparing your LispM system" . "正在準備您的 LispM 系統")
     ("Installing the components needed to get started." . "正在安裝啟動所需的組件。")
     ("Startup stages: %d / %d" . "啟動階段：%d / %d")
     ("Working — this may take a few minutes." . "正在處理 — 首次啟動可能需要幾分鐘。")
     ("This may take a few minutes." . "首次啟動可能需要幾分鐘。")
     ("Packages installed: %d" . "已安裝套件：%d")
     ("Last: %s" . "最近完成：%s")
     ("Preparing installation..." . "正在準備安裝…")
     ("Refreshing software sources..." . "正在重新整理軟體來源…")
     ("Preparing: %s" . "正在準備：%s")
     ("Completed: %s" . "已完成：%s")
     ("Installing package: %s" . "正在安裝套件：%s")
     ("Installing project: %s" . "正在安裝組件：%s")
     ("Updating project: %s" . "正在更新組件：%s")
     ("Downloading (%d%%)…" . "正在下載（%d%%）…")
     ("Compiling (%d%%)…" . "正在編譯（%d%%）…")
     ("INSTALLATION STOPPED" . "INSTALLATION STOPPED / 安裝已中止")
     ("Stage: %s" . "階段：%s")
     ("The installation could not finish." . "安裝未能完成。")
     ("Check network access and available disk space." . "請檢查網路連線與可用磁碟空間。")
     ("Correct the problem, then restart Emacs." . "解決問題後請重新啟動 Emacs。")
     ("No automatic retry or cleanup was performed." . "未執行自動重試或清理。")
     ("Press q to dismiss this page." . "按 q 關閉此頁面。"))
    (ja
     ("LispM Installation" . "LispM インストール")
     ("LISPM  /  SYSTEM INSTALLATION" . "LISPM  /  システムインストール")
     ("Preparing your LispM system" . "LispM システムを準備しています")
     ("Installing the components needed to get started." . "起動に必要なコンポーネントをインストールしています。")
     ("Startup stages: %d / %d" . "起動ステージ: %d / %d")
     ("Working — this may take a few minutes." . "処理中 — 初回は数分かかる場合があります。")
     ("This may take a few minutes." . "初回は数分かかる場合があります。")
     ("Packages installed: %d" . "インストール済みパッケージ: %d")
     ("Last: %s" . "直近: %s")
     ("Preparing installation..." . "インストールを準備中…")
     ("Refreshing software sources..." . "パッケージ一覧を更新中…")
     ("Preparing: %s" . "準備中: %s")
     ("Completed: %s" . "完了: %s")
     ("Installing package: %s" . "パッケージをインストール中: %s")
     ("Installing project: %s" . "プロジェクトをインストール中: %s")
     ("Updating project: %s" . "プロジェクトを更新中: %s")
     ("Downloading (%d%%)…" . "ダウンロード中 (%d%%)…")
     ("Compiling (%d%%)…" . "コンパイル中 (%d%%)…"))
    (fr
     ("LispM Installation" . "Installation de LispM")
     ("Preparing your LispM system" . "Préparation de votre système LispM")
     ("Installing the components needed to get started." . "Installation des composants nécessaires au démarrage.")
     ("Startup stages: %d / %d" . "Étapes de démarrage : %d / %d")
     ("Working — this may take a few minutes." . "En cours — cela peut prendre quelques minutes.")
     ("This may take a few minutes." . "Cela peut prendre quelques minutes.")
     ("Packages installed: %d" . "Paquets installés : %d")
     ("Last: %s" . "Dernier : %s")
     ("Preparing installation..." . "Préparation de l'installation…")
     ("Refreshing software sources..." . "Actualisation des sources logicielles…")
     ("Preparing: %s" . "Préparation : %s")
     ("Completed: %s" . "Terminé : %s")
     ("Installing package: %s" . "Installation du paquet : %s")
     ("Installing project: %s" . "Installation du projet : %s")
     ("Updating project: %s" . "Mise à jour du projet : %s")
     ("Downloading (%d%%)…" . "Téléchargement (%d%%)…")
     ("Compiling (%d%%)…" . "Compilation (%d%%)…"))
    (de
     ("LispM Installation" . "LispM-Installation")
     ("Preparing your LispM system" . "Ihr LispM-System wird vorbereitet")
     ("Installing the components needed to get started." . "Die für den Start benötigten Komponenten werden installiert.")
     ("Startup stages: %d / %d" . "Startphasen: %d / %d")
     ("Working — this may take a few minutes." . "Bitte warten — dies kann einige Minuten dauern.")
     ("This may take a few minutes." . "Dies kann einige Minuten dauern.")
     ("Packages installed: %d" . "Installierte Pakete: %d")
     ("Last: %s" . "Zuletzt: %s")
     ("Preparing installation..." . "Installation wird vorbereitet …")
     ("Refreshing software sources..." . "Paketquellen werden aktualisiert …")
     ("Preparing: %s" . "Vorbereiten: %s")
     ("Completed: %s" . "Abgeschlossen: %s")
     ("Installing package: %s" . "Paket wird installiert: %s")
     ("Installing project: %s" . "Projekt wird installiert: %s")
     ("Updating project: %s" . "Projekt wird aktualisiert: %s")
     ("Downloading (%d%%)…" . "Wird heruntergeladen (%d%%) …")
     ("Compiling (%d%%)…" . "Wird kompiliert (%d%%) …"))))

(defun lispm-desktop-installer-screen--parse-locale (locale)
  "Return a supported language symbol for LOCALE string, or nil."
  (when (and (stringp locale) (not (string-empty-p locale)))
    (let ((norm (downcase (replace-regexp-in-string "[_@.]" "-" locale))))
      (cond ((or (string-prefix-p "zh-hant" norm)
                 (string-match-p "\\`zh-\\(tw\\|hk\\|mo\\)" norm))
             'zh-Hant)
            ((string-prefix-p "zh" norm) 'zh-Hans)
            ((string-prefix-p "ja" norm) 'ja)
            ((string-prefix-p "fr" norm) 'fr)
            ((string-prefix-p "de" norm) 'de)
            ((string-prefix-p "en" norm) 'en)))))

(defun lispm-desktop-installer-screen--system-language ()
  "Return the detected language symbol for the installer screen."
  (cond
   (lispm-desktop-installer-screen-language
    (if (symbolp lispm-desktop-installer-screen-language)
        lispm-desktop-installer-screen-language
      (intern (format "%s" lispm-desktop-installer-screen-language))))
   ((and (boundp 'lispm-desktop-language) lispm-desktop-language)
    (if (symbolp lispm-desktop-language)
        lispm-desktop-language
      (intern (format "%s" lispm-desktop-language))))
   (noninteractive 'en)
   ((not (equal lispm-desktop-installer-screen--detected-language '(unread)))
    lispm-desktop-installer-screen--detected-language)
   (t
    (setq lispm-desktop-installer-screen--detected-language
          (or (and (fboundp 'lispm-desktop-current-language)
                   (ignore-errors (lispm-desktop-current-language)))
              (when (eq system-type 'darwin)
                (ignore-errors
                  (with-temp-buffer
                    (when (zerop (call-process "defaults" nil t nil "read" "-g" "AppleLanguages"))
                      (goto-char (point-min))
                      (let (langs)
                        (while (re-search-forward "\"?\\([A-Za-z]+\\(?:[-_][A-Za-z0-9]+\\)*\\)\"?[,\n]" nil t)
                          (push (match-string 1) langs))
                        (seq-some #'lispm-desktop-installer-screen--parse-locale (nreverse langs)))))))
              (seq-some #'lispm-desktop-installer-screen--parse-locale
                        (list (getenv "LC_ALL") (getenv "LC_MESSAGES") (getenv "LANG")))
              'en)))))

(defun lispm-desktop-installer-screen--tr (message &rest args)
  "Translate MESSAGE with ARGS for the early installer screen."
  (let* ((lang (lispm-desktop-installer-screen--system-language))
         (table (alist-get lang lispm-desktop-installer-screen--translations))
         (template (or (cdr (assoc message table)) message)))
    (if args
        (apply #'format template args)
      template)))

(defun lispm-desktop-installer-screen--localize-status (status)
  "Translate a status line STATUS if it matches a known pattern."
  (if (not (stringp status))
      ""
    (cond
     ((assoc status (alist-get (lispm-desktop-installer-screen--system-language)
                               lispm-desktop-installer-screen--translations))
      (lispm-desktop-installer-screen--tr status))
     ((string-match "\\`\\(Preparing\\|Completed\\|Installing package\\|Installing project\\|Updating project\\): \\(.*\\)\\'" status)
      (lispm-desktop-installer-screen--tr (concat (match-string 1 status) ": %s")
                                          (match-string 2 status)))
     ((string-match "\\`Downloading (\\([0-9]+\\)%)…\\'" status)
      (lispm-desktop-installer-screen--tr "Downloading (%d%%)…"
                                          (string-to-number (match-string 1 status))))
     ((string-match "\\`Compiling (\\([0-9]+\\)%)…\\'" status)
      (lispm-desktop-installer-screen--tr "Compiling (%d%%)…"
                                          (string-to-number (match-string 1 status))))
     (t status))))

(defun lispm-desktop-installer-screen--failure-lines (stage)
  "Return safe, actionable failure text for STAGE."
  (list (lispm-desktop-installer-screen--tr "INSTALLATION STOPPED")
        (lispm-desktop-installer-screen--tr "Stage: %s" stage)
        (lispm-desktop-installer-screen--tr "The installation could not finish.")
        (lispm-desktop-installer-screen--tr "Check network access and available disk space.")
        (lispm-desktop-installer-screen--tr "Correct the problem, then restart Emacs.")
        (lispm-desktop-installer-screen--tr "No automatic retry or cleanup was performed.")
        (lispm-desktop-installer-screen--tr "Press q to dismiss this page.")))

(defun lispm-desktop-installer-screen-fail (stage)
  "Display a persistent, sanitized failure page for STAGE."
  (setq lispm-desktop-installer-screen--failure t
        lispm-desktop-installer-screen--stage stage
        lispm-desktop-installer-screen--active t)
  (lispm-desktop-installer-screen-report stage)
  (when-let* ((buffer (get-buffer lispm-desktop-installer-screen--buffer)))
    (with-current-buffer buffer
      (use-local-map (let ((map (make-sparse-keymap)))
                       (define-key map (kbd "q") #'lispm-desktop-installer-screen-stop)
                       map)))))

(defun lispm-desktop-installer-screen--progress ()
  "Return the number of completed startup stages."
  (length lispm-desktop-installer-screen--finished))

(defun lispm-desktop-installer-screen--progress-fraction ()
  "Return smooth stage progress between 0.0 and total stages."
  (let ((total (length lispm-desktop-installer-screen--stages))
        (done (lispm-desktop-installer-screen--progress))
        (sub (min 0.85 (* 0.12 lispm-desktop-installer-screen--stage-packages))))
    (min (float total) (+ (float done) (if (< done total) sub 0.0)))))

(defun lispm-desktop-installer-screen-complete (stage)
  "Record successful completion of startup STAGE once."
  (when (and lispm-desktop-installer-screen--active
             (memq stage lispm-desktop-installer-screen--stages)
             (not (memq stage lispm-desktop-installer-screen--finished)))
    (push stage lispm-desktop-installer-screen--finished)
    (setq lispm-desktop-installer-screen--stage-packages 0)
    (lispm-desktop-installer-screen-report (format "Completed: %s" stage))))

(defun lispm-desktop-installer-screen--ensure (original feature &rest args)
  "Run ORIGINAL for FEATURE and ARGS, recording successful startup stages."
  (when (memq feature lispm-desktop-installer-screen--stages)
    (setq lispm-desktop-installer-screen--stage-packages 0)
    (lispm-desktop-installer-screen-report (format "Preparing: %s" feature)))
  (prog1 (apply original feature args)
    (lispm-desktop-installer-screen-complete feature)))

(defun lispm-desktop-installer-screen--capture-message (original format-string &rest args)
  "Show messages from ORIGINAL with FORMAT-STRING and ARGS in the installer."
  (if (or (not lispm-desktop-installer-screen--active)
          inhibit-message
          lispm-desktop-installer-screen--reporting
          (not (stringp format-string)))
      (apply original format-string args)
    (let ((inhibit-message t)
          (lispm-desktop-installer-screen--reporting t))
      (prog1 (apply original format-string args)
        (setq lispm-desktop-installer-screen--message
              (condition-case nil (apply #'format-message format-string args) (error nil)))
        (when (and lispm-desktop-installer-screen--message
                   (not (string-empty-p lispm-desktop-installer-screen--message)))
          (lispm-desktop-installer-screen-report lispm-desktop-installer-screen--status))))))

(defun lispm-desktop-installer-screen--svg-available-p ()
  "Return non-nil when this frame can display SVG images."
  (and (display-graphic-p) (image-type-available-p 'svg)
       (file-readable-p lispm-desktop-installer-screen--logo)))

(defun lispm-desktop-installer-screen--draw (width height status)
  "Draw an early installation page WIDTH by HEIGHT, reporting STATUS."
  (let* ((svg (svg-create width height))
         (w (min 620 (max 1 (- width 32))))
         (h (min 400 (max 1 (- height 32))))
         (x (/ (- width w) 2))
         (y (/ (- height h) 2))
         (logo-size (max 1 (min 112 (/ w 4) (/ h 3))))
         (center (+ x (/ w 2)))
         (bar-w (max 1 (- w 80)))
         (display-status (lispm-desktop-installer-screen--localize-status status))
         (display-message (lispm-desktop-installer-screen--localize-status
                           (or lispm-desktop-installer-screen--message ""))))
    (svg-rectangle svg 0 0 width height :fill "#111318")
    (svg-rectangle svg (+ x 5) (+ y 5) w h :fill "#000000")
    (svg-rectangle svg x y w h :fill "#dedede" :stroke "#000000" :stroke-width 2)
    (svg-rectangle svg (+ x 2) (+ y 2) (- w 4) 27 :fill "#c7c7c7")
    (dotimes (i 4)
      (svg-line svg (+ x 10) (+ y 7 (* i 4)) (+ x (- w 10)) (+ y 7 (* i 4))
                :stroke "#888888" :stroke-width 1))
    (svg-rectangle svg (- center 76) (+ y 4) 152 20 :fill "#c7c7c7")
    (svg-text svg (lispm-desktop-installer-screen--tr "LispM Installation")
              :x center :y (+ y 19) :text-anchor "middle"
              :font-family "sans-serif" :font-size 14 :font-weight "bold" :fill "#111111")
    (svg-embed svg lispm-desktop-installer-screen--logo "image/png" nil
               :x (- center (/ logo-size 2)) :y (+ y 45) :width logo-size :height logo-size)
    (svg-text svg (lispm-desktop-installer-screen--tr "Preparing your LispM system")
              :x center :y (+ y 185)
              :text-anchor "middle" :font-family "sans-serif" :font-size 20
              :font-weight "bold" :fill "#111111")
    (svg-text svg (lispm-desktop-installer-screen--tr "Installing the components needed to get started.")
              :x center :y (+ y 211)
              :text-anchor "middle" :font-family "sans-serif" :font-size 13 :fill "#444444")
    (svg-rectangle svg (+ x 40) (+ y 238) bar-w 22 :fill "#ffffff" :stroke "#222222" :stroke-width 2)
    (let ((filled (round (/ (* (max 0 (- bar-w 8))
                               (lispm-desktop-installer-screen--progress-fraction))
                            (float (length lispm-desktop-installer-screen--stages))))))
      (when (> filled 0)
        (svg-rectangle svg (+ x 44) (+ y 242) filled 14 :fill "#306ba4")))
    (svg-text svg (lispm-desktop-installer-screen--tr
                   "Startup stages: %d / %d"
                   (lispm-desktop-installer-screen--progress)
                   (length lispm-desktop-installer-screen--stages))
              :x (+ x 40) :y (+ y 277) :font-family "sans-serif" :font-size 12 :fill "#444444")
    (svg-text svg (truncate-string-to-width display-status (max 8 (/ (- w 60) 8)) nil nil t)
              :x (+ x 40) :y (+ y 300) :font-family "sans-serif" :font-size 15 :fill "#111111")
    (svg-text svg (truncate-string-to-width display-message
                                           (max 8 (/ (- w 60) 7)) nil nil t)
              :x (+ x 40) :y (+ y 324) :font-family "sans-serif" :font-size 12 :fill "#555555")
    (svg-text svg (lispm-desktop-installer-screen--tr "Working — this may take a few minutes.")
              :x (+ x 40) :y (+ y 347)
              :font-family "sans-serif" :font-size 12 :fill "#555555")
    svg))

(defun lispm-desktop-installer-screen--line (text width)
  "Return TEXT fitted inside a bordered line of WIDTH columns."
  (when (and lispm-desktop-installer-screen--large-logo
             (text-property-not-all 0 (length text) 'face nil text))
    (setq text (apply #'concat
                      (mapcar (lambda (index)
                                (let ((character (substring text index (1+ index))))
                                  (concat character character)))
                              (number-sequence 0 (1- (length text)))))))
  (concat "| " (truncate-string-to-width text (- width 4) nil ?\s t) " |"))

(defun lispm-desktop-installer-screen-report (status)
  "Display STATUS before a potentially slow installation operation."
  (when lispm-desktop-installer-screen--active
    (setq lispm-desktop-installer-screen--status status)
    (unless lispm-desktop-installer-screen--failure
      (setq lispm-desktop-installer-screen--stage status)))
  (when (and lispm-desktop-installer-screen--active
             (or lispm-desktop-installer-screen--failure
                 (not (and (display-graphic-p)
                           (not (frame-visible-p (selected-frame)))))))
    (let* ((window (selected-window))
           (width (max 20 (min 76 (- (window-body-width window) 2))))
           (lispm-desktop-installer-screen--large-logo (>= width 70))
           (margin (make-string (max 0 (/ (- (window-body-width window) width) 2)) ?\s))
           (top (make-string (max 0 (/ (- (window-body-height window) 22) 2)) ?\n))
           (border (concat "+" (make-string (- width 2) ?-) "+"))
           (display-status (lispm-desktop-installer-screen--localize-status status))
           (display-message (lispm-desktop-installer-screen--localize-status
                             (or lispm-desktop-installer-screen--message "")))
           (lines (list border
                        (lispm-desktop-installer-screen--line
                         (lispm-desktop-installer-screen--tr "LISPM  /  SYSTEM INSTALLATION") width)
                        border
                        (lispm-desktop-installer-screen--line "" width)
                        (lispm-desktop-installer-screen--line (propertize "        #########" 'face '(:foreground "#f35a55" :weight bold)) width)
                        (lispm-desktop-installer-screen--line (propertize "        ###   ####" 'face '(:foreground "#f66b4b" :weight bold)) width)
                        (lispm-desktop-installer-screen--line (propertize "               ####" 'face '(:foreground "#fa7f36" :weight bold)) width)
                        (lispm-desktop-installer-screen--line (propertize "               ####" 'face '(:foreground "#ff9221" :weight bold)) width)
                        (lispm-desktop-installer-screen--line (propertize "              ######" 'face '(:foreground "#f6a52b" :weight bold)) width)
                        (lispm-desktop-installer-screen--line (propertize "            #### ####" 'face '(:foreground "#c4bb45" :weight bold)) width)
                        (lispm-desktop-installer-screen--line (propertize "          ####    ####" 'face '(:foreground "#73b98a" :weight bold)) width)
                        (lispm-desktop-installer-screen--line (propertize "        ####       ####" 'face '(:foreground "#15b9d6" :weight bold)) width)
                        (lispm-desktop-installer-screen--line (propertize "      ######        ###########" 'face '(:foreground "#00bafa" :weight bold)) width)
                        (lispm-desktop-installer-screen--line "" width)
                        (lispm-desktop-installer-screen--line
                         (lispm-desktop-installer-screen--tr "Preparing your LispM system") width)
                        (lispm-desktop-installer-screen--line "" width)
                        (lispm-desktop-installer-screen--line
                         (let* ((size (max 5 (min 30 (- width 12))))
                                (filled (round (/ (* size (lispm-desktop-installer-screen--progress-fraction))
                                                  (float (length lispm-desktop-installer-screen--stages))))))
                           (format "[%s%s] %d/%d" (make-string filled ?#)
                                   (make-string (- size filled) ?-)
                                   (lispm-desktop-installer-screen--progress)
                                   (length lispm-desktop-installer-screen--stages))) width)
                        (lispm-desktop-installer-screen--line display-status width)
                        (lispm-desktop-installer-screen--line display-message width)
                        (lispm-desktop-installer-screen--line
                         (lispm-desktop-installer-screen--tr
                          "Packages installed: %d" lispm-desktop-installer-screen--completed) width)
                        (lispm-desktop-installer-screen--line
                         (if lispm-desktop-installer-screen--last
                             (lispm-desktop-installer-screen--tr "Last: %s" lispm-desktop-installer-screen--last)
                           (lispm-desktop-installer-screen--tr "This may take a few minutes.")) width)
                        (lispm-desktop-installer-screen--line "" width)
                        border)))
      (when (< (window-body-height window) (length lines))
        (setq lines (delete (lispm-desktop-installer-screen--line "" width) lines)))
      (unless (window-live-p lispm-desktop-installer-screen--window)
        (setq lispm-desktop-installer-screen--window window
              lispm-desktop-installer-screen--previous (window-buffer window)))
      (with-current-buffer (get-buffer-create lispm-desktop-installer-screen--buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (cond
           (lispm-desktop-installer-screen--failure
            (let ((lines (lispm-desktop-installer-screen--failure-lines
                          lispm-desktop-installer-screen--stage)))
              (insert "\n")
              (dolist (line lines)
                (let ((start (point)))
                  (insert (mapconcat #'identity
                                     (split-string (with-temp-buffer
                                                     (insert line)
                                                     (let ((fill-column (max 16 (- width 6))))
                                                       (fill-region (point-min) (point-max)))
                                                     (buffer-string)) "\n")
                                     (concat "\n" margin "  ")) "\n\n")
                  (when (= start 2)
                    (add-face-text-property start (point)
                                            '(:foreground "#ff7474" :weight bold)))))))
           ((and (lispm-desktop-installer-screen--svg-available-p)
                 (>= (window-body-width window t) 360)
                 (>= (window-body-height window t) 390))
            (condition-case nil
                (insert (propertize " " 'display
                                    (svg-image (lispm-desktop-installer-screen--draw
                                                (- (window-body-width window t)
                                                   (frame-char-width (window-frame window)))
                                                (window-body-height window t) status)
                                               :ascent 100 :scale 1)))
              (error (insert top (mapconcat (lambda (line) (concat margin line)) lines "\n") "\n"))))
           (t (insert top (mapconcat (lambda (line) (concat margin line)) lines "\n") "\n")))
          (setq-local face-remapping-alist '((default (:background "black" :foreground "white") default)))
          (setq-local mode-line-format nil)
          (setq-local cursor-type nil)
          (setq-local truncate-lines t)
          (goto-char (point-min))
          (setq buffer-read-only t)))
      (set-window-buffer lispm-desktop-installer-screen--window lispm-desktop-installer-screen--buffer)
      (set-window-start lispm-desktop-installer-screen--window 1)
      (redisplay t))))

(defun lispm-desktop-installer-screen--archive (original desc &rest args)
  "Run ORIGINAL with DESC and ARGS, reporting a successful archive install."
  (let ((name (package-desc-name desc)))
    (lispm-desktop-installer-screen-report (format "Installing package: %s" name))
    (prog1 (apply original desc args)
      (cl-incf lispm-desktop-installer-screen--completed)
      (cl-incf lispm-desktop-installer-screen--stage-packages)
      (setq lispm-desktop-installer-screen--last name))))

(defun lispm-desktop-installer-screen--refresh (original &rest args)
  "Run ORIGINAL with ARGS while showing archive refresh."
  (lispm-desktop-installer-screen-report "Refreshing software sources...")
  (apply original args))

(defun lispm-desktop-installer-screen-start ()
  "Watch interactive startup's package installation."
  (unless (or noninteractive (daemonp))
    (when (and (display-graphic-p)
               (not (member "--lispm-hidden" command-line-args)))
      (unless (frame-visible-p (selected-frame))
        (make-frame-visible (selected-frame)))
      (select-frame-set-input-focus (selected-frame)))
    (setq lispm-desktop-installer-screen--active t
          lispm-desktop-installer-screen--completed 0
          lispm-desktop-installer-screen--stage-packages 0
          lispm-desktop-installer-screen--finished nil
          lispm-desktop-installer-screen--message nil
          lispm-desktop-installer-screen--failure nil
          lispm-desktop-installer-screen--stage "Preparing installation"
          lispm-desktop-installer-screen--status "Preparing installation..."
          lispm-desktop-installer-screen--last nil)
    (advice-add 'message :around #'lispm-desktop-installer-screen--capture-message)
    (lispm-desktop-installer-screen-report lispm-desktop-installer-screen--status)
    (advice-add 'package-install-from-archive :around #'lispm-desktop-installer-screen--archive)
    (advice-add 'package-refresh-contents :around #'lispm-desktop-installer-screen--refresh)))

(defun lispm-desktop-installer-screen-watch-deps ()
  "Watch the desktop's dependency stages once its dependency module is loaded."
  (when lispm-desktop-installer-screen--active
    (advice-add 'lispm-desktop-deps-ensure :around #'lispm-desktop-installer-screen--ensure)))

(defun lispm-desktop-installer-screen-stop ()
  "Remove startup instrumentation and restore the previous window."
  (interactive)
  (setq lispm-desktop-installer-screen--active nil
        lispm-desktop-installer-screen--failure nil)
  (advice-remove 'message #'lispm-desktop-installer-screen--capture-message)
  (advice-remove 'lispm-desktop-deps-ensure #'lispm-desktop-installer-screen--ensure)
  (advice-remove 'package-install-from-archive #'lispm-desktop-installer-screen--archive)
  (advice-remove 'package-refresh-contents #'lispm-desktop-installer-screen--refresh)
  (when (and (window-live-p lispm-desktop-installer-screen--window)
             (buffer-live-p lispm-desktop-installer-screen--previous)
             (eq (window-buffer lispm-desktop-installer-screen--window)
                 (get-buffer lispm-desktop-installer-screen--buffer)))
    (set-window-buffer lispm-desktop-installer-screen--window lispm-desktop-installer-screen--previous))
  (setq lispm-desktop-installer-screen--window nil
        lispm-desktop-installer-screen--previous nil)
  (when-let* ((buffer (get-buffer lispm-desktop-installer-screen--buffer)))
    (kill-buffer buffer)))

(provide 'lispm-desktop-installer-screen)
;;; lispm-desktop-installer-screen.el ends here
