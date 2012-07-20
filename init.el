; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; ------------------------------------------------------------------------
;; @ coding system

   ;; 日本語入力のための設定
   (set-keyboard-coding-system 'cp932)

   (prefer-coding-system 'utf-8-dos)
   (set-file-name-coding-system 'cp932)
   (setq default-process-coding-system '(cp932 . cp932))

;; ------------------------------------------------------------------------
;; @ ime

   ;; 標準 IME の設定
   (setq default-input-method "W32-IME")

   ;; IME 状態のモードライン表示
   (setq-default w32-ime-mode-line-state-indicator "[Aa]")
   (setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))

   ;; IME の初期化
   (w32-ime-initialize)

   ;; IME OFF 時の初期カーソルカラー
   ;; (set-cursor-color "red")

   ;; IME ON/OFF 時のカーソルカラー
   ;; (add-hook 'input-method-activate-hook
   ;;           (lambda () (set-cursor-color "green")))
   ;; (add-hook 'input-method-inactivate-hook
   ;;           (lambda () (set-cursor-color "red")))

   ;; バッファ切り替え時に IME 状態を引き継ぐ
   (setq w32-ime-buffer-switch-p nil)

;; ------------------------------------------------------------------------
;; @ encode

   ;; 機種依存文字
   (require 'cp5022x)
   (define-coding-system-alias 'euc-jp 'cp51932)

   ;; decode-translation-table の設定
   (coding-system-put 'euc-jp :decode-translation-table
              (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
   (coding-system-put 'iso-2022-jp :decode-translation-table
              (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
   (coding-system-put 'utf-8 :decode-translation-table
              (get 'japanese-ucs-jis-to-cp932-map 'translation-table))

   ;; encode-translation-table の設定
   (coding-system-put 'euc-jp :encode-translation-table
              (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
   (coding-system-put 'iso-2022-jp :encode-translation-table
              (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
   (coding-system-put 'cp932 :encode-translation-table
              (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
   (coding-system-put 'utf-8 :encode-translation-table
              (get 'japanese-ucs-jis-to-cp932-map 'translation-table))

   ;; charset と coding-system の優先度設定
   (set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
             'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)
   (set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)

   ;; PuTTY 用の terminal-coding-system の設定
   (apply 'define-coding-system 'utf-8-for-putty
      "UTF-8 (translate jis to cp932)"
      :encode-translation-table
      (get 'japanese-ucs-jis-to-cp932-map 'translation-table)
      (coding-system-plist 'utf-8))
   (set-terminal-coding-system 'utf-8-for-putty)

   ;; East Asian Ambiguous
   (defun set-east-asian-ambiguous-width (width)
     (while (char-table-parent char-width-table)
       (setq char-width-table (char-table-parent char-width-table)))
     (let ((table (make-char-table nil)))
       (dolist (range
            '(#x00A1 #x00A4 (#x00A7 . #x00A8) #x00AA (#x00AD . #x00AE)
             (#x00B0 . #x00B4) (#x00B6 . #x00BA) (#x00BC . #x00BF)
             #x00C6 #x00D0 (#x00D7 . #x00D8) (#x00DE . #x00E1) #x00E6
             (#x00E8 . #x00EA) (#x00EC . #x00ED) #x00F0
             (#x00F2 . #x00F3) (#x00F7 . #x00FA) #x00FC #x00FE
             #x0101 #x0111 #x0113 #x011B (#x0126 . #x0127) #x012B
             (#x0131 . #x0133) #x0138 (#x013F . #x0142) #x0144
             (#x0148 . #x014B) #x014D (#x0152 . #x0153)
             (#x0166 . #x0167) #x016B #x01CE #x01D0 #x01D2 #x01D4
             #x01D6 #x01D8 #x01DA #x01DC #x0251 #x0261 #x02C4 #x02C7
             (#x02C9 . #x02CB) #x02CD #x02D0 (#x02D8 . #x02DB) #x02DD
             #x02DF (#x0300 . #x036F) (#x0391 . #x03A9)
             (#x03B1 . #x03C1) (#x03C3 . #x03C9) #x0401
             (#x0410 . #x044F) #x0451 #x2010 (#x2013 . #x2016)
             (#x2018 . #x2019) (#x201C . #x201D) (#x2020 . #x2022)
             (#x2024 . #x2027) #x2030 (#x2032 . #x2033) #x2035 #x203B
             #x203E #x2074 #x207F (#x2081 . #x2084) #x20AC #x2103
             #x2105 #x2109 #x2113 #x2116 (#x2121 . #x2122) #x2126
             #x212B (#x2153 . #x2154) (#x215B . #x215E)
             (#x2160 . #x216B) (#x2170 . #x2179) (#x2190 . #x2199)
             (#x21B8 . #x21B9) #x21D2 #x21D4 #x21E7 #x2200
             (#x2202 . #x2203) (#x2207 . #x2208) #x220B #x220F #x2211
             #x2215 #x221A (#x221D . #x2220) #x2223 #x2225
             (#x2227 . #x222C) #x222E (#x2234 . #x2237)
             (#x223C . #x223D) #x2248 #x224C #x2252 (#x2260 . #x2261)
             (#x2264 . #x2267) (#x226A . #x226B) (#x226E . #x226F)
             (#x2282 . #x2283) (#x2286 . #x2287) #x2295 #x2299 #x22A5
             #x22BF #x2312 (#x2460 . #x24E9) (#x24EB . #x254B)
             (#x2550 . #x2573) (#x2580 . #x258F) (#x2592 . #x2595)
             (#x25A0 . #x25A1) (#x25A3 . #x25A9) (#x25B2 . #x25B3)
             (#x25B6 . #x25B7) (#x25BC . #x25BD) (#x25C0 . #x25C1)
             (#x25C6 . #x25C8) #x25CB (#x25CE . #x25D1)
             (#x25E2 . #x25E5) #x25EF (#x2605 . #x2606) #x2609
             (#x260E . #x260F) (#x2614 . #x2615) #x261C #x261E #x2640
             #x2642 (#x2660 . #x2661) (#x2663 . #x2665)
             (#x2667 . #x266A) (#x266C . #x266D) #x266F #x273D
             (#x2776 . #x277F) (#xE000 . #xF8FF) (#xFE00 . #xFE0F)
             #xFFFD
             ))
     (set-char-table-range table range width))
       (optimize-char-table table)
       (set-char-table-parent table char-width-table)
       (setq char-width-table table)))
   (set-east-asian-ambiguous-width 2)

   ;; emacs-w3m
   (eval-after-load "w3m"
     '(when (coding-system-p 'cp51932)
        (add-to-list 'w3m-compatible-encoding-alist '(euc-jp . cp51932))))

   ;; Gnus
   (eval-after-load "mm-util"
     '(when (coding-system-p 'cp50220)
        (add-to-list 'mm-charset-override-alist '(iso-2022-jp . cp50220))))

   ;; SEMI (cf. http://d.hatena.ne.jp/kiwanami/20091103/1257243524)
   (eval-after-load "mcs-20"
     '(when (coding-system-p 'cp50220)
        (add-to-list 'mime-charset-coding-system-alist
             '(iso-2022-jp . cp50220))))

   ;; 全角チルダ/ 波ダッシュを Windows スタイルにする
   (let ((table (make-translation-table-from-alist '((#x301c . #xff5e))) ))
     (mapc
      (lambda (coding-system)
        (coding-system-put coding-system :decode-translation-table table)
        (coding-system-put coding-system :encode-translation-table table)
        )
      '(utf-8 cp932 utf-16le)))

;; ------------------------------------------------------------------------
;; @ font

   ;; 標準フォントの設定
   ;; (set-default-font "M+2VM+IPAG circle-12")
   ;; (set-default-font "MigMix 2P-11")

   ;; IME 変換時フォントの設定 (テストバージョンのみ)
   ;; (setq w32-ime-font-face "MigMix 1M")
   ;; (setq w32-ime-font-height 22)

   ;; 固定等幅フォントの設定
   ;; (set-face-attribute 'fixed-pitch    nil :family "M+2VM+IPAG circle")

   ;; 可変幅フォントの設定
   ;; (set-face-attribute 'variable-pitch nil :family "M+2VM+IPAG circle")

;; ------------------------------------------------------------------------
;; @ frame

   ;; フレームタイトルの設定
   (setq frame-title-format "%b")

;; ------------------------------------------------------------------------
;; @ buffer

   ;; バッファ画面外文字の切り詰め表示
   (setq truncate-lines nil)

   ;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
   (setq truncate-partial-width-windows t)

   ;; 同一バッファ名にディレクトリ付与
   (require 'uniquify)
   (setq uniquify-buffer-name-style 'forward)
   (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
   (setq uniquify-ignore-buffers-re "*[^*]+*")

;; ------------------------------------------------------------------------
;; @ fringe

   ;; バッファ中の行番号表示
   (global-linum-mode t)

   ;; 行番号のフォーマット
   ;; (set-face-attribute 'linum nil :foreground "red" :height 0.8)
   (set-face-attribute 'linum nil :height 0.8)
   (setq linum-format "%4d")

;; ------------------------------------------------------------------------
;; @ modeline

   ;; 行番号の表示
   (line-number-mode t)

   ;; 列番号の表示
   (column-number-mode t)

  ;; 時刻の表示
   (require 'time)
   (setq display-time-24hr-format t)
   (setq display-time-string-forms '(24-hours ":" minutes))
   (display-time-mode t)

   ;; cp932 エンコード時の表示を「 P 」とする
   (coding-system-put 'cp932 :mnemonic ?P)
   (coding-system-put 'cp932-dos :mnemonic ?P)
   (coding-system-put 'cp932-unix :mnemonic ?P)
   (coding-system-put 'cp932-mac :mnemonic ?P)

;; ------------------------------------------------------------------------
;; @ cursor

   ;; カーソル点滅表示
   (blink-cursor-mode 0)

   ;; スクロール時のカーソル位置の維持
   (setq scroll-preserve-screen-position t)

   ;; スクロール行数 (一行ごとのスクロール)
   (setq vertical-centering-font-regexp ".*")
   (setq scroll-conservatively 35)
   (setq scroll-margin 0)
   (setq scroll-step 1)

   ;; 画面スクロール時の重複行数
   (setq next-screen-context-lines 1)

;; ------------------------------------------------------------------------
;; @ default setting

   ;; 起動メッセージの非表示
   (setq inhibit-startup-message t)

   ;; スタートアップ時のエコー領域メッセージの非表示
   (setq inhibit-startup-echo-area-message -1)

;; ------------------------------------------------------------------------
;; @ image-library
   (setq image-library-alist
         '((xpm "libxpm.dll")
           (png "libpng14.dll")
           (jpeg "libjpeg.dll")
           (tiff "libtiff3.dll")
           (gif "libungif4.dll")
           (svg "librsvg-2-2.dll")
           (gdk-pixbuf "libgdk_pixbuf-2.0-0.dll")
           (glib "libglib-2.0-0.dll")
           (gobject "libgobject-2.0-0.dll"))
         )

;; ------------------------------------------------------------------------
;; @ backup

   ;; 変更ファイルのバックアップ
   (setq make-backup-files nil)

   ;; 変更ファイルの番号つきバックアップ
   (setq version-control nil)

   ;; 編集中ファイルのバックアップ
   (setq auto-save-list-file-name nil)
   (setq auto-save-list-file-prefix nil)

   ;; 編集中ファイルのバックアップ先
   (setq auto-save-file-name-transforms
         `((".*" ,temporary-file-directory t)))

   ;; 編集中ファイルのバックアップ間隔 (秒)
   (setq auto-save-timeout 30)

   ;; 編集中ファイルのバックアップ間隔 (打鍵)
   (setq auto-save-interval 500)

   ;; バックアップ世代数
   (setq kept-old-versions 1)
   (setq kept-new-versions 2)

   ;; 上書き時の警告表示
   ;; (setq trim-versions-without-asking nil)

   ;; 古いバックアップファイルの削除
   (setq delete-old-versions t)

;; ------------------------------------------------------------------------
;; @ key bind

   ;; 標準キーバインド変更
   (global-set-key "\C-z"          'scroll-down)

;; ------------------------------------------------------------------------
;; @ scroll

   ;; バッファの先頭までスクロールアップ
   (defadvice scroll-up (around scroll-up-around)
     (interactive)
     (let* ( (start_num (+ 1 (count-lines (point-min) (point))) ) )
       (goto-char (point-max))
       (let* ( (end_num (+ 1 (count-lines (point-min) (point))) ) )
         (goto-line start_num )
         (let* ( (limit_num (- (- end_num start_num) (window-height)) ))
           (if (< (- (- end_num start_num) (window-height)) 0)
               (goto-char (point-max))
             ad-do-it)) )) )
   (ad-activate 'scroll-up)

   ;; バッファの最後までスクロールダウン
   (defadvice scroll-down (around scroll-down-around)
     (interactive)
     (let* ( (start_num (+ 1 (count-lines (point-min) (point)))) )
       (if (< start_num (window-height))
           (goto-char (point-min))
         ad-do-it) ))
   (ad-activate 'scroll-down)

;; ------------------------------------------------------------------------
;; @ print

   (require 'cl)
   (defun listsubdir (basedir)
     (remove-if (lambda (x) (not (file-directory-p x)))
                (directory-files basedir t "^[^.]")))

   (setq ps-print-color-p t
         ps-lpr-command "gswin32c.exe"
         ps-multibyte-buffer 'non-latin-printer
         ps-lpr-switches '("-sDEVICE=mswinpr2" "-dNOPAUSE" "-dBATCH" "-dWINKANJI")
         printer-name nil
         ps-printer-name nil
         ps-printer-name-option nil
         ps-print-header nil          ; ヘッダの非表示
         )

;; ------------------------------------------------------------------------
;; @ hiwin-mode
   ;; (require 'hiwin)

   ;; ;; 非アクティブ window の背景色 (hiwin-mode の実行前に設定が必要)
   ;; (setq hiwin-deactive-color "gray30")

   ;; ;; hiwin-mode を有効にする
   ;; (hiwin-mode)

   ;; ;; kill-buffer で再描画されるようにする
   ;; (defadvice kill-buffer
   ;;   (around kill-buffer-around activate)
   ;;   ad-do-it
   ;;   (if hiwin-ol (hiwin-draw-ol)))

;; ------------------------------------------------------------------------
;; @ tabbar

   (require 'tabbar)

   ;; tabbar有効化
   (tabbar-mode)

   ;; タブ切替にマウスホイールを使用（0：有効，-1：無効）
   (tabbar-mwheel-mode -1)

   ;; タブグループを使用（t：有効，nil：無効）
   (setq tabbar-buffer-groups-function nil)

   ;; ボタン非表示
   (dolist (btn '(tabbar-buffer-home-button
                  tabbar-scroll-left-button
                  tabbar-scroll-right-button))
     (set btn (cons (cons "" nil) (cons "" nil))))

   ;; タブ表示 一時バッファ一覧
   (defvar tabbar-displayed-buffers
     '("*scratch*" "*Messages*" "*Backtrace*" "*Colors*"
       "*Faces*" "*Apropos*" "*Customize*" "*shell*" "*Help*")
     "*Regexps matches buffer names always included tabs.")

   ;; 作業バッファの一部を非表示
   (setq tabbar-buffer-list-function
         (lambda ()
           (let* ((hides (list ?\  ?\*))
                  (re (regexp-opt tabbar-displayed-buffers))
                  (cur-buf (current-buffer))
                  (tabs (delq
                         nil
                         (mapcar
                          (lambda (buf)
                            (let ((name (buffer-name buf)))
                              (when (or (string-match re name)
                                        (not (memq (aref name 0) hides)))
                                buf)))
                          (buffer-list)))))
             (if (memq cur-buf tabs)
                 tabs
               (cons cur-buf tabs)))))

   ;; キーバインド設定
   (global-set-key (kbd "<C-tab>")   'tabbar-forward-tab)
   (global-set-key (kbd "<C-S-tab>") 'tabbar-backward-tab)

   ;; タブ表示欄の見た目（フェイス）
   (set-face-attribute 'tabbar-default nil
                       :background "gray40")

   ;; 選択タブの見た目（フェイス）
   (set-face-attribute 'tabbar-selected nil
                       :foreground "red3"
                       :background "#F3F2EF"
                       :box (list
                             :line-width 1
                             :color "gray80"
                             :style 'released-button)
                       :overline "#F3F2EF"
		       :weight 'bold
                       :family "ＭＳ Ｐゴシック"
                       )

   ;; 非選択タブの見た目（フェイス）
   (set-face-attribute 'tabbar-unselected nil
                       :foreground "black"
                       :background "#808080"
                       :box (list
                             :line-width 1
                             :color "gray90"
                             :style 'released-button)
                       :overline "gray80"
                       :family "ＭＳ Ｐゴシック"
                       )

   ;; タブ間隔の調整
   (set-face-attribute 'tabbar-separator nil
                       :height 0.1)

;; ------------------------------------------------------------------------
;; @ setup-cygwin
   (setq cygwin-mount-cygwin-bin-directory
         (concat (getenv "CYGWIN_DIR") "\\bin"))
   (require 'setup-cygwin)

;; ------------------------------------------------------------------------
;; @ shell
   (require 'shell)
   (setq explicit-shell-file-name "bash.exe")
   (setq shell-command-switch "-c")
   (setq shell-file-name "bash.exe")

   ;; (M-! and M-| and compile.el)
   (setq shell-file-name "bash.exe")
   (modify-coding-system-alist 'process ".*sh\\.exe" 'cp932)

   ;; shell モードの時の^M 抑制
   (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

   ;; shell-mode での補完 (for drive letter)
   (setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-")

   ;; エスケープシーケンス処理の設定
   (autoload 'ansi-color-for-comint-mode-on "ansi-color"
             "Set `ansi-color-for-comint-mode' to t." t)

   (setq shell-mode-hook
         (function
          (lambda ()

            ;; シェルモードの入出力文字コード
            (set-buffer-process-coding-system 'sjis-dos 'sjis-unix)
            (set-buffer-file-coding-system    'sjis-unix)
            )))

;; ------------------------------------------------------------------------
;; @ menu-tree
   ;; (setq menu-tree-coding-system 'utf-8)
   ;; (require 'menu-tree)

;; ------------------------------------------------------------------------
;; @ migemo/cmigemo
   (setq migemo-command (concat (getenv "INST_DIR")
                                "\\app\\cmigemo\\cmigemo"))
   (setq migemo-options '("-q" "--emacs"))
   (setq migemo-dictionary (concat (getenv "INST_DIR")
                                   "\\app\\cmigemo\\dict\\utf-8\\migemo-dict"))
   (setq migemo-user-dictionary nil)
   (setq migemo-regex-dictionary nil)
   (setq migemo-use-pattern-alist t)
   (setq migemo-use-frequent-pattern-alist t)
   (setq migemo-pattern-alist-length 1024)
   (setq migemo-coding-system 'utf-8-unix)
   (load-library "migemo")
   (migemo-init)

;; ------------------------------------------------------------------------
;; @ color-theme
   (require 'color-theme)
   (color-theme-initialize)


;;
;; Customized Area
;;

;; ------------------------------------------------------------------------
;; @ load-path

;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリを load-path に追加
(add-to-load-path "elisp" "conf" "plugins")

;; ------------------------------------------------------------------------
;; @ ELPA

;; (install-elisp "http://bit.ly/pkg-el23")

(when (require 'package nil t)
  ;; パッケージリポジトリにMarmaladeと開発者運営のELPAを追加
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  ;; インストールしたパッケージにロードパスを通して読み込む
  (package-initialize))

;; ------------------------------------------------------------------------
;; @ auto-install

(when (require 'auto-install nil t)
;; インストール先
(setq auto-install-directory "~/.emacs.d/elisp/")

;; 起動時に EmacsWiki のページ名を補完候補に加える
(auto-install-update-emacswiki-package-name t)

;; install.elisp.el 互換モードにする
(auto-install-compatibility-setup))

;; ------------------------------------------------------------------------
;; @ key-bind

;; M-/ に略語展開・補完機能を割り当て
(define-key global-map (kbd "M-/") 'hippie-expand)

;; C-h をバックスペースに変更
(keyboard-translate ?\C-h ?\C-?)

;; C-: に anything-for-files を割り当て
(define-key global-map (kbd "C-:") 'anything-for-files)

;; フレームの移動
(global-set-key (kbd "C-t") 'next-multiframe-window)
(global-set-key (kbd "C-S-t") 'previous-multiframe-window)

;; インデント
(global-set-key (kbd "C-S-i") 'indent-region)

;; インデントの削除
(global-set-key (kbd "C-c d") 'delete-indentation)

;; Eclipse ライクな行の複製
(defun duplicate-line-backward ()
  "Duplicate the current line backward."
  (interactive "*")
  (save-excursion
    (let ((contents
           (buffer-substring
            (line-beginning-position)
            (line-end-position))))
      (beginning-of-line)
      (insert contents ?\n)))
  (previous-line 1))

(defun duplicate-region-backward ()
  "If mark is active duplicates the region backward."
  (interactive "*")
  (if mark-active

      (let* (
             (deactivate-mark nil)
             (start (region-beginning))
             (end (region-end))
             (contents (buffer-substring
                        start
                        end)))
        (save-excursion
          (goto-char start)
          (insert contents))
        (goto-char end)
        (push-mark (+ end (- end start))))
    (error
     "Mark is not active. Region not duplicated.")))

(defun duplicate-line-forward ()
  "Duplicate the current line forward."
  (interactive "*")
  (save-excursion
    (let ((contents (buffer-substring
                     (line-beginning-position)
                     (line-end-position))))
      (end-of-line)
      (insert ?\n contents)))
  (next-line 1))

(defun duplicate-region-forward ()
  "If mark is active duplicates the region forward."
  (interactive "*")
  (if mark-active
      (let* (
             (deactivate-mark nil)
             (start (region-beginning))
             (end (region-end))
             (contents (buffer-substring
                        start
                        end)))
        (save-excursion
          (goto-char end)
          (insert contents))
        (goto-char start)
        (push-mark end)
        (exchange-point-and-mark))
    (error "Mark is not active. Region not duplicated.")))

(global-set-key (kbd "<M-up>") 'duplicate-line-backward)
(global-set-key (kbd "<M-down>") 'duplicate-line-forward)

;; ------------------------------------------------------------------------
;; @ utility

; #* というバックアップファイルを作らない
(setq auto-save-default nil)

;; *.~ というバックアップファイルを作らない
(setq make-backup-files nil)

;; 現在位置のファイル・ URL を開く
(ffap-bindings)

;; ベルを鳴らさない
(setq ring-bell-function 'ignore)

;; redo
;; 1) M-x install-elisp-from-emacswiki redo+.el
(require 'redo+)
(global-set-key (kbd "C-M-/") 'redo)
(setq undo-no-redo t)
(setq undo-limit 600000)
(setq undo-strong-limit 900000)

;; 略語展開・補完を行うコマンドをまとめる
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially   ;ファイル名の一部
        try-complete-file-name             ;ファイル名全体
        try-expand-all-abbrevs             ;静的略語展開
        try-expand-dabbrev                 ;動的略語展開 (カレントバッファ)
        try-expand-dabbrev-all-buffers     ;動的略語展開 (全バッファ)
        try-expand-dabbrev-from-kill       ;動的略語展開 (キルリング:M-w/C-w の履歴)
        try-complete-lisp-symbol-partially ;Lisp シンボル名の一部
        try-complete-lisp-symbol           ;Lisp シンボル名全体
        ))

;; Indent
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(c-set-offset 'case-label '+)

;; C-k で改行を含め削除
(setq kill-whole-line t)

;; カーソル位置から行頭まで削除する
(defun backward-kill-line (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (kill-line 0))
(global-set-key (kbd "C-S-k") 'backward-kill-line)

;; ファイルを自動保存する
;; M-x auto-install-from-url http://homepage3.nifty.com/oatu/emacs/archives/auto-save-buffers.el
(require 'auto-save-buffers)
(run-with-idle-timer 1 t 'auto-save-buffers) ; アイドル 1 秒で保存

;; タイトルバーに編集中のファイルのパス名を表示
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))

;; 折り返さない (t で折り返さない, nil で折り返す)
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

;; バッファ切り替えの強化
(iswitchb-mode 1)
(setq read-buffer-function 'iswitchb-read-buffer)
(setq iswitchb-regexp nil)
(setq iswitchb-prompt-newbuffer nil)

;; save-buffer 時, buffer 末尾に空行が常にあるように
(setq require-final-newline t)

;; auto-async-byte-compile
;; Warning: elisp ファイル上にエラーがある場合はバイトコンパイルされないので注意
(require 'auto-async-byte-compile)
;; 自動バイトコンパイルを無効にするファイルの正規表現
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;; 終了前に確認する
(defadvice save-buffers-kill-emacs
  (before safe-save-buffers-kill-emacs activate)
  "safe-save-buffers-kill-emacs"
  (unless (y-or-n-p "Really exit emacs? ")
    (keyboard-quit)))

;; sequential-command
;; http://d.hatena.ne.jp/rubikitch/20090219/sequential_command
;; (auto-install-batch "sequential-command")
(require 'sequential-command-config)
(define-sequential-command seq-home
  back-to-indentation beginning-of-line beginning-of-buffer seq-return)
(define-sequential-command seq-end
  end-of-line end-of-buffer seq-return)
(sequential-command-setup-keys)

;; ------------------------------------------------------------------------
;; @ anything

;; Install
;; (auto-install-batch "anything")

(when (require 'anything nil t)

  (require 'anything-startup)

  (setq
   ;; 候補を表示するまでの時間. デフォルトは 0.5
   anything-idle-delay 0.3
   ;; タイプして再描写するまでの時間. デフォルトは 0.1
   anything-input-idle-delay 0.2
   ;; 候補の最大表示数. デフォルトは 50
   anything-candidate-number-limit 100
   ;; 候補が多いときに体感速度を早くする
   anything-quick-update t
   ;; 候補選択ショートカットをアルファベットに
   anything-enable-shortcuts 'alphabet)

  (when (require 'anything-config nil t)
    ;; root 権限でアクションを実行するときのコマンド
    ;; デフォルトは"su"
    (setq anything-su-or-sudo "sudo"))

  (require 'anything-match-plugin nil t)

  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (require 'anything-migemo nil t))

  (when (require 'anything-complete nil t)
    ;; lisp シンボルの補完候補の再検索時間
    (anything-lisp-complete-symbol-set-timer 150))

  (require 'anything-show-completion nil t)

  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))

  (when (require 'descbinds-anything nil t)
    ;; describe-bindings を Anything に置き換える
    (descbinds-anything-install)))

;; M-y に anything-show-kill-ring を割り当てる
(define-key global-map (kbd "M-y") 'anything-show-kill-ring)

;;
;; anything-c-moccur

;; M-x install-elisp-from-emacswiki color-moccur.el
;; (install-elisp "http://svn.coderepos.org/share/lang/elisp/anything-c-moccur/trunk/anything-c-moccur.el")
;; See http://d.hatena.ne.jp/IMAKADO/20080724/1216882563

(when (require 'anything-c-moccur nil t)
  (setq
   ;; anything-c-moccur 用 `anything-idle-delay'
   anything-c-moccur-anything-idle-delay 0.1
   ;; バッファの情報をハイライトする
   anything-c-moccur-higligt-info-line-flag t
   ;; 現在選択中の候補の位置をほかの window に表示する
   anything-c-moccur-enable-auto-look-flag t
   ;; 起動時にポイントの位置の単語を初期パターンにする
   anything-c-moccur-enable-initial-pattern t)
  ;; Key bind
  (global-set-key (kbd "M-o") 'anything-c-moccur-occur-by-moccur) ;バッファ内検索
  (global-set-key (kbd "C-M-o") 'anything-c-moccur-dmoccur) ;ディレクトリ
  (add-hook 'dired-mode-hook ;dired
            '(lambda ()
               (local-set-key (kbd "O") 'anything-c-moccur-dired-do-moccur-by-moccur))))

;;
;; tag
;; Anything から TAGS を利用しやすくするコマンド作成
(when (and (require 'anything-exuberant-ctags nil t)
           (require 'anything-gtags nil t))
  ;; anything-for-tags 用のソースを定義
  (setq anything-for-tags
        (list anything-c-source-imenu
              anything-c-source-gtags-select
              ;; etags を利用する場合はコメントを外す
              ;; anything-c-source-etags-select
              anything-c-source-exuberant-ctags-select
              ))

  ;; anything-for-tags コマンドを作成
  (defun anything-for-tags ()
    "Preconfigured `anything' for anything-for-tags."
    (interactive)
    (anything anything-for-tags
              (thing-at-point 'symbol)
              nil nil nil "*anything for tags*"))

  ;; M-t に anything-for-current を割り当て
  (define-key global-map (kbd "M-t") 'anything-for-tags))

;; ------------------------------------------------------------------------
;; @ color

(set-foreground-color                                  "#ffffff") ; 文字色
(set-background-color                                  "#000000") ; 背景色
(set-cursor-color                                      "#ffffff") ; カーソル色
(set-face-background 'region                           "#0000CD") ; リージョン
(set-face-foreground 'modeline                         "#ffffff") ; モードライン文字
(set-face-background 'modeline                         "#00008B") ; モードライン背景
(set-face-foreground 'mode-line-inactive               "#000000") ; モードライン文字 (非アクティブ)
(set-face-background 'mode-line-inactive               "#ffffff") ; モードライン背景 (非アクティブ)
(set-face-foreground 'font-lock-comment-delimiter-face "#888888") ; コメントデリミタ
(set-face-foreground 'font-lock-comment-face           "#888888") ; コメント
(set-face-foreground 'font-lock-string-face            "#ff4500") ; 文字列
(set-face-foreground 'font-lock-function-name-face     "#ffffff") ; 関数名
(set-face-foreground 'font-lock-keyword-face           "#FF7F7F") ; キーワード
(set-face-foreground 'font-lock-constant-face          "#FF7F7F") ; 定数 (this, self なども)
(set-face-foreground 'font-lock-variable-name-face     "#6495ED") ; 変数
(set-face-foreground 'font-lock-type-face              "#20b2aa") ; クラス
(set-face-foreground 'fringe                           "#666666") ; fringe (折り返し記号なでが出る部分)
(set-face-background 'fringe                           "#282828") ; fringe

;; 現在行をハイライト
(global-hl-line-mode t)
(set-face-background 'hl-line "#222244")

;; ------------------------------------------------------------------------
;; @ frame-setting
(if window-system
    (progn
      (set-frame-parameter nil 'alpha 88)  ; 透明度
      (set-scroll-bar-mode nil)            ; スクロールバー非表示
      (tool-bar-mode nil)
      (setq ns-pop-up-frames nil)))

;; ------------------------------------------------------------------------
;; @ display

;; paren-mode 対応する括弧を強調表示
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)
(show-paren-mode t)
(set-face-background 'show-paren-match-face "#008080")
(set-face-foreground 'show-paren-mismatch-face "red")

;; 行番号表示
;; (global-linum-mode)
;; (setq linum-format "%4d")

;; 全角スペース, タブの強調表示
(defface my-face-b-1 '((t (:background "medium aquamarine"))) nil)
(defface my-face-b-2 '((t (:background "medium aquamarine"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(
     ("　" 0 my-face-b-1 append)
     ("\t" 0 my-face-b-2 append)
     ("[ ]+$" 0 my-face-u-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
(add-hook 'find-file-hooks
          '(lambda ()
             (if font-lock-mode nil
               (font-lock-mode t))) t)

;; 改行コードを表示
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; カーソル位置のフェースを調べる関数
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

;; 行の折り返しトグルコマンド
(define-key global-map (kbd "C-c r") 'toggle-truncate-lines)

;; ------------------------------------------------------------------------
;; @ text-adjust

;; (install-elisp "http://taiyaki.org/elisp/mell/src/mell.el")
;; (install-elisp "http://taiyaki.org/elisp/text-adjust/src/text-adjust.el")

(require 'text-adjust)
(defun text-adjust-space-before-save-if-needed ()
  (when (memq major-mode
              '(org-mode text-mode)) ;; 特定のモードで保存するときに自動で text-adjust-space-buffer を実行
    (text-adjust-space-buffer)))
(add-hook 'before-save-hook 'text-adjust-space-before-save-if-needed)

;; ------------------------------------------------------------------------
;; @ alias

(defalias 'qr  'query-replace)
(defalias 'qrr 'query-replace-regexp)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'tab 'text-adjust-buffer)

;; ------------------------------------------------------------------------
;; @ cua-mode

;; cua-mode をオン
(cua-mode t)

;; CUA キーバインドを無効にする
(setq cua-enable-cua-keys nil)

;; ------------------------------------------------------------------------
;; @ dired

;; dired-find-alternate-file の有効化
(put 'dired-find-alternate-file 'disabled nil)

;; dired-x
(require 'dired-x)

;; wdired
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; ファイルなら別バッファで、ディレクトリなら同じバッファで開く
;; See http://d.hatena.ne.jp/nishikawasasaki/20120222/1329932699
(defun dired-open-in-accordance-with-situation ()
  (interactive)
  (let ((file (dired-get-filename)))
    (if (file-directory-p file)
        (dired-find-alternate-file)
      (dired-find-file))))

;; RET 標準の dired-find-file では dired バッファが複数作られるので
;; dired-find-alternate-file を代わりに使う
;; Note: この設定により, '.' と '..' の相対パスに移動する場合は a をタイプする必要がある.
(define-key dired-mode-map (kbd "RET") 'dired-open-in-accordance-with-situation)
(define-key dired-mode-map (kbd "a") 'dired-find-file)

;; ------------------------------------------------------------------------
;; @ igrep

(require 'igrep)
(igrep-define lgrep (igrep-use-zgrep nil)(igrep-regex-option "-n -Ou8"))
(igrep-find-define lgrep (igrep-use-zgrep nil)(igrep-regex-option "-n -Ou8"))
(setq igrep-find-use-xargs nil)

(require 'grep-edit)

;; ------------------------------------------------------------------------
;; @ smartchr

;; (install-elisp "https://raw.github.com/imakado/emacs-smartchr/master/smartchr.el")

(require 'smartchr)
(defun smartchr-custom-keybindings ()
  (local-set-key (kbd "=") (smartchr '("=" " = " " == " " === ")))
  ;; !! がカーソルの位置
  (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "[") (smartchr '("[`!!']" "[[`!!']]" "[")))
  (local-set-key (kbd "{") (smartchr '("{`!!'}" "{\n`!!'\n}" "{")))
  (local-set-key (kbd "`") (smartchr '("\``!!''" "\`")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
  (local-set-key (kbd ">") (smartchr '(">" "->" ">>")))
  )

(defun smartchr-custom-keybindings-objc ()
  (local-set-key (kbd "@") (smartchr '("@\"`!!'\"" "@")))
  )

(add-hook 'php-mode-hook 'smartchr-custom-keybindings)
(add-hook 'c-mode-common-hook 'smartchr-custom-keybindings)
(add-hook 'js2-mode-hook 'smartchr-custom-keybindings)
(add-hook 'ruby-mode-hook 'smartchr-custom-keybindings)
(add-hook 'cperl-mode-hook 'smartchr-custom-keybindings)
(add-hook 'objc-mode-hook 'smartchr-custom-keybindings-objc)

;; ------------------------------------------------------------------------
;; @ yasnippet

;; https://github.com/capitaomorte/yasnippet

(require 'yasnippet)
(setq yas/snippet-dirs '("~/.emacs.d/plugins/yasnippet/snippets"))
(yas/global-mode 1)

;; ------------------------------------------------------------------------
;; @ auto-complete

;; auto-complete
;; (package-install 'auto-complete)

;; company
;; (package-install 'company)

;; ac-company
;; (install-elisp "https://raw.github.com/buzztaiki/auto-complete/master/ac-company.el")

(when (require 'auto-complete-config nil t)
  ;; (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

  ;; 辞書補完
  ;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/conf/ac-dict")

  ;; サンプル設定の有効化
  (ac-config-default)

  ;; 補完ウィンドウ内でのキー定義
  (define-key ac-completing-map (kbd "C-n") 'ac-next)
  (define-key ac-completing-map (kbd "C-p") 'ac-previous)
  (define-key ac-completing-map (kbd "M-/") 'ac-stop)

  ;; 補完が自動で起動するのを停止
  (setq ac-auto-start t)

  ;; 起動キーの設定
  (ac-set-trigger-key "TAB")

  ;; 候補の最大件数 デフォルトは 10件
  (setq ac-candidate-max 20)

  ;; 補完を開始する文字数
  (setq ac-auto-start 1)

  ;; 補完リストが表示されるまでの時間
  (setq ac-auto-show-menu 0.2)

  (defun auto-complete-init-sources ()
    (setq ac-sources '(ac-source-yasnippet
                       ac-source-dictionary
                       ac-source-gtags
                       ac-source-words-in-buffer)))

  (auto-complete-init-sources)

  (add-to-list 'ac-modes 'emacs-lisp-mode)
  (add-to-list 'ac-modes 'html-mode)
  (add-to-list 'ac-modes 'js2-mode)
  (add-to-list 'ac-modes 'tmt-mode)
  (add-to-list 'ac-modes 'yaml-mode)

  (require 'ac-company)

  ;; for emacs-lisp-mode
  (add-hook 'emacs-lisp-mode-hook
            '(lambda ()
               (auto-complete-init-sources)
               (add-to-list 'ac-sources 'ac-source-functions)
               (add-to-list 'ac-sources 'ac-source-symbols))))

;; ------------------------------------------------------------------------
;; @ flymake

(require 'flymake)

;; 全てのファイルで flymakeを有効化
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; Makefile があれば利用し, なければ直接コマンドを実行する
;; Makefile の種類を定義
(defvar flymake-makefile-filenames
  '("Makefile" "makefile" "GNUmakefile")
  "File names for make.")

;; Makefile がなければコマンドを直接利用するコマンドラインを生成
(defun flymake-get-make-gcc-cmdline (source base-dir)
  (let (found)
    (dolist (makefile flymake-makefile-filenames)
      (if (file-readable-p (concat base-dir "/" makefile))
          (setq found t)))
    (if found
        (list "make"
              (list "-s"
                    "-C"
                    base-dir
                    (concat "CHK_SOURCES=" source)
                    "SYNTAX_CHECK_MODE=1"
                    "check-syntax"))
      (list (if (string= (file-name-extension source) "c") "gcc" "g++")
            (list "-o"
                  "/dev/null"
                  "-fsyntax-only"
                  "-Wall"
                  source)))))

;; Flymake 初期化関数の生成
(defun flymake-simple-make-gcc-init-impl
  (create-temp-f use-relative-base-dir
                 use-relative-source build-file-name get-cmdline-f)
  "Create syntax check command line for a directly checked source file.
Use CREATE-TEMP-F for creating temp copy."
  (let* ((args nil)
         (source-file-name buffer-file-name)
         (buildfile-dir (file-name-directory source-file-name)))
    (if buildfile-dir
        (let* ((temp-source-file-name
                (flymake-init-create-temp-buffer-copy create-temp-f)))
          (setq args
                (flymake-get-syntax-check-program-args
                 temp-source-file-name
                 buildfile-dir
                 use-relative-base-dir
                 use-relative-source
                 get-cmdline-f))))
    args))

;; 初期化関数を定義
(defun flymake-simple-make-gcc-init ()
  (message "%s" (flymake-simple-make-gcc-init-impl
                 'flymake-create-temp-inplace t t "Makefile"
                 'flymake-get-make-gcc-cmdline))
  (flymake-simple-make-gcc-init-impl
   'flymake-create-temp-inplace t t "Makefile"
   'flymake-get-make-gcc-cmdline))

;; 拡張子 .c, .cpp, c++ などのときに上記の関数を利用する
(add-to-list 'flymake-allowed-file-name-masks
             '("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'"
               flymake-simple-make-gcc-init))


;; XML 用 Flymake の設定
(defun flymake-xml-init ()
  (list "xmllint" (list "--valid"
                        (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))))


;; HTML 用 Flymake の設定
(defun flymake-html-init ()
  (list "tidy" (list (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.html\\'" flymake-html-init))

;; tidy error pattern
(add-to-list 'flymake-err-line-patterns
             '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)"
               nil 1 2 4))


;; JS 用 Flymake の初期化関数の定義
(defun flymake-jsl-init ()
  (list "jsl" (list "-process" (flymake-init-create-temp-buffer-copy
                                'flymake-create-temp-inplace))))
;; JavaScript 編集で Flymake を起動する
(add-to-list 'flymake-allowed-file-name-masks
             '("\\.js\\'" flymake-jsl-init))

(add-to-list 'flymake-err-line-patterns
             '("^\\(.+\\) (\\([0-9]+\\)): \\(.*warning\\|SyntaxError\\): \\(.*\\)"
               1 2 nil 4))


;; Ruby 用 Flymake の設定
(defun flymake-ruby-init ()
  (list "ruby" (list "-c" (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.rb\\'" flymake-ruby-init))

(add-to-list 'flymake-err-line-patterns
             '("\\(.*\\):(\\([0-9]+\\)): \\(.*\\)" 1 2 nil 3))


;; Python 用 Flymake の設定
;; (install-elisp "https://raw.github.com/seanfisk/emacs/sean/src/lib/flymake-python.el")
(when (require 'flymake-python nil t)
  ;; flake8 を利用する
  (when (executable-find "flake8")
    (setq flymake-python-syntax-checker "flake8"))
  ;; pep8 を利用する
  ;; (setq flymake-python-syntax-checker "pep8")
  )

;; ------------------------------------------------------------------------
;; @ js2-mode

;; インデント設定
;; リージョン選択後 C-S-i
(add-hook 'js2-mode-hook 'js-indent-hook)

;; References
;; http://d.hatena.ne.jp/m-hiyama/20080627/1214549228
;; http://d.hatena.ne.jp/speg03/20091011/1255244329

;; ------------------------------------------------------------------------
;; @ perl

;; perl-modeをcperl-modeのエイリアスにする
(defalias 'perl-mode 'cperl-mode)

(add-to-list 'auto-mode-alist '("\\.pl$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pm$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.t$" . cperl-mode))

;; cperl-modeのインデント設定
(setq cperl-indent-level 4                         ; インデント幅を 4 にする
      cperl-continued-statement-offset 4           ; 継続する文のオフセット
      cperl-brace-offset -4                        ; ブレースのオフセット
      cperl-label-offset -4                        ; label のオフセット
      cperl-indent-parens-as-block t               ; 括弧もブロックとしてインデント
      cperl-close-paren-offset -4                  ; 閉じ括弧のオフセット
      cperl-tab-always-indent t                    ; TAB をインデントにする
      cperl-indent-region-fix-constructs t
      cperl-highlight-variables-indiscriminately t ; スカラを常にハイライトする
      cperl-comment-column 40)

;; perl-completion
;; (install-elisp "http://www.emacswiki.org/emacs/download/perl-completion.el")

(defun perl-completion-hook ()
  (when (require 'perl-completion nil t)
    (perl-completion-mode t)
    (when (require 'auto-complete nil t)
      (auto-complete-mode t)
    ;; 補完のキーバインドを変更
    (define-key cperl-mode-map (kbd "C-o") 'plcmp-cmd-smart-complete)
      (make-variable-buffer-local 'ac-sources)
      (setq ac-sources
            '(ac-source-perl-completion)))))

(add-hook  'cperl-mode-hook 'perl-completion-hook)

;; ------------------------------------------------------------------------
;; @ php

;; php-mode の設定
(when (require 'php-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.ctp\\'" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.twig\\'" . php-mode))
  (setq php-search-url "http://jp.php.net/ja/")
  (setq php-manual-url "http://jp.php.net/manual/ja/"))

;; php-mode のインデント設定
(defun php-indent-hook ()
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  ;; (c-set-offset 'case-label '+) ; switch 文の case ラベル
  (c-set-offset 'arglist-intro '+) ; 配列の最初の要素が改行した場合
  (c-set-offset 'arglist-close 0)) ; 配列の閉じ括弧

(add-hook 'php-mode-hook 'php-indent-hook)

;; php-mode の補完を強化する
(defun php-completion-hook ()
  (when (require 'php-completion nil t)
    (php-completion-mode t)
    ;; anything による補完
    (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)

    (when (require 'auto-complete nil t)
      (make-variable-buffer-local 'ac-sources)
      (add-to-list 'ac-sources 'ac-source-php-completion)
      (auto-complete-mode t))))

(add-hook 'php-mode-hook 'php-completion-hook)


;;
;; CakePHP
;;

;; (install-elisp "https://raw.github.com/k1LoW/emacs-historyf/master/historyf.el")
;; (install-elisp "https://raw.github.com/k1LoW/emacs-cake/master/cake-inflector.el")
;; (install-elisp "https://raw.github.com/k1LoW/emacs-cake/master/cake.el")
;; (install-elisp "https://raw.github.com/k1LoW/emacs-cake/master/ac-cake.el")
;; (install-elisp "https://raw.github.com/k1LoW/emacs-cake2/master/cake2.el")
;; (install-elisp "https://raw.github.com/k1LoW/emacs-cake2/master/ac-cake2.el")

;; CakePHP 1 系統の emacs-cake
(when (require 'cake nil t)
  ;; emacs-cake の標準キーバインドを利用する
  (cake-set-default-keymap)
  ;; 標準で emacs-cake をオフ
  (global-cake -1))

;; CakePHP 2 系統の emacs-cake
(when (require 'cake2 nil t)
  ;; emacs-cake2 の標準キーバインドを利用する
  (cake2-set-default-keymap)
  ;; 標準で emacs-cake2 をオン
  (global-cake2 t))

;; emacs-cake を切り替えるコマンドを定義
(defun toggle-emacs-cake ()
  "emacs-cake と emacs-cake2 を切り替える"
  (interactive)
  (cond ((eq cake2 t) ; cake2 がオンであれば
         (cake2 -1) ; cake2 をオフにして
         (cake t)) ; cake をオンにする
        ((eq cake t) ; cake がオンであれば
         (cake -1) ; cake をオフにして
         (cake2 t)) ; cake2 をオンにする
        (t nil))) ; どちらもオフであれば何もしない

;; C-c t に toggle-emacs-cake を割り当て
(define-key cake-key-map (kbd "C-c t") 'toggle-emacs-cake)
(define-key cake2-key-map (kbd "C-c t") 'toggle-emacs-cake)

;; auto-complete, ac-cake, ac-cake2 の読み込みをチェック
(when (and (require 'auto-complete nil t)
           (require 'ac-cake nil t)
           (require 'ac-cake2 nil t))
  ;; ac-cake 用の関数定義
  (defun ac-cake-hook ()
    (make-variable-buffer-local 'ac-sources)
    (add-to-list 'ac-sources 'ac-source-cake)
    (add-to-list 'ac-sources 'ac-source-cake2))
  ;; php-mode-hook に ac-cake 用の関数を追加
  (add-hook 'php-mode-hook 'ac-cake-hook))

;; ------------------------------------------------------------------------
;; @ ruby

;; ruby-mode のインデント設定
(setq ;; ruby-indent-level 3      ; インデント幅を 3 に. 初期値は 2
 ruby-deep-indent-paren-style t ; 改行時のインデントを調整する
 ;; ruby-mode 実行時に indent-tabs-mode を設定値に変更
 ;; ruby-indent-tabs-mode t       ; タブ文字を使用する. 初期値は nil
 )

;; 括弧の自動挿入── ruby-electric
;; (install-elisp "https://raw.github.com/ruby/ruby/trunk/misc/ruby-electric.el")
(require 'ruby-electric nil t)

;; end に対応する行のハイライト── ruby-block
;; M-x auto-install-from-emacswiki RET ruby-block.el
(when (require 'ruby-block nil t)
  (setq ruby-block-highlight-toggle t))

;; インタラクティブ Ruby を利用する── inf-ruby
;; (install-elisp "https://raw.github.com/ruby/ruby/trunk/misc/inf-ruby.el")
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

;; ruby-mode-hook 用の関数を定義
(defun ruby-mode-hooks ()
  (inf-ruby-keys)
  (ruby-electric-mode t)
  (ruby-block-mode t))

;; ruby-mode-hook に追加
(add-hook 'ruby-mode-hook 'ruby-mode-hooks)

;; ------------------------------------------------------------------------
;; @ yaml

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; for yaml-mode
(add-hook 'yaml-mode-hook
          '(lambda ()
             (auto-complete-init-sources)
             (setq ac-sources '(ac-source-words-in-buffer))))

;; ------------------------------------------------------------------------
;; @ markdown

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq markdown-command "perl /home/.emacs.d/lib/Markdown.pl")
(add-hook 'markdown-mode-hook 'turn-on-orgtbl)

(add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdwn$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdt$" . markdown-mode))
