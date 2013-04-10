(require 'cl)

;; ------------------------------------------------------------------------
;; @dired

;; http://d.hatena.ne.jp/tam5917/20130126/1359206522
;; http://nishikawasasaki.hatenablog.com/entry/20120222/1329932699
(defun dired-open-in-accordance-with-situation ()
    (interactive)
    (cond ((string-match "\\(?:\\.\\.?\\)"
                         (format "%s" (thing-at-point 'filename)))
           (dired-find-alternate-file))
          ((file-directory-p (dired-get-filename))
           (dired-find-alternate-file))
          (t
           (dired-find-file))))

;; dired-find-alternate-file の有効化
(put 'dired-find-alternate-file 'disabled nil)

;; ------------------------------------------------------------------------
;; @PATH

;; http://sakito.jp/emacs/emacsshell.html#path
;; http://d.hatena.ne.jp/peccu/20101116/emacs_evernote

;; より下に記述した物が PATH の先頭に追加されます
(dolist (dir (list
              "/sbin"
              "/usr/sbin"
              "/bin"
              "/usr/bin"
              "/sw/bin"
              "/usr/local/share/python"
              "/usr/local/bin"
              (expand-file-name "~/bin")
              (expand-file-name "~/.perlbrew/bin")
              (expand-file-name "~/.perlbrew/perls/perl-5.14.2/bin")
              ))

  ;; PATH と exec-path に同じ物を追加します
  (when (and (file-exists-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))

;; ------------------------------------------------------------------------
;; @Custom key

;; Command キーを Meta に、 Option を SUPER キー（ Win キー）に入れ替え
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; ------------------------------------------------------------------------
;; @backup file

;; #* というバックアップファイルを作らない
(setq auto-save-default nil)

;; *.~ というバックアップファイルを作らない
(setq make-backup-files nil)

;; ------------------------------------------------------------------------
;; @etc

;; sticky
;; (require 'sticky)
;; セミコロンを押下時に shift キーを押した状態になる
;; 英大文字や $ 記号などの入力に便利
;; (use-sticky-key ";" sticky-alist:ja)

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

;; 最近使ったファイルを開く
;; M-x auto-install-from-emacswiki recentf-ext.el
;; See: http://d.hatena.ne.jp/tomoya/20110217/1297928222
(when (require 'recentf nil t)
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1))

;; recentf の強化版
(require 'recentf-ext)

;; save-buffer 時、 buffer 末尾に空行が常にあるように
(setq require-final-newline t)

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c e") 'eval-region)
             (setq indent-tabs-mode nil)))

;; ------------------------------------------------------------------------
;; @auto-async-byte-compile

;; Warning: elisp ファイル上にエラーがある場合はバイトコンパイルされないので注意
(require 'auto-async-byte-compile)

;; 自動バイトコンパイルを無効にするファイルの正規表現
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;; ------------------------------------------------------------------------
;; @alias

(defalias 'qr  'query-replace)
(defalias 'qrr 'query-replace-regexp)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'tab 'text-adjust-buffer)
(defalias 'tasb 'text-adjust-space-buffer)
(defalias 'icd 'insert-current-date)
(defalias 'ict 'insert-current-time)

;; ------------------------------------------------------------------------
;; @cua-mode

;; cua-mode をオン
(cua-mode t)

;; cua キーバインドを無効にする
(setq cua-enable-cua-keys nil)

;; ------------------------------------------------------------------------
;; @display

;; font and frame setting
(if window-system
    (progn
      (set-frame-parameter nil 'alpha 88)  ; 透明度
      (setq inhibit-startup-message t)     ; 起動時のメッセージを非表示
      (setq inhibit-startup-screen t)      ; スタートアップメッセージを非表示
      (menu-bar-mode 0)                    ; menu-bar を非表示
      (tool-bar-mode nil)                  ; ツールバー非表示
      (setq initial-frame-alist
            '((width . 198)
              (height . 58)
              (top . 0)
              (left . 2)))                 ; 起動時のフレームの設定
      (set-scroll-bar-mode nil)            ; スクロールバー非表示
      (setq line-spacing 0.2)              ; 行間
      (when (>= emacs-major-version 23)
        (tool-bar-mode nil)
        ;; (set-frame-font "Menlo-12")
        (set-frame-font "Migu 1m")
        (set-fontset-font (frame-parameter nil 'font)
                          'japanese-jisx0208
                          (font-spec :family "Hiragino Maru Gothic ProN" :size 13))
        (set-fontset-font (frame-parameter nil 'font)
                          'japanese-jisx0212
                          (font-spec :family "Hiragino Maru Gothic ProN" :size 13))
        (set-fontset-font (frame-parameter nil 'font)
                          'katakana-jisx0201
                          (font-spec :family "Hiragino Maru Gothic ProN" :size 13)))
      (setq ns-pop-up-frames nil)))

;; ------------------------------------------------------------------------
;; @session

;; http://d.hatena.ne.jp/whitypig/20110331/1301521329
;; http://openlab.dino.co.jp/2008/09/26/230919351.html
;; installed from ELPA

(when (require 'session nil t)
  (setq session-save-file-coding-system 'utf-8-unix)
  (setq session-save-file (expand-file-name "~/.emacs.d/.session.dat"))
  (setq session-initialize '(session places))
  (setq session-globals-max-size 1024)
  (setq session-globals-max-string (* 1024 1024))
  (setq session-globals-include '((kill-ring 512)
                                  (session-file-alist 512)
                                  (file-name-history 512)
                                  (tags-table-set-list 128)
                                  (tags-table-list 128)))
  (add-hook 'after-init-hook 'session-initialize)
  (setq session-undo-check -1)
  ;; Save session info every 30 minutes
  (setq my-timer-for-session-save-session (run-at-time t 1800 'session-save-session)))

;; ------------------------------------------------------------------------
;; @popwin

;; http://d.hatena.ne.jp/m2ym/20110120/1295524932
;; http://d.hatena.ne.jp/sokutou-metsu/20110205/1296915272
;; http://d.hatena.ne.jp/hirose31/20110302/1299062869

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-width 0.4)
(push '(" *auto-async-byte-compile*" :position right :noselect t) popwin:special-display-config)
(push '(dired-mode :position right) popwin:special-display-config)

;; ------------------------------------------------------------------------
;; @color-theme

;; カラーテーマ (http://www.nongnu.org/color-theme/)
;; テーマサンプル (http://code.google.com/p/gnuemacscolorthemetest/)

;; (require 'color-theme)
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;;      (color-theme-midnight)))

;; ------------------------------------------------------------------------
;; @color

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

;; カーソル位置のフェースを調べる関数
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

;; 現在行をハイライト
(global-hl-line-mode t)
(set-face-background 'hl-line "#222244")

;; ------------------------------------------------------------------------
;; @etc

;; paren-mode 対応する括弧を強調表示
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)
(show-paren-mode t)
(set-face-background 'show-paren-match-face "#008080")
(set-face-foreground 'show-paren-mismatch-face "red")

;; 行番号表示
(global-linum-mode)
(setq linum-format "%4d")

;; 行番号、列番号の表示 (モードライン)
(line-number-mode t)
(column-number-mode t)

;; 全角スペース、タブの強調表示
(defface my-face-b-1 '((t (:background "#222244"))) nil)
(defface my-face-b-2 '((t (:background "#222244" :underline t))) nil)
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

;; 行の折り返しトグルコマンド
(define-key global-map (kbd "C-c r") 'toggle-truncate-lines)

;;; GC を減らす
(setq gc-cons-threshold (* 50 gc-cons-threshold))

;;; ログの記録量を増やす
(setq message-log-max 10000)

;;; 履歴存数を増やす
(setq history-length 1000)

;;; ダイアログボックスを使わない
(setq use-dialog-box nil)
(defalias 'message-box 'message)

;;; yes で答る部分も y で答えられるように
(defalias 'yes-or-no-p 'y-or-n-p)

;; ediff 関連のバッファを１つのフレームにまとめる
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; ------------------------------------------------------------------------
;; @encoding

(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

;; ------------------------------------------------------------------------
;; @key binding

;; recentf-open-files を SUPER-f に割り当て
;; キーバインド設定参考 http://d.hatena.ne.jp/tomoya/20090415/1239809615
(define-key global-map [(s f)] 'recentf-open-files)

;; C-l に略語展開・補完機能を割り当て
(define-key global-map (kbd "M-/") 'hippie-expand)

;; C-h をバックスペースに変更
(keyboard-translate ?\C-h ?\C-?)

;; C-: に anything-for-files を割り当て
(define-key global-map (kbd "C-:") 'anything-for-files)

;; フレームの移動
(global-set-key (kbd "C-t") 'next-multiframe-window)
(global-set-key (kbd "C-S-t") 'previous-multiframe-window)

;;; windmove
;; (windmove-default-keybindings) ; 引数なしの場合は Shift
;; Alt + 矢印でウィンドウを移動する
;; (windmove-default-keybindings 'meta) ; Alt の場合は meta を指定
;; Mac の Command + 矢印でウィンドウを移動する
(windmove-default-keybindings 'super) ; Mac の人はこちらをオススメ

;; インデント
(global-set-key (kbd "C-S-i") 'indent-region)

;; 行番号を指定して移動
(global-set-key "\M-g" 'goto-line)

;; 範囲指定していないとき、 C-w で前の単語を削除
(defadvice kill-region (around kill-word-or-kill-region activate)
  (if (and (interactive-p) transient-mark-mode (not mark-active))
      (backward-kill-word 1)
    ad-do-it))

;; minibuffer 用
(define-key minibuffer-local-completion-map "\C-w" 'backward-kill-word)

;; カーソル位置の単語を削除
(defun kill-word-at-point ()
  (interactive)
  (let ((char (char-to-string (char-after (point)))))
    (cond
     ((string= " " char) (delete-horizontal-space))
     ((string-match "[\t\n -@\[-`{-~]" char) (kill-word 1))
     (t (forward-char) (backward-word) (kill-word 1)))))
(global-set-key "\M-d" 'kill-word-at-point)

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

(global-set-key (kbd "<M-s-up>") 'duplicate-line-backward)
(global-set-key (kbd "<M-s-down>") 'duplicate-line-forward)

;; ------------------------------------------------------------------------
;; @speedbar

(defun my-speedbar-expand-line ()
  (interactive)
  (if (= (point-max) (progn (speedbar-expand-line) (point-max)))
      (save-current-buffer
        (speedbar-edit-line))))

(when (locate-library "speedbar")
  (require 'speedbar)
  ;; "a" で無視ファイル表示/ 非表示のトグル
  (define-key speedbar-file-key-map "a" 'speedbar-toggle-show-all-files)
  ;; ← や → でもディレクトリを開閉 ;;デフォルト: "=" "+", "-"
  (define-key speedbar-file-key-map [right] 'my-speedbar-expand-line)
  (define-key speedbar-file-key-map "\C-f" 'my-speedbar-expand-line)
  (define-key speedbar-file-key-map [left] 'speedbar-contract-line)
  (define-key speedbar-file-key-map "\C-b" 'speedbar-contract-line)
  ;; BS でも上位ディレクトリへ ;;デフォルト: "U"
  (define-key speedbar-file-key-map [backspace] 'speedbar-up-directory)
  (define-key speedbar-file-key-map "\C-h" 'speedbar-up-directory)
  ;; 起動位置を直接指定する
  (setq speedbar-frame-parameters
        (append (list '(top . 40)
                      '(left . 780)
                      '(width . 25))
                speedbar-frame-parameters))
  ;; Speedbar で表示するファイルタイプ
  (setq speedbar-supported-extension-expressions
        (append '("*.*")))
  ) ;; end of speedbar

;; F4 で Speedbar
(global-set-key [(meta f4)] 'speedbar-get-focus)

;; ------------------------------------------------------------------------
;; @functions

;; http://d.hatena.ne.jp/kitokitoki/20100425/p1
(setq byte-compile-warnings '(free-vars unresolved callargs redefine obsolete noruntime cl-functions interactive-only make-local))

;; http://www.fan.gr.jp/~ring/doc/elisp_20/elisp_38.html#SEC609
;; 日付を挿入
(defun insert-current-date ()
  (interactive)
    (insert (format-time-string "%a, %b. %d, %Y")))

;; 現在時刻を挿入
(defun insert-current-time ()
  (interactive)
    (insert (format-time-string "%Y%m%d%H%M%S")))

;; すべてのバッファを kill
(defun kill-all-buffers ()
  (interactive)
  (loop for buffer being the buffers
        do (kill-buffer buffer)))

;; ケータイコーディング用
(defun ktai-hankaku-katakana-region (start end)
  (interactive "r")
  (while (string-match
          "[０-９Ａ-Ｚａ-ｚァ-ンー]+"
          (buffer-substring start end))
    (save-excursion
      (japanese-hankaku-region
       (+ start (match-beginning 0))
       (+ start (match-end 0))
       ))))

(defun ktai-hankaku-katakana-buffer ()
  (interactive)
  (ktai-hankaku-katakana-region (point-min) (point-max)))

;; リージョン内の文字幅をしらべる
(defun string-width-in-region (start end)
  (interactive "r")
  (princ (string-width (buffer-substring start end))))

;; ------------------------------------------------------------------------
;; @align

(require 'align)

;; Align for php-mode
;; http://d.hatena.ne.jp/Tetsujin/20070614/1181757931
(add-to-list 'align-rules-list
             '(php-assignment
               (regexp   . "[^-=!^&*+<>/.| \t\n]\\(\\s-*[.-=!^&*+<>/|]*\\)=>?\\(\\s-*\\)\\([^= \t\n]\\|$\\)")
               (justify  . t)
               (tab-stop . nil)
               (modes    . '(php-mode))))
(add-to-list 'align-dq-string-modes 'php-mode)
(add-to-list 'align-sq-string-modes 'php-mode)
(add-to-list 'align-open-comment-modes 'php-mode)
(setq align-region-separate (concat "\\(^\\s-*$\\)\\|"
                                    "\\([({}\\(/\*\\)]$\\)\\|"
                                    "\\(^\\s-*[)}\\(\*/\\)][,;]?$\\)\\|"
                                    "\\(^\\s-*\\(}\\|for\\|while\\|if\\|else\\|"
                                    "switch\\|case\\|break\\|continue\\|do\\) [ ;]\\)"
                                    ))

;; for ruby-mode
;; http://d.hatena.ne.jp/rubikitch/20080227/1204051280
(add-to-list 'align-rules-list
             '(ruby-comma-delimiter
               (regexp . ",\\(\\s-*\\) [^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list
             '(ruby-hash-literal
               (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list
             '(ruby-assignment-literal
               (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list          ;TODO add to rcodetools.el
             '(ruby-xmpfilter-mark
               (regexp . "\\(\\s-*\\)# => [^#\t\n]")
               (repeat . nil)
               (modes  . '(ruby-mode))))

;; for cperl-mode
(add-to-list 'align-rules-list
             '(perl-assignment
               (regexp   . "[^-=!^&*+<>/.| \t\n]\\(\\s-*[.-=!^&*+<>/|]*\\)=>?\\(\\s-*\\)\\([^= \t\n]\\|$\\)")
               (justify  . t)
               (tab-stop . nil)
               (modes    . '(cperl-mode))))
(add-to-list 'align-dq-string-modes 'cperl-mode)
(add-to-list 'align-sq-string-modes 'cperl-mode)
(add-to-list 'align-open-comment-modes 'cperl-mode)
(setq align-region-separate (concat "\\(^\\s-*$\\)\\|"
                                    "\\( [({}\\[\\]\\(/\*\\)]$ \\)\\|"
                                    "\\(^\\s-*[)}\\(\*/\\)][,;]?$\\)\\|"
                                    "\\(^\\s-*\\(}\\|for\\|while\\|if\\|else\\|"
                                    "switch\\|case\\|break\\|continue\\|do\\) [ ;]\\)"
                                    ))

;; ------------------------------------------------------------------------
;; @anything

;; Install
;; (auto-install-batch "anything")

(when (require 'anything nil t)

  (require 'anything-startup)

  (setq
   ;; 候補を表示するまでの時間。デフォルトは 0.5
   anything-idle-delay 0.3
   ;; タイプして再描写するまでの時間。デフォルトは 0.1
   anything-input-idle-delay 0.2
   ;; 候補の最大表示数。デフォルトは 50
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

;; ------------------------------------------------------------------------
;; @anything-c-moccur

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
  ;; C-M-o に anything-c-moccur-occur-by-moccur を割り当てる
  (global-set-key (kbd "C-M-o") 'anything-c-moccur-occur-by-moccur))

;; ------------------------------------------------------------------------
;; @auto-install

(require 'auto-install)

;; インストール先
(setq auto-install-directory "~/.emacs.d/elisp/")

;; 起動時に EmacsWiki のページ名を補完候補に加える
;; (auto-install-update-emacswiki-package-name t)

;; install.elisp.el 互換モードにする
(auto-install-compatibility-setup)

;; ------------------------------------------------------------------------
;; @auto-insert

;; ファイル形式に応じて自動でテンプレート挿入
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory "~/.emacs.d/templates")
(setq auto-insert-alist
      '((cperl-mode    . "perl-template.pl")
        (php-mode      . "php-template.php")
        (markdown-mode . "md_template.md")
        (ruby-mode     . "ruby-template.rb")
        (python-mode   . "python-template.py")
        (nxml-mode     . "html-template.html")))

;; ------------------------------------------------------------------------
;; @moccur-edit

;; [Summary]
;; M-x auto-install-from-emacswiki moccur-edit.el

(require 'moccur-edit)
(setq moccur-split-word t)
(require 'color-moccur)

;; ------------------------------------------------------------------------
;; @dired

;; dired-x
(require 'dired-x)

;; wdired
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; ファイルなら別バッファで、ディレクトリなら同じバッファで開く
;; http://d.hatena.ne.jp/nishikawasasaki/20120222/1329932699
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

;; http://d.hatena.ne.jp/oh_cannot_angel/20101216/1292506110
;; dired でマークをつけたファイルを開く
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "F") 'my-dired-find-marked-files)
     (defun my-dired-find-marked-files (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))

;; dired でマークをつけたファイルを view モードで開く
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "V") 'my-dired-view-marked-files)
     (defun my-dired-view-marked-files (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'view-file fn-list)))))

;; ------------------------------------------------------------------------
;; @grep

;; igrep
;; 1) M-x auto-install-from-emacswiki igrep.el
;; [Usage]
;; 1) M-x igrep-find
;; 2) 検索文字列を入力
;; 3) 検索ファイルを入力 (ex)/opt/local/apache2/htdocs/fw/*.php
;; [Memo]
;; xargs の -e オプションでエラーになるので xargs をオフに変更
(require 'igrep)
(igrep-define lgrep (igrep-use-zgrep nil) (igrep-regex-option "-n -Ou8"))
(igrep-find-define lgrep (igrep-use-zgrep nil) (igrep-regex-option "-n -Ou8"))
(setq igrep-find-use-xargs nil)

;; grep-edit
;; 1) M-x auto-install-from-emacswiki grep-edit.el
;; [Patch]
;; http://d.hatena.ne.jp/tomoya/20090826/1251261798
;; [Summary]
;; grep の検索結果を直接編集することで複数ファイルの一括置換が可能となる
;; [Usage]
;; C-c C-e 変更を反映する
;; C-c C-r リージョンの変更点を破棄する
;; C-c C-u 全変更点を破棄する
;; C-x s   変更されたファイルを保存
(require 'grep-edit)

;; ------------------------------------------------------------------------
;; @utility

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

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; sudo-ext
;; http://d.hatena.ne.jp/rubikitch/20101018/sudoext
;; (install-elisp-from-emacswiki "sudo-ext.el")
(require 'sudo-ext)

;; open-junk-file.el
;; (install-elisp-from-emacswiki "open-junk-file.el")
(require 'open-junk-file)
(setq open-junk-file-format "~/junk/%Y/%m/%Y%m%d_%H%M%S.")

;; ------------------------------------------------------------------------
;; @smartchr

;; http://sakito.jp/emacs/emacsobjectivec.html
;; http://tech.kayac.com/archive/emacs-tips-smartchr.html

(require 'smartchr)
(defun smartchr-custom-keybindings ()
  (local-set-key (kbd "=") (smartchr '("=" " = " " == " " === ")))
  ;; !! がカーソルの位置
  (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "[") (smartchr '("[`!!']" "[" "[[`!!']]")))
  (local-set-key (kbd "{") (smartchr '("{`!!'}" "{\n`!!'\n}" "{")))
  (local-set-key (kbd "`") (smartchr '("\``!!'\`" "\`")))
  (local-set-key (kbd "'") (smartchr '("\'`!!'\'" "\'")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
  (local-set-key (kbd ">") (smartchr '(">" " > " "->" " => " ">>")))
  )

(defun smartchr-custom-keybindings-objc ()
  (local-set-key (kbd "@") (smartchr '("@\"`!!'\"" "@")))
  )

(add-hook 'php-mode-hook 'smartchr-custom-keybindings)
(add-hook 'c-mode-common-hook 'smartchr-custom-keybindings)
(add-hook 'js2-mode-hook 'smartchr-custom-keybindings)
(add-hook 'ruby-mode-hook 'smartchr-custom-keybindings)
(add-hook 'cperl-mode-hook 'smartchr-custom-keybindings)
(add-hook 'python-mode-hook 'smartchr-custom-keybindings)
(add-hook 'objc-mode-hook 'smartchr-custom-keybindings-objc)

;; ------------------------------------------------------------------------
;; @text-adjust

;; http://d.hatena.ne.jp/rubikitch/20090220/text_adjust

;; 全角文字と半角文字の間に自動でスペースを開ける
;;
;; INSTALL
;; (install-elisp "http://taiyaki.org/elisp/text-adjust/src/text-adjust.el")
;; (install-elisp "http://taiyaki.org/elisp/mell/src/mell.el")

(require 'text-adjust)
;; (defun text-adjust-space-before-save-if-needed ()
;;   (when (memq major-mode
;;               '(org-mode text-mode mew-draft-mode myhatena-mode))
;;     (text-adjust-space-buffer)))
;;(add-hook 'before-save-hook 'text-adjust-space-before-save-if-needed)

;; ------------------------------------------------------------------------
;; @yasnippet

(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets" "~/.emacs.d/plugins/yasnippet/snippets"))
(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.

;; References
;; https://github.com/capitaomorte/yasnippet
;; http://yasnippet.googlecode.com/svn/trunk/doc/index.html
;; http://d.hatena.ne.jp/botchy/20080502/1209717204
;; http://yasnippet-doc-jp.googlecode.com/svn/trunk/doc-jp/index.html
;; http://sakito.jp/emacs/emacsobjectivec.html

;; ------------------------------------------------------------------------
;; @auto-complete

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
  (setq ac-auto-start nil)

  ;; 起動キーの設定
  (ac-set-trigger-key "TAB")

  ;; 候補の最大件数 デフォルトは 10 件
  (setq ac-candidate-max 20)

  ;; 補完を開始する文字数
  (setq ac-auto-start 1)

  ;; 補完リストが表示されるまでの時間
  (setq ac-auto-show-menu 0.5)

  (defun auto-complete-init-sources ()
    (setq ac-sources '(ac-source-yasnippet
                       ac-source-dictionary
                       ac-source-gtags
                       ac-source-words-in-buffer)))

  (auto-complete-init-sources)

  (add-to-list 'ac-modes 'emacs-lisp-mode)
  (add-to-list 'ac-modes 'nxml-mode)
  (add-to-list 'ac-modes 'js2-mode)
  (add-to-list 'ac-modes 'tmt-mode)
  (add-to-list 'ac-modes 'yaml-mode)
  (add-to-list 'ac-modes 'sh-mode)
  (add-to-list 'ac-modes 'python-2-mode)

  ;; company
  ;; (install-elisp "http://nschum.de/src/emacs/company-mode/company-0.5.tar.bz2")

  ;; ac-company
  ;; (install-elisp "https://raw.github.com/buzztaiki/auto-complete/master/ac-company.el")
  (require 'ac-company)

  ;; ac-python
  ;; http://d.hatena.ne.jp/CortYuming/20111224/p1#
  (require 'ac-python)

  ;; for emacs-lisp-mode
  (add-hook 'emacs-lisp-mode-hook
            '(lambda ()
               (auto-complete-init-sources)
               (add-to-list 'ac-sources 'ac-source-functions)
               (add-to-list 'ac-sources 'ac-source-symbols))))

;; ------------------------------------------------------------------------
;; @flymake

(require 'flymake)

;; 全てのファイルで flymake を有効化
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

;; js2 Flymake の初期化関数の定義
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
;; @js2-mode

;; References
;; http://d.hatena.ne.jp/m-hiyama/20080627/1214549228
;; http://d.hatena.ne.jp/speg03/20091011/1255244329

;; インデントの関数の再設定 (http://blog.kiftwi.net/2011/12/13/emacs-js2-mode-indent/)
;; Usage ブロック内で C-S-i
(setq-default js2-basic-offset 4)

(when (load "js2" t)
  (setq js2-bounce-indent-flag nil)
  (set-face-foreground 'js2-function-param-face (face-foreground font-lock-variable-name-face))
  (defun indent-and-back-to-indentation ()
    (interactive)
    (indent-for-tab-command)
    (let ((point-of-indentation
           (save-excursion
             (back-to-indentation)
             (point))))
      (skip-chars-forward "\s " point-of-indentation)))
  (define-key js2-mode-map (kbd "C-i") 'indent-and-back-to-indentation)
  (define-key js2-mode-map (kbd "C-m") nil)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

;; インデントの関数の再設定
(add-hook 'js2-mode-hook
          #'(lambda ()
              (require 'js)
              (setq js-indent-level 2
                    js-expr-indent-offset 2
                    indent-tabs-mode nil)
              (set (make-local-variable 'indent-line-function) 'js-indent-line)))

;; ------------------------------------------------------------------------
;; @zencoding-mode

;; (install-elisp "https://raw.github.com/rooney/zencoding/1f62291a67ee3ef86df0d4a2304395cdfb315b31/zencoding-mode.el")
;; http://code.google.com/p/zen-coding
;; http://code.google.com/p/zen-coding/wiki/ZenHTMLElementsEn

(require 'zencoding-mode)
(add-hook 'nxml-mode-hook 'zencoding-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(safe-local-variable-values (quote ((clmemo-mode . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-hook 'zencoding-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-z C-z") 'zencoding-expand-line)
             ))

;; ------------------------------------------------------------------------
;; @anything-for-tags

;; Anything から TAGS を利用しやすくするコマンド作成
(when (and (require 'anything-exuberant-ctags nil t)
           (require 'anything-gtags nil t))
  ;; anything-for-tags 用のソースを定義
  (setq anything-for-tags
        (list anything-c-source-imenu
              anything-c-source-gtags-select
              ;; etags を利用する場合はコメントを外す
              anything-c-source-etags-select
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
;; @ctags

(require 'ctags nil t)
(setq tags-revert-without-query t)
;; ctags を呼び出すコマンドライン. パスが通っていればフルパスでなくてもよい
;; etags 互換タグを利用する場合はコメントを外す
;; (setq ctags-command "ctags -e -R ")
;; anything-exuberant-ctags.el を利用しない場合はコメントアウトする
(setq ctags-command "ctags -R --fields=\"+afikKlmnsSzt\" ")
(global-set-key (kbd "<f5>") 'ctags-create-or-update-tags-table)

;; ------------------------------------------------------------------------
;; @gtags

;; [Usage]
;; 1) cd /path/to/pjhome
;; 2) gtags -v
;; 3) M-x gtags-mode (gtags-mode を設定していないモードの場合)
;;
;; http://www.bookshelf.jp/soft/meadow_42.html#SEC638
;; http://www.neutralworks.com/blog/emacs/emacsglobalgtags.html
;; http://uguisu.skr.jp/Windows/gtags.html

;; (autoload 'gtags-mode "gtags" "" t)
;; (setq gtags-mode-hook
;;       '(lambda ()
;;          (local-set-key "\M-t" 'gtags-find-tag)    ;; 関数の定義元へ移動
;;          (local-set-key "\M-r" 'gtags-find-rtag)   ;; 関数の参照元の一覧を表示
;;          (local-set-key "\M-s" 'gtags-find-symbol) ;; 変数の定義元と参照元の一覧を表示
;;          (local-set-key "\C-t" 'gtags-pop-stack)   ;; 前のバッファへ戻る
;;          ))

;; ;; 自動的に gtags-mode に切り替える
;; (add-hook 'c-mode-common-hook
;;           '(lambda ()
;;              (gtags-mode 1)
;;              (gtags-make-complete-list)
;;              ))
;; (add-hook 'php-mode-common-hook
;;           '(lambda ()
;;              (gtags-mode 1)
;;              (gtags-make-complete-list)
;;              ))
;; (add-hook 'java-mode-common-hook
;;           '(lambda ()
;;              (gtags-mode 1)
;;              (gtags-make-complete-list)
;;              ))

;; ------------------------------------------------------------------------
;; @nxml-mode

;; HTML 編集のデフォルトモードを nxml-mode にする
(add-to-list 'auto-mode-alist '("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . nxml-mode))

;; HTML5
;; $ cd ~/.emacs.d/plugins
;; $ git clone git://github.com/hober/html5-el.git
;; $ cd html5-el
;; $ make relaxng
(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files "~/.emacs.d/plugins/html5-el/schemas.xml"))
(require 'whattf-dt)

;;; nxml-mode の基本設定
;; </ を入力すると自動的にタグを閉じる
(setq nxml-slash-auto-complete-flag t)

;; M-TAB でタグを補完する
(setq nxml-bind-meta-tab-to-complete-flag t)

;; 子要素のインデント幅を設定する。初期値は 2
(setq nxml-child-indent 2)

;; 属性値のインデント幅を設定する。初期値は 4
(setq nxml-attribute-indent 0)

;; ------------------------------------------------------------------------
;; @markdown-mode

;; (install-elisp "http://jblevins.org/projects/markdown-mode/markdown-mode.el")
;; http://jblevins.org/projects/markdown-mode/
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdwn$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdt$" . markdown-mode))

;; org-mode の表作成を自動で利用可能にする
(add-hook 'markdown-mode-hook 'turn-on-orgtbl)

;; ------------------------------------------------------------------------
;; @objc-mode

;; objc-mode
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))

;; Xcode 上でコンパイル＆実行
(defun xcode-buildandrun ()
  (interactive)
  (do-applescript
   (format
    (concat
     "tell application \"Xcode\" to activate \r"
     "tell application \"System Events\" \r"
     "     tell process \"Xcode\" \r"
     "          key code 36 using {command down} \r"
     "    end tell \r"
     "end tell \r"
     ))))

;; Xcode にブレークポイント追加
;; http://d.hatena.ne.jp/kaniza/20090915/p1
(defun xcode-add-breakpoint-at-line ()
  (interactive)
  (let ((line (number-to-string (line-number-at-pos)))
        (file-path buffer-file-name))
    (do-applescript (concat
                     "tell application \"Xcode\"
        activate
        tell front project
          repeat with r in file references
            set p to full path of r
            if \"" file-path "\" = p then
              set bp to make new file breakpoint with properties {line number:" line "}
              set file reference of bp to r
              set enabled of bp to true
              exit repeat
            end if
         end repeat
       end tell
     end tell"))))

(add-hook 'objc-mode-hook
          (lambda ()
            (define-key objc-mode-map (kbd "C-c C-r") 'xcode-buildandrun)
            (define-key objc-mode-map (kbd "C-c C-b") 'xcode-add-breakpoint-at-line)
            ))

;; ------------------------------------------------------------------------
;; @Objective-C

;; http://sakito.jp/emacs/emacsobjectivec.html

;; (add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
;; (add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
;; (add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))

;; ;; 例えば #import <UIKit/UIKit.h> 等の記述から簡単にヘッダファイルを開きたい場合があります。
;; ;; ffap を利用すると C-x C-f するとカーソルがある部分を解析してファイルとして開こうとします。
;; ;; ffap は C-x C-f の挙動が変化するので注意してください。
;; (ffap-bindings)
;; ;; 探すパスは ffap-c-path で設定する
;; ;; (setq ffap-c-path
;; ;;     '("/usr/include" "/usr/local/include"))
;; ;; 新規ファイルの場合には確認する
;; (setq ffap-newfile-prompt t)
;; ;; ffap-kpathsea-expand-path で展開するパスの深さ
;; (setq ffap-kpathsea-depth 5)

;; ;; また、 .h を開いている時に対応する .m を開いたり、 .m を開いている時に
;; ;; 対応する .h を開いたりしたい場合、 ff-find-other-file を利用します。
;; ;; 以下のように設定すると C-c o で対応するファイルを開いてくれます。
;; (setq ff-other-file-alist
;;       '(("\\.mm?$" (".h"))
;;         ("\\.cc$"  (".hh" ".h"))
;;         ("\\.hh$"  (".cc" ".C"))

;;         ("\\.c$"   (".h"))
;;         ("\\.h$"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m" ".mm"))

;;         ("\\.C$"   (".H"  ".hh" ".h"))
;;         ("\\.H$"   (".C"  ".CC"))

;;         ("\\.CC$"  (".HH" ".H"  ".hh" ".h"))
;;         ("\\.HH$"  (".CC"))

;;         ("\\.cxx$" (".hh" ".h"))
;;         ("\\.cpp$" (".hpp" ".hh" ".h"))

;;         ("\\.hpp$" (".cpp" ".c"))))
;; (add-hook 'objc-mode-hook
;;           (lambda ()
;;             (define-key c-mode-base-map (kbd "C-c o") 'ff-find-other-file)
;;             ))

;; ;; ロード
;; (require 'ac-company)
;; ;; ac-company で company-xcode を有効にする
;; (ac-company-define-source ac-source-company-xcode company-xcode)
;; ;; objc-mode で補完候補を設定
;; (setq ac-modes (append ac-modes '(objc-mode)))
;; ;; hook
;; (add-hook 'objc-mode-hook
;;           (lambda ()
;;             (define-key objc-mode-map (kbd "\t") 'ac-complete)
;;             ;; XCode を利用した補完を有効にする
;;             (push 'ac-source-company-xcode ac-sources)
;;             ;; C++ のキーワード補完をする Objective-C++ を利用する人だけ設定してください
;;             ;; (push 'ac-source-c++-keywords ac-sources)
;;             ))

;; ;; etags-table の機能を有効にする
;; (require 'etags-table)
;; (add-to-list  'etags-table-alist
;;               '("\\.[mh]$" "~/.emacs.d/conf/tags/objc.TAGS"))
;; ;; auto-complete に etags の内容を認識させるための変数
;; ;; 以下の例だと 3 文字以上打たないと補完候補にならないように設定してあります。 requires の次の数字で指定します
;; (defvar ac-source-etags
;;   '((candidates . (lambda ()
;;                     (all-completions ac-target (tags-completion-table))))
;;     (candidate-face . ac-candidate-face)
;;     (selection-face . ac-selection-face)
;;     (requires . 3))
;;   "etags をソースにする")
;; ;; objc で etags からの補完を可能にする
;; (add-hook 'objc-mode-hook
;;           (lambda ()
;;             (push 'ac-source-etags ac-sources)))

;; (defun xcode:buildandrun ()
;;   (interactive)
;;   (do-applescript
;;    (format
;;     (concat
;;      "tell application \"Xcode\" to activate \r"
;;      "tell application \"System Events\" \r"
;;      "     tell process \"Xcode\" \r"
;;      "          key code 36 using {command down} \r"
;;      "    end tell \r"
;;      "end tell \r"
;;      ))))

;; (add-hook 'objc-mode-hook
;;           (lambda ()
;;             (define-key objc-mode-map (kbd "C-c C-r") 'xcode:buildandrun)
;;             ))

;; ------------------------------------------------------------------------
;; @org-mode

(require 'org-install)

(setq org-log-done t)
(setq org-tags-column 72)                 ; タグを表示する位置
(setq org-hide-leading-stars t)           ; 見出しの余計なアスタリスクを表示しない
                                        ;(set-face-foreground 'org-hide "#282828") ; 表示しないアスタリスクの色
(setq org-startup-truncated nil)          ; 開始時にツリーを閉じない
(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

                                        ; 検索対象にアーカイブを含める
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

;; key bindings
(global-set-key (kbd "C-c l") 'org-store-link)

;; file
(setq org-directory "~/note/")
(setq org-my-private-file (concat org-directory "private.org"))
(setq org-my-kayac-file (concat org-directory "kayac.org"))

(setq org-default-notes-file org-my-private-file)
(setq org-agenda-files
      (list org-my-private-file
            org-my-kayac-file
            ))

;; org-capture
(setq org-capture-templates
      '(("T" "Todo private" entry (file+olp org-my-private-file "Todo" "Future") "** TODO %?\n   %U\n   %i\n" :unnarrowed t)
        ("I" "Idea private" entry (file+headline org-my-private-file "Idea") "** %?\n   %T\n   %i\n" :unnarrowed t)
        ("M" "Memo private" entry (file+headline org-my-private-file "Memo") "** %?\n   %T\n   %i\n" :unnarrowed t)
        ("t" "Todo kayac" entry (file+olp org-my-kayac-file "Todo" "Future") "** TODO %?\n   %U\n   %i\n" :unnarrowed t)
        ("i" "Idea kayac" entry (file+headline org-my-kayac-file "Idea") "** %?\n   %U\n   %i\n" :unnarrowed t)
        ("m" "Memo kayac" entry (file+headline org-my-kayac-file "Memo") "** %?\n   %U\n   %i\n" :unnarrowed t)
        ))

;; todo
(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '((sequence "TODO (t)" "STARTED (s)" "WAITING (w)" "|" "DONE (x)" "CANCEL (c)")))

;; keybindings
(global-set-key (kbd "C-c A") 'org-agenda)
(add-hook 'org-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-m") 'org-return-indent)))

;; ------------------------------------------------------------------------
;; @perl

;; perl-mode を cperl-mode のエイリアスにする
(defalias 'perl-mode 'cperl-mode)
(require 'cperl-mode)

(add-to-list 'auto-mode-alist '("\\.pl$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pm$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.psgi$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.t$" . cperl-mode))

;; cperl-mode のインデント設定
(setq cperl-indent-level 4                         ; インデント幅を 4 にする
      cperl-continued-statement-offset 4           ; 継続する文のオフセット
      cperl-brace-offset -4                        ; ブレースのオフセット
      cperl-label-offset -4                        ; label のオフセット
      cperl-indent-parens-as-block t               ; 括弧もブロックとしてインデント
      cperl-close-paren-offset -4                  ; 閉じ括弧のオフセット n
      cperl-tab-always-indent t                    ; TAB をインデントにする
      cperl-indent-region-fix-constructs t
      cperl-highlight-variables-indiscriminately t ; スカラを常にハイライトする
      cperl-comment-column 40)

;; perl-completion の設定
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

;; http://d.hatena.ne.jp/hakutoitoi/20090208/1234069614
;; モジュールソースバッファの場合はその場で,
;; その他のバッファの場合は別ウィンドウに開く.

(put 'perl-module-thing 'end-op
     (lambda ()
       (re-search-forward "\\=[a-zA-Z][a-zA-Z0-9_:]*" nil t)))
(put 'perl-module-thing 'beginning-op
     (lambda ()
       (if (re-search-backward "[^a-zA-Z0-9_:]" nil t)
           (forward-char)
         (goto-char (point-min)))))

(defun perldoc-m ()
  (interactive)
  (let ((module (thing-at-point 'perl-module-thing))
        (pop-up-windows t)
        (cperl-mode-hook nil))
    (when (string= module "")
      (setq module (read-string "Module Name: ")))
    (let ((result (substring (shell-command-to-string (concat "perldoc -m " module)) 0 -1))
          (buffer (get-buffer-create (concat "*Perl " module "*")))
          (pop-or-set-flag (string-match "*Perl " (buffer-name))))
      (if (string-match "No module found for" result)
          (message "%s" result)
        (progn
          (with-current-buffer buffer
            (toggle-read-only -1)
            (erase-buffer)
            (insert result)
            (goto-char (point-min))
            (cperl-mode)
            (toggle-read-only 1)
            )
          (if pop-or-set-flag
              (switch-to-buffer buffer)
            (display-buffer buffer)))))))

;; コード整形
;; http://d.hatena.ne.jp/hakutoitoi/20090208/1234069614
;; install
;; 1) perltidy $ sudo port install p5-perl-tidy
;; 2)$HOME にフォーマットの設定ファイル .perltidyrc を置く
;; [Testing the .perltidyrc]
;; $ perltidy -dpro

(defun perltidy-region ()
  "Run perltidy based on $HOME/.perltidyrc on the current region."
  (interactive)
  (save-excursion
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)))

(defun perltidy-buffer ()
  "Run perltidy on the current buffer."
  (interactive)
  (save-excursion (mark-whole-buffer)
                  (perltidy-region)))

(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
                  (perltidy-region)))

(define-key cperl-mode-map (kbd "C-M-q") 'perltidy-region)

;; pod-mode
;; install
;; (install-elisp "http://github.com/renormalist/emacs-pod-mode/raw/master/pod-mode.el")
(require 'pod-mode)
(add-to-list 'auto-mode-alist '("\\.pod$" . pod-mode))
(add-hook 'pod-mode-hook
          '(lambda ()
             (progn
               (font-lock-mode)
               (auto-fill-mode 1)
               (flyspell-mode 1)
               (local-set-key (kbd "C-x m") 'perldoc-m)
               )))

;; set-perl5lib
;; install
;; (install-elisp "http://coderepos.org/share/browser/lang/elisp/set-perl5lib/set-perl5lib.el?format=txt")
(require 'set-perl5lib)

;; flymake
;; http://unknownplace.org/memo/2007/12/21#e001
(defvar flymake-perl-err-line-patterns
  '(("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\) [,.\n]" 2 3 nil 1)))

(defconst flymake-allowed-perl-file-name-masks
  '(("\\.pl$" flymake-perl-init)
    ("\\.pm$" flymake-perl-init)
    ("\\.t$" flymake-perl-init)))

(defun flymake-perl-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "perl" (list "-wc" local-file))))

(defun flymake-perl-load ()
  (interactive)
  (set-perl5lib)
  (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-perl-file-name-masks))
  (setq flymake-err-line-patterns flymake-perl-err-line-patterns)
  (flymake-mode t))

;; tmt-mode
;; for Text::MicroTemplate
;; install
;; (install-elisp "https://github.com/yoshiki/tmt-mode/raw/master/tmt-mode.el")

(autoload 'tmt-mode "tmt-mode"
  "Major mode for editing Text::MicroTemplate syntax")
(add-to-list 'auto-mode-alist '("\\.mt$" . tmt-mode))

;; perl-eval
(defun perl-eval (begin end)
  "Run selected region as Perl code"
  (interactive "r")
  (shell-command-on-region begin end "perl"))

;; hook
(add-hook
 'cperl-mode-hook
 '(lambda ()
    (progn
      'flymake-perl-load
      (local-set-key (kbd "C-x m") 'perldoc-m)
      (local-set-key (kbd "C-x C-e") 'perl-eval)
      )))

;; ------------------------------------------------------------------------
;; @php-mode

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

;; CakePHP

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
;; @python-mode

;; M-x package-install RET python-mode RET

;; (setq ipython-command (expand-file-name "~/.pythonbrew/pythons/Python-2.7.3/bin/ipython"))
;; (require 'ipython)

;; ------------------------------------------------------------------------
;; @ruby-mode

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
;; @text-mode

(add-hook 'text-mode-hook
          '(lambda ()
             (setq tab-width 4)
             (setq c-basic-offset 4)))

;; ------------------------------------------------------------------------
;; @yaml

(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (auto-complete-init-sources)
               (setq ac-sources '(ac-source-words-in-buffer)))))

;; ------------------------------------------------------------------------
;; @auto-highlight-symbol

;; (auto-install-from-url "https://raw.github.com/mitsuo-saito/auto-highlight-symbol-mode/master/auto-highlight-symbol.el")
;; (auto-install-from-url "https://raw.github.com/mitsuo-saito/auto-highlight-symbol-mode/master/auto-highlight-symbol-config.el")
;; http://hiroki.jp/2011/01/25/1561/#more-1561
;; https://github.com/mitsuo-saito/auto-highlight-symbol-mode
;; Note:
;; 変数上にカーソルをおいて C-x C-a をタイプすることで、現在ハイライトされている変数の名前を全部一括して変更できる。
;; ただし、表示されていない部分は変更されないので注意する必要がある。

(when (require 'auto-highlight-symbol nil t)
(global-auto-highlight-symbol-mode t))

;; ------------------------------------------------------------------------
;; @fill-column-indicator

;; http://www.emacswiki.org/FillColumnIndicator
;; Usage: M-x fci-mode

(when (require 'fill-column-indicator nil t)
(setq fci-rule-width 1)
(setq fci-rule-color "darkblue")
(setq-default fci-rule-column 80))

;; ------------------------------------------------------------------------
;; @foreign-regexp

;; https://github.com/k-talo/foreign-regexp.el
;; http://shibayu36.hatenablog.com/entry/2013/01/15/201827
;; http://fukuyama.co/foreign-regexp
;; http://emacswiki.org/emacs/RegularExpression

;; (install-elisp https://raw.github.com/k-talo/foreign-regexp.el/master/foreign-regexp.el)

(setq migemo-isearch-enable-p nil)
(require 'foreign-regexp)

(custom-set-variables
 '(foreign-regexp/regexp-type 'perl) ;; Choose by your preference.
 '(reb-re-syntax 'foreign-regexp))   ;; Tell re-builder to use foreign regexp.

;; ------------------------------------------------------------------------
;; @Auto Indentation

;; http://emacswiki.org/emacs/AutoIndentation

(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))

(add-hook 'lisp-mode-hook 'set-newline-and-indent)
(add-hook 'yaml-mode-hook 'set-newline-and-indent)
(add-hook 'js2-mode-hook 'set-newline-and-indent)
(add-hook 'cperl-mode-hook 'set-newline-and-indent)
(add-hook 'python-mode-hook 'set-newline-and-indent)
(add-hook 'ruby-mode-hook 'set-newline-and-indent)
(add-hook 'php-mode-hook 'set-newline-and-indent)
(add-hook 'nxml-mode-hook 'set-newline-and-indent)

;; ------------------------------------------------------------------------
;; @Auto Revert

;; http://yoshiori.github.com/blog/2013/01/31/file-update-emacs/
;; http://d.hatena.ne.jp/syohex/20130206/1360157000
;; http://d.hatena.ne.jp/tomoya/20100826/1282823932

;; 変更のあったファイルの自動再読み込み
(global-auto-revert-mode 1)
