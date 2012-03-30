;;
;; auto-save-buffers.el
;;
;; ｸｵ､ﾎ･ｳ｡ｼ･ﾉ､ﾏｻｳｲｬｹ鏸�ｻ皃ｬｽ､､ﾆ､ｯ､ﾀ､ｵ､ﾃ､ｿ (ELF:01128)
;;
;; ｻﾈ､､ﾊ�:
;;
;;   (require 'auto-save-buffers)
;;   (run-with-idle-timer 0.5 t 'auto-save-buffers) ; ･｢･､･ﾉ･�0.5ﾉﾃ､ﾇﾊﾝﾂｸ
;;
;; auto-save-buffers ､ﾎ on/off ､ﾚ､�ﾂﾘ､ｨ､�､ｿ､皃ﾎ･ｭ｡ｼﾄ�ｵﾁ (C-x a s)
;;
;;   (define-key ctl-x-map "as" 'auto-save-buffers-toggle)
;;

;; 2005-01-16 02:55:33 ･ﾕ･｡･､･�ﾊﾝﾂｸｻ�､ﾎ･皈ﾃ･ｻ｡ｼ･ｸ､ﾐ､ｵ､ﾊ､､､隍ｦ､ﾋﾊﾑｹｹ by okuyama

;; auto-save-buffers ､ﾇﾂﾐｾﾝ､ﾈ､ｹ､�･ﾕ･｡･､･�ﾌｾ､ﾎﾀｵｵｬﾉｽｸｽ
(defvar auto-save-buffers-regexp ""
  "*Regexp that matches `buffer-file-name' to be auto-saved.")

;; auto-save-buffers ､ﾇｽ�ｳｰ､ｹ､�･ﾕ･｡･､･�ﾌｾ､ﾎﾀｵｵｬﾉｽｸｽ
(defvar auto-save-buffers-exclude-regexp "^$"
  "*Regexp that matches `buffer-file-name' not to be auto-saved.")

;;
;; ､｢､�､､､ﾏ auto-save-buffers ､ﾎｰ惞ﾇﾀｵｵｬﾉｽｸｽ､ﾘﾄ熙ｹ､�､ｳ､ﾈ､筅ﾇ､ｭ､�
;;
;; (require 'auto-save-buffers)
;; (run-with-idle-timer 0.5 t 'auto-save-buffers "\\.c$" "^$") ; .c ､ﾀ､ｱﾂﾐｾﾝ
;; (run-with-idle-timer 0.5 t 'auto-save-buffers ""   "\\.h$") ; .h ､ﾀ､ｱｽ�ｳｰ
;;

;; nil ､ﾊ､鬣ｻ｡ｼ･ﾖ､ｷ､ﾊ､､ (･ｿ･､･ﾞ｡ｼ､ﾏｲﾃ､ｿ､ﾞ､ﾞ)
(defvar auto-save-buffers-active-p t
  "If non-nil, `auto-save-buffers' saves buffers.")

;; ･ｪ･�･ｸ･ﾊ･�､ﾎ write-region ､猜�
(fset 'original-write-region (symbol-function 'write-region))

;; ･皈ﾃ･ｻ｡ｼ･ｸ､ﾐ､ｵ､ﾊ､､ write-region ､鋿ｮ
(defun auto-save-buffers-write-region (start end filename &optional append
                                             visit lockname mustbenew)
  (original-write-region start end filename append
                         (cond ((stringp visit) visit)
                               ((not visit) nil)
                               (t 'BeQuiet)) lockname mustbenew))

;; ｾﾊﾎｬｲﾄﾇｽ､ﾎｰ惞ﾇ｡｢include/exclude ﾍﾑ､ﾎﾀｵｵｬﾉｽｸｽ､ﾘﾄ熙ﾇ､ｭ､�
(defun auto-save-buffers (&rest regexps)
  "Save buffers if `buffer-file-name' matches `auto-save-buffers-regexp'."
  (let ((include-regexp (or (car  regexps) auto-save-buffers-regexp))
        (exclude-regexp (or (cadr regexps) auto-save-buffers-exclude-regexp))
        (buffers (buffer-list)))
    (unwind-protect
        (save-excursion
          (fset 'write-region (symbol-function 'auto-save-buffers-write-region))
          (while buffers
            (set-buffer (car buffers))
            (when (and buffer-file-name
                       auto-save-buffers-active-p
                       (buffer-modified-p)
                       (not buffer-read-only)
                       (string-match include-regexp buffer-file-name)
                       (not (string-match exclude-regexp buffer-file-name))
                       (not (buffer-base-buffer)) ;; ｴ�･ﾐ･ﾃ･ﾕ･｡､ﾎ､ﾟﾊﾝﾂｸ
                       (file-writable-p buffer-file-name))
              (basic-save-buffer)
              (set-visited-file-modtime)
              (set-buffer-modified-p nil))
            (setq buffers (cdr buffers))))
      (fset 'write-region (symbol-function 'original-write-region)))))

;; auto-save-buffers ､ﾎ on/off ､ﾈ･ｰ･�､ﾇﾀﾚ､�ﾂﾘ､ｨ､�
;; Based on the code by Yoshihiro (､､､荀ﾊﾆ�ｵｭ 2004-03-23)
(defun auto-save-buffers-toggle ()
  "Toggle `auto-save-buffers'"
  (interactive)
  (if auto-save-buffers-active-p
      (setq auto-save-buffers-active-p nil)
    (setq auto-save-buffers-active-p t))
  (if auto-save-buffers-active-p
      (message "auto-save-buffers on")
    (message "auto-save-buffers off")))

;;
;; Emacs 21 ｰﾊｹﾟ､ﾇ Makefile ､ﾎﾊﾔｽｸｻ�､ﾋ "Suspicious line XXX. Save anyway"
;; ､ﾈ､､､ｦ･ﾗ･愠ﾗ･ﾈ､ﾐ､ｵ､ﾊ､､､隍ｦ､ﾋ､ｹ､�､ｿ､皃ﾎ､ｪ､ﾞ､ｸ､ﾊ､､
;;
(add-hook 'makefile-mode-hook
          (function (lambda ()
                      (fset 'makefile-warn-suspicious-lines 'ignore))))

(provide 'auto-save-buffers)
