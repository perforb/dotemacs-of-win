;;
;; auto-save-buffers.el
;;
;; ¸µ¤Υ³¡¼¥ɤϻ³²¬¹îÈþ»᤬½񤤤Ƥ¯¤À¤µ¤ä¿ (ELF:01128)
;;
;; »Ȥ¤Êý:
;;
;;   (require 'auto-save-buffers)
;;   (run-with-idle-timer 0.5 t 'auto-save-buffers) ; ¥¢¥¤¥ɥë0.5ÉäÇÊÝ¸
;;
;; auto-save-buffers ¤Î on/off ¤òÀڤêÂؤ¨¤뤿¤á¤Υ­¡¼ÄêµÁ (C-x a s)
;;
;;   (define-key ctl-x-map "as" 'auto-save-buffers-toggle)
;;

;; 2005-01-16 02:55:33 ¥ե¡¥¤¥ëÊÝ¸»þ¤Υá¥å»¡¼¥¸¤ò½Фµ¤ʤ¤¤褦¤ËÊѹ¹ by okuyama

;; auto-save-buffers ¤ÇÂоݤȤ¹¤ë¥ե¡¥¤¥ë̾¤ÎÀµµ¬ɽ¸½
(defvar auto-save-buffers-regexp ""
  "*Regexp that matches `buffer-file-name' to be auto-saved.")

;; auto-save-buffers ¤ǽü³°¤¹¤ë¥ե¡¥¤¥ë̾¤ÎÀµµ¬ɽ¸½
(defvar auto-save-buffers-exclude-regexp "^$"
  "*Regexp that matches `buffer-file-name' not to be auto-saved.")

;;
;; ¤¢¤뤤¤Ï auto-save-buffers ¤ΰú¿ô¤ÇÀµµ¬ɽ¸½¤ò»ØÄꤹ¤뤳¤Ȥâ¤Ǥ­¤ë
;;
;; (require 'auto-save-buffers)
;; (run-with-idle-timer 0.5 t 'auto-save-buffers "\\.c$" "^$") ; .c ¤À¤±ÂоÝ
;; (run-with-idle-timer 0.5 t 'auto-save-buffers ""   "\\.h$") ; .h ¤À¤±½ü³°
;;

;; nil ¤ʤ饻¡¼¥֤·¤ʤ¤ (¥¿¥¤¥ޡ¼¤ϲó¤ä¿¤ޤÞ)
(defvar auto-save-buffers-active-p t
  "If non-nil, `auto-save-buffers' saves buffers.")

;; ¥ª¥ꥸ¥ʥë¤Î write-region ¤òÂàÈò
(fset 'original-write-region (symbol-function 'write-region))

;; ¥á¥å»¡¼¥¸¤ò½Фµ¤ʤ¤ write-region ¤òºîÀ®
(defun auto-save-buffers-write-region (start end filename &optional append
                                             visit lockname mustbenew)
  (original-write-region start end filename append
                         (cond ((stringp visit) visit)
                               ((not visit) nil)
                               (t 'BeQuiet)) lockname mustbenew))

;; ¾Êά²Äǽ¤ΰú¿ô¤ǡ¢include/exclude ÍѤÎÀµµ¬ɽ¸½¤ò»ØÄê¤Ǥ­¤ë
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
                       (not (buffer-base-buffer)) ;; ´ðÄì¥Хåե¡¤ΤßÊÝ¸
                       (file-writable-p buffer-file-name))
              (basic-save-buffer)
              (set-visited-file-modtime)
              (set-buffer-modified-p nil))
            (setq buffers (cdr buffers))))
      (fset 'write-region (symbol-function 'original-write-region)))))

;; auto-save-buffers ¤Î on/off ¤ò¥ȥ°¥ë¤ÇÀڤêÂؤ¨¤ë
;; Based on the code by Yoshihiro (¤¤¤ä¤ÊÆüµ­ 2004-03-23)
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
;; Emacs 21 °ʹߤÇ Makefile ¤ÎÊԽ¸»þ¤Ë "Suspicious line XXX. Save anyway"
;; ¤Ȥ¤¤¦¥ץí¥ó¥ץȤò½Фµ¤ʤ¤¤褦¤ˤ¹¤뤿¤á¤Τª¤ޤ¸¤ʤ¤
;;
(add-hook 'makefile-mode-hook
          (function (lambda ()
                      (fset 'makefile-warn-suspicious-lines 'ignore))))

(provide 'auto-save-buffers)
