;;; emacs-config.el --- Configuration file of Emacs itself. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; 将emacs中设置的选项保存在custom.el
(setq custom-file
      (expand-file-name "custom.el"
                        user-emacs-directory))

;;; Modify display
;; 修改显示效果
(add-hook 'prog-mode-hook #'show-paren-mode)   ;编程模式下高亮另一个括号
(setq make-backup-files nil)        ;关闭自动备份
(setq inhibit-startup-message t)    ;关闭开启信息页面
(defalias 'yes-or-no-p 'y-or-n-p)   ;用 y/n 代替 yes/or

;; 全局显示行号
(global-display-line-numbers-mode 1)
;;(global-linum-mode t) broken in the newer version

;; 全局高亮当前行
(global-hl-line-mode t)

;; 设置默认的 高宽为30x80
(setq default-frame-alist '((width . 80)(height . 30)))

;; 设置显示空白字符
(if window-system                       ;由于company 的原因，在非gui界面。whitespace-mode 和 company 无法正常搭配使用。
    (progn
      (setq whitespace-style
            '(face
              tabs  ; 高亮tab
              trailing ;高亮行尾的空格
              lines ; 高亮过长的行
              ;; 根据indent-tabs-mode,高亮tab或者tab前/后的空格
              space-before-tab
              space-after-tab
              ;; 根据indent-tabs-mode,高亮行首的tab or spaces.
              indentation
              ;; 高亮换行？
              newline
              ;; 显示空行
              empty
              ;; 突出显示，文件结尾不是换行符的
              missing-newline-at-eof
              ;; 用特殊字符显示tab和换行符
              tab-mark newline-mark))
      (global-whitespace-mode)))

;; 设置显示更好看的字符
;;(setq prettify-symbols-mode t)

;; 设置tab为4个空格
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)


;;; set key-bind
;;; by myself

(global-set-key (kbd "C-x r D") 'bookmark-delete)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)


;;; custom function

(defun print-key-name ()
  "Print the name of the key pressed."
  (interactive)
  (let ((key (read-key-sequence "Press a key: ")))
    (print (key-description key))))

(provide 'emacs-config)
;;; emacs-config.el ends here
