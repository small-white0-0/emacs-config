;;; packages.el --- Initialize use-package -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; add mirrors url
(setq package-archives
      '(("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("org"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
        ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))

(require 'package)

;; Initialize package
(unless (bound-and-true-p package-initialized)
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)	; 默认 :ensure 为t
  (setq use-package-always-defer t)	; 默认所有为迟加载
  (setq use-package-verbose t))		;加载超时 0.1s,报告

;; provide :ensure-system-package keyword
(use-package use-package-ensure-system-package
  :defer nil
  :ensure t
  :config
  (if (executable-find "guix") ;; the config require guix
      (let* ((profiles-dir
              (concat (getenv "HOME") "/guix-profiles"))
             (emacs-profile
              (concat profiles-dir "/emacs-profile"))
             (pkg-mgr
              (concat "guix package --profile="
                      emacs-profile)))
        (if (not (file-exists-p profiles-dir))
            (mkdir profiles-dir))
        (defvar guix-emacs-load-path
          (concat emacs-profile "/share/emacs/site-lisp"))
        (defvar guix-emacs-exec-path
          (concat emacs-profile "/bin"))
        (defvar system-packages-supported-package-managers
          `((guix .
                  ((default-sudo . nil)
                   (install . ,(concat pkg-mgr " -i"))
                   (search . ,(concat pkg-mgr " -s"))
                   (uninstall . ,(concat pkg-mgr " -r"))
                   (update . ,(concat pkg-mgr " --upgrade"))
                   (clean-cache . ,(concat pkg-mgr " gc"))
                   (log . nil)
                   (get-info . nil)
                   (get-info-remote . nil)
                   (list-files-provided-by . nil)
                   (verify-all-packages . nil)
                   (verify-all-dependencies . nil)
                   (remove-orphaned . nil)
                   (list-installed-packages . ,(concat pkg-mgr " -I"))
                   (list-installed-packages-all . ,(concat pkg-mgr " -I"))
                   (list-dependencies-of . nil)
                   (noconfirm . nil)))))
        (add-to-list 'exec-path guix-emacs-exec-path))))

;; install quelpa and integrated to use-package.
(use-package quelpa
  :ensure t)
(use-package quelpa-use-package
  :ensure t)

(provide 'packages)
;;; packages.el ends here
