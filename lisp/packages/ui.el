;;; ui.el --- config emacs ui
;;; Commentary:

;;; Code:

;;; nice UI
;;; config from 第253页图灵笔记
(use-package gruvbox-theme
  :init (load-theme 'gruvbox-dark-soft t))

(use-package smart-mode-line
  :defines (sml/no-confirm-load-theme sml/theme)
  :functions (sml/setup)
  :init
  (setq column-number-mode t
        sml/no-confirm-load-theme t
        sml/theme 'respectful)
  (sml/setup))

;; rainbow racket
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; provide icon for company.
(use-package company-box
  :if window-system
  :after company  ;; load after company. if there isn't company, it won't load.
  :ensure t
  :hook (company-mode . company-box-mode))

;; provide icons fonts
(use-package all-the-icons
  :if (display-graphic-p))

(provide 'packages/ui)
;;; ui.el ends here
