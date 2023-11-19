;;; complete.el --- complete configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Provide auto completion by company and yasnippet.
;; There are based on company.

;;; Code:

(use-module/require packages/flymake-checker
                    packages/base-mode)

(use-package company
  :ensure t
  :defer t
  :config
  (setq company-dabbrev-code-everywhere t
        company-dabbrev-code-modes t
        company-dabbrev-code-other-buffers 'all
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-dabbrev-other-buffers 'all
        company-require-match nil
        company-minimum-prefix-length 2
        company-show-quick-access t
        company-tooltip-limit 20
        company-idle-delay 0
        company-echo-delay 0
        company-tooltip-offset-display 'scrollbar
        company-begin-commands '(self-insert-command))
  :hook (;;to enable company-tng-mode first and then enable company-mode.
         (company-tng-mode . company-mode)
         (prog-mode . company-tng-mode)))

;; provide snippet only when use company.
(use-package yasnippet
  :ensure t
  :defer t
  :after company
  :defines (yas-minor-mode-map company-backends)
  :hook
  ;; (company-mode . yas-minor-mode)
  (concerned-prog-mode . yas-minor-mode)
  :config
  ;; add company-yasnippet to company-backends
  (defun company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends
        (mapcar #'company-mode/backend-with-yas company-backends))
  ;; unbind <TAB> completion in yas-minor-mode-map.
  (define-key yas-minor-mode-map [(tab)]        nil)
  (define-key yas-minor-mode-map (kbd "TAB")    nil)
  (define-key yas-minor-mode-map (kbd "<tab>")  nil))

(use-package yasnippet-snippets
  :ensure t
  :requires yasnippet)

(provide 'packages/complete)
;;; complete.el ends here
