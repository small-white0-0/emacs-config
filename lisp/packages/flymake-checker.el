;;; flymake-checker.el --- configure flymake -*- lexical-binding: t; -*-
;;; Commentary:
;; configure flymake.

;;; Code:

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error)))

(use-module/conflict flycheck)
(provide 'packages/flymake-checker)
;;; flymake-checker.el ends here
