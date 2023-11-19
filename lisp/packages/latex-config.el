;;; latex-config.el --- configuration for latex -*- lexical-binding: t; -*-

;;; Commentary:
;; There are AUCTeX.

;;; Code:

(use-package latex
  :defer t
  :ensure auctex
  :custom
  (TeX-PDF-mode t)
  (TeX-DVI-via-PDFTeX t)
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  :hook
  ;; There isn't `LaTeX-mode` really, but there is `LaTeX-mode-hook` in auctex.
  ;; Its name has the same one in the buildin latex module. Care about it!
  ;; (LaTeX-mode . turn-on-cdlatex)
  (LaTeX-mode . reftex-mode)
  :config
  ;; add xelatex to compile commands.
  (add-to-list 'TeX-command-list
               '("XeLaTeX" "%l%`xelatex --synctex=1%(mode)%' %T" TeX-run-TeX t t))
  ;; add latexmk with xelatex to compile commands.
  (add-to-list 'TeX-command-list
               '("LaTeXmk-xelatex" "%`latexmk -pdf -xelatex --synctex=1%(mode)%(file-line-error)%' %t" TeX-run-TeX t t))

  (setq-default TeX-master t) ; 默认询问主文件
  )

(use-package cdlatex
  :after latex
  :config
  (add-hook 'cdlatex-tab-hook 'indent-for-tab-command)
  :hook
  (LaTeX-mode . cdlatex-mode))

(provide 'packages/latex-config)
;;; latex-config.el ends here
