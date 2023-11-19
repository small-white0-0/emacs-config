;;; simple-lang.el -- basic configuration of lang -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure eglot and project which are builtin.

;;; Code:

(use-module/require packages/base-mode)

(use-package eglot
  :defer t
  :hook
  ((c-mode rust-mode c++-mode). eglot-ensure)
  :bind
  (:map eglot-mode-map
        ("C-x C-a a" . eglot-code-action-actions)
        ("C-x C-a f" . eglot-code-action-quickfix)
        ("C-x C-a e" . eglot-code-action-extract)
        ("C-x C-a l" . eglot-code-action-inline)
        ("C-x C-a r" . eglot-code-action-rewrite)
        ("C-x C-a i" . eglot-code-action-organize-imports)))

(use-package project
  :defer t
  :ensure nil
  :init
  ;; 添加project 根目录判断标志文件
  (setq project-vc-extra-root-markers '(;; by build tools
                                        "go.mod"        ; go
                                        "Cargo.toml"    ; rust
                                        "project.clj"   ;??
                                        "pom.xml"       ; maven of java
                                        "package.json"  ; js
                                        "Makefile"      ; makefile of c/c++
                                        ;; by explain file
                                        "README.org"
                                        "README.md")))

(provide 'packages/simple-lang)
;;; simple-lang.el ends here
