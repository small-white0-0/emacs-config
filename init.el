;;; init.el --- emacs startup file -*- lexical-binding: t; -*-

;;; Commentary:
;; let flycheck's warning disappear.

;;; Code:
(require 'use-module
         (expand-file-name "lisp/use-module.el" user-emacs-directory))
(add-to-list 'load-path lisp-dir)

;; other import
(use-module (emacs-config))
(use-module (packages))
(use-module (packages base-mode))
(use-module (packages ui))
(use-module (packages common))
(use-module (packages flymake-checker))
(use-module (packages complete))
(use-module (packages simple-lang))
(use-module (packages meow-config))
(use-module (packages latex-config))

;; if custome-file isn't `nil` and custome-file exists, load it.
(and custom-file (file-exists-p custom-file) (load-file custom-file))

;;; init.el ends here
