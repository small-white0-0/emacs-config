;;; base-mode.el -- provide extra mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Nothing.

;;; Code:

;;;###autoload
(define-minor-mode concerned-prog-mode
  "Just start in the part of modes derived from `prog-mode`.")

(use-package concerned-prog-mode
  ;; virtual package, prevent load "concerd-prog-mode" package.
  :ensure use-package
  :defer t
  :hook
  (cons
   (java-mode
    c++-mode
    c-mode
    lisp-mode)
   concerned-prog-mode))

(use-package rust-mode
  :ensure t
  :hook
  (rust-mode . concerned-prog-mode))

;; use pandoc to preview for markdown document.
(use-package markdown-mode
  :ensure t
  :if nil
  :mode ("README\\.md\\'" . gfm-mode))

(provide 'packages/base-mode)
;;; base-mode.el ends here
