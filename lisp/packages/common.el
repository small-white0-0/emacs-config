;;; common.el --- some frequently-used packages

;;; Commentary:
;;; Add some packages.

;;; Code:

;; test speed of start.
(use-package benchmark-init
  :if nil
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate))

;; provide keyboard shortcut tips
(use-package which-key
  :defer nil
  :functions which-key-mode              ; to make the warning disappear
  :config (which-key-mode))

;; 竖式炸开minibuffer 提示信息
(use-package vertico
  :if nil
  :ensure t
  :defer nil
  :custom (vertico-cycle t)
  :functions (vertico-mode)
  :config (vertico-mode 1))

;; provide more information in the minibuffer.
(use-package marginalia
  :if nil
  :ensure t
  :defer t
  :functions (marginalia-mode)
  :config (marginalia-mode 1))

;; Make completetions of minibuffer better.
;; Find commands by regex and literal matching.
(use-package orderless
  :if nil
  :ensure t
  :defer nil
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; fast switch window
(use-package ace-window
  :functions ace-window                  ; to make the warning disappear
  :bind (("M-o" . 'ace-window)))

;; syntax check
(use-package flycheck
  :if nil    ;; close, because eglot use flymake.
  :ensure t
  :init (global-flycheck-mode))

;; command history
(use-package amx
  :ensure t
  :commands amx-mode
  :init (amx-mode))

;; Optimize C-a and C-e.
;; C-a once, like 'back-to-indentation';C-a twice, like 'move-beginning-of-line'
;; C-e is similar to C-a.
(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;; hydra, need not to explain.
(use-package hydra
  :ensure t)

;; provide better undo and redo
(use-package undo-tree
  :ensure t
  :after hydra
  :functions defhydra global-undo-tree-mode
  :init
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil)
  :bind ("C-x C-h u" . hydra-undo-tree/body)
  :config
  (defhydra hydra-undo-tree (:hint nil)
    "
  _p_: undo  _n_: redo _s_: save _l_: load
  "
    ("p"   undo-tree-undo)
    ("n"   undo-tree-redo)
    ("s"   undo-tree-save-history)
    ("l"   undo-tree-load-history)
    ("u"   undo-tree-visualize "visualize" :color blue)
    ("q"   nil "quit" :color blue)))


(provide 'packages/common)
;;; common.el ends here
