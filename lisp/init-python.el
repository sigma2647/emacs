;; [[file:../init.org::*Python 配置][Python 配置:1]]
;;; init-python.el --- Python specific configurations -*- lexical-binding: t -*-

(use-package python
  :ensure t
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (setq python-indent-offset 4)
  
  ;; 代码格式化
  (use-package blacken
    :ensure t
    :diminish
    :hook (python-mode . blacken-mode)
    :config 
    (setq blacken-line-length 88
          blacken-allow-py36 t
          blacken-skip-string-normalization t))

  ;; LSP 集成
  (use-package lsp-pyright
    :ensure t
    :after python
    :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))
    :config
    (setq lsp-pyright-use-library-code-for-types t
          lsp-pyright-diagnostic-mode "workspace"
          lsp-pyright-typechecking-mode "basic"
          lsp-pyright-auto-import-completions t
          lsp-pyright-auto-search-paths t)))

(provide 'init-python)
;; Python 配置:1 ends here
