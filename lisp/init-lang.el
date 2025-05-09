;; [[file:../init.org::*语言配置][语言配置:1]]
;;; init-lang.el --- Language specific configurations -*- lexical-binding: t -*-

;; General LSP settings (for lsp-mode, often a dependency)
(use-package lsp-mode
  :ensure nil ; It's likely a dependency, we're just configuring it
  :commands (lsp lsp-deferred) ; Ensures it's loaded when LSP commands are used
  :config
  (setq lsp-log-io nil)      ;; Disable LSP I/O logging for performance
  (setq lsp-idle-delay 0.6)  ;; Slightly increase idle delay (default 0.5s)
  (setq lsp-enable-file-watchers nil) ;; Disable file watchers for performance
  (setq lsp-enable-snippet nil) ;; Disable lsp-mode's snippets if you use others or none
  
  ;; Server installation settings
  (setq lsp-auto-configure t)  ;; Auto-configure LSP features
  (setq lsp-auto-guess-root t) ;; Auto-detect project root
  (setq lsp-enable-indentation t) ;; Enable LSP-based indentation
  (setq lsp-enable-on-type-formatting t) ;; Enable formatting on type
  (setq lsp-enable-symbol-highlighting t) ;; Enable symbol highlighting
  (setq lsp-enable-text-document-color t) ;; Enable color support
  (setq lsp-enable-semantic-highlighting t) ;; Enable semantic highlighting
  
  ;; Server installation preferences
  (setq lsp-install-server-command '("npm" "install" "-g")) ;; Use npm for server installation
  (setq lsp-server-install-dir (expand-file-name "lsp-servers" user-emacs-directory)) ;; Custom installation directory
  
  ;; Diagnostic settings
  (setq lsp-diagnostic-package :none) ;; Use built-in diagnostics
  (setq lsp-diagnostic-clean-after-change t) ;; Clean diagnostics after changes
  (setq lsp-diagnostic-disabled-modes '(org-mode markdown-mode)) ;; Disable diagnostics in certain modes
  )

;; 通用编程配置
(use-package editorconfig
  :ensure t
  :diminish
  :hook (prog-mode . editorconfig-mode))

(use-package format-all
  :ensure t
  :diminish
  :hook ((prog-mode . format-all-mode)
         (before-save . format-all-buffer))
  :config
  (setq format-all-formatters
        '(("Python" black))))

(use-package flycheck
  :ensure t
  :diminish
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-idle-change-delay 0.5)
  (custom-set-faces
   '(flycheck-error ((t (:underline (:style wave :color "#ff5555")))))
   '(flycheck-warning ((t (:underline (:style wave :color "#ffb86c")))))
   '(flycheck-info ((t (:underline (:style wave :color "#8be9fd")))))))

(use-package company
  :ensure t
  :diminish
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-tooltip-limit 10
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t)
  (setq company-backends
        '((company-capf
           company-dabbrev-code
           company-keywords
           company-files)
          (company-dabbrev))))

;; 加载语言特定配置
(require 'init-python)

(provide 'init-lang)
;; 语言配置:1 ends here
