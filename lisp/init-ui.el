;; [[file:../init.org::*UI 配置][UI 配置:1]]
;; Enable pixel scroll
  (pixel-scroll-precision-mode 1)
  
  ;; Enable smooth scrolling
  (setq pixel-scroll-precision-interpolate-page t)
  
  ;; Use pixel scroll for all scrolling commands
  (defalias 'scroll-up-command 'pixel-scroll-interpolate-down)
  (defalias 'scroll-down-command 'pixel-scroll-interpolate-up)
  
  ;; Additional scroll settings for better experience
  (setq scroll-conservatively 101)  ;; Don't recenter point
  (setq scroll-margin 0)            ;; No margin when scrolling
  (setq scroll-preserve-screen-position t)  ;; Keep cursor position relative to screen
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))  ;; Fine-tune mouse wheel scrolling
  (setq mouse-wheel-progressive-speed nil)  ;; Disable progressive speed

  ;; 配置行号显示
  (setq display-line-numbers-type 'relative)  ;; 设置相对行号
  (global-display-line-numbers-mode 1)        ;; 全局启用行号

  ;; 在特定模式下禁用行号
  (dolist (mode '(org-mode markdown-mode gfm-mode dashboard-mode vterm-mode eshell-mode shell-mode term-mode))
    (add-hook (intern (format "%s-hook" mode)) 
              (lambda () (display-line-numbers-mode 0))))

  ;; 配置 doom-modeline
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)                ;; 设置高度
  (doom-modeline-bar-width 3)              ;; 设置指示条宽度
  (doom-modeline-icon t)                   ;; 显示图标
  (doom-modeline-major-mode-icon t)        ;; 显示主模式图标
  (doom-modeline-buffer-encoding t)        ;; 显示编码
  (doom-modeline-indent-info t)            ;; 显示缩进信息
  (doom-modeline-lsp t)                    ;; 显示 LSP 状态
  (doom-modeline-time t)                   ;; 显示时间
  (doom-modeline-time-24hr t))             ;; 使用24小时制

  (provide 'init-ui)
;; UI 配置:1 ends here
