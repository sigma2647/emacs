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

  ;; Enable relative line numbers for non-org and non-markdown files
  (defun enable-relative-line-numbers ()
    (unless (or (derived-mode-p 'org-mode)
                (derived-mode-p 'markdown-mode)
                (derived-mode-p 'gfm-mode))
      (display-line-numbers-mode 1)
      (setq display-line-numbers-type 'relative)))

  ;; Add hook to enable relative line numbers
  (add-hook 'prog-mode-hook 'enable-relative-line-numbers)
  (add-hook 'text-mode-hook 'enable-relative-line-numbers)

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
