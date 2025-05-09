;; [[file:../init.org::*补全系统配置][补全系统配置:1]]
;;; init-completion.el --- Initialize completion configurations -*- lexical-binding: t -*-

;; 启用 vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)  ;; 循环选择
  (vertico-resize t) ;; 自动调整大小
  (vertico-count 10) ;; 显示10个候选项
  (vertico-scroll-margin 0)) ;; 滚动边距

;; 启用 orderless 补全风格
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles orderless)))))

;; 配置 M-x 命令补全
(defun my/vertico-command-completion ()
  "Configure command completion for M-x."
  (setq-local vertico-sort-function nil)  ;; 禁用排序
  (setq-local vertico-group-format #'vertico-group-format)  ;; 启用分组
  (setq-local vertico-count-format #'vertico-count-format))  ;; 启用计数

(add-hook 'vertico-mode-hook #'my/vertico-command-completion)

;; 配置 M-x 命令历史
(use-package savehist
  :ensure t
  :init (savehist-mode 1)
  :custom
  (savehist-additional-variables '(extended-command-history file-name-history)))

;; 配置命令建议
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  :custom
  (marginalia-align 'right)
  (marginalia-annotators '(marginalia-annotators-heavy)))

;; 配置命令提示
(use-package consult
  :ensure t
  :custom
  (consult-preview-key nil)  ;; 禁用预览
  (consult-narrow-key "<")   ;; 设置窄化键
  (consult-widen-key ">"))   ;; 设置扩展键

;; 文件补全增强
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; 文件补全历史记录
(use-package recentf
  :ensure t
  :init
  (recentf-mode)
  :custom
  (recentf-max-saved-items 100)
  (recentf-exclude '("/tmp/" "/var/tmp/" ".git/" ".cache/"))
  (recentf-auto-cleanup 'never))

;; 确保文件补全工作
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; 启用部分补全
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles orderless))))

;; 启用文件补全缓存
(setq file-name-history-file (expand-file-name "file-name-history" user-emacs-directory))

(provide 'init-completion)
;; 补全系统配置:1 ends here
