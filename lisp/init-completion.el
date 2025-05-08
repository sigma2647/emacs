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
  (completion-category-overrides '((file (styles basic-remote orderless)))))

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
  :init
  (savehist-mode)
  :custom
  (savehist-additional-variables '(extended-command-history)))

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

(provide 'init-completion)
;; 补全系统配置:1 ends here
