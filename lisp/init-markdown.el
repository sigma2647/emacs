;; [[file:../init.org::*Markdown 配置][Markdown 配置:1]]
;;; init-markdown.el --- Initialize markdown configurations -*- lexical-binding: t -*-

(use-package markdown-mode
  :ensure t
  :defer t  ;; 延迟加载
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :init
  (setq markdown-command "multimarkdown")
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  (markdown-hide-urls t)
  (markdown-indent-on-enter 'indent-and-new-item)
  (markdown-list-indent-width 2))

;; 使用 org-modern 风格的标题样式
(custom-set-faces
 '(markdown-header-face-1 ((t (:inherit org-level-1 :height 1.5 :weight bold))))
 '(markdown-header-face-2 ((t (:inherit org-level-2 :height 1.25 :weight bold))))
 '(markdown-header-face-3 ((t (:inherit org-level-3 :height 1.1 :weight bold))))
 '(markdown-header-face-4 ((t (:inherit org-level-4 :height 1.0 :weight bold))))
 '(markdown-header-face-5 ((t (:inherit org-level-5 :height 1.0 :weight bold))))
 '(markdown-header-face-6 ((t (:inherit org-level-6 :height 1.0 :weight bold)))))

;; 使用 org-modern 风格的列表样式
(use-package org-modern
  :ensure t
  :config
  (setq org-modern-list '((?- . "•") (?+ . "◦") (?* . "▹"))))

;; 使用 prettify-symbols-mode 替换特殊标记
(defun my/markdown-prettify-symbols ()
  "Enable prettify symbols in Markdown mode.
Replaces common Markdown constructs with Unicode symbols for better readability."
  (setq prettify-symbols-alist
        '(("```" . "🖥")
          ("`" . "⌨")
          ("*" . "•")
          ("**" . "✧")
          ("***" . "✦")
          ("_" . "▱")
          ("__" . "▰")
          ("~~" . "≈")))
  (prettify-symbols-mode 1))

(add-hook 'markdown-mode-hook 'my/markdown-prettify-symbols)

;; 使用 olivetti 居中显示内容
(use-package olivetti
  :ensure t
  :hook (markdown-mode . olivetti-mode)
  :config
  (setq olivetti-body-width 0.618))

;; 启用自动换行
(add-hook 'markdown-mode-hook 'visual-line-mode)

(provide 'init-markdown)
;; Markdown 配置:1 ends here
