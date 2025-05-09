;; [[file:../init.org::*主题配置][主题配置:1]]
;; 安装doom-themes和所有图标
(use-package doom-themes
  :ensure t
  :config
  ;; 全局设置
  (setq doom-themes-enable-bold t    ; 如果nil，粗体将被禁用
        doom-themes-enable-italic t) ; 如果nil，斜体将被禁用
  
  ;; 加载Nord主题（冷色系）
  (load-theme 'doom-nord t)
  
  ;; 启用闪亮的代码块
  (doom-themes-org-config)
  
  ;; 改进代码块边框和背景
  (custom-set-faces
   '(org-block-begin-line ((t (:foreground "#5B6268" :background "#2d333b" :extend t :italic t :height 0.9))))
   '(org-block ((t (:background "#21242b" :extend t))))
   '(org-block-end-line ((t (:foreground "#5B6268" :background "#2d333b" :extend t :italic t :height 0.9))))))

;; 代码块美化设置
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-delay 0
        highlight-indent-guides-auto-character-face-perc 30))

;; 彩虹分隔符
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; 代码块美化
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; 为org-modern和olivetti模式提供更好的主题集成
(with-eval-after-load 'org-modern
  (custom-set-faces
   '(org-modern-label ((t (:height 0.9 :box (:line-width -1 :color "#81a1c1") :foreground "#81a1c1" :background "#2e3440"))))
   '(org-modern-tag ((t (:foreground "#5e81ac" :background "#2e3440" :box (:line-width -1 :color "#4c566a")))))))

;; 提供自定义的modeline配置
(with-eval-after-load 'doom-modeline
  (custom-set-faces
   '(mode-line ((t (:background "#3b4252" :foreground "#eceff4"))))
   '(mode-line-inactive ((t (:background "#2e3440" :foreground "#4c566a"))))))

;; 源代码块语法高亮
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0)
      
;; 美化内联代码
(defface org-inline-code-face
  '((t :inherit (shadow fixed-pitch)
       :foreground "#88c0d0"
       :background "#2e3440"
       :box (:line-width -1 :color "#4c566a")))
  "Face for inline code in Org mode.")

(defun my/org-prettify-inline-code ()
  "Make inline code more visible in Org mode."
  (font-lock-add-keywords
   nil
   '(("\\(\\\\begin{[a-zA-Z0-9]+}\\)" 1 'org-block-begin-line prepend)
     ("\\(\\\\end{[a-zA-Z0-9]+}\\)" 1 'org-block-end-line prepend)
     ("\\(~\\)\\([^~]+\\)\\(~\\)" (0 'org-inline-code-face t)))))

(add-hook 'org-mode-hook 'my/org-prettify-inline-code)

;; 全局启用行号但在特定模式禁用
(global-display-line-numbers-mode 1)
(dolist (mode '(org-mode vterm-mode eshell-mode shell-mode term-mode dashboard-mode))
  (add-hook (intern (format "%s-hook" mode)) 
            (lambda () (display-line-numbers-mode 0))))

(provide 'init-theme)
;; 主题配置:1 ends here
