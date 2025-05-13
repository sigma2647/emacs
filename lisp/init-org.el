;; [[file:../init.org::*Org 配置][Org 配置:1]]
(require 'org-tempo)

  ;; 配置 Org table 字体
  (set-face-attribute 'org-table nil
                     :font (face-attribute 'default :font)
                     :height (face-attribute 'default :height))

  ;; Org mode 表格对齐设置
  (setq org-table-align-indent t)
  (setq org-table-align-char ?\s)

  ;; 确保 Org 表格中的字体大小正确
  (add-hook 'org-mode-hook
            (lambda ()
              (set-face-attribute 'org-table nil
                                :font (face-attribute 'default :font)
                                :height (face-attribute 'default :height))))
                                
  ;; 设置标题样式
  (custom-set-faces
   '(org-document-title ((t (:height 1.75 :weight bold))))
   '(org-level-1 ((t (:height 1.5 :weight bold))))
   '(org-level-2 ((t (:height 1.25 :weight bold))))
   '(org-level-3 ((t (:height 1.1 :weight bold))))
   '(org-level-4 ((t (:height 1.0 :weight bold))))
   '(org-level-5 ((t (:height 1.0 :weight bold))))
   '(org-level-6 ((t (:height 1.0 :weight bold))))
   '(org-level-7 ((t (:height 1.0 :weight bold))))
   '(org-level-8 ((t (:height 1.0 :weight bold)))))

  ;; 使用 org-modern 美化界面
  (use-package org-modern
    :ensure t
    :hook (org-mode . org-modern-mode)
    :config
    (setq org-modern-star '("★" "☆" "◆" "◇" "▶" "▷" "✿" "❀"))
    (setq org-modern-list '((?- . "•") (?+ . "◦") (?* . "▹")))
    (setq org-modern-table t)
    (setq org-modern-block-name
          '((t . t)
            ("src" "»" "«")
            ("example" "»–" "–«")
            ("quote" "❝" "❞"))))

  ;; 使用 org-appear 显示强调标记
  (use-package org-appear
    :ensure t
    :hook (org-mode . org-appear-mode)
    :config
    (setq org-appear-autolinks t
          org-appear-autosubmarkers t
          org-appear-autoentities t
          org-appear-autokeywords t
          org-appear-inside-latex t))

  ;; 使用 prettify-symbols-mode 替换特殊标记
  (defun my/org-prettify-symbols ()
    "Enable prettify symbols in Org mode.
Replaces common Org mode constructs with Unicode symbols for better readability."
    (setq prettify-symbols-alist
          '(("#+TITLE:" . "📘")
            ("#+AUTHOR:" . "👤")
            ("#+DATE:" . "📅")
            ("#+EMAIL:" . "📧")
            ("#+OPTIONS:" . "⚙")
            ("#+BEGIN_SRC" . "🖥")
            ("#+END_SRC" . "🖥")
            ("#+BEGIN_QUOTE" . "❝")
            ("#+END_QUOTE" . "❞")
            ("#+RESULTS:" . "📊")))
    (prettify-symbols-mode 1))
  (add-hook 'org-mode-hook 'my/org-prettify-symbols)

  ;; 使用 olivetti 居中显示内容
  (use-package olivetti
    :ensure t
    :hook (org-mode . olivetti-mode)
    :config
    (setq olivetti-body-width 0.618))

  ;; 设置其他 Org-mode 选项
  (setq org-hide-emphasis-markers t
        org-startup-indented t
        org-ellipsis " ▼ "
        org-pretty-entities t
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-hide-leading-stars t
        org-startup-with-inline-images t
        org-image-actual-width '(300)
        org-adapt-indentation nil)

  ;; 美化内联代码
  (defface org-inline-code-face
    '((t :inherit (shadow fixed-pitch)
         :foreground "#88c0d0"
         :background "#2e3440"
         :box (:line-width -1 :color "#4c566a")
         :height 0.9))
    "Face for inline code in Org mode.")

  (defun my/org-prettify-inline-code ()
    "Make inline code more visible in Org mode.
Adds a subtle background and border to inline code blocks."
    (font-lock-add-keywords
     nil
     '(("\\(\\\\begin{[a-zA-Z0-9]+}\\)" 1 'org-block-begin-line prepend)
       ("\\(\\\\end{[a-zA-Z0-9]+}\\)" 1 'org-block-end-line prepend)
       ("\\(~\\)\\([^~]+\\)\\(~\\)" (0 'org-inline-code-face t))
       ("\\(=\\)\\([^=]+\\)\\(=\\)" (0 'org-inline-code-face t)))))  ;; 添加等号标记的代码

  (add-hook 'org-mode-hook 'my/org-prettify-inline-code)

  (provide 'init-org)
;; Org 配置:1 ends here
