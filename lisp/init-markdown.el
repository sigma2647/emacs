;; [[file:../init.org::*Markdown 配置][Markdown 配置:1]]
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :init
  (setq markdown-command "multimarkdown"))

(provide 'init-markdown)
;; Markdown 配置:1 ends here
