;; [[file:../init.org::*Org 配置][Org 配置:1]]
(require 'org-tempo)

;; 配置 Org table 字体
(set-face-attribute 'org-table nil
                  :font "Maple Mono NF CN"
                  :height (face-attribute 'default :height))

;; Org mode 表格对齐设置
(setq org-table-align-indent t)
(setq org-table-align-char ?\s)

;; 确保 Org 表格中的字体大小正确
(add-hook 'org-mode-hook
          (lambda ()
            (set-face-attribute 'org-table nil
                              :font "Maple Mono NF CN"
                              :height (face-attribute 'default :height))))

(provide 'init-org)
;; Org 配置:1 ends here
