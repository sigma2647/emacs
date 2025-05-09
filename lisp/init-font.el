;; [[file:../init.org::*字体配置][字体配置:1]]
(setq inhibit-compacting-font-caches t) ;; Improve performance with icons/diverse fonts

;; 设置默认字体族和大小
(set-face-attribute 'default nil
                  :font "Maple Mono NF CN"
                  :height 130)

;; 确保 fixed-pitch face 也使用 Maple Mono NF CN
(set-face-attribute 'fixed-pitch nil
                  :font "Maple Mono NF CN"
                  :height (face-attribute 'default :height))

;; 为 CJK 字符设置字体
(set-fontset-font t 'han (font-spec :family "Maple Mono NF CN" 
                                  :height (face-attribute 'default :height)))

;; 确保所有等宽文本使用相同字体
(set-face-attribute 'fixed-pitch-serif nil
                  :font "Maple Mono NF CN"
                  :height (face-attribute 'default :height))

(provide 'init-font)
;; 字体配置:1 ends here
