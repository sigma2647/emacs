;; [[file:../init.org::*字体配置][字体配置:1]]
(setq inhibit-compacting-font-caches t) ;; Improve performance with icons/diverse fonts

;; 确保 all-the-icons 包已安装并加载
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :config
  (setq all-the-icons-scale-factor 1.0)
  (setq all-the-icons-default-adjust 0))

;; 系统特定的字体名称
(defvar my/font-names
  '((darwin . ("MapleMonoNL-NF-CN" "SF Mono" "Menlo" "Monaco"))
    (gnu/linux . ("MapleMonoNL-NF-CN" "DejaVu Sans Mono" "Monospace"))
    (windows-nt . ("MapleMonoNL-NF-CN" "Consolas" "Monospace")))
  "系统特定的字体名称列表")

;; 设置默认字体族和大小
(defun my/set-font (font-family)
  "安全地设置字体，如果字体不可用则返回 nil"
  (when (member font-family (font-family-list))
    (set-face-attribute 'default nil
                      :font font-family
                      :height 130)
    (set-face-attribute 'fixed-pitch nil
                      :font font-family
                      :height (face-attribute 'default :height))
    (set-fontset-font t 'han (font-spec :family font-family
                                      :height (face-attribute 'default :height)))
    (set-face-attribute 'fixed-pitch-serif nil
                      :font font-family
                      :height (face-attribute 'default :height))
    t))

;; 获取当前系统的字体列表
(defun my/get-system-fonts ()
  (or (cdr (assoc system-type my/font-names))
      '("Monospace")))

;; 按优先级尝试设置字体
(unless (cl-some #'my/set-font (my/get-system-fonts))
  (message "Warning: No suitable font found"))

(provide 'init-font)
;; 字体配置:1 ends here
