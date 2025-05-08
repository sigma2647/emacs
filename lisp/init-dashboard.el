;; [[file:../init.org::*Dashboard 配置][Dashboard 配置:1]]
;;; init-dashboard.el --- Initialize dashboard configurations -*- lexical-binding: t -*-

;; 确保 all-the-icons 已安装
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package dashboard
  :ensure t
  :custom
  (dashboard-startup-banner (or centaur-logo 'official))  ;; 使用自定义 logo
  (dashboard-banner-logo-title "Welcome to Emacs")  ;; 欢迎信息
  (dashboard-set-heading-icons t)  ;; 显示标题图标
  (dashboard-set-file-icons t)     ;; 显示文件图标
  (dashboard-center-content t)     ;; 居中显示内容
  (dashboard-image-banner-max-width 300)  ;; 图片最大宽度
  (dashboard-image-banner-max-height 300) ;; 图片最大高度
  (dashboard-items '((recents  . 5)    ;; 最近文件数量
                     (projects . 5)    ;; 项目数量
                     (bookmarks . 5)   ;; 书签数量
                     (agenda . 5)      ;; 日程数量
                     (registers . 5))) ;; 寄存器数量
  (dashboard-show-shortcuts t)     ;; 显示快捷键
  (dashboard-set-navigator t)      ;; 显示导航器
  (dashboard-set-init-info t)      ;; 显示初始化信息
  (dashboard-set-footer t)         ;; 显示页脚
  :config
  (dashboard-setup-startup-hook))

;; 优化 dashboard 样式和性能
(with-eval-after-load 'dashboard
  ;; 设置 dashboard 主题
  (setq dashboard-theme 'doom)
  ;; 自定义 dashboard 项目
  (setq dashboard-projects-backend 'projectile)
  ;; 设置 dashboard 刷新间隔（秒）
  (setq dashboard-refresh-buffer-time 300)
  
  ;; 添加缓存机制
  (setq dashboard-cache-file (expand-file-name "dashboard-cache.el" user-emacs-directory))
  (setq dashboard-cache-prefetch t)
  
  ;; 优化显示效果
  (setq dashboard-heading-icons '((recents . "file-text")
                                 (bookmarks . "bookmark")
                                 (projects . "briefcase")
                                 (agenda . "calendar")
                                 (registers . "database")))
  
  ;; 自定义样式
  (setq dashboard-footer-messages '("Welcome to Emacs!"))
  (when (display-graphic-p)
    (setq dashboard-footer-icon (all-the-icons-octicon "dashboard" :height 1.1 :v-adjust -0.05 :face 'font-lock-keyword-face)))
  
  ;; 确保内容居中
  (setq dashboard-center-content t)
  (setq dashboard-page-break-line t)
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  
  ;; 添加自定义 CSS
  (add-hook 'dashboard-mode-hook
            (lambda ()
              (setq-local line-spacing 0.2)
              (setq-local fill-column 80)
              (setq-local visual-fill-column-center-text t))))

(provide 'init-dashboard)
;; Dashboard 配置:1 ends here
