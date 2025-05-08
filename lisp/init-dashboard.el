;; [[file:../init.org::*Dashboard 配置][Dashboard 配置:1]]
;;; init-dashboard.el --- Initialize dashboard configurations -*- lexical-binding: t -*-

(use-package dashboard
  :ensure t
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
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
                     (bookmarks . 5))) ;; 书签数量
  :config
  (dashboard-setup-startup-hook))

;; 可选：自定义 dashboard 样式
(with-eval-after-load 'dashboard
  ;; 设置 dashboard 主题
  (setq dashboard-theme 'doom)
  ;; 自定义 dashboard 项目
  (setq dashboard-projects-backend 'projectile)
  ;; 设置 dashboard 刷新间隔（秒）
  (setq dashboard-refresh-buffer-time 300))

(provide 'init-dashboard)
;; Dashboard 配置:1 ends here
