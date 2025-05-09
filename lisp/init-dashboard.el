;; [[file:../init.org::*Dashboard 配置][Dashboard 配置:1]]
;;; init-dashboard.el --- Initialize dashboard configurations -*- lexical-binding: t -*-

;; 确保 all-the-icons 已安装并正确加载
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package dashboard
  :ensure t
  :init
  ;; 基本设置
  (setq dashboard-startup-banner (or centaur-logo 'official))  ;; 保留自定义 logo
  (setq dashboard-banner-logo-title "Welcome to Emacs")  ;; 欢迎信息
  
  ;; 关键居中设置
  (setq dashboard-center-content t)     ;; 水平居中显示内容
  (setq dashboard-vertically-center-content t)  ;; 垂直居中内容
  (setq dashboard-items-default-length 5)  ;; 每个部分显示的条目数
  
  ;; 内容显示设置
  (setq dashboard-set-heading-icons t)  ;; 显示标题图标
  (setq dashboard-set-file-icons t)     ;; 显示文件图标
  (setq dashboard-image-banner-max-width 300)  ;; 图片最大宽度
  (setq dashboard-image-banner-max-height 300) ;; 图片最大高度
  (setq dashboard-items '((recents  . 5)    ;; 最近文件数量
                          ;; (projects . 5)  ;; 项目数量 - 已移除因为需要projectile
                          (bookmarks . 5)   ;; 书签数量
                          (agenda . 5)      ;; 日程数量
                          (registers . 5))) ;; 寄存器数量
  
  ;; 样式设置
  (setq dashboard-set-navigator nil)      ;; 不显示导航器
  (setq dashboard-set-init-info t)      ;; 显示初始化信息
  (setq dashboard-set-footer t)         ;; 显示页脚
  (setq dashboard-footer-messages '("Welcome to Emacs!"))  ;; 自定义页脚消息
  
  ;; 最关键的标题格式设置
  (setq dashboard-item-names '(("Recent Files:" . "Recent Files:")  ;; 保持英文一致性
                               ("Bookmarks:" . "Bookmarks:")
                               ("Agenda for today:" . "Agenda for today:")
                               ("Registers:" . "Registers:")))
  :config
  ;; 图标设置 - 放在:config部分确保all-the-icons已加载
  (when (display-graphic-p)
    (setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
                                                     :height 1.1
                                                     :v-adjust -0.05
                                                     :face 'font-lock-keyword-face)))
  (dashboard-setup-startup-hook))

;; 优化 dashboard 样式和性能
(with-eval-after-load 'dashboard
  ;; 基本优化设置
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator nil)
  
  ;; 缓存设置
  (setq dashboard-cache-file (expand-file-name "dashboard-cache.el" user-emacs-directory))
  (setq dashboard-cache-prefetch t))

(provide 'init-dashboard)
;; Dashboard 配置:1 ends here
