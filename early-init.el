;;; early-init.el --- Emacs pre-initialization config -*- lexical-binding: t -*-

;; 优化垃圾回收，加快启动速度
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; 禁用 package.el 自动加载
(setq package-enable-at-startup nil)
(setq package-quickstart nil)

;; 禁用菜单栏、工具栏、滚动条
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; 禁用启动画面和文件对话框
(setq inhibit-splash-screen t
      use-file-dialog nil)

;; 禁止自动缩放窗口
(setq frame-inhibit-implied-resize t)

;; Emacs 28+：禁止延迟编译
(when (boundp 'comp-deferred-compilation)
  (setq comp-deferred-compilation nil))

;; 启动后恢复正常垃圾回收阈值
(let ((normal-gc-cons-threshold (* 20 1024 1024)))
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(provide 'early-init)
;;; early-init.el ends here 