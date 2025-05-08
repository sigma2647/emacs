;; [[file:../init.org::*基础配置][基础配置:1]]
;; 基础设置
(setq inhibit-splash-screen t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; 编码设置
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; 设置 M-p 为 execute-extended-command
(global-set-key (kbd "M-p") 'execute-extended-command)

;; 自动更新文件
(setq auto-revert-interval 1)  ;; 每秒检查一次
(setq auto-revert-check-vc-info t)  ;; 检查版本控制信息
(setq auto-revert-verbose nil)  ;; 不显示提示信息
(setq auto-revert-use-notify t)  ;; 使用文件系统通知
(setq auto-revert-stop-on-user-input nil)  ;; 用户输入时继续更新
(setq auto-revert-remote-files t)  ;; 支持远程文件
(global-auto-revert-mode 1)  ;; 全局启用

;; 优化文件监控
(when (and (fboundp 'file-notify-add-watch)
           (executable-find "inotifywait"))
  (setq auto-revert-notify-exclude-dir-regexp
        (concat "\\`" (regexp-opt '("/tmp" "/var/tmp" "/dev" "/sys" "/proc")) "\\'")))

(provide 'init-basic)
;; 基础配置:1 ends here
