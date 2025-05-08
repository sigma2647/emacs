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

(provide 'init-basic)
;; 基础配置:1 ends here
