;; [[file:../init.org::*UI 配置][UI 配置:1]]
;; Enable pixel scroll
(pixel-scroll-precision-mode 1)

;; Enable smooth scrolling
(setq pixel-scroll-precision-interpolate-page t)

;; Use pixel scroll for all scrolling commands
(defalias 'scroll-up-command 'pixel-scroll-interpolate-down)
(defalias 'scroll-down-command 'pixel-scroll-interpolate-up)

;; Additional scroll settings for better experience
(setq scroll-conservatively 101)  ;; Don't recenter point
(setq scroll-margin 0)            ;; No margin when scrolling
(setq scroll-preserve-screen-position t)  ;; Keep cursor position relative to screen
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))  ;; Fine-tune mouse wheel scrolling
(setq mouse-wheel-progressive-speed nil)  ;; Disable progressive speed

;; Enable relative line numbers for non-org and non-markdown files
(defun enable-relative-line-numbers ()
  (unless (or (derived-mode-p 'org-mode)
              (derived-mode-p 'markdown-mode)
              (derived-mode-p 'gfm-mode))
    (display-line-numbers-mode 1)
    (setq display-line-numbers-type 'relative)))

;; Add hook to enable relative line numbers
(add-hook 'prog-mode-hook 'enable-relative-line-numbers)
(add-hook 'text-mode-hook 'enable-relative-line-numbers)

;; 配置 doom-modeline
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)  ;; 设置高度
  (doom-modeline-bar-width 3)  ;; 设置指示条宽度
  (doom-modeline-icon t)  ;; 显示图标
  (doom-modeline-major-mode-icon t)  ;; 显示主模式图标
  (doom-modeline-major-mode-color-icon t)  ;; 使用彩色图标
  (doom-modeline-minor-modes t)  ;; 显示次要模式
  (doom-modeline-buffer-encoding t)  ;; 显示编码
  (doom-modeline-indent-info t)  ;; 显示缩进信息
  (doom-modeline-checker-simple-format t)  ;; 简化检查器显示
  (doom-modeline-vcs-max-length 12)  ;; 版本控制信息最大长度
  (doom-modeline-workspace-name t)  ;; 显示工作区名称
  (doom-modeline-persp-name t)  ;; 显示透视名称
  (doom-modeline-display-default-persp-name t)  ;; 显示默认透视名称
  (doom-modeline-lsp t)  ;; 显示 LSP 状态
  (doom-modeline-github t)  ;; 显示 GitHub 信息
  (doom-modeline-github-interval 60)  ;; GitHub 信息更新间隔
  (doom-modeline-mu4e t)  ;; 显示 mu4e 信息
  (doom-modeline-irc t)  ;; 显示 IRC 信息
  (doom-modeline-irc-stylize 'identity)  ;; IRC 样式
  (doom-modeline-battery t)  ;; 显示电池信息
  (doom-modeline-battery-alert-threshold 20)  ;; 电池警告阈值
  (doom-modeline-battery-update-interval 60)  ;; 电池信息更新间隔
  (doom-modeline-time t)  ;; 显示时间
  (doom-modeline-time-24hr t)  ;; 使用24小时制
  (doom-modeline-time-icon t)  ;; 显示时间图标
  (doom-modeline-env-version t)  ;; 显示环境版本
  (doom-modeline-env-load-string "...")  ;; 环境加载字符串
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)  ;; 文件名显示样式
  (doom-modeline-project-detection 'projectile)  ;; 使用 projectile 检测项目
  (doom-modeline-buffer-state-icon t)  ;; 显示缓冲区状态图标
  (doom-modeline-buffer-modification-icon t)  ;; 显示修改图标
  (doom-modeline-unicode-fallback t)  ;; 使用 Unicode 回退
  (doom-modeline-mu4e-alert-enable t)  ;; 启用 mu4e 提醒
  (doom-modeline-mu4e-alert-icon t)  ;; 显示 mu4e 提醒图标
  (doom-modeline-mu4e-alert-timeout 10)  ;; mu4e 提醒超时
  (doom-modeline-mu4e-alert-style 'notifications)  ;; mu4e 提醒样式
  (doom-modeline-mu4e-alert-group t)  ;; 分组 mu4e 提醒
  (doom-modeline-mu4e-alert-group-timeout 10)  ;; mu4e 提醒组超时
  (doom-modeline-mu4e-alert-group-style 'notifications)  ;; mu4e 提醒组样式
  (doom-modeline-mu4e-alert-group-max 5)  ;; mu4e 提醒组最大数量
  (doom-modeline-mu4e-alert-group-min 1)  ;; mu4e 提醒组最小数量
  (doom-modeline-mu4e-alert-group-threshold 3)  ;; mu4e 提醒组阈值
  (doom-modeline-mu4e-alert-group-interval 60)  ;; mu4e 提醒组间隔
  (doom-modeline-mu4e-alert-group-timeout 10)  ;; mu4e 提醒组超时
  (doom-modeline-mu4e-alert-group-style 'notifications)  ;; mu4e 提醒组样式
  (doom-modeline-mu4e-alert-group-max 5)  ;; mu4e 提醒组最大数量
  (doom-modeline-mu4e-alert-group-min 1)  ;; mu4e 提醒组最小数量
  (doom-modeline-mu4e-alert-group-threshold 3)  ;; mu4e 提醒组阈值
  (doom-modeline-mu4e-alert-group-interval 60))  ;; mu4e 提醒组间隔

(provide 'init-ui)
;; UI 配置:1 ends here
