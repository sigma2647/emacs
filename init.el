;; [[file:init.org::*主入口][主入口:1]]
;; 添加 lisp 目录到 load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; 加载各个模块
(require 'init-basic)
(require 'init-package)
(require 'init-evil)
(require 'init-ui)
(require 'init-font)
(require 'init-markdown)
(require 'init-org)
(require 'init-startup)  ;; 添加启动配置
(require 'init-dashboard)  ;; 添加 dashboard 模块

;; 确保必要的包已安装
(use-package visual-fill-column :ensure t)
;; 主入口:1 ends here
