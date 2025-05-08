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
(require 'init-startup)
(require 'init-dashboard)
(require 'init-completion)  ;; 添加补全系统配置

;; 确保必要的包已安装
(use-package visual-fill-column :ensure t)
;; 主入口:1 ends here
