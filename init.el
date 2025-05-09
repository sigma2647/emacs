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
(require 'init-completion)
(require 'init-theme)
(require 'init-lang)      ;; 添加语言配置

;; 确保必要的包已安装
(use-package visual-fill-column :ensure t)

;; 解决org文件本地变量安全提示问题
;; 方法1：使用safe-local-eval-forms
(add-to-list 'safe-local-eval-forms
             '(add-hook 'after-save-hook (lambda nil (org-babel-tangle)) nil t))

;; 方法2：备用方案 - 使用safe-local-variable
(put 'eval 'safe-local-variable
     (lambda (x)
       (and (functionp x)
            (string-match-p "org-babel-tangle" (prin1-to-string x)))))
;; 主入口:1 ends here
