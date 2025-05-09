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
(require 'init-theme)      ;; 添加主题配置

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "#3b4252" :foreground "#eceff4"))))
 '(mode-line-inactive ((t (:background "#2e3440" :foreground "#4c566a"))))
 '(org-block ((t (:background "#21242b" :extend t))))
 '(org-block-begin-line ((t (:foreground "#5B6268" :background "#2d333b" :extend t :italic t :height 0.9))))
 '(org-block-end-line ((t (:foreground "#5B6268" :background "#2d333b" :extend t :italic t :height 0.9))))
 '(org-document-title ((t (:height 1.75 :weight bold))))
 '(org-level-1 ((t (:height 1.5 :weight bold))))
 '(org-level-2 ((t (:height 1.25 :weight bold))))
 '(org-level-3 ((t (:height 1.1 :weight bold))))
 '(org-level-4 ((t (:height 1.0 :weight bold))))
 '(org-level-5 ((t (:height 1.0 :weight bold))))
 '(org-level-6 ((t (:height 1.0 :weight bold))))
 '(org-level-7 ((t (:height 1.0 :weight bold))))
 '(org-level-8 ((t (:height 1.0 :weight bold)))))
