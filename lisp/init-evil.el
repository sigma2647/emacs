;; [[file:../init.org::*Evil 配置][Evil 配置:1]]
(require 'evil)
(evil-mode 1)

;; 检查 fcitx5-remote 是否可用
(defun check-fcitx5-remote ()
  (executable-find "fcitx5-remote"))

;; 切换到英文输入法
(defun switch-to-english-input ()
  (when (check-fcitx5-remote)
    (call-process "fcitx5-remote" nil nil nil "-c")))

;; 在进入普通模式时切换到英文输入法
(add-hook 'evil-normal-state-entry-hook 'switch-to-english-input)

;; 自定义键位映射
(evil-define-key 'normal 'global
  "H" 'evil-first-non-blank  ;; 移动到行首非空白字符
  "L" 'evil-end-of-line)     ;; 移动到行尾

(evil-define-key 'visual 'global
  "H" 'evil-first-non-blank  ;; 移动到行首非空白字符
  "L" 'evil-end-of-line)     ;; 移动到行尾

;; 可选：添加更多常用键位映射
(evil-define-key 'normal 'global
  "gh" 'evil-window-top      ;; 移动到窗口顶部
  "gl" 'evil-window-bottom   ;; 移动到窗口底部
  "gj" 'evil-next-line       ;; 向下移动一行
  "gk" 'evil-previous-line)  ;; 向上移动一行

(provide 'init-evil)
;; Evil 配置:1 ends here
