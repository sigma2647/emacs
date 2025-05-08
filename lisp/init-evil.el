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

(provide 'init-evil)
;; Evil 配置:1 ends here
