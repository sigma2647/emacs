;; [[file:../init.org::*启动配置][启动配置:1]]
;;; init-startup.el --- Initialize startup configurations -*- lexical-binding: t -*-

;; ┌─────────┐
;; │ setting │
;; └─────────┘

;; 设置 logo
(defcustom centaur-logo (expand-file-name
                        (if (display-graphic-p) "~/.emacs.d/dashboard/eva.png" "~/.emacs.d/dashboard/banner.txt")
                        user-emacs-directory)
  "Set Centaur logo. nil means official logo."
  :group 'centaur
  :type 'string)

;; 设置启动缓冲区
(defun my/startup-buffer ()
  "Return the buffer to show at startup."
  (let ((file (car (cdr command-line-args))))  ;; 获取第一个非选项参数
    (if (and file (file-exists-p file))
        (find-file file)
      (get-buffer-create "*dashboard*"))))

;; 设置启动行为
(setq initial-buffer-choice #'my/startup-buffer)

;; 处理命令行参数
(add-hook 'after-init-hook
          (lambda ()
            (when (and (not (daemonp))
                      (not (get-buffer "*dashboard*")))
              (dashboard-refresh-buffer))))

(provide 'init-startup)
;; 启动配置:1 ends here
