;; [[file:init.org::*add mirror][add mirror:1]]
(require 'package)
(setq package-archives
      '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
(package-initialize)

;; Ensure package contents are loaded
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure `use-package` and `evil` are installed
(dolist (pkg '(use-package evil))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Make M-p work like M-x
(global-set-key (kbd "M-p") 'execute-extended-command)
;; add mirror:1 ends here

;; [[file:init.org::*evilmode][evilmode:1]]
(require 'evil)
(evil-mode 1)
;; evilmode:1 ends here

;; [[file:init.org::*Level 2][Level 2:1]]
(setq inhibit-splash-screen t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(menu-bar-mode -1)



(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
;; Level 2:1 ends here

;; [[file:init.org::*UI][UI:1]]
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-interpolate-page t)
(defalias 'scroll-up-command 'pixel-scroll-interpolate-down)
(defalias 'scroll-down-command 'pixel-scroll-interpolate-up)
;; UI:1 ends here

;; [[file:init.org::*org mode][org mode:1]]
(require 'org-tempo)
;; org mode:1 ends here

;; [[file:init.org::*font][font:1]]
(set-face-attribute 'default nil :font "Maple Mono NF CN" :height 140)  ; 14pt

;; 显式设置中文字体（可选，如果 Maple Mono NF CN 已经含中文）
(set-fontset-font t 'han (font-spec :family "Maple Mono NF CN"))
;; font:1 ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook (lambda nil (org-babel-tangle)) nil
	   t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
