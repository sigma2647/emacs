;; [[file:../init.org::*Nix 配置][Nix 配置:1]]
;;; init-nix.el --- Nix specific configurations -*- lexical-binding: t -*-

(use-package nix-mode
  :ensure t
  :defer t
  :mode ("\\.nix\\'" . nix-mode)
  :config
  (setq nix-indent-offset 2))

;; Nix 格式化
(use-package nixpkgs-fmt
  :ensure t
  :after nix-mode
  :hook (nix-mode . nixpkgs-fmt-on-save-mode))

(provide 'init-nix)
;; Nix 配置:1 ends here
