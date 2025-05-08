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
;; 设置默认字体族和大小。
;; "Maple Mono NF CN" 字体设计旨在实现 CJK 字符与拉丁字符 2:1 的宽度比。
(set-face-attribute 'default nil
                    :font "Maple Mono NF CN"
                    :height 140) ; 根据需要调整高度（单位：1/10pt）

;; 确保 fixed-pitch face 也使用 Maple Mono NF CN 并继承高度。
(set-face-attribute 'fixed-pitch nil
                    :font "Maple Mono NF CN"
                    :height (face-attribute 'default :height))

;; 关键步骤：告知 Emacs 为 'han' (CJK) 字符使用 "Maple Mono NF CN"。
;; 这使得 Emacs 可以利用该字体固有的 CJK 字符 2:1 宽度特性。
;; 第一个参数 t 表示此设置应用于标准字体集。
(set-fontset-font t 'han (font-spec :family "Maple Mono NF CN"))
;; 如果需要为 Han 字符明确指定与 default face 相同的大小，可使用：
;; (set-fontset-font t 'han (font-spec :family "Maple Mono NF CN" :height (face-attribute 'default :height)))

;; 可选项：如果希望 variable-pitch 文本（例如某些 Org mode 视图中）
;; 也使用等宽字体，请取消注释以下代码。
;; 否则，variable-pitch 将使用系统的比例宽度字体。
; (set-face-attribute 'variable-pitch nil
;                     :font "Maple Mono NF CN"
;                     :height (face-attribute 'default :height))

;; 配置 Org table 字体，并继承高度。
(set-face-attribute 'org-table nil
                    :font "Maple Mono NF CN"
                    :height (face-attribute 'default :height))

;; 用户原配置中的以下 `char-width-alist` 设置，
;; 如果 "Maple Mono NF CN" 被正确用于 'han' 书写系统，则很可能不再需要，
;; 因为字体本身会处理 2:1 的宽度。
;; 另外，书写系统名称 'chinese' 是不正确的，应为 'han'。
;; 请首先尝试不使用此行代码。
;; (setq char-width-alist '((han. 2)))

;; 用户原配置中的标准 Org mode 表格对齐设置（通常是好的）
(setq org-table-align-indent t)
(setq org-table-align-char?\s)

;; 应用此配置后，请使用 C-u C-x = 检查 CJK 字符的属性，
;; 并检查表格对齐情况。
;; font:1 ends here
