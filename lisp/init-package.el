;; [[file:../init.org::*包管理配置][包管理配置:1]]
(setq use-package-compute-statistics t) ;; Enable timing for M-x use-package-report

(require 'package)

(setq package-archives
      '(("gnu"   . "https://mirrors.ustc.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
        ("org"   . "https://mirrors.ustc.edu.cn/elpa/emacs-policy/elpa-org/")))

(package-initialize)

;; Ensure package contents are loaded
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure `use-package`, `evil`, `markdown-mode`, `vertico`, and `orderless` are installed
(dolist (pkg '(use-package evil markdown-mode vertico orderless))
  (unless (package-installed-p pkg)
    (package-install pkg)))

(provide 'init-package)
;; 包管理配置:1 ends here
