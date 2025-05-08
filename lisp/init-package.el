;; [[file:../init.org::*包管理配置][包管理配置:1]]
(require 'package)
(setq package-archives
      '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
(package-initialize)

;; Ensure package contents are loaded
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure `use-package`, `evil`, and `markdown-mode` are installed
(dolist (pkg '(use-package evil markdown-mode))
  (unless (package-installed-p pkg)
    (package-install pkg)))

(provide 'init-package)
;; 包管理配置:1 ends here
