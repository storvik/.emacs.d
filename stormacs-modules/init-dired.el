;; init-dired.el --- Dired settings -*- lexical-binding: t; -*-

(elpaca-use-package
    (dired+ :host github :repo "emacsmirror/dired-plus")
  :init
  (setq diredp-hide-details-initially-flag nil)
  :config
  (diredp-toggle-find-file-reuse-dir 1))

(elpaca-use-package
    (all-the-icons-dired :host github :repo "wyuenho/all-the-icons-dired")
  :hook (dired-mode . all-the-icons-dired-mode))

(elpaca-use-package
    (dired-rsync :host github :repo "stsquad/dired-rsync")
  :config
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map))

(provide 'init-dired)
