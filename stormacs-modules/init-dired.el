;; init-dired.el --- Dired settings -*- lexical-binding: t; -*-

(use-package dired+
  :elpaca (dired+ :host github :repo "emacsmirror/dired-plus" :main "dired+.el")
  :init
  (setq diredp-hide-details-initially-flag nil)
  :config
  (diredp-toggle-find-file-reuse-dir 1))

(use-package nerd-icons-dired
  :elpaca (nerd-icons-dired :host github :repo "rainstormstudio/nerd-icons-dired")
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dired-rsync
  :elpaca (dired-rsync :host github :repo "stsquad/dired-rsync")
  :config
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map))

(provide 'init-dired)
