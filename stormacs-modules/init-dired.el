;; init-dired.el --- Dired settings -*- lexical-binding: t; -*-

(put 'dired-find-alternate-file 'disabled nil)

(use-package dired+
  :ensure (dired+ :host github :repo "emacsmirror/dired-plus" :main "dired+.el")
  :init
  (setq diredp-hide-details-initially-flag nil)
  :config
  (diredp-toggle-find-file-reuse-dir 1))

(use-package nerd-icons-dired
  :ensure (nerd-icons-dired :host github :repo "rainstormstudio/nerd-icons-dired")
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dired-rsync
  :ensure (dired-rsync :host github :repo "stsquad/dired-rsync")
  :config
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map))

(use-package dired-preview
  :ensure (dired-preview :host github :repo "protesilaos/dired-preview")
  :config
  (setq dired-preview-delay 0.01)
  (add-to-list 'dired-preview-trigger-commands #'diredp-next-line)
  (add-to-list 'dired-preview-trigger-commands #'diredp-previous-line)
  (dired-preview-global-mode 1))

(provide 'init-dired)
