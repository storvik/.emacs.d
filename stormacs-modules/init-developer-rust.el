;; init-developer-rust.el --- Developer rust settings -*- lexical-binding: t; -*-

(use-package rust-mode
  :ensure (rust-mode :host github :repo "rust-lang/rust-mode")
  :init
  (setq rust-mode-treesitter-derive t)
  :custom
  (rust-format-on-save t))

(use-package flymake-clippy
  :ensure (flymake-clippy :host github :repo "mgmarlow/flymake-clippy")
  :hook (eglot-managed-mode . (lambda ()
                                (when (derived-mode-p '(rust-mode rust-ts-mode))
                                  (flymake-clippy-setup-backend)))))

(provide 'init-developer-rust)
