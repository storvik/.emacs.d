;; init-developer-nix.el --- Developer nix settings -*- lexical-binding: t; -*-

(use-package lsp-nix
  :elpaca (nix-mode :host github :repo "NixOS/nix-mode")
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-nix-nil-formatter ["nixpkgs-fmt"]))

(use-package nix-mode
  :elpaca (nix-mode :host github :repo "NixOS/nix-mode")
  :mode "\\.nix\\'"
  :hook (nix-mode . lsp-deferred))

(use-package nix-update
  :elpaca (nix-update :host github :repo "jwiegley/nix-update-el"))

(provide 'init-developer-nix)
