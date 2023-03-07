;; init-developer-nix.el --- Developer nix settings -*- lexical-binding: t; -*-

(use-package nix-mode
  :elpaca (nix-mode :host github :repo "NixOS/nix-mode")
  :mode "\\.nix\\'")

(use-package nix-update
  :elpaca (nix-update :host github :repo "jwiegley/nix-update-el"))

(provide 'init-developer-nix)
