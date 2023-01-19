;; init-developer-nix.el --- Developer nix settings -*- lexical-binding: t; -*-

(elpaca-use-package
 (nix-mode :host github :repo "NixOS/nix-mode")
 :mode "\\.nix\\'")

(elpaca-use-package
 (nix-update :host github :repo "jwiegley/nix-update-el"))

(provide 'init-developer-nix)
