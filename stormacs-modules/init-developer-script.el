;; init-developer-script.el --- Developer script languages settings -*- lexical-binding: t; -*-

(use-package fish-mode
  :ensure (fish-mode :host github :repo "wwwjfy/emacs-fish"))

(use-package gdscript-mode
  :ensure (gdscript-mode :host github :repo "godotengine/emacs-gdscript-mode"))

(use-package powershell
  :ensure (powershell :host github :repo "jschaf/powershell.el"))

(use-package protobuf-mode
  :ensure (protobuf-mode :host github :repo "protocolbuffers/protobuf"))

(use-package scad-mode)

(provide 'init-developer-script)
