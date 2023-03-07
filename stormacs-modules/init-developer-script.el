;; init-developer-script.el --- Developer script languages settings -*- lexical-binding: t; -*-

(use-package fish-mode
  :elpaca (fish-mode :host github :repo "wwwjfy/emacs-fish"))

(use-package gdscript-mode
  :elpaca (gdscript-mode :host github :repo "godotengine/emacs-gdscript-mode"))

(use-package powershell
  :elpaca (powershell :host github :repo "jschaf/powershell.el"))

(use-package protobuf-mode
  :elpaca (protobuf-mode :host github :repo "protocolbuffers/protobuf"))

(use-package scad-mode)

(provide 'init-developer-script)
