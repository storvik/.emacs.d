;; init-developer-script.el --- Developer script languages settings -*- lexical-binding: t; -*-

(use-package fish-mode
  :ensure (fish-mode :host github :repo "wwwjfy/emacs-fish"))

(use-package gdscript-mode
  :ensure (gdscript-mode :host github :repo "godotengine/emacs-gdscript-mode"))

(use-package powershell
  :ensure (powershell :host github :repo "jschaf/powershell.el"))

(use-package flatbuffers-mode
  :ensure (flatbuffers-mode :host github :repo "Asalle/flatbuffers-mode"))

(use-package protobuf-mode
  :ensure (protobuf-mode :host github :repo "protocolbuffers/protobuf"))

(use-package scad-mode
  :ensure (scad-mode :host github :repo "openscad/emacs-scad-mode")
  :after eglot
  :config
  (add-to-list 'eglot-server-programs
		       '(scad-mode . ("openscad-lsp" "--stdio"))))

(provide 'init-developer-script)
