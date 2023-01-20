;; init-developer-script.el --- Developer script languages settings -*- lexical-binding: t; -*-

(elpaca-use-package
 (fish-mode :host github :repo "wwwjfy/emacs-fish"))

(elpaca-use-package
  (gdscript-mode :host github :repo "godotengine/emacs-gdscript-mode"))

(elpaca-use-package
 (powershell :host github :repo "jschaf/powershell.el"))

(elpaca-use-package
 (protobuf-mode :host github :repo "protocolbuffers/protobuf"))

(elpaca-use-package scad-mode)

(add-to-list 'auto-mode-alist '("\\.create\\'" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.drop\\'" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.alter\\'" . sql-mode))

(provide 'init-developer-script)
