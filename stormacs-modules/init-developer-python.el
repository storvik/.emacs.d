;; init-developer-python.el --- Developer python settings -*- lexical-binding: t; -*-

(elpaca-use-package
 (lsp-pyright :host github :repo "emacs-lsp/lsp-pyright"))

(elpaca-use-package
 (pyvenv :host github :repo "jorgenschaefer/pyvenv"))

(provide 'init-developer-python)
