;; init-developer-python.el --- Developer python settings -*- lexical-binding: t; -*-

(use-package lsp-pyright
  :elpaca (lsp-pyright :host github :repo "emacs-lsp/lsp-pyright"))

(use-package pyvenv
  :elpaca (pyvenv :host github :repo "jorgenschaefer/pyvenv"))

(provide 'init-developer-python)
