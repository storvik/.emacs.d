;; init-developer-python.el --- Developer python settings -*- lexical-binding: t; -*-

(use-package pyvenv
  :ensure (pyvenv :host github :repo "jorgenschaefer/pyvenv"))

(use-package flymake-ruff
  :ensure (flymake-ruff :host github :repo "erickgnavar/flymake-ruff")
  :hook ((python-mode . flymake-ruff-load)
         (eglot-managed-mode . (lambda ()
                                 (when (derived-mode-p '(python-mode python-ts-mode))
                                   (flymake-ruff-load))))))

(provide 'init-developer-python)
