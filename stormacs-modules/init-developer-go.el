;; init-developer-go.el --- Developer go settings -*- lexical-binding: t; -*-

(use-package go-mode
  :ensure (go-mode :host github :repo "dominikh/go-mode.el")
  :custom
  (go-ts-mode-indent-offset 4))

(use-package go-add-tags
  :ensure (go-add-tags :host github :repo "syohex/emacs-go-add-tags")
  :requires go-mode)

(use-package go-dlv
  :ensure (go-dlv :host github :repo "benma/go-dlv.el")
  :requires go-mode)

(use-package go-guru
  :ensure (go-guru :host github :repo "dominikh/go-mode.el")
  :requires go-mode)

(use-package go-stacktracer
  :ensure (go-stacktracer :host github :repo "samertm/go-stacktracer.el")
  :requires go-mode)

(use-package gotest
  :ensure (gotest :host github :repo "nlamirault/gotest.el")
  :requires go-mode)

(use-package flymake-golangci
  :ensure (flymake-golangci :host github :repo "storvik/flymake-golangci")
  :hook (eglot-managed-mode . (lambda ()
                                (when (derived-mode-p '(go-mode go-ts-mode))
                                  (flymake-golangci-load-backend)))))

(provide 'init-developer-go)
