;; init-developer-go.el --- Developer go settings -*- lexical-binding: t; -*-

(use-package go-mode
  :elpaca (go-mode :host github :repo "dominikh/go-mode.el"))

(use-package go-add-tags
  :elpaca (go-add-tags :host github :repo "syohex/emacs-go-add-tags")
  :requires go-mode)

(use-package go-dlv
  :elpaca (go-dlv :host github :repo "benma/go-dlv.el")
  :requires go-mode)

(use-package go-guru
  :elpaca (go-guru :host github :repo "dominikh/go-mode.el")
  :requires go-mode)

(use-package go-stacktracer
  :elpaca (go-stacktracer :host github :repo "samertm/go-stacktracer.el")
  :requires go-mode)

(use-package gotest
  :elpaca (gotest :host github :repo "nlamirault/gotest.el")
  :requires go-mode)

(use-package flymake-golangci
  :elpaca (flymake-golangci :host github :repo "storvik/flymake-golangci")
  :hook (eglot-managed-mode . (lambda ()
                                (when (derived-mode-p '(go-mode go-ts-mode))
                                  (flymake-golangci-load-backend)))))

(provide 'init-developer-go)
