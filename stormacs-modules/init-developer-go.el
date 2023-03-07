;; init-developer-go.el --- Developer go settings -*- lexical-binding: t; -*-

(use-package go-mode
  :elpaca (go-mode :host github :repo "dominikh/go-mode.el"))

(elpaca
    nil
  (use-package emacs
    :elpaca nil
    :after lsp-mode
    :init
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection "gopls")
                      :major-modes '(go-mode)
                      :remote? t
                      :server-id 'gopls-remote))))

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

(provide 'init-developer-go)
