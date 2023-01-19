;; init-developer-go.el --- Developer go settings -*- lexical-binding: t; -*-

(elpaca-use-package
 (go-mode :host github :repo "dominikh/go-mode.el"))

;; TODO: Why is this not working?
;; (elpaca-use-package
;;  nil
;;  :after lsp-mode
;;  :init
;;  (lsp-register-client
;;   (make-lsp-client :new-connection (lsp-tramp-connection "gopls")
;;                    :major-modes '(go-mode)
;;                    :remote? t
;;                    :server-id 'gopls-remote)))

(elpaca-use-package
 (go-add-tags :host github :repo "syohex/emacs-go-add-tags")
 :requires go-mode)

(elpaca-use-package
 (go-dlv :host github :repo "benma/go-dlv.el")
 :requires go-mode)

(elpaca-use-package
 (go-guru :host github :repo "dominikh/go-mode.el")
 :requires go-mode)

(elpaca-use-package
 (go-stacktracer :host github :repo "samertm/go-stacktracer.el")
 :requires go-mode)

(elpaca-use-package
 (gotest :host github :repo "nlamirault/gotest.el")
 :requires go-mode)

(provide 'init-developer-go)
