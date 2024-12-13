;; init-developer.el --- Developer -*- lexical-binding: t; -*-

;; Use completing read functions instead of xref popup
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

(use-package eglot
  :ensure (:inherit elpaca-menu-gnu-devel-elpa))

(use-package emacs
  :ensure nil
  :after (transient)
  :bind (:map stormacs-prefix-map ("l" . stormacs-tsc-developer))
  :hook (eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1)))
  :config
  (transient-define-prefix stormacs-tsc-developer ()
    "Prefix with descriptions specified with slots."
    ["Stormacs awesome dynamic developer transient\n"
     ["Symbol"
      ("d" "definitions" xref-find-definitions)
      ("r" "references" xref-find-references)
      ("D" "declaration" eglot-find-declaration)
      ("t" "type" eglot-find-typeDefinition)]

     [("R" "rename" eglot-rename)
      ("D" "declaration" eglot-find-declaration)]

     ["Consult"
      ("s" "symbol" consult-eglot-symbols)
      ("f" "flymake" consult-flymake)
      ("i" "imenu" consult-imenu)]

     ["Buffer"
      ("F" "format" eglot-format)
      ("x" "exec action" eglot-code-actions)
      ("E" "eldoc" eldoc)]

     ["Eglot"
      ("eh" "inlay hints" eglot-inlay-hints-mode)
      ("er" "restart" eglot-reconnect)
      ("es" "shutdown" eglot-shutdown)]]))

(use-package treesit-auto
  :ensure (treesit-auto :host github :repo "renzmann/treesit-auto")
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package consult-eglot
  :ensure (consult-eglot :host github :repo "mohkale/consult-eglot")
  :after consult
  :commands consult-eglot-symbols)

(use-package eldoc-box
  :ensure (eldoc-box :host github :repo "casouri/eldoc-box")
  :hook
  (eglot-managed-mode . eldoc-box-hover-mode)
  :custom
  (eldoc-box-clear-with-C-g t))

(use-package dumb-jump
  :ensure (dumb-jump :host github :repo "jacktasia/dumb-jump")
  :hook (xref-backend-functions . dumb-jump-xref-activate))

(use-package symbol-overlay
  :ensure (symbol-overlay :host github :repo "wolray/symbol-overlay")
  :hook (prog-mode . symbol-overlay-mode))

(use-package apheleia
  :ensure (apheleia :host github :repo "raxod502/apheleia")
  :config
  (add-to-list 'apheleia-formatters '(goimports "goimports"))
  (setf (alist-get 'go-mode apheleia-mode-alist) 'goimports)
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'goimports)
  (add-to-list 'apheleia-formatters '(nixpkgs-fmt "nixpkgs-fmt"))
  (setf (alist-get 'nix-mode apheleia-mode-alist) 'nixpkgs-fmt)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
  (apheleia-global-mode +1))

(require 'init-developer-c)
(require 'init-developer-config)
(require 'init-developer-dart)
(require 'init-developer-kotlin)
(require 'init-developer-lisp)
(require 'init-developer-go)
(require 'init-developer-rust)
(require 'init-developer-nix)
(require 'init-developer-python)
(require 'init-developer-script)
(require 'init-developer-tex)
(require 'init-developer-web)

(use-package envrc
  :ensure (envrc :host github :repo "purcell/envrc")
  :when (and sys-unix-p
             (executable-find "direnv"))
  :config
  (envrc-global-mode))

(provide 'init-developer)
