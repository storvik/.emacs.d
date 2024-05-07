;; init-developer.el --- Developer -*- lexical-binding: t; -*-

;; Use completing read functions instead of xref popup
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

(use-package eglot
  :ensure (:inherit elpaca-menu-gnu-devel-elpa))

(elpaca nil
  (use-package emacs
    :ensure nil
    :bind (:map stormacs-prefix-map ("l" . stormacs-tsc-developer))
    :hook (eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1)))
    :config
    (transient-define-prefix stormacs-tsc-developer ()
      "Prefix with descriptions specified with slots."
      ["Stormacs awesome dynamic developer transient\n"
       ["Symbol"
        ("d" "definitions" xref-find-definitions :transient t)
        ("r" "references" xref-find-references :transient t)
        ("D" "declaration" eglot-find-declaration :transient t)
        ("t" "type" eglot-find-typeDefinition :transient t)]

       [("R" "rename" eglot-rename :transient t)
        ("D" "declaration" eglot-find-declaration :transient t)]

       ["Consult"
        ("s" "symbol" consult-eglot-symbols :transient t)
        ("f" "flymake" consult-flymake :transient t)
        ("i" "imenu" consult-imenu :transient t)]

       ["Buffer"
        ("F" "format" eglot-format :transient t)
        ("x" "exec action" eglot-code-actions :transient t)
        ("E" "eldoc" eldoc :transient t)]

       ["Eglot"
        ("eh" "inlay hints" eglot-inlay-hints-mode)
        ("er" "restart" eglot-reconnect)
        ("es" "shutdown" eglot-shutdown)]])))

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

(use-package apheleia
  :ensure (apheleia :host github :repo "raxod502/apheleia")
  :config
  (setf (alist-get 'clang-format apheleia-formatters)
        '("clang-format" "-style={BasedOnStyle: Google, IndentWidth: 4, ColumnLimit: 120, SortIncludes: false}"))
  (add-to-list 'apheleia-formatters '(goimports "goimports"))
  (setf (alist-get 'go-mode apheleia-mode-alist) 'goimports)
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'goimports)
  (add-to-list 'apheleia-formatters '(nixpkgs-fmt "nixpkgs-fmt"))
  (setf (alist-get 'nix-mode apheleia-mode-alist) 'nixpkgs-fmt)
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
