;; init-developer.el --- Developer -*- lexical-binding: t; -*-

(use-package flycheck
  :elpaca (flycheck :host github :repo "flycheck/flycheck")
  :defer t
  :init
  ;;(add-hook 'prog-mode-hook #'flycheck-mode)
  :commands flycheck-mode)

(elpaca nil
  (use-package emacs
    :elpaca nil
    :bind (:map stormacs-prefix-map ("l" . stormacs-hydra-eglot/body))
    :hook (eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1)))
    :config
    (defhydra stormacs-hydra-eglot (:exit t :hint nil)
      "
  ^^^^^^^^^^                                                                                                                 ╭──────────┐
  Symbol^^            ^ ^                      Consult^^                 Buffer^^                  Server^^                  │ eglot    │
 ╭^^^^^^^^^^─────────────────────────────────────────────────────────────────────────────────────────────────────────────────┴──────────╯
  [_d_] Declaration  [_i_] Implementation      [_s_] Symbol              [_f_] Format              [_M-h_] Inlay hints mode
  [_D_] Definition   [_t_] Type                [_F_] Flymake             [_x_] Execute action      [_M-r_] Restart
  [_R_] References   [_r_] Rename               ^ ^                      [_e_] Eldoc               [_M-S_] Shutdown
  "
      ("d" eglot-find-declaration)
      ("D" xref-find-definitions)
      ("R" xref-find-references)
      ("i" eglot-find-implementation)
      ("t" eglot-fint-typeDefinition)
      ("r" eglot-rename)

      ("s" consult-eglot-symbols)
      ("F" consult-flymake)

      ("f" eglot-format-buffer)
      ("x" eglot-code-actions)
      ("e" eldoc)

      ("M-h" eglot-inlay-hints-mode)
      ("M-r" eglot-reconnect)
      ("M-S" eglot-shutdown)

      ("g" nil)
      ("q" nil))))

(use-package consult-eglot
  :elpaca (consult-eglot :host github :repo "mohkale/consult-eglot")
  :after consult
  :commands consult-eglot-symbols)

(use-package eldoc-box
  :elpaca (eldoc-box :host github :repo "casouri/eldoc-box")
  :hook
  (eglot-managed-mode . eldoc-box-hover-mode)
  :custom
  (eldoc-box-clear-with-C-g t))

(use-package apheleia
  :elpaca (apheleia :host github :repo "raxod502/apheleia")
  :config
  (setf (alist-get 'clang-format apheleia-formatters)
        '("clang-format" "-style={BasedOnStyle: Google, IndentWidth: 4, ColumnLimit: 120, SortIncludes: false}"))
  (add-to-list 'apheleia-formatters '(goimports "goimports"))
  (setf (alist-get 'go-mode apheleia-mode-alist) 'goimports)
  (add-to-list 'apheleia-formatters '(nixpkgs-fmt "nixpkgs-fmt"))
  (setf (alist-get 'nix-mode apheleia-mode-alist) 'nixpkgs-fmt)
  (setf (alist-get 'clj-zprint apheleia-formatters)
        '("zprint" "{:style [:community :justified] :map {:comma? false}}"))
  (add-to-list 'apheleia-mode-alist '(clojure-mode . clj-zprint))
  (setf (alist-get 'cljs-zprint apheleia-formatters)
        '("zprint" "{:style [:hiccup] :map {:comma? false}}"))
  (add-to-list 'apheleia-mode-alist '(clojurescript-mode . cljs-zprint))
  (apheleia-global-mode +1))

(require 'init-developer-c)
(require 'init-developer-config)
(require 'init-developer-dart)
(require 'init-developer-kotlin)
(require 'init-developer-lisp)
(require 'init-developer-go)
(require 'init-developer-nix)
(require 'init-developer-python)
(require 'init-developer-script)
(require 'init-developer-tex)
(require 'init-developer-web)

(use-package envrc
  :elpaca (envrc :host github :repo "purcell/envrc")
  :when (executable-find "direnv")
  :config
  (envrc-global-mode))

(provide 'init-developer)
