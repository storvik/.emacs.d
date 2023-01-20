;; init-developer.el --- Developer -*- lexical-binding: t; -*-

(elpaca-use-package
    (flycheck :host github :repo "flycheck/flycheck")
  :defer t
  :init
  ;;(add-hook 'prog-mode-hook #'flycheck-mode)
  :commands flycheck-mode)

(elpaca-use-package
    (lsp-mode :host github :repo "emacs-lsp/lsp-mode")
  :bind-keymap ("C-c l" . lsp-command-map)
  :bind (:map stormacs-prefix-map ("l" . stormacs-hydra-lsp/body))
  :hook (lsp-completion-mode . stormacs-lsp-mode-setup-completion)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-prefer-flymake nil)
  (lsp-auto-execute-action nil)
  (lsp-enable-indentation nil)
  (lsp-enable-snippet nil)
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  ;; Performance, see LSP wiki
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (defun stormacs-orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))
  (defun stormacs-lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  ;; Optionally configure the first word as flex filtered.
  (add-hook 'orderless-style-dispatchers #'stormacs-orderless-dispatch-flex-first nil 'local)
  (defhydra stormacs-hydra-lsp (:exit t :hint nil)
    "
  ^^^^^^^^^^                                                                                                                 ╭──────────┐
  Symbol^^            ^ ^                      Consult^^                 Buffer^^                  LSP Server^^              │ LSP mode │
 ╭^^^^^^^^^^─────────────────────────────────────────────────────────────────────────────────────────────────────────────────┴──────────╯
  [_d_] Declaration  [_i_] Implementation      [_s_] Symbol              [_f_] Format              [_M-r_] Restart
  [_D_] Definition   [_t_] Type                [_S_] Symbol current file [_m_] Imenu               [_M-S_] Shutdown
  [_R_] References   [_x_] Signature           [_h_] Diagnostic          [_x_] Execute action      [_M-s_] Describe session
  [_r_] Rename       [_o_] Documentation
  "
    ("d" lsp-find-declaration)
    ("D" lsp-find-definition)
    ("R" lsp-find-references)
    ("i" lsp-find-implementation)
    ("t" lsp-find-type-definition)
    ("x" lsp-signature-help)
    ("o" lsp-describe-thing-at-point)
    ("r" lsp-rename)

    ("s" consult-lsp-symbols)
    ("S" consult-lsp-file-symbols)
    ("h" consult-lsp-diagnostics)

    ("f" lsp-format-buffer)
    ("m" lsp-ui-imenu)
    ("x" lsp-execute-code-action)

    ("M-s" lsp-describe-session)
    ("M-r" lsp-restart-workspace)
    ("M-S" lsp-shutdown-workspace)

    ("g" nil)
    ("q" nil)))

(elpaca-use-package
    (lsp-ui :host github :repo "emacs-lsp/lsp-ui")
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-use-webkit t)
  :custom-face
  (lsp-ui-doc-background ((t (:background nil)))))

(elpaca-use-package
    (consult-lsp :host github :repo "gagbo/consult-lsp")
  :after (consult lsp-mode)
  :commands consult-lsp-symbols
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

;; TODO: Check if integrated tree-sitter affects this
;; (elpaca-use-package
;;  (tree-sitter :host github :repo "emacs-tree-sitter/elisp-tree-sitter")
;;  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
;;  :config
;;  (global-tree-sitter-mode))

;; (elpaca-use-package
;;  (tree-sitter-langs :repo "emacs-tree-sitter/tree-sitter-langs"))

(elpaca-use-package
    (apheleia :host github :repo "raxod502/apheleia")
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
(require 'init-developer-lisp)
(require 'init-developer-go)
(require 'init-developer-nix)
(require 'init-developer-python)
(require 'init-developer-script)
(require 'init-developer-tex)
(require 'init-developer-web)

(elpaca-use-package
    (envrc :host github :repo "purcell/envrc")
  :when (executable-find "direnv")
  :config
  (envrc-global-mode))

(provide 'init-developer)
