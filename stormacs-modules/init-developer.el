;; init-developer.el --- Developer -*- lexical-binding: t; -*-

;; Use completing read functions instead of xref popup
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

;; Make compilation buffer scroll
(setq compilation-scroll-output t)

;; https://stackoverflow.com/questions/17659212/dont-display-compilation-buffer-in-emacs-until-the-process-exits-with-error-o/17788551#17788551
;; Change compilation start so that compilation buffer is not opened when it's open in another
;; window accross all frames. This is useful when working on multiple monitors.
(defadvice compilation-start
    (around inhibit-display
            (command &optional mode name-function highlight-regexp))
  (if (get-buffer-window "*compilation*" t)
      (flet ((display-buffer)
             (set-window-point)
             (goto-char))
        (fset 'display-buffer 'ignore)
        (fset 'goto-char 'ignore)
        (fset 'set-window-point 'ignore)
        (save-window-excursion
          ad-do-it))
    ad-do-it))
(ad-activate 'compilation-start)

;; TODO: Add notify function for this??
;; (defun stormacs-compile-finish (buffer outstr)
;;   (unless (string-match "finished" outstr)
;;     (switch-to-buffer-other-window buffer))
;;   t)
;; (setq compilation-finish-functions 'stormacs-compile-finish)


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

(use-package dape
  :ensure (dape :host github :repo "svaante/dape")
  ;; :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  ;; :hook
  ;; Save breakpoints on quit
  ;; (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  ;; (after-init . dape-breakpoint-load)

  :config
  ;; Turn on global bindings for setting breakpoints with mouse
  ;; (dape-breakpoint-global-mode)

  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'right)

  ;; Info buffers like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)
  ;; (setq dape-info-hide-mode-line nil)

  ;; Pulse source line (performance hit)
  (add-hook 'dape-display-source-hook 'pulsar-highlight-line)

  ;; Showing inlay hints
  ;; (setq dape-inlay-hints t)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook 'kill-buffer)
  )

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

(use-package aider
  :ensure (:host github :repo "tninja/aider.el")
  :bind ("C-c a" . aider-transient-menu)
  :config
  ;; Use claude-3-5-sonnet cause it is best in aider benchmark
  (setq aider-args `("--model" "anthropic/claude-3-5-sonnet-20241022"
                     "--anthropic-api-key" ,(anthropic-api-key)
                     "--no-auto-commits")))

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
