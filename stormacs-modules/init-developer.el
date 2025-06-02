;; init-developer.el --- Developer -*- lexical-binding: t; -*-

;; Use completing read functions instead of xref popup
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

;; Add prefix map used in M-.
;; Map should contain navigation and some AI related stuff.
(global-unset-key (kbd "M-."))
(define-prefix-command 'stormacs-xref-keymap)
(bind-keys :prefix-map stormacs-xref-keymap
           :prefix-docstring "Stormacs xref map"
           :prefix "M-.")
(define-key stormacs-xref-keymap (kbd ",") 'xref-go-back)
(define-key stormacs-xref-keymap (kbd ".") 'xref-find-definitions)
(define-key stormacs-xref-keymap (kbd "/") 'xref-find-references)

;; Assign stormacs-xref-keymap to M-. globally
(global-set-key (kbd "M-.") stormacs-xref-keymap)

(use-package emacs
  :ensure nil
  :bind (:map stormacs-overrides-minor-mode-map
              ("M-C" . recompile))
  :custom
  (compilation-scroll-output t)
  (compilation-auto-jump-to-first-error nil)
  :config
  ;; https://stackoverflow.com/questions/17659212/dont-display-compilation-buffer-in-emacs-until-the-process-exits-with-error-o/17788551#17788551
  ;; Change compilation start so that compilation buffer is not opened when it's open in another
  ;; window accross all frames. This is useful when working on multiple monitors.
  (require 'cl-lib)
  (defadvice compilation-start
      (around inhibit-display
              (command &optional mode name-function highlight-regexp))
    (if (get-buffer-window "*compilation*" t)
        (cl-flet ((display-buffer (&rest args) (ignore))
                  (goto-char (point))
                  (set-window-point (selected-window)))
          (save-window-excursion
            ad-do-it))
      ad-do-it))
  (ad-activate 'compilation-start)

  (defun stormacs-compile-finish (buffer outstr)
    (alert (concat "Compilation finished "
                   (if (string-match "finished" outstr)
                       "successfully"
                     "with errors"))
           :title "Emacs" :id 'compilation-notification :style 'osx-notifier)
    t)
  (setq compilation-finish-functions 'stormacs-compile-finish))

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
  :hook (prog-mode . symbol-overlay-mode)
  :custom
  (symbol-overlay-inhibit-map t)
  :config
  (transient-define-prefix stormacs-tsc-symbol-overlay ()
    "Prefix with descriptions specified with slots."
    ["Symbol overlay\n"
     [("p" "previous" symbol-overlay-jump-prev :transient t)
      ("n" "next" symbol-overlay-jump-next :transient t)
      ("<" "first" symbol-overlay-jump-first :transient t)
      (">" "last" symbol-overlay-jump-last :transient t)]
     [("i" "put" symbol-overlay-put :transient t)
      ("y" "copy" symbol-overlay-save-symbol)
      ("r" "rename" symbol-overlay-rename)]
     [("q" "query replace" symbol-overlay-query-replace)
      ("t" "toggle scope" symbol-overlay-toggle-in-scope :transient t)
      ("d" "definition" symbol-overlay-jump-to-definition)
      ("m" "mark" symbol-overlay-echo-mark)]
     [("P" "prev symbol" symbol-overlay-switch-backward :transient t)
      ("N" "next symbol" symbol-overlay-switch-forward :transient t)
      ""
      ("d" "remove all" symbol-overlay-remove-all)]]))

(use-package visual-replace
  :ensure (visual-replace :host github :repo "szermatt/visual-replace")
  :bind (:map stormacs-overrides-minor-mode-map
              ("M-s v" . (lambda () (interactive) (call-interactively 'visual-replace))))
  :config
  (define-key visual-replace-mode-map (kbd "M-s")
              visual-replace-secondary-mode-map))

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

(use-package aidermacs
  :ensure (aidermacs :host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :bind (:map stormacs-overrides-minor-mode-map
              ("C-c a" . aidermacs-transient-menu))
  :config
  (setq aidermacs-use-architect-mode t)
  (setenv "OPENAI_API_KEY" (openai-api-key))
  (setq aidermacs-architect-model "o1-mini")
  (setenv "ANTHROPIC_API_KEY" (anthropic-api-key))
  (setq aidermacs-editor-model "anthropic/claude-3-5-sonnet-20241022"))

(use-package compile-multi
  :ensure (compile-multi :host github :repo "mohkale/compile-multi")
  :bind
  (:map stormacs-overrides-minor-mode-map
        ("M-c" . compile-multi))
  :config
  (defun current-project-root ()
    (project-root (project-current)))
  (setq compile-multi-default-directory #'current-project-root)
  (setq compile-multi-config
        '(((file-exists-p "Makefile")
           ("make:build" . "make build")
           ("make:test" . "make test")
           ("make:all" . "make all"))))
  (push `((file-exists-p "pubspec.yaml")
          ("flutter:run" . ,#'flutter-run)
          ("flutter:hot reload" . ,#'flutter-run-or-hot-reload)
          ("flutter:restart" . ,#'flutter-hot-restart)
          ("flutter:quit" . ,#'flutter-quit)
          ("flutter:build apk" . "flutter build apk"))
        compile-multi-config))

(use-package consult-compile-multi
  :ensure (consult-compile-multi
           :host github :repo "mohkale/compile-multi"
           :files (:defaults "extensions/consult-compile-multi/consult-compile-multi.el"))
  :after compile-multi
  :config
  (consult-compile-multi-mode))


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
