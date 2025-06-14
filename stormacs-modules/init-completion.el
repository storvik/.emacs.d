;; init-completion.el --- Completion framework and friends -*- lexical-binding: t; -*-

(use-package vertico
  :ensure (vertico :host github :repo "minad/vertico")
  :custom
  (vertico-count 20)
  (vertico-resize t)
  (vertico-cycle t)
  :custom-face
  (vertico-group-title ((t (:slant normal)))) ;; Fix unreadable vertico group titles, Iosevka Nerd Font doesn't like italic
  :bind
  (:map vertico-map
        ("C-M-n" . vertico-next-group)
        ("C-M-p" . vertico-previous-group))
  :init
  (vertico-mode))

;; Preserve history across restarts
(use-package emacs
  :ensure nil
  :init
  (savehist-mode))

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

(use-package vertico-directory
  :ensure (vertico-directory :host github :repo "minad/vertico" :files (:defaults "extensions/vertico-directory.el"))
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("C-M-m" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("C-M-i" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-quick
  :ensure (vertico-quick :host github :repo "minad/vertico" :files (:defaults "extensions/vertico-quick.el"))
  :after vertico
  :bind (:map vertico-map
              ("C-q" . vertico-quick-insert)
              ("M-q" . vertico-quick-exit)))

(use-package consult
  :ensure (consult :host github :repo "minad/consult")
  :bind (("C-x C-r" . consult-recent-file)
         :map stormacs-overrides-minor-mode-map
         ;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complet-command
         ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)     ;; orig. yank-pop
         ("<help> a" . consult-apropos) ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s R" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Customizations that map to ivy
         ("C-c o" . consult-file-externally)
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch) ;; orig. isearch-edit-string
         ("M-s l" . consult-line))   ;; required by consult-line to detect isearch
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; function that returns project root, works for project.el
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;; narrow key
  (setq consult-narrow-key "<"))

(use-package consult-info
  :ensure (consult-info :host github :repo "minad/consult")
  :after consult)

(use-package orderless
  :ensure (orderless :host github :repo "oantolin/orderless")
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package embark
  :ensure (embark :host github :repo "oantolin/embark")
  :after sudo-edit
  :demand t
  :bind (("C-." . embark-act)
         ;; ("M-." . embark-dwim)
         ("C-h b" . embark-bindings)
         :map embark-file-map
         ("s" . sudo-edit))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure (embark-consult :host github :repo "oantolin/embark")
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :ensure (marginalia :host github :repo "minad/marginalia")
  :after vertico
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package completion-preview
  :ensure nil
  :hook
  ((comint-mode-hook
    eshell-mode-hook
    prog-mode-hook) . completion-preview-mode)
  (minibuffer-setup-hook . completion-preview-enable-in-minibuffer)
  :bind
  (:map completion-preview-active-mode-map
        ("C-i" . completion-preview-complete)
        ("C-e" . completion-preview-insert))
  :init
  (setq completion-preview-adapt-background-color nil)
  (setq completion-preview-minimum-symbol-length 2)
  :config
  (push 'puni-backward-delete-char completion-preview-commands)
  (defun completion-preview-enable-in-minibuffer ()
    "Enable Corfu completion in the minibuffer, e.g., `eval-expression'."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (completion-preview-mode 1))))

(use-package corfu
  :ensure (corfu :host github :repo "minad/corfu")
  :after orderless
  :custom
  (corfu-cycle t)                     ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                      ;; Enable auto completion
  (corfu-quit-at-boundary 'separator) ;; Automatically quit at word boundary unless `corfu-insert-separator' has been used
  (corfu-separator ?\s)               ;; Separator is set to space, in order to use with ordeless
  (corfu-quit-no-match t)             ;; Quit if no match
  (tab-always-indent 'complete)       ;; Use tab to complete
  :bind (:map corfu-map
              ("C-SPC" . corfu-insert-separator)
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)
              ("M-l" . corfu-show-location)
              ("M-d" . corfu-show-documentation))
  :init
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  ;; Enable corfu globally
  (global-corfu-mode)
  ;; Extensions
  (corfu-history-mode))

(use-package nerd-icons-corfu
  :ensure (nerd-icons-corfu :host github :repo "LuigiPiucco/nerd-icons-corfu")
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package corfu-doc
  :ensure (corfu-doc :host github :repo "galeo/corfu-doc")
  :after corfu
  ;; :hook (corfu-mode . corfu-doc-mode)
  :bind (:map corfu-map
              ([remap corfu-show-documentation] . corfu-doc-toggle)
              ("M-n" . corfu-doc-scroll-up)
              ("M-p" . corfu-doc-scroll-down))
  :custom
  (corfu-doc-delay 0.5))

(use-package cape
  :ensure (cape :host github :repo "minad/cape")
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-emoji)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package affe
  :ensure (affe :host github :repo "minad/affe")
  :bind (:map stormacs-overrides-minor-mode-map
              ("M-s r" . affe-grep))
  :preface
  (defun affe-orderless-regexp-compiler (input _type)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  :config
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless-highlight-matches))

(use-package nerd-icons-completion
  :ensure (nerd-icons-completion :host github :repo "rainstormstudio/nerd-icons-completion")
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (with-eval-after-load 'stormacs-gui
    (nerd-icons-completion-mode)))

(use-package consult-project-extra
  :ensure (consult-project-extra :host github :repo "Qkessler/consult-project-extra")
  :bind (:map stormacs-overrides-minor-mode-map
              ("C-x p f" . consult-project-extra-find)
              ("C-x p o" . consult-project-extra-find-other-window)))

(use-package tempel
  :ensure (tempel :host github :repo "minad/tempel")
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :hook ((prog-mode . tempel-setup-capf)
         (conf-mode . tempel-setup-capf)
         (text-mode . tempel-setup-capf)
         (eglot-managed-mode . tempel-setup-capf)))

(provide 'init-completion)
