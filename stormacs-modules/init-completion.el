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

;; Preserve history avvross restarts
(use-package savehist
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
         ("M-." . embark-dwim)
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

(use-package corfu
  :ensure (corfu :host github :repo "minad/corfu")
  :after orderless
  :custom
  (corfu-cycle t)                     ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                      ;; Enable auto completion
  (corfu-quit-at-boundary 'separator) ;; Automatically quit at word boundary unless `corfu-insert-separator' has been used
  (corfu-separator ?\s)               ;; Separator is set to space, in order to use with ordeless
  (corfu-quit-no-match 'separator)    ;; Quit if no match, unless `corfu-insert-separator' has been used
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
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :ensure (nerd-icons-corfu :host github :repo "LuigiPiucco/nerd-icons-corfu")
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; (use-package kind-icon
;;   :ensure (kind-icon :host github :repo "jdtsmith/kind-icon")
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default)
;;   (kind-icon-blend-background nil)
;;   (kind-icon-blend-frac 0.08)
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
;;   (setq kind-icon-use-icons nil)
;;   (setq kind-icon-mapping
;;         `(
;;           (array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
;;           (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
;;           (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
;;           (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
;;           (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
;;           (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
;;           (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
;;           (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
;;           (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
;;           (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
;;           (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
;;           (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
;;           (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
;;           (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
;;           (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
;;           (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
;;           (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
;;           (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
;;           (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
;;           (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
;;           (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
;;           (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
;;           (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
;;           (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
;;           (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
;;           (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
;;           (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
;;           (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
;;           (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
;;           (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
;;           (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
;;           (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
;;           (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
;;           (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
;;           (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
;;           (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face))))

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

(provide 'init-completion)
