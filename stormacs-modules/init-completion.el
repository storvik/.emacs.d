;; init-completion.el --- Completion framework and friends -*- lexical-binding: t; -*-

(elpaca-use-package
    (vertico :host github :repo "minad/vertico")
  :custom
  (vertico-count 20)
  (vertico-resize t)
  (vertico-cycle t)
  :bind
  (:map vertico-map
        ("C-M-n" . vertico-next-group)
        ("C-M-p" . vertico-previous-group))
  :init
  (vertico-mode))

;; Preserve history avvross restarts
(use-package savehist
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

(elpaca-use-package
    (consult :host github :repo "minad/consult")
  :bind (:map stormacs-overrides-minor-mode-map
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
         ("C-x C-r" . consult-recent-file)
         ("C-c o" . consult-file-externally)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         ;; ("s-t" . jnf/consult-find-using-fd)
         :map isearch-mode-map
         ("M-e" . consult-isearch)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch) ;; orig. isearch-edit-string
         ("M-s l" . consult-line))                 ;; required by consult-line to detect isearch
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
  (setq consult-narrow-key "<")
  (with-eval-after-load 'vertico
    (require 'consult-vertico)))

(elpaca-use-package
    (orderless :host github :repo "oantolin/orderless")
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(elpaca-use-package
    (embark :host github :repo "oantolin/embark")
  :after sudo-edit
  :demand t
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h b" . embark-bindings)
         :map embark-file-map
         ("s" . sudo-edit))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

(elpaca-use-package
    (embark-consult :host github :repo "oantolin/embark")
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(elpaca-use-package
    (marginalia :host github :repo "minad/marginalia")
  :after vertico
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(elpaca-use-package
    (corfu :host github :repo "minad/corfu")
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
  (with-eval-after-load 'st/gui
    (set-face-attribute 'corfu-current nil
                        :background (color-darken-name (face-background 'corfu-current) 2)))
  ;; Enable corfu globally
  (global-corfu-mode))

(elpaca-use-package
    (kind-icon :host github :repo "jdtsmith/kind-icon")
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(elpaca-use-package
    (corfu-doc :host github :repo "galeo/corfu-doc")
  :after corfu
  ;; :hook (corfu-mode . corfu-doc-mode)
  :bind (:map corfu-map
         ([remap corfu-show-documentation] . corfu-doc-toggle)
         ("M-n" . corfu-doc-scroll-up)
         ("M-p" . corfu-doc-scroll-down))
  :custom
  (corfu-doc-delay 0.5))

(elpaca-use-package
    (cape :host github :repo "minad/cape")
  :bind (:map stormacs-prefix-map ("p" . stormacs-cape-hydra/body))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :config
  (defhydra stormacs-cape-hydra (:color pink :exit t :hint nil)
    "
^cape completion
^^^^^^^^-------------------------------------------------------------------
_p_: completion           _s_: symbol               _-_: tex
_t_: tag                  _a_: abbrev               _&_: sgml
_d_: dabbrev              _i_: ispell               _r_: rfc1345
_f_: file                 _l_: line
_k_: keyword              _w_: dict
"
    ("p" completion-at-point)
    ("t" complete-tag)
    ("d" cape-dabbrev)
    ("f" cape-file)
    ("k" cape-keyword)
    ("s" cape-symbol)
    ("a" cape-abbrev)
    ("i" cape-ispell)
    ("l" cape-line)
    ("w" cape-dict)
    ("-" cape-tex)
    ("&" cape-sgml)
    ("r" cape-rfc1345)
    ("q" nil "cancel")))

(elpaca-use-package
    (affe :host github :repo "minad/affe")
  :bind (:map stormacs-overrides-minor-mode-map
         ("M-s r" . affe-grep))
  :preface
  (defun affe-orderless-regexp-compiler (input _type)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  :config
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless-highlight-matches))

(elpaca-use-package
    (all-the-icons-completion :host github :repo "iyefrat/all-the-icons-completion")
  :after marginalia
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (with-eval-after-load 'stormacs-gui
    (all-the-icons-completion-marginalia-setup)))

(elpaca-use-package
    (consult-project-extra :host github :repo "Qkessler/consult-project-extra")
  :bind (:map stormacs-overrides-minor-mode-map
         ("C-x p f" . consult-project-extra-find)
         ("C-x p o" . consult-project-extra-find-other-window)))

(provide 'init-completion)
