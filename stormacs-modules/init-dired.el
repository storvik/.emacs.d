;; init-dired.el --- Dired settings -*- lexical-binding: t; -*-

(put 'dired-find-alternate-file 'disabled nil)

;; Fix dired on darwin, macos ls not working
;; gls is a part of coreutils
(when (eq system-type 'darwin)
  (setq insert-directory-program "gls"
        dired-use-ls-dired t))

(setq dired-listing-switches "-al --group-directories-first")

(use-package dired+
  :ensure (dired+ :host github :repo "emacsmirror/dired-plus" :main "dired+.el")
  :init
  (setq diredp-hide-details-initially-flag nil)
  :config
  (diredp-toggle-find-file-reuse-dir 1))

(use-package nerd-icons-dired
  :ensure (nerd-icons-dired :host github :repo "rainstormstudio/nerd-icons-dired")
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dired-rsync
  :ensure (dired-rsync :host github :repo "stsquad/dired-rsync")
  :config
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map))

(use-package dired-preview
  :ensure (dired-preview :host github :repo "protesilaos/dired-preview")
  :config
  (setq dired-preview-delay 0.01)
  (add-to-list 'dired-preview-trigger-commands #'diredp-next-line)
  (add-to-list 'dired-preview-trigger-commands #'diredp-previous-line)
  (dired-preview-global-mode 1))

(use-package dired-sidebar
  :ensure (dired-sidebar :host github :repo "jojojames/dired-sidebar")
  :commands (dired-sidebar-toggle-sidebar)
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-theme 'nerd)
  (setq dired-sidebar-use-custom-font t))

(provide 'init-dired)
