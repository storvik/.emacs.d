;; init-detached.el --- Detached -*- lexical-binding: t; -*-

(use-package detached
  :ensure (detached :host sourcehut :repo "niklaseklund/detached.el")
  :when (executable-find "dtach")
  :init
  (detached-init)
  :bind (([remap detached-open-session] . detached-consult-session)
         ([remap async-shell-command] . detached-shell-command))
  :custom ((detached-show-output-on-attach t))
  :config
  (defvar embark-detached-map (make-composed-keymap detached-action-map embark-general-map))
  (add-to-list 'embark-keymap-alist '(detached . embark-detached-map))
  (defun stormacs-detached-dired-rsync (command _details)
    "Run COMMAND with `detached'."
    (let ((detached-local-session t)
          (detached-session-origin 'rsync))
      (detached-start-session command t)))
  (advice-add #'dired-rsync--do-run :override #'stormacs-detached-dired-rsync))

(provide 'init-detached)
