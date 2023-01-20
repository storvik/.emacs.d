;; init-detached.el --- Detached -*- lexical-binding: t; -*-

(elpaca-use-package
    (detached :host sourcehut :repo "niklaseklund/detached.el")
  :when (executable-find "dtach")
  :init
  (detached-init)
  :bind (([remap detached-open-session] . detached-consult-session)
         ([remap async-shell-command] . detached-shell-command)
         ([remap compile] . detached-compile)
         ([remap recompile] . detached-compile-recompile)
         :map stormacs-prefix-map
         ("d" . stormacs-detached-hydra/body))
  :custom ((detached-show-output-on-attach t))
  :config
  (defvar embark-detached-map (make-composed-keymap detached-action-map embark-general-map))
  (add-to-list 'embark-keymap-alist '(detached . embark-detached-map))
  (defun stormacs-detached-dired-rsync (command _details)
    "Run COMMAND with `detached'."
    (let ((detached-local-session t)
          (detached-session-origin 'rsync))
      (detached-start-session command t)))
  (advice-add #'dired-rsync--do-run :override #'stormacs-detached-dired-rsync)
  (defhydra stormacs-detached-hydra (:color pink :exit t :hint nil)
    "
                                                                                                    ╭──────────┐
                                                                                                    │ detached │
 ╭───────────────────────────────────────────────────────----------------------------───────────────┴──────────╯
  [_o_] open                [_k_] kill                 [_t_] tail                  [_c_] compile
  [_a_] attach              [_d_] delete               [_=_] diff                  [_w_] copy command
  [_v_] view                [_r_] rerun                [_i_] insert command        [_W_] copy
"
    ("o" detached-open-session)
    ("a" detached-attach-session)
    ("c" detached-compile-session)
    ("k" detached-kill-session)
    ("v" detached-view-session)
    ("d" detached-delete-session)
    ("W" detached-copy-session)
    ("r" detached-rerun-session)
    ("t" detached-tail-session)
    ("=" detached-diff-session)
    ("i" detached-insert-session-command)
    ("w" detached-copy-session-command)
    ("q" nil "cancel")))

(provide 'init-detached)
