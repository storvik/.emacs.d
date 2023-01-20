;; init-common.el --- Common settings -*- lexical-binding: t; -*-

;; Prefere UTF-8 coding
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)

(fset 'yes-or-no-p 'y-or-n-p)                           ;; Use y/n prompts instead of yes/no
(require 'uniquify)                                     ;; Make sure buffers have unique names

(setq-default indent-tabs-mode nil)                     ;; Use spaces instead of tabs
(setq-default tab-width 4)                              ;; Default tab width
(setq project-vc-extra-root-markers '(".project"))      ;; Consider dirs with .project file as project
(setq inhibit-startup-message t)                        ;; Turn off startup message
(setq initial-scratch-message "")                       ;; Set default startup message in scratch buffer
(setq inhibit-startup-echo-area-message t)              ;; Turn off echo message
(setq ring-bell-function 'ignore)                       ;; Turn off audible bell
(setq compilation-scroll-output t)                      ;; Make compilation buffer scroll
(setq require-final-newline t)                          ;; Add newline to the end of files
(setq recentf-max-saved-items 2048)                     ;; Max recent files

(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "emacs.saves/"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "emacs.saves/") t)))

(add-hook 'elpaca-after-init-hook (lambda () (recentf-mode 1)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(elpaca-use-package
    (exec-path-from-shell :host github :repo "purcell/exec-path-from-shell")
  :when sys-unix-p
  :init
  (exec-path-from-shell-initialize))

(elpaca-use-package
    (hydra :host github :repo "abo-abo/hydra"))

(elpaca-use-package
    (sudo-edit :host github :repo "nflath/sudo-edit"))

(setq ispell-dictionary "en")
(elpaca-use-package
    (spell-fu :host codeberg :repo "ideasman42/emacs-spell-fu"))

(provide 'init-common)
