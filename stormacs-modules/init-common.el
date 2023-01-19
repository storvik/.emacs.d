;; init-common.el --- Common settings -*- lexical-binding: t; -*-

(elpaca-use-package
 nil
 :hook (before-save . delete-trailing-whitespace)
 :init
 (unless (getenv "WSL")
   (when (boundp 'pgtk-initialized)
     (setq default-frame-alist '((undecorated . t)))))   ;; Remove title bar when using pgtk, but not when WSL is set
 (when (fboundp 'pixel-scroll-precision-mode)
   (pixel-scroll-precision-mode 1))                      ;; Turn on pixel scroll precision mode if available
 (prefer-coding-system 'utf-8-unix)
 (set-default-coding-systems 'utf-8-unix)
 (set-terminal-coding-system 'utf-8-unix)
 (set-keyboard-coding-system 'utf-8-unix)
 (setq-default indent-tabs-mode nil)                     ;; Use spaces instead of tabs
 (setq-default tab-width 4)                              ;; Default tab width
 (fset 'yes-or-no-p 'y-or-n-p)                           ;; Use y/n prompts instead of yes/no
 (require 'uniquify)                                     ;; Make sure buffers have unique names
 (setq project-vc-extra-root-markers '(".project"))
 :config
 (recentf-mode 1)                                        ;; Recentf mode
 :custom
 (inhibit-startup-message t)                             ;; Turn off startup message
 (initial-scratch-message "")                            ;; Set default startup message in scratch buffer
 (inhibit-startup-echo-area-message t)                   ;; Turn off echo message
 (ring-bell-function 'ignore)                            ;; Turn off audible bell
 (compilation-scroll-output t)                           ;; Make compilation buffer scroll
 (require-final-newline t)                               ;; Add newline to the end of files
 (setq recentf-max-saved-items 2048)                     ;; Max recent files
 (backup-directory-alist `((".*" . ,(concat user-emacs-directory "emacs.saves/"))))
 (auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "emacs.saves/") t))))

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
