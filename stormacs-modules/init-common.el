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

(use-package exec-path-from-shell
  :elpaca (exec-path-from-shell :host github :repo "purcell/exec-path-from-shell")
  :when sys-unix-p
  :init
  (exec-path-from-shell-initialize))

(use-package hydra
    :elpaca (hydra :host github :repo "abo-abo/hydra"))

(use-package sudo-edit
    :elpaca (sudo-edit :host github :repo "nflath/sudo-edit"))

(use-package spell-fu
    :elpaca (spell-fu :host codeberg :repo "ideasman42/emacs-spell-fu")
  :bind (:map stormacs-prefix-map
         ("s" . stormacs-hydra-spell/body))
  :config
  (defun stormacs-spell-remove-all ()
    (interactive)
    (mapc (lambda (dict) (spell-fu-dictionary-remove dict)) spell-fu-dictionaries))
  (defun stormacs-spell-add-en ()
    (interactive)
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en-computers"))
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en-science"))
    (spell-fu-dictionary-add
     (spell-fu-get-personal-dictionary "en-personal" (expand-file-name "aspell.en.pws" spell-fu-directory))))
  (defun stormacs-spell-add-nb ()
    (interactive)
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "nb"))
    (spell-fu-dictionary-add
     (spell-fu-get-personal-dictionary "nb-personal" (expand-file-name "aspell.nb.pws" spell-fu-directory))))
  (defhydra stormacs-hydra-spell (:color pink :exit nil :hint nil)
    "
 ^^^^^^^^                                                                                         ╭──────────┐
 navigation^^              personal^^               language^^                spell-fu^^          │ spelling │
╭^^^^^^^^─────────────────────────────────────────────────────────────────────────────────────────┴──────────╯
 [_c_] check buffer        [_w_] word add           [_E_] english             [_e_] enable / disable
 [_n_] next error          [_W_] word remove        [_N_] norwegian           [_r_] reset
 [_p_] previous error       ^ ^                     [_R_] remove all          [_q_] quit
"
    ("c" spell-fu-buffer)
    ("n" spell-fu-goto-next-error)
    ("p" spell-fu-goto-previous-error)
    ("w" spell-fu-word-add)
    ("W" spell-fu-word-remove)
    ("E" stormacs-spell-add-en)
    ("N" stormacs-spell-add-nb)
    ("R" stormacs-spell-remove-all)
    ("e" spell-fu-mode)
    ("r" spell-fu-reset)
    ("q" nil)))

(when (wsl-p)
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic
            search-web-default-browser 'browse-url-generic)))

  (defun stormacs-wsl-kill-ring-to-clipboard ()
    (interactive)
    (let ((text (substring-no-properties (car kill-ring))))
      (shell-command (concat "echo '" text "' | clip.exe")))))

(provide 'init-common)
