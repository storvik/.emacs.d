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
(setq use-dialog-box nil)                               ;; Do not use popup boxes

(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "emacs.saves/"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "emacs.saves/") t)))

(add-hook 'elpaca-after-init-hook (lambda () (recentf-mode 1)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq epg-pinentry-mode 'loopback)

(use-package exec-path-from-shell
  :ensure (exec-path-from-shell :host github :repo "purcell/exec-path-from-shell")
  :when sys-unix-p
  :init
  (exec-path-from-shell-initialize))

(use-package sudo-edit
  :ensure (sudo-edit :host github :repo "nflath/sudo-edit"))

(use-package transient
  :ensure (transient :host github :repo "magit/transient"))

(use-package repeat
  :ensure nil
  :custom
  (setq repeat-on-final-keystroke t)
  (setq set-mark-command-repeat-pop t)
  :config
  ;; https://www.reddit.com/r/emacs/comments/1adwnse/repeatmode_is_awesome_share_you_useful_configs/
  (defun repeatify (repeat-map)
    "Set the `repeat-map' property on all commands bound in REPEAT-MAP."
    (named-let process ((keymap (symbol-value repeat-map)))
      (map-keymap
       (lambda (_key cmd)
         (cond
          ((symbolp cmd) (put cmd 'repeat-map repeat-map))
          ((keymapp cmd) (process cmd))))
       keymap)))

  (repeat-mode 1)

  ;; navigation repeat map
  ;; (defvar buffer-navigation-map
  ;;   (let ((map (make-sparse-keymap)))
  ;;     (define-key map (kbd "n") #'next-line)
  ;;     (define-key map (kbd "p") #'previous-line)
  ;;     (define-key map (kbd "f") #'forward-word)
  ;;     (define-key map (kbd "b") #'backward-word)
  ;;     (define-key map (kbd "u") #'scroll-up-command)
  ;;     (define-key map (kbd "d") #'scroll-down-command)
  ;;     (define-key map (kbd "e") #'move-end-of-line)
  ;;     (define-key map (kbd "a") #'move-beginning-of-line)
  ;;     (define-key map (kbd "SPC") #'set-mark-command)
  ;;     map))
  ;; (repeatify 'buffer-navigation-map)

  ;; change cursor when repeat mode is active
  ;; https://gist.github.com/jdtsmith/a169362879388bc1bdf2bbb977782d4f
  (let ((orig (default-value 'repeat-echo-function))
	    rcol ccol in-repeat)
    (setq
     repeat-echo-function
     (lambda (map)
       (if orig (funcall orig map))
       (unless rcol (setq rcol (face-foreground 'error)))
       (if map
	       (unless in-repeat		; new repeat sequence
	         (setq in-repeat t
		           ccol (face-background 'cursor))
	         (set-frame-parameter nil 'my/repeat-cursor ccol))
	     (setq in-repeat nil)
	     (set-frame-parameter nil 'my/repeat-cursor nil))
       (set-cursor-color (if map rcol ccol))))
    (add-function
     :after after-focus-change-function
     (let ((sym 'my/remove-repeat-cursor-color-on-focus-change))
       (defalias sym
	     (lambda ()
	       (when in-repeat
	         (dolist (frame (frame-list))
	           (when-let ((col (frame-parameter frame 'my/repeat-cursor)))
		         (with-selected-frame frame
		           (set-cursor-color col)))))))
       sym))))

(use-package jinx
  :ensure (jinx :host github :repo "minad/jinx")
  :when sys-unix-p
  :hook (elpaca-after-init . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package gptel
  :ensure (gptel :host github :repo "karthink/gptel")
  :config
  ;; Set default model
  (setq gptel-model "llama3:latest"
        gptel-backend (gptel-make-ollama "Ollama:llama3"
                        :host "localhost:11434"
                        :stream t
                        :models '("llama3:latest")))
  ;; Register mistral model, can be selected
  (gptel-make-ollama "Ollama:mistral"
    :host "localhost:11434"
    :stream t
    :models '("mistral:latest")))

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
