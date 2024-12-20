;; init-keybindings.el --- Keybindings -*- lexical-binding: t; -*-

;; All overrides (key bindings that overrides existing ones) are placed in a minor mode,
;; stormacs-overrides-minor-mode. This way it's easy to completely disable all overrides if default
;; behaviour is needed.
;;
;; Other custom keybindings are placed behind a prefix map, =stormacs-prefix-map, making it easy to
;; maintain and discover using whick-key.
;;
;; Adding keybindings to map can be done the following ways:*
;;
;; : (bind-key "m w" #'function-name map-name)
;;
;; or
;;
;; : (bind-keys
;; :  :map map-name
;; :  ("f" . function-name))
;;
;; or
;;
;; : (use-package example-package
;; :   :bind (:map map-name
;; :         ("f" . function-name)))

(defvar stormacs-overrides-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'newline-and-indent)
    (define-key map (kbd "C-j") 'newline-and-indent)
    map)
  "stormacs-overrides-minor-mode keymap.")

(define-minor-mode stormacs-overrides-minor-mode
  "A minor mode so that my key settings override major modes."
  :init-value t
  :lighter " stormacs-key-overrides")

;; TODO: Should this be in a hook?
(stormacs-overrides-minor-mode 1)

;; Disable overrides minor mode in minibuffer
(defun stormacs-overrides-minor-mode-disable-hook ()
  (stormacs-overrides-minor-mode 0))

;; Disable overrides in some modes
(mapc
 (lambda (hook)
   (add-hook hook 'stormacs-overrides-minor-mode-disable-hook))
 '(minibuffer-setup-hook
   eshell-mode-hook
   mu4e-headers-mode-hook
   magit-post-display-buffer-hook
   python-shell-first-prompt-hook
   org-agenda-mode-hook
   mu4e-main-mode-hook
   sly-db-hook
   sly-mrepl-mode-hook
   dired-mode-hook
   deadgrep-mode-hook
   help-mode-hook
   cider-repl-mode-hook
   calendar-mode-hook
   dashboard-mode-hook
   grep-mode-hook
   comint-mode-hook
   elpaca-log-mode-hook))

;; Prefix map
;; TODO: Maybe this should be configurable?
(global-unset-key (kbd "C-M-s"))
(define-prefix-command 'stormacs-prefix-map)

(bind-keys :prefix-map stormacs-prefix-map
           :prefix-docstring "Stormacs keyboard map"
           :prefix "C-M-s")

(when (darwin-p)
  ;; This must be set when using emacs on a Mac
  (setq mac-command-modifier 'control)

  ;; This must only be set when using macport emacs
  ;; See: https://github.com/railwaycat/homebrew-emacsmacport/issues/191
  ;; (setq mac-option-modifier 'meta)

  ;; This setup uses the left modifier as meta and disables the right modifier.
  ;; Must be done when using emacs with emacs-plus patches and be able to use
  ;; right alt (alt gr) to input special characters.
  ;; https://stackoverflow.com/questions/45697790/how-to-enter-special-symbols-with-alt-in-emacs-under-mac-os-x
  ;; (setq ns-alternate-modifier 'meta)  ;; Disabled this, not needed it seems
  (setq ns-right-alternate-modifier 'none))

(provide 'init-keybindings)
