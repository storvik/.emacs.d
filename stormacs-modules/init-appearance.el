;; init-appearance.el --- Appearance settings -*- lexical-binding: t; -*-

(setq stormacs-font-lodpi (sys-diversification
                           "Iosevka Nerd Font-10" ;
                           "Iosevka Nerd Font-10"
                           "Courier New-10"))

(setq stormacs-font-hidpi (sys-diversification
                           "Iosevka Nerd Font-15" ;
                           "Iosevka Nerd Font-15"
                           "Courier New-15"))

(defun stormacs-wsl-hidpi ()
  ;; Adjust font to fit hidpi
  (interactive)
  (set-face-attribute 'default nil :font stormacs-font-hidpi))

(defun stormacs-wsl-lodpi ()
  ;; Adjust font to fit lodpi
  (interactive)
  (set-face-attribute 'default nil :font stormacs-font-lodpi))

;; TODO: Add a stormacs-wsl-dpi-dwim function
(with-eval-after-load 'stormacs-gui
  (set-face-attribute 'default nil :font stormacs-font-lodpi))

;; Remove title bar when using pgtk, but not when WSL is set
(unless (getenv "WSL")
  (when (boundp 'pgtk-initialized)
    (setq default-frame-alist '((undecorated . t)))))

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(elpaca-use-package
    (modus-themes :host sourcehut :repo "protesilaos/modus-themes")
  :demand t
  :config
  ;; Add all your customizations prior to loading the themes
  ;; (setq modus-themes-italic-constructs t
  ;;       modus-themes-bold-constructs nil)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-faint)

  ;; Load the theme of your choice.
  (load-theme 'modus-vivendi-tinted :no-confirm)
  :bind ("<f5>" . modus-themes-toggle))

(elpaca-use-package
    (all-the-icons :host github :repo "domtronn/all-the-icons.el"))

(elpaca-use-package
    (doom-modeline :host github :repo "seagle0128/doom-modeline")
  :hook (elpaca-after-init . doom-modeline-mode)
  :config
  (setq find-file-visit-truename t)
  (setq doom-modeline-icon t))

(elpaca-use-package
    (emojify :host github :repo "iqbalansari/emacs-emojify")
  :when (getenv "WSL")
  :hook (elpaca-after-init . global-emojify-mode))

(elpaca-use-package
    (hl-todo :github :repo "tarsius/hl-todo")
  :hook (elpaca-after-init . global-hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"))

(elpaca-use-package
    (rainbow-delimiters :host github :repo "Fanael/rainbow-delimiters")
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(show-paren-mode 1)
(electric-pair-mode 1)

(elpaca-use-package
    (goggles :host github :repo "minad/goggles")
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(elpaca-use-package
    (pulsar :host sourcehut :repo "protesilaos/pulsar")
  :config
  (setq pulsar-pulse-functions (append pulsar-pulse-functions
                                       '(ace-window)))
  (setq pulsar-delay 0.06)
  (pulsar-global-mode 1))

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(elpaca
    nil
  (use-package emacs
    :bind (:map stormacs-prefix-map
           ("w" . stormacs-hydra-window/body))
    :config
    (defhydra stormacs-hydra-window (:color blue :hint nil)
      "
                                                                                                  ╭─────────┐
    Move to              Size            Scroll                Split                        Do    │ Windows │
 ╭────────────────────────────────────────────────────────────────────────────────────────────────┴─────────╯
        ^_k_^                   ^_K_^                ^_p_^             ╭─┬─┐^ ^        ╭─┬─┐^ ^             ✗ [_d_] close window
       ^^↑^^                   ^^↑^^               ^^↑^^            │ │ │_v_ertical ├─┼─┤_b_alance       ⇋ [_w_] cycle window
   _h_ ←   → _l_         _H_ ←   → _L_           ^^ ^^             ╰─┴─╯^ ^        ╰─┴─╯^ ^
       ^^↓^^                   ^^↓^^               ^^↓^^            ╭───┐^ ^        ╭───┐^ ^
       ^_j_^                    ^_J_^                ^_n_^             ├───┤_s_tack    │   │_z_oom
        ^^ ^^                   ^^ ^^                ^^ ^^             ╰───╯^ ^        ╰───╯^ ^               [_q_] quit
    "
      ("h" windmove-left :color red)
      ("j" windmove-down :color red)
      ("k" windmove-up :color red)
      ("l" windmove-right :color red)
      ("H" shrink-window-horizontally :color red)
      ("J" shrink-window :color red)
      ("K" enlarge-window :color red)
      ("L" enlarge-window-horizontally :color red)
      ("s" split-window-vertically :color red)
      ("v" split-window-horizontally :color red)
      ("b" balance-windows)
      ("z" delete-other-windows)
      ("n" scroll-up :color red)
      ("p" scroll-down :color red)
      ("d" delete-window)
      ("w" other-window)
      ("q" nil))))

(provide 'init-appearance)