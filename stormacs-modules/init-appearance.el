;; init-appearance.el --- Appearance settings -*- lexical-binding: t; -*-

(setq stormacs-font-lodpi (sys-diversification
                           "Iosevka Nerd Font-10" ;
                           "Iosevka Nerd Font-10"
                           "Courier New-10"))

(setq stormacs-font-hidpi (sys-diversification
                           "Iosevka Nerd Font-14" ;
                           "Iosevka Nerd Font-14"
                           "Courier New-14"))

(defun stormacs-wsl-hidpi ()
  ;; Adjust font to fit hidpi
  (interactive)
  (set-face-attribute 'default nil :font stormacs-font-hidpi))

(defun stormacs-wsl-lodpi ()
  ;; Adjust font to fit lodpi
  (interactive)
  (set-face-attribute 'default nil :font stormacs-font-lodpi))

(with-eval-after-load 'stormacs-gui
  (if (wsl-p)
      (stormacs-wsl-hidpi)
    (stormacs-wsl-lodpi)))

;; Remove title bar when using pgtk, but not when WSL is set
(unless (wsl-p)
  (when (boundp 'pgtk-initialized)
    (setq default-frame-alist '((undecorated . t)))))

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(use-package ef-themes
  :elpaca (ef-themes :host sourcehut :repo "protesilaos/ef-themes")
  :demand t
  :custom
  (ef-themes-to-toggle '(ef-elea-dark ef-light))
  :config
  ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes)

  ;; Load the theme of your choice.
  ;; (load-theme 'ef-elea-dark :no-confirm)
  (ef-themes-select 'ef-elea-dark))

;; (use-package modus-themes
;;   :elpaca (modus-themes :host sourcehut :repo "protesilaos/modus-themes")
;;   :demand t
;;   :config
;;   ;; Add all your customizations prior to loading the themes
;;   ;; (setq modus-themes-italic-constructs t
;;   ;;       modus-themes-bold-constructs nil)

;;   ;; Maybe define some palette overrides, such as by using our presets
;;   (setq modus-themes-common-palette-overrides
;;         modus-themes-preset-overrides-faint)

;;   ;; Load the theme of your choice.
;;   (load-theme 'modus-vivendi-tinted :no-confirm)
;;   :bind ("<f5>" . modus-themes-toggle))

(use-package nerd-icons
  :elpaca (nerd-icons :host github :repo "rainstormstudio/nerd-icons.el")
  :custom
  ;; Had to set custom font as Symbols Nerd Font Mono made Emacs crash
  (nerd-icons-font-family "Iosevka Nerd Font"))

(use-package doom-modeline
  :elpaca (doom-modeline :host github :repo "seagle0128/doom-modeline")
  :hook (elpaca-after-init . doom-modeline-mode)
  :config
  (setq find-file-visit-truename t)
  (setq doom-modeline-icon t))

(use-package emojify
  :elpaca (emojify :host github :repo "iqbalansari/emacs-emojify")
  :when (wsl-p)
  :hook (elpaca-after-init . global-emojify-mode))

(use-package hl-todo
  :elpaca (hl-todo :host github :repo "tarsius/hl-todo")
  :hook (elpaca-after-init . global-hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"))

(use-package rainbow-delimiters
  :elpaca (rainbow-delimiters :host github :repo "Fanael/rainbow-delimiters")
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(show-paren-mode 1)
(electric-pair-mode 1)

(use-package svg-tag-mode
  :hook ((prog-mode . stormacs-svg-tag-mode)
         (org-mode . stormacs-org-svg-tag-mode))
  :config
  (setq stormacs-svg-tag-tags
        '(("[/|#|;]* FIXME:" . ((lambda (tag) (svg-lib-tag "FIXME" nil
                                                           :background "#d08770"
                                                           :foreground "white"))))
          ("[/|#|;]* TODO:" . ((lambda (tag) (svg-lib-tag "TODO" nil
                                                          :background "#d08770"
                                                          :foreground "white"))))))
  (setq stormacs-org-svg-tag-tags
        '(("TODO" . ((lambda (tag) (svg-lib-tag tag nil
                                                :background "orange red"
                                                :foreground "white"))))
          ("NEXT" . ((lambda (tag) (svg-lib-tag tag nil
                                                :background "dark orange"
                                                :foreground "white"))))
          ("DEPENDSON" . ((lambda (tag) (svg-lib-tag tag nil
                                                     :background "orange"
                                                     :foreground "white"))))
          ("DELEGATED" . ((lambda (tag) (svg-lib-tag tag nil
                                                     :background "light green"
                                                     :foreground "white"))))
          ("\\(WIPS\\)\\|\\(FOLLOWUPS\\)" . ((lambda (tag) (svg-lib-tag tag nil
                                                                        :background "deep sky blue"
                                                                        :foreground "white"))))
          ("\\(DONE\\)\\|\\(CANCELLED\\)" . ((lambda (tag) (svg-lib-tag tag nil
                                                                        :background "forest green"
                                                                        :foreground "white"))))))
  (defun stormacs-org-svg-tag-mode ()
    (interactive)
    (setq-local svg-tag-tags stormacs-org-svg-tag-tags)
    (svg-tag-mode))
  (defun stormacs-svg-tag-mode ()
    (interactive)
    (setq-local svg-tag-tags stormacs-svg-tag-tags)
    (svg-tag-mode))
  ;; Fix org agenda and dashboard
  ;; See: https://github.com/rougier/svg-tag-mode/issues/27#issuecomment-1246703561
  (defun stormacs-svg-tag-agenda-fix ()
    (interactive)
    (setq-local svg-tag-tags stormacs-org-svg-tag-tags)
    (let* ((case-fold-search nil)
           (keywords (mapcar #'svg-tag--build-keywords stormacs-org-svg-tag-tags))
           (keyword (car keywords)))
      (while keyword
        (save-excursion
          (while (re-search-forward (nth 0 keyword) nil t)
            (overlay-put (make-overlay
                          (match-beginning 0) (match-end 0))
                         'display  (nth 3 (eval (nth 2 keyword)))) ))
        (pop keywords)
        (setq keyword (car keywords)))))

  (add-hook 'dashboard-mode-hook #'stormacs-svg-tag-agenda-fix)
  (add-hook 'org-agenda-finalize-hook #'stormacs-svg-tag-agenda-fix)
  ;; Load style after gui init, needed when running in daemon
  (with-eval-after-load 'storvik/gui
    (setq svg-lib-style-default (svg-lib-style-compute-default))))

(use-package goggles
  :elpaca (goggles :host github :repo "minad/goggles")
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(use-package pulsar
  :elpaca (pulsar :host sourcehut :repo "protesilaos/pulsar")
  :config
  (setq pulsar-pulse-functions (append pulsar-pulse-functions
                                       '(ace-window
                                         avy-goto-char-timer)))
  (setq pulsar-delay 0.06)
  (pulsar-global-mode 1))

(use-package topsy
  :elpaca (topsy :host github :repo "alphapapa/topsy.el")
  :hook
  (prog-mode . topsy-mode)
  (magit-section-mode . topsy-mode))

(use-package ansi-color
  :elpaca nil
  :hook (compilation-filter . ansi-color-compilation-filter))

(elpaca
    nil
  (use-package emacs
    :elpaca nil
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

(use-package dashboard
  :elpaca (dashboard :host github :repo "emacs-dashboard/emacs-dashboard")
  :demand t
  :after svg-tag-mode
  :bind (:map stormacs-prefix-map
              ("d" . dashboard-open))
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-projects-backend 'project-el)
  (dashboard-center-content t)
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-agenda-tags-format 'ignore)
  (dashboard-agenda-prefix-format "%i %-26(stormacs-agenda-title) %-12s")
  (dashboard-item-names '(("Projects:" . " Projects:")
                          ("Agenda for today:" . " Agenda:")))
  (dashboard-week-agenda nil)
  (dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (dashboard-match-agenda-entry "-someday/!+TODO|+NEXT|+WIPS|+DEPENDSON|+DELEGATED|+FOLLOWUPS")
  (dashboard-agenda-release-buffers nil) ;; this will release org agenda files, dash refresh will be slower
  ;; Override init info, this is not used atm. Using advice in order to customize faces, see stormacs-dashboard-insert-init-info.
  (dashboard-init-info (lambda ()
                         (concat "inbox: " (number-to-string (length (org-map-entries t "inbox" 'agenda))) " • "
                                 "todo: " (number-to-string (length (org-map-entries t "-someday/+TODO" 'agenda))) " • "
                                 (propertize "terstt" 'face (car org-todo-keyword-faces))
                                 "next: " (number-to-string (length (org-map-entries t "-someday/+NEXT" 'agenda))) " • "
                                 "wips: " (number-to-string (length (org-map-entries t "-someday/+WIPS" 'agenda))) " • "
                                 "dependson: " (number-to-string (length (org-map-entries t "-someday/+DEPENDSON" 'agenda))) " • "
                                 "delegated: " (number-to-string (length (org-map-entries t "-someday/+DELEGATED" 'agenda))) " • "
                                 "followups: " (number-to-string (length (org-map-entries t "-someday/+FOLLOWUPS" 'agenda))))))
  (dashboard-items '((projects . 8)
                     (agenda . 36)))
  :config
  (defun stormacs-dashboard-insert-init-info-svg ()
    "Custom version of `dashboard-insert-init-info' which displays org agenda stats."
    (insert "\n")
    (let ((start (point)))
      (insert-image (svg-tag-make "storvikdev" :inverse t :crop-right t :margin 0))
      (insert-image (svg-tag-make (shell-command-to-string "echo -n $( mu find maildir:/storvikdev/Inbox flag:unread 2>/dev/null | wc -l )")
                                  :inverse nil :crop-left t :margin 1))
      (insert "   ")
      (insert-image (svg-tag-make "svartisenfestivalen" :inverse t :crop-right t :margin 0))
      (insert-image (svg-tag-make (shell-command-to-string "echo -n $( mu find maildir:/svartisenfestivalen/Inbox flag:unread 2>/dev/null | wc -l )")
                                  :inverse nil :crop-left t :margin 1))
      (dashboard-center-text start (point))
      (insert "\n\n\n"))
    (let ((start (point)))
      (insert-image (svg-tag-make "inbox" :inverse t :crop-right t :margin 0))
      (insert-image (svg-tag-make (number-to-string (length (org-map-entries t "inbox" 'agenda)))
                                  :inverse nil :crop-left t :margin 1))
      (insert "  ")
      (insert-image (svg-tag-make "todo" :inverse nil :crop-right t :margin 0 :face '(:background "orange red" :weight bold)))
      (insert-image (svg-tag-make (number-to-string (length (org-map-entries t "-someday/+TODO" 'agenda)))
                                  :inverse nil :crop-left t :margin 0))
      (insert "  ")
      (insert-image (svg-tag-make "next" :inverse nil :crop-right t :margin 0 :face '(:background "dark orange" :weight bold)))
      (insert-image (svg-tag-make (number-to-string (length (org-map-entries t "-someday/+NEXT" 'agenda)))
                                  :inverse nil :crop-left t :margin 1))
      (insert "  ")
      (insert-image (svg-tag-make "wips" :inverse nil :crop-right t :margin 0 :face '(:background "deep sky blue" :weight bold)))
      (insert-image (svg-tag-make (number-to-string (length (org-map-entries t "-someday/+WIPS" 'agenda)))
                                  :inverse nil :crop-left t :margin 1))
      (insert "  ")
      (insert-image (svg-tag-make "dependson" :inverse nil :crop-right t :margin 0 :face '(:background "orange" :weight bold)))
      (insert-image (svg-tag-make (number-to-string (length (org-map-entries t "-someday/+DEPENDSON" 'agenda)))
                                  :inverse nil :crop-left t :margin 1))
      (insert "  ")
      (insert-image (svg-tag-make "delegated" :inverse nil :crop-right t :margin 0 :face '(:background "sea green" :weight bold)))
      (insert-image (svg-tag-make (number-to-string (length (org-map-entries t "-someday/+DELEGATED" 'agenda)))
                                  :inverse nil :crop-left t :margin 1))
      (insert "  ")
      (insert-image (svg-tag-make "followups" :inverse nil :crop-right t :margin 0 :face '(:background "deep sky blue" :weight bold)))
      (insert-image (svg-tag-make (number-to-string (length (org-map-entries t "-someday/+FOLLOWUPS" 'agenda)))
                                  :inverse nil :crop-left t :margin 1))
      (dashboard-center-text start (point))))
  (defun stormacs-dashboard-insert-init-info-text ()
    "Custom version of `dashboard-insert-init-info' which displays org agenda stats."
    ;; For org-map-entries matching, see https://orgmode.org/manual/Matching-tags-and-properties.html
    (dashboard-insert-center (propertize (concat "inbox: " (number-to-string (length (org-map-entries t "inbox" 'agenda)))) 'face 'font-lock-comment-face)
                             (propertize " • " 'face 'font-lock-comment-face)
                             (propertize (concat "todo: " (number-to-string (length (org-map-entries t "-someday/+TODO" 'agenda)))) 'face (nth 0 org-todo-keyword-faces))
                             (propertize " • " 'face 'font-lock-comment-face)
                             (propertize (concat "next: " (number-to-string (length (org-map-entries t "-someday/+NEXT" 'agenda)))) 'face (nth 1 org-todo-keyword-faces))
                             (propertize " • " 'face 'font-lock-comment-face)
                             (propertize (concat "wips: " (number-to-string (length (org-map-entries t "-someday/+WIPS" 'agenda)))) 'face (nth 2 org-todo-keyword-faces))
                             (propertize " • " 'face 'font-lock-comment-face)
                             (propertize (concat "dependson: " (number-to-string (length (org-map-entries t "-someday/+DEPENDSON" 'agenda)))) 'face (nth 3 org-todo-keyword-faces))
                             (propertize " • " 'face 'font-lock-comment-face)
                             (propertize (concat "delegated: " (number-to-string (length (org-map-entries t "-someday/+DELEGATED" 'agenda)))) 'face (nth 4 org-todo-keyword-faces))
                             (propertize " • " 'face 'font-lock-comment-face)
                             (propertize (concat "followups: " (number-to-string (length (org-map-entries t "-someday/+FOLLOWUPS" 'agenda)))) 'face (nth 5 org-todo-keyword-faces))))
  (advice-add #'dashboard-insert-init-info :override #'stormacs-dashboard-insert-init-info-svg)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))

(provide 'init-appearance)
