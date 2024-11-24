;; init-appearance.el --- Appearance settings -*- lexical-binding: t; -*-

(setq stormacs-font-lodpi (sys-diversification
                           "Iosevka Nerd Font-10" ;
                           "Iosevka Nerd Font Mono-10"
                           "Courier New-10"))

(setq stormacs-font-hidpi (sys-diversification
                           "Iosevka Nerd Font-14" ;
                           "Iosevka Nerd Font Mono-14"
                           "Courier New-14"))

(defun stormacs-wsl-hidpi ()
  ;; Adjust font to fit hidpi
  (interactive)
  (set-face-attribute 'default nil :font stormacs-font-hidpi)
  (setq svg-lib-style-default (svg-lib-style-compute-default)))

(defun stormacs-wsl-lodpi ()
  ;; Adjust font to fit lodpi
  (interactive)
  (set-face-attribute 'default nil :font stormacs-font-lodpi)
  (setq svg-lib-style-default (svg-lib-style-compute-default)))

(with-eval-after-load 'stormacs-gui
  (stormacs-wsl-hidpi))

;; Remove title bar when using pgtk, but not when WSL is set
(unless (wsl-p)
  (when (boundp 'pgtk-initialized)
    (setq default-frame-alist '((undecorated . t)))))

;; Uses rounded courners without title bar on darwin
(when (darwin-p)
  (add-to-list 'default-frame-alist '(undecorated-round . t)))

(when (and (fboundp 'pixel-scroll-precision-mode)
           (darwin-p))
  (pixel-scroll-precision-mode 1))

(use-package ef-themes
  :ensure (ef-themes :host sourcehut :repo "protesilaos/ef-themes")
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
;;   :ensure (modus-themes :host sourcehut :repo "protesilaos/modus-themes")
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
  :ensure (nerd-icons :host github :repo "rainstormstudio/nerd-icons.el")
  :custom
  ;; Had to set custom font as Symbols Nerd Font Mono made Emacs crash
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package doom-modeline
  :ensure (doom-modeline :host github :repo "seagle0128/doom-modeline")
  :init
  (with-eval-after-load 'stormacs-gui
    (doom-modeline-mode))
  :config
  (setq find-file-visit-truename t)
  (setq doom-modeline-icon t))

(use-package emojify
  :ensure (emojify :host github :repo "iqbalansari/emacs-emojify")
  :when (wsl-p)
  :hook (elpaca-after-init . global-emojify-mode))

(use-package hl-todo
  :ensure (hl-todo :host github :repo "tarsius/hl-todo")
  :hook (elpaca-after-init . global-hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"))

(use-package rainbow-delimiters
  :ensure (rainbow-delimiters :host github :repo "Fanael/rainbow-delimiters")
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(show-paren-mode 1)
(electric-pair-mode 1)

(use-package svg-tag-mode
  :demand t
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
          ("WIPS" . ((lambda (tag) (svg-lib-tag tag nil
                                                :background "deep sky blue"
                                                :foreground "white"))))
          ("FOLLOWUPS" . ((lambda (tag) (svg-lib-tag tag nil
                                                     :background "deep sky blue"
                                                     :foreground "white"))))
          ("DONE" . ((lambda (tag) (svg-lib-tag tag nil
                                                :background "forest green"
                                                :foreground "white"))))
          ("CANCELLED" . ((lambda (tag) (svg-lib-tag tag nil
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
  (with-eval-after-load 'stormacs-gui
    (setq svg-lib-style-default (svg-lib-style-compute-default))))

(use-package goggles
  :ensure (goggles :host github :repo "minad/goggles")
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(use-package pulsar
  :ensure (pulsar :host sourcehut :repo "protesilaos/pulsar")
  :config
  (setq pulsar-pulse-functions (append pulsar-pulse-functions
                                       '(ace-window
                                         avy-goto-char-timer)))
  (setq pulsar-delay 0.06)
  (pulsar-global-mode 1))

(use-package topsy
  :ensure (topsy :host github :repo "alphapapa/topsy.el")
  :hook
  (prog-mode . topsy-mode)
  (magit-section-mode . topsy-mode))

(use-package colorful-mode
  :ensure (colorful-mode :host github :repo "DevelopmentCool2449/colorful-mode")
  :hook (prog-mode text-mode))

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package emacs
  :ensure nil
  :after (transient)
  :bind ("C-M-o" . stormacs-tsc-window)
  :config
  (transient-define-prefix stormacs-tsc-window ()
    "Prefix with descriptions specified with slots."
    ["Stormacs window\n" ; yes the newline works
     ["Move"
      ("m" "left" windmove-left :transient t)
      ("n" "down" windmove-down :transient t)
      ("e" "up" windmove-up :transient t)
      ("i" "right" windmove-right :transient t)]

     ["Size"
      ("M" "left" shrink-window-horizontally :transient t)
      ("N" "down" shrink-window :transient t)
      ("E" "up" enlarge-window :transient t)
      ("I" "right" enlarge-window-horizontally :transient t)]

     ["Layout"
      ("v" "vertical" split-window-vertically :transient t)
      ("h" "horizontal" split-window-horizontally :transient t)
      ("b" "balance" balance-windows :transient t)
      ("z" "zoom" delete-other-windows :transient t)
      ("d" "delete" delete-window :transient t)]

     [("a" "ace windows" ace-window :transient t)
      ("o" "cycle windows" other-window :transient t)]]))

(use-package dashboard
  :ensure (dashboard :host github :repo "emacs-dashboard/emacs-dashboard")
  :demand t
  :after (svg-tag-mode)
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
  (dashboard-match-agenda-entry "-nodash-someday/!+TODO|+NEXT|+WIPS|+DEPENDSON|+DELEGATED|+FOLLOWUPS")
  (dashboard-match-agenda-entry "-nodash-someday/!+NEXT|+WIPS|+DEPENDSON|+DELEGATED|+FOLLOWUPS")
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
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (with-eval-after-load 'stormacs-gui
    (dashboard-setup-startup-hook)))

(provide 'init-appearance)
