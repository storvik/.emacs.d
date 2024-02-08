;; init-navigation.el --- Navigation and editing -*- lexical-binding: t; -*-

(use-package ace-window
  :elpaca (ace-window :host github :repo "abo-abo/ace-window")
  :bind (:map stormacs-overrides-minor-mode-map
         ("M-o" . ace-window))
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package dogears
  :elpaca (dogears :host github :repo "alphapapa/dogears.el")
  :commands (dogears-list dogears-mode)
  :hook (elpaca-after-init . dogears-mode)
  :bind (:map stormacs-prefix-map
              ("z" . stormacs-hydra-dogears/body))
  :custom
  (dogears-idle-timer 2)
  (dogears-functions '(avy-goto-char-timer))
  :config
  (defhydra stormacs-hydra-dogears (:color pink :exit nil :hint nil)
    "
 ^^^^^^                                                                                           ╭───────────┐
 ^^^^^^                                                                                           │  dogears  │
╭^^^^^^───────────────────────────────────────────────────────────────────────────────────────────┴───────────╯
 [_p_] back                [_l_] list                    [_g_] go
 [_n_] forward             [_s_] sidebar                 [_q_] cancel
"
    ("p" dogears-back)
    ("n" dogears-forward)
    ("l" dogears-list)
    ("s" dogears-sidebar)
    ("g" dogears-go)
    ("q" nil)))

(use-package avy
  :elpaca (avy :host github :repo "abo-abo/avy")
  :bind (:map stormacs-overrides-minor-mode-map
         ("M-g g" . avy-goto-line)
         ("M-j" . avy-goto-char-timer))
  :config
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (defun avy-show-dispatch-help ()
    "Display action shortucts in echo area."
    (let ((len (length "avy-action-"))
          (itms (length avy-dispatch-alist))
          (msg ""))
      (dotimes (i itms)
        (let ((x (nth i avy-dispatch-alist)))
          (setf msg (concat msg
                            (when (and (eq (mod i 4) 0)
                                       (not (eq i 0)))
                              "\n")
                            (format "%s: %-30s"
                                    (propertize
                                     (char-to-string (car x))
                                     'face 'aw-key-face)
                                    (substring (symbol-name (cdr x)) len))))))
      (message msg)))
  ;; (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark
  ;;       (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char
  ;;       (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
  ;;       (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
  ;;       (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
  ;;       (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line
  ;;       (alist-get ?y avy-dispatch-alist) 'avy-action-yank
  ;;       (alist-get ?w avy-dispatch-alist) 'avy-action-copy
  ;;       (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
  ;;       (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)
  (setq avy-dispatch-alist '((46 . avy-action-embark)
                             (116 . avy-action-teleport)
                             (121 . avy-action-yank)
                             (107 . avy-action-kill-stay)
                             (32 . avy-action-mark-to-char)
                             (84 . avy-action-teleport-whole-line)
                             (89 . avy-action-yank-whole-line)
                             (75 . avy-action-kill-whole-line)
                             (122 . avy-action-zap-to-char)
                             (119 . avy-action-copy)
                             (105 . avy-action-ispell)
                             (120 . avy-action-kill-move)
                             (109 . avy-action-mark)
                             (87 . avy-action-copy-whole-line))))

(use-package deadgrep
  :elpaca (deadgrep :host github :repo "Wilfred/deadgrep")
  :bind (:map stormacs-overrides-minor-mode-map
         ("M-s s" . deadgrep)))

;; TODO: Do I need this?
;; (elpaca-use-package
;;  (fancy-dabbrev :host github :repo "jrosdahl/fancy-dabbrev")
;;  :commands (fancy-dabbrev-mode)
;;  :bind (:map stormacs-overrides-minor-mode-map
;;         ("C-<tab>" . fancy-dabbrev-expand))
;;  :custom
;;  (fancy-dabbrev-preview-delay 0.0)
;;  (fancy-dabbrev-preview-context 'before-non-word)
;;  (fancy-dabbrev-expansion-on-preview-only t)
;;  (fancy-dabbrev-indent-command 'indent-for-tab-command)
;;  :init
;;  (with-eval-after-load 'stormacs-gui
;;    (global-fancy-dabbrev-mode)))

(use-package move-text
  :elpaca (move-text :host github :repo "emacsfodder/move-text")
  :bind (:map stormacs-prefix-map
         ("m" . stormacs-move-text-hydra/body))
  :config
  (defhydra stoamacs-move-text-hydra (:hint nil)
    "Move line or region"
    ("p" move-text-up "Up")
    ("n" move-text-down "Down")))

(use-package puni
  :elpaca (puni :host github :repo "AmaiKinono/puni")
  :bind (:map puni-mode-map
         ("C-<right>" . puni-slurp-forward)
         ("C-<left>" . puni-slurp-backward)
         ("M-r" . puni-raise)
         ("M-?" . puni-convolute))
  :init
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode))

(use-package tempel
  :elpaca (tempel :host github :repo "minad/tempel")
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :hook ((prog-mode . tempel-setup-capf)
         (text-mode . tempel-setup-capf)))

(provide 'init-navigation)
