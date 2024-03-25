;; init-git.el --- Git and verion control -*- lexical-binding: t; -*-

(use-package magit
  :ensure (magit :host github :repo "magit/magit")
  :bind (:map stormacs-prefix-map
              ("g" . magit-status)
              :map magit-status-mode-map
              ("TAB" . magit-section-toggle)
              ("<C-tab>" . magit-section-cycle))
  :config
  (setq magit-git-executable "git"))

(use-package diff-hl
  :ensure (diff-hl :host github :repo "dgutov/diff-hl")
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-side 'right)
  :init
  (with-eval-after-load 'stormacs-gui
    (global-diff-hl-mode)))

(use-package magit-todos
  :ensure (magit-todos :host github
                       :repo "alphapapa/magit-todos"
                       ;; since hl-todo does not specify package version, skip check
                       :build (:not elpaca--check-version))
  :hook (elpaca-after-init . magit-todos-mode))

(use-package git-timemachine
  :ensure (git-timemachine :host codeberg :repo "pidu/git-timemachine")
  :commands (git-timemachine))

(elpaca nil
  (use-package emacs
    :ensure nil
    :bind (:map stormacs-prefix-map
                ("v" . stormacs-hydra-git/body))
    :config
    (defhydra stormacs-hydra-git (:color pink :exit t :hint nil)
      "
 ^^^^^^                                                                                           ╭────────┐
 ^^^^^^                                                                                           │  git   │
╭^^^^^^───────────────────────────────────────────────────────────────────────────────────────────┴────────╯
 [_g_] magit               [_n_] next hunk               [_t_] git timemachine
 [_b_] blame               [_p_] previous hunk           [_q_] cancel
"
      ("t" git-timemachine)
      ("g" magit)
      ("b" magit-blame)
      ("n" diff-hl-next-hunk)
      ("p" diff-hl-previous-hunk)
      ("q" nil))))

(elpaca nil
  (use-package emacs
    :ensure nil
    :hook (magit-diff-visit-file . (lambda ()
                                     (when smerge-mode
                                       (stormacs-smerge-hydra/body))))
    :config
    (defhydra stormacs-smerge-hydra
      (:color pink :hint nil :post (smerge-auto-leave))
      "
^^^^^^^^                                                                                            ╭────────┐
 move^^                keep^^                diff^^                other^^                          │ smerge │
╭^^^^^^^^───────────────────────────────────────────────────────────────────────────────────────────┴────────╯
 [_n_] next            [_b_] base            [_<_] upper/base      [_C_] combine
 [_p_] prev            [_u_] lpper           [_=_] upper/lowe      [_r_] resolve
  ^ ^                  [_l_] lower           [_>_] base/lower      [_k_] kill current
  ^ ^                  [_a_] all             [_R_] efine
  ^ ^                  _RET_ current         [_E_] diff
"
      ("n" smerge-next)
      ("p" smerge-prev)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("R" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("k" smerge-kill-current)
      ("ZZ" (lambda ()
              (interactive)
              (save-buffer)
              (bury-buffer))
       "Save and bury buffer" :color blue)
      ("q" nil "cancel" :color blue))))

(provide 'init-git)
