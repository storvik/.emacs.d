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

(use-package forge
  :ensure (forge :host github :repo "magit/forge")
  :after magit
  :commands (forge-pull))

(use-package emacs
  :ensure nil
  :after (transient)
  :bind (:map stormacs-prefix-map
              ("v" . stormacs-tsc-git))
  :config
  (transient-define-prefix stormacs-tsc-git ()
    "Prefix with descriptions specified with slots."
    ["Stormacs git transient\n"
     [("g" "magit" magit)
      ("b" "blame" magit-blame)]

     [("n" "next hunk" diff-hunk-next :transient t)
      ("p" "prev hunk" diff-hunk-prev :transient t)]

     [("t" "timemachine" git-timemachine)]]))

(use-package emacs
  :ensure nil
  :after (transient)
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (stormacs-tsc-smerge))))
  :config
  (transient-define-prefix stormacs-tsc-smerge ()
    "Prefix with descriptions specified with slots."
    ["Stormacs smerge transient\n"
     ["Move"
      ("n" "next" smerge-next :transient t)
      ("p" "prev" smerge-prev :transient t)]

     ["Keep"
      ("b" "base" smerge-keep-base :transient t)
      ("u" "upper" smerge-keep-upper :transient t)
      ("l" "lower" smerge-keep-lower :transient t)
      ("a" "all" smerge-keep-all :transient t)
      ("RET" "current" smerge-keep-current :transient t)]

     ["Diff"
      ("<" "upper/base" smerge-diff-base-upper :transient t)
      ("=" "upper/lower" smerge-diff-upper-lower :transient t)
      (">" "base/lower" smerge-diff-base-lower :transient t)
      ("R" "refine" smerge-refine :transient t)
      ("E" "ediff" smerge-ediff :transient t)]

     [("C" "combine" smerge-combine-with-next :transient t)
      ("r" "resolve" smerge-resolve :transient t)
      ("k" "kill current" smerge-kill-current :transient t)]]))

(provide 'init-git)
