;; init-git.el --- Git and verion control -*- lexical-binding: t; -*-

(use-package magit
  :ensure (magit :host github :repo "magit/magit")
  :bind (:map stormacs-prefix-map
              ("g" . magit-status)
              :map magit-status-mode-map
              ("TAB" . magit-section-toggle)
              ("<C-tab>" . magit-section-cycle))
  :custom
  (auto-revert-check-vc-info t)
  (magit-git-executable "git"))

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
  :commands (git-timemachine)
  :hook (git-timemachine-mode . (lambda ()
                                  (when git-timemachine-mode
                                    (stormacs-tsc-timemachine))))
  :config
  (transient-define-prefix stormacs-tsc-timemachine ()
    "Git timemachine transient ."
    [[:description
      (lambda ()
        ;; silence echo area and log
        (let ((inhibit-message t)
              (message-log-max nil))
          (concat "Git timemachine: "
                  (when git-timemachine-revision
                    (git-timemachine--show-minibuffer-details git-timemachine-revision))
                  "\n")))
      :pad-keys t
      ("q" "quit" git-timemachine-quit)]]
    [[""
      ("n" "Show next revision" (lambda () (interactive) (with-silent-execution (git-timemachine-show-next-revision))) :transient t)
      ("p" "Show prev revision" (lambda () (interactive) (with-silent-execution (git-timemachine-show-previous-revision))) :transient t)
      ("g" "Show nth revision" (lambda () (interactive) (with-silent-execution (git-timemachine-show-nth-revision))) :transient t)
      ("h" "Show nearest revision" (lambda () (interactive) (with-silent-execution (git-timemachine-show-nearest-revision))) :transient t)
      ("t" "Show fuzzy revision" (lambda () (interactive) (with-silent-execution (git-timemachine-show-revision-fuzzy))) :transient t)
      ("i" "Show revision introducing" (lambda () (interactive) (with-silent-execution (git-timemachine-show-revision-introducing))) :transient t)]
     [""
      ("w" "kill abbreviated revision" git-timemachine-kill-abbreviated-revision :transient t)
      ("W" "kill revision" git-timemachine-kill-revision :transient t)]
     [""
      ("b" "blame current revision" git-timemachine-blame :transient t)
      ("c" "show commit" git-timemachine-show-commit :transient t)]]))

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
