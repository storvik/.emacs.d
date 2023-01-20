;; init-org.el --- org mode  settings -*- lexical-binding: t; -*-
;; TODO: Should make some of these options configurable
(elpaca
    org
  (setq org-archive-location "archive/%s_archive::")

  (setq org-catch-invisible-edit 'nil)

  ;; File locations
  (setq org-directory "~/developer/org/org")
  (setq org-default-notes-file "~/developer/org/org/inbox-computer.org")
  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))

  ;; Refile
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes (quote confirm))
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))

  ;; Clock
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-clock-into-drawer 1)
  (setq org-log-done 'time)

  ;; Tags / context
  (setq org-tag-persistent-alist
        '(("@work" . ?w)
          ("@mo" . ?m)
          ("@nesna" .?n)
          ("@home" . ?h)
          ("@computer" . ?c)
          ("@phone" . ?p)))

  ;; Misc
  (setq org-export-allow-bind-keywords t)

  (setq org-capture-templates
        (quote (("t" "Todo" entry (file "~/developer/org/org/inbox-computer.org")
                 "* TODO %?\n")
                ("c" "Todo code" entry (file "~/developer/org/org/inbox-computer.org")
                 "* TODO %?\n%l\n")
                ("n" "Note" entry (file "~/developer/org/org/inbox-computer.org")
                 "* %? :NOTE:\n")
                ("m" "Meeting" entry (file "~/developer/org/org/inbox-computer.org")
                 "* MEETING %t %? :meeting:\n" :clock-in t :clock-resume t)
                ("p" "Phone Call" entry (file "~/developer/org/org/inbox-computer.org")
                 "* PHONE %T %? :phone:\n" :clock-in t :clock-resume t)
                ("r" "Respond to email, must be run from mu4e" entry (file "~/developer/org/org/inbox-computer.org")
                 "* TODO Respond to %:from on %:subject\n\t%a\n" :immediate-finish t)
                ("e" "E-mail todo, must be run from mu4e" entry (file "~/developer/org/org/inbox-computer.org")
                 "* TODO %?\n%a\n")
                ("k" "Cliplink bookmark" entry (file "~/developer/org/org/bookmarks.org")
                 "* %(org-cliplink-capture)%?\n" :empty-lines-before 0))))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n@/!)" "WIP(s@/!)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "DELEGATED(g@/!)" "FOLLOWUP(f@/!)" "|" "CANCELLED(c@/!)")
          (sequence "PHONE" "MEETING")))

  (setq org-todo-keyword-faces
        '(("TODO" :foreground "orange red" :weight bold)
          ("NEXT" :foreground "dark orange" :wight bold)
          ("WIP" :foreground "deep sky blue" :weight bold)
          ("DONE" :foreground "forest green" :weight bold)
          ("WAITING" :foreground "orange" :weight bold)
          ("DELEGATED" :foreground "light green" :weight bold)
          ("FOLLOWUP" :foreground "deep sky blue" :weight bold)
          ("CANCELLED" :foreground "forest green" :weight bold)
          ("MEETING" :foreground "hot pink" :weight bold)
          ("PHONE" :foreground "violet red" :weight bold))))

(elpaca-use-package
    (org-super-agenda :host github :repo "alphapapa/org-super-agenda")
  :after org
  :config
  (defun stormacs-agenda-context-emoji ()
    "Should insert emoji for given context, but alignment never worked."
    (let ((tags (concat (org-entry-get (point) "TAGS"))))
      (concat (when (string-match-p "@computer" tags)
                "@computer")
              (when (string-match-p "@phone" tags)
                "@phone")
              (when (string-match-p "@work" tags)
                "@work")
              (when (string-match-p "@home" tags)
                "@home"))))
  (setq org-agenda-custom-commands
        '(("w" " Work"
           ((agenda ""
                    ((org-agenda-prefix-format " %i %-22:c%?-12t% s")
                     (org-agenda-overriding-header "")
                     (org-agenda-remove-tags t) ;; remove tags from agenda view
                     (org-super-agenda-groups
                      '((:discard (:not (:tag ("goodtech"))))
                        (:name "This week")))))
            (alltodo ""
                     ((org-agenda-prefix-format "  %i %-16:c %-10(stormacs-agenda-context-emoji) %-6e ")
                      (org-agenda-hide-tags-regexp "@") ;; remove context tags from tag list
                      (org-agenda-remove-tags t)
                      (org-agenda-overriding-header "")
                      (org-super-agenda-groups
                       '((:discard (:not (:tag ("goodtech"))))
                         (:name "🛠️ Work in progress" :todo "WIP")
                         (:name "⏳ Next" :todo "NEXT")
                         (:name "🗒️ Todo" :todo "TODO")
                         (:name "🕙 Waiting" :todo "WAITING")
                         (:discard (:todo ("PHONE" "MEETING")))))))))
          ("p" " Private"
           ((agenda ""
                    ((org-agenda-prefix-format " %i %-22:c%?-12t% s")
                     (org-agenda-overriding-header "")
                     (org-super-agenda-groups
                      '((:discard (:tag ("goodtech")))
                        (:name "This week")))))
            (alltodo ""
                     ((org-agenda-prefix-format " %i %-22:c")
                      (org-agenda-remove-tags t)
                      (org-agenda-overriding-header "")
                      (org-super-agenda-groups
                       '((:discard (:tag ("goodtech")))
                         (:name "🛠️ Work in progress" :and (:todo "WIP" :not (:scheduled future)))
                         (:name "⏳ Next" :and (:todo "NEXT" :not (:scheduled future)))
                         (:name "🗒️ Todo" :and (:todo "TODO" :not (:scheduled future)))
                         (:name "🕙 Waiting" :and (:todo "WAITING" :not (:scheduled future)))
                         (:name "📌 Someday" :and (:tag "someday"  :not (:scheduled future)))
                         (:name "⚠️ Scheduled for later" :scheduled future)
                         (:discard (:todo ("PHONE" "MEETING")))))))))
          ("r" " Weekly review"
           ((alltodo ""
                     ((org-agenda-prefix-format " %i %-22:c")
                      (org-agenda-remove-tags t)
                      (org-agenda-overriding-header "")
                      (org-super-agenda-groups
                       '((:discard (:tag ("goodtech")))
                         (:name "🛠️ Work in progress" :and (:todo "WIP" :not (:scheduled future)))
                         (:name "⏳ Next" :and (:todo "NEXT" :not (:scheduled future)))
                         (:name "🗒️ Todo" :and (:todo "TODO" :not (:scheduled future)))
                         (:name "🕙 Waiting" :and (:todo "WAITING" :not (:scheduled future)))
                         (:name "📌 Someday" :and (:tag "someday"  :not (:scheduled future)))
                         (:name "⚠️ Scheduled for later" :scheduled future)
                         (:discard (:todo ("PHONE" "MEETING")))))))))))
  (org-super-agenda-mode))

;; Koma letter export
(eval-after-load 'ox '(require 'ox-koma-letter))
(eval-after-load 'ox-latex
  '(add-to-list 'org-latex-packages-alist '("AUTO" "babel" t) t))


(elpaca-use-package
    (org-cliplink :host github :repo "rexim/org-cliplink"))

(setq org-plantuml-exec-mode 'plantuml)

(elpaca-use-package
    (org-modern :host github :repo "minad/org-modern")
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

(elpaca-use-package
    (org-download :host github :repo "abo-abo/org-download")
  :config
  (when (getenv "WSL")
    (setq org-download-screenshot-method "powershell.exe -Command \"(Get-Clipboard -Format image).Save('$(wslpath -w %s)')\"")))

;; TODO: denote dir must be configurable
(elpaca-use-package
    (denote :host sourcehut :repo "protesilaos/denote")
  :demand t
  :custom
  (denote-directory (expand-file-name "~/developer/org/denote"))
  (denote-dired-directories (expand-file-name "~/developer/org/denote"))
  (denote-date-prompt-use-org-read-date t)
  (denote-known-keywords '("draft" "linux" "project" "powershell"))
  (denote-templates `((note . "- tags ::")
                      (people . ,(concat "- phone ::\n"
                                         "- email ::\n"
                                         "- company ::\n"
                                         "- role ::\n"
                                         "- location ::\n"
                                         "- how we met ::\n"
                                         "- birthday ::\n"
                                         "- interests ::\n"
                                         "- tags ::"))))
  :hook
  (dired-mode-hook . #'denote-dired-mode-in-directories)
  (find-file-hook . #'denote-link-buttonize-buffer)
  :bind ;; TODO: Shold be added to key overrides
  ("C-c n n" . #'denote)
  ("C-c n N" . #'denote-type)
  ("C-c n d" . #'denote-date)
  ("C-c n s" . #'denote-subdirectory)
  ("C-c n t" . #'denote-template)
  ("C-c n i" . #'denote-link)
  ("C-c n I" . #'denote-link-add-links)
  ("C-c n b" . #'denote-link-backlinks)
  ;; ("C-c n f f" . #'denote-link-find-file)
  ;; ("C-c n f b" . #'denote-link-find-backlink)
  ("C-c n r" . #'denote-rename-file)
  ("C-c n R" . #'denote-rename-file-using-front-matter)
  (:map dired-mode-map
        ("C-c C-d C-i" . #'denote-link-dired-marked-notes)
        ("C-c C-d C-r" . #'denote-dired-rename-marked-files)
        ("C-c C-d C-R" . #'denote-dired-rename-marked-files-using-front-matter))
  ;; (:map org-mode-map
  ;;       ("C-c n x" . my/denote-split-org-subtree))
  )

(defun denote-subdirectory-with-template ()
  "Create note while also prompting for a template and subdirectory.

  This is equivalent to calling `denote' when `denote-prompts' is
  set to '(template subdirectory title keywords)."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(template subdirectory title)))
    (call-interactively #'denote)))

;; (defun my/denote-split-org-subtree ()
;;   "Create new Denote note as an Org file using current Org subtree."
;;   (interactive)
;;   (let ((text (org-get-entry))
;;         (heading (org-get-heading :no-tags :no-todo :no-priority :no-comment))
;;         (tags (org-get-tags)))
;;     (delete-region (org-entry-beginning-position) (org-entry-end-position))
;;     (denote heading tags 'org)
;;     (insert text)))

;; TODO: Should be able to search my org notes too, not denote only
(elpaca-use-package
    (consult-notes :host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :custom
  (consult-notes-use-rg t)
  (consult-notes-ripgrep-args "rg --multiline --null --line-buffered --color=never --max-columns=1000 --path-separator /   --ignore-case --no-heading --line-number --hidden --glob=!.git/ -L --sortr=accessed --type org")
  (consult-notes-file-dir-sources
   '(("Org" ?o "~/developer/org/org")))
  :config
  (when (locate-library "denote")
    (consult-notes-denote-mode)))

(elpaca nil
  (use-package emacs
    :bind (:map stormacs-prefix-map ("o" . stormacs-org-hydra/body))
    :config
    (defhydra stormacs-org-hydra (:color pink :exit t :hint nil)
      "
  ^^^^^^                                                                                              ╭──────────┐
  org^^                      roam^^^^                                                                  │ org mode │
 ╭^^^^^^──────────────────────────────────────────────────────────────────────────────────────────────┴──────────╯
  [_a_] agenda               [_d_] denote                [_n_] consult notes
  [_c_] capture              [_t_] denote template       [_r_] consult notes ripgrep
  ^ ^                        [_s_] denote subdirectory
  [_q_] cancel
"
      ("a" org-agenda)
      ("c" org-capture)
      ("d" denote)
      ("t" denote-template)
      ("s" denote-subdirectory)
      ("n" consult-notes)
      ("r" consult-notes-search-in-all-notes)
      ("q" nil))))

(provide 'init-org)
