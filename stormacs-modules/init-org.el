;; init-org.el --- org mode  settings -*- lexical-binding: t; -*-
;; TODO: Should make some of these options configurable
(use-package org
  :ensure t
  :init
  (setq org-archive-location "archive/%s_archive::")
  (setq org-directory "~/developer/org/org")
  (setq org-default-notes-file "~/developer/org/org/inbox--computer.org")
  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
  :config
  ;; Refile
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes (quote confirm))
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))

  ;; clock
  (setq org-clock-persist 'history)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-log-into-drawer t)
  (setq org-clock-into-drawer 1)
  (setq org-log-done 'time)

  ;; tags / context
  (setq org-tag-persistent-alist '((:startgroup . nil)
                                   ("@work" . ?w)
                                   ("@home" . ?h)
                                   (:endgroup . nil)
                                   (:startgroup . nil)
                                   ("@computer" . ?c)
                                   ("@phone" . ?p)
                                   (:endgroup . nil)
                                   (:startgroup . nil)
                                   ("billable" .?b)
                                   (:endgroup . nil)
                                   (:startgroup . nil)
                                   ("emacs" .?e)
                                   (:endgroup . nil)
                                   (:startgroup . nil)
                                   ("nodash" .?n)
                                   (:endgroup . nil)
                                   (:startgroup . nil)
                                   ("someday" .?s)
                                   (:endgroup . nil)))

  ;; TODO: Check if org-tag-faces can be used for something useful

  ;; Misc
  (setq org-export-allow-bind-keywords t)

  (setq org-agenda-window-setup 'only-window)
  (add-hook 'org-agenda-finalize-hook #'hl-line-mode)

  (setq org-capture-templates
        (quote (("t" "Todo" entry (file "~/developer/org/org/inbox--computer.org")
                 "* TODO %?\n")
                ("c" "Todo code" entry (file "~/developer/org/org/inbox--computer.org")
                 "* TODO %?\n%l\n")
                ("n" "Note" entry (file "~/developer/org/org/inbox--computer.org")
                 "* %? :NOTE:\n")
                ("m" "Meeting" entry (file "~/developer/org/org/inbox--computer.org")
                 "* MEETING %t %? :meeting:\n" :clock-in t :clock-resume t)
                ("p" "Phone Call" entry (file "~/developer/org/org/inbox--computer.org")
                 "* PHONE %T %? :phone:\n" :clock-in t :clock-resume t)
                ("r" "Respond to email, must be run from mu4e" entry (file "~/developer/org/org/inbox--computer.org")
                 "* TODO Respond to %:from on %:subject\n\t%a\n" :immediate-finish t)
                ("e" "E-mail todo, must be run from mu4e" entry (file "~/developer/org/org/inbox--computer.org")
                 "* TODO %?\n%a\n")
                ("k" "Cliplink bookmark" entry (file "~/developer/org/org/bookmarks.org")
                 "* %(org-cliplink-capture)%?\n" :empty-lines-before 0))))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n@/!)" "WIPS(s@/!)" "|" "DONE(d)")
          (sequence "DEPENDSON(w@/!)" "FOLLOWUPS(f@/!)" "|" "DELEGATED(g@/!)" "CANCELLED(c@/!)")
          (sequence "PHONE" "MEETING" "|")))

  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "orange red" :weight bold))
          ("NEXT" . (:foreground "dark orange" :weight bold))
          ("WIPS" . (:foreground "deep sky blue" :weight bold))
          ("DONE" . (:foreground "forest green" :weight bold))
          ("DEPENDSON" . (:foreground "orange" :weight bold))
          ("DELEGATED" . (:foreground "sea green" :weight bold))
          ("FOLLOWUPS" . (:foreground "deep sky blue" :weight bold))
          ("CANCELLED" . (:foreground "forest green" :weight bold))
          ("MEETING" . (:foreground "hot pink" :weight bold))
          ("PHONE" . (:foreground "violet red" :weight bold)))))

(use-package org-super-agenda
  :ensure (org-super-agenda :host github :repo "alphapapa/org-super-agenda")
  :after org
  :config
  (define-key org-super-agenda-header-map "e" nil)
  (defun stormacs-agenda-title ()
    "Pretty title in agenda, need filename similar to denote."
    (interactive)
    (let ((category (org-entry-get (point) "CATEGORY")))
      (if (string-match "\\([a-z]*\\)--\\([a-z-0-9]*\\)" category)
          (concat (match-string 1 category)
                  " 󰨕 "
                  (replace-regexp-in-string "-" " " (match-string 2 category)))
        category)))
  (defun stormacs-agenda-context-emoji ()
    "Should insert emoji for given context, but alignment never worked."
    (let ((tags (concat (org-entry-get (point) "TAGS"))))
      (concat (if (string-match-p "@phone" tags)
                  ""
                "  ")
              "  "
              (if (string-match-p "@computer" tags)
                  ""
                "  ")
              "  "
              (if (string-match-p "@work" tags)
                  ""
                "  ")
              "  "
              (when (string-match-p "@home" tags)
                ""))))
  (defun stormacs-agenda-deadline ()
    (let ((deadline (org-get-deadline-time (point))))
      (if deadline
          (format-time-string "%Y-%m-%d" deadline)
        "")))
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
                     ((org-agenda-prefix-format "  %i %-22(stormacs-agenda-title) %-12(stormacs-agenda-context-emoji)")
                      (org-agenda-hide-tags-regexp "@") ;; remove context tags from tag list
                      (org-agenda-remove-tags t)
                      (org-agenda-overriding-header "")
                      (org-super-agenda-groups
                       '((:discard (:not (:tag ("goodtech"))) :file-path "inbox")
                         (:name "🛠️ Work in progress\n" :todo "WIPS")
                         (:name "⏳ Next\n" :todo "NEXT")
                         (:name "🗒️ Todo\n" :todo "TODO")
                         (:name "🕙 Waiting\n" :and (:todo ("DEPENDSON" "FOLLOWUPS")))
                         (:discard (:todo ("PHONE" "MEETING")))))))))
          ("p" " Private"
           ((agenda ""
                    ((org-agenda-prefix-format " %i %-22:c%?-12t% s")
                     (org-agenda-overriding-header "")
                     (org-super-agenda-groups
                      '((:discard (:tag ("goodtech")))
                        (:name "This week")))))
            (alltodo ""
                     ((org-agenda-prefix-format " %i %-32:c")
                      (org-agenda-remove-tags nil)
                      (org-agenda-overriding-header "")
                      (org-super-agenda-groups
                       '((:discard (:tag ("goodtech") :file-path "inbox"))
                         (:name "🛠️ Work in progress" :and (:todo "WIPS" :not (:scheduled future)))
                         (:name "⏳ Next" :and (:todo "NEXT" :not (:scheduled future)))
                         (:name "🗒️ Todo" :and (:todo "TODO" :not (:scheduled future) :not (:tag "someday")))
                         (:name "🕙 Waiting" :and (:todo ("DEPENDSON" "FOLLOWUPS") :not (:scheduled future)))
                         (:name "📌 Someday" :and (:tag "someday" :not (:scheduled future)))
                         (:name "⚠️ Scheduled for later" :scheduled future)
                         (:discard (:todo ("PHONE" "MEETING")))))))))
          ("f" " RBUL"
           ((agenda ""
                    ((org-agenda-prefix-format " %i %-22:c%?-12t% s")
                     (org-agenda-overriding-header "")
                     (org-super-agenda-groups
                      '((:discard (:not (:tag ("rbul"))))
                        (:name "This week")))))
            (alltodo ""
                     ((org-agenda-prefix-format " %i %-14:(stormacs-agenda-deadline)")
                      (org-agenda-remove-tags t)
                      (org-agenda-overriding-header "")
                      (org-super-agenda-groups
                       '((:discard (:not (:tag ("rbul") :file-path "inbox")))
                         (:name "🛠️ Work in progress" :and (:todo "WIPS" :not (:scheduled future)))
                         (:name "⏳ Next" :and (:todo "NEXT" :not (:scheduled future)))
                         (:name "🗒️ Todo" :and (:todo "TODO" :not (:scheduled future) :not (:tag "someday")))
                         (:name "🕙 Waiting" :and (:todo ("DEPENDSON" "FOLLOWUPS") :not (:scheduled future)))
                         (:name "📌 Someday" :and (:tag "someday" :not (:scheduled future)))
                         (:name "⚠️ Scheduled for later" :scheduled future)
                         (:discard (:todo ("PHONE" "MEETING")))))))))
          ("r" " Weekly review"
           ((alltodo ""
                     ((org-agenda-prefix-format " %i %-28:(stormacs-agenda-title) %-14:(stormacs-agenda-deadline)")
                      (org-agenda-remove-tags nil)
                      (org-agenda-todo-ignore-with-date nil)
                      (org-agenda-overriding-header "")
                      (org-super-agenda-groups
                       '((:name "📬 Inbox" :and (:file-path "inbox"))
                         (:name "🛠️ Work in progress" :and (:todo "WIPS" :not (:scheduled future)))
                         (:name "⏳ Next" :and (:todo "NEXT" :not (:scheduled future)))
                         (:name "🗒️ Todo" :and (:todo "TODO" :not (:scheduled future) :not (:tag "someday")))
                         (:name "🕙 Waiting" :and (:todo ("DEPENDSON" "FOLLOWUPS") :not (:scheduled future)))
                         (:name "📌 Someday" :and (:tag "someday" :not (:scheduled future)))
                         (:name "⚠️ Scheduled for later" :scheduled future)
                         (:discard (:todo ("PHONE" "MEETING")))))))))))
  (org-super-agenda-mode))

;; Koma letter export
(eval-after-load 'ox '(require 'ox-koma-letter))
(eval-after-load 'ox-latex
  '(add-to-list 'org-latex-packages-alist '("AUTO" "babel" t) t))

(use-package org-cliplink
  :ensure (org-cliplink :host github :repo "rexim/org-cliplink"))

(setq org-plantuml-exec-mode 'plantuml)

(use-package org-modern
  :ensure (org-modern :host github :repo "minad/org-modern")
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-todo nil)
  :config
  (setq org-modern-todo-faces
        '(("TODO" :foreground "gray10" :background "orange red")
          ("NEXT" :foreground "gray10" :background "dark orange")
          ("WIPS" :foreground "gray10" :background "deep sky blue")
          ("DONE" :foreground "gray10" :background "forest green")
          ("DEPENDSON" :foreground "gray10" :background "orange")
          ("DELEGATED" :foreground "gray10" :background "forest green")
          ("FOLLOWUPS" :foreground "gray10" :background "deep sky blue")
          ("CANCELLED" :foreground "gray10" :background "forest green")
          ("MEETING" :foreground "gray10" :background "hot pink")
          ("PHONE" :foreground "gray10" :background "violet red"))))

(use-package org-modern-indent
  :ensure (org-modern-indent :host github :repo "jdtsmith/org-modern-indent")
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package org-download
  :ensure (org-download :host github :repo "abo-abo/org-download")
  :custom
  (org-download-image-dir "./graphics/")
  (org-download-heading-lvl nil)
  :config
  (when (wsl-p)
    (setq org-download-screenshot-method "powershell.exe -Command \"(Get-Clipboard -Format image).Save('$(wslpath -w %s)')\"")))

;; TODO: denote dir must be configurable
(use-package denote
  :ensure (denote :host sourcehut :repo "protesilaos/denote")
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


(defun stormacs-denote-people ()
  (interactive)
  (let ((title (denote-title-prompt))
        (subdirectory (expand-file-name "people" denote-directory)))
    (denote title nil nil subdirectory nil 'people)))

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
(use-package consult-notes
  :ensure (consult-notes :host github :repo "mclear-tools/consult-notes")
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

(use-package emacs
  :ensure nil
  :after (transient)
  :bind
  (:map stormacs-prefix-map ("o" . stormacs-tsc-org))
  :config
  (transient-define-prefix stormacs-tsc-org ()
    "Prefix with descriptions specified with slots."
    ["Stormacs org mode transient\n"
     ["Org"
      ("a" "agenda" org-agenda)
      ("c" "capture" org-capture)]

     ["Denote"
      ("d" "denote" denote)
      ("t" "denote template" denote-template)
      ("s" "denote subdir" denote-subdirectory)]

     [("n" "consult notes" consult-notes)
      ("r" "consult notes ripgrep" consult-notes-search-in-all-notes)
      ("b" "browse with dired" (lambda () (interactive) (dired org-directory)))]]))

(provide 'init-org)
