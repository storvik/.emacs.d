;; init-developer-config.el --- Developer config and doc file settings -*- lexical-binding: t; -*-

(use-package csv-mode)

(use-package dockerfile-mode
  :ensure (dockerfile-mode :host github :repo "spotify/dockerfile-mode"))

(use-package edit-indirect
  :ensure (edit-indirect :host github :repo "Fanael/edit-indirect"))

;; https://github.com/jrblevin/markdown-mode/issues/578
(setq native-comp-deferred-compilation-deny-list '("markdown-mode\\.el$"))
(setq native-comp-jit-compilation-deny-list '("markdown-mode\\.el$"))

(use-package markdown-mode
  :ensure (markdown-mode :host github :repo "jrblevin/markdown-mode")
  :after edit-indirect
  :commands (markdown-mode gfm-mode)
  :custom
  (markdown-command "multimarkdown")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package markdown-toc
  :ensure (markdown-toc :host github :repo "ardumont/markdown-toc"))

(use-package markdown-preview-mode
  :ensure (markdown-preview-mode :host github :repo "ancane/markdown-preview-mode"))

(use-package nginx-mode
  :ensure (nginx-mode :host github :repo "ajc/nginx-mode"))

(use-package plantuml-mode
  :ensure (plantuml-mode :host github :repo "skuro/plantuml-mode")
  :custom
  (plantuml-default-exec-mode 'executable)
  :config
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))

(use-package yaml-mode
  :ensure (yaml-mode :host github :repo "yoshiki/yaml-mode"))

(use-package kbd-mode
  :ensure (kbd-mode :host github :repo "kmonad/kbd-mode"))

(use-package yuck-mode
  :ensure (yuck-mode :host github :repo "mmcjimsey26/yuck-mode"))

(provide 'init-developer-config)
