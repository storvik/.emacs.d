;; init-developer-config.el --- Developer config and doc file settings -*- lexical-binding: t; -*-

(use-package csv-mode)

(use-package dockerfile-mode
  :elpaca (dockerfile-mode :host github :repo "spotify/dockerfile-mode"))

(use-package edit-indirect
  :elpaca (edit-indirect :host github :repo "Fanael/edit-indirect"))

;; https://github.com/jrblevin/markdown-mode/issues/578
(setq native-comp-deferred-compilation-deny-list '("markdown-mode\\.el$"))
(setq native-comp-jit-compilation-deny-list '("markdown-mode\\.el$"))

(use-package markdown-mode
  :elpaca (markdown-mode :host github :repo "jrblevin/markdown-mode")
  :after edit-indirect
  :commands (markdown-mode gfm-mode)
  :custom
  (markdown-command "multimarkdown")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package markdown-toc
  :elpaca (markdown-toc :host github :repo "ardumont/markdown-toc"))

(use-package markdown-preview-mode
  :elpaca (markdown-preview-mode :host github :repo "ancane/markdown-preview-mode"))

(use-package nginx-mode
  :elpaca (nginx-mode :host github :repo "ajc/nginx-mode"))

(use-package plantuml-mode
  :elpaca (plantuml-mode :host github :repo "skuro/plantuml-mode")
  :custom
  (plantuml-default-exec-mode 'executable)
  :config
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))

(use-package yaml-mode
  :elpaca (yaml-mode :host github :repo "yoshiki/yaml-mode"))

(provide 'init-developer-config)
