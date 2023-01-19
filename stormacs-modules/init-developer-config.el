;; init-developer-config.el --- Developer config and doc file settings -*- lexical-binding: t; -*-

;; TODO: Fix this
;; (elpaca-use-package
;;  (csv-mode :host github :repo ""))

(elpaca-use-package
 (dockerfile-mode :host github :repo "spotify/dockerfile-mode"))

(elpaca-use-package
 (edit-indirect :host github :repo "Fanael/edit-indirect"))

(elpaca-use-package
 (markdown-mode :host github :repo "jrblevin/markdown-mode")
 :after edit-indirect
 :commands (markdown-mode gfm-mode)
 :custom
 (markdown-command "multimarkdown")
 :mode (("README\\.md\\'" . gfm-mode)
        ("\\.md\\'" . gfm-mode)
        ("\\.markdown\\'" . markdown-mode)))

(elpaca-use-package
 (markdown-toc :host github :repo "ardumont/markdown-toc"))

(elpaca-use-package
 (markdown-preview-mode :host github :repo "ancane/markdown-preview-mode"))

(elpaca-use-package
 (nginx-mode :host github :repo "ajc/nginx-mode"))

;; TODO: Cannot load before org
;; (elpaca-use-package
;;  (plantuml-mode :host github :repo "skuro/plantuml-mode")
;;  :custom
;;  (plantuml-default-exec-mode 'executable)
;;  :config
;;  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
;;  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))

(elpaca-use-package
 (yaml-mode :host github :repo "yoshiki/yaml-mode"))

(provide 'init-developer-config)
