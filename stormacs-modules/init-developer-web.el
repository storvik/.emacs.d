;; init-developer-web.el --- Developer web settings -*- lexical-binding: t; -*-

(use-package emmet-mode
  :ensure (emmet-mode :host github :repo "smihica/emmet-mode")
  :hook (web-mode sgml-mode)
  :config
  (setq emmet-expand-jsx-className? t))

(use-package web-mode
  :ensure (web-mode :host github :repo "fxbois/web-mode")
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.sass?\\'" . web-mode))
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")
          ("jsx" . "\\.sass?\\'")))
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 4)
  (setq-default web-mode-code-indent-offset 4)
  (setq-default web-mode-sql-indent-offset 4)
  (setq web-mode-enable-current-column-highlight t))

(provide 'init-developer-web)
