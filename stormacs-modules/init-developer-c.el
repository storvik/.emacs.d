;; init-developer-c.el --- Developer C / C++ settings -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.cu\\'" . c-mode))

(defun stormacs-c-mode-common-hook ()
  (c-set-offset 'substatement-open 0)
  ;;(setq c++-tab-always-indent nil)
  (setq c-basic-offset 4)
  (setq c-indent-level 4)
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4))
(add-hook 'c-mode-common-hook 'stormacs-c-mode-common-hook)

(setq c-ts-mode-indent-offset 4)

(use-package meson-mode
  :ensure (meson-mode :host github :repo "wentasah/meson-mode"))

(use-package emacs
  :ensure nil
  :mode ("CMakeLists.txt\\'" . cmake-ts-mode))

(provide 'init-developer-c)
