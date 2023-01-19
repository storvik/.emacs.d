;; Disable package.el
(setq package-enable-at-startup nil)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Silent warnings in native compilation
(setq native-comp-async-report-warnings-errors 'silent)
