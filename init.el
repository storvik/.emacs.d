(package-initialize)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(require 'org)
(org-babel-load-file "~/.emacs.d/emacs.org")
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
