;; Keep emacs Custom-settings in separate file, load if exists
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Require org and load emacs_init.org
(require 'org)
(org-babel-load-file (expand-file-name "emacs_init.org" user-emacs-directory))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
