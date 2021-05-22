;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Make use-package use straight and ensure packages are installed
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Keep emacs Custom-settings in separate file, load if exists
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Install org before using it to load init file
(straight-use-package 'org-contrib)

;; Require org and load emacs_init.org
(org-babel-load-file (expand-file-name "emacs_init.org" user-emacs-directory))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
