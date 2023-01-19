;; init-tramp.el --- Tramp settings -*- lexical-binding: t; -*-

(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-auto-save-directory (concat user-emacs-directory "tramp.autosave/"))
(setq explicit-shell-file-name "/bin/bash")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(defun stormacs-tramp-parse-sconfigs (path)
  (mapcan 'tramp-parse-sconfig
          (directory-files path t "[^\.].+$")))

(map-put tramp-completion-function-alist-ssh
         'stormacs-tramp-parse-sconfigs "~/.ssh/config.d")

(mapc (lambda (method)
        (tramp-set-completion-function method
         '((tramp-parse-sconfig "/etc/ssh_config")
           (tramp-parse-sconfig "~/.ssh/config")
           (tramp-parse-shosts "~/.ssh/known_hosts")
           (tramp-parse-rhosts "~/.rhosts")
           (tramp-parse-rhosts "~/.shosts")
           (tramp-parse-rhosts "/etc/hosts.equiv")
           (tramp-parse-rhosts "/etc/shosts.equiv")
           (tramp-parse-shosts "/etc/ssh_known_hosts")
           (tramp-parse-sconfig "/etc/ssh_config")
           (st/tramp-parse-sconfigs "~/.ssh/config.d"))))
      '("scp" "scpc" "ssh"))

(provide 'init-tramp)
