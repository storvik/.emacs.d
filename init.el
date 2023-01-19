;;; init.el --- Storviks emacs config -*- lexical-binding: t -*-

;; Bootstrap elpaca
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :build (:not elpaca--activate-package)))
(when-let ((repo  (expand-file-name "repos/elpaca/" elpaca-directory))
           (build (expand-file-name "elpaca/" elpaca-builds-directory))
           (order (cdr elpaca-order))
           ((add-to-list 'load-path (if (file-exists-p build) build repo)))
           ((not (file-exists-p repo))))
  (condition-case-unless-debug err
      (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
               ((zerop (call-process "git" nil buffer t "clone"
                                     (plist-get order :repo) repo)))
               (default-directory repo)
               ((zerop (call-process "git" nil buffer t "checkout"
                                     (or (plist-get order :ref) "--")))))
          (progn
            (byte-recompile-directory repo 0 'force)
            (require 'elpaca)
            (and (fboundp 'elpaca-generate-autoloads)
                 (elpaca-generate-autoloads "elpaca" repo))
            (kill-buffer buffer))
        (error "%s" (with-current-buffer buffer (buffer-string))))
    ((error)
     (warn "%s" err)
     (delete-directory repo 'recursive))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package
(elpaca use-package)

(defgroup stormacs nil
  "User options for my emacs config."
  :group 'file)

(dolist (path '("site-lisp" "stormacs-lisp" "stormacs-modules"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Look up host specific config
(let ((host-cfg (expand-file-name (file-name-concat
                                   user-emacs-directory
                                   "hosts"
                                   (concat (downcase system-name) ".el")))))
  (if (file-exists-p host-cfg)
      (setq stormacs-host-config host-cfg)
    (setq stormacs-host-config (file-name-concat (file-name-directory host-cfg) "default.el"))))
(load stormacs-host-config)

(setq sys-unix-p (or (eq system-type 'gnu/linux)
                     (eq system-type 'darwin)))

(defun sys-type-p (name)
  (eq system-type name))

(defmacro sys-diversification (gnu/linux &optional darwin win)
  (cond ((sys-type-p 'gnu/linux) gnu/linux)
        ((sys-type-p 'darwin) darwin)
        ((sys-type-p 'windows-nt) win)
        (t nil)))

;; This neat little code let me run stuff after first gui frame is created.
;; Especially helpful when running in daemon mode. Usage:
;; - :after stormacs-gui
;; - (with-eval-after-load 'st/gui (...)) - must be placed in :init when used in use-package
;; TODO: Use (daemonp) to check if daemon
(defun stormacs-first-graphical-frame-hook-function ()
  (remove-hook 'focus-in-hook #'stormacs-first-graphical-frame-hook-function)
  (provide 'stormacs-gui))
(add-hook 'focus-in-hook #'stormacs-first-graphical-frame-hook-function)

;; TODO: Custom method to detect wsl, should be possible to override

(require 'init-common)
(require 'init-appearance)
(require 'init-keybindings)
(require 'init-completion)
(require 'init-meow)
(require 'init-navigation)
(require 'init-git)
(require 'init-tramp)
(require 'init-eshell)
(require 'init-detached)
(require 'init-dired)
(require 'init-org)
(require 'init-developer)

(defun stormacs-load-all-in-directory (dir)
  "`load' all elisp libraries in directory DIR which are not already loaded."
  (interactive "D")
  (let ((libraries-loaded (mapcar #'file-name-sans-extension
                                  (delq nil (mapcar #'car load-history)))))
    (dolist (file (directory-files dir t ".+\\.elc?$"))
      (let ((library (file-name-sans-extension file)))
        (unless (member library libraries-loaded)
          (load library nil t)
          (push library libraries-loaded))))))

(stormacs-load-all-in-directory (locate-user-emacs-file "stormacs-lisp"))
