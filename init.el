;;; init.el --- Storviks emacs config -*- lexical-binding: t -*-

;; Bootstrap elpaca
(defvar elpaca-installer-version 0.2)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(when-let ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
           (build (expand-file-name "elpaca/" elpaca-builds-directory))
           (order (cdr elpaca-order))
           ((add-to-list 'load-path (if (file-exists-p build) build repo)))
           ((not (file-exists-p repo))))
  (condition-case-unless-debug err
      (if-let ((buffer (pop-to-buffer-same-window "*elpaca-installer*"))
               ((zerop (call-process "git" nil buffer t "clone"
                                     (plist-get order :repo) repo)))
               (default-directory repo)
               ((zerop (call-process "git" nil buffer t "checkout"
                                     (or (plist-get order :ref) "--"))))
               (emacs (concat invocation-directory invocation-name))
               ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                     "--eval" "(byte-recompile-directory \".\" 0 'force)"))))
          (progn (require 'elpaca)
                 (elpaca-generate-autoloads "elpaca" repo)
                 (kill-buffer buffer))
        (error "%s" (with-current-buffer buffer (buffer-string))))
    ((error) (warn "%s" err) (delete-directory repo 'recursive))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract (current-time) before-init-time)))
                     gcs-done)))

;; Install use-package
(elpaca elpaca-use-package
  ;; Enable :elpaca keyword support
  (elpaca-use-package-mode)
  ;; Assume :elpaca t for packages unless otherwise specified.
  (setq elpaca-use-package-by-default t))


;; Block until current queue processed.
(elpaca-wait)

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

(defun wsl-p ()
  (when (and (eq system-type 'gnu/linux)
             (getenv "WSLENV"))))

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
