;;; init.el --- Storviks emacs config -*- lexical-binding: t -*-

;; Bootstrap elpaca
(defvar elpaca-installer-version 0.9)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract (current-time) before-init-time)))
                     gcs-done)))

(setq sys-unix-p (or (eq system-type 'gnu/linux)
                     (eq system-type 'darwin)))

;; Make elpaca work on windows
(when (not sys-unix-p)
  (setq elpaca-queue-limit 25)
  (elpaca-no-symlink-mode))

;; Install use-package
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))

;; Block until current queue processed.
(elpaca-wait)

;; Load config file with options and default values
(load-file (expand-file-name "config.el" user-emacs-directory))

;; Add directories to load pahth
(dolist (path '("site-lisp" "stormacs-lisp" "stormacs-modules"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

;; Load custom file if exists
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
(load-file stormacs-host-config)

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
;; - (with-eval-after-load 'stormacs-gui (...)) - must be placed in :init when used in use-package
;; TODO: Use (daemonp) to check if daemon
(defun stormacs-first-graphical-frame-hook-function ()
  (remove-hook 'focus-in-hook #'stormacs-first-graphical-frame-hook-function)
  (provide 'stormacs-gui))
(add-hook 'focus-in-hook #'stormacs-first-graphical-frame-hook-function)

(defun darwin-p ()
  (eq system-type 'darwin))

(defun wsl-p ()
  (when (and (eq system-type 'gnu/linux)
             (getenv "WSL"))))

(with-eval-after-load 'stormacs-gui
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (and (fboundp 'menu-bar-mode)
             (not (darwin-p))) ;; menu-bar-mode needed when using macports emacs
    (menu-bar-mode -1)))

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

;; process elpaca queue before loading conditional stuff
(elpaca-process-queues)
(when stormacs-email
  (require 'init-email))

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

;; Start emacs server if it's not running
(require 'server)
(unless (server-running-p)
  (server-start))
