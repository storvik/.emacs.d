;; init-eshell.el --- Eshell config -*- lexical-binding: t; -*-

(defun eshell/ll (&rest args)
  "Alias ll -> ls -l"
  (apply #'eshell/ls (cons '-l args)))

(defun eshell/clc ()
  "Clear the eshell buffer."
  (eshell/clear-scrollback))

(defun stormacs-eshell-fishy-path (path)
  "Takes eshell path and makes it fishy."
  (let ((pathlist (split-string (replace-regexp-in-string
                                 (file-truename "~") "~" path) "/")))
    (concat (string-join (mapcar (lambda (el)
                                   (unless (= (length el) 0)
                                     (substring el 0 1)))
                                 (butlast pathlist 1))
                         "/")
            (unless (and (eq (length pathlist) 1)
                         (string= (car pathlist) "~"))
              "/")
            (car (last pathlist)))))

(defun stormacs-eshell-prompt-function ()
  "Custom eshell prompt function."
  (concat
   (if (file-remote-p default-directory)
       (propertize (file-remote-p default-directory)
                   'face font-lock-keyword-face)
     (concat (propertize user-login-name 'face font-lock-function-name-face)
             "@"
             (propertize system-name 'face font-lock-keyword-face)))
   (propertize (concat "  "
                       (st/eshell-fishy-path (eshell/pwd)))
               'face font-lock-string-face)
   (when (magit-get-current-branch)
     (propertize (concat " ( "
                         (magit-get-current-branch)
                         ")")
                 'face font-lock-comment-face))
   "> "))

(setq eshell-prompt-function 'stormacs-eshell-prompt-function)
(setq eshell-prompt-regexp "[a-zA-Z0-9-_@:/]+\\ \\ [a-zA-Z0-9-_/~]+\\( \( [a-zA-Z0-9-_@/.]+\)\\)*>\\ ")

(defun stormacs-eshell-rename-buffer ()
  "Rename buffer based on path."
  (interactive)
  (rename-buffer (concat "*eshell*<"
                         (replace-regexp-in-string
                          (file-truename "~") "~" (eshell/pwd))
                         ">") t))

(add-hook 'eshell-mode-hook 'stormacs-eshell-rename-buffer)
(add-hook 'eshell-directory-change-hook 'stormacs-eshell-rename-buffer)

(provide 'init-eshell)
