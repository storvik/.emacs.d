;; init-developer-dart.el --- Developer dart settings -*- lexical-binding: t; -*-

(use-package dart-mode
  :elpaca (dart-mode :host github :repo "bradyt/dart-mode"))

(defun stormacs-lsp-dart ()
  (interactive)
  (envrc-reload-all)
  (when-let (dart-exec (executable-find "dart"))
    (let ((dart-sdk-path (-> dart-exec
                             file-chase-links
                             file-name-directory
                             directory-file-name
                             file-name-directory)))
      (setq lsp-dart-sdk-dir dart-sdk-path
            lsp-dart-dap-flutter-hot-reload-on-save t)))
  (lsp))

(use-package lsp-dart
  :elpaca (lsp-dart :host github :repo "emacs-lsp/lsp-dart"))

(provide 'init-developer-dart)
