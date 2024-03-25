;; init-developer-dart.el --- Developer dart settings -*- lexical-binding: t; -*-

(use-package dart-mode
  :ensure (dart-mode :host github :repo "bradyt/dart-mode"))

(use-package flutter
  :ensure (flutter :host github :repo "amake/flutter.el"))

(provide 'init-developer-dart)
