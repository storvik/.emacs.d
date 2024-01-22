;; init-developer-lisp.el --- Developer lisp settings -*- lexical-binding: t; -*-

(use-package sly
  :elpaca (sly :host github :repo "joaotavora/sly")
  :init
  (cond ((and (executable-find "sbcl")
              (executable-find "ecl"))
         (setq sly-lisp-implementations
               '((sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)
                 (ecl ("ecl")))))
        ((executable-find "sbcl")
         (setq inferior-lisp-program "sbcl --noinform"))
        ((executable-find "ecl")
         (setq inferior-lisp-program "ecl"))))

(use-package sly-asdf
  :elpaca (sly-asdf :host github :repo "mmgeorge/sly-asdf")
  :after sly)

(use-package clojure-mode
  :elpaca (clojure-mode :host github :repo "clojure-emacs/clojure-mode"))

(use-package cider
  :elpaca (cider :host github :repo "clojure-emacs/cider"))

(use-package inf-clojure
  :elpaca (inf-clojure :host github :repo "clojure-emacs/inf-clojure")
  :after clojure-mode)

(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

(use-package package-lint
  :elpaca (package-lint :host github :repo "purcell/package-lint"))

(provide 'init-developer-lisp)
