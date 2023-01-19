;; init-developer-lisp.el --- Developer lisp settings -*- lexical-binding: t; -*-

(elpaca-use-package
 (sly "joaotavora/sly")
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

(elpaca-use-package
 (sly-asdf :host github :repo "mmgeorge/sly-asdf")
 :after sly)

(elpaca-use-package
 (clojure-mode :host github :repo "clojure-emacs/clojure-mode"))

(elpaca-use-package
 (cider :host github :repo "clojure-emacs/cider"))

(elpaca-use-package
 (inf-clojure :host github :repo "clojure-emacs/inf-clojure")
 :after clojure-mode)

(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

(elpaca-use-package
 (package-lint :host github :repo "purcell/package-lint"))

(provide 'init-developer-lisp)
