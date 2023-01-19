;; stormacs-indent-buffer.el --- Reindent buffer -*- lexical-binding: t; -*-

(defun stormacs-indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (if indent-tabs-mode
        ;; Add more modes before/after web-mode
        (if (derived-mode-p 'web-mode)
            (untabify (point-min) (point-max))
          (tabify (point-min) (point-max)))
      (untabify (point-min) (point-max)))))

(bind-key "/" #'stormacs-indent-buffer stormacs-prefix-map)

(provide 'stormacs-indent-buffer)
