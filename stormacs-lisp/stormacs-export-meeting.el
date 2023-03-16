;; stormacs-export-meeting.el --- Export meeting note -*- lexical-binding: t; -*-

(defun stormacs-export-meeting ()
  "Function that exports meeting note.

If stormacs-meeting-export-directory is set, export to that dir."
  (interactive)
  (let* ((headline (car (last (org-get-outline-path t))))
         (headline-list (split-string headline " "))
         (time-str (format-time-string "%Y.%m.%d"
                                       (org-read-date nil t (substring headline 0 16) nil)))
         (filename (concat
                    (file-name-as-directory stormacs-meeting-export-directory)
                    time-str
                    " - "
                    (substring headline 17 nil) ".html")))
    (save-restriction
      (org-narrow-to-subtree)
      (org-export-to-file
          'html
          filename nil nil nil nil)
          (widen))))

(provide 'stormacs-export-meeting)
