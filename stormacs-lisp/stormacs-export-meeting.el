;; stormacs-export-meeting.el --- Export meeting note -*- lexical-binding: t; -*-

(defun stormacs-export-meeting (&optional backend async subtreep visible-only body-only ext-plist)
  "Export org subtree to file and name it according to subtree heading.

`stormacs-meeting-export-directory' and `stormacs-meeting-export-backend'
can be set in order to control where exported document will be stored
and which backend to be used when exporting. These is typically set
as a file local variable inside an org file.

Available backends are the ones of `org-export-backends' and 'pdf.

When optional argument SUBTREEP is non-nil, transcode the
sub-tree at point, extracting information from the headline
properties first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only return body
code, without surrounding template.

Optional argument EXT-PLIST, when provided, is a property list
with external parameters overriding Org default settings, but
still inferior to file-local settings."
  (interactive)
  (let* ((backend (unless backend
                    (if stormacs-meeting-export-backend
                        (intern stormacs-meeting-export-backend)
                      (intern
                       (completing-read "Available backends: "
                                        (append org-export-backends '(pdf)))))))
         (headline (car (last (org-get-outline-path t))))
         (headline-list (split-string headline " "))
         (time-str (format-time-string "%Y.%m.%d"
                                       (org-read-date nil t (substring headline 0 16) nil)))
         (filename (concat
                    (file-name-as-directory stormacs-meeting-export-directory)
                    time-str
                    " - "
                    (substring headline 17 nil) "." (if (eq backend 'pdf) "tex" backend))))
    (save-restriction
      (org-narrow-to-subtree)
      (org-export-to-file
          (if (eq backend 'pdf) 'latex backend)
          filename async subtreep visible-only body-only ext-plist
          (when (eq backend 'pdf)
            (lambda (file) (org-latex-compile file) (delete-file filename))))
      (widen))))

(provide 'stormacs-export-meeting)
