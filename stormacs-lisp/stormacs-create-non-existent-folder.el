;; stormacs-create-non-existent-directory.el --- Create non existent folders -*- lexical-binding: t; -*-

(defun stormacs-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'stormacs-create-non-existent-directory)

(provide 'stormacs-create-non-existent-directory)
