;; stormacs-popup-window.el --- Useful emacs popup windows -*- lexical-binding: t; -*-

;; https://protesilaos.com/codelog/2024-09-19-emacs-command-popup-frame-emacsclient/
;; https://localauthor.github.io/posts/popup-frames.html
(defun popup-frame-delete (&rest _)
  "Kill selected frame if it has parameter `popup-frame'."
  (when (frame-parameter nil 'popup-frame))
  (delete-frame))

(defmacro popup-frame-define (command title &optional delete-frame)
  "Define interactive function to call COMMAND in frame with TITLE."
  `(defun ,(intern (format "popup-frame-%s" command)) ()
     (interactive)
     (let* ((display-buffer-alist '(("")
                                    (display-buffer-full-frame)))
            (frame (make-frame
                    '((title . ,title)
                      (window-system . ns)
                      (popup-frame . t)))))
       (select-frame frame)
       (switch-to-buffer " popup-frame-hidden-buffer")
       (condition-case nil
           (progn
             (call-interactively ',command)
             (delete-other-windows))
         (error (delete-frame frame)))
       (when ,delete-frame
         (sit-for 0.2)
         (delete-frame frame)))))

(popup-frame-define org-capture "capture-popup")
(add-hook 'org-capture-after-finalize-hook #'popup-frame-delete)

(popup-frame-define calc "minimal-popup")

(popup-frame-define gptel "large-popup")

(provide 'stormacs-popup-window)
