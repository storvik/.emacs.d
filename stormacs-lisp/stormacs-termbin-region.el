;; stormacs-termbin-region.el --- Send region to termbin -*- lexical-binding: t; -*-

(defun stormacs-termbin-region (begin end)
  "Sends region to termbin, if no region active send entire buffer"
  (interactive "r")
  (kill-new
   (car
    (split-string
     (with-output-to-string
       (if (use-region-p)
           (shell-command-on-region begin end "nc termbin.com 9999" standard-output)
         (shell-command-on-region (point-min) (point-max) "nc termbin.com 9999" standard-output)))
     "\n"))))

(provide 'stormacs-termbin-region)
