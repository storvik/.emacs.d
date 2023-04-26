;; init-email.el --- Email / MU4E config -*- lexical-binding: t; -*-

;; Load path to mu4e, platform dependent.
(add-to-list 'load-path (expand-file-name "~/.nix-profile/share/emacs/site-lisp/mu4e"))

(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

;; Automatic updates
(setq mu4e-update-interval 300)

;; Don't save messages to Sent Messages, Gmail/IMAP takes care of this
;; (setq mu4e-sent-messages-behavior 'delete)

;; Attempt to show images when viewing messages
(setq mu4e-view-show-images t)

;; Save attachment (this can also be a function)
(setq mu4e-attachment-dir "~/Downloads")

;; Change filename when moving, to avoid duplicate uid error in mbsync
;;(setq mu4e-change-filenames-when-moving t)

;; Remove message buffer when finished
(setq message-kill-buffer-on-exit t)

;; Avoid mu4e quit prompt
(setq mu4e-confirm-quit nil)

;; Turn off auto-fill
;; (defun no-auto-fill ()
;;   "Turn off auto-fill-mode."
;;   (auto-fill-mode -1))
;; (add-hook 'mu4e-compose-mode-hook #'no-auto-fill)

(defun stormacs-add-cc-bcc ()
  (save-excursion (message-add-header "Cc:\n"))
  (save-excursion (message-add-header "Bcc:\n")))
(add-hook 'mu4e-compose-mode-hook #'stormacs-add-cc-bcc)

;; https://tushartyagi.com/blog/configure-mu4e-and-msmtp/
;; https://macowners.club/posts/email-emacs-mu4e-macos/
(setq send-mail-function 'sendmail-send-it                           ;; Use sendmail (msmtp)
      sendmail-program (executable-find "msmtp")                     ;; Specify smtp
      message-send-mail-function 'message-send-mail-with-sendmail    ;; Use sendmail
      message-sendmail-extra-arguments '("--read-envelope-from")     ;; msmtp reads sender from msg header and use correct account
      message-sendmail-f-is-evil t                                   ;; Remove username from Emacs message
      message-sendmail-envelope-from 'header)                        ;; Use from header of message

;; Setup contexts
(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "Festival"
           :enter-func (lambda () (mu4e-message "Entering svartisenfestivalen context"))
           :leave-func (lambda () (mu4e-message "Leaving svartisenfestivalen context"))
           ;; we match based on the maildir of the message
           ;; this matches maildir /Arkham and its sub-directories
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               :to "petter@svartisenfestivalen.no")))
           :vars '( (user-mail-address      . "petter@svartisenfestivalen.no")
                    (user-full-name         . "Petter S. Storvik")
                    (mu4e-drafts-folder     . "/svartisenfestivalen/Drafts")
                    (mu4e-sent-folder       . "/svartisenfestivalen/Sent")
                    (mu4e-trash-folder      . "/svartisenfestivalen/Trash")
                    (mu4e-refile-folder     . "/svartisenfestivalen/Archive")
                    (mu4e-compose-signature .
                      (concat
                       "\nMed vennlig hilsen\n"
                       "Petter Sakrihei Storvik\n\n"
                       "Svartisenfestivalen\n"
                       "Booking / Sponsorkontakt\n"
                       "Web: http://www.svartisenfestivalen.no\n"
                       "Tlf: +47 958 83 676\n"
                       "E-mail: petter@svartisenfestivalen.no"))))
         ,(make-mu4e-context
           :name "Storvik.dev"
           :enter-func (lambda () (mu4e-message "Entering storvik.dev context"))
           :leave-func (lambda () (mu4e-message "Leaving storvik.dev context"))
           ;; we match based on the maildir of the message
           ;; this matches maildir /Arkham and its sub-directories
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               :to "petter@storvik.dev")))
           :vars '( (user-mail-address              . "petter@storvik.dev"  )
                    (user-full-name                 . "Petter S. Storvik")
                    (mu4e-drafts-folder             . "/storvikdev/Drafts")
                    (mu4e-sent-folder               . "/storvikdev/Sent")
                    (mu4e-trash-folder              . "/storvikdev/Trash")
                    (mu4e-refile-folder             . "/storvikdev/Archive")
                    (mu4e-compose-signature .
                      (concat
                       "\nMed vennlig hilsen\n"
                       "Petter Sakrihei Storvik\n\n"))))))

;; This sets `mu4e-user-mail-address-list' to the concatenation of all
;; `user-mail-address' values for all contexts. If you have other mail
;; addresses as well, you'll need to add those manually.
(setq mu4e-user-mail-address-list
      (delq nil
            (mapcar (lambda (context)
                      (when (mu4e-context-vars context)
                        (cdr (assq 'user-mail-address (mu4e-context-vars context)))))
                    mu4e-contexts)))

;; start with default context
(setq mu4e-context-policy 'pick-first)

;; Shortcuts to maildirs
(setq mu4e-maildir-shortcuts
      '(("/svartisenfestivalen/Inbox"               . ?p)
        ("/svartisenfestivalen/Archive"             . ?t)
        ("/svartisenfestivalen/Sent"                . ?d)
        ("/storvikdev/Inbox"                        . ?f)
        ("/storvikdev/Archive"                      . ?s)
        ("/storvikdev/Sent"                         . ?c)))

;; Change date format
(setq mu4e-headers-date-format "%Y.%m.%d %H:%M")

;; Include related mail, use carefully when deleting mail with search queries.
(setq mu4e-headers-include-related t)

;; Show number of recipients
(add-to-list 'mu4e-header-info-custom
  '(:recipnum .
     ( :name "Number of recipients"                  ;; long name, as seen in the message-view
       :shortname " R"                                ;; short name, as seen in the headers view
       :help "Number of recipients for this message" ;; tooltip
       :function (lambda (msg)
          (format "%2d"
            (+ (length (mu4e-message-field msg :to))
               (length (mu4e-message-field msg :cc))))))))
(add-to-list 'mu4e-view-fields :recipnum)
(add-to-list 'mu4e-headers-fields '(:recipnum))

;; Make date field wider
(add-to-list 'mu4e-headers-fields '(:human-date . 17))

(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; Remove automatic newlines
(setq mu4e-compose-format-flowed t)

;; Attach files to bottom to avoid exchange bug
(defun stormacs-attach-file ()
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((pos (point-marker)))
      (goto-char (point-max))
      (call-interactively 'mail-add-attachment)
      (goto-char pos))))

(define-key mu4e-compose-mode-map (kbd "C-c C-a") 'stormacs-attach-file)

(add-hook 'message-send-hook
          (lambda ()
            (unless (yes-or-no-p "Are you sure you want to send this?")
              (signal 'quit nil))))

;; Make org capture templates with =%a= include link to selected email in mu4e.
(require 'org-mu4e)
(setq org-mu4e-link-query-in-headers-mode nil)

(provide 'init-email)
