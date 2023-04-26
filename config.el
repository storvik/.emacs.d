;; init-config.el --- Configuration options -*- lexical-binding: t; -*-

;;; Commentary:

;; This file includes all configuration options and default values meant
;; to be set in host-specific confguration `./hosts/<hostname>.el'.

(defgroup stormacs nil
  "User options for my emacs config."
  :group 'file)

(defcustom stormacs-email nil
  "If t, email config is loaded."
  :type 'boolean
  :group 'stormacs)

(provide 'init-config)
