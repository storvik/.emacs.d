;; init-meow.el --- Meow modal editing -*- lexical-binding: t; -*-

;; TODO: Review this:
;; (setq meow-two-char-escape-sequence "jk")
;; (setq meow-two-char-escape-delay 0.5)

;; (defun meow--two-char-exit-insert-state (s)
;;   (when (meow-insert-mode-p)
;;     (let ((modified (buffer-modified-p)))
;;       (insert (elt s 0))
;;       (let* ((second-char (elt s 1))
;;              (event
;;               (if defining-kbd-macro
;;                   (read-event nil nil)
;;                 (read-event nil nil meow-two-char-escape-delay))))
;;         (when event
;;           (if (and (characterp event) (= event second-char))
;;               (progn
;;                 (backward-delete-char 1)
;;                 (set-buffer-modified-p modified)
;;                 (meow--execute-kbd-macro "<escape>"))
;;             (push event unread-command-events)))))))
;; (defun meow-two-char-exit-insert-state ()
;;   (interactive)
;;   (meow--two-char-exit-insert-state meow-two-char-escape-sequence))
;; (define-key meow-insert-state-keymap (substring meow-two-char-escape-sequence 0 1)
;;             #'meow-two-char-exit-insert-state)

;; TODO: Should add flymake prev / next, expand region, gptel, consult?
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak-dh)
  (meow-motion-overwrite-define-key
   ;; had to change meow-next/prev for next/previous-line in order
   ;; to make dired-preview work
   '("n" . next-line)
   '("e" . previous-line)
   '("<escape>" . ignore))
  (meow-leader-define-key
   '("?" . meow-cheatsheet)
   ;; To execute the originally e in MOTION state, use SPC e.
   '("e" . "H-e")
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '(":" . stormacs-tsc-symbol-overlay)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("/" . meow-visit)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . goto-last-change)
   '("d" . stormacs-tsc-dogears)
   '("e" . meow-prev)
   '("E" . meow-prev-expand)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("m" . meow-left)
   '("M" . meow-left-expand)
   '("i" . meow-right)
   '("I" . meow-right-expand)
   '("j" . meow-join)
   '("J" . avy-goto-char-timer)
   '("k" . meow-kill)
   '("l" . meow-line)
   '("L" . meow-goto-line)
   '("h" . meow-mark-word)
   '("H" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("s" . meow-insert)
   '("S" . meow-open-above)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-search)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("y" . meow-save)
   '("Y" . gptel-menu)
   '("z" . meow-pop-selection)
   '("Z" . pop-global-mark)
   '("'" . repeat)
   '("<escape>" . ignore))
  (meow-thing-register 'arrow '(pair ("<") (">")) '(pair ("<") (">")))
  (add-to-list 'meow-char-thing-table '(?a . arrow)))

(use-package meow
  :ensure (meow :host github :repo "meow-edit/meow")
  :demand t
  :hook ((git-commit-setup . meow-insert)
         (org-capture-mode . meow-insert)
         (meow-insert-exit . meow--corfu-quit))
  :custom
  (meow-use-clipboard t)
  (meow-expand-hint-remove-delay 0)
  :config
  (defun meow--corfu-quit ()
    "Quit corfu if candidates."
	(when corfu--candidates
	  (corfu-quit)))
  (meow-setup)
  (meow-global-mode 1))

(use-package meow-tree-sitter
  :ensure (meow-tree-sitter :host github :repo "skissue/meow-tree-sitter")
  :after meow
  :config
  (meow-tree-sitter-register-defaults))

(provide 'init-meow)
