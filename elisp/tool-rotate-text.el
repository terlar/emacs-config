;;; tool-rotate-text.el --- Text rotation -*- lexical-binding: t; -*-

;;; Commentary:
;;; Switch between different text values.

;;; Code:

;;;
;; Packages

(use-package rotate-text :ensure nil :pin manual
  :load-path "vendor/rotate-text/"
  :commands (rotate-text rotate-text-backward)
  :config
  (dolist (item '(("true" "false")
                  ("assert" "refute")))
    (push item rotate-text-words)))

(provide 'tool-rotate-text)
;;; tool-rotate-text.el ends here
