;;; tool-rotate-text.el --- Text rotation

;;; Commentary:
;;; Switch between different text values.

;;; Code:
(use-package rotate-text :ensure nil
  :load-path "vendor/rotate-text/"
  :commands (rotate-text rotate-text-backward)
  :config
  (dolist (item '(("true" "false")
                  ("assert" "refute")))
    (push item rotate-text-words)))

(provide 'tool-rotate-text)
;;; tool-rotate-text.el ends here
