;;; tool-rotate-text.el --- Text rotation -*- lexical-binding: t; -*-

;;; Commentary:
;;; Switch between different text values.

;;; Code:

;;;
;; Packages

(req-package rotate-text
  :loader :el-get
  :commands (rotate-text rotate-text-backward)
  :config
  (dolist (item '(("true" "false")
                  ("assert" "refute")))
    (push item rotate-text-words)))

(provide 'tool-rotate-text)
;;; tool-rotate-text.el ends here
