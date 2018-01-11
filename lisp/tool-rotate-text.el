;;; tool-rotate-text.el --- Text rotation -*- lexical-binding: t; -*-

;;; Commentary:
;;; Switch between different text values.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(req-package rotate-text
  :el-get t
  :commands (rotate-text rotate-text-backward)
  :config
  (dolist (item '(("true" "false")
                  ("assert" "refute")))
    (push item rotate-text-words)))

(provide 'tool-rotate-text)
;;; tool-rotate-text.el ends here
