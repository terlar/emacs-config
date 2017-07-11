;;; feature-debug.el --- Debug

;;; Commentary:
;; Locate those bugs.

;;; Code:

;;;
;; Packages

(use-package realgud
  :commands (realgud:gdb realgud:trepanjs realgud:bashdb realgud:zshdb))

(provide 'feature-debug)
;;; feature-debug.el ends here
