;;; feature-debug.el --- Debug -*- lexical-binding: t; -*-

;;; Commentary:
;; Locate those bugs.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(req-package realgud
  :commands
  (realgud:gdb
   realgud:trepanjs
   realgud:bashdb
   realgud:zshdb))

(provide 'feature-debug)
;;; feature-debug.el ends here
