;;; tool-notes.el --- Notes -*- lexical-binding: t; -*-

;;; Commentary:
;;; Taking notes.

;;; Code:

;;;
;; Packages

(use-package deft
  :commands deft
  :config
  (setq deft-directory "~/notes"
        deft-extensions
        '("txt" "tex" "org" "md" "rst")))

(provide 'tool-notes)
;;; tool-notes.el ends here
