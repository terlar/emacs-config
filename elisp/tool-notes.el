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
        '("txt" "tex" "org" "md" "rst")
        deft-default-extension "org"
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t))

(provide 'tool-notes)
;;; tool-notes.el ends here
