;;; tool-notes.el --- Notes

;;; Commentary:
;;; Taking notes.

;;; Code:
(use-package deft
  :commands deft
  :config
  (setq deft-directory "~/notes"
        deft-extensions
        '("txt" "tex" "org" "md" "rst")))

(provide 'tool-notes)
;;; tool-notes.el ends here
