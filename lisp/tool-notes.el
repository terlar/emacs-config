;;; tool-notes.el --- Notes -*- lexical-binding: t; -*-

;;; Commentary:
;;; Taking notes.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(req-package deft
  :commands (deft deft-find-file)
  :hook (deft-mode . line-cursor)
  :init
  (setq deft-directory "~/org"
        deft-extensions
        '("txt" "tex" "org" "md" "rst")
        deft-default-extension "org"
        deft-recursive t
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t)
  :config
  (set-evil-state 'deft-mode 'emacs))

(provide 'tool-notes)
;;; tool-notes.el ends here
