;;; tool-sidebar.el --- Sidebar -*- lexical-binding: t; -*-

;;; Commentary:
;; File navigation tree.

;;; Code:

(eval-when-compile
  (require 'base-lib)
  (require 'base-package))

;;;
;; Packages

(use-package dired-sidebar
  :commands dired-sidebar-toggle-sidebar
  :hook
  (dired-sidebar-mode . +dired-sidebar-setup)
  :preface
  (defun +dired-sidebar-setup ()
    (hide-mode-line)
    (line-cursor)
    (setq-local beacon-mode nil)
    (stripe-buffer-mode 0)))

(provide 'tool-sidebar)
;;; tool-sidebar.el ends here
