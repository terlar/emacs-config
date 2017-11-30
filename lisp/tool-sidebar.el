;;; tool-sidebar.el --- Sidebar -*- lexical-binding: t; -*-

;;; Commentary:
;; File navigation tree.

;;; Code:

(eval-when-compile
  (require 'base-lib)
  (require 'base-package))

;;;
;; Packages

(req-package dired-sidebar
  :commands dired-sidebar-toggle-sidebar
  :hook
  (dired-sidebar-mode . hide-mode-line)
  (dired-sidebar-mode . line-cursor)
  (dired-sidebar-mode
   . (lambda ()
       (setq-local beacon-mode nil)
       (stripe-buffer-mode 0))))

(provide 'tool-sidebar)
;;; tool-sidebar.el ends here
