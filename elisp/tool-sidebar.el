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
  :config
  (add-hook! 'dired-sidebar-mode
             (setq-local beacon-mode nil)
             (stripe-buffer-mode 0)
             (add-hook 'buffer-list-update-hook #'line-cursor nil t))

  (add-hooks-pair 'dired-sidebar-mode '(line-cursor
                                        hide-mode-line)))

(provide 'tool-sidebar)
;;; tool-sidebar.el ends here
