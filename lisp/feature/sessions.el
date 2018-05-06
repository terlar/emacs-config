;;; sessions.el --- Sessions -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Never-ending state.

;;; Code:

(eval-when-compile
  (defvar desktop-modes-not-to-save))

(desktop-save-mode 1)

(add-to-list 'desktop-modes-not-to-save 'pdf-view-mode)
(add-to-list 'desktop-modes-not-to-save 'image-mode)
(add-to-list 'desktop-locals-to-save 'compile-history)
(add-to-list 'desktop-locals-to-save 'compile-command)
(add-to-list 'desktop-locals-to-save 'ispell-local-dictionary)

(provide 'feature-sessions)
;;; sessions.el ends here
