;;; sessions.el --- Sessions -*- coding: utf-8; -*-

;;; Commentary:
;; Never-ending state.

;;; Code:

(require 'desktop)

(setq desktop-save t)

(defun load-desktop ()
  "Load the desktop and enable autosaving."
  (interactive)
  (let ((desktop-load-locked-desktop "ask"))
    (desktop-read)
    (desktop-save-mode 1)))

(add-to-list 'desktop-modes-not-to-save 'pdf-view-mode)
(add-to-list 'desktop-modes-not-to-save 'image-mode)
(add-to-list 'desktop-locals-to-save 'compile-history)
(add-to-list 'desktop-locals-to-save 'compile-command)
(add-to-list 'desktop-locals-to-save 'ispell-local-dictionary)

(provide 'feature-sessions)
;;; sessions.el ends here
