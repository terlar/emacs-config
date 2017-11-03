;;; feature-speed-reading.el --- Speed reading -*- lexical-binding: t; -*-

;;; Commentary:
;; Reading on steroids.

;;; Code:

;;;
;; Packages

(use-package spray :demand t
  :commands my|start-spray
  :preface
  (eval-when-compile
    (defvar evil-state)
    (declare-function evil-emacs-state "evil"))

  (defvar my-spray-pre-evil-state
    "Hold  evil state to resume after exit.")
  (defvar my-spray-pre-read-only
    "Hold read-only mode to resume back after exit.")

  (defun my|start-spray ()
    "Start spray speed reading on current buffer at current point."
    (interactive)
    (setq-local my-spray-pre-evil-state evil-state)
    (setq-local my-spray-pre-read-only view-read-only)

    (evil-emacs-state)
    (read-only-mode +1)

    (spray-mode +1)
    (internal-show-cursor (selected-window) nil))

  (defadvice spray-quit (after activate)
    "Correctly quit spray."
    (internal-show-cursor (selected-window) t)

    (evil-change-state my-spray-pre-evil-state)
    (setq buffer-read-only my-spray-pre-read-only)
    (goto-char (point-min))))

(provide 'feature-speed-reading)
;;; feature-speed-reading.el ends here
