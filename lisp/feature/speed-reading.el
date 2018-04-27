;;; feature-speed-reading.el --- Speed reading -*- lexical-binding: t; -*-

;;; Commentary:
;; Reading on steroids.

;;; Code:

(eval-when-compile
  (require 'base-package))

(defvar-local +spray-pre-evil-state
  "Hold  evil state to resume after exit.")
(defvar-local +spray-pre-read-only
  "Hold read-only mode to resume back after exit.")

;;;
;; Packages

(req-package spray
  :commands spray-mode
  :init
  (defun speed-read ()
    "Start spray speed reading on current buffer at current point."
    (interactive)
    (setq +spray-pre-evil-state evil-state)
    (setq +spray-pre-read-only view-read-only)

    (evil-emacs-state)
    (read-only-mode 1)

    (spray-mode 1)
    (internal-show-cursor (selected-window) nil))

  (defadvice spray-quit (after activate)
    "Correctly quit spray."
    (internal-show-cursor (selected-window) t)

    (evil-change-state +spray-pre-evil-state)

    (setq buffer-read-only +spray-pre-read-only)
    (goto-char (point-min)))

  (setq spray-margin-left 2
        spray-height 500)
  :config
  (setq spray-unsupported-minor-modes
        (append '(beacon-mode centered-window-mode visual-fill-column-mode)
                spray-unsupported-minor-modes)))

(provide 'feature-speed-reading)
;;; feature-speed-reading.el ends here
