;;; feature-workspaces.el --- Workspaces -*- lexical-binding: t; -*-

;;; Commentary:
;; Your very own workspace.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

;;;
;; Packages

(use-package persp-mode
  :demand t
  :init
  (setq persp-keymap-prefix (kbd "C-c TAB")
        persp-autokill-buffer-on-remove 'kill-weak
        persp-nil-name "main"
        persp-nil-hidden t
        persp-auto-save-fname "autosave"
        persp-save-dir (concat my-cache-dir "workspaces/")
        persp-remove-buffers-from-nil-persp-behaviour nil
        ;; Auto-load on startup
        persp-auto-resume-time (if (daemonp) 3.0 -1)
        ;; Auto-save on kill
        persp-auto-save-opt (if (daemonp) 1 0)
        ;; Don't shorten perspective name
        persp-lighter
        '(:eval
          (format
           (propertize
            " #%s"
            'face (let ((persp (get-current-persp)))
                    (if persp
                        (if (persp-contain-buffer-p (current-buffer) persp)
                            'persp-face-lighter-default
                          'persp-face-lighter-buffer-not-in-persp)
                      'persp-face-lighter-nil-persp)))
           (safe-persp-name (get-current-persp)))))
  :config
  (persp-mode 1))

;;;
;; Buffer filtering

(defun +is-useful-buffer (buffer)
  "Determine if BUFFER is useful."
  (not (string-match
        "^ ?\\*.*\\*\\(<[0-9]+>\\)?$"
        (buffer-name buffer))))

(defun +is-current-persp-buffer (buffer)
  "Determine if BUFFER belongs to current persp."
  (if (fboundp 'persp-buffer-list)
      (memq buffer (persp-buffer-list))
    t))

(defun +is-visible-buffer (buffer)
  "Determine if BUFFER should be visible."
  (and (+is-useful-buffer buffer) (+is-current-persp-buffer buffer)))

(provide 'feature-workspaces)
;;; feature-workspaces.el ends here
