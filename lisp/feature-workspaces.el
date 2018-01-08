;;; feature-workspaces.el --- Workspaces -*- lexical-binding: t; -*-

;;; Commentary:
;; Your very own workspace.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

;;;
;; Packages

(req-package persp-mode
  :demand t
  :init
  (setq persp-keymap-prefix (kbd "C-c W")
        persp-auto-save-fname "autosave"
        persp-save-dir (concat my-cache-dir "workspaces/")
        persp-nil-hidden t
        persp-remove-buffers-from-nil-persp-behaviour nil
        persp-autokill-buffer-on-remove 'kill-weak
        ;; Auto-load on startup
        persp-auto-resume-time (if (daemonp) 2.0 0)
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

;; Auto-create project perspectives
(req-package persp-mode-projectile-bridge
  :preface
  (defun +persp-mode-projectile-bridge-setup ()
    (if persp-mode-projectile-bridge-mode
        (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
      (persp-mode-projectile-bridge-kill-perspectives)))
  :hook
  (after-init . persp-mode-projectile-bridge-mode)
  (persp-mode . persp-mode-projectile-bridge-mode)
  (persp-mode-projectile-bridge-mode . +persp-mode-projectile-bridge-setup))

(req-package perspeen
  :disabled t
  :init
  (setq perspeen-keymap-prefix (kbd "C-c TAB")
        perspeen-use-tab nil)
  :config
  (perspeen-mode 1))

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
