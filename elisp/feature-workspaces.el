;;; feature-workspaces.el --- Workspaces

;;; Commentary:
;; Your very own workspace.

;;; Code:
;; base.el vars
(defvar my-cache-dir nil)

;;;
;; Packages
(use-package persp-mode :demand t
  :commands (persp-mode persp-switch)
  :init
  (setq-default persp-keymap-prefix (kbd "C-c W"))
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-nil-name "main"
        persp-nil-hidden t
        persp-auto-save-fname "autosave"
        persp-save-dir (concat my-cache-dir "workspaces/")
        persp-remove-buffers-from-nil-persp-behaviour nil
        ;; Auto-load on startup
        persp-auto-resume-time (if (daemonp) 3.0 -1)
        ;; Auto-save on kill
        persp-auto-save-opt (if (daemonp) 1 0))

  ;; Don't shorten perspective name
  (setq persp-lighter
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

  (add-hook 'after-init-hook
            #'(lambda ()
                (if (or (display-graphic-p) (daemonp))
                    (persp-mode +1)
                  (add-hook 'after-make-frame-functions #'persp-mode)))))

(provide 'feature-workspaces)
;;; feature-workspaces.el ends here
