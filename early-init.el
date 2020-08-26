;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; URL: https://github.com/terlar/emacs-config
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:
;; Configuration that should be done early or help speed up the loading.

;;; Time the startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Loaded Emacs in %.03fs"
                     (float-time (time-subtract after-init-time before-init-time)))))

;;; Temporarily reduce garbage collection to gain some performance boost.
(let ((normal-gc-cons-threshold gc-cons-threshold)
      (normal-gc-cons-percentage gc-cons-percentage)
      (normal-file-name-handler-alist file-name-handler-alist)
      (init-gc-cons-threshold most-positive-fixnum)
      (init-gc-cons-percentage 0.6))
  (setq gc-cons-threshold init-gc-cons-threshold
        gc-cons-percentage init-gc-cons-percentage
        file-name-handler-alist nil)
  (add-hook 'after-init-hook
            `(lambda ()
               (setq gc-cons-threshold ,normal-gc-cons-threshold
                     gc-cons-percentage ,normal-gc-cons-percentage
                     file-name-handler-alist ',normal-file-name-handler-alist))))

;; Disable GUI components
(setq use-dialog-box nil)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Don't implicitly resize frames when changes various settings.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources
(advice-add #'x-apply-session-resources :override #'ignore)

;; Load `use-package' if possible.
(eval-when-compile
  (require 'use-package nil t))

(provide 'early-init)
;;; early-init.el ends here
