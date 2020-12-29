;;; early-init.el --- Early Emacs Initialization -*- lexical-binding: t; -*-

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

;; Quiet startup
(setq inhibit-default-init t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Don't implicitly resize frames when changes various settings.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources
(advice-add #'x-apply-session-resources :override #'ignore)
