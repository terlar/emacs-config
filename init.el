;;; init.el --- Main init file -*- lexical-binding: t; -*-

;;; Commentary:
;;; The main init file that starts the whole shebang.

;;; Code:

(eval-and-compile
  (defvar +debug-mode
    (or (getenv "DEBUG") init-file-debug)
    "Debug mode, enable through DEBUG=1 or use --debug-init.")
  (setq debug-on-error (and (not noninteractive) +debug-mode)))

;; Temporarily reduce garbage collection during startup.
(eval-and-compile
  (let ((normal-gc-cons-threshold 800000)
        (normal-gc-cons-percentage 0.1)
        (normal-file-name-handler-alist file-name-handler-alist)
        (init-gc-cons-threshold 402653184)
        (init-gc-cons-percentage 0.6))
    (setq gc-cons-threshold init-gc-cons-threshold
          gc-cons-percentage init-gc-cons-percentage
          file-name-handler-alist nil)
    (add-hook 'after-init-hook
              (lambda ()
                (setq gc-cons-threshold normal-gc-cons-threshold
                      gc-cons-percentage normal-gc-cons-percentage
                      file-name-handler-alist normal-file-name-handler-alist)))))

;; Report startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Loaded Emacs in %.03fs"
                     (float-time (time-subtract after-init-time before-init-time)))))

;; Disable GUI components early
(when window-system
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (tooltip-mode 0)
  (setq use-dialog-box nil))

;; Frame look
(setq default-frame-alist
      '((fullscreen . maximized)
	(internal-border-width . 12)
	(vertical-scroll-bars . nil)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)))

;; Quiet startup
(setq inhibit-default-init t
      inhibit-startup-screen t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Initialize paths
(require 'init-paths (expand-file-name "init-paths" user-emacs-directory))

;; Package setup
(eval-and-compile
  (require 'package)
  (require 'tls)
  (package-initialize t))

(setq gnutls-verify-error t
      tls-checktrust t
      package-enable-at-startup nil
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
	("org"   . "https://orgmode.org/elpa/")
	("melpa" . "https://melpa.org/packages/")))

(eval-when-compile
  (dolist (package '(el-get use-package use-package-el-get))
    (unless (package-installed-p package)
      (package-refresh-contents)
      (package-install package))))

(require 'use-package)
(setq use-package-always-defer t
      use-package-expand-minimally (eval-when-compile (not +debug-mode))
      use-package-minimum-reported-time (if +debug-mode 0 0.1)
      use-package-verbose +debug-mode)
(require 'use-package-el-get)
(push :el-get use-package-keywords)

;; Libraries
(require 'cl-lib)

;; Load org based configuration
(when (file-exists-p +org-config-path)
  (org-babel-load-file +org-config-path))

(provide 'init)
;;; init.el ends here
