;;; init.el --- Main init file -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:
;;; The init file that loads all the components.

;;; Code:

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
  ;; Show tool-tips in echo-area
  (tooltip-mode 0))

;; Quiet startup
(advice-add #'display-startup-echo-area-message :override #'ignore)
(setq inhibit-default-init t
      inhibit-startup-screen t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(eval-and-compile
  (push (expand-file-name "lisp" user-emacs-directory) load-path)
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path))

(defun load-directory (dir)
  "Load Elisp files in DIR."
  (dolist (file (directory-files dir nil (concat "[[:alnum:]-]*\\.el$")))
    (load-file (expand-file-name file (file-name-as-directory dir)))))

;;;
;; Base

;; Calls (package-initialize)
(require 'base)
(dolist (type '(feature completion tool lang))
  (load-directory (expand-file-name (concat "lisp" "/" (symbol-name type)) user-emacs-directory)))

(unless noninteractive
  (require 'bindings)
  (require 'commands)
  (require 'theme))

(req-package-finish)

;;; init.el ends here
