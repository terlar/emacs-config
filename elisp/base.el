;;; base.el --- Base configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Setting sane defaults.

;;; Code:

(require 'base-vars)

(defvar my-init-time nil
  "The time it took, in seconds, for Emacs to initialize.")

;;;
;; Settings

;; UTF-8 as the default coding system
(set-charset-priority 'unicode)
(prefer-coding-system        'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(setq-default
 ad-redefinition-action 'accept ; Silence advised function warnings
 apropos-do-all t               ; Make `apropos' search for more stuff
 compilation-always-kill t      ; Kill compilation process before starting another
 compilation-ask-about-save nil ; Save all buffers on `compile'
 compilation-scroll-output t
 confirm-nonexistent-file-or-buffer t
 enable-recursive-minibuffers nil
 debug-on-error (and (not noninteractive) my-debug-mode)
 ;; Update UI less often
 idle-update-delay 2
 ;; Keep the point out of the minibuffer
 minibuffer-prompt-properties
 '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
 ;; History & backup settings (save nothing)
 auto-save-default nil
 create-lockfiles nil
 history-delete-duplicates t
 history-length 500
 make-backup-files nil
 ;; Files
 abbrev-file-name                  (concat my-data-dir "abbrev.el")
 auto-save-list-file-name          (concat my-cache-dir "autosave")
 backup-directory-alist            (list (cons "." (concat my-cache-dir "backup/")))
 pcache-directory                  (concat my-cache-dir "pcache/")
 semanticdb-default-save-directory (concat my-cache-dir "semanticdb/")
 server-auth-dir                   (concat my-cache-dir "server/")
 shared-game-score-directory       (concat my-data-dir "shared-game-score/")
 tramp-auto-save-directory         (concat my-cache-dir "tramp-auto-save/")
 tramp-backup-directory-alist      backup-directory-alist
 tramp-persistency-file-name       (concat my-cache-dir "tramp-persistency.el")
 url-cache-directory               (concat my-cache-dir "url/")
 url-configuration-directory       (concat my-data-dir "url/"))

;; Move custom defs out of init.el
(setq custom-file (concat my-data-dir "custom.el"))
(load custom-file t t)

;; Quiet startup
(advice-add #'display-startup-echo-area-message :override #'ignore)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; X
(setq x-gtk-use-system-tooltips nil
      ;; Clipboard
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      ;; Use shared clipboard
      select-enable-clipboard t
      select-enable-primary t)

;;;
;; Initialize

(eval-and-compile
  ;; Temporarily reduce garbage collection during startup
  (let ((normal-gc-cons-threshold 800000)
        (normal-gc-cons-percentage 0.1)
        (normal-file-name-handler-alist file-name-handler-alist)
        (init-gc-cons-threshold 402653184)
        (init-gc-cons-percentage 0.6))
    (setq gc-cons-threshold init-gc-cons-threshold
          gc-cons-percentage init-gc-cons-percentage
          file-name-handler-alist nil)
    (add-hook 'after-init-hook
              #'(lambda ()
                  (setq gc-cons-threshold normal-gc-cons-threshold
                        gc-cons-percentage normal-gc-cons-percentage
                        file-name-handler-alist normal-file-name-handler-alist))))

  (require 'cl-lib)
  (require 'base-package)

  (eval-when-compile
    (my|packages-initialize))
  (setq load-path (eval-when-compile load-path))

  (require 'base-lib))

;; Startup time
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (message "Loaded Emacs in %.03fs"
                       (float-time (time-subtract after-init-time before-init-time)))))

;;;
;; Bootstrap

(unless noninteractive
  (require 'base-ui)
  (require 'base-modeline)
  (require 'base-popups)
  (require 'base-keybinds)
  (require 'base-editor)
  (require 'base-projects))

(provide 'base)
;;; base.el ends here
