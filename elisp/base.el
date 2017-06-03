;;; base.el --- Base configuration

;;; Commentary:
;; Setting sane defaults.

;;; Code:
(defvar my-debug-mode (or (getenv "DEBUG") init-file-debug)
  "Debug mode, enable through DEBUG=1 or use --debug-init.")

(defvar my-emacs-dir user-emacs-directory
  "The path to this .emacs.d directory.")

(defvar my-cache-dir
  (if (getenv "XDG_CACHE_HOME")
      (concat (getenv "XDG_CACHE_HOME") "/emacs/")
    (expand-file-name "~/.cache/emacs/"))
  "Use XDG-based cache directory.")

(defvar my-data-dir
  (if (getenv "XDG_DATA_HOME")
      (concat (getenv "XDG_DATA_HOME") "/emacs/")
    (expand-file-name "~/.local/share/emacs/"))
  "Use XDG-based data directory.")

(defvar my-packages-dir (concat my-data-dir "packages/")
  "Use XDG-based packages directory.")

(setq package-user-dir (expand-file-name "elpa" my-packages-dir))
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

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
 idle-update-delay 2            ; update UI less often
 ;; Keep the point out of the minibuffer
 minibuffer-prompt-properties
 '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
 ;; History & backup settings (save nothing)
 auto-save-default nil
 create-lockfiles nil
 history-delete-duplicates t
 history-length 1000
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
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;;;
;; Packages
(require 'use-package)
(setq
 load-prefer-newer noninteractive
 package--init-file-ensured t
 package-enable-at-startup nil

 use-package-always-defer t
 use-package-always-ensure t
 use-package-expand-minimally (not my-debug-mode)
 use-package-debug nil
 use-package-verbose my-debug-mode
 use-package-minimum-reported-time (if my-debug-mode 0 0.1)

 byte-compile-dynamic nil
 byte-compile-verbose my-debug-mode
 byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;;;
;; OS

;; Clipboard
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      ;; Use shared clipboard
      select-enable-clipboard t
      select-enable-primary t)

(setq x-gtk-use-system-tooltips nil)

;;;
;; Setup
(eval-when-compile
  (require 'cl-lib))

(provide 'base)
;;; base.el ends here
