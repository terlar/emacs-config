;;; base.el --- Base configuration

;;; Commentary:
;; Setting sane defaults.

;;; Code:
(defvar my-debug-mode (or (getenv "DEBUG") init-file-debug)
  "Debug mode, enable through DEBUG=1 or use --debug-init.")

(defvar my-emacs-dir user-emacs-directory
  "The path to this .emacs.d directory.")

(defvar my-elisp-dir (concat user-emacs-directory "elisp/")
  "The path to the elisp files.")

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
;; Bootstrap

(let (file-name-handler-alist)
  (require 'cl-lib)
  (eval-and-compile
    (require 'base-packages (concat my-elisp-dir "base-packages")))
  (eval-when-compile
    (my-packages-initialize))

  (require 'base-functions)

  (unless noninteractive
    (require 'base-theme)
    (require 'base-ui)
    (require 'base-popups)
    (require 'base-modeline)
    (require 'base-editor)
    (require 'base-projects)))

;;;
;; X settings
(setq x-gtk-use-system-tooltips nil
      ;; Clipboard
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      ;; Use shared clipboard
      select-enable-clipboard t
      select-enable-primary t)

(provide 'base)
;;; base.el ends here
