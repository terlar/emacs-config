;;; base-package.el --- Package configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Sharing is caring.

;;; Code:

(eval-when-compile
  (require 'base-vars)

  (require 'package)
  (require 'tls))

(defvar my-packages-init-p nil
  "Non-nil if the package system has been initialized.
This will be nil if you have byte-compiled your configuration.")

(defvar my-el-get-dir (expand-file-name "el-get" my-packages-dir))

;;;
;; Settings

(setq-default
 load-prefer-newer noninteractive
 package--init-file-ensured t
 package-enable-at-startup nil
 package-user-dir (expand-file-name "elpa" my-packages-dir)
 package-gnupghome-dir (expand-file-name "gnupg" package-user-dir)
 package-archives
 '(("gnu"   . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/"))

 gnutls-verify-error t
 tls-checktrust gnutls-verify-error

 use-package-always-defer t
 use-package-always-ensure t
 use-package-debug nil
 use-package-expand-minimally (eval-when-compile (not my-debug-mode))
 use-package-minimum-reported-time (if my-debug-mode 0 0.1)
 use-package-verbose my-debug-mode

 byte-compile-dynamic nil
 byte-compile-verbose my-debug-mode
 byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; Prevent packages from being saved to custom file.
(defun package--save-selected-packages (&optional value)
  "Set and (don't!) save `package-selected-packages' to VALUE."
  (when value
    (setq package-selected-packages value))
  (unless after-init-time
    (add-hook 'after-init-hook #'package--save-selected-packages)))

(defun +packages-initialize (&optional force-p)
  "Initialize installed packages and ensure they are installed.
When FORCE-P is provided it will run no matter the preconditions.
When base.el is compiled, this function will be avoided to speed up startup."
  (when (or (not my-packages-init-p) force-p)
    (unless noninteractive
      (message "Emacs initialized"))

    (setq package-activated-list nil)

    ;; Ensure folders exist
    (dolist (dir (list my-cache-dir my-data-dir my-packages-dir package-user-dir my-el-get-dir))
      (unless (file-directory-p dir)
        (make-directory dir t)))

    (package-initialize t)
    ;; Setup load paths
    (dolist (dir (list package-user-dir my-el-get-dir))
      (setq load-path (append load-path (directory-files dir t "^[^.]" t)))
      (setq custom-theme-load-path (append custom-theme-load-path (directory-files dir t "theme" t))))

    (unless package-archive-contents
      (package-refresh-contents))

    (unless (package-installed-p 'use-package)
      (package-install 'use-package))
    (load "use-package" nil t)

    (use-package req-package
      :demand t
      :init
      (if (and (not noninteractive) my-debug-mode)
          (setq req-package-log-level 'debug)))
    (use-package el-get
      :demand t
      :init
      (setq el-get-dir my-el-get-dir
            el-get-status-file (expand-file-name ".status.el" el-get-dir)
            el-get-autoload-file (expand-file-name ".loaddefs.el" el-get-dir))
      :config
      (add-to-list 'el-get-recipe-path (expand-file-name "recipes" user-emacs-directory)))

    (setq my-packages-init-p t)))

(provide 'base-package)
;;; base-package.el ends here
