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
(defvar my-el-get-recipes (expand-file-name "recipes" user-emacs-directory))

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
   ("org"   . "https://orgmode.org/elpa/")
   ("melpa" . "https://melpa.org/packages/"))

 gnutls-verify-error t
 tls-checktrust gnutls-verify-error

 ;; use-package
 use-package-always-defer t
 use-package-always-ensure t
 use-package-debug nil
 use-package-expand-minimally (eval-when-compile (not my-debug-mode))
 use-package-minimum-reported-time (if my-debug-mode 0 0.1)
 use-package-verbose my-debug-mode

 ;; el-get
 el-get-dir my-el-get-dir
 el-get-status-file (expand-file-name ".status.el" my-el-get-dir)
 el-get-autoload-file (expand-file-name ".loaddefs.el" my-el-get-dir)
 el-get-recipe-path `(,my-el-get-recipes)

 req-package-log-level (if (and (not noninteractive) my-debug-mode)
                           'debug
                         'info)

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

;;;
;; Macros

(autoload 'use-package "use-package" nil nil 'macro)
(autoload 'req-package "req-package" nil nil 'macro)

;;;
;; Functions

(defun +packages-initialize-load-path ()
  "Initialize load path used by packages."
  (dolist (dir (list package-user-dir my-el-get-dir))
    (setq load-path (append load-path (directory-files dir t "^[^.]" t))
          custom-theme-load-path (append custom-theme-load-path (directory-files dir t "theme" t)))))

(defun +packages-initialize (&optional force-p)
  "Initialize installed packages and ensure they are installed.
When FORCE-P is provided it will run no matter the preconditions.
When base.el is compiled, this function will be avoided to speed up startup."
  (when (or (not my-packages-init-p) force-p)
    (setq package-activated-list nil)

    ;; Ensure folders exist
    (dolist (dir (list my-cache-dir my-data-dir my-packages-dir package-user-dir my-el-get-dir))
      (unless (file-directory-p dir)
        (make-directory dir t)))

    (package-initialize t)
    (+packages-initialize-load-path)
    (unless package-archive-contents
      (package-refresh-contents))

    (dolist (package '(el-get use-package req-package))
      (unless (package-installed-p package)
        (package-install package))
      (load (symbol-name package) nil t))

    (setq my-packages-init-p t)))

(provide 'base-package)
;;; base-package.el ends here
