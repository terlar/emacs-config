;;; init-paths.el --- Paths init file -*- lexical-binding: t; -*-

;;; Commentary:
;;; Initializing all the paths. Ensures Emacs doesn't store stuff in unwanted places.

;;; Code:

;; Variables
(eval-and-compile
  (defvar +org-config-path
    (expand-file-name "config.org" user-emacs-directory)
    "Path to org config file."))

(eval-when-compile
  (defvar desktop-dirname)
  (defvar package-user-dir))

(setq server-auth-dir  (expand-file-name "server/" +cache-dir))

;; Initialize load path used by packages
(eval-and-compile
  (setq load-path
        (append load-path
	        (directory-files +packages-dir t "^[^.]" t)))
  (push (expand-file-name "lisp" user-emacs-directory) load-path)
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path)

  (setq custom-theme-load-path
        (append custom-theme-load-path
	        (directory-files +packages-dir t "theme" t))))

(provide 'init-paths)
;;; init-paths.el ends here
