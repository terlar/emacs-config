;;; init-paths.el --- Paths init file -*- lexical-binding: t; -*-

;;; Commentary:
;;; Initializing all the paths. Ensures Emacs doesn't store stuff in unwanted places.

;;; Code:

;; Variables
(defvar +cache-dir
  (if (getenv "XDG_CACHE_HOME")
      (concat (getenv "XDG_CACHE_HOME") "/emacs/")
    (expand-file-name "~/.cache/emacs/"))
  "Directory for cache.")

(defvar +data-dir
  (if (getenv "XDG_DATA_HOME")
      (concat (getenv "XDG_DATA_HOME") "/emacs/")
    (expand-file-name "~/.local/share/emacs/"))
  "Directory for data.")

(defvar +packages-dir
  (expand-file-name "packages/" +data-dir)
  "Directory for packages.")

(defvar +site-lisp-dir
  (expand-file-name "site-lisp" user-emacs-directory)
  "Directory for shared files.")

(defvar +el-get-recipes-dir
  (expand-file-name "recipes" user-emacs-directory)
  "Directory for `el-get' recipes.")

(defvar +org-config-path
  (expand-file-name "config.org" user-emacs-directory)
  "Path to org config file.")

;; Ensure folders exist
(dolist (dir (list +cache-dir +data-dir +packages-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

(eval-when-compile
  (defvar desktop-dirname)
  (defvar package-user-dir))

(setq-default
 abbrev-file-name                  (expand-file-name "abbrev_defs" +data-dir)
 auto-save-list-file-name          (expand-file-name "autosave" +cache-dir)
 backup-directory-alist            (list (cons "." (expand-file-name "backup/" +cache-dir)))
 bookmark-default-file             (expand-file-name "bookmarks" +data-dir)
 desktop-dirname                   +data-dir
 desktop-path                      (list desktop-dirname)
 eshell-directory-name             (expand-file-name "eshell/" +data-dir)
 eshell-history-file-name          (expand-file-name "eshell-history" +data-dir)
 package-user-dir                  +packages-dir
 package-gnupghome-dir             (expand-file-name "gnupg" package-user-dir)
 pcache-directory                  (expand-file-name "pcache/" +cache-dir)
 recentf-save-file                 (expand-file-name "recentf" +cache-dir)
 semanticdb-default-save-directory (expand-file-name "semanticdb/" +cache-dir)
 savehist-file                     (expand-file-name "savehist" +cache-dir)
 save-place-file                   (expand-file-name "saveplace" +cache-dir)
 server-auth-dir                   (expand-file-name "server/" +cache-dir)
 shared-game-score-directory       (expand-file-name "shared-game-score/" +data-dir)
 source-directory                  (expand-file-name "emacs" "~/src/git.sv.gnu.org")
 tramp-auto-save-directory         (expand-file-name "tramp-auto-save/" +cache-dir)
 tramp-backup-directory-alist      backup-directory-alist
 tramp-persistency-file-name       (expand-file-name "tramp-persistency.el" +cache-dir)
 url-cache-directory               (expand-file-name "url/" +cache-dir)
 url-configuration-directory       (expand-file-name "url/" +data-dir))

(setq-default
 el-get-dir           +packages-dir
 el-get-status-file   (expand-file-name ".status.el" +packages-dir)
 el-get-autoload-file (expand-file-name ".loaddefs.el" +packages-dir)
 el-get-recipe-path   `(,+el-get-recipes-dir))

(setq-default
 smex-save-file (expand-file-name "smex-items" +cache-dir))

(setq-default
 projectile-cache-file          (expand-file-name "projectile.cache" +cache-dir)
 projectile-known-projects-file (expand-file-name "projectile.projects" +data-dir))

;; Custom defs to a separate file
(setq custom-file (expand-file-name "custom.el" +data-dir))

;; Initialize load path used by packages
(setq load-path
      (append load-path
	      (directory-files +packages-dir t "^[^.]" t))
      custom-theme-load-path
      (append custom-theme-load-path
	      (directory-files +packages-dir t "theme" t)))

(push (expand-file-name "lisp" user-emacs-directory) load-path)
(push (expand-file-name "site-lisp" user-emacs-directory) load-path)

(provide 'init-paths)
;;; init-paths.el ends here
