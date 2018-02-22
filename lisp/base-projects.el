;;; base-projects.el --- Project configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Project management.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

;;;
;; Packages

(req-package projectile
  :demand t
  :diminish projectile-mode
  :hook (find-file . +projectile-relative-buf-name)
  :preface
  (defun +projectile-relative-buf-name ()
    (let ((buffer-name (if (projectile-project-p)
                           (file-relative-name buffer-file-name (projectile-project-root))
                         (abbreviate-file-name buffer-file-name))))
      (rename-buffer buffer-name)))
  :init
  (setq projectile-cache-file (concat my-cache-dir "projectile.cache")
        projectile-enable-caching nil
        projectile-file-exists-remote-cache-expire nil
        projectile-globally-ignored-file-suffixes
        '(".elc" ".pyc" ".o" ".hi" ".class" ".cache")
        projectile-globally-ignored-files
        '("TAGS" "GPATH" "GRTAGS" "GTAGS")
        projectile-indexing-method 'alien
        projectile-ignored-projects (list my-data-dir)
        projectile-known-projects-file (concat my-data-dir "projectile.projects"))
  :config
  (defun +projectile-cache-current-file (orig-fun &rest args)
    "Don't cache ignored files."
    (unless (cl-some (lambda (path)
                       (string-prefix-p buffer-file-name
                                        (expand-file-name path)))
                     (projectile-ignored-directories))
      (apply orig-fun args)))
  (advice-add #'projectile-cache-current-file :around #'+projectile-cache-current-file)

  (setq projectile-globally-ignored-directories
        (append '("_build" "elm-stuff" "tests/elm-stuff")
                projectile-globally-ignored-directories))

  (setq projectile-project-root-files
        (append '("package.json" "Package.swift" "README.md")
                projectile-project-root-files))
  (setq projectile-other-file-alist
        (append '(("less" "css")
                  ("styl" "css")
                  ("sass" "css")
                  ("scss" "css")
                  ("css" "scss" "sass" "less" "styl")
                  ("jade" "html")
                  ("pug" "html")
                  ("html" "jade" "pug" "jsx" "tsx"))
                projectile-other-file-alist))

  (projectile-mode 1))

(provide 'base-projects)
;;; base-projects.el ends here
