;;; base-projects.el --- Project configuration
;;; Commentary:
;;; Project management.
;;; Code:
(use-package projectile :demand t
  :init
  (setq projectile-cache-file (concat my-cache-dir "projectile.cache")
        projectile-enable-caching (not noninteractive)
        projectile-file-exists-remote-cache-expire nil
        projectile-globally-ignored-directories `(,my-data-dir ".sync")
        projectile-globally-ignored-file-suffixes
        '(".elc" ".pyc" ".o" ".hi")
        projectile-globally-ignored-files
        '(".DS_Store")
        projectile-indexing-method 'alien
        projectile-known-projects-file (concat my-cache-dir "projectile.projects")
        projectile-require-project-root nil
        projectile-project-root-files
        '(".git" ".hg" ".svn" ".project"
          "Makefile" "build.gradle" "build.sbt" "pom.xml"
          "package.json" "setup.py" "Gemfile" "stack.yaml" "Cargo.toml" "Package.swift"
          "wercker.yml" "docker-compose.yml" "Dockerfile"))
  :config
  (projectile-mode +1)

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

  (defun my/projectile-cache-current-file (orig-fun &rest args)
    "Don't cache ignored files."
    (unless (cl-some (lambda (path)
                       (string-prefix-p buffer-file-name
                                        (expand-file-name path)))
                     (projectile-ignored-directories))
      (apply orig-fun args)))
  (advice-add #'projectile-cache-current-file :around #'my/projectile-cache-current-file))

;;;
;; Projects
(defvar-local my-project nil
  "A list of project mode symbols to enable. Used for .dir-locals.el.")

(defun my|autoload-project-mode ()
  "Auto-enable projects listed in `my-project', which is meant to be set from .dir-locals.el files."
  (dolist (mode my-project)
    (funcall mode)))
(add-hook 'after-change-major-mode-hook #'my|autoload-project-mode)

(provide 'base-projects)
;;; base-projects.el ends here
