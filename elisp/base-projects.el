;;; base-projects.el --- Project configuration

;;; Commentary:
;; Project management.

;;; Code:
(require 'base-vars)

(use-package projectile :demand t
  :diminish projectile-mode
  :functions projectile-ignored-directories
  :preface
  (defun my|projectile-cache-current-file (orig-fun &rest args)
    "Don't cache ignored files."
    (unless (cl-some (lambda (path)
                       (string-prefix-p buffer-file-name
                                        (expand-file-name path)))
                     (projectile-ignored-directories))
      (apply orig-fun args)))
  :init
  (setq projectile-cache-file (concat my-cache-dir "projectile.cache")
        projectile-enable-caching nil
        projectile-file-exists-remote-cache-expire nil
        projectile-globally-ignored-file-suffixes
        '(".elc" ".pyc" ".o" ".hi" ".class" ".cache")
        projectile-globally-ignored-files
        '("TAGS" "GPATH" "GRTAGS" "GTAGS")
        projectile-indexing-method 'alien
        projectile-ignored-projects `(,my-data-dir)
        projectile-known-projects-file (concat my-cache-dir "projectile.projects"))
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

  (advice-add #'projectile-cache-current-file :around #'my|projectile-cache-current-file))

;;;
;; Buffer filtering
(defun is-useful-buffer (buffer)
  "Determine if BUFFER is useful."
  (not (string-match
        "^ ?\\*.*\\*\\(<[0-9]+>\\)?$"
        (buffer-name buffer))))

(defun is-current-persp-buffer (buffer)
  "Determine if BUFFER belongs to current persp."
  (if (fboundp 'persp-buffer-list)
      (memq buffer (persp-buffer-list))
    t))

(defun is-visible-buffer (buffer)
  "Determine if BUFFER should be visible."
  (and (is-useful-buffer buffer) (is-current-persp-buffer buffer)))

;; Filter out buffers that is not deemed visible.
(push '(buffer-predicate . is-visible-buffer) default-frame-alist)

(provide 'base-projects)
;;; base-projects.el ends here
