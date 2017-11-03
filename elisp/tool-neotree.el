;;; tool-neotree.el --- Tree navigation -*- lexical-binding: t; -*-

;;; Commentary:
;; Show your files in a tree view.

;;; Code:

(eval-when-compile
  (autoload 'projectile-project-root "projectile"))

;;;
;; Packages

(use-package neotree
  :commands (neotree-show
             neotree-hide
             neotree-toggle
             neotree-dir
             neotree-find
             neo-global--select-window
             neo-global--with-buffer
             neo-global--window-exists-p)
  :functions (off-p)
  :preface
  (eval-when-compile
    (defvar winner-boring-buffers))
  :config
  (setq
   neo-create-file-auto-open nil
   neo-auto-indent-point nil
   neo-autorefresh nil
   neo-mode-line-type 'none
   ;; Allow temporary resizing of drawer
   neo-window-fixed-size nil
   ;; Always fallback to a fixed size
   neo-window-width 30
   neo-show-updir-line nil
   neo-theme (if (display-graphic-p) 'icons 'arrow)
   neo-banner-message nil
   neo-confirm-create-file #'off-p
   neo-confirm-create-directory #'off-p
   neo-show-hidden-files nil
   neo-hidden-regexp-list
   '(;; vcs folders
     "^\\.\\(git\\|hg\\|svn\\)$"
     ;; compiled files
     "\\.\\(pyc\\|o\\|elc\\|lock\\|css.map\\)$"
     ;; generated files, caches or local pkgs
     "^\\(node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
     ;; org-mode folders
     "^\\.\\(sync\\|export\\|attach\\)$"
     "~$"
     "^#.*#$"))

  (when (bound-and-true-p winner-mode)
    (push neo-buffer-name winner-boring-buffers)))

;;;
;; Autoloads

;;;###autoload
(defun neotree|toggle ()
  "Toggle NeoTree window."
  (interactive)
  (let ((project-dir (ignore-errors (projectile-project-root)))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

;;;###autoload
(defun neotree|window ()
  "Switch to and/or open NeoTree window."
  (interactive)
  (if (neo-global--window-exists-p)
      (neo-global--select-window)
    (neotree|toggle)))

(provide 'tool-neotree)
;;; tool-neotree.el ends here
