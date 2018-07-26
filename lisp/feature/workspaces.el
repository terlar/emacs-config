;;; workspaces.el --- Workspaces -*- lexical-binding: t; -*-

;;; Commentary:
;; Your very own workspace.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package)
  (require 'base-keybinds))

;;;
;; Functions

;;;###autoload
(defun +eyebrowse-rename-window-config-to-project-name ()
  "Set window configuration name to project name."
  (interactive)
  (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) (projectile-project-name)))

;;;
;; Packages

(req-package eyebrowse
  :demand t
  :general
  ("C-c W ;" '+eyebrowse-rename-window-config-to-project-name)
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-c W"))
  :config
  (eyebrowse-mode 1))

(provide 'feature-workspaces)
;;; workspaces.el ends here
