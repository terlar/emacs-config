;;; feature-jump.el --- Jump to definition -*- lexical-binding: t; -*-

;;; Commentary:
;; Go to the source.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

;; Recenter after certain jumps
(add-hooks-pair '(imenu-after-jump
                  evil-jumps-post-jump
                  counsel-grep-post-action
                  dumb-jump-after-jump)
                'recenter)

;;;
;; Packages

(req-package dumb-jump
  :commands
  (dumb-jump-go
   dumb-jump-quick-look
   dumb-jump-back)
  :init
  (setq dumb-jump-default-project my-emacs-dir
        dumb-jump-aggressive nil
        dumb-jump-selector 'ivy))

(req-package gxref
  :after xref
  :commands
  (gxref-xref-backend
   gxref-create-db
   gxref-update-db
   gxref-single-update-db
   gxref-set-project-dir)
  :config
  (cl-pushnew 'gxref-xref-backend xref-backend-functions))

(provide 'feature-jump)
;;; feature-jump.el ends here
