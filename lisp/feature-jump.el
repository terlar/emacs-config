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

(use-package dumb-jump
  :commands
  (dumb-jump-go
   dumb-jump-quick-look
   dumb-jump-back)
  :init
  (setq dumb-jump-default-project user-emacs-directory
        dumb-jump-aggressive nil
        dumb-jump-selector 'ivy))

(use-package smart-jump
  :commands
  (smart-jump-register
   smart-jump-go
   smart-jump-back
   smart-jump-references
   smart-jump-simple-find-references)
  :init
  (setq smart-jump-simple-find-references-function
        #'+smart-jump-find-references-with-counsel-rg))

(use-package gxref
  :commands gxref-xref-backend
  :init
  (with-eval-after-load "xref"
    (cl-pushnew 'gxref-xref-backend xref-backend-functions)))

;;;
;; Autoloads

;;;###autoload
(defun +smart-jump-find-references-with-counsel-rg ()
  "Use `rg' and `counsel' to find references."
  (interactive)
  (if (fboundp 'counsel-rg)
      (counsel-rg
       (cond ((use-region-p)
              (buffer-substring-no-properties (region-beginning)
                                              (region-end)))
             ((symbol-at-point)
              (substring-no-properties
               (symbol-name (symbol-at-point))))))
    (message "Install swiper to use `+smart-jump-simple-find-references-with-counsel-rg'.")))

(provide 'feature-jump)
;;; feature-jump.el ends here
