;;; feature-jump.el --- Jump to definition -*- lexical-binding: t; -*-

;;; Commentary:
;; Go to the source.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package)
  (require 'base-keybinds))

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
  (dumb-jump-quick-look
   dumb-jump-back)
  :general
  ("M-g i" 'dumb-jump-go-prompt
   "M-g o" 'dumb-jump-go-other-window
   "M-g x" 'dumb-jump-go-prefer-external
   "M-g z" 'dumb-jump-go-prefer-external-other-window)
  :init
  (setq dumb-jump-default-project user-emacs-directory
        dumb-jump-aggressive nil
        dumb-jump-selector 'ivy))

(req-package smart-jump
  :commands
  (smart-jump-register
   smart-jump-back
   smart-jump-simple-find-references)
  :general
  ([remap evil-goto-definition] 'smart-jump-go
   "M-g j" 'smart-jump-go
   "M-g r" 'smart-jump-references)
  :init
  (setq smart-jump-find-references-fallback-function
        #'+smart-jump-find-references-with-counsel-rg))

(req-package gxref
  :commands gxref-xref-backend
  :init
  (with-eval-after-load 'xref
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
