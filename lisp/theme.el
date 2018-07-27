;;; theme.el --- Theme configuration

;;; Commentary:
;; The look of things.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

;;;
;; Theme

(req-package tao-theme :force t :demand t
  :el-get t :ensure nil
  :init (setq tao-theme-use-height t
              tao-theme-use-sepia t))

(load-theme my-theme t)
(load-theme 'local t)

;;;
;; Typography

(when (or (display-graphic-p) (daemonp))
  (with-demoted-errors "FONT ERROR: %s"
    (set-face-attribute 'default nil :height my-default-font-height :family my-font)
    (set-face-attribute 'fixed-pitch nil :family my-font)
    (when my-variable-pitch-font
      (set-face-attribute 'variable-pitch nil :family my-variable-pitch-font))))

(provide 'theme)
;;; theme.el ends here
