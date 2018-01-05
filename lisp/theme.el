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
  :init
  (setq tao-theme-use-height t))

(req-package punpun-theme :force t)
(req-package eziam-theme :force t)
(req-package flatui-theme :force t)
(req-package leuven-theme :force t)
(req-package color-theme-sanityinc-tomorrow :force t)
(req-package twilight-bright-theme :force t)

(load-theme my-theme t)
(load-theme 'local t)

;;;
;; Typography

(when (or (display-graphic-p) (daemonp))
  (with-demoted-errors "FONT ERROR: %s"
    (set-face-attribute 'default nil :height my-default-font-height :family my-font)
    (when my-variable-pitch-font
      (set-face-attribute 'variable-pitch nil :family my-variable-pitch-font :height 1.2))))

(provide 'theme)
;;; theme.el ends here
