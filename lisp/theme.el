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
  :init (setq tao-theme-use-height t))

;; Override grayscale with sepiascale
(defun tao-theme-scale-to-colors (scale)
  "Create sepiascale from colors alist SCALE."
  (mapcar (lambda (it)
            (let* ((depth 20)
                   (saturation 1.03)
                   (r (+ it (* depth 2)))
                   (g (+ it depth))
                   (b (* it saturation)))
              (format "#%02X%02X%02X"
                      (if (> r 255) 255 r)
                      (if (> g 255) 255 g)
                      (if (> b 255) 255 b)))) scale))

;; (req-package punpun-theme)
;; (req-package eziam-theme)

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
