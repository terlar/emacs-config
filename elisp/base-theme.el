;;; base-theme.el --- Theme configuration
;;; Commentary:
;;; The look of things.
;;; Code:
(defvar my-theme 'tao-yang
  "The color theme to use.")

(defvar my-font
  (font-spec :family "Input Mono Narrow" :weight 'normal :size 20)
  "The monospace font to use.")

(defvar my-variable-pitch-font
  (font-spec :family "Noto Sans" :weight 'normal :size 22)
  "The regular font to use.")

(defvar my-unicode-font
  (font-spec :family "Noto Mono" :weight 'normal :size 20)
  "Fallback font for unicode glyphs.")

(defun my|on-frame-open (&optional frame)
  "If the FRAME created in terminal don't load background color."
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))
(add-hook 'after-make-frame-functions #'my|on-frame-open)

;;;
;; Theme
(use-package tao-theme)
(use-package punpun-theme)
(use-package eziam-theme)
(use-package flatui-theme)
(use-package leuven-theme)
(use-package color-theme-sanityinc-tomorrow)
(use-package twilight-bright-theme)

(load-theme my-theme t)

;;;
;; Typography
(when (display-graphic-p)
  (with-demoted-errors "FONT ERROR: %s"
    (set-frame-font my-font t t)
    ;; Fallback to `my-unicode-font' for Unicode characters
    (when my-unicode-font
      (set-fontset-font t 'unicode my-unicode-font))
    ;; ...and for variable-pitch mode
    (when my-variable-pitch-font
      (set-face-attribute 'variable-pitch nil
                          :font my-variable-pitch-font))))

(provide 'base-theme)
;;; base-theme.el ends here
