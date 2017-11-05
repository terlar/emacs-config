;;; theme.el --- Theme configuration

;;; Commentary:
;; The look of things.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (defvar neo-global--window)
  (declare-function neo-global--select-window "neotree"))

;;;
;; Faces

(defface my-code-face
  `((t (:inherit fixed-pitch)))
  "Face for fixed-width text like code snippets."
  :group 'editing)

(defface my-folded-face
  `((t (:inherit font-lock-comment-face)))
  "Face to hightlight `hideshow' overlays."
  :group 'editing)

;;;
;; Theme

(use-package tao-theme :demand t
  :commands tao-with-color-variables
  :config
  (setq tao-theme-use-height t))

(use-package punpun-theme)
(use-package eziam-theme)
(use-package flatui-theme)
(use-package leuven-theme)
(use-package color-theme-sanityinc-tomorrow)
(use-package twilight-bright-theme)

(load-theme my-theme t)
(load-theme 'local t)

;;;
;; Typography

(when (or (display-graphic-p) (daemonp))
  (with-demoted-errors "FONT ERROR: %s"
    (set-face-attribute 'default nil :height my-default-font-height :family my-font)
    (when my-variable-pitch-font
      (set-face-attribute 'variable-pitch nil :family my-variable-pitch-font))))

;;;
;; NeoTree

(defun my|neotree-no-fringes ()
  "Remove fringes in neotree buffer.
They get reset each time you select the neotree pane and are highlighted incorrectly."
  (set-window-fringes neo-global--window 1 0))

(with-eval-after-load "neotree"
  (advice-add #'neo-global--select-window :after #'my|neotree-no-fringes)
  (add-hook 'neotree-mode-hook
            #'(lambda ()
                ;; Setup spacing
                (setq line-spacing 2
                      tab-width 1)

                ;; Hide cursor and highlight full line instead
                (hl-line-mode +1)
                (setq cursor-type nil)
                (with-eval-after-load "evil"
                  (defadvice evil-refresh-cursor (around evil activate)
                    (unless (eq major-mode 'neotree-mode) ad-do-it))))))

(provide 'theme)
;;; theme.el ends here
