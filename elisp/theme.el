;;; theme.el --- Theme configuration

;;; Commentary:
;; The look of things.

;;; Code:
(require 'base-vars)

(eval-when-compile
  (defvar hl-todo-keyword-faces)
  (defvar neo-global--window)
  (defvar zoom-window-mode-line-color)
  (declare-function neo-global--select-window "neotree"))

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
(when (or (display-graphic-p) (daemonp))
  (with-demoted-errors "FONT ERROR: %s"
    (set-face-attribute 'default nil :height my-default-font-height :family my-font)

    ;; Fallback to `my-unicode-font' for Unicode characters
    (when my-unicode-font
      (set-fontset-font t 'unicode (font-spec :name my-unicode-font)))
    ;; ... and for variable-pitch mode
    (when my-variable-pitch-font
      (set-face-attribute 'variable-pitch nil :family my-variable-pitch-font))))

;;;
;; Faces
(defface my-code-face
  '((t (:inherit fixed-pitch :background "grey91")))
  "Face for fixed-width text like code snippets."
  :group 'editing)

(defface my-folded-face
  `((((background dark))
     (:inherit font-lock-comment-face :background "black"))
    (((background light))
     (:inherit font-lock-comment-face :background "grey91")))
  "Face to hightlight `hideshow' overlays."
  :group 'editing)

(set-face-attribute 'error nil :foreground "tomato")
(set-face-attribute 'success nil :foreground "sea green")
(set-face-attribute 'warning nil :foreground "dark orange" :weight 'bold)

;; Mode line
(set-face-attribute 'mode-line nil
                    :family my-variable-pitch-font
                    :background "#697D8A"
                    :box '(:line-width 6 :color "#697D8A"))
(set-face-attribute 'mode-line-inactive nil
                    :family my-variable-pitch-font
                    :background "#889BA7"
                    :box '(:line-width 6 :color "#889BA7"))

;; Plugins
(with-eval-after-load "anzu"
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "white"
                      :background my-evil-default-mode-color
                      :box nil))

(with-eval-after-load "company"
  (set-face-attribute 'company-tooltip nil :family my-font)
  (set-face-foreground 'company-tooltip-annotation (face-background 'company-tooltip-selection)))

(with-eval-after-load "ediff"
  (set-face-attribute 'ediff-current-diff-A nil :background "#FEF3F3")
  (set-face-attribute 'ediff-fine-diff-A    nil :background "#B22222")
  (set-face-attribute 'ediff-current-diff-B nil :background "#EBF8EC")
  (set-face-attribute 'ediff-fine-diff-B    nil :background "#00AA13")
  (set-face-attribute 'ediff-current-diff-C nil :background "#EBEFF8")
  (set-face-attribute 'ediff-fine-diff-C    nil :background "#4A8BB3"))

(with-eval-after-load "flycheck"
  (set-face-underline 'flycheck-error '(:style wave :color "tomato"))
  (set-face-underline 'flycheck-warning '(:style wave :color "dark orange")))

(with-eval-after-load "hl-todo"
  (setq hl-todo-keyword-faces
        `(("TODO"  . (:box '(:line-width 1) :foreground ,(face-foreground 'warning)))
          ("FIXME" . (:box '(:line-width 1) :foreground ,(face-foreground 'error)))
          ("NOTE"  . (:box '(:line-width 1))))))

(with-eval-after-load "indent-guide"
  (set-face-foreground 'indent-guide-face (face-foreground 'vertical-border)))

(with-eval-after-load "inline-docs"
  (set-face-attribute 'inline-docs-border-face nil :inherit 'fringe)
  (set-face-attribute 'inline-docs-indicator-face nil :inherit 'fringe))

(with-eval-after-load "markdown-mode"
  (set-face-attribute 'markdown-inline-code-face nil :box '(:line-width 1)))

(with-eval-after-load "nav-flash"
  (set-face-attribute 'nav-flash-face nil :background "pale goldenrod"))

(defun my|neotree-no-fringes ()
  "Remove fringes in neotree buffer.
They get reset each time you select the neotree pane and are highlighted incorrectly."
  (set-window-fringes neo-global--window 1 0))

(with-eval-after-load "neotree"
  (advice-add #'neo-global--select-window :after #'my|neotree-no-fringes)

  (set-face-attribute 'neo-root-dir-face  nil :family my-variable-pitch-font)
  (set-face-attribute 'neo-dir-link-face  nil :family my-variable-pitch-font)
  (set-face-attribute 'neo-file-link-face nil :family my-variable-pitch-font)

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

(with-eval-after-load "nlinum"
  (set-face-attribute 'linum nil
                      :inherit 'default
                      :family my-variable-pitch-font)
  (set-face-attribute 'nlinum-current-line nil
                      :foreground "tomato"
                      :weight 'bold))

(with-eval-after-load "rst"
  (set-face-attribute 'rst-literal nil :inherit 'my-code-face))

(with-eval-after-load "shm"
  (require 'hl-line)
  (set-face-background 'shm-current-face (face-attribute 'hl-line :background))
  (set-face-background 'shm-quarantine-face "#ffdddd"))

(with-eval-after-load "web-mode"
  (set-face-attribute 'web-mode-current-element-highlight-face nil
                      :background nil
                      :weight 'bold))

(with-eval-after-load "zoom-window"
  (setq zoom-window-mode-line-color (face-background 'mode-line)))

(provide 'theme)
;;; theme.el ends here
