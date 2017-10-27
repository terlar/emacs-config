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
(use-package tao-theme :demand t
  :config
  (setq tao-theme-use-height t))
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
    (when my-variable-pitch-font
      (set-face-attribute 'variable-pitch nil :family my-variable-pitch-font))))

;;;
;; Colors
(defvar theme-color-mode-line "#697D8A"
  "Color used for active mode line.")
(defvar theme-color-mode-line-inactive "#889BA7"
  "Color used for inactive mode line.")

(defvar theme-color-error "tomato"
  "Color used to indicate error.")
(defvar theme-color-success "sea green"
  "Color used to indicate success.")
(defvar theme-color-warning "dark orange"
  "Color used to indicate warning.")

(defvar theme-color-highlight "black"
  "Color used to highlight elements.")
(defvar theme-color-lighter "grey91"
  "Color used for less visible elements.")

;;;
;; Faces
(defface my-code-face
  `((t (:inherit fixed-pitch :background ,theme-color-lighter)))
  "Face for fixed-width text like code snippets."
  :group 'editing)

(defface my-folded-face
  `((((background dark))
     (:inherit font-lock-comment-face :background "black"))
    (((background light))
     (:inherit font-lock-comment-face :background ,theme-color-lighter)))
  "Face to hightlight `hideshow' overlays."
  :group 'editing)

;; Messages
(set-face-attribute 'error nil :foreground theme-color-error)
(set-face-attribute 'success nil :foreground theme-color-success)
(set-face-attribute 'warning nil :foreground theme-color-warning :weight 'bold)

;; Line numbers
(set-face-attribute 'line-number-current-line nil
                    :foreground theme-color-highlight
                    :weight 'bold)

;; Mode line
(set-face-attribute 'mode-line nil
                    :family my-variable-pitch-font
                    :background theme-color-mode-line
                    :box `(:line-width 6 :color ,theme-color-mode-line))
(set-face-attribute 'mode-line-inactive nil
                    :family my-variable-pitch-font
                    :background theme-color-mode-line-inactive
                    :box `(:line-width 6 :color ,theme-color-mode-line-inactive))

;; Plugins
(with-eval-after-load "anzu"
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "white"
                      :background my-evil-default-mode-color
                      :box nil))

(with-eval-after-load "company"
  (set-face-attribute 'company-tooltip nil :family my-font)
  (set-face-foreground 'company-tooltip-annotation (face-background 'company-tooltip-selection)))

(with-eval-after-load "coverlay"
  (setq-default coverlay:untested-line-background-color "#FEF3F3"
                coverlay:tested-line-background-color "#EBF8EC"))

(with-eval-after-load "ediff"
  (set-face-attribute 'ediff-current-diff-A nil :background "#FEF3F3")
  (set-face-attribute 'ediff-fine-diff-A    nil :background "#B22222")
  (set-face-attribute 'ediff-current-diff-B nil :background "#EBF8EC")
  (set-face-attribute 'ediff-fine-diff-B    nil :background "#00AA13")
  (set-face-attribute 'ediff-current-diff-C nil :background "#EBEFF8")
  (set-face-attribute 'ediff-fine-diff-C    nil :background "#4A8BB3"))

(with-eval-after-load "eros"
  (set-face-attribute 'eros-result-overlay-face nil
                      :foreground "white"
                      :background "#697D8A"
                      :box nil))

(with-eval-after-load "flycheck"
  (set-face-underline 'flycheck-error theme-color-error)
  (set-face-underline 'flycheck-warning theme-color-warning))

(with-eval-after-load "hl-todo"
  (setq hl-todo-keyword-faces
        `(("TODO"  . (:box '(:line-width 1) :foreground ,theme-color-warning))
          ("FIXME" . (:box '(:line-width 1) :foreground ,theme-color-error))
          ("NOTE"  . (:box '(:line-width 1))))))

(with-eval-after-load "indent-guide"
  (set-face-foreground 'indent-guide-face (face-foreground 'vertical-border)))

(with-eval-after-load "js2-mode"
  (set-face-underline 'js2-error theme-color-error)
  (set-face-underline 'js2-warning theme-color-warning))

(with-eval-after-load "lsp-mode"
  (set-face-attribute 'lsp-face-highlight-textual nil :background nil :box '(:line-width 1 :color "dark orange"))
  (set-face-attribute 'lsp-face-highlight-read    nil :background nil :box '(:line-width 1 :color "tomato"))
  (set-face-attribute 'lsp-face-highlight-write   nil :background nil :box '(:line-width 1 :color "sea green")))

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
