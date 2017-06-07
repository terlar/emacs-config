;;; base-theme.el --- Theme configuration

;;; Commentary:
;; The look of things.

;;; Code:
(defvar my-theme 'tao-yang
  "The color theme to use.")

(defvar my-default-font-height 140
  "The default font height to use.")

(defvar my-font "Fira Mono"
  "The monospace font to use.")

(defvar my-variable-pitch-font "Fira Sans Book"
  "The regular font to use.")

(defvar my-unicode-font "Noto Mono"
  "Fallback font for unicode glyphs.")

(defvar my-evil-default-mode-color "#AB47BC"
  "Default mode color for Evil states.")

(defcustom my-evil-mode-color-list
  `((normal   . "#4CAF50")
    (emacs    . "#2196F3")
    (insert   . "#2196F3")
    (replace  . "#F44336")
    (visual   . "#FF9800"))
  "Mode color corresponding to Evil state."
  :type '(alist :key-type symbol :value-type string)
  :group 'evil-mode-line)

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
      (set-fontset-font t 'unicode (font-spec :family my-unicode-font)))
    ;; ... and for variable-pitch mode
    (when my-variable-pitch-font
      (set-face-attribute 'variable-pitch nil :family my-variable-pitch-font))))

;;;
;; Faces

;; Line numbers
(with-eval-after-load 'nlinum
  (set-face-attribute 'linum nil
                      :inherit 'default
                      :family my-variable-pitch-font)
  (set-face-attribute 'nlinum-current-line nil
                      :foreground "tomato"
                      :weight 'bold))

;; Messages
(set-face-attribute 'error nil :foreground "tomato")
(set-face-attribute 'success nil :foreground "sea green")
(set-face-attribute 'warning nil :foreground "dark orange" :weight 'bold)

;; Mode-line
(defface neotree-hl-line
  '((t :inherit hl-line))
  "Face for highlighting the current line with `neotree-hl-line'."
  :group 'hl-line)

(defface my-folded-face
  `((((background dark))
     (:inherit font-lock-comment-face :background "black"))
    (((background light))
     (:inherit font-lock-comment-face :background "light grey")))
  "Face to hightlight `hideshow' overlays."
  :group 'editing)

(set-face-attribute 'mode-line nil
                    :inherit 'variable-pitch
                    :background "#697D8A"
                    :box '(:line-width 6 :color "#697D8A"))
(set-face-attribute 'mode-line-inactive nil
                    :inherit 'variable-pitch
                    :background "#889BA7"
                    :box '(:line-width 6 :color "#889BA7"))

;; Anzu
(with-eval-after-load 'anzu
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "white"
                      :background my-evil-default-mode-color
                      :box nil))

;; Company
(with-eval-after-load 'company
  (set-face-attribute 'company-tooltip nil :family my-font)
  (set-face-attribute 'company-tooltip-annotation-selection nil
                      :inherit 'company-tooltip-selection))

;; Ediff
(with-eval-after-load 'ediff
  (set-face-attribute 'ediff-current-diff-A nil :background "#FEF3F3")
  (set-face-attribute 'ediff-fine-diff-A    nil :background "#B22222")
  (set-face-attribute 'ediff-current-diff-B nil :background "#EBF8EC")
  (set-face-attribute 'ediff-fine-diff-B    nil :background "#00AA13")
  (set-face-attribute 'ediff-current-diff-C nil :background "#EBEFF8")
  (set-face-attribute 'ediff-fine-diff-C    nil :background "#4A8BB3"))

;; Flash
(with-eval-after-load 'nav-flash
  (set-face-attribute 'nav-flash-face nil :background "pale goldenrod"))

;; Neotree
(with-eval-after-load 'neotree
  (set-face-attribute 'neo-root-dir-face  nil :inherit 'variable-pitch)
  (set-face-attribute 'neo-dir-link-face  nil :inherit 'variable-pitch)
  (set-face-attribute 'neo-file-link-face nil :inherit 'variable-pitch))

(provide 'base-theme)
;;; base-theme.el ends here
