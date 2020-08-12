;;; local-theme.el --- Local theme overrides -*- coding: utf-8; lexical-binding: t; -*-

;; Title: Local Theme
;; Project: local-theme
;; Version: 0.1.0
;; URL: https://github.com/terlar/emacs-config
;; Package-Requires: ((emacs "24.1"))

;;; Commentary:
;; Making things personal.

;;; Code:

(require 'color)

(deftheme local "Local theme overrides")

(defgroup local-theme nil
  "Local theme customization options."
  :group 'local)

(defun local-theme-darken-default-background ()
  "Darkens the `default' face background."
  (let ((bg (face-background 'default)))
    (if (color-name-to-rgb bg)
        (color-darken-name bg 3)
      bg)))

(defun local-theme-lighten-default-background ()
  "Lightens the `default' face background."
  (let ((bg (face-background 'default)))
    (if (color-name-to-rgb bg)
        (color-lighten-name bg 3)
      bg)))

;;;
;; Typography

(defcustom local-theme-default-font-height nil
  "Default font height."
  :type 'string
  :group 'local-theme)

(defcustom local-theme-line-spacing 0
  "Spacing between lines."
  :type 'number
  :group 'local-theme)

(defcustom local-theme-fixed-pitch-font "Monospace"
  "Font used for fixed-pitch."
  :type 'string
  :group 'local-theme)

(defcustom local-theme-fixed-pitch-serif-font "Monospace"
  "Font used for fixed-pitch serif."
  :type 'string
  :group 'local-theme)

(defcustom local-theme-variable-pitch-font "sans-serif"
  "Font used for variable-pitch."
  :type 'string
  :group 'local-theme)

(defcustom local-theme-serif-font "serif"
  "Font used for serif."
  :type 'string
  :group 'local-theme)

;;;
;; Faces

(defface local-theme-critical
  '((((background light)) (:foreground "#ffffff" :background "tomato"))
    (((background dark)) (:foreground "#385f38" :background "#f8f893")))
  "Face used for critical information that requires immediate action/attention."
  :group 'local-theme)

(defface local-theme-highlight
  '((((background light)) (:foreground "#ffa07a"))
    (((background dark)) (:foreground "#f0dfaf")))
  "Face used for information that needs action/attention."
  :group 'local-theme)

(defface local-theme-strong
  '((((background light)) (:weight bold))
    (((background dark)) (:weight bold)))
  "Face used for information of strong importance."
  :group 'local-theme)

(defface local-theme-important
  '((((background light)) (:foreground "#00008b" :weight light))
    (((background dark)) (:foreground "#dca3a3" :weight light)))
  "Face used for information of importance."
  :group 'local-theme)

(defface local-theme-subordinate
  '((((background light)) (:foreground "#999999" :weight light))
    (((background dark)) (:foreground "#777767" :weight light)))
  "Face used for information of less importance."
  :group 'local-theme)

(defface local-theme-secondary
  nil
  "Face used to distinguish from default but not stand out."
  :group 'local-theme)

;;;
;; Theme Faces

(custom-theme-set-faces
 'local
 `(local-theme-secondary
   ((((background light)) (:background ,(local-theme-darken-default-background)))
    (((background dark)) (:background ,(local-theme-lighten-default-background)))))

 `(default           ((t (:height ,local-theme-default-font-height :family ,local-theme-fixed-pitch-font :weight light))))
 `(cursor            ((t (:background ,(face-foreground 'local-theme-highlight)))))
 `(region            ((t (:inherit local-theme-secondary :foreground nil :background nil))))
 `(highlight         ((t (:inherit local-theme-secondary :foreground nil :background nil))))

 ;; Typography
 `(fixed-pitch       ((t (:family ,local-theme-fixed-pitch-font :weight light))))
 `(fixed-pitch-serif ((t (:family ,local-theme-fixed-pitch-serif-font :weight light))))
 `(variable-pitch    ((t (:family ,local-theme-variable-pitch-font :weight light))))
 `(bold              ((t (:inherit local-theme-strong))))
 `(bold-italic       ((t (:inherit local-theme-strong))))

 ;; Semantic
 `(shadow  ((t (:inherit local-theme-subordinate))))
 `(success ((t (:inherit local-theme-important :foreground nil :weight unspecified))))
 `(warning ((t (:inherit local-theme-highlight :foreground nil :background nil :weight unspecified))))
 `(error   ((t (:inherit local-theme-critical :foreground nil :background nil :weight unspecified))))
 `(link    ((t (:inherit local-theme-important :underline t :foreground nil :weight unspecified :underline t))))
 `(custom-link    ((t (:inherit local-theme-important :underline t :foreground nil :weight unspecified :underline t))))
 `(button  ((t (:inherit local-theme-important :underline t :foreground nil :weight unspecified :underline t))))

 ;; Interface
 `(mode-line                               ((t (:height 0.8 :family ,local-theme-variable-pitch-font :box (:line-width 6 :color ,(face-background 'mode-line))))))
 `(mode-line-inactive                      ((t (:inherit
                                                (local-theme-subordinate mode-line)
                                                :foreground nil
                                                :background ,(face-background 'default)
                                                :box (:line-width 6 :color ,(face-background 'default))))))
 `(mode-line-buffer-id                     ((t (:inherit local-theme-strong :foreground nil))))
 `(header-line                             ((t (:inherit mode-line))))
 `(header-line-highlight                   ((t (:inherit mode-line-highlight))))
 `(line-number                             ((t (:inherit fixed-pitch))))
 `(line-number-current-line                ((t (:inherit (local-theme-highlight local-theme-strong hl-line)))))

 ;; ediff
 `(ediff-current-diff-A                    ((t (:background "#FFEEF0"))))
 `(ediff-fine-diff-A                       ((t (:background "#EECCCC"))))
 `(ediff-current-diff-B                    ((t (:background "#E6FFED"))))
 `(ediff-fine-diff-B                       ((t (:background "#CCEECC"))))
 `(ediff-current-diff-C                    ((t (:background "#F1F8FF"))))
 `(ediff-fine-diff-C                       ((t (:background "#DDEEFF"))))
 `(ediff-current-diff-Ancestor             ((t (:background "#FEE6FF"))))
 `(ediff-fine-diff-Ancestor                ((t (:background "#EEDDFF"))))
 ;; erc
 `(erc-current-nick-face                   ((t (:inherit local-theme-highlight))))
 ;; eshell
 `(eshell-prompt                           ((t (:inherit nil :bold t))))
 `(eshell-ls-archive                       ((t (:inherit nil :bold t))))
 `(eshell-ls-backup                        ((t (:inherit font-lock-comment-face))))
 `(eshell-ls-clutter                       ((t (:inherit font-lock-comment-face))))
 `(eshell-ls-directory                     ((t (:inherit nil :bold t))))
 `(eshell-ls-executable                    ((t (:inherit nil :bold t))))
 `(eshell-ls-unreadable                    ((t (:inherit nil))))
 `(eshell-ls-missing                       ((t (:inherit font-lock-warning-face))))
 `(eshell-ls-product                       ((t (:inherit font-lock-doc-face))))
 `(eshell-ls-special                       ((t (:inherit nil :bold t))))
 `(eshell-ls-symlink                       ((t (:inherit nil :bold t))))
 ;; org-mode
 `(org-document-title                      ((t (:inherit nil :bold nil :height 1.8))))
 `(org-link                                ((t (:inherit link :foreground nil))))
 `(org-level-1                             ((t (:inherit nil :bold nil :height 1.6))))
 `(org-level-2                             ((t (:inherit nil :bold nil :height 1.4))))
 `(org-level-3                             ((t (:inherit nil :bold nil :height 1.2))))
 `(org-level-4                             ((t (:inherit nil :bold nil :height 1.1 :italic t))))
 `(org-level-5                             ((t (:inherit nil :bold nil :height 1.1 :italic t))))
 `(org-level-6                             ((t (:inherit nil :bold nil :height 1.1 :italic t))))
 `(org-level-7                             ((t (:inherit nil :bold nil :height 1.1 :italic t))))
 `(org-level-8                             ((t (:inherit nil :bold nil :height 1.1 :italic t))))
 `(org-todo                                ((t (:height unspecified))))
 `(org-done                                ((t (:height unspecified))))
 `(org-headline-done                       ((t (:inherit nil))))
 `(org-headline-todo                       ((t (:inherit nil))))
 `(org-block                               ((t (:weight light :extend t))))
 `(org-block-begin-line                    ((t (:box nil :background nil))))
 `(org-block-end-line                      ((t (:box nil :background nil))))
 `(org-checkbox                            ((t (:inherit fixed-pitch))))
 `(org-code                                ((t (:inherit org-verbatim))))
 `(org-hide                                ((t (:inherit fixed-pitch))))
 `(org-verbatim                            ((t (:inherit fixed-pitch))))
 ;; outline
 `(outline-1                               ((t (:inherit nil :bold nil :height 1.6))))
 `(outline-2                               ((t (:inherit nil :bold nil :height 1.4))))
 `(outline-3                               ((t (:inherit nil :bold nil :height 1.2))))
 `(outline-4                               ((t (:inherit nil :bold nil :height 1.1 :italic t))))
 `(outline-5                               ((t (:inherit nil :bold nil :height 1.1 :italic t))))
 `(outline-6                               ((t (:inherit nil :bold nil :height 1.1 :italic t))))
 `(outline-7                               ((t (:inherit nil :bold nil :height 1.1 :italic t))))
 `(outline-8                               ((t (:inherit nil :bold nil :height 1.1 :italic t))))
 ;; show-paren-mode
 `(show-paren-match                        ((t (:inherit local-theme-strong :underline t :box nil))))
 `(show-paren-mismatch                     ((t (:inherit local-theme-critical))))
 ;; smerge
 `(smerge-upper                            ((((background light)) (:background "#E6FFED"))
                                            (((background dark)) (:background "#004400"))))
 `(smerge-lower                            ((((background light)) (:background "#FFEEF0"))
                                            (((background dark)) (:background "#440000"))))
 `(smerge-base                             ((t (:inherit local-theme-secondary))))
 ;; term
 `(term-color-red                          ((t (:inherit local-theme-critical))))
 ;; whitespace
 `(whitespace-space                        ((t (:background nil :family ,local-theme-fixed-pitch-font))))
 ;; window-divider
 `(window-divider                          ((t (:inherit vertical-border))))
 `(window-divider-first-pixel              ((t (:inherit window-divider))))
 `(window-divider-last-pixel               ((t (:inherit window-divider))))

 ;; cakecrumbs
 `(cakecrumbs-tag                          ((t (:inherit font-lock-keyword-face))))
 ;; cider
 `(cider-test-failure-face                 ((t (:inherit local-theme-critical))))
 `(cider-result-overlay-face               ((t (:inherit local-theme-secondary))))
 ;; company
 `(company-tooltip-search                  ((t (:bold t))))
 `(company-tooltip-search-selection        ((t (:bold t))))
 ;; eldoc-posframe
 `(eldoc-posframe-background-face          ((t (:inherit local-theme-secondary))))
 ;; eros
 `(eros-result-overlay-face                ((t (:inherit highlight :box nil))))
 ;; haskell-mode
 `(haskell-interactive-face-prompt         ((t (:inherit font-lock-keyword-face))))
 ;; indent-guide
 `(indent-guide-face                       ((t (:inherit fringe))))
 ;; ivy
 `(ivy-current-match                       ((t (:inherit hl-line :foreground nil :background nil :extend t))))
 ;; ivy-posframe
 `(ivy-posframe                            ((t (:inherit local-theme-secondary))))
 ;; markdown
 `(markdown-header-face                    ((t (:weight normal :italic t))))
 `(markdown-header-face-1                  ((t (:inherit org-level-1))))
 `(markdown-header-face-2                  ((t (:inherit org-level-2))))
 `(markdown-header-face-3                  ((t (:inherit org-level-3 :underline nil))))
 `(markdown-header-face-4                  ((t (:inherit org-level-4 :underline nil))))
 `(markdown-header-face-5                  ((t (:inherit org-level-5 :underline nil))))
 `(markdown-header-face-6                  ((t (:inherit org-level-6 :underline nil))))
 `(markdown-code-face                      ((t (:inherit fixed-pitch))))
 `(markdown-hr-face                        ((t (:inherit fixed-pitch :height unspecified))))
 `(markdown-inline-code-face               ((t (:inherit local-theme-secondary))))
 `(markdown-pre-face                       ((t (:inherit fixed-pitch))))
 `(markdown-table-face                     ((t (:inherit fixed-pitch))))
 `(markdown-gfm-checkbox-face              ((t (:inherit org-checkbox))))
 ;; org-tree-slide
 `(org-tree-slide-header-overlay-face      ((t (:inherit header-line :background nil))))
 ;; perspeen
 `(perspeen-selected-face                  ((t (:bold t))))
 ;; popup
 `(popup-tip-face                          ((t (:inherit local-theme-secondary :foreground nil))))
 ;; rainbow-delimiters
 `(rainbow-delimiters-unmatched-face       ((t (:inherit local-theme-critical))))
 ;; readable
 `(readable-variable-pitch                 ((t (:height 1.1 :family ,local-theme-serif-font))))
 ;; rst
 `(rst-level-1                             ((t (:inherit org-level-1))))
 `(rst-level-2                             ((t (:inherit org-level-2))))
 `(rst-level-3                             ((t (:inherit org-level-3))))
 `(rst-level-4                             ((t (:inherit org-level-4))))
 `(rst-level-5                             ((t (:inherit org-level-5))))
 `(rst-level-6                             ((t (:inherit org-level-6))))
 `(rst-literal                             ((t (:inherit fixed-pitch))))
 ;; shm
 `(shm-current-face                        ((t (:inherit hl-line))))
 `(shm-quarantine-face                     ((((background light)) (:background "#FFEEF0"))
                                            (((background dark)) (:background "#440000"))))
 ;; smartparens
 `(sp-show-pair-match-face                 ((t (:inherit show-paren-match :box nil))))
 `(sp-show-pair-mismatch-face              ((t (:inherit show-paren-mismatch))))
 ;; spray
 `(spray-base-face                         ((t (:weight normal :underline nil :family ,local-theme-serif-font))))
 `(spray-accent-face                       ((t (:foreground "tomato" :underline (:color ,(face-foreground 'default)) :overline ,(face-foreground 'default)))))
 ;; stripe-buffer
 `(stripe-highlight                        ((t (:inherit local-theme-secondary :extend t))))
 ;; web-mode
 `(web-mode-current-element-highlight-face ((t (:background nil :foreground nil :bold t))))
 ;; which-key
 `(which-key-local-map-description-face    ((t (:bold t))))
 ;; quick-peek
 `(quick-peek-background-face              ((t (:inherit local-theme-secondary)))))

;;;
;; Theme Variables

(custom-theme-set-variables
 'local
 `(line-spacing ,local-theme-line-spacing)
 ;; ansi-color
 `(ansi-color-names-vector [,(face-foreground 'default)
                            "tomato"
                            ,(face-foreground 'default)
                            ,(face-foreground 'default)
                            ,(face-foreground 'default)
                            ,(face-foreground 'default)
                            ,(face-foreground 'default)
                            ,(face-foreground 'default)])
 ;; coverlay
 `(coverlay:untested-line-background-color ((((background light)) (:background "#FFEEF0"))
                                            (((background dark)) (:background "#440000"))))
 `(coverlay:tested-line-background-color   ((((background light)) (:background "#E6FFED"))
                                            (((background dark)) (:background "#004400"))))
 ;; hl-todo
 `(hl-todo-keyword-faces
   `(("TODO"  . (:inherit local-theme-strong :box (:line-width 1)))
     ("FIXME" . (:inherit local-theme-highlight :box (:line-width 1)))
     ("NOTE"  . (:box (:line-width 1)))))
 ;; markdown-mode
 `(markdown-header-scaling-values
   '(2.2 1.6 1.4 1.2 1.0 1.0))
 ;; rainbow-identifiers
 `(rainbow-identifiers-cie-l*a*b*-saturation 65)
 `(rainbow-identifiers-cie-l*a*b*-lightness 45)
 ;; rst-mode
 `(rst-header-scaling-values
   '(2.2 1.6 1.4 1.2 1.0 1.0))
 ;; vc-annotate
 `(vc-annotate-color-map
   '((20 . "#FFCCCC")
     (40 . "#FFD8CC")
     (60 . "#FFE4CC")
     (80 . "#FFF0CC")
     (100 . "#FFFCCC")
     (120 . "#F6FFCC")
     (140 . "#EAFFCC")
     (160 . "#DEFFCC")
     (180 . "#D2FFCC")
     (200 . "#CCFFD2")
     (220 . "#CCFFDE")
     (240 . "#CCFFEA")
     (260 . "#CCFFF6")
     (280 . "#CCFCFF")
     (300 . "#CCF0FF")
     (320 . "#CCE4FF")
     (340 . "#CCD8FF")
     (360 . "#CCCCFF")))
 ;; zoom-window
 `(zoom-window-mode-line-color ,(face-background 'mode-line)))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'local)
(provide 'local-theme)
;;; local-theme.el ends here
