;;; local-theme.el --- Local theme overrides

;; Title: Local Theme
;; Project: local-theme
;; Version: 0.1.0
;; URL: https://github.com/terlar/emacs.d
;; Package-Requires: ((emacs "24.1"))

;;; Commentary:
;; Making things personal.

;;; Code:

(require 'tao-theme)

(deftheme local "Local theme overrides")

(defgroup local-theme nil
  "Local theme customization options."
  :group 'local)

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

(defcustom local-theme-variable-pitch-font "sans-serif"
  "Font used for variable-pitch."
  :type 'string
  :group 'local-theme)

(defcustom local-theme-serif-font "serif"
  "Font used for serif."
  :type 'string
  :group 'local-theme)

;;;
;; Colors

(defcustom local-theme-color-error "tomato"
  "Color used to indicate error."
  :type 'string
  :group 'local-theme)
(defcustom local-theme-color-success "sea green"
  "Color used to indicate success."
  :type 'string
  :group 'local-theme)
(defcustom local-theme-color-warning "gold"
  "Color used to indicate warning."
  :type 'string
  :group 'local-theme)

;;;
;; Theme Faces

(custom-theme-set-faces
 'local
 `(default                                 ((t (:height ,local-theme-default-font-height :family ,local-theme-fixed-pitch-font :weight light))))
 `(fixed-pitch                             ((t (:height 1.0 :family ,local-theme-fixed-pitch-font :weight light))))
 `(variable-pitch                          ((t (:height 1.0 :family ,local-theme-variable-pitch-font :weight normal))))
 `(mode-line                               ((t (:height 0.8 :family ,local-theme-variable-pitch-font :background "#E2E3E8" :box (:line-width 4 :color "#E2E3E8")))))
 `(mode-line-inactive                      ((t (:height 0.8 :family ,local-theme-variable-pitch-font :background "#FAFAFA" :box (:line-width 4 :color "#FAFAFA")))))
 `(mode-line-buffer-id                     ((t (:foreground nil :bold t))))
 `(header-line                             ((t (:inherit mode-line))))
 `(header-line-highlight                   ((t (:inherit mode-line-highlight))))
 `(line-number                             ((t (:inherit fixed-pitch))))
 `(line-number-current-line                ((t (:inherit (line-number hl-line) :bold t))))

 `(error                                   ((t (:foreground ,local-theme-color-error))))
 `(success                                 ((t (:foreground ,local-theme-color-success))))
 `(warning                                 ((t (:foreground ,local-theme-color-warning :bold t))))

 ;; comint
 `(comint-highlight-prompt                 ((t (:height 1.0))))
 ;; compile
 `(compilation-error                       ((t (:foreground ,local-theme-color-error))))
 `(compilation-warning                     ((t (:foreground ,local-theme-color-warning))))
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
 `(erc-current-nick-face                   ((t (:foreground ,local-theme-color-error))))
 ;; org-mode
 `(org-document-title                      ((t (:inherit nil :bold nil :height 1.8))))
 `(org-level-1                             ((t (:inherit nil :bold nil :height 1.6))))
 `(org-level-2                             ((t (:inherit nil :bold nil :height 1.4))))
 `(org-level-3                             ((t (:inherit nil :bold nil :height 1.2))))
 `(org-level-4                             ((t (:inherit nil :bold nil :height 1.1 :italic t))))
 `(org-level-5                             ((t (:inherit nil :bold nil :height 1.1 :italic t))))
 `(org-level-6                             ((t (:inherit nil :bold nil :height 1.1 :italic t))))
 `(org-level-7                             ((t (:inherit nil :bold nil :height 1.1 :italic t))))
 `(org-level-8                             ((t (:inherit nil :bold nil :height 1.1 :italic t))))
 `(org-todo                                ((t (:height ,local-theme-default-font-height))))
 `(org-done                                ((t (:height ,local-theme-default-font-height))))
 `(org-block                               ((t (:weight light))))
 `(org-checkbox                            ((t (:inherit fixed-pitch))))
 `(org-code                                ((t (:inherit org-verbatim))))
 `(org-hide                                ((t (:inherit fixed-pitch))))
 `(org-verbatim                            ((t (:inherit fixed-pitch))))

 ;; show-paren-mode
 `(show-paren-match                        ((t (:bold t :underline t :box nil))))
 `(show-paren-mismatch                     ((t (:bold t :foreground ,local-theme-color-error))))
 ;; smerge
 `(smerge-upper                            ((t (:background "#E6FFED"))))
 `(smerge-lower                            ((t (:background "#FFEEF0"))))
 ;; `(smerge-base                             ((t (:background ,color-5))))
 ;; term
 `(term-color-red                          ((t (:foreground ,local-theme-color-error :background nil))))
 ;; whitespace
 `(whitespace-space                        ((t (:background nil :family ,local-theme-fixed-pitch-font))))
 ;; window-divider
 `(window-divider                          ((t (:inherit 'vertical-border))))
 `(window-divider-first-pixel              ((t (:inherit 'window-divider))))
 `(window-divider-last-pixel               ((t (:inherit 'window-divider))))

 ;; cakecrumbs
 `(cakecrumbs-tag                          ((t (:inherit font-lock-keyword-face))))
 ;; cargo
 `(cargo-process--standard-face            ((t (:foreground nil))))
 `(cargo-process--ok-face                  ((t (:foreground ,local-theme-color-success))))
 `(cargo-process--error-face               ((t (:foreground ,local-theme-color-error))))
 `(cargo-process--warning-face             ((t (:foreground ,local-theme-color-warning))))
 ;; cider
 `(cider-test-failure-face                 ((t (:background ,local-theme-color-error))))
 ;; `(cider-result-overlay-face               ((t (:background ,color-5 :foreground ,color-9 :box nil))))
 ;; company
 `(company-tooltip-search                  ((t (:bold t))))
 `(company-tooltip-search-selection        ((t (:bold t))))
 ;; eldoc-posframe
 ;; `(eldoc-posframe-background-face          ((t (:background ,color-5 :foreground ,color-9))))
 ;; eros
 `(eros-result-overlay-face                ((t (:inherit highlight :box nil))))
 ;; haskell-mode
 `(haskell-interactive-face-prompt         ((t (:inherit font-lock-keyword-face))))
 ;; idle-highlight
 ;; `(idle-highlight                          ((t (:background ,local-theme-color-highlight))))
 ;; indent-guide
 `(indent-guide-face                       ((t (:inherit fringe))))
 ;; ivy-posframe
 ;; `(ivy-posframe                            ((t (:background ,color-5 :foreground ,color-11))))
 ;; markdown
 `(markdown-header-face                    ((t (:weight normal :italic t))))
 `(markdown-header-face-1                  ((t (:inherit org-level-1))))
 `(markdown-header-face-2                  ((t (:inherit org-level-2))))
 `(markdown-header-face-3                  ((t (:inherit org-level-3 :underline nil))))
 `(markdown-header-face-4                  ((t (:inherit org-level-4 :underline nil))))
 `(markdown-header-face-5                  ((t (:inherit org-level-5 :underline nil))))
 `(markdown-header-face-6                  ((t (:inherit org-level-6 :underline nil))))
 `(markdown-code-face                      ((t (:inherit fixed-pitch))))
 `(markdown-hr-face                        ((t (:inherit fixed-pitch :height ,local-theme-default-font-height))))
 ;; `(markdown-inline-code-face               ((t (:background ,color-3))))
 `(markdown-pre-face                       ((t (:inherit fixed-pitch))))
 `(markdown-table-face                     ((t (:inherit fixed-pitch))))
 `(markdown-gfm-checkbox-face              ((t (:inherit org-checkbox))))
 ;; org-tree-slide
 `(org-tree-slide-header-overlay-face      ((t (:inherit header-line :background nil))))
 ;; perspeen
 `(perspeen-selected-face                  ((t (:bold t))))
 ;; popup
 ;; `(popup-tip-face                          ((t (:background ,color-5 :foreground ,color-9))))
 ;; rainbow-delimiters
 `(rainbow-delimiters-unmatched-face       ((t (:bold t :foreground ,local-theme-color-error))))
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
 `(shm-quarantine-face                     ((t (:background "#FFEEF0"))))
 ;; smartparens
 `(sp-show-pair-match-face                 ((t (:inherit show-paren-match :box nil))))
 `(sp-show-pair-mismatch-face              ((t (:inherit show-paren-mismatch))))
 ;; spray
 `(spray-base-face                         ((t (:weight normal :underline nil :family ,local-theme-serif-font))))
 `(spray-accent-face                       ((t (:foreground ,local-theme-color-error :underline (:color (face-attribute 'default :foreground)) :overline (face-attribute 'default :foreground)))))
 ;; stripe-buffer
 ;; `(stripe-highlight                        ((t (:background ,color-5))))
 ;; web-mode
 `(web-mode-current-element-highlight-face ((t (:background nil :foreground nil :bold t))))
 ;; which-key
 `(which-key-local-map-description-face    ((t (:bold t))))
 ;; quick-peek
 ;; `(quick-peek-border-face                  ((t (:background ,color-5 :height 0.1 :box (:line-width 1 :color ,color-7)))))
 `(quick-peek-padding-face                 ((t (:height 0.1)))))

  ;;;
;; Theme Variables

(custom-theme-set-variables
 'local
 `(line-spacing ,local-theme-line-spacing)
 ;; ansi-color
 `(ansi-color-names-vector [(face-attribute 'default :foreground)
                            ,local-theme-color-error
                            (face-attribute 'default :foreground)
                            (face-attribute 'default :foreground)
                            (face-attribute 'default :foreground)
                            (face-attribute 'default :foreground)
                            (face-attribute 'default :foreground)
                            (face-attribute 'default :foreground)])
 ;; beacon
 ;; `(beacon-color ,color-5)
 ;; coverlay
 `(coverlay:untested-line-background-color "#FFEEF0")
 `(coverlay:tested-line-background-color   "#E6FFED")
 ;; hl-todo
 `(hl-todo-keyword-faces
   `(("TODO"  . (:box (:line-width 1) :foreground ,local-theme-color-warning))
     ("FIXME" . (:box (:line-width 1) :foreground ,local-theme-color-error))
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
 ;; `(vc-annotate-color-map
 ;;   '(( 20. . ,color-2)
 ;;     ( 40. . ,color-3)
 ;;     ( 60. . ,color-3)
 ;;     ( 80. . ,color-4)
 ;;     (100. . ,color-4)
 ;;     (120. . ,color-5)
 ;;     (140. . ,color-5)
 ;;     (160. . ,color-6)
 ;;     (180. . ,color-6)
 ;;     (200. . ,color-6)
 ;;     (220. . ,color-7)
 ;;     (240. . ,color-7)
 ;;     (260. . ,color-7)
 ;;     (280. . ,color-8)
 ;;     (300. . ,color-8)
 ;;     (320. . ,color-8)
 ;;     (340. . ,color-9)
 ;;     (360. . ,color-9)))
 ;; zoom-window
 `(zoom-window-mode-line-color (face-background 'mode-line)))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'local)
(provide 'local-theme)
;;; local-theme.el ends here
