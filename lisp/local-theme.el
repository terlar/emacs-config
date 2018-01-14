;;; local-theme.el --- Local theme overrides

;;; Commentary:
;; Making things personal.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'tao-theme))

(deftheme local "Local theme overrides")

(tao-with-color-variables
  tao-theme-yang-palette

  ;;;
  ;; Theme Colors

  (defvar theme-color-error "tomato"
    "Color used to indicate error.")
  (defvar theme-color-success "sea green"
    "Color used to indicate success.")
  (defvar theme-color-warning "dark orange"
    "Color used to indicate warning.")

  (defvar theme-color-highlight color-14
    "Color used to highlight elements.")
  (defvar theme-color-lighter color-3
    "Color used for less visible elements.")

  ;;;
  ;; Theme Faces

  (custom-theme-set-faces
   'local
   `(mode-line                               ((t (:family ,my-variable-pitch-font :foreground ,color-12 :background ,color-5 :box (:line-width 15 :color ,color-5)))))
   `(mode-line-inactive                      ((t (:family ,my-variable-pitch-font :foreground ,color-9 :background ,color-6 :box (:line-width 15 :color ,color-6)))))
   `(mode-line-buffer-id                     ((t (:foreground nil :weight bold))))
   `(header-line-highlight                   ((t (:inherit mode-line-highlight))))
   `(line-number-current-line                ((t (:foreground ,theme-color-highlight :weight bold))))
   `(hl-line                                 ((t (:background ,color-5))))

   `(error                                   ((t (:foreground ,theme-color-error))))
   `(success                                 ((t (:foreground ,theme-color-success))))
   `(warning                                 ((t (:foreground ,theme-color-warning :weight bold))))

   ;; comint
   `(comint-highlight-prompt                 ((t (:height 1.0))))
   ;; compile
   `(compilation-error                       ((t (:foreground ,theme-color-error))))
   `(compilation-warning                     ((t (:foreground ,theme-color-warning))))
   ;; ediff
   `(ediff-current-diff-A                    ((t (:background "#EECCCC"))))
   `(ediff-fine-diff-A                       ((t (:background "#AA2222"))))
   `(ediff-current-diff-B                    ((t (:background "#CCEECC"))))
   `(ediff-fine-diff-B                       ((t (:background "#22AA22"))))
   `(ediff-current-diff-C                    ((t (:background "#CCFFFF"))))
   `(ediff-fine-diff-C                       ((t (:background "#4488BB"))))
   `(ediff-current-diff-Ancestor             ((t (:background "#DDF1F1"))))
   `(ediff-fine-diff-Ancestor                ((t (:background "#66CCCC"))))
   ;; org-mode
   `(org-block                               ((t (:inherit fixed-pitch))))
   `(org-formula                             ((t (:inherit fixed-pitch))))
   `(org-table                               ((t (:inherit fixed-pitch))))
   `(org-verbatim                            ((t (:inherit fixed-pitch))))

   ;; anzu
   `(anzu-mode-line                          ((t (:foreground "white" :background ,my-evil-default-mode-color :box nil))))
   ;; cargo
   `(cargo-process--standard-face            ((t (:foreground nil))))
   `(cargo-process--ok-face                  ((t (:foreground ,theme-color-success))))
   `(cargo-process--error-face               ((t (:foreground ,theme-color-error))))
   `(cargo-process--warning-face             ((t (:foreground ,theme-color-warning))))
   ;; company
   `(company-tooltip                         ((t (:family ,my-font))))
   `(company-tooltip-search                  ((t (:weight bold))))
   `(company-tooltip-search-selection        ((t (:weight bold))))
   ;; eros
   `(eros-result-overlay-face                ((t (:foreground "white" :background "#697D8A" :box nil))))
   ;; indent-guide
   `(indent-guide-face                       ((t (:inherit fringe))))
   ;; js2-mode
   `(js2-error                               ((t (:inherit flycheck-error))))
   `(js2-warning                             ((t (:inherit flycheck-warning))))
   ;; lsp-mode
   `(lsp-face-highlight-textual              ((t (:background nil :box (:line-width 1 :color "dark orange")))))
   `(lsp-face-highlight-read                 ((t (:background nil :box (:line-width 1 :color "tomato")))))
   `(lsp-face-highlight-write                ((t (:background nil :box (:line-width 1 :color "sea green")))))
   ;; flycheck
   `(flycheck-error                          ((t (:underline (:color ,theme-color-error)))))
   `(flycheck-warning                        ((t (:underline (:color ,theme-color-warning)))))
   ;; nav-flash
   `(nav-flash-face                          ((t (:background "#FFFBDD"))))
   ;; markdown
   `(markdown-hr-face                        ((t (:inherit fixed-pitch :height ,my-default-font-height))))
   `(markdown-pre-face                       ((t (:inherit fixed-pitch))))
   ;; rst
   `(rst-literal                             ((t (:inherit fixed-pitch))))
   ;; perspeen
   `(perspeen-selected-face                  ((t (:weight bold))))
   ;; shm
   `(shm-current-face                        ((t (:inherit hl-line))))
   `(shm-quarantine-face                     ((t (:background "#FFDDDD"))))
   ;; spray
   `(spray-base-face                         ((t (:foreground ,color-10 :family "Noto Serif" :weight normal :underline nil))))
   `(spray-accent-face                       ((t (:foreground "red" :underline (:color ,color-10) :overline ,color-10))))
   ;; stripe-buffer
   `(stripe-highlight                        ((t (:background ,color-5))))
   ;; web-mode
   `(web-mode-current-element-highlight-face ((t (:foreground nil :background nil :weight bold))))
   ;; which-key
   `(which-key-local-map-description-face    ((t (:weight bold)))))

  ;;;
  ;; Theme Variables

  (custom-theme-set-variables
   'local
   ;; beacon
   '(beacon-color "#FFFBDD")
   ;; coverlay
   '(coverlay:untested-line-background-color "#FFDDDD")
   '(coverlay:tested-line-background-color   "#EEFFCC")
   ;; hl-todo
   `(hl-todo-keyword-faces
     `(("TODO"  . (:box (:line-width 1) :foreground ,theme-color-warning))
       ("FIXME" . (:box (:line-width 1) :foreground ,theme-color-error))
       ("NOTE"  . (:box (:line-width 1)))))
   ;; lsp-ui
   `(lsp-ui-doc-background ,color-5)
   ;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,color-2)
       ( 40. . ,color-3)
       ( 60. . ,color-3)
       ( 80. . ,color-4)
       (100. . ,color-4)
       (120. . ,color-5)
       (140. . ,color-5)
       (160. . ,color-6)
       (180. . ,color-6)
       (200. . ,color-6)
       (220. . ,color-7)
       (240. . ,color-7)
       (260. . ,color-7)
       (280. . ,color-8)
       (300. . ,color-8)
       (320. . ,color-8)
       (340. . ,color-9)
       (360. . ,color-9)))
   ;; zoom-window
   `(zoom-window-mode-line-color (face-background 'mode-line))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'local)
;;; local-theme.el ends here
