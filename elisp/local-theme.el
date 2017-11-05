;;; local-theme.el --- Local theme overrides

;;; Commentary:
;; Making things personal.

;;; Code:

(eval-when-compile
  (require 'base-vars))

(deftheme local "Local theme overrides")

;;;
;; Theme Colors

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
;; Theme Faces

(custom-theme-set-faces
 'local
 `(mode-line                               ((t (:family ,my-variable-pitch-font :foreground "#252525" :background "#DADADA" :box (:line-width 6 :color "#DADADA")))))
 `(mode-line-inactive                      ((t (:family ,my-variable-pitch-font :foreground "#9E9E9E" :background "#F6F6F6" :box (:line-width 6 :color "#F6F6F6")))))
 `(mode-line-buffer-id                              ((t (:foreground nil :weight bold))))
 `(header-line-highlight                            ((t (:inherit mode-line-highlight))))
 `(line-number-current-line                         ((t (:foreground ,theme-color-highlight :weight bold))))

 `(error                                            ((t (:foreground ,theme-color-error))))
 `(success                                          ((t (:foreground ,theme-color-success))))
 `(warning                                          ((t (:foreground ,theme-color-warning :weight bold))))

 `(my-code-face                                     ((t (:background ,theme-color-lighter))))
 `(my-folded-face                                   ((t (:background ,theme-color-lighter))))

 ;; anzu
 `(anzu-mode-line                                   ((t (:foreground "white" :background ,my-evil-default-mode-color :box nil))))
 ;; company
 `(company-tooltip                                  ((t (:family ,my-font))))
 ;; ediff
 `(ediff-current-diff-A                             ((t (:background "#FEF3F3"))))
 `(ediff-fine-diff-A                                ((t (:background "#B22222"))))
 `(ediff-current-diff-B                             ((t (:background "#EBF8EC"))))
 `(ediff-fine-diff-B                                ((t (:background "#00AA13"))))
 `(ediff-current-diff-C                             ((t (:background "#EBEFF8"))))
 `(ediff-fine-diff-C                                ((t (:background "#4A8BB3"))))
 ;; eros
 `(eros-result-overlay-face                         ((t (:foreground "white" :background "#697D8A" :box nil))))
 ;; indent-guide
 `(indent-guide-face                                ((t (:inherit fringe))))
 ;; js2-mode
 `(js2-error                                        ((t (:inherit flycheck-error))))
 `(js2-warning                                      ((t (:inherit flycheck-warning))))
 ;; lsp-mode
 `(lsp-face-highlight-textual                       ((t (:background nil :box (:line-width 1 :color "dark orange")))))
 `(lsp-face-highlight-read                          ((t (:background nil :box (:line-width 1 :color "tomato")))))
 `(lsp-face-highlight-write                         ((t (:background nil :box (:line-width 1 :color "sea green")))))
 ;; flycheck
 `(flycheck-error                                   ((t (:underline (:color ,theme-color-error)))))
 `(flycheck-warning                                 ((t (:underline (:color ,theme-color-warning)))))
 ;; markdown-mode
 `(markdown-inline-code-face                        ((t (:box (:line-width 1)))))
 ;; nav-flash
 `(nav-flash-face                                   ((t (:background "pale goldenrod"))))
 ;; neotree
 `(neo-root-dir-face                                ((t (:family my-variable-pitch-font))))
 `(neo-dir-link-face                                ((t (:family my-variable-pitch-font))))
 `(neo-file-link-face                               ((t (:family my-variable-pitch-font))))
 ;; rst
 `(rst-literal                                      ((t (:inherit my-code-face))))
 ;; shm
 `(shm-current-face                                 ((t (:inherit hl-line))))
 `(shm-quarantine-face                              ((t (:background "#ffdddd"))))
 ;; web-mode
 `(web-mode-current-element-highlight-face          ((t (:foreground nil :background nil :weight bold)))))

;;;
;; Theme Variables

(custom-theme-set-variables
 'local
 ;; coverlay
 '(coverlay:untested-line-background-color "#FEF3F3")
 '(coverlay:tested-line-background-color   "#EBF8EC")
 ;; hl-todo
 `(hl-todo-keyword-faces
   `(("TODO"  . (:box (:line-width 1) :foreground ,theme-color-warning))
     ("FIXME" . (:box (:line-width 1) :foreground ,theme-color-error))
     ("NOTE"  . (:box (:line-width 1)))))
 ;; zoom-window
 `(zoom-window-mode-line-color (face-background 'mode-line)))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'local)
;;; local-theme.el ends here
