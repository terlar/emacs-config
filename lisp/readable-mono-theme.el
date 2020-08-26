;;; readable-mono-theme.el --- Readable mostly monochromatic theme -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2020 Terje Larsen
;; All rights reserved.

;; Author: Terje Larsen <terlar@gmail.com>
;; URL: https://github.com/terlar/emacs-config
;; Keywords: faces
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

;; readable-mono-theme is free software: you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software Foundation,
;; either version 3 of the License, or (at your option) any later version.

;; readable-mono-theme is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Minimal and mostly monochromatic theme.
;;
;; Faces TODO:
;; - ivy-remote
;; - ivy-match-required-face
;; - ivy-confirm-face
;; - ivy-cursor
;; - edebug-disabled-breakpoint
;; - hi-black-hb
;; - hi-red-b
;; - rainbow-delimiters-depth-1-face
;; - company-echo-common
;; - company-preview-search
;; - company-preview-common
;; - company-scrollbar-bg
;; - company-scrollbar-fg
;; - company-tooltip-annotation-selection
;; - company-tooltip-annotation
;; - company-tooltip-common-selection
;; - company-tooltip-common
;; - company-tooltip-selection
;; - company-tooltip
;; - hl-todo
;; - flyspell-duplicate
;; - ert-test-result-unexpected
;; - ert-test-result-expected

;;; Code:
(deftheme readable-mono "Minimal and monochromatic theme")

(defgroup readable-mono-theme nil
  "Minimal and monochromatic theme customization options."
  :group 'faces)

;; Light theme
(defgroup readable-mono-theme-light nil
  "Minimal and monochromatic light theme customization options."
  :group 'readable-mono-theme)

(defcustom readable-mono-theme-light-cursor "#bb3e06"
  "Cursor for light theme."
  :type 'string
  :group 'readable-mono-theme-light)

(defcustom readable-mono-theme-light-foreground "#596e76"
  "Default foreground for light theme."
  :type 'string
  :group 'readable-mono-theme-light)

(defcustom readable-mono-theme-light-foreground-secondary "#98a6a6"
  "Secondary foreground for light theme."
  :type 'string
  :group 'readable-mono-theme-light)

(defcustom readable-mono-theme-light-background "#fffce9"
  "Default background for light theme."
  :type 'string
  :group 'readable-mono-theme-light)

(defcustom readable-mono-theme-light-background-secondary "#f4eedb"
  "Secondary background for light theme."
  :type 'string
  :group 'readable-mono-theme-light)

;; Dark theme
(defgroup readable-mono-theme-dark nil
  "Minimal and monochromatic dark theme customization options."
  :group 'readable-mono-theme)

(defcustom readable-mono-theme-dark-cursor "#db5823"
  "Cursor for dark theme."
  :type 'string
  :group 'readable-mono-theme-dark)

(defcustom readable-mono-theme-dark-foreground "#8d9fa1"
  "Default foreground for dark theme."
  :type 'string
  :group 'readable-mono-theme-dark)

(defcustom readable-mono-theme-dark-foreground-secondary "#62787f"
  "Secondary foreground for dark theme."
  :type 'string
  :group 'readable-mono-theme-dark)

(defcustom readable-mono-theme-dark-background "#002732"
  "Default background for dark theme."
  :type 'string
  :group 'readable-mono-theme-dark)

(defcustom readable-mono-theme-dark-background-secondary "#01323d"
  "Secondary background for dark theme."
  :type 'string
  :group 'readable-mono-theme-dark)

;; Faces
(defface readable-mono-theme-critical
  '((((background light)) (:foreground "#ffffff" :background "#cc1f24"))
    (((background dark)) (:foreground "#ffffff" :background "#ec423a")))
  "Face used for critical information that requires immediate action/attention."
  :group 'readable-mono-theme)

(defface readable-mono-theme-emphasis
  '((((background light)) (:foreground "#c42475"))
    (((background dark)) (:foreground "#e2468f")))
  "Face used for information that needs action/attention."
  :group 'readable-mono-theme)

(defface readable-mono-theme-strong
  '((t (:weight bold)))
  "Face used for information of strong importance."
  :group 'readable-mono-theme)

(defface readable-mono-theme-subordinate
  `((((background light)) (:weight light :foreground ,readable-mono-theme-light-foreground-secondary))
    (((background dark)) (:weight light :foreground ,readable-mono-theme-dark-foreground-secondary)))
  "Face used for information of less importance."
  :group 'readable-mono-theme)

(defface readable-mono-theme-actionable
  '((((background light)) (:foreground "#007ec4" :weight light))
    (((background dark)) (:foreground "#3c98e0" :weight light)))
  "Face used for information that is actionable.
For example links."
  :group 'readable-mono-theme)

(defface readable-mono-theme-secondary
  `((((background light)) (:background ,readable-mono-theme-light-background-secondary))
    (((background dark)) (:background ,readable-mono-theme-dark-background-secondary)))
  "Face used to distinguish from default but not stand out."
  :group 'readable-mono-theme)

(let ((l-bg readable-mono-theme-light-background)
      (l-bg-s readable-mono-theme-light-background-secondary)
      (l-fg readable-mono-theme-light-foreground)
      (l-green-bg "#eeedcb")
      (l-green-bg-s "#d5d99d")
      (l-red-bg "#ffe1cb")
      (l-red-bg-s "#ffb79f")
      (l-blue-bg "#e6ebe7")
      (l-blue-bg-s "#bfd2e6")
      (d-bg readable-mono-theme-dark-background)
      (d-bg-s readable-mono-theme-dark-background-secondary)
      (d-fg readable-mono-theme-dark-foreground)
      (d-green-bg "#1e3531")
      (d-green-bg-s "#354725")
      (d-red-bg "#2f2c31")
      (d-red-bg-s "#582b29")
      (d-blue-bg "#023447")
      (d-blue-bg-s "#004363"))
  ;; Theme faces
  (custom-theme-set-faces
   'readable-mono
   `(default
      ((((background light)) (:background ,l-bg :foreground ,l-fg))
       (((background dark)) (:background ,d-bg :foreground ,d-fg))))

   ;; Semantic
   `(bold ((t (:inherit readable-mono-theme-strong))))
   `(bold-italic ((t (:inherit readable-mono-theme-strong))))
   `(link ((t (:inherit readable-mono-theme-actionable :underline t))))
   `(shadow ((t (:inherit readable-mono-theme-subordinate))))
   `(success ((t (:foreground nil))))
   `(warning ((t (:inherit readable-mono-theme-emphasis))))
   `(error ((t (:inherit readable-mono-theme-critical))))
   `(font-lock-comment-face ((t (:inherit readable-mono-theme-subordinate :italic t))))
   `(font-lock-function-name-face ((t (:foreground nil))))
   `(font-lock-variable-name-face ((t (:foreground nil))))
   `(font-lock-constant-face ((t (:foreground nil))))
   `(font-lock-doc-string-face ((t (:foreground nil))))
   `(font-lock-doc-face ((t (:foreground nil))))
   `(font-lock-preprocessor-face ((t (:foreground nil))))
   `(font-lock-reference-face ((t (:foreground nil))))
   `(font-lock-string-face ((t (:foreground nil))))
   `(font-lock-type-face ((t (:inherit readable-mono-theme-strong))))
   `(font-lock-builtin-face ((t (:inherit readable-mono-theme-strong))))
   `(font-lock-keyword-face ((t (:inherit readable-mono-theme-strong))))
   `(font-lock-warning-face ((t (:inherit readable-mono-theme-emphasis))))

   ;; Headlines
   `(custom-variable-tag ((t (:inherit readable-mono-theme-strong))))

   ;; Search and highlight
   `(highlight ((t (:inverse-video t))))
   `(lazy-highlight ((t (:inherit readable-mono-theme-subordinate :inverse-video t))))
   `(isearch ((t (:inherit highlight))))
   `(isearch-fail ((t (:inherit readable-mono-theme-critical))))
   `(match ((t (:inherit highlight))))
   `(region ((t (:inherit readable-mono-theme-secondary))))

   ;; Visual aid
   `(cursor
     ((((background light)) (:background ,readable-mono-theme-light-cursor))
      (((background dark)) (:background ,readable-mono-theme-dark-cursor))))
   `(hl-line ((t (:inherit readable-mono-theme-secondary))))
   `(line-number-current-line
     ((((background light)) (:inherit readable-mono-theme-strong :foreground ,readable-mono-theme-light-cursor))
      (((background dark)) (:inherit readable-mono-theme-strong :foreground ,readable-mono-theme-dark-cursor))))
   `(show-paren-match ((t (:inherit readable-mono-theme-strong :underline t))))
   `(show-paren-mismatch ((t (:inherit readable-mono-theme-critical))))

   ;; Interface
   `(mode-line
     ((((background light)) (:background ,l-bg-s :box (:line-width 6 :color ,l-bg-s)))
      (((background dark)) (:background ,d-bg-s :box (:line-width 6 :color ,d-bg-s)))))
   `(mode-line-inactive
     ((((background light)) (:background ,l-bg :box (:line-width 6 :color ,l-bg)))
      (((background dark)) (:background ,d-bg :box (:line-width 6 :color ,d-bg)))))
   `(mode-line-emphasis ((t :bold nil)))
   `(mode-line-buffer-id ((t :inherit readable-mono-theme-strong)))
   `(button ((t (:inherit readable-mono-theme-actionable :underline t))))
   `(custom-button-face ((t (:inherit readable-mono-theme-button))))
   `(fringe ((t (:inherit readable-mono-theme-subordinate))))
   `(header-line ((t (:inherit mode-line))))
   `(header-line-highlight ((t (:inherit mode-line-highlight))))
   `(menu ((t (:inherit readable-mono-theme-secondary))))
   `(minibuffer-prompt ((t (:inherit readable-mono-theme-strong))))
   `(secondary-selection ((t (:inherit readable-mono-theme-secondary))))
   `(tool-bar ((t (:inherit readable-mono-theme-secondary))))
   `(tooltip ((t (:inherit readable-mono-theme-secondary))))
   `(vertical-border
     ((((background light)) (:foreground ,l-bg-s))
      (((background dark)) (:foreground ,d-bg-s))))
   `(widget-button ((t (:inherit button))))
   `(widget-field ((t (:inherit readable-mono-theme-secondary))))
   `(window-divider ((t (:inherit vertical-border))))
   `(window-divider-first-pixel ((t (:inherit window-divider))))
   `(window-divider-last-pixel ((t (:inherit window-divider))))

   ;; cider
   `(cider-test-failure-face ((t (:inherit readable-mono-theme-critical))))
   `(cider-result-overlay-face ((t (:inherit highlight))))

   ;; Diff
   `(ediff-current-diff-A
     ((((background light)) (:background ,l-red-bg))
      (((background dark)) (:background ,d-red-bg))))
   `(ediff-fine-diff-A
     ((((background light)) (:background ,l-red-bg-s))
      (((background dark)) (:background ,d-red-bg-s))))
   `(ediff-current-diff-B
     ((((background light)) (:background ,l-green-bg))
      (((background dark)) (:background ,d-green-bg))))
   `(ediff-fine-diff-B
     ((((background light)) (:background ,l-green-bg-s))
      (((background dark)) (:background ,d-green-bg-s))))
   `(ediff-current-diff-C
     ((((background light)) (:background ,l-blue-bg))
      (((background dark)) (:background ,d-blue-bg))))
   `(ediff-fine-diff-C
     ((((background light)) (:background ,l-blue-bg-s))
      (((background dark)) (:background ,d-blue-bg-s))))
   `(ediff-current-diff-Ancestor ((t (:inherit readable-mono-theme-secondary))))
   `(ediff-fine-diff-Ancestor ((t (:inherit highlight))))
   `(smerge-base ((t (:inherit readable-mono-theme-secondary))))
   `(smerge-lower
     ((((background light)) (:background ,l-red-bg))
      (((background dark)) (:background ,d-red-bg))))
   `(smerge-upper
     ((((background light)) (:background ,l-green-bg))
      (((background dark)) (:background ,d-green-bg))))

   ;; Dired
   `(dired-directory ((t (:inherit readable-mono-theme-strong))))

   ;; ERC
   `(erc-my-nick-face ((t (:inherit readable-mono-theme-emphasis))))

   ;; Eros
   `(eros-result-overlay-face ((t (:inherit highlight))))

   ;; EShell
   `(eshell-prompt ((t (:inherit readable-mono-theme-strong))))
   `(eshell-ls-archive ((t (:inherit readable-mono-theme-strong))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:inherit readable-mono-theme-strong))))
   `(eshell-ls-executable ((t (:inherit readable-mono-theme-strong))))
   `(eshell-ls-unreadable ((t (:inherit nil))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:inherit readable-mono-theme-strong))))
   `(eshell-ls-symlink ((t (:inherit readable-mono-theme-strong))))

   ;; Haskell-mode
   `(haskell-interactive-face-prompt ((t (:inherit readable-mono-theme-strong))))

   ;; Indent-guide
   `(indent-guide-face ((t (:inherit fringe))))

   ;; Ivy
   `(ivy-current-match ((t (:inherit highlight))))
   `(ivy-minibuffer-match-face-1 ((t (:inherit readable-mono-theme-strong))))
   `(ivy-minibuffer-match-face-2 ((t (:inherit readable-mono-theme-strong))))
   `(ivy-minibuffer-match-face-3 ((t (:inherit readable-mono-theme-strong))))
   `(ivy-minibuffer-match-face-4 ((t (:inherit readable-mono-theme-strong))))

   ;; Magit
   `(magit-section-heading ((t (:foreground nil))))
   `(magit-section-highlight ((t (:inherit readable-mono-theme-secondary))))

   ;; Popup
   `(popup-tip-face ((t (:inherit (nil readable-mono-theme-secondary readable-mono-theme-subordinate)))))

   ;; Rainbow-delimiters
   `(rainbow-delimiters-unmatched-face ((t (:inherit readable-mono-theme-critical))))

   ;; rst
   `(rst-directive ((t (:inherit font-lock-comment-face))))
   `(rst-external ((t (:inherit font-lock-comment-face))))
   `(rst-literal ((t (:inherit readable-mono-theme-secondary :extend t))))

   ;; Smartparens
   `(sp-show-pair-match-face ((t (:inherit show-paren-match))))
   `(sp-show-pair-mismatch-face ((t (:inherit show-paren-mismatch))))
 
   ;; Spray
   `(spray-accent-face
     ((((background light)) (:foreground ,readable-mono-theme-light-cursor :underline (:color ,(face-foreground 'default)) :overline ,(face-foreground 'default)))
      (((background dark)) (:foreground ,readable-mono-theme-dark-cursor :underline (:color ,(face-foreground 'default)) :overline ,(face-foreground 'default)))))

   ;; Stripe-buffer
   `(stripe-highlight ((t (:inherit readable-mono-theme-secondary :extend t))))

   ;; Term
   `(term-color-red ((t (:inherit readable-mono-theme-critical))))

   ;; Markdown
   `(markdown-code-face ((t (:inherit (fixed-pitch readable-mono-theme-secondary) :extend t))))
   `(markdown-inline-code-face ((t (:inherit readable-mono-theme-secondary))))

   ;; Org
   `(org-ellipsis ((t (:inherit readable-mono-theme-subordinate))))
   `(org-done ((t (:foreground nil))))
   `(org-todo ((t (:foreground nil))))
   `(org-headline-done ((t (:foreground nil))))
   `(org-headline-todo ((t (:foreground nil))))
   `(org-block ((t (:inherit readable-mono-theme-secondary))))
   `(org-date ((t (:foreground nil :underline nil))))
   `(org-document-info ((t (:foreground nil))))
   `(org-drawer ((t (:inherit readable-mono-theme-subordinate))))
   `(org-verbatim ((t (:inherit readable-mono-theme-secondary))))

   ;; Org-tree-slide
   `(org-tree-slide-header-overlay-face ((t (:inherit header-line))))

   ;; Quick-peek
   `(quick-peek-background-face ((t :inherit readable-mono-theme-secondary)))

   ;; Web-mode
   `(web-mode-current-element-highlight-face ((t (:inherit show-paren-match)))))

  ;; Theme variables
  (custom-theme-set-variables
   'readable-mono
   ;; ansi-color
   `(ansi-color-names-vector ,(pcase (frame-parameter nil 'background-mode)
                                ('light
                                 `[,l-fg ,(face-foreground 'readable-mono-theme-emphasis) ,l-fg ,l-fg ,l-fg ,l-fg ,l-fg ,l-fg])
                                ('dark
                                 `[,d-fg ,(face-foreground 'readable-mono-theme-emphasis) ,d-fg ,d-fg ,d-fg ,d-fg ,d-fg ,d-fg])))
   ;; coverlay
   `(coverlay:untested-line-background-color
     ,(pcase (frame-parameter nil 'background-mode)
        ('light l-red-bg)
        ('dark d-red-bg)))
   `(coverlay:tested-line-background-color
     ,(pcase (frame-parameter nil 'background-mode)
        ('light l-green-bg)
        ('dark d-green-bg)))
    ;; hl-todo
   `(hl-todo-keyword-faces
     `(("TODO"  . (:inherit readable-mono-theme-strong :box (:line-width 1)))
       ("FIXME" . (:inherit readable-mono-theme-emphasis :box (:line-width 1)))
       ("NOTE"  . (:box (:line-width 1)))))
   ;; rainbow-identifiers
   `(rainbow-identifiers-cie-l*a*b*-saturation 65)
   `(rainbow-identifiers-cie-l*a*b*-lightness 45)
   ;; zoom-window
   `(zoom-window-mode-line-color ,(pcase (frame-parameter nil 'background-mode)
                                    ('light l-bg-s)
                                    ('dark d-bg-s)))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'readable-mono)
(provide 'readable-mono-theme)
;;; readable-mono-theme.el ends here
