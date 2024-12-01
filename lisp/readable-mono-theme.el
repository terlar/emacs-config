;;; readable-mono-theme.el --- Readable mostly monochromatic theme -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2022 Terje Larsen
;; All rights reserved.

;; Author: Terje Larsen <terlar@gmail.com>
;; URL: https://github.com/terlar/emacs-config
;; Keywords: faces
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))

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

;;; Code:
(deftheme readable-mono "Minimal and monochromatic theme.")

(defgroup readable-mono-theme nil
  "Minimal and monochromatic theme customization options."
  :group 'faces)

;;;; Light theme
(defgroup readable-mono-theme-light nil
  "Minimal and monochromatic light theme customization options."
  :group 'readable-mono-theme)

(defcustom readable-mono-theme-light-background "#fbf7ef"
  "Default background color for light theme."
  :type 'string
  :group 'readable-mono-theme-light)

(defcustom readable-mono-theme-light-secondary-background "#f1ede5"
  "Secondary background color for light theme."
  :type 'string
  :group 'readable-mono-theme-light)

(defcustom readable-mono-theme-light-highlight-background "#f1e9d2"
  "Highlight background color for light theme."
  :type 'string
  :group 'readable-mono-theme-light)

(defcustom readable-mono-theme-light-foreground "#000011"
  "Default foreground color for light theme."
  :type 'string
  :group 'readable-mono-theme-light)

(defcustom readable-mono-theme-light-secondary-foreground "#474747"
  "Secondary foreground color for light theme."
  :type 'string
  :group 'readable-mono-theme-light)

(defcustom readable-mono-theme-light-attention "#d6000c"
  "Attention color for light theme."
  :type 'string
  :group 'readable-mono-theme-light)

;;;; Dark theme
(defgroup readable-mono-theme-dark nil
  "Minimal and monochromatic dark theme customization options."
  :group 'readable-mono-theme)

(defcustom readable-mono-theme-dark-background "#000011"
  "Default background color for dark theme."
  :type 'string
  :group 'readable-mono-theme-dark)

(defcustom readable-mono-theme-dark-secondary-background "#252525"
  "Secondary background color for dark theme."
  :type 'string
  :group 'readable-mono-theme-dark)

(defcustom readable-mono-theme-dark-highlight-background "#3b3b3b"
  "Highlight background color for dark theme."
  :type 'string
  :group 'readable-mono-theme-dark)

(defcustom readable-mono-theme-dark-foreground "#ffffee"
  "Default foreground color for dark theme."
  :type 'string
  :group 'readable-mono-theme-dark)

(defcustom readable-mono-theme-dark-secondary-foreground "#8d9fa1"
  "Secondary foreground color for dark theme."
  :type 'string
  :group 'readable-mono-theme-dark)

(defcustom readable-mono-theme-dark-attention "#ec423a"
  "Attention color for dark theme."
  :type 'string
  :group 'readable-mono-theme-dark)

;;;; Faces
(defface readable-mono-theme-secondary
  `((((background light)) (:background ,readable-mono-theme-light-secondary-background))
    (((background dark)) (:background ,readable-mono-theme-dark-secondary-background)))
  "Face used to distinguish from default but not stand out."
  :group 'readable-mono-theme)

(let ((l-bg readable-mono-theme-light-background)
      (l-bg-s readable-mono-theme-light-secondary-background)
      (l-bg-hl readable-mono-theme-light-highlight-background)
      (l-fg readable-mono-theme-light-foreground)
      (l-fg-s readable-mono-theme-light-secondary-foreground)
      (l-attention readable-mono-theme-light-attention)

      (d-bg readable-mono-theme-dark-background)
      (d-bg-s readable-mono-theme-dark-secondary-background)
      (d-bg-hl readable-mono-theme-dark-highlight-background)
      (d-fg readable-mono-theme-dark-foreground)
      (d-fg-s readable-mono-theme-dark-secondary-foreground)
      (d-attention readable-mono-theme-dark-attention))

;;;; Theme faces
  (custom-theme-set-faces
   'readable-mono

;;;;; Basic
   `(default
     ((((type graphic) (background light)) (:background ,l-bg :foreground ,l-fg))
      (((type graphic) (background dark)) (:background ,d-bg :foreground ,d-fg))
      (((type tty) (background light)) (:background unspecified :foreground ,l-fg))
      (((type tty) (background dark)) (:background unspecified :foreground ,d-fg))))

   `(shadow
     ((((background light)) (:foreground ,l-fg-s))
      (((background dark)) (:foreground ,d-fg-s))))

   `(highlight
     ((((background light)) (:background ,l-bg-hl))
      (((background dark)) (:background ,d-bg-hl))))
   `(region
     ((((background light)) (:background ,l-bg :foreground ,l-fg :inverse-video t))
      (((background dark)) (:background ,d-bg :foreground ,l-bg :inverse-video t))))
   '(secondary-selection ((t (:inherit highlight))))

   `(help-key-binding
     ((((background light)) (:background ,l-bg-hl :foreground ,l-fg-s))
      (((background dark)) (:background ,d-bg-hl :foreground ,d-fg-s))))

   `(error
     ((((background light)) (:background ,l-attention :foreground "#ffffff"))
      (((background dark)) (:background ,d-attention :foreground "#ffffff"))))
   '(warning ((t (:inherit bold))))
   '(success ((t (:foreground unspecified))))

;;;;; Visual aid
   `(cursor
     ((((background light)) (:background ,l-attention))
      (((background dark)) (:background ,d-attention))))
   `(line-number-current-line
     ((((background light)) (:inherit bold :foreground ,l-attention))
      (((background dark)) (:inherit bold :foreground ,d-attention))))
   '(hl-line ((t (:inherit highlight))))
   '(show-paren-match ((t (:inherit bold :underline t))))
   '(show-paren-mismatch ((t (:inherit error))))
   '(trailing-whitespace ((t (:inherit error))))
   '(whitespace-line ((t (:inherit error))))
   '(whitespace-trailing ((t (:inherit error))))

;;;;; Search
   '(completions-annotations ((t (:inherit italic))))
   '(completions-common-part ((t (:inherit region))))
   '(completion-preview-exact ((t (:inherit underline))))
   '(isearch ((t (:inherit region))))
   '(isearch-fail ((t (:inherit error))))
   '(lazy-highlight ((t (:inherit highlight))))
   '(match ((t (:inherit highlight))))

;;;;; Interface
   `(mode-line-active
     ((((background light)) (:background ,l-bg-s :box (:line-width 6 :color ,l-bg-s)))
      (((background dark)) (:background ,d-bg-s :box (:line-width 6 :color ,d-bg-s)))))
   `(mode-line-inactive
     ((((type graphic) (background light)) (:background ,l-bg :box (:line-width 6 :color ,l-bg)))
      (((type graphic) (background dark)) (:background ,d-bg :box (:line-width 6 :color ,d-bg)))
      (((type tty) (background light)) (:background unspecified :foreground ,l-fg-s))
      (((type tty) (background dark)) (:background unspecified :foreground ,d-fg-s))))
   '(mode-line-emphasis ((t :bold nil)))
   '(mode-line-buffer-id ((t :inherit bold)))

   `(header-line
     ((((background light)) (:foreground ,l-bg :background ,l-fg :box (:line-width 6 :color ,l-fg)))
      (((background dark)) (:foreground ,d-bg :background ,d-fg :box (:line-width 6 :color ,d-fg)))))

   '(minibuffer-prompt ((t (:inherit bold))))

   '(fringe ((t (:inherit shadow))))

   '(button ((t (:inherit underline))))
   '(widget-button ((t (:inherit button))))
   `(widget-field
     ((((background light)) (:inherit readable-mono-theme-secondary :box (:line-width 1 :color ,l-bg)))
      (((background dark)) (:inherit readable-mono-theme-secondary :box (:line-width 1 :color ,d-bg)))))

   '(menu ((t (:inherit readable-mono-theme-secondary))))
   '(tool-bar ((t (:inherit readable-mono-theme-secondary))))
   '(tooltip ((t (:inherit readable-mono-theme-secondary))))

   `(vertical-border
     ((((background light)) (:foreground ,l-bg-s))
      (((background dark)) (:foreground ,d-bg-s))))
   '(window-divider ((t (:inherit vertical-border))))
   '(window-divider-first-pixel ((t (:inherit window-divider))))
   '(window-divider-last-pixel ((t (:inherit window-divider))))

;;;;; Headlines
   '(custom-group-tag ((t (:inherit bold))))
   '(custom-state ((t (:inherit bold))))
   '(custom-variable-tag ((t (:inherit bold))))

;;;;; font-lock
   '(font-lock-function-name-face ((t (:foreground unspecified))))
   '(font-lock-variable-name-face ((t (:foreground unspecified))))
   '(font-lock-constant-face ((t (:foreground unspecified))))
   '(font-lock-doc-string-face ((t (:foreground unspecified))))
   '(font-lock-doc-face ((t (:foreground unspecified))))
   '(font-lock-preprocessor-face ((t (:foreground unspecified))))
   '(font-lock-reference-face ((t (:foreground unspecified))))
   '(font-lock-string-face ((t (:foreground unspecified))))
   '(font-lock-warning-face ((t (:inherit warning))))

;;;;; Diff
   '(diff-header ((t (:inherit readable-mono-theme-secondary))))
   '(diff-file-header ((t (:inherit header-line))))
   '(diff-hunk-header ((t (:inherit shadow))))
   `(diff-indicator-removed
     ((((background light)) (:background ,l-attention :foreground ,l-attention))
      (((background dark)) (:background ,d-attention :foreground ,d-attention))))
   `(diff-removed ((t (:inherit shadow :strike-through t))))
   `(diff-refine-removed ((t (:inherit diff-removed))))
   `(diff-indicator-added
     ((((background light)) (:background ,l-fg :foreground ,l-fg))
      (((background dark)) (:background ,d-fg :foreground ,d-fg))))
   `(diff-added ((t (:inherit highlight))))
   `(diff-refine-added ((t (:inherit diff-added))))
   `(diff-indicator-changed
     ((((background light)) (:background ,l-fg-s :foreground ,l-fg-s))
      (((background dark)) (:background ,d-fg-s :foreground ,d-fg-s))))
   '(diff-refine-changed ((t (:inverse-video t))))
   '(diff-error ((t (:inherit error))))

   '(ediff-current-diff-A ((t (:inherit highlight))))
   '(ediff-fine-diff-A ((t (:inherit region))))
   '(ediff-current-diff-B ((t (:inherit highlight))))
   '(ediff-fine-diff-B ((t (:inherit region))))
   `(ediff-current-diff-C ((t (:inherit highlight))))
   `(ediff-fine-diff-C ((t (:inherit region))))
   '(ediff-current-diff-Ancestor ((t (:inherit readable-mono-theme-secondary))))
   '(ediff-fine-diff-Ancestor ((t (:inherit highlight))))

   '(smerge-base ((t (:inherit readable-mono-theme-secondary))))
   '(smerge-lower ((t (:background unspecified))))
   '(smerge-upper ((t (:background unspecified))))
   '(smerge-refined-added ((t (:inherit diff-refine-added))))
   '(smerge-refined-changed ((t (:inherit diff-refine-changed))))
   '(smerge-refined-removed ((t (:inherit diff-refine-removed))))

   '(diff-hl-insert ((t (:inherit diff-indicator-added))))
   `(diff-hl-change
     ((((background light)) (:background ,l-bg-s :foreground ,l-bg-s))
      (((background dark)) (:background ,d-bg-s :foreground ,d-bg-s))))
   '(diff-hl-delete ((t (:inherit diff-indicator-removed))))
   `(diff-hl-unknown
     ((((background light)) (:background ,l-bg-s :foreground ,l-bg-s))
      (((background dark)) (:background ,d-bg-s :foreground ,d-bg-s))))

;;;;; ansi-color
   `(ansi-color-black
     ((((background light)) (:foreground ,l-fg))
      (((background dark)) (:foreground ,d-fg))))
   `(ansi-color-bright-black
     ((((background light)) (:foreground ,l-fg-s))
      (((background dark)) (:foreground ,d-fg-s))))
   `(ansi-color-red
     ((((background light)) (:foreground ,l-attention))
      (((background dark)) (:foreground ,d-attention))))
   '(ansi-color-bright-red ((t (:inherit ansi-color-red))))
   '(ansi-color-green ((t (:inherit success))))
   '(ansi-color-bright-green ((t (:inherit ansi-color-green))))
   '(ansi-color-yellow ((t (:inherit shadow))))
   '(ansi-color-bright-yellow ((t (:inherit ansi-color-yellow))))
   '(ansi-color-blue ((t (:inherit ansi-color-black))))
   '(ansi-color-bright-blue ((t (:inherit ansi-color-blue))))
   '(ansi-color-magenta ((t (:inherit ansi-color-black))))
   '(ansi-color-bright-magenta ((t (:inherit ansi-color-magenta))))
   '(ansi-color-cyan ((t (:inherit ansi-color-black))))
   '(ansi-color-bright-cyan ((t (:inherit ansi-color-cyan))))
   '(ansi-color-white ((t (:inherit shadow))))
   '(ansi-color-bright-white ((t (:inherit ansi-color-white))))

;;;;; cider
   '(cider-test-failure-face ((t (:inherit error))))
   '(cider-result-overlay-face ((t (:inherit highlight))))

;;;;; company
   '(company-echo-common ((t (:inherit completions-common-part))))
   '(company-preview ((t (:inherit shadow))))
   '(company-preview-common ((t (:inherit completions-common-part))))
   '(company-preview-search ((t (:inverse-video t))))
   '(company-scrollbar-bg ((t (:inherit region))))
   '(company-scrollbar-fg ((t (:inherit cursor))))
   '(company-tooltip ((t (:inherit tooltip))))
   '(company-tooltip-annotation ((t (:inherit shadow))))
   '(company-tooltip-common ((t (:inherit completions-common-part))))
   '(company-tooltip-selection ((t (:inherit highlight))))

;;;;; compilation
   '(compilation-mode-line-fail ((t (:inherit compilation-error))))

;;;;; corfu
   '(corfu-default ((t (:inherit readable-mono-theme-secondary))))
   '(corfu-bar ((t (:inherit nil :inverse-video t))))
   '(corfu-current ((t (:inherit highlight))))

;;;;; cov
   `(cov-none-face
     ((((background light)) (:foreground ,l-attention))
      (((background dark)) (:foreground ,d-attention))))
   `(cov-light-face
     ((((background light)) (:foreground ,l-fg-s))
      (((background dark)) (:foreground ,d-fg-s))))
   `(cov-med-face
     ((((background light)) (:foreground ,l-fg-s))
      (((background dark)) (:foreground ,d-fg-s))))
   `(cov-heavy-face
     ((((background light)) (:foreground ,l-fg))
      (((background dark)) (:foreground ,d-fg))))

   `(cov-coverage-not-run-face
     ((((background light)) (:foreground ,l-attention))
      (((background dark)) (:foreground ,d-attention))))
   `(cov-coverage-run-face
     ((((background light)) (:foreground ,l-fg))
      (((background dark)) (:foreground ,d-fg))))

;;;;; dired
   '(all-the-icons-dired-dir-face ((t (:foreground unspecified))))
   '(dired-directory ((t (:inherit bold))))
   '(dired-broken-symlink ((t (:inherit error))))
   '(dired-flagged ((t (:inherit bold))))

;;;;; erc
   '(erc-current-nick-face ((t (:inherit bold))))
   '(erc-input-face ((t (:inherit readable-mono-theme-secondary :extend t))))
   '(erc-my-nick-face ((t (:inherit bold))))
   '(erc-nick-default-face ((t (:inherit bold))))
   '(erc-notice-face ((t (:inherit nil))))
   '(erc-prompt-face ((t (:inherit bold))))
   '(erc-timestamp-face ((t (:inherit shadow))))

;;;;; eros
   '(eros-result-overlay-face ((t (:inherit highlight))))

;;;;; eshell
   '(eshell-prompt ((t (:inherit bold))))
   '(eshell-ls-archive ((t (:inherit bold))))
   '(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   '(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   '(eshell-ls-directory ((t (:inherit bold))))
   '(eshell-ls-executable ((t (:inherit bold))))
   '(eshell-ls-unreadable ((t (:inherit nil))))
   '(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   '(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   '(eshell-ls-special ((t (:inherit bold))))
   '(eshell-ls-symlink ((t (:inherit bold))))

;;;;; flymake
   `(flymake-error
     ((((background light)) (:underline (:style wave :color ,l-attention)))
      (((background dark)) (:underline (:style wave :color ,d-attention)))))

;;;;; flyspell
   `(flyspell-duplicate
     ((((background light)) (:underline (:style wave :color ,l-fg-s)))
      (((background dark)) (:underline (:style wave :color ,d-fg-s)))))
   `(flyspell-incorrect
     ((((background light)) (:underline (:style wave :color ,l-fg)))
      (((background dark)) (:underline (:style wave :color ,d-fg)))))

;;;;; ghelp
   '(ghelp-header-button ((t (:inherit info-header-node :box nil))))

;;;;; gotest
   '(go-test--standard-face ((t (:foreground unspecified))))
   '(go-test--ok-face ((t (:foreground unspecified))))
   '(go-test--error-face ((t (:inherit error))))
   '(go-test--pointer-face ((t (:foreground unspecified))))
   '(go-test--warning-face ((t (:inherit warning))))

;;;;; haskell
   '(haskell-interactive-face-prompt ((t (:inherit bold))))

;;;;; idle-highlight
   '(idle-highlight ((t (:inherit underline))))

;;;;; imenu-list
   '(imenu-list-entry-face-0 ((t (:foreground unspecified))))
   '(imenu-list-entry-face-1 ((t (:foreground unspecified))))
   '(imenu-list-entry-face-2 ((t (:foreground unspecified))))
   '(imenu-list-entry-face-3 ((t (:foreground unspecified))))

;;;;; indent-guide
   '(indent-guide-face ((t (:inherit fringe))))

;;;;; Info
   '(info-header-node ((t (:inherit bold))))
   '(info-header-xref ((t (:inherit bold :box nil))))

;;;;; magit
   '(magit-section-heading-selection ((t (:inherit region))))
   '(magit-section-highlight ((t (:inherit readable-mono-theme-secondary))))
   '(magit-diff-file-heading-selection ((t (:inherit region))))
   '(magit-diff-context ((t (:background unspecified))))
   '(magit-diff-context-highlight ((t (:inherit readable-mono-theme-secondary))))
   '(magit-diff-removed ((t (:inherit diff-refine-removed))))
   '(magit-diff-removed-highlight ((t (:inherit (diff-removed readable-mono-theme-secondary)))))
   '(magit-diff-added ((t (:inherit diff-refine-added))))
   '(magit-diff-added-highlight ((t (:inherit diff-added))))
   '(magit-diff-hunk-heading ((t (:inherit nil))))
   '(magit-diff-hunk-heading-highlight ((t (:inherit readable-mono-theme-secondary))))
   '(magit-diff-hunk-heading-selection ((t (:inherit region))))
   '(magit-diff-lines-boundary ((t (:inherit region))))
   '(magit-diff-lines-heading ((t (:inherit region))))
   '(magit-process-ok ((t (:inherit success))))
   '(magit-process-ng ((t (:inherit error))))

   '(magit-branch-current ((t (:inherit bold :box (:line-width 1)))))
   '(magit-branch-local ((t (:inherit bold))))
   '(magit-branch-remote ((t (:inherit bold))))
   '(magit-head ((t (:inherit bold))))
   '(magit-tag ((t (:inherit italic))))

;;;;; marginalia
   '(marginalia-file-priv-no ((t (:inherit nil))))

;;;;; markdown
   '(markdown-code-face ((t (:inherit readable-mono-theme-secondary :extend t))))
   `(markdown-inline-code-face
     ((((background light)) (:inherit readable-mono-theme-secondary :box (:line-width 1 :color ,l-bg)))
      (((background dark)) (:inherit readable-mono-theme-secondary :box (:line-width 1 :color ,d-bg)))))

;;;;; message
   '(message-cited-text-1 ((t (:foreground unspecified))))
   '(message-cited-text-2 ((t (:foreground unspecified))))
   '(message-cited-text-3 ((t (:foreground unspecified))))
   '(message-cited-text-4 ((t (:foreground unspecified))))
   '(message-header-cc ((t (:foreground unspecified))))
   '(message-header-name ((t (:foreground unspecified))))
   '(message-header-newsgroups ((t (:inherit bold))))
   '(message-header-other ((t (:foreground unspecified))))
   '(message-header-subject ((t (:inherit bold))))
   '(message-header-to ((t (:inherit bold))))
   '(message-header-xheader ((t (:foreground unspecified))))
   '(message-mml ((t (:foreground unspecified))))
   '(message-separator ((t (:inherit shadow))))

;;;;; orderless
   '(orderless-match-face-0 ((t (:inherit completions-common-part))))
   '(orderless-match-face-1 ((t (:inherit completions-common-part))))
   '(orderless-match-face-2 ((t (:inherit completions-common-part))))
   '(orderless-match-face-3 ((t (:inherit completions-common-part))))

;;;;; org
   '(org-meta-line ((t (:inherit (shadow font-lock-comment-face)))))
   '(org-ellipsis ((t (:inherit shadow))))
   '(org-done ((t (:foreground unspecified))))
   '(org-todo ((t (:inherit bold))))
   '(org-headline-done ((t (:foreground unspecified))))
   '(org-headline-todo ((t (:foreground unspecified))))
   '(org-block ((t (:inherit readable-mono-theme-secondary :extend t))))
   `(org-code
     ((((background light)) (:inherit readable-mono-theme-secondary :box (:line-width 1 :color ,l-bg)))
      (((background dark)) (:inherit readable-mono-theme-secondary :box (:line-width 1 :color ,d-bg)))))
   '(org-date ((t (:foreground unspecified :underline nil))))
   '(org-document-info ((t (:foreground unspecified))))
   '(org-drawer ((t (:inherit shadow))))
   `(org-hide
     ((((background light)) (:foreground ,l-bg))
      (((background dark)) (:foreground ,d-bg))))
   '(org-table ((t (:foreground unspecified))))
   '(org-quote ((t (:foreground unspecified))))

   '(org-agenda-structure ((t (:foreground unspecified))))
   '(org-agenda-date ((t (:foreground unspecified))))
   '(org-agenda-date-today ((t (:inherit (bold highlight)))))
   '(org-agenda-date-weekend ((t (:inherit bold))))
   '(org-scheduled ((t (:foreground unspecified))))
   '(org-scheduled-today ((t (:inherit underline))))
   '(org-scheduled-previously ((t (:inherit bold))))

;;;;; org-tree-slide
   '(org-tree-slide-header-overlay-face ((t (:inherit header-line))))

;;;;; outline
   '(outline-minor-0 ((t (:background unspecified))))

;;;;; popup
   '(popup-face ((t (:inherit (readable-mono-theme-secondary default)))))
   '(popup-isearch-match ((t (:inherit bold))))
   '(popup-menu-mouse-face ((t (:underline t))))
   '(popup-menu-selection-face ((t (:inherit (highlight default)))))
   '(popup-scroll-bar-background-face ((t (:inherit region))))
   '(popup-scroll-bar-foreground-face ((t (:inherit cursor))))
   '(popup-summary-face ((t (:inherit (popup-face shadow)))))
   `(popup-tip-face
     ((((background light)) (:inherit default :background ,l-bg-s :box (:line-width 6 :color ,l-bg-s)))
      (((background dark)) (:inherit default :background ,d-bg-s :box (:line-width 6 :color ,d-bg-s)))))

;;;;; quick-peek
   '(quick-peek-background-face ((t :inherit readable-mono-theme-secondary)))

;;;;; rainbow-delimiters
   '(rainbow-delimiters-unmatched-face ((t (:inherit error))))

;;;;; rst
   '(rst-directive ((t (:inherit font-lock-comment-face))))
   '(rst-external ((t (:inherit font-lock-comment-face))))
   '(rst-literal ((t (:inherit readable-mono-theme-secondary :extend t))))

;;;;; sh-script
   '(sh-quoted-exec ((t (:inherit readable-mono-theme-secondary))))
   '(sh-heredoc ((t (:foreground unspecified))))

;;;;; spray
   `(spray-accent-face
     ((((background light)) (:foreground ,l-attention :underline (:color ,(face-foreground 'default)) :overline ,(face-foreground 'default)))
      (((background dark)) (:foreground ,d-attention :underline (:color ,(face-foreground 'default)) :overline ,(face-foreground 'default)))))

;;;;; stripe-buffer
   `(stripe-highlight
     ((((background light)) (:background ,l-bg-s :foreground ,l-fg :extend t))
      (((background dark)) (:background ,d-bg-s :foreground ,d-fg :extend t))))

;;;;; term
   `(term-color-black
     ((((background light)) (:foreground ,l-fg :background ,l-fg-s))
      (((background dark)) (:foreground ,d-fg :foreground ,d-fg-s))))
   `(term-color-red
     ((((background light)) (:foreground ,l-attention :background ,l-attention))
      (((background dark)) (:foreground ,d-attention :foreground ,d-attention))))
   '(term-color-green ((t (:inherit term-color-black))))
   '(term-color-yellow ((t (:inherit shadow))))
   '(term-color-blue ((t (:inherit term-color-black))))
   '(term-color-magenta ((t (:inherit term-color-black))))
   '(term-color-cyan ((t (:inherit term-color-black))))
   '(term-color-white ((t (:inherit shadow))))

;;;;; terraform
   '(terraform-resource-name-face ((t (:foreground unspecified))))
   '(terraform-resource-type-face ((t (:foreground unspecified))))

;;;;; visual-replace
   '(visual-replace-region ((t (:inherit highlight))))
   '(visual-replace-delete-match ((t (:inverse-video t :strike-through t))))

;;;;; web
   '(web-mode-current-element-highlight-face ((t (:inherit show-paren-match))))

;;;;; wgrep
   '(wgrep-face ((t (:inverse-video t))))
   '(wgrep-delete-face ((t (:inverse-video t :strike-through t))))
   '(wgrep-file-face ((t (:inverse-video t))))
   '(wgrep-reject-face ((t (:inherit shadow))))
   '(wgrep-done-face ((t (:inherit readable-mono-theme-secondary)))))

;;;; Theme variables
  (custom-theme-set-variables
   'readable-mono

;;;;; hl-todo
   '(hl-todo-keyword-faces
     '(("TODO"  . (:inherit bold :box (:line-width 1)))
       ("FIXME" . (:inherit error :box (:line-width 1)))
       ("NOTE"  . (:box (:line-width 1)))))

;;;;; indent-bars
   '(indent-bars-color '(shadow :blend 0.4))

;;;;; rainbow-identifiers
   '(rainbow-identifiers-cie-l*a*b*-saturation 65)
   '(rainbow-identifiers-cie-l*a*b*-lightness 45)

;;;;; zoom-window
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
