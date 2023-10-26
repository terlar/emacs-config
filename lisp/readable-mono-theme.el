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
;;
;; TODO faces:
;; - ivy
;; - edebug
;; - hi
;; - rainbow-delimiters
;; - hl-todo
;; - ert

;;; Code:
(deftheme readable-mono "Minimal and monochromatic theme.")

(defgroup readable-mono-theme nil
  "Minimal and monochromatic theme customization options."
  :group 'faces)

;;;; Light theme
(defgroup readable-mono-theme-light nil
  "Minimal and monochromatic light theme customization options."
  :group 'readable-mono-theme)

(defcustom readable-mono-theme-light-cursor "#bb3e06"
  "Cursor for light theme."
  :type 'string
  :group 'readable-mono-theme-light)

(defcustom readable-mono-theme-light-attention "#cc1f24"
  "Main attention color for light theme."
  :type 'string
  :group 'readable-mono-theme-light)

(defcustom readable-mono-theme-light-secondary-attention "#c42475"
  "Secondary attention color for light theme."
  :type 'string
  :group 'readable-mono-theme-light)

(defcustom readable-mono-theme-light-foreground "#002b37"
  "Default foreground for light theme."
  :type 'string
  :group 'readable-mono-theme-light)

(defcustom readable-mono-theme-light-secondary-foreground "#596e76"
  "Secondary foreground for light theme."
  :type 'string
  :group 'readable-mono-theme-light)

(defcustom readable-mono-theme-light-background "#fffce9"
  "Default background for light theme."
  :type 'string
  :group 'readable-mono-theme-light)

(defcustom readable-mono-theme-light-secondary-background "#f4eedb"
  "Secondary background for light theme."
  :type 'string
  :group 'readable-mono-theme-light)

;;;; Dark theme
(defgroup readable-mono-theme-dark nil
  "Minimal and monochromatic dark theme customization options."
  :group 'readable-mono-theme)

(defcustom readable-mono-theme-dark-cursor "#db5823"
  "Cursor for dark theme."
  :type 'string
  :group 'readable-mono-theme-dark)

(defcustom readable-mono-theme-dark-attention "#ec423a"
  "Main attention color for dark theme."
  :type 'string
  :group 'readable-mono-theme-dark)

(defcustom readable-mono-theme-dark-secondary-attention "#e2468f"
  "Secondary attention color for dark theme."
  :type 'string
  :group 'readable-mono-theme-dark)

(defcustom readable-mono-theme-dark-foreground "#ffffee"
  "Default foreground for dark theme."
  :type 'string
  :group 'readable-mono-theme-dark)

(defcustom readable-mono-theme-dark-secondary-foreground "#8d9fa1"
  "Secondary foreground for dark theme."
  :type 'string
  :group 'readable-mono-theme-dark)

(defcustom readable-mono-theme-dark-background "#002732"
  "Default background for dark theme."
  :type 'string
  :group 'readable-mono-theme-dark)

(defcustom readable-mono-theme-dark-secondary-background "#01323d"
  "Secondary background for dark theme."
  :type 'string
  :group 'readable-mono-theme-dark)

;;;; Faces
(defface readable-mono-theme-critical
  `((((background light)) (:background ,readable-mono-theme-light-attention :foreground "#ffffff"))
    (((background dark)) (:background ,readable-mono-theme-dark-attention :foreground "#ffffff")))
  "Face used for critical information that requires immediate action/attention."
  :group 'readable-mono-theme)

(defface readable-mono-theme-emphasis
  `((((background light)) (:foreground ,readable-mono-theme-light-secondary-attention))
    (((background dark)) (:foreground ,readable-mono-theme-dark-secondary-attention)))
  "Face used for information that needs action/attention."
  :group 'readable-mono-theme)

(defface readable-mono-theme-strong
  '((t (:weight bold)))
  "Face used for information of strong importance."
  :group 'readable-mono-theme)

(defface readable-mono-theme-subordinate
  `((((background light)) (:foreground ,readable-mono-theme-light-secondary-foreground))
    (((background dark)) (:foreground ,readable-mono-theme-dark-secondary-foreground)))
  "Face used for information of less importance."
  :group 'readable-mono-theme)

(defface readable-mono-theme-actionable
  '((((background light)) (:foreground "#007ec4"))
    (((background dark)) (:foreground "#3c98e0")))
  "Face used for information that is actionable.
For example links."
  :group 'readable-mono-theme)

(defface readable-mono-theme-secondary
  `((((background light)) (:background ,readable-mono-theme-light-secondary-background))
    (((background dark)) (:background ,readable-mono-theme-dark-secondary-background)))
  "Face used to distinguish from default but not stand out."
  :group 'readable-mono-theme)

(let ((l-bg readable-mono-theme-light-background)
      (l-bg-s readable-mono-theme-light-secondary-background)
      (l-fg readable-mono-theme-light-foreground)
      (l-fg-s readable-mono-theme-light-secondary-foreground)
      (l-attention readable-mono-theme-light-attention)
      (l-cursor readable-mono-theme-light-cursor)

      (l-green "#eeedcb")
      (l-green-s "#d5d99d")
      (l-red "#ffe1cb")
      (l-red-s "#ffb79f")
      (l-blue "#e6ebe7")
      (l-blue-s "#bfd2e6")

      (d-bg readable-mono-theme-dark-background)
      (d-bg-s readable-mono-theme-dark-secondary-background)
      (d-fg readable-mono-theme-dark-foreground)
      (d-fg-s readable-mono-theme-dark-secondary-foreground)
      (d-attention readable-mono-theme-dark-attention)
      (d-cursor readable-mono-theme-dark-cursor)

      (d-green "#1e3531")
      (d-green-s "#354725")
      (d-red "#2f2c31")
      (d-red-s "#582b29")
      (d-blue "#023447")
      (d-blue-s "#004363"))
;;;; Theme faces
  (custom-theme-set-faces
   'readable-mono
   `(default
      ((((type graphic) (background light)) (:background ,l-bg :foreground ,l-fg))
       (((type graphic) (background dark)) (:background ,d-bg :foreground ,d-fg))
       (((type tty) (background light)) (:background unspecified :foreground ,l-fg))
       (((type tty) (background dark)) (:background unspecified :foreground ,d-fg))))

;;;;; Semantic
   `(bold ((t (:inherit readable-mono-theme-strong))))
   `(bold-italic ((t (:inherit readable-mono-theme-strong))))
   `(link ((t (:inherit readable-mono-theme-actionable :underline t))))
   `(shadow ((t (:inherit readable-mono-theme-subordinate))))
   `(success ((t (:foreground unspecified))))
   `(warning ((t (:inherit readable-mono-theme-emphasis))))
   `(error ((t (:inherit readable-mono-theme-critical))))
   `(font-lock-comment-face ((t (:italic t))))
   `(font-lock-function-name-face ((t (:foreground unspecified))))
   `(font-lock-variable-name-face ((t (:foreground unspecified))))
   `(font-lock-constant-face ((t (:foreground unspecified))))
   `(font-lock-doc-string-face ((t (:foreground unspecified))))
   `(font-lock-doc-face ((t (:foreground unspecified))))
   `(font-lock-preprocessor-face ((t (:foreground unspecified))))
   `(font-lock-reference-face ((t (:foreground unspecified))))
   `(font-lock-string-face ((t (:foreground unspecified))))
   `(font-lock-type-face ((t (:inherit readable-mono-theme-strong))))
   `(font-lock-builtin-face ((t (:inherit readable-mono-theme-strong))))
   `(font-lock-keyword-face ((t (:inherit readable-mono-theme-strong))))
   `(font-lock-warning-face ((t (:inherit warning))))

   `(semantic-tag-boundary-face ((((background light)) (:overline ,l-fg-s))
				 (((background dark)) (:overline ,d-fg-s))))
   `(semantic-decoration-on-unparsed-includes ((t (:inherit readable-mono-theme-subordinate))))
   `(semantic-decoration-on-unknown-includes ((((background light)) (:background ,l-red-s))
					      (((background dark)) (:background ,d-red-s))))

;;;;; Headlines
   `(custom-group-tag ((t (:inherit readable-mono-theme-strong))))
   `(custom-state ((t (:inherit readable-mono-theme-emphasis))))
   `(custom-variable-tag ((t (:inherit readable-mono-theme-strong))))

;;;;; Search and highlight
   `(completions-annotations ((t (:inherit italic))))
   `(completions-common-part ((t (:inherit readable-mono-theme-strong))))
   `(completions-first-difference ((t (:inherit readable-mono-theme-emphasis))))
   `(highlight ((t (:inverse-video t))))
   `(isearch ((t (:inherit highlight))))
   `(isearch-fail ((t (:inherit error))))
   `(lazy-highlight ((t (:inherit readable-mono-theme-subordinate :inverse-video t))))
   `(match ((t (:inherit highlight))))
   `(region
     ((((background light)) (:foreground ,l-bg :background ,l-fg))
      (((background dark)) (:foreground ,d-bg :background ,d-fg))))

;;;;; Visual aid
   `(cursor
     ((((background light)) (:background ,l-cursor))
      (((background dark)) (:background ,d-cursor))))
   `(hl-line ((t (:inherit readable-mono-theme-secondary))))
   `(line-number-current-line
     ((((background light)) (:inherit readable-mono-theme-strong :foreground ,l-cursor))
      (((background dark)) (:inherit readable-mono-theme-strong :foreground ,d-cursor))))
   `(show-paren-match ((t (:inherit readable-mono-theme-strong :underline t))))
   `(show-paren-mismatch ((t (:inherit error))))
   `(trailing-whitespace ((t (:inherit error))))
   `(whitespace-line ((t (:inherit error))))
   `(whitespace-trailing ((t (:inherit error))))

;;;;; Interface
   `(mode-line-active
     ((((background light)) (:background ,l-bg-s :box (:line-width 6 :color ,l-bg-s)))
      (((background dark)) (:background ,d-bg-s :box (:line-width 6 :color ,d-bg-s)))))
   `(mode-line-inactive
     ((((type graphic) (background light)) (:background ,l-bg :box (:line-width 6 :color ,l-bg)))
      (((type graphic) (background dark)) (:background ,d-bg :box (:line-width 6 :color ,d-bg)))
      (((type tty) (background light)) (:background unspecified :foreground ,l-fg-s))
      (((type tty) (background dark)) (:background unspecified :foreground ,d-fg-s))))
   `(mode-line-emphasis ((t :bold nil)))
   `(mode-line-buffer-id ((t :inherit readable-mono-theme-strong)))

   `(button ((t (:inherit readable-mono-theme-actionable :underline t))))
   `(custom-button-face ((t (:inherit button))))
   `(fringe ((t (:inherit readable-mono-theme-subordinate))))
   `(header-line
     ((((background light)) (:foreground ,l-bg :background ,l-fg :box (:line-width 6 :color ,l-fg)))
      (((background dark)) (:foreground ,d-bg :background ,d-fg :box (:line-width 6 :color ,d-fg)))))
   `(menu ((t (:inherit readable-mono-theme-secondary))))
   `(minibuffer-prompt ((t (:inherit readable-mono-theme-strong))))
   `(secondary-selection ((t (:inherit (region highlight)))))
   `(tool-bar ((t (:inherit readable-mono-theme-secondary))))
   `(tooltip ((t (:inherit readable-mono-theme-secondary))))
   `(vertical-border
     ((((background light)) (:foreground ,l-bg-s))
      (((background dark)) (:foreground ,d-bg-s))))
   `(widget-button ((t (:inherit button))))
   `(widget-field
     ((((background light)) (:inherit readable-mono-theme-secondary :box (:line-width 1 :color ,l-bg)))
      (((background dark)) (:inherit readable-mono-theme-secondary :box (:line-width 1 :color ,d-bg)))))
   `(window-divider ((t (:inherit vertical-border))))
   `(window-divider-first-pixel ((t (:inherit window-divider))))
   `(window-divider-last-pixel ((t (:inherit window-divider))))

;;;;; Diff
   `(diff-header ((t (:inherit readable-mono-theme-secondary))))
   `(diff-file-header ((t (:inherit header-line))))
   `(diff-hunk-header ((t (:inherit shadow))))
   `(diff-indicator-removed
     ((((background light)) (:background ,l-red :foreground ,l-red))
      (((background dark)) (:background ,d-red :foreground ,d-red))))
   `(diff-removed
     ((((background light)) (:background ,l-red))
      (((background dark)) (:background ,d-red))))
   `(diff-refine-removed
     ((((background light)) (:background ,l-red-s))
      (((background dark)) (:background ,d-red-s))))
   `(diff-indicator-added
     ((((background light)) (:background ,l-green :foreground ,l-green))
      (((background dark)) (:background ,d-green :foreground ,d-green))))
   `(diff-added
     ((((background light)) (:background ,l-green))
      (((background dark)) (:background ,d-green))))
   `(diff-refine-added
     ((((background light)) (:background ,l-green-s))
      (((background dark)) (:background ,d-green-s))))
   `(diff-indicator-changed
     ((((background light)) (:background ,l-blue :foreground ,l-blue))
      (((background dark)) (:background ,d-blue :foreground ,d-blue))))
   `(diff-refine-changed ((t (:inverse-video t))))
   `(diff-error ((t (:inherit error))))

   `(ediff-current-diff-A ((t (:inherit diff-removed))))
   `(ediff-fine-diff-A ((t (:inherit diff-refine-removed))))
   `(ediff-current-diff-B ((t (:inherit diff-added))))
   `(ediff-fine-diff-B ((t (:inherit diff-refine-added))))
   `(ediff-current-diff-C
     ((((background light)) (:background ,l-blue))
      (((background dark)) (:background ,d-blue))))
   `(ediff-fine-diff-C
     ((((background light)) (:background ,l-blue-s))
      (((background dark)) (:background ,d-blue-s))))
   `(ediff-current-diff-Ancestor ((t (:inherit readable-mono-theme-secondary))))
   `(ediff-fine-diff-Ancestor ((t (:inherit highlight))))

   `(smerge-base ((t (:inherit readable-mono-theme-secondary))))
   `(smerge-lower ((t (:background unspecified))))
   `(smerge-upper ((t (:background unspecified))))
   `(smerge-refined-added ((t (:inherit diff-refine-added))))
   `(smerge-refined-changed ((t (:inherit diff-refine-changed))))
   `(smerge-refined-removed ((t (:inherit diff-refine-removed))))

   `(diff-hl-insert ((t (:inherit diff-indicator-added))))
   `(diff-hl-change
     ((((background light)) (:background ,l-blue :foreground ,l-blue))
      (((background dark)) (:background ,d-blue :foreground ,d-blue))))
   `(diff-hl-delete ((t (:inherit diff-indicator-removed))))
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
   `(ansi-color-bright-red ((t (:inherit ansi-color-red))))
   `(ansi-color-green ((t (:inherit success))))
   `(ansi-color-bright-green ((t (:inherit ansi-color-green))))
   `(ansi-color-yellow ((t (:inherit shadow))))
   `(ansi-color-bright-yellow ((t (:inherit ansi-color-yellow))))
   `(ansi-color-blue ((t (:inherit ansi-color-black))))
   `(ansi-color-bright-blue ((t (:inherit ansi-color-blue))))
   `(ansi-color-magenta ((t (:inherit ansi-color-black))))
   `(ansi-color-bright-magenta ((t (:inherit ansi-color-magenta))))
   `(ansi-color-cyan ((t (:inherit ansi-color-black))))
   `(ansi-color-bright-cyan ((t (:inherit ansi-color-cyan))))
   `(ansi-color-white ((t (:inherit shadow))))
   `(ansi-color-bright-white ((t (:inherit ansi-color-white))))

;;;;; cider
   `(cider-test-failure-face ((t (:inherit error))))
   `(cider-result-overlay-face ((t (:inherit highlight))))

;;;;; company
   `(company-echo-common ((t (:inherit completions-common-part))))
   `(company-preview ((t (:inherit shadow))))
   `(company-preview-common ((t (:inherit completions-common-part))))
   `(company-preview-search ((t (:inverse-video t))))
   `(company-scrollbar-bg ((t (:inherit region))))
   `(company-scrollbar-fg ((t (:inherit cursor))))
   `(company-tooltip ((t (:inherit readable-mono-theme-secondary))))
   `(company-tooltip-annotation ((t (:inherit shadow))))
   `(company-tooltip-common ((t (:inherit completions-common-part))))
   `(company-tooltip-selection ((t (:inherit highlight))))

;;;;; compilation
   `(compilation-mode-line-fail ((t (:inherit compilation-error))))

;;;;; corfu
   `(corfu-default ((t (:inherit readable-mono-theme-secondary))))
   `(corfu-bar ((t (:inherit nil :inverse-video t))))
   `(corfu-current ((t (:inherit highlight))))

;;;;; cov
   `(cov-none-face
     ((((background light)) (:foreground ,l-red-s))
      (((background dark)) (:foreground ,d-red-s))))
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
     ((((background light)) (:foreground ,l-red-s))
      (((background dark)) (:foreground ,d-red-s))))
   `(cov-coverage-run-face
     ((((background light)) (:foreground ,l-green-s))
      (((background dark)) (:foreground ,d-green-s))))

;;;;; dired
   `(all-the-icons-dired-dir-face ((t (:foreground unspecified))))
   `(dired-directory ((t (:inherit readable-mono-theme-strong))))
   `(dired-broken-symlink ((t (:inherit error))))
   `(dired-flagged ((t (:inherit readable-mono-theme-emphasis))))

;;;;; erc
   `(erc-current-nick-face ((t (:inherit readable-mono-theme-emphasis))))
   `(erc-input-face ((t (:inherit readable-mono-theme-secondary :extend t))))
   `(erc-my-nick-face ((t (:inherit readable-mono-theme-emphasis))))
   `(erc-nick-default-face ((t (:inherit readable-mono-theme-strong))))
   `(erc-notice-face ((t (:inherit nil))))
   `(erc-prompt-face ((t (:inherit readable-mono-theme-strong))))
   `(erc-timestamp-face ((t (:inherit readable-mono-theme-subordinate))))

;;;;; eros
   `(eros-result-overlay-face ((t (:inherit highlight))))

;;;;; eshell
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

;;;;; flymake
   `(flymake-error
     ((((background light)) (:background ,l-red :extend t))
      (((background dark)) (:background ,d-red :extend t))))

;;;;; flyspell
   `(flyspell-duplicate
     ((((background light)) (:underline (:style wave :color ,l-fg-s)))
      (((background dark)) (:underline (:style wave :color ,d-fg-s)))))
   `(flyspell-incorrect
     ((((background light)) (:underline (:style wave :color ,l-fg)))
      (((background dark)) (:underline (:style wave :color ,d-fg)))))

;;;;; ghelp
   `(ghelp-header-button ((t (:inherit info-header-node :box nil))))

;;;;; gotest
   `(go-test--standard-face ((t (:foreground unspecified))))
   `(go-test--ok-face ((t (:foreground unspecified))))
   `(go-test--error-face ((t (:inherit error))))
   `(go-test--pointer-face ((t (:foreground unspecified))))
   `(go-test--warning-face ((t (:inherit warning))))

;;;;; haskell
   `(haskell-interactive-face-prompt ((t (:inherit readable-mono-theme-strong))))

;;;;; indent-guide
   `(indent-guide-face ((t (:inherit fringe))))

;;;;; Info
   `(info-header-node ((t (:inherit readable-mono-theme-strong))))

;;;;; ivy
   `(ivy-current-match ((t (:inherit highlight))))
   `(ivy-minibuffer-match-face-1 ((t (:inherit readable-mono-theme-strong))))
   `(ivy-minibuffer-match-face-2 ((t (:inherit readable-mono-theme-strong))))
   `(ivy-minibuffer-match-face-3 ((t (:inherit readable-mono-theme-strong))))
   `(ivy-minibuffer-match-face-4 ((t (:inherit readable-mono-theme-strong))))

;;;;; magit
   `(magit-section-heading-selection ((t (:inherit region))))
   `(magit-section-highlight ((t (:inherit readable-mono-theme-secondary))))
   `(magit-diff-file-heading-selection ((t (:inherit region))))
   `(magit-diff-context ((t (:background unspecified))))
   `(magit-diff-context-highlight ((t (:inherit readable-mono-theme-secondary))))
   `(magit-diff-removed ((t (:inherit diff-removed))))
   `(magit-diff-removed-highlight ((t (:inherit diff-refine-removed))))
   `(magit-diff-added ((t (:inherit diff-added))))
   `(magit-diff-added-highlight ((t (:inherit diff-refine-added))))
   `(magit-process-ok ((t (:inherit success))))
   `(magit-process-ng ((t (:inherit error))))

   `(magit-branch-current ((t (:inherit readable-mono-theme-emphasis :box (:line-width 1)))))
   `(magit-branch-local ((t (:inherit readable-mono-theme-emphasis))))
   `(magit-branch-remote ((t (:inherit readable-mono-theme-strong))))
   `(magit-head ((t (:inherit readable-mono-theme-strong))))
   `(magit-tag ((t (:inherit italic))))

;;;;; marginalia
   `(marginalia-file-priv-no ((t (:inherit nil))))

;;;;; markdown
   `(markdown-code-face ((t (:inherit readable-mono-theme-secondary :extend t))))
   `(markdown-inline-code-face
     ((((background light)) (:inherit readable-mono-theme-secondary :box (:line-width 1 :color ,l-bg)))
      (((background dark)) (:inherit readable-mono-theme-secondary :box (:line-width 1 :color ,d-bg)))))

;;;;; message
   `(message-cited-text-1 ((t (:foreground unspecified))))
   `(message-cited-text-2 ((t (:foreground unspecified))))
   `(message-cited-text-3 ((t (:foreground unspecified))))
   `(message-cited-text-4 ((t (:foreground unspecified))))
   `(message-header-cc ((t (:foreground unspecified))))
   `(message-header-name ((t (:foreground unspecified))))
   `(message-header-newsgroups ((t (:inherit readable-mono-theme-strong))))
   `(message-header-other ((t (:foreground unspecified))))
   `(message-header-subject ((t (:inherit readable-mono-theme-strong))))
   `(message-header-to ((t (:inherit readable-mono-theme-strong))))
   `(message-header-xheader ((t (:foreground unspecified))))
   `(message-mml ((t (:foreground unspecified))))
   `(message-separator ((t (:inherit readable-mono-theme-subordinate))))

;;;;; orderless
   `(orderless-match-face-0 ((t (:inherit completions-common-part))))
   `(orderless-match-face-1 ((t (:inherit completions-common-part))))
   `(orderless-match-face-2 ((t (:inherit completions-common-part))))
   `(orderless-match-face-3 ((t (:inherit completions-common-part))))

;;;;; org
   `(org-ellipsis ((t (:inherit readable-mono-theme-subordinate))))
   `(org-done ((t (:foreground unspecified))))
   `(org-todo ((t (:inherit readable-mono-theme-strong))))
   `(org-headline-done ((t (:foreground unspecified))))
   `(org-headline-todo ((t (:foreground unspecified))))
   `(org-block ((t (:inherit readable-mono-theme-secondary :extend t))))
   `(org-code
     ((((background light)) (:inherit readable-mono-theme-secondary :box (:line-width 1 :color ,l-bg)))
      (((background dark)) (:inherit readable-mono-theme-secondary :box (:line-width 1 :color ,d-bg)))))
   `(org-date ((t (:foreground unspecified :underline nil))))
   `(org-document-info ((t (:foreground unspecified))))
   `(org-drawer ((t (:inherit readable-mono-theme-subordinate))))
   `(org-hide
     ((((background light)) (:foreground ,l-bg))
      (((background dark)) (:foreground ,d-bg))))
   `(org-table ((t (:foreground unspecified))))
   `(org-quote ((t (:foreground unspecified))))

   `(org-agenda-structure ((t (:foreground unspecified))))
   `(org-agenda-date ((t (:foreground unspecified))))
   `(org-agenda-date-today ((t (:inherit readable-mono-theme-strong))))
   `(org-agenda-date-weekend ((t (:inherit readable-mono-theme-emphasis))))
   `(org-scheduled ((t (:foreground unspecified))))
   `(org-scheduled-today ((t (:inherit readable-mono-theme-actionable))))
   `(org-scheduled-previously ((t (:inherit readable-mono-theme-emphasis))))

;;;;; org-tree-slide
   `(org-tree-slide-header-overlay-face ((t (:inherit header-line))))

;;;;; outline
   `(outline-minor-0 ((t (:background unspecified))))

;;;;; popup
   `(popup-face ((t (:inherit (readable-mono-theme-secondary default)))))
   `(popup-isearch-match ((t (:inherit readable-mono-theme-strong))))
   `(popup-menu-mouse-face ((t (:underline t))))
   `(popup-menu-selection-face ((t (:inherit (highlight default)))))
   `(popup-scroll-bar-background-face ((t (:inherit region))))
   `(popup-scroll-bar-foreground-face ((t (:inherit cursor))))
   `(popup-summary-face ((t (:inherit (popup-face readable-mono-theme-subordinate)))))
   `(popup-tip-face
     ((((background light)) (:inherit default :background ,l-bg-s :box (:line-width 6 :color ,l-bg-s)))
      (((background dark)) (:inherit default :background ,d-bg-s :box (:line-width 6 :color ,d-bg-s)))))

;;;;; quick-peek
   `(quick-peek-background-face ((t :inherit readable-mono-theme-secondary)))

;;;;; rainbow-delimiters
   `(rainbow-delimiters-unmatched-face ((t (:inherit readable-mono-theme-critical))))

;;;;; rst
   `(rst-directive ((t (:inherit font-lock-comment-face))))
   `(rst-external ((t (:inherit font-lock-comment-face))))
   `(rst-literal ((t (:inherit readable-mono-theme-secondary :extend t))))

;;;;; sh-script
   `(sh-quoted-exec ((t (:inherit readable-mono-theme-secondary))))

;;;;; spray
   `(spray-accent-face
     ((((background light)) (:foreground ,l-cursor :underline (:color ,(face-foreground 'default)) :overline ,(face-foreground 'default)))
      (((background dark)) (:foreground ,d-cursor :underline (:color ,(face-foreground 'default)) :overline ,(face-foreground 'default)))))

;;;;; stripe-buffer
   `(stripe-highlight ((t (:inherit region))))

;;;;; term
   `(term-color-black
     ((((background light)) (:foreground ,l-fg :background ,l-fg-s))
      (((background dark)) (:foreground ,d-fg :foreground ,d-fg-s))))
   `(term-color-red
     ((((background light)) (:foreground ,l-attention :background ,l-attention))
      (((background dark)) (:foreground ,d-attention :foreground ,d-attention))))
   `(term-color-green ((t (:inherit term-color-black))))
   `(term-color-yellow ((t (:inherit shadow))))
   `(term-color-blue ((t (:inherit term-color-black))))
   `(term-color-magenta ((t (:inherit term-color-black))))
   `(term-color-cyan ((t (:inherit term-color-black))))
   `(term-color-white ((t (:inherit shadow))))

;;;;; terraform
   `(terraform-resource-name-face ((t (:foreground unspecified))))
   `(terraform-resource-type-face ((t (:foreground unspecified))))

;;;;; visual-regexp
   `(vr/match-0 ((t (:inverse-video t))))
   `(vr/match-1 ((t (:inherit readable-mono-theme-subordinate :inverse-video t))))

;;;;; web
   `(web-mode-current-element-highlight-face ((t (:inherit show-paren-match))))

;;;;; wgrep
   `(wgrep-face ((t (:inverse-video t))))
   `(wgrep-delete-face ((t (:inverse-video t :strike-through t))))
   `(wgrep-file-face ((t (:inverse-video t))))
   `(wgrep-reject-face ((t (:inherit readable-mono-theme-subordinate))))
   `(wgrep-done-face ((t (:inherit readable-mono-theme-secondary)))))

;;;; Theme variables
  (custom-theme-set-variables
   'readable-mono
;;;;; hl-todo
   `(hl-todo-keyword-faces
     `(("TODO"  . (:inherit readable-mono-theme-strong :box (:line-width 1)))
       ("FIXME" . (:inherit readable-mono-theme-emphasis :box (:line-width 1)))
       ("NOTE"  . (:box (:line-width 1)))))

;;;;; rainbow-identifiers
   `(rainbow-identifiers-cie-l*a*b*-saturation 65)
   `(rainbow-identifiers-cie-l*a*b*-lightness 45)

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
