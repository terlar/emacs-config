;;; readable-typo-theme.el --- Readable typographic theme -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2022 Terje Larsen
;; All rights reserved.

;; Author: Terje Larsen <terlar@gmail.com>
;; URL: https://github.com/terlar/emacs-config
;; Keywords: faces
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

;; readable-typo-theme is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later version.

;; readable-typo-theme is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Typographic theme to achieve a more readable and pleasant experience.

;;; Code:
(deftheme readable-typo "Readable typographic theme.")

(defgroup readable-typo-theme nil
  "Readable typographic theme customization options."
  :group 'faces)

;;;; Customization options
(defcustom readable-typo-theme-default-font-height 120
  "Default font height."
  :type 'string
  :group 'readable-typo-theme)

(defcustom readable-typo-theme-font-scaling t
  "Scale font sizes."
  :type 'boolean
  :group 'readable-typo-theme)

(defcustom readable-typo-theme-font-height-small 0.8
  "Small font height."
  :type 'string
  :group 'readable-typo-theme)

(defcustom readable-typo-theme-font-height-title 1.8
  "Title font height."
  :type 'string
  :group 'readable-typo-theme)

(defcustom readable-typo-theme-font-height-level-1 1.6
  "Level 1 font height."
  :type 'string
  :group 'readable-typo-theme)

(defcustom readable-typo-theme-font-height-level-2 1.4
  "Level 2 font height."
  :type 'string
  :group 'readable-typo-theme)

(defcustom readable-typo-theme-font-height-level-3 1.2
  "Level 3 font height."
  :type 'string
  :group 'readable-typo-theme)

(defcustom readable-typo-theme-font-height-level-4 1.1
  "Level 4 font height."
  :type 'string
  :group 'readable-typo-theme)

(defcustom readable-typo-theme-font-height-level-5 1.1
  "Level 5 font height."
  :type 'string
  :group 'readable-typo-theme)

(defcustom readable-typo-theme-font-height-level-6 1.1
  "Level 6 font height."
  :type 'string
  :group 'readable-typo-theme)

(defcustom readable-typo-theme-font-height-level-7 1.1
  "Level 7 font height."
  :type 'string
  :group 'readable-typo-theme)

(defcustom readable-typo-theme-font-height-level-8 1.1
  "Level 8 font height."
  :type 'string
  :group 'readable-typo-theme)

(defcustom readable-typo-theme-default-font-weight 'light
  "Default font weight."
  :type 'string
  :group 'readable-typo-theme)

(defcustom readable-typo-theme-bold-font-weight 'medium
  "Bold font weight."
  :type 'string
  :group 'readable-typo-theme)

(defcustom readable-typo-theme-line-spacing 0.25
  "Spacing between lines."
  :type 'number
  :group 'readable-typo-theme)

(defcustom readable-typo-theme-fixed-pitch-font "Monospace"
  "Font used for fixed-pitch."
  :type 'string
  :group 'readable-typo-theme)

(defcustom readable-typo-theme-fixed-pitch-serif-font "Monospace"
  "Font used for fixed-pitch serif."
  :type 'string
  :group 'readable-typo-theme)

(defcustom readable-typo-theme-variable-pitch-font "sans-serif"
  "Font used for variable-pitch."
  :type 'string
  :group 'readable-typo-theme)

(defcustom readable-typo-theme-serif-font "serif"
  "Font used for serif."
  :type 'string
  :group 'readable-typo-theme)

;;;; Faces
(defface readable-typo-theme-echo-area
  nil
  "Face used for echo area."
  :group 'readable-mono-theme)

(let ((default-height readable-typo-theme-default-font-height)
      (default-weight readable-typo-theme-default-font-weight)
      (bold-weight readable-typo-theme-bold-font-weight)
      (fixed-pitch readable-typo-theme-fixed-pitch-font)
      (fixed-pitch-serif readable-typo-theme-fixed-pitch-serif-font)
      (variable-pitch readable-typo-theme-variable-pitch-font)
      (serif readable-typo-theme-serif-font)
      (small-height (if readable-typo-theme-font-scaling readable-typo-theme-font-height-small nil))
      (title-height (if readable-typo-theme-font-scaling readable-typo-theme-font-height-title nil))
      (level-1-height (if readable-typo-theme-font-scaling readable-typo-theme-font-height-level-1 nil))
      (level-2-height (if readable-typo-theme-font-scaling readable-typo-theme-font-height-level-2 nil))
      (level-3-height (if readable-typo-theme-font-scaling readable-typo-theme-font-height-level-3 nil))
      (level-4-height (if readable-typo-theme-font-scaling readable-typo-theme-font-height-level-4 nil))
      (level-5-height (if readable-typo-theme-font-scaling readable-typo-theme-font-height-level-5 nil))
      (level-6-height (if readable-typo-theme-font-scaling readable-typo-theme-font-height-level-6 nil))
      (level-7-height (if readable-typo-theme-font-scaling readable-typo-theme-font-height-level-7 nil))
      (level-8-height (if readable-typo-theme-font-scaling readable-typo-theme-font-height-level-8 nil)))

;;;; Theme faces
  (custom-theme-set-faces
   'readable-typo

   `(default ((t (:height ,default-height :family ,fixed-pitch :weight ,default-weight))))

   `(bold ((t (:weight ,bold-weight))))
   '(bold-italic ((t (:inherit bold))))

   `(fixed-pitch ((t (:family ,fixed-pitch :weight ,default-weight))))
   `(fixed-pitch-serif ((t (:family ,fixed-pitch-serif :weight ,default-weight))))
   `(variable-pitch ((t (:family ,variable-pitch :weight ,default-weight))))
   `(variable-pitch-text ((t (:height 1.1 :family ,serif))))

   '(link ((t (:inherit underline))))

   `(readable-typo-theme-echo-area ((t (:family ,variable-pitch :weight ,default-weight))))

;;;;; Interface
   `(mode-line-active ((t (:family ,variable-pitch :height ,small-height))))
   `(mode-line-inactive ((t (:family ,variable-pitch :height ,small-height))))
   `(header-line ((t (:family ,variable-pitch :height ,small-height))))
   `(line-number ((t (:family ,fixed-pitch))))
   `(whitespace-space ((t (:family ,fixed-pitch))))
   `(help-key-binding ((t (:inherit fixed-pitch))))

;;;;; font-lock
   '(font-lock-comment-face ((t (:inherit italic))))
   '(font-lock-type-face ((t (:inherit bold))))
   '(font-lock-builtin-face ((t (:inherit bold))))
   '(font-lock-keyword-face ((t (:inherit bold))))

;;;;; corfu
   `(corfu-default ((t (:family ,fixed-pitch))))

;;;;; Info
   `(info-title-1 ((t (:height ,level-1-height))))
   `(info-title-2 ((t (:height ,level-2-height))))
   `(info-title-3 ((t (:height ,level-3-height))))
   `(info-title-4 ((t (:height ,level-4-height :slant italic))))

;;;;; magit
   `(magit-section-heading ((t (:family ,variable-pitch :height ,level-2-height))))

;;;;; markdown
   `(markdown-header-face-1 ((t (:height ,level-1-height :weight ,bold-weight))))
   `(markdown-header-face-2 ((t (:height ,level-2-height :weight ,bold-weight))))
   `(markdown-header-face-3 ((t (:height ,level-3-height :weight ,bold-weight))))
   `(markdown-header-face-4 ((t (:height ,level-4-height :weight ,bold-weight :slant italic))))
   `(markdown-header-face-5 ((t (:height ,level-5-height :weight ,bold-weight :slant italic))))
   `(markdown-header-face-6 ((t (:height ,level-6-height :weight ,bold-weight :slant italic))))
   `(markdown-code-face ((t (:family ,fixed-pitch))))
   `(markdown-gfm-checkbox-face ((t (:family ,fixed-pitch))))
   `(markdown-inline-code-face ((t (:family ,fixed-pitch))))
   `(markdown-markup-face ((t (:family ,fixed-pitch))))
   `(markdown-pre-face ((t (:family ,fixed-pitch))))
   `(markdown-table-face ((t (:family ,fixed-pitch))))

;;;;; org
   `(org-document-title ((t (:height ,title-height :weight ,bold-weight))))
   `(org-level-1 ((t (:height ,level-1-height :weight ,bold-weight))))
   `(org-level-2 ((t (:height ,level-2-height :weight ,bold-weight))))
   `(org-level-3 ((t (:height ,level-3-height :weight ,bold-weight))))
   `(org-level-4 ((t (:height ,level-4-height :weight ,bold-weight :slant italic))))
   `(org-level-5 ((t (:height ,level-5-height :weight ,bold-weight :slant italic))))
   `(org-level-6 ((t (:height ,level-6-height :weight ,bold-weight :slant italic))))
   `(org-level-7 ((t (:height ,level-7-height :weight ,bold-weight :slant italic))))
   `(org-level-8 ((t (:height ,level-8-height :weight ,bold-weight :slant italic))))
   `(org-list-dt ((t (:weight ,bold-weight))))
   `(org-block ((t (:family ,fixed-pitch))))
   `(org-checkbox ((t (:family ,fixed-pitch))))
   `(org-code ((t (:family ,fixed-pitch))))
   `(org-meta-line ((t (:family ,fixed-pitch))))
   `(org-table ((t (:family ,fixed-pitch))))
   `(org-verbatim ((t (:family ,fixed-pitch))))

   `(org-agenda-structure ((t (:family ,variable-pitch :height ,level-1-height))))

;;;;; outline
   `(outline-1 ((t (:height ,level-1-height :weight ,bold-weight))))
   `(outline-2 ((t (:height ,level-2-height :weight ,bold-weight))))
   `(outline-3 ((t (:height ,level-3-height :weight ,bold-weight))))
   `(outline-4 ((t (:height ,level-4-height :weight ,bold-weight :slant italic))))
   `(outline-5 ((t (:height ,level-5-height :weight ,bold-weight :slant italic))))
   `(outline-6 ((t (:height ,level-6-height :weight ,bold-weight :slant italic))))
   `(outline-7 ((t (:height ,level-7-height :weight ,bold-weight :slant italic))))
   `(outline-8 ((t (:height ,level-8-height :weight ,bold-weight :slant italic))))
   `(outline-minor-0 ((t (:family ,serif))))

;;;;; rst
   `(rst-level-1 ((t (:height ,level-1-height :weight ,bold-weight))))
   `(rst-level-2 ((t (:height ,level-2-height :weight ,bold-weight))))
   `(rst-level-3 ((t (:height ,level-3-height :weight ,bold-weight))))
   `(rst-level-4 ((t (:height ,level-4-height :weight ,bold-weight :slant italic))))
   `(rst-level-5 ((t (:height ,level-5-height :weight ,bold-weight :slant italic))))
   `(rst-level-6 ((t (:height ,level-6-height :weight ,bold-weight :slant italic))))
   `(rst-literal ((t (:family ,fixed-pitch))))

;;;;; spray
   `(spray-base-face ((t (:inherit default :family ,serif :weight ,bold-weight)))))

;;;; Theme variables
  (custom-theme-set-variables
   'readable-typo
   `(line-spacing ,readable-typo-theme-line-spacing)
   `(x-underline-at-descent-line t)))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'readable-typo)
(provide 'readable-typo-theme)
;;; readable-typo-theme.el ends here
