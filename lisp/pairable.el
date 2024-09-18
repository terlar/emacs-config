;;; pairable.el --- Optimize for pair-programming -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2022 Terje Larsen
;; All rights reserved.

;; Author: Terje Larsen <terlar@gmail.com>
;; Keywords: faces
;; URL: https://github.com/terlar/emacs-config/blob/main/lisp/pariable.el
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; pariable is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; pairable is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `pairable' is a small minor mode that optimizes for pair-programming, this is
;; achieved by Increase font-sizes, enable line-numbering, e.t.c.

;;; Code:

(require 'face-remap)

(defgroup pairable nil
  "Settings for pair-programming."
  :group 'faces)

;;;###autoload
(defcustom pairable-text-scale 2
  "Scaling increment for text.
Will be multiplied with `global-text-scale-adjust--increment-factor'"
  :type 'number
  :group 'pairable)

;;;###autoload
(defcustom pairable-display-line-numbers t
  "Enable line-numbers or not."
  :type 'boolean
  :group 'pairable)

;;;###autoload
(defcustom pairable-hl-line t
  "Enable highlighting of the current line or not."
  :type 'boolean
  :group 'pairable)

;;;###autoload
(defcustom pairable-lighter " Pairable"
  "Mode-line indicator for `pairable-mode'."
  :type '(choice (const :tag "No lighter" "") string)
  :group 'pairable)

;;;###autoload
(define-minor-mode pairable-mode
  "Toggle Pariable mode.

In Pariable mode, the text scale is increased, line numbers enabled and various
other improvements to optimize for pair-programming."
  :lighter pairable-lighter
  :global t
  :group 'pairable
  (if pairable-mode
      (progn
        (when pairable-display-line-numbers
          (global-display-line-numbers-mode 1))
        (when pairable-hl-line
          (global-hl-line-mode 1))
        (when (> pairable-text-scale 0)
          (let ((inc (* global-text-scale-adjust--increment-factor pairable-text-scale)))
            (setq global-text-scale-adjust--default-height (face-attribute 'default :height))
            (set-face-attribute 'default nil :height (+ global-text-scale-adjust--default-height inc))
            (redisplay 'force))))
    (progn
      (when pairable-display-line-numbers
        (global-display-line-numbers-mode 0))
      (when pairable-hl-line
        (global-hl-line-mode 0))
      (when (> pairable-text-scale 0)
        (set-face-attribute 'default nil :height global-text-scale-adjust--default-height)
        (redisplay 'force)))))

(defun pairable-mode-enable ()
  "Enable `pairable-mode' in the current buffer."
  (unless (minibufferp)
    (pairable-mode 1)))

;;;###autoload
(define-global-minor-mode global-pairable-mode
  pairable-mode pairable-mode-enable
  :require 'pairable
  :group 'pairable)

(provide 'pairable)
;;; pairable.el ends here
