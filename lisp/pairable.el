;;; pairable.el --- Optimize for pair-programming -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2022 Terje Larsen
;; All rights reserved.

;; Author: Terje Larsen <terlar@gmail.com>
;; Keywords: faces
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

(defgroup pairable nil
  "Settings for pair-programming."
  :group 'faces)

;;;###autoload
(defcustom pairable-text-scale 2
  "Scaling factor for text."
  :type 'number
  :group 'pairable)

;;;###autoload
(defcustom pairable-display-line-numbers t
  "Use line-numbers or not."
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
  :group 'pairable
  (if pairable-mode
      (progn
        (global-display-line-numbers-mode 1)
        (text-scale-set pairable-text-scale))
    (progn
      (global-display-line-numbers-mode 0)
      (text-scale-increase 0))))

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
