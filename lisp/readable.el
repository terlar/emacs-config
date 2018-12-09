;;; readable.el --- A minor-mode to make text more readable -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2018 Terje Larsen
;; All rights reserved.

;; Author: Terje Larsen <terlar@gmail.com>
;; URL: https://github.com/terlar/readable.el
;; Keywords: faces
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; readable is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; readable is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `readable' is a small Emacs minor mode that improves readability by changing
;; the type face, size, line spacing e.t.c.

;;; Code:

(defgroup readable nil
  "Make text more readable."
  :group 'faces)

;;;###autoload
(defface readable-fixed-pitch
  '((t (:inherit fixed-pitch-serif)))
  "Face used to increase readability for `fixed-pitch' face."
  :group 'readable)

;;;###autoload
(defface readable-variable-pitch
  '((t (:family "serif")))
  "Face used to increase readability for `variable-pitch' face."
  :group 'readable)

;;;###autoload
(defcustom readable-line-spacing 0.4
  "Line spacing used in `readable-mode'."
  :type 'number
  :group 'readable)

;;;###autoload
(defcustom readable-lighter " Readable"
  "Mode-line indicator for `readable-mode'."
  :type '(choice (const :tag "No lighter" "") string)
  :group 'readable)

(defvar-local readable-mode-saved-state-plist nil
  "Properties containing the state before `readable-mode' was enabled.
Contains following state:

  :line-spacing NUMBER
  :variable-pitch BOOLEAN")

(defvar-local readable-mode-fixed-pitch-remapping nil
  "Readable `face-remap' cookie for `fixed-pitch' face.")

(defvar-local readable-mode-variable-pitch-remapping nil
  "Readable `face-remap' cookie for `variable-pitch' face.")

(autoload 'face-remap-add-relative "face-remap")
(autoload 'face-remap-remove-relative "face-remap")

;;;###autoload
(define-minor-mode readable-mode
  "Toggle Readable mode.

In Readable mode, the type face is changed, line spacing increased and various
other improvements to increase readability.

The face used is `readable' and `line-spacing' is configured by
`readable-line-spacing'."
  :lighter readable-lighter
  :group 'readable
  (when readable-mode-fixed-pitch-remapping
    (face-remap-remove-relative readable-mode-fixed-pitch-remapping)
    (setq readable-mode-fixed-pitch-remapping nil))

  (when readable-mode-variable-pitch-remapping
    (face-remap-remove-relative readable-mode-variable-pitch-remapping)
    (setq readable-mode-variable-pitch-remapping nil))

  (if readable-mode
      (progn
        ;; Save state
        (setq readable-mode-saved-state-plist
              (list :line-spacing line-spacing
                    :variable-pitch (bound-and-true-p buffer-face-mode)))

        ;; Configure readability
        (variable-pitch-mode 1)
        (setq line-spacing readable-line-spacing
              readable-mode-fixed-pitch-remapping
              (face-remap-add-relative 'fixed-pitch 'readable-fixed-pitch)
              readable-mode-variable-pitch-remapping
              (face-remap-add-relative 'variable-pitch 'readable-variable-pitch)))
    (progn
      (variable-pitch-mode (plist-get readable-mode-saved-state-plist :variable-pitch))
      (setq line-spacing (plist-get readable-mode-saved-state-plist :line-spacing)
            readable-mode-saved-state-plist nil))))

(provide 'readable)
;;; readable.el ends here
