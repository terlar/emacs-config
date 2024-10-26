;;; screenshots.el --- Configuration screenshots -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2022 Terje Larsen
;; All rights reserved.

;; This file is NOT part of GNU Emacs.

;; screenshots is free software: you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software Foundation,
;; either version 3 of the License, or (at your option) any later version.

;; screenshots is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Script to take screenshots to demo configuration.

;;; Code:
(let* ((media-dir (expand-file-name "media"))
       (captures
        `((org-mode
           . ((find-file (expand-file-name "samples/sample.org" ,media-dir))
              (goto-char 49)
              (follow-mode 1)
              (split-window-right)
              (follow-redraw)))
          (markdown-mode
           . ((find-file (expand-file-name "samples/sample.md" ,media-dir))
              (goto-char 51)
              (follow-mode 1)
              (split-window-right)
              (follow-redraw)))
          (rst-mode
           . ((find-file (expand-file-name "samples/sample.rst" ,media-dir))
              (goto-char 239)
              (follow-mode 1)
              (split-window-right)
              (follow-redraw)))
          (emacs-lisp-mode
           . ((find-file (expand-file-name "../lisp/readable-mono-theme.el" ,media-dir))
              (goto-char 1223)
              (follow-mode 1)
              (split-window-right)
              (follow-redraw))))))
  (global-hide-fringes-mode 1)
  (blink-cursor-mode 0)

  (dolist (capture captures)
    (let ((name (symbol-name (car capture)))
          (actions (cdr capture)))
      (delete-other-windows)

      (dolist (action actions) (eval action))

      (font-lock-update)

      (dolist (background-mode '(light dark))
        (customize-set-variable 'frame-background-mode background-mode)
        (customize-set-variable 'custom-enabled-themes custom-enabled-themes)
        (message nil)
        (with-temp-file (expand-file-name
                         (concat name
                                 "-"
                                 (symbol-name background-mode)
                                 ".png")
                         media-dir)
          (insert (x-export-frames nil 'png)))))))

(provide 'screenshots)
;;; screenshots.el ends here
