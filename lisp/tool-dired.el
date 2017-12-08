;;; tool-dired.el --- Directory edit -*- lexical-binding: t; -*-

;;; Commentary:
;; Directory based operations.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

;;;
;; Packages

(use-package dired
  :hook
  (dired-mode . hl-line-mode)
  (dired-mode
   . (lambda ()
       (face-remap-add-relative 'hl-line :background "#DDDDDD")))
  :init
  ;; Always copy/delete recursively
  (setq dired-recursive-copies  'always
        dired-recursive-deletes 'top))

;; Display subtrees
(use-package dired-subtree
  :commands
  (dired-subtree-toggle
   dired-subtree-cycle)
  :init
  (setq dired-subtree-use-backgrounds nil))

;; Pretty icons
(use-package all-the-icons-dired
  :diminish all-the-icons-dired-mode
  :commands all-the-icons-dired-mode
  :init
  (add-graphic-hook
   (add-hooks-pair 'dired-mode 'all-the-icons-dired-mode)))

(use-package image-dired
  :init
  (setq image-dired-dir (concat my-cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "image-dired/db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")))

;; Striped dired buffers
(use-package stripe-buffer
  :hook (dired-mode . stripe-buffer-mode))

(provide 'tool-dired)
;;; tool-dired.el ends here
