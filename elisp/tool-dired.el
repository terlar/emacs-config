;;; tool-dired.el --- Directory edit -*- lexical-binding: t; -*-

;;; Commentary:
;; Directory based operations.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

;;;
;; Packages

(req-package dired
  :loader :built-in
  :init
  ;; Always copy/delete recursively
  (setq dired-recursive-copies  'always
        dired-recursive-deletes 'top)
  :config
  (add-hook! 'dired-mode
             (face-remap-add-relative 'hl-line :background "#DDDDDD"))
  (add-hooks-pair 'dired-mode 'hl-line-mode))

;; Display subtrees
(req-package dired-subtree
  :commands
  (dired-subtree-toggle
   dired-subtree-cycle)
  :init
  (setq dired-subtree-use-backgrounds nil))

;; Pretty icons
(req-package all-the-icons-dired
  :diminish all-the-icons-dired-mode
  :commands all-the-icons-dired-mode
  :init
  (add-graphic-hook
   (add-hooks-pair 'dired-mode #'all-the-icons-dired-mode)))

(req-package image-dired
  :loader :built-in
  :require dired
  :after dired
  :init
  (setq image-dired-dir (concat my-cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "image-dired/db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")))

;; Striped dired buffers
(req-package stripe-buffer
  :require dired
  :commands stripe-buffer-mode
  :init
  (add-hooks-pair 'dired-mode 'stripe-buffer-mode))

(provide 'tool-dired)
;;; tool-dired.el ends here
