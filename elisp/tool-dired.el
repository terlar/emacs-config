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
        dired-recursive-deletes 'top))

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

;; Prettier dired buffers
(req-package dired-k
  :require dired
  :after dired
  :commands
  (dired-k
   dired-k-no-revert)
  :init
  (setq dired-k-style 'git)

  (add-hooks-pair 'dired-initial-position 'dired-k)
  (add-hooks-pair 'dired-after-readin 'dired-k-no-revert)
  :config
  (defun +dired-k-highlight (orig-fn &rest args)
    "Butt out if the requested directory is remote (i.e. through tramp)."
    (unless (file-remote-p default-directory)
      (apply orig-fn args)))
  (advice-add #'dired-k--highlight :around #'+dired-k-highlight))

;; Striped dired buffers
(req-package stripe-buffer
  :require dired
  :commands stripe-buffer-mode
  :init
  (add-hooks-pair 'dired-mode 'stripe-buffer-mode))

(provide 'tool-dired)
;;; tool-dired.el ends here
