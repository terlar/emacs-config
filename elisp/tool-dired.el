;;; tool-dired.el --- Directory edit

;;; Commentary:
;; Directory based operations.

;;; Code:
(require 'base-vars)

(require 'dired)
(require 'image-dired)
(require 'autorevert)

(setq
 ;; Always copy/delete recursively
 dired-recursive-copies  'always
 dired-recursive-deletes 'top
 ;; Auto refresh dired, but be quiet about it
 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil
 ;; files
 image-dired-dir (concat my-cache-dir "image-dired/")
 image-dired-db-file (concat image-dired-dir "image-dired/db.el")
 image-dired-gallery-dir (concat image-dired-dir "gallery/")
 image-dired-temp-image-file (concat image-dired-dir "temp-image")
 image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image"))

;; Prettier dired buffers
(use-package dired-k
  :after dired
  :functions dired-k--highlight
  :preface
  (defun my|dired-k-highlight (orig-fn &rest args)
    "Butt out if the requested directory is remote (i.e. through tramp)."
    (unless (file-remote-p default-directory)
      (apply orig-fn args)))
  :config
  (setq dired-k-style 'git)

  (advice-add #'dired-k--highlight :around #'my|dired-k-highlight)

  (add-hook 'dired-initial-position-hook #'dired-k)
  (add-hook 'dired-after-readin-hook #'dired-k-no-revert))

;; Striped dired buffers
(use-package stripe-buffer
  :commands stripe-buffer-mode
  :init (add-hook 'dired-mode-hook #'stripe-buffer-mode))

(provide 'tool-dired)
;;; tool-dired.el ends here
