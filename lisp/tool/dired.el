;;; tool-dired.el --- Directory edit -*- lexical-binding: t; -*-

;;; Commentary:
;; Directory based operations.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package)
  (require 'base-keybinds))

;;;
;; Packages

(req-package dired :ensure nil
  :hook
  (dired-mode . hl-line-mode)
  (dired-mode . +dired-mode-faces)
  (dired-mode . dired-hide-details-mode)
  :preface
  (defun +dired-mode-faces ()
    (face-remap-add-relative 'hl-line
                             :background (face-background 'isearch)))
  :general
  (:keymaps 'dired-mode-map
            :major-modes t
            :states 'motion
            "RET" 'dired-find-file
            "/" 'counsel-grep-or-swiper
            "?" 'counsel-grep-or-swiper
            "i" 'dired-toggle-read-only
            "gi" 'dired-maybe-insert-subdir)
  (:keymaps 'wdired-mode-map :states 'normal
            "ZQ" 'wdired-abort-changes
            "ZZ" 'wdired-finish-edit)
  :init
  ;; Always copy/delete recursively
  (setq dired-recursive-copies  'always
        dired-recursive-deletes 'top)

  (set-evil-state 'wdired-mode 'normal))

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
  :hook (dired-mode . all-the-icons-dired-mode))

(req-package image-dired
  :init
  (setq image-dired-dir (concat my-cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "image-dired/db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")))

;; Striped dired buffers
(req-package stripe-buffer
  :hook (dired-mode . stripe-buffer-mode))

(provide 'tool-dired)
;;; dired.el ends here
