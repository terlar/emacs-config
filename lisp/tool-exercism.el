;;; tool-exercism.el --- exercism.io integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise: it's good for you.

;;; Code:

(eval-when-compile
  (require 'base-package))

(defvar exercism-auto-enable nil)

;;;
;; Packages
(req-package request)

(req-package exercism
  :el-get t
  :commands
  (exercism
   exercism-submit
   exercism-unsubmit
   exercism-fetch
   exercism-tracks)
  :init
  (setq exercism-dir (expand-file-name "exercism" "~/src/"))
  ;; Fix the auto-mode-alist addition
  (push `(,exercism-dir . exercism-mode) auto-mode-alist))

(provide 'tool-exercism)
;;; tool-exercism.el ends here
