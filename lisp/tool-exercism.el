;;; tool-exercism.el --- exercism.io integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise: it's good for you.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages
(req-package request)

(req-package exercism
  :require request
  :loader :el-get
  :commands
  (exercism
   exercism-submit
   exercism-unsubmit
   exercism-fetch
   exercism-tracks)
  :init
  (setq exercism-dir (expand-file-name "exercism" "~/src/")))

(provide 'tool-exercism)
;;; tool-exercism.el ends here
