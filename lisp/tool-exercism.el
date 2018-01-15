;;; tool-exercism.el --- exercism.io integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise: it's good for you.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package)
  (require 'base-keybinds))

(defvar exercism-auto-enable nil)

;;;
;; Packages

(req-package exercism
  :el-get t :ensure nil
  :commands
  (exercism
   exercism-submit
   exercism-unsubmit
   exercism-fetch
   exercism-tracks)
  :general
  (:prefix my-leader-key
           "E f" 'exercism-fetch
           "E l" 'exercism-list
           "E o" 'exercism-open
           "E s" 'exercism-submit
           "E u" 'exercism-unsubmit
           "E t" 'exercism-tracks)
  :init
  (setq exercism-dir (expand-file-name "exercism" "~/src/"))
  ;; Fix the auto-mode-alist addition
  (push `(,exercism-dir . exercism-mode) auto-minor-mode-alist))

(provide 'tool-exercism)
;;; tool-exercism.el ends here
