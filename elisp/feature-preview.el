;;; feature-preview.el --- Preview

;;; Commentary:
;; WYSIWYG

;;; Code:
(use-package impatient-mode
  :commands impatient-mode
  :preface
  (eval-when-compile
    (declare-function markdown "markdown-mode"))

  (defun imp-markdown-filter (in)
    (let ((out (current-buffer)))
      (with-current-buffer in
        (markdown out))))
  :config
  (push (cons 'markdown-mode #'imp-markdown-filter)
        imp-default-user-filters))

(provide 'feature-preview)
;;; feature-preview.el ends here
