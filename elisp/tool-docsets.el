;;; tool-docsets.el --- Docsets -*- lexical-binding: t; -*-

;;; Commentary:
;; Browse docsets.

;;; Code:

;;;
;; Packages

(use-package counsel-dash
  :commands (counsel-dash
             counsel-dash-install-docset
             counsel-dash-at-point)
  :preface
  (defun counsel-dash-at-point ()
    "Counsel dash with selected point"
    (interactive)
    (counsel-dash
     (if (use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end))
       (substring-no-properties (or (thing-at-point 'symbol) "")))))
  :init
  (add-hooks-pair 'emacs-lisp-mode '(lambda () (setq-local counsel-dash-docsets '("Emacs Lisp"))))
  (add-hooks-pair 'js2-mode        '(lambda () (setq-local counsel-dash-docsets '("JavaScript" "NodeJS"))))
  (add-hooks-pair 'ruby-mode       '(lambda () (setq-local counsel-dash-docsets '("Ruby"))))
  (add-hooks-pair 'rust-mode       '(lambda () (setq-local counsel-dash-docsets '("Rust"))))
  (add-hooks-pair 'scala-mode      '(lambda () (setq-local counsel-dash-docsets '("Scala" "Java"))))
  :config
  (setq counsel-dash-browser-func 'eww
        counsel-dash-enable-debugging nil))

(provide 'tool-docsets)
;;; tool-docsets.el ends here
