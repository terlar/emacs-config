;;; tool-docsets.el --- Docsets -*- lexical-binding: t; -*-

;;; Commentary:
;; Browse docsets.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(use-package counsel-dash
  :commands
  (counsel-dash
   counsel-dash-install-docset)
  :hook
  (emacs-lisp-mode
   . (lambda () (setq-local counsel-dash-docsets '("Emacs Lisp"))))
  (haskell-mode
   . (lambda () (setq-local counsel-dash-docsets '("Haskell"))))
  (js2-mode
   . (lambda () (setq-local counsel-dash-docsets '("JavaScript" "NodeJS"))))
  (lua-mode
   . (lambda () (setq-local counsel-dash-docsets '("Lua"))))
  (ruby-mode
   . (lambda () (setq-local counsel-dash-docsets '("Ruby"))))
  (rust-mode
   . (lambda () (setq-local counsel-dash-docsets '("Rust"))))
  (scala-mode
   . (lambda () (setq-local counsel-dash-docsets '("Scala" "Java"))))
  :init
  (defun counsel-dash-at-point ()
    "Counsel dash with selected point."
    (interactive)
    (counsel-dash
     (if (use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end))
       (substring-no-properties (or (thing-at-point 'symbol) "")))))

  (setq counsel-dash-browser-func 'eww
        counsel-dash-enable-debugging nil))

(provide 'tool-docsets)
;;; tool-docsets.el ends here
