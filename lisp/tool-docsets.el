;;; tool-docsets.el --- Docsets -*- lexical-binding: t; -*-

;;; Commentary:
;; Browse docsets.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(req-package counsel-dash
  :commands
  (counsel-dash
   counsel-dash-install-docset)
  :init
  (defun counsel-dash-at-point ()
    "Counsel dash with selected point."
    (interactive)
    (counsel-dash
     (if (use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end))
       (substring-no-properties (or (thing-at-point 'symbol) "")))))

  (setq counsel-dash-browser-func 'eww
        counsel-dash-enable-debugging nil)

  (add-hook! 'emacs-lisp-mode (setq-local counsel-dash-docsets '("Emacs Lisp")))
  (add-hook! 'haskell-mode    (setq-local counsel-dash-docsets '("Haskell")))
  (add-hook! 'js2-mode        (setq-local counsel-dash-docsets '("JavaScript" "NodeJS")))
  (add-hook! 'lua-mode        (setq-local counsel-dash-docsets '("Lua")))
  (add-hook! 'ruby-mode       (setq-local counsel-dash-docsets '("Ruby")))
  (add-hook! 'rust-mode       (setq-local counsel-dash-docsets '("Rust")))
  (add-hook! 'scala-mode      (setq-local counsel-dash-docsets '("Scala" "Java"))))

(provide 'tool-docsets)
;;; tool-docsets.el ends here
