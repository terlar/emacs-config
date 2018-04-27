;;; init.el --- Main init file -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:
;;; The init file that loads all the components.

;;; Code:

(eval-and-compile
  (push (expand-file-name "lisp" user-emacs-directory) load-path)
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path))

(defun load-directory (dir)
  "Load Elisp files in DIR."
  (dolist (file (directory-files dir nil (concat "[[:alnum:]-]*\\.el$")))
    (load-file (expand-file-name file (file-name-as-directory dir)))))

;;;
;; Base

;; Calls (package-initialize)
(require 'base)
(dolist (type '(feature completion tool lang))
  (load-directory (expand-file-name (concat "lisp" "/" (symbol-name type)) user-emacs-directory)))

(unless noninteractive
  (require 'bindings)
  (require 'commands)
  (require 'theme))

(req-package-finish)

;;; init.el ends here
