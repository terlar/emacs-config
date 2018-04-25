;;; init.el --- Main init file -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:
;;; The init file that loads all the components.

;;; Code:

(eval-and-compile
  (push (expand-file-name "lisp" user-emacs-directory) load-path)
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path))

(defun load-directory-files-with-prefixes (dir types)
  "Load Elisp files in DIR with TYPES prefix."
  (let ((prefixes (mapconcat 'symbol-name types "\\|")))
    (dolist (file (directory-files dir nil (concat "\\(" prefixes "\\)-[[:alnum:]-]*\\.el$")))
      (load-file (expand-file-name file (file-name-as-directory dir))))))

;;;
;; Base

;; Calls (package-initialize)
(require 'base)
(load-directory-files-with-prefixes
 (expand-file-name "lisp" user-emacs-directory)
 '(feature completion tool lang))

(unless noninteractive
  (require 'bindings)
  (require 'commands)
  (require 'theme))

(req-package-finish)

;;; init.el ends here
