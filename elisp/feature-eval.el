;;; feature-eval.el --- Code evaluation and REPL -*- lexical-binding: t; -*-

;;; Commentary:
;; Evaluation.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

;;;
;; Settings
(defvar eval-builder-alist nil
  "Alist mapping major modes to build functions.")
(defvar eval-repl-alist nil
  "Alist mapping major modes to REPL functions.")
(defvar eval-runner-alist nil
  "Alist mapping major modes to interactive runner functions.")

;;;
;; Packages

(req-package paredit
  :demand t)

(req-package eval-in-repl
  :loader :el-get
  :require dash paredit
  :commands
  (eir-repl-start
   eir-eval-in-repl-lisp
   eir-next-code-line)
  :init
  (setq eir-jump-after-eval t
        eir-repl-placement 'below))

(req-package quickrun
  :commands
  (quickrun
   quickrun-region
   quickrun-with-arg
   quickrun-shell
   quickrun-compile-only
   quickrun-replace-region))

;;;
;; Autoloads

;;;###autoload
(defun repl ()
  "Open (or reopen) the REPL associated with the current `major-mode' and place the cursor at the prompt."
  (interactive)
  (when-let* ((command (cdr (assq major-mode eval-repl-alist))))
    (funcall command)))

;;;###autoload
(defun repl-eval ()
  "Evaluate in REPL."
  (interactive)
  (when-let* ((command (cdr (assq major-mode eval-runner-alist))))
    (funcall command)))

(provide 'feature-eval)
;;; feature-eval.el ends here
