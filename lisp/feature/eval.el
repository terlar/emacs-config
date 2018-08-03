;;; feature-eval.el --- Code evaluation and REPL -*- lexical-binding: t; -*-

;;; Commentary:
;; Evaluation.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package)
  (require 'base-keybinds))

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
  :commands paredit-mode
  :hook
  ((eval-expression-minibuffer-setup
    ielm-mode lisp-interaction-mode) . paredit-mode))

(req-package eval-in-repl
  :el-get t :ensure nil
  :commands
  (eir-repl-start
   eir-eval-in-repl-lisp
   eir-next-code-line)
  :init
  (setq eir-jump-after-eval t
        eir-repl-placement 'below))

(req-package quickrun
  :general
  (:keymaps 'quickrun--mode-map :states '(normal motion)
            "q" 'quit-window)
  :commands
  (quickrun-autorun-mode
   quickrun
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
