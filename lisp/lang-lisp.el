;;; lang-lisp.el --- Lisp -*- lexical-binding: t; -*-

;;; Commentary:
;; Lisp (historically, LISP) is a family of computer programming languages with
;; a long history and a distinctive, fully parenthesized prefix notation.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-lib)
  (require 'base-keybinds))

;;;
;; Packages

(req-package slime
  :commands
  (slime-mode
   slime-describe-symbol)
  :general
  (:keymaps
   '(slime-doc-map
     slime-popup-buffer-mode-map
     slime-trace-dialog--detail-mode-map
     slime-trace-dialog-mode-map)
   :states 'normal
   "q" 'quit-window)
  :commands
  (slime slime-repl slime-connected-p)
  :init
  (autoload 'eir-eval-in-slime "eval-in-repl-slime" nil t)

  (defun lisp-repl ()
    "Open a Lisp REPL (`slime')."
    (interactive)
    (unless (slime-connected-p)
      (slime))
    (slime-repl))

  (set-repl-command 'lisp-mode #'lisp-repl)
  (set-eval-command 'lisp-mode #'eir-eval-in-slime)
  (set-doc-fn 'lisp-mode #'slime-describe-symbol)

  (set-popup-buffer (rx bos "*slime-" (one-or-more anything) "*" eos))

  (setq inferior-lisp-program "clisp")

  (add-hooks-pair 'lisp-mode 'slime-mode)
  :config
  (smart-jump-register :modes 'lisp-mode
                       :jump-fn #'slime-edit-definition
                       :pop-fn #'slime-pop-find-definition-stack
                       :refs-fn #'slime-who-references)

  (slime-setup '(slime-fancy)))

(provide 'lang-lisp)
;;; lang-lisp.el ends here
