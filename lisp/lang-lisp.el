;;; lang-lisp.el --- Lisp -*- lexical-binding: t; -*-

;;; Commentary:
;; Lisp (historically, LISP) is a family of computer programming languages with
;; a long history and a distinctive, fully parenthesized prefix notation.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package)
  (require 'base-lib)
  (require 'base-keybinds))

;;;
;; Functions

(defun lisp-repl ()
  "Open a Lisp REPL (`slime')."
  (interactive)
  (unless (slime-connected-p)
    (slime))
  (slime-repl))

(defun lisp-test ()
  "Test Lisp code."
  (interactive)
  (unless (slime-connected-p)
    (slime))
  (slime-load-file (test-file (buffer-file-name (current-buffer)))))

;;;
;; Packages

(req-package slime
  :commands
  (slime-mode
   slime-describe-symbol)
  :general
  (:keymaps 'lisp-mode-map :major-modes t
            :prefix my-local-leader-key
            "o" 'slime
            "r" 'lisp-repl
            "t" 'lisp-test)
  (:keymaps
   '(slime-doc-map
     slime-popup-buffer-mode-map
     slime-trace-dialog--detail-mode-map
     slime-trace-dialog-mode-map)
   :states 'normal
   "q" 'quit-window)
  :commands
  (slime slime-repl slime-connected-p)
  :hook (lisp-mode . slime-mode)
  :init
  (autoload 'eir-eval-in-slime "eval-in-repl-slime" nil t)

  (set-repl-command 'lisp-mode #'lisp-repl)
  (set-eval-command 'lisp-mode #'eir-eval-in-slime)
  (set-doc-fn 'lisp-mode #'slime-describe-symbol)

  (set-popup-buffer (rx bos "*inferior-lisp*" eos))

  (setq inferior-lisp-program "sbcl"
        slime-contribs '(slime-fancy slime-repl slime-company))
  :config
  (smart-jump-register :modes 'lisp-mode
                       :jump-fn #'slime-edit-definition
                       :pop-fn #'slime-pop-find-definition-stack
                       :refs-fn #'slime-who-references))

(req-package slime-company)

(provide 'lang-lisp)
;;; lang-lisp.el ends here
