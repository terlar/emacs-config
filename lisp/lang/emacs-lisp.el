;;; lang-emacs-lisp.el --- Emacs Lisp -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs Lisp is a dialect of the Lisp programming language used as a scripting
;; language by Emacs (a text editor family most commonly associated with GNU
;; Emacs.

;;; Code:

;;;
;; Packages

(use-package elisp-mode :ensure nil
  :init
  (autoload 'eir-eval-in-ielm "eval-in-repl-ielm")
  (set-repl-command 'emacs-lisp-mode #'elisp-repl)
  (set-eval-command 'emacs-lisp-mode #'eir-eval-in-ielm)
  :config
  (set-doc-fn 'emacs-lisp-mode #'helpful-at-point)

  (with-eval-after-load 'smart-jump
    (smart-jump-register :modes 'emacs-lisp-mode))

  (set-prettify-symbols 'emacs-lisp-mode '(("defun"    . ?ƒ)
                                           ("defmacro" . ?μ)
                                           ("defvar"   . ?ν)))

  (set-evil-state 'checkdoc-output-mode 'motion)

  (set-on-evil-state 'emacs-lisp-mode 'insert
                     (nameless-mode 0)
                     (easy-escape-minor-mode 0))
  (set-on-evil-state 'emacs-lisp-mode 'normal
                     (nameless-mode 1)
                     (easy-escape-minor-mode 1)))

;; Shorten package prefixes
(use-package nameless
  :diminish nameless-mode
  :hook (emacs-lisp-mode . nameless-mode))

;; Improve readability of escape characters in regular expressions
(use-package easy-escape
  :diminish easy-escape-minor-mode
  :hook (emacs-lisp-mode . easy-escape-minor-mode))

;; Emacs Start Up Profiler
(use-package esup
  :commands esup
  :config
  (set-evil-state 'esup-mode 'motion))

(use-package package-lint
  :commands package-lint-current-buffer)

;;;
;; Autoloads

;;;###autoload
(defun elisp-repl ()
  "Open the Emacs Lisp REPL (`ielm')."
  (interactive)
  (open-and-switch-to-buffer #'ielm "*ielm*" t))

(defun elisp-test ()
  "Run test file."
  (interactive)
  (when (string-match (buffer-name (current-buffer))
                      "-test.el$")
    (autoload 'ert-delete-all-tests "ert")
    (ert-delete-all-tests))

  (eval-buffer)
  (ert t))

(provide 'lang-emacs-lisp)
;;; lang-emacs-lisp.el ends here
