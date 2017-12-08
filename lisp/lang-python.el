;;; lang-python.el --- Python -*- lexical-binding: t; -*-

;;; Commentary:
;; Python is a widely used high-level programming language for general-purpose
;; programming, created by Guido van Rossum and first released in 1991. An
;; interpreted language, Python has a design philosophy which emphasizes code
;; readability (notably using whitespace indentation to delimit code blocks
;; rather than curly brackets or keywords), and a syntax which allows
;; programmers to express concepts in fewer lines of code than might be used in
;; languages such as C++ or Java. The language provides constructs intended to
;; enable writing clear programs on both a small and large scale.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package)
  (require 'base-lib))

;;;
;; Packages

(use-package python
  :mode
  ("\\.py$" . python-mode)
  :interpreter
  ("python" . python-mode)
  :commands python-mode
  :hook (python-mode . highlight-numbers-mode)
  :init
  (autoload 'eir-eval-in-python "eval-in-repl-python")

  (defun python-repl ()
    "Open a Python REPL."
    (interactive)
    (open-and-switch-to-buffer #'run-python "*Python*" t))

  (set-repl-command 'python-mode #'python-repl)
  (set-eval-command 'python-mode #'eir-eval-in-python)

  (set-popup-buffer (rx bos "*Python*" eos))

  (set-prettify-symbols 'python-mode
                        '(("def"    . ?ƒ)
                          ("sum"    . ?Σ)
                          ("**2"    . ?²)
                          ("**3"    . ?³)
                          ("None"   . ?∅)
                          ("in"     . ?∈)
                          ("not in" . ?∉)
                          ("return" . ?➡)))

  (setq python-environment-directory my-cache-dir
        python-indent-guess-indent-offset-verbose nil
        python-shell-interpreter "python"))

(use-package pip-requirements
  :mode ("/requirements.txt$" . pip-requirements-mode))

(use-package lsp-python
  :requires lsp-mode
  :hook (python-mode . lsp-python-enable)
  :config
  (smart-jump-register :modes 'python-mode))

(use-package py-autopep8
  :hook (python-mode . py-autopep8-enable-on-save))

(use-package pydoc
  :commands pydoc-at-point
  :init
  (set-doc-fn 'python-mode #'pydoc-at-point)
  (set-evil-state 'pydoc-mode 'motion)
  (set-popup-buffer (rx bos "*pydoc*" eos)))

(use-package python-x
  :hook (python-mode . python-x-setup))

(use-package python-test
  :commands
  (python-test
   python-test-class
   python-test-method
   python-test-function
   python-test-file
   python-test-project)
  :init
  (setq python-test-backend 'pytest)
  :config
  (setq python-test-project-root-files
        (append '("README.md")
                python-test-project-root-files))

  (set-popup-buffer (rx bos "*python-test*" eos)))

(provide 'lang-python)
;;; lang-python.el ends here
