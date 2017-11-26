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
  (require 'base-lib)
  (require 'base-keybinds))

;;;
;; Packages

(req-package python
  :mode
  ("\\.py$" . python-mode)
  :interpreter
  ("python" . python-mode)
  :commands python-mode
  :init
  (autoload 'eir-eval-in-python "eval-in-repl-python")

  (defun python-repl ()
    "Open a Python REPL."
    (interactive)
    (open-and-switch-to-buffer #'run-python "*Python*" t))

  (set-repl-command 'python-mode #'python-repl)
  (set-eval-command 'python-mode #'eir-eval-in-python)

  (set-popup-buffer (rx bos "*Python*" eos))

  (setq python-environment-directory my-cache-dir
        python-indent-guess-indent-offset-verbose nil
        python-shell-interpreter "python")
  :config
  (add-hooks-pair 'python-mode 'highlight-numbers-mode))

(req-package python-x
  :require python
  :after python)

(req-package pip-requirements
  :require python
  :mode ("/requirements.txt$" . pip-requirements-mode))

(req-package lsp-python
  :require lsp-mode python
  :commands lsp-python-enable
  :init
  (add-hooks-pair 'python-mode 'lsp-python-enable)
  :config
  (smart-jump-register :modes 'python-mode))

(req-package py-autopep8
  :require python
  :after python
  :config
  (add-hooks-pair 'python-mode 'py-autopep8-enable-on-save))

(req-package pydoc
  :after python
  :commands pydoc-at-point
  :config
  (set-doc-fn 'python-mode #'pydoc-at-point)
  (set-evil-state 'pydoc-mode 'motion)
  (set-popup-buffer (rx bos "*pydoc*" eos)))

(req-package python-test
  :require python
  :after python
  :init
  (setq python-test-backend 'pytest)
  :config
  (setq python-test-project-root-files
        (append '("README.md")
                python-test-project-root-files))

  (set-popup-buffer (rx bos "*python-test*" eos)))

(provide 'lang-python)
;;; lang-python.el ends here
