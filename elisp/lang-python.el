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
  (require 'base-keybinds)

  (defvar shackle-rules))

(autoload 'sp-local-pair "smartparens")
(autoload 'sp-with-modes "smartparens")

(autoload 'push-repl-command "base-lib")

;;;
;; Packages

(use-package python
  :commands python-mode
  :defines python-environment-directory
  :preface
  (defun python-repl ()
    "Open a Python REPL."
    (interactive)
    (process-buffer (run-python nil t t)))
  :init
  (setq python-environment-directory my-cache-dir
        python-indent-guess-indent-offset-verbose nil
        python-shell-interpreter "python")
  :config
  (push-repl-command 'python-mode #'python-repl)

  (add-hooks-pair 'python-mode 'highlight-numbers-mode)
  (sp-with-modes 'python-mode
    (sp-local-pair "'" nil :unless '(sp-point-before-word-p sp-point-after-word-p sp-point-before-same-p))))

(use-package lsp-python
  :after lsp-mode
  :commands lsp-python-enable
  :init
  (add-hooks-pair 'python-mode 'lsp-python-enable))

(use-package pip-requirements
  :mode ("/requirements.txt$" . pip-requirements-mode))

(use-package py-autopep8
  :commands py-autopep8-enable-on-save
  :init
  (add-hooks-pair 'python-mode 'py-autopep8-enable-on-save))

(use-package nose
  :commands nose-mode
  :general
  (:keymaps 'nose-mode-map :states 'normal :prefix "C-c t"
            "r" '(nosetests-again)
            "a" '(nosetests-all)
            "s" '(nosetests-one)
            "v" '(nosetests-module)
            "A" '(nosetests-pdb-all)
            "O" '(nosetests-pdb-one)
            "V" '(nosetests-pdb-module))
  :preface
  (defvar nose-mode-map (make-sparse-keymap))
  :config
  (with-eval-after-load "shackle"
    (push '("*nosetests*" :size 0.4 :noselect t) shackle-rules)))

(provide 'lang-python)
;;; lang-python.el ends here
