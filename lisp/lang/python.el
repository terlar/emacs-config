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
;; Functions

(defun python-repl ()
  "Open a Python REPL."
  (interactive)
  (open-and-switch-to-buffer #'run-python "*Python*" t))

(defun +python-test ()
  "Test Python code."
  (interactive)
  (compile (concat "python " (test-file (buffer-file-name (current-buffer))))))

;;;
;; Packages

(req-package python
  :mode
  ("\\.py$" . python-mode)
  :interpreter
  ("python" . python-mode)
  :commands python-mode
  :preface
  (defun +python-setup ()
    (setq test-suffix "_test"))
  :hook
  (python-mode . highlight-numbers-mode)
  (python-mode . +python-setup)
  :init
  (autoload 'eir-eval-in-python "eval-in-repl-python")

  (set-repl-command 'python-mode #'python-repl)
  (set-eval-command 'python-mode #'eir-eval-in-python)

  (set-prettify-symbols 'python-mode
                        '(("def"    . ?𝒇)
                          ("class"  . ?𝑪)
                          ("="      . ?≝)
                          ("sum"    . ?Σ)
                          ("and"    . ?∧)
                          ("or"     . ?∨)
                          ("not"    . ?￢)
                          ("in"     . ?∈)
                          ("not in" . ?∉)
                          ("for"    . ?∀)
                          ("**2"    . ?²)
                          ("**3"    . ?³)
                          ("None"   . ?∅)
                          ("[]"     . ?⃞)
                          ("return" . ?⟼)
                          ("yield"  . ?⟻)))

  (setq python-environment-directory my-cache-dir
        python-indent-guess-indent-offset-verbose nil
        python-shell-interpreter "python"))

(req-package pip-requirements
  :mode ("/requirements.txt$" . pip-requirements-mode))

(req-package py-autopep8
  :hook (python-mode . py-autopep8-enable-on-save))

(req-package pydoc
  :commands pydoc-at-point
  :init
  (set-doc-fn 'python-mode #'pydoc-at-point)
  (set-evil-state 'pydoc-mode 'motion))

(req-package python-x
  :hook (python-mode . python-x-setup))

(req-package python-test
  :general
  (:keymaps 'python-mode-map :major-modes t
            :prefix my-local-leader-key
            "r" 'python-repl
            "t" 'python-test-project)
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
                python-test-project-root-files)))

(provide 'lang-python)
;;; lang-python.el ends here
