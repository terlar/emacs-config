;;; lang-java.el --- Java -*- lexical-binding: t; -*-

;;; Commentary:
;; Java is a general-purpose computer programming language that is concurrent,
;; class-based, object-oriented, and specifically designed to have as few
;; implementation dependencies as possible. It is intended to let application
;; developers "write once, run anywhere" (WORA), meaning that compiled Java code
;; can run on all platforms that support Java without the need for
;; recompilation.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-keybinds))

(set-aggressive-indent 'java-mode :disabled t)

;;;
;; Packages

(req-package java-repl
  :load-path my-site-lisp-dir
  :commands run-java-repl
  :init
  (defalias 'java-repl 'run-java-repl)

  (defun java-repl-eval-fn (stmt)
    "Send STMT to Java REPL."
    (let ((buffer-name "*java-repl*"))
      (unless (get-buffer buffer-name)
        (run-java-repl))

      (with-current-buffer buffer-name
        (insert stmt)
        (comint-send-input))))

  (defun java-repl-eval-region (begin end)
    "Evaluate selected region from BEGIN to END."
    (interactive "r")
    (let ((stmt (buffer-substring begin end)))
      (java-repl-eval-fn stmt)))

  (defun java-repl-eval-line (&optional next-line)
    "Evaluate current line."
    (interactive "P")
    (java-repl-eval-region (line-beginning-position) (line-end-position))
    (when next-line
      (call-interactively 'next-logical-line)))

  (defun java-repl-eval ()
    "Evaluate code in Java REPL."
    (if (use-region-p)
        (java-repl-eval-region (region-beginning) (region-end))
      (java-repl-eval-line t)))

  (set-repl-command 'java-mode #'java-repl)
  (set-eval-command 'java-mode #'java-repl-eval)

  (set-evil-state 'java-repl-mode 'insert)

  (setq java-repl-file-path "/usr/bin/javarepl"))

(req-package javadoc-lookup
  :commands javadoc-lookup
  :init
  (set-doc-fn 'java-mode #'javadoc-lookup))

(req-package android-mode
  :commands
  (android-mode
   android-create-project
   android-start-emulator))

(req-package groovy-mode
  :mode
  "\\.gr\\(adle\\|oovy\\)\\'"
  "Jenkinsfile")

(req-package gradle-mode
  :hook ((java-mode kotlin-mode) . gradle-mode)
  :general
  (:keymaps 'gradle-mode-map
            :prefix my-local-leader-key
            "t" 'gradle-test))

(req-package flycheck-gradle
  :hook ((java-mode kotlin-mode) . flycheck-gradle-setup))

(req-package kotlin-mode
  :commands kotlin-repl
  :mode "\\.kts?\\'")

(req-package log4j-mode
  :mode "\\.log\\'"
  :interpreter "syslog-mode")

(provide 'lang-java)
;;; lang-java.el ends here
