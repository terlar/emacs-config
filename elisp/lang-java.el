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
  (require 'base-package))

;;;
;; Packages

(req-package lsp-java
  :require lsp-mode
  :commands lsp-java-enable
  :init
  (setq lsp-java-server-install-dir "/opt/jdt-language-server/")
  (add-hooks-pair 'java-mode 'lsp-java-enable)
  :config
  (smart-jump-register :modes 'java-mode))

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
  :mode "\\.gr\\(adle\\|oovy\\)$")

(req-package gradle-mode
  :commands gradle-mode
  :init
  (add-hooks-pair 'java-mode 'gradle-mode))

(req-package log4j-mode
  :mode "\\.log$"
  :interpreter "syslog-mode")

(provide 'lang-java)
;;; lang-java.el ends here
