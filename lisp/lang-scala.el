;;; lang-scala.el --- Scala -*- lexical-binding: t; -*-

;;; Commentary:
;; Scala is a general-purpose programming language providing support for
;; functional programming and a strong static type system. Designed to be
;; concise, many of Scala's design decisions aimed to address criticisms of
;; Java.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

;;;
;; Packages

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :hook
  (scala-mode . flycheck-mode)
  (scala-mode . prettify-symbols-mode)
  (scala-mode
   . (lambda ()
       (setq-local prettify-symbols-alist scala-prettify-symbols-alist)))
  :init
  (setq scala-indent:align-parameters t)
  :config
  (set-aggressive-indent 'scala-mode :disabled t))

(use-package sbt-mode
  :after scala-mode)

(use-package ensime
  :hook (scala-mode . ensime-mode)
  :init
  (autoload 'eir-eval-in-scala "eval-in-repl-scala")

  (setq ensime-startup-notification nil
        ensime-eldoc-hints 'all
        ensime-search-interface 'ivy)
  :config
  (set-repl-command 'scala-mode #'ensime-inf-switch)
  (set-eval-command 'scala-mode #'eir-eval-in-scala)

  (set-company-backends 'scala-mode 'ensime-company)
  (set-doc-fn 'ensime-mode #'ensime-show-doc-for-symbol-at-point)
  (smart-jump-register :modes 'ensime-mode
                       :jump-fn #'ensime-edit-definition
                       :pop-fn #'ensime-pop-find-definition-stack
                       :refs-fn #'ensime-show-uses-of-symbol-at-point)

  (set-evil-state 'ensime-inf-mode 'insert)
  (set-popup-buffer (rx bos "*Scala REPL*" eos)))

(provide 'lang-scala)
;;; lang-scala.el ends here
