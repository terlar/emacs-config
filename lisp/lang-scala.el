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

(req-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :init
  (setq scala-indent:align-parameters t)
  :config
  (set-aggressive-indent 'scala-mode :disabled t)
  (set-company-backends 'scala-mode 'ensime-company)

  (add-hook! 'scala-mode
             (setq-local prettify-symbols-alist scala-prettify-symbols-alist))

  (add-hooks-pair 'scala-mode
                  '(flycheck-mode
                    prettify-symbols-mode)))

(req-package sbt-mode
  :require scala-mode
  :after scala-mode)

(req-package ensime
  :require scala-mode
  :after scala-mode
  :init
  (autoload 'eir-eval-in-scala "eval-in-repl-scala")

  (setq ensime-startup-notification nil
        ensime-eldoc-hints 'all
        ensime-search-interface 'ivy)
  :config
  (set-repl-command 'scala-mode #'ensime-inf-switch)
  (set-eval-command 'scala-mode #'eir-eval-in-scala)

  (set-doc-fn 'ensime-mode #'ensime-show-doc-for-symbol-at-point)
  (smart-jump-register :modes 'ensime-mode
                       :jump-fn #'ensime-edit-definition
                       :pop-fn #'ensime-pop-find-definition-stack
                       :refs-fn #'ensime-show-uses-of-symbol-at-point)

  (set-evil-state 'ensime-inf-mode 'insert)
  (set-popup-buffer (rx bos "*Scala REPL*" eos))

  (add-hooks-pair 'scala-mode 'ensime-mode))

(provide 'lang-scala)
;;; lang-scala.el ends here
