;;; lang-scala.el --- Scala

;;; Commentary:
;; Scala is a general-purpose programming language providing support for
;; functional programming and a strong static type system. Designed to be
;; concise, many of Scala's design decisions aimed to address criticisms of
;; Java.

;;; Code:
(require 'base-vars)
(require 'base-lib)

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :config
  (setq scala-indent:align-parameters t)

  (with-eval-after-load "company"
    (push-company-backends 'scala-mode '(ensime-company)))

  (add-hook 'scala-mode-hook
            #'(lambda ()
                (setq-local prettify-symbols-alist scala-prettify-symbols-alist)
                (prettify-symbols-mode +1)

                (flycheck-mode +1)

                (require 'imenu)
                (ensime-mode +1))))

(use-package sbt-mode :after scala-mode)

(use-package ensime
  :commands (ensime ensime-mode ensime-scala-mode-hook)
  :config
  (setq ensime-startup-snapshot-notification nil
        ensime-startup-notification nil
        ensime-eldoc-hints 'all
        ensime-search-interface (cond ((eq my-completion-system 'ivy) 'ivy)
                                      ((eq my-completion-system 'helm) 'helm)
                                      (t nil))))

(provide 'lang-scala)
;;; lang-scala.el ends here
