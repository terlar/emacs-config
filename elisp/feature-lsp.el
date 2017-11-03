;;; feature-lsp.el --- Language Server Protocol -*- lexical-binding: t; -*-

;;; Commentary:
;; Servant of languages.

;;; Code:

(eval-when-compile
  (require 'base-vars))

(autoload 'push-company-backends "base-lib")

(use-package lsp-mode
  :preface
  (eval-when-compile
    (defvar lsp-enable-eldoc))
  :commands (lsp-mode
             lsp-define-stdio-client lsp-client-on-notification
             lsp-make-traverser)
  :init
  (add-hooks-pair 'lsp-mode 'flycheck-mode)
  :config
  (setq lsp-enable-eldoc t)
  (with-eval-after-load 'lsp-mode
    (require 'lsp-flycheck)))

(use-package company-lsp
  :commands company-lsp
  :preface
  (eval-when-compile
    (defvar company-lsp-async))
  :init
  (with-eval-after-load "company"
    (push-company-backends 'lsp-mode 'company-lsp))
  :config
  (setq company-lsp-async t))

(provide 'feature-lsp)
;;; feature-lsp.el ends here
