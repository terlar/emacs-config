;;; feature-lsp.el --- Language Server Protocol -*- lexical-binding: t; -*-

;;; Commentary:
;; Servant of languages.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

(req-package lsp-mode
  :demand t
  :init
  (setq lsp-enable-eldoc t)
  :config
  (require 'lsp-flycheck)
  (add-hooks-pair 'lsp-mode 'flycheck-mode))

(req-package company-lsp
  :require company lsp-mode
  :commands company-lsp
  :after lsp-mode
  :init
  (setq company-lsp-async t)
  :config
  (push 'company-lsp company-backends))

(provide 'feature-lsp)
;;; feature-lsp.el ends here
