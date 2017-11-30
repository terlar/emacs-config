;;; feature-lsp.el --- Language Server Protocol -*- lexical-binding: t; -*-

;;; Commentary:
;; Servant of languages.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

(req-package lsp-mode
  :demand t
  :hook (lsp-mode . flycheck-mode)
  :init
  (setq lsp-enable-eldoc t))

(req-package company-lsp
  :require company lsp-mode
  :after lsp-mode
  :commands company-lsp
  :init
  (setq company-lsp-async t)
  :config
  (push 'company-lsp company-backends))

(provide 'feature-lsp)
;;; feature-lsp.el ends here
