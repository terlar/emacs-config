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
  (setq lsp-enable-eldoc t
        lsp-inhibit-message t)
  :config
  (with-eval-after-load 'flycheck
    (require 'lsp-flycheck)))

(req-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-doc-include-signature nil
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-ignore-duplicate t))

(req-package company-lsp
  :requires company
  :commands company-lsp
  :init
  (setq company-lsp-async t)
  (with-eval-after-load 'company
    (push 'company-lsp company-backends)))

(provide 'feature-lsp)
;;; feature-lsp.el ends here
