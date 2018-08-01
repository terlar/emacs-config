;;; lsp.el --- Language Server Protocol -*- lexical-binding: t; -*-

;;; Commentary:
;; Servant of languages.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

(req-package eglot
  :hook
  (js2-mode . eglot-ensure)
  (ruby-mode . eglot-ensure)
  (sh-mode . eglot-ensure)
  :init
  (set-popup-buffer (rx bos "*EGLOT" (one-or-more anything) "*" eos)))

(provide 'feature-lsp)
;;; lsp.el ends here
