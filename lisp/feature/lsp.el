;;; lsp.el --- Language Server Protocol -*- lexical-binding: t; -*-

;;; Commentary:
;; Servant of languages.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

(req-package eglot
  :general
  (:keymaps 'eglot-mode-map :states 'normal
            "K" 'eglot-help-at-point)
  :hook
  (js2-mode  . eglot-ensure)
  (ruby-mode . eglot-ensure)
  (sh-mode   . eglot-ensure)
  :init
  (with-eval-after-load 'smart-jump
    (smart-jump-register :modes '(js2-mode ruby-mode sh-mode)))
  (set-popup-buffer (rx bos "*EGLOT" (one-or-more anything) "*" eos)))

(provide 'feature-lsp)
;;; lsp.el ends here
