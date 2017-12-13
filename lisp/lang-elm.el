;;; lang-elm.el --- Elm -*- lexical-binding: t; -*-

;;; Commentary:
;; Elm is a domain-specific programming language for declaratively creating web
;; browser-based graphical user interfaces. Elm is purely functional, and is
;; developed with emphasis on usability, performance, and robustness. It
;; advertises "no runtime exceptions in practice," made possible by the Elm
;; compiler's static type checking.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-lib))

;;;
;; Packages

(use-package elm-mode
  :mode ("\\.elm$")
  :hook (elm-mode . rainbow-delimiters-mode)
  :init
  (autoload 'eir-eval-in-elm "eval-in-repl-elm")

  (defalias 'elm-repl 'elm-repl-load)

  (set-repl-command 'elm-mode #'elm-repl)
  (set-eval-command 'elm-mode #'eir-eval-in-elm)

  (set-popup-buffer (rx bos "*elm*" eos))
  (set-evil-state 'elm-interactive-mode 'insert)

  (setq elm-format-on-save t
        elm-tags-on-save t
        elm-tags-exclude-elm-stuff nil)
  :config
  (set-doc-fn 'elm-mode #'elm-oracle-doc-at-point)
  (smart-jump-register :modes 'elm-mode)

  (set-aggressive-indent 'elm-mode :disabled t)
  (set-company-backends 'elm-mode 'company-elm)

  (set-popup-buffer (rx bos "*elm-make*" eos)
                    (rx bos "*elm-test*" eos)))

(use-package flycheck-elm
  :requires flycheck
  :hook
  (elm-mode . flycheck-mode)
  (flycheck-mode . flycheck-elm-setup))

(provide 'lang-elm)
;;; lang-elm.el ends here
