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

(req-package elm-mode
  :mode ("\\.elm$")
  :commands elm-mode
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
  (unless (executable-find "elm-format")
    (warn "elm-mode: couldn't find elm-format; auto-formatting won't work"))
  (unless (executable-find "elm-oracle")
    (warn "elm-mode: couldn't find elm-oracle; completion and documentation won't work"))
  (unless (executable-find "elm-repl")
    (warn "elm-mode: couldn't find elm-repl; repl and eval won't work"))

  (set-doc-fn 'elm-mode #'elm-oracle-doc-at-point)
  (smart-jump-register :modes 'elm-mode)

  (set-aggressive-indent 'elm-mode :disabled t)
  (set-company-backends 'elm-mode 'company-elm)

  (set-popup-buffer (rx bos "*elm-make*" eos)
                    (rx bos "*elm-test*" eos))

  (add-hooks-pair 'elm-mode 'rainbow-delimiters-mode))

(req-package flycheck-elm
  :require flycheck elm-mode
  :after elm-mode
  :commands flycheck-elm-setup
  :config
  (flycheck-elm-setup))

(provide 'lang-elm)
;;; lang-elm.el ends here
