;;; lang-elm.el --- Elm

;;; Commentary:
;; Elm is a domain-specific programming language for declaratively creating web
;; browser-based graphical user interfaces. Elm is purely functional, and is
;; developed with emphasis on usability, performance, and robustness. It
;; advertises "no runtime exceptions in practice," made possible by the Elm
;; compiler's static type checking.

;;; Code:
(require 'base-lib)

(use-package elm-mode :mode ("\\.elm$")
  :commands elm-mode
  :init
  (with-eval-after-load "company"
    (push-company-backends 'elm-mode '(company-elm)))
  (add-hooks-pair 'elm-mode
                  '(flycheck-mode
                    rainbow-delimiters-mode))
  :config
  (setq elm-format-on-save t))

(use-package flycheck-elm
  :after elm-mode
  :commands flycheck-elm-setup
  :config
  (add-hooks-pair 'flycheck-mode 'flycheck-elm-setup))

(provide 'lang-elm)
;;; lang-elm.el ends here
