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
  (add-hook 'elm-mode-hook
            #'(lambda ()
                (flycheck-mode +1)
                (rainbow-delimiters-mode +1)))
  :config
  (setq elm-format-on-save t))

(use-package flycheck-elm
  :after elm-mode
  :commands flycheck-elm-setup
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-elm-setup))

(provide 'lang-elm)
;;; lang-elm.el ends here
