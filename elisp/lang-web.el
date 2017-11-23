;;; lang-web.el --- Web development -*- lexical-binding: t; -*-

;;; Commentary:
;; All things web.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(req-package web-mode
  :mode "\\.\\(phtml\\|php\\|[agj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tmpl\\)$"
  :init
  (setq web-mode-enable-html-entities-fontification t
        ;; Highlight enclosing tags of the element under cursor
        web-mode-enable-current-element-highlight t)
  :config
  (add-hooks-pair 'web-mode
                  '(flycheck-mode
                    turn-off-smartparens-mode))

  (with-eval-after-load "flycheck"
    (flycheck-add-mode 'html-tidy 'web-mode))

  ;; No padding for nested sections inside HTML
  (with-eval-after-load "editorconfig"
    (add-hook 'editorconfig-custom-hooks
              (lambda (_)
                (setq web-mode-block-padding 0
                      web-mode-script-padding 0
                      web-mode-style-padding 0)))))

(req-package company-web
  :require web-mode
  :after web-mode
  :config
  (set-company-backends 'web-mode
                        '(company-web-html
                          company-web-jade
                          company-web-slim
                          company-css)))

;; Snippets and Zen Coding for HTML
(req-package emmet-mode
  :demand t
  :preface
  (defvar emmet-mode-keymap (make-sparse-keymap))
  :init
  (setq emmet-move-cursor-between-quotes t)
  :config
  (add-hooks-pair '(css-mode
                    web-mode
                    html-mode haml-mode
                    nxml-mode rsjx-mode)
                  'emmet-mode))

(req-package slim-mode
  :mode "\\.slim$"
  :config
  (set-aggressive-indent 'slim-mode :disabled t))

(req-package haml-mode
  :mode "\\.haml$")

(req-package pug-mode
  :mode
  "\\.\\(pug\\|jade\\)$"
  :config
  (set-aggressive-indent 'pug-mode :disabled t))

;; configure CSS mode company backends
(req-package css-mode
  :mode "\\.s?css$"
  :config
  (set-company-backends 'css-mode 'company-css))

(provide 'lang-web)
;;; lang-web.el ends here
