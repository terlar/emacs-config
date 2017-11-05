;;; lang-web.el --- Web development -*- lexical-binding: t; -*-

;;; Commentary:
;; All things web.

;;; Code:

(eval-when-compile
  (defvar aggressive-indent-excluded-modes))

(autoload 'push-company-backends "base-lib")
(autoload 'flycheck-add-mode "flycheck")

;;;
;; Packages

(use-package web-mode
  :mode "\\.\\(phtml\\|php|[agj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tmpl\\)$"
  :init
  (add-hooks-pair 'web-mode
                  '(flycheck-mode
                    turn-off-smartparens-mode))
  :config
  (setq web-mode-enable-html-entities-fontification t
        ;; Highlight enclosing tags of the element under cursor
        web-mode-enable-current-element-highlight t)

  (with-eval-after-load "flycheck"
    (flycheck-add-mode 'html-tidy 'web-mode))

  ;; No padding for nested sections inside HTML
  (with-eval-after-load "editorconfig"
    (add-hook 'editorconfig-custom-hooks
              #'(lambda (_)
                  (setq web-mode-block-padding 0
                        web-mode-script-padding 0
                        web-mode-style-padding 0)))))

(use-package company-web :after web-mode
  :init
  (with-eval-after-load "company"
    (push-company-backends 'web-mode
                           '(company-web-html
                             company-web-jade
                             company-web-slim
                             company-css))))

;; Snippets and Zen Coding for HTML
(use-package emmet-mode
  :after web-mode
  :commands emmet-mode
  :preface
  (defvar emmet-mode-keymap (make-sparse-keymap))
  :init
  (add-hooks-pair '(css-mode
                    web-mode
                    html-mode haml-mode
                    nxml-mode rsjx-mode)
                  'emmet-mode)
  :config
  (setq emmet-move-cursor-between-quotes t))

(use-package slim-mode :mode "\\.slim$"
  :config
  (with-eval-after-load "aggressive-indent"
    (push 'slim-mode aggressive-indent-excluded-modes)))

(use-package haml-mode :mode "\\.haml$")

(use-package pug-mode :mode ("\\.jade$" "\\.pug$")
  :config
  (with-eval-after-load "aggressive-indent"
    (push 'pug-mode aggressive-indent-excluded-modes)))

;; configure CSS mode company backends
(use-package css-mode
  :mode "\\.s?css$"
  :init
  (push-company-backends 'css-mode 'company-css))

(provide 'lang-web)
;;; lang-web.el ends here
