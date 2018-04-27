;;; lang-web.el --- Web development -*- lexical-binding: t; -*-

;;; Commentary:
;; All things web.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(req-package cakecrumbs
  :hook
  ((css-mode
    nxml-mode
    pug-mode
    web-mode) . cakecrumbs-mode))

(req-package web-mode
  :mode "\\.\\(phtml\\|php\\|[agj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tmpl\\)$"
  :hook
  (web-mode . flycheck-mode)
  (web-mode . turn-off-smartparens-mode)
  :init
  (setq web-mode-enable-html-entities-fontification t
        ;; Highlight enclosing tags of the element under cursor
        web-mode-enable-current-element-highlight t)
  :config
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'html-tidy 'web-mode))

  ;; No padding for nested sections inside HTML
  (with-eval-after-load 'editorconfig
    (add-hook 'editorconfig-custom-hooks
              (lambda (_)
                (setq web-mode-block-padding 0
                      web-mode-script-padding 0
                      web-mode-style-padding 0)))))

(req-package company-web
  :requires company
  :commands
  (company-web-html
   company-web-jade
   company-web-slim
   company-css)
  :init
  (set-company-backends 'web-mode
                        '(company-web-html
                          company-web-jade
                          company-web-slim
                          company-css)))

;; Snippets and Zen Coding for HTML
(req-package emmet-mode
  :hook
  (css-mode web-mode html-mode haml-mode nxml-mode rsjx-mode)
  :preface
  (defvar emmet-mode-keymap (make-sparse-keymap))
  :init
  (setq emmet-move-cursor-between-quotes t))

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

;;;
;; Skewer

(req-package skewer-mode
  :commands (skewer-mode run-skewer)
  :general
  (:keymaps 'skewer-mode-map :states 'normal :prefix ","
            "sE" '(skewer-eval-last-expression)
            "se" '(skewer-eval-defun)
            "sf" '(skewer-load-buffer)))

(req-package skewer-css :ensure nil
  :require skewer-mode
  :commands skewer-css-mode
  :general
  (:keymaps 'skewer-css-mode-map :states 'normal :prefix ","
            "se" '(skewer-css-eval-current-declaration)
            "sr" '(skewer-css-eval-current-rule)
            "sb" '(skewer-css-eval-buffer)
            "sc" '(skewer-css-clear-all)))

(req-package skewer-html :ensure nil
  :require skewer-mode
  :commands skewer-html-mode
  :general
  (:keymaps 'skewer-html-mode-map :states 'normal :prefix ","
            "se" '(skewer-html-eval-tag)))

(provide 'lang-web)
;;; lang-web.el ends here
