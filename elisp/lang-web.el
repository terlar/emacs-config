;;; lang-web.el --- Web development

;;; Commentary:
;; All things web.

;;; Code:
(require 'base-lib)

(use-package web-mode
  :mode
  (("\\.h?html?$" . web-mode)
   ("\\.\\(tpl\\|blade\\)\\(\\.php\\)?$" . web-mode)
   ("\\.[agj]sp$" . web-mode)
   ("\\.as[cp]x$" . web-mode)
   ("\\.erb$" . web-mode)
   ("\\.mustache$" . web-mode)
   ("\\.tsx$" . web-mode)
   ("\\.jsx$" . web-mode)
   ("\\.djhtml$" . web-mode)
   ("wp-content/themes/.+/.+\\.php$" . web-mode))
  :init
  (add-hooks-pair 'web-mode 'turn-off-smartparens-mode)
  (with-eval-after-load "company"
    (push-company-backends 'web-mode
                           '(company-tern
                             company-css
                             company-web-html
                             company-files)))
  :config
  (setq web-mode-enable-html-entities-fontification t
        ;; Highlight enclosing tags of the element under cursor
        web-mode-enable-current-element-highlight t)

  ;; No padding for nested sections inside HTML
  (with-eval-after-load "editorconfig"
    (add-hook 'editorconfig-custom-hooks
              #'(lambda (_)
                  (setq web-mode-block-padding 0
                        web-mode-script-padding 0
                        web-mode-style-padding 0)))))

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

(use-package company-web :after web-mode)

(use-package slim-mode :mode "\\.slim$")

(use-package haml-mode :mode "\\.haml$")

(use-package pug-mode :mode ("\\.jade$" "\\.pug$"))

;; configure CSS mode company backends
(use-package css-mode
  :mode (("\\.css$"  . css-mode)
         ("\\.scss$" . scss-mode))
  :init
  (push-company-backends 'css-mode '(company-css
                                     company-dabbrev-code
                                     company-files)))

(provide 'lang-web)
;;; lang-web.el ends here
