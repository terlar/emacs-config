;;; lang-web.el --- Web development

;;; Commentary:
;; All things web.

;;; Code:
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
  (add-hook 'web-mode-hook
            #'(lambda ()
                (turn-off-smartparens-mode)
                (setq-local company-backends
                            '((company-tern
                               company-css
                               company-web-html
                               company-files)))))
  :config
  (setq web-mode-enable-html-entities-fontification t
        ;; Highlight enclosing tags of the element under cursor
        web-mode-enable-current-element-highlight t))

;; Snippets for HTML
(use-package emmet-mode
  :after web-mode
  :commands emmet-mode
  :preface
  (defvar emmet-mode-keymap (make-sparse-keymap))
  :init
  (dolist (hook '(css-mode-hook
                  web-mode-hook
                  html-mode-hook haml-mode-hook
                  nxml-mode-hook rsjx-mode-hook))
    (add-hook hook #'emmet-mode))
  :config
  (setq emmet-move-cursor-between-quotes t))

(use-package company-web
  :when (package-installed-p 'company)
  :after web-mode)

(use-package slim-mode :mode "\\.slim$")

(use-package haml-mode :mode "\\.haml$")

(use-package pug-mode
  :mode ("\\.jade$" "\\.pug$"))

;; configure CSS mode company backends
(use-package css-mode
  :mode ("\\.css$"
         ("\\.scss$" . scss-mode))
  :init
  (add-hook 'css-mode-hook
            #'(lambda ()
                (setq-local company-backends
                            '((company-css
                               company-dabbrev-code
                               company-files))))))

;; Live refresh of web pages
(use-package impatient-mode
  :commands (impatient-mode))

(provide 'lang-web)
;;; lang-web.el ends here
