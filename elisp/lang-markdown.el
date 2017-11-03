;;; lang-markdown.el --- Markdown -*- lexical-binding: t; -*-

;;; Commentary:
;; Markdown is a lightweight markup language with plain text formatting syntax.

;;; Code:

(autoload 'sp-local-pair "smartparens")

;;;
;; Packages

(use-package markdown-mode
  :mode
  "/README$"
  ("/README\\.md$" . gfm-mode)
  :commands (markdown-mode
             gfm-mode
             markdown-toggle-markup-hiding)
  :preface
  (defun markdown-setup ()
    (setq fill-column 80
          line-spacing 2)
    (customize-set-variable 'markdown-header-scaling t))

  (defun markdown|add-insert-state-hooks ()
    (add-hook 'evil-insert-state-entry-hook #'markdown|evil-insert-state-entry nil t)
    (add-hook 'evil-insert-state-exit-hook #'markdown|evil-insert-state-exit nil t))

  (defun markdown|evil-insert-state-entry ()
    "Setup markdown edit mode."
    (markdown-toggle-markup-hiding -1))

  (defun markdown|evil-insert-state-exit ()
    "Reset markdown edit mode."
    (markdown-toggle-markup-hiding +1))
  :init
  (setq
   markdown-command
   "pandoc -f markdown -t html5 -s --self-contained --smart"
   markdown-enable-math t
   markdown-enable-wiki-links t
   markdown-fontify-code-blocks-natively t
   markdown-gfm-additional-languages '("sh")
   markdown-hide-markup t
   markdown-italic-underscore t
   markdown-make-gfm-checkboxes-buttons t)
  :config
  (add-hooks-pair 'markdown-mode
                  '(auto-fill-mode
                    variable-pitch-mode
                    markdown-setup
                    markdown|add-insert-state-hooks))

  (sp-local-pair
   '(markdown-mode gfm-mode)
   "\`\`\`" "\`\`\`" :post-handlers '(("||\n" "RET"))))

(use-package markdown-toc :commands markdown-toc-generate-toc)

(provide 'lang-markdown)
;;; lang-markdown.el ends here
