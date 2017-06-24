;;; lang-markdown.el --- Markdown

;;; Commentary:
;; Markdown is a lightweight markup language with plain text formatting syntax.

;;; Code:
(use-package markdown-mode
  :mode
  (("/README$" . markdown-mode)
   ("/README\\.md$" . gfm-mode))
  :commands (markdown-mode
             gfm-mode
             markdown-toggle-markup-hiding)
  :preface
  (autoload 'sp-local-pair "smartparens")

  (defun markdown|evil-insert-state-entry ()
    "Setup markdown edit mode."
    (when (member major-mode '(markdown-mode gfm-mode))
      (markdown-toggle-markup-hiding -1)))

  (defun markdown|evil-insert-state-exit ()
    "Reset markdown edit mode."
    (when (member major-mode '(markdown-mode gfm-mode))
      (markdown-toggle-markup-hiding +1)))
  :init
  (setq markdown-enable-wiki-links t
        markdown-enable-math t
        markdown-fontify-code-blocks-natively t
        markdown-italic-underscore t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-additional-languages '("sh")
        markdown-command
        "pandoc -f markdown -t html5 -s --self-contained --smart")
  :config
  (add-hook 'markdown-mode-hook
            #'(lambda()
                (setq line-spacing 2
                      fill-column 80
                      markdown-hide-markup t)

                (linum-mode -1)
                (auto-fill-mode +1)

                (variable-pitch-mode +1)
                (customize-set-variable 'markdown-header-scaling t)

                (with-eval-after-load "evil"
                  (add-hook 'evil-insert-state-entry-hook #'markdown|evil-insert-state-entry)
                  (add-hook 'evil-insert-state-exit-hook #'markdown|evil-insert-state-exit))))

  (sp-local-pair
   '(markdown-mode gfm-mode)
   "\`\`\`" "\`\`\`" :post-handlers '(("||\n" "RET"))))

(use-package markdown-toc :commands markdown-toc-generate-toc)

(provide 'lang-markdown)
;;; lang-markdown.el ends here
