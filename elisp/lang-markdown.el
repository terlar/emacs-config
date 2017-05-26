;;; lang-markdown.el --- Markdown
;;; Commentary:
;;; Markdown is a lightweight markup language with plain text formatting syntax.
;;; Code:
(use-package markdown-mode
  :mode
  ("\\.m\\(d\\|arkdown\\)$"
   "/README$"
   ("/README\\.md$" . gfm-mode))
  :commands (markdown-mode gfm-mode)
  :init
  (setq
   markdown-enable-wiki-links t
   markdown-enable-math t
   markdown-italic-underscore t
   markdown-make-gfm-checkboxes-buttons t
   markdown-gfm-additional-languages '("sh"))
  :config
  (add-hook 'markdown-mode-hook
            #'(lambda()
                (linum-mode -1)
                (auto-fill-mode +1)
                (variable-pitch-mode +1)
                (setq line-spacing 2
                      fill-column 80)))
  (sp-local-pair
   '(markdown-mode gfm-mode)
   "\`\`\`" "\`\`\`" :post-handlers '(("||\n" "RET")))

  ;; Typography
  (set-face-attribute 'markdown-pre-face nil :inherit 'fixed-pitch)
  (set-face-attribute 'markdown-inline-code-face nil :inherit 'fixed-pitch)

  (set-face-attribute 'markdown-header-face-1 nil :height 1.8)
  (set-face-attribute 'markdown-header-face-2 nil :height 1.4)
  (set-face-attribute 'markdown-header-face-3 nil :height 1.2)
  (set-face-attribute 'markdown-header-face-4 nil :height 1.0)
  (set-face-attribute 'markdown-header-face-5 nil :height 1.0)
  (set-face-attribute 'markdown-header-face-6 nil :height 1.0))

(use-package markdown-toc :commands markdown-toc-generate-toc)

(provide 'lang-markdown)
;;; lang-markdown.el ends here
