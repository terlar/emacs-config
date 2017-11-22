;;; lang-markdown.el --- Markdown -*- lexical-binding: t; -*-

;;; Commentary:
;; Markdown is a lightweight markup language with plain text formatting syntax.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-lib))

;;;
;; Packages

(req-package markdown-mode
  :mode
  "\\.markdown$"
  "\\.md$"
  "/README$"
  ("/README\\.md$" . gfm-mode)
  :init
  (setq
   markdown-command
   "pandoc -f markdown -t html5 -s --self-contained --smart"
   markdown-enable-math t
   markdown-enable-wiki-links t
   markdown-fontify-code-blocks-natively t
   markdown-hide-markup t
   markdown-italic-underscore t
   markdown-make-gfm-checkboxes-buttons t)

  (customize-set-variable 'markdown-header-scaling t)
  :config
  (set-evil-state-change
   '(markdown-mode gfm-mode)
   :on-insert (lambda () (markdown-toggle-markup-hiding 0))
   :on-normal (lambda () (markdown-toggle-markup-hiding 1)))

  (add-hooks-pair 'markdown-mode
                  '(auto-fill-mode
                    readability-mode)))

(req-package edit-indirect)

(req-package markdown-toc
  :require markdown-mode
  :commands markdown-toc-generate-toc)

(provide 'lang-markdown)
;;; lang-markdown.el ends here
