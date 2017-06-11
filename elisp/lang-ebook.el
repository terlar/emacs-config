;;; lang-ebook.el --- E-book file formats

;;; Commentary:
;; Support for ePUB and PDF.

;;; Code:
(use-package ereader
  :mode
  ("\\.epub$" . ereader-mode)
  :commands ereader-mode
  :config
  (add-hook 'ereader-mode-hook
            #'(lambda ()
                (page-break-lines-mode +1))))

(use-package pdf-tools
  :mode
  ("\\.pdf$" . pdf-view-mode)
  :commands (pdf-view-mode pdf-tools-install))

(provide 'lang-ebook)
;;; lang-ebook.el ends here
