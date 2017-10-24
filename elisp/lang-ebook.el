;;; lang-ebook.el --- E-book file formats

;;; Commentary:
;; Support for ePUB and PDF.

;;; Code:
(require 'base-vars)
(require 'base-keybinds)

(use-package nov :mode ("\\.epub$" . nov-mode)
  :general
  (:keymaps 'nov-mode-map :states 'normal
            "]p"      'nov-next-document
            "[p"      'nov-previous-document
            "<up>"    'nov-scroll-down
            "<down>"  'nov-scroll-up
            "<left>"  'nov-previous-document
            "<right>" 'nov-next-document)
  :preface
  (defvar nov-text-width)
  (defvar cwm-centered-window-width)

  (defun epub-setup ()
    (setq nov-save-place-file (concat my-data-dir "nov-places")
          nov-text-width most-positive-fixnum
          cwm-centered-window-width 120)
    (face-remap-add-relative 'variable-pitch :family "Noto Serif"
                             :height 1.5))
  :init
  (add-hooks-pair 'nov-mode '(epub-setup
                              centered-window-mode)))

(use-package pdf-tools :mode ("\\.pdf$" . pdf-view-mode)
  :commands (pdf-view-mode pdf-tools-install))

(provide 'lang-ebook)
;;; lang-ebook.el ends here
