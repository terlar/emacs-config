;;; lang-ebook.el --- E-book file formats -*- lexical-binding: t; -*-

;;; Commentary:
;; Support for ePUB and PDF.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-keybinds))

;;;
;; Packages

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
  (eval-when-compile
    (defvar nov-text-width)
    (defvar cwm-centered-window-width))

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
