;;; lang-ebook.el --- E-book file formats -*- lexical-binding: t; -*-

;;; Commentary:
;; Support for ePUB and PDF.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

;;;
;; Packages

(req-package justify-kp
  :el-get t :ensure nil
  :commands
  (pj-justify
   pj-line-width))

(req-package nov
  :mode ("\\.epub$" . nov-mode)
  :general
  (:keymaps 'nov-mode-map :states 'normal
            "[p"      'nov-previous-document
            "]p"      'nov-next-document
            "<up>"    'nov-scroll-down
            "<down>"  'nov-scroll-up
            "<left>"  'nov-previous-document
            "<right>" 'nov-next-document
            "q"       'kill-this-buffer)
  :hook
  (nov-mode . visual-fill-column-mode)
  (nov-mode . hide-fringes)
  (nov-mode . readable-mode)
  (nov-mode . +nov-mode-delayed-render)
  (nov-post-html-render . +nov-post-html-render-hook)
  :preface
  (defun +nov-mode-delayed-render ()
    (run-with-idle-timer 0.2 nil 'nov-render-document))
  :init
  (setq nov-save-place-file (concat my-data-dir "nov-places")
        nov-text-width most-positive-fixnum)
  :config
  (defun +nov-window-configuration-change-hook ()
    (+nov-post-html-render-hook)
    (remove-hook 'window-configuration-change-hook
                 '+nov-window-configuration-change-hook
                 t))

  (defun +nov-post-html-render-hook ()
    (if (get-buffer-window)
        (let ((max-width (pj-line-width))
              buffer-read-only)
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (not (looking-at "^[[:space:]]*$"))
                (goto-char (line-end-position))
                (when (> (shr-pixel-column) max-width)
                  (goto-char (line-beginning-position))
                  (pj-justify)))
              (forward-line 1))))
      (add-hook 'window-configuration-change-hook
                '+nov-window-configuration-change-hook
                nil t))))

(req-package pdf-tools
  :mode ("\\.pdf$" . pdf-view-mode)
  :commands
  (pdf-tools-install
   pdf-view-bookmark-jump-handler)
  :general
  (:keymaps 'pdf-view-mode-map
            "]]"  'pdf-view-scroll-up-or-next-page
            "[["  'pdf-view-scroll-down-or-previous-page
            "]p"  'pdf-view-next-page
            "[p"  'pdf-view-previous-page
            "h"   'pdf-view-previous-page
            "j"   'pdf-view-next-line-or-next-page
            "k"   'pdf-view-previous-line-or-previous-page
            "l"   'pdf-view-next-page
            "q"   'kill-this-buffer))

(req-package bibliothek
  :commands bibliothek
  :init
  (setq bibliothek-path '("~/books" "~/documents/research/papers"))
  :config
  (set-evil-state 'bibliothek-mode 'motion))

(provide 'lang-ebook)
;;; lang-ebook.el ends here
