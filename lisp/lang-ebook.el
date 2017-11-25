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
  :loader :el-get
  :commands
  (pj-justify
   pj-line-width))

(req-package nov
  :mode ("\\.epub$" . nov-mode)
  :init
  (setq nov-save-place-file (concat my-data-dir "nov-places")
        nov-text-width most-positive-fixnum)
  :config
  (defun +nov-delayed-render ()
    "Rerender nov after load."
    (run-with-idle-timer 0.2 nil 'nov-render-document))

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
                nil t)))
  (add-hook 'nov-post-html-render-hook '+nov-post-html-render-hook)

  (add-hooks-pair 'nov-mode '(+nov-delayed-render
                              centered-window-mode
                              hide-fringes
                              readability-mode)))

(req-package pdf-tools
  :mode ("\\.pdf$" . pdf-view-mode)
  :commands pdf-tools-install)

(provide 'lang-ebook)
;;; lang-ebook.el ends here
