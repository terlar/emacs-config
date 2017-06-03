;;; lang-emacs-lisp.el --- Emacs Lisp

;;; Commentary:
;; Emacs Lisp is a dialect of the Lisp programming language used as a scripting
;; language by Emacs (a text editor family most commonly associated with GNU
;; Emacs.

;;; Code:
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (eldoc-mode +1)
              (highlight-quoted-mode +1)
              (auto-compile-on-save-mode +1)
              (rainbow-delimiters-mode +1)

              (flycheck-mode +1)))

;;;
;; Plugins
(use-package auto-compile
  :commands (auto-compile-on-save-mode auto-compile-byte-compile)
  :preface
  (defun my-emacs-lisp-load-after-compile (success)
    "Reload the current emacs-lisp file after it's recompiled, if an older
version is loaded."
    (when (eq success t)
      (let ((buffer-path (file-truename buffer-file-name)))
        (when (assoc buffer-path load-history)
          (load-file buffer-path)))))
  :config
  (setq auto-compile-display-buffer nil
        auto-compile-use-mode-line nil)

  (advice-add #'auto-compile-byte-compile :filter-return #'my-emacs-lisp-load-after-compile))

(use-package highlight-quoted
  :commands highlight-quoted-mode)

(use-package slime
  :config
  (setq inferior-lisp-program "clisp")
  (require 'slime-fuzzy))

(provide 'lang-emacs-lisp)
;;; lang-emacs-lisp.el ends here
