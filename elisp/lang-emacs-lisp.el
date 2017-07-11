;;; lang-emacs-lisp.el --- Emacs Lisp

;;; Commentary:
;; Emacs Lisp is a dialect of the Lisp programming language used as a scripting
;; language by Emacs (a text editor family most commonly associated with GNU
;; Emacs.

;;; Code:
(add-hooks-pair 'emacs-lisp-mode
                '(eldoc-mode
                  highlight-quoted-mode
                  auto-compile-on-save-mode
                  rainbow-delimiters-mode
                  flycheck-mode))

;;;
;; Packages

(use-package auto-compile
  :commands (auto-compile-on-save-mode auto-compile-byte-compile)
  :preface
  (defun my|emacs-lisp-load-after-compile (success)
    "Reload the current emacs-lisp file after it's recompiled, if an older
version is loaded."
    (when (eq success t)
      (let ((buffer-path (file-truename buffer-file-name)))
        (when (assoc buffer-path load-history)
          (load-file buffer-path)))))
  :init
  (add-hooks-pair 'emacs-lisp-mode
                  '(auto-compile-on-load-mode
                    auto-compile-on-save-mode))
  :config
  (setq auto-compile-display-buffer nil
        auto-compile-use-mode-line nil)

  (advice-add #'auto-compile-byte-compile :filter-return #'my|emacs-lisp-load-after-compile))

(use-package highlight-quoted
  :commands highlight-quoted-mode)

(use-package slime
  :config
  (setq inferior-lisp-program "clisp")
  (require 'slime-fuzzy))

(provide 'lang-emacs-lisp)
;;; lang-emacs-lisp.el ends here
