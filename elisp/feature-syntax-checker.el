;;; feature-syntax-checker.el --- Syntax checking -*- lexical-binding: t; -*-

;;; Commentary:
;; Catching your errors.

;;; Code:

;; pkg-info doesn't get autoloaded when `flycheck-version' needs it.
(autoload 'pkg-info-version-info "pkg-info")

;;;
;; Packages

(use-package flycheck
  :diminish flycheck-mode
  :commands (flycheck-mode
             flycheck-list-errors flycheck-buffer
             flycheck-add-next-checker)
  :init
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; Inline error messages
(use-package flycheck-inline :ensure nil :pin manual
  :load-path "vendor/flycheck-inline/"
  :after flycheck
  :commands flycheck-inline-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)
  (setq flycheck-display-errors-delay 0.5))

;; Pop-up error messages
(use-package flycheck-popup-tip
  :disabled t
  :after flycheck
  :commands flycheck-popup-tip-mode
  :config
  (add-hooks-pair 'flycheck-mode 'flycheck-popup-tip-mode))

(provide 'feature-syntax-checker)
;;; feature-syntax-checker.el ends here
