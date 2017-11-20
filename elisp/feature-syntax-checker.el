;;; feature-syntax-checker.el --- Syntax checking -*- lexical-binding: t; -*-

;;; Commentary:
;; Catching your errors.

;;; Code:

(eval-when-compile
  (require 'base-package))

;; pkg-info doesn't get autoloaded when `flycheck-version' needs it.
(autoload 'pkg-info-version-info "pkg-info")

;;;
;; Packages

(req-package flycheck
  :diminish flycheck-mode
  :commands
  (flycheck-mode
   flycheck-list-errors flycheck-buffer
   flycheck-add-next-checker)
  :init
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (set-evil-state 'flycheck-error-list-mode 'motion)
  (set-popup-buffer (rx bos "*Flycheck errors*" eos)
                    (rx bos "*Flycheck checker*" eos)))

;; Inline error messages
(req-package flycheck-inline
  :require flycheck
  :loader :el-get
  :commands flycheck-inline-mode
  :init
  (setq flycheck-display-errors-delay 0.5)
  (add-hooks-pair 'flycheck-mode 'flycheck-inline-mode))

;; Pop-up error messages
(req-package flycheck-popup-tip
  :require flycheck
  :disabled t
  :commands flycheck-popup-tip-mode
  :init
  (add-hooks-pair 'flycheck-mode 'flycheck-popup-tip-mode))

(provide 'feature-syntax-checker)
;;; feature-syntax-checker.el ends here
