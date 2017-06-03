;;; feature-syntax-checker.el --- Syntax checking

;;; Commentary:
;; Catching your errors.

;;; Code:
;; pkg-info doesn't get autoloaded when `flycheck-version' needs it.
(autoload 'pkg-info-version-info "pkg-info")

(use-package flycheck
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; Popup for errors
(use-package flycheck-pos-tip
  :after flycheck
  :config
  (setq flycheck-pos-tip-timeout 10
        flycheck-display-errors-delay 0.5)
  (flycheck-pos-tip-mode +1))

(provide 'feature-syntax-checker)
;;; feature-syntax-checker.el ends here
