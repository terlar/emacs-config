;;; syntax-checker.el --- Syntax checking -*- lexical-binding: t; -*-

;;; Commentary:
;; Catching your errors.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-keybinds))

;; pkg-info doesn't get autoloaded when `flycheck-version' needs it.
(autoload 'pkg-info-version-info "pkg-info")

;;;
;; Built-ins

(use-package flymake
  :preface
  (defun flymake-diagnostics-next-error ()
    (interactive)
    (next-line)
    (when (eobp) (previous-line))
    (flymake-show-diagnostic (point)))

  (defun flymake-diagnostics-prev-error ()
    (interactive)
    (previous-line)
    (flymake-show-diagnostic (point)))
  :hook
  (flymake-mode . (lambda () (setq next-error-function 'flymake-goto-next-error)))
  :custom
  (help-at-pt-timer-delay 0.1)
  (help-at-pt-display-when-idle '(flymake-diagnostic))
  :general
  (:keymaps 'flymake-diagnostics-buffer-mode-map :states '(normal motion emacs)
            "C-n" 'flymake-diagnostics-next-error
            "C-p" 'flymake-diagnostics-prev-error
            "j"   'flymake-diagnostics-next-error
            "k"   'flymake-diagnostics-prev-error
            "RET" 'flymake-goto-diagnostic
            "TAB" 'flymake-show-diagnostic)
  :config
  (set-evil-state 'flymake-diagnostics-buffer-mode 'motion))

;;;
;; Packages

(req-package flycheck
  :demand t
  :commands
  (flycheck-list-errors
   flycheck-buffer
   flycheck-add-next-checker)
  :general
  (:keymaps 'flycheck-error-list-mode-map :states '(normal motion emacs)
            "C-n" 'flycheck-error-list-next-error
            "C-p" 'flycheck-error-list-previous-error
            "j"   'flycheck-error-list-next-error
            "k"   'flycheck-error-list-previous-error
            "RET" 'flycheck-error-list-goto-error)
  (:keymaps 'motion
            "[e" 'previous-error
            "]e" 'next-error)
  :init
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (set-evil-state 'flycheck-error-list-mode 'motion))

;; Pos-frame error messages
(req-package flycheck-posframe
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (flycheck-posframe-configure-pretty-defaults))

;; Inline error messages
(req-package flycheck-inline
  :el-get t :ensure nil
  :disabled t
  :hook (after-init . flycheck-inline-mode)
  :init
  (setq flycheck-display-errors-delay 0.5))

;; Pop-up error messages
(req-package flycheck-popup-tip
  :disabled t
  :hook (flycheck-mode . flycheck-popup-tip-mode))

(provide 'feature-syntax-checker)
;;; syntax-checker.el ends here
