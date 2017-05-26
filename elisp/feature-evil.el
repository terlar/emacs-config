;;; feature-evil.el --- Extensible vi layer
;;; Commentary:
;;; The way of the vi!
;;; Code:
(use-package evil :demand t
  :init
  (setq
   evil-want-C-u-scroll t
   evil-want-visual-char-semi-exclusive t
   evil-want-Y-yank-to-eol t
   evil-magic t
   evil-echo-state t
   evil-indent-convert-tabs t
   evil-ex-search-vim-style-regexp t
   evil-ex-substitute-global t
   ;; Column range for ex commands
   evil-ex-visual-char-range t
   evil-insert-skip-empty-lines t
   evil-mode-line-format 'nil
   ;; More vim-like behavior
   evil-symbol-word-search t
   ;; Don't activate mark on shift-click
   shift-select-mode nil)
  :config
  (evil-mode +1)
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; Default modes
  (dolist (mode '(tabulated-list-mode
                  view-mode comint-mode term-mode
                  calendar-mode Man-mode grep-mode))
    (evil-set-initial-state mode 'emacs))
  (dolist (mode '(help-mode debugger-mode))
    (evil-set-initial-state mode 'normal))

  ;; Make `try-expand-dabbrev' from `hippie-expand' work in mini-buffer
  ;; @see `he-dabbrev-beg', so we need re-define syntax for '/'
  (defun minibuffer-inactive-mode-hook-setup ()
    (set-syntax-table (let* ((table (make-syntax-table)))
                        (modify-syntax-entry ?/ "." table)
                        table)))
  (add-hook 'minibuffer-inactive-mode-hook #'minibuffer-inactive-mode-hook-setup))

(provide 'feature-evil)
;;; feature-evil.el ends here
