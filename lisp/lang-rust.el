;;; lang-rust.el --- Rust -*- lexical-binding: t; -*-

;;; Commentary:
;; Rust is a systems programming language that runs blazingly fast, prevents
;; segfaults, and guarantees thread safety.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-keybinds))

;;;
;; Packages

(req-package rust-mode
  :mode "\\.rs$"
  :general
  (:keymaps
   'rust-mode-map
   :states 'normal
   :prefix my-local-leader-key
   "t a" 'cargo-process-test)
  :init
  (setq rust-format-on-save (executable-find "rustfmt"))
  :config
  (set-popup-buffer (rx bos "*rustfmt*" eos)))

(req-package racer
  :require rust-mode
  :after rust-mode
  :config
  (set-doc-fn 'rust-mode #'racer-describe)
  ;; jump-fn #'racer-find-definition
  ;; pop-fn #'pop-tag-mark

  (set-evil-state 'racer-help-mode 'motion)
  (set-popup-buffer (rx bos "*Racer Help*" eos))

  (add-hooks-pair 'rust-mode 'racer-mode))

(req-package flycheck-rust
  :require flycheck rust-mode
  :after rust-mode
  :config
  (add-hooks-pair 'rust-mode 'flycheck-mode)
  (add-hooks-pair 'flycheck-mode 'flycheck-rust-setup))

(req-package lsp-rust
  :disabled t
  :require lsp-mode rust-mode
  :after rust-mode
  :init
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
  :config
  (add-hooks-pair 'rust-mode 'lsp-rust-enable))

(req-package cargo
  :require rust-mode
  :after rust-mode
  :general
  (:keymaps
   'cargo-process-mode-map
   :states '(normal motion insert evil)
   "q" 'quit-window)
  :config
  (set-popup-buffer (rx bos "*Cargo " (one-or-more anything) "*" eos))

  (add-hooks-pair 'rust-mode 'cargo-minor-mode))

(provide 'lang-rust)
;;; lang-rust.el ends here
