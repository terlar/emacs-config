;;; lang-rust.el --- Rust

;;; Commentary:
;; Rust is a systems programming language that runs blazingly fast, prevents
;; segfaults, and guarantees thread safety.

;;; Code:
(require 'base-lib)
(require 'base-keybinds)

;;;
;; Packages

(use-package rust-mode
  :mode "\\.rs$"
  :general
  (:keymaps 'rust-mode-map :states 'normal
            :prefix my-local-leader-key
            "t a" 'cargo-process-test)
  :config
  (setq rust-format-on-save (executable-find "rustfmt")))

(use-package lsp-rust
  :after lsp-mode
  :defines lsp-rust-rls-command
  :commands lsp-rust-enable
  :init (add-hooks-pair 'rust-mode 'lsp-rust-enable)
  :config (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls")))

(use-package cargo
  :commands cargo-minor-mode
  :init (add-hooks-pair 'rust-mode 'cargo-minor-mode))

(provide 'lang-rust)
;;; lang-rust.el ends here
