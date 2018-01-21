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
  (:keymaps 'rust-mode-map :major-modes t
            :prefix my-local-leader-key
            "t" 'cargo-process-test)
  :init
  (setq rust-format-on-save (executable-find "rustfmt"))
  :config
  (set-popup-buffer (rx bos "*rustfmt*" eos)))

(req-package racer
  :hook (rust-mode . racer-mode)
  :config
  (set-doc-fn 'rust-mode #'racer-describe)
  ;; jump-fn #'racer-find-definition
  ;; pop-fn #'pop-tag-mark

  (set-evil-state 'racer-help-mode 'motion)
  (set-popup-buffer (rx bos "*Racer Help*" eos)))

(req-package flycheck-rust
  :requires flycheck
  :hook
  (rust-mode . flycheck-mode)
  (flycheck-mode . flycheck-rust-setup))

(req-package lsp-rust
  :requires lsp-mode
  :hook (rust-mode . lsp-rust-enable)
  :init
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls")))

(req-package cargo
  :hook (rust-mode . cargo-minor-mode)
  :general
  (:keymaps
   'cargo-process-mode-map
   :states '(normal motion insert evil)
   "q" 'quit-window)
  :config
  (set-popup-buffer (rx bos "*Cargo " (one-or-more anything) "*" eos)))

(provide 'lang-rust)
;;; lang-rust.el ends here
