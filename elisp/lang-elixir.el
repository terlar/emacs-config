;;; lang-elixir.el --- Elixir

;;; Commentary:
;; Elixir is a functional, concurrent, general-purpose programming language that
;; runs on the Erlang virtual machine (BEAM).

;;; Code:
(use-package elixir-mode :mode ("\\.ex$")
  :config
  (add-hooks-pair 'elixir-mode 'turn-off-smartparens-mode))

(use-package alchemist
  :after elixir-mode
  :commands alchemist-mode
  :config
  (add-hooks-pair 'elixir-mode 'alchemist-mode))

(use-package flycheck-credo
  :after elixir-mode
  :commands flycheck-credo-setup)

(use-package flycheck-mix
  :after elixir-mode
  :commands flycheck-mix-setup)

(provide 'lang-elixir)
;;; lang-elixir.el ends here
