;;; lang-elixir.el --- Elixir -*- lexical-binding: t; -*-

;;; Commentary:
;; Elixir is a functional, concurrent, general-purpose programming language that
;; runs on the Erlang virtual machine (BEAM).

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-keybinds))

;;;
;; Packages

(use-package elixir-mode
  :mode "\\.exs?$"
  :hook
  (elixir-mode . flycheck-mode)
  (elixir-mode . turn-off-smartparens-mode))

(use-package alchemist
  :commands alchemist-iex-run
  :general
  (:keymaps
   'alchemist-help-minor-mode-map
   :states 'normal
   "q" 'quit-window)
  :hook (elixir-mode . alchemist-mode)
  :init
  (autoload 'eir-eval-in-iex "eval-in-repl-iex")

  (defalias 'elixir-repl 'alchemist-iex-run)

  (set-repl-command 'elixir-mode #'alchemist-iex-run)
  (set-eval-command 'elixir-mode #'eir-eval-in-iex)
  :config
  (set-doc-fn 'alchemist-mode #'alchemist-help-search-at-point)
  (smart-jump-register :modes 'alchemist-mode
                       :jump-fn #'alchemist-goto-definition-at-point
                       :pop-fn #'alchemist-goto-jump-back
                       :refs-fn #'smart-jump-simple-find-references
                       :should-jump t
                       :heuristic 'point
                       :async 500)

  (set-evil-state '(alchemist-execute-mode
                    alchemist-mix-mode
                    alchemist-test-report-mode)
                  'motion)
  (set-evil-state 'alchemist-iex-mode 'insert)
  (set-popup-buffer (rx bos "*alchemist " (one-or-more anything) "*" eos)
                    (rx bos "*alchemist-refcard*" eos)
                    (rx bos "*Alchemist-IEx*" eos)))

(use-package flycheck-credo
  :requires flycheck
  :hook (flycheck-mode . flycheck-credo-setup))

(use-package flycheck-mix
  :requires flycheck
  :hook (flycheck-mode . flycheck-mix-setup))

(provide 'lang-elixir)
;;; lang-elixir.el ends here
