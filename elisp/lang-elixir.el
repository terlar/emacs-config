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

(req-package elixir-mode
  :mode "\\.exs?$"
  :config
  (add-hooks-pair 'elixir-mode '(flycheck-mode
                                 turn-off-smartparens-mode)))

(req-package alchemist
  :require elixir-mode
  :after elixir-mode
  :commands alchemist-mode
  :general
  (:keymaps
   'alchemist-help-minor-mode-map
   :states 'normal
   "q" 'quit-window)
  :init
  (autoload 'eir-eval-in-iex "eval-in-repl-iex")
  :config
  (set-eval-command 'elixir-mode #'eir-eval-in-iex)
  (set-repl-command 'elixir-mode #'alchemist-iex-run)
  (set-doc-fn 'elixir-mode #'alchemist-help-search-at-point)
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
                    (rx bos "*Alchemist-IEx*" eos))

  (add-hooks-pair 'elixir-mode 'alchemist-mode))

(req-package flycheck-credo
  :require flycheck elixir-mode
  :after elixir-mode
  :config
  (flycheck-credo-setup))

(req-package flycheck-mix
  :require flycheck elixir-mode
  :after elixir-mode
  :config
  (flycheck-mix-setup))

(provide 'lang-elixir)
;;; lang-elixir.el ends here
