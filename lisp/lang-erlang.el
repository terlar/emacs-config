;;; lang-erlang.el --- Erlang -*- lexical-binding: t; -*-

;;; Commentary:
;; Erlang is a general-purpose, concurrent, functional programming language, as
;; well as a garbage-collected runtime system.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-lib))

;;;
;; Packages

(req-package erlang
  :mode
  ("\\.[eh]rl$" . erlang-mode)
  ("rebar\\.config" . erlang-mode)
  :init
  (autoload 'eir-eval-in-erlang "eval-in-repl-erlang")

  (defalias 'erlang-repl 'erlang-shell-display)

  (set-repl-command 'erlang-mode #'erlang-repl)
  (set-eval-command 'erlang-mode #'eir-eval-in-erlang)

  (set-popup-buffer (rx bos (zero-or-more anything) "*erlang*" eos))

  (setq erlang-check-module-name t
        erlang-root-dir "/usr/lib/erlang")
  :config
  (set-doc-fn 'erlang-mode 'erlang-man-function)
  (smart-jump-register :modes 'erlang-mode)

  (require 'erlang-start)

  (add-hooks-pair 'erlang-mode 'flycheck-mode))

(req-package company-erlang
  :require company erlang
  :after erlang
  :commands company-erlang
  :config
  (set-company-backends 'erlang-mode 'company-erlang))

(req-package flycheck-rebar3
  :require erlang
  :commands flycheck-rebar3-setup
  :after erlang
  :config
  (flycheck-rebar3-setup))

(provide 'lang-erlang)
;;; lang-erlang.el ends here
