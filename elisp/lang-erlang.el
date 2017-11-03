;;; lang-erlang.el --- Erlang -*- lexical-binding: t; -*-

;;; Commentary:
;; Erlang is a general-purpose, concurrent, functional programming language, as
;; well as a garbage-collected runtime system.

;;; Code:

(autoload 'push-company-backends "base-lib")

;;;
;; Packages

(use-package erlang
  :init
  (add-hooks-pair 'erlang-mode 'flycheck-mode)
  :config
  (require 'erlang-start)
  (setq erlang-check-module-name t))

(use-package company-erlang
  :after erlang
  :init
  (push-company-backends 'erlang-mode '(company-erlang)))

(use-package flycheck-rebar3
  :after erlang
  :init
  (add-hooks-pair 'erlang-mode 'flycheck-rebar3-setup))

(provide 'lang-erlang)
;;; lang-erlang.el ends here
