;;; lang-erlang.el --- Erlang

;;; Commentary:
;; Erlang is a general-purpose, concurrent, functional programming language, as
;; well as a garbage-collected runtime system.

;;; Code:
(require 'base-lib)

(use-package erlang
  :init
  (add-hook 'erlang-mode-hook #'flycheck-mode)
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
  (add-hook 'erlang-mode-hook #'flycheck-rebar3-setup))

(provide 'lang-erlang)
;;; lang-erlang.el ends here
