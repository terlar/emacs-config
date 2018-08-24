;;; lang-data.el --- Data formats -*- lexical-binding: t; -*-

(use-package nxml-mode :ensure nil
  :mode "\\.plist$"
  :hook (nxml-mode . flycheck-mode)
  :init
  (setq nxml-slash-auto-complete-flag t)
  :config
  (set-company-backends 'nxml-mode 'company-nxml)
  (with-eval-after-load 'smartparens
    (sp-local-pair '(nxml-mode) "<" ">" :actions :rem)))



(use-package yaml-mode
  :mode "\\.ya?ml$"
  :hook (yaml-mode . indent-guide-mode))

(use-package flycheck-yamllint
  :requires flycheck
  :init
  :hook
  (flycheck-mode . flycheck-yamllint-setup)
  (yaml-mode . flycheck-mode))

(use-package sql
  :mode ("\\.sql$" . sql-mode)
  :commands
  (sql-connect
   sql-set-product)
  :hook
  (sql-interactive-mode . +sql-interactive-mode-setup)
  :preface
  (defun +sql-interactive-mode-setup ()
    (toggle-truncate-lines t))
  :init
  (setq sql-mysql-options '("--protocol=tcp" "--prompt=" "--disable-pager"))
  :config
  (set-evil-state 'sql-interactive-mode 'insert))

(use-package es-mode
  :mode "\\.es$")

(use-package protobuf-mode
  :mode "\\.proto$")

(use-package thrift
  :mode ("\\.thrift$" . thrift-mode))

(provide 'lang-data)
;;; lang-data.el ends here
