;;; lang-data.el --- Data formats -*- lexical-binding: t; -*-

;;; Commentary:
;; Data, data, data.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-lib))

;;;
;; Packages

(use-package nxml-mode
  :mode "\\.plist$"
  :hook (nxml-mode . flycheck-mode)
  :init
  (setq nxml-slash-auto-complete-flag t)
  :config
  (set-company-backends 'nxml-mode 'company-nxml)
  (with-eval-after-load "smartparens"
    (sp-local-pair '(nxml-mode) "<" ">" :actions :rem)))

(use-package csv-mode
  :mode "\\.[ct]sv$"
  :hook
  (csv-mode . +csv-mode-setup)
  :preface
  (defun +csv-mode-setup ()
    (visual-line-mode 0)
    (centered-window-mode 0)))

(use-package json-mode
  :mode "\\.js\\(on\\|[hl]int\\(rc\\)?\\)$")

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
  (set-evil-state 'sql-interactive-mode 'insert)
  (set-popup-buffer (rx bos "*SQL: *" eos)))

(use-package es-mode
  :mode "\\.es$")

(use-package protobuf-mode
  :mode "\\.proto$")

(use-package thrift
  :mode ("\\.thrift$" . thrift-mode))

(provide 'lang-data)
;;; lang-data.el ends here
