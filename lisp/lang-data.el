;;; lang-data.el --- Data formats -*- lexical-binding: t; -*-

;;; Commentary:
;; Data, data, data.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-lib))

;;;
;; Packages

(req-package nxml-mode
  :loader :built-in
  :mode "\\.plist$"
  :hook (nxml-mode . flycheck-mode)
  :init
  (setq nxml-slash-auto-complete-flag t)
  :config
  (set-company-backends 'nxml-mode 'company-nxml)
  (with-eval-after-load "smartparens"
    (sp-local-pair '(nxml-mode) "<" ">" :actions :rem)))

(req-package csv-mode
  :mode "\\.[ct]sv$"
  :hook
  (csv-mode
   . (lambda ()
       (visual-line-mode 0)
       (centered-window-mode 0))))

(req-package json-mode
  :mode "\\.js\\(on\\|[hl]int\\(rc\\)?\\)$")

(req-package yaml-mode
  :mode "\\.ya?ml$"
  :hook (yaml-mode . indent-guide-mode))

(req-package sql
  :mode ("\\.sql$" . sql-mode)
  :commands
  (sql-connect
   sql-set-product)
  :hook
  (sql-interactive-mode
   . (lambda ()
       (toggle-truncate-lines t)))
  :init
  (setq sql-mysql-options '("--protocol=tcp" "--prompt=" "--disable-pager"))
  :config
  (set-evil-state 'sql-interactive-mode 'insert)
  (set-popup-buffer (rx bos "*SQL: *" eos)))

(req-package es-mode
  :mode "\\.es$")

(req-package protobuf-mode
  :mode "\\.proto$")

(req-package thrift
  :mode ("\\.thrift$" . thrift-mode))

(provide 'lang-data)
;;; lang-data.el ends here
