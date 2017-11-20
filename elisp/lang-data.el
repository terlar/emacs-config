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
  :init
  (setq nxml-slash-auto-complete-flag t)
  :config
  (add-hooks-pair 'nxml-mode 'flycheck-mode)
  (set-company-backends 'nxml-mode 'company-nxml)
  (with-eval-after-load "smartparens"
    (sp-local-pair '(nxml-mode) "<" ">" :actions :rem)))

(req-package csv-mode
  :mode "\\.[ct]sv$"
  :config
  (add-hook! 'csv-mode
             (visual-line-mode 0)
             (centered-window-mode 0)))

(req-package json-mode
  :mode "\\.js\\(on\\|[hl]int\\(rc\\)?\\)$")

(req-package yaml-mode
  :mode
  "\\.ya?ml$"
  :config
  (add-hooks-pair 'yaml-mode 'indent-guide-mode))

(req-package sql
  :mode ("\\.sql$" . sql-mode)
  :commands
  (sql-connect
   sql-set-product)
  :init
  (setq sql-mysql-options '("--protocol=tcp" "--prompt=" "--disable-pager"))
  :config
  (set-evil-state 'sql-interactive-mode 'insert)
  (set-popup-buffer (rx bos "*SQL: *" eos))
  (add-hook! 'sql-interactive-mode (toggle-truncate-lines t)))

(req-package protobuf-mode
  :mode "\\.proto$")

(req-package thrift
  :mode ("\\.thrift$" . thrift-mode))

(provide 'lang-data)
;;; lang-data.el ends here
