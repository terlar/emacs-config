;;; lang-data.el --- Data formats

;;; Commentary:
;; Data, data, data.

;;; Code:
(require 'nxml-mode)
(add-to-list 'auto-mode-alist '("\\.plist$" . nxml-mode))

(with-eval-after-load 'company
  (add-hook 'nxml-mode-hook
            #'(lambda ()
                (setq-local company-backends
                            '((company-nxml
                               company-yasnippet))))))

(use-package csv-mode
  :mode "\\.[ct]sv$")

(use-package yaml-mode
  :mode "\\.ya?ml$")

(use-package json-mode
  :mode "\\.js\\(on\\|[hl]int\\(rc\\)?\\)$")

(use-package protobuf-mode
  :mode "\\.proto$")

(use-package thrift
  :mode ("\\.thrift$" . thrift-mode)
  :config
  (add-hook 'thrift-mode-hook
            #'(lambda ()
                (run-hooks 'prog-mode-hook))))

(use-package sql
  :mode ("\\.sql$" . sql-mode)
  :commands (sql-connect sql-set-product))

(use-package dockerfile-mode
  :mode "/Dockerfile$")

(provide 'lang-data)
;;; lang-data.el ends here
