;;; lang-data.el --- Data formats

;;; Commentary:
;; Data, data, data.

;;; Code:
(require 'base-lib)

(with-eval-after-load "company"
  (push-company-backends 'nxml-mode '(company-nxml)))

(use-package csv-mode :mode "\\.[ct]sv$")
(use-package toml-mode :mode "\\.toml$")

(use-package yaml-mode
  :mode (("\\.ya?ml$" . yaml-mode)
         ("\\.sls$"   . yaml-mode))
  :config
  (add-hook 'yaml-mode-hook #'indent-guide-mode))

(use-package json-mode
  :mode "\\.js\\(on\\|[hl]int\\(rc\\)?\\)$")

(use-package protobuf-mode :mode "\\.proto$")

(use-package thrift :mode ("\\.thrift$" . thrift-mode)
  :config
  (add-hook 'thrift-mode-hook
            #'(lambda ()
                (run-hooks 'prog-mode-hook))))

(use-package sql :mode ("\\.sql$" . sql-mode)
  :commands (sql-connect sql-set-product))

(use-package dockerfile-mode :mode "/Dockerfile$")

(provide 'lang-data)
;;; lang-data.el ends here
