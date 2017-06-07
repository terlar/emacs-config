;;; lang-raml.el --- RESTful API Modeling Language

;;; Commentary:
;; RAML is a YAML-based language for describing RESTful APIs.[2] It provides
;; all the information necessary to describe RESTful or practically RESTful
;; APIs. Although designed with RESTful APIs in mind, RAML is capable of
;; describing APIs that do not obey all constraints of REST (hence the
;; description "practically RESTful"). It encourages reuse, enables discovery
;; and pattern-sharing and aims for merit-based emergence of best practices.

;;; Code:
(use-package raml-mode :ensure nil
  :load-path "vendor/raml-mode/"
  :mode
  ("\\.raml$" . raml-mode)
  :commands raml-mode)

(provide 'lang-raml)
;;; lang-raml.el ends here
