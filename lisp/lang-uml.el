;;; lang-uml.el --- UML Schemas -*- lexical-binding: t; -*-

;;; Commentary:
;; The Unified Modeling Language (UML) is a general-purpose, developmental,
;; modeling language in the field of software engineering, that is intended to
;; provide a standard way to visualize the design of a system.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(use-package plantuml-mode
  :mode "\\.p\\(lant\\)?uml$"
  :init
  (setq plantuml-java-command "java-headless"
        plantuml-jar-path "/opt/plantuml/plantuml.jar"))

(use-package flycheck-plantuml
  :requires flycheck
  :hook
  (plantuml-mode . flycheck-mode)
  (flycheck-mode . flycheck-plantuml-setup))

(provide 'lang-uml)
;;; lang-uml.el ends here
