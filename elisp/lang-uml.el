;;; lang-uml.el --- UML Schemas -*- lexical-binding: t; -*-

;;; Commentary:
;; The Unified Modeling Language (UML) is a general-purpose, developmental,
;; modeling language in the field of software engineering, that is intended to
;; provide a standard way to visualize the design of a system.

;;; Code:

;;;
;; Packages

(use-package plantuml-mode
  :mode "\\.p\\(lant\\)?uml\\'"
  :init
  (add-hooks-pair 'plantuml-mode 'flycheck-mode)
  :config
  (setq plantuml-java-command "java-headless"
        plantuml-jar-path "/opt/plantuml/plantuml.jar"))

(use-package flycheck-plantuml
  :after plantuml-mode
  :commands flycheck-plantuml-setup
  :config
  (add-hooks-pair 'flycheck-mode 'flycheck-plantuml-setup))

(provide 'lang-uml)
;;; lang-uml.el ends here
