;;; lang-uml.el --- UML Schemas

;;; Commentary:
;; The Unified Modeling Language (UML) is a general-purpose, developmental,
;; modeling language in the field of software engineering, that is intended to
;; provide a standard way to visualize the design of a system.

;;; Code:
(use-package plantuml-mode
  :mode "\\.p\\(lant\\)?uml\\'"
  :init
  (add-hook 'plantuml-mode-hook #'flycheck-mode)
  :config
  (setq plantuml-java-command "java-headless"
        plantuml-jar-path "/opt/plantuml/plantuml.jar"))

(use-package flycheck-plantuml
  :after plantuml-mode
  :commands flycheck-plantuml-setup
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-plantuml-setup))

(provide 'lang-uml)
;;; lang-uml.el ends here
