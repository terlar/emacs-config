;;; lang-java.el --- Java

;;; Commentary:
;; Java is a general-purpose computer programming language that is concurrent,
;; class-based, object-oriented, and specifically designed to have as few
;; implementation dependencies as possible. It is intended to let application
;; developers "write once, run anywhere" (WORA), meaning that compiled Java code
;; can run on all platforms that support Java without the need for
;; recompilation.

;;; Code:
(require 'base-vars)

(use-package meghanada
  :commands meghanada-mode
  :init
  (setq meghanada-server-install-dir (concat my-data-dir "meghanada-server/")
        meghanada-auto-start t
        meghanada-use-company t
        meghanada-use-flycheck t)

  (add-hooks-pair 'java-mode
                  '(flycheck-mode
                    meghanada-mode)))


(use-package android-mode :commands android-mode)

(use-package groovy-mode :mode "\\.g\\(radle\\|roovy\\)$")

(use-package gradle-mode)
(use-package log4j-mode)

(provide 'lang-java)
;;; lang-java.el ends here
