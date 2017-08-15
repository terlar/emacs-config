;;; lang-rest.el --- HTTP REST client

;;; Commentary:
;; This is a tool to manually explore and test HTTP REST webservices. Runs
;; queries from a plain-text query sheet, displays results as a pretty-printed
;; XML, JSON and even images.

;;; Code:
(require 'base-lib)
(require 'base-keybinds)

;;;
;; Packages

(use-package restclient
  :mode ("\\.http$" . restclient-mode)
  :commands restclient-mode
  :general
  (:keymaps 'restclient-mode-map :states 'normal
            "e" 'restclient-http-send-current
            "E" 'restclient-http-send-current-raw
            "c" 'restclient-copy-curl-command))

(use-package company-restclient
  :after restclient
  :config
  (with-eval-after-load "company"
    (push-company-backends 'restclient-mode '(company-restclient))))

(provide 'lang-rest)
;;; lang-rest.el ends here
