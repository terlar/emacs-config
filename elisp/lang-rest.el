;;; lang-rest.el --- HTTP REST client

;;; Commentary:
;; This is a tool to manually explore and test HTTP REST webservices. Runs
;; queries from a plain-text query sheet, displays results as a pretty-printed
;; XML, JSON and even images.

;;; Code:
(require 'base-lib)

(use-package restclient
  :mode ("\\.http$" . restclient-mode)
  :commands restclient-mode
  :preface
  (eval-when-compile
    (defvar evil-normal-state-local-map))
  :config
  (with-eval-after-load "evil"
    (add-hook 'restclient-mode-hook
              #'(lambda ()
                  (bind-keys :map evil-normal-state-local-map
                             ("e" . restclient-http-send-current)
                             ("E" . restclient-http-send-current-raw)
                             ("c" . restclient-copy-curl-command))))))

(use-package company-restclient
  :after restclient
  :config
  (with-eval-after-load "company"
    (push-company-backends 'restclient-mode '(company-restclient))))

(provide 'lang-rest)
;;; lang-rest.el ends here
