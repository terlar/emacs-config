;;; lang-rest.el --- HTTP REST client

;;; Commentary:
;; This is a tool to manually explore and test HTTP REST webservices. Runs
;; queries from a plain-text query sheet, displays results as a pretty-printed
;; XML, JSON and even images.

;;; Code:
(use-package restclient
  :mode ("\\.http$" . restclient-mode)
  :commands restclient-mode
  :config
  (add-hook 'restclient-mode-hook
            #'(lambda ()
                (bind-keys :map evil-normal-state-local-map
                           ("e" . restclient-http-send-current)
                           ("E" . restclient-http-send-current-raw)
                           ("c" . restclient-copy-curl-command)))))

(use-package company-restclient
  :when (package-installed-p 'company)
  :after restclient
  :config
  (add-hook 'restclient-mode-hook
            #'(lambda ()
                (setq-local company-backends (append '(company-restclient) company-backends)))))

(provide 'lang-rest)
;;; lang-rest.el ends here
