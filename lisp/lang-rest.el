;;; lang-rest.el --- HTTP REST client -*- lexical-binding: t; -*-

;;; Commentary:
;; This is a tool to manually explore and test HTTP REST webservices. Runs
;; queries from a plain-text query sheet, displays results as a pretty-printed
;; XML, JSON and even images.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-keybinds))

;;;
;; Packages

(use-package restclient
  :mode
  ("\\.http$" . restclient-mode)
  :interpreter
  ("restclient" . restclient-mode)
  :general
  (:keymaps
   'restclient-mode-map
   :states 'normal
   "e" 'restclient-http-send-current
   "E" 'restclient-http-send-current-raw
   "c" 'restclient-copy-curl-command)
  :config
  (rx bos "*HTTP Response*" eos))

(use-package company-restclient
  :requires company
  :commands company-restclient
  :init
  (set-company-backends 'restclient-mode 'company-restclient))

(provide 'lang-rest)
;;; lang-rest.el ends here
