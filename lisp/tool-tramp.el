;;; tool-tramp.el --- Transparent Remote Access, Multiple Protocols -*- lexical-binding: t; -*-

;;; Commentary:
;; A long way from home.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

;;;
;; Packages

(use-package tramp
  :init
  (setq tramp-default-method "ssh"))

(provide 'tool-tramp)
;;; tool-tramp.el ends here
