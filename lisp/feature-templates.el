;;; feature-templates.el --- Templates -*- lexical-binding: t; -*-

;;; Commentary:
;; Quick start.

;;; Code:

;;;
;; Built-ins

(req-package autoinsert
  :defer 1
  :init
  (setq auto-insert-query nil
        auto-insert-alist nil)
  :config
  (auto-insert-mode 1))

;;;
;; Packages

(req-package yatemplate
  :defer 1
  :hook (yas-global-mode . yatemplate-fill-alist))

(provide 'feature-templates)
;;; feature-templates.el ends here
