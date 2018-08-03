;;; lang-crystal.el --- Crystal -*- lexical-binding: t; -*-

;;; Commentary:
;; Crystal is a general-purpose, object-oriented programming language, designed
;; and developed by Ary Borenszweig and Juan Wajnerman and more than 200
;; contributors. With syntax inspired by the language Ruby, it is a compiled
;; language with static type-checking, but specifying the types of variables or
;; method arguments is generally unneeded. Types are resolved by an advanced
;; global type inference algorithm.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(req-package crystal-mode
  :mode "\\.cr$"
  :interpreter "crystal")

(req-package inf-crystal
  :commands inf-crystal
  :hook (crystal-mode . inf-crystal-minor-mode)
  :config
  (set-evil-state 'inf-crystal-mode 'insert))

(provide 'lang-crystal)
;;; lang-crystal.el ends here
