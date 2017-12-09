;;; lang-crystal.el --- Crystal -*- lexical-binding: t; -*-

;;; Commentary:
;; Crystal is a general-purpose, object-oriented programming language, designed
;; and developed by Ary Borenszweig and Juan Wajnerman and more than 200
;; contributors. With syntax inspired by the language Ruby, it is a compiled
;; language with static type-checking, but specifying the types of variables or
;; method arguments is generally unneeded. Types are resolved by an advanced
;; global type inference algorithm.

;;; Code:

;;;
;; Packages

(use-package crystal-mode
  :mode "\\.cr$"
  :interpreter "crystal")

(provide 'lang-crystal)
;;; lang-crystal.el ends here
