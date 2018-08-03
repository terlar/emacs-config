;;; lang-racket.el --- Racket -*- lexical-binding: t; -*-

;;; Commentary:
;; Racket is a general-purpose programming language as well as the world’s first
;; ecosystem for developing and deploying new languages. Make your dream
;; language, or use one of the dozens already available.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(req-package racket-mode
  :mode "\\.rkt[dl]?\\'"
  :interpreter "racket"
  :commands racket-repl
  :init
  (set-repl-command 'racket-mode #'racket-repl))

(provide 'lang-racket)
;;; lang-racket.el ends here
