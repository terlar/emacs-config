;;; lang-ocaml.el --- OCaml -*- lexical-binding: t; -*-

;;; Commentary:
;; OCaml is a general purpose programming language with an emphasis on
;; expressiveness and safety. Developed for more than 20 years at Inria by a
;; group of leading researchers, it has an advanced type system that helps catch
;; your mistakes without getting in your way. It's used in environments where a
;; single mistake can cost millions and speed matters, is supported by an active
;; community, and has a rich set of libraries and development tools. For all its
;; power, OCaml is also pretty simple, which is one reason it's often used as a
;; teaching language.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(req-package tuareg
  :mode ("\\.ml[ilpy]?\\'" . tuareg-mode)
  :config
  (set-repl-command 'tuareg-mode #'run-ocaml)
  (set-eval-command 'tuareg-mode #'tuareg-eval-phrase)
  (set-popup-buffer (rx bos "*OCaml*" eos)))

(provide 'lang-ocaml)
;;; lang-ocaml.el ends here
