;;; lang-typescript.el --- TypeScript -*- lexical-binding: t; -*-

;;; Commentary:
;; TypeScript is a free and open-source programming language developed and
;; maintained by Microsoft. It is a strict syntactical superset of JavaScript,
;; and adds optional static typing to the language. Anders Hejlsberg, lead
;; architect of C# and creator of Delphi and Turbo Pascal, has worked on the
;; development of TypeScript. TypeScript may be used to develop JavaScript
;; applications for client-side or server-side (Node.js) execution.

;;; Code:

;;;
;; Packages

(use-package typescript-mode
  :mode "\\.tsx?$"
  :init
  (add-hooks-pair 'typescript-mode
                  '(rainbow-delimiters-mode
                    rainbow-identifiers-mode)))

(provide 'lang-typescript)
;;; lang-typescript.el ends here
