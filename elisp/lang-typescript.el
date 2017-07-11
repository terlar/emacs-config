;;; lang-typescript.el --- TypeScript

;;; Commentary:
;; TypeScript is a free and open-source programming language developed and
;; maintained by Microsoft. It is a strict syntactical superset of JavaScript,
;; and adds optional static typing to the language. Anders Hejlsberg, lead
;; architect of C# and creator of Delphi and Turbo Pascal, has worked on the
;; development of TypeScript. TypeScript may be used to develop JavaScript
;; applications for client-side or server-side (Node.js) execution.

;;; Code:
(require 'base-lib)
(require 'base-keybinds)

(use-package typescript-mode
  :mode "\\.tsx?$"
  :init
  (add-hooks-pair 'typescript-mode
                  '(eldoc-mode
                    flycheck-mode
                    rainbow-delimiters-mode
                    rainbow-identifiers-mode)))

(use-package tide
  :after typescript-mode
  :general
  (:keymaps 'typescript-mode-map :states 'normal
            "K"  'tide-documentation-at-point
            "gd" 'tide-jump-to-definition
            "gD" 'tide-references)
  :init
  (add-hooks-pair 'typescript-mode 'tide-setup)
  :config
  (with-eval-after-load "company"
    (push-company-backends 'typescript-mode '(company-tide))))

(provide 'lang-typescript)
;;; lang-typescript.el ends here
