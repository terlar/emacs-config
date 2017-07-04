;;; lang-javascript.el --- JavaScript

;;; Commentary:
;; JavaScript, often abbreviated as "JS", is a high-level, dynamic, untyped,
;; object-based, multi-paradigm, and interpreted programming language.

;;; Code:
(require 'base-lib)
(require 'base-keybinds)

;;;
;; Packages

(use-package js2-mode
  :mode
  ("\\.js$"   . js2-mode)
  ("\\.json$" . js2-jsx-mode)
  :interpreter "node"
  :preface
  (autoload 'sp-local-pair "smartparens")
  (autoload 'sp-with-modes "smartparens")

  (eval-when-compile
    (defvar js2-skip-preprocessor-directives)
    (defvar js2-highlight-external-variables)
    (defvar js2-mode-show-parse-errors))
  :config
  (setq js2-skip-preprocessor-directives t
        js2-highlight-external-variables nil
        js2-mode-show-parse-errors nil)

  (add-hooks-pair 'js2-mode
                  '(flycheck-mode
                    rainbow-delimiters-mode))
  (sp-with-modes '(js2-mode rjsx-mode)
    (sp-local-pair "/* " " */" :post-handlers '(("| " "SPC")))))

(use-package xref-js2 :commands xref-js2-xref-backend)

(use-package nodejs-repl :commands nodejs-repl)

(use-package js2-refactor
  :commands
  (js2r-extract-function
   js2r-extract-method js2r-introduce-parameter
   js2r-localize-parameter js2r-expand-object js2r-contract-object
   js2r-expand-function js2r-contract-function js2r-expand-array
   js2r-contract-array js2r-wrap-buffer-in-iife js2r-inject-global-in-iife
   js2r-add-to-globals-annotation js2r-extract-var js2r-inline-var
   js2r-rename-var js2r-var-to-this js2r-arguments-to-object js2r-ternary-to-if
   js2r-split-var-declaration js2r-split-string js2r-unwrap js2r-log-this
   js2r-debug-this js2r-forward-slurp js2r-forward-barf))

(use-package tern
  :commands tern-mode
  :init (add-hooks-pair 'js2-mode 'tern-mode))

(use-package company-tern
  :when (package-installed-p 'company)
  :after tern
  :init
  (with-eval-after-load "company"
    (push-company-backends 'js2-mode '(company-tern company-files))))

(use-package rjsx-mode
  :mode
  ("\\.jsx$"             . rjsx-mode)
  ("components/.+\\.js$" . rjsx-mode)
  :commands rjsx-mode
  :general
  (:keymaps 'rjsx-mode-map
            "<"   '(nil)
            "C-d" '(nil))
  :preface
  (autoload 'sp-point-in-string-or-comment "smartparens")

  (eval-when-compile
    (defvar flycheck-disabled-checkers))
  :init
  ;; Auto-detect JSX file
  (push (cons (lambda ()
                (and buffer-file-name
                     (equal (file-name-extension buffer-file-name) "js")
                     (re-search-forward "\\(^\\s-*import React\\|\\( from \\|require(\\)[\"']react\\)"
                                        magic-mode-regexp-match-limit t)
                     (progn
                       (goto-char (match-beginning 1))
                       (not (sp-point-in-string-or-comment)))))
              'rjsx-mode)
        magic-mode-alist)
  :config
  (add-hook 'rjsx-mode-hook
            #'(lambda ()
                ;; jshint doesn't really know how to deal with jsx
                (push 'javascript-jshint flycheck-disabled-checkers))))

(use-package coffee-mode
  :mode "\\.coffee$"
  :defines coffee-indent-like-python-mode
  :init (setq coffee-indent-like-python-mode t))

(use-package web-beautify
  :commands web-beautify-js
  :general
  (:keymaps '(json-mode js2-mode-map) :states 'normal
            "gQ" '(web-beautify-js)))

(use-package js-comint
  :defines inferior-js-program-command
  :config (setq inferior-js-program-command "node"))

;;;
;; Skewer

(use-package skewer-mode
  :commands (skewer-mode run-skewer)
  :general
  (:keymaps 'skewer-mode-map :states 'normal :prefix ","
            "sE" '(skewer-eval-last-expression)
            "se" '(skewer-eval-defun)
            "sf" '(skewer-load-buffer)))

(use-package skewer-css :ensure nil
  :commands skewer-css-mode
  :general
  (:keymaps 'skewer-css-mode-map :states 'normal :prefix ","
            "se" '(skewer-css-eval-current-declaration)
            "sr" '(skewer-css-eval-current-rule)
            "sb" '(skewer-css-eval-buffer)
            "sc" '(skewer-css-clear-all)))

(use-package skewer-html :ensure nil
  :commands skewer-html-mode
  :general
  (:keymaps 'skewer-html-mode-map :states 'normal :prefix ","
            "se" '(skewer-html-eval-tag)))

(provide 'lang-javascript)
;;; lang-javascript.el ends here
