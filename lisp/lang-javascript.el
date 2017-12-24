;;; lang-javascript.el --- JavaScript -*- lexical-binding: t; -*-

;;; Commentary:
;; JavaScript, often abbreviated as "JS", is a high-level, dynamic, untyped,
;; object-based, multi-paradigm, and interpreted programming language.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-keybinds))

;;;
;; Packages

(use-package js2-mode
  :mode
  "\\.js$"
  ("\\.json$" . js2-jsx-mode)
  :interpreter
  "node"
  "nodejs"
  :hook
  (js2-mode . flycheck-mode)
  (js2-mode . rainbow-delimiters-mode)
  (js2-mode . +coverlay-mode-enable)
  (js2-mode . +color-identifiers-delayed-refresh)
  :init
  (setq js2-highlight-external-variables nil
        js2-mode-show-parse-errors nil
        js2-skip-preprocessor-directives t
        js2-strict-trailing-comma-warning nil)

  (with-eval-after-load 'editorconfig
    (add-to-list 'editorconfig-indentation-alist
                 '(js2-mode js2-basic-offset js-switch-indent-offset))))

(use-package typescript-mode
  :mode "\\.tsx?$"
  :hook
  (typescript-mode . flycheck-mode)
  (typescript-mode . rainbow-delimiters-mode)
  (typescript-mode . rainbow-identifiers-mode))

(use-package lsp-javascript-typescript
  :requires lsp-mode
  :hook
  (js2-mode . lsp-javascript-typescript-enable)
  (rjsx-mode . lsp-javascript-typescript-enable)
  (typescript-mode . lsp-javascript-typescript-enable)
  :config
  (smart-jump-register :modes '(js2-mode
                                rsjx-mode
                                typescript-mode))
  (with-eval-after-load 'flycheck
    (flycheck-add-next-checker 'lsp 'javascript-eslint 'append)))

(use-package js2-refactor
  :diminish js2-refactor-mode
  :general
  (:keymaps 'js2-mode-map
            "C-k" '(js2r-kill))
  :commands
  (js2r-add-keybindings-with-prefix
   js2r-kill js2r-extract-function js2r-extract-method js2r-introduce-parameter
   js2r-localize-parameter js2r-expand-object js2r-contract-object
   js2r-expand-function js2r-contract-function js2r-expand-array
   js2r-contract-array js2r-wrap-buffer-in-iife js2r-inject-global-in-iife
   js2r-add-to-globals-annotation js2r-extract-var js2r-inline-var
   js2r-rename-var js2r-var-to-this js2r-arguments-to-object js2r-ternary-to-if
   js2r-split-var-declaration js2r-split-string js2r-unwrap js2r-log-this
   js2r-debug-this js2r-forward-slurp js2r-forward-barf)
  :hook
  (js2-mode . js2-refactor-mode)
  (js2-mode . +js2r-setup-keybindings)
  :preface
  (defun +js2r-setup-keybindings ()
    (js2r-add-keybindings-with-prefix "C-c c R")))

(use-package nodejs-repl
  :commands
  (nodejs-repl
   nodejs-repl-send-region
   nodejs-repl-send-line
   nodejs-repl-load-file)
  :init
  (defun javascript-repl ()
    "Open a JavaScript REPL."
    (interactive)
    (open-and-switch-to-buffer #'nodejs-repl "*nodejs*" t))

  (defun javascript-repl-eval ()
    "Evaluate code in JavaScript REPL"
    (if (use-region-p)
        (nodejs-repl-send-region (region-beginning) (region-end))
      (nodejs-repl-send-line)))

  (set-repl-command 'js2-mode #'javascript-repl)
  (set-eval-command 'js2-mode #'javascript-repl-eval)

  (set-popup-buffer (rx bos "*nodejs*" eos))
  (set-evil-state 'nodejs-repl-mode 'insert))

(use-package indium)

(use-package rjsx-mode
  :mode
  ("\\.jsx$"             . rjsx-mode)
  ("components/.+\\.js$" . rjsx-mode)
  :commands rjsx-mode
  :general
  (:keymaps 'rjsx-mode-map
            "<"   'nil
            "C-d" 'nil)
  :preface
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
        magic-mode-alist))

(use-package coffee-mode
  :mode "\\.coffee$"
  :defines coffee-indent-like-python-mode
  :init (setq coffee-indent-like-python-mode t))

(use-package web-beautify
  :commands
  (web-beautify-js web-beautify-html web-beautify-css)
  :general
  (:keymaps '(js2-mode-map json-mode-map) :states 'normal
            "gQ" '(web-beautify-js))
  (:keymaps 'html-mode-map :states 'normal
            "gQ" '(web-beautify-html))
  (:keymaps 'css-mode-map :states 'normal
            "gQ" '(web-beautify-css)))

;;;
;; Skewer

(use-package skewer-mode
  :commands (skewer-mode run-skewer)
  :general
  (:keymaps 'skewer-mode-map :states 'normal :prefix ","
            "sE" '(skewer-eval-last-expression)
            "se" '(skewer-eval-defun)
            "sf" '(skewer-load-buffer)))

(use-package skewer-css
  :commands skewer-css-mode
  :general
  (:keymaps 'skewer-css-mode-map :states 'normal :prefix ","
            "se" '(skewer-css-eval-current-declaration)
            "sr" '(skewer-css-eval-current-rule)
            "sb" '(skewer-css-eval-buffer)
            "sc" '(skewer-css-clear-all)))

(use-package skewer-html
  :commands skewer-html-mode
  :general
  (:keymaps 'skewer-html-mode-map :states 'normal :prefix ","
            "se" '(skewer-html-eval-tag)))

(provide 'lang-javascript)
;;; lang-javascript.el ends here
