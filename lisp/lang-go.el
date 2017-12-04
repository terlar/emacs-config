;;; lang-go.el --- Golang -*- lexical-binding: t; -*-

;;; Commentary:
;; Go (often referred to as golang) is a free and open source programming
;; language created at Google in 2007 by Robert Griesemer, Rob Pike, and Ken
;; Thompson. It is a compiled, statically typed language in the tradition of
;; Algol and C, with garbage collection, limited structural typing, memory
;; safety features and CSP-style concurrent programming features added.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-lib))

;;;
;; Packages

(use-package go-mode
  :mode "\\.go$"
  :interpreter "go"
  :hook
  (go-mode . flycheck-mode)
  (go-mode
   . (lambda ()
       (add-hook 'before-save-hook #'gofmt-before-save nil t)))
  :init
  (setq gofmt-command "goimports")
  :config
  (set-doc-fn 'go-mode 'godoc-at-point)

  (set-prettify-symbols 'go-mode
                        '(("func" . ?ƒ)
                          (":="   . ?←)))

  (unless (executable-find "goimports")
    (warn "go-mode: couldn't find goimports; no code formatting/fixed imports on save")))

(use-package go-eldoc
  :hook (go-mode . go-eldoc-setup))

;; Code navigation & refactoring
(use-package go-guru
  :after go-mode
  :commands
  (go-guru-describe
   go-guru-freevars go-guru-implements go-guru-peers
   go-guru-referrers go-guru-definition go-guru-pointsto
   go-guru-callstack go-guru-whicherrs go-guru-callers go-guru-callees
   go-guru-expand-region)
  :config
  (if (executable-find "guru")
      (smart-jump-register :modes 'go-mode
                           :jump-fn #'go-guru-definition
                           :pop-fn #'xref-pop-marker-stack
                           :refs-fn #'go-guru-referrers)
    (warn "go-mode: couldn't find guru; refactoring commands won't work"))

  (set-popup-buffer (rx bos "*go-guru-output*" eos))
  (set-evil-state 'go-guru-output-mode 'motion))

;; REPL
(use-package gorepl-mode
  :after go-mode
  :commands
  (gorepl-run
   gorepl-run-load-current-file)
  :init
  (defun go-repl ()
    "Open a Go REPL."
    (interactive)
    (open-and-switch-to-buffer #'gorepl-run "*Go REPL*" t))

  (defun go-repl-eval ()
    "Evaluate code in Go REPL."
    (if (use-region-p)
        (gorepl-eval-region (region-beginning) (region-end))
      (gorepl-eval-line-goto-next-line)))

  (set-repl-command 'go-mode #'go-repl)
  (set-eval-command 'go-mode #'go-repl-eval)

  (set-popup-buffer (rx bos "*Go REPL*" eos))
  :config
  (unless (executable-find "gore")
    (warn "go-mode: couldn't find gore, REPL support disabled")))

;; Completion
(use-package company-go
  :requires company
  :after go-mode
  :init
  (setq command-go-gocode-command "gocode")
  :config
  (if (executable-find command-go-gocode-command)
      (set-company-backends 'go-mode 'company-go)
    (warn "go-mode: couldn't find gocode, code completion won't work")))

(provide 'lang-go)
;;; lang-go.el ends here
