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

(req-package go-mode
  :mode "\\.go$"
  :interpreter "go"
  :init
  (setq gofmt-command "goimports")
  :config
  (set-doc-fn 'go-mode 'godoc-at-point)

  (if (executable-find "goimports")
      (add-hook! 'go-mode (add-hook 'before-save-hook #'gofmt-before-save nil t))
    (warn "go-mode: couldn't find goimports; no code formatting/fixed imports on save"))

  (add-hooks-pair 'go-mode 'flycheck-mode))

(req-package go-eldoc
  :require go-mode
  :after go-mode
  :commands go-eldoc-setup
  :config (add-hooks-pair 'go-mode #'go-eldoc-setup))

;; Code navigation & refactoring
(req-package go-guru
  :require go-mode
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

  (set-evil-state 'go-guru-output-mode 'motion)
  (set-popup-buffer (rx bos "*go-guru-output*" eos)))

;; REPL
(req-package gorepl-mode
  :require go-mode
  :after go-mode
  :commands
  (gorepl-run
   gorepl-run-load-current-file)
  :config
  (defun +gorepl-eval ()
    "Evaluate in Go REPL."
    (if (use-region-p)
        (gorepl-eval-region (region-beginning) (region-end))
      (gorepl-eval-line-goto-next-line)))

  (if (executable-find "gore")
      (progn
        (set-eval-command 'go-mode #'+gorepl-eval)
        (set-repl-command 'go-mode #'gorepl-run))
    (warn "go-mode: couldn't find gore, REPL support disabled"))

  (set-popup-buffer (rx bos "*Go REPL*" eos)))

;; Completion
(req-package company-go
  :require company go-mode
  :after go-mode
  :commands company-go
  :init
  (setq command-go-gocode-command "gocode")
  :config
  (if (executable-find command-go-gocode-command)
      (set-company-backends 'go-mode 'company-go)
    (warn "go-mode: couldn't find gocode, code completion won't work")))

(provide 'lang-go)
;;; lang-go.el ends here
