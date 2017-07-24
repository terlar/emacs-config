;;; lang-go.el --- Golang

;;; Commentary:
;; Go (often referred to as golang) is a free and open source programming
;; language created at Google in 2007 by Robert Griesemer, Rob Pike, and Ken
;; Thompson. It is a compiled, statically typed language in the tradition of
;; Algol and C, with garbage collection, limited structural typing, memory
;; safety features and CSP-style concurrent programming features added.

;;; Code:
(require 'base-lib)
(require 'base-keybinds)

(use-package go-mode
  :mode "\\.go$"
  :interpreter "go"
  :commands (go-mode gofmt-before-save)
  :general
  (:keymaps 'go-mode-map :states 'normal
            "K"  'godoc-at-point
            "gd" 'go-guru-definition
            "gD" 'go-guru-referrers)
  :preface
  (eval-when-compile
    (defvar gofmt-command))

  (defun go|add-before-save-hook ()
    (add-hook 'before-save-hook #'gofmt-before-save nil t))

  (defun go|setup-compile ()
    "Customize compile command to run go build."
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet")))

  (defun go-repl ()
    "Open a Go REPL."
    (interactive)
    (pop-to-buffer
     (or (get-buffer "*Go REPL*")
         (progn (gorepl-run)
                (let ((buf (get-buffer "*Go REPL*")))
                  (bury-buffer buf)
                  buf)))))
  :config
  (push-repl-command 'go-mode #'go-repl)

  ;; Use goimports instead of gofmt
  (setq gofmt-command "goimports")
  (if (not (executable-find "goimports"))
      (warn "go-mode: couldn't find goimports; no code formatting/fixed imports on save")
    (add-hooks-pair 'go-mode 'go|add-before-save-hook))

  (add-hooks-pair 'go-mode
                  '(flycheck-mode
                    go|setup-compile)))

(use-package go-eldoc
  :after go-mode
  :commands go-eldoc-setup
  :config (add-hooks-pair 'go-mode 'go-eldoc-setup))

(use-package go-guru
  :after go-mode
  :commands (go-guru-describe
             go-guru-freevars go-guru-implements go-guru-peers
             go-guru-referrers go-guru-definition go-guru-pointsto
             go-guru-callstack go-guru-whicherrs go-guru-callers go-guru-callees
             go-guru-expand-region)
  :config
  (unless (executable-find "guru")
    (warn "go-mode: couldn't find guru, refactoring commands won't work")))

(use-package gorepl-mode
  :after go-mode
  :commands (gorepl-run gorepl-run-load-current-file)
  :config
  (unless (executable-find "gore")
    (warn "go-mode: couldn't find gore, REPL support disabled")))

(use-package company-go
  :after go-mode
  :defines company-go-show-annotation
  :preface
  (eval-when-compile
    (defvar command-go-gocode-command))
  :init (setq command-go-gocode-command "gocode")
  :config
  (setq company-go-show-annotation t)
  (if (executable-find command-go-gocode-command)
      (with-eval-after-load "company"
        (push-company-backends 'go-mode '(company-go)))
    (warn "go-mode: couldn't find gocode, code completion won't work")))

(provide 'lang-go)
;;; lang-go.el ends here
