;;; lang-shell.el --- Shell scripting
(defalias 'shell-repl 'eir-shell-repl)

;; Extended shell/comint mode
(use-package shx
  :hook (shell-mode . shx-global-mode))

;; Bash tests
(use-package bats-mode
  :mode "\\.bats$"
  :interpreter "bats")

(use-package sh-script
  :hook
  (sh-mode . highlight-numbers-mode)
  :init
  (set-repl-command 'sh-mode #'shell-repl)
  (set-eval-command 'sh-mode #'eir-eval-in-shell)

  ;; Use regular indentation for line-continuation
  (setq sh-indent-after-continuation 'always)
  :config
  (set-doc-fn 'sh-mode #'man))

;; Completion for keywords, executable files in PATH and ENV variables.
(use-package company-shell
  :requires company
  :after company
  :commands
  (company-shell
   company-shell-env
   company-fish-shell)
  :init
  (setq company-shell-delete-duplicates t)
  :config
  (set-company-backends 'sh-mode
                        '(company-shell
                          company-shell-env
                          company-files
                          company-dabbrev-code))
  (set-company-backends 'fish-mode
                        '(company-fish-shell
                          company-shell
                          company-files)))

(provide 'lang-shell)
;;; shell.el ends here
