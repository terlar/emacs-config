;;; lang-shell.el --- Shell scripting

;;; Commentary:
;; A shell script is a computer program designed to be run by the Unix shell, a
;; command-line interpreter.

;;; Code:
(use-package bats-mode :commands bats-mode)

(use-package fish-mode
  :mode (("\\.fish$"           . fish-mode)
         ("/fish_funced\\..*$" . fish-mode))
  :commands fish-mode
  :preface
  :init
  (add-hook 'fish-mode-hook
            #'(lambda ()
                (add-hook 'before-save-hook #'fish_indent-before-save))))

(use-package sh-script
  :mode
  (("\\.zsh\\'"  . sh-mode)
   ("\\.bash\\'" . sh-mode))
  :init
  (add-hook 'sh-mode-hook
            #'(lambda ()
                (flycheck-mode +1)
                (highlight-numbers-mode +1)))
  :config
  ;; Use regular indentation for line-continuation
  (setq sh-indent-after-continuation 'always))

;; Completion for keywords, executable files in PATH and ENV variables.
(use-package company-shell
  :when (package-installed-p 'company)
  :after sh-script
  :init
  (add-hook 'sh-mode-hook
            #'(lambda ()
                (require 'company)
                (setq-local company-backends
                            (append
                             '((company-keywords
                                company-shell
                                company-shell-env
                                company-files
                                company-dabbrev-code)) company-backends))))
  (add-hook 'fish-mode-hook
            #'(lambda ()
                (require 'company)
                (setq-local company-backends
                            (append
                             '((company-fish-shell
                                company-shell
                                company-files)) company-backends))))
  :config
  (setq company-shell-delete-duplicates t)

  (with-eval-after-load 'company
    (push '(sh-mode "alias" "bg" "bind" "builtin" "caller" "case" "in" "esac"
                    "command" "compgen" "complete" "continue" "declare" "dirs"
                    "disown" "do" "done" "echo" "enable" "eval" "exec" "exit"
                    "export" "false" "fc" "fg" "for" "function" "getopts" "hash"
                    "help" "history" "if" "elif" "else" "fi" "jobs" "kill" "let"
                    "local" "logout" "popd" "printf" "pushd" "pwd" "read"
                    "readonly" "return" "select" "set" "shift" "shopt" "source"
                    "suspend" "test" "then" "time" "times" "trap" "true" "type"
                    "typeset" "ulimit" "umask" "unalias" "unset" "until"
                    "variables" "while") company-keywords-alist)))

(provide 'lang-shell)
;;; lang-shell.el ends here
