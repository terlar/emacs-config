;;; lang-shell.el --- Shell scripting -*- lexical-binding: t; -*-

;;; Commentary:
;; A shell script is a computer program designed to be run by the Unix shell, a
;; command-line interpreter.

;;; Code:

(autoload 'push-company-backends "base-lib")
(autoload 'push-repl-command "base-lib")
(autoload 'eshell-send-input "eshell")

(add-hooks-pair 'eshell-mode '(lambda ()
                                (setq-local scroll-margin 0)))

;;;
;; Packages

(use-package bats-mode
  :commands bats-mode
  :interpreter "bats")

(use-package fish-mode
  :commands fish-mode
  :preface
  (defun fish|add-before-save-hook ()
    (add-hook 'before-save-hook #'fish_indent-before-save nil t))
  :init
  (add-hooks-pair 'fish-mode 'fish|add-before-save-hook))

(use-package sh-script
  :commands sh-mode
  :preface
  (eval-when-compile
    (declare-function sh-shell-process "sh-script")

    (defvar sh-shell-file))

  (defun sh-repl ()
    "Open a shell REPL."
    (interactive)
    (let* ((dest-sh (symbol-name sh-shell))
           (buffer-name (format "*shell [%s]*" dest-sh))
           (sh-shell-file dest-sh))

      (pop-to-buffer
       (or (get-buffer buffer-name)
           (progn (sh-shell-process t)
                  (let ((buf (get-buffer "*shell*")))
                    (bury-buffer buf)
                    (with-current-buffer buf
                      (rename-buffer buffer-name))
                    buf))))))
  :init
  (add-hooks-pair 'sh-mode
                  '(flycheck-mode
                    highlight-numbers-mode))
  :config
  (push-repl-command 'sh-mode #'sh-repl)

  ;; Use regular indentation for line-continuation
  (setq sh-indent-after-continuation 'always))

;; Completion for keywords, executable files in PATH and ENV variables.
(use-package company-shell
  :after company
  :preface
  (require 'company-keywords)
  :config
  (setq company-shell-delete-duplicates t)

  (push '(sh-mode "alias" "bg" "bind" "builtin" "caller" "case" "in" "esac"
                  "command" "compgen" "complete" "continue" "declare" "dirs"
                  "disown" "do" "done" "echo" "enable" "eval" "exec" "exit"
                  "export" "false" "fc" "fg" "for" "function" "getopts" "hash"
                  "help" "history" "if" "elif" "else" "fi" "jobs" "kill" "let"
                  "local" "logout" "popd" "printf" "pushd" "pwd" "read"
                  "readonly" "return" "select" "set" "shift" "shopt" "source"
                  "suspend" "test" "then" "time" "times" "trap" "true" "type"
                  "typeset" "ulimit" "umask" "unalias" "unset" "until"
                  "variables" "while") company-keywords-alist)

  (with-eval-after-load "company"
    (push-company-backends 'sh-mode '(company-keywords
                                      company-shell
                                      company-shell-env
                                      company-files
                                      company-dabbrev-code))
    (push-company-backends 'fish-mode '(company-fish-shell
                                        company-shell
                                        company-files))))

;;;
;; Functions

;;;###autoload
(defun eshell-clear-buffer ()
  "Clear eshell terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(provide 'lang-shell)
;;; lang-shell.el ends here
