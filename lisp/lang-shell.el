;;; lang-shell.el --- Shell scripting

;;; Commentary:
;; A shell script is a computer program designed to be run by the Unix shell, a
;; command-line interpreter.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-keybinds))

(autoload 'eshell-send-input "eshell")
(autoload 'eir-shell-repl "eval-in-repl-shell" nil t)
(autoload 'eir-eval-in-shell "eval-in-repl-shell")

(defalias 'shell-repl 'eir-shell-repl)

;; Use Emacs compatible pager
(setenv "PAGER" "/usr/bin/cat")

;; Use Emacs compatible shell for comint
(setq shell-file-name "bash")

;;;
;; Built-ins

;; Emacs Shell
(req-package esh-mode
  :loader :built-in
  :hook
  (eshell-mode
   . (lambda ()
       (general-define-key
        :keymaps 'eshell-mode-map
        "C-l" 'eshell-clear-buffer))))

;;;
;; Packages

;; Bash tests
(req-package bats-mode
  :mode "\\.bats$"
  :interpreter "bats")

(req-package fish-mode
  :mode
  "\\.fish$"
  "/fish_funced\\..*$"
  :interpreter "fish"
  :hook
  (fish-mode
   . (lambda ()
       (add-hook 'before-save-hook #'fish_indent-before-save nil t)))
  :init
  (set-repl-command 'fish-mode #'shell-repl)
  (set-eval-command 'fish-mode #'eir-eval-in-shell)
  :config
  (set-doc-fn 'fish-mode #'man))

(req-package sh-script
  :hook
  (sh-mode . flycheck-mode)
  (sh-mode . highlight-numbers-mode)
  :init
  (set-repl-command 'sh-mode #'shell-repl)
  (set-eval-command 'sh-mode #'eir-eval-in-shell)

  (set-popup-buffer (rx bos "*shell*" eos)
                    (rx bos "*shell [" (one-or-more anything) "]*" eos))

  ;; Use regular indentation for line-continuation
  (setq sh-indent-after-continuation 'always)
  :config
  (set-doc-fn 'sh-mode #'man))

;; Completion for keywords, executable files in PATH and ENV variables.
(req-package company-shell
  :require company
  :demand t
  :init
  (setq company-shell-delete-duplicates t)
  :config
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

  (set-company-backends 'sh-mode
                        '(company-keywords
                          company-shell
                          company-shell-env
                          company-files
                          company-dabbrev-code))
  (set-company-backends 'fish-mode
                        '(company-fish-shell
                          company-shell
                          company-files)))

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
