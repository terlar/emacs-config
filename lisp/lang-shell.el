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

;; Make less work
(setenv "LESS" "--dumb --prompt=s")

;; Use Emacs compatible shell for comint
(setq shell-file-name "bash")

;;;
;; Built-ins

;; Emacs Shell
(use-package eshell
  :hook
  (eshell-mode . eshell-smart-initialize)
  (eshell-mode . +eshell-define-keys)
  (eshell-mode . +eshell-setup)
  :general
  (:keymaps 'company-mode-map
            :states 'insert
            "TAB" (general-predicate-dispatch nil
                    (eq major-mode 'eshell-mode) 'company-complete-common)
            [tab] (general-predicate-dispatch nil
                    (eq major-mode 'eshell-mode) 'company-complete-common))
  :preface
  (defun +eshell-define-keys ()
    (general-define-key
     :keymaps 'eshell-mode-map
     :states 'insert
     [remap eshell-pcomplete] 'completion-at-point
     "C-r" 'eshell-list-history
     "C-l" 'eshell-clear-buffer))
  (defun +eshell-setup ()
    (setq eshell-visual-commands
          (append '("fish" "most" "ssh" "tail" "watch") eshell-visual-commands)))
  :init
  (autoload 'eshell-smart-initialize "em-smart")

  (defun eshell/gs (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo))

  (setq eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
        eshell-buffer-maximum-lines 20000
        eshell-history-size 1000
        eshell-hist-ignoredups t
        eshell-error-if-no-glob t
        eshell-prefer-lisp-functions nil
        eshell-save-history-on-exit t
        eshell-destroy-buffer-when-process-dies t
        eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t))

;;;
;; Packages

;; Eshell fringe status indicator
(use-package eshell-fringe-status
  :hook (eshell-mode . eshell-fringe-status-mode))

(use-package company-eshell-autosuggest
  :commands company-eshell-autosuggest
  :init
  (set-company-backends 'eshell-mode
                        '(company-capf company-eshell-autosuggest)))

(use-package fish-completion
  :hook (eshell-mode . fish-completion-mode))

;; Extended shell/comint mode
(use-package shx
  :hook (shell-mode . shx-global-mode))

;; Bash tests
(use-package bats-mode
  :mode "\\.bats$"
  :interpreter "bats")

(use-package fish-mode
  :mode
  "\\.fish$"
  "/fish_funced\\..*$"
  :interpreter "fish"
  :hook
  (fish-mode . +fish-mode-setup)
  :preface
  (defun +fish-mode-setup()
    (add-hook 'before-save-hook #'fish_indent-before-save nil t))
  :init
  (set-repl-command 'fish-mode #'shell-repl)
  (set-eval-command 'fish-mode #'eir-eval-in-shell)
  :config
  (set-doc-fn 'fish-mode #'man))

(use-package sh-script
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
                    "variables" "while") company-keywords-alist))

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
