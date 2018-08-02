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
;; Bindings

(general-define-key
 :keymaps 'shell-mode-map
 "C-l" 'comint-clear-buffer)

;;;
;; Built-ins

;; Emacs Shell
(req-package eshell
  :hook
  (eshell-mode . abbrev-mode)
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
        eshell-smart-space-goes-to-end t)

  (define-abbrev-table 'eshell-mode-abbrev-table
    '(("base64" "base64 -w0")
      ("d" "docker")
      ("dim" "docker images")
      ("dp" "docker ps")
      ("dc" "docker-compose")
      ("dcl" "docker-compose logs")
      ("j" "journalctl --since=today")
      ("jb" "journalctl --boot")
      ("je" "journalctl --since=today --priority=0..3")
      ("jf" "journalctl --follow")
      ("ju" "journalctl --unit")
      ("juu" "journalctl --user-unit")
      ("sc" "systemctl")
      ("scl" "systemctl list-units")
      ("scs" "systemctl status")
      ("scu" "systemctl --user")
      ("scul" "systemctl --user list-units")
      ("scus" "systemctl --user status")
      ("time" "time -p")
      ("tree" "tree -a")
      ("week" "date '+%V'"))))

;;;
;; Packages

;; Eshell fringe status indicator
(req-package eshell-fringe-status
  :hook (eshell-mode . eshell-fringe-status-mode))

(req-package esh-autosuggest
  :commands esh-autosuggest
  :init
  (set-company-backends 'eshell-mode
                        '(company-capf esh-autosuggest)))

(req-package fish-completion
  :hook (eshell-mode . fish-completion-mode))

;; Extended shell/comint mode
(req-package shx
  :hook (shell-mode . shx-global-mode))

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
  (fish-mode . +fish-mode-setup)
  :preface
  (defun +fish-mode-setup()
    (add-hook 'before-save-hook #'fish_indent-before-save nil t))
  :init
  (set-repl-command 'fish-mode #'shell-repl)
  (set-eval-command 'fish-mode #'eir-eval-in-shell)
  :config
  (set-doc-fn 'fish-mode #'man))

(req-package sh-script
  :hook
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

;;;
;; Functions

;;;###autoload
(defun eshell-clear-buffer ()
  "Clear eshell terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun run-eshell ()
  "Run Eshell inside project or locally."
  (interactive)
  (if (projectile-project-p)
      (projectile-run-eshell)
    (eshell)))

(provide 'lang-shell)
;;; shell.el ends here
