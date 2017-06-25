;;; lang-haskell.el --- Haskell

;;; Commentary:
;; Haskell is a standardized, general-purpose purely functional programming
;; language, with non-strict semantics and strong static typing. It is named
;; after logician Haskell Curry. The latest standard of Haskell is Haskell 2010.
;; As of May 2016, a group is working on the next version, Haskell 2020.

;;; Code:
(require 'base-lib)
(require 'base-keybinds)

;;;
;; Packages

(use-package haskell-mode
  :mode (("\\.hs$"    . haskell-mode)
         ("\\.ghci$"  . ghci-script-mode)
         ("\\.cabal$" . haskell-cabal-mode))
  :interpreter (("runghc"     . haskell-mode)
                ("runhaskell" . haskell-mode))
  :defines haskell-font-lock-symbols
  :preface
  (eval-when-compile
    (defvar aggressive-indent-excluded-modes))
  :config
  (setq
   haskell-notify-p t
   haskell-stylish-on-save t

   ;; Fancy symbols
   haskell-font-lock-symbols t

   haskell-interactive-mode-eval-mode 'haskell-mode
   haskell-interactive-mode-include-file-name nil

   ;; Use interpreter  `stack ghci'
   haskell-process-type 'stack-ghci
   haskell-process-auto-import-loaded-modules t
   haskell-process-show-debug-tips nil
   haskell-process-suggest-haskell-docs-imports t
   haskell-process-suggest-hoogle-imports nil
   haskell-process-suggest-remove-import-lines t
   haskell-process-use-presentation-mode t)

  (add-hook 'haskell-mode-hook #'rainbow-identifiers-mode)

  (with-eval-after-load "aggressive-indent"
    (push 'haskell-mode aggressive-indent-excluded-modes)))

(use-package intero
  :after haskell-mode
  :diminish (intero-mode . " λ")
  :commands intero-mode
  :config
  (setq haskell-process-args-stack-ghci
        '("--ghc-options=-ferror-spans" "--with-ghc=intero" "--install-ghc"))

  (add-hook 'haskell-mode-hook #'intero-mode)

  (with-eval-after-load "company"
    (push-company-backends 'haskell-mode '(company-intero company-files))))

(use-package shm
  :after haskell-mode
  :general
  (:keymaps 'shm-map :states 'normal
            "o" 'shm|evil-open-below
            "O" 'shm|evil-open-above)
  :preface
  (eval-when-compile
    (defvar shm-auto-insert-bangs)
    (defvar shm-auto-insert-skeletons)
    (defvar shm-indent-point-after-adding-where-clause)
    (defvar shm-use-hdevtools)
    (defvar shm-use-presentation-mode))
  :commands (structured-haskell-mode
             structured-haskell-repl-mode
             shm/newline-indent)
  :preface
  (eval-when-compile
    (require 'evil)
    (declare-function evil-insert-state "evil-commands")
    (declare-function evil-maybe-remove-spaces "evil-states"))

  (defun shm|evil-open-above (count)
    "Insert a new line above point and switch to Insert state.
The insertion will be repeated COUNT times."
    (interactive "p")
    (back-to-indentation)
    (save-excursion (shm/newline-indent))
    (setq evil-insert-count count)
    (setq evil-insert-lines t)
    (setq evil-insert-vcount nil)
    (evil-insert-state +1)
    (add-hook 'post-command-hook #'evil-maybe-remove-spaces))

  (defun shm|evil-open-below (count)
    "Insert a new line below point and switch to Insert state.
The insertion will be repeated COUNT times."
    (interactive "p")
    (goto-char (line-end-position))
    (shm/newline-indent)
    (setq evil-insert-count count)
    (setq evil-insert-lines t)
    (setq evil-insert-vcount nil)
    (evil-insert-state +1)
    (add-hook 'post-command-hook #'evil-maybe-remove-spaces))
  :config
  (setq shm-auto-insert-bangs t
        shm-auto-insert-skeletons t
        shm-indent-point-after-adding-where-clause t
        shm-use-hdevtools t
        shm-use-presentation-mode t)

  (add-hook 'structured-haskell-mode-hook
            #'(lambda ()
                (nlinum-mode -1)
                (hl-line-mode -1)

                (haskell-indent-mode -1)
                (haskell-indentation-mode -1)))

  (if (executable-find "structured-haskell-mode")
      (progn
        (add-hook 'haskell-mode-hook #'structured-haskell-mode)
        (add-hook 'haskell-interactive-mode-hook #'structured-haskell-repl-mode))
    (warn "haskell-mode: couldn't find structured-haskell-mode, structured editing won't work")))

;; Smart indentation
(use-package hi2
  :diminish (hi2-mode)
  :general
  (:keymaps 'hi2-mode-map
            "RET" '(nil)
            "TAB" '(hi2-indent-line))
  :commands (turn-on-hi2 hi2-mode)
  :preface
  (eval-when-compile
    (defvar editorconfig-indentation-alist))
  :init (add-hook 'haskell-mode-hook #'turn-on-hi2)
  :config
  (with-eval-after-load 'editorconfig
    (add-to-list 'editorconfig-indentation-alist
                 '(haskell-mode hi2-layout-offset hi2-left-offset hi2-ifte-offset)))

  (add-hook 'editorconfig-custom-hooks
            #'(lambda (props)
                "Use half indentation space for keyword `where'."
                (let ((indent_size (gethash 'indent_size props)))
                  (setq-default hi2-where-pre-offset
                                (/ (string-to-number (if indent_size indent_size "4"))
                                   2))
                  (setq-default hi2-where-post-offset
                                (/ (string-to-number (if indent_size indent_size "4"))
                                   2))))))

(provide 'lang-haskell)
;;; lang-haskell.el ends here
