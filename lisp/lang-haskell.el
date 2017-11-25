;;; lang-haskell.el --- Haskell -*- lexical-binding: t; -*-

;;; Commentary:
;; Haskell is a standardized, general-purpose purely functional programming
;; language, with non-strict semantics and strong static typing. It is named
;; after logician Haskell Curry. The latest standard of Haskell is Haskell 2010.
;; As of May 2016, a group is working on the next version, Haskell 2020.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-keybinds))

;;;
;; Packages

(req-package haskell-mode
  :mode
  "\\.[gh]s$"
  "\\.hsc$"
  ("\\.l[gh]s$" . literate-haskell-mode)
  :interpreter
  "runghc"
  "runhaskell"
  :init
  (autoload 'haskell-doc-current-info "haskell-doc")
  (autoload 'haskell-hoogle-lookup-from-local "haskell-hoogle")

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
  :config
  (set-aggressive-indent 'haskell-mode :disabled t)

  (add-hooks-pair 'haskell-mode 'rainbow-identifiers-mode))

(req-package intero
  :require haskell-mode
  :after haskell-mode
  :diminish intero-mode
  :config
  (set-eval-command 'haskell-mode #'intero-repl-eval-region)
  (set-repl-command 'haskell-mode #'intero-repl)
  (set-evil-state 'intero-repl-mode 'insert)
  (set-popup-buffer (rx bos "*intero:" (one-or-more anything) "*" eos))

  (set-doc-fn 'haskell-mode #'intero-info)
  (smart-jump-register :modes 'haskell-mode
                       :jump-fn #'intero-goto-definition
                       :pop-fn #'xref-pop-marker-stack
                       :refs-fn #'smart-jump-simple-find-references)

  (set-company-backends 'haskell-mode 'company-intero)

  (set-evil-state 'intero-help-mode 'motion)
  (set-popup-buffer (rx bos "*Intero-Help*" eos))

  (add-hooks-pair 'haskell-mode 'intero-mode))

(req-package shm
  :require haskell-mode
  :after haskell-mode
  :diminish structured-haskell-mode
  :general
  (:keymaps
   'shm-map
   :states 'normal
   "o" 'shm|evil-open-below
   "O" 'shm|evil-open-above)
  :init
  (setq shm-auto-insert-bangs t
        shm-auto-insert-skeletons t
        shm-indent-point-after-adding-where-clause t
        shm-use-hdevtools t
        shm-use-presentation-mode t)

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
  (if (executable-find "structured-haskell-mode")
      (progn
        (add-hooks-pair 'haskell-mode 'structured-haskell-mode)
        (add-hooks-pair 'haskell-interactive-mode 'structured-haskell-repl-mode))
    (warn "haskell-mode: couldn't find structured-haskell-mode, structured editing won't work"))

  (add-hook! 'structured-haskell-mode-hook
             (hl-line-mode -1)
             (haskell-indentation-mode -1)))

;; Smart indentation
(req-package hi2
  :require haskell-mode
  :after haskell-mode
  :diminish hi2-mode
  :general
  (:keymaps
   'hi2-mode-map
   "RET" 'nil
   "TAB" 'hi2-indent-line)
  :config
  (with-eval-after-load "editorconfig"
    (add-to-list 'editorconfig-indentation-alist
                 '(haskell-mode hi2-layout-offset hi2-left-offset hi2-ifte-offset)))

  (add-hook 'editorconfig-custom-hooks
            (lambda (props)
              "Use half indentation space for keyword `where'."
              (let ((indent_size (gethash 'indent_size props)))
                (setq-default hi2-where-pre-offset
                              (/ (string-to-number (if indent_size indent_size "4"))
                                 2))
                (setq-default hi2-where-post-offset
                              (/ (string-to-number (if indent_size indent_size "4"))
                                 2)))))

  (add-hooks-pair 'haskell-mode 'turn-on-hi2))

(provide 'lang-haskell)
;;; lang-haskell.el ends here
