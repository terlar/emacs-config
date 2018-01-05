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
  :hook (haskell-mode . rainbow-identifiers-mode)
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
  (set-aggressive-indent 'haskell-mode :disabled t))

(req-package intero
  :diminish intero-mode
  :commands intero-repl
  :hook (haskell-mode . intero-mode)
  :general
  (:keymaps
   'intero-mode-map
   "C-c TAB" 'nil)
  :init
  (defalias 'haskell-repl 'intero-repl)

  (set-repl-command 'haskell-mode #'intero-repl)
  (set-eval-command 'haskell-mode #'intero-repl-eval-region)

  (set-evil-state 'intero-repl-mode 'insert)
  (set-popup-buffer (rx bos "*intero:" (one-or-more anything) "*" eos))
  :config
  (set-doc-fn 'haskell-mode #'intero-info)
  (smart-jump-register :modes 'intero-mode
                       :jump-fn #'intero-goto-definition
                       :pop-fn #'xref-pop-marker-stack
                       :refs-fn #'smart-jump-simple-find-references)

  (set-company-backends 'haskell-mode 'company-intero)

  (set-evil-state 'intero-help-mode 'motion)
  (set-popup-buffer (rx bos "*Intero-Help*" eos)))

(req-package shm
  :diminish structured-haskell-mode
  :hook
  (haskell-mode . structured-haskell-mode)
  (haskell-interactive-mode . structured-haskell-repl-mode)
  (structured-haskell-mode . +shm-setup)
  :general
  (:keymaps
   'shm-map
   :states 'normal
   "o" '+shm-evil-open-below
   "O" '+shm-evil-open-above)
  :preface
  (defun +shm-setup ()
    (hl-line-mode -1)
    (haskell-indentation-mode -1))
  :init
  (setq shm-auto-insert-bangs t
        shm-auto-insert-skeletons t
        shm-indent-point-after-adding-where-clause t
        shm-use-hdevtools t
        shm-use-presentation-mode t)

  (defun +shm-evil-open-above (count)
    "Insert a new line above point and switch to Insert state.
The insertion will be repeated COUNT times."
    (interactive "p")
    (back-to-indentation)
    (save-excursion (shm/newline-indent))
    (setq evil-insert-count count)
    (setq evil-insert-lines t)
    (setq evil-insert-vcount nil)
    (evil-insert-state 1)
    (add-hook 'post-command-hook #'evil-maybe-remove-spaces))

  (defun +shm-evil-open-below (count)
    "Insert a new line below point and switch to Insert state.
The insertion will be repeated COUNT times."
    (interactive "p")
    (goto-char (line-end-position))
    (shm/newline-indent)
    (setq evil-insert-count count)
    (setq evil-insert-lines t)
    (setq evil-insert-vcount nil)
    (evil-insert-state 1)
    (add-hook 'post-command-hook #'evil-maybe-remove-spaces))
  :config
  (unless (executable-find "structured-haskell-mode")
    (warn "haskell-mode: couldn't find structured-haskell-mode, structured editing won't work")))

;; Smart indentation
(req-package hi2
  :diminish hi2-mode
  :hook (haskell-mode . turn-on-hi2)
  :general
  (:keymaps
   'hi2-mode-map
   "RET" 'nil
   "TAB" 'hi2-indent-line)
  :config
  (with-eval-after-load 'editorconfig
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
                                 2))))))

(provide 'lang-haskell)
;;; lang-haskell.el ends here
