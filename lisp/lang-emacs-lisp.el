;;; lang-emacs-lisp.el --- Emacs Lisp -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs Lisp is a dialect of the Lisp programming language used as a scripting
;; language by Emacs (a text editor family most commonly associated with GNU
;; Emacs.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-lib)
  (require 'base-package)
  (require 'base-keybinds))

;;;
;; Packages

(req-package elisp-mode :ensure nil
  :mode
  ("recipes/.*$" . emacs-lisp-mode)
  :general
  (:keymaps 'emacs-lisp-mode-map :major-modes t
            :prefix my-local-leader-key
            "c" 'emacs-lisp-byte-compile
            "C" 'emacs-lisp-byte-compile-and-load
            "t" 'elisp-test)
  (:keymaps 'ert-results-mode :states '(normal motion)
            "q" 'quit-window)
  :hook
  (emacs-lisp-mode . flycheck-mode)
  :preface
  (autoload 'eir-eval-in-ielm "eval-in-repl-ielm")
  :init
  (set-repl-command 'emacs-lisp-mode #'elisp-repl)
  (set-eval-command 'emacs-lisp-mode #'eir-eval-in-ielm)
  :config
  (set-doc-fn 'emacs-lisp-mode #'helpful-at-point)

  (with-eval-after-load 'smart-jump
    (smart-jump-register :modes 'emacs-lisp-mode))

  (set-prettify-symbols 'emacs-lisp-mode '(("defun"    . ?ƒ)
                                           ("defmacro" . ?μ)
                                           ("defvar"   . ?ν)))

  (set-evil-state 'checkdoc-output-mode 'motion)
  (set-popup-buffer (rx bos "*ielm*" eos)
                    (rx bos "*Style Warnings*" eos))

  (set-on-evil-state 'emacs-lisp-mode 'insert
                     (nameless-mode -1)
                     (easy-escape-minor-mode -1))
  (set-on-evil-state 'emacs-lisp-mode 'normal
                     (nameless-mode +1)
                     (easy-escape-minor-mode +1)))

(req-package auto-compile
  :commands auto-compile-byte-compile
  :hook
  (emacs-lisp-mode . auto-compile-on-load-mode)
  (emacs-lisp-mode . auto-compile-on-save-mode)
  :init
  (setq auto-compile-display-buffer nil
        auto-compile-use-mode-line nil)
  :config
  (defun +emacs-lisp-load-after-compile (success)
    "Reload the current emacs-lisp file after it's recompiled, if an older
version is loaded."
    (when (eq success t)
      (let ((buffer-path (file-truename buffer-file-name)))
        (when (assoc buffer-path load-history)
          (load-file buffer-path)))))
  (advice-add #'auto-compile-byte-compile :filter-return #'+emacs-lisp-load-after-compile))

(req-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

;; Shorten package prefixes
(req-package nameless
  :diminish nameless-mode
  :hook (emacs-lisp-mode . nameless-mode))

;; Improve readability of escape characters in regular expressions
(req-package easy-escape
  :diminish easy-escape-minor-mode
  :hook (emacs-lisp-mode . easy-escape-minor-mode))

;; Evaluation result overlays.
(req-package eros
  :hook (emacs-lisp-mode . eros-mode))

;; Discover elisp functions
(req-package suggest
  :el-get t :ensure nil
  :preface
  (defun +suggest-popup ()
    "Open suggest as a popup."
    (interactive)
    (open-and-switch-to-buffer #'suggest "*suggest*"))
  :commands
  (suggest suggest-update)
  :init
  (setq suggest-pop-to-buffer t)
  (set-popup-buffer (rx bos "*suggest*" eos)))

;; Emacs Start Up Profiler
(req-package esup
  :commands esup
  :config
  (set-evil-state 'esup-mode 'motion)
  (set-popup-buffer (rx bos "*esup*" eos)))

(req-package package-lint
  :commands package-lint-current-buffer)

;;;
;; Autoloads

;;;###autoload
(defun elisp-repl ()
  "Open the Emacs Lisp REPL (`ielm')."
  (interactive)
  (open-and-switch-to-buffer #'ielm "*ielm*" t))

(defun elisp-test ()
  "Run test file."
  (interactive)
  (when (string-match (buffer-name (current-buffer))
                      "-test.el$")
    (autoload 'ert-delete-all-tests "ert")
    (ert-delete-all-tests))

  (eval-buffer)
  (ert t))

(provide 'lang-emacs-lisp)
;;; lang-emacs-lisp.el ends here
