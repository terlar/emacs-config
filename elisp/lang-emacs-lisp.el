;;; lang-emacs-lisp.el --- Emacs Lisp -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs Lisp is a dialect of the Lisp programming language used as a scripting
;; language by Emacs (a text editor family most commonly associated with GNU
;; Emacs.

;;; Code:

(eval-when-compile
  (require 'base-lib)
  (require 'base-package))

(autoload 'eir-eval-in-ielm "eval-in-repl-ielm")

(add-hook! 'after-init
           (defun emacs-lisp-repl ()
             "Open the Emacs Lisp REPL (`ielm')."
             (interactive)
             (let* ((buffer-name "*ielm*")
                    (buffer-regexp (regexp-quote buffer-name)))
               (eir-repl-start buffer-regexp #'ielm)
               (pop-to-buffer (get-buffer buffer-name))
               (add-to-list 'popup-buffer-list buffer-regexp)))

           (set-eval-command 'emacs-lisp-mode #'eir-eval-in-ielm)
           (set-repl-command 'emacs-lisp-mode #'emacs-lisp-repl)
           (set-doc-fn 'emacs-lisp-mode 'helpful-at-point)
           ;; jump-fn #'xref-find-definitions
           ;; pop-fn #'xref-pop-marker-stack
           ;; refs-fn #'xref-find-references

           (add-hooks-pair 'emacs-lisp-mode
                           '(flycheck-mode
                             rainbow-delimiters-mode)))

;;;
;; Packages

(req-package auto-compile
  :commands
  (auto-compile-on-load-mode
   auto-compile-on-save-mode
   auto-compile-byte-compile)
  :init
  (setq auto-compile-display-buffer nil
        auto-compile-use-mode-line nil)

  (add-hooks-pair 'emacs-lisp-mode
                  '(auto-compile-on-load-mode
                    auto-compile-on-save-mode))
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
  :commands highlight-quoted-mode
  :init
  (add-hooks-pair 'emacs-lisp-mode 'highlight-quoted-mode))

;; Evaluation result overlays.
(req-package eros
  :commands eros-mode
  :init
  (add-hooks-pair 'emacs-lisp-mode 'eros-mode))

;; Emacs Start Up Profiler
(req-package esup
  :commands esup
  :config
  (set-evil-state 'esup-mode 'motion)
  (set-popup-buffer (rx bos "*esup*" eos)))

(provide 'lang-emacs-lisp)
;;; lang-emacs-lisp.el ends here
