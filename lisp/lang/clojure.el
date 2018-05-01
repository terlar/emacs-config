;;; lang-clojure.el --- Clojure -*- lexical-binding: t; -*-

;;; Commentary:
;; Clojure is a dynamic, general-purpose programming language, combining the
;; approachability and interactive development of a scripting language with an
;; efficient and robust infrastructure for multithreaded programming.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-keybinds))

;;;
;; Functions
(defun clojure-repl ()
  "Open a Clojure REPL."
  (interactive)
  (condition-case nil
      (call-interactively 'cider-switch-to-repl-buffer)
    (user-error
     (call-interactively 'cider-jack-in))))

;;;
;; Packages

(req-package clojure-mode
  :mode "\\.clj$"
  :general
  (:keymaps 'clojure-mode-map :major-modes t
            "C-c SPC" 'nil))

(req-package cider
  :commands
  (cider-jack-in
   cider-switch-to-repl-buffer
   cider-test-run-project-tests
   cider-test-run-ns-tests
   cider-test-run-test)
  :general
  (:keymaps 'cider-mode-map
            "C-c RET" 'nil)
  :hook
  (cider-mode . cider-auto-test-mode)
  :init
  (setq cider-repl-display-help-banner nil)

  (set-repl-command 'clojure-mode #'clojure-repl)
  (set-eval-command 'clojure-mode #'cider-eval-last-sexp)

  (set-test-fns 'clojure-mode
                :all #'cider-test-run-project-tests
                :file #'cider-test-run-ns-tests
                :at-point #'cider-test-run-test)
  :config
  (set-evil-state 'cider-repl-mode 'insert)
  (set-evil-state 'cider-test-report-mode 'insert)
  (set-popup-buffer (rx bos "*cider-repl " (one-or-more anything) "*")
                    (rx bos "*cider-test-report*")))

(provide 'lang-clojure)
;;; clojure.el ends here
