;;; lang-ruby.el --- Ruby -*- lexical-binding: t; -*-

;;; Commentary:
;; Ruby is a dynamic, reflective, object-oriented, general-purpose programming
;; language. It was designed and developed in the mid-1990s by
;; Yukihiro "Matz" Matsumoto in Japan.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

;;;
;; Packages

(use-package enh-ruby-mode
  :mode
  ("\\.rb$" . enh-ruby-mode)
  ("\\.\\(rake\\|gemspec\\|ru\\|thor\\|pryrc\\)$" . enh-ruby-mode)
  ("/\\(Gem\\|Cap\\|Vagrant\\|Rake\\|Pod\\|Puppet\\|Berks\\)file$" . enh-ruby-mode)
  :interpreter
  ("ruby" . enh-ruby-mode)
  :hook
  (enh-ruby-mode . flycheck-mode)
  (enh-ruby-mode . rainbow-identifiers-mode)
  (enh-ruby-mode . +rainbow-identifiers-delayed-refresh)
  :init
  ;; Don't indent the parenthesis or bracket based on the previous line.
  (setq enh-ruby-deep-indent-paren nil))

(use-package robe
  :hook (enh-ruby-mode . robe-mode)
  :config
  (set-doc-fn 'enh-ruby-mode #'robe-doc)
  (smart-jump-register :modes 'enh-ruby-mode
                       :jump-fn #'robe-jump
                       :pop-fn #'xref-pop-marker-stack
                       :refs-fn #'smart-jump-simple-find-references)

  (set-company-backends 'enh-ruby-mode 'company-robe)
  (set-popup-buffer (rx bos "*robe-doc*" eos)))

(use-package yard-mode
  :diminish yard-mode
  :hook enh-ruby-mode)

(use-package ruby-refactor
  :hook (enh-ruby-mode . ruby-refactor-mode))

(use-package rspec-mode
  :minor
  "_spec\\.rb$")

(use-package inf-ruby
  :commands
  (inf-ruby
   inf-ruby-console-auto)
  :init
  (autoload 'eir-eval-in-ruby "eval-in-repl-ruby")

  (defun ruby-repl ()
    "Open a Ruby REPL."
    (interactive)
    (ignore-errors
      (open-and-switch-to-buffer #'inf-ruby-console-auto "*gem*" t))
    (unless (get-buffer "*gem*")
      (open-and-switch-to-buffer #'inf-ruby "*pry*" t)))

  (set-repl-command 'enh-ruby-mode #'ruby-repl)
  (set-eval-command 'enh-ruby-mode #'eir-eval-in-ruby)

  (set-popup-buffer (rx bos "*"
                        (or "ruby" "pry" "gem" "bundle console")
                        "*" eos))
  (set-evil-state 'inf-ruby-mode 'insert)

  (setq inf-ruby-default-implementation "pry"))

(use-package company-inf-ruby
  :requires company
  :commands company-inf-ruby
  :init
  (set-company-backends 'inf-ruby-mode 'company-inf-ruby))

(provide 'lang-ruby)
;;; lang-ruby.el ends here
