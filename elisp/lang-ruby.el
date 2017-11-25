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

(req-package enh-ruby-mode
  :mode
  ("\\.rb$" . enh-ruby-mode)
  ("\\.\\(rake\\|gemspec\\|ru\\|thor\\|pryrc\\)$" . enh-ruby-mode)
  ("/\\(Gem\\|Cap\\|Vagrant\\|Rake\\|Pod\\|Puppet\\|Berks\\)file$" . enh-ruby-mode)
  :interpreter
  ("ruby" . enh-ruby-mode)
  :init
  ;; Don't indent the parenthesis or bracket based on the previous line.
  (setq enh-ruby-deep-indent-paren nil)
  :config
  (add-hooks-pair 'enh-ruby-mode
                  '(flycheck-mode
                    rainbow-identifiers-mode)))

(req-package robe
  :require enh-ruby-mode
  :after enh-ruby-mode
  :config
  (set-doc-fn 'enh-ruby-mode #'robe-doc)
  (smart-jump-register :modes 'enh-ruby-mode
                       :jump-fn #'robe-jump
                       :pop-fn #'xref-pop-marker-stack
                       :refs-fn #'smart-jump-simple-find-references)

  (set-company-backends 'enh-ruby-mode 'company-robe)
  (set-popup-buffer (rx bos "*robe-doc*" eos))

  (add-hooks-pair 'enh-ruby-mode 'robe-mode))

(req-package yard-mode
  :require enh-ruby-mode
  :after enh-ruby-mode
  :diminish yard-mode
  :config
  (add-hooks-pair 'enh-ruby-mode 'yard-mode))

(req-package ruby-refactor
  :require enh-ruby-mode
  :after enh-ruby-mode)

(req-package rspec-mode
  :minor
  "_spec\\.rb$")

(req-package inf-ruby
  :require enh-ruby-mode
  :after enh-ruby-mode
  :commands
  (inf-ruby
   inf-ruby-console-auto)
  :init
  (autoload 'eir-repl-start "eval-in-repl" nil t)
  (autoload 'eir-eval-in-ruby "eval-in-repl-ruby" nil t)

  (setq inf-ruby-default-implementation "pry")

  (set-popup-buffer (rx bos "*ruby*" eos)
                    (rx bos "*pry*" eos)
                    (rx bos "*gem*" eos)
                    (rx bos "*bundle console*" eos))
  :config
  (set-repl-command 'enh-ruby-mode 'inf-ruby-console-auto)
  (set-eval-command 'enh-ruby-mode 'eir-eval-in-ruby)

  (set-evil-state 'inf-ruby-mode 'insert))

(req-package company-inf-ruby
  :require company inf-ruby
  :after inf-ruby
  :config
  (set-company-backends 'inf-ruby-mode 'company-inf-ruby))

(provide 'lang-ruby)
;;; lang-ruby.el ends here
