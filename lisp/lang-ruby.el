;;; lang-ruby.el --- Ruby -*- lexical-binding: t; -*-

;;; Commentary:
;; Ruby is a dynamic, reflective, object-oriented, general-purpose programming
;; language. It was designed and developed in the mid-1990s by
;; Yukihiro "Matz" Matsumoto in Japan.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package)
  (require 'base-keybinds))

;;;
;; Packages

(req-package ruby-mode
  :mode
  "\\.rb$"
  "\\.\\(rake\\|gemspec\\|ru\\|thor\\|pryrc\\)$"
  "/\\(Gem\\|Cap\\|Vagrant\\|Rake\\|Pod\\|Puppet\\|Berks\\)file$"
  :interpreter "ruby"
  :hook
  (ruby-mode . flycheck-mode)
  (ruby-mode . rainbow-identifiers-mode)
  (ruby-mode . +rainbow-identifiers-delayed-refresh)
  :general
  (:keymaps 'ruby-mode-map
            "C-c /" 'nil)
  :init
  (setq ruby-align-chained-calls t))

(req-package robe
  :hook
  ((ruby-mode enh-ruby-mode) . robe-mode)
  :config
  (set-doc-fn '(ruby-mode enh-ruby-mode) #'robe-doc)
  (smart-jump-register :modes 'robe-mode
                       :jump-fn #'robe-jump
                       :pop-fn #'xref-pop-marker-stack
                       :refs-fn #'smart-jump-simple-find-references)

  (set-company-backends '(ruby-mode enh-ruby-mode) 'company-robe)
  (set-popup-buffer (rx bos "*robe-doc*" eos)))

(req-package yard-mode
  :diminish yard-mode
  :hook (ruby-mode enh-ruby-mode))

(req-package ruby-refactor
  :hook ((ruby-mode enh-ruby-mode) . ruby-refactor-mode))

(req-package rspec-mode
  :minor
  "_spec\\.rb$")

(req-package inf-ruby
  :hook
  ((ruby-mode enh-ruby-mode) . inf-ruby-minor-mode)
  (compilation-filter . inf-ruby-auto-enter)
  :commands
  (inf-ruby
   inf-ruby-console-auto)
  :init
  (autoload 'eir-eval-in-ruby "eval-in-repl-ruby")

  (defun ruby-repl ()
    "Open a Ruby REPL."
    (interactive)
    (if (and (projectile-project-p) (projectile-file-exists-p "Gemfile"))
        (inf-ruby-console-auto)
      (inf-ruby)))

  (set-repl-command '(ruby-mode enh-ruby-mode) #'ruby-repl)
  (set-eval-command '(ruby-mode enh-ruby-mode) #'eir-eval-in-ruby)

  (set-popup-buffer (rx bos "*"
                        (or "ruby" "pry" "gem" "bundle console")
                        "*" eos))
  (set-evil-state 'inf-ruby-mode 'insert)

  (setq inf-ruby-default-implementation "pry"))

(req-package company-inf-ruby
  :requires company
  :commands company-inf-ruby
  :init
  (set-company-backends 'inf-ruby-mode 'company-inf-ruby))

(provide 'lang-ruby)
;;; lang-ruby.el ends here
