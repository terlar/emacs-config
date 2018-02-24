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
  (setq ruby-align-chained-calls t
        ruby-deep-indent-paren t))

(req-package yard-mode
  :diminish yard-mode
  :hook (ruby-mode enh-ruby-mode))

(req-package ruby-refactor
  :hook ((ruby-mode enh-ruby-mode) . ruby-refactor-mode)
  :general
  (:keymaps 'ruby-mode-map :major-modes t
            :prefix my-local-leader-key
            :infix "r"
            "b" 'ruby-toggle-block
            "ec" 'ruby-refactor-extract-constant
            "el" 'ruby-refactor-extract-to-let
            "em" 'ruby-refactor-extract-to-method
            "ev" 'ruby-refactor-extract-local-variable
            "ad" 'ruby-refactor-add-parameter
            "cc" 'ruby-refactor-convert-post-conditional))

(req-package ruby-test-mode
  :hook ruby-mode
  :general
  (:keymaps 'ruby-mode-map :major-modes t
            :prefix my-leader-key
            :infix "f"
            "a" 'ruby-test-toggle-implementation-and-specification))

(req-package rspec-mode
  :minor
  "_spec\\.rb$"
  :commands rspec-mode
  :general
  (:keymaps 'ruby-mode-map :major-modes t
            :prefix my-local-leader-key
            :infix "t"
            "r" #'rspec-rerun
            "a" #'rspec-verify-all
            "s" #'rspec-verify-single
            "v" #'rspec-verify)
  (:keymaps 'ruby-mode-map :major-modes t
            :prefix my-local-leader-key
            "a" #'rspec-toggle-spec-and-target)
  :init
  (setq rspec-key-command-prefix (kbd "C-c C-t"))
  :config
  (set-popup-buffer (rx bos "*rspec-compilation*" eos)))

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

(req-package rake
  :commands (rake rake-find-task rake-rerun)
  :config
  (setq rake-completion-system 'default
        rake-cache-file (concat my-cache-dir "rake.cache"))
  (set-popup-buffer (rx bos "*rake-compilation*" eos)))

(req-package robe
  :config
  (set-doc-fn '(ruby-mode enh-ruby-mode) #'robe-doc)
  (smart-jump-register :modes 'robe-mode
                       :jump-fn #'robe-jump
                       :pop-fn #'xref-pop-marker-stack
                       :refs-fn #'smart-jump-simple-find-references)

  (set-company-backends '(ruby-mode enh-ruby-mode) 'company-robe)
  (set-popup-buffer (rx bos "*robe-doc*" eos)))

(provide 'lang-ruby)
;;; lang-ruby.el ends here
