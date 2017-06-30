;;; lang-ruby.el --- Ruby

;;; Commentary:
;; Ruby is a dynamic, reflective, object-oriented, general-purpose programming
;; language. It was designed and developed in the mid-1990s by
;; Yukihiro "Matz" Matsumoto in Japan.

;;; Code:
(require 'base-vars)
(require 'base-lib)
(require 'base-keybinds)

;;;
;; Packages

(use-package enh-ruby-mode
  :mode (("\\.rb$" . enh-ruby-mode)
         ("\\.\\(rake\\|gemspec\\|ru\\|thor\\|pryrc\\)$" . enh-ruby-mode)
         ("/\\(Gem\\|Cap\\|Vagrant\\|Rake\\|Pod\\|Puppet\\|Berks\\)file$" . enh-ruby-mode))
  :interpreter "ruby"
  :defines enh-ruby-deep-indent-paren
  :config
  ;; Don't indent the parenthesis or bracket based on the previous line.
  (setq enh-ruby-deep-indent-paren nil)

  (add-hook 'enh-ruby-mode-hook #'flycheck-mode)
  (add-hook 'enh-ruby-mode-hook #'rainbow-identifiers-mode))

(use-package robe
  :commands (robe-mode robe-start)
  :init
  (add-hook 'enh-ruby-mode-hook #'robe-mode)
  (with-eval-after-load "company"
    (push-company-backends 'enh-ruby-mode '(company-robe company-dabbrev-code company-files))))

(use-package ruby-refactor
  :commands
  (ruby-refactor-extract-to-method
   ruby-refactor-extract-local-variable
   ruby-refactor-extract-constant ruby-refactor-add-parameter
   ruby-refactor-extract-to-let ruby-refactor-convert-post-conditional))

(use-package yard-mode
  :commands yard-mode
  :init (add-hook 'enh-ruby-mode-hook #'yard-mode))

(use-package rspec-mode
  :general
  (:keymaps 'rspec-mode-map :states 'normal :prefix "C-c t"
            "r" '(rspec-rerun)
            "a" '(rspec-verify-all)
            "s" '(rspec-verify-single)
            "v" '(rspec-verify))
  :init
  (defvar rspec-mode-verifiable-map (make-sparse-keymap))
  (defvar evilmi-ruby-match-tags
    '((("unless" "if") ("elsif" "else") "end")
      ("begin" ("rescue" "ensure") "end")
      ("case" ("when" "else") "end")
      (("class" "def" "while" "do" "module" "for" "until") () "end")
      ;; Rake
      (("task" "namespace") () "end"))))

(use-package inf-ruby
  :commands (inf-ruby inf-ruby-console-auto))

(use-package company-inf-ruby
  :when (package-installed-p 'company)
  :after inf-ruby
  :config
  (with-eval-after-load "company"
    (push-company-backends 'inf-ruby-mode '(company-inf-ruby))))

(provide 'lang-ruby)
;;; lang-ruby.el ends here
