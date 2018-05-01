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
;; Functions

(defun ruby-test-all ()
  "Test all files using either RSpec or MiniTest."
  (interactive)
  (if (bound-and-true-p rspec-mode)
      (rspec-verify-all)
    (minitest-verify-all)))

(defun ruby-test-file ()
  "Test current file using either RSpec or MiniTest."
  (interactive)
  (if (bound-and-true-p rspec-mode)
      (rspec-verify)
    (minitest-verify)))

(defun ruby-test-at-point ()
  "Test definition at point using either RSpec or MiniTest."
  (interactive)
  (if (bound-and-true-p rspec-mode)
      (rspec-verify-single)
    (minitest-verify-single)))

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
  :init
  (setq ruby-align-chained-calls t
        ruby-deep-indent-paren t)

  (with-eval-after-load 'hideshow
    (push `(ruby-mode
            ,(rx (or "def" "class" "module" "do" "{" "["))   ; Block start
            ,(rx (or "}" "]" "end"))                         ; Block end
            ,(rx bol
                 (or (+ (zero-or-more blank) "#") "=begin")) ; Comment start
            ruby-forward-sexp nil) hs-special-modes-alist))

  (set-test-fns 'ruby-mode
                :all #'ruby-test-all
                :file #'ruby-test-file
                :at-point #'ruby-test-at-point))

(req-package yard-mode
  :diminish yard-mode
  :hook (ruby-mode enh-ruby-mode))

(req-package yari
  :commands yari
  :init
  (define-key 'help-command (kbd "R") #'yari)
  (set-doc-fn '(ruby-mode enh-ruby-mode) #'yari)
  :config
  (set-evil-state 'yari-mode 'motion)
  (set-popup-buffer (rx bos "*yari " (one-or-more anything) "*" eos)))

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

(req-package minitest
  :hook
  ((ruby-mode enh-ruby-mode) . minitest-enable-appropriate-mode)
  :commands minitest-mode
  :init
  (setq minitest-keymap-prefix (kbd "C-c C-t"))
  :config
  (set-popup-buffer (rx bos "*Minitest" (one-or-more anything) "*" eos)))

(req-package rspec-mode
  :hook
  ((ruby-mode enh-ruby-mode) . rspec-enable-appropriate-mode)
  :commands rspec-mode
  :init
  (setq rspec-key-command-prefix (kbd "C-c C-t")
        rspec-use-opts-file-when-available nil
        rspec-command-options "--format progress")
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
;;; ruby.el ends here
