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

(set-test-fns 'ruby-mode
              :all #'ruby-test-all
              :file #'ruby-test-file
              :at-point #'ruby-test-at-point)

(use-package yard-mode
  :diminish yard-mode
  :hook (ruby-mode enh-ruby-mode))

(use-package yari
  :commands yari
  :init
  (define-key 'help-command (kbd "R") #'yari)
  (set-doc-fn '(ruby-mode enh-ruby-mode) #'yari)
  :config
  (set-evil-state 'yari-mode 'motion))

(use-package ruby-refactor
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

(use-package minitest
  :hook
  ((ruby-mode enh-ruby-mode) . minitest-enable-appropriate-mode)
  :commands minitest-mode
  :init
  (setq minitest-keymap-prefix (kbd "C-c C-t")))

(use-package rspec-mode
  :hook
  ((ruby-mode enh-ruby-mode) . rspec-enable-appropriate-mode)
  :commands rspec-mode
  :init
  (setq rspec-key-command-prefix (kbd "C-c C-t")
        rspec-use-opts-file-when-available nil
        rspec-command-options "--format progress"))



(use-package company-inf-ruby
  :requires company
  :commands company-inf-ruby
  :init
  (set-company-backends 'inf-ruby-mode 'company-inf-ruby))

(use-package rake
  :commands (rake rake-find-task rake-rerun)
  :config
  (setq rake-completion-system 'default
        rake-cache-file (concat my-cache-dir "rake.cache")))

(provide 'lang-ruby)
;;; ruby.el ends here
