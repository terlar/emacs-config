;;; lang-shell.el --- Shell scripting

;;; Commentary:
;; A shell script is a computer program designed to be run by the Unix shell, a
;; command-line interpreter.

;;; Code:
(use-package bats-mode :commands bats-mode)

(use-package fish-mode
  :mode (("\\.fish$"           . fish-mode)
         ("/fish_funced\\..*$" . fish-mode))
  :commands fish-mode
  :preface
  :init
  (add-hook 'fish-mode-hook
            #'(lambda ()
                (add-hook 'before-save-hook #'fish_indent-before-save))))

(use-package sh-script
  :mode
  (("\\.zsh\\'"  . sh-mode)
   ("\\.bash\\'" . sh-mode))
  :init
  (add-hook 'sh-mode-hook
            #'(lambda ()
                (flycheck-mode +1)
                (highlight-numbers-mode +1)))
  :config
  ;; Use regular indentation for line-continuation
  (setq sh-indent-after-continuation 'always))

(use-package company-shell
  :when (package-installed-p 'company)
  :after sh-script
  :init
  (add-hook 'sh-mode-hook
            #'(lambda ()
                (setq-local company-backends
                            '((company-keywords
                               company-shell
                               company-files)))))
  (add-hook 'fish-mode-hook
            #'(lambda ()
                (setq-local company-backends
                            '((company-keywords
                               company-shell
                               company-fish-shell
                               company-files)))))
  :config
  (setq company-shell-delete-duplicates t))

(provide 'lang-shell)
;;; lang-shell.el ends here
