;;; feature-snippets.el --- Snippets -*- lexical-binding: t; -*-

;;; Commentary:
;; Explosive text.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(req-package yasnippet
  :defer 2
  :diminish yas-minor-mode
  :mode
  ("\\.snippet$" . snippet-mode)
  ("\\.yasnippet$" . snippet-mode)
  :hook (after-init . yas-global-mode)
  :init
  (setq yas-snippet-dirs (list (concat user-emacs-directory "snippets"))
        yas-verbosity 0
        yas-indent-line 'auto
        yas-also-auto-indent-first-line t
        yas-snippet-revival t
        ;; Nested snippets
        yas-triggers-in-field t
        yas-wrap-around-region t)
  :config
  (with-eval-after-load 'evil
    (add-hook 'yas/before-expand-snippet-hook #'evil-insert-state)
    (add-hook 'evil-normal-state-entry-hook #'yas-abort-snippet)))

(req-package yasnippet-snippets
  :defer 2)

(provide 'feature-snippets)
;;; feature-snippets.el ends here
