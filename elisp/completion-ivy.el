;;; completion-ivy.el --- Completion system

;;; Commentary:
;; Completing all your things.

;;; Code:
;; base.el vars
(defvar my-cache-dir nil)

(use-package ivy :demand t
  :bind
  (:map
   ivy-mode-map
   ([remap find-file]                 . counsel-find-file)
   ([remap switch-to-buffer]          . ivy/switch-buffer)
   ([remap recentf]                   . counsel-recentf)
   ([remap imenu]                     . counsel-imenu)
   ([remap bookmark-jump]             . counsel-bookmark)
   ([remap projectile-switch-project] . counsel-projectile-switch-project)
   ([remap projectile-find-file]      . counsel-projectile-find-file)
   ([remap imenu-anywhere]            . ivy-imenu-anywhere)
   ([remap execute-extended-command]  . counsel-M-x)
   ([remap describe-function]         . counsel-describe-function)
   ([remap describe-variable]         . counsel-describe-variable)
   ([remap describe-face]             . counsel-describe-face))
  :config
  (setq ivy-height 12
        ivy-do-completion-in-region nil
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        projectile-completion-system 'ivy
        smex-completion-method 'ivy
        ;; Don't use ^ as initial input
        ivy-initial-inputs-alist nil
        ;; highlight til EOL
        ivy-format-function #'ivy-format-function-line
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil)

  (with-eval-after-load 'magit
    (setq magit-completing-read-function #'ivy-completing-read))

  (ivy-mode +1))

(use-package swiper :commands (swiper swiper-all))

(use-package counsel
  :after ivy
  :config
  (use-package counsel-projectile :demand t)
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"))

;; Used by `counsel-M-x'
(use-package smex
  :commands (smex smex-major-mode-commands)
  :config
  (setq smex-save-file (concat my-cache-dir "/smex-items"))
  (smex-initialize))


(provide 'completion-ivy)
;;; completion-ivy.el ends here
