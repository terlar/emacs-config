;;; completion-ivy.el --- Completion system -*- lexical-binding: t; -*-

;;; Commentary:
;; Completing all your things.

;;; Code:
(eval-when-compile
  (require 'base-vars)
  (require 'base-package)
  (require 'base-lib)
  (require 'base-keybinds))

;;;
;; Packages

(req-package ivy
  :diminish ivy-mode
  :general
  (:keymaps 'ivy-mode-map
            [remap switch-to-buffer] 'ivy-switch-buffer
            [remap imenu-anywhere]   'ivy-imenu-anywhere)
  (:keymaps 'ivy-occur-grep-mode-map :states 'normal
            "i" 'ivy-wgrep-change-to-wgrep-mode
            "q" 'quit-window)
  :demand t
  :init
  (setq-default projectile-completion-system 'ivy
                smex-completion-method 'ivy
                magit-completing-read-function #'ivy-completing-read)

  (setq ivy-height 12
        ivy-do-completion-in-region nil
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ;; Don't use ^ as initial input
        ivy-initial-inputs-alist nil
        ;; highlight til EOL
        ivy-format-function #'ivy-format-function-line
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil)
  :config
  (set-popup-buffer (rx bos "*ivy-occur " (one-or-more anything) "*" eos))

  (ivy-mode 1))

(req-package swiper
  :require ivy
  :after ivy
  :commands
  (swiper
   swiper-multi
   swiper-all))

(req-package counsel
  :require swiper
  :after ivy
  :general
  (:keymaps 'ivy-mode-map
            [remap find-file]                 'counsel-find-file
            [remap recentf]                   'counsel-recentf
            [remap imenu]                     'counsel-imenu
            [remap bookmark-jump]             'counsel-bookmark
            [remap projectile-switch-project] 'counsel-projectile-switch-project
            [remap projectile-find-file]      'counsel-projectile-find-file
            [remap execute-extended-command]  'counsel-M-x
            [remap describe-function]         'counsel-describe-function
            [remap describe-variable]         'counsel-describe-variable
            [remap describe-face]             'counsel-describe-face)
  :init
  (setq counsel-find-file-ignore-regexp
        "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
        counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))
(req-package counsel-projectile
  :require counsel projectile
  :after counsel)

;; Used by `counsel-M-x'
(req-package smex
  :init
  (setq smex-auto-update nil
        smex-save-file (concat my-cache-dir "/smex-items"))
  :config
  (smex-initialize))

;; Icons in ivy buffers
(req-package all-the-icons-ivy
  :require ivy
  :after ivy
  :commands all-the-icons-ivy-setup
  :init
  (add-graphic-hook (all-the-icons-ivy-setup)))

(provide 'completion-ivy)
;;; completion-ivy.el ends here
