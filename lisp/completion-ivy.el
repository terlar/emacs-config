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

(use-package ivy
  :diminish ivy-mode
  :hook (after-init . ivy-mode)
  :general
  (:keymaps 'ivy-mode-map
            [remap switch-to-buffer] 'ivy-switch-buffer
            [remap imenu-anywhere]   'ivy-imenu-anywhere)
  (:keymaps 'ivy-occur-grep-mode-map :states 'normal
            "i" 'ivy-wgrep-change-to-wgrep-mode
            "q" 'quit-window)
  :init
  (setq-default projectile-completion-system 'ivy
                smex-completion-method 'ivy
                magit-completing-read-function #'ivy-completing-read)

  (setq ivy-height 12
        ivy-do-completion-in-region t
        ivy-wrap t
        ivy-use-virtual-buffers t
        ivy-fixed-height-minibuffer t
        ;; Don't use ^ as initial input
        ivy-initial-inputs-alist nil
        ;; highlight til EOL
        ivy-format-function #'ivy-format-function-line
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil)
  :config
  (set-popup-buffer (rx bos "*ivy-occur " (one-or-more anything) "*" eos)))

(use-package swiper
  :commands
  (swiper
   swiper-multi
   swiper-all))

;; Used by `counsel-M-x'
(use-package smex
  :init
  (setq smex-auto-update nil
        smex-save-file (concat my-cache-dir "/smex-items"))
  :config
  (smex-initialize))

(use-package counsel
  :demand t
  :init
  (setq counsel-find-file-ignore-regexp
        "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
        counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  :config
  (general-define-key
   :keymaps 'ivy-mode-map
   [remap find-file]                 'counsel-find-file
   [remap recentf]                   'counsel-recentf
   [remap imenu]                     'counsel-imenu
   [remap bookmark-jump]             'counsel-bookmark
   [remap execute-extended-command]  'counsel-M-x
   [remap describe-function]         'counsel-describe-function
   [remap describe-variable]         'counsel-describe-variable
   [remap describe-face]             'counsel-describe-face
   [remap eshell-list-history]       'counsel-esh-history))

(use-package counsel-projectile
  :demand t
  :config
  (general-define-key
   :keymaps 'ivy-mode-map
   [remap projectile-switch-project]   'counsel-projectile-switch-project
   [remap projectile-switch-to-buffer] 'counsel-projectile-switch-to-buffer
   [remap projectile-find-file]        'counsel-projectile-find-file
   [remap projectile-find-dir]         'counsel-projectile-find-dir))

;; Use ivy for xref lookups
(use-package ivy-xref
  :commands ivy-xref-show-xrefs
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; Icons in ivy buffers
(use-package all-the-icons-ivy
  :demand t
  :after (ivy counsel counsel-projectile)
  :config
  (add-graphic-hook (all-the-icons-ivy-setup)))

(provide 'completion-ivy)
;;; completion-ivy.el ends here
