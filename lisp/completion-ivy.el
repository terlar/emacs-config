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
;; Functions

(defun counsel-abbrev (abbrev-name)
  "Insert abbreviation matching ABBREV-NAME."
  (interactive
   (list
    (ivy-completing-read "Insert abbrev: "
                         (cl-loop for table in (abbrev--active-tables)
                                  unless (abbrev-table-empty-p table)
                                  append (append (delete 0 table) ())))))
  (progn
    (dolist (table (abbrev--active-tables))
      (when (abbrev-symbol abbrev-name table)
        (abbrev-insert (abbrev-symbol abbrev-name table))))))

;;;
;; Packages

(req-package ivy
  :diminish ivy-mode
  :hook (after-init . ivy-mode)
  :general
  (:keymaps 'ivy-mode-map
            [remap switch-to-buffer] 'ivy-switch-buffer
            [remap imenu-anywhere]   'ivy-imenu-anywhere
            "C-o"                    'ivy-dispatching-done)
  (:keymaps 'ivy-occur-grep-mode-map :states '(normal emacs)
            "i" 'ivy-wgrep-change-to-wgrep-mode
            "q" 'quit-window)
  (:keymaps 'ivy-minibuffer-map
            [escape] 'keyboard-escape-quit
            "M-v"    'yank
            "M-z"    'undo
            "C-r"    'evil-paste-from-register
            "C-e"    'ivy-insert-current
            "C-k"    'ivy-previous-line
            "C-j"    'ivy-next-line
            "C-l"    'ivy-alt-done
            "C-w"    'ivy-backward-kill-word
            "C-u"    'ivy-kill-line
            "C-b"    'backward-word
            "C-f"    'forward-word)
  :init
  (setq-default projectile-completion-system 'ivy
                smex-completion-method 'ivy
                magit-completing-read-function #'ivy-completing-read)

  (setq ivy-height 12
        ivy-fixed-height-minibuffer t
        ivy-do-completion-in-region t
        ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t
        ivy-wrap t
        ;; Don't use ^ as initial input
        ivy-initial-inputs-alist nil
        ;; highlight til EOL
        ivy-format-function #'ivy-format-function-line
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil)
  :config
  (set-evil-state 'ivy-occur-grep-mode 'normal)
  (set-popup-buffer (rx bos "*ivy-occur " (one-or-more anything) "*" eos)))

(req-package swiper
  :commands
  (swiper
   swiper-multi
   swiper-all))

;; Used by `counsel-M-x'
(req-package smex
  :init
  (setq smex-auto-update nil
        smex-save-file (concat my-cache-dir "/smex-items"))
  :config
  (smex-initialize))

(req-package counsel
  :demand t
  :general
  (:keymaps 'ivy-mode-map
            [remap find-file]                 'counsel-find-file
            [remap recentf]                   'counsel-recentf
            [remap imenu]                     'counsel-imenu
            [remap bookmark-jump]             'counsel-bookmark
            [remap execute-extended-command]  'counsel-M-x
            [remap describe-function]         'counsel-describe-function
            [remap describe-variable]         'counsel-describe-variable
            [remap describe-face]             'counsel-describe-face
            [remap eshell-list-history]       'counsel-esh-history)
  (:keymaps 'counsel-ag-map
            [backtab] 'ivy-occur
            "C-SPC"   'ivy-call-and-recenter)
  :init
  (setq counsel-find-file-ignore-regexp
        "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
        counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))

(req-package counsel-projectile
  :demand t
  :after (counsel projectile)
  :general
  (:keymaps 'ivy-mode-map
            [remap projectile-switch-project]   'counsel-projectile-switch-project
            [remap projectile-switch-to-buffer] 'counsel-projectile-switch-to-buffer
            [remap projectile-find-file]        'counsel-projectile-find-file
            [remap projectile-find-dir]         'counsel-projectile-find-dir))

(req-package counsel-tramp
  :commands counsel-tramp)

;; Use ivy for xref lookups
(req-package ivy-xref
  :commands ivy-xref-show-xrefs
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; Icons in ivy buffers
(req-package all-the-icons-ivy
  :demand t
  :after (ivy counsel counsel-projectile)
  :config
  (add-graphic-hook (all-the-icons-ivy-setup)))

(provide 'completion-ivy)
;;; completion-ivy.el ends here
