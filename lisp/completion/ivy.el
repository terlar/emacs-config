;;; ivy.el --- Completion system -*- lexical-binding: t; -*-

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

;;;
;; Packages

(req-package ivy
  :diminish ivy-mode
  :hook (after-init . ivy-mode)
  :custom
  (ivy-wrap t)
  (ivy-on-del-error-function #'ignore)
  (ivy-use-virtual-buffers t)
  ;; Allow selecting the prompt as a candidate (e.g for creating a new file)
  (ivy-use-selectable-prompt t)
  ;; Highlight whole line
  (ivy-format-function #'ivy-format-function-line)
  (ivy-fixed-height-minibuffer t)
  :general
  (:keymaps 'ivy-mode-map
            [remap switch-to-buffer] 'ivy-switch-buffer
            [remap imenu-anywhere]   'ivy-imenu-anywhere
            "C-o"                    'ivy-dispatching-done
            "C-c C-r"                'ivy-resume)
  (:keymaps 'ivy-occur-grep-mode-map :states '(normal emacs)
            "i" 'ivy-wgrep-change-to-wgrep-mode
            "q" 'quit-window)
  (:keymaps 'ivy-minibuffer-map
            "M-v"    'yank
            "M-z"    'undo
            "C-k"    'ivy-previous-line
            "C-j"    'ivy-next-line
            "C-l"    'ivy-alt-done
            "C-w"    'ivy-backward-kill-word
            "C-u"    'ivy-kill-line
            "C-b"    'backward-word
            "C-f"    'forward-word)
  :init
  ;; Don't use ^ as initial input
  (setq ivy-initial-inputs-alist nil)

  (setq-default projectile-completion-system 'ivy
                smex-completion-method 'ivy
                magit-completing-read-function #'ivy-completing-read)
  :config
  (set-evil-state 'ivy-occur-grep-mode 'normal))

(req-package swiper
  :commands
  (swiper
   swiper-multi
   swiper-all))

;; Used by `counsel-M-x' for sorting
(req-package smex
  :init
  (setq smex-auto-update nil
        smex-save-file (concat my-cache-dir "/smex-items")))

(req-package counsel
  :diminish counsel-mode
  :hook (ivy-mode . counsel-mode)
  :custom
  (counsel-find-file-ignore-regexp
   "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")
  (counsel-grep-base-command
   "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (counsel-mode-override-describe-bindings t)
  :general
  (:keymaps 'evil-ex-completion-map
            "C-r" 'counsel-minibuffer-history)
  (:keymaps 'counsel-mode-map
            "C-c g" 'counsel-git
            "C-c j" 'counsel-git-grep
            "C-c J" 'counsel-rg
            ;; Use counsel/swiper for search
            "C-r"   'counsel-grep-or-swiper
            "C-s"   'counsel-grep-or-swiper
            "C-x /" 'counsel-abbrev)
  (:keymaps 'counsel-ag-map
            "C-SPC" 'ivy-call-and-recenter)
  :config
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
          (abbrev-insert (abbrev-symbol abbrev-name table)))))))

(req-package counsel-projectile
  :diminish counsel-projectile-mode
  :hook (counsel-mode . counsel-projectile-mode))

(req-package counsel-tramp
  :commands counsel-tramp)

;; Use ivy for xref lookups
(req-package ivy-xref
  :commands ivy-xref-show-xrefs
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; Snippets with preview
(req-package ivy-yasnippet
  :general
  ([remap yas-insert-snippet] 'ivy-yasnippet))

;; Icons in ivy buffers
(req-package all-the-icons-ivy
  :after (ivy counsel counsel-projectile)
  :defer 1
  :custom
  (all-the-icons-ivy-file-commands '(counsel-projectile counsel-find-file counsel-projectile-find-file counsel-projectile-find-dir counsel-git))
  :config
  (all-the-icons-ivy-setup))

(provide 'completion-ivy)
;;; ivy.el ends here
