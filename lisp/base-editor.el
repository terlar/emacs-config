;;; base-editor.el --- Editor configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Defining the behavior of things.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package)
  (require 'base-lib)
  (require 'base-keybinds))

;;;
;; Settings

(setq-default
 vc-follow-symlinks t
 ;; Save clipboard contents into kill-ring before replacing them
 save-interprogram-paste-before-kill t
 ;; Bookmarks
 bookmark-default-file (concat my-data-dir "bookmarks")
 bookmark-save-flag 1
 ;; Formatting
 delete-trailing-lines nil
 fill-column 80
 sentence-end-double-space nil
 ;; White-space
 indent-tabs-mode nil
 require-final-newline t
 tab-always-indent t
 tab-width 4
 tabify-regexp "^\t* [ \t]+"
 whitespace-line-column fill-column
 whitespace-style
 '(face tabs tab-mark spaces space-mark trailing lines-tail)
 whitespace-display-mappings
 '((tab-mark ?\t [?› ?\t])
   (newline-mark 10 [?¬ 10])
   (space-mark 32 [183] [46]))
 ;; Give the text some space
 line-spacing 0.2
 ;; Wrapping
 default-truncate-lines nil
 truncate-partial-width-windows 50
 word-wrap t
 ;; Don't prompt for local variables
 enable-local-eval nil
 enable-local-variables :safe)

(defun +dont-kill-scratch-buffer ()
  "Don't kill scratch buffers."
  (or (not (string= (buffer-name) "*scratch*"))
      (ignore (bury-buffer))))
(add-hook 'kill-buffer-query-functions #'+dont-kill-scratch-buffer)

;; Make scripts executable on save
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Automatic indentation
(electric-indent-mode 1)

;;;
;; Built-ins

;; Revert buffers for changed files
(req-package autorevert
  :diminish auto-revert-mode
  :defer 2
  :init
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode 1))

;; Documentation lines
(req-package eldoc
  :diminish eldoc-mode)

;; Ediff: use existing frame instead of creating a new one
(req-package ediff
  :commands
  (ediff-copy-diff
   ediff-get-region-contents
   ediff-setup-windows-plain)
  :hook (ediff-quit . winner-undo)
  :general
  (:keymaps 'ediff-mode-map
            "d" '(ediff-copy-both-to-C      :wk "Copy both to C")
            "j" '(ediff-next-difference     :wk "Next difference")
            "k" '(ediff-previous-difference :wk "Previous difference"))
  :init
  (defun ediff-copy-both-to-C ()
    "Copy change from both A and B to C."
    (interactive)
    (ediff-copy-diff
     ediff-current-difference nil 'C nil
     (concat
      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

  (setq ediff-diff-options "-w"
        ;; Split horizontally
        ediff-merge-split-window-function #'split-window-horizontally
        ediff-split-window-function #'split-window-horizontally
        ;; No extra frames
        ediff-window-setup-function #'ediff-setup-windows-plain))

;; Smart expansion completions
(req-package hippie-exp
  :demand t
  :init
  (setq hippie-expand-try-functions-list
        '(;; Try to expand word "dynamically", searching the current buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill ring.
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a file name, as many characters as unique.
          try-complete-file-name-partially
          ;; Try to complete text as a file name.
          try-complete-file-name
          ;; Try to expand word before point according to all abbrev tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-list
          try-expand-line
          ;; Try to complete as an Emacs Lisp symbol, as many characters as unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol)))

;; Keep track of recently opened files
(req-package recentf
  :defer 1
  :init
  (setq recentf-exclude
        (list "/tmp/"           ; Temp-files
              "/dev/shm"        ; Potential secrets
              "/ssh:"           ; Files over SSH
              "/TAGS$"          ; Tag files
              "^/\\.git/.+$"    ; Git contents
              "\\.?ido\\.last$"
              "\\.revive$"
              "^/var/folders/.+$"
              (concat "^" my-data-dir ".+$"))
        recentf-filename-handlers '(abbreviate-file-name)
        recentf-max-menu-items 0
        recentf-max-saved-items 250
        recentf-save-file (concat my-cache-dir "recentf")
        recentf-auto-cleanup 'never)
  :config
  (quiet! (recentf-mode 1)))

;; Persistent minibuffer history
(req-package savehist
  :demand t
  :init
  (setq savehist-additional-variables '(search-ring regexp-search-ring)
        ;; Save on kill only
        savehist-autosave-interval 60
        savehist-file (concat my-cache-dir "savehist")
        savehist-save-minibuffer-history t)
  :config
  (savehist-mode 1))

;; Persistent point place
(req-package saveplace
  :demand t
  :init
  (setq save-place-file (concat my-cache-dir "saveplace"))
  :config
  (save-place-mode 1))

;;;
;; Packages

;; Automatic indentation as you type
(req-package aggressive-indent
  :diminish aggressive-indent-mode
  :commands
  (aggressive-indent-mode
   global-aggressive-indent-mode)
  :defer 2
  :config
  ;; Disabled modes
  (dolist (mode '(diff-auto-refine-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Disabled commands
  (dolist (command '(evil-undo-pop
                     ws-butler-clean-region))
    (push command aggressive-indent-protected-commands))

  (global-aggressive-indent-mode 1))

;; Delete selection upon insertion or DEL
(req-package delsel
  :commands delete-selection-mode
  :defer 2
  :config
  (delete-selection-mode 1))

;; Handles white-space (tabs/spaces) settings externally. This way projects can
;; specify their own formatting rules.
(req-package editorconfig
  :mode ("\\.?editorconfig$" . editorconfig-conf-mode)
  :diminish editorconfig-mode
  :demand t
  :init
  (autoload 'editorconfig-conf-mode "editorconfig-conf-mode" nil t)
  :config
  (add-hook 'editorconfig-custom-hooks #'+ws-butler-editorconfig)
  (editorconfig-mode 1))

(req-package exec-path-from-shell
  :commands exec-path-from-shell-initialize
  :hook (projectile-after-switch-project . exec-path-from-shell-initialize)
  :init
  (when (memq window-system '(mac ns))
    (setq exec-path-from-shell-shell-name "/usr/local/bin/fish")
    (exec-path-from-shell-initialize)))

;; Ignore files
(req-package ignoramus
  :demand t
  :config
  ;; Ignore some additional directories
  (dolist (name '("node_modules" "vendor" "elm-stuff"))
    (push name ignoramus-file-basename-exact-names))
  (ignoramus-setup))

;; Auto-close delimiters and blocks as you type
(req-package smartparens
  :diminish smartparens-mode
  :demand t
  :init
  (setq sp-autowrap-region nil ; Let evil-surround handle this
        sp-highlight-pair-overlay nil
        sp-cancel-autoskip-on-backward-movement nil
        sp-show-pair-delay 0
        sp-max-pair-length 3)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1))

;; Branching & persistent undo
(req-package undo-tree
  :diminish undo-tree-mode
  :demand t
  :general
  (:keymaps 'visual
            "C-u" 'undo-tree-undo
            "C-r" 'undo-tree-redo)
  (:keymaps 'normal
            "u"   'undo-tree-undo
            "C-r" 'undo-tree-redo)
  :init
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        (list (cons "." (concat my-cache-dir "undo-tree-hist/")))
        undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t)
  :config
  (defun clear-undo-tree ()
    "Clear undo-tree."
    (interactive)
    (setq buffer-undo-tree nil))

  (defun +undo-tree-quiet-load (orig-fn &rest args)
    "Silence undo-tree load errors."
    (quiet! (apply orig-fn args)))
  (advice-add #'undo-tree-load-history-hook :around #'+undo-tree-quiet-load)

  (global-undo-tree-mode 1))

;; Delete trailing white-space before save
(req-package ws-butler
  :diminish ws-butler-mode
  :commands ws-butler-mode
  :init
  (defun +ws-butler-editorconfig (props)
    "Use ws-butler mode instead of delete-trailing-whitespace."
    (if (equal (gethash 'trim_trailing_whitespace props) "true")
        (progn
          (setq write-file-functions (delete 'delete-trailing-whitespace write-file-functions))
          (ws-butler-mode 1))
      (ws-butler-mode 0))))

;;;
;; Auto-loaded Packages

;; Selection helper
(req-package expand-region
  :commands (er/contract-region
             er/expand-region
             er/mark-symbol
             er/mark-word))

;; Move point through buffer-undo-list positions
(req-package goto-last-change
  :commands goto-last-change)

;; A better *help* buffer
(req-package helpful
  :commands
  (helpful-at-point
   helpful-callable helpful-command
   helpful-function helpful-key helpful-macro
   helpful-symbol helpful-variable)
  :general
  (:keymaps 'helpful-mode-map :states '(normal motion emacs)
            "[[" 'backward-button
            "]]" 'forward-button
            "o" '(ace-link-help :package 'ace-link))
  (:keymaps 'help-map
            "C" 'helpful-command
            "f" 'helpful-callable
            "F" 'helpful-function
            "k" 'helpful-key
            "v" 'helpful-variable
            "M" 'helpful-macro)
  :config
  (set-evil-state 'helpful-mode 'motion)
  (set-popup-buffer (rx bos "*helpful" (one-or-more anything) "*" eos)))

;; Jump to document locations in any buffer
(req-package imenu-anywhere
  :commands
  (helm-imenu-anywhere
   ido-imenu-anywhere
   ivy-imenu-anywhere))

;; Document locations
(req-package imenu-list
  :commands imenu-list-minor-mode)

;; Convert between regexp syntax
(req-package pcre2el
  :commands rxt-quote-pcre)

;; Display colors
(req-package rainbow-mode
  :minor
  "-theme\\.el$"
  :commands rainbow-mode)

;; Semantic navigation
(req-package smart-forward
  :commands
  (smart-backward
   smart-forward
   smart-down
   smart-up))

;; Peek definition (Display function source inline)
(req-package source-peek
  :el-get t :ensure nil
  :commands source-peek)

;; Treat camel-case and snake-case words as separate words
(req-package subword
  :diminish subword-mode
  :commands subword-mode)

;; Utility for opening files with sudo
(req-package sudo-edit
  :commands sudo-edit)

;; Writable grep buffer and apply the changes to files
(req-package wgrep-ag
  :commands
  (wgrep-ag-setup
   wgrep-change-to-wgrep-mode)
  :general
  (:keymaps 'wgrep-mode-map :states 'normal
            "d"  'wgrep-mark-deletion
            "ZZ" 'wgrep-finish-edit)
  :init
  (setq wgrep-auto-save-buffer t))

(req-package zoom-window
  :commands zoom-window-zoom)

(provide 'base-editor)
;;; base-editor.el ends here
