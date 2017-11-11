;;; feature-evil.el --- Extensible vi layer -*- lexical-binding: t; -*-

;;; Commentary:
;; The way of the vi!

;;; Code:

(eval-when-compile
  (require 'base-keybinds)

  (defvar buffer-face-mode))

(defvar-local evil-pre-insert-state-variable-pitch-mode
  "Hold original `variable-pitch-mode'.")

;;;
;; Packages

(use-package evil :demand t
  :commands
  (evil-normal-state
   evil-select-search-module
   evil-state-property evil-set-initial-state evil-force-normal-state
   evil-ex-nohighlight evil-ex-hl-active-p
   evil-window-top evil-window-middle evil-window-bottom
   evil-shift-left evil-shift-right
   evil-visual-restore evil-visual-make-selection)
  :preface
  ;; Make `try-expand-dabbrev' from `hippie-expand' work in mini-buffer
  ;; @see `he-dabbrev-beg', so we need re-define syntax for '/'
  (defun minibuffer-inactive-mode-hook-setup ()
    (set-syntax-table (let* ((table (make-syntax-table)))
                        (modify-syntax-entry ?/ "." table)
                        table)))
  :init
  (setq-default
   evil-mode-line-format '(before . mode-line-front-space)
   evil-want-C-u-scroll t
   evil-want-visual-char-semi-exclusive t
   evil-magic t
   evil-echo-state t
   evil-indent-convert-tabs t
   evil-ex-search-vim-style-regexp t
   evil-ex-substitute-global t
   ;; Column range for ex commands
   evil-ex-visual-char-range t
   evil-insert-skip-empty-lines t
   ;; More vim-like behavior
   evil-symbol-word-search t
   ;; Don't activate mark on shift-click
   shift-select-mode nil)

  ;; evil-want-Y-yank-to-eol must be set via customize to have an effect
  (customize-set-variable 'evil-want-Y-yank-to-eol t)
  :config
  (evil-mode +1)
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; Use Emacs key-map for insert state.
  (setq evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

  ;; Default modes
  (dolist (mode '(tabulated-list-mode
                  view-mode comint-mode term-mode
                  calendar-mode Man-mode grep-mode
                  git-rebase-mode deft-mode))
    (evil-set-initial-state mode 'emacs))
  (dolist (mode '(help-mode helpful-mode debugger-mode))
    (evil-set-initial-state mode 'normal))
  (dolist (mode '(git-commit-mode))
    (evil-set-initial-state mode 'insert))
  (dolist (mode '(package-menu-mode))
    (evil-set-initial-state mode 'motion))

  (add-hooks-pair 'minibuffer-inactive-mode 'minibuffer-inactive-mode-hook-setup))

;; Escape hooks
(defvar my-evil-esc-hook '(t)
  "A hook run after ESC is pressed in normal mode.
E.g. invoked by `evil-force-normal-state'.
If a hook returns non-nil, all hooks after it are ignored.")

(defun evil|attach-escape-hook ()
  "Run the `my-evil-esc-hook'."
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ((evil-ex-hl-active-p 'evil-ex-search)
         ;; disable ex search buffer highlights.
         (evil-ex-nohighlight))
        (t
         ;; Run all escape hooks. If any returns non-nil, then stop there.
         (run-hook-with-args-until-success 'my-evil-esc-hook))))
(advice-add #'evil-force-normal-state :after #'evil|attach-escape-hook)

;;;
;; Packages

(use-package evil-escape :demand t
  :diminish evil-escape-mode
  :commands evil-escape
  :init
  (setq-default evil-escape-excluded-states '(normal visual multiedit)
                evil-escape-excluded-major-modes '(neotree-mode))
  :config
  (evil-escape-mode +1)
  ;; Escape everything
  (general-define-key :states '(normal insert replace visual operator)
                      "C-g" 'evil-escape))

(use-package evil-commentary :demand t
  :after evil
  :diminish evil-commentary-mode
  :commands (evil-commentary evil-commentary-yank evil-commentary-line)
  :config (evil-commentary-mode +1))

(use-package evil-matchit
  :after evil
  :commands (global-evil-matchit-mode evilmi-jump-items)
  :general
  ([remap evil-jump-item] 'evilmi-jump-items)
  :config (global-evil-matchit-mode +1))

(use-package evil-surround
  :after evil
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode +1))

(use-package evil-embrace
  :after evil-surround
  :config
  (setq evil-embrace-show-help-p nil)
  (evil-embrace-enable-evil-surround-integration))

;;;
;; Autoloads

;;;###autoload
(defun evil|visual-indent ()
  "Visual indentation restore selection after operation."
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

;;;###autoload
(defun evil|visual-outdent ()
  "Visual outdentation restore selection after operation."
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

;;;###autoload
(defun evil|reselect-paste ()
  "Go back into visual mode and reselect the last pasted region."
  (interactive)
  (destructuring-bind (_ _ _ beg end) evil-last-paste
    (evil-visual-make-selection
     (save-excursion (goto-char beg) (point-marker))
     end)))

;;;###autoload
(defun evil|insert-state-disable-variable-pitch-mode ()
  "Disable `variable-pitch-mode' and store original value."
  (interactive)
  (setq evil-pre-insert-state-variable-pitch-mode buffer-face-mode)
  (variable-pitch-mode -1))

;;;###autoload
(defun evil|insert-state-restore-variable-pitch-mode ()
  "Restore `variable-pitch-mode' from original value."
  (interactive)
  (when evil-pre-insert-state-variable-pitch-mode
    (variable-pitch-mode +1)))

(provide 'feature-evil)
;;; feature-evil.el ends here
