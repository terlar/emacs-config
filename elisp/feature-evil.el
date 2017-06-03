;;; feature-evil.el --- Extensible vi layer

;;; Commentary:
;; The way of the vi!

;;; Code:
;; base-theme.el vars
(defvar my-evil-mode-color nil)
(defvar my-evil-default-mode-color nil)

(use-package evil :demand t
  :commands (evil-select-search-module evil-set-initial-state evil-state-property)
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
                  help-mode
                  git-rebase-mode))
    (evil-set-initial-state mode 'emacs))
  (dolist (mode '(debugger-mode))
    (evil-set-initial-state mode 'normal))
  (dolist (mode '(git-commit-mode))
    (evil-set-initial-state mode 'insert))

  (add-hook 'minibuffer-inactive-mode-hook #'minibuffer-inactive-mode-hook-setup))

;; State indicator box
(defun evil-generate-mode-line-tag (&optional state)
  "Generate the evil mode-line tag for STATE."
  (let ((tag (evil-state-property state :tag t))
        (color (alist-get state my-evil-mode-color my-evil-default-mode-color)))
    ;; prepare mode-line: add tooltip
    (if (stringp tag)
        (progn
          (face-remap-add-relative 'anzu-mode-line :background color)
          (propertize " "
                      'face (list :background color :foreground color :box t)
                      'help-echo (evil-state-property state :name)
                      'mouse-face 'mode-line-highlight))
      tag)))

;; Escape hooks
(defvar my-evil-esc-hook '(t)
  "A hook run after ESC is pressed in normal mode (invoked by
`evil-force-normal-state'). If a hook returns non-nil, all hooks after it are
ignored.")

(defun my-evil-attach-escape-hook ()
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

(advice-add #'evil-force-normal-state :after #'my-evil-attach-escape-hook)

;;;
;; Plugins
(use-package evil-escape :demand t
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit)
        evil-escape-excluded-major-modes '(neotree-mode))
  :config
  (evil-escape-mode +1)

  ;; Escape everything
  (dolist (map '(evil-normal-state-map
                 evil-replace-state-map
                 evil-visual-state-map
                 evil-operator-state-map))
    (bind-key "C-g" #'evil-escape (eval map))))

(use-package evil-commentary
  :after evil
  :commands (evil-commentary evil-commentary-yank evil-commentary-line)
  :config (evil-commentary-mode +1))

(use-package evil-matchit
  :after evil
  :commands (global-evil-matchit-mode evilmi-jump-items)
  :bind
  ([remap evil-jump-item] . evilmi-jump-items)
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

(provide 'feature-evil)
;;; feature-evil.el ends here
