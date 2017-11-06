;;; base-ui.el --- UI configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; The behavior of things.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-lib))

;;;
;; Settings

(setq-default
 ;; Disable bidirectional text for tiny performance boost
 bidi-display-reordering nil
 ;; Disable non selected window highlight
 cursor-in-non-selected-windows nil
 highlight-nonselected-windows nil
 display-line-number-width 3
 blink-matching-paren nil
 frame-inhibit-implied-resize t
 redisplay-dont-pause t
 jit-lock-defer-time nil
 jit-lock-stealth-nice 0.1
 jit-lock-stealth-time 0.2
 jit-lock-stealth-verbose nil
 max-mini-window-height 0.3
 mouse-yank-at-point t           ; Middle-click paste at point, not at click
 resize-mini-windows 'grow-only  ; Minibuffer resizing
 show-help-function nil          ; Hide :help-echo text
 split-width-threshold nil       ; Favor horizontal splits
 use-dialog-box nil              ; Avoid GUI
 visible-cursor nil
 x-stretch-cursor nil
 uniquify-buffer-name-style 'forward
 ;; Fringes
 fringes-outside-margins t
 indicate-buffer-boundaries 'right
 indicate-empty-lines t
 visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
 ;; `pos-tip' defaults
 pos-tip-internal-border-width 6
 pos-tip-border-width 1
 ;; No blinking or beeping
 ring-bell-function #'ignore
 visible-bell nil
 calendar-week-start-day 1)

;; y/n instead of yes/no
(setq-default confirm-kill-emacs 'y-or-n-p)
(fset #'yes-or-no-p #'y-or-n-p)

(defun my|enable-ui-keystrokes ()
  "Enable keystrokes in minibuffer."
  (setq echo-keystrokes 0.02))
(defun my|disable-ui-keystrokes ()
  "Disable keystrokes in minibuffer."
  (setq echo-keystrokes 0))
(my|enable-ui-keystrokes)
(add-hooks-pair 'isearch-mode-hook 'my|disable-ui-keystrokes)
(add-hooks-pair 'isearch-mode-end-hook 'my|enable-ui-keystrokes)

;; Disable menu bar
(menu-bar-mode -1)
;; Tooltips in echo area
(tooltip-mode -1)

(add-graphic-hook
 (scroll-bar-mode -1)
 (tool-bar-mode -1)

 ;; Standardize fringe width
 (push (cons 'left-fringe  my-fringe-width) default-frame-alist)
 (push (cons 'right-fringe my-fringe-width) default-frame-alist)
 (add-hooks-pair '(emacs-startup minibuffer-setup)
                 '(lambda () (set-window-fringes (minibuffer-window) 0 0 nil))))

;; Undo/redo changes to window layout
(defvar winner-dont-bind-my-keys t)
(require 'winner)
(add-hook 'window-setup-hook #'winner-mode)

(defun my|reset-non-gui-bg-color (&optional frame)
  "Unset background color for FRAME without graphic."
  (unless (display-graphic-p frame)
    (set-face-background 'default nil frame)))
(add-hook 'after-make-frame-functions #'my|reset-non-gui-bg-color)
(add-hook 'after-init-hook #'my|reset-non-gui-bg-color)

;; Use an Emacs compatible pager
(setenv "PAGER" "/usr/bin/cat")

;; Highlight matching delimiters
(require 'paren)
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)
(show-paren-mode +1)

;; Filter ANSI escape codes in compilation-mode output
(autoload 'ansi-color-apply-on-region "ansi-color" nil t)
(require 'compile)
(add-hook 'compilation-filter-hook
          #'(lambda ()
              (let ((inhibit-read-only t))
                (ansi-color-apply-on-region compilation-filter-start (point)))))

;; Text scaling
(defadvice text-scale-increase (around all-buffers (arg) activate)
  "Text scale across all buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer ad-do-it)))

;; Visual line wrapping
(diminish 'visual-line-mode)
(add-hooks-pair '(text-mode prog-mode)
                'visual-line-mode)

;; Visual mode for browser
(add-hooks-pair 'eww-mode 'buffer-face-mode)

;; Documenation
(diminish 'eldoc-mode)

;;;
;; Packages

;; Align visually wrapped lines
;; NOTE: This can cause performance issues with font-lock.
(use-package adaptive-wrap
  :commands adaptive-wrap-prefix-mode)

;; Pretty icons
(use-package all-the-icons)

(use-package all-the-icons-dired
  :commands all-the-icons-dired-mode
  :init (add-graphic-hook (add-hooks-pair 'dired-mode #'all-the-icons-dired-mode)))

;; Centered window mode
(use-package centered-window-mode
  :diminish centered-window-mode
  :commands centered-window-mode
  :init (add-hooks-pair '(text-mode
                          prog-mode
                          help-mode helpful-mode) 'centered-window-mode)
  :config (setq cwm-centered-window-width 120))

;; Highlight source code identifiers based on their name
(defun color-identifiers-toggle ()
  "Toggle identifier colorization."
  (interactive)
  (if (or (bound-and-true-p color-identifiers-mode) (bound-and-true-p rainbow-identifiers-mode))
      (progn
        (color-identifiers-mode -1)
        (rainbow-identifiers-mode -1))
    (rainbow-identifiers-mode +1)))

(use-package color-identifiers-mode
  :diminish color-identifiers-mode
  :commands (color-identifiers-mode global-color-identifiers-mode)
  :init
  (global-color-identifiers-mode +1))
(use-package rainbow-identifiers
  :diminish rainbow-identifiers-mode
  :commands rainbow-identifiers-mode
  :functions rainbow-identifiers-cie-l*a*b*-choose-face
  :config
  (setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face
        rainbow-identifiers-cie-l*a*b*-saturation 65
        rainbow-identifiers-cie-l*a*b*-lightness 45))
(use-package symbol-overlay
  :diminish symbol-overlay-mode
  :commands
  (symbol-overlay-mode
   symbol-overlay-put
   symbol-overlay-switch-backward symbol-overlay-switch-forward)
  :init
  (add-hooks-pair 'prog-mode 'symbol-overlay-mode))

;; Dynamically change the default text scale
(use-package default-text-scale
  :commands (default-text-scale-increase default-text-scale-decrease))

(use-package eldoc-overlay-mode
  :diminish eldoc-overlay-mode
  :commands eldoc-overlay-mode)

;; Highlight TODO inside comments and strings
(use-package hl-todo
  :commands hl-todo-mode
  :init
  (add-hooks-pair 'prog-mode 'hl-todo-mode))

;; Clickable links (builtin)
(use-package goto-addr
  :init
  (add-hooks-pair 'text-mode 'goto-address-mode)
  (add-hooks-pair 'prog-mode 'goto-address-prog-mode))

;; A better *help* buffer
(use-package helpful
  :commands (helpful-at-point
             helpful-callable helpful-command
             helpful-function helpful-key helpful-macro
             helpful-symbol helpful-variable))

;; Code folding (builtin)
(use-package hideshow
  :diminish hs-minor-mode
  :commands hs-minor-mode
  :init
  (add-hooks-pair 'prog-mode 'hs-minor-mode)
  :config
  (setq hs-hide-comments-when-hiding-all nil
        ;; Nicer code-folding overlays
        hs-set-up-overlay
        (lambda (ov)
          (when (eq 'code (overlay-get ov 'hs))
            (overlay-put
             ov 'display (propertize " … " 'face 'my-folded-face))))))

;; For modes that don't adequately highlight numbers
(use-package highlight-numbers :commands highlight-numbers-mode)

;; Line highlighting (builtin)
(use-package hl-line
  :commands hl-line-mode
  :init
  (add-hooks-pair '(prog-mode text-mode conf-mode) 'hl-line-mode)
  :config
  ;; Only highlight in selected window
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil))

;; Indentation guides
(use-package indent-guide
  :commands indent-guide-mode
  :diminish indent-guide-mode
  :init
  (add-hooks-pair 'prog-mode 'indent-guide-mode)
  :config
  (setq indent-guide-delay 0.2
        indent-guide-char "\x2502"))

;; Flash the line around cursor on large movements
(use-package nav-flash
  :preface
  (eval-when-compile
    (declare-function windmove-do-window-select "windmove")
    (declare-function evil-window-top "evil")
    (declare-function evil-window-middle "evil")
    (declare-function evil-window-bottom "evil"))

  (defun my|blink-cursor (&rest _)
    "Blink current line using `nav-flash'."
    (interactive)
    (unless (minibufferp)
      (nav-flash-show)
      ;; only show in the current window
      (overlay-put compilation-highlight-overlay 'window (selected-window))))

  :commands nav-flash-show
  :init
  (advice-add #'recenter :around #'my|blink-cursor)

  (with-eval-after-load "windmove"
    (advice-add #'windmove-do-window-select :around #'my|blink-cursor))

  (with-eval-after-load "evil"
    (advice-add #'evil-window-top    :after #'my|blink-cursor)
    (advice-add #'evil-window-middle :after #'my|blink-cursor)
    (advice-add #'evil-window-bottom :after #'my|blink-cursor)))

;; Display page breaks as a horizontal line
(use-package page-break-lines
  :commands (page-break-lines-mode global-page-break-lines-mode)
  :diminish (page-break-lines-mode)
  :init (global-page-break-lines-mode +1))

;; Visually separate delimiter pairs
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (add-hooks-pair 'lisp-mode 'rainbow-delimiters-mode)
  :config (setq rainbow-delimiters-max-face-count 3))

(provide 'base-ui)
;;; base-ui.el ends here
