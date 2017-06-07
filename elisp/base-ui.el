;;; base-ui.el --- UI configuration

;;; Commentary:
;; The behavior of things.

;;; Code:
(require 'ansi-color)
(require 'compile)
(require 'paren)
(require 'winner)

;; All-the-icons doesn't work in the terminal
(unless (or (display-graphic-p) (daemonp))
  (defalias 'all-the-icons-octicon    #'ignore)
  (defalias 'all-the-icons-faicon     #'ignore)
  (defalias 'all-the-icons-fileicon   #'ignore)
  (defalias 'all-the-icons-wicon      #'ignore)
  (defalias 'all-the-icons-alltheicon #'ignore))

;;;
;; Settings
(defvar my-fringe-width 12
  "The fringe width to use.")

(defvar my-completion-system 'ivy
  "The completion system to use.")

(setq-default
 ;; Disable bidirectional text for tiny performance boost
 bidi-display-reordering nil
 ;; Disable non selected window highlight
 cursor-in-non-selected-windows nil
 highlight-nonselected-windows nil
 blink-matching-paren nil
 frame-inhibit-implied-resize t
 jit-lock-chunk-size 10000
 jit-lock-defer-time 0.05
 jit-lock-stealth-nice 0.1
 jit-lock-stealth-time 0.2
 jit-lock-stealth-load nil
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

;; Show typed keystrokes in Minibuffer, while not inside isearch
(setq echo-keystrokes 0.02)
(add-hook 'isearch-mode-hook #'(lambda() (setq echo-keystrokes 0)))
(add-hook 'isearch-mode-end-hook #'(lambda() (setq echo-keystrokes 0.02)))

;; Undo/redo changes to window layout
(defvar winner-dont-bind-my-keys t)
(add-hook 'window-setup-hook #'winner-mode)

;; Use Emacs compatible pager
(setenv "PAGER" "/usr/bin/cat")

;; Highlight matching delimiters
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)
(show-paren-mode +1)

;; Filter ANSI escape codes in compilation-mode output
(add-hook 'compilation-filter-hook
          #'(lambda ()
              (let ((inhibit-read-only t))
                (ansi-color-apply-on-region compilation-filter-start (point)))))

;; Text scaling
(defadvice text-scale-increase (around all-buffers (arg) activate)
  "Text scale across all buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer ad-do-it)))

;; Word wrapping
(diminish 'visual-line-mode)
(dolist (hook '(text-mode-hook prog-mode-hook))
  (add-hook hook #'visual-line-mode))

;;;
;; Setup

;; Disable toolbar & menubar
(tooltip-mode -1) ; Tooltips in echo area
(menu-bar-mode -1)

(when (or (display-graphic-p) (daemonp))
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  ;; Standardize fringe width
  (push (cons 'left-fringe  my-fringe-width) default-frame-alist)
  (push (cons 'right-fringe my-fringe-width) default-frame-alist)
  ;; No fringe in minibuffer
  (dolist (hook '(emacs-startup-hook minibuffer-setup-hook))
    (add-hook hook
              #'(lambda()
                  (set-window-fringes (minibuffer-window) 0 0 nil)))))

;; Visual mode for browser
(add-hook 'eww-mode-hook #'buffer-face-mode)

;;;
;; Plugins

;; Align wrapped lines
(use-package adaptive-wrap
  :commands adaptive-wrap-prefix-mode
  :init
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

;; Pretty icons
(use-package all-the-icons
  :when (display-graphic-p)
  :commands (all-the-icons-faicon all-the-icons-faicon-family))

;; Highlight source code identifiers based on their name
(use-package color-identifiers-mode
  :init
  (add-hook 'after-init-hook #'global-color-identifiers-mode))

;; Dynamically change the default text scale
(use-package default-text-scale
  :commands (default-text-scale-increase default-text-scale-decrease))

;; Highlight TODO inside comments and strings
(use-package hl-todo
  :commands hl-todo-mode
  :init
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("FIXME" . ,(face-foreground 'error))
          ("NOTE"  . ,(face-foreground 'success)))))

;; Clickable links (builtin)
(use-package goto-addr
  :init
  (add-hook 'text-mode-hook #'goto-address-mode)
  (add-hook 'prog-mode-hook #'goto-address-prog-mode))

;; Code folding (builtin)
(use-package hideshow
  :commands (hs-minor-mode hs-toggle-hiding hs-already-hidden-p)
  :init
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  :config
  (setq hs-hide-comments-when-hiding-all nil)

  ;; Nicer code-folding overlays (with fringe indicators)
  (setq hs-set-up-overlay
        (lambda (ov)
          (when (eq 'code (overlay-get ov 'hs))
            (overlay-put
             ov 'display (propertize " … " 'face 'my-folded-face))))))

;; Indentation guides
(use-package highlight-indent-guides
  :commands highlight-indent-guides-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; For modes that don't adequately highlight numbers
(use-package highlight-numbers :commands highlight-numbers-mode)

;; Line highlighting (builtin)
(use-package hl-line
  :init
  (dolist (hook '(linum-mode-hook nlinum-mode-hook))
    (add-hook hook #'hl-line-mode))
  :config
  ;; Only highlight in selected window
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil))

;; Line numbers
(use-package nlinum :ensure nil
  :load-path "vendor/nlinum/"
  :commands nlinum-mode
  :preface (defvar nlinum-format "%4d ")
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'(lambda()
                       (unless (eq major-mode 'org-mode)
                         (nlinum-mode +1)))))
  :config
  (custom-set-variables
   '(nlinum-highlight-current-line t)))

;; Flash the line around cursor on large movements
(use-package nav-flash
  :commands nav-flash-show
  :preface
  (defun my-blink-cursor (&rest _)
    "Blink current line using `nav-flash'."
    (interactive)
    (unless (minibufferp)
      (nav-flash-show)))
  :init
  (add-hook 'focus-in-hook #'my-blink-cursor)
  (advice-add #'windmove-do-window-select :around #'my-blink-cursor)
  (advice-add #'recenter :around #'my-blink-cursor)

  (with-eval-after-load 'evil
    (advice-add #'evil-window-top    :after #'my-blink-cursor)
    (advice-add #'evil-window-middle :after #'my-blink-cursor)
    (advice-add #'evil-window-bottom :after #'my-blink-cursor)))

;; Tree navigation
(use-package neotree
  :commands (neotree-show
             neotree-hide
             neotree-toggle
             neotree-dir
             neotree-find
             neo-global--select-window
             neo-global--with-buffer
             neo-global--window-exists-p)
  :functions (off-p)
  :preface
  (defun switch-to-neotree ()
    "Switch to NeoTree window."
    (interactive)
    (if (neo-global--window-exists-p)
        (neo-global--select-window)
      (neotree-project-dir)))

  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
  :config
  (setq
   neo-create-file-auto-open nil
   neo-auto-indent-point nil
   neo-autorefresh nil
   neo-mode-line-type 'none
   ;; Allow temporary resizing of drawer
   neo-window-fixed-size nil
   ;; Always fallback to a fixed size
   neo-window-width 30
   neo-show-updir-line nil
   neo-theme (if (display-graphic-p) 'icons 'arrow)
   neo-banner-message nil
   neo-confirm-create-file #'off-p
   neo-confirm-create-directory #'off-p
   neo-show-hidden-files nil
   neo-hidden-regexp-list
   '(;; vcs folders
     "^\\.\\(git\\|hg\\|svn\\)$"
     ;; compiled files
     "\\.\\(pyc\\|o\\|elc\\|lock\\|css.map\\)$"
     ;; generated files, caches or local pkgs
     "^\\(node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
     ;; org-mode folders
     "^\\.\\(sync\\|export\\|attach\\)$"
     "~$"
     "^#.*#$"))

  (with-eval-after-load 'projectile
    (custom-set-variables
     '(projectile-switch-project-action #'neotree-projectile-action)))

  (push neo-buffer-name winner-boring-buffers))

;; Display page breaks as a horizontal line
(use-package page-break-lines :demand t
  :diminish (page-break-lines-mode)
  :config (global-page-break-lines-mode -1))

;; Visually separate delimiter pairs
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  :config (setq rainbow-delimiters-max-face-count 3))

;; Smooth scrolling and centered mode
(use-package sublimity
  :commands sublimity-mode
  :init
  (add-hook 'after-init-hook #'sublimity-mode)
  (dolist (hook '(text-mode-hook prog-mode-hook help-mode-hook))
    (add-hook hook
              #'(lambda ()
                  (unless (minibufferp)
                    ;; Enable centered buffer mode
                    (set (make-local-variable 'sublimity-attractive-centering-width) 120)))))
  :config
  ;; This cannot be set with setq as it is defined to only allow integer values.
  (defvar sublimity-attractive-centering-width nil)
  (require 'sublimity-scroll)
  (require 'sublimity-attractive))

(provide 'base-ui)
;;; base-ui.el ends here
