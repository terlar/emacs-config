;;; base-ui.el --- UI configuration
;;; Commentary:
;;; The behavior of things.
;;; Code:
(setq-default
 ;; Disable bidirectional text for tiny performance boost
 bidi-display-reordering nil
 ;; Disable non selected window highlight
 cursor-in-non-selected-windows nil
 highlight-nonselected-windows nil
 blink-matching-paren nil
 frame-inhibit-implied-resize t
 indicate-buffer-boundaries nil
 indicate-empty-lines nil
 jit-lock-defer-time nil
 jit-lock-stealth-nice 0.1
 jit-lock-stealth-time 0.2
 jit-lock-stealth-verbose nil
 max-mini-window-height 0.3
 mode-line-default-help-echo nil ; Disable mode line mouseovers
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
 ;; `pos-tip' defaults
 pos-tip-internal-border-width 6
 pos-tip-border-width 1
 ;; No blinking or beeping
 ring-bell-function #'ignore
 visible-bell nil)

;; y/n instead of yes/no
(setq-default confirm-kill-emacs 'y-or-n-p)
(fset #'yes-or-no-p #'y-or-n-p)

;; Show typed keystrokes in Minibuffer, while not inside isearch
(setq echo-keystrokes 0.02)
(add-hook 'isearch-mode-hook #'(lambda() (setq echo-keystrokes 0)))
(add-hook 'isearch-mode-end-hook #'(lambda() (setq echo-keystrokes 0.02)))

;; Undo/redo changes to window layout
(defvar winner-dont-bind-my-keys t)
(require 'winner)
(add-hook 'window-setup-hook #'winner-mode)

;; Highlight matching delimiters
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)
(show-paren-mode +1)

;; Text Scale
(defadvice text-scale-increase (around all-buffers (arg) activate)
  "Text scale across all buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer ad-do-it)))

;;;
;; Setup

;; Disable toolbar & menubar
(tooltip-mode -1) ; Tooltips in echo area
(menu-bar-mode -1)

(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (push (cons 'left-fringe  '4) default-frame-alist)
  (push (cons 'right-fringe '4) default-frame-alist)
  ;; No fringe in minibuffer
  (dolist (hook '(emacs-startup-hook minibuffer-setup-hook))
    (add-hook hook #'(lambda()
                       (set-window-fringes (minibuffer-window) 0 0 nil)))))

;;;
;; Plugins

;; Pretty icons
(use-package all-the-icons :demand t
  :when (display-graphic-p))

;; Dynamically change the default text scale
(use-package default-text-scale
  :commands (default-text-scale-increase default-text-scale-decrease))

;; Code folding (builtin)
(use-package hideshow
  :commands (hs-minor-mode hs-toggle-hiding hs-already-hidden-p)
  :config
  (setq hs-hide-comments-when-hiding-all nil))

;; Indentation guides
(use-package highlight-indentation
  :commands (highlight-indentation-mode highlight-indentation-current-column-mode)
  :config
  (defun my|inject-trailing-whitespace (&optional start end)
    "The opposite of `delete-trailing-whitespace'. Injects whitespace into
buffer so that `highlight-indentation-mode' will display uninterrupted indent
markers. This whitespace is stripped out on save, as not to affect the resulting
file."
    (interactive (progn (barf-if-buffer-read-only)
                        (if (use-region-p)
                            (list (region-beginning) (region-end))
                          (list nil nil))))
    (unless indent-tabs-mode
      (save-match-data
        (save-excursion
          (let ((end-marker (copy-marker (or end (point-max))))
                (start (or start (point-min))))
            (goto-char start)
            (while (and (re-search-forward "^$" end-marker t) (< (point) end-marker))
              (let (line-start line-end next-start next-end)
                (save-excursion
                  ;; Check previous line indent
                  (forward-line -1)
                  (setq line-start (point)
                        line-end (save-excursion (back-to-indentation) (point)))
                  ;; Check next line indent
                  (forward-line 2)
                  (setq next-start (point)
                        next-end (save-excursion (back-to-indentation) (point)))
                  ;; Back to origin
                  (forward-line -1)
                  ;; Adjust indent
                  (let* ((line-indent (- line-end line-start))
                         (next-indent (- next-end next-start))
                         (indent (min line-indent next-indent)))
                    (insert (make-string (if (zerop indent) 0 (1+ indent)) ? )))))
              (forward-line 1)))))
      (set-buffer-modified-p nil))
    nil)

  (dolist (hook '(highlight-indentation-mode-hook highlight-indentation-current-column-mode-hook))
    (add-hook hook #'(lambda()
                       (if (or highlight-indentation-mode highlight-indentation-current-column-mode)
                           (progn
                             (my|inject-trailing-whitespace)
                             (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
                             (add-hook 'after-save-hook #'my|inject-trailing-whitespace nil t))
                         (remove-hook 'before-save-hook #'delete-trailing-whitespace t)
                         (remove-hook 'after-save-hook #'my|inject-trailing-whitespace t)
                         (delete-trailing-whitespace))))))

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
(use-package linum
  :commands linum-mode
  :preface (defvar linum-format "%4d ")
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'(lambda()
                       (unless (eq major-mode 'org-mode)
                         (linum-mode +1)))))
  :config
  ;; Highlight current line number
  (use-package hlinum :demand t
    :config
    (hlinum-activate)))

;; Tree navigation
(use-package neotree
  :commands
  (neotree-show
   neotree-hide
   neotree-toggle
   neotree-dir
   neotree-find
   neo-global--with-buffer
   neo-global--window-exists-p)
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
   neo-theme 'icons
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

  (push neo-buffer-name winner-boring-buffers))

;; Display page breaks as a horizontal line
(use-package page-break-lines :demand t
  :diminish (page-break-lines-mode)
  :config (global-page-break-lines-mode +1))

;; Visually separate delimiter pairs
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  :config (setq rainbow-delimiters-max-face-count 3))

;; Wrap lines at fill-column and center buffer
(use-package visual-fill-column
  :commands visual-fill-column-mode
  :config
  (setq-default visual-fill-column-center-text t
                visual-fill-column-width 120))

;;;
;; Modeline
(use-package mode-icons :demand t
  :when (display-graphic-p)
  :config
  (setq mode-icons-desaturate-active t)
  (mode-icons-mode +1))

(provide 'base-ui)
;;; base-ui.el ends here
