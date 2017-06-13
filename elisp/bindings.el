;;; bindings.el --- Key binding configuration

;;; Commentary:
;; How to interact with Emacs.

;;; Code:
(require 'bind-key)
(require 'evil)

(autoload 'my|neotree-toggle "tool-neotree" nil t)
(autoload 'my|neotree-window "tool-neotree" nil t)
(autoload 'rotate-text "rotate-text" nil t)
(autoload 'rotate-text-backward "rotate-text" nil t)

;; base-theme.el vars
(defvar my-default-font-height nil)

;; Some bindings require simulating another key press
(defun simulate-key-press (key)
  "Pretend that KEY was pressed.
KEY must be given in `kbd' notation."
  `(lambda () (interactive)
     (setq prefix-arg current-prefix-arg)
     (setq unread-command-events (listify-key-sequence (read-kbd-macro ,key)))))

(use-package which-key :demand t
  :config
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 5
        which-key-idle-delay 0.5)

  (push '(("<\\([[:alnum:]-]+\\)>" . nil) . ("\\1" . nil)) which-key-replacement-alist)
  (push '(("\\`\\?\\?\\'" . nil) . ("λ" . nil)) which-key-replacement-alist)
  (push '(("<up>"    . nil) . ("↑" . nil)) which-key-replacement-alist)
  (push '(("<right>" . nil) . ("→" . nil)) which-key-replacement-alist)
  (push '(("<down>"  . nil) . ("↓" . nil)) which-key-replacement-alist)
  (push '(("<left>"  . nil) . ("←" . nil)) which-key-replacement-alist)
  (push '(("SPC" . nil) . ("␣" . nil)) which-key-replacement-alist)
  (push '(("TAB" . nil) . ("↹" . nil)) which-key-replacement-alist)
  (push '(("RET" . nil) . ("⏎" . nil)) which-key-replacement-alist)
  (push '(("DEL" . nil) . ("⌫" . nil)) which-key-replacement-alist)
  (push '(("deletechar" . nil) . ("⌦" . nil)) which-key-replacement-alist)

  (which-key-add-key-based-replacements
    "C-c !" "flycheck"
    "C-c =" "diff"
    "C-c @" "outline"
    "C-c a" "apps"
    "C-c g" "git"
    "C-c p" "projects"
    "C-c w" "windows"
    "C-c W" "workspaces"
    "C-c ~" "toggles")

  ;; Embolden local bindings
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (which-key-mode +1))

;; Help
(add-hook 'help-mode-hook
          #'(lambda ()
              (bind-keys :map evil-normal-state-local-map
                         ("[[" . help-go-back)
                         ("]]" . help-go-forward)
                         ("o"  . ace-link-help)
                         ("q"  . quit-window))))

;; Escape acts as an extra C-g
(bind-key "<escape>" 'keyboard-escape-quit)

;; Text Scaling
(defun default-text-scale-reset ()
  "Reset the height of the default face to `my-default-font-height'."
  (interactive)
  (set-face-attribute 'default nil :height my-default-font-height))

(bind-key "C-=" #'default-text-scale-reset)
(bind-key "C--" #'default-text-scale-decrease)
(bind-key "C-+" #'default-text-scale-increase)
(dolist (key '("<C-mouse-4>"
               "<left-margin> <C-mouse-4>" "<right-margin> <C-mouse-4>"
               "<left-fringe> <C-mouse-4>" "<right-fringe> <C-mouse-4>"))
  (bind-key key #'text-scale-decrease))
(dolist (key '("<C-mouse-5>"
               "<left-margin> <C-mouse-5>" "<right-margin> <C-mouse-5>"
               "<left-fringe> <C-mouse-5>" "<right-fringe> <C-mouse-5>"))
  (bind-key key #'text-scale-increase))

;; Neotree
(bind-keys ("<left-margin> <mouse-1>" . my|neotree-toggle)
           ("<left-fringe> <mouse-1>" . my|neotree-toggle))

(with-eval-after-load 'neotree
  (add-hook 'neotree-mode-hook
            #'(lambda ()
                (bind-keys :map evil-normal-state-local-map
                           ([tab]       . neotree-quick-look)
                           ("RET"       . neotree-enter)
                           ([backspace] . evil-window-prev)
                           ("j"         . neotree-next-line)
                           ("k"         . neotree-previous-line)
                           ("n"         . neotree-next-line)
                           ("p"         . neotree-previous-line)
                           ("J"         . neotree-select-next-sibling-node)
                           ("K"         . neotree-select-previous-sibling-node)
                           ("H"         . neotree-select-up-node)
                           ("L"         . neotree-select-down-node)
                           ("q"         . neotree-hide)
                           ("c"         . neotree-create-node)
                           ("d"         . neotree-delete-node)
                           ("r"         . neotree-rename-node)
                           ("R"         . neotree-refresh)))))

;; Smarter abbrev completion
(bind-key [remap dabbrev-expand] #'hippie-expand)

;; Consistent jumping

(bind-key [remap evil-goto-definition] #'dumb-jump-go)
(bind-key [remap evil-jump-to-tag] #'projectile-find-tag)
(bind-key [remap find-tag] #'projectile-find-tag)

;; Ediff
;; Setting up the mappings through the bind command will leave them behind,
;; breaking all further modes. Setup with a hook instead.
(add-hook 'ediff-keymap-setup-hook
          #'(lambda ()
              (bind-keys :map ediff-mode-map
                         ("d" . ediff-copy-both-to-C)
                         ("j" . ediff-next-difference)
                         ("k" . ediff-previous-difference))))

(bind-keys :prefix-map diff-map
           :prefix "C-c ="
           ("b" . ediff-buffers)
           ("B" . ediff-buffers3)
           ("c" . compare-windows)
           ("=" . ediff-files)
           ("f" . ediff-files)
           ("F" . ediff-files3)
           ("r" . ediff-revision)
           ("p" . ediff-patch-file)
           ("P" . ediff-patch-buffer)
           ("l" . ediff-regions-linewise)
           ("w" . ediff-regions-wordwise))

;; Open alternate file
(bind-key "<C-tab>" #'ff-find-related-file)

(bind-key "C-c q" #'fill-region)
(bind-key "C-c ;" #'comment-or-uncomment-region)

(bind-keys :prefix-map app-map
           :prefix "C-c a"
           ("c" . calendar)
           ("C" . display-time-world)
           ("t" . shell)
           ("T" . ansi-term)
           ("w" . eww))

;; C-c ~ (Toggle)
(bind-keys :prefix-map toggle-map
           :prefix "C-c ~"
           ("a" . goto-address-mode)                         ; Clickable links
           ("c" . rainbow-mode)                              ; Color display
           ("d" . toggle-debug-on-error)                     ; Debug on error
           ("f" . hs-minor-mode)                             ; Code folding
           ("F" . flycheck-mode)                             ; Syntax checker
           ("h" . hl-line-mode)                              ; Line highlight
           ("i" . highlight-indentation-mode)                ; Indent guides
           ("I" . highlight-indentation-current-column-mode) ; Indent guides (column)
           ("l" . nlinum-mode)                               ; Line numbers
           ("r" . ruler-mode)                                ; Ruler
           ("s" . flyspell-mode)                             ; Spell-checking
           ("v" . variable-pitch-mode)                       ; Fixed-width/variable-width font
           ("w" . whitespace-mode)                           ; Display white-space characters
           ("W" . auto-fill-mode)                            ; Automatic line-wrapping
           ("z" . sublimity-mode))                           ; Zoomed/distraction free mode

;; C-c g (Git)
(bind-keys :prefix-map git-map
           :prefix "C-c g"
           ("b" . magit-blame)                 ; Git blame
           ("B" . vcs/git-browse)              ; Git browse
           ("c" . magit-clone)                 ; Git clone
           ("I" . vcs/git-browse-issues)       ; Git browse issues
           ("l" . magit-log-buffer-file)       ; Git log
           ("m" . git-messenger:popup-message) ; Git popup message
           ("p" . magit-pull)                  ; Git pull
           ("s" . magit-status)                ; Git status
           ("t" . git-timemachine-toggle))     ; Git time machine

(bind-key "C-c >" #'rotate-text)
(bind-key "C-c <" #'rotate-text-backward)

;; Normal state
(bind-keys :map evil-normal-state-map
           ("]b" . next-buffer)
           ("[b" . previous-buffer)
           ("]w" . persp-next)
           ("[w" . persp-prev))

;; Normal/visual state
(dolist (map
         '(evil-normal-state-map
           evil-visual-state-map))
  ;; Space key in normal/visual mode acts as C-c
  (bind-key "SPC" (simulate-key-press "C-c") (eval map))
  ;; Tab keys in normal/visual mode navigates buffers
  (bind-keys :map (eval map)
             ([tab]     . next-buffer)
             ("TAB"     . next-buffer)
             ([backtab] . previous-buffer)))

;; Window keys (prefix C-w or C-c w)
(bind-key "C-c w" (simulate-key-press "C-w"))
(bind-keys :map evil-window-map
           ;; Navigation
           ("C-h"       . evil-window-left)
           ("C-j"       . evil-window-down)
           ("C-k"       . evil-window-up)
           ("C-l"       . evil-window-right)
           ("C-w"       . ace-window)
           ("B"         . switch-to-minibuffer)
           ([tab]       . my|neotree-window)
           ("TAB"       . my|neotree-window)
           ("<C-tab>"   . my|neotree-window)
           ([backtab]   . my|neotree-toggle)
           ;; Swapping windows
           ("C-S-w" . ace-swap-window)
           ("z"     . zoom-window-zoom)
           ;; Undo/redo
           ("u"   . winner-undo)
           ("C-u" . winner-undo)
           ("C-r" . winner-redo)
           ;; Delete
           ("C-C" . ace-delete-window))

;; Flycheck
(with-eval-after-load 'flycheck
  (bind-keys
   :map evil-motion-state-map
   ("]e" . next-error)
   ("[e" . previous-error)
   :map flycheck-error-list-mode-map
   ("C-n" . flycheck-error-list-next-error)
   ("C-p" . flycheck-error-list-previous-error)
   ("j"   . flycheck-error-list-next-error)
   ("k"   . flycheck-error-list-previous-error)
   ("RET" . flycheck-error-list-goto-error)))

;; Jumping
(bind-keys
 ("M-g o" . dumb-jump-go-other-window)
 ("M-g j" . dumb-jump-go)
 ("M-g i" . dumb-jump-go-prompt)
 ("M-g x" . dumb-jump-go-prefer-external)
 ("M-g z" . dumb-jump-go-prefer-external-other-window))

(provide 'bindings)
;;; bindings.el ends here
