;;; bindings.el --- My key bindings -*- lexical-binding: t; -*-

;;; Commentary:
;; My key binding setup.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-keybinds))

(eval `(general-define-key :keymaps '(normal visual)
                           ,my-normal-leader-key (general-simulate-keys ,my-leader-key nil nil t)))

;;;
;; Remaps

;; Smarter abbrev completion
(general-define-key [remap dabbrev-expand] 'hippie-expand)

;; Consistent jumping
(general-define-key [remap evil-goto-definition] 'dumb-jump-go)
(general-define-key [remap evil-jump-to-tag] 'projectile-find-tag)
(general-define-key [remap find-tag] 'projectile-find-tag)

;;;
;; Global

;; Emacs debug utilities
(general-define-key "M-;" 'eval-expression)

;; Text-scaling
(general-define-key
 "C-=" 'default-text-scale-reset
 "C--" 'default-text-scale-decrease
 "C-+" 'default-text-scale-increase
 "M-=" '(lambda () (interactive) (text-scale-set 0))
 "M--" 'text-scale-decrease
 "M-+" 'text-scale-increase
 "<C-mouse-4>" 'text-scale-decrease
 "<C-mouse-5>" 'text-scale-increase)

;; Screen refresh
(general-define-key "C-l" 'refresh)

;; Leader
(general-define-key
 :prefix my-leader-key
 "SPC" (general-predicate-dispatch #'projectile-switch-project
         :docstring "Find file in project or switch project"
         (projectile-project-p) #'projectile-find-file)
 ","   '(switch-to-buffer                       :wk "Switch to buffer")
 "."   '(find-file                              :wk "Browse files")
 "RET" '(counsel-bookmark                       :wk "Jump to bookmark")
 "v"   '(:keymap
         symbol-overlay-map
         :package symbol-overlay :wk "overlays")
 "w"   '(:keymap
         evil-window-map
         :package evil :wk "window")
 "x"   '(scratch-buffer                         :wk "Pop up scratch buffer")

 "["   '(:ignore t :wk "previous...")
 "[ b" '(previous-buffer                        :wk "Buffer")
 "[ d" '(diff-hl-previous-hunk                  :wk "Diff Hunk")
 "[ t" '(hl-todo-previous                       :wk "Todo")
 "[ e" '(previous-error                         :wk "Error")
 "[ w" '(persp-prev                             :wk "Workspace")
 "[ h" '(smart-backward                         :wk "Smart jump")
 "[ s" '(evil-prev-flyspell-error               :wk "Spelling error")
 "[ S" '(flyspell-correct-previous-word-generic :wk "Spelling correction")

 "]"   '(:ignore t :wk "next...")
 "] b" '(next-buffer                            :wk "Buffer")
 "] d" '(diff-hl-next-hunk                      :wk "Diff Hunk")
 "] t" '(hl-todo-next                           :wk "Todo")
 "] e" '(next-error                             :wk "Error")
 "] w" '(persp-next                             :wk "Workspace")
 "] h" '(smart-forward                          :wk "Smart jump")
 "] s" '(evil-next-flyspell-error               :wk "Spelling error")
 "] S" '(flyspell-correct-word-generic          :wk "Spelling correction")

 "/"   '(:ignore t :wk "search")
 "/ /" '(swiper                :wk "Swiper")
 "/ i" '(imenu                 :wk "Imenu")
 "/ I" '(imenu-anywhere        :wk "Imenu across buffers")
 "/ g" '(counsel-rg            :wk "Grep files")
 "/ G" '(counsel-projectile-rg :wk "Grep project files")

 "TAB" '(:ignore t :wk "workspace")
 "TAB ." '(persp-switch :wk "Switch workspace")

 "b"   '(:ignore t :wk "buffer")
 "b f" '(list-flycheck-errors :wk "Pop up flycheck errors")
 "b n" '(evil-buffer-new      :wk "New empty buffer")
 "b b" '(switch-to-buffer     :wk "Switch buffer")
 "b k" '(kill-buffer          :wk "Kill buffer")
 "b o" '(kill-other-buffers   :wk "Kill other buffers")
 "b s" '(save-buffer          :wk "Save buffer")
 "b x" '(scratch-buffer       :wk "Pop up scratch buffer")
 "b z" '(bury-buffer          :wk "Bury buffer")
 "b ]" '(next-buffer          :wk "Next buffer")
 "b [" '(previous-buffer      :wk "Previous buffer")
 "b S" '(sudo-edit            :wk "Sudo edit this file")

 "c"   '(:ignore t :wk "code")
 "c c" '(editorconfig-apply   :wk "Apply editorconfig")
 "c x" '(flycheck-list-errors :wk "List errors")
 "c e" '(eval-buffer          :wk "Evaluate buffer")
 "c d" '(evil-goto-definition :wk "Jump to definition")
 "c p" '(source-peek          :wk "Peek definition")
 "c r" '(eval|repl            :wk "Open REPL")

 "f"   '(:ignore t :wk "file")
 "f ." '(find-file                              :wk "Find file")
 "f /" '(projectile-find-file                   :wk "Find file in project")
 "f ?" '(counsel-file-jump                      :wk "Find file here")
 "f a" '(projectile-find-other-file             :wk "Find other file")
 "f c" '(editorconfig-find-current-editorconfig :wk "Open project editorconfig")
 "f g" '(counsel-rg                             :wk "Grep files")
 "f G" '(counsel-projectile-rg                  :wk "Grep project files")
 "f r" '(recentf                                :wk "Recent files")
 "f R" '(projectile-recentf                     :wk "Recent project files")

 "g"   '(:ignore t :wk "git")
 "g s" '(magit-status                :wk "Status")
 "g l" '(magit-log-buffer-file       :wk "Log")
 "g b" '(magit-blame                 :wk "Blame")
 "g t" '(git-timemachine-toggle      :wk "Time machine")
 "g r" '(diff-hl-revert-hunk         :wk "Revert hunk")
 "g ]" '(diff-hl-next-hunk           :wk "Next hunk")
 "g [" '(diff-hl-previous-hunk       :wk "Previous hunk")
 "g p" '(magit-pull                  :wk "Pull")
 "g c" '(magit-clone                 :wk "Clone")
 "g B" '(vcs/git-browse              :wk "Browse")
 "g I" '(vcs/git-browse-issues       :wk "Browse issues")
 "g m" '(git-messenger:popup-message :wk "Popup message")

 "h" '(:ignore t :wk "help")
 "h h" '(:keymap help-map)
 "h a" '(apropos              :wk "Apropos")
 "h l" '(find-library         :wk "Find library")
 "h f" '(describe-function    :wk "Describe function")
 "h k" '(helpful-key          :wk "Describe key")
 "h c" '(helpful-command      :wk "Describe command")
 "h C" '(describe-char        :wk "Describe char")
 "h m" '(helpful-macro        :wk "Describe macro")
 "h M" '(describe-mode        :wk "Describe mode")
 "h v" '(describe-variable    :wk "Describe variable")
 "h f" '(helpful-function     :wk "Describe function")
 "h F" '(describe-face        :wk "Describe face")
 "h '" '(what-cursor-position :wk "What face")
 "h i" '(info                 :wk "Info")

 "o" '(:ignore t :wk "open")
 "o c" '(calendar           :wk "Calendar")
 "o C" '(display-time-world :wk "World Time")
 "o d" '(deft               :wk "Notes")
 "o n" '(neotree|toggle     :wk "NeoTree")
 "o N" '(neotree|window     :wk "NeoTree Window")
 "o t" '(eshell             :wk "Terminal")
 "o T" '(ansi-term          :wk "ANSI Terminal")
 "o w" '(eww                :wk "Browser")

 "=" '(:ignore t :wk "diff")
 "= b" '(ediff-buffers          :wk "Buffers")
 "= B" '(ediff-buffers3         :wk "Buffers (3-way)")
 "= c" '(compare-windows        :wk "Compare windows")
 "= =" '(ediff-files            :wk "Files")
 "= f" '(ediff-files            :wk "Files")
 "= F" '(ediff-files3           :wk "Files (3-way)")
 "= r" '(ediff-revision         :wk "Compare versions")
 "= p" '(ediff-patch-file       :wk "Patch file")
 "= P" '(ediff-patch-buffer     :wk "Patch buffer")
 "= l" '(ediff-regions-linewise :wk "Linewise")
 "= w" '(ediff-regions-wordwise :wk "Wordwise")

 "~" '(:ignore t :wk "toggle")
 "~ a" '(goto-address-mode           :wk "Clickable links")
 "~ c" '(color-identifiers-toggle    :wk "Colorize identifiers")
 "~ C" '(rainbow-mode                :wk "Colorize color values")
 "~ d" '(toggle-debug-on-error       :wk "Debug on error")
 "~ e" '(eldoc-overlay-mode          :wk "Eldoc inline")
 "~ f" '(hs-minor-mode               :wk "Code folding")
 "~ F" '(flycheck-mode               :wk "Syntax checker")
 "~ g" '(indent-guide-mode           :wk "Indent guides")
 "~ h" '(hl-line-mode                :wk "Line highlight")
 "~ i" '(aggressive-indent-mode      :wk "Automatic indentation")
 "~ l" '(toggle-display-line-numbers :wk "Line numbers")
 "~ L" '(coverlay-toggle-overlays    :wk "Coverage overlays")
 "~ r" '(ruler-mode                  :wk "Ruler")
 "~ R" '(my|start-spray              :wk "Speed-reading")
 "~ s" '(flyspell-mode               :wk "Spell-checking")
 "~ S" '(subword-mode                :wk "Subword")
 "~ v" '(variable-pitch-mode         :wk "Fixed-width/variable-width font")
 "~ w" '(whitespace-mode             :wk "Display white-space characters")
 "~ W" '(auto-fill-mode              :wk "Automatic line-wrapping")

 "q" '(:ignore t :wk "quit")
 "q q" '(evil-save-and-quit :wk "Quit"))

;; Normal state
(general-define-key
 :keymaps 'normal
 "]b" 'next-buffer
 "[b" 'previous-buffer
 "]e" 'next-error
 "[e" 'previous-error
 "]w" 'persp-next
 "[w" 'persp-prev
 "gp" 'evil|reselect-paste
 "gr" 'eval-region
 "gR" 'eval-buffer
 "K"  'source-peek
 "zx" 'kill-buffer)

(general-define-key
 :keymaps 'motion
 "gd" 'xref-find-definitions
 "gD" 'xref-find-references)

;; Visual state
(general-define-key
 :keymaps 'visual
 "." 'evil-repeat
 "<" 'evil|visual-outdent
 ">" 'evil|visual-indent)

(general-define-key
 :keymaps 'evil-window-map
 ;; Navigation
 "C-w"     '(ace-window           :wk "Select a window")
 "B"       '(switch-to-minibuffer :wk "Switch to minibuffer")
 "TAB"     '(neotree|window       :wk "Switch to NeoTree")
 [backtab] '(neotree|toggle       :wk "Toggle NeoTree")
 ;; Swapping
 "C-S-w"   '(ace-swap-window      :wk "Swap window")
 "z"       '(zoom-window-zoom     :wk "Zoom window")
 ;; Undo/redo
 "u"       '(winner-undo          :wk "Undo")
 "C-u"     '(winner-undo          :wk "Undo")
 "C-r"     '(winner-redo          :wk "Redo")
 ;; Delete
 "C-C"     '(ace-delete-window    :wk "Select and delete a window"))

;;;
;; Built-in plugins

;; comint
(general-define-key
 :keymaps 'comint-mode-map
 "TAB" 'company-complete)

;; conf-mode
(general-define-key
 :keymaps 'conf-mode-map
 ;; Disable conflicting key
 "C-c SPC" '(:ignore t))

;; debug
(general-define-key
 :keymaps 'debugger-mode-map
 :states 'normal
 "RET" 'debug-help-follow
 "n"   'debugger-step-through
 "c"   'debugger-continue
 "q"   'top-level)

;; ediff
(add-hook 'ediff-keymap-setup-hook
          #'(lambda ()
              (general-define-key
               :keymaps 'ediff-mode-map
               "d" '(ediff-copy-both-to-C      :wk "Copy both to C")
               "j" '(ediff-next-difference     :wk "Next difference")
               "k" '(ediff-previous-difference :wk "Previous difference"))))

;; emacs-lisp-mode
(general-define-key
 :keymaps 'emacs-lisp-mode-map
 :states 'normal
 "K" 'helpful-at-point)

;; elisp-refs-mode
(general-define-key
 :keymaps 'elisp-refs-mode-map
 :states 'normal
 "q" 'kill-this-buffer)

;; eshell
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (general-define-key
               :keymaps 'eshell-mode-map
               "C-l" 'eshell-clear-buffer
               [tab] 'company-complete-common-or-cycle
               "TAB" 'company-complete-common-or-cycle)))

;; eww
(general-define-key
 :keymaps 'eww-mode-map
 :states 'normal
 "h" 'eww-back-url
 "l" 'eww-next-url
 "q" 'quit-window)

;; help-mode
(general-define-key
 :keymaps 'help-mode-map
 :states 'normal
 "[[" 'help-go-back
 "]]" 'help-go-forward
 "o"  'ace-link-help
 "q"  'quit-window)

;; package
(general-define-key
 :keymaps 'package-menu-mode-map
 :states 'motion
 "q" 'kill-this-buffer)

;; vc-annotate
(general-define-key
 :keymaps 'vc-annotate-mode-map
 :states 'normal
 "q"   'kill-this-buffer
 "d"   'vc-annotate-show-diff-revision-at-line
 "D"   'vc-annotate-show-changeset-diff-revision-at-line
 "SPC" 'vc-annotate-show-log-revision-at-line
 "]]"  'vc-annotate-next-revision
 "[["  'vc-annotate-prev-revision
 "TAB" 'vc-annotate-toggle-annotation-visibility
 "RET" 'vc-annotate-find-revision-at-line)

;; xref
(general-define-key
 :keymaps 'xref--xref-buffer-mode-map
 :states 'normal
 "q" 'quit-window)

;;;
;; Plugins

;; company
(general-define-key
 :keymaps 'company-mode-map
 :states 'insert
 "C-SPC"   'company-indent-or-complete-common
 "C-x C-l" 'company|whole-lines
 "C-x C-k" 'company|dict-or-keywords
 "C-x C-f" 'company-files
 "C-x C-]" 'company-etags
 "C-x s"   'company-ispell
 "C-x C-s" 'company-yasnippet
 "C-x C-o" 'company-capf
 "C-x C-n" 'company-dabbrev-code
 "C-x C-p" 'company|dabbrev-code-previous)
(general-define-key
 :keymaps 'company-active-map
 ;; Don't interfere with `evil-delete-backward-word' in insert mode
 "C-w"     '(:ignore t)
 ;; Don't interfere with the return key
 [return]  '(:ignore t)
 "RET"     '(:ignore t)
 ;; Abort on escape but leave current completion
 [escape]  'company-abort

 "C-e"     'company-complete-selection
 "C-f"     'company-complete-selection
 "C-SPC"   'company-complete-common
 "TAB"     'company-complete-common-or-cycle
 [tab]     'company-complete-common-or-cycle
 [backtab] 'company-select-previous

 "C-o"     'company-search-kill-others
 "C-n"     'company-select-next
 "C-p"     'company-select-previous
 "C-h"     'company-quickhelp-manual-begin
 "C-S-h"   'company-show-doc-buffer
 "C-S-s"   'company-search-candidates
 "C-s"     'company-filter-candidates)
(general-define-key
 :keymaps 'company-search-map
 "C-n"    'company-search-repeat-forward
 "C-p"    'company-search-repeat-backward
 "C-s"    'company|search-abort-and-filter-candidates
 [escape] 'company-search-abort)

;; counsel
(general-define-key
 :keymaps 'ivy-mode-map
 "C-o" 'ivy-dispatching-done)
(general-define-key
 :keymaps 'counsel-ag-map
 [backtab] 'ivy-wgrep-occur
 "C-SPC" 'counsel-git-grep-recenter)

;; coverlay
(general-define-key
 :keymaps 'coverlay-stats-mode-map
 :states 'normal
 "q" 'quit-window)

;; diff-hl
(general-define-key
 :keymaps 'motion
 "]d" 'diff-hl-next-hunk
 "[d" 'diff-hl-previous-hunk)

;; dumb-jump
(general-define-key
 "M-g o" 'dumb-jump-go-other-window
 "M-g j" 'dumb-jump-go
 "M-g i" 'dumb-jump-go-prompt
 "M-g x" 'dumb-jump-go-prefer-external
 "M-g z" 'dumb-jump-go-prefer-external-other-window)

;; evil-commentary
(general-define-key
 :keymaps 'normal
 "gc" 'evil-commentary)

;; evil-exchange
(general-define-key
 :keymaps 'normal
 "gx" 'evil-exchange)

;; evil-magit
(general-define-key
 :keymaps '(magit-status-mode-map magit-revision-mode-map)
 :states 'normal
 "C-j" '(:ignore t)
 "C-k" '(:ignore t))

;; evil-surround
(general-define-key
 :keymaps 'visual
 "S" 'evil-surround-region)
(general-define-key
 :keymaps 'operator
 "s" 'evil-surround-edit
 "S" 'evil-surround-edit)

;; flycheck
(general-define-key
 :keymaps 'motion
 "]e" 'next-error
 "[e" 'previous-error)
(general-define-key
 :keymaps 'flycheck-error-list-mode-map
 :states 'normal
 "C-n" 'flycheck-error-list-next-error
 "C-p" 'flycheck-error-list-previous-error
 "j"   'flycheck-error-list-next-error
 "k"   'flycheck-error-list-previous-error
 "RET" 'flycheck-error-list-goto-error
 "q"   'quit-window)

;; flyspell
(general-define-key
 :keymaps 'motion
 "]S" 'flyspell-correct-word-generic
 "[S" 'flyspell-correct-previous-word-generic)

;; git-timemachine
(general-define-key
 :keymaps 'git-timemachine-mode-map
 :states '(normal visual)
 "p" 'git-timemachine-show-previous-revision
 "n" 'git-timemachine-show-next-revision
 "g" 'git-timemachine-show-nth-revision
 "q" 'git-timemachine-quit
 "w" 'git-timemachine-kill-abbreviated-revision
 "W" 'git-timemachine-kill-revision
 "b" 'git-timemachine-blame)

;; helpful
(general-define-key
 :keymaps 'helpful-mode-map
 :states 'normal
 "o"  'ace-link-help
 "q"  'quit-window)
(general-define-key
 :keymaps 'help-map
 "f" '(helpful-function :package helpful)
 "k" '(helpful-key      :package helpful)
 "v" '(helpful-variable :package helpful)
 "M" '(helpful-macro    :package helpful))

;; hl-todo
(general-define-key
 :keymaps 'motion
 "]t" 'hl-todo-next
 "[t" 'hl-todo-previous)

;; ivy
(general-define-key
 :keymaps 'ivy-minibuffer-map
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

;; neotree
(general-define-key
 "<left-margin> <mouse-1>" 'neotree|toggle
 "<left-fringe> <mouse-1>" 'neotree|toggle)
(general-define-key
 :keymaps 'neotree-mode-map
 :states 'normal
 "g"         '(:ignore t)
 "TAB"       'neotree-quick-look
 "RET"       'neotree-enter
 "v"         'neotree-enter-vertical-split
 "s"         'neotree-enter-horizontal-split
 ;; Navigation
 "j"         'neotree-next-line
 "k"         'neotree-previous-line
 "n"         'neotree-next-line
 "p"         'neotree-previous-line
 "J"         'neotree-select-next-sibling-node
 "K"         'neotree-select-previous-sibling-node
 "H"         'neotree-select-up-node
 "L"         'neotree-select-down-node
 ;; Files
 "c"         'neotree-create-node
 "d"         'neotree-delete-node
 "r"         'neotree-rename-node
 ;; Window
 [backspace] 'evil-window-prev
 "q"         'neotree-hide
 "R"         'neotree-refresh)

;; realgud
(general-define-key
 :keymaps 'realgud:shortkey-mode-map
 :states 'normal
 "j" 'evil-next-line
 "k" 'evil-previous-line
 "h" 'evil-backward-char
 "l" 'evil-forward-char
 "c" 'realgud:cmd-continue)
(general-define-key
 :keymaps 'realgud:shortkey-mode-map
 :states 'motion
 "n" 'realgud:cmd-next
 "b" 'realgud:cmd-break
 "B" 'realgud:cmd-clear)

;; rotate-text
(general-define-key
 :keymaps 'normal
 "+" 'rotate-text
 "-" 'rotate-text-backward)

;; smart-forward
(general-define-key
 :keymaps 'motion
 "g]" 'smart-forward
 "g[" 'smart-backward)

;; symbol-overlay
(general-define-key
 :keymaps 'symbol-overlay-map
 [escape] 'symbol-overlay-remove-all
 "C-g"    'symbol-overlay-remove-all)

;; undo-tree
(general-define-key
 :keymaps 'visual
 "C-u" 'undo-tree-undo
 "C-r" 'undo-tree-redo)

(provide 'bindings)
;;; bindings.el ends here
