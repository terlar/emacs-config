;;; bindings.el --- My key bindings

;;; Commentary:
;; My key binding setup.

;;; Code:
(require 'base-vars)
(require 'base-keybinds)

(eval `(general-define-key :keymaps '(normal visual)
                           ,my-normal-leader-key (general-simulate-keys ,my-leader-key)))

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

;; Leader
(general-define-key
 :prefix my-leader-key
 "SPC" '(projectile-find-file :which-key "Find file in project")
 ","   '(switch-to-buffer     :which-key "Switch to buffer")
 "."   '(find-file            :which-key "Browse files")
 "RET" '(counsel-bookmark     :which-key "Jump to bookmark")
 "w"   '(:keymap evil-window-map :package evil :which-key "window")
 "x"   '(scratch-buffer       :which-key "Pop up scratch buffer")

 "["   '(:ignore t :which-key "previous...")
 "[ b" '(previous-buffer                        :which-key "Buffer")
 "[ d" '(diff-hl-previous-hunk                  :which-key "Diff Hunk")
 "[ t" '(hl-todo-previous                       :which-key "Todo")
 "[ e" '(previous-error                         :which-key "Error")
 "[ w" '(persp-prev                             :which-key "Workspace")
 "[ h" '(smart-backward                         :which-key "Smart jump")
 "[ s" '(evil-prev-flyspell-error               :which-key "Spelling error")
 "[ S" '(flyspell-correct-previous-word-generic :which-key "Spelling correction")

 "]"   '(:ignore t :which-key "next...")
 "] b" '(next-buffer                            :which-key "Buffer")
 "] d" '(diff-hl-next-hunk                      :which-key "Diff Hunk")
 "] t" '(hl-todo-next                           :which-key "Todo")
 "] e" '(next-error                             :which-key "Error")
 "] w" '(persp-next                             :which-key "Workspace")
 "] h" '(smart-forward                          :which-key "Smart jump")
 "] s" '(evil-next-flyspell-error               :which-key "Spelling error")
 "] S" '(flyspell-correct-word-generic          :which-key "Spelling correction")

 "/"   '(:ignore t :which-key "search")
 "/ /" '(swiper                :which-key "Swiper")
 "/ i" '(imenu                 :which-key "Imenu")
 "/ I" '(imenu-anywhere        :which-key "Imenu across buffers")
 "/ g" '(counsel-rg            :which-key "Grep files")
 "/ G" '(counsel-projectile-rg :which-key "Grep project files")

 "TAB" '(:ignore t :which-key "workspace")
 "TAB ." '(persp-switch :which-key "Switch workspace")

 "b"   '(:ignore t :which-key "buffer")
 "b n" '(evil-buffer-new    :which-key "New empty buffer")
 "b b" '(switch-to-buffer   :which-key "Switch buffer")
 "b k" '(kill-buffer        :which-key "Kill buffer")
 "b o" '(kill-other-buffers :which-key "Kill other buffers")
 "b s" '(save-buffer        :which-key "Save buffer")
 "b x" '(scratch-buffer     :which-key "Pop up scratch buffer")
 "b z" '(bury-buffer        :which-key "Bury buffer")
 "b ]" '(next-buffer        :which-key "Next buffer")
 "b [" '(previous-buffer    :which-key "Previous buffer")
 "b S" '(sudo-edit          :which-key "Sudo edit this file")

 "c"   '(:ignore t :which-key "code")
 "c c" '(editorconfig-apply   :which-key "Apply editorconfig")
 "c x" '(flycheck-list-errors :which-key "List errors")
 "c e" '(eval-buffer          :which-key "Evaluate buffer")
 "c d" '(evil-goto-definition :which-key "Jump to definition")
 "c p" '(source-peek          :which-key "Peek definition")
 "c r" '(eval|repl            :which-key "Open REPL")

 "f"   '(:ignore t :which-key "file")
 "f ." '(find-file                              :which-key "Find file")
 "f /" '(projectile-find-file                   :which-key "Find file in project")
 "f ?" '(counsel-file-jump                      :which-key "Find file here")
 "f a" '(projectile-find-other-file             :which-key "Find other file")
 "f c" '(editorconfig-find-current-editorconfig :which-key "Open project editorconfig")
 "f g" '(counsel-rg                             :which-key "Grep files")
 "f G" '(counsel-projectile-rg                  :which-key "Grep project files")
 "f r" '(recentf                                :which-key "Recent files")
 "f R" '(projectile-recentf                     :which-key "Recent project files")

 "g"   '(:ignore t :which-key "git")
 "g s" '(magit-status                :which-key "Status")
 "g l" '(magit-log-buffer-file       :which-key "Log")
 "g b" '(magit-blame                 :which-key "Blame")
 "g t" '(git-timemachine-toggle      :which-key "Time machine")
 "g r" '(diff-hl-revert-hunk         :which-key "Revert hunk")
 "g ]" '(diff-hl-next-hunk           :which-key "Next hunk")
 "g [" '(diff-hl-previous-hunk       :which-key "Previous hunk")
 "g p" '(magit-pull                  :which-key "Pull")
 "g c" '(magit-clone                 :which-key "Clone")
 "g B" '(vcs/git-browse              :which-key "Browse")
 "g I" '(vcs/git-browse-issues       :which-key "Browse issues")
 "g m" '(git-messenger:popup-message :which-key "Popup message")

 "h" '(:ignore t :which-key "help")
 "h h" '(:keymap help-map)
 "h a" '(apropos              :which-key "Apropos")
 "h l" '(find-library         :which-key "Find library")
 "h f" '(describe-function    :which-key "Describe function")
 "h k" '(describe-key         :which-key "Describe key")
 "h c" '(describe-char        :which-key "Describe char")
 "h M" '(describe-mode        :which-key "Describe mode")
 "h v" '(describe-variable    :which-key "Describe variable")
 "h F" '(describe-face        :which-key "Describe face")
 "h '" '(what-cursor-position :which-key "What face")
 "h i" '(info                 :which-key "Info")

 "o" '(:ignore t :which-key "open")
 "o c" '(calendar           :which-key "Calendar")
 "o C" '(display-time-world :which-key "World Time")
 "o d" '(deft               :which-key "Notes")
 "o n" '(neotree|toggle     :which-key "NeoTree")
 "o N" '(neotree|window     :which-key "NeoTree Window")
 "o t" '(shell              :which-key "Terminal")
 "o T" '(ansi-term          :which-key "ANSI Terminal")
 "o w" '(eww                :which-key "Browser")

 "=" '(:ignore t :which-key "diff")
 "= b" '(ediff-buffers          :which-key "Buffers")
 "= B" '(ediff-buffers3         :which-key "Buffers (3-way)")
 "= c" '(compare-windows        :which-key "Compare windows")
 "= =" '(ediff-files            :which-key "Files")
 "= f" '(ediff-files            :which-key "Files")
 "= F" '(ediff-files3           :which-key "Files (3-way)")
 "= r" '(ediff-revision         :which-key "Compare versions")
 "= p" '(ediff-patch-file       :which-key "Patch file")
 "= P" '(ediff-patch-buffer     :which-key "Patch buffer")
 "= l" '(ediff-regions-linewise :which-key "Linewise")
 "= w" '(ediff-regions-wordwise :which-key "Wordwise")

 "~" '(:ignore t :which-key "toggle")
 "~ a" '(goto-address-mode           :which-key "Clickable links")
 "~ c" '(rainbow-identifiers-mode    :which-key "Color identifiers")
 "~ C" '(rainbow-mode                :which-key "Color display")
 "~ d" '(toggle-debug-on-error       :which-key "Debug on error")
 "~ e" '(eldoc-overlay-mode          :which-key "Eldoc inline")
 "~ f" '(hs-minor-mode               :which-key "Code folding")
 "~ F" '(flycheck-mode               :which-key "Syntax checker")
 "~ g" '(indent-guide-mode           :which-key "Indent guides")
 "~ h" '(hl-line-mode                :which-key "Line highlight")
 "~ i" '(aggressive-indent-mode      :which-key "Automatic indentation")
 "~ l" '(toggle-display-line-numbers :which-key "Line numbers")
 "~ r" '(ruler-mode                  :which-key "Ruler")
 "~ R" '(my|start-spray              :which-key "Speed-reading")
 "~ s" '(flyspell-mode               :which-key "Spell-checking")
 "~ S" '(subword-mode                :which-key "Subword")
 "~ v" '(variable-pitch-mode         :which-key "Fixed-width/variable-width font")
 "~ w" '(whitespace-mode             :which-key "Display white-space characters")
 "~ W" '(auto-fill-mode              :which-key "Automatic line-wrapping")

 "q" '(:ignore t :which-key "quit")
 "q q" '(evil-save-and-quit :which-key "Quit"))

;; Normal state
(general-define-key
 :keymaps 'normal
 "]b" '(next-buffer)
 "[b" '(previous-buffer)
 "]e" '(next-error)
 "[e" '(previous-error)
 "]w" '(persp-next)
 "[w" '(persp-prev)
 "gp" '(evil|reselect-paste)
 "gr" '(eval-region)
 "gR" '(eval-buffer)
 "zx" '(kill-buffer))

;; Visual state
(general-define-key
 :keymaps 'visual
 "." '(evil-repeat)
 "<" '(evil|visual-dedent)
 ">" '(evil|visual-indent))

(general-define-key
 :keymaps 'evil-window-map
 ;; Navigation
 "C-w"     '(ace-window           :which-key "Select a window")
 "B"       '(switch-to-minibuffer :which-key "Switch to minibuffer")
 "TAB"     '(neotree|window       :which-key "Switch to NeoTree")
 [backtab] '(neotree|toggle       :which-key "Toggle NeoTree")
 ;; Swapping
 "C-S-w"   '(ace-swap-window      :which-key "Swap window")
 "z"       '(zoom-window-zoom     :which-key "Zoom window")
 ;; Undo/redo
 "u"       '(winner-undo          :which-key "Undo")
 "C-u"     '(winner-undo          :which-key "Undo")
 "C-r"     '(winner-redo          :which-key "Redo")
 ;; Delete
 "C-C"     '(ace-delete-window    :which-key "Select and delete a window"))

;;;
;; Built-in plugins

;; comint
(general-define-key
 :keymaps 'comint-mode-map
 "TAB" '(company-complete))

;; conf-mode
(general-define-key
 :keymaps 'conf-mode-map
 ;; Disable conflicting key
 "C-c SPC" '(nil))

;; debug
(general-define-key
 :keymaps 'debugger-mode-map
 :states 'normal
 "RET" '(debug-help-follow)
 "n"   '(debugger-step-through)
 "c"   '(debugger-continue)
 "q"   '(top-level))

;; ediff
(add-hook 'ediff-keymap-setup-hook
          #'(lambda ()
              (general-define-key
               :keymaps 'ediff-mode-map
               "d" '(ediff-copy-both-to-C      :which-key "Copy both to C")
               "j" '(ediff-next-difference     :which-key "Next difference")
               "k" '(ediff-previous-difference :which-key "Previous difference"))))

;; eww
(general-define-key
 :keymaps 'eww-mode-map
 :states 'normal
 "h" '(eww-back-url)
 "l" '(eww-next-url)
 "q" '(quit-window))

;; help-mode
(general-define-key
 :keymaps 'help-mode-map
 :states 'normal
 "[[" '(help-go-back)
 "]]" '(help-go-forward)
 "o"  '(ace-link-help)
 "q"  '(quit-window))

;; vc-annotate
(general-define-key
 :keymaps 'vc-annotate-mode-map
 :states 'normal
 "q"   '(kill-this-buffer)
 "d"   '(vc-annotate-show-diff-revision-at-line)
 "D"   '(vc-annotate-show-changeset-diff-revision-at-line)
 "SPC" '(vc-annotate-show-log-revision-at-line)
 "]]"  '(vc-annotate-next-revision)
 "[["  '(vc-annotate-prev-revision)
 "TAB" '(vc-annotate-toggle-annotation-visibility)
 "RET" '(vc-annotate-find-revision-at-line))

;;;
;; Plugins

;; company
(general-define-key
 :keymaps 'company-mode-map
 :states 'insert
 "C-SPC"   '(company-indent-or-complete-common)
 "C-x C-l" '(company|whole-lines)
 "C-x C-k" '(company|dict-or-keywords)
 "C-x C-f" '(company-files)
 "C-x C-]" '(company-etags)
 "C-x s"   '(company-ispell)
 "C-x C-s" '(company-yasnippet)
 "C-x C-o" '(company-capf)
 "C-x C-n" '(company-dabbrev-code)
 "C-x C-p" '(company|dabbrev-code-previous))
(general-define-key
 :keymaps 'company-active-map
 ;; Don't interfere with `evil-delete-backward-word' in insert mode
 "C-w"     '(nil)
 ;; Don't interfere with the return key
 [return]  '(nil)
 "RET"     '(nil)
 ;; Abort on escape but leave current completion
 [escape]  '(company-abort)

 "C-e"     '(company-complete-selection)
 "C-f"     '(company-complete-selection)
 "C-SPC"   '(company-complete-common)
 "TAB"     '(company-complete-common-or-cycle)
 [backtab] '(company-select-previous)

 "C-o"     '(company-search-kill-others)
 "C-n"     '(company-select-next)
 "C-p"     '(company-select-previous)
 "C-h"     '(company-quickhelp-manual-begin)
 "C-S-h"   '(company-show-doc-buffer)
 "C-S-s"   '(company-search-candidates)
 "C-s"     '(company-filter-candidates))
(general-define-key
 :keymaps 'company-search-map
 "C-n"    '(company-search-repeat-forward)
 "C-p"    '(company-search-repeat-backward)
 "C-s"    '(company|search-abort-and-filter-candidates)
 [escape] '(company-search-abort))

;; counsel
(general-define-key
 :keymaps 'ivy-mode-map
 "C-o" '(ivy-dispatching-done))
(general-define-key
 :keymaps 'counsel-ag-map
 [backtab] '(ivy-wgrep-occur)
 "C-SPC" '(counsel-git-grep-recenter))

;; diff-hl
(general-define-key
 :keymaps 'motion
 "]d" '(diff-hl-next-hunk)
 "[d" '(diff-hl-previous-hunk))

;; dumb-jump
(general-define-key
 "M-g o" '(dumb-jump-go-other-window)
 "M-g j" '(dumb-jump-go)
 "M-g i" '(dumb-jump-go-prompt)
 "M-g x" '(dumb-jump-go-prefer-external)
 "M-g z" '(dumb-jump-go-prefer-external-other-window))

;; evil-commentary
(general-define-key
 :keymaps 'normal
 "gc" '(evil-commentary))

;; evil-exchange
(general-define-key
 :keymaps 'normal
 "gx" '(evil-exchange))

;; evil-magit
(general-define-key
 :keymaps '(magit-status-mode-map magit-revision-mode-map)
 :states 'normal
 "C-j" '(nil)
 "C-k" '(nil))

;; evil-surround
(general-define-key
 :keymaps 'visual
 "S" '(evil-surround-region))
(general-define-key
 :keymaps 'operator
 "s" '(evil-surround-edit)
 "S" '(evil-surround-edit))

;; flycheck
(general-define-key
 :keymaps 'motion
 "]e" '(next-error)
 "[e" '(previous-error))
(general-define-key
 :keymaps 'flycheck-error-list-mode-map
 :states 'normal
 "C-n" '(flycheck-error-list-next-error)
 "C-p" '(flycheck-error-list-previous-error)
 "j"   '(flycheck-error-list-next-error)
 "k"   '(flycheck-error-list-previous-error)
 "RET" '(flycheck-error-list-goto-error))

;; flyspell
(general-define-key
 :keymaps 'motion
 "]S" '(flyspell-correct-word-generic)
 "[S" '(flyspell-correct-previous-word-generic))

;; git-timemachine
(general-define-key
 :keymaps 'git-timemachine-mode-map
 :states '(normal visual)
 "p" '(git-timemachine-show-previous-revision)
 "n" '(git-timemachine-show-next-revision)
 "g" '(git-timemachine-show-nth-revision)
 "q" '(git-timemachine-quit)
 "w" '(git-timemachine-kill-abbreviated-revision)
 "W" '(git-timemachine-kill-revision)
 "b" '(git-timemachine-blame))

;; hl-todo
(general-define-key
 :keymaps 'motion
 "]t" '(hl-todo-next)
 "[t" '(hl-todo-previous))

;; ivy
(general-define-key
 :keymaps 'ivy-minibuffer-map
 [escape] '(keyboard-escape-quit)
 "M-v"    '(yank)
 "M-z"    '(undo)
 "C-r"    '(evil-paste-from-register)
 "C-k"    '(ivy-previous-line)
 "C-j"    '(ivy-next-line)
 "C-l"    '(ivy-alt-done)
 "C-w"    '(ivy-backward-kill-word)
 "C-u"    '(ivy-kill-line)
 "C-b"    '(backward-word)
 "C-f"    '(forward-word))

;; neotree
(general-define-key
 "<left-margin> <mouse-1>" '(neotree|toggle)
 "<left-fringe> <mouse-1>" '(neotree|toggle))
(general-define-key
 :keymaps 'neotree-mode-map
 :states 'normal
 "g"         '(nil)
 "TAB"       '(neotree-quick-look)
 "RET"       '(neotree-enter)
 "v"         '(neotree-enter-vertical-split)
 "s"         '(neotree-enter-horizontal-split)
 ;; Navigation
 "j"         '(neotree-next-line)
 "k"         '(neotree-previous-line)
 "n"         '(neotree-next-line)
 "p"         '(neotree-previous-line)
 "J"         '(neotree-select-next-sibling-node)
 "K"         '(neotree-select-previous-sibling-node)
 "H"         '(neotree-select-up-node)
 "L"         '(neotree-select-down-node)
 ;; Files
 "c"         '(neotree-create-node)
 "d"         '(neotree-delete-node)
 "r"         '(neotree-rename-node)
 ;; Window
 [backspace] '(evil-window-prev)
 "q"         '(neotree-hide)
 "R"         '(neotree-refresh))

;; realgud
(general-define-key
 :keymaps 'realgud:shortkey-mode-map
 :states 'normal
 "j" '(evil-next-line)
 "k" '(evil-previous-line)
 "h" '(evil-backward-char)
 "l" '(evil-forward-char)
 "c" '(realgud:cmd-continue))
(general-define-key
 :keymaps 'realgud:shortkey-mode-map
 :states 'motion
 "n" '(realgud:cmd-next)
 "b" '(realgud:cmd-break)
 "B" '(realgud:cmd-clear))

;; rotate-text
(general-define-key
 :keymaps 'normal
 "+" '(rotate-text)
 "-" '(rotate-text-backward))

;; smart-forward
(general-define-key
 :keymaps 'motion
 "g]" '(smart-forward)
 "g[" '(smart-backward))

;; undo-tree
(general-define-key
 :keymaps 'visual
 "C-u" '(undo-tree-undo)
 "C-r" '(undo-tree-redo))

(provide 'bindings)
;;; bindings.el ends here
