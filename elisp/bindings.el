;;; bindings.el --- My key bindings -*- lexical-binding: t; -*-

;;; Commentary:
;; My key binding setup.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-keybinds))

(eval `(general-define-key :keymaps '(normal motion visual)
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

;; Use counsel/swiper for search
(general-define-key
 :keymaps 'global
 "C-s" 'counsel-grep-or-swiper
 "C-r" 'counsel-grep-or-swiper)

;; Accessible REPL
(general-define-key
 :keymaps 'global
 "M-;" 'elisp-repl)

;; Text-scaling
(general-define-key
 "C-=" 'default-text-scale-reset
 "C--" 'default-text-scale-decrease
 "C-+" 'default-text-scale-increase
 "M-=" (lambda () (interactive) (text-scale-set 0))
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
 "'"   '(toggle-popup-buffer                    :wk "Toggle popup buffer")
 ","   '(switch-to-buffer                       :wk "Switch to buffer")
 "."   '(find-file                              :wk "Browse files")
 ";"   '(counsel-bookmark                       :wk "Jump to bookmark")
 "RET" '(repl                                   :wk "Open REPL")
 "v"   '(:keymap
         symbol-overlay-map
         :package symbol-overlay :wk "overlays")
 "w"   '(:keymap
         evil-window-map
         :package evil :wk "window")
 "x"   '(toggle-scratch-buffer                  :wk "Toggle scratch buffer")

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
 "b [" '(previous-buffer            :wk "Previous buffer")
 "b ]" '(next-buffer                :wk "Next buffer")
 "b b" '(switch-to-buffer           :wk "Switch buffer")
 "b f" '(editorconfig-format-buffer :wk "Reformat buffer")
 "b k" '(kill-buffer                :wk "Kill buffer")
 "b n" '(evil-buffer-new            :wk "New empty buffer")
 "b o" '(kill-other-buffers         :wk "Kill other buffers")
 "b s" '(save-buffer                :wk "Save buffer")
 "b S" '(sudo-edit                  :wk "Sudo edit this file")
 "b t" '(retab-buffer               :wk "Retab buffer")
 "b z" '(bury-buffer                :wk "Bury buffer")

 "c"   '(:ignore t :wk "code")
 "c ." '(editorconfig-apply         :wk "Apply editorconfig")
 "c c" '(recompile                  :wk "Recompile")
 "c C" '(projectile-compile-project :wk "Compile")
 "c d" '(counsel-dash-at-point      :wk "Lookup documentation at point")
 "c D" '(counsel-dash               :wk "Lookup documentation")
 "c e" '(repl-eval                  :wk "Evaluate code")
 "c o" '(imenu-list-minor-mode      :wk "Outline")
 "c p" '(source-peek                :wk "Peek definition")
 "c r" '(repl                       :wk "Open REPL")
 "c x" '(flycheck-list-errors       :wk "List errors")

 "d"   '(:ignore t :wk "docker")
 "d c" '(docker-containers :wk "Docker containers")
 "d i" '(docker-images     :wk "Docker images")
 "d n" '(docker-networks   :wk "Docker networks")
 "d v" '(docker-volumes    :wk "Docker volumes")

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
 "g [" '(diff-hl-previous-hunk       :wk "Previous hunk")
 "g ]" '(diff-hl-next-hunk           :wk "Next hunk")
 "g b" '(magit-blame                 :wk "Blame")
 "g B" '(vcs-git-browse              :wk "Browse")
 "g c" '(magit-clone                 :wk "Clone")
 "g f" '(magit-file-popup            :wk "File popup")
 "g I" '(vcs-git-browse-issues       :wk "Browse issues")
 "g l" '(magit-log-buffer-file       :wk "Log")
 "g m" '(git-messenger:popup-message :wk "Popup message")
 "g p" '(magit-pull                  :wk "Pull")
 "g r" '(diff-hl-revert-hunk         :wk "Revert hunk")
 "g s" '(magit-status                :wk "Status")
 "g t" '(git-timemachine-toggle      :wk "Time machine")

 "h" '(:ignore t :wk "help")
 "h h" '(:keymap help-map)
 "h a" '(apropos                 :wk "Apropos")
 "h d" '(counsel-dash            :wk "Documentation")
 "h i" '(info                    :wk "Info")
 "h l" '(find-library            :wk "Find library")
 "h b" '(describe-buffer         :wk "Describe buffer")
 "h c" '(helpful-command         :wk "Describe command")
 "h C" '(describe-char           :wk "Describe char")
 "h f" '(describe-function       :wk "Describe function")
 "h k" '(helpful-key             :wk "Describe key")
 "h K" '(describe-keymap         :wk "Describe keymap")
 "h m" '(helpful-macro           :wk "Describe macro")
 "h M" '(describe-mode           :wk "Describe mode")
 "h o" '(describe-option         :wk "Describe option")
 "h O" '(describe-option-of-type :wk "Describe option (of type)")
 "h v" '(helpful-variable        :wk "Describe variable")
 "h f" '(helpful-function        :wk "Describe function")
 "h F" '(describe-face           :wk "Describe face")
 "h '" '(what-cursor-position    :wk "What face")

 "o" '(:ignore t :wk "open")
 "o c" '(calendar           :wk "Calendar")
 "o C" '(display-time-world :wk "World Time")
 "o d" '(deft               :wk "Notes")
 "o n" '(+neotree-toggle    :wk "NeoTree")
 "o N" '(+neotree-window    :wk "NeoTree Window")
 "o p" '(list-processes     :wk "List processes")
 "o s" '(speed-read         :wk "Speed-reading")
 "o t" '(eshell             :wk "Terminal")
 "o T" '(ansi-term          :wk "ANSI Terminal")
 "o u" '(ace-link           :wk "Open link")
 "o w" '(eww                :wk "Browser")
 "o x" '(re-builder         :wk "Browser")

 "s" '(:ignore t :wk "spell")
 "s d" '(adict-guess-dictionary  :wk "Guess dictionary")
 "s D" '(adict-change-dictionary :wk "Select dictionary")

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
 "~ c" '(+color-identifiers-toggle   :wk "Colorize identifiers")
 "~ C" '(rainbow-mode                :wk "Colorize color values")
 "~ d" '(toggle-debug-on-error       :wk "Debug on error")
 "~ e" '(eldoc-overlay-mode          :wk "Eldoc inline")
 "~ f" '(hs-minor-mode               :wk "Code folding")
 "~ F" '(flycheck-mode               :wk "Syntax checker")
 "~ g" '(indent-guide-mode           :wk "Indent guides")
 "~ h" '(hl-line-mode                :wk "Line highlight")
 "~ i" '(aggressive-indent-mode      :wk "Automatic indentation")
 "~ l" '(display-line-numbers-mode   :wk "Line numbers")
 "~ L" '(coverlay-toggle-overlays    :wk "Coverage overlays")
 "~ r" '(ruler-mode                  :wk "Ruler")
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
 "<C-return>" 'repl-eval
 "]b" 'next-buffer
 "[b" 'previous-buffer
 "]c" 'eir-next-code-line
 "]e" 'next-error
 "[e" 'previous-error
 "]w" 'persp-next
 "[w" 'persp-prev
 "gp" 'evil-reselect-paste
 "gr" 'eval-region
 "gR" 'eval-buffer
 "K"  'documentation-at-point
 "zx" 'kill-buffer)

(general-define-key
 :keymaps '(normal motion)
 "gd" 'xref-find-definitions
 "gD" 'xref-find-references)

;; Visual state
(general-define-key
 :keymaps 'visual
 "." 'evil-repeat
 "<" 'evil-visual-outdent
 ">" 'evil-visual-indent)

(general-define-key
 :keymaps 'evil-window-map
 ;; Navigation
 "C-w"     '(ace-window           :wk "Select a window")
 "B"       '(switch-to-minibuffer :wk "Switch to minibuffer")
 "TAB"     '(+neotree-window      :wk "Switch to NeoTree")
 [backtab] '(+neotree-toggle      :wk "Toggle NeoTree")
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

;; conf-mode
(general-define-key
 :keymaps 'conf-mode-map
 ;; Disable conflicting key
 "C-c \"" 'nil
 "C-c '" 'nil
 "C-c :" 'nil
 "C-c SPC" 'nil)

;; debug
(general-define-key
 :keymaps 'debugger-mode-map
 :states '(normal motion insert emacs)
 "RET" 'debug-help-follow
 "n"   'debugger-step-through)

;; dired
(general-define-key
 :keymaps 'dired-mode-map
 :major-modes t
 :states '(normal motion)
 "gg" 'evil-goto-first-line
 "G" 'evil-goto-line
 "j" 'dired-next-line
 "k" 'dired-previous-line
 "/" 'counsel-grep-or-swiper
 "?" 'counsel-grep-or-swiper)

;; ediff
(add-hook! 'ediff-keymap-setup
           (general-define-key
            :keymaps 'ediff-mode-map
            "d" '(ediff-copy-both-to-C      :wk "Copy both to C")
            "j" '(ediff-next-difference     :wk "Next difference")
            "k" '(ediff-previous-difference :wk "Previous difference")))

;; eshell
(add-hook! 'eshell-mode
           (general-define-key
            :keymaps 'eshell-mode-map
            "C-l" 'eshell-clear-buffer
            [tab] 'company-complete-common-or-cycle
            "TAB" 'company-complete-common-or-cycle))

;; eww
(general-define-key
 :keymaps 'eww-mode-map
 :states 'normal
 "h" 'eww-back-url
 "l" 'eww-next-url
 "q" 'quit-window)

;; help-mode
(general-define-key
 :keymaps '(help-mode-map
            helpful-mode-map)
 :states '(normal motion insert emacs)
 "[[" 'help-go-back
 "]]" 'help-go-forward
 "o"  'ace-link-help
 "q"  'quit-window)

;; package
(general-define-key
 :keymaps 'package-menu-mode-map
 :states '(normal motion insert emacs)
 "q" 'kill-this-buffer)

;; vc-annotate
(general-define-key
 :keymaps 'vc-annotate-mode-map
 :states '(normal motion insert emacs)
 "d"   'vc-annotate-show-diff-revision-at-line
 "D"   'vc-annotate-show-changeset-diff-revision-at-line
 "SPC" 'vc-annotate-show-log-revision-at-line
 "]]"  'vc-annotate-next-revision
 "[["  'vc-annotate-prev-revision
 "TAB" 'vc-annotate-toggle-annotation-visibility
 "RET" 'vc-annotate-find-revision-at-line)

;;;
;; Plugins

;; company
(general-define-key
 :keymaps 'company-mode-map
 :states 'insert
 "C-SPC"   'company-indent-or-complete-common
 "C-x C-l" '+company-whole-lines
 "C-x C-k" '+company-dict-or-keywords
 "C-x C-f" 'company-files
 "C-x C-]" 'company-etags
 "C-x s"   'company-ispell
 "C-x C-s" 'company-yasnippet
 "C-x C-o" 'company-capf
 "C-x C-n" 'company-dabbrev-code
 "C-x C-p" '+company-dabbrev-code-previous)
(general-define-key
 :keymaps 'company-active-map
 ;; Don't interfere with `evil-delete-backward-word' in insert mode
 "C-w"     'nil
 ;; Don't interfere with the return key
 [return]  'nil
 "RET"     'nil
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
 "C-s"    '+company-search-abort-and-filter-candidates
 [escape] 'company-search-abort)

;; counsel
(general-define-key
 :keymaps 'ivy-mode-map
 "C-o" 'ivy-dispatching-done)
(general-define-key
 :keymaps 'counsel-ag-map
 [backtab] 'ivy-occur
 "C-SPC" 'ivy-call-and-recenter)

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
 "gc" 'evil-commentary
 "gy" 'evil-commentary-yank)

;; evil-exchange
(general-define-key
 :keymaps 'normal
 "gx" 'evil-exchange)

;; evil-magit
(general-define-key
 :keymaps '(magit-status-mode-map magit-revision-mode-map)
 :states 'normal
 "C-j" 'nil
 "C-k" 'nil)

;; evil-surround
(general-define-key
 :keymaps 'visual
 "S" 'evil-surround-region)
(general-define-key
 :keymaps 'operator
 "s" 'evil-surround-edit
 "S" 'evil-Surround-edit)

;; flycheck
(general-define-key
 :keymaps 'motion
 "]e" 'next-error
 "[e" 'previous-error)
(general-define-key
 :keymaps 'flycheck-error-list-mode-map
 :states '(normal motion insert emacs)
 "C-n" 'flycheck-error-list-next-error
 "C-p" 'flycheck-error-list-previous-error
 "j"   'flycheck-error-list-next-error
 "k"   'flycheck-error-list-previous-error
 "RET" 'flycheck-error-list-goto-error)

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
 :keymaps 'help-map
 "f" 'helpful-function
 "k" 'helpful-key
 "v" 'helpful-variable
 "M" 'helpful-macro)

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
 "<left-margin> <mouse-1>" '+neotree-toggle
 "<left-fringe> <mouse-1>" '+neotree-toggle)
(general-define-key
 :keymaps 'neotree-mode-map
 :states 'normal
 "g"         'nil
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

;; nov
(general-define-key
 :keymaps 'nov-mode-map
 :states 'normal
 "]p"      'nov-next-document
 "[p"      'nov-previous-document
 "<up>"    'nov-scroll-down
 "<down>"  'nov-scroll-up
 "<left>"  'nov-previous-document
 "<right>" 'nov-next-document
 "q"       'image-kill-buffer)

;; pdfview
(general-define-key
 :keymaps 'pdf-view-mode-map
 "h" 'pdf-view-previous-page
 "j" 'pdf-view-next-line-or-next-page
 "k" 'pdf-view-previous-line-or-previous-page
 "l" 'pdf-view-next-page
 "q" 'image-kill-buffer)

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

;; wgrep
(general-define-key
 :keymaps 'wgrep-mode-map
 :states 'normal
 "d" 'wgrep-mark-deletion
 "ZZ" 'wgrep-finish-edit)

(provide 'bindings)
;;; bindings.el ends here
