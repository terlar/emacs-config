;;; bindings.el --- My key bindings -*- lexical-binding: t; -*-

;;; Commentary:
;; My key binding setup.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-lib)
  (require 'base-keybinds))

(with-eval-after-load 'evil
  (eval `(general-define-key :keymaps '(normal motion visual)
                             ,my-normal-leader-key
                             (general-simulate-keys ,my-leader-key nil nil t))))

;;;
;; Remaps

;; Smarter abbrev completion
(general-define-key [remap dabbrev-expand] 'hippie-expand)

;; Consistent jumping
(general-define-key
 [remap find-tag]         'projectile-find-tag
 [remap evil-jump-to-tag] 'projectile-find-tag)

;;;
;; Global

(general-define-key
 :keymaps 'global

 ;; Window navigation
 "C-`" 'window-toggle-side-windows
 "C-§" 'window-toggle-side-windows

 ;; Use counsel/swiper for search
 "C-s" 'counsel-grep-or-swiper
 "C-r" 'counsel-grep-or-swiper
 "C-x /" 'counsel-abbrev

 ;; Text-scaling
 "C-=" 'default-text-scale-reset
 "C--" 'default-text-scale-decrease
 "C-+" 'default-text-scale-increase
 "M-=" 'text-scale-reset
 "M--" 'text-scale-decrease
 "M-+" 'text-scale-increase
 "<C-mouse-4>" 'text-scale-decrease
 "<C-mouse-5>" 'text-scale-increase

 ;; Screen refresh
 "C-l" 'refresh)

;; Leader

(general-define-key
 :prefix my-leader-key
 :infix "["
 ""  '(:ignore t :wk "previous...")
 "[" '(text-scale-decrease                    :wk "Text size")
 "b" '(previous-buffer                        :wk "Buffer")
 "d" '(diff-hl-previous-hunk                  :wk "Diff Hunk")
 "t" '(hl-todo-previous                       :wk "Todo")
 "e" '(previous-error                         :wk "Error")
 "w" '(persp-prev                             :wk "Workspace")
 "h" '(smart-backward                         :wk "Smart jump")
 "s" '(evil-prev-flyspell-error               :wk "Spelling error")
 "S" '(flyspell-correct-previous-word-generic
       :package flyspell-correct-ivy          :wk "Spelling correction"))

(general-define-key
 :prefix my-leader-key
 :infix "]"
 ""  '(:ignore t :wk "next...")
 "]" '(text-scale-increase                    :wk "Text size")
 "b" '(next-buffer                            :wk "Buffer")
 "d" '(diff-hl-next-hunk                      :wk "Diff Hunk")
 "t" '(hl-todo-next                           :wk "Todo")
 "e" '(next-error                             :wk "Error")
 "w" '(persp-next                             :wk "Workspace")
 "h" '(smart-forward                          :wk "Smart jump")
 "s" '(evil-next-flyspell-error               :wk "Spelling error")
 "S" '(flyspell-correct-word-generic
       :package flyspell-correct-ivy          :wk "Spelling correction"))

(general-define-key
 :prefix my-leader-key
 :infix "/"
 ""  '(:ignore t :wk "search")
 "/" '(swiper                :wk "Swiper")
 "i" '(imenu                 :wk "Imenu")
 "I" '(imenu-anywhere        :wk "Imenu across buffers")
 "g" '(counsel-rg            :wk "Grep files")
 "G" '(counsel-projectile-rg :wk "Grep project files"))

(general-define-key
 :prefix my-leader-key
 :infix "="
 ""  '(:ignore t :wk "diff")
 "b" '(ediff-buffers          :wk "Buffers")
 "B" '(ediff-buffers3         :wk "Buffers (3-way)")
 "c" '(compare-windows        :wk "Compare windows")
 "=" '(ediff-files            :wk "Files")
 "f" '(ediff-files            :wk "Files")
 "F" '(ediff-files3           :wk "Files (3-way)")
 "r" '(ediff-revision         :wk "Compare versions")
 "p" '(ediff-patch-file       :wk "Patch file")
 "P" '(ediff-patch-buffer     :wk "Patch buffer")
 "l" '(ediff-regions-linewise :wk "Linewise")
 "w" '(ediff-regions-wordwise :wk "Wordwise"))

(general-define-key
 :prefix my-leader-key
 :infix "b"
 ""  '(:ignore t :wk "buffer")
 "[" '(previous-buffer            :wk "Previous buffer")
 "]" '(next-buffer                :wk "Next buffer")
 "b" '(switch-to-buffer           :wk "Switch buffer")
 "k" '(kill-buffer                :wk "Kill buffer")
 "n" '(evil-buffer-new            :wk "New empty buffer")
 "m" '(view-echo-area-messages    :wk "Messages buffer")
 "o" '(kill-other-buffers         :wk "Kill other buffers")
 "s" '(save-buffer                :wk "Save buffer")
 "S" '(sudo-edit                  :wk "Sudo edit this file")
 "x" '(toggle-scratch-buffer      :wk "Scratch buffer")
 "z" '(bury-buffer                :wk "Bury buffer"))

(general-define-key
 :prefix my-leader-key
 :infix "c"
 ""  '(:ignore t :wk "code")
 "." '(editorconfig-apply         :wk "Apply editorconfig")
 "c" '(recompile                  :wk "Recompile")
 "C" '(projectile-compile-project :wk "Compile")
 "d" '(counsel-dash-at-point      :wk "Lookup documentation at point")
 "D" '(counsel-dash               :wk "Lookup documentation")
 "e" '(repl-eval                  :wk "Evaluate code")
 "f" '(editorconfig-format-buffer :wk "Reformat")
 "o" '(imenu-list-minor-mode      :wk "Outline")
 "p" '(source-peek                :wk "Peek definition")
 "r" '(repl                       :wk "Open REPL")
 "t" '(retab-buffer               :wk "Retab")
 "x" '(flycheck-list-errors       :wk "List errors"))

(general-define-key
 :prefix my-leader-key
 :infix "d"
 ""  '(:ignore t :wk "docker")
 "c" '(docker-containers :wk "Docker containers")
 "i" '(docker-images     :wk "Docker images")
 "n" '(docker-networks   :wk "Docker networks")
 "v" '(docker-volumes    :wk "Docker volumes"))

(general-define-key
 :prefix my-leader-key
 :infix "f"
 ""  '(:ignore t :wk "file")
 "." '(find-file                              :wk "Find file")
 "/" '(projectile-find-file                   :wk "Find file in project")
 "?" '(counsel-file-jump                      :wk "Find file here")
 "a" '(find-implementation-or-test-file       :wk "Find implementation/test file")
 "A" '(projectile-find-other-file             :wk "Find other file")
 "c" '(editorconfig-find-current-editorconfig :wk "Open project editorconfig")
 "g" '(counsel-rg                             :wk "Grep files")
 "G" '(counsel-projectile-rg                  :wk "Grep project files")
 "r" '(recentf                                :wk "Recent files")
 "R" '(projectile-recentf                     :wk "Recent project files")
 "t" '(counsel-tramp                          :wk "Find TRAMP file"))

(general-define-key
 :prefix my-leader-key
 :infix "g"
 ""  '(:ignore t :wk "git")
 "[" '(diff-hl-previous-hunk       :wk "Previous hunk")
 "]" '(diff-hl-next-hunk           :wk "Next hunk")
 "b" '(magit-blame                 :wk "Blame")
 "B" '(vcs-git-browse              :wk "Browse")
 "c" '(magit-clone                 :wk "Clone")
 "f" '(magit-file-popup            :wk "File popup")
 "I" '(vcs-git-browse-issues       :wk "Browse issues")
 "l" '(magit-log-buffer-file       :wk "Log")
 "m" '(git-messenger:popup-message :wk "Popup message")
 "p" '(magit-pull                  :wk "Pull")
 "r" '(diff-hl-revert-hunk         :wk "Revert hunk")
 "R" '(vc-revert                   :wk "Revert buffer")
 "s" '(magit-status                :wk "Status")
 "t" '(git-timemachine-toggle      :wk "Time machine"))

(general-define-key
 :prefix my-leader-key
 :infix "h"
 ""  '(:ignore t :wk "help")
 "h" '(:keymap help-map)
 "a" '(apropos                 :wk "Apropos")
 "d" '(counsel-dash            :wk "Documentation")
 "i" '(info                    :wk "Info")
 "l" '(find-library            :wk "Find library")
 "s" '(+suggest-popup          :wk "Suggest functions")
 "c" '(helpful-command         :wk "Describe command")
 "C" '(describe-char           :wk "Describe char")
 "f" '(describe-function       :wk "Describe function")
 "k" '(helpful-key             :wk "Describe key")
 "m" '(helpful-macro           :wk "Describe macro")
 "M" '(describe-mode           :wk "Describe mode")
 "v" '(helpful-variable        :wk "Describe variable")
 "f" '(helpful-function        :wk "Describe function")
 "F" '(describe-face           :wk "Describe face")
 "'" '(what-cursor-position    :wk "What face"))

(general-define-key
 :prefix my-leader-key
 :infix "i"
 ""  '(:ignore t :wk "insert")
 "y" '(counsel-yank-pop   :wk "From kill-ring")
 "s" '(yas-insert-snippet :wk "From snippet"))

(general-define-key
 :prefix my-leader-key
 :infix "o"
 ""  '(:ignore t :wk "open")
 "b" '(bibliothek                   :wk "Bibliothek")
 "c" '(calendar                     :wk "Calendar")
 "C" '(display-time-world           :wk "World Time")
 "d" '(deft                         :wk "Notes")
 "f" '(dired-sidebar-toggle-sidebar :wk "File tree")
 "p" '(list-processes               :wk "List processes")
 "P" '(redtick                      :wk "Pomodoro")
 "r" '(camcorder-record             :wk "Record video")
 "s" '(speed-read                   :wk "Speed-reading")
 "t" '(eshell                       :wk "Terminal")
 "T" '(ansi-term                    :wk "ANSI Terminal")
 "u" '(ace-link                     :wk "Open link")
 "w" '(eww                          :wk "Browser")
 "x" '(re-builder                   :wk "Browser"))

(general-define-key
 :prefix my-leader-key
 :infix "r"
 ""  '(:ignore t :wk "REPL")
 "c" '(inf-crystal  :wk "Crystal")
 "e" '(elixir-repl  :wk "Elixir")
 "g" '(go-repl      :wk "Golang")
 "h" '(haskell-repl :wk "Haskell")
 "j" '(java-repl    :wk "Java")
 "k" '(kotlin-repl  :wk "Kotlin")
 "l" '(lua-repl     :wk "Lua")
 "L" '(lisp-repl    :wk "LISP")
 "n" '(nodejs-repl  :wk "NodeJS")
 "p" '(python-repl  :wk "Python")
 "r" '(ruby-repl    :wk "Ruby")
 "s" '(shell-repl   :wk "Shell"))

(general-define-key
 :prefix my-leader-key
 :infix "s"
 ""  '(:ignore t :wk "spell")
 "d" '(adict-guess-dictionary  :wk "Guess dictionary")
 "D" '(adict-change-dictionary :wk "Select dictionary"))

(general-define-key
 :prefix my-leader-key
 :infix "t"
 ""  '(:ignore t :wk "toggle")
 "a" '(goto-address-mode           :wk "Clickable links")
 "c" '(+color-identifiers-toggle   :wk "Colorize identifiers")
 "C" '(rainbow-mode                :wk "Colorize color values")
 "d" '(toggle-debug-on-error       :wk "Debug on error")
 "e" '(eldoc-overlay-mode          :wk "Eldoc inline")
 "f" '(focus-mode                  :wk "Focus")
 "F" '(hs-minor-mode               :wk "Code folding")
 "g" '(indent-guide-mode           :wk "Indent guides")
 "h" '(hl-line-mode                :wk "Line highlight")
 "i" '(aggressive-indent-mode      :wk "Automatic indentation")
 "l" '(display-line-numbers-mode   :wk "Line numbers")
 "L" '(coverlay-toggle-overlays    :wk "Coverage overlays")
 "p" '(redtick-mode                :wk "Pomodoro")
 "q" '(quickrun-autorun-mode       :wk "Auto-run compilation")
 "r" '(ruler-mode                  :wk "Ruler")
 "s" '(flyspell-mode               :wk "Spell-checking")
 "S" '(subword-mode                :wk "Subword")
 "t" '(toggle-truncate-lines       :wk "Truncate lines")
 "v" '(variable-pitch-mode         :wk "Fixed-width/variable-width font")
 "w" '(whitespace-mode             :wk "Display white-space characters")
 "W" '(auto-fill-mode              :wk "Automatic line-wrapping")
 "x" '(flycheck-mode               :wk "Syntax checker"))

(general-define-key
 :prefix my-leader-key
 "SPC" (general-predicate-dispatch #'projectile-switch-project
         :docstring "Find file in project or switch project"
         (projectile-project-p) #'projectile-find-file)
 ","   '(switch-to-buffer                       :wk "Switch to buffer")
 "."   '(find-file                              :wk "Browse files")
 "~"   '(window-toggle-side-windows             :wk "Toggle last popup")
 ":"   '(elisp-repl                             :wk "Emacs Lisp REPL")
 ";"   '(counsel-bookmark                       :wk "Jump to bookmark")
 "RET" '(repl                                   :wk "Open REPL")
 "a"   '(org-agenda     :package org)
 "l"   '(org-store-link :package org)
 "u"   '(universal-argument                     :wk "Universal argument")
 "v"   '(:keymap symbol-overlay-map
                 :package symbol-overlay :wk "overlays")
 "w"   '(:keymap evil-window-map
                 :package evil :wk "window")
 "x"   '(toggle-scratch-buffer                  :wk "Toggle scratch buffer")
 "W" '(:ignore t :wk "workspace"))

(with-eval-after-load 'evil
  ;; Normal state
  (general-define-key
   :keymaps 'normal
   "<C-return>" 'repl-eval
   "gr" 'eval-region
   "gR" 'eval-buffer
   "gt" 'quickrun
   "gV" '+evil-reselect-paste
   "+" '(rotate-text :package rotate-text)
   "-" '(rotate-text-backward :package rotate-text))

  (general-define-key
   :keymaps '(normal motion)
   "]b" 'next-buffer
   "[b" 'previous-buffer
   "]c" 'eir-next-code-line
   "]e" 'next-error
   "[e" 'previous-error
   "]w" 'persp-next
   "[w" 'persp-prev
   "C-t" 'smart-jump-back
   "K"  '(documentation-at-point :wk "Documentation for symbol")
   "g[" '(smart-backward :package smart-forward)
   "g]" '(smart-forward  :package smart-forward)
   "gd" '(smart-jump-go          :wk "Go to definition")
   "gD" '(smart-jump-references  :wk "Find all references")
   "gh" '(documentation-at-point :wk "Documentation for symbol")
   "gp" '(source-peek            :wk "Peek definition")
   "gP" (general-predicate-dispatch nil
          (bound-and-true-p lsp-mode) 'lsp-ui-peek-find-references)
   "zx" 'kill-buffer
   "ZX" 'bury-buffer)

  ;; Visual state
  (general-define-key
   :keymaps 'visual
   "." 'evil-repeat
   "<" '+evil-visual-outdent
   ">" '+evil-visual-indent)

  (general-define-key
   :keymaps 'evil-window-map
   ;; Navigation
   "C-w"     '(ace-window           :wk "Select a window")
   "B"       '(switch-to-minibuffer :wk "Switch to minibuffer")
   ;; Swapping
   "C-S-w"   '(ace-swap-window      :wk "Swap window")
   "z"       '(maximize-window      :wk "Maximize window")
   "Z"       '(zoom-window-zoom     :wk "Zoom window")
   ;; Undo/redo
   "u"       '(winner-undo          :wk "Undo")
   "C-u"     '(winner-undo          :wk "Undo")
   "C-r"     '(winner-redo          :wk "Redo")
   ;; Delete
   "C-C"     '(ace-delete-window    :wk "Select and delete a window"))

  (general-define-key
   :keymaps '(help-mode-map
              elisp-refs-mode-map)
   :states '(normal motion emacs)
   "[[" 'help-go-back
   "]]" 'help-go-forward
   "K"  '(helpful-at-point :package 'helpful)
   "o"  '(ace-link-help    :package 'ace-link))

  (general-define-key
   :keymaps 'package-menu-mode-map
   :states '(normal motion insert emacs)
   "q" 'kill-this-buffer))

(provide 'bindings)
;;; bindings.el ends here
