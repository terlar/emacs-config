;;; feature-debug.el --- Debug -*- lexical-binding: t; -*-

;;; Commentary:
;; Locate those bugs.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-keybinds))

;;;
;; Packages

(req-package debug
  :general
  (:keymaps 'debugger-mode-map
            :states '(normal motion insert emacs)
            "RET" 'debug-help-follow
            "n"   'debugger-step-through))

(req-package realgud
  :commands
  (realgud:gdb
   realgud:trepanjs
   realgud:bashdb
   realgud:zshdb)
  :general
  (:keymaps 'realgud:shortkey-mode-map :states 'normal
            "j" 'evil-next-line
            "k" 'evil-previous-line
            "h" 'evil-backward-char
            "l" 'evil-forward-char
            "c" 'realgud:cmd-continue)
  (:keymaps 'realgud:shortkey-mode-map :states 'motion
            "n" 'realgud:cmd-next
            "b" 'realgud:cmd-break
            "B" 'realgud:cmd-clear))

(provide 'feature-debug)
;;; feature-debug.el ends here
