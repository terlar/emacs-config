;;; base-popups.el --- Popups configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Pop, pop, pop.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Settings

(add-to-list 'display-buffer-alist
             '("^*Async Shell Command*" . (display-buffer-no-window)))

(set-popup-buffer
 (rx bos "*Messages*" eos)
 (rx bos "*Warnings*" eos)
 (rx bos "*Backtrace*" eos)
 (rx bos "*Compile-Log*" eos)
 (rx bos "*Pp Eval Output*" eos)
 (rx bos "*Shell Command Output*" eos)
 (rx bos "*Org-Babel Error Output*" eos)
 (rx bos "*Buffer List*" eos)
 (rx bos "*Process List*" eos)
 (rx bos "*Help*" eos)
 (rx bos "*Man " (one-or-more anything) "*" eos)
 (rx bos "*Finder*" eos)
 (rx bos "*General Keybindings*" eos)
 (rx bos "*Colors*" eos)
 (rx bos "*compilation*" eos)
 (rx bos "*xref*" eos)
 (rx bos "*ert*" eos)
 (rx bos "*eshell*" eos)
 (rx bos "*scratch*" eos)
 (rx bos "COMMIT_EDITMSG" eos))

(provide 'base-popups)
;;; base-popups.el ends here
