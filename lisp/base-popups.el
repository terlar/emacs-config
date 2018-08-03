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
 (rx bos "*" (one-or-more anything) "*" eos))

(provide 'base-popups)
;;; base-popups.el ends here
