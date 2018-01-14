;;; feature-spellcheck.el --- Spellchecking -*- lexical-binding: t; -*-

;;; Commentary:
;; Catching your typos.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package)
  (require 'base-keybinds))

;;;
;; Packages

(req-package flyspell
  :diminish flyspell-mode
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode)
  (message-mode . flyspell-mode)
  :init
  (setq ispell-programs-name (executable-find "aspell")
        ispell-list-command "--list"
        ispell-extr-args '("--dont-tex-check-comments")))

(req-package flyspell-correct-ivy
  :general
  (:keymaps 'motion
            "]S" 'flyspell-correct-word-generic
            "[S" 'flyspell-correct-previous-word-generic))

;; Automatically infer dictionary
(req-package auto-dictionary
  :commands
  (adict-change-dictionary
   adict-guess-dictionary))

(provide 'feature-spellcheck)
;;; feature-spellcheck.el ends here
