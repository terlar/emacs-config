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
  :loader :built-in
  :diminish flyspell-mode
  :commands flyspell-mode
  :init
  (setq ispell-programs-name (executable-find "aspell")
        ispell-list-command "--list"
        ispell-extr-args '("--dont-tex-check-comments"))

  (add-hooks-pair 'prog-mode 'flyspell-prog-mode)
  (add-hooks-pair '(text-mode message-mode) 'flyspell-mode))

(req-package flyspell-correct-ivy
  :require flyspell ivy
  :after flyspell
  :commands
  (flyspell-correct-word-generic
   flyspell-correct-previous-word-generic))

;; Automatically infer dictionary
(req-package auto-dictionary
  :commands
  (adict-change-dictionary
   adict-guess-dictionary))

(provide 'feature-spellcheck)
;;; feature-spellcheck.el ends here
