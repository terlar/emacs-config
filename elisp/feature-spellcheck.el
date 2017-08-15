;;; feature-spellcheck.el --- Spellchecking

;;; Commentary:
;; Catching your typos.

;;; Code:
(require 'base-vars)
(require 'base-keybinds)

(use-package flyspell ; builtin
  :commands flyspell-mode
  :general
  (:keymaps 'flyspell-mode-map
            "C-c s" 'flyspell-correct-word-generic
            "C-c S" 'flyspell-correct-previous-word-generic)
  :init
  (add-hooks-pair 'prog-mode 'flyspell-prog-mode)
  (add-hooks-pair '(text-mode message-mode) 'flyspell-mode)
  :config
  (setq-default ispell-programs-name (executable-find "aspell")
                ispell-list-command "--list"
                ispell-extr-args '("--dont-tex-check-comments")))

(cond ((eq my-completion-system 'ivy)
       (use-package flyspell-correct-ivy))
      ((eq my-completion-system 'helm)
       (use-package flyspell-correct-helm))
      (t
       (use-package flyspell-correct-popup)))

(use-package flyspell-correct
  :commands
  (flyspell-correct-word-generic
   flyspell-correct-previous-word-generic)
  :config
  (cond ((eq my-completion-system 'ivy)
         (require 'flyspell-correct-ivy))
        ((eq my-completion-system 'helm)
         (require 'flyspell-correct-helm))
        (t
         (require 'flyspell-correct-popup)
         (setq flyspell-correct-auto-delay 0.8))))

;; Automatically infer dictionary
(use-package auto-dictionary
  :commands (adict-change-dictionary adict-guess-dictionary)
  :general
  (:keymaps 'flyspell-mode-map
            "C-c d" 'adict-guess-dictionary
            "C-c D" 'adict-change-dictionary))

(provide 'feature-spellcheck)
;;; feature-spellcheck.el ends here
