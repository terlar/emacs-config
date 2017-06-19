;;; feature-spellcheck.el --- Spellchecking

;;; Commentary:
;; Catching your typos.

;;; Code:
(require 'base-vars)

(use-package flyspell ; builtin
  :commands flyspell-mode
  :bind
  (:map
   flyspell-mode-map
   ("C-c s" . flyspell-correct-word-generic)
   ("C-c S" . flyspell-correct-previous-word-generic))
  :init
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (dolist (hook '(text-mode-hook message-mode-hook))
    (add-hook hook #'flyspell-mode))
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
  :bind
  (:map
   flyspell-mode-map
   ("C-c d" . adict-guess-dictionary)
   ("C-c D" . adict-change-dictionary)))

(provide 'feature-spellcheck)
;;; feature-spellcheck.el ends here
