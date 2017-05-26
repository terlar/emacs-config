;;; base-keybinds.el --- Key binding configuration
;;; Commentary:
;;; How to interact with Emacs.
;;; Code:
(require 'bind-key)

(use-package which-key :demand t
  :config
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 5)

  ;; Embolden local bindings
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (which-key-mode +1))

;; Text Scaling
(bind-key "<C-mouse-4>" #'text-scale-decrease)
(bind-key "<C-mouse-5>" #'text-scale-increase)
(bind-key "C--" #'default-text-scale-decrease)
(bind-key "C-+" #'default-text-scale-increase)

;; Consistent tag jump
(bind-key [remap evil-jump-to-tag] #'projectile-find-tag)
(bind-key [remap find-tag] #'projectile-find-tag)

(provide 'base-keybinds)
;;; base-keybinds.el ends here
