;;; base-popups.el --- Popups configuration

;;; Commentary:
;; Pop, pop, pop.

;;; Code:
(use-package shackle :demand t
  :init
  (setq-default
   shackle-default-alignment 'below
   shackle-default-size 8
   shackle-rules
   '((apropos-mode :size 0.3 :align t :select t)
     (Buffer-menu-mode :size 20 :align t :select t)
     (grep-mode :size 25 :align t :noselect t)
     (profiler-report-mode :size 0.3 :align t :select t :regexp t)
     (special-mode :align t :noselect t)
     ("*info*" :size 0.5 :align t :select t)
     ("*Backtrace*" :size 20 :align t :noselect t)
     ("*Warnings*" :size 5 :align t :noselect t)
     ("*Messages*" :size 12 :align t :noselect t)
     ("*Help*" :size 0.3 :align t :select t)
     ("^\\*.*Shell Command.*\\*$" :regexp t :size 20 :align t :noselect t)
     ("^\\*"  :regexp t :align t :noselect t)
     ("^ \\*" :regexp t :size 12 :align t :noselect t)))
  :config
  (if (display-graphic-p)
      (shackle-mode +1)
    (add-hook 'after-make-frame-functions #'shackle-mode)
    (add-hook 'after-init-hook #'shackle-mode)))

(with-eval-after-load 'neotree
  (push '(" *NeoTree*" :size 25 :align left :select t) shackle-rules)

  (defun my|neotree-display (buffer _alist)
    (let ((win (shackle-display-buffer
                buffer nil '(:align 'left :size 25))))
      (setq neo-global--buffer (window-buffer win)
            neo-global--window win)))
  (setq neo-display-action '(my|neotree-display)))

(provide 'base-popups)
;;; base-popups.el ends here
