;;; base-keybinds.el --- Key binding support -*- lexical-binding: t; -*-

;;; Commentary:
;; How to interact with Emacs.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Config

;; Leader key
(defvar my-leader-key "C-c"
  "The key used for most custom operations.")
(defvar my-local-leader-key "C-c m"
  "The key used for major mode operations.")
(defvar my-normal-leader-key "SPC"
  "The key used for most custom operations in `evil-normal-state'.")

;;;
;; Packages

(req-package general :force t :demand t)

(req-package which-key :force t
  :diminish which-key-mode
  :commands
  (which-key-mode
   which-key-key-order-alpha)
  :defer 1
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 5
        which-key-idle-delay 0.5)
  :config
  (push '(("<\\([[:alnum:]-]+\\)>" . nil) . ("\\1" . nil)) which-key-replacement-alist)
  (push '(("\\`\\?\\?\\'" . nil) . ("λ" . nil)) which-key-replacement-alist)
  (push '(("<up>"    . nil) . ("↑" . nil)) which-key-replacement-alist)
  (push '(("<right>" . nil) . ("→" . nil)) which-key-replacement-alist)
  (push '(("<down>"  . nil) . ("↓" . nil)) which-key-replacement-alist)
  (push '(("<left>"  . nil) . ("←" . nil)) which-key-replacement-alist)
  (push '(("SPC" . nil) . ("␣" . nil)) which-key-replacement-alist)
  (push '(("TAB" . nil) . ("↹" . nil)) which-key-replacement-alist)
  (push '(("RET" . nil) . ("⏎" . nil)) which-key-replacement-alist)
  (push '(("DEL" . nil) . ("⌫" . nil)) which-key-replacement-alist)
  (push '(("deletechar" . nil) . ("⌦" . nil)) which-key-replacement-alist)

  (which-key-add-key-based-replacements
    "C-c !"   "flycheck"
    "C-c @"   "outline"
    "C-c p"   "project")

  (which-key-setup-side-window-bottom)

  (which-key-mode 1))

(provide 'base-keybinds)
;;; base-keybinds.el ends here
