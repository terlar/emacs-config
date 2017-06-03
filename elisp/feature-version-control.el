;;; feature-version-control.el --- Version Control

;;; Commentary:
;; Tracking your changes.

;;; Code:
(setq vc-make-backup-files nil)

(use-package gitconfig-mode
  :mode "/\\.?git/?config$"
  :mode "/\\.gitmodules$")

(use-package gitignore-mode
  :mode "/\\.gitignore$")

(use-package diff-hl
  :after magit
  :commands (global-diff-hl-mode
             diff-hl-flydiff-mode diff-hl-dired-mode)
  :init
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)

  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (add-hook 'after-init-hook
            #'(lambda ()
                (global-diff-hl-mode +1)
                (diff-hl-flydiff-mode +1))))

(use-package magit
  :commands (global-magit-file-mode magit-status magit-blame)
  :config
  (global-magit-file-mode +1))

(use-package git-timemachine
  :after magit
  :commands (git-timemachine git-timemachine-toggle)
  :config
  (require 'magit-blame))

(use-package git-link
  :commands (git-link git-link-commit git-link-homepage))

(use-package evil-magit
  :after magit
  :init (setq-default evil-magit-want-horizontal-movement t))

(use-package magit-gh-pulls
  :after magit
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

(provide 'feature-version-control)
;;; feature-version-control.el ends here
