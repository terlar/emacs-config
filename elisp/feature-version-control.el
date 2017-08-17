;;; feature-version-control.el --- Version Control

;;; Commentary:
;; Tracking your changes.

;;; Code:

;;;
;; Settings
(setq vc-make-backup-files nil)

;;;
;; Packages
(use-package gitconfig-mode
  :mode "/\\.?git/?config$"
  :mode "/\\.gitmodules$")

(use-package gitignore-mode
  :mode "/\\.gitignore$")

(use-package diff-hl
  :commands (global-diff-hl-mode
             diff-hl-magit-post-refresh)
  :preface
  (autoload 'diff-hl-flydiff-mode "diff-hl-flydiff")
  (autoload 'diff-hl-dired-mode "diff-hl-dired")
  :init
  (add-hooks-pair 'after-init
                  '(global-diff-hl-mode
                    diff-hl-flydiff-mode))

  (add-hooks-pair 'dired-mode 'diff-hl-dired-mode)
  (with-eval-after-load "magit"
    (add-hooks-pair 'magit-post-refresh 'diff-hl-magit-post-refresh)))

(use-package magit
  :commands (global-magit-file-mode magit-status magit-blame)
  :config
  (setq magit-log-buffer-file-locked t
        magit-refs-show-commit-count 'all
        magit-save-repository-buffers 'dontask)

  ;; Unset pager as it is not supported properly inside emacs.
  (setenv "GIT_PAGER" "")

  (global-magit-file-mode +1))

;; Pop-up commit message for current line
(use-package git-messenger
  :commands git-messenger:popup-message)

(use-package git-timemachine
  :after magit
  :commands (git-timemachine git-timemachine-toggle)
  :config
  (require 'magit-blame))

(use-package git-link
  :commands (git-link
             git-link-commit git-link-homepage
             git-link--remote-dir
             git-link--remote-host
             git-link--select-remote))

(use-package evil-magit
  :after evil
  :init (setq-default evil-magit-want-horizontal-movement t))

(use-package magit-gh-pulls
  :after magit
  :init (add-hooks-pair 'magit-mode 'turn-on-magit-gh-pulls))

;;;
;; Autoloads

;;;###autoload
(defun vcs-root ()
  "Get git url root."
  (let ((remote (git-link--select-remote)))
    (if (git-link--remote-host remote)
        (format "https://%s/%s"
                (git-link--remote-host remote)
                (git-link--remote-dir remote))
      (error  "Remote `%s' is unknown or contains an unsupported URL" remote))))

;;;###autoload
(defun vcs/git-browse ()
  "Open the website for the current version controlled file.
Fallback to repository root."
  (interactive)
  (let ((git-link-open-in-browser t))
    (call-interactively 'git-link)))

;;;###autoload
(defun vcs/git-browse-issues ()
  "Open the github issues page for current repo."
  (interactive)
  (if-let (root (vcs-root))
      (browse-url (concat root "/issues"))
    (user-error "No git root found!")))

(provide 'feature-version-control)
;;; feature-version-control.el ends here
