;;; feature-version-control.el --- Version Control

;;; Commentary:
;; Tracking your changes.

;;; Code:

;;;
;; Settings

(eval-when-compile
  (require 'base-package))

(setq vc-make-backup-files nil)

;;;
;; Packages

(req-package gitconfig-mode
  :mode "/\\.?git/?config$"
  :mode "/\\.gitmodules$")

(req-package gitignore-mode
  :mode "/\\.gitignore$")

(req-package diff-hl
  :demand t
  :init
  (autoload 'diff-hl-flydiff-mode "diff-hl-flydiff" nil t)
  (autoload 'diff-hl-dired-mode "diff-hl-dired" nil t)

  (add-hooks-pair 'dired-mode 'diff-hl-dired-mode)
  (with-eval-after-load "magit"
    (add-hooks-pair 'magit-post-refresh 'diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1))

(req-package magit
  :demand t
  :init
  (setq magit-log-buffer-file-locked t
        magit-refs-show-commit-count 'all
        magit-save-repository-buffers 'dontask)

  ;; Unset pager as it is not supported properly inside emacs.
  (setenv "GIT_PAGER" "")
  :config
  (global-magit-file-mode 1))

;; Popup commit message for current line
(req-package git-messenger
  :commands git-messenger:popup-message)

(req-package git-timemachine
  :require magit
  :commands
  (git-timemachine
   git-timemachine-toggle)
  :config
  (require 'magit-blame))

(req-package git-link
  :commands
  (git-link
   git-link-commit git-link-homepage
   git-link--remote-dir
   git-link--remote-host
   git-link--select-remote))

(req-package evil-magit
  :require evil magit
  :after evil
  :init
  (setq evil-magit-want-horizontal-movement t))

(req-package magit-gh-pulls
  :require magit
  :after magit
  :commands turn-on-magit-gh-pulls
  :config
  (add-hooks-pair 'magit-mode 'turn-on-magit-gh-pulls))

;;;
;; Autoloads

;;;###autoload
(defun +vcs-root ()
  "Get git url root."
  (let ((remote (git-link--select-remote)))
    (if (git-link--remote-host remote)
        (format "https://%s/%s"
                (git-link--remote-host remote)
                (git-link--remote-dir remote))
      (error  "Remote `%s' is unknown or contains an unsupported URL" remote))))

;;;###autoload
(defun vcs-git-browse ()
  "Open the website for the current version controlled file.
Fallback to repository root."
  (interactive)
  (let ((git-link-open-in-browser t))
    (call-interactively 'git-link)))

;;;###autoload
(defun vcs-git-browse-issues ()
  "Open the github issues page for current repo."
  (interactive)
  (if-let* ((root (+vcs-root))
            (browse-url (concat root "/issues")))
      (user-error "No git root found!")))

(provide 'feature-version-control)
;;; feature-version-control.el ends here
