;;; feature-version-control.el --- Version Control

;;; Commentary:
;; Tracking your changes.

;;; Code:

;;;
;; Settings

(eval-when-compile
  (require 'base-package)
  (require 'base-keybinds))

(setq vc-make-backup-files nil)

;;;
;; Packages

(req-package vc-annotate
  :general
  (:keymaps 'vc-annotate-mode-map :states '(normal motion emacs)
            "d"   'vc-annotate-show-diff-revision-at-line
            "D"   'vc-annotate-show-changeset-diff-revision-at-line
            "SPC" 'vc-annotate-show-log-revision-at-line
            "]]"  'vc-annotate-next-revision
            "[["  'vc-annotate-prev-revision
            "TAB" 'vc-annotate-toggle-annotation-visibility
            "RET" 'vc-annotate-find-revision-at-line))

(req-package gitconfig-mode
  :mode "/\\.?git/?config$"
  :mode "/\\.gitmodules$")

(req-package gitignore-mode
  :mode "/\\.gitignore$")

(req-package diff-hl
  :demand t
  :general
  (:keymaps 'motion
            "[d" 'diff-hl-previous-hunk
            "]d" 'diff-hl-next-hunk)
  :hook
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :init
  (autoload 'diff-hl-flydiff-mode "diff-hl-flydiff" nil t)
  (autoload 'diff-hl-dired-mode "diff-hl-dired" nil t)
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
  :commands
  (git-timemachine
   git-timemachine-toggle)
  :general
  (:keymaps 'git-timemachine-mode :states '(normal insert emacs) :definer 'minor-mode
            "p" 'git-timemachine-show-previous-revision
            "n" 'git-timemachine-show-next-revision
            "g" 'git-timemachine-show-nth-revision
            "q" 'git-timemachine-quit
            "w" 'git-timemachine-kill-abbreviated-revision
            "W" 'git-timemachine-kill-revision
            "b" 'git-timemachine-blame)
  :config
  (require 'magit-blame))

(req-package git-link
  :commands
  (git-link
   git-link-commit git-link-homepage
   git-link--remote-dir
   git-link--remote-host
   git-link--select-remote))

(req-package magit-gh-pulls
  :hook (magit-mode . magit-gh-pulls-mode))

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
