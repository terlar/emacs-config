;;; feature-version-control.el --- Version Control

;;; Commentary:
;; Tracking your changes.

;;; Code:

;;;
;; Settings

(setq vc-make-backup-files nil)

;;;
;; Packages

(use-package vc-annotate :ensure nil
  :general
  (:keymaps 'vc-annotate-mode-map :states '(normal motion emacs)
            "d"   'vc-annotate-show-diff-revision-at-line
            "D"   'vc-annotate-show-changeset-diff-revision-at-line
            "SPC" 'vc-annotate-show-log-revision-at-line
            "]]"  'vc-annotate-next-revision
            "[["  'vc-annotate-prev-revision
            "TAB" 'vc-annotate-toggle-annotation-visibility
            "RET" 'vc-annotate-find-revision-at-line))

(use-package gitconfig-mode
  :mode "/\\.?git/?config$"
  :mode "/\\.gitmodules$")

(use-package gitignore-mode
  :mode "/\\.gitignore$")

(use-package magit
  :defer 2
  :init
  (setq magit-log-buffer-file-locked t
        magit-refs-show-commit-count 'all
        magit-save-repository-buffers 'dontask)

  ;; Unset pager as it is not supported properly inside emacs.
  (setenv "GIT_PAGER" "")
  :config
  (set-popup-buffer (rx bos "magit" (one-or-more anything) eos))

  (global-magit-file-mode 1))

;; Popup commit message for current line
(use-package git-messenger
  :commands git-messenger:popup-message)

(use-package git-timemachine
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

(use-package git-link
  :commands
  (git-link
   git-link-commit git-link-homepage
   git-link--remote-dir
   git-link--remote-host
   git-link--select-remote))

(use-package magit-gh-pulls
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
