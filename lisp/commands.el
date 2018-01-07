;;; commands.el --- Evil mode commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Welcome back commander...

;;; Code:

(with-eval-after-load 'evil
  (autoload 'vcs/git-browse "feature-version-control" nil t)
  (autoload 'vcs/git-browse-issues "feature-version-control" nil t)
  (autoload 'magit-status "magit" nil t)
  (autoload 'magit-stage "magit" nil t)
  (autoload 'magit-unstage "magit" nil t)
  (autoload 'magit-blame "magit" nil t)

  (evil-ex-define-cmd "gbrowse"     #'vcs/git-browse)        ; show file in github/gitlab
  (evil-ex-define-cmd "gissues"     #'vcs/git-browse-issues) ; show github issues
  (evil-ex-define-cmd "git"         #'magit-status)          ; open magit status window
  (evil-ex-define-cmd "gstage"      #'magit-stage)
  (evil-ex-define-cmd "gunstage"    #'magit-unstage)
  (evil-ex-define-cmd "gblame"      #'magit-blame))

(provide 'commands)
;;; commands.el ends here
