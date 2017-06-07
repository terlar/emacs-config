;;; commands.el --- Evil mode commands

;;; Commentary:
;; Welcome back commander...

;;; Code:
(require 'evil)

(evil-ex-define-cmd "gbrowse"     #'vcs/git-browse)        ; show file in github/gitlab
(evil-ex-define-cmd "gissues"     #'vcs/git-browse-issues) ; show github issues
(evil-ex-define-cmd "git"         #'magit-status)          ; open magit status window
(evil-ex-define-cmd "gstage"      #'magit-stage)
(evil-ex-define-cmd "gunstage"    #'magit-unstage)
(evil-ex-define-cmd "gblame"      #'magit-blame)

(provide 'commands)
;;; commands.el ends here
