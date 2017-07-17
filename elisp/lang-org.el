;;; lang-org.el --- Org mode

;;; Commentary:
;; Org mode is for keeping notes, maintaining TODO lists, planning projects, and
;; authoring documents with a fast and effective plain-text system.

;;; Code:

;;;
;; Packages

(use-package org
  :defines org-plantuml-jar-path
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((calc       . t)
     (clojure    . t)
     (css        . t)
     (dot        . t)
     (emacs-lisp . t)
     (haskell    . t)
     (js         . t)
     (plantuml   . t)
     (python     . t)
     (ruby       . t)
     (scala      . t)
     (shell      . t)))

  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-plantuml-jar-path "/opt/plantuml/plantuml.jar")

  (add-hooks-pair 'org-babel-after-execute 'org-redisplay-inline-images))

(provide 'lang-org)
;;; lang-org.el ends here
