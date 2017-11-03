;;; lang-org.el --- Org mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Org mode is for keeping notes, maintaining TODO lists, planning projects, and
;; authoring documents with a fast and effective plain-text system.

;;; Code:

;;;
;; Packages

(use-package ob-elixir)
(use-package ob-go)
(use-package ob-http)
(use-package ob-restclient)
(use-package ob-rust)
(use-package ob-translate)

(use-package org
  :defines org-plantuml-jar-path
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((calc       . t)
     (clojure    . t)
     (css        . t)
     (dot        . t)
     (elixir     . t)
     (emacs-lisp . t)
     (haskell    . t)
     (http       . t)
     (js         . t)
     (plantuml   . t)
     (python     . t)
     (ruby       . t)
     (rust       . t)
     (scala      . t)
     (shell      . t)
     (translate  . t)))

  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-plantuml-jar-path "/opt/plantuml/plantuml.jar")

  (add-hooks-pair 'org-babel-after-execute 'org-redisplay-inline-images))

(use-package org-preview-html
  :commands org-preview-html-mode)

(provide 'lang-org)
;;; lang-org.el ends here
