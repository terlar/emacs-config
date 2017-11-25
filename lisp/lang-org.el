;;; lang-org.el --- Org mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Org mode is for keeping notes, maintaining TODO lists, planning projects, and
;; authoring documents with a fast and effective plain-text system.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-lib))

;;;
;; Packages

(req-package ob-elixir)
(req-package ob-go)
(req-package ob-http)
(req-package ob-restclient)
(req-package ob-rust)
(req-package ob-translate)

(req-package org
  :init
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
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

  (add-hooks-pair 'org-mode 'readability-mode)
  (add-hooks-pair 'org-babel-after-execute 'org-redisplay-inline-images))

(req-package org-preview-html
  :commands org-preview-html-mode)

(provide 'lang-org)
;;; lang-org.el ends here
