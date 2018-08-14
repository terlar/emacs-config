;;; org.el --- Org mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Org mode is for keeping notes, maintaining TODO lists, planning projects, and
;; authoring documents with a fast and effective plain-text system.

;;; Code:

(use-package org :pin org
  :hook
  (org-mode . readable-mode)
  (org-mode . +org-setup-babel)
  (org-babel-after-execute . org-redisplay-inline-images)
  :general
  (:keymaps 'org-mode-map :major-modes t
            "C-c SPC" 'nil)
  (:keymaps 'org-mode-map :states 'normal
            "<<" 'org-metaleft
            ">>" 'org-metaright
            "RET" 'org-edit-src-code)
  (:keymaps 'org-src-mode-map
            "C-c C-k" 'org-edit-src-abort
            "C-c C-c" 'org-edit-src-exit)
  (:keymaps 'org-src-mode :states 'normal :definer 'minor-mode
            "ZQ" 'org-edit-src-abort
            "ZZ" 'org-edit-src-exit)
  :preface
  (defun +org-setup-babel ()
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
       (restclient . t)
       (ruby       . t)
       (rust       . t)
       (scala      . t)
       (shell      . t)
       (translate  . t))))
  :init
  (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
  :config
  (set-on-evil-state 'org-mode 'insert
                     (setq org-hide-block-startup nil
                           org-hide-emphasis-markers nil)
                     (org-bullets-mode 0)
                     (font-lock-fontify-block))
  (set-on-evil-state 'org-mode 'normal
                     (setq org-hide-block-startup t
                           org-hide-emphasis-markers t)
                     (org-bullets-mode 1)
                     (font-lock-fontify-block))

  (defun +org-display-inline-images--with-color-theme-background-color (args)
    "Specify background color of Org-mode inline image through modify `ARGS'."
    (let* ((file (car args))
           (type (cadr args))
           (data-p (caddr args))
           (props (cdddr args)))
      ;; get this return result style from `create-image'
      (append (list file type data-p)
              (list :background (face-background 'default))
              props)))

  (advice-add 'create-image :filter-args
              #'+org-display-inline-images--with-color-theme-background-color))

(use-package org-plus-contrib
  :init
  (require 'org-eldoc))

(use-package org-preview-html
  :commands org-preview-html-mode)

(use-package org-noter
  :general
  (:keymaps
   'org-mode-map
   :prefix +local-leader-key
   "n" 'org-noter)
  :commands org-noter)

(req-package ob-elixir)
(req-package ob-go)
(req-package ob-http)
(req-package ob-restclient)
(req-package ob-rust)
(req-package ob-translate)

(provide 'lang-org)
;;; org.el ends here
