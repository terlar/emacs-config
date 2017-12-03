;;; lang-org.el --- Org mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Org mode is for keeping notes, maintaining TODO lists, planning projects, and
;; authoring documents with a fast and effective plain-text system.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-lib)
  (require 'base-keybinds))

;;;
;; Packages

(req-package ob-elixir)
(req-package ob-go)
(req-package ob-http)
(req-package ob-restclient)
(req-package ob-rust)
(req-package ob-translate)

(req-package org
  :hook
  (org-mode . readable-mode)
  (org-babel-after-execute . org-redisplay-inline-images)
  :general
  (:keymaps
   'org-mode-map
   :states 'normal
   "o" '+org-evil-open-below
   "O" '+org-evil-open-above
   "<<" 'org-metaleft
   ">>" 'org-metaright)
  :init
  (setq org-confirm-babel-evaluate nil
        org-hide-block-startup t
        org-hide-emphasis-markers t
        org-special-ctrl-a/e t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-tag-alist '(("@work"  . ?w)
                        ("@home"  . ?h)
                        ("laptop" . ?l))
        org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
  :config
  (set-evil-state-change
   'org-mode
   :on-insert (lambda ()
                (setq org-hide-block-startup nil
                      org-hide-emphasis-markers nil)
                (font-lock-fontify-block))
   :on-normal (lambda ()
                (setq org-hide-block-startup t
                      org-hide-emphasis-markers t)
                (font-lock-fontify-block)))

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
     (translate  . t))))

(req-package org-bullets
  :hook (org-mode . org-bullets-mode))

(req-package org-preview-html
  :commands org-preview-html-mode)

;;;###autoload
(defun +org-evil-open-above (count)
  "Insert a new line above point and switch to Insert state.
The insertion will be repeated COUNT times.
It acts in the same way as `org-meta-return'."
  (interactive "p")
  (unless (eq evil-want-fine-undo t)
    (evil-start-undo-step))

  (back-to-indentation)
  (org-meta-return)
  (evil-move-end-of-line)

  (setq evil-insert-count count)
  (setq evil-insert-lines t)
  (setq evil-insert-vcount nil)
  (evil-insert-state 1))

;;;###autoload
(defun +org-evil-open-below (count)
  "Insert a new line below point and switch to Insert state.
The insertion will be repeated COUNT times.
It acts in the same way as `org-meta-return'."
  (interactive "p")
  (unless (eq evil-want-fine-undo t)
    (evil-start-undo-step))
  (push (point) buffer-undo-list)

  (evil-move-end-of-line)
  (org-meta-return)

  (setq evil-insert-count count)
  (setq evil-insert-lines t)
  (setq evil-insert-vcount nil)
  (evil-insert-state 1))

(provide 'lang-org)
;;; lang-org.el ends here
