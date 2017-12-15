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
;; Functions

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

;;;
;; Packages

(use-package ob-elixir)
(use-package ob-go)
(use-package ob-http)
(use-package ob-restclient)
(use-package ob-rust)
(use-package ob-translate)

(use-package org
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
  :preface
  (defun +org-mode-on-insert-state-entry ()
    (setq org-hide-block-startup nil
          org-hide-emphasis-markers nil)
    (font-lock-fontify-block))

  (defun +org-mode-on-normal-state-entry ()
    (setq org-hide-block-startup t
          org-hide-emphasis-markers t)
    (font-lock-fontify-block))

  (defun +org-edit-src-save-and-exit ()
    "Save parent buffer with current state source-code buffer and exit."
    (interactive)
    (org-edit-src-save)
    (org-edit-src-exit))
  :init
  (setq org-confirm-babel-evaluate nil
        org-hide-block-startup t
        org-hide-emphasis-markers t
        org-special-ctrl-a/e t
        org-edit-src-content-indentation 0
        org-src-preserve-indentation nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-tag-alist '(("@work"  . ?w)
                        ("@home"  . ?h)
                        ("laptop" . ?l))
        org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
  :config
  (evil-stateful-set-state-entry
   'org-mode
   :on-insert #'+org-mode-on-insert-state-entry
   :on-normal #'+org-mode-on-normal-state-entry)

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

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-preview-html
  :commands org-preview-html-mode)

(provide 'lang-org)
;;; lang-org.el ends here
