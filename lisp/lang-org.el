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

(req-package org :pin org
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
            "RET" 'org-edit-src-code
            "o" '+org-evil-open-below
            "O" '+org-evil-open-above)
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
       (ruby       . t)
       (rust       . t)
       (scala      . t)
       (shell      . t)
       (translate  . t))))
  :init
  (setq org-agenda-files '("~/org")
        org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 0
        org-hide-block-startup t
        org-hide-emphasis-markers t
        org-log-done 'time
        org-startup-with-inline-images t
        org-special-ctrl-a/e t
        org-src-preserve-indentation nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-tag-alist '(("@work"  . ?w)
                        ("@home"  . ?h)
                        ("laptop" . ?l))
        org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
  :config
  (set-on-evil-state 'org-mode 'insert
                     (setq org-hide-block-startup nil
                           org-hide-emphasis-markers nil)
                     (font-lock-fontify-block))
  (set-on-evil-state 'org-mode 'normal
                     (setq org-hide-block-startup t
                           org-hide-emphasis-markers t)
                     (font-lock-fontify-block))

  (set-popup-buffer (rx bos "*Org Agenda*" eos)))

(req-package org-cliplink
  :general
  (:keymaps
   'org-mode-map
   :prefix my-local-leader-key
   "u" 'org-cliplink))

(req-package org-bullets
  :hook (org-mode . org-bullets-mode))

(req-package org-radiobutton
  :hook (org-mode . org-radiobutton-mode))

(req-package org-preview-html
  :commands org-preview-html-mode)

(req-package org-noter
  :general
  (:keymaps
   'org-mode-map
   :prefix my-local-leader-key
   "n" 'org-noter)
  :commands org-noter)

(req-package ob-elixir)
(req-package ob-go)
(req-package ob-http)
(req-package ob-restclient)
(req-package ob-rust)
(req-package ob-translate)

(provide 'lang-org)
;;; lang-org.el ends here
