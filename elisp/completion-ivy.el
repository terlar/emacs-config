;;; completion-ivy.el --- Completion system

;;; Commentary:
;; Completing all your things.

;;; Code:
(require 'base-vars)
(require 'base-keybinds)

;;;
;; Packages

(use-package ivy :demand t
  :diminish ivy-mode
  :commands (ivy-mode
             ivy-format-function-line
             ivy-exit-with-action)
  :general
  (:keymaps 'ivy-mode-map
            [remap find-file]                 'counsel-find-file
            [remap switch-to-buffer]          'ivy-switch-buffer
            [remap recentf]                   'counsel-recentf
            [remap imenu]                     'counsel-imenu
            [remap bookmark-jump]             'counsel-bookmark
            [remap projectile-switch-project] 'counsel-projectile-switch-project
            [remap projectile-find-file]      'counsel-projectile-find-file
            [remap imenu-anywhere]            'ivy-imenu-anywhere
            [remap execute-extended-command]  'counsel-M-x
            [remap describe-function]         'counsel-describe-function
            [remap describe-variable]         'counsel-describe-variable
            [remap describe-face]             'counsel-describe-face)
  :config
  (setq-default projectile-completion-system 'ivy
                smex-completion-method 'ivy
                magit-completing-read-function #'ivy-completing-read)

  (setq ivy-height 12
        ivy-do-completion-in-region nil
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ;; Don't use ^ as initial input
        ivy-initial-inputs-alist nil
        ;; highlight til EOL
        ivy-format-function #'ivy-format-function-line
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil)

  (ivy-mode +1))

(use-package swiper :commands (swiper swiper-all))

(use-package counsel
  :after ivy
  :config
  (use-package counsel-projectile :demand t)
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"))

;; Used by `counsel-M-x'
(use-package smex
  :commands (smex smex-major-mode-commands)
  :config
  (setq smex-save-file (concat my-cache-dir "/smex-items"))
  (smex-initialize))

(use-package all-the-icons-ivy
  :after ivy
  :when (window-system)
  :commands all-the-icons-ivy-setup
  :init
  (all-the-icons-ivy-setup))

;;;
;; Autoloads

;;;###autoload
(defun ivy|wgrep-occur ()
  "Invoke the search+replace wgrep buffer on the current ag/rg search results."
  (interactive)
  (unless (window-minibuffer-p)
    (user-error "No completion session is active"))
  (require 'wgrep)
  (let* ((caller (ivy-state-caller ivy-last))
         (occur-fn (plist-get ivy--occurs-list caller))
         (buffer
          (generate-new-buffer
           (format "*ivy-occur%s \"%s\"*"
                   (if caller (concat " " (prin1-to-string caller)) "")
                   ivy-text))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall occur-fn))
      (setf (ivy-state-text ivy-last) ivy-text)
      (setq ivy-occur-last ivy-last)
      (setq-local ivy--directory ivy--directory))
    (ivy-exit-with-action
     `(lambda (_)
        (pop-to-buffer ,buffer)
        (ivy-wgrep-change-to-wgrep-mode)))))


(provide 'completion-ivy)
;;; completion-ivy.el ends here
