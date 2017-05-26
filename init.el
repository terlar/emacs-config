;;; init.el --- Main init file
;;; Commentary:
;;; The init file that loads all the components.
;;; Code:
;;(package-initialize)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(add-to-list 'load-path (concat user-emacs-directory "elisp"))

;;;
;; Base
(require 'base)
(require 'base-functions)
(unless noninteractive
  (require 'base-theme)
  (require 'base-ui)
  (require 'base-editor)
  (require 'base-projects)
  (require 'base-keybinds))

;;;
;; Features
(require 'feature-evil) ; Extensible vi layer

;;;
;; Completion

;;;
;; Language support
(require 'lang-markdown)
(require 'lang-rst)

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))
;;; init.el ends here
