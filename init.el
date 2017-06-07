;;; init.el --- Main init file

;;; Commentary:
;;; The init file that loads all the components.

;;; Code:
;;(package-initialize)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(dolist (dir '("elisp" "vendor"))
  (add-to-list 'load-path (concat user-emacs-directory dir)))

;;;
;; Base
(require 'base)
(require 'base-functions)
(unless noninteractive
  (require 'base-theme)
  (require 'base-ui)
  (require 'base-modeline)
  (require 'base-editor)
  (require 'base-projects))

;;;
;; Features
(require 'feature-evil)            ; Extensible vi layer
(require 'feature-jump)            ; Jump to definition
(require 'feature-spellcheck)
(require 'feature-syntax-checker)
(require 'feature-version-control)
(require 'feature-workspaces)

;;;
;; Completion
(require 'completion-company)      ; In-buffer completion
(require 'completion-ivy)          ; General completion

;;;
;; Tools
(require 'tool-dired)              ; Directories
(require 'tool-rotate-text)        ; Toggle between different text

;;;
;; Completion

;;;
;; Language support
(require 'lang-emacs-lisp)
(require 'lang-markdown)
(require 'lang-raml)
(require 'lang-rest)
(require 'lang-rst)
;; (require 'lang-elixir)
;; (require 'lang-go)
;; (require 'lang-haskell)
;; (require 'lang-javascript)
;; (require 'lang-python)
;; (require 'lang-ruby)
;; (require 'lang-rust)
;; (require 'lang-web)

;; Keybindings
(unless noninteractive
  (require 'bindings)
  (require 'commands))

(add-hook 'after-init-hook
          #'(lambda ()
              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.1)))
;;; init.el ends here
