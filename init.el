;;; init.el --- Main init file

;;; Commentary:
;;; The init file that loads all the components.

;;; Code:
;;(package-initialize)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(push (concat user-emacs-directory "elisp") load-path)

;;;
;; Base
(require 'base)

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
(require 'tool-neotree)            ; Tree navigation
(require 'tool-rotate-text)        ; Toggle between different text

;;;
;; Completion

;;;
;; Language support
(require 'lang-conf)
(require 'lang-data)
(require 'lang-ebook)
(require 'lang-elixir)
(require 'lang-elm)
(require 'lang-emacs-lisp)
(require 'lang-markdown)
(require 'lang-pkgbuild)
(require 'lang-raml)
(require 'lang-rest)
(require 'lang-rst)
(require 'lang-shell)
(require 'lang-web)

;; Keybindings
(unless noninteractive
  (require 'bindings)
  (require 'commands))

(add-hook 'after-init-hook
          #'(lambda ()
              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.1)))
;;; init.el ends here
