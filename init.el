;;; init.el --- Main init file -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:
;;; The init file that loads all the components.

;;; Code:

(eval-and-compile
  (push (concat user-emacs-directory "elisp") load-path)
  (push (concat user-emacs-directory "vendor") load-path))

;;;
;; Base

;; Calls (package-initialize)
(require 'base)

;;;
;; Features

(require 'feature-evil)
(require 'feature-workspaces)
(require 'feature-version-control)
(require 'feature-spellcheck)
(require 'feature-syntax-checker)
(require 'feature-eval)
(require 'feature-jump)
(require 'feature-debug)
(require 'feature-lsp)
(require 'feature-preview)
(require 'feature-speed-reading)

;;;
;; Completion

(require 'completion-company)      ; In-buffer completion
(require 'completion-ivy)          ; General completion

;;;
;; Tools

(require 'tool-coverage)           ; Code coverage
(require 'tool-docsets)            ; Docsets
(require 'tool-dired)              ; Directories
(require 'tool-neotree)            ; Tree navigation
(require 'tool-notes)              ; Note taking
(require 'tool-rotate-text)        ; Toggle between different text
(require 'tool-container)          ; Container management

;;;
;; Language support

(require 'lang-data)
(require 'lang-conf)
(require 'lang-ebook)
(require 'lang-elixir)
(require 'lang-elm)
(require 'lang-emacs-lisp)
(require 'lang-erlang)
(require 'lang-go)
(require 'lang-haskell)
(require 'lang-java)
(require 'lang-javascript)
(require 'lang-lisp)
(require 'lang-lua)
(require 'lang-markdown)
(require 'lang-opengl)
(require 'lang-org)
(require 'lang-pkgbuild)
(require 'lang-python)
(require 'lang-rest)
(require 'lang-rst)
(require 'lang-ruby)
(require 'lang-rust)
(require 'lang-scala)
(require 'lang-shell)
(require 'lang-uml)
(require 'lang-web)

(req-package-finish)

(unless noninteractive
  (require 'bindings)
  (require 'commands)
  (require 'theme))

;;; init.el ends here
