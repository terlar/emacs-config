;;; lang-lua.el --- Lua -*- lexical-binding: t; -*-

;;; Commentary:
;; Lua is a powerful, efficient, lightweight, embeddable scripting language. It
;; supports procedural programming, object-oriented programming, functional
;; programming, data-driven programming, and data description.

;;; Code:

(autoload 'push-company-backends "base-lib")
(autoload 'push-repl-command "base-lib")

;;;
;; Packages

(use-package lua-mode :mode ("\\.lua$")
  :interpreter "lua"
  :preface
  (defun lua-repl ()
    "Open Lua REPL."
    (interactive)
    (lua-start-process "lua" "lua")
    (pop-to-buffer lua-process-buffer))
  :init
  (add-hooks-pair 'lua-mode 'flycheck-mode)
  :config
  (push-repl-command 'lua-mode #'lua-repl))

(use-package company-lua
  :when (package-installed-p 'company)
  :after lua-mode
  :config
  (with-eval-after-load "company"
    (push-company-backends 'lua-mode '(company-lua))))

(provide 'lang-lua)
;;; lang-lua.el ends here
