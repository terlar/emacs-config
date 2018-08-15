;;; lang-javascript.el --- JavaScript -*- lexical-binding: t; -*-

(use-package typescript-mode
  :mode "\\.tsx?$"
  :hook
  (typescript-mode . flycheck-mode)
  (typescript-mode . rainbow-delimiters-mode)
  (typescript-mode . rainbow-identifiers-mode))

(use-package json-mode
  :mode "\\.json$"
  :hook
  (json-mode . flycheck-mode))

(use-package add-node-modules-path
  :hook (js2-mode . add-node-modules-path))

(use-package prettier-js
  :diminish prettier-js-mode
  :hook (js2-mode . prettier-js-mode))

(use-package rjsx-mode
  :mode
  ("\\.jsx$"             . rjsx-mode)
  ("components/.+\\.js$" . rjsx-mode)
  :commands rjsx-mode
  :general
  (:keymaps 'rjsx-mode-map
            "<"   'nil
            "C-d" 'nil)
  :preface
  :init
  ;; Auto-detect JSX file
  (push (cons (lambda ()
                (and buffer-file-name
                     (equal (file-name-extension buffer-file-name) "js")
                     (re-search-forward "\\(^\\s-*import React\\|\\( from \\|require(\\)[\"']react\\)"
                                        magic-mode-regexp-match-limit t)
                     (progn
                       (goto-char (match-beginning 1))
                       (not (sp-point-in-string-or-comment)))))
              'rjsx-mode)
        magic-mode-alist))

(use-package coffee-mode
  :mode "\\.coffee$"
  :defines coffee-indent-like-python-mode
  :init (setq coffee-indent-like-python-mode t))

(use-package web-beautify
  :commands
  (web-beautify-js web-beautify-html web-beautify-css)
  :general
  (:keymaps '(js2-mode-map json-mode-map) :states 'normal
            "gQ" 'web-beautify-js)
  (:keymaps 'html-mode-map :states 'normal
            "gQ" 'web-beautify-html)
  (:keymaps 'css-mode-map :states 'normal
            "gQ" 'web-beautify-css))

(provide 'lang-javascript)
;;; lang-javascript.el ends here
