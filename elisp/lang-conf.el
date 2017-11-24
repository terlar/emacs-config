;;; lang-conf.el --- Configuration formats/management -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration, config, conf...

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-lib)
  (require 'base-keybinds))

;;;
;; Packages

(req-package nginx-mode
  :mode
  "nginx\\.conf$"
  "nginx/.+\\.conf$"
  "conf/.+\\.conf$"
  :magic-fallback
  "\\(?:.*\n\\)*\\(?:http\\|server\\|location .+\\|upstream .+\\)[ \n\t]+{")

(req-package systemd
  :mode
  ("\\.\\(automount\\|busname\\|mount\\|service\\|slice\\|socket\\|swap\\|target\\|timer\\|link\\|netdev\\|network\\)$" . systemd-mode))

(req-package ansible
  :minor
  "site\\.yml$"
  "roles/.+\\.yml$"
  :require yaml-mode
  :commands ansible)
(req-package ansible-doc
  :require ansible
  :after ansible
  :diminish ansible-doc-mode
  :commands
  (ansible-doc
   ansible-doc-mode)
  :config
  (defun ansible-doc-at-point ()
    "Ansible doc with selected point"
    (interactive)
    (ansible-doc (thing-at-point 'symbol)))
  (set-doc-fn 'ansible 'ansible-doc-at-point)
  (set-evil-state 'ansible-doc-module-mode 'motion)
  (set-popup-buffer (rx bos "*ansible-doc " (one-or-more anything) "*" eos))

  (add-hook 'ansible-hook #'ansible-doc-mode))
(req-package company-ansible
  :require company ansible
  :after ansible
  :commands company-ansible
  :config
  (set-company-backends 'ansible 'company-ansible))

(req-package puppet-mode
  :mode "\\.pp$")

(req-package salt-mode
  :mode
  "\\.sls$"
  :config
  (set-doc-fn 'salt-mode 'salt-mode-browse-doc)

  (add-hooks-pair 'salt-mode 'flyspell-mode))

(provide 'lang-conf)
;;; lang-conf.el ends here
