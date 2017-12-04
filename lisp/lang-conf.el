;;; lang-conf.el --- Configuration formats/management -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration, config, conf...

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-lib))

;;;
;; Packages

(use-package nginx-mode
  :mode
  "nginx\\.conf$"
  "nginx/.+\\.conf$"
  "conf/.+\\.conf$"
  :magic-fallback
  "\\(?:.*\n\\)*\\(?:http\\|server\\|location .+\\|upstream .+\\)[ \n\t]+{")

(use-package systemd
  :mode
  ("\\.\\(automount\\|busname\\|mount\\|service\\|slice\\|socket\\|swap\\|target\\|timer\\|link\\|netdev\\|network\\)$" . systemd-mode))

(use-package ansible
  :minor
  "site\\.yml$"
  "roles/.+\\.yml$"
  :commands ansible)
(use-package ansible-doc
  :diminish ansible-doc-mode
  :commands ansible-doc
  :hook (ansible . ansible-doc-mode)
  :config
  (defun ansible-doc-at-point ()
    "Ansible doc with selected point"
    (interactive)
    (ansible-doc (thing-at-point 'symbol)))
  (set-doc-fn 'ansible 'ansible-doc-at-point)
  (set-evil-state 'ansible-doc-module-mode 'motion)
  (set-popup-buffer (rx bos "*ansible-doc " (one-or-more anything) "*" eos)))
(use-package company-ansible
  :after ansible
  :commands company-ansible
  :config
  (set-company-backends 'ansible 'company-ansible))

(use-package puppet-mode
  :mode "\\.pp$")

(use-package salt-mode
  :mode "\\.sls$"
  :hook (salt-mode . flyspell-mode)
  :config
  (set-doc-fn 'salt-mode 'salt-mode-browse-doc))

(provide 'lang-conf)
;;; lang-conf.el ends here
