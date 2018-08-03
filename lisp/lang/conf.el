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

(req-package conf-mode
  :general
  (:keymaps 'conf-mode-map
            ;; Disable conflicting keys
            "C-c \"" 'nil
            "C-c '" 'nil
            "C-c :" 'nil
            "C-c SPC" 'nil))

(req-package nix-mode
  :mode "\\.nix\\'"
  :hook (nix-mode . flycheck-mode)
  :config
  (set-aggressive-indent 'nix-mode :disabled t))

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
  :commands ansible)
(req-package ansible-doc
  :diminish ansible-doc-mode
  :commands ansible-doc
  :hook (ansible . ansible-doc-mode)
  :config
  (defun ansible-doc-at-point ()
    "Ansible doc with selected point"
    (interactive)
    (ansible-doc (thing-at-point 'symbol)))
  (set-doc-fn 'ansible 'ansible-doc-at-point)
  (set-evil-state 'ansible-doc-module-mode 'motion))
(req-package company-ansible
  :commands company-ansible
  :init
  (set-company-backends 'ansible 'company-ansible))

(req-package puppet-mode
  :mode "\\.pp$")

(req-package salt-mode
  :mode "\\.sls$"
  :hook (salt-mode . flyspell-mode)
  :config
  (set-doc-fn 'salt-mode 'salt-mode-browse-doc))

(provide 'lang-conf)
;;; lang-conf.el ends here
