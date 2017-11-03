;;; lang-conf.el --- Configuration formats/management -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration, config, conf...

;;; Code:

(autoload 'push-company-backends "base-lib")

;;;
;; Packages

(use-package nginx-mode
  :mode "/conf/.*\\.conf$")

(use-package systemd
  :config
  (add-hook 'systemd-mode-hook
            #'(lambda ()
                (run-hooks 'prog-mode-hook))))

(use-package ansible
  :after yaml-mode
  :commands ansible
  :init
  (add-hooks-pair 'yaml-mode 'ansible))

(use-package ansible-doc :after ansible :commands ansible-doc)

(use-package company-ansible
  :after ansible
  :commands company-ansible
  :init
  (with-eval-after-load "company"
    (push-company-backends 'yaml-mode '(company-ansible))))

(use-package puppet-mode)

(provide 'lang-conf)
;;; lang-conf.el ends here
