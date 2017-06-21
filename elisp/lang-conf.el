;;; lang-conf.el --- Configuration formats/management

;;; Commentary:
;; Configuration, config, conf...

;;; Code:
(require 'base-lib)

;;;
;; Packages
(use-package nginx-mode)

(use-package systemd
  :config
  (add-hook 'systemd-mode-hook
            #'(lambda ()
                (run-hooks 'prog-mode-hook))))

(use-package ansible
  :after yaml-mode
  :commands ansible
  :init
  (add-hook 'yaml-mode-hook #'ansible))

(use-package ansible-doc :after ansible :commands ansible-doc)

(use-package company-ansible
  :after ansible
  :commands company-ansible
  :init
  (with-eval-after-load "company"
    (push-company-backends 'yaml-mode '(company-ansible))))

(use-package puppet-mode
  :mode (("\\.pp\\'"      . puppet-mode)
         ("Puppetfile\\'" . puppet-mode)))

(provide 'lang-conf)
;;; lang-conf.el ends here
