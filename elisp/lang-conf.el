;;; lang-conf.el --- Configuration formats

;;; Commentary:
;; Configuration, config, conf...

;;; Code:
(use-package nginx-mode)

(use-package systemd
  :config
  (add-hook 'systemd-mode-hook
            #'(lambda ()
                (run-hooks 'prog-mode-hook))))

(provide 'lang-conf)
;;; lang-conf.el ends here
