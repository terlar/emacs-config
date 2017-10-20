;;; tool-coverage.el --- Code coverage

;;; Commentary:
;; Visualize code coverage.

;;; Code:
(use-package coverlay
  :commands (coverlay-mode
             coverlay-watch-file
             coverlay-load-file
             coverlay-reload-file
             coverlay-display-stats
             coverlay-toggle-overlays)
  :config
  (setq coverlay:mark-tested-lines nil))

(provide 'tool-coverage)
;;; tool-coverage.el ends here
