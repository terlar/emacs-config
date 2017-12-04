;;; tool-coverage.el --- Code coverage -*- lexical-binding: t; -*-

;;; Commentary:
;; Visualize code coverage.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(use-package coverlay
  :diminish coverlay-mode
  :commands
  (coverlay-mode
   coverlay-watch-file
   coverlay-load-file
   coverlay-reload-file
   coverlay-display-stats
   coverlay-toggle-overlays)
  :init
  (setq coverlay:mark-tested-lines nil)
  :config
  (set-evil-state 'coverlay-stats-mode 'motion)
  (set-popup-buffer (rx bos "*coverlay-stats*" eos)))

;;;
;; Autoloads

;;;### autoload
(defun +coverlay-mode-enable ()
  "Turn on `coverlay-mode'."
  (coverlay-mode 1)
  (unless (bound-and-true-p coverlay--loaded-filepath)
    (let* ((coverage-file (concat
                           (locate-dominating-file (file-name-directory buffer-file-name) "coverage")
                           "coverage"
                           "/lcov.info")))
      (when (file-exists-p coverage-file)
        (coverlay-watch-file coverage-file)))))

(provide 'tool-coverage)
;;; tool-coverage.el ends here
