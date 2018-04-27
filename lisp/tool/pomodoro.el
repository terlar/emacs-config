;;; tool-pomodoro.el --- Pomodoro Technique -*- lexical-binding: t; -*-

;;; Commentary:
;;; Improving your efficency.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(req-package redtick
  :commands
  (redtick-mode
   redtick
   redtick-with-description))

(provide 'tool-pomodoro)
;;; tool-pomodoro.el ends here
