;;; tool-recording.el --- Recording -*- lexical-binding: t; -*-

;;; Commentary:
;; Screen capture

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

;;;
;; Packages

(use-package camcorder
  :commands
  (camcorder-record
   camcorder-convert-to-gif)
  :init
  (setq camcorder-output-directory (getenv "XDG_VIDEOS_DIR")
        camcorder-gif-output-directory camcorder-output-directory)
  :config
  (set-popup-buffer (rx bos "*camcorder output*" eos)))

(provide 'tool-recording)
;;; tool-recording.el ends here
