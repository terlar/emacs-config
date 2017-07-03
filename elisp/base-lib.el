;;; base-lib.el --- Library functions

;;; Commentary:
;; Custom functions, macros and helpers.

;;; Code:
(require 'base-vars)

;;;
;; Packages

;; Set multiple hooks
(use-package add-hooks
  :commands (add-hooks add-hooks-pair))

(use-package f)    ; files and paths
(use-package s)    ; strings
(use-package dash) ; lists
(use-package ov)   ; overlays

;;;
;; Setup

(defmacro quiet! (&rest forms)
  "Run FORMS without making any noise."
  `(if my-debug-mode
       (progn ,@forms)
     (fset 'my--old-write-region-fn (symbol-function 'write-region))
     (cl-letf ((standard-output (lambda (&rest _)))
               ((symbol-function 'load-file) (lambda (file) (load file nil t)))
               ((symbol-function 'message) (lambda (&rest _)))
               ((symbol-function 'write-region)
                (lambda (start end filename &optional append visit lockname mustbenew)
                  (unless visit (setq visit 'no-message))
                  (when (fboundp 'my--old-write-region-fn)
                    (my--old-write-region-fn
                     start end filename append visit lockname mustbenew))))
               (inhibit-message t)
               (save-silently t))
       ,@forms)))

(defun push-company-backends (mode backends)
  "For MODE add BACKENDS to buffer-local version of `company-backends'."
  (let ((backends (if (listp backends) backends (list backends)))
        (hook (intern (format "%s-hook" (symbol-name mode))))
        (quoted (eq (car-safe backends) 'quote)))
    (add-hook hook `(lambda ()
                      (when (equal major-mode ',mode)
                        (require 'company)
                        (unless (member ',backends company-backends)
                          (setq-local company-backends (append '((,@backends)) company-backends))))))))

;;;
;; Buffers

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (cl-remove-if-not 'buffer-file-name (buffer-list)))))

(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

;;;
;; Editing

(defun retab ()
  "Convert tabs to spaces, or spaces to tabs based on `indent-tabs-mode' and `tab-width'."
  (interactive)
  (if indent-tabs-mode
      (tabify (point-min) (point-max))
    (untabify (point-min) (point-max))))

;;;
;; UI

(defun default-text-scale-reset ()
  "Reset the height of the default face to `my-default-font-height'."
  (interactive)
  (set-face-attribute 'default nil :height my-default-font-height))

(provide 'base-lib)
;;; base-lib.el ends here
