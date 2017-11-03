;;; base-lib.el --- Library functions

;;; Commentary:
;; Custom functions, macros and helpers.

;;; Code:

(eval-when-compile
  (require 'base-vars)

  (defvar eval-repl-alist)

  (declare-function projectile-project-root "projectile")
  (declare-function shackle-match "shackle")
  (declare-function shackle-display-buffer "shackle"))

;;;
;; Packages

;; Set multiple hooks
(use-package add-hooks
  :commands (add-hooks add-hooks-pair))

;; Hide lines based on regexp
(use-package hide-lines
  :commands (hide-lines
             hide-lines-matching
             hide-lines-not-matching
             hide-lines-show-all))

;; Inline popups
(use-package quick-peek
  :commands (quick-peek-show quick-peek-hide))

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

(defmacro add-graphic-hook (&rest forms)
  "Add FORMS as a graphical hook."
  `(if (daemonp)
       (add-hook 'after-make-frame-functions
                 (lambda (frame)
                   (with-selected-frame frame
                     (progn ,@forms))))
     (when (display-graphic-p)
       (add-hook 'after-init-hook
                 (lambda ()
                   (progn ,@forms))))))

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

(defun push-repl-command (mode command)
  "Define a REPL for a MODE by running COMMAND function."
  (push (cons mode command) eval-repl-alist))

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

(defun popup-buffer (buffer &rest plist)
  "Display BUFFER in a shackle popup. Optional PLIST with `shackle-rules'.
Returns the new popup window."
  (declare (indent defun))
  (unless (bufferp buffer)
    (error "%s is not a valid buffer" buffer))
  (setq plist (append plist (shackle-match buffer)))
  (shackle-display-buffer
   buffer
   nil (or plist (shackle-match buffer))))

(defun scratch-buffer (&optional arg)
  "Opens the scratch buffer in a popup window.

If ARG (universal argument) is non-nil, open it in the current window instead of
a popup.

If a region is active, copy it into the scratch buffer."
  (interactive "P")
  (let ((text (and (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))))
        (mode major-mode)
        (derived-p (derived-mode-p 'prog-mode 'text-mode))
        (old-project (projectile-project-root))
        (new-buf (get-buffer-create "*scratch*")))
    (if arg
        (switch-to-buffer new-buf)
      (popup-buffer new-buf))
    (with-current-buffer new-buf
      (setq default-directory old-project)
      (when (and (not (eq major-mode mode))
                 derived-p
                 (functionp mode))
        (funcall mode))
      (if text (insert text)))))

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

(defun refresh ()
  "Refresh buffer."
  (interactive)
  (font-lock-flush))

(provide 'base-lib)
;;; base-lib.el ends here
