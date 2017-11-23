;;; base-lib.el --- Library functions

;;; Commentary:
;; Custom functions, macros and helpers.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

(defvar popup-buffer-list nil
  "List of popup buffers.")

(defvar-local documentation-function nil
  "Function to use for documentation look-ups.")

(defvar evil-state-change-mode-alist nil
  "An alist mapping functions to evil state changes for major modes.")

(defvar-local evil-state-change-functions nil
  "The evil state change functions for the current buffer.")

;;;
;; Packages

;; Set multiple hooks
(use-package add-hooks
  :commands
  (add-hooks
   add-hooks-pair))

;; Enable Emacs minor modes by buffer name and contents
(use-package auto-minor-mode
  :demand t)

;; Hide lines based on regexp
(use-package hide-lines
  :commands
  (hide-lines
   hide-lines-matching
   hide-lines-not-matching
   hide-lines-show-all))

;; Inline popups
(use-package quick-peek
  :commands
  (quick-peek-show quick-peek-hide))

(use-package spinner
  :commands
  (spinner-start
   spinner-create
   spinner-print))

(use-package f)    ; files and paths
(use-package s)    ; strings
(use-package dash) ; lists
(use-package ov)   ; overlays

;;;
;; Setup

(defmacro add-hook! (mode &rest forms)
  "Add a lambda hook for MODE using FORMS as body."
  `(add-hooks-pair ,mode (lambda () ,@forms)))

(defmacro set-evil-state (modes state)
  "Set MODES initial STATE using `evil-set-initial-state'."
  `(with-eval-after-load "evil"
     (dolist (mode (if (listp ,modes) ,modes (list ,modes)))
       (evil-set-initial-state mode ,state))))

(defmacro set-evil-state-change (modes &rest plist)
  "Set MODES state change behavior configuration through PLIST.
The list accepts the following properties:

:on-insert FN
  Add code to be run on insert entry.
:on-normal FN
  Add code to be run on normal entry."
  `(with-eval-after-load "evil"
     (dolist (mode (if (listp ,modes) ,modes (list ,modes)))
       (cl-pushnew (cons mode (list ,@plist)) evil-state-change-mode-alist :test #'equal))))

(defmacro set-aggressive-indent (modes &rest plist)
  "Set MODES `agressive-indent' configuration through PLIST.
The list accepts the following properties:

:disabled BOOLEAN
  Disable auto-indent."
  (let ((disabled (plist-get plist :disabled)))
    `(with-eval-after-load "aggressive-indent"
       (dolist (mode (if (listp ,modes) ,modes (list ,modes)))
         (when ,disabled
           (cl-pushnew mode aggressive-indent-excluded-modes :test #'equal))))))

(defmacro set-doc-fn (mode function)
  "Set MODE documentation FUNCTION using `documentation-function'."
  `(add-hooks-pair ,mode (lambda () (setq documentation-function ,function))))

(defun documentation-at-point ()
  "Get documentation at point using `documentation-function'."
  (interactive)
  (if (commandp documentation-function)
      (call-interactively documentation-function)
    (call-interactively 'source-peek)))

(defmacro set-company-backends (mode &rest backends)
  "For MODE add BACKENDS to buffer-local version of `company-backends'."
  `(with-eval-after-load "company"
     (add-hooks-pair
      ,mode
      (lambda ()
        (make-variable-buffer-local 'company-backends)
        (dolist (backend (list ,@(reverse backends)))
          (cl-pushnew backend company-backends :test #'equal))))))

(defun set-popup-buffer (&rest buffers)
  "Display BUFFERS as popup."
  (dolist (buffer buffers)
    (push buffer popup-buffer-list)
    (cl-pushnew `(,buffer
                  (display-buffer-reuse-window
                   display-buffer-in-side-window)
                  (reusable-frames . visible)
                  (side            . bottom)
                  (window-height   . 0.4))
                display-buffer-alist :test #'equal)))

(defun set-repl-command (mode command)
  "Define a REPL for MODE by running COMMAND."
  (cl-pushnew (cons mode command) eval-repl-alist :test #'equal))

(defun set-eval-command (mode command)
  "Define eval function for MODE by running COMMAND."
  (cl-pushnew (cons mode command) eval-runner-alist :test #'equal))

(defmacro add-graphic-hook (&rest forms)
  "Add FORMS as a graphical hook."
  `(if (daemonp)
       (add-hook 'after-make-frame-functions
                 (lambda (frame)
                   (with-selected-frame frame
                     (progn ,@forms))))
     (when (display-graphic-p)
       (add-hook 'after-init-hook
                 (lambda () (progn ,@forms))))))

(defmacro add-terminal-hook (&rest forms)
  "Add FORMS as a graphical hook."
  `(if (daemonp)
       (add-hook 'after-make-frame-functions
                 (lambda (frame)
                   (unless (display-graphic-p frame)
                     (with-selected-frame frame
                       (progn ,@forms)))))
     (unless (display-graphic-p)
       (add-hook 'after-init-hook
                 (lambda () (progn ,@forms))))))

(defmacro quiet! (&rest forms)
  "Run FORMS without making any noise."
  `(if my-debug-mode
       (progn ,@forms)
     (fset '+old-write-region-fn (symbol-function 'write-region))
     (cl-letf ((standard-output (lambda (&rest _)))
               ((symbol-function 'load-file) (lambda (file) (load file nil t)))
               ((symbol-function 'message) (lambda (&rest _)))
               ((symbol-function 'write-region)
                (lambda (start end filename &optional append visit lockname mustbenew)
                  (unless visit (setq visit 'no-message))
                  (when (fboundp '+old-write-region-fn)
                    (+old-write-region-fn
                     start end filename append visit lockname mustbenew))))
               (inhibit-message t)
               (save-silently t))
       ,@forms)))

(defun run-prog-mode-hooks ()
  "Run hooks all `prog-mode' hooks."
  (run-hooks 'prog-mode-hook))

;;;
;; Buffers

;;;### autoload
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (cl-remove-if-not 'buffer-file-name (buffer-list)))))

;;;### autoload
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

;;;### autoload
(defun toggle-scratch-buffer ()
  "Toggle scratch buffer."
  (interactive)
  (toggle-popup-buffer t (rx bos "*scratch*" eos)))

;;;### autoload
(defun open-and-switch-to-buffer (command buffer &optional do-switch)
  "Open a `COMMAND', and switch to that `BUFFER' when `DO-SWITCH'."
  (interactive)
  (if (get-buffer buffer)
      (switch-to-buffer buffer)
    (funcall command)
    (bury-buffer)
    (when do-switch
      (switch-to-buffer buffer))))

;;;### autoload
(defun get-buffer-display-time (buffer)
  "Get the display time for BUFFER."
  (with-current-buffer buffer
    (float-time buffer-display-time)))

;;;### autoload
(defun toggle-popup-buffer (&optional select buffer-rx)
  "Toggle and SELECT popup buffer matching BUFFER-RX."
  (interactive)
  (let ((open-popup-buffers
         (if buffer-rx
             (seq-filter
              (lambda (buff)
                (string-match buffer-rx (buffer-name buff)))
              (mapcar #'window-buffer (window-at-side-list)))
           (seq-filter
            (lambda (buff)
              (seq-some
               (lambda (buff-rx)
                 (string-match buff-rx (buffer-name buff)))
               popup-buffer-list))
            (mapcar #'window-buffer (window-at-side-list)))))
        (closed-popup-buffers
         (if buffer-rx
             (seq-filter
              (lambda (buff)
                (string-match buffer-rx (buffer-name buff)))
              (buffer-list))
           (seq-filter
            (lambda (buff)
              (seq-some
               (lambda (buff-rx)
                 (string-match buff-rx (buffer-name buff)))
               popup-buffer-list))
            (buffer-list)))))
    (cond ((= 1 (length open-popup-buffers))
           (delete-window (get-buffer-window (car open-popup-buffers))))
          ((and (> 0 (length open-popup-buffers) (not select)))
           (delete-window
            (get-buffer-window
             (car
              (sort
               open-popup-buffers
               (lambda (a b)
                 (> (get-buffer-display-time a)
                    (get-buffer-display-time b))))))))
          ((> 0 (length open-popup-buffers))
           (ivy-read "Close popup: "
                     (mapcar #'buffer-name open-popup-buffers)
                     :action (lambda (x) (delete-window (get-buffer-window x)))
                     :caller 'toggle-popup-buffer))
          ((seq-empty-p closed-popup-buffers)
           (message "No popup buffers found"))
          ((= 1 (length closed-popup-buffers))
           (pop-to-buffer (car closed-popup-buffers)))
          ((not select)
           (pop-to-buffer
            (car
             (sort
              closed-popup-buffers
              (lambda (a b)
                (> (get-buffer-display-time a)
                   (get-buffer-display-time b)))))))
          (t
           (ivy-read "Open popup: "
                     (mapcar #'buffer-name closed-popup-buffers)
                     :action (lambda (x) (pop-to-buffer x))
                     :caller 'toggle-popup-buffer)))))

;;;
;; Editing

;;;### autoload
(defun retab-buffer ()
  "Convert tabs to spaces, or spaces to tabs based on `indent-tabs-mode' and `tab-width'."
  (interactive)
  (save-excursion
    (if indent-tabs-mode
        (tabify (point-min) (point-max))
      (untabify (point-min) (point-max)))))

;;;
;; UI

(defface readability-mode-face
  '((t (:family "Noto Serif" :height 1.2)))
  "Face to use for `readability-mode'."
  :group 'readability-mode)

(defcustom readability-mode-line-spacing 0.4
  "Readability `line-spacing'."
  :type 'number
  :group 'readability-mode)

(defvar-local readability-mode-saved-state-plist nil
  "Properties containing the state before `readability-mode' was enabled.
Contains following state:

  :line-spacing NUMBER
  :variable-pitch BOOLEAN")
(defvar-local readability-mode-remapping nil
  "Readability `face-remap' cookie.")

;;;### autoload
(defun readability-mode ()
  "Improve the readability."
  (interactive)
  (when readability-mode-remapping
    (face-remap-remove-relative readability-mode-remapping))

  (if (null (get this-command 'state-on-p))
      (progn
        (setq readability-mode-saved-state-plist
              (list :line-spacing line-spacing :variable-pitch (bound-and-true-p buffer-face-mode)))

        (variable-pitch-mode 1)
        (setq line-spacing readability-mode-line-spacing
              readability-mode-remapping
              (face-remap-add-relative 'variable-pitch 'readability-mode-face))
        (put this-command 'state-on-p t))
    (progn
      (variable-pitch-mode (plist-get readability-mode-saved-state-plist :variable-pitch))
      (setq line-spacing (plist-get readability-mode-saved-state-plist :line-spacing)
            readability-mode-saved-state-plist nil)

      (put this-command 'state-on-p nil)))

  (force-window-update (current-buffer)))

;;;### autoload
(defun get-faces (pos)
  "Get the font faces at POS."
  (remq nil
        (list
         (get-char-property pos 'read-face-name)
         (get-char-property pos 'face)
         (plist-get (text-properties-at pos) 'face))))

;;;### autoload
(defun hide-fringes ()
  "Hide fringes for window."
  (interactive)
  (set-window-fringes nil 0 0))

;;;### autoload
(defun default-text-scale-reset ()
  "Reset the height of the default face to `my-default-font-height'."
  (interactive)
  (set-face-attribute 'default nil :height my-default-font-height))

;;;### autoload
(defun refresh ()
  "Refresh buffer."
  (interactive)
  (font-lock-flush))

(provide 'base-lib)
;;; base-lib.el ends here
