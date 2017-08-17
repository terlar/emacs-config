;;; feature-eval.el --- Code evaluation and REPL

;;; Commentary:
;; Evaluation.

;;; Code:
(require 'base-lib)
(require 'base-vars)

(require 'subr-x)

(eval-when-compile
  (declare-function evil-append-line "evil-commands")
  (defvar comint-last-prompt)
  (defvar shackle-rules))

;;;
;; Build
(defvar eval-builder-alist nil
  "Alist mapping major modes to build functions.")

;;;
;; REPLs
(defvar eval-repl-alist nil
  "Alist mapping major modes to REPL functions.")

(define-minor-mode eval-repl-mode
  "A minor mode for REPL buffers."
  :init-value nil)

(defvar eval-repl-buffer nil
  "The buffer of the last open repl.")

(defun eval--ensure-in-repl-buffer (&optional command)
  "Open a REPL buffer using COMMAND."
  (or (eq (current-buffer) eval-repl-buffer)
      (progn
        (if (and eval-repl-buffer (buffer-live-p eval-repl-buffer))
            (if-let (win (get-buffer-window eval-repl-buffer))
                (select-window win)
              (popup-buffer eval-repl-buffer))
          (when command
            (let ((repl-buffer (save-window-excursion (call-interactively command))))
              (unless (bufferp repl-buffer)
                (error "REPL command didn't return a buffer"))
              (with-current-buffer repl-buffer (eval-repl-mode +1))
              (setq eval-repl-buffer repl-buffer)
              (select-window (popup-buffer repl-buffer)))))
        (when (eq (current-buffer) eval-repl-buffer)
          (goto-char (if (and (derived-mode-p 'comint-mode)
                              (cdr comint-last-prompt))
                         (cdr comint-last-prompt)
                       (point-max)))
          t))))

(defun eval|repl ()
  "Open (or reopen) the REPL associated with the current `major-mode' and place the cursor at the prompt."
  (interactive)
  (when-let (command (cdr (assq major-mode eval-repl-alist)))
    (when (eval--ensure-in-repl-buffer command)
      (when (bound-and-true-p evil-mode)
        (call-interactively #'evil-append-line))
      t)))

(with-eval-after-load "shackle"
  (push '(:custom
          (lambda (b &rest _) (buffer-local-value 'eval-repl-mode b))
          :size 16)
        shackle-rules))

;;;
;; Evaluation
(defvar eval-runner-alist nil
  "Alist mapping major modes to interactive runner functions.")

(use-package quickrun
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region)
  :config
  (with-eval-after-load "shackle"
    (push '("*quickrun*" :size 10) shackle-rules)
    (push '("*eval*" :size 12 :noselect t) shackle-rules)
    (push '("*Pp Eval Output*" :size 12 :noselect t) shackle-rules)))

(provide 'feature-eval)
;;; feature-eval.el ends here
