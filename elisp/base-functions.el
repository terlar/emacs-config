;;; base-functions.el --- Custom functions

;;; Commentary:
;; Custom functions, macros and helpers.

;;; Code:
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
                  (my--old-write-region-fn
                   start end filename append visit lockname mustbenew)))
               (inhibit-message t)
               (save-silently t))
       ,@forms)))

(defun retab ()
  "Convert tabs to spaces, or spaces to tabs based on `indent-tabs-mode' and `tab-width'."
  (interactive)
  (if indent-tabs-mode
      (tabify (point-min) (point-max))
    (untabify (point-min) (point-max))))

(defun ediff-copy-both-to-C ()
  "Copy change from both A and B to C."
  (interactive)
  (ediff-copy-diff
   ediff-current-difference nil 'C nil
   (concat
    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(provide 'base-functions)
;;; base-functions.el ends here
