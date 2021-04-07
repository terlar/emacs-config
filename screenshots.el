;; (run-with-timer 1 nil `(lambda () ))
(let* ((media-dir (expand-file-name "media"))
       (captures `((org-mode . ((find-file (expand-file-name "samples/sample.org" ,media-dir))
                                (no-fringes)
                                (follow-mode 1)
                                (split-window-right)
                                (goto-char 49)))
                   (markdown-mode . ((find-file (expand-file-name "samples/sample.md" ,media-dir))
                                     (no-fringes)
                                     (follow-mode 1)
                                     (split-window-right)
                                     (goto-char 51)))
                   (rst-mode . ((find-file (expand-file-name "samples/sample.rst" ,media-dir))
                                (no-fringes)
                                (follow-mode 1)
                                (split-window-right)
                                (goto-char 239)))
                   (emacs-lisp-mode . ((find-file (expand-file-name "../lisp/readable-mono-theme.el" ,media-dir))
                                       (no-fringes)
                                       (follow-mode 1)
                                       (split-window-right)
                                       (goto-char 1443))))))
  (blink-cursor-mode 0)

  (dolist (capture captures)
    (let ((name (symbol-name (car capture)))
          (actions (cdr capture)))
      (delete-other-windows)
      (dolist (action actions) (eval action))
      (message nil)
      (unwind-protect (with-temp-file (expand-file-name (concat name ".svg") media-dir)
                      (insert (x-export-frames nil 'svg)))))))