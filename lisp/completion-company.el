;;; completion-company.el --- Auto-completion backend

;;; Commentary:
;; Modular in-buffer completion framework.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package)
  (require 'base-lib))

;;;
;; Packages

(req-package company
  :diminish company-mode
  :commands
  (company-mode
   global-company-mode
   company-begin-backend
   company-grab-line
   company-complete-common-or-cycle
   company-select-previous-or-abort
   company-search-abort
   company-filter-candidates)
  :preface
  (defvar-local company--indent-guide-mode-on-p nil)
  (defvar-local company--whitespace-mode-on-p nil)

  (defun +company-add-disable-indent-guides-hooks ()
    (add-hook 'company-completion-started-hook #'+company-turn-off-indent-guides)
    (add-hook 'company-completion-finished-hook #'+company-maybe-turn-on-indent-guides)
    (add-hook 'company-completion-cancelled-hook #'+company-maybe-turn-on-indent-guides))

  (defun +company-add-disable-whitespace-hooks ()
    (add-hook 'company-completion-started-hook #'+company-turn-off-whitespace)
    (add-hook 'company-completion-finished-hook #'+company-maybe-turn-on-whitespace)
    (add-hook 'company-completion-cancelled-hook #'+company-maybe-turn-on-whitespace))

  (defun +company-maybe-turn-on-indent-guides (&rest ignore)
    (when company--indent-guide-mode-on-p (indent-guide-mode 1)))

  (defun +company-turn-off-indent-guides (&rest ignore)
    (when (boundp 'indent-guide-mode)
      (setq company--indent-guide-mode-on-p indent-guide-mode)
      (when indent-guide-mode (indent-guide-mode 0))))

  (defun +company-maybe-turn-on-whitespace (&rest ignore)
    (when company--whitespace-mode-on-p (whitespace-mode 1)))

  (defun +company-turn-off-whitespace (&rest ignore)
    (when (boundp 'whitespace-mode)
      (setq company--whitespace-mode-on-p whitespace-mode)
      (when whitespace-mode (whitespace-mode 0))))
  :demand t
  :init
  (setq-default company-dabbrev-downcase nil
                company-dabbrev-ignore-case nil
                company-dabbrev-code-other-buffers t)

  (setq company-echo-delay 0
        company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-require-match 'never
        company-tooltip-limit 10
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-show-numbers t
        company-global-modes '(not comint-mode erc-mode message-mode help-mode helpful-mode)
        company-frontends
        '(company-tng-frontend
          company-pseudo-tooltip-unless-just-one-frontend
          company-echo-metadata-frontend
          company-preview-frontend
          company-quickhelp-frontend)
        company-backends
        '((company-files
           company-keywords
           company-capf
           company-yasnippet)
          (company-abbrev company-dabbrev)
          company-ispell)
        company-transformers '(company-sort-by-occurrence))

  (autoload 'company-tng-frontend "company-tng" nil t)
  :config
  (+company-add-disable-indent-guides-hooks)
  (+company-add-disable-whitespace-hooks)

  (global-company-mode 1))

(req-package company-try-hard
  :require company
  :commands company-try-hard
  :bind ("C-\\" . company-try-hard)
  :config
  (bind-keys :map company-active-map
             ("C-\\" . company-try-hard)))

(req-package company-statistics
  :require company
  :after company
  :init
  (setq company-statistics-file (concat my-cache-dir "company-stats-cache.el"))
  :config
  (quiet! (company-statistics-mode 1)))

(req-package company-quickhelp
  :require company
  :after company
  :init
  (setq company-quickhelp-delay nil)
  :config
  (company-quickhelp-mode 1))

(req-package company-dict
  :require company
  :commands company-dict)

;;;
;; Autoloads

(autoload 'company-capf "company-capf")
(autoload 'company-yasnippet "company-yasnippet")
(autoload 'company-dabbrev "company-dabbrev")
(autoload 'company-dabbrev-code "company-dabbrev-code")
(autoload 'company-keywords "company-keywords")
(autoload 'company-etags "company-etags")
(autoload 'company-elisp "company-elisp")
(autoload 'company-files "company-files")
(autoload 'company-gtags "company-gtags")
(autoload 'company-ispell "company-ispell")

;;;###autoload
(defun +company-search-abort-and-filter-candidates ()
  "Abort and filter candidates."
  (interactive)
  (company-search-abort)
  (company-filter-candidates))

;;;###autoload
(defun +company-whole-lines (command &optional arg &rest _)
  "Complete lines based on COMMAND and ARG."
  (interactive (list 'interactive))
  (require 'company)
  (pcase command
    ('interactive (company-begin-backend '+company-whole-lines))
    ('prefix      (company-grab-line "^[\t\s]*\\(.+\\)" 1))
    ('candidates
     (all-completions
      arg
      (split-string
       (replace-regexp-in-string
        "^[\t\s]+" ""
        (concat (buffer-substring-no-properties (point-min) (line-beginning-position))
                (buffer-substring-no-properties (line-end-position) (point-max))))
       "\\(\r\n\\|[\n\r]\\)" t)))))

;;;###autoload
(defun +company-dict-or-keywords ()
  "`company-mode' completion combining `company-dict' and `company-keywords'."
  (interactive)
  (require 'company-dict)
  (require 'company-keywords)
  (let ((company-backends '((company-keywords company-dict))))
    (call-interactively 'company-complete)))

;;;###autoload
(defun +company-dabbrev-code-previous ()
  "Complete like `company-dabbrev-code' but backwards."
  (interactive)
  (require 'company-dabbrev)
  (let ((company-selection-wrap-around t))
    (call-interactively #'company-dabbrev-code)
    (company-select-previous-or-abort)))


(provide 'completion-company)
;;; completion-company.el ends here
