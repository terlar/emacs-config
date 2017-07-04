;;; completion-company.el --- Auto-completion backend

;;; Commentary:
;; Modular in-buffer completion framework.

;;; Code:
(require 'base-vars)
(require 'base-lib)

(use-package company
  :diminish company-mode
  :commands (company-mode
             company-begin-backend
             company-grab-line
             company-complete-common-or-cycle
             company-select-previous-or-abort
             company-search-abort
             company-filter-candidates)
  :preface
  (defvar-local company-whitespace-mode-on-p nil)

  (defun company|add-disable-whitespace-hooks ()
    (add-hook 'company-completion-started-hook #'company-turn-off-whitespace)
    (add-hook 'company-completion-finished-hook #'company-maybe-turn-on-whitespace)
    (add-hook 'company-completion-cancelled-hook #'company-maybe-turn-on-whitespace))

  (defun company-maybe-turn-on-whitespace (&rest ignore)
    (when company-whitespace-mode-on-p (whitespace-mode +1)))

  (defun company-turn-off-whitespace (&rest ignore)
    (when (boundp 'whitespace-mode)
      (setq company-whitespace-mode-on-p whitespace-mode)
      (when whitespace-mode (whitespace-mode -1))))
  :init
  (add-hooks-pair 'after-init
                  '(global-company-mode
                    company|add-disable-whitespace-hooks))
  :config
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
        company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode)
        company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
        company-backends
        '(company-capf
          company-files
          (company-dabbrev-code company-gtags company-etags company-keywords)
          company-dabbrev)
        company-transformers '(company-sort-by-occurrence)))


(use-package company-statistics
  :after company
  :config
  (setq company-statistics-file (concat my-cache-dir "company-stats-cache.el"))
  (quiet! (company-statistics-mode +1)))

(use-package company-quickhelp
  :after company
  :config
  (setq company-quickhelp-delay nil)
  (company-quickhelp-mode +1))

(use-package company-dict
  :commands company-dict)

;; Emoji-word completion
(use-package company-emoji
  :after company
  :config
  (with-eval-after-load "company"
    (push-company-backends 'text-mode '(company-emoji))))

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
(defun company|search-abort-and-filter-candidates ()
  "Abort and filter candidates."
  (interactive)
  (company-search-abort)
  (company-filter-candidates))

;;;###autoload
(defun company|whole-lines (command &optional arg &rest _)
  "Complete lines based on COMMAND and ARG."
  (interactive (list 'interactive))
  (require 'company)
  (pcase command
    ('interactive (company-begin-backend '+company/whole-lines))
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
(defun company|dict-or-keywords ()
  "`company-mode' completion combining `company-dict' and `company-keywords'."
  (interactive)
  (require 'company-dict)
  (require 'company-keywords)
  (let ((company-backends '((company-keywords company-dict))))
    (call-interactively 'company-complete)))

;;;###autoload
(defun company|dabbrev-code-previous ()
  "Complete like `company-dabbrev-code' but backwards."
  (interactive)
  (require 'company-dabbrev)
  (let ((company-selection-wrap-around t))
    (call-interactively #'company-dabbrev-code)
    (company-select-previous-or-abort)))


(provide 'completion-company)
;;; completion-company.el ends here
