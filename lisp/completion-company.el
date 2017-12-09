;;; completion-company.el --- Auto-completion backend

;;; Commentary:
;; Modular in-buffer completion framework.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package)
  (require 'base-lib)
  (require 'base-keybinds))

;;;
;; Packages

(use-package company
  :diminish company-mode
  :hook
  (after-init . global-company-mode)
  (company-completion-started   . +company-turn-off-indent-guides)
  (company-completion-finished  . +company-maybe-turn-on-indent-guides)
  (company-completion-cancelled . +company-maybe-turn-on-indent-guides)
  (company-completion-started   . +company-turn-off-whitespace)
  (company-completion-finished  . +company-maybe-turn-on-whitespace)
  (company-completion-cancelled . +company-maybe-turn-on-whitespace)
  :general
  (:keymaps 'company-mode-map
            :states 'insert
            "C-SPC" 'company-complete-common
            "C-x C-l" '+company-whole-lines
            "C-x C-k" '+company-dict-or-keywords
            "C-x C-f" 'company-files
            "C-x C-]" 'company-etags
            "C-x s"   'company-ispell
            "C-x C-s" 'company-yasnippet
            "C-x C-o" 'company-capf
            "C-x C-n" 'company-dabbrev-code
            "C-x C-p" '+company-dabbrev-code-previous)
  (:keymaps 'company-active-map
            ;; No interference with return key
            [return]  'nil
            "RET"     'nil
            ;; Abort company instead of insert mode
            [escape]  'company-abort
            ;; Complete the common part before cycling
            [tab]     'company-complete-common-or-cycle
            "TAB"     'company-complete-common-or-cycle
            [backtab] 'company-select-previous
            "S-TAB"   'company-select-previous
            ;; Complete the current selection
            "C-e"     'company-complete-selection
            "C-f"     'company-complete-selection
            "C-S-h"   'company-show-doc-buffer
            ;; Filter and search
            "C-s"     'company-filter-candidates
            "C-S-s"   'company-search-candidates)
  (:keymaps 'company-search-map
            ;; Navigate search
            "C-n" 'company-search-repeat-forward
            "C-p" 'company-search-repeat-backward
            ;; Switch between modes
            "C-o" 'company-search-toggle-filtering
            "C-s" '+company-search-abort-and-filter-candidates)
  :preface
  (defvar-local company--indent-guide-mode-on-p nil)
  (defvar-local company--whitespace-mode-on-p nil)

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

  (defun company-preview-if-none-selected-frontend (command)
    "`company-preview-frontend', but only when nothing is selected."
    (message "%s" company-selection)
    (unless (and (eq command 'post-command)
                 (> company-selection 0))
      (company-preview-frontend command)))
  :init
  (autoload 'company-tng-frontend "company-tng" nil t)

  (setq-default company-dabbrev-downcase nil
                company-dabbrev-ignore-case nil
                company-dabbrev-code-other-buffers t)

  (setq company-idle-delay 0.2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-show-numbers t
        company-global-modes
        '(not comint-mode
              erc-mode
              help-mode helpful-mode
              message-mode)
        company-frontends
        '(company-tng-frontend
          company-pseudo-tooltip-unless-just-one-frontend
          company-preview-if-none-selected-frontend
          company-echo-metadata-frontend)
        company-backends
        '(company-capf
          company-yasnippet
          company-files
          (company-dabbrev-code
           company-gtags company-etags
           company-keywords)
          company-oddmuse company-dabbrev)
        company-transformers '(company-sort-by-occurrence)))

(use-package company-try-hard
  :commands company-try-hard)

(use-package company-statistics
  :hook (company-mode . company-statistics-mode)
  :init
  (setq company-statistics-file (concat my-cache-dir "company-stats-cache.el")))

(req-package company-quickhelp
  :loader :el-get
  :hook (company-mode . company-quickhelp-mode)
  :general
  (:keymaps 'company-active-map
            "C-h" 'company-quickhelp-manual-begin)
  :init
  (setq company-quickhelp-delay nil
        company-quickhelp-use-system-tooltip t))

(use-package company-dict
  :commands company-dict)

;;;
;; Autoloads

(autoload 'company-capf "company-capf")
(autoload 'company-dabbrev "company-dabbrev")
(autoload 'company-dabbrev-code "company-dabbrev-code")
(autoload 'company-elisp "company-elisp")
(autoload 'company-etags "company-etags")
(autoload 'company-files "company-files")
(autoload 'company-gtags "company-gtags")
(autoload 'company-ispell "company-ispell")
(autoload 'company-keywords "company-keywords")
(autoload 'company-yasnippet "company-yasnippet")

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
