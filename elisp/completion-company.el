;;; completion-company.el --- Auto-completion backend

;;; Commentary:
;; Modular in-buffer completion framework.

;;; Code:
;; base.el vars
(defvar my-cache-dir nil)

(use-package company
  :commands (company-mode
             global-company-mode
             company-complete company-complete-common
             company-manual-begin company-grab-line)
  :bind
  (:map
   company-mode-map
   ([tab] . company-indent-or-complete)
   :map
   company-active-map
   ;; Don't interfere with `evil-delete-backward-word' in insert mode
   ("C-w"     . nil)

   ;; Don't affect return key
   ("RET"      . nil)
   ("<return>" . nil)
   ([escape]   . company-abort)

   ("C-e"     . company-complete-selection)
   ("C-f"     . company-complete-selection)
   ([tab]     . company-complete-common-or-cycle)
   ([backtab] . company-select-previous)

   ("C-o"     . company-search-toggle-filtering)
   ("C-n"     . company-select-next)
   ("C-p"     . company-select-previous)
   ("C-h"     . company-quickhelp-manual-begin)
   ("C-S-h"   . company-show-doc-buffer)
   ("C-S-s"   . company-search-candidates)
   ("C-s"     . company-filter-candidates)
   :map
   company-search-map
   ("C-n" . company-search-repeat-forward)
   ("C-p" . company-search-repeat-backward)
   ("C-s" . company-search-abort-and-filter-candidates)
   ([escape] . company-search-abort))
  :preface
  (defvar-local company-whitespace-mode-on-p nil)

  (defun company-maybe-turn-on-whitespace (&rest ignore)
    (when company-whitespace-mode-on-p (whitespace-mode +1)))

  (defun company-turn-off-whitespace (&rest ignore)
    (when (boundp 'whitespace-mode)
      (setq company-whitespace-mode-on-p whitespace-mode)
      (when whitespace-mode (whitespace-mode -1))))

  (defun company-indent-or-complete ()
    "Try to indent before trying to complete."
    (interactive)
    (if (looking-at "\\_>")
        (company-complete-common-or-cycle)
      (indent-for-tab-command)))

  (defun company-search-abort-and-filter-candidates ()
    "Abort and filter candidates."
    (interactive)
    (company-search-abort)
    (company-filter-candidates))
  :init
  (add-hook 'after-init-hook
            #'(lambda ()
                (add-hook 'company-completion-started-hook #'company-turn-off-whitespace)
                (add-hook 'company-completion-finished-hook #'company-maybe-turn-on-whitespace)
                (add-hook 'company-completion-cancelled-hook #'company-maybe-turn-on-whitespace)
                (global-company-mode +1)))
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
        company-transformers '(company-sort-by-occurrence))

  (with-eval-after-load 'yasnippet
    (nconc company-backends '(company-yasnippet))))


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
  (add-hook 'text-mode-hook
            #'(lambda ()
                (setq-local company-backends (append '((company-emoji)) company-backends)))))

;;;
;; Autoloads
(autoload 'company-capf "company-capf")
(autoload 'company-yasnippet "company-yasnippet")
(autoload 'company-dabbrev "company-dabbrev")
(autoload 'company-dabbrev-code "company-dabbrev-code")
(autoload 'company-etags "company-etags")
(autoload 'company-elisp "company-elisp")
(autoload 'company-files "company-files")
(autoload 'company-gtags "company-gtags")
(autoload 'company-ispell "company-ispell")

(provide 'completion-company)
;;; completion-company.el ends here
