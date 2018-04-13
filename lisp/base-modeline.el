;;; base-modeline.el --- Status bar -*- lexical-binding: t; -*-

;;; Commentary:
;; Master of the state

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package)
  (require 'base-lib)
  (require 'base-keybinds))

;;;
;; Variables

(defvar flycheck-mode-line "")

(defvar mode-line-right-format
  (list
   (propertize " " 'display '((space :width 2)))
   '(:eval flycheck-mode-line)
   (propertize " " 'display '((space :width 1)))
   '(:eval mode-line-position)
   '(:eval mode-line-mule-info))
  "The mode line to display on the right side.")

;;;
;; Settings

;; Disable mode line mouse-overs
(setq-default mode-line-default-help-echo nil)

;; Show column and line number
(column-number-mode 1)
(line-number-mode 1)

(delete 'mode-line-mule-info mode-line-format)
(delete 'mode-line-position mode-line-format)
(setq-default mode-line-format
              (append
               mode-line-format
               mode-line-right-format))

;;;
;; Packages

;; Displays current match and total matches information
(req-package anzu
  :diminish anzu-mode
  :hook
  (after-init . global-anzu-mode)
  (isearch-mode-end . anzu--reset-status)
  (my-evil-esc . anzu--reset-status)
  :general
  ([remap query-replace]                'anzu-query-replace)
  ([remap query-replace-regexp]         'anzu-query-replace-regexp)
  (:keymaps
   'isearch-mode-map
   [remap isearch-query-replace]        'anzu-isearch-query-replace
   [remap isearch-query-replace-regexp] 'anzu-isearch-query-replace-regexp)
  :defer 2
  :init
  (defun +anzu-update-mode-line (here total)
    (when anzu--state
      (let ((status (cl-case anzu--state
                      (search (format "(%s/%d%s)"
                                      (anzu--format-here-position here total)
                                      total (if anzu--overflow-p "+" "")))
                      (replace-query (format "(%d replace)" total))
                      (replace (format "(%d/%d)" here total))))
            (face (if (and (zerop total) (not (string= isearch-string "")))
                      'anzu-mode-line-no-match
                    'anzu-mode-line)))
        (propertize (concat " " status) 'face face))))

  (setq anzu-mode-line-update-function #'+anzu-update-mode-line
        anzu-minimum-input-length 1
        anzu-search-threshold 250))

(req-package evil-anzu
  :demand t
  :after (evil anzu))

;; Hide mode line
(req-package hide-mode-line
  :commands hide-mode-line-mode)

;; Display info about indentation current indentation settings
(req-package indent-info
  :demand t
  :init
  (setq indent-info-prefix nil
        indent-info-suffix " ")
  :config
  (global-indent-info-mode 1))

;; Icons in mode-line
(req-package mode-icons
  :commands mode-icons-mode
  :init
  (add-graphic-hook (mode-icons-mode 1)))

;; Evil state indicator bar
(defun +mode-line-evil-state-bar (&optional state)
  "Generate the evil mode-line tag for STATE as a colorized bar."
  (let ((tag (evil-state-property state :tag t))
        (color (alist-get state my-evil-mode-color-list my-evil-default-mode-color)))
    ;; prepare mode-line: add tooltip
    (if (stringp tag)
        (progn
          (face-remap-add-relative 'anzu-mode-line :background color)
          (propertize " "
                      'face (list :background color :foreground color :box nil)
                      'help-echo (evil-state-property state :name)
                      'mouse-face 'mode-line-highlight))
      tag)))
(with-eval-after-load 'evil
  (defalias 'evil-generate-mode-line-tag #'+mode-line-evil-state-bar))

;; Flycheck
(with-eval-after-load 'flycheck
  (setq flycheck-mode-line
        '(:eval
          (pcase flycheck-last-status-change
            (`finished (if flycheck-current-errors
                           (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                          (+ (or .warning 0) (or .error 0)))))
                             (propertize (format "✖ %s Issue%s" count (if (eq 1 count) "" "s"))
                                         'face 'error))
                         (propertize "✔ No Issues" 'face 'success)))
            (`running     (propertize "⟲ Running" 'face 'warning))
            (`no-checker  (propertize "⚠ No Checker" 'face 'warning))
            (`not-checked "✖ Disabled")
            (`errored     (propertize "⚠ Error" 'face 'error))
            (`interrupted (propertize "⛔ Interrupted" 'face 'error))
            (`suspicious  "")))))

(provide 'base-modeline)
;;; base-modeline.el ends here
