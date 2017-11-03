;;; base-modeline.el --- Status bar -*- lexical-binding: t; -*-

;;; Commentary:
;; Master of the state

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-lib)
  (require 'base-keybinds))

;;;
;; Variables

(defvar my-mode-line-right-format
  (list
   '(:eval flycheck-mode-line)
   "  "
   '(:eval mode-line-position)
   '(:eval mode-line-mule-info))
  "The mode line to display on the right side.")

(defvar my-mode-line-fill
  (propertize " " 'display `((space :width 3)))
  "The mode line fill space.")

(defvar my-mode-line-bar-string " "
  "The string to use for the mode line bar.")

(defvar my-mode-line-bar-padding 6
  "The padding around the mode line bar.")

;;;
;; Packages

;; Displays current match and total matches information
(use-package anzu
  :diminish anzu-mode
  :commands
  (global-anzu-mode
   anzu-query-replace anzu-query-replace-regexp
   anzu-isearch-query-replace
   anzu-isearch-query-replace-regexp)
  :functions anzu--reset-status
  :preface
  (defun my--update-mode-line (here total)
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
        (propertize (concat my-mode-line-bar-string status) 'face face))))
  :general
  ([remap query-replace]        'anzu-query-replace)
  ([remap query-replace-regexp] 'anzu-query-replace-regexp)
  (:keymaps 'isearch-mode-map
            [remap isearch-query-replace]        'anzu-isearch-query-replace
            [remap isearch-query-replace-regexp] 'anzu-isearch-query-replace-regexp)
  :config
  (setq anzu-mode-line-update-function #'my--update-mode-line
        anzu-minimum-input-length 1
        anzu-search-threshold 250)

  ;; Ensure anzu state is cleared when searches are done
  (add-hooks-pair '(isearch-mode-end my-evil-esc) #'anzu--reset-status)

  (global-anzu-mode +1))

(use-package evil-anzu :after evil)

(use-package mode-icons
  :commands mode-icons-mode
  :init (add-graphic-hook (mode-icons-mode +1)))

;; Enable eldoc support when minibuffer is in use
(use-package eldoc-eval
  :commands eldoc-in-minibuffer-mode
  :init (eldoc-in-minibuffer-mode +1))

;; Display info about indentation current indentation settings
(use-package indent-info
  :commands (indent-info-mode
             global-indent-info-mode
             indent-info-toggle-indent-mode
             indent-info-cycle-tab-width-increase
             indent-info-cycle-tab-width-decrease)
  :init (global-indent-info-mode +1)
  :config (setq indent-info-prefix nil
                indent-info-suffix " "))

;;;
;; Configuration
;; Disable mode line mouse-overs
(setq-default mode-line-default-help-echo nil)

;; Show column and line number
(column-number-mode +1)
(line-number-mode +1)

;; Evil state indicator
(autoload 'evil-state-property "evil-common")
(defun my|mode-line-bar-evil-state (&optional state)
  "Generate the evil mode-line tag for STATE as a colorized bar."
  (let ((tag (evil-state-property state :tag t))
        (color (alist-get state my-evil-mode-color-list my-evil-default-mode-color)))
    ;; prepare mode-line: add tooltip
    (if (stringp tag)
        (progn
          (face-remap-add-relative 'anzu-mode-line :background color)
          (propertize my-mode-line-bar-string
                      'face (list :background color :foreground color :box t)
                      'help-echo (evil-state-property state :name)
                      'mouse-face 'mode-line-highlight))
      tag)))
(with-eval-after-load "evil"
  (defalias 'evil-generate-mode-line-tag #'my|mode-line-bar-evil-state))

;; Right aligned mode line support
(defun my|mode-line-fill-right (right)
  "Return empty space between LEFT and RIGHT mode line."
  (let* ((available-width (- (window-total-width) (string-width right))))
    (propertize " "
                'display `((space
                            :align-to ,available-width)))))

(delete 'mode-line-mule-info mode-line-format)
(delete 'mode-line-position mode-line-format)
(setq-default mode-line-format
              (append
               mode-line-format
               (list
                my-mode-line-fill
                my-mode-line-right-format)))

(eval-when-compile
  (defvar flycheck-mode-line))

(with-eval-after-load "flycheck"
  (setq flycheck-mode-line
        '(:eval
          (pcase flycheck-last-status-change
            (`finished (if flycheck-current-errors
                           (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                          (+ (or .warning 0) (or .error 0)))))
                             (propertize (format "✖ %s Issue%s" count (if (eq 1 count) "" "s"))
                                         'face 'error))
                         (propertize "✔ No Issues"
                                     'face 'success)))
            (`running     (propertize "⟲ Running"
                                      'face 'warning))
            (`no-checker  (propertize "⚠ No Checker"
                                      'face 'warning))
            (`not-checked "✖ Disabled")
            (`errored     (propertize "⚠ Error"
                                      'face 'error))
            (`interrupted (propertize "⛔ Interrupted"
                                      'face 'error))
            (`suspicious  "")))))

(provide 'base-modeline)
;;; base-modeline.el ends here
