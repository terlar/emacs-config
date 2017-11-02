;;; base-modeline.el --- Status bar

;;; Commentary:
;; Master of the state

;;; Code:
(require 'base-vars)
(require 'base-keybinds)

;;;
;; Variables

(defvar my-mode-line-right-format
  (list
   '(:eval mode-line-position)
   '(:eval mode-line-mule-info))
  "The mode line to display on the right side.")

(defvar my-mode-line-right-padding 0
  "The padding on the rightmost side of mode line.")

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
                      (replace (format " (%d/%d) " here total))))
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

;; Enable eldoc support when minibuffer is in use
(use-package eldoc-eval :demand t
  :config (eldoc-in-minibuffer-mode +1))

;; Display info about indentation current indentation settings
(use-package indent-info
  :commands (indent-info-mode
             global-indent-info-mode
             indent-info-toggle-indent-mode
             indent-info-cycle-tab-width-increase
             indent-info-cycle-tab-width-decrease)
  :init
  (global-indent-info-mode +1)
  :config
  ;; Setup indent info padding
  (setq indent-info-prefix nil
        indent-info-suffix " "))

;; Show icons instead of mode names
(use-package mode-icons
  :when (window-system)
  :commands mode-icons-mode
  :init
  (setq mode-icons-desaturate-active t
        mode-icons-desaturate-inactive t)

  (add-hook 'after-make-frame-functions #'mode-icons-mode)
  (add-hook 'after-init-hook #'mode-icons-mode))

;;;
;; Configuration
;; Disable mode line mouse-overs
(setq-default mode-line-default-help-echo nil)

;; Show column and line number
(column-number-mode +1)
(line-number-mode +1)

;; Hidden modes
(add-hook 'after-init-hook
          #'(lambda ()
              (diminish 'abbrev-mode)
              (diminish 'auto-revert-mode)
              (diminish 'eldoc-mode)))

;; Icon modes
(with-eval-after-load "mode-icons"
  (push '("=>" #xf03c FontAwesome) mode-icons)
  (push '("Fill" #xf039 FontAwesome) mode-icons)
  (push '("ws" #xf06e FontAwesome) mode-icons))

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
(defun my|mode-line-fill-right (reserve)
  "Return empty space leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))))

(defun my|mode-line-right-reserve ()
  "Reserve space needed for `my-mode-line-right-format'."
  (+ my-mode-line-right-padding (length (format-mode-line my-mode-line-right-format))))

(delete 'mode-line-mule-info mode-line-format)
(delete 'mode-line-position mode-line-format)
(setq-default mode-line-format
              (append
               mode-line-format
               (list
                '(:eval (my|mode-line-fill-right (my|mode-line-right-reserve)))
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
