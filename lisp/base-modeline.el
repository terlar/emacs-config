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

(defvar mode-line-space
  '(:propertize
    " " display
    ((space :width 1)))
  "Space between mode line components.")

(defvar mode-line-right-format
  `((:eval mode-line-mule-info)
    ,mode-line-space
    (:eval vc-mode)
    ,mode-line-space
    (:eval mode-line-modes))
  "The mode line to display on the right side.")

;;;
;; Functions
(defun mode-line-modified-icons ()
  "Icon representation of `mode-line-modified'."
  (cond (buffer-read-only
         (concat (all-the-icons-octicon "lock" :v-adjust -0.05) " "))
        ((buffer-modified-p)
         (concat (all-the-icons-faicon "floppy-o" :v-adjust -0.05) " "))
        ((and buffer-file-name
              (not (file-exists-p buffer-file-name)))
         (concat (all-the-icons-octicon "circle-slash" :v-adjust -0.05) " "))))

(defun mode-line-remote-icons ()
  "Icon representation of `mode-line-remote'."
  (when (and buffer-file-name
             (file-remote-p buffer-file-name))
    (concat (all-the-icons-octicon "radio-tower" :v-adjust -0.02) " ")))

(defun mode-line-right ()
  "Render the `mode-line-right-format'."
  (let ((formatted-line (format-mode-line mode-line-right-format)))
    (list
     (propertize
      " "
      'display `((space :align-to
                        (- (+ right right-fringe right-margin)
                           ,(+ 1
                               (* (string-width formatted-line)
                                  0.75))))))
     formatted-line)))

;;;
;; Settings

;; Disable mode line mouse-overs
(setq-default mode-line-default-help-echo nil)

;; Show column and line number
(column-number-mode 1)
(line-number-mode 1)

;; Remove defaults and add right mode line.
(delete 'mode-line-mule-info mode-line-format)
(delete 'mode-line-modes mode-line-format)
(delete '(vc-mode vc-mode) mode-line-format)
(setq-default mode-line-modified '((:eval (mode-line-modified-icons))))
(setq-default mode-line-remote '((:eval (mode-line-remote-icons))))
(setq-default mode-line-format
              (append
               mode-line-format
               '((:eval (mode-line-right)))))

;;;
;; Packages

;; Icons for the mode line
(req-package all-the-icons
  :commands
  (all-the-icons-alltheicon
   all-the-icons-faicon
   all-the-icons-fileicon
   all-the-icons-oction
   all-the-icons-wicon))

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
  :preface
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
  :init
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
  (setq indent-info-insert-target 'mode-line-mule-info
        indent-info-prefix nil
        indent-info-suffix " ")
  :config
  (global-indent-info-mode 1))

;; Evil state indicator bar
(defun +mode-line-evil-state-bar (&optional state)
  "Generate the evil mode-line tag for STATE as a colorized bar."
  (let ((tag (evil-state-property state :tag t))
        (color (alist-get state my-evil-mode-color-list my-evil-default-mode-color)))
    ;; prepare mode-line: add tooltip
    (if (stringp tag)
        (progn
          (with-eval-after-load 'anzu
            (face-remap-add-relative 'anzu-mode-line :background color))
          (propertize " "
                      'face (list :background color :foreground color :box nil)
                      'help-echo (evil-state-property state :name)
                      'mouse-face 'mode-line-highlight))
      tag)))
(with-eval-after-load 'evil
  (defalias 'evil-generate-mode-line-tag #'+mode-line-evil-state-bar))

;; VC
(defun +shorten-vc-mode-line (string)
  "Shorten `version-control' STRING in mode-line and add icon."
  (cond
   ((string-prefix-p "Git" string)
    (concat (all-the-icons-octicon "git-branch" :v-adjust -0.05)
            " "
            (if (> (length string) 30)
                (concat (substring-no-properties string 4 30) "…")
              (substring-no-properties string 4))))
   (t
    string)))
(advice-add 'vc-git-mode-line-string :filter-return '+shorten-vc-mode-line)

(provide 'base-modeline)
;;; base-modeline.el ends here
