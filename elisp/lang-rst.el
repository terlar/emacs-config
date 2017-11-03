;;; lang-rst.el --- reStructuredText -*- lexical-binding: t; -*-

;;; Commentary:
;; reStructuredText (sometimes abbreviated as RST, ReST, or reST) is a
;; file format for textual data used primarily in the Python
;; programming language community for technical documentation.

;;; Code:

(defcustom rst-header-scaling nil
  "Whether to use variable-height faces for headers.
When non-nil, `rst-header-face' will inherit from
`variable-pitch' and the scaling values in
`rst-header-scaling-values' will be applied to
headers of levels one through six respectively."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (set-default symbol value)
         (rst-update-header-faces value))
  :group 'rst-faces)

(defcustom rst-header-scaling-values
  '(2.0 1.7 1.4 1.1 1.0 1.0)
  "List of scaling values for headers of level one through six.
Used when `rst-header-scaling' is non-nil."
  :type 'list
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (set-default symbol value)
         (rst-update-header-faces rst-header-scaling value))
  :group 'rst-faces)

(defvar rst-adornment-regexp nil
  "Regular expression to match adornments.")

(eval-when-compile
  (autoload 'evil|insert-state-disable-variable-pitch-mode "feature-evil")
  (autoload 'evil|insert-state-restore-variable-pitch-mode "feature-evil")
  (autoload 'hide-lines-matching "hide-lines"))

;;;
;; Packages

(use-package rst
  :mode ("\\.\\(txt\\|re?st\\)$" . rst-mode)
  :commands rst-mode
  :preface
  (defun rst-setup ()
    (setq line-spacing 2
          fill-column 80)
    (customize-set-variable 'rst-header-scaling t)
    (hide-lines-matching rst-adornment-regexp))

  (defun rst|add-insert-state-hooks ()
    (add-hook 'evil-insert-state-entry-hook #'rst|evil-insert-state-entry nil t)
    (add-hook 'evil-insert-state-exit-hook #'rst|evil-insert-state-exit nil t))

  (defun rst|evil-insert-state-entry ()
    "Setup reStructuredText edit mode."
    (evil|insert-state-disable-variable-pitch-mode)
    (customize-set-variable 'rst-header-scaling nil)
    (hide-lines-show-all))

  (defun rst|evil-insert-state-exit ()
    "Reset reStructuredText edit mode."
    (evil|insert-state-restore-variable-pitch-mode)
    (customize-set-variable 'rst-header-scaling t)
    (hide-lines-matching rst-adornment-regexp))
  :config
  (setq rst-preferred-adornments
        '((35 over-and-under 0) ; ?# For parts
          (42 over-and-under 0) ; ?* For chapters
          (61 simple 0)         ; ?= For sections
          (45 simple 0)         ; ?- For subsections
          (94 simple 0)         ; ?^ For subsubsections
          (34 simple 0))        ; ?" For paragraphs
        rst-adornment-regexp
        (concat "^[" rst-adornment-chars "]\\{3,\\}$"))

  (add-hooks-pair 'rst-mode
                  '(auto-fill-mode
                    variable-pitch-mode
                    rst-setup
                    rst|add-insert-state-hooks)))

;;;
;; Autoloads

;;;###autoload
(defun rst-update-header-faces (&optional scaling scaling-values)
  "Update header faces, depending on if header SCALING is desired.
If so, use given list of SCALING-VALUES relative to the baseline
size of `rst-header-face'."
  (dotimes (num 6)
    (let* ((face-name (intern (format "rst-level-%s" (1+ num))))
           (scale (cond ((not scaling) 1.0)
                        (scaling-values (float (nth num scaling-values)))
                        (t (float (nth num rst-header-scaling-values))))))
      (unless (get face-name 'saved-face) ; Don't update customized faces
        (set-face-attribute face-name nil :background nil :weight 'bold :height scale)))))

(provide 'lang-rst)
;;; lang-rst.el ends here
