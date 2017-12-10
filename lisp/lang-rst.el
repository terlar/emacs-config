;;; lang-rst.el --- reStructuredText -*- lexical-binding: t; -*-

;;; Commentary:
;; reStructuredText (sometimes abbreviated as RST, ReST, or reST) is a
;; file format for textual data used primarily in the Python
;; programming language community for technical documentation.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-lib))

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
         (+rst-update-header-faces value))
  :group 'rst-faces)

(defcustom rst-header-scaling-values
  '(2.0 1.7 1.4 1.1 1.0 1.0)
  "List of scaling values for headers of level one through six.
Used when `rst-header-scaling' is non-nil."
  :type 'list
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (set-default symbol value)
         (+rst-update-header-faces rst-header-scaling value))
  :group 'rst-faces)

(defvar rst-adornment-regexp nil
  "Regular expression to match adornments.")

(eval-when-compile
  (autoload '+evil-insert-state-disable-variable-pitch-mode "feature-evil")
  (autoload '+evil-insert-state-restore-variable-pitch-mode "feature-evil"))

;;;
;; Packages

(use-package rst
  :mode
  ("\\.rst$" . rst-mode)
  :hook
  (rst-mode . auto-fill-mode)
  (rst-mode . readable-mode)
  (rst-mode . +rst-mode-setup)
  :preface
  (defun +rst-mode-setup ()
    (customize-set-variable 'rst-header-scaling t)
    (hide-lines-matching rst-adornment-regexp))

  (defun +rst-mode-on-insert-state-entry ()
    (+evil-insert-state-disable-variable-pitch-mode)
    (customize-set-variable 'rst-header-scaling nil)
    (hide-lines-show-all))

  (defun +rst-mode-on-normal-state-entry ()
    (+evil-insert-state-restore-variable-pitch-mode)
    (customize-set-variable 'rst-header-scaling t)
    (hide-lines-matching rst-adornment-regexp))
  :config
  (setq rst-preferred-adornments
        '((?# over-and-under 0)  ; # For parts
          (?* over-and-under 0)  ; * For chapters
          (?= simple 0)          ; = For sections
          (?- simple 0)          ; - For subsections
          (?^ simple 0)          ; ^ For subsubsections
          (?\" simple 0))        ; " For paragraphs
        rst-adornment-regexp
        (concat "^[" rst-adornment-chars "]\\{3,\\}$"))

  (evil-stateful-set-state-entry
   'rst-mode
   :on-insert #'+rst-mode-on-insert-state-entry
   :on-normal #'+rst-mode-on-normal-state-entry))

;;;
;; Autoloads

;;;###autoload
(defun +rst-update-header-faces (&optional scaling scaling-values)
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
