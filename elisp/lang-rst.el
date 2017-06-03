;;; lang-rst.el --- reStructuredText

;;; Commentary:
;; reStructuredText (sometimes abbreviated as RST, ReST, or reST) is a
;; file format for textual data used primarily in the Python
;; programming language community for technical documentation.

;;; Code:
(use-package rst
  :mode
  ("\\.\\(txt\\|re?st\\)$" . rst-mode)
  :commands rst-mode
  :config
  (add-hook 'rst-mode-hook
            #'(lambda ()
                (linum-mode -1)
                (auto-fill-mode +1)
                (variable-pitch-mode +1)
                (setq line-spacing 2
                      fill-column 80)))

  ;; Header underline display
  (set-face-attribute 'rst-adornment nil
                      :strike-through "black"
                      :foreground (face-background 'default)
                      :background (face-background 'default))

  ;; Typography
  (set-face-attribute 'rst-literal nil :inherit 'fixed-pitch)

  (set-face-attribute 'rst-level-1 nil
                      :background 'unspecified :weight 'bold :height 1.8)
  (set-face-attribute 'rst-level-2 nil
                      :background 'unspecified :weight 'bold :height 1.4)
  (set-face-attribute 'rst-level-3 nil
                      :background 'unspecified :weight 'bold :height 1.2)
  (set-face-attribute 'rst-level-4 nil
                      :background 'unspecified :weight 'bold :height 1.0)
  (set-face-attribute 'rst-level-5 nil
                      :background 'unspecified :weight 'bold :height 1.0)
  (set-face-attribute 'rst-level-6 nil
                      :background 'unspecified :weight 'bold :height 1.0))

(provide 'lang-rst)
;;; lang-rst.el ends here
