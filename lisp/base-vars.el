;;; base-vars.el --- Base variables -*- lexical-binding: t; -*-

;;; Commentary:
;; Customization through variables.

;;; Code:

;;;
;; Base

(defvar my-debug-mode (or (getenv "DEBUG") init-file-debug)
  "Debug mode, enable through DEBUG=1 or use --debug-init.")

(defvar my-site-lisp-dir
  (expand-file-name "site-lisp" user-emacs-directory)
  "Directory for shared files.")

(defvar my-cache-dir
  (if (getenv "XDG_CACHE_HOME")
      (concat (getenv "XDG_CACHE_HOME") "/emacs/")
    (expand-file-name "~/.cache/emacs/"))
  "Use XDG-based cache directory.")

(defvar my-data-dir
  (if (getenv "XDG_DATA_HOME")
      (concat (getenv "XDG_DATA_HOME") "/emacs/")
    (expand-file-name "~/.local/share/emacs/"))
  "Use XDG-based data directory.")

(defvar my-packages-dir (concat my-data-dir "packages/")
  "Use XDG-based packages directory.")

;;;
;; UI

(defvar my-fringe-width 12
  "The fringe width to use.")

(defvar my-completion-system 'ivy
  "The completion system to use.")

;;;
;; Theme

(defvar my-theme 'tao-yang
  "The color theme to use.")

(defvar my-default-font-height 120
  "The default font height to use.")

(defvar my-font "Iosevka Slab"
  "The monospace font to use.")

(defvar my-variable-pitch-font "Noto Sans"
  "The regular font to use.")

(defvar my-unicode-font "Noto Mono"
  "Fallback font for unicode glyphs.")

(defvar my-evil-default-mode-color "#AB47BC"
  "Default mode color for Evil states.")

(defvar my-evil-mode-color-list
  `((normal   . "#4CAF50")
    (emacs    . "#2196F3")
    (insert   . "#2196F3")
    (replace  . "#F44336")
    (visual   . "#FF9800"))
  "Mode color corresponding to Evil state.")

(provide 'base-vars)
;;; base-vars.el ends here
