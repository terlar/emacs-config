;;; lang-pkgbuild.el --- PKGBUILD -*- lexical-binding: t; -*-

;;; Commentary:
;; A PKGBUILD is a shell script containing the build information required by
;; Arch Linux packages.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(use-package pkgbuild-mode
  :mode "/PKGBUILD$")

(provide 'lang-pkgbuild)
;;; lang-pkgbuild.el ends here
