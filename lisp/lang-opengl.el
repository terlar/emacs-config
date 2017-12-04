;;; lang-opengl.el --- OpenGL -*- lexical-binding: t; -*-

;;; Commentary:
;; Open Graphics Library (OpenGL) is a cross-language, cross-platform
;; application programming interface (API) for rendering 2D and 3D vector
;; graphics. The API is typically used to interact with a graphics processing
;; unit (GPU), to achieve hardware-accelerated rendering.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(use-package glsl-mode
  :mode "\\.\\(frag\\|vert\\)$")

(provide 'lang-opengl)
;;; lang-opengl.el ends here
