;;; tool-container.el --- Container -*- lexical-binding: t; -*-

;;; Commentary:
;; Containers, the way of the future!

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-lib))

;;;
;; Packages

(req-package dockerfile-mode
  :mode "Dockerfile$")

(req-package docker
  :init
  (setq docker-containers-show-all t)

  (autoload 'docker-images "docker-images" nil t)
  (autoload 'docker-containers "docker-containers" nil t)
  (autoload 'docker-volumes "docker-volumes" nil t)
  (autoload 'docker-networks "docker-networks" nil t)

  (set-popup-buffer
   (rx bos "*" (or "docker-images" "docker-containers"
                   "docker-volumes" "docker-networks") "*" eos))

  (set-evil-state '(docker-images-mode
                    docker-containers-mode
                    docker-volumes-mode
                    docker-networks-mode) 'emacs))

(req-package docker-compose-mode
  :mode "docker-compose\\.yml")

(req-package docker-tramp
  :demand t
  :init
  (setq docker-tramp-use-names t))

(req-package kubernetes
  :commands
  (kubernetes-overview
   kubernetes-display-pods
   kubernetes-display-configmaps))

(req-package kubernetes-evil
  :demand t
  :after (evil kubernetes))

(req-package kubernetes-tramp
  :demand t)

(provide 'tool-container)
;;; tool-container.el ends here
