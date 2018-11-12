;;; blacklist-minor-modes.el --- Blacklist minor modes -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2017 Terje Larsen
;; All rights reserved.

;; Author: Terje Larsen <terlar@gmail.com>
;; Keywords: emacs
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; indent-info is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; indent-info is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `blacklist-minor-modes' is a small utility function to disable and re-enable
;; minor modes on certain hooks.

;;; Code:

(defvar-local blacklist-minor-modes-list nil
  "Stores the currently blacklisted minor-modes.")

(defun blacklist-minor-modes-restore (&rest args)
  "Restore blacklisted minor modes from `blacklist-minor-modes-list'.
Takes any number of ARGS as some hooks will run the function with ARGS."
  (dolist (mode blacklist-minor-modes-list)
    (funcall mode 1))
  (setq blacklist-minor-modes-list nil))

(defun blacklist-minor-modes (modes disable-hooks restore-hooks)
  "Blacklist MODES for all DISABLE-HOOKS and restore on RESTORE-HOOKS."
  (let* ((modes (if (listp modes) modes (list modes)))
         (disable-hooks (if (listp disable-hooks)
                            disable-hooks
                          (list disable-hooks)))
         (restore-hooks (if (listp restore-hooks)
                            restore-hooks
                          (list restore-hooks)))
         (disable-fn-name (intern (format
                                   "blacklist-disable--%s"
                                   (mapconcat #'symbol-name modes "-")))))
    (defalias disable-fn-name
      `(lambda (&rest args)
         (dolist (mode ',modes)
           (when (and (boundp mode) (eval mode))
             (funcall mode 0)
             (push mode blacklisted-minor-modes)))))

    (dolist (hook disable-hooks)
      (add-hook hook disable-fn-name))
    (dolist (hook restore-hooks)
      (add-hook hook #'blacklist-minor-modes-restore))))

(provide 'blacklist-minor-modes)
;;; blacklist-minor-modes.el ends here
