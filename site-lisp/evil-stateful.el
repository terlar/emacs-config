;;; evil-stateful.el --- Attach functions to evil state changes -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2017 Terje Larsen
;; All rights reserved.

;; Author: Terje Larsen <terlar@gmail.com>
;; URL: https://github.com/terlar/evil-stateful.el
;; Keywords: convenience
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `evil-stateful' is a Emacs minor mode that is intended as an extension with
;; convenience functions for `evil-mode'. It adds the ability to attach
;; functions per major-mode and state change. For example if you are using
;; markdown-mode you might want to hide markup on `evil-normal-state' entry, but
;; show markup on `evil-insert-state' entry.

;;; Code:
(require 'evil)

(defgroup evil-stateful nil
  "Evil state related extensions."
  :group 'evil)

(defvar evil-stateful-state-entry-functions-mode-alist nil
  "An alist mapping functions to evil state entry for major modes.")

(defvar-local evil-stateful-state-entry-functions nil
  "The evil state entry functions for the current buffer.")

(defun evil-stateful-init-state-entry-functions ()
  "Initialize evil state entry functions."
  (when-let* ((plist (cdr (assq major-mode evil-stateful-state-entry-functions-mode-alist))))
    (setq-local evil-stateful-state-entry-functions plist)))

(defun evil-stateful-normal-state-entry-run-function ()
  "Run normal state entry function."
  (let ((fn (plist-get evil-stateful-state-entry-functions :on-normal)))
    (when (functionp fn) (funcall fn))))

(defun evil-stateful-insert-state-entry-run-function ()
  "Run insert state entry function."
  (let ((fn (plist-get evil-stateful-state-entry-functions :on-insert)))
    (when (functionp fn) (funcall fn))))

(defun evil-stateful-visual-state-entry-run-function ()
  "Run visual state entry function."
  (let ((fn (plist-get evil-stateful-state-entry-functions :on-visual)))
    (when (functionp fn) (funcall fn))))

(defun evil-stateful-replace-state-entry-run-function ()
  "Run replace state entry function."
  (let ((fn (plist-get evil-stateful-state-entry-functions :on-replace)))
    (when (functionp fn) (funcall fn))))

(defun evil-stateful-operator-state-entry-run-function ()
  "Run operator state entry function."
  (let ((fn (plist-get evil-stateful-state-entry-functions :on-operator)))
    (when (functionp fn) (funcall fn))))

(defun evil-stateful-motion-state-entry-run-function ()
  "Run motion state entry function."
  (let ((fn (plist-get evil-stateful-state-entry-functions :on-motion)))
    (when (functionp fn) (funcall fn))))

(defun evil-stateful-mode-enable ()
  "Enable `evil-stateful-mode' in the current buffer."
  (unless (minibufferp)
    (evil-stateful-mode 1)))

;;;###autoload
(defmacro evil-stateful-set-state-entry (modes &rest plist)
  "Set MODES state entry behavior configuration through PLIST.
The list accepts the following properties:

:on-normal FN
  Add code to be run on normal entry.
:on-insert FN
  Add code to be run on insert entry.
:on-visual FN
  Add code to be run on visual entry.
:on-replace FN
  Add code to be run on replace entry.
:on-operator FN
  Add code to be run on operator entry.
:on-motion FN
  Add code to be run on motion entry.

The configured function will be executed on state entry for the configured property."
  `(with-eval-after-load 'evil
     (dolist (mode (if (listp ,modes) ,modes (list ,modes)))
       (cl-pushnew (cons mode (list ,@plist)) evil-stateful-state-entry-functions-mode-alist :test #'equal))))

;;;###autoload
(define-minor-mode evil-stateful-mode
  "Toggle `evil-stateful-mode'.

When `evil-stateful-mode' is activated it will execute functions attached to
state entry for a specific mode when configured."
  :global nil
  :lighter nil
  :group 'evil-stateful
  (if evil-stateful-mode
      (progn
        (add-hook 'after-change-major-mode-hook #'evil-stateful-init-state-entry-functions)
        (add-hook 'evil-normal-state-entry-hook #'evil-stateful-normal-state-entry-run-function)
        (add-hook 'evil-insert-state-entry-hook #'evil-stateful-insert-state-entry-run-function)
        (add-hook 'evil-visual-state-entry-hook #'evil-stateful-visual-state-entry-run-function)
        (add-hook 'evil-replace-state-entry-hook #'evil-stateful-replace-state-entry-run-function)
        (add-hook 'evil-operator-state-entry-hook #'evil-stateful-operator-state-entry-run-function)
        (add-hook 'evil-motion-state-entry-hook #'evil-stateful-motion-state-entry-run-function))
    (progn
      (remove-hook 'after-change-major-mode-hook #'evil-stateful-init-state-entry-functions)
      (remove-hook 'evil-normal-state-entry-hook #'evil-stateful-normal-state-entry-run-function)
      (remove-hook 'evil-insert-state-entry-hook #'evil-stateful-insert-state-entry-run-function)
      (remove-hook 'evil-visual-state-entry-hook #'evil-stateful-visual-state-entry-run-function)
      (remove-hook 'evil-replace-state-entry-hook #'evil-stateful-replace-state-entry-run-function)
      (remove-hook 'evil-operator-state-entry-hook #'evil-stateful-operator-state-entry-run-function)
      (remove-hook 'evil-motion-state-entry-hook #'evil-stateful-motion-state-entry-run-function))))

;;;###autoload
(define-globalized-minor-mode global-evil-stateful-mode
  evil-stateful-mode evil-stateful-mode-enable
  "Global minor mode to run functions on evil state change.")

(provide 'evil-stateful)
;;; evil-stateful.el ends here
