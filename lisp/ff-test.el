;;; ff-test.el --- Find a test/implementation file related to current file -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2019 Terje Larsen
;; All rights reserved.

;; Author: Terje Larsen <terlar@gmail.com>
;; Keywords: files
;; Package-Requires: ((emacs "25.1"))
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

;; Find related test file from implementation and vice-versa.

;;; Code:

(defgroup ff-test nil
  "Find a test/implementation file related to current file."
  :group 'find-file)

(defcustom ff-test-suffixes '(".test" ".spec")
  "List of test suffixes to look associate with implementation file."
  :type '(repeat string)
  :group 'ff-test)

(make-variable-buffer-local 'ff-test-suffixes)

(require 'find-file)
(require 'seq)

;; Optional dependencies to set project local directories.
(autoload 'project-roots "project")
(autoload 'project-current "project")

;;;###autoload
(defun ff-test-project-dirs (dirs)
  "Return DIRS as a list of expanded project directories."
  (apply 'append
         (mapcar (lambda (root)
                   (mapcar (lambda (dir) (concat root dir))
                           dirs))
                 (seq-map #'expand-file-name (project-roots (project-current t))))))

;;;###autoload
(defun ff-test-converter (file-name)
  "Possible related test/implementation files for FILE-NAME."
  (let* ((file-dir (file-name-directory file-name))
         (file-base (file-name-base file-name))
         (ext (file-name-extension file-name))
         (file-target (replace-regexp-in-string
                       (concat "\\(" (mapconcat #'regexp-quote ff-test-suffixes "\\|") "\\)$")
                       ""
                       file-base))
         (test-file-p (not (string= file-base file-target)))
         (search-in-dirs (when (listp ff-search-directories) ff-search-directories))
         file-targets)
    (cond
     (test-file-p
      (setq file-targets (list (concat file-target "." ext))))
     (t
      (setq file-targets (mapcar (lambda (suffix) (concat file-target suffix "." ext))
                                 ff-test-suffixes))))
    (apply 'append
           file-targets
           (mapcar (lambda (dir) (let ((relative-dir
                                   (replace-regexp-in-string
                                    (replace-regexp-in-string "\\\\\\*" "[^/]+" (regexp-quote (concat dir "/")))
                                    ""
                                    file-dir)))
                              (unless (string= relative-dir file-dir)
                                (mapcar (lambda (target) (concat relative-dir target))
                                        file-targets))))
                   search-in-dirs))))

(provide 'ff-test)
;;; ff-test.el ends here
